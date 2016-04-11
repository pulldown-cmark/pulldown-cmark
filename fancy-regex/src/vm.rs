// Copyright 2016 Google Inc. All rights reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

//! Backtracking VM for implementing fancy regexes.

use std::usize;
use regex::Regex;

use codepoint_len;
use prev_codepoint_ix;
use Result;
use Error;

pub const OPTION_TRACE: u32 = 1;

// TODO: make configurable
const MAX_STACK: usize = 1000000;

#[derive(Debug)]
pub enum Insn {
    End,
    Any,
    AnyNoNL,
    Lit(String),  // should be cow?
    Split(usize, usize),
    Jmp(usize),
    Save(usize),
    Save0(usize),
    Restore(usize),
    RepeatGr { lo: usize, hi: usize, next: usize, repeat: usize },
    RepeatNg { lo: usize, hi: usize, next: usize, repeat: usize },
    RepeatEpsilonGr { lo: usize, next: usize, repeat: usize, check: usize },
    RepeatEpsilonNg { lo: usize, next: usize, repeat: usize, check: usize },
    DoubleFail,
    GoBack(usize),
    Backref(usize),
    DelegateSized(Box<Regex>, usize),
    Delegate {
        inner: Box<Regex>,
        inner1: Option<Box<Regex>>,  // regex with 1-char look-behind
        start_group: usize,
        end_group: usize,
    },
}

#[derive(Debug)]
pub struct Prog {
    body: Vec<Insn>,
    n_saves: usize,
    max_stack: usize,
}

impl Prog {
    pub fn new(body: Vec<Insn>, n_saves: usize) -> Prog {
        Prog {
            body: body,
            n_saves: n_saves,
            max_stack: MAX_STACK,
        }
    }

    pub fn debug_print(&self) {
        for (i, insn) in self.body.iter().enumerate() {
            println!("{:3}: {:?}", i, insn);
        }
    }
}

struct State {
    pub saves: Vec<usize>,  // mostly indices to s, but can be repeat values etc

    // pc, index to string, nsave value
    pub stack: Vec<(usize, usize, usize)>,

    oldsave: Vec<(usize, usize)>,
    nsave: usize,
}

// Each element in the stack conceptually represents the entire state
// of the machine: the pc (index into prog), the index into the
// string, and the entire vector of saves. However, copying the save
// vector on every push/pop would be inefficient, so instead we use a
// copy-on-write approach for each slot within the save vector. The
// top `nsave` elements in `oldsave` represent the delta from the
// current machine state to the top of stack.

impl State {
    fn new(n_saves: usize) -> State {
        State {
            saves: vec![usize::MAX; n_saves],
            stack: Vec::new(),
            oldsave: Vec::new(),
            nsave: 0,
        }
    }

    fn push(&mut self, pc: usize, ix: usize, max_stack: usize) -> Result<()> {
        if self.stack.len() < max_stack {
            self.stack.push((pc, ix, self.nsave));
            self.nsave = 0;
            Ok(())
        } else {
            Err(Error::StackOverflow)
        }
    }

    fn pop(&mut self) -> (usize, usize) {
        for _ in 0..self.nsave {
            let (slot, val) = self.oldsave.pop().unwrap();
            self.saves[slot] = val;
        }
        let (pc, ix, nsave) = self.stack.pop().unwrap();
        self.nsave = nsave;
        (pc, ix)
    }

    fn save(&mut self, slot: usize, val: usize) {
        for i in 0..self.nsave {
            // could avoid this iteration with some overhead; worth it?
            if self.oldsave[self.oldsave.len() - i - 1].0 == slot {
                // already saved, just update
                self.saves[slot] = val;
                return;
            }
        }
        self.oldsave.push((slot, self.saves[slot]));
        self.nsave += 1;
        self.saves[slot] = val;
    }

    fn get(&self, slot: usize) -> usize {
        self.saves[slot]
    }
}

fn codepoint_len_at(s: &str, ix: usize) -> usize {
    codepoint_len(s.as_bytes()[ix])
}

pub fn run(prog: &Prog, s: &str, pos: usize, options: u32) ->
        Result<Option<Vec<usize>>> {
    let mut state = State::new(prog.n_saves);
    let mut pc = 0;
    let mut ix = pos;
    loop {
        // break from this loop to fail, causes stack to pop
        'fail: loop {
            if options & OPTION_TRACE != 0 {
                println!("{} {} {:?} {}",
                    state.stack.len(), pc, prog.body[pc], ix);
            }
            match prog.body[pc] {
                Insn::End => {
                    // save of end position into slot 1 is now done
                    // with an explicit group; we might want to
                    // optimize that.
                    //state.saves[1] = ix;
                    if options & OPTION_TRACE != 0 {
                        println!("{:?}", state.saves);
                    }
                    return Ok(Some(state.saves));
                }
                Insn::Any => {
                    if ix < s.len() {
                        ix += codepoint_len_at(s, ix)
                    } else {
                        break 'fail;
                    }
                }
                Insn::AnyNoNL => {
                    if ix < s.len() && s.as_bytes()[ix] != b'\n' {
                        ix += codepoint_len_at(s, ix)
                    } else {
                        break 'fail;
                    }
                }
                Insn::Lit(ref val) => {
                    let end = ix + val.len();
                    if end > s.len() || &s.as_bytes()[ix..end] != val.as_bytes() {
                        break 'fail;
                    }
                    ix = end;
                }
                Insn::Split(x, y) => {
                    try!(state.push(y, ix, prog.max_stack));
                    pc = x;
                    continue;
                }
                Insn::Jmp(target) => {
                    pc = target;
                    continue;
                }
                Insn::Save(slot) => state.save(slot, ix),
                Insn::Save0(slot) => state.save(slot, 0),
                Insn::Restore(slot) => ix = state.get(slot),
                Insn::RepeatGr { lo, hi, next, repeat } => {
                    let repcount = state.get(repeat);
                    if repcount == hi {
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        try!(state.push(next, ix, prog.max_stack));
                    }
                }
                Insn::RepeatNg { lo, hi, next, repeat } => {
                    let repcount = state.get(repeat);
                    if repcount == hi {
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        try!(state.push(pc + 1, ix, prog.max_stack));
                        pc = next;
                        continue;
                    }
                }
                Insn::RepeatEpsilonGr { lo, next, repeat, check } => {
                    let repcount = state.get(repeat);
                    if repcount > lo && state.get(check) == ix {
                        // prevent zero-length match on repeat
                        break 'fail;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.save(check, ix);
                        try!(state.push(next, ix, prog.max_stack));
                    }
                }
                Insn::RepeatEpsilonNg { lo, next, repeat, check } => {
                    let repcount = state.get(repeat);
                    if repcount > lo && state.get(check) == ix {
                        // prevent zero-length match on repeat
                        break 'fail;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.save(check, ix);
                        try!(state.push(pc + 1, ix, prog.max_stack));
                        pc = next;
                        continue;
                    }
                }
                Insn::GoBack(count) => {
                    for _ in 0..count {
                        if ix == 0 {
                            break 'fail;
                        }
                        ix = prev_codepoint_ix(s, ix);
                    }
                }
                Insn::DoubleFail => {
                    // negative lookaround
                    let _ = state.pop();
                    break 'fail;
                }
                Insn::Backref(slot) => {
                    let lo = state.get(slot);
                    let hi = state.get(slot + 1);
                    let ix_end = ix + (hi - lo);
                    if ix_end > s.len() || s[ix..ix_end] != s[lo..hi] {
                        break 'fail;
                    }
                    ix = ix_end;
                }
                Insn::DelegateSized(ref inner, size) => {
                    if inner.is_match(&s[ix..]) {
                        // We could analyze for ascii-only, and ix += size in
                        // that case. Unlikely to be speed-limiting though.
                        for _ in 0..size {
                            ix += codepoint_len_at(s, ix);
                        }
                    } else {
                        break 'fail;
                    }
                }
                Insn::Delegate { ref inner, ref inner1, start_group, end_group } => {
                    let re = match inner1 {
                        &Some(ref inner1) if ix > 0 => {
                            ix = prev_codepoint_ix(s, ix);
                            inner1
                        }
                        _ => inner,
                    };
                    if start_group == end_group {
                        match re.find(&s[ix..]) {
                            Some((_, end)) => ix += end,
                            _ => break 'fail
                        }
                    } else {
                        if let Some(caps) = re.captures(&s[ix..]) {
                            let mut slot = start_group * 2;
                            for i in 0..(end_group - start_group) {
                                if let Some((beg, end)) = caps.pos(i + 1) {
                                    state.save(slot, beg);
                                    state.save(slot + 1, end);
                                } else {
                                    state.save(slot, usize::MAX);
                                    state.save(slot + 1, usize::MAX);
                                }
                                slot += 2;
                            }
                            ix += caps.pos(0).unwrap().1;
                        } else {
                            break 'fail;
                        }
                    }
                }
            }
            pc += 1;
        }
        // "break 'fail" goes here
        if state.stack.is_empty() {
            return Ok(None);
        }
        let (newpc, newix) = state.pop();
        pc = newpc;
        ix = newix;
    }

}

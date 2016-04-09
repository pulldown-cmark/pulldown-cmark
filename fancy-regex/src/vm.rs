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

pub const OPTION_TRACE: u32 = 1;

#[derive(Debug)]
pub enum Insn {
    End,
    Any,
    Lit(String),  // should be cow?
    Split(usize, usize),
    Jmp(usize),
    Save(usize),
    Save0(usize),
    Restore(usize),
    RepeatGr{ lo: usize, hi: usize, next: usize, repeat: usize },
    RepeatNg{ lo: usize, hi: usize, next: usize, repeat: usize },
    RepeatEpsilonGr{ lo: usize, next: usize, repeat: usize, check: usize },
    RepeatEpsilonNg{ lo: usize, next: usize, repeat: usize, check: usize },
    DoubleFail,
    Backref(usize),
    DelegateSized(Box<Regex>, usize),
    Delegate(Box<Regex>),
}

#[derive(Debug)]
pub struct Prog {
    body: Vec<Insn>,
    n_saves: usize,
}

impl Prog {
    pub fn new(body: Vec<Insn>, n_saves: usize) -> Prog {
        Prog {
            body: body,
            n_saves: n_saves,
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

impl State {
    fn new(n_saves: usize) -> State {
        State {
            saves: vec![usize::MAX; n_saves],
            stack: Vec::new(),
            oldsave: Vec::new(),
            nsave: 0,
        }
    }

    fn push(&mut self, pc: usize, ix: usize) {
        // TODO: overflow check
        self.stack.push((pc, ix, self.nsave));
        self.nsave = 0;
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

pub fn run(prog: &Prog, s: &str, pos: usize, options: u32) -> bool {
    let mut state = State::new(prog.n_saves);
    state.saves[0] = pos;  // TODO: "find" semantics
    let mut pc = 0;
    let mut ix = pos;
    loop {
        // break from this loop to fail, causes stack to pop
        loop {
            if options & OPTION_TRACE != 0 {
                println!("{} {} {:?} {}",
                    state.stack.len(), pc, prog.body[pc], ix);
            }
            match prog.body[pc] {
                Insn::End => {
                    state.saves[1] = ix;
                    if options & OPTION_TRACE != 0 {
                        println!("{:?}", state.saves);
                    }
                    return true
                }
                Insn::Any => {
                    if ix < s.len() {
                        ix += codepoint_len_at(s, ix)
                    } else {
                        break;
                    }
                }
                Insn::Lit(ref val) => {
                    let end = ix + val.len();
                    if end > s.len() || &s[ix..end] != val {
                        break;
                    }
                    ix = end;
                }
                Insn::Split(x, y) => {
                    state.push(y, ix);
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
                Insn::RepeatGr{ lo, hi, next, repeat } => {
                    let repcount = state.get(repeat);
                    if repcount == hi {
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.push(next, ix);
                    }
                }
                Insn::RepeatNg{ lo, hi, next, repeat } => {
                    let repcount = state.get(repeat);
                    if repcount == hi {
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.push(pc + 1, ix);
                        pc = next;
                        continue;
                    }
                }
                Insn::RepeatEpsilonGr{ lo, next, repeat, check } => {
                    let repcount = state.get(repeat);
                    if repcount > lo && state.get(check) == ix {
                        // prevent zero-length match on repeat
                        break;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.save(check, ix);
                        state.push(next, ix);
                    }
                }
                Insn::RepeatEpsilonNg{ lo, next, repeat, check } => {
                    let repcount = state.get(repeat);
                    if repcount > lo && state.get(check) == ix {
                        // prevent zero-length match on repeat
                        break;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.save(check, ix);
                        state.push(pc + 1, ix);
                        pc = next;
                        continue;
                    }
                }
                Insn::DoubleFail => {
                    // negative lookaround
                    let _ = state.pop();
                    break;
                }
                Insn::Backref(slot) => {
                    let lo = state.get(slot);
                    let hi = state.get(slot + 1);
                    let ix_end = ix + (hi - lo);
                    if ix_end > s.len() || s[ix..ix_end] != s[lo..hi] {
                        break;
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
                        break;
                    }
                }
                Insn::Delegate(ref inner) => {
                    match inner.find(&s[ix..]) {
                        Some((_, end)) => ix += end,
                        _ => break
                    }
                }
            }
            pc += 1;
        }
        if state.stack.is_empty() {
            return false;
        }
        let (newpc, newix) = state.pop();
        pc = newpc;
        ix = newix;
    }
}

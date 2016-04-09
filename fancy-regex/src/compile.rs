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

//! Compilation of regexes to VM.

use std::usize;
use regex::Regex;

use Expr;
use LookAround::*;
use analyze::Analysis;
use vm::{Insn,Prog};

// I'm thinking it probably doesn't make a lot of sense having this split
// out from Compiler.
struct VMBuilder {
    prog: Vec<Insn>,
    n_saves: usize,
}

impl VMBuilder {
    fn new(max_group: usize) -> VMBuilder {
        VMBuilder {
            prog: Vec::new(),
            n_saves: max_group * 2,
        }
    }

    fn build(self) -> Prog {
        Prog::new(self.prog, self.n_saves)
    }

    fn newsave(&mut self) -> usize {
        let result = self.n_saves;
        self.n_saves += 1;
        result
    }

    fn pc(&self) -> usize {
        self.prog.len()
    }

    // would "emit" be a better name?
    fn add(&mut self, insn: Insn) {
        self.prog.push(insn);
    }

    fn set_jmp_target(&mut self, jmp_pc: usize, target: usize) {
        match self.prog[jmp_pc] {
            Insn::Jmp(ref mut next) => *next = target,
            _ => panic!("mutating instruction other than Jmp")
        }
    }

    fn set_split_target(&mut self, jmp_pc: usize, target: usize, second: bool) {
        match self.prog[jmp_pc] {
            Insn::Split(ref mut x, ref mut y) => {
                if second {
                    *y = target;
                } else {
                    *x = target;
                }
            }
            _ => panic!("mutating instruction other than Split")
        }
    }

    fn set_repeat_target(&mut self, jmp_pc: usize, target: usize) {
        match self.prog[jmp_pc] {
            Insn::RepeatGr { ref mut next, .. } => *next = target,
            Insn::RepeatNg { ref mut next, .. } => *next = target,
            Insn::RepeatEpsilonGr { ref mut next, .. } => *next = target,
            Insn::RepeatEpsilonNg { ref mut next, .. } => *next = target,
            _ => panic!("mutating instruction other than Repeat")
        }
    }
}

struct Compiler<'a> {
    a: &'a Analysis<'a>,
    b: VMBuilder,
}

impl<'a> Compiler<'a> {
    fn visit(&mut self, ix: usize, hard: bool) {
        let info = &self.a.infos[ix];
        if !hard && !info.hard {
            // easy case, delegate entire subexpr
            self.compile_delegates(&[ix]);
            return;
        }
        match *info.expr {
            Expr::Empty => (),
            Expr::Literal{ ref val } => {
                self.b.add(Insn::Lit(val.clone()));
            }
            Expr::Any => {
                self.b.add(Insn::Any);
            }
            Expr::Concat(_) => {
                self.compile_concat(ix, hard);
            }
            Expr::Alt(_) => {
                let mut jmps = Vec::new();
                let mut last_pc = usize::MAX;
                let mut ix = ix + 1;
                loop {
                    let next = self.a.infos[ix].next_sibling;
                    let pc = self.b.pc();
                    if next != 0 {
                        self.b.add(Insn::Split(pc + 1, usize::MAX));
                    }
                    if last_pc != usize::MAX {
                        self.b.set_split_target(last_pc, pc, true);
                    }
                    last_pc = pc;
                    self.visit(ix, hard);
                    if next != 0 {
                        let pc = self.b.pc();
                        jmps.push(pc);
                        self.b.add(Insn::Jmp(0));
                        ix = next;
                    } else {
                        break;
                    }
                }
                let pc = self.b.pc();
                for jmp_pc in jmps {
                    self.b.set_jmp_target(jmp_pc, pc);
                }
            }
            Expr::Group(_) => {
                let group = self.a.infos[ix].start_group;
                self.b.add(Insn::Save(group * 2));
                self.visit(ix + 1, hard);
                self.b.add(Insn::Save(group * 2 + 1));
            }
            Expr::Repeat { lo, hi, greedy, .. } => {
                self.compile_repeat(lo, hi, greedy, ix, hard);
            }
            Expr::LookAround(_, LookAhead) => {
                let save = self.b.newsave();
                self.b.add(Insn::Save(save));
                self.visit(ix + 1, false);
                self.b.add(Insn::Restore(save));
            }
            Expr::LookAround(_, LookAheadNeg) => {
                let pc = self.b.pc();
                self.b.add(Insn::Split(pc + 1, usize::MAX));
                self.visit(ix + 1, false);
                self.b.add(Insn::DoubleFail);
                let next_pc = self.b.pc();
                self.b.set_split_target(pc, next_pc, true);
            }
            Expr::Backref(group) => {
                self.b.add(Insn::Backref(group * 2));
            }
            Expr::Delegate { .. } => {
                self.compile_delegates(&[ix]);
            }
            _ => ()
        }
    }

    fn compile_concat(&mut self, ix: usize, hard: bool) {
        // collect children into vec, makes things easier to work with
        let mut children = Vec::new();
        let mut child_ix = ix + 1;
        loop {
            children.push(child_ix);
            child_ix = self.a.infos[child_ix].next_sibling;
            if child_ix == 0 {
                break;
            }
        }

        // First: determine a prefix which is constant size and not hard.
        let mut prefix_end = 0;
        for &child_ix in &children {
            let info = &self.a.infos[child_ix];
            if !info.const_size || info.hard {
                break;
            }
            prefix_end += 1;
        }

        // If incoming difficulty is not hard, the suffix after the last
        // hard child can be done with NFA.
        let mut suffix_begin = children.len();
        if !hard {
            for &child_ix in children[prefix_end..].iter().rev() {
                if self.a.infos[child_ix].hard {
                    break;
                }
                suffix_begin -= 1;
            }
        }

        self.compile_delegates(&children[..prefix_end]);

        for &child_ix in &children[prefix_end..suffix_begin] {
            self.visit(child_ix, true);
        }

        self.compile_delegates(&children[suffix_begin..]);
    }

    fn compile_repeat(&mut self, lo: usize, hi: usize, greedy: bool,
            ix: usize, hard: bool) {
        if lo == 0 && hi == 1 {
            // e?
            let pc = self.b.pc();
            self.b.add(Insn::Split(pc + 1, pc + 1));
            // TODO: do we want to do an epsilon check here? If we do
            // it here and in Alt, we might be able to make a good
            // bound on stack depth
            self.visit(ix + 1, hard);
            let next_pc = self.b.pc();
            self.b.set_split_target(pc, next_pc, greedy);
            return;
        }
        let hard = hard | self.a.infos[ix].hard;
        if hi == usize::MAX && self.a.infos[ix + 1].min_size == 0 {
            // Use RepeatEpsilon instructions to prevent empty repeat
            let repeat = self.b.newsave();
            let check = self.b.newsave();
            self.b.add(Insn::Save0(repeat));
            let pc = self.b.pc();
            if greedy {
                self.b.add(Insn::RepeatEpsilonGr{ lo: lo, next: usize::MAX,
                        repeat: repeat, check: check });
            } else {
                self.b.add(Insn::RepeatEpsilonNg{ lo: lo, next: usize::MAX,
                        repeat: repeat, check: check });
            }
            self.visit(ix + 1, hard);
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_repeat_target(pc, next_pc);
        } else if lo == 0 && hi == usize::MAX {
            // e*
            let pc = self.b.pc();
            self.b.add(Insn::Split(pc + 1, pc + 1));
            self.visit(ix + 1, hard);
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_split_target(pc, next_pc, greedy);
        } else if lo == 1 && hi == usize::MAX {
            // e+
            let pc = self.b.pc();
            self.visit(ix + 1, hard);
            let next = self.b.pc() + 1;
            let (x, y) = if greedy { (pc, next) } else { (next, pc) };
            self.b.add(Insn::Split(x, y));
        } else {
            let repeat = self.b.newsave();
            self.b.add(Insn::Save0(repeat));
            let pc = self.b.pc();
            if greedy {
                self.b.add(Insn::RepeatGr{ lo: lo, hi: hi, next: usize::MAX,
                        repeat: repeat });
            } else {
                self.b.add(Insn::RepeatNg{ lo: lo, hi: hi, next: usize::MAX,
                        repeat: repeat });
            }
            self.visit(ix + 1, hard);
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_repeat_target(pc, next_pc);
        }
    }

    fn compile_delegates(&mut self, ixs: &[usize]) {
        // TODO: plumb capture groups
        if ixs.is_empty() {
            return;
        }
        let mut annotated = String::new();
        annotated.push('^');
        let mut min_size = 0;
        let mut const_size = true;
        let mut looks_left = false;
        for &ix in ixs {
            let info = &self.a.infos[ix];
            let expr = info.expr;
            looks_left |= info.looks_left && min_size == 0;
            min_size += info.min_size;
            const_size &= info.const_size;
            expr.to_str(&mut annotated);
        }
        self.make_delegate(&annotated, min_size, const_size, looks_left);
    }

    fn make_delegate(&mut self, inner_re: &str,
            min_size: usize, const_size: bool, looks_left: bool) {
        match Regex::new(&inner_re) {
            Ok(compiled) => {
                if looks_left {
                    let mut inner1 = String::with_capacity(inner_re.len() + 5);
                    inner1.push_str("^.(?:");
                    inner1.push_str(&inner_re[1..]);
                    inner1.push(')');
                    match Regex::new(&inner1) {
                        Ok(compiled1) => {
                            self.b.add(Insn::Delegate(Box::new(compiled),
                                Some(Box::new(compiled1))));
                        }
                        Err(e) => panic!("inner compilation error {:?}", e)
                    }
                } else if const_size {
                    let size = min_size;
                    self.b.add(Insn::DelegateSized(Box::new(compiled), size));
                } else {
                    self.b.add(Insn::Delegate(Box::new(compiled), None));
                }
            }
            // TODO: bubble up Result
            Err(e) => panic!("inner compilation error {:?}", e)
        }
    }
}

// Don't need the expr because the analysis points to it
pub fn compile(analysis: &Analysis) -> Prog {
    let mut c = Compiler {
        a: analysis,
        b: VMBuilder::new(analysis.group_ix),
    };
    c.visit(0, false);
    c.b.add(Insn::End);
    c.b.build()
}


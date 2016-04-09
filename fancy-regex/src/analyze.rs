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

//! Analysis of regex expressions.

use bit_set::BitSet;
use std::cmp::min;
use std::usize;

use Expr;

#[derive(Debug)]
pub struct Info<'a> {
    pub expr: &'a Expr,
    pub next_sibling: usize,
    pub start_group: usize,
    pub end_group: usize,
    pub min_size: usize,
    pub const_size: bool,
    pub hard: bool,
}

#[derive(Debug)]
pub struct Analysis<'a> {
    pub infos: Vec<Info<'a>>,
    pub backrefs: &'a BitSet,
    pub group_ix: usize,
}

impl<'a> Analysis<'a> {
    pub fn analyze(expr: &'a Expr, backrefs: &'a BitSet) -> Analysis<'a> {
        let mut analysis = Analysis {
            infos: Vec::new(),
            backrefs: backrefs,
            group_ix: 1,
        };
        analysis.visit(expr);
        analysis
    }

    fn visit(&mut self, expr: &'a Expr) {
        let ix = self.infos.len();
        self.infos.push(Info {
            expr: expr,
            next_sibling: 0,
            start_group: self.group_ix,
            end_group: self.group_ix,
            min_size: 0,
            const_size: false,
            hard: false,
        });
        let mut min_size = 0;
        let mut const_size = false;
        let mut hard = false;
        match *expr {
            Expr::Empty => {
                const_size = true;
            }
            Expr::Any => {
                min_size = 1;
                const_size = true;
            }
            Expr::Literal{..} => {
                // right now each character in a literal gets its own node, that might change
                min_size = 1;
                const_size = true;
            }
            Expr::Concat(ref v) => {
                let mut last_ix = usize::MAX;
                const_size = true;
                for child in v {
                    let ix = self.infos.len();
                    self.visit(&child);
                    if last_ix != usize::MAX {
                        self.infos[last_ix].next_sibling = ix;
                    }
                    min_size += self.infos[ix].min_size;
                    const_size &= self.infos[ix].const_size;
                    hard |= self.infos[ix].hard;
                    last_ix = ix;
                }
            }
            Expr::Alt(ref v) => {
                let ix = self.infos.len();
                self.visit(&v[0]);
                min_size = self.infos[ix].min_size;
                const_size = self.infos[ix].const_size;
                hard = self.infos[ix].hard;
                let mut last_ix = ix;
                for child in &v[1..] {
                    let ix = self.infos.len();
                    self.visit(&child);
                    self.infos[last_ix].next_sibling = ix;
                    const_size &= self.infos[ix].const_size && min_size == self.infos[ix].min_size;
                    min_size = min(min_size, self.infos[ix].min_size);
                    hard |= self.infos[ix].hard;
                    last_ix = ix;
                }
            }
            Expr::Group(ref child) => {
                let group = self.group_ix;
                self.group_ix += 1;
                let ix = self.infos.len();
                self.visit(child);
                let child_info = &self.infos[ix];
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                hard = child_info.hard | self.backrefs.contains(group);
            }
            Expr::LookAround(ref child, _) => {
                self.visit(child);
                // min_size = 0
                const_size = true;
                hard = true;
            }
            Expr::Repeat { ref child, lo, hi, .. } => {
                let ix = self.infos.len();
                self.visit(child);
                let child_info = &self.infos[ix];
                min_size = child_info.min_size * lo;
                const_size = child_info.const_size && lo == hi;
                hard = child_info.hard;
            }
            Expr::Delegate { size, .. } => {
                // currently only used for empty and single-char matches
                min_size = size;
                const_size = true;
            }
            Expr::Backref(_) => {
                hard = true;
            }
        }
        self.infos[ix].end_group = self.group_ix;
        self.infos[ix].min_size = min_size;
        self.infos[ix].const_size = const_size;
        self.infos[ix].hard = hard;
    }
}

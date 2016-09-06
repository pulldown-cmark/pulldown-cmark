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
    pub looks_left: bool,
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
            group_ix: 0,
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
            looks_left: false,
        });
        let mut min_size = 0;
        let mut const_size = false;
        let mut hard = false;
        let mut looks_left = false;
        match *expr {
            Expr::Empty | Expr::EndText | Expr::EndLine => {
                const_size = true;
            }
            Expr::Any{..} => {
                min_size = 1;
                const_size = true;
            }
            Expr::Literal{ ref val, casei } => {
                // right now each character in a literal gets its own node, that might change
                min_size = 1;
                const_size = literal_const_size(val, casei);
            }
            Expr::StartText | Expr::StartLine => {
                const_size = true;
                looks_left = true;
            }
            Expr::Concat(ref v) => {
                let mut last_ix = usize::MAX;
                const_size = true;
                for child in v {
                    let ix = self.infos.len();
                    self.visit(child);
                    if last_ix != usize::MAX {
                        self.infos[last_ix].next_sibling = ix;
                    }
                    looks_left |= self.infos[ix].looks_left && min_size == 0;
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
                    self.visit(child);
                    self.infos[last_ix].next_sibling = ix;
                    const_size &= self.infos[ix].const_size && min_size == self.infos[ix].min_size;
                    min_size = min(min_size, self.infos[ix].min_size);
                    hard |= self.infos[ix].hard;
                    looks_left |= self.infos[ix].looks_left;
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
                looks_left = child_info.looks_left;
                hard = child_info.hard | self.backrefs.contains(group);
            }
            Expr::LookAround(ref child, _) => {
                self.visit(child);
                // min_size = 0
                const_size = true;
                hard = true;
                looks_left = self.infos[ix].looks_left;
            }
            Expr::Repeat { ref child, lo, hi, .. } => {
                let ix = self.infos.len();
                self.visit(child);
                let child_info = &self.infos[ix];
                min_size = child_info.min_size * lo;
                const_size = child_info.const_size && lo == hi;
                hard = child_info.hard;
                looks_left = child_info.looks_left;
            }
            Expr::Delegate { size, .. } => {
                // currently only used for empty and single-char matches
                min_size = size;
                const_size = true;
                looks_left = size == 0;  // TODO: conservative for \z
            }
            Expr::Backref(_) => {
                hard = true;
            }
        }
        self.infos[ix].end_group = self.group_ix;
        self.infos[ix].min_size = min_size;
        self.infos[ix].const_size = const_size;
        self.infos[ix].hard = hard;
        self.infos[ix].looks_left = looks_left;
    }

    pub fn n_groups(&self) -> usize {
        self.infos[0].end_group
    }

    pub fn is_literal(&self, ix: usize) -> bool {
        match *self.infos[ix].expr {
            Expr::Literal { casei, .. } => !casei,
            Expr::Concat(_) => {
                let mut child = ix + 1;
                loop {
                    if !self.is_literal(child) { return false; }
                    child = self.infos[child].next_sibling;
                    if child == 0 { break; }
                }
                true
            }
            _ => false
        }
    }

    pub fn push_literal(&self, ix: usize, buf: &mut String) {
        match *self.infos[ix].expr {
            // could be more paranoid about checking casei
            Expr::Literal { ref val, .. } => buf.push_str(val),
            Expr::Concat(_) => {
                let mut child = ix + 1;
                loop {
                    self.push_literal(child, buf);
                    child = self.infos[child].next_sibling;
                    if child == 0 { break; }
                }
            }
            _ => panic!("push_literal called on non-literal")
        }
    }
}

fn literal_const_size(_: &str, _: bool) -> bool {
    // Right now, regex doesn't do sophisticated case folding,
    // test below will fail when that changes, then we need to
    // do something fancier here.
    true
}

#[cfg(test)]
mod tests {
    use regex;
    use super::literal_const_size;

    #[test]
    fn case_folding_safe() {
        let re = regex::Regex::new("(?i:ß)").unwrap();
        if re.is_match("SS") {
            assert!(!literal_const_size("ß", true));
        }

        // Another tricky example, Armenian ECH YIWN
        let re = regex::Regex::new("(?i:\\x{0587})").unwrap();
        if re.is_match("\u{0565}\u{0582}") {
            assert!(!literal_const_size("\u{0587}", true));
        }
    }
}

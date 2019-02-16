// Copyright 2018 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

//! A Vec-based container for a tree structure.

pub const NIL: usize = !0;

#[derive(Debug)]
pub struct Node<T> {
    pub child: usize,
    pub next: usize,
    pub item: T,
}

/// A tree abstraction, intended for fast building as a preorder traversal.
pub struct Tree<T> {
    pub nodes: Vec<Node<T>>,
    pub spine: Vec<usize>, // indices of nodes on path to current node
    pub cur: usize,
}

impl<T> Tree<T> {
    pub fn new() -> Tree<T> {
        Tree {
            nodes: Vec::new(),
            spine: Vec::new(),
            cur: NIL,
        }
    }

    /// Append one item to the current position in the tree.
    pub fn append(&mut self, item: T) {
        let this = self.create_node(item);
        if self.cur != NIL {
            self.nodes[self.cur].next = this;
        } else if let Some(&parent) = self.spine.last() {
            self.nodes[parent].child = this;
        }
        self.cur = this;
    }

    /// Create an isolated node.
    pub fn create_node(&mut self, item: T) -> usize {
        let this = self.nodes.len();
        self.nodes.push(Node {
            child: NIL,
            next: NIL,
            item: item,
        });
        this
    }

    /// Push down one level, so that new items become children of the current node.
    pub fn push(&mut self) {
        self.spine.push(self.cur);
        self.cur = NIL;
    }

    /// Pop back up a level.
    pub fn pop(&mut self) {
        self.cur = self.spine.pop().unwrap();
    }

    /// Look at the parent node.
    pub fn peek_up(&self) -> Option<usize> {
        self.spine.last().cloned()
    }

    /// Look at grandparent node.
    pub fn peek_grandparent(&self) -> Option<usize> {
        if self.spine.len() >= 2 {
            Some(self.spine[self.spine.len() - 2].clone())
        } else {
            None
        }
    }
}

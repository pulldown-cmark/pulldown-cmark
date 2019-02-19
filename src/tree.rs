// Copyright 2018 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

//! A Vec-based container for a tree structure.

use std::num::NonZeroUsize;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TreeIndex {
    Nil,
    Valid(NonZeroUsize),
}

#[derive(Debug)]
pub struct Node<T> {
    pub child: TreeIndex,
    pub next: TreeIndex,
    pub item: T,
}

/// A tree abstraction, intended for fast building as a preorder traversal.
pub struct Tree<T> {
    nodes: Vec<Node<T>>,
    pub spine: Vec<NonZeroUsize>, // indices of nodes on path to current node
    pub cur: TreeIndex,
}

impl<T: Default> Tree<T> {
    // Indices start at one, so we place a dummy value at index zero.
    // The alternative would be subtracting one from every NonZeroUsize
    // every time we convert it to usize to index our nodes.
    pub fn new() -> Tree<T> {
        Tree {
            nodes: vec![Node {
                child: TreeIndex::Nil,
                next: TreeIndex::Nil,
                item: <T as Default>::default(),
            }],
            spine: Vec::new(),
            cur: TreeIndex::Nil,
        }
    }

    /// Append one item to the current position in the tree.
    pub fn append(&mut self, item: T) -> NonZeroUsize {
        let ix = self.create_node(item);
        let this = TreeIndex::Valid(ix);

        if let TreeIndex::Valid(ix) = self.cur {
            self[ix].next = this;
        } else if let Some(&parent) = self.spine.last() {
            self[parent].child = this;
        }
        self.cur = this;
        ix
    }

    /// Create an isolated node.
    pub fn create_node(&mut self, item: T) -> NonZeroUsize {
        let this = self.nodes.len();
        self.nodes.push(Node {
            child: TreeIndex::Nil,
            next: TreeIndex::Nil,
            item,
        });
        NonZeroUsize::new(this).unwrap()
    }

    /// Push down one level, so that new items become children of the current node.
    pub fn push(&mut self) {
        match self.cur {
            TreeIndex::Nil => panic!("Tried to push when cur was Nil!"),
            TreeIndex::Valid(ix) => self.spine.push(ix),
        }
        self.cur = TreeIndex::Nil;
    }

    /// Pop back up a level.
    pub fn pop(&mut self) {
        self.cur = TreeIndex::Valid(self.spine.pop().unwrap());
    }

    /// Look at the parent node.
    pub fn peek_up(&self) -> Option<NonZeroUsize> {
        self.spine.last().cloned()
    }

    /// Look at grandparent node.
    pub fn peek_grandparent(&self) -> Option<NonZeroUsize> {
        if self.spine.len() >= 2 {
            Some(self.spine[self.spine.len() - 2])
        } else {
            None
        }
    }

    /// Returns true when there are no nodes in the tree, false otherwise.
    pub fn is_empty(&self) -> bool {
        self.nodes.len() <= 1
    }
}

impl<T> std::ops::Index<NonZeroUsize> for Tree<T> {
    type Output = Node<T>;

    fn index(&self, ix: NonZeroUsize) -> &Self::Output {
        self.nodes.index(ix.get())
    }
}

impl<T> std::ops::IndexMut<NonZeroUsize> for Tree<T> {
    fn index_mut(&mut self, ix: NonZeroUsize) -> &mut Node<T> {
        self.nodes.index_mut(ix.get())
    }
}

impl<T> std::ops::Index<TreeIndex> for Tree<T> {
    type Output = Node<T>;

    fn index(&self, ix: TreeIndex) -> &Self::Output {
        match ix {
            TreeIndex::Nil => panic!("Indexing with Nil!"),
            TreeIndex::Valid(good) => self.nodes.index(good.get())
        }
    }
}

impl<T> std::ops::IndexMut<TreeIndex> for Tree<T> {
    fn index_mut(&mut self, ix: TreeIndex) -> &mut Node<T> {
        match ix {
            TreeIndex::Nil => panic!("Indexing with Nil!"),
            TreeIndex::Valid(good) => self.nodes.index_mut(good.get())
        }
    }
}
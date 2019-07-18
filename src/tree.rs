// Copyright 2018 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

//! A Vec-based container for a tree structure.

use std::num::NonZeroUsize;
use std::ops::{Add, Sub};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TreePointer {
    Nil,
    Valid(TreeIndex),
}

impl TreePointer {
    pub fn unwrap(self) -> TreeIndex {
        match self {
            TreePointer::Nil => panic!("Called unwrap on a Nil value"),
            TreePointer::Valid(ix) => ix,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, PartialOrd)]
pub struct TreeIndex(NonZeroUsize);

impl TreeIndex {
    fn new(i: usize) -> Self {
        TreeIndex(NonZeroUsize::new(i).unwrap())
    }

    pub fn get(self) -> usize {
        self.0.get()
    }
}

impl Add<usize> for TreeIndex {
    type Output = TreeIndex;

    fn add(self, rhs: usize) -> Self {
        let inner = self.0.get() + rhs;
        TreeIndex::new(inner)
    }
}

impl Sub<usize> for TreeIndex {
    type Output = TreeIndex;

    fn sub(self, rhs: usize) -> Self {
        let inner = self.0.get().checked_sub(rhs).unwrap();
        TreeIndex::new(inner)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Node<T> {
    pub child: TreePointer,
    pub next: TreePointer,
    pub item: T,
}

/// A tree abstraction, intended for fast building as a preorder traversal.
#[derive(Clone)]
pub struct Tree<T> {
    nodes: Vec<Node<T>>,
    spine: Vec<TreeIndex>, // indices of nodes on path to current node
    cur: TreePointer,
}

impl<T: Default> Tree<T> {
    // Indices start at one, so we place a dummy value at index zero.
    // The alternative would be subtracting one from every TreeIndex
    // every time we convert it to usize to index our nodes.
    pub fn with_capacity(cap: usize) -> Tree<T> {
        let mut nodes = Vec::with_capacity(cap);
        nodes.push(Node {
            child: TreePointer::Nil,
            next: TreePointer::Nil,
            item: <T as Default>::default(),
        });
        Tree {
            nodes,
            spine: Vec::new(),
            cur: TreePointer::Nil,
        }
    }

    /// Returns the index of the element currently in focus.
    pub fn cur(&self) -> TreePointer {
        self.cur
    }

    /// Append one item to the current position in the tree.
    pub fn append(&mut self, item: T) -> TreeIndex {
        let ix = self.create_node(item);
        let this = TreePointer::Valid(ix);

        if let TreePointer::Valid(ix) = self.cur {
            self[ix].next = this;
        } else if let Some(&parent) = self.spine.last() {
            self[parent].child = this;
        }
        self.cur = this;
        ix
    }

    /// Create an isolated node.
    pub fn create_node(&mut self, item: T) -> TreeIndex {
        let this = self.nodes.len();
        self.nodes.push(Node {
            child: TreePointer::Nil,
            next: TreePointer::Nil,
            item,
        });
        TreeIndex::new(this)
    }

    /// Push down one level, so that new items become children of the current node.
    /// The new focus index is returned.
    pub fn push(&mut self) -> TreeIndex {
        let cur_ix = self.cur.unwrap();
        self.spine.push(cur_ix);
        self.cur = self[cur_ix].child;
        cur_ix
    }

    /// Pop back up a level.
    pub fn pop(&mut self) -> Option<TreeIndex> {
        let ix = self.spine.pop()?;
        self.cur = TreePointer::Valid(ix);
        Some(ix)
    }

    /// Look at the parent node.
    pub fn peek_up(&self) -> Option<TreeIndex> {
        self.spine.last().cloned()
    }

    /// Look at grandparent node.
    pub fn peek_grandparent(&self) -> Option<TreeIndex> {
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

    /// Returns the length of the spine.
    pub fn spine_len(&self) -> usize {
        self.spine.len()
    }

    /// Resets the focus to the first node added to the tree, if it exists.
    pub fn reset(&mut self) {
        self.cur = if self.is_empty() {
            TreePointer::Nil
        } else {
            TreePointer::Valid(TreeIndex::new(1))
        };
        self.spine.truncate(0);
    }

    /// Walks the spine from a root node up to, but not including, the current node.
    pub fn walk_spine(&self) -> impl std::iter::DoubleEndedIterator<Item = &TreeIndex> {
        self.spine.iter()
    }

    /// Moves focus to the next sibling of the given node.
    pub fn next_sibling(&mut self, cur_ix: TreeIndex) -> TreePointer {
        self.cur = self[cur_ix].next;
        self.cur
    }
}

impl<T> std::fmt::Debug for Tree<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn debug_tree<T>(
            tree: &Tree<T>,
            cur: TreeIndex,
            indent: usize,
            f: &mut std::fmt::Formatter,
        ) -> std::fmt::Result
        where
            T: std::fmt::Debug,
        {
            for _ in 0..indent {
                write!(f, "  ")?;
            }
            writeln!(f, "{:?}", &tree[cur].item)?;
            if let TreePointer::Valid(child_ix) = tree[cur].child {
                debug_tree(tree, child_ix, indent + 1, f)?;
            }
            if let TreePointer::Valid(next_ix) = tree[cur].next {
                debug_tree(tree, next_ix, indent, f)?;
            }
            Ok(())
        }

        if self.nodes.len() > 1 {
            let cur = TreeIndex(NonZeroUsize::new(1).unwrap());
            debug_tree(self, cur, 0, f)
        } else {
            write!(f, "Empty tree")
        }
    }
}

impl<T> std::ops::Index<TreeIndex> for Tree<T> {
    type Output = Node<T>;

    fn index(&self, ix: TreeIndex) -> &Self::Output {
        self.nodes.index(ix.get())
    }
}

impl<T> std::ops::IndexMut<TreeIndex> for Tree<T> {
    fn index_mut(&mut self, ix: TreeIndex) -> &mut Node<T> {
        self.nodes.index_mut(ix.get())
    }
}

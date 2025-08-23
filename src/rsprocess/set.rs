use super::translator::IdType;
use std::collections::BTreeSet;
use std::hash::Hash;
use serde::{Deserialize, Serialize};

/// Basic set of entities.
#[derive(Clone, Debug, PartialOrd, Eq, Ord, Serialize, Deserialize)]
pub struct Set {
    pub identifiers: BTreeSet<IdType>,
}

impl<const N: usize> From<[IdType; N]> for Set {
    fn from(arr: [IdType; N]) -> Self {
	Set {
	    identifiers: BTreeSet::from(arr),
	}
    }
}

impl From<&[IdType]> for Set {
    fn from(arr: &[IdType]) -> Self {
	Set {
	    identifiers: BTreeSet::from_iter(arr.to_vec()),
	}
    }
}

impl From<Vec<IdType>> for Set {
    fn from(arr: Vec<IdType>) -> Self {
	Set {
	    identifiers: BTreeSet::from_iter(arr),
	}
    }
}

impl Set {
    pub const fn new() -> Self {
	Set {
	    identifiers: BTreeSet::new(),
	}
    }

    pub fn is_subset(&self, b: &Set) -> bool {
	self.identifiers.is_subset(&b.identifiers)
    }

    pub fn is_disjoint(&self, b: &Set) -> bool {
	self.identifiers.is_disjoint(&b.identifiers)
    }

    // returns the new set a \cup b
    pub fn union(&self, b: &Set) -> Set {
	let mut ret: Set = b.clone();
	ret.identifiers.extend(self.identifiers.iter());
	ret
    }

    pub fn union_option(&mut self, b: Option<&Set>) {
	if let Some(b) = b {
	    self.identifiers.extend(b.iter());
	}
    }

    /// returns the new set a \cap b
    pub fn intersection(&self, b: &Set) -> Set {
	// TODO maybe find more efficient way without copy/clone
	let res: BTreeSet<_> = b
	    .identifiers
	    .intersection(&self.identifiers)
	    .copied()
	    .collect();
	Set { identifiers: res }
    }

    /// returns the new set a ∖ b
    pub fn subtraction(&self, b: &Set) -> Set {
	// TODO maybe find more efficient way without copy/clone
	let res: BTreeSet<_> = self
	    .identifiers
	    .difference(&b.identifiers)
	    .copied()
	    .collect();
	Set { identifiers: res }
    }

    pub fn iter(&self) -> std::collections::btree_set::Iter<'_, IdType> {
	self.identifiers.iter()
    }

    pub fn len(&self) -> usize {
	self.identifiers.len()
    }

    pub fn insert(&mut self, el: IdType) -> bool {
	self.identifiers.insert(el)
    }

    pub fn push(&mut self, b: &Set) {
	self.identifiers.extend(b.iter())
    }

    pub fn is_empty(&self) -> bool {
	self.identifiers.is_empty()
    }

    /// Returns the prefix and the loop from a trace.
    pub fn split<'a>(
	&'a self,
	trace: &'a [Self]
    ) -> Option<(&'a [Self], &'a [Self])> {
	let position = trace.iter().rposition(|x| x == self);
	position.map(|pos| trace.split_at(pos))
    }
}

impl Default for Set {
    fn default() -> Self {
	Set::new()
    }
}

impl PartialEq for Set {
    fn eq(&self, other: &Self) -> bool {
	self.identifiers.eq(&other.identifiers)
    }
}

impl Hash for Set {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
	self.identifiers.hash(state)
    }
}

impl IntoIterator for Set {
    type Item = IdType;
    type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
	self.identifiers.into_iter()
    }
}

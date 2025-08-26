use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::hash::Hash;
use std::fmt;

use super::translator::{IdType, Translator, PrintableWithTranslator};


/// Basic trait for all Set implementations.
/// Implement IntoIterator for &Self to have .iter() (not required directly by
/// the trait).
pub trait BasicSet
where Self: Clone + Eq + Ord + Default + Serialize + IntoIterator
    + PrintableWithTranslator,
for<'de> Self: Deserialize<'de>
{
    fn is_subset(&self, other: &Self) -> bool;
    fn is_disjoint(&self, other: &Self) -> bool;
    fn union(&self, other: &Self) -> Self;
    fn push(&mut self, other: &Self);
    fn extend(&mut self, other: Option<&Self>);
    fn intersection(&self, other: &Self) -> Self;
    fn subtraction(&self, other: &Self) -> Self;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
}

pub trait ExtensionsSet {
    fn iter(
	&self
    ) -> <&Self as IntoIterator>::IntoIter
    where for<'b> &'b Self: IntoIterator;

    fn split<'a>(
	&'a self,
	trace: &'a [Self]
    ) -> Option<(&'a [Self], &'a [Self])>
    where Self: Sized;
}

/// Implementations for all sets.
impl<T: BasicSet> ExtensionsSet for T {
    fn iter(&self) -> <&T as IntoIterator>::IntoIter
    where for<'b> &'b T: IntoIterator {
	self.into_iter()
    }

    /// Returns the prefix and the loop from a trace.
    fn split<'a>(
	&'a self,
	trace: &'a [Self]
    ) -> Option<(&'a [Self], &'a [Self])> {
	let position = trace.iter().rposition(|x| x == self);
	position.map(|pos| trace.split_at(pos))
    }
}


// -----------------------------------------------------------------------------

/// Basic set of entities.
#[derive(Clone, Debug, Default, PartialOrd, Eq, Ord, Serialize, Deserialize)]
pub struct Set {
    pub identifiers: BTreeSet<IdType>,
}

impl BasicSet for Set {
    fn is_subset(&self, other: &Self) -> bool {
	self.identifiers.is_subset(&other.identifiers)
    }

    fn is_disjoint(&self, other: &Self) -> bool {
	self.identifiers.is_disjoint(&other.identifiers)
    }

    // returns the new set a \cup b
    fn union(&self, other: &Self) -> Self {
	let mut ret: Set = other.clone();
	ret.identifiers.extend(self.identifiers.iter());
	ret
    }

    fn push(&mut self, b: &Self) {
	self.identifiers.extend(b.iter())
    }

    fn extend(&mut self, other: Option<&Self>) {
	if let Some(other) = other {
	    self.identifiers.extend(other);
	}
    }

    /// returns the new set a \cap b
    fn intersection(&self, other: &Self) -> Self {
	// TODO maybe find more efficient way without copy/clone
	let res: BTreeSet<_> = other
	    .identifiers
	    .intersection(&self.identifiers)
	    .copied()
	    .collect();
	Set { identifiers: res }
    }

    /// returns the new set a ∖ b
    fn subtraction(&self, other: &Self) -> Self {
	// TODO maybe find more efficient way without copy/clone
	let res: BTreeSet<_> = self
	    .identifiers
	    .difference(&other.identifiers)
	    .copied()
	    .collect();
	Set { identifiers: res }
    }

    fn len(&self) -> usize {
	self.identifiers.len()
    }

    fn is_empty(&self) -> bool {
	self.identifiers.is_empty()
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

impl<'a> IntoIterator for &'a Set {
    type Item = &'a IdType;
    type IntoIter = std::collections::btree_set::Iter<'a, IdType>;

    fn into_iter(self) -> Self::IntoIter {
	self.identifiers.iter()
    }
}

impl PrintableWithTranslator for Set {
    fn print(
	&self,
	f: &mut fmt::Formatter,
	translator: &Translator,
    ) -> fmt::Result {
	write!(f, "{{")?;
	let mut it = self.iter().peekable();
	while let Some(el) = it.next() {
	    if it.peek().is_none() {
		write!(f,
		       "{}",
		       translator.decode(*el).unwrap_or("Missing".into()))?;
	    } else {
		write!(f,
		       "{}, ",
		       translator.decode(*el).unwrap_or("Missing".into()))?;
	    }
	}
	write!(f, "}}")
    }
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

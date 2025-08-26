use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, BTreeMap};
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
	let mut ret: Self = other.clone();
	ret.identifiers.extend(self.identifiers.iter());
	ret
    }

    fn push(&mut self, other: &Self) {
	self.identifiers.extend(other.iter())
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

// -----------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize,
	 Deserialize)]
pub enum IdState {
    Positive,
    Negative
}

impl std::fmt::Display for IdState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match self {
	    Self::Positive => write!(f, "+"),
	    Self::Negative => write!(f, "-")
	}
    }
}

#[derive(Clone, Debug, Default, PartialOrd, Eq, Ord, Serialize, Deserialize)]
pub struct PositiveSet {
    pub identifiers: BTreeMap<IdType, IdState>,
}

impl BasicSet for PositiveSet {
    fn is_subset(&self, other: &Self) -> bool {
	for (id, s) in self.iter() {
	    if let Some(s1) = other.identifiers.get(id) {
		if s1 != s {
		    return false
		}
	    } else {
		return false
	    }
	}
	true
    }

    fn is_disjoint(&self, other: &Self) -> bool {
	for (id, _s) in self.iter() {
	    if other.identifiers.contains_key(id) {
		return false
	    }
	}
	true
    }

    /// ☞ The operation cannot fail, so we prefer self elements to other,
    /// but if the reaction system is consistent there wont be any problem.
    fn union(&self, other: &Self) -> Self {
	let mut ret: Self = other.clone();
	ret.identifiers.extend(self.identifiers.iter());
	ret
    }

    fn push(&mut self, other: &Self) {
	self.identifiers.extend(other.iter())
    }

    fn extend(&mut self, other: Option<&Self>) {
	if let Some(other) = other {
	    self.identifiers.extend(other);
	}
    }

    /// ☞ only returns values that are shared among both, meaning if they have
    /// different state (positive, negative) they are considered different.
    fn intersection(&self, other: &Self) -> Self {
	let res: BTreeMap<_, _> = other
	    .identifiers
	    .iter()
	    .filter(|(id, s)| {
		if let Some(s1) = self.identifiers.get(id) && s1 == *s {
		    true
		} else {
		    false
		}
	    })
	    .map(|(id, s)| (*id, *s))
	    .collect();
	PositiveSet { identifiers: res }
    }

    /// ☞ returns a ∖ b, values that are shared but with different state are
    /// preserved in the subtraction.
    fn subtraction(&self, other: &Self) -> Self {
	let res: BTreeMap<_, _> = self
	    .identifiers
	    .iter()
	    .filter(|(id, s)| {
		if let Some(s1) = other.identifiers.get(id) && s1 == *s {
		    false
		} else {
		    true
		}
	    })
	    .map(|(id, s)| (*id, *s))
	    .collect();
	PositiveSet { identifiers: res }
    }

    fn len(&self) -> usize {
	self.identifiers.len()
    }

    fn is_empty(&self) -> bool {
	self.identifiers.is_empty()
    }
}

impl PrintableWithTranslator for PositiveSet {
    fn print(
	&self,
	f: &mut fmt::Formatter,
	translator: &Translator,
    ) -> fmt::Result {
	write!(f, "{{")?;
	let mut it = self.iter().peekable();
	while let Some((id, s)) = it.next() {
	    if it.peek().is_none() {
		write!(f,
		       "{}{}",
		       s,
		       translator.decode(*id).unwrap_or("Missing".into()))?;
	    } else {
		write!(f,
		       "{}{}, ",
		       s,
		       translator.decode(*id).unwrap_or("Missing".into()))?;
	    }
	}
	write!(f, "}}")
    }
}

impl PartialEq for PositiveSet {
    fn eq(&self, other: &Self) -> bool {
	self.identifiers.eq(&other.identifiers)
    }
}

impl IntoIterator for PositiveSet {
    type Item = (IdType, IdState);
    type IntoIter = std::collections::btree_map::IntoIter<IdType, IdState>;

    fn into_iter(self) -> Self::IntoIter {
	self.identifiers.into_iter()
    }
}

impl<'a> IntoIterator for &'a PositiveSet {
    type Item = (&'a IdType, &'a IdState);
    type IntoIter = std::collections::btree_map::Iter<'a, IdType, IdState>;

    fn into_iter(self) -> Self::IntoIter {
	self.identifiers.iter()
    }
}

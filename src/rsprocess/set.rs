use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, BTreeMap};
use std::hash::Hash;
use std::fmt;

use super::translator::{Formatter, Translator, PrintableWithTranslator};
use super::element::{IdType, PositiveType, IdState};

/// Basic trait for all Set implementations.
/// Implement IntoIterator for &Self to have .iter() (not required directly by
/// the trait).
pub trait BasicSet
where Self: Clone + Eq + Ord + Default + Serialize + IntoIterator
    + PrintableWithTranslator,
for<'de> Self: Deserialize<'de>
{
    type Element;

    fn is_subset(&self, other: &Self) -> bool;
    fn is_disjoint(&self, other: &Self) -> bool;
    fn union(&self, other: &Self) -> Self;
    fn push(&mut self, other: &Self);
    fn extend(&mut self, other: Option<&Self>);
    fn intersection(&self, other: &Self) -> Self;
    fn subtraction(&self, other: &Self) -> Self;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
    fn contains(&self, el: &Self::Element) -> bool;
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
    type Element = IdType;

    fn is_subset(&self, other: &Self) -> bool {
	self.identifiers.is_subset(&other.identifiers)
    }

    fn is_disjoint(&self, other: &Self) -> bool {
	self.identifiers.is_disjoint(&other.identifiers)
    }

    // returns the new set a \cup b
    fn union(&self, other: &Self) -> Self {
	self.iter().chain(other.iter()).cloned().collect::<Vec<_>>().into()
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

    fn contains(&self, el: &Self::Element) -> bool {
	self.identifiers.contains(el)
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
		write!(f, "{}", Formatter::from(translator, el))?;
	    } else {
		write!(f, "{}, ", Formatter::from(translator, el))?;
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

impl Set {
    /// Converts set to positive set. All elements with the same state.
    pub fn to_positive_set(&self, state: IdState) -> PositiveSet {
	PositiveSet { identifiers: self.iter().map(|x| (*x, state)).collect() }
    }

    /// Computes minimized prohibiting set from reactants and inhibitors.
    /// Computes the powerset of the union of all reactants ∪ inhibitors sets
    /// and checks for each element of that set if they are also in all other
    /// unions. Then minimizes the result.
    pub fn prohibiting_set(
	reactants: &[Set],
	inhibitors: &[Set],
    ) -> Result<Vec<PositiveSet>, String> {
	if reactants.len() != inhibitors.len() {
	    return Err(format!("Different length inputs supplied to create \
				prohibiting set. reactants: {:?}, \
				inhibitors: {:?}",
			       reactants, inhibitors))
	}
	if let Some((r, i)) =
	    reactants.iter()
	    .zip(inhibitors.iter())
	    .find(|(sr, si)| !sr.intersection(si).is_empty())
	{
	    return Err(format!("Element in both reactants and inhibitors when \
				creating prohibiting set. reactants: {:?}, \
				inhibitors: {:?}",
			       r, i))
	}

	let mut t = {
	    let union = reactants.iter()
		.zip(inhibitors.iter())
		.map(|(sr, si)| {
		    sr.to_positive_set(IdState::Negative)
			.union(&si.to_positive_set(IdState::Positive))
		})
		.collect::<Vec<_>>();
	    let union_union = union.iter()
		.fold(PositiveSet::default(), |acc, s| acc.union(s));
	    let mut t = union_union.powerset();
	    for set in union.iter() {
		t.retain(|el| !el.intersection(set).is_empty());
	    }
	    t
	};

	// minimization
	// remove sets that contain other sets
	{
	    let mut tmp_t = t.clone().into_iter();
	    let mut e = tmp_t.next().unwrap_or_default();
	    loop {
		let mut modified = false;
		t.retain(|set| {
		    if *set == e {
			true
		    } else if e.is_subset(set) {
			modified = true;
			false
		    } else {
			true
		    }
		});
		if !modified {
		    e = {
			match tmp_t.next() {
			    Some(a) => a,
			    None => break,
			}
		    };
		}
	    }
	}

	// replace pair of sets that have a common negative-positive element
	// with set without
	// cannot happen, caught by error "Element in both ..." above

	// for e in t.clone() {
	//     let mut removed_e = false;
	//     let mut position = 0;
	//     let mut removed_elements = vec![];
	//     t.retain(|set| {
	//	if set == &e {
	//	    position += 1;
	//	    true
	//	} else if removed_e {
	//	    true
	//	} else if let elements = set.opposite_intersection(&e)
	//	    && !elements.is_empty()
	//	{
	//	    removed_e = true;
	//	    removed_elements.extend(elements);
	//	    false
	//	} else {
	//	    position += 1;
	//	    true
	//	}
	//     });
	//     if removed_e {
	//	let mut set = t.get(position).unwrap().clone();
	//	set = set.subtraction(&Set::from(removed_elements.clone())
	//			      .to_positive_set(IdState::Positive));
	//	set = set.subtraction(&Set::from(removed_elements)
	//			      .to_positive_set(IdState::Negative));
	//	t.remove(position);
	//	t.push(set);
	//     }
	// }

	Ok(t)
    }
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialOrd, Eq, Ord, Serialize, Deserialize)]
pub struct PositiveSet {
    pub identifiers: BTreeMap<IdType, IdState>,
}

impl BasicSet for PositiveSet {
    type Element = PositiveType;

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
	self.iter().chain(other.iter()).map(|(a, b)| (*a, *b))
	    .collect::<Vec<_>>().into()
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

    fn contains(&self, el: &Self::Element) -> bool {
	if let Some(e) = self.identifiers.get(&el.id) && *e == el.state {
	    true
	} else {
	    false
	}
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
		       "{}",
		       Formatter::from(translator,
				       &PositiveType { id: *id, state: *s })
		)?;
	    } else {
		write!(f,
		       "{}, ",
		       Formatter::from(translator,
				       &PositiveType { id: *id, state: *s })
		)?;
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

impl Hash for PositiveSet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
	self.identifiers.hash(state)
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


impl<const N: usize> From<[(IdType, IdState); N]> for PositiveSet {
    fn from(arr: [(IdType, IdState); N]) -> Self {
	PositiveSet {
	    identifiers: BTreeMap::from(arr),
	}
    }
}

impl From<&[(IdType, IdState)]> for PositiveSet {
    fn from(arr: &[(IdType, IdState)]) -> Self {
	PositiveSet {
	    identifiers: BTreeMap::from_iter(arr.to_vec()),
	}
    }
}

impl From<Vec<(IdType, IdState)>> for PositiveSet {
    fn from(arr: Vec<(IdType, IdState)>) -> Self {
	PositiveSet {
	    identifiers: BTreeMap::from_iter(arr),
	}
    }
}

impl PositiveSet {
    pub fn powerset(&self) -> Vec<Self> {
	self.into_iter().fold({
	    let mut asd = Vec::with_capacity(2_usize.pow(self.len() as u32));
	    asd.push(vec![]);
	    asd
	}, |mut p, x| {
	    let i = p.clone().into_iter()
		.map(|mut s| {
		    s.push(x);
		    s
		});
	    p.extend(i);
	    p
	}).into_iter()
	    .map(|x| Self::from(x.into_iter()
				.map(|(el, s)| (*el, *s))
				.collect::<Vec<_>>()))
	    .collect::<Vec<_>>()
    }

    pub fn opposite_intersection(&self, other: &Self) -> Vec<IdType> {
	let mut ret = vec![];
	for (el, state) in self {
	    if let Some(state2) = other.identifiers.get(el) && *state == !*state2 {
		ret.push(*el);
	    }
	}
	ret
    }
}

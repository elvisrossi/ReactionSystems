use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::hash::Hash;

use serde::{Deserialize, Serialize};

use super::element::{IdState, IdType, PositiveType};
use super::translator::{Formatter, PrintableWithTranslator, Translator};

/// Basic trait for all Set implementations.
/// Implement IntoIterator for &Self to have .iter() (not required directly by
/// the trait).
pub trait BasicSet
where
    Self: Clone
        + Eq
        + Ord
        + Default
        + Serialize
        + IntoIterator
        + PrintableWithTranslator,
    for<'de> Self: Deserialize<'de>,
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
    fn add(&mut self, el: Self::Element);
}

pub trait ExtensionsSet {
    fn iter(&self) -> <&Self as IntoIterator>::IntoIter
    where
        for<'b> &'b Self: IntoIterator;

    fn split<'a>(
        &'a self,
        trace: &'a [Self],
    ) -> Option<(&'a [Self], &'a [Self])>
    where
        Self: Sized;
}

/// Implementations for all sets.
impl<T: BasicSet> ExtensionsSet for T {
    fn iter(&self) -> <&T as IntoIterator>::IntoIter
    where
        for<'b> &'b T: IntoIterator,
    {
        self.into_iter()
    }

    /// Returns the prefix and the loop from a trace.
    fn split<'a>(
        &'a self,
        trace: &'a [Self],
    ) -> Option<(&'a [Self], &'a [Self])> {
        let position = trace.iter().rposition(|x| x == self);
        position.map(|pos| trace.split_at(pos))
    }
}

// -----------------------------------------------------------------------------

/// Basic set of entities.
#[derive(
    Clone, Debug, Default, PartialOrd, Eq, Ord, Serialize, Deserialize,
)]
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
        self.iter()
            .chain(other.iter())
            .cloned()
            .collect::<Vec<_>>()
            .into()
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

    fn add(&mut self, el: Self::Element) {
        self.identifiers.insert(el);
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
        PositiveSet {
            identifiers: self.iter().map(|x| (*x, state)).collect(),
        }
    }

    /// Computes minimized prohibiting set from reactants and inhibitors.
    /// Computes the powerset of the union of all reactants ∪ inhibitors sets
    /// and checks for each element of that set if they are also in all other
    /// unions. Then minimizes the result.
    pub fn prohibiting_set(
        reactants: &[Set],
        inhibitors: &[Set],
    ) -> Result<Vec<PositiveSet>, String> {
        if reactants.is_empty() && inhibitors.is_empty() {
            return Ok(vec![]);
        }
        if reactants.len() != inhibitors.len() {
            return Err(format!(
                "Different length inputs supplied to create \
				prohibiting set. reactants: {:?}, \
				inhibitors: {:?}",
                reactants, inhibitors
            ));
        }
        if let Some((r, i)) = reactants
            .iter()
            .zip(inhibitors.iter())
            .find(|(sr, si)| !sr.intersection(si).is_empty())
        {
            return Err(format!(
                "Element in both reactants and inhibitors when \
				creating prohibiting set. reactants: {:?}, \
				inhibitors: {:?}",
                r, i
            ));
        }
        // if we encounter a reaction with no reactants or inhibitors what do we
        // do? do we report an error or remove the two sets? here we have
        // choosen return an error.
        if let Some((r, i)) = reactants
            .iter()
            .zip(inhibitors.iter())
            .find(|(sr, si)| sr.is_empty() && si.is_empty())
        {
            return Err(format!(
                "Reaction with no reactants and no inhibitors: \
                 reactants: {:?}, inhibitors: {:?}",
                r, i
            ));
        }
        // code to instead remove the offending sets:
        // let (reactants, inhibitors): (Vec<_>, Vec<_>) =
        //     reactants.iter()
        //     .zip(inhibitors.iter())
        //     .filter(|(sr, si)| !sr.is_empty() || !si.is_empty())
        //     .unzip();

        // generate all valid combinations, keeping track of invalid ones (where
        // one simbol is both positive and negative)
        let mut t = {
            let unions = reactants
                .iter()
                .zip(inhibitors.iter())
                .map(|(sr, si)| {
                    sr.iter()
                        .map(|&id| PositiveType {
                            id,
                            state: IdState::Negative,
                        })
                        .chain(si.iter().map(|&id| PositiveType {
                            id,
                            state: IdState::Positive,
                        }))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();

            let mut state = vec![0_usize; unions.len()];
            let mut t = vec![];

            loop {
                let mut new_combination = unions
                    .iter()
                    .zip(state.iter())
                    .map(|(els, pos)| els[*pos])
                    .collect::<Vec<_>>();
                new_combination.sort_by(|a, b| {
                    a.id.cmp(&b.id).then(a.state.cmp(&b.state))
                });

                let mut error = false;

                'external: for i in 0..new_combination.len() - 1 {
                    let mut j = i + 1;
                    loop {
                        if new_combination[i].id != new_combination[j].id {
                            break;
                        } else if new_combination[i].id == new_combination[j].id
                            && new_combination[i].state
                                != new_combination[j].state
                        {
                            error = true;
                            break 'external;
                        } else {
                            j += 1;
                            if j >= new_combination.len() {
                                break;
                            }
                        }
                    }
                }

                if !error {
                    t.push(PositiveSet::from(new_combination));
                }

                let next = unions
                    .iter()
                    .zip(state.iter())
                    .enumerate()
                    .rfind(|(_, (els, pos))| **pos < els.len() - 1);
                match next {
                    | None => break,
                    | Some((pos, _)) => {
                        state[pos] += 1;
                        state.iter_mut().skip(pos + 1).for_each(|el| *el = 0);
                    },
                }
            }

            t
        };

        t.sort();
        t.dedup();

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
                            | Some(a) => a,
                            | None => break,
                        }
                    };
                }
            }
        }

        // replace pair of sets that have a common negative-positive element
        // with set without
        let mut removed = 0;

        for (pos_set1, set1) in t.clone().iter_mut().enumerate() {
            // we find another set that has at least one opposite element in
            // common
            if let Some((pos_set2, set2)) = t
                .iter()
                .enumerate()
                .find(|(_, set2)| set1.equal_except_negated_elements(set2))
            {
                let intersection = set1.opposite_intersection(set2);
                if intersection.len() != 1 {
                    continue;
                }
                set1.remove_elements(intersection);

                t[pos_set1 - removed] = set1.clone();
                t.remove(pos_set2);
                removed += 1;
            }
        }

        Ok(t)
    }
}

// -----------------------------------------------------------------------------

#[derive(
    Clone, Debug, Default, PartialOrd, Eq, Ord, Serialize, Deserialize,
)]
pub struct PositiveSet {
    pub identifiers: BTreeMap<IdType, IdState>,
}

impl BasicSet for PositiveSet {
    type Element = PositiveType;

    fn is_subset(&self, other: &Self) -> bool {
        for (id, s) in self.iter() {
            if let Some(s1) = other.identifiers.get(id) {
                if s1 != s {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }

    fn is_disjoint(&self, other: &Self) -> bool {
        for (id, _s) in self.iter() {
            if other.identifiers.contains_key(id) {
                return false;
            }
        }
        true
    }

    /// ☞ The operation cannot fail, so we prefer self elements to other,
    /// but if the reaction system is consistent there wont be any problem.
    fn union(&self, other: &Self) -> Self {
        self.iter()
            .chain(other.iter())
            .map(|(a, b)| (*a, *b))
            .collect::<Vec<_>>()
            .into()
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
                if let Some(s1) = self.identifiers.get(id) {
                    s1 == *s
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
                if let Some(s1) = other.identifiers.get(id) {
                    s1 == *s
                } else {
                    false
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
        if let Some(e) = self.identifiers.get(&el.id) {
            *e == el.state
        } else {
            false
        }
    }

    fn add(&mut self, el: Self::Element) {
        self.identifiers.insert(el.id, el.state);
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
                write!(
                    f,
                    "{}",
                    Formatter::from(translator, &PositiveType {
                        id:    *id,
                        state: *s,
                    })
                )?;
            } else {
                write!(
                    f,
                    "{}, ",
                    Formatter::from(translator, &PositiveType {
                        id:    *id,
                        state: *s,
                    })
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

impl<const N: usize> From<[PositiveType; N]> for PositiveSet {
    fn from(arr: [PositiveType; N]) -> Self {
        arr.into_iter()
            .map(|el| (el.id, el.state))
            .collect::<Vec<_>>()
            .into()
    }
}

impl From<&[PositiveType]> for PositiveSet {
    fn from(arr: &[PositiveType]) -> Self {
        arr.iter()
            .map(|el| (el.id, el.state))
            .collect::<Vec<_>>()
            .into()
    }
}

impl From<Vec<PositiveType>> for PositiveSet {
    fn from(arr: Vec<PositiveType>) -> Self {
        arr.into_iter()
            .map(|el| (el.id, el.state))
            .collect::<Vec<_>>()
            .into()
    }
}

impl FromIterator<PositiveType> for PositiveSet {
    fn from_iter<T: IntoIterator<Item = PositiveType>>(iter: T) -> Self {
        Self {
            identifiers: iter.into_iter().map(|el| (el.id, el.state)).collect(),
        }
    }
}

impl FromIterator<(IdType, IdState)> for PositiveSet {
    fn from_iter<T: IntoIterator<Item = (IdType, IdState)>>(iter: T) -> Self {
        Self {
            identifiers: iter.into_iter().collect(),
        }
    }
}

impl PositiveSet {
    /// Returns the list of elements that are in both set with opposite state.
    /// Example: [+1, +2, -3] ⩀ [-1, +2, +3] = [1, 3]
    pub fn opposite_intersection(&self, other: &Self) -> Vec<IdType> {
        let mut ret = vec![];
        for (el, state) in self {
            #[allow(clippy::collapsible_if)]
            if let Some(state2) = other.identifiers.get(el) {
                if *state == !*state2 {
                    ret.push(*el);
                }
            }
        }
        ret
    }

    /// Returns all elements that are present in self and are positive in other.
    pub fn mask(&self, other: &Self) -> Self {
        Self::from_iter(
            self.iter()
                .filter(|el| {
                    other.contains(&PositiveType::from((
                        *el.0,
                        IdState::Positive,
                    )))
                })
                .map(|el| (*el.0, *el.1)),
        )
    }

    pub fn inverted_mask(&self, other: &Self) -> Self {
        Self::from_iter(
            self.iter()
                .filter(|el| {
                    !other.contains(&PositiveType::from((
                        *el.0,
                        IdState::Positive,
                    )))
                })
                .map(|el| (*el.0, *el.1)),
        )
    }

    fn remove_elements(&mut self, other: Vec<IdType>) {
        for element in other {
            self.identifiers.remove(&element);
        }
    }

    fn equal_except_negated_elements(&self, other: &Self) -> bool {
        let mut intersection = self.opposite_intersection(other);
        intersection.sort();
        let mut self_copy = self.identifiers.clone();
        for el in other {
            if intersection.binary_search(el.0).is_err()
                || self_copy.get(el.0) != Some(el.1)
            {
                return false;
            }
            self_copy.remove(el.0);
        }
        self_copy.is_empty()
    }

    /// Returns only the positive entities.
    pub fn positives(&self) -> Self {
        self.iter()
            .filter(|el| *el.1 == IdState::Positive)
            .map(|el| (*el.0, *el.1))
            .collect::<PositiveSet>()
    }

    /// Returns only the negative entities.
    pub fn negatives(&self) -> Self {
        self.iter()
            .filter(|el| *el.1 == IdState::Negative)
            .map(|el| (*el.0, *el.1))
            .collect::<PositiveSet>()
    }

    /// Adds only the elements not in self that are in other.
    pub fn add_unique(&self, other: &Self) -> Self {
        other
            .iter()
            .filter(|e| !self.contains(&PositiveType::from((*e.0, *e.1))))
            .map(|el| (*el.0, *el.1))
            .collect::<PositiveSet>()
            .union(self)
    }

    pub fn elements(&self) -> Set {
        self.iter().map(|el| *el.0).collect::<Vec<_>>().into()
    }

    pub fn has_positives(&self, state: &Self) -> bool {
        self.iter().all(|a| {
            if *a.1 == IdState::Positive {
                state.contains(&PositiveType::from(a))
            } else {
                !state.contains(&PositiveType::from((*a.0, IdState::Positive)))
            }
        })
    }

    fn has_element(&self, el: &PositiveType) -> bool {
        self.identifiers.contains_key(&el.id)
    }

    pub fn push_unique(&self, other: &Self) -> Self {
        self.union(
            &other
                .iter()
                .filter(|el| !self.has_element(&PositiveType::from(*el)))
                .map(|el| (*el.0, *el.1))
                .collect::<Vec<_>>()
                .into(),
        )
    }
}

//! Definitions for the 'classical' mechanism for computation.
//!
//! Allows to define the 'classical' mechanism to compute in a Reaction System
//! (RS) Framework.

use serde::{Deserialize, Serialize};
use std::hash::Hash;

use super::element::{IdState, IdType};
use super::set::{BasicSet, ExtensionsSet, PositiveSet, Set};
use super::translator::{Formatter, PrintableWithTranslator, Translator};

pub trait BasicReaction: Clone + Default + Eq + Hash + Serialize + PrintableWithTranslator
where
    for<'de> Self: Deserialize<'de>,
{
    type Set: BasicSet;
    fn enabled(&self, state: &Self::Set) -> bool;
    fn compute_step(&self, state: &Self::Set) -> Option<&Self::Set>;
}

pub trait ExtensionReaction: Sized {
    type Set: BasicSet;

    fn compute_all(reactions: &[Self], state: &Self::Set) -> Self::Set;

    fn find_loop(
        reactions: &[Self],
        entities: Self::Set,
        q: &Self::Set,
    ) -> (Vec<Self::Set>, Vec<Self::Set>);

    fn find_only_loop(reactions: &[Self], entities: &Self::Set, q: &Self::Set) -> Vec<Self::Set>;

    fn find_prefix_len_loop(
        reactions: &[Self],
        entities: Self::Set,
        q: &Self::Set,
    ) -> (usize, Vec<Self::Set>);

    fn lollipops_only_loop_decomposed_q(
        reactions: &[Self],
        entities: &Self::Set,
        q: &Self::Set,
    ) -> Vec<Self::Set>;
}

/// Implementations for all reactions.
impl<T: BasicReaction<Set = Set>, Set: BasicSet> ExtensionReaction for T {
    type Set = Set;

    /// Computes the result of a series of reactions. Returns the union of all
    /// products.
    /// see result
    fn compute_all(reactions: &[Self], state: &Set) -> Set
    where
        Self: Sized,
    {
        reactions.iter().fold(Set::default(), |mut acc: Set, r| {
            acc.extend(r.compute_step(state));
            acc
        })
    }

    /// Finds the loops by simulating the system.
    fn find_loop(reactions: &[Self], entities: Set, q: &Set) -> (Vec<Set>, Vec<Set>) {
        let mut entities = entities;
        let mut trace = vec![];
        loop {
            if let Some((prefix, hoop)) = entities.split(&trace) {
                return (prefix.to_vec(), hoop.to_vec());
            } else {
                let t = entities.union(q);
                let products = Self::compute_all(reactions, &t);
                trace.push(entities.clone());
                entities = products;
            }
        }
    }

    /// Finds the loops by simulating the system.
    fn find_only_loop(reactions: &[Self], entities: &Set, q: &Set) -> Vec<Set> {
        let mut entities = entities.clone();
        let mut trace = vec![];
        loop {
            if let Some((_prefix, hoop)) = entities.split(&trace) {
                return hoop.to_vec();
            } else {
                let t = entities.union(q);
                let products = Self::compute_all(reactions, &t);
                trace.push(entities.clone());
                entities = products;
            }
        }
    }

    /// Finds the loops and the length of the prefix by simulating the system.
    fn find_prefix_len_loop(reactions: &[Self], entities: Set, q: &Set) -> (usize, Vec<Set>) {
        let mut entities = entities;
        let mut trace = vec![];
        loop {
            if let Some((prefix, hoop)) = entities.split(&trace) {
                return (prefix.len(), hoop.to_vec());
            } else {
                let t = entities.union(q);
                let products = Self::compute_all(reactions, &t);
                trace.push(entities.clone());
                entities = products;
            }
        }
    }

    /// see loop/5
    fn lollipops_only_loop_decomposed_q(reactions: &[Self], entities: &Set, q: &Set) -> Vec<Set> {
        let find_loop_fn = |q| Self::find_only_loop(reactions, entities, q);
        find_loop_fn(q)
    }
}

// -----------------------------------------------------------------------------

/// Basic structure for a reaction.
#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Reaction {
    pub reactants: Set,
    pub inhibitors: Set,
    pub products: Set,
}

impl BasicReaction for Reaction {
    type Set = Set;

    /// returns true if ```current_state``` enables the reaction
    /// see enable
    fn enabled(&self, current_state: &Self::Set) -> bool {
        self.reactants.is_subset(current_state) && self.inhibitors.is_disjoint(current_state)
    }

    /// Computes the result of a single reaction (if enabled returns the
    /// products) otherwise returns None.
    /// see result
    fn compute_step(&self, state: &Self::Set) -> Option<&Self::Set> {
        if self.enabled(state) {
            Some(&self.products)
        } else {
            None
        }
    }
}

impl PrintableWithTranslator for Reaction {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator) -> std::fmt::Result {
        write!(
            f,
            "(r: {}, i: {}, p: {})",
            Formatter::from(translator, &self.reactants),
            Formatter::from(translator, &self.inhibitors),
            Formatter::from(translator, &self.products)
        )
    }
}

impl Reaction {
    pub fn from(reactants: Set, inhibitors: Set, products: Set) -> Self {
        Reaction {
            reactants,
            inhibitors,
            products,
        }
    }

    pub fn all_products(reactions: &[Self]) -> Set {
        reactions
            .iter()
            .fold(Set::default(), |acc, r| acc.union(&r.products))
    }

    pub fn all_reactions_with_product<'a>(reactions: &'a [Self], el: &IdType) -> Vec<&'a Self> {
        reactions.iter().fold(vec![], |mut acc, r| {
            if r.products.contains(el) {
                acc.push(r);
            }
            acc
        })
    }
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PositiveReaction {
    pub reactants: PositiveSet,
    pub products: PositiveSet,
}

impl BasicReaction for PositiveReaction {
    type Set = PositiveSet;

    fn enabled(&self, state: &Self::Set) -> bool {
        self.reactants.is_subset(state)
    }

    fn compute_step(&self, state: &Self::Set) -> Option<&Self::Set> {
        if self.enabled(state) {
            Some(&self.products)
        } else {
            None
        }
    }
}

impl PrintableWithTranslator for PositiveReaction {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator) -> std::fmt::Result {
        write!(
            f,
            "(r: {}, p: {})",
            Formatter::from(translator, &self.reactants),
            Formatter::from(translator, &self.products),
        )
    }
}

impl PositiveReaction {
    pub fn from(reactants: PositiveSet, products: PositiveSet) -> Self {
        Self {
            reactants,
            products,
        }
    }

    pub fn create(reactants: Set, inhibitors: Set, products: Set) -> Self {
        Self {
            reactants: reactants
                .to_positive_set(IdState::Positive)
                .union(&inhibitors.to_positive_set(IdState::Negative)),
            products: products.to_positive_set(IdState::Positive),
        }
    }
}

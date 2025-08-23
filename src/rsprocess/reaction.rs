//! Definitions for the 'classical' mechanism for computation.
//!
//! Allows to define the 'classical' mechanism to compute in a Reaction System
//! (RS) Framework.

use std::hash::Hash;
use serde::{Deserialize, Serialize};
use super::set::Set;

/// Basic structure for a reaction.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Reaction {
    pub reactants: Set,
    pub inhibitors: Set,
    pub products: Set,
}

impl Reaction {
    pub fn new() -> Self {
	Reaction {
	    reactants: Set::new(),
	    inhibitors: Set::new(),
	    products: Set::new(),
	}
    }

    pub fn from(reactants: Set, inhibitors: Set, products: Set) -> Self {
	Reaction {
	    reactants,
	    inhibitors,
	    products,
	}
    }

    /// returns true if ```current_state``` enables the reaction
    /// see enable
    pub fn enabled(&self, current_state: &Set) -> bool {
	self.reactants.is_subset(current_state)
	    && self.inhibitors.is_disjoint(current_state)
    }

    /// Computes the result of a single reaction (if enabled returns the products)
    /// otherwise returns None.
    /// see result
    pub fn compute_step<'a>(
	&'a self,
	current_state: &'a Set,
    ) -> Option<&'a Set> {
	if self.enabled(current_state) {
	    Some(&self.products)
	} else {
	    None
	}
    }

    /// Computes the result of a series of reactions. Returns the union of all
    /// products.
    /// see result
    pub fn compute_all<'a>(
	current_state: &'a Set,
	reactions: &'a [Self]
    ) -> Set {
	reactions.iter().fold(Set::new(), |mut acc, r| {
	    acc.union_option(r.compute_step(current_state));
	    acc
	})
    }


    /// Finds the loops by simulating the system.
    pub fn find_loop(
	rs: &[Self],
	entities: Set,
	q: &Set
    ) -> (Vec<Set>, Vec<Set>) {
	let mut entities = entities;
	let mut trace = vec![];
	loop {
	    if let Some((prefix, hoop)) = entities.split(&trace) {
		return (prefix.to_vec(), hoop.to_vec());
	    } else {
		let t = entities.union(q);
		let products = Self::compute_all(&t, rs);
		trace.push(entities.clone());
		entities = products;
	    }
	}
    }


    /// Finds the loops by simulating the system.
    pub fn find_only_loop(
	rs: &[Self],
	entities: Set,
	q: &Set
    ) -> Vec<Set> {
	let mut entities = entities;
	let mut trace = vec![];
	loop {
	    if let Some((_prefix, hoop)) = entities.split(&trace) {
		return hoop.to_vec();
	    } else {
		let t = entities.union(q);
		let products = Self::compute_all(&t, rs);
		trace.push(entities.clone());
		entities = products;
	    }
	}
    }


    /// Finds the loops and the length of the prefix by simulating the system.
    pub fn find_prefix_len_loop(
	rs: &[Self],
	entities: Set,
	q: &Set
    ) -> (usize, Vec<Set>) {
	let mut entities = entities;
	let mut trace = vec![];
	loop {
	    if let Some((prefix, hoop)) = entities.split(&trace) {
		return (prefix.len(), hoop.to_vec());
	    } else {
		let t = entities.union(q);
		let products = Self::compute_all(&t, rs);
		trace.push(entities.clone());
		entities = products;
	    }
	}
    }


    /// see loop/5
    pub fn lollipops_only_loop_decomposed_q(
	reaction_rules: &[Self],
	q: &Set,
	available_entities: &Set,
    ) -> Vec<Set> {
	let find_loop_fn =
	    |q| Reaction::find_only_loop(reaction_rules,
					 available_entities.clone(),
					 q);
	find_loop_fn(q)
    }
}

impl Default for Reaction {
    fn default() -> Self {
	Reaction::new()
    }
}

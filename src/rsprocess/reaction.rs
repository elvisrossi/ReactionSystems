//! Definitions for the 'classical' mechanism for computation.
//!
//! Allows to define the 'classical' mechanism to compute in a Reaction System
//! (RS) Framework.

use serde::{Deserialize, Serialize};
use std::hash::Hash;

use super::set::{BasicSet, ExtensionsSet, Set};
use super::translator::{Translator, PrintableWithTranslator, Formatter};

pub trait BasicReaction<S: BasicSet>:
Clone + Default + Eq + Hash + Serialize + PrintableWithTranslator
where for<'de> Self: Deserialize<'de>,
{
	fn enabled(&self, state: &S) -> bool;
	fn compute_step(&self, state: &S) -> Option<&S>;
}

pub trait ExtensionReaction<S: BasicSet> {
	fn compute_all(reactions: &[Self], state: &S) -> S
	where Self: Sized;

	fn find_loop(reactions: &[Self], entities: S, q: &S) -> (Vec<S>, Vec<S>)
	where Self: Sized;

	fn find_only_loop(reactions: &[Self], entities: S, q: &S) -> Vec<S>
	where Self: Sized;

	fn find_prefix_len_loop(
		reactions: &[Self],
		entities: S,
		q: &S
	) -> (usize, Vec<S>)
	where Self: Sized;

	fn lollipops_only_loop_decomposed_q(
		reactions: &[Self],
		entities: &S,
		q: &S
	) -> Vec<S>
	where Self: Sized;
}


impl<T: BasicReaction<S>, S: BasicSet> ExtensionReaction<S> for T {
	/// Computes the result of a series of reactions. Returns the union of all
	/// products.
	/// see result
	fn compute_all(
		reactions: &[Self],
		state: &S
	) -> S
	where Self: Sized {
		reactions.iter().fold(S::default(), |mut acc: S, r| {
			acc.extend(r.compute_step(state));
			acc
		})
	}

	/// Finds the loops by simulating the system.
	fn find_loop(
		reactions: &[Self],
		entities: S,
		q: &S
	) -> (Vec<S>, Vec<S>) {
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
	fn find_only_loop(
		reactions: &[Self],
		entities: S,
		q: &S
	) -> Vec<S> {
		let mut entities = entities;
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
	fn find_prefix_len_loop(
		reactions: &[Self],
		entities: S,
		q: &S
	) -> (usize, Vec<S>) {
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
	fn lollipops_only_loop_decomposed_q(
		reactions: &[Self],
		entities: &S,
		q: &S,
	) -> Vec<S> {
		let find_loop_fn =
			|q| Self::find_only_loop(reactions,
									 entities.clone(),
									 q);
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

impl BasicReaction<Set> for Reaction {
	/// returns true if ```current_state``` enables the reaction
	/// see enable
	fn enabled(&self, current_state: &Set) -> bool {
		self.reactants.is_subset(current_state)
			&& self.inhibitors.is_disjoint(current_state)
	}

	/// Computes the result of a single reaction (if enabled returns the
	/// products) otherwise returns None.
	/// see result
	fn compute_step(
		&self,
		state: &Set,
	) -> Option<&Set> {
		if self.enabled(state) {
			Some(&self.products)
		} else {
			None
		}
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
}

impl PrintableWithTranslator for Reaction {
	fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator)
			 -> std::fmt::Result {
		write!(
			f,
			"(r: {}, i: {}, p: {})",
			Formatter::from(translator, &self.reactants),
			Formatter::from(translator, &self.inhibitors),
			Formatter::from(translator, &self.products)
		)
	}
}

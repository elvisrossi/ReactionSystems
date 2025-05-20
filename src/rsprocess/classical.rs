//! Definitions for the 'classical' mechanism for computation.
//!
//! This initial part allows to define the 'classical' mechanism to compute in a
//! Reaction System (RS) Framework.
//! The data is held in RSset or RSreaction, in the latter the reagents,
//! inhibitors and products are held.
#![allow(dead_code)]

use super::structure::{RSset, RSreaction};

/// Computes the result of a single reaction (if enabled returns the products)
/// otherwise returns None.
pub fn compute_step<'a>(current_state: &RSset<'a>, reaction: &'a RSreaction<'a>) -> Option<&'a RSset<'a>> {
    if reaction.enabled(current_state) {
	Some(reaction.products())
    } else {
	None
    }
}

/// Computes the result of a series of reactions. Returns the union of all
/// products.
pub fn compute_all<'a>(current_state: &RSset<'a>, reactions: Vec<&'a RSreaction<'a>>) -> RSset<'a> {
    reactions.iter().fold(RSset::new(), |acc, r| acc.union_option(compute_step(current_state, r)))
}

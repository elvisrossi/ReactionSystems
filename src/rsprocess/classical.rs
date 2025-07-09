//! Definitions for the 'classical' mechanism for computation.
//!
//! This initial part allows to define the 'classical' mechanism to compute in a
//! Reaction System (RS) Framework.
//! The data is held in RSset or RSreaction, in the latter the reagents,
//! inhibitors and products are held.
use super::structure::{RSreaction, RSset};

/// Computes the result of a single reaction (if enabled returns the products)
/// otherwise returns None.
/// see result
pub fn compute_step<'a>(
    current_state: &'a RSset,
    reaction: &'a RSreaction
) -> Option<&'a RSset> {
    if reaction.enabled(current_state) {
        Some(&reaction.products)
    } else {
        None
    }
}

/// Computes the result of a series of reactions. Returns the union of all
/// products.
/// see result
pub fn compute_all<'a>(
    current_state: &'a RSset,
    reactions: &'a [RSreaction]
) -> RSset {
    reactions.iter().fold(RSset::new(), |mut acc, r| {
        acc.union_option(compute_step(current_state, r));
	acc
    })
}

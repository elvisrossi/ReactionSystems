use super::perpetual::{
    lollipops_decomposed_named, lollipops_prefix_len_loop_decomposed,
    lollipops_prefix_len_loop_decomposed_named,
};
use super::structure::{RSenvironment, RSreaction, RSset};
use super::translator::IdType;
use std::cmp;
use std::collections::HashSet;


/// Two set of entities E1 and E2 are confluent w.r.t. the perpetual context
/// delta iff they reach the same loop.
/// confluent checks if all the sets of entities in ```entities``` are confluent
/// and if so returns the maximal length of prefixes traversed to reached the
/// loop, its dimension (length) and the loop.
/// see confluent, confluents
pub fn confluent(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    entities: &[RSset],
) -> Option<(usize, usize, Vec<RSset>)> {
    let all_loops = lollipops_prefix_len_loop_decomposed(delta,
							 reaction_rules,
							 entities.first()?);
    let (prefix_len, hoop) = all_loops.first()?.clone();
    let dimension = hoop.len();
    let mut max_distance = prefix_len;

    for available_entities in entities.iter().skip(1) {
        let all_loops =
            lollipops_prefix_len_loop_decomposed(delta,
						 reaction_rules,
						 available_entities);
        let (prefix_len, new_hoop) = all_loops.first()?;

        if new_hoop.len() != dimension || !hoop.contains(new_hoop.first()?) {
            return None;
        }
        max_distance = cmp::max(max_distance, *prefix_len);
    }
    Some((max_distance, dimension, hoop))
}

/// Two set of entities E1 and E2 are confluent w.r.t. the perpetual context Q
/// iff they reach the same loop.
/// The predicate confluent(Rs,Q,Es,Loop,Distance,Dimension) checks if all the
/// sets of entities in Es are confluent and if so returns the Loop, the maximal
/// length of prefixes traversed to reached the loop and its dimension (length).
/// see confluent, confluents
pub fn confluent_named(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    entities: &[RSset],
    symb: IdType,
) -> Option<(usize, usize, Vec<RSset>)> {
    let (prefix_len, first_hoop) =
        lollipops_prefix_len_loop_decomposed_named(delta,
						   reaction_rules,
						   entities.first()?,
						   symb)?;
    let dimension = first_hoop.len();
    let mut max_distance = prefix_len;
    let hoop = first_hoop;

    for available_entities in entities.iter().skip(1) {
        let (prefix_len, new_hoop) = lollipops_prefix_len_loop_decomposed_named(
            delta,
            reaction_rules,
            available_entities,
            symb,
        )?;

        if new_hoop.len() != dimension || !hoop.contains(new_hoop.first()?) {
            return None;
        }
        max_distance = cmp::max(max_distance, prefix_len);
    }
    Some((max_distance, dimension, hoop))
}

// -----------------------------------------------------------------------------

/// invariant_named checks if all the sets of entities in ```entities``` are
/// confluent and if so returns the set of all traversed states, together with the loop.
/// see invariant
pub fn invariant_named(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    entities: &[RSset],
    symb: IdType,
) -> Option<(Vec<RSset>, Vec<RSset>)> {
    let (prefix, hoop) =
        lollipops_decomposed_named(delta,
				   reaction_rules,
				   entities.first()?,
				   symb)?;
    let mut invariant = vec![];
    invariant.append(&mut prefix.clone());
    invariant.append(&mut hoop.clone());
    let dimension = hoop.len();

    for available_entities in entities {
        let (new_prefix, new_hoop) =
            lollipops_decomposed_named(delta,
				       reaction_rules,
				       available_entities,
				       symb)?;
        if new_hoop.len() != dimension || !hoop.contains(new_hoop.first()?) {
            return None;
        }
        invariant.append(&mut new_prefix.clone());
    }
    // remove duplicates, maybe better with sorting?
    invariant = invariant
        .iter()
        .cloned()
        .collect::<HashSet<_>>()
        .iter()
        .cloned()
        .collect::<Vec<_>>();
    Some((invariant, hoop))
}

// -----------------------------------------------------------------------------

/// Suppose the context has the form
/// Q1. ... Q1.Q2. ... Q2. ... Qn. ... Qn. ...
/// and that each context Q1, Q2, ... , Q(n-1) is provided for a large number
/// of times, enough to stabilize the system in a loop (while Qn is provided
/// infinitely many times).  Then it can be the case that when the context
/// switches from Qi to Q(i+1), no matter what is the current state of the loop
/// for Qi at the moment of the switching, the system will stabilize in the same
/// loop for Q(i+1): if this is the case the system is called "loop confluent".
/// loop_confluent_named checks this property over the list of contexts
/// [Q1,Q2,...,Qn] and returns the lists of Loops, Distances and Dimensions for
/// all Qi's.
/// see loop_confluent
pub fn loop_confluent_named(
    deltas: &[RSenvironment],
    reaction_rules: &[RSreaction],
    entities: &[RSset],
    symb: IdType,
) -> Option<Vec<(usize, usize, Vec<RSset>)>> {
    deltas
        .iter()
        .map(|q| confluent_named(q, reaction_rules, entities, symb))
        .collect::<Option<Vec<_>>>()
}

/// "strong confluence" requires loop confluence and additionally check
/// that even if the context is switched BEFORE REACHING THE LOOP for Qi
/// the traversed states are still confluent for Q(i+1)
/// IMPORTANT: this notion of confluence assumes each context can be executed 0
/// or more times
/// see strong_confluent
#[allow(clippy::type_complexity)]
pub fn strong_confluent_named(
    deltas: &[RSenvironment],
    reaction_rules: &[RSreaction],
    entities: &[RSset],
    symb: IdType,
) -> Option<Vec<(Vec<RSset>, usize, Vec<RSset>)>> {
    deltas
        .iter()
        .map(|q| {
            let (invariant, hoop) = invariant_named(q,
						    reaction_rules,
						    entities,
						    symb)?;
            let length = invariant.len();
            Some((invariant, length, hoop))
        })
        .collect::<Option<Vec<_>>>()
}

// TODO: weak confluence

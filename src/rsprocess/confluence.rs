#![allow(dead_code)]

use super::perpetual::{
    lollipops_decomposed_named,
    lollipops_named,
    lollipops_only_loop_decomposed,
    lollipops_only_loop_decomposed_named,
    lollipops_prefix_len_loop_decomposed,
    lollipops_prefix_len_loop_decomposed_named
};
use super::structure::{RSenvironment, RSreaction, RSset};
use super::translator::IdType;
use std::cmp;


// see confluent, confluents
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
	let all_loops = lollipops_prefix_len_loop_decomposed(delta,
							     reaction_rules,
							     available_entities);
        // FIXME we take just the first? do we compare all?
        let (prefix_len, new_hoop) = all_loops.first()?;

        if new_hoop.len() != dimension
	    || !hoop.contains(new_hoop.first().unwrap()) {
            return None;
        }
        max_distance = cmp::max(max_distance, *prefix_len);
    }
    Some((max_distance, dimension, hoop))
}


// see confluent, confluents
pub fn confluent_named(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    entities: &[RSset],
    symb: IdType
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
        let (prefix_len, new_hoop) =
            lollipops_prefix_len_loop_decomposed_named(delta,
						       reaction_rules,
						       available_entities,
						       symb)?;

        if new_hoop.len() != dimension
	    || !hoop.contains(new_hoop.first().unwrap()) {
            return None;
        }
        max_distance = cmp::max(max_distance, prefix_len);
    }
    Some((max_distance, dimension, hoop))
}

#![allow(dead_code)]

use super::perpetual::{
    lollipops_only_loop_decomposed,
    lollipops_only_loop_decomposed_named,
    lollipops_prefix_len_loop_decomposed,
    lollipops_prefix_len_loop_decomposed_named};
use super::structure::{RSenvironment, RSreaction, RSset};
use super::translator::IdType;
use std::cmp;

pub fn confluent(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    entities: &[RSset],
) -> Option<(usize, usize, Vec<RSset>)> {
    let mut max_distance = 0;
    let mut dimension = 0;
    let mut hoop = vec![];

    if let Some(el) = entities.first() {
        if let Some(new_hoop) = lollipops_only_loop_decomposed(delta, reaction_rules, el).first() {
            dimension = new_hoop.len();
            hoop = new_hoop.clone();
        }
    }

    for available_entities in entities.iter().skip(1) {
        // FIXME we take just the first? do we compare all?
        if let Some((prefix_len, new_hoop)) =
            lollipops_prefix_len_loop_decomposed(delta, reaction_rules, available_entities).first()
        {
            if hoop.len() != dimension || hoop != *new_hoop {
                return None;
            }
            max_distance = cmp::max(max_distance, *prefix_len);
        } else {
            return None;
        }
    }
    Some((max_distance, dimension, hoop))
}


pub fn confluent_named(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    entities: &[RSset],
    symb: IdType
) -> Option<(usize, usize, Vec<RSset>)> {
    let mut max_distance = 0;
    let mut dimension = 0;
    let mut hoop = vec![];

    if let Some(el) = entities.first() {
        if let Some(new_hoop) = lollipops_only_loop_decomposed_named(delta, reaction_rules, el, symb) {
            dimension = new_hoop.len();
            hoop = new_hoop.clone();
        }
    }

    for available_entities in entities.iter().skip(1) {
        // FIXME we take just the first? do we compare all?
        if let Some((prefix_len, new_hoop)) =
            lollipops_prefix_len_loop_decomposed_named(delta, reaction_rules, available_entities, symb)
        {
            if hoop.len() != dimension || hoop != *new_hoop {
                return None;
            }
            max_distance = cmp::max(max_distance, prefix_len);
        } else {
            return None;
        }
    }
    Some((max_distance, dimension, hoop))
}

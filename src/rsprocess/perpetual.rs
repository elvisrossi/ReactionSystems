#![allow(dead_code)]

use std::rc::Rc;
use super::classical::compute_all_owned;
use super::translator::IdType;
use super::structure::{RSenvironment, RSprocess, RSreaction, RSset, RSsystem};


// returns the prefix and the loop from a trace
fn split<'a>(
    set: &'a RSset,
    trace: &'a [RSset]
) -> Option<(&'a[RSset], &'a[RSset])> {
    let position = trace.iter().rposition(|x| x == set);
    position.map(|pos| trace.split_at(pos))
}

// finds the loops by simulating the system
fn find_loop(
    rs: &[RSreaction],
    entities: RSset,
    q: &RSset
) -> (Vec<RSset>, Vec<RSset>) {
    let mut entities = entities;
    let mut trace = vec![];
    loop {
	if let Some((prefix, hoop)) = split(&entities, &trace) {
	    return (prefix.to_vec(), hoop.to_vec());
	} else {
	    let t = entities.union(q);
	    let products = compute_all_owned(&t, rs);
	    trace.push(entities.clone());
	    entities = products;
	}
    }
}


// finds the loops by simulating the system
fn find_only_loop(rs: &[RSreaction], entities: RSset, q: &RSset) -> Vec<RSset> {
    let mut entities = entities;
    let mut trace = vec![];
    loop {
	if let Some((_prefix, hoop)) = split(&entities, &trace) {
	    return hoop.to_vec();
	} else {
	    let t = entities.union(q);
	    let products = compute_all_owned(&t, rs);
	    trace.push(entities.clone());
	    entities = products;
	}
    }
}


// finds the loops and the length of the prefix by simulating the system
fn find_prefix_len_loop(
    rs: &[RSreaction],
    entities: RSset,
    q: &RSset
) -> (usize, Vec<RSset>) {
    let mut entities = entities;
    let mut trace = vec![];
    loop {
	if let Some((prefix, hoop)) = split(&entities, &trace) {
	    return (prefix.len(), hoop.to_vec());
	} else {
	    let t = entities.union(q);
	    let products = compute_all_owned(&t, rs);
	    trace.push(entities.clone());
	    entities = products;
	}
    }
}

// -----------------------------------------------------------------------------

// finds only the rules X = pre(Q, rec(X)), but not only x = pre(Q, rec(x))
// to use in filter_map
fn filter_delta<'a>(x: (&IdType, &'a RSprocess)) -> Option<&'a RSset> {
    use super::structure::RSprocess::*;
    let (id, rest) = x;
    match rest {
	EntitySet{ entities, next_process} => {
	    match &**next_process {
		RecursiveIdentifier{ identifier } if identifier == id => {
		    Some(entities)
		},
		_ => None
	    }
	},
	_ => None
    }
}


// see lollipop
pub fn lollipops_decomposed(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    available_entities: &RSset
) -> Vec<(Vec<RSset>, Vec<RSset>)> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = delta.iter().filter_map(filter_delta);

    let find_loop_fn = |q| find_loop(reaction_rules,
				     available_entities.clone(),
				     q);

    filtered.map(find_loop_fn).collect::<Vec<_>>()
}

// see lollipop
pub fn lollipops(system: RSsystem) -> Vec<(Vec<RSset>, Vec<RSset>)> {
    lollipops_decomposed(system.get_delta(),
			 system.get_reaction_rules(),
			 system.get_available_entities())
}

// see loop
pub fn lollipops_only_loop(system: RSsystem) -> Vec<Vec<RSset>> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = system.get_delta().iter().filter_map(filter_delta);

    let find_loop_fn = |q| find_only_loop(system.get_reaction_rules(),
				     system.get_available_entities().clone(),
				     q);

    filtered.map(find_loop_fn).collect::<Vec<_>>()
}


pub fn lollipops_prefix_len_loop_decomposed(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    available_entities: &RSset
) -> Vec<(usize, Vec<RSset>)> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = delta.iter().filter_map(filter_delta);

    let find_loop_fn = |q| find_prefix_len_loop(reaction_rules,
						available_entities.clone(),
						q);

    filtered.map(find_loop_fn).collect::<Vec<_>>()
}

// see loop
pub fn lollipops_only_loop_decomposed(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    available_entities: &RSset
) -> Vec<Vec<RSset>> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = delta.iter().filter_map(filter_delta);

    let find_loop_fn = |q| find_only_loop(reaction_rules,
				     available_entities.clone(),
				     q);

    filtered.map(find_loop_fn).collect::<Vec<_>>()
}

// -----------------------------------------------------------------------------
// Named versions
// -----------------------------------------------------------------------------

// finds only the rules symb = pre(Q, rec(symb)), get symb from a translator
// to use in filter_map
fn filter_delta_named<'a>(
    x: (&IdType, &'a RSprocess),
    symb: &IdType
) -> Option<&'a RSset> {
    use super::structure::RSprocess::*;
    let (id, rest) = x;
    if id != symb {
	return None;
    }
    match rest {
	EntitySet{ entities, next_process} => {
	    match &**next_process {
		RecursiveIdentifier{ identifier } if identifier == id => {
		    Some(entities)
		},
		_ => None
	    }
	},
	_ => None
    }
}

// see lollipop
pub fn lollipops_decomposed_named(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    available_entities: &RSset,
    symb: IdType
) -> Option<(Vec<RSset>, Vec<RSset>)> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = delta.iter().filter_map(|x| filter_delta_named(x, &symb)).next();

    let find_loop_fn = |q| find_loop(reaction_rules,
				     available_entities.clone(),
				     q);

    filtered.map(find_loop_fn)
}

// see lollipop
pub fn lollipops_named(
    system: RSsystem,
    symb: IdType
) -> Option<(Vec<RSset>, Vec<RSset>)> {
    lollipops_decomposed_named(system.get_delta(),
			       system.get_reaction_rules(),
			       system.get_available_entities(),
			       symb)
}

// see loop
pub fn lollipops_only_loop_named(
    system: RSsystem,
    symb: IdType
) -> Option<Vec<RSset>> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = system.get_delta().iter()
	.filter_map(|x| filter_delta_named(x, &symb)).next();

    let find_loop_fn =
	|q| find_only_loop(system.get_reaction_rules(),
			   system.get_available_entities().clone(),
			   q);

    filtered.map(find_loop_fn)
}

pub fn lollipops_prefix_len_loop_decomposed_named(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    available_entities: &RSset,
    symb: IdType
) -> Option<(usize, Vec<RSset>)> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = delta.iter()
	.filter_map(|x| filter_delta_named(x, &symb)).next();

    let find_loop_fn = |q| find_prefix_len_loop(reaction_rules,
						available_entities.clone(),
						q);

    filtered.map(find_loop_fn)
}

// see loop
pub fn lollipops_only_loop_decomposed_named(
    delta: &RSenvironment,
    reaction_rules: &[RSreaction],
    available_entities: &RSset,
    symb: IdType
) -> Option<Vec<RSset>> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = delta.iter()
	.filter_map(|x| filter_delta_named(x, &symb)).next();

    let find_loop_fn = |q| find_only_loop(reaction_rules,
					  available_entities.clone(),
					  q);

    filtered.map(find_loop_fn)
}

#![allow(dead_code)]

use std::rc::Rc;
use super::classical::compute_all_owned;
use super::translator::IdType;
use super::structure::{RSsystem, RSprocess, RSset, RSreaction};

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

// returns the prefix and the loop from a trace
fn split<'a>(set: &'a RSset, trace: &'a [RSset]) -> Option<(&'a[RSset], &'a[RSset])> {
    let position = trace.iter().rposition(|x| x == set);
    position.map(|pos| trace.split_at(pos))
}

// finds the loops by simulating the system
fn find_loop(rs: &Rc<Vec<RSreaction>>, entities: RSset, q: &RSset) -> (Vec<RSset>, Vec<RSset>) {
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
fn find_only_loop(rs: &Rc<Vec<RSreaction>>, entities: RSset, q: &RSset) -> Vec<RSset> {
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

// see lollipop
pub fn lollipops(system: RSsystem) -> Vec<(Vec<RSset>, Vec<RSset>)> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = system.get_delta().iter().filter_map(filter_delta);

    let find_loop_fn = |q| find_loop(system.get_reaction_rules(),
				     system.get_available_entities().clone(),
				     q);

    filtered.map(find_loop_fn).collect::<Vec<_>>()
}

pub fn lollipops_only_loop(system: RSsystem) -> Vec<Vec<RSset>> {
    // FIXME: i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = system.get_delta().iter().filter_map(filter_delta);

    let find_loop_fn = |q| find_only_loop(system.get_reaction_rules(),
				     system.get_available_entities().clone(),
				     q);

    filtered.map(find_loop_fn).collect::<Vec<_>>()
}

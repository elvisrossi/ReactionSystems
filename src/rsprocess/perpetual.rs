#![allow(dead_code)]

use std::rc::Rc;

use super::classical::compute_all_owned;
use super::translator::IdType;
use super::structure::{RSsystem, RSprocess, RSset, RSreaction};

fn split<'a>(set: &'a RSset, trace: &'a [RSset]) -> Option<(&'a[RSset], &'a[RSset])> {
    let position = trace.iter().rposition(|x| x == set);
    position.map(|pos| trace.split_at(pos))
}

fn find_loop(rs: &Rc<Vec<RSreaction>>, e: RSset, q: &RSset) -> (Vec<RSset>, Vec<RSset>) {
    let mut e = e;
    let mut trace = vec![];
    loop {
	if let Some((prefix, hoop)) = split(&e, &trace) {
	    return (prefix.to_vec(), hoop.to_vec());
	} else {
	    let t = e.union(q);
	    let p = compute_all_owned(&t, rs);
	    trace.push(e.clone());
	    e = p;
	}
    }
}

// see lollipop
pub fn lollipops(system: RSsystem) -> Vec<(Vec<RSset>, Vec<RSset>)> {
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

    // FIXME: what? i think we are only interested in "x", not all symbols that
    // satisfy X = pre(Q, rec(X))
    let filtered = system.get_delta().iter().filter_map(filter_delta);

    filtered.map(|q| find_loop(system.get_reaction_rules(),
			       system.get_available_entities().clone(),
			       q)
    ).collect::<Vec<_>>()
}

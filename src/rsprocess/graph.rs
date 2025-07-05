#![allow(dead_code)]

use petgraph::Graph;
use std::collections::HashMap;
use super::structure::{RSlabel, RSsystem};
use super::support_structures::TransitionsIterator;

pub fn digraph(
    system: RSsystem
) -> Result<Graph<RSsystem, RSlabel>, String> {
    let mut graph: Graph<RSsystem, RSlabel> = Graph::new();
    let node = graph.add_node(system.clone());

    let mut association = HashMap::new();
    association.insert(system.clone(), node);

    let mut stack = vec![system];
    let mut current;


    while !stack.is_empty() {
	// depth first
	current = stack.pop().unwrap();
	let current_node = *association.get(&current).unwrap();

	// cycle through all next nodes
	let tr = TransitionsIterator::from(&current)?;

	for (label, next) in tr {
	    // if not already visited
	    let next_node = association.entry(next.clone()).or_insert_with(|| {
		stack.push(next.clone());
		graph.add_node(next)
	    });
	    graph.add_edge(current_node, *next_node, label);
	}
    }
    Ok(graph)
}

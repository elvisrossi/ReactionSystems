#![allow(dead_code)]

use petgraph::Graph;
use std::collections::HashMap;
use super::structure::{RSlabel, RSsystem, RSset};
use super::support_structures::TransitionsIterator;
use super::translator;
use std::rc::Rc;

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

// -----------------------------------------------------------------------------
//                              helper functions
// -----------------------------------------------------------------------------


// Nodes -----------------------------------------------------------------------

pub enum GraphMapNodes {
    Hide,
    Entities,
    MaskEntities { mask: RSset },
    Context,
}

type GraphMapNodesFnTy = dyn Fn(petgraph::prelude::NodeIndex, &RSsystem) -> String;
pub struct GraphMapNodesTy {
    function: Box<GraphMapNodesFnTy>
}

impl GraphMapNodesTy {
    pub fn from(
	f: GraphMapNodes,
	translator: Rc<translator::Translator>
    ) -> Self {
	use GraphMapNodes::*;
	let function: Box<GraphMapNodesFnTy> =
	// rust cant unify closures (they all have different types) so box needs
	// to happen inside the match
	// we use move because translator is from the env, so we transfer the
	// borrow to the struct, also translator needs to be in box, a reference
	// is not enough
	    match f {
		Hide => {
		    Box::new(
			|_, _|
			String::new()
		    )
		},
		Entities => {
		    Box::new(
			move |_, node: &RSsystem|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    node.get_available_entities())
			)
		    )
		},
		MaskEntities { mask } => {
		    Box::new(
			move |_, node: &RSsystem| {
			    let masked_entities =
				node.get_available_entities()
				.intersection(&mask);
			    format!("{}",
				    translator::RSsetDisplay::from(
					&translator,
					&masked_entities)
			    )
			}
		    )
		},
		Context => {
		    Box::new(
			move |_, node: &RSsystem|
			format!("{}",
				translator::RSprocessDisplay::from(
				    &translator,
				    node.get_context_process())
			)
		    )
		},
	    };
	GraphMapNodesTy { function }
    }

    pub fn get(&self) -> &GraphMapNodesFnTy {
	&self.function
    }
}

impl From<GraphMapNodesTy> for Box<GraphMapNodesFnTy> {
    fn from(g: GraphMapNodesTy) -> Self {
	g.function
    }
}

// Edges -----------------------------------------------------------------------

pub enum GraphMapEdges {
    Hide,
    Products,
    MaskProducts { mask: RSset },
    Entities,
    MaskEntities { mask: RSset },
    Context,
    MaskContext { mask: RSset },
    Union,
    MaskUnion { mask: RSset },
    Difference,
    MaskDifference { mask: RSset },
    EntitiesDeleted,
    MaskEntitiesDeleted { mask: RSset },
    EntitiesAdded,
    MaskEntitiesAdded { mask: RSset },
}

type GraphMapEdgesFnTy = dyn Fn(petgraph::prelude::EdgeIndex, &RSlabel) -> String;
pub struct GraphMapEdgesTy {
    function: Box<GraphMapEdgesFnTy>
}

impl GraphMapEdgesTy {
    pub fn from(
	f: GraphMapEdges,
	translator: Rc<translator::Translator>
    ) -> Self {
	use GraphMapEdges::*;
	let function: Box<GraphMapEdgesFnTy> =
	// rust cant unify closures (they all have different types) so box needs
	// to happen inside the match
	// we use move because translator is from the env, so we transfer the
	// borrow to the struct, also translator needs to be in box, a reference
	// is not enough
	    match f {
		Hide => {
		    Box::new(
			|_, _|
			String::new()
		    )
		},
		Products => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &edge.products
				)
			)
		    )
		},
		MaskProducts { mask } => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &mask.intersection(&edge.products)
				)
			)
		    )
		},
		Entities => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &edge.available_entities
				)
			)
		    )
		},
		MaskEntities { mask } => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &mask.intersection(&edge.available_entities)
				)
			)
		    )
		},
		Context => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &edge.context
				)
			)
		    )
		},
		MaskContext { mask } => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &mask.intersection(&edge.context)
				)
			)
		    )
		},
		Union => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &edge.t
				)
			)
		    )
		},
		MaskUnion { mask } => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &mask.intersection(&edge.t)
				)
			)
		    )
		},
		Difference => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &edge.context.subtraction(
					&edge.available_entities
				    )
				)
			)
		    )
		},
		MaskDifference { mask } => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &mask.intersection(
					&edge.context.subtraction(
					    &edge.available_entities
					)
				    )
				)
			)
		    )
		},
		EntitiesDeleted => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &edge.available_entities.subtraction(
					&edge.products
				    )
				)
			)
		    )
		},
		MaskEntitiesDeleted { mask } => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &mask.intersection(
					&edge.available_entities.subtraction(
					    &edge.products
					)
				    )
				)
			)
		    )
		},
		EntitiesAdded => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &edge.products.subtraction(
					&edge.available_entities
				    )
				)
			)
		    )
		},
		MaskEntitiesAdded { mask } => {
		    Box::new(
			move |_, edge: &RSlabel|
			format!("{}",
				translator::RSsetDisplay::from(
				    &translator,
				    &mask.intersection(
					&edge.products.subtraction(
					    &edge.available_entities
					)
				    )
				)
			)
		    )
		},
	    };
	GraphMapEdgesTy { function }
    }

    pub fn get(&self) -> &GraphMapEdgesFnTy {
	&self.function
    }
}

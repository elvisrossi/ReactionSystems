#![allow(dead_code)]

use petgraph::{Graph, Directed};
use std::collections::HashMap;
use super::structure::{RSlabel, RSsystem, RSset, RSprocess};
use super::support_structures::TransitionsIterator;
use super::translator;
use std::rc::Rc;

type RSgraph = Graph<RSsystem, RSlabel, Directed, u32>;

pub fn digraph(
    system: RSsystem
) -> Result<RSgraph, String> {
    let mut graph = Graph::default();
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
				    &node.available_entities)
			)
		    )
		},
		MaskEntities { mask } => {
		    Box::new(
			move |_, node: &RSsystem| {
			    let masked_entities =
				node.available_entities
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
				    &node.context_process)
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

// -----------------------------------------------------------------------------
//                          Formatting Nodes & Edges
// -----------------------------------------------------------------------------
use petgraph::visit::{IntoNodeReferences, IntoEdgeReferences, EdgeRef};

type RSdotGraph = Graph<String, String, Directed, u32>;
type RSformatNodeTy =
    dyn Fn(&RSdotGraph, <&RSdotGraph as IntoNodeReferences>::NodeRef) -> String;

type RSformatEdgeTy =
    dyn Fn(&RSdotGraph, <&RSdotGraph as IntoEdgeReferences>::EdgeRef) -> String;

pub fn default_node_formatter(
    original_graph: Rc<RSgraph>
) -> Box<RSformatNodeTy>
{
    Box::new(
	move |_g, n|
	String::from(
	    match original_graph.node_weight(n.0).unwrap().context_process
	    {
		RSprocess::Nill =>
		    ", fillcolor=white",
		RSprocess::RecursiveIdentifier { identifier: _ } =>
		    ", fillcolor=\"#BBFF99\"",
		RSprocess::EntitySet { entities: _, next_process: _ } =>
		    ", fillcolor=\"#AAEEFF\"",
		RSprocess::NondeterministicChoice { children: _ } =>
		    ", fillcolor=\"#FFEE99\"",
		RSprocess::Summation { children: _ } =>
		    ", fillcolor=\"#CC99FF\"",
		RSprocess::WaitEntity
		{ repeat: _, repeated_process: _, next_process: _ } =>
		    ", fillcolor=\"#FF99AA\"",
	    }
	)
    )
}

pub fn default_edge_formatter(
    original_graph: Rc<RSgraph>
) -> Box<RSformatEdgeTy>
{
    Box::new(
	move |_g, e| String::from(
	    if original_graph.edge_weight(e.id()).unwrap().products.is_empty() {
		"color=black, fontcolor=black"
	    } else {
		"color=blue, fontcolor=blue"
	    }
    ))
}

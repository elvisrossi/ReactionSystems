//! Definitions for generating graphs from a simulation.

use petgraph::{Graph, Directed};
use std::collections::HashMap;
use super::structure::{RSlabel, RSsystem, RSset, RSprocess};
use super::support_structures::TransitionsIterator;
use super::translator::{self, IdType};
use std::rc::Rc;

type RSgraph = Graph<RSsystem, RSlabel, Directed, u32>;

/// Creates a graph starting from a system as root node
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

	for (label, next) in TransitionsIterator::from(&current)? {
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

/// Helper structure that specifies what information to display for nodes.
#[derive(Clone)]
pub enum GraphMapNodes {
    Hide,
    Entities,
    MaskEntities { mask: RSset },
    Context,
}

type GraphMapNodesFnTy =
    dyn Fn(petgraph::prelude::NodeIndex, &RSsystem) -> String;
/// Helper structure that holds a formatting function from node as RSsystem to
/// string
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

/// Helper structure that specifies what information to display for edges
#[derive(Clone)]
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
/// Helper structure that holds a formatting function from node as RSsystem to
/// string
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
    dyn Fn(
	&RSdotGraph,
	<&RSdotGraph as IntoNodeReferences>::NodeRef
    ) -> Option<String>;

#[derive(Clone, Copy)]
pub enum OperationType {
    Equals,
    Subset,
    SubsetEqual,
    Superset,
    SupersetEqual
}

impl OperationType {
    pub fn evaluate(&self, a: &RSset, b: &RSset) -> bool {
	match self {
	    Self::Equals => {
		a.is_subset(b) && b.is_subset(a)
	    },
	    Self::Subset => {
		a.is_subset(b) && !b.is_subset(a)
	    },
	    Self::SubsetEqual => {
		a.is_subset(b)
	    },
	    Self::Superset => {
		b.is_subset(a) && !a.is_subset(b)
	    },
	    Self::SupersetEqual => {
		b.is_subset(a)
	    }
	}
    }
}

#[derive(Clone)]
pub enum ContextColorConditional {
    Nill,
    RecursiveIdentifier(IdType),
    EntitySet(OperationType, RSset),
    NonDeterministicChoice,
    Summation,
    WaitEntity
}

#[derive(Clone)]
pub enum NodeColorConditional {
    ContextConditional(ContextColorConditional),
    EntitiesConditional(OperationType, RSset)
}

#[derive(Clone)]
pub struct NodeColor {
    pub conditionals: Vec<(NodeColorConditional, String)>,
    pub base_color: String
}

pub fn node_formatter_base_color(
    base_color: String
) -> String
{
    ", fillcolor=".to_string() + &base_color
}

pub fn node_formatter(
    original_graph: Rc<RSgraph>,
    rule: NodeColorConditional,
    color: String,
    star: Option<IdType>,
) -> Box<RSformatNodeTy>
{
    match rule {
	NodeColorConditional::ContextConditional(ccc) => {
	    match ccc {
		ContextColorConditional::Nill => {
		    Box::new(
			move |_, n| {
			    let rssystem = original_graph.node_weight(n.0).unwrap();
			    if rssystem.context_process == RSprocess::Nill {
				Some(", fillcolor=".to_string() + &color)
			    } else {
				None
			    }
			}
		    )
		},
		ContextColorConditional::RecursiveIdentifier(s) => {
		    Box::new(
			move |_, n| {
			    let rssystem = original_graph.node_weight(n.0).unwrap();
			    match (Some(s) == star, &rssystem.context_process) {
				(true, RSprocess::RecursiveIdentifier { identifier: _ }) => {
				    Some(", fillcolor=".to_string() + &color)
				},
				(false, RSprocess::RecursiveIdentifier { identifier: id }) if id == &s => {
				    Some(", fillcolor=".to_string() + &color)
				},
				_ => {None}
			    }
			}
		    )
		},
		ContextColorConditional::EntitySet(ot, set) => {
		    Box::new(
			move |_, n| {
			    let rssystem = original_graph.node_weight(n.0).unwrap();
			    match &rssystem.context_process {
				RSprocess::EntitySet { entities, next_process: _ } if ot.evaluate(entities, &set) => {
				    Some(", fillcolor=".to_string() + &color)
				},
				_ => {None}
			    }
			}
		    )
		},
		ContextColorConditional::NonDeterministicChoice => {
		    Box::new(
			move |_, n| {
			    let rssystem = original_graph.node_weight(n.0).unwrap();
			    if let RSprocess::NondeterministicChoice { children: _ } = rssystem.context_process {
				Some(", fillcolor=".to_string() + &color)
			    } else {
				None
			    }
			}
		    )
		},
		ContextColorConditional::Summation => {
		    Box::new(
			move |_, n| {
			    let rssystem = original_graph.node_weight(n.0).unwrap();
			    if let RSprocess::Summation { children: _ } = rssystem.context_process {
				Some(", fillcolor=".to_string() + &color)
			    } else {
				None
			    }
			}
		    )
		},
		ContextColorConditional::WaitEntity => {
		    Box::new(
			move |_, n| {
			    let rssystem = original_graph.node_weight(n.0).unwrap();
			    if let RSprocess::WaitEntity { repeat: _, repeated_process: _, next_process: _ } = &rssystem.context_process  {
				Some(", fillcolor=".to_string() + &color)
			    } else {
				None
			    }
			}
		    )
		},
	    }
	},
	NodeColorConditional::EntitiesConditional(ot, set) => {
	    Box::new(
		move |_, n| {
		    let rssystem = original_graph.node_weight(n.0).unwrap();
		    if ot.evaluate(&rssystem.available_entities, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
    }
}

type RSformatEdgeTy =
    dyn Fn(
	&RSdotGraph,
	<&RSdotGraph as IntoEdgeReferences>::EdgeRef
    ) -> Option<String>;

#[derive(Clone)]
pub enum EdgeColorConditional {
    Entities(OperationType, RSset),
    Context(OperationType, RSset),
    T(OperationType, RSset),
    Reactants(OperationType, RSset),
    ReactantsAbsent(OperationType, RSset),
    Inhibitors(OperationType, RSset),
    InhibitorsPresent(OperationType, RSset),
    Products(OperationType, RSset),
}

#[derive(Clone)]
pub struct EdgeColor {
    pub conditionals: Vec<(EdgeColorConditional, String)>,
    pub base_color: String
}


pub fn edge_formatter_base_color(
    base_color: String
) -> String
{
    ", fillcolor=".to_string() + &base_color
}

pub fn edge_formatter(
    original_graph: Rc<RSgraph>,
    rule: EdgeColorConditional,
    color: String,
) -> Box<RSformatEdgeTy>
{
    match rule {
	EdgeColorConditional::Entities(ot, set) => {
	    Box::new(
		move |_, e| {
		    let rssystem = original_graph.edge_weight(e.id()).unwrap();
		    if ot.evaluate(&rssystem.available_entities, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
	EdgeColorConditional::Context(ot, set) => {
	    Box::new(
		move |_, e| {
		    let rssystem = original_graph.edge_weight(e.id()).unwrap();
		    if ot.evaluate(&rssystem.context, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
	EdgeColorConditional::T(ot, set) => {
	    Box::new(
		move |_, e| {
		    let rssystem = original_graph.edge_weight(e.id()).unwrap();
		    if ot.evaluate(&rssystem.t, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
	EdgeColorConditional::Reactants(ot, set) => {
	    Box::new(
		move |_, e| {
		    let rssystem = original_graph.edge_weight(e.id()).unwrap();
		    if ot.evaluate(&rssystem.reactants, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
	EdgeColorConditional::ReactantsAbsent(ot, set) => {
	    Box::new(
		move |_, e| {
		    let rssystem = original_graph.edge_weight(e.id()).unwrap();
		    if ot.evaluate(&rssystem.reactants_absent, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
	EdgeColorConditional::Inhibitors(ot, set) => {
	    Box::new(
		move |_, e| {
		    let rssystem = original_graph.edge_weight(e.id()).unwrap();
		    if ot.evaluate(&rssystem.inhibitors, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
	EdgeColorConditional::InhibitorsPresent(ot, set) => {
	    Box::new(
		move |_, e| {
		    let rssystem = original_graph.edge_weight(e.id()).unwrap();
		    if ot.evaluate(&rssystem.inhibitors_present, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
	EdgeColorConditional::Products(ot, set) => {
	    Box::new(
		move |_, e| {
		    let rssystem = original_graph.edge_weight(e.id()).unwrap();
		    if ot.evaluate(&rssystem.products, &set) {
			Some(", fillcolor=".to_string() + &color)
		    } else {
			None
		    }
		}
	    )
	},
    }
}

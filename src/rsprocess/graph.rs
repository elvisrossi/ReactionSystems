//! Definitions for generating graphs from a simulation.

use petgraph::{Graph, Directed};
use std::collections::HashMap;
use super::structure::{RSlabel, RSsystem, RSset};
use super::support_structures::TransitionsIterator;
use super::translator::{self, IdType};
use std::rc::Rc;


pub type RSgraph = Graph<RSsystem, RSlabel, Directed, u32>;

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


pub fn common_entities(
    graph: &RSgraph
) -> RSset {
    graph.node_references().fold(
	None,
	|acc, node|
	match acc {
	    None => Some(node.1.available_entities.clone()),
	    Some(acc) => Some(node.1.available_entities.intersection(&acc))
	}
    ).unwrap_or(RSset::new())
}

// -----------------------------------------------------------------------------
//                              helper functions
// -----------------------------------------------------------------------------


// Nodes -----------------------------------------------------------------------

/// Helper structure that specifies what information to display for nodes.
#[derive(Clone)]
pub enum GraphMapNodes {
    String { string: String },
    Hide,
    Entities,
    MaskEntities { mask: RSset },
    ExcludeEntities { mask: RSset },
    Context,
}

type GraphMapNodesFnTy =
    dyn Fn(petgraph::prelude::NodeIndex, &RSsystem) -> String;
/// Helper structure that holds a formatting function from node as RSsystem to
/// string
pub struct GraphMapNodesTy {
    functions: Vec<Box<GraphMapNodesFnTy>>,
    translator: Rc<translator::Translator>
}

impl<const N: usize> From<([GraphMapNodes; N], Rc<translator::Translator>)> for GraphMapNodesTy {
    fn from(value: ([GraphMapNodes; N], Rc<translator::Translator>)) -> Self {
	Self::from((value.0.to_vec(), value.1))
    }
}

impl<const N: usize> From<(&[GraphMapNodes; N], Rc<translator::Translator>)> for GraphMapNodesTy {
    fn from(value: (&[GraphMapNodes; N], Rc<translator::Translator>)) -> Self {
	Self::from((value.0.to_vec(), value.1))
    }
}

impl From<(Vec<GraphMapNodes>, Rc<translator::Translator>)> for GraphMapNodesTy {
    fn from(value: (Vec<GraphMapNodes>, Rc<translator::Translator>)) -> Self {
	use GraphMapNodes::*;
	use super::format_helpers::graph_map_nodes_ty_from::*;

	let mut new = GraphMapNodesTy {functions: vec![], translator: value.1};

	for f in value.0 {
	    match f {
		String { string } => {
		    new.functions.push(format_string(string.clone()));
		}
		Hide => {
		    new.functions.push(format_hide(
			Rc::clone(&new.translator)
		    ));
		},
		Entities => {
		    new.functions.push(format_entities(
			Rc::clone(&new.translator)
		    ));
		},
		MaskEntities { mask } => {
		    new.functions.push(format_mask_entities(
			Rc::clone(&new.translator),
			mask.clone()
		    ));
		},
		ExcludeEntities { mask } => {
		    new.functions.push(format_exclude_entities(
			Rc::clone(&new.translator),
			mask.clone()
		    ));
		}
		Context => {
		    new.functions.push(format_context(
			Rc::clone(&new.translator)
		    ));
		},
	    };
	}

	new
    }
}

impl GraphMapNodesTy {
    pub fn generate<'a>(
	&self
    ) -> Box<dyn Fn(petgraph::prelude::NodeIndex, &'a RSsystem) -> String + 'a>
    {
	todo!()
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
	use super::format_helpers::graph_map_edges_ty_from::*;

	let function: Box<GraphMapEdgesFnTy> =
	// rust cant unify closures (they all have different types) so box needs
	// to happen inside the match
	// we use move because translator is from the env, so we transfer the
	// borrow to the struct, also translator needs to be in box, a reference
	// is not enough
	    match f {
		Hide => {
		    format_hide(translator)
		},
		Products => {
		    format_products(translator)
		},
		MaskProducts { mask } => {
		    format_mask_products(translator, mask)
		},
		Entities => {
		    format_entities(translator)
		},
		MaskEntities { mask } => {
		    format_mask_entities(translator, mask)
		},
		Context => {
		    format_context(translator)
		},
		MaskContext { mask } => {
		    format_mask_context(translator, mask)
		},
		Union => {
		    format_union(translator)
		},
		MaskUnion { mask } => {
		    format_mask_union(translator, mask)
		},
		Difference => {
		    format_difference(translator)
		},
		MaskDifference { mask } => {
		    format_mask_difference(translator, mask)
		},
		EntitiesDeleted => {
		    format_entities_deleted(translator)
		},
		MaskEntitiesDeleted { mask } => {
		    format_mask_entities_deleted(translator, mask)
		},
		EntitiesAdded => {
		    format_entities_added(translator)
		},
		MaskEntitiesAdded { mask } => {
		    format_mask_entities_added(translator, mask)
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
use petgraph::visit::{IntoEdgeReferences, IntoNodeReferences};

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
    use super::format_helpers::node_formatter::*;
    match rule {
	NodeColorConditional::ContextConditional(ccc) => {
	    match ccc {
		ContextColorConditional::Nill => {
		    format_nill(original_graph, color, star)
		},
		ContextColorConditional::RecursiveIdentifier(s) => {
		    format_recursive_identifier(original_graph, color, star, s)
		},
		ContextColorConditional::EntitySet(ot, set) => {
		    format_entity_set(original_graph, color, star, ot, set)
		},
		ContextColorConditional::NonDeterministicChoice => {
		    format_non_deterministic_choice(original_graph, color, star)
		},
		ContextColorConditional::Summation => {
		    format_summation(original_graph, color, star)
		},
		ContextColorConditional::WaitEntity => {
		    format_wait_entity(original_graph, color, star)
		},
	    }
	},
	NodeColorConditional::EntitiesConditional(ot, set) => {
	    format_entities_conditional(original_graph, color, star, ot, set)
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
    ", color=".to_string() + &base_color
}


pub fn edge_formatter(
    original_graph: Rc<RSgraph>,
    rule: EdgeColorConditional,
    color: String,
) -> Box<RSformatEdgeTy>
{
    use super::format_helpers::edge_formatter::*;
    match rule {
	EdgeColorConditional::Entities(ot, set) => {
	    format_entities(original_graph, color, ot, set)
	},
	EdgeColorConditional::Context(ot, set) => {
	    format_context(original_graph, color, ot, set)
	},
	EdgeColorConditional::T(ot, set) => {
	    format_t(original_graph, color, ot, set)
	},
	EdgeColorConditional::Reactants(ot, set) => {
	    format_reactants(original_graph, color, ot, set)
	},
	EdgeColorConditional::ReactantsAbsent(ot, set) => {
	    format_reactants_absent(original_graph, color, ot, set)
	},
	EdgeColorConditional::Inhibitors(ot, set) => {
	    format_inhibitors(original_graph, color, ot, set)
	},
	EdgeColorConditional::InhibitorsPresent(ot, set) => {
	    format_inhibitors_present(original_graph, color, ot, set)
	},
	EdgeColorConditional::Products(ot, set) => {
	    format_products(original_graph, color, ot, set)
	},
    }
}

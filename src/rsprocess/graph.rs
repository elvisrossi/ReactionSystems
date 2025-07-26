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


pub fn common_entities(graph: &RSgraph) -> RSset {
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

type GraphMapNodesFnTy<'a> =
    dyn Fn(petgraph::prelude::NodeIndex, &'a RSsystem) -> String + 'a;
/// Helper structure that holds a formatting function from node as RSsystem to
/// string
pub struct GraphMapNodesTy<'a> {
    functions: Vec<Box<GraphMapNodesFnTy<'a>>>,
    translator: Rc<translator::Translator>
}

impl<'a, const N: usize> From<([GraphMapNodes; N], Rc<translator::Translator>)> for GraphMapNodesTy<'a> {
    fn from(value: ([GraphMapNodes; N], Rc<translator::Translator>)) -> Self {
	Self::from((value.0.to_vec(), value.1))
    }
}

impl<'a> From<(&[GraphMapNodes], Rc<translator::Translator>)> for GraphMapNodesTy<'a> {
    fn from(value: (&[GraphMapNodes], Rc<translator::Translator>)) -> Self {
	Self::from((value.0.to_vec(), value.1))
    }
}

impl<'a> From<(Vec<GraphMapNodes>, Rc<translator::Translator>)> for GraphMapNodesTy<'a> {
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

impl<'a> GraphMapNodesTy<'a> {
    pub fn generate(self) -> Box<GraphMapNodesFnTy<'a>> {
	let mut accumulator: Box<GraphMapNodesFnTy<'a>> =
	    super::format_helpers::graph_map_nodes_ty_from::format_hide(
		Rc::clone(&self.translator)
	    );
	for f in self.functions {
	    accumulator = Box::new(move |i, n| {
		(accumulator)(i, n)
		    + &f(i, n)
	    })
	}

	accumulator
    }
}


// Edges -----------------------------------------------------------------------

/// Helper structure that specifies what information to display for edges
#[derive(Clone)]
pub enum GraphMapEdges {
    String { string: String },
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

type GraphMapEdgesFnTy<'a> =
    dyn Fn(petgraph::prelude::EdgeIndex, &'a RSlabel) -> String + 'a;
/// Helper structure that holds a formatting function from node as RSsystem to
/// string
pub struct GraphMapEdgesTy<'a> {
    functions: Vec<Box<GraphMapEdgesFnTy<'a>>>,
    translator: Rc<translator::Translator>
}

impl<'a, const N: usize> From<([GraphMapEdges; N], Rc<translator::Translator>)> for GraphMapEdgesTy<'a> {
    fn from(value: ([GraphMapEdges; N], Rc<translator::Translator>)) -> Self {
	Self::from((value.0.to_vec(), value.1))
    }
}

impl<'a> From<(&[GraphMapEdges], Rc<translator::Translator>)> for GraphMapEdgesTy<'a> {
    fn from(value: (&[GraphMapEdges], Rc<translator::Translator>)) -> Self {
	Self::from((value.0.to_vec(), value.1))
    }
}

impl<'a> From<(Vec<GraphMapEdges>, Rc<translator::Translator>)> for GraphMapEdgesTy<'a> {
    fn from(value: (Vec<GraphMapEdges>, Rc<translator::Translator>)) -> Self {
	use GraphMapEdges::*;
	use super::format_helpers::graph_map_edges_ty_from::*;

	let mut new = GraphMapEdgesTy {functions: vec![], translator: value.1};

	for f in value.0 {
	    match f {
		String { string } => {
		    new.functions.push(format_string(
			Rc::clone(&new.translator), string))
		}
		Hide => {
		    new.functions.push(format_hide(
			Rc::clone(&new.translator)
		    ))
		},
		Products => {
		    new.functions.push(format_products(
			Rc::clone(&new.translator)
		    ))
		},
		MaskProducts { mask } => {
		    new.functions.push(format_mask_products(
			Rc::clone(&new.translator), mask))
		},
		Entities => {
		    new.functions.push(format_entities(
			Rc::clone(&new.translator)
		    ))
		},
		MaskEntities { mask } => {
		    new.functions.push(format_mask_entities(
			Rc::clone(&new.translator), mask))
		},
		Context => {
		    new.functions.push(format_context(
			Rc::clone(&new.translator)
		    ))
		},
		MaskContext { mask } => {
		    new.functions.push(format_mask_context(
			Rc::clone(&new.translator), mask))
		},
		Union => {
		    new.functions.push(format_union(
			Rc::clone(&new.translator)
		    ))
		},
		MaskUnion { mask } => {
		    new.functions.push(format_mask_union(
			Rc::clone(&new.translator), mask))
		},
		Difference => {
		    new.functions.push(format_difference(
			Rc::clone(&new.translator)
		    ))
		},
		MaskDifference { mask } => {
		    new.functions.push(format_mask_difference(
			Rc::clone(&new.translator), mask))
		},
		EntitiesDeleted => {
		    new.functions.push(format_entities_deleted(
			Rc::clone(&new.translator)
		    ))
		},
		MaskEntitiesDeleted { mask } => {
		    new.functions.push(format_mask_entities_deleted(
			Rc::clone(&new.translator), mask))
		},
		EntitiesAdded => {
		    new.functions.push(format_entities_added(
			Rc::clone(&new.translator)
		    ))
		},
		MaskEntitiesAdded { mask } => {
		    new.functions.push(format_mask_entities_added(
			Rc::clone(&new.translator), mask))
		},
	    };
	}

	new
    }
}

impl<'a> GraphMapEdgesTy<'a> {
    pub fn generate(self) -> Box<GraphMapEdgesFnTy<'a>> {
	let mut accumulator: Box<GraphMapEdgesFnTy<'a>> =
	    super::format_helpers::graph_map_edges_ty_from::format_hide(
		Rc::clone(&self.translator)
	    );
	for f in self.functions {
	    accumulator = Box::new(move |i, n| {
		(accumulator)(i, n)
		    + &f(i, n)
	    })
	}

	accumulator
    }
}

// -----------------------------------------------------------------------------
//                            Color Nodes & Edges
// -----------------------------------------------------------------------------


// Node ------------------------------------------------------------------------
use petgraph::visit::{IntoEdgeReferences, IntoNodeReferences};

type RSdotGraph = Graph<String, String, Directed, u32>;
type RSformatNodeTy<'a> =
    dyn Fn(
	&'a RSdotGraph,
	<&'a RSdotGraph as IntoNodeReferences>::NodeRef
    ) -> String + 'a;
type RSformatNodeTyOpt<'a> =
    dyn Fn(
	&'a RSdotGraph,
	<&'a RSdotGraph as IntoNodeReferences>::NodeRef
    ) -> Option<String> + 'a;

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
    pub base_color: String,
}

#[inline(always)]
fn node_formatter_base_color(
    base_color: String
) -> String
{
    ", fillcolor=".to_string() + &base_color
}

#[inline(always)]
fn match_node_color_conditional<'a>(
    rule: &'a NodeColorConditional,
    color: &'a String,
    original_graph: Rc<RSgraph>,
    star: Option<IdType>
) -> Box<RSformatNodeTyOpt<'a>> {
    use super::format_helpers::node_formatter::*;
    match rule {
	NodeColorConditional::ContextConditional(ccc) => {
	    match ccc {
		ContextColorConditional::Nill => {
		    format_nill(Rc::clone(&original_graph),
				color.to_string(),
				star)
		},
		ContextColorConditional::RecursiveIdentifier(s) => {
		    format_recursive_identifier(Rc::clone(&original_graph),
						color.to_string(),
						star,
						*s)
		},
		ContextColorConditional::EntitySet(ot, set) => {
		    format_entity_set(Rc::clone(&original_graph),
				      color.to_string(),
				      star,
				      *ot,
				      set.clone())
		},
		ContextColorConditional::NonDeterministicChoice => {
		    format_non_deterministic_choice(Rc::clone(&original_graph),
						    color.to_string(),
						    star)
		},
		ContextColorConditional::Summation => {
		    format_summation(Rc::clone(&original_graph),
				     color.to_string(),
				     star)
		},
		ContextColorConditional::WaitEntity => {
		    format_wait_entity(Rc::clone(&original_graph),
				       color.to_string(),
				       star)
		},
	    }
	},
	NodeColorConditional::EntitiesConditional(ot, set) => {
	    format_entities_conditional(Rc::clone(&original_graph),
					color.to_string(),
					star,
					*ot,
					set.clone())
	},
    }
}

impl NodeColor {
    pub fn generate<'a>(
	self,
	original_graph: Rc<RSgraph>,
	star: Option<IdType>
    ) -> Box<RSformatNodeTy<'a>> {
	Box::new(
	    move |i, n| {
		for (rule, color) in &self.conditionals {
		    let f = match_node_color_conditional(
			rule,
			color,
			Rc::clone(&original_graph),
			star
		    );

		    if let Some(s) = (f)(i, n) {
			return s
		    }
		}
		node_formatter_base_color(self.base_color.clone())
	    }
	)
    }
}


// Edge ------------------------------------------------------------------------

type RSformatEdgeTy<'a> =
    dyn Fn(
	&'a RSdotGraph,
	<&'a RSdotGraph as IntoEdgeReferences>::EdgeRef
    ) -> String + 'a;
type RSformatEdgeTyOpt<'a> =
    dyn Fn(
	&'a RSdotGraph,
	<&'a RSdotGraph as IntoEdgeReferences>::EdgeRef
    ) -> Option<String> + 'a;


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


fn edge_formatter_base_color(
    base_color: String
) -> String
{
    ", color=".to_string() + &base_color
}

fn match_edge_color_conditional<'a>(
    rule: &'a EdgeColorConditional,
    color: &'a String,
    original_graph: Rc<RSgraph>
) -> Box<RSformatEdgeTyOpt<'a>> {
    use super::format_helpers::edge_formatter::*;
    match rule {
	EdgeColorConditional::Entities(ot, set) => {
	    format_entities(Rc::clone(&original_graph),
			    color.to_string(),
			    *ot,
			    set.clone())
	},
	EdgeColorConditional::Context(ot, set) => {
	    format_context(Rc::clone(&original_graph),
			   color.to_string(),
			   *ot,
			   set.clone())
	},
	EdgeColorConditional::T(ot, set) => {
	    format_t(Rc::clone(&original_graph),
		     color.to_string(),
		     *ot,
		     set.clone())
	},
	EdgeColorConditional::Reactants(ot, set) => {
	    format_reactants(Rc::clone(&original_graph),
			     color.to_string(),
			     *ot,
			     set.clone())
	},
	EdgeColorConditional::ReactantsAbsent(ot, set) => {
	    format_reactants_absent(Rc::clone(&original_graph),
				    color.to_string(),
				    *ot,
				    set.clone())
	},
	EdgeColorConditional::Inhibitors(ot, set) => {
	    format_inhibitors(Rc::clone(&original_graph),
			      color.to_string(),
			      *ot,
			      set.clone())
	},
	EdgeColorConditional::InhibitorsPresent(ot, set) => {
	    format_inhibitors_present(Rc::clone(&original_graph),
				      color.to_string(),
				      *ot,
				      set.clone())
	},
	EdgeColorConditional::Products(ot, set) => {
	    format_products(Rc::clone(&original_graph),
			    color.to_string(),
			    *ot,
			    set.clone())
	},
    }
}

impl EdgeColor {
    pub fn generate<'a>(
	self,
	original_graph: Rc<RSgraph>,
    ) -> Box<RSformatEdgeTy<'a>> {
	Box::new(
	    move |i, n| {
		for (rule, color) in &self.conditionals {
		    let f = match_edge_color_conditional(
			rule,
			color,
			Rc::clone(&original_graph),
		    );

		    if let Some(s) = (f)(i, n) {
			return s
		    }
		}
		edge_formatter_base_color(self.base_color.clone())
	    }
	)
    }
}

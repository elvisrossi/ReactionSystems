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

    while let Some(current) = stack.pop() {
	// depth first
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


fn common_system_entities(graph: &RSgraph) -> RSset {
    graph.node_references().fold(
	None,
	|acc, node|
	match acc {
	    None => Some(node.1.available_entities.clone()),
	    Some(acc) => Some(node.1.available_entities.intersection(&acc))
	}
    ).unwrap_or(RSset::new())
}

macro_rules! common_label {
    (
	$name:ident,
	[$edge_name:ident, $acc_name:ident],
	$empty_expr:expr,
	$some_expr:expr
    ) => {
	fn $name(graph: &RSgraph) -> RSset {
	    graph.edge_references().fold(
		None,
		|$acc_name, $edge_name| {
		    let $edge_name = $edge_name.weight();
		    match $acc_name {
			None => Some($empty_expr),
			Some($acc_name) => Some($some_expr)
		    }
		}
	    ).unwrap_or(RSset::new())
	}
    };
}

common_label!(
    common_label_products,
    [edge, acc],
    edge.products.clone(),
    edge.products.intersection(&acc)
);
common_label!(
    common_label_entities,
    [edge, acc],
    edge.available_entities.clone(),
    edge.available_entities.intersection(&acc)
);
common_label!(
    common_label_context,
    [edge, acc],
    edge.context.clone(),
    edge.context.intersection(&acc)
);
common_label!(
    common_label_union,
    [edge, acc],
    edge.t.clone(),
    edge.t.intersection(&acc)
);
common_label!(
    common_label_difference,
    [edge, acc],
    edge.context.subtraction(&edge.available_entities),
    edge.context.subtraction(&edge.available_entities).intersection(&acc)
);
common_label!(
    common_label_entities_deleted,
    [edge, acc],
    edge.available_entities.subtraction(&edge.products),
    edge.available_entities.subtraction(&edge.products).intersection(&acc)
);
common_label!(
    common_label_entities_added,
    [edge, acc],
    edge.products.subtraction(&edge.available_entities),
    edge.products.subtraction(&edge.available_entities).intersection(&acc)
);

// -----------------------------------------------------------------------------
//                              helper functions
// -----------------------------------------------------------------------------

/// Very inelegant way to provide our graph with a map method where the edges
/// are mapped until the first error.
pub trait MapEdges<'a, N: 'a, E, Ty, Ix>
where
    Ty: petgraph::EdgeType,
    Ix: petgraph::graph::IndexType
{
    fn map_edges(
	&self,
	edge_map: &super::assert::RSassert,
	translator: &mut super::translator::Translator
    ) -> Result<Graph<RSsystem, super::assert::AssertReturnValue, Ty, Ix>, String>;
}

impl<'a> MapEdges<'a, RSsystem, RSlabel, Directed, u32>
    for RSgraph
{
    fn map_edges(
	&self,
	edge_map: &super::assert::RSassert,
	translator: &mut super::translator::Translator
    )-> Result<Graph<RSsystem, super::assert::AssertReturnValue, Directed, u32>, String> {
	use petgraph::graph::EdgeIndex;

	let mut g = Graph::with_capacity(self.node_count(), self.edge_count());
	let nodes = self.raw_nodes();
	let edges = self.raw_edges();

	let edges = edges.iter().enumerate().map(
	    |(i, edge)|
	    match edge_map.execute(self, &EdgeIndex::new(i), translator) {
		Err(e) => Err(e),
		Ok(val) => Ok((edge.source(), edge.target(), val))
	    }
	).collect::<Result<Vec<_>, _>>()?;
	nodes.iter().for_each(|node| { g.add_node(node.weight.clone()); });

	edges.into_iter().for_each(|(source, target, v)| { g.add_edge(source, target, v); });

	Ok(g)
    }
}


// Nodes -----------------------------------------------------------------------

/// Helper structure that specifies what information to display for nodes.
#[derive(Clone)]
pub enum NodeDisplayBase {
    String { string: String },
    Hide,
    Entities,
    MaskEntities { mask: RSset },
    ExcludeEntities { mask: RSset },
    Context,
    UncommonEntities,
    MaskUncommonEntities { mask: RSset }
}

pub struct NodeDisplay {
    pub base: Vec<NodeDisplayBase>
}

type GraphMapNodesFnTy<'a> =
    dyn Fn(petgraph::prelude::NodeIndex, &'a RSsystem) -> String + 'a;


fn match_node_display<'a>(
    base: &NodeDisplayBase,
    common_entities: Rc<RSset>,
    translator: Rc<translator::Translator>
) -> Box<GraphMapNodesFnTy<'a>> {
    use NodeDisplayBase::*;
    use super::format_helpers::graph_map_nodes_ty_from::*;

    match base {
	String { string } => {
	    format_string(string.clone())
	},
	Hide => {
	    format_hide(translator)
	},
	Entities => {
	    format_entities(translator)
	},
	MaskEntities { mask } => {
	    format_mask_entities(translator, mask.clone())
	},
	ExcludeEntities { mask } => {
	    format_exclude_entities(translator, mask.clone())
	},
	Context => {
	    format_context(translator)
	},
	UncommonEntities => {
	    format_exclude_entities(translator, (*common_entities).clone())
	},
	MaskUncommonEntities { mask } => {
	    format_exclude_entities(translator,
				    mask.intersection(&common_entities))
	}
    }
}


impl NodeDisplay {
    fn contains_uncommon(&self) -> bool {
	self.base.iter().any(
	    |b|
	    matches!(b, NodeDisplayBase::UncommonEntities |
			NodeDisplayBase::MaskUncommonEntities { mask: _ }))
    }

    pub fn generate<'a>(
	self,
	translator: Rc<translator::Translator>,
	current_graph: &RSgraph
    ) -> Box<GraphMapNodesFnTy<'a>> {
	let common_entities =
	    if self.contains_uncommon() {
		Rc::new(common_system_entities(current_graph))
	    } else {
		Rc::new(RSset::new())
	    };

	Box::new(
	    move |i, n| {
		let mut accumulator = String::new();
		for b in &self.base {
		    let f = match_node_display(b,
					       Rc::clone(&common_entities),
					       Rc::clone(&translator));

		    accumulator.push_str(&(f)(i, n));
		}
		accumulator
	    }
	)
    }
}


// Edges -----------------------------------------------------------------------

#[derive(Clone)]
pub enum EdgeDisplayBase {
    String { string: String },
    Hide,
    Products { mask: Option<RSset>, filter_common: bool },
    Entities { mask: Option<RSset>, filter_common: bool },
    Context { mask: Option<RSset>, filter_common: bool },
    Union { mask: Option<RSset>, filter_common: bool },
    Difference { mask: Option<RSset>, filter_common: bool },
    EntitiesDeleted { mask: Option<RSset>, filter_common: bool },
    EntitiesAdded { mask: Option<RSset>, filter_common: bool },
}

pub struct EdgeDisplay {
    pub base: Vec<EdgeDisplayBase>,
}

type GraphMapEdgesFnTy<'a> =
    dyn Fn(petgraph::prelude::EdgeIndex, &'a RSlabel) -> String + 'a;

#[derive(Default, Clone)]
struct CommonEntities {
    common_products: RSset,
    common_entities: RSset,
    common_context: RSset,
    common_union: RSset,
    common_difference: RSset,
    common_entities_deleted: RSset,
    common_entities_added: RSset,
}

fn match_edge_display<'a>(
    base: &'a EdgeDisplayBase,
    translator: Rc<translator::Translator>,
    common: CommonEntities
) -> Box<GraphMapEdgesFnTy<'a>> {
    use EdgeDisplayBase::*;
    use super::format_helpers::graph_map_edges_ty_from::*;

    match base {
	String { string } => {
	    format_string(translator, string.clone())
	}
	Hide => {
	    format_hide(translator)
	},
	Products { mask, filter_common } => {
	    if *filter_common {
		format_products(translator, mask.clone(),
				Some(common.common_products))
	    } else {
		format_products(translator, mask.clone(), None)
	    }
	},
	Entities { mask, filter_common } => {
	    if *filter_common {
		format_entities(translator, mask.clone(),
				Some(common.common_entities))
	    } else {
		format_entities(translator, mask.clone(), None)
	    }
	},
	Context { mask, filter_common } => {
	    if *filter_common {
		format_context(translator, mask.clone(),
			       Some(common.common_context))
	    } else {
		format_context(translator, mask.clone(), None)
	    }
	},
	Union { mask, filter_common } => {
	    if *filter_common {
		format_union(translator, mask.clone(),
			     Some(common.common_union))
	    } else {
		format_union(translator, mask.clone(), None)
	    }
	},
	Difference { mask, filter_common } => {
	    if *filter_common {
		format_difference(translator, mask.clone(),
				  Some(common.common_difference))
	    } else {
		format_difference(translator, mask.clone(), None)
	    }
	},
	EntitiesDeleted { mask, filter_common } => {
	    if *filter_common {
		format_entities_deleted(translator, mask.clone(),
					Some(common.common_entities_deleted))
	    } else {
		format_entities_deleted(translator, mask.clone(), None)
	    }
	},
	EntitiesAdded { mask, filter_common } => {
	    if *filter_common {
		format_entities_added(translator, mask.clone(),
				      Some(common.common_entities_added))
	    } else {
		format_entities_added(translator, mask.clone(), None)
	    }
	},
    }
}

macro_rules! common_entity {
    ($name:ident, $match:pat, $filter_common:ident) => {
	fn $name(&self) -> bool {
	    self.base.iter().any(
		|b|
		if let $match = b {
		    *$filter_common
		} else {
		    false
		}
	    )
	}
    };
}


impl EdgeDisplay {
    common_entity!(common_products,
		   EdgeDisplayBase::Products {mask: _, filter_common},
		   filter_common);
    common_entity!(common_entities,
		   EdgeDisplayBase::Entities {mask: _, filter_common},
		   filter_common);
    common_entity!(common_context,
		   EdgeDisplayBase::Context {mask: _, filter_common},
		   filter_common);
    common_entity!(common_union,
		   EdgeDisplayBase::Union {mask: _, filter_common},
		   filter_common);
    common_entity!(common_difference,
		   EdgeDisplayBase::Difference {mask: _, filter_common},
		   filter_common);
    common_entity!(common_entities_deleted,
		   EdgeDisplayBase::EntitiesDeleted {mask: _, filter_common},
		   filter_common);
    common_entity!(common_entities_added,
		   EdgeDisplayBase::EntitiesAdded {mask: _, filter_common},
		   filter_common);


    pub fn generate<'a>(
	self,
	translator: Rc<translator::Translator>,
	current_graph: &RSgraph
    ) -> Box<GraphMapEdgesFnTy<'a>> {
	// create the structure for common entities if required
	let common = {
	    let mut tmp = CommonEntities::default();
	    if self.common_products() {
		tmp.common_products = common_label_products(current_graph);
	    }
	    if self.common_entities() {
		tmp.common_entities = common_label_entities(current_graph);
	    }
	    if self.common_context() {
		tmp.common_context = common_label_context(current_graph);
	    }
	    if self.common_union() {
		tmp.common_union = common_label_union(current_graph);
	    }
	    if self.common_difference() {
		tmp.common_difference = common_label_difference(current_graph);
	    }
	    if self.common_entities_deleted() {
		tmp.common_entities_deleted = common_label_entities_deleted(current_graph);
	    }
	    if self.common_entities_added() {
		tmp.common_entities_added = common_label_entities_added(current_graph);
	    }
	    tmp
	};

	Box::new(
	    move |i, n| {
		let mut accumulator = String::new();
		for b in &self.base {
		    let f = match_edge_display(b,
					       Rc::clone(&translator),
					       common.clone());
		    accumulator.push_str(&(f)(i, n));
		}
		accumulator
	    }
	)
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

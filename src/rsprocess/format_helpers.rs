pub mod graph_map_nodes_ty_from {
    use super::super::structure::{RSsystem, RSset};
    use super::super::translator;
    use std::rc::Rc;

    type GraphMapNodesFnTy =
	dyn Fn(petgraph::prelude::NodeIndex, &RSsystem) -> String;

    pub fn format_string(
	s: String
    ) -> Box<GraphMapNodesFnTy> {
	Box::new(move |_, _| s.clone())
    }

    pub fn format_hide(
	_translator: Rc<translator::Translator>
    ) -> Box<GraphMapNodesFnTy> {
	Box::new(|_, _| String::new())
    }

    pub fn format_entities(
	translator: Rc<translator::Translator>
    ) -> Box<GraphMapNodesFnTy> {
	Box::new(
	    move |_, node: &RSsystem|
	    format!("{}",
		    translator::RSsetDisplay::from(&translator,
						   &node.available_entities))
	)
    }

    pub fn format_mask_entities(
	translator: Rc<translator::Translator>,
	mask: RSset
    ) -> Box<GraphMapNodesFnTy> {
	Box::new(
	    move |_, node: &RSsystem| {
		let masked_entities =
		    node.available_entities.intersection(&mask);
		format!("{}",
			translator::RSsetDisplay::from(&translator,
						       &masked_entities))
	    }
	)
    }

    pub fn format_exclude_entities(
	translator: Rc<translator::Translator>,
	mask: RSset
    ) -> Box<GraphMapNodesFnTy> {
	Box::new(
	    move |_, node: &RSsystem| {
		let masked_entities =
		    node.available_entities.subtraction(&mask);
		format!("{}",
			translator::RSsetDisplay::from(&translator,
						       &masked_entities))
	    }
	)
    }

    pub fn format_context(
	translator: Rc<translator::Translator>
    ) -> Box<GraphMapNodesFnTy> {
	Box::new(
	    move |_, node: &RSsystem|
	    format!("{}",
		    translator::RSprocessDisplay::from(&translator,
						       &node.context_process))
	)
    }
}


pub mod graph_map_edges_ty_from {
    use super::super::structure::{RSlabel, RSset};
    use super::super::translator;
    use std::rc::Rc;

    type GraphMapEdgesFnTy<'a> =
	dyn Fn(petgraph::prelude::EdgeIndex, &'a RSlabel) -> String + 'a;

    pub fn format_string<'a>(
	_translator: Rc<translator::Translator>,
	s: String
    ) -> Box<GraphMapEdgesFnTy<'a>> {
	Box::new(move |_, _| s.clone())
    }

    pub fn format_hide<'a>(
	_translator: Rc<translator::Translator>
    ) -> Box<GraphMapEdgesFnTy<'a>> {
	Box::new(|_, _| String::new())
    }

    macro_rules! create_format_edge {
	( $name:ident,
	  [$edge_name:ident, $mask_name:ident, $common_name:ident],
	  $mask_common:expr,
	  $mask:expr,
	  $common:expr,
	  $default:expr )
	    =>
	{
	    pub fn $name<'a>(
		translator: Rc<translator::Translator>,
		$mask_name: Option<RSset>,
		$common_name: Option<RSset>
	    ) -> Box<GraphMapEdgesFnTy<'a>> {
		if let Some($mask_name) = $mask_name {
		    if let Some($common_name) = $common_name {
			Box::new(
			    move |_, $edge_name: &RSlabel|
			    format!("{}",
				    translator::RSsetDisplay::from(
					&translator, $mask_common
				    ))
			)
		    } else {
			Box::new(
			    move |_, $edge_name: &RSlabel|
			    format!("{}",
				    translator::RSsetDisplay::from(
					&translator, $mask
				    ))
			)
		    }
		} else {
		    if let Some($common_name) = $common_name {
			Box::new(
			    move |_, $edge_name: &RSlabel|
			    format!("{}",
				    translator::RSsetDisplay::from(
					&translator, $common
				    ))
			)
		    } else {
			Box::new(
			    move |_, $edge_name: &RSlabel|
			    format!("{}",
				    translator::RSsetDisplay::from(
					&translator, $default
				    ))
			)
		    }
		}
	    }
	}
    }

    create_format_edge!(
	format_products,
	[edge, mask, common],
	&mask.intersection(&edge.products).subtraction(&common),
	&mask.intersection(&edge.products),
	&edge.products.subtraction(&common),
	&edge.products
    );

    create_format_edge!(
	format_entities,
	[edge, mask, common],
	&mask.intersection(&edge.available_entities).subtraction(&common),
	&mask.intersection(&edge.available_entities),
	&edge.available_entities.subtraction(&common),
	&edge.available_entities
    );

    create_format_edge!(
	format_context,
	[edge, mask, common],
	&mask.intersection(&edge.context).subtraction(&common),
	&mask.intersection(&edge.context),
	&edge.context.subtraction(&common),
	&edge.context
    );

    create_format_edge!(
	format_union,
	[edge, mask, common],
	&mask.intersection(&edge.t).subtraction(&common),
	&mask.intersection(&edge.t),
	&edge.t.subtraction(&common),
	&edge.t
    );

    create_format_edge!(
	format_difference,
	[edge, mask, common],
	&mask.intersection(&edge.context.subtraction(&edge.available_entities))
			   .subtraction(&common),
	&mask.intersection(&edge.context.subtraction(&edge.available_entities)),
	&edge.context.subtraction(&edge.available_entities).subtraction(&common),
	&edge.context.subtraction(&edge.available_entities)
    );

    create_format_edge!(
	format_entities_deleted,
	[edge, mask, common],
	&mask.intersection(&edge.available_entities.subtraction(&edge.products))
	    .subtraction(&common),
	&mask.intersection(&edge.available_entities.subtraction(&edge.products)),
	&edge.available_entities.subtraction(&edge.products).subtraction(&common),
	&edge.available_entities.subtraction(&edge.products)
    );

    create_format_edge!(
	format_entities_added,
	[edge, mask, common],
	&mask.intersection(&edge.products.subtraction(&edge.available_entities))
	    .subtraction(&common),
	&mask.intersection(&edge.products.subtraction(&edge.available_entities)),
	&edge.products.subtraction(&edge.available_entities).subtraction(&common),
	&edge.products.subtraction(&edge.available_entities)
    );
}


pub mod node_formatter {
    use super::super::translator::IdType;
    use super::super::graph::{RSgraph, OperationType};
    use std::rc::Rc;
    use super::super::structure::{RSset, RSprocess};

    use petgraph::{Graph, Directed};
    use petgraph::visit::IntoNodeReferences;


    type RSdotGraph = Graph<String, String, Directed, u32>;
    type RSformatNodeTy =
	dyn Fn(
	    &RSdotGraph,
	    <&RSdotGraph as IntoNodeReferences>::NodeRef
	) -> Option<String>;

    pub fn format_nill(
	original_graph: Rc<RSgraph>,
	color: String,
	_star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
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
    }

    pub fn format_recursive_identifier(
	original_graph: Rc<RSgraph>,
	color: String,
	star: Option<IdType>,
	s: IdType
    ) -> Box<RSformatNodeTy> {
	Box::new(
	    move |_, n| {
		let rssystem = original_graph.node_weight(n.0).unwrap();
		match (Some(s) == star, &rssystem.context_process) {
		    (true, RSprocess::RecursiveIdentifier { identifier: _ })
			=> {
			    Some(", fillcolor=".to_string() + &color)
			},
		    (false, RSprocess::RecursiveIdentifier { identifier: id })
			if id == &s => {
			    Some(", fillcolor=".to_string() + &color)
			},
		    _ => {None}
		}
	    }
	)
    }

    pub fn format_entity_set(
	original_graph: Rc<RSgraph>,
	color: String,
	_star: Option<IdType>,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatNodeTy> {
	Box::new(
	    move |_, n| {
		let rssystem = original_graph.node_weight(n.0).unwrap();
		match &rssystem.context_process {
		    RSprocess::EntitySet { entities, next_process: _ }
		    if ot.evaluate(entities, &set) => {
			Some(", fillcolor=".to_string() + &color)
		    },
		    _ => {None}
		}
	    }
	)
    }


    pub fn format_non_deterministic_choice(
	original_graph: Rc<RSgraph>,
	color: String,
	_star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
	Box::new(
	    move |_, n| {
		let rssystem = original_graph.node_weight(n.0).unwrap();
		if let RSprocess::NondeterministicChoice { children: _ } =
		    rssystem.context_process
		{
		    Some(", fillcolor=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_summation(
	original_graph: Rc<RSgraph>,
	color: String,
	_star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
	Box::new(
	    move |_, n| {
		let rssystem = original_graph.node_weight(n.0).unwrap();
		if let RSprocess::Summation { children: _ } =
		    rssystem.context_process
		{
		    Some(", fillcolor=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }


    pub fn format_wait_entity(
	original_graph: Rc<RSgraph>,
	color: String,
	_star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
	Box::new(
	    move |_, n| {
		let rssystem = original_graph.node_weight(n.0).unwrap();
		if let RSprocess::WaitEntity { repeat: _,
					       repeated_process: _,
					       next_process: _ } =
		    &rssystem.context_process
		{
		    Some(", fillcolor=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_entities_conditional(
	original_graph: Rc<RSgraph>,
	color: String,
	_star: Option<IdType>,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatNodeTy> {
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
    }
}

pub mod edge_formatter {
    use super::super::graph::{RSgraph, OperationType};
    use std::rc::Rc;
    use super::super::structure::RSset;

    use petgraph::{Graph, Directed};
    use petgraph::visit::{IntoEdgeReferences, EdgeRef};

    type RSdotGraph = Graph<String, String, Directed, u32>;
    type RSformatEdgeTy =
	dyn Fn(
	    &RSdotGraph,
	    <&RSdotGraph as IntoEdgeReferences>::EdgeRef
	) -> Option<String>;


    pub fn format_entities(
	original_graph: Rc<RSgraph>,
	color: String,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatEdgeTy> {
	Box::new(
	    move |_, e| {
		let rssystem = original_graph.edge_weight(e.id()).unwrap();
		if ot.evaluate(&rssystem.available_entities, &set) {
		    Some(", color=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_context(
	original_graph: Rc<RSgraph>,
	color: String,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatEdgeTy> {
	Box::new(
	    move |_, e| {
		let rssystem = original_graph.edge_weight(e.id()).unwrap();
		if ot.evaluate(&rssystem.context, &set) {
		    Some(", color=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_t(
	original_graph: Rc<RSgraph>,
	color: String,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatEdgeTy> {
	Box::new(
	    move |_, e| {
		let rssystem = original_graph.edge_weight(e.id()).unwrap();
		if ot.evaluate(&rssystem.t, &set) {
		    Some(", color=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_reactants(
	original_graph: Rc<RSgraph>,
	color: String,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatEdgeTy> {
	Box::new(
	    move |_, e| {
		let rssystem = original_graph.edge_weight(e.id()).unwrap();
		if ot.evaluate(&rssystem.reactants, &set) {
		    Some(", color=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_reactants_absent(
	original_graph: Rc<RSgraph>,
	color: String,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatEdgeTy> {
	Box::new(
	    move |_, e| {
		let rssystem = original_graph.edge_weight(e.id()).unwrap();
		if ot.evaluate(&rssystem.reactants_absent, &set) {
		    Some(", color=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_inhibitors(
	original_graph: Rc<RSgraph>,
	color: String,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatEdgeTy> {
	Box::new(
	    move |_, e| {
		let rssystem = original_graph.edge_weight(e.id()).unwrap();
		if ot.evaluate(&rssystem.inhibitors, &set) {
		    Some(", color=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_inhibitors_present(
	original_graph: Rc<RSgraph>,
	color: String,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatEdgeTy> {
	Box::new(
	    move |_, e| {
		let rssystem = original_graph.edge_weight(e.id()).unwrap();
		if ot.evaluate(&rssystem.inhibitors_present, &set) {
		    Some(", color=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }

    pub fn format_products(
	original_graph: Rc<RSgraph>,
	color: String,
	ot: OperationType,
	set: RSset
    ) -> Box<RSformatEdgeTy> {
	Box::new(
	    move |_, e| {
		let rssystem = original_graph.edge_weight(e.id()).unwrap();
		if ot.evaluate(&rssystem.products, &set) {
		    Some(", color=".to_string() + &color)
		} else {
		    None
		}
	    }
	)
    }
}

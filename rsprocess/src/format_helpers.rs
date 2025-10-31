pub mod graph_map_nodes_ty_from {
    use std::sync::Arc;

    use super::super::set::{BasicSet, Set};
    use super::super::system::System;
    use super::super::translator;

    type GraphMapNodesFnTy =
        dyn Fn(petgraph::prelude::NodeIndex, &System) -> String;

    pub fn format_string(s: String) -> Box<GraphMapNodesFnTy> {
        Box::new(move |_, _| s.clone())
    }

    pub fn format_hide(
        _translator: Arc<translator::Translator>,
    ) -> Box<GraphMapNodesFnTy> {
        Box::new(|_, _| String::new())
    }

    pub fn format_entities(
        translator: Arc<translator::Translator>,
    ) -> Box<GraphMapNodesFnTy> {
        Box::new(move |_, node: &System| {
            format!(
                "{}",
                translator::Formatter::from(
                    &translator,
                    &node.available_entities
                )
            )
        })
    }

    pub fn format_mask_entities(
        translator: Arc<translator::Translator>,
        mask: Set,
    ) -> Box<GraphMapNodesFnTy> {
        Box::new(move |_, node: &System| {
            let masked_entities = node.available_entities.intersection(&mask);
            format!(
                "{}",
                translator::Formatter::from(&translator, &masked_entities)
            )
        })
    }

    pub fn format_exclude_entities(
        translator: Arc<translator::Translator>,
        mask: Set,
    ) -> Box<GraphMapNodesFnTy> {
        Box::new(move |_, node: &System| {
            let masked_entities = node.available_entities.subtraction(&mask);
            format!(
                "{}",
                translator::Formatter::from(&translator, &masked_entities)
            )
        })
    }

    pub fn format_context(
        translator: Arc<translator::Translator>,
    ) -> Box<GraphMapNodesFnTy> {
        Box::new(move |_, node: &System| {
            format!(
                "{}",
                translator::Formatter::from(&translator, &node.context_process)
            )
        })
    }
}

pub mod graph_map_edges_ty_from {
    use std::sync::Arc;

    use super::super::label::Label;
    use super::super::set::{BasicSet, Set};
    use super::super::translator;

    type GraphMapEdgesFnTy<'a> =
        dyn Fn(petgraph::prelude::EdgeIndex, &'a Label) -> String + 'a;

    pub fn format_string<'a>(
        _translator: Arc<translator::Translator>,
        s: String,
    ) -> Box<GraphMapEdgesFnTy<'a>> {
        Box::new(move |_, _| s.clone())
    }

    pub fn format_hide<'a>(
        _translator: Arc<translator::Translator>,
    ) -> Box<GraphMapEdgesFnTy<'a>> {
        Box::new(|_, _| String::new())
    }

    macro_rules! create_format_edge {
        ( $name:ident,
	  [$edge_name:ident, $mask_name:ident, $common_name:ident],
	  $mask_common:expr,
	  $mask:expr,
	  $common:expr,
	  $default:expr ) => {
            pub fn $name<'a>(
                translator: Arc<translator::Translator>,
                $mask_name: Option<Set>,
                $common_name: Option<Set>,
            ) -> Box<GraphMapEdgesFnTy<'a>> {
                if let Some($mask_name) = $mask_name {
                    if let Some($common_name) = $common_name {
                        Box::new(move |_, $edge_name: &Label| {
                            format!(
                                "{}",
                                translator::Formatter::from(
                                    &translator,
                                    $mask_common
                                )
                            )
                        })
                    } else {
                        Box::new(move |_, $edge_name: &Label| {
                            format!(
                                "{}",
                                translator::Formatter::from(&translator, $mask)
                            )
                        })
                    }
                } else {
                    if let Some($common_name) = $common_name {
                        Box::new(move |_, $edge_name: &Label| {
                            format!(
                                "{}",
                                translator::Formatter::from(
                                    &translator,
                                    $common
                                )
                            )
                        })
                    } else {
                        Box::new(move |_, $edge_name: &Label| {
                            format!(
                                "{}",
                                translator::Formatter::from(
                                    &translator,
                                    $default
                                )
                            )
                        })
                    }
                }
            }
        };
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
        &mask
            .intersection(&edge.available_entities)
            .subtraction(&common),
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
        &mask
            .intersection(&edge.context.subtraction(&edge.available_entities))
            .subtraction(&common),
        &mask.intersection(&edge.context.subtraction(&edge.available_entities)),
        &edge
            .context
            .subtraction(&edge.available_entities)
            .subtraction(&common),
        &edge.context.subtraction(&edge.available_entities)
    );

    create_format_edge!(
        format_entities_deleted,
        [edge, mask, common],
        &mask
            .intersection(&edge.available_entities.subtraction(&edge.products))
            .subtraction(&common),
        &mask
            .intersection(&edge.available_entities.subtraction(&edge.products)),
        &edge
            .available_entities
            .subtraction(&edge.products)
            .subtraction(&common),
        &edge.available_entities.subtraction(&edge.products)
    );

    create_format_edge!(
        format_entities_added,
        [edge, mask, common],
        &mask
            .intersection(&edge.products.subtraction(&edge.available_entities))
            .subtraction(&common),
        &mask
            .intersection(&edge.products.subtraction(&edge.available_entities)),
        &edge
            .products
            .subtraction(&edge.available_entities)
            .subtraction(&common),
        &edge.products.subtraction(&edge.available_entities)
    );
}

pub mod node_formatter {
    use std::sync::Arc;

    use petgraph::visit::IntoNodeReferences;
    use petgraph::{Directed, Graph};

    use super::super::element::IdType;
    use super::super::graph::{OperationType, SystemGraph};
    use super::super::process::Process;
    use super::super::set::Set;

    type RSdotGraph = Graph<String, String, Directed, u32>;
    type RSformatNodeTy = dyn Fn(
        &RSdotGraph,
        <&RSdotGraph as IntoNodeReferences>::NodeRef,
    ) -> Option<String>;

    pub fn format_nill(
        original_graph: Arc<SystemGraph>,
        color: String,
        _star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if rssystem.context_process == Process::Nill {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_recursive_identifier(
        original_graph: Arc<SystemGraph>,
        color: String,
        star: Option<IdType>,
        s: IdType,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            match (Some(s) == star, &rssystem.context_process) {
                | (true, Process::RecursiveIdentifier { identifier: _ }) =>
                    Some(", fillcolor=".to_string() + &color),
                | (false, Process::RecursiveIdentifier { identifier: id })
                    if id == &s =>
                    Some(", fillcolor=".to_string() + &color),
                | _ => None,
            }
        })
    }

    pub fn format_entity_set(
        original_graph: Arc<SystemGraph>,
        color: String,
        _star: Option<IdType>,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            match &rssystem.context_process {
                | Process::EntitySet {
                    entities,
                    next_process: _,
                } if ot.evaluate(entities, &set) =>
                    Some(", fillcolor=".to_string() + &color),
                | _ => None,
            }
        })
    }

    pub fn format_non_deterministic_choice(
        original_graph: Arc<SystemGraph>,
        color: String,
        _star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if let Process::NondeterministicChoice { children: _ } =
                rssystem.context_process
            {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_summation(
        original_graph: Arc<SystemGraph>,
        color: String,
        _star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if let Process::Summation { children: _ } = rssystem.context_process
            {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_wait_entity(
        original_graph: Arc<SystemGraph>,
        color: String,
        _star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if let Process::WaitEntity {
                repeat: _,
                repeated_process: _,
                next_process: _,
            } = &rssystem.context_process
            {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_entities_conditional(
        original_graph: Arc<SystemGraph>,
        color: String,
        _star: Option<IdType>,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if ot.evaluate(&rssystem.available_entities, &set) {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }
}

pub mod edge_formatter {
    use std::sync::Arc;

    use petgraph::visit::{EdgeRef, IntoEdgeReferences};
    use petgraph::{Directed, Graph};

    use super::super::graph::{OperationType, SystemGraph};
    use super::super::set::Set;

    type RSdotGraph = Graph<String, String, Directed, u32>;
    type RSformatEdgeTy = dyn Fn(
        &RSdotGraph,
        <&RSdotGraph as IntoEdgeReferences>::EdgeRef,
    ) -> Option<String>;

    pub fn format_entities(
        original_graph: Arc<SystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate(&rssystem.available_entities, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_context(
        original_graph: Arc<SystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate(&rssystem.context, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_t(
        original_graph: Arc<SystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate(&rssystem.t, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_reactants(
        original_graph: Arc<SystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate(&rssystem.reactants, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_reactants_absent(
        original_graph: Arc<SystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate(&rssystem.reactants_absent, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_inhibitors(
        original_graph: Arc<SystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate(&rssystem.inhibitors, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_inhibitors_present(
        original_graph: Arc<SystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate(&rssystem.inhibitors_present, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_products(
        original_graph: Arc<SystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate(&rssystem.products, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }
}

// -----------------------------------------------------------------------------
//                                  Positive
// -----------------------------------------------------------------------------

pub mod positive_graph_map_nodes_ty_from {
    use std::sync::Arc;

    use super::super::element::IdState;
    use super::super::set::{BasicSet, Set};
    use super::super::system::PositiveSystem;
    use super::super::translator;

    type PositiveGraphMapNodesFnTy =
        dyn Fn(petgraph::prelude::NodeIndex, &PositiveSystem) -> String;

    pub fn format_string(s: String) -> Box<PositiveGraphMapNodesFnTy> {
        Box::new(move |_, _| s.clone())
    }

    pub fn format_hide(
        _translator: Arc<translator::Translator>,
    ) -> Box<PositiveGraphMapNodesFnTy> {
        Box::new(|_, _| String::new())
    }

    pub fn format_entities(
        translator: Arc<translator::Translator>,
    ) -> Box<PositiveGraphMapNodesFnTy> {
        Box::new(move |_, node: &PositiveSystem| {
            format!(
                "{}",
                translator::Formatter::from(
                    &translator,
                    &node.available_entities
                )
            )
        })
    }

    pub fn format_mask_entities(
        translator: Arc<translator::Translator>,
        mask: Set,
    ) -> Box<PositiveGraphMapNodesFnTy> {
        Box::new(move |_, node: &PositiveSystem| {
            let masked_entities = node
                .available_entities
                .mask(&mask.to_positive_set(IdState::Positive));
            format!(
                "{}",
                translator::Formatter::from(&translator, &masked_entities)
            )
        })
    }

    pub fn format_exclude_entities(
        translator: Arc<translator::Translator>,
        mask: Set,
    ) -> Box<PositiveGraphMapNodesFnTy> {
        Box::new(move |_, node: &PositiveSystem| {
            let masked_entities = node
                .available_entities
                .subtraction(&mask.to_positive_set(IdState::Negative))
                .subtraction(&mask.to_positive_set(IdState::Negative));
            format!(
                "{}",
                translator::Formatter::from(&translator, &masked_entities)
            )
        })
    }

    pub fn format_context(
        translator: Arc<translator::Translator>,
    ) -> Box<PositiveGraphMapNodesFnTy> {
        Box::new(move |_, node: &PositiveSystem| {
            format!(
                "{}",
                translator::Formatter::from(&translator, &node.context_process)
            )
        })
    }
}

pub mod positive_graph_map_edges_ty_from {
    use std::sync::Arc;

    use super::super::element::IdState;
    use super::super::label::PositiveLabel;
    use super::super::set::{BasicSet, Set};
    use super::super::translator;

    type PositiveGraphMapEdgesFnTy<'a> =
        dyn Fn(petgraph::prelude::EdgeIndex, &'a PositiveLabel) -> String + 'a;

    pub fn format_string<'a>(
        _translator: Arc<translator::Translator>,
        s: String,
    ) -> Box<PositiveGraphMapEdgesFnTy<'a>> {
        Box::new(move |_, _| s.clone())
    }

    pub fn format_hide<'a>(
        _translator: Arc<translator::Translator>,
    ) -> Box<PositiveGraphMapEdgesFnTy<'a>> {
        Box::new(|_, _| String::new())
    }

    macro_rules! create_format_edge {
        ( $name:ident,
	  [$edge_name:ident, $mask_name:ident, $common_name:ident],
	  $mask_common:expr,
	  $mask:expr,
	  $common:expr,
	  $default:expr ) => {
            pub fn $name<'a>(
                translator: Arc<translator::Translator>,
                $mask_name: Option<Set>,
                $common_name: Option<Set>,
            ) -> Box<PositiveGraphMapEdgesFnTy<'a>> {
                match ($mask_name, $common_name) {
                    | (Some($mask_name), Some($common_name)) =>
                        Box::new(move |_, $edge_name: &PositiveLabel| {
                            format!(
                                "{}",
                                translator::Formatter::from(
                                    &translator,
                                    $mask_common
                                )
                            )
                        }),
                    | (Some($mask_name), None) =>
                        Box::new(move |_, $edge_name: &PositiveLabel| {
                            format!(
                                "{}",
                                translator::Formatter::from(&translator, $mask)
                            )
                        }),
                    | (None, Some($common_name)) =>
                        Box::new(move |_, $edge_name: &PositiveLabel| {
                            format!(
                                "{}",
                                translator::Formatter::from(
                                    &translator,
                                    $common
                                )
                            )
                        }),
                    | (None, None) =>
                        Box::new(move |_, $edge_name: &PositiveLabel| {
                            format!(
                                "{}",
                                translator::Formatter::from(
                                    &translator,
                                    $default
                                )
                            )
                        }),
                }
            }
        };
    }

    create_format_edge!(
        format_products,
        [edge, mask, common],
        &edge
            .products
            .mask(&mask.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.products.mask(&mask.to_positive_set(IdState::Positive)),
        &edge
            .products
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.products
    );

    create_format_edge!(
        format_entities,
        [edge, mask, common],
        &edge
            .available_entities
            .mask(&mask.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge
            .available_entities
            .mask(&mask.to_positive_set(IdState::Positive)),
        &edge
            .available_entities
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.available_entities
    );

    create_format_edge!(
        format_context,
        [edge, mask, common],
        &edge
            .context
            .mask(&mask.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.context.mask(&mask.to_positive_set(IdState::Positive)),
        &edge
            .context
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.context
    );

    create_format_edge!(
        format_union,
        [edge, mask, common],
        &edge
            .t
            .mask(&mask.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.t.mask(&mask.to_positive_set(IdState::Positive)),
        &edge
            .t
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.t
    );

    create_format_edge!(
        format_difference,
        [edge, mask, common],
        &edge
            .context
            .subtraction(&edge.available_entities)
            .mask(&mask.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge
            .context
            .subtraction(&edge.available_entities)
            .mask(&mask.to_positive_set(IdState::Positive)),
        &edge
            .context
            .subtraction(&edge.available_entities)
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.context.subtraction(&edge.available_entities)
    );

    create_format_edge!(
        format_entities_deleted,
        [edge, mask, common],
        &edge
            .available_entities
            .subtraction(&edge.products)
            .mask(&mask.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge
            .available_entities
            .subtraction(&edge.products)
            .mask(&mask.to_positive_set(IdState::Positive)),
        &edge
            .available_entities
            .subtraction(&edge.products)
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.available_entities.subtraction(&edge.products)
    );

    create_format_edge!(
        format_entities_added,
        [edge, mask, common],
        &edge
            .products
            .subtraction(&edge.available_entities)
            .mask(&mask.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge
            .products
            .subtraction(&edge.available_entities)
            .mask(&mask.to_positive_set(IdState::Positive)),
        &edge
            .products
            .subtraction(&edge.available_entities)
            .subtraction(&common.to_positive_set(IdState::Positive))
            .subtraction(&common.to_positive_set(IdState::Negative)),
        &edge.products.subtraction(&edge.available_entities)
    );
}

pub mod positive_node_formatter {
    use std::sync::Arc;

    use petgraph::visit::IntoNodeReferences;
    use petgraph::{Directed, Graph};

    use super::super::element::IdType;
    use super::super::graph::{OperationType, PositiveSystemGraph};
    use super::super::process::PositiveProcess;
    use super::super::set::Set;

    type RSdotGraph = Graph<String, String, Directed, u32>;
    type RSformatNodeTy = dyn Fn(
        &RSdotGraph,
        <&RSdotGraph as IntoNodeReferences>::NodeRef,
    ) -> Option<String>;

    pub fn format_nill(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        _star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if rssystem.context_process == PositiveProcess::Nill {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_recursive_identifier(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        star: Option<IdType>,
        s: IdType,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            match (Some(s) == star, &rssystem.context_process) {
                | (
                    true,
                    PositiveProcess::RecursiveIdentifier { identifier: _ },
                ) => Some(", fillcolor=".to_string() + &color),
                | (
                    false,
                    PositiveProcess::RecursiveIdentifier { identifier: id },
                ) if id == &s => Some(", fillcolor=".to_string() + &color),
                | _ => None,
            }
        })
    }

    pub fn format_entity_set(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        _star: Option<IdType>,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            match &rssystem.context_process {
                | PositiveProcess::EntitySet {
                    entities,
                    next_process: _,
                } if ot.evaluate_positive(entities, &set) =>
                    Some(", fillcolor=".to_string() + &color),
                | _ => None,
            }
        })
    }

    pub fn format_non_deterministic_choice(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        _star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if let PositiveProcess::NondeterministicChoice { children: _ } =
                rssystem.context_process
            {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_summation(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        _star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if let PositiveProcess::Summation { children: _ } =
                rssystem.context_process
            {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_wait_entity(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        _star: Option<IdType>,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if let PositiveProcess::WaitEntity {
                repeat: _,
                repeated_process: _,
                next_process: _,
            } = &rssystem.context_process
            {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_entities_conditional(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        _star: Option<IdType>,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatNodeTy> {
        Box::new(move |_, n| {
            let rssystem = original_graph.node_weight(n.0).unwrap();
            if ot.evaluate_positive(&rssystem.available_entities, &set) {
                Some(", fillcolor=".to_string() + &color)
            } else {
                None
            }
        })
    }
}

pub mod positive_edge_formatter {
    use std::sync::Arc;

    use petgraph::visit::{EdgeRef, IntoEdgeReferences};
    use petgraph::{Directed, Graph};

    use super::super::graph::{OperationType, PositiveSystemGraph};
    use super::super::set::Set;

    type RSdotGraph = Graph<String, String, Directed, u32>;
    type RSformatEdgeTy = dyn Fn(
        &RSdotGraph,
        <&RSdotGraph as IntoEdgeReferences>::EdgeRef,
    ) -> Option<String>;

    pub fn format_entities(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate_positive(&rssystem.available_entities, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_context(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate_positive(&rssystem.context, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_t(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate_positive(&rssystem.t, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_reactants(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate_positive(&rssystem.reactants, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_reactants_absent(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate_positive(&rssystem.reactants_absent, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_inhibitors(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate_positive(&rssystem.inhibitors, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_inhibitors_present(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate_positive(&rssystem.inhibitors_present, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }

    pub fn format_products(
        original_graph: Arc<PositiveSystemGraph>,
        color: String,
        ot: OperationType,
        set: Set,
    ) -> Box<RSformatEdgeTy> {
        Box::new(move |_, e| {
            let rssystem = original_graph.edge_weight(e.id()).unwrap();
            if ot.evaluate_positive(&rssystem.products, &set) {
                Some(", color=".to_string() + &color)
            } else {
                None
            }
        })
    }
}

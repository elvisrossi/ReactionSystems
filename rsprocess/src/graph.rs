//! Definitions for generating graphs from a simulation.

use std::sync::Arc;

use petgraph::visit::{IntoEdgeReferences, IntoNodeReferences};
use petgraph::{Directed, Graph};
use serde::{Deserialize, Serialize};

use super::element::IdType;
use super::label::{Label, PositiveLabel};
use super::set::{BasicSet, PositiveSet, Set};
use super::system::{PositiveSystem, System};
use super::translator;

pub type SystemGraph = Graph<System, Label, Directed, u32>;

pub type PositiveSystemGraph =
    Graph<PositiveSystem, PositiveLabel, Directed, u32>;

// -----------------------------------------------------------------------------
//                                   Helpers
// -----------------------------------------------------------------------------

fn common_system_entities(graph: &SystemGraph) -> Set {
    graph
        .node_references()
        .fold(None, |acc, node| match acc {
            | None => Some(node.1.available_entities.clone()),
            | Some(acc) => Some(node.1.available_entities.intersection(&acc)),
        })
        .unwrap_or(Set::default())
}

fn positive_common_system_entities(graph: &PositiveSystemGraph) -> Set {
    graph
        .node_references()
        .fold(None, |acc, node| match acc {
            | None => Some(node.1.available_entities.elements()),
            | Some(acc) =>
                Some(node.1.available_entities.elements().intersection(&acc)),
        })
        .unwrap_or(Set::default())
}

macro_rules! common_label {
    (
	$name:ident,
	[$edge_name:ident, $acc_name:ident],
	$empty_expr:expr,
	$some_expr:expr
    ) => {
        fn $name(graph: &SystemGraph) -> Set {
            graph
                .edge_references()
                .fold(None, |$acc_name, $edge_name| {
                    let $edge_name = $edge_name.weight();
                    match $acc_name {
                        | None => Some($empty_expr),
                        | Some($acc_name) => Some($some_expr),
                    }
                })
                .unwrap_or(Set::default())
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
    edge.context
        .subtraction(&edge.available_entities)
        .intersection(&acc)
);
common_label!(
    common_label_entities_deleted,
    [edge, acc],
    edge.available_entities.subtraction(&edge.products),
    edge.available_entities
        .subtraction(&edge.products)
        .intersection(&acc)
);
common_label!(
    common_label_entities_added,
    [edge, acc],
    edge.products.subtraction(&edge.available_entities),
    edge.products
        .subtraction(&edge.available_entities)
        .intersection(&acc)
);

macro_rules! common_positive_label {
    (
	$name:ident,
	[$edge_name:ident, $acc_name:ident],
	$empty_expr:expr,
	$some_expr:expr
    ) => {
        fn $name(graph: &PositiveSystemGraph) -> Set {
            graph
                .edge_references()
                .fold(None, |$acc_name, $edge_name| {
                    let $edge_name = $edge_name.weight();
                    match $acc_name {
                        | None => Some($empty_expr),
                        | Some($acc_name) => Some($some_expr),
                    }
                })
                .unwrap_or(Set::default())
        }
    };
}

common_positive_label!(
    common_positive_label_products,
    [edge, acc],
    edge.products.elements(),
    edge.products.elements().intersection(&acc)
);
common_positive_label!(
    common_positive_label_entities,
    [edge, acc],
    edge.available_entities.elements(),
    edge.available_entities.elements().intersection(&acc)
);
common_positive_label!(
    common_positive_label_context,
    [edge, acc],
    edge.context.elements(),
    edge.context.elements().intersection(&acc)
);
common_positive_label!(
    common_positive_label_union,
    [edge, acc],
    edge.t.elements(),
    edge.t.elements().intersection(&acc)
);
common_positive_label!(
    common_positive_label_difference,
    [edge, acc],
    edge.context
        .elements()
        .subtraction(&edge.available_entities.elements()),
    edge.context
        .elements()
        .subtraction(&edge.available_entities.elements())
        .intersection(&acc)
);
common_positive_label!(
    common_positive_label_entities_deleted,
    [edge, acc],
    edge.available_entities
        .elements()
        .subtraction(&edge.products.elements()),
    edge.available_entities
        .elements()
        .subtraction(&edge.products.elements())
        .intersection(&acc)
);
common_positive_label!(
    common_positive_label_entities_added,
    [edge, acc],
    edge.products
        .elements()
        .subtraction(&edge.available_entities.elements()),
    edge.products
        .elements()
        .subtraction(&edge.available_entities.elements())
        .intersection(&acc)
);

// Nodes -----------------------------------------------------------------------

/// Helper structure that specifies what information to display for nodes.
#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub enum NodeDisplayBase {
    String { string: String },
    Hide,
    Entities,
    MaskEntities { mask: Set },
    ExcludeEntities { mask: Set },
    Context,
    UncommonEntities,
    MaskUncommonEntities { mask: Set },
}

#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub struct NodeDisplay {
    pub base: Vec<NodeDisplayBase>,
}

type GraphMapNodesFnTy<'a> =
    dyn Fn(petgraph::prelude::NodeIndex, &'a System) -> String + 'a;

type PositiveGraphMapNodesFnTy<'a> =
    dyn Fn(petgraph::prelude::NodeIndex, &'a PositiveSystem) -> String + 'a;

impl NodeDisplayBase {
    fn match_node_display<'a>(
        &self,
        common_entities: Arc<Set>,
        translator: Arc<translator::Translator>,
    ) -> Box<GraphMapNodesFnTy<'a>> {
        use super::format_helpers::graph_map_nodes_ty_from::*;

        match self {
            | Self::String { string } => format_string(string.clone()),
            | Self::Hide => format_hide(translator),
            | Self::Entities => format_entities(translator),
            | Self::MaskEntities { mask } =>
                format_mask_entities(translator, mask.clone()),
            | Self::ExcludeEntities { mask } =>
                format_exclude_entities(translator, mask.clone()),
            | Self::Context => format_context(translator),
            | Self::UncommonEntities =>
                format_exclude_entities(translator, (*common_entities).clone()),
            | Self::MaskUncommonEntities { mask } => format_exclude_entities(
                translator,
                mask.intersection(&common_entities),
            ),
        }
    }

    fn positive_match_node_display<'a>(
        &self,
        common_entities: Arc<Set>,
        translator: Arc<translator::Translator>,
    ) -> Box<PositiveGraphMapNodesFnTy<'a>> {
        use super::format_helpers::positive_graph_map_nodes_ty_from::*;

        match self {
            | Self::String { string } => format_string(string.clone()),
            | Self::Hide => format_hide(translator),
            | Self::Entities => format_entities(translator),
            | Self::MaskEntities { mask } =>
                format_mask_entities(translator, mask.clone()),
            | Self::ExcludeEntities { mask } =>
                format_exclude_entities(translator, mask.clone()),
            | Self::Context => format_context(translator),
            | Self::UncommonEntities =>
                format_exclude_entities(translator, (*common_entities).clone()),
            | Self::MaskUncommonEntities { mask } => format_exclude_entities(
                translator,
                mask.intersection(&common_entities),
            ),
        }
    }
}

impl NodeDisplay {
    fn contains_uncommon(&self) -> bool {
        self.base.iter().any(|b| {
            matches!(
                b,
                NodeDisplayBase::UncommonEntities
                    | NodeDisplayBase::MaskUncommonEntities { mask: _ }
            )
        })
    }

    pub fn generate<'a>(
        self,
        translator: Arc<translator::Translator>,
        current_graph: &SystemGraph,
    ) -> Box<GraphMapNodesFnTy<'a>> {
        let common_entities = if self.contains_uncommon() {
            Arc::new(common_system_entities(current_graph))
        } else {
            Arc::new(Set::default())
        };

        Box::new(move |i, n| {
            let mut accumulator = String::new();
            for b in &self.base {
                let f = b.match_node_display(
                    Arc::clone(&common_entities),
                    Arc::clone(&translator),
                );

                accumulator.push_str(&(f)(i, n));
            }
            accumulator
        })
    }

    pub fn generate_positive<'a>(
        self,
        translator: Arc<translator::Translator>,
        current_graph: &PositiveSystemGraph,
    ) -> Box<PositiveGraphMapNodesFnTy<'a>> {
        let common_entities = if self.contains_uncommon() {
            Arc::new(positive_common_system_entities(current_graph))
        } else {
            Arc::new(Set::default())
        };

        Box::new(move |i, n| {
            let mut accumulator = String::new();
            for b in &self.base {
                let f = b.positive_match_node_display(
                    Arc::clone(&common_entities),
                    Arc::clone(&translator),
                );

                accumulator.push_str(&(f)(i, n));
            }
            accumulator
        })
    }
}

// Edges -----------------------------------------------------------------------

#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub enum EdgeDisplayBase {
    String {
        string: String,
    },
    Hide,
    Products {
        mask: Option<Set>,
        filter_common: bool,
    },
    Entities {
        mask: Option<Set>,
        filter_common: bool,
    },
    Context {
        mask: Option<Set>,
        filter_common: bool,
    },
    Union {
        mask: Option<Set>,
        filter_common: bool,
    },
    Difference {
        mask: Option<Set>,
        filter_common: bool,
    },
    EntitiesDeleted {
        mask: Option<Set>,
        filter_common: bool,
    },
    EntitiesAdded {
        mask: Option<Set>,
        filter_common: bool,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub struct EdgeDisplay {
    pub base: Vec<EdgeDisplayBase>,
}

type GraphMapEdgesFnTy<'a> =
    dyn Fn(petgraph::prelude::EdgeIndex, &'a Label) -> String + 'a;

type PositiveGraphMapEdgesFnTy<'a> =
    dyn Fn(petgraph::prelude::EdgeIndex, &'a PositiveLabel) -> String + 'a;

#[derive(Default, Clone)]
struct CommonEntities {
    common_products: Set,
    common_entities: Set,
    common_context: Set,
    common_union: Set,
    common_difference: Set,
    common_entities_deleted: Set,
    common_entities_added: Set,
}

impl EdgeDisplayBase {
    fn match_edge_display<'a>(
        &'a self,
        translator: Arc<translator::Translator>,
        common: CommonEntities,
    ) -> Box<GraphMapEdgesFnTy<'a>> {
        use super::format_helpers::graph_map_edges_ty_from::*;

        match self {
            | Self::String { string } =>
                format_string(translator, string.clone()),
            | Self::Hide => format_hide(translator),
            | Self::Products {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_products(
                        translator,
                        mask.clone(),
                        Some(common.common_products),
                    )
                } else {
                    format_products(translator, mask.clone(), None)
                },
            | Self::Entities {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_entities(
                        translator,
                        mask.clone(),
                        Some(common.common_entities),
                    )
                } else {
                    format_entities(translator, mask.clone(), None)
                },
            | Self::Context {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_context(
                        translator,
                        mask.clone(),
                        Some(common.common_context),
                    )
                } else {
                    format_context(translator, mask.clone(), None)
                },
            | Self::Union {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_union(
                        translator,
                        mask.clone(),
                        Some(common.common_union),
                    )
                } else {
                    format_union(translator, mask.clone(), None)
                },
            | Self::Difference {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_difference(
                        translator,
                        mask.clone(),
                        Some(common.common_difference),
                    )
                } else {
                    format_difference(translator, mask.clone(), None)
                },
            | Self::EntitiesDeleted {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_entities_deleted(
                        translator,
                        mask.clone(),
                        Some(common.common_entities_deleted),
                    )
                } else {
                    format_entities_deleted(translator, mask.clone(), None)
                },
            | Self::EntitiesAdded {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_entities_added(
                        translator,
                        mask.clone(),
                        Some(common.common_entities_added),
                    )
                } else {
                    format_entities_added(translator, mask.clone(), None)
                },
        }
    }

    fn positive_match_edge_display<'a>(
        &'a self,
        translator: Arc<translator::Translator>,
        common: CommonEntities,
    ) -> Box<PositiveGraphMapEdgesFnTy<'a>> {
        use super::format_helpers::positive_graph_map_edges_ty_from::*;

        match self {
            | Self::String { string } =>
                format_string(translator, string.clone()),
            | Self::Hide => format_hide(translator),
            | Self::Products {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_products(
                        translator,
                        mask.clone(),
                        Some(common.common_products),
                    )
                } else {
                    format_products(translator, mask.clone(), None)
                },
            | Self::Entities {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_entities(
                        translator,
                        mask.clone(),
                        Some(common.common_entities),
                    )
                } else {
                    format_entities(translator, mask.clone(), None)
                },
            | Self::Context {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_context(
                        translator,
                        mask.clone(),
                        Some(common.common_context),
                    )
                } else {
                    format_context(translator, mask.clone(), None)
                },
            | Self::Union {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_union(
                        translator,
                        mask.clone(),
                        Some(common.common_union),
                    )
                } else {
                    format_union(translator, mask.clone(), None)
                },
            | Self::Difference {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_difference(
                        translator,
                        mask.clone(),
                        Some(common.common_difference),
                    )
                } else {
                    format_difference(translator, mask.clone(), None)
                },
            | Self::EntitiesDeleted {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_entities_deleted(
                        translator,
                        mask.clone(),
                        Some(common.common_entities_deleted),
                    )
                } else {
                    format_entities_deleted(translator, mask.clone(), None)
                },
            | Self::EntitiesAdded {
                mask,
                filter_common,
            } =>
                if *filter_common {
                    format_entities_added(
                        translator,
                        mask.clone(),
                        Some(common.common_entities_added),
                    )
                } else {
                    format_entities_added(translator, mask.clone(), None)
                },
        }
    }
}

macro_rules! common_entity {
    ($name:ident, $match:pat, $filter_common:ident) => {
        fn $name(&self) -> bool {
            self.base.iter().any(|b| {
                if let $match = b {
                    *$filter_common
                } else {
                    false
                }
            })
        }
    };
}

impl EdgeDisplay {
    common_entity!(
        common_products,
        EdgeDisplayBase::Products {
            mask: _,
            filter_common
        },
        filter_common
    );
    common_entity!(
        common_entities,
        EdgeDisplayBase::Entities {
            mask: _,
            filter_common
        },
        filter_common
    );
    common_entity!(
        common_context,
        EdgeDisplayBase::Context {
            mask: _,
            filter_common
        },
        filter_common
    );
    common_entity!(
        common_union,
        EdgeDisplayBase::Union {
            mask: _,
            filter_common
        },
        filter_common
    );
    common_entity!(
        common_difference,
        EdgeDisplayBase::Difference {
            mask: _,
            filter_common
        },
        filter_common
    );
    common_entity!(
        common_entities_deleted,
        EdgeDisplayBase::EntitiesDeleted {
            mask: _,
            filter_common
        },
        filter_common
    );
    common_entity!(
        common_entities_added,
        EdgeDisplayBase::EntitiesAdded {
            mask: _,
            filter_common
        },
        filter_common
    );

    pub fn generate<'a>(
        self,
        translator: Arc<translator::Translator>,
        current_graph: &SystemGraph,
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
                tmp.common_entities_deleted =
                    common_label_entities_deleted(current_graph);
            }
            if self.common_entities_added() {
                tmp.common_entities_added =
                    common_label_entities_added(current_graph);
            }
            tmp
        };

        Box::new(move |i, n| {
            let mut accumulator = String::new();
            for b in &self.base {
                let f = b
                    .match_edge_display(Arc::clone(&translator), common.clone());
                accumulator.push_str(&(f)(i, n));
            }
            accumulator
        })
    }

    pub fn generate_positive<'a>(
        self,
        translator: Arc<translator::Translator>,
        current_graph: &PositiveSystemGraph,
    ) -> Box<PositiveGraphMapEdgesFnTy<'a>> {
        // create the structure for common entities if required
        let common = {
            let mut tmp = CommonEntities::default();
            if self.common_products() {
                tmp.common_products =
                    common_positive_label_products(current_graph);
            }
            if self.common_entities() {
                tmp.common_entities =
                    common_positive_label_entities(current_graph);
            }
            if self.common_context() {
                tmp.common_context =
                    common_positive_label_context(current_graph);
            }
            if self.common_union() {
                tmp.common_union = common_positive_label_union(current_graph);
            }
            if self.common_difference() {
                tmp.common_difference =
                    common_positive_label_difference(current_graph);
            }
            if self.common_entities_deleted() {
                tmp.common_entities_deleted =
                    common_positive_label_entities_deleted(current_graph);
            }
            if self.common_entities_added() {
                tmp.common_entities_added =
                    common_positive_label_entities_added(current_graph);
            }
            tmp
        };

        Box::new(move |i, n| {
            let mut accumulator = String::new();
            for b in &self.base {
                let f = b.positive_match_edge_display(
                    Arc::clone(&translator),
                    common.clone(),
                );
                accumulator.push_str(&(f)(i, n));
            }
            accumulator
        })
    }
}

// -----------------------------------------------------------------------------
//                            Color Nodes & Edges
// -----------------------------------------------------------------------------

// Node ------------------------------------------------------------------------
type RSdotGraph = Graph<String, String, Directed, u32>;
type RSformatNodeTy<'a> = dyn Fn(
        &'a RSdotGraph,
        <&'a RSdotGraph as IntoNodeReferences>::NodeRef,
    ) -> String
    + 'a;
type RSformatNodeTyOpt<'a> = dyn Fn(
        &'a RSdotGraph,
        <&'a RSdotGraph as IntoNodeReferences>::NodeRef,
    ) -> Option<String>
    + 'a;

#[derive(Clone, Copy, Debug, Serialize, Deserialize, Hash)]
pub enum OperationType {
    Equals,
    Subset,
    SubsetEqual,
    Superset,
    SupersetEqual,
}

impl OperationType {
    pub fn evaluate(&self, a: &Set, b: &Set) -> bool {
        match self {
            | Self::Equals => a.is_subset(b) && b.is_subset(a),
            | Self::Subset => a.is_subset(b) && !b.is_subset(a),
            | Self::SubsetEqual => a.is_subset(b),
            | Self::Superset => b.is_subset(a) && !a.is_subset(b),
            | Self::SupersetEqual => b.is_subset(a),
        }
    }

    pub fn evaluate_positive(&self, a: &PositiveSet, b: &Set) -> bool {
        match self {
            | Self::Equals =>
                a.elements().is_subset(b) && b.is_subset(&a.elements()),
            | Self::Subset =>
                a.elements().is_subset(b) && !b.is_subset(&a.elements()),
            | Self::SubsetEqual => a.elements().is_subset(b),
            | Self::Superset =>
                b.is_subset(&a.elements()) && !a.elements().is_subset(b),
            | Self::SupersetEqual => b.is_subset(&a.elements()),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub enum ContextColorConditional {
    Nill,
    RecursiveIdentifier(IdType),
    EntitySet(OperationType, Set),
    NonDeterministicChoice,
    Summation,
    WaitEntity,
}

#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub enum NodeColorConditional {
    ContextConditional(ContextColorConditional),
    EntitiesConditional(OperationType, Set),
}

#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub struct NodeColor {
    pub conditionals: Vec<(NodeColorConditional, String)>,
    pub base_color:   String,
}

#[inline(always)]
fn node_formatter_base_color(base_color: String) -> String {
    ", fillcolor=".to_string() + &base_color
}

impl NodeColorConditional {
    fn match_node_color_conditional<'a>(
        &self,
        color: &'a String,
        original_graph: Arc<SystemGraph>,
        star: Option<IdType>,
    ) -> Box<RSformatNodeTyOpt<'a>> {
        use super::format_helpers::node_formatter::*;
        match self {
            | Self::ContextConditional(ContextColorConditional::Nill) =>
                format_nill(Arc::clone(&original_graph), color.to_string(), star),
            | Self::ContextConditional(
                ContextColorConditional::RecursiveIdentifier(s),
            ) => format_recursive_identifier(
                Arc::clone(&original_graph),
                color.to_string(),
                star,
                *s,
            ),
            | Self::ContextConditional(ContextColorConditional::EntitySet(
                ot,
                set,
            )) => format_entity_set(
                Arc::clone(&original_graph),
                color.to_string(),
                star,
                *ot,
                set.clone(),
            ),
            | Self::ContextConditional(
                ContextColorConditional::NonDeterministicChoice,
            ) => format_non_deterministic_choice(
                Arc::clone(&original_graph),
                color.to_string(),
                star,
            ),
            | Self::ContextConditional(ContextColorConditional::Summation) =>
                format_summation(
                    Arc::clone(&original_graph),
                    color.to_string(),
                    star,
                ),
            | Self::ContextConditional(ContextColorConditional::WaitEntity) =>
                format_wait_entity(
                    Arc::clone(&original_graph),
                    color.to_string(),
                    star,
                ),
            | Self::EntitiesConditional(ot, set) =>
                format_entities_conditional(
                    Arc::clone(&original_graph),
                    color.to_string(),
                    star,
                    *ot,
                    set.clone(),
                ),
        }
    }

    fn match_positive_node_color_conditional<'a>(
        &self,
        color: &'a String,
        original_graph: Arc<PositiveSystemGraph>,
        star: Option<IdType>,
    ) -> Box<RSformatNodeTyOpt<'a>> {
        use super::format_helpers::positive_node_formatter::*;
        match self {
            | Self::ContextConditional(ContextColorConditional::Nill) =>
                format_nill(Arc::clone(&original_graph), color.to_string(), star),
            | Self::ContextConditional(
                ContextColorConditional::RecursiveIdentifier(s),
            ) => format_recursive_identifier(
                Arc::clone(&original_graph),
                color.to_string(),
                star,
                *s,
            ),
            | Self::ContextConditional(ContextColorConditional::EntitySet(
                ot,
                set,
            )) => format_entity_set(
                Arc::clone(&original_graph),
                color.to_string(),
                star,
                *ot,
                set.clone(),
            ),
            | Self::ContextConditional(
                ContextColorConditional::NonDeterministicChoice,
            ) => format_non_deterministic_choice(
                Arc::clone(&original_graph),
                color.to_string(),
                star,
            ),
            | Self::ContextConditional(ContextColorConditional::Summation) =>
                format_summation(
                    Arc::clone(&original_graph),
                    color.to_string(),
                    star,
                ),
            | Self::ContextConditional(ContextColorConditional::WaitEntity) =>
                format_wait_entity(
                    Arc::clone(&original_graph),
                    color.to_string(),
                    star,
                ),
            | Self::EntitiesConditional(ot, set) =>
                format_entities_conditional(
                    Arc::clone(&original_graph),
                    color.to_string(),
                    star,
                    *ot,
                    set.clone(),
                ),
        }
    }
}

impl NodeColor {
    pub fn generate<'a>(
        self,
        original_graph: Arc<SystemGraph>,
        star: Option<IdType>,
    ) -> Box<RSformatNodeTy<'a>> {
        Box::new(move |i, n| {
            for (rule, color) in &self.conditionals {
                let f = rule.match_node_color_conditional(
                    color,
                    Arc::clone(&original_graph),
                    star,
                );

                if let Some(s) = (f)(i, n) {
                    return s;
                }
            }
            node_formatter_base_color(self.base_color.clone())
        })
    }

    pub fn generate_positive<'a>(
        self,
        original_graph: Arc<PositiveSystemGraph>,
        star: Option<IdType>,
    ) -> Box<RSformatNodeTy<'a>> {
        Box::new(move |i, n| {
            for (rule, color) in &self.conditionals {
                let f = rule.match_positive_node_color_conditional(
                    color,
                    Arc::clone(&original_graph),
                    star,
                );

                if let Some(s) = (f)(i, n) {
                    return s;
                }
            }
            node_formatter_base_color(self.base_color.clone())
        })
    }
}

// Edge ------------------------------------------------------------------------

type RSformatEdgeTy<'a> = dyn Fn(
        &'a RSdotGraph,
        <&'a RSdotGraph as IntoEdgeReferences>::EdgeRef,
    ) -> String
    + 'a;
type RSformatEdgeTyOpt<'a> = dyn Fn(
        &'a RSdotGraph,
        <&'a RSdotGraph as IntoEdgeReferences>::EdgeRef,
    ) -> Option<String>
    + 'a;

#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub enum EdgeColorConditional {
    Entities(OperationType, Set),
    Context(OperationType, Set),
    T(OperationType, Set),
    Reactants(OperationType, Set),
    ReactantsAbsent(OperationType, Set),
    Inhibitors(OperationType, Set),
    InhibitorsPresent(OperationType, Set),
    Products(OperationType, Set),
}

#[derive(Clone, Debug, Serialize, Deserialize, Hash)]
pub struct EdgeColor {
    pub conditionals: Vec<(EdgeColorConditional, String)>,
    pub base_color:   String,
}

fn edge_formatter_base_color(base_color: String) -> String {
    ", color=".to_string() + &base_color
}

impl EdgeColorConditional {
    fn match_edge_color_conditional<'a>(
        &'a self,
        color: &'a String,
        original_graph: Arc<SystemGraph>,
    ) -> Box<RSformatEdgeTyOpt<'a>> {
        use super::format_helpers::edge_formatter::*;
        match self {
            | Self::Entities(ot, set) => format_entities(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::Context(ot, set) => format_context(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::T(ot, set) => format_t(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::Reactants(ot, set) => format_reactants(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::ReactantsAbsent(ot, set) => format_reactants_absent(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::Inhibitors(ot, set) => format_inhibitors(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::InhibitorsPresent(ot, set) => format_inhibitors_present(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::Products(ot, set) => format_products(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
        }
    }

    fn match_positive_edge_color_conditional<'a>(
        &'a self,
        color: &'a String,
        original_graph: Arc<PositiveSystemGraph>,
    ) -> Box<RSformatEdgeTyOpt<'a>> {
        use super::format_helpers::positive_edge_formatter::*;

        match self {
            | Self::Entities(ot, set) => format_entities(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::Context(ot, set) => format_context(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::T(ot, set) => format_t(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::Reactants(ot, set) => format_reactants(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::ReactantsAbsent(ot, set) => format_reactants_absent(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::Inhibitors(ot, set) => format_inhibitors(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::InhibitorsPresent(ot, set) => format_inhibitors_present(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
            | Self::Products(ot, set) => format_products(
                Arc::clone(&original_graph),
                color.to_string(),
                *ot,
                set.clone(),
            ),
        }
    }
}

impl EdgeColor {
    pub fn generate<'a>(
        self,
        original_graph: Arc<SystemGraph>,
    ) -> Box<RSformatEdgeTy<'a>> {
        Box::new(move |i, n| {
            for (rule, color) in &self.conditionals {
                let f = rule.match_edge_color_conditional(
                    color,
                    Arc::clone(&original_graph),
                );

                if let Some(s) = (f)(i, n) {
                    return s;
                }
            }
            edge_formatter_base_color(self.base_color.clone())
        })
    }

    pub fn generate_positive<'a>(
        self,
        original_graph: Arc<PositiveSystemGraph>,
    ) -> Box<RSformatEdgeTy<'a>> {
        Box::new(move |i, n| {
            for (rule, color) in &self.conditionals {
                let f = rule.match_positive_edge_color_conditional(
                    color,
                    Arc::clone(&original_graph),
                );

                if let Some(s) = (f)(i, n) {
                    return s;
                }
            }
            edge_formatter_base_color(self.base_color.clone())
        })
    }
}

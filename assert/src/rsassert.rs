use std::collections::HashMap;

use rsprocess::translator::PrintableWithTranslator;
use rsprocess::{graph, label, set, system, translator};
use serde::{Deserialize, Serialize};

use super::{dsl, positivedsl};

// -----------------------------------------------------------------------------
//                       Specific Assert Implementation
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
// System

/// Module that has all types and structures for bisimilarity relabeler.
pub mod useful_types_edge_relabeler {
    macro_rules! export_types {
	( $( $x:ident ),* ) => {
	    $(
		pub type $x = super::super::dsl::$x<super::EdgeRelablerInput>;
	    )*
	};
    }

    macro_rules! export_types_no_parameter {
	( $( $x:ident ),* ) => {
	    $(
		pub type $x = super::super::dsl::$x;
	    )*
	};
    }

    export_types!(Assert, Tree, Variable, Expression, Range);

    export_types_no_parameter!(
        Unary,
        QualifierRestricted,
        QualifierLabel,
        QualifierSystem,
        QualifierEdge,
        QualifierNode,
        Qualifier,
        Binary,
        AssertReturnValue
    );

    pub type Special = super::EdgeRelablerInput;
}

// Implementation for graph labeling in bisimulation.

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EdgeRelablerInput {
    Label,
    Edge,
}

#[derive(Clone, Serialize, Deserialize)]
enum EdgeRelablerInputValues {
    Label(label::Label),
    Edge(petgraph::graph::EdgeIndex),
}

impl dsl::SpecialVariables<EdgeRelablerInputValues> for EdgeRelablerInput {
    fn type_of(&self) -> dsl::AssertionTypes {
        match self {
            | Self::Edge => dsl::AssertionTypes::Edge,
            | Self::Label => dsl::AssertionTypes::Label,
        }
    }

    fn type_qualified(&self, q: &dsl::Qualifier) -> Result<dsl::AssertionTypes, String> {
        match (self, q) {
            | (Self::Label, dsl::Qualifier::Label(_))
            | (Self::Label, dsl::Qualifier::Restricted(_)) =>
                Ok(dsl::AssertionTypes::Set),
            | (s, q) =>
                Err(format!("Wrong use of qualifier {q:?} on variable {s:?}.")),
        }
    }

    fn new_context(
        input: HashMap<Self, EdgeRelablerInputValues>,
    ) -> HashMap<Self, dsl::AssertReturnValue> {
        input
            .iter()
            .map(|(key, value)| match value {
                | EdgeRelablerInputValues::Edge(e) =>
                    (*key, dsl::AssertReturnValue::Edge(*e)),
                | EdgeRelablerInputValues::Label(l) =>
                    (*key, dsl::AssertReturnValue::Label(l.clone())),
            })
            .collect::<HashMap<Self, dsl::AssertReturnValue>>()
    }

    fn correct_type(&self, other: &dsl::AssertReturnValue) -> bool {
        match (self, other) {
            | (Self::Edge, dsl::AssertReturnValue::Edge(_))
            | (Self::Label, dsl::AssertReturnValue::Label(_)) => true,
            | (_, _) => false,
        }
    }
}

impl std::fmt::Debug for EdgeRelablerInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Label => write!(f, "label"),
            | Self::Edge => write!(f, "edge"),
        }
    }
}

impl PrintableWithTranslator for EdgeRelablerInput {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        _translator: &translator::Translator,
    ) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl dsl::Assert<EdgeRelablerInput> {
    pub fn typecheck(&self) -> Result<(), String> {
        let mut context = dsl::TypeContext::new();
        let ty = dsl::typecheck(&self.tree, &mut context)?;
        match ty {
            | dsl::AssertionTypes::Boolean
            | dsl::AssertionTypes::Integer
            | dsl::AssertionTypes::String
            | dsl::AssertionTypes::Label
            | dsl::AssertionTypes::Set
            | dsl::AssertionTypes::Element
            | dsl::AssertionTypes::Edge
            | dsl::AssertionTypes::Node
            | dsl::AssertionTypes::System
            | dsl::AssertionTypes::Context => Ok(()),
            | dsl::AssertionTypes::NoType =>
                Err("No return type, at least one return statement required."
                    .into()),
            | dsl::AssertionTypes::RangeInteger
            | dsl::AssertionTypes::RangeSet
            | dsl::AssertionTypes::RangeNeighbours =>
                Err(format!("Returned type {ty:?} is not a valid return type.")),
        }
    }

    pub fn execute(
        &self,
        graph: &graph::SystemGraph,
        edge: &<graph::SystemGraph as petgraph::visit::GraphBase>::EdgeId,
        translator: &mut translator::Translator,
    ) -> Result<dsl::AssertReturnValue, String> {
        let label = graph
            .edge_weight(*edge)
            .ok_or("Missing edge {{debug: {edge:?}}}")?;

        let mut input_vals = HashMap::new();
        input_vals.insert(
            EdgeRelablerInput::Edge,
            EdgeRelablerInputValues::Edge(*edge),
        );
        input_vals.insert(
            EdgeRelablerInput::Label,
            EdgeRelablerInputValues::Label(label.clone()),
        );

        let mut context = dsl::Context::new(input_vals);
        if let Some(v) = dsl::execute(&self.tree, &mut context, translator, graph)? {
            Ok(v)
        } else {
            Err("No value returned.".into())
        }
    }
}

// -----------------------------------------------------------------------------
// Implementation for node grouping.
// -----------------------------------------------------------------------------

/// Module that has all types and structures for bisimilarity relabeler.
pub mod useful_types_node_relabeler {
    macro_rules! export_types {
	( $( $x:ident ),* ) => {
	    $(
		pub type $x = super::super::dsl::$x<super::NodeRelablerInput>;
	    )*
	};
    }

    macro_rules! export_types_no_parameter {
	( $( $x:ident ),* ) => {
	    $(
		pub type $x = super::super::dsl::$x;
	    )*
	};
    }

    export_types!(Assert, Tree, Variable, Expression, Range);

    export_types_no_parameter!(
        Unary,
        QualifierRestricted,
        QualifierLabel,
        QualifierSystem,
        QualifierEdge,
        QualifierNode,
        Qualifier,
        Binary,
        AssertReturnValue
    );

    pub type Special = super::NodeRelablerInput;
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum NodeRelablerInput {
    Entities,
    Node,
}

#[derive(Clone, Serialize, Deserialize)]
enum NodeRelablerInputValues {
    Entities(set::Set),
    Node(petgraph::graph::NodeIndex),
}

impl dsl::SpecialVariables<NodeRelablerInputValues> for NodeRelablerInput {
    fn type_of(&self) -> dsl::AssertionTypes {
        match self {
            | Self::Entities => dsl::AssertionTypes::Set,
            | Self::Node => dsl::AssertionTypes::Node,
        }
    }

    fn type_qualified(&self, q: &dsl::Qualifier) -> Result<dsl::AssertionTypes, String> {
        match (self, q) {
            | (Self::Node, dsl::Qualifier::Node(dsl::QualifierNode::System)) =>
                Ok(dsl::AssertionTypes::System),
            | (Self::Node, dsl::Qualifier::Node(dsl::QualifierNode::Neighbours)) =>
                Ok(dsl::AssertionTypes::RangeNeighbours),
            | (s, q) =>
                Err(format!("Wrong use of qualifier {q:?} on variable {s:?}.")),
        }
    }

    fn new_context(
        input: HashMap<Self, NodeRelablerInputValues>,
    ) -> HashMap<Self, dsl::AssertReturnValue> {
        input
            .iter()
            .map(|(key, value)| match value {
                | NodeRelablerInputValues::Entities(e) =>
                    (*key, dsl::AssertReturnValue::Set(e.clone())),
                | NodeRelablerInputValues::Node(n) =>
                    (*key, dsl::AssertReturnValue::Node(*n)),
            })
            .collect::<HashMap<Self, dsl::AssertReturnValue>>()
    }

    fn correct_type(&self, other: &dsl::AssertReturnValue) -> bool {
        match (self, other) {
            | (Self::Entities, dsl::AssertReturnValue::Set(_))
            | (Self::Node, dsl::AssertReturnValue::Node(_)) => true,
            | (_, _) => false,
        }
    }
}

impl std::fmt::Debug for NodeRelablerInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Entities => write!(f, "entities"),
            | Self::Node => write!(f, "node"),
        }
    }
}

impl PrintableWithTranslator for NodeRelablerInput {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        _translator: &translator::Translator,
    ) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl dsl::Assert<NodeRelablerInput> {
    pub fn typecheck(&self) -> Result<(), String> {
        let mut context = dsl::TypeContext::new();
        let ty = dsl::typecheck(&self.tree, &mut context)?;
        match ty {
            | dsl::AssertionTypes::Boolean
            | dsl::AssertionTypes::Integer
            | dsl::AssertionTypes::String
            | dsl::AssertionTypes::Label
            | dsl::AssertionTypes::Set
            | dsl::AssertionTypes::Element
            | dsl::AssertionTypes::Edge
            | dsl::AssertionTypes::Node
            | dsl::AssertionTypes::System
            | dsl::AssertionTypes::Context => Ok(()),
            | dsl::AssertionTypes::NoType =>
                Err("No return type, at least one return statement required."
                    .into()),
            | dsl::AssertionTypes::RangeInteger
            | dsl::AssertionTypes::RangeSet
            | dsl::AssertionTypes::RangeNeighbours =>
                Err(format!("Returned type {ty:?} is not a valid return type.")),
        }
    }

    pub fn execute(
        &self,
        graph: &graph::SystemGraph,
        node: &<graph::SystemGraph as petgraph::visit::GraphBase>::NodeId,
        translator: &mut translator::Translator,
    ) -> Result<dsl::AssertReturnValue, String> {
        let system::System {
            available_entities: entities,
            ..
        } = graph
            .node_weight(*node)
            .ok_or("Missing node {{debug: {node:?}}}")?;

        let mut input_vals = HashMap::new();
        input_vals.insert(
            NodeRelablerInput::Entities,
            NodeRelablerInputValues::Entities(entities.clone()),
        );
        input_vals.insert(
            NodeRelablerInput::Node,
            NodeRelablerInputValues::Node(*node),
        );

        let mut context = dsl::Context::new(input_vals);
        if let Some(v) = dsl::execute(&self.tree, &mut context, translator, graph)? {
            Ok(v)
        } else {
            Err("No value returned.".into())
        }
    }
}


// -----------------------------------------------------------------------------
// Positive System


/// Module that has all types and structures for bisimilarity relabeler.
pub mod useful_types_positive_edge_relabeler {
    macro_rules! export_types {
	( $( $x:ident ),* ) => {
	    $(
		pub type $x = super::super::positivedsl::$x<super::PositiveEdgeRelablerInput>;
	    )*
	};
    }

    macro_rules! export_types_no_parameter {
	( $( $x:ident ),* ) => {
	    $(
		pub type $x = super::super::positivedsl::$x;
	    )*
	};
    }

    export_types!(
        PositiveAssert,
        PositiveTree,
        PositiveVariable,
        PositiveExpression,
        PositiveRange
    );

    export_types_no_parameter!(
        PositiveUnary,
        QualifierRestricted,
        QualifierLabel,
        QualifierSystem,
        QualifierEdge,
        QualifierNode,
        PositiveQualifier,
        PositiveBinary,
        PositiveAssertReturnValue
    );

    pub type Special = super::PositiveEdgeRelablerInput;
}


#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PositiveEdgeRelablerInput {
    Label,
    Edge,
}

#[derive(Clone, Serialize, Deserialize)]
enum PositiveEdgeRelablerInputValues {
    Label(label::PositiveLabel),
    Edge(petgraph::graph::EdgeIndex),
}

impl positivedsl::SpecialVariables<PositiveEdgeRelablerInputValues> for PositiveEdgeRelablerInput {
    fn type_of(&self) -> positivedsl::PositiveAssertionTypes {
        match self {
            | Self::Edge => positivedsl::PositiveAssertionTypes::Edge,
            | Self::Label => positivedsl::PositiveAssertionTypes::Label,
        }
    }

    fn type_qualified(&self, q: &positivedsl::PositiveQualifier) -> Result<positivedsl::PositiveAssertionTypes, String> {
        match (self, q) {
            | (Self::Label, positivedsl::PositiveQualifier::Label(_))
            | (Self::Label, positivedsl::PositiveQualifier::Restricted(_)) =>
                Ok(positivedsl::PositiveAssertionTypes::Set),
            | (s, q) =>
                Err(format!("Wrong use of qualifier {q:?} on variable {s:?}.")),
        }
    }

    fn new_context(
        input: HashMap<Self, PositiveEdgeRelablerInputValues>,
    ) -> HashMap<Self, positivedsl::PositiveAssertReturnValue> {
        input
            .iter()
            .map(|(key, value)| match value {
                | PositiveEdgeRelablerInputValues::Edge(e) =>
                    (*key, positivedsl::PositiveAssertReturnValue::Edge(*e)),
                | PositiveEdgeRelablerInputValues::Label(l) =>
                    (*key, positivedsl::PositiveAssertReturnValue::Label(l.clone())),
            })
            .collect::<HashMap<Self, positivedsl::PositiveAssertReturnValue>>()
    }

    fn correct_type(&self, other: &positivedsl::PositiveAssertReturnValue) -> bool {
        match (self, other) {
            | (Self::Edge, positivedsl::PositiveAssertReturnValue::Edge(_))
            | (Self::Label, positivedsl::PositiveAssertReturnValue::Label(_)) => true,
            | (_, _) => false,
        }
    }
}

impl std::fmt::Debug for PositiveEdgeRelablerInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Label => write!(f, "label"),
            | Self::Edge => write!(f, "edge"),
        }
    }
}

impl PrintableWithTranslator for PositiveEdgeRelablerInput {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        _translator: &translator::Translator,
    ) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}


impl positivedsl::PositiveAssert<PositiveEdgeRelablerInput> {
    pub fn typecheck(&self) -> Result<(), String> {
        let mut context = positivedsl::TypeContext::new();
        let ty = positivedsl::typecheck(&self.tree, &mut context)?;
        match ty {
            | positivedsl::PositiveAssertionTypes::Boolean
            | positivedsl::PositiveAssertionTypes::Integer
            | positivedsl::PositiveAssertionTypes::String
            | positivedsl::PositiveAssertionTypes::Label
            | positivedsl::PositiveAssertionTypes::Set
            | positivedsl::PositiveAssertionTypes::PositiveElement
            | positivedsl::PositiveAssertionTypes::Edge
            | positivedsl::PositiveAssertionTypes::Node
            | positivedsl::PositiveAssertionTypes::System
            | positivedsl::PositiveAssertionTypes::Context => Ok(()),
            | positivedsl::PositiveAssertionTypes::NoType =>
                Err("No return type, at least one return statement required."
                    .into()),
            | positivedsl::PositiveAssertionTypes::RangeInteger
            | positivedsl::PositiveAssertionTypes::RangeSet
            | positivedsl::PositiveAssertionTypes::RangeNeighbours =>
                Err(format!("Returned type {ty:?} is not a valid return type.")),
        }
    }

    pub fn execute(
        &self,
        graph: &graph::PositiveSystemGraph,
        edge: &<graph::PositiveSystemGraph as petgraph::visit::GraphBase>::EdgeId,
        translator: &mut translator::Translator,
    ) -> Result<positivedsl::PositiveAssertReturnValue, String> {
        let label = graph
            .edge_weight(*edge)
            .ok_or("Missing edge {{debug: {edge:?}}}")?;

        let mut input_vals = HashMap::new();
        input_vals.insert(
            PositiveEdgeRelablerInput::Edge,
            PositiveEdgeRelablerInputValues::Edge(*edge),
        );
        input_vals.insert(
            PositiveEdgeRelablerInput::Label,
            PositiveEdgeRelablerInputValues::Label(label.clone()),
        );

        let mut context = positivedsl::Context::new(input_vals);
        if let Some(v) = positivedsl::execute(&self.tree, &mut context, translator, graph)? {
            Ok(v)
        } else {
            Err("No value returned.".into())
        }
    }
}


// -----------------------------------------------------------------------------
// Positive node grouping

pub mod useful_types_positive_node_relabeler {
    macro_rules! export_types {
	( $( $x:ident ),* ) => {
	    $(
		pub type $x = super::super::positivedsl::$x<super::PositiveNodeRelablerInput>;
	    )*
	};
    }

    macro_rules! export_types_no_parameter {
	( $( $x:ident ),* ) => {
	    $(
		pub type $x = super::super::positivedsl::$x;
	    )*
	};
    }

    export_types!(
        PositiveAssert,
        PositiveTree,
        PositiveVariable,
        PositiveExpression,
        PositiveRange
    );

    export_types_no_parameter!(
        PositiveUnary,
        QualifierRestricted,
        QualifierLabel,
        QualifierSystem,
        QualifierEdge,
        QualifierNode,
        PositiveQualifier,
        PositiveBinary,
        PositiveAssertReturnValue
    );

    pub type Special = super::PositiveNodeRelablerInput;
}


#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PositiveNodeRelablerInput {
    Entities,
    Node,
}

#[derive(Clone, Serialize, Deserialize)]
enum PositiveNodeRelablerInputValues {
    Entities(set::PositiveSet),
    Node(petgraph::graph::NodeIndex),
}


impl positivedsl::SpecialVariables<PositiveNodeRelablerInputValues> for PositiveNodeRelablerInput {
    fn type_of(&self) -> positivedsl::PositiveAssertionTypes {
        match self {
            | Self::Entities => positivedsl::PositiveAssertionTypes::Set,
            | Self::Node => positivedsl::PositiveAssertionTypes::Node,
        }
    }

    fn type_qualified(&self, q: &positivedsl::PositiveQualifier) -> Result<positivedsl::PositiveAssertionTypes, String> {
        match (self, q) {
            | (Self::Node, positivedsl::PositiveQualifier::Node(positivedsl::QualifierNode::System)) =>
                Ok(positivedsl::PositiveAssertionTypes::System),
            | (Self::Node, positivedsl::PositiveQualifier::Node(positivedsl::QualifierNode::Neighbours)) =>
                Ok(positivedsl::PositiveAssertionTypes::RangeNeighbours),
            | (s, q) =>
                Err(format!("Wrong use of qualifier {q:?} on variable {s:?}.")),
        }
    }

    fn new_context(
        input: HashMap<Self, PositiveNodeRelablerInputValues>,
    ) -> HashMap<Self, positivedsl::PositiveAssertReturnValue> {
        input
            .iter()
            .map(|(key, value)| match value {
                | PositiveNodeRelablerInputValues::Entities(e) =>
                    (*key, positivedsl::PositiveAssertReturnValue::Set(e.clone())),
                | PositiveNodeRelablerInputValues::Node(n) =>
                    (*key, positivedsl::PositiveAssertReturnValue::Node(*n)),
            })
            .collect::<HashMap<Self, positivedsl::PositiveAssertReturnValue>>()
    }

    fn correct_type(&self, other: &positivedsl::PositiveAssertReturnValue) -> bool {
        match (self, other) {
            | (Self::Entities, positivedsl::PositiveAssertReturnValue::Set(_))
            | (Self::Node, positivedsl::PositiveAssertReturnValue::Node(_)) => true,
            | (_, _) => false,
        }
    }
}


impl std::fmt::Debug for PositiveNodeRelablerInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Entities => write!(f, "entities"),
            | Self::Node => write!(f, "node"),
        }
    }
}

impl PrintableWithTranslator for PositiveNodeRelablerInput {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        _translator: &translator::Translator,
    ) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl positivedsl::PositiveAssert<PositiveNodeRelablerInput> {
    pub fn typecheck(&self) -> Result<(), String> {
        let mut context = positivedsl::TypeContext::new();
        let ty = positivedsl::typecheck(&self.tree, &mut context)?;
        match ty {
            | positivedsl::PositiveAssertionTypes::Boolean
            | positivedsl::PositiveAssertionTypes::Integer
            | positivedsl::PositiveAssertionTypes::String
            | positivedsl::PositiveAssertionTypes::Label
            | positivedsl::PositiveAssertionTypes::Set
            | positivedsl::PositiveAssertionTypes::PositiveElement
            | positivedsl::PositiveAssertionTypes::Edge
            | positivedsl::PositiveAssertionTypes::Node
            | positivedsl::PositiveAssertionTypes::System
            | positivedsl::PositiveAssertionTypes::Context => Ok(()),
            | positivedsl::PositiveAssertionTypes::NoType =>
                Err("No return type, at least one return statement required."
                    .into()),
            | positivedsl::PositiveAssertionTypes::RangeInteger
            | positivedsl::PositiveAssertionTypes::RangeSet
            | positivedsl::PositiveAssertionTypes::RangeNeighbours =>
                Err(format!("Returned type {ty:?} is not a valid return type.")),
        }
    }

    pub fn execute(
        &self,
        graph: &graph::PositiveSystemGraph,
        node: &<graph::PositiveSystemGraph as petgraph::visit::GraphBase>::NodeId,
        translator: &mut translator::Translator,
    ) -> Result<positivedsl::PositiveAssertReturnValue, String> {
        let system::PositiveSystem {
            available_entities: entities,
            ..
        } = graph
            .node_weight(*node)
            .ok_or("Missing node {{debug: {node:?}}}")?;

        let mut input_vals = HashMap::new();
        input_vals.insert(
            PositiveNodeRelablerInput::Entities,
            PositiveNodeRelablerInputValues::Entities(entities.clone()),
        );
        input_vals.insert(
            PositiveNodeRelablerInput::Node,
            PositiveNodeRelablerInputValues::Node(*node),
        );

        let mut context = positivedsl::Context::new(input_vals);
        if let Some(v) = positivedsl::execute(&self.tree, &mut context, translator, graph)? {
            Ok(v)
        } else {
            Err("No value returned.".into())
        }
    }
}

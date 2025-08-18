use super::dsl::*;
use super::super::{structure, translator, graph};
use std::collections::HashMap;

// ----------------------------------------------------------------------------
//                       Specific Assert Implementation
// ----------------------------------------------------------------------------

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

    export_types!(RSassert, Tree, Variable, Expression, Range);

    export_types_no_parameter!(Unary, QualifierRestricted, QualifierLabel,
			       QualifierSystem, QualifierEdge, QualifierNode,
			       Qualifier, Binary, AssertReturnValue);

    pub type Special = super::EdgeRelablerInput;
}


// Implementation for graph labeling in bisimulation.

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum EdgeRelablerInput {
    Label,
    Edge,
}

#[derive(Debug, Clone)]
enum EdgeRelablerInputValues {
    Label(structure::RSlabel),
    Edge(petgraph::graph::EdgeIndex),
}

impl SpecialVariables<EdgeRelablerInputValues> for EdgeRelablerInput {
    fn type_of(&self) -> AssertionTypes {
	match self {
	    Self::Edge => AssertionTypes::Edge,
	    Self::Label => AssertionTypes::Label,
	}
    }

    fn type_qualified(&self, q: &Qualifier) -> Result<AssertionTypes, String> {
	match (self, q) {
	    (Self::Label, Qualifier::Label(_)) |
	    (Self::Label, Qualifier::Restricted(_)) =>
		Ok(AssertionTypes::Set),
	    (s, q) =>
		Err(format!("Wrong use of qualifier {q} on variable {s}."))
	}
    }

    fn new_context(input: HashMap<Self, EdgeRelablerInputValues>)
		   -> HashMap<Self, AssertReturnValue> {
	input.iter().map(|(key, value)| {
	    match value {
		EdgeRelablerInputValues::Edge(e) =>
		    (*key, AssertReturnValue::Edge(*e)),
		EdgeRelablerInputValues::Label(l) =>
		    (*key, AssertReturnValue::Label(l.clone())),
	    }
	}).collect::<HashMap<Self, AssertReturnValue>>()
    }

    fn correct_type(&self, other: &AssertReturnValue) -> bool {
	match (self, other) {
	    (Self::Edge, AssertReturnValue::Edge(_)) |
	    (Self::Label, AssertReturnValue::Label(_)) => true,
	    (_, _) => false
	}
    }
}

impl std::fmt::Display for EdgeRelablerInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Label => write!(f, "label"),
	    Self::Edge => write!(f, "edge"),
	}
    }
}

impl RSassert<EdgeRelablerInput> {
    pub fn typecheck(&self) -> Result<(), String> {
	let mut context = TypeContext::new();
	let ty = typecheck(&self.tree, &mut context)?;
	match ty {
	    AssertionTypes::Boolean |
	    AssertionTypes::Integer |
	    AssertionTypes::String |
	    AssertionTypes::Label |
	    AssertionTypes::Set |
	    AssertionTypes::Element |
	    AssertionTypes::Edge |
	    AssertionTypes::Node |
	    AssertionTypes::System |
	    AssertionTypes::Context =>
		Ok(()),
	    AssertionTypes::NoType =>
		Err(format!("No return type, at least one return statement \
			     required.")),
	    AssertionTypes::RangeInteger |
	    AssertionTypes::RangeSet |
	    AssertionTypes::RangeNeighbours =>
		Err(format!("Returned type {ty} is not a valid return type.")),
	}
    }

    pub fn execute(
	&self,
	graph: &graph::RSgraph,
	edge: &<graph::RSgraph as petgraph::visit::GraphBase>::EdgeId,
	translator: &mut translator::Translator,
    ) -> Result<AssertReturnValue, String> {
	let label = graph.edge_weight(*edge)
	    .ok_or("Missing edge {{debug: {edge:?}}}")?;

	let mut input_vals = HashMap::new();
	input_vals.insert(EdgeRelablerInput::Edge,
			  EdgeRelablerInputValues::Edge(*edge));
	input_vals.insert(EdgeRelablerInput::Label,
			  EdgeRelablerInputValues::Label(label.clone()));

	let mut context = Context::new(input_vals);
	if let Some(v) = execute(&self.tree, &mut context, translator, graph)? {
	    Ok(v)
	} else {
	    Err("No value returned.".into())
	}
    }
}

// -----------------------------------------------------------------------------
// Implementation for node grouping.
// -----------------------------------------------------------------------------

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum NodeRelablerInput {
    Entities,
    Node,
}

#[derive(Debug, Clone)]
enum NodeRelablerInputValues {
    Entities(structure::RSset),
    Node(petgraph::graph::NodeIndex),
}

impl SpecialVariables<NodeRelablerInputValues> for NodeRelablerInput {
    fn type_of(&self) -> AssertionTypes {
	match self {
	    Self::Entities => AssertionTypes::Set,
	    Self::Node => AssertionTypes::Node,
	}
    }

    fn type_qualified(&self, q: &Qualifier) -> Result<AssertionTypes, String> {
	match (self, q) {
	    (Self::Node, Qualifier::Node(QualifierNode::System)) =>
		Ok(AssertionTypes::System),
	    (Self::Node, Qualifier::Node(QualifierNode::Neighbours)) =>
		Ok(AssertionTypes::RangeNeighbours),
	    (s, q) =>
		Err(format!("Wrong use of qualifier {q} on variable {s}."))
	}
    }

    fn new_context(input: HashMap<Self, NodeRelablerInputValues>)
		   -> HashMap<Self, AssertReturnValue> {
	input.iter().map(|(key, value)| {
	    match value {
		NodeRelablerInputValues::Entities(e) =>
		    (*key, AssertReturnValue::Set(e.clone())),
		NodeRelablerInputValues::Node(n) =>
		    (*key, AssertReturnValue::Node(*n)),
	    }
	}).collect::<HashMap<Self, AssertReturnValue>>()
    }

    fn correct_type(&self, other: &AssertReturnValue) -> bool {
	match (self, other) {
	    (Self::Entities, AssertReturnValue::Set(_)) |
	    (Self::Node, AssertReturnValue::Node(_)) => true,
	    (_, _) => false
	}
    }
}

impl std::fmt::Display for NodeRelablerInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Entities => write!(f, "entities"),
	    Self::Node => write!(f, "node"),
	}
    }
}


impl RSassert<NodeRelablerInput> {
    pub fn typecheck(&self) -> Result<(), String> {
	let mut context = TypeContext::new();
	let ty = typecheck(&self.tree, &mut context)?;
	match ty {
	    AssertionTypes::Boolean |
	    AssertionTypes::Integer |
	    AssertionTypes::String |
	    AssertionTypes::Label |
	    AssertionTypes::Set |
	    AssertionTypes::Element |
	    AssertionTypes::Edge |
	    AssertionTypes::Node |
	    AssertionTypes::System |
	    AssertionTypes::Context =>
		Ok(()),
	    AssertionTypes::NoType =>
		Err(format!("No return type, at least one return statement \
			     required.")),
	    AssertionTypes::RangeInteger |
	    AssertionTypes::RangeSet |
	    AssertionTypes::RangeNeighbours =>
		Err(format!("Returned type {ty} is not a valid return type.")),
	}
    }

    pub fn execute(
	&self,
	graph: &graph::RSgraph,
	node: &<graph::RSgraph as petgraph::visit::GraphBase>::NodeId,
	translator: &mut translator::Translator,
    ) -> Result<AssertReturnValue, String> {
	let structure::RSsystem {available_entities: entities, ..} =
	    graph.node_weight(*node).ok_or("Missing node {{debug: {node:?}}}")?;

	let mut input_vals = HashMap::new();
	input_vals.insert(NodeRelablerInput::Entities,
			  NodeRelablerInputValues::Entities(entities.clone()));
	input_vals.insert(NodeRelablerInput::Node,
			  NodeRelablerInputValues::Node(*node));

	let mut context = Context::new(input_vals);
	if let Some(v) = execute(&self.tree, &mut context, translator, graph)? {
	    Ok(v)
	} else {
	    Err("No value returned.".into())
	}
    }
}

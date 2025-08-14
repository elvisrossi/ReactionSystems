// If changing IntegerType in assert.rs, also change from Num to another
// similar parser with different return type in grammar.lalrpop in
// AssertExpression
type IntegerType = i64;

#[derive(Debug, Clone)]
pub struct RSassert {
    pub tree: Tree
}

#[derive(Debug, Clone)]
pub enum Tree {
    Concat(Box<Tree>, Box<Tree>),
    If(Box<Expression>, Box<Tree>),
    IfElse(Box<Expression>, Box<Tree>, Box<Tree>),
    Assignment(AssignmentVariable, Box<Expression>),
    Return(Box<Expression>),
    For(Variable, Range, Box<Tree>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variable  {
    Id(String),
    Label, // special label that is the input of the function
    Edge, // special edge that is the input of the function
}

#[derive(Debug, Clone)]
pub enum AssignmentVariable {
    Var(Variable),
    QualifiedVar(Variable, QualifierRestricted)
}

#[derive(Debug, Clone)]
pub enum Expression {
    True,
    False,
    Integer(IntegerType),
    Label(Box<super::structure::RSlabel>),
    Set(super::structure::RSset),
    Element(super::translator::IdType),
    String(String),
    Var(AssignmentVariable),

    Unary(Unary, Box<Expression>),
    Binary(Binary, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Range {
    IterateOverSet(Box<Expression>),
    IterateInRange(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, Copy)]
pub enum Unary {
    Not,
    Rand,

    Empty,
    Length,
    ToStr,
    QualifierLabel(QualifierLabel),
    QualifierSystem(QualifierSystem),
    ToEl,
    Source,
    Target,
    Neighbours,
    System,
}

impl Unary {
    fn is_prefix(&self) -> bool {
	match self {
	    Self::Not |
	    Self::Rand => true,
	    Self::Empty |
	    Self::Length |
	    Self::ToStr |
	    Self::QualifierLabel(_) |
	    Self::QualifierSystem(_) |
	    Self::ToEl |
	    Self::Source |
	    Self::Target |
	    Self::Neighbours |
	    Self::System => false,
	}
    }

    fn is_suffix(&self) -> bool {
	!self.is_prefix()
    }

    fn associate(
	&self,
	type_exp: &AssertionTypes
    ) -> Result<AssertionTypes, String> {
	match (self, type_exp) {
	    (Self::Not, AssertionTypes::Boolean) => {
		Ok(AssertionTypes::Boolean)
	    },
	    (Self::Rand, AssertionTypes::Integer) => {
		Ok(AssertionTypes::Integer)
	    },
	    (Self::Empty, AssertionTypes::Set) => {
		Ok(AssertionTypes::Boolean)
	    },
	    (Self::Length, AssertionTypes::Set) |
	    (Self::Length, AssertionTypes::String) => {
		Ok(AssertionTypes::Integer)
	    },
	    (Self::ToStr, AssertionTypes::Boolean) |
	    (Self::ToStr, AssertionTypes::Element) |
	    (Self::ToStr, AssertionTypes::Integer) => {
		Ok(AssertionTypes::String)
	    },
	    (Self::QualifierLabel(_), AssertionTypes::Label) => {
		Ok(AssertionTypes::Set)
	    },
	    (Self::ToEl, AssertionTypes::String) => {
		Ok(AssertionTypes::Element)
	    },
	    (Self::Source, AssertionTypes::Edge) => {
		Ok(AssertionTypes::Node)
	    },
	    (Self::Target, AssertionTypes::Edge) => {
		Ok(AssertionTypes::Node)
	    },
	    (Self::Neighbours, AssertionTypes::Node) => {
		Ok(AssertionTypes::RangeNeighbours)
	    },
	    (Self::QualifierSystem(QualifierSystem::Entities),
	     AssertionTypes::System) => {
		Ok(AssertionTypes::Set)
	    },
	    (Self::QualifierSystem(QualifierSystem::Context),
	     AssertionTypes::System) => {
		Ok(AssertionTypes::Context)
	    },
	    (Self::System, AssertionTypes::Node) => {
		Ok(AssertionTypes::System)
	    },
	    (op, type_exp) => {
		Err(format!("Expression has incompatible type with operation: \
			     {type_exp} with operation {op}."))
	    }
	}
    }
}

#[derive(Debug, Clone, Copy)]
pub enum QualifierRestricted {
    Entities,
    Context,
    Reactants,
    ReactantsAbsent,
    Inhibitors,
    InhibitorsPresent,
    Products,
}

impl QualifierRestricted {
    pub fn referenced_mut<'a>(
	&self,
	label: &'a mut super::structure::RSlabel,
    ) -> &'a mut super::structure::RSset {
	match self {
	    Self::Entities => {&mut label.available_entities},
	    Self::Context => {&mut label.context},
	    Self::Reactants => {&mut label.reactants},
	    Self::ReactantsAbsent => {&mut label.reactants_absent},
	    Self::Inhibitors => {&mut label.inhibitors},
	    Self::InhibitorsPresent => {&mut label.inhibitors_present},
	    Self::Products => {&mut label.products},
	}
    }

    pub fn referenced<'a>(
	&self,
	label: &'a super::structure::RSlabel,
    ) -> &'a super::structure::RSset {
	match self {
	    Self::Entities => {&label.available_entities},
	    Self::Context => {&label.context},
	    Self::Reactants => {&label.reactants},
	    Self::ReactantsAbsent => {&label.reactants_absent},
	    Self::Inhibitors => {&label.inhibitors},
	    Self::InhibitorsPresent => {&label.inhibitors_present},
	    Self::Products => {&label.products},
	}
    }
}

#[derive(Debug, Clone, Copy)]
pub enum QualifierLabel {
    AvailableEntities,
    AllReactants,
    AllInhibitors,
    Restricted(QualifierRestricted),
}

impl QualifierLabel {
    pub fn get(
	&self,
	l: &super::structure::RSlabel,
    ) -> AssertReturnValue {
	match self {
	    QualifierLabel::AvailableEntities => {
		AssertReturnValue::Set(l.t.clone())
	    },
	    QualifierLabel::AllReactants => {
		AssertReturnValue::Set(l.reactants.union(&l.reactants_absent))
	    },
	    QualifierLabel::AllInhibitors => {
		AssertReturnValue::Set(
		    l.inhibitors.union(&l.inhibitors_present))
	    },
	    QualifierLabel::Restricted(q) => {
		AssertReturnValue::Set(q.referenced(l).clone())
	    }
	}
    }
}

#[derive(Debug, Clone, Copy)]
pub enum QualifierSystem {
    Entities,
    Context,
}

impl QualifierSystem {
    pub fn get(
	&self,
	l: &super::structure::RSsystem,
    ) -> AssertReturnValue {
	match self {
	    Self::Context => {
		AssertReturnValue::Context(l.context_process.clone())
	    },
	    Self::Entities => {
		AssertReturnValue::Set(l.available_entities.clone())
	    }
	}
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Binary {
    And,
    Or,
    Xor,
    Less,
    LessEq,
    More,
    MoreEq,
    Eq,
    NotEq,
    Plus,
    Minus,
    Times,
    Exponential,
    Quotient,
    Reminder,
    Concat,

    SubStr,
    Min,
    Max,
    CommonSubStr,
}

impl Binary {
    fn is_prefix(&self) -> bool {
	match self {
	    Self::And |
	    Self::Or |
	    Self::Xor |
	    Self::Less |
	    Self::LessEq |
	    Self::More |
	    Self::MoreEq |
	    Self::Eq |
	    Self::NotEq |
	    Self::Plus |
	    Self::Minus |
	    Self::Times |
	    Self::Exponential |
	    Self::Quotient |
	    Self::Reminder |
	    Self::Concat => false,
	    Self::SubStr |
	    Self::Min |
	    Self::Max |
	    Self::CommonSubStr => true,
	}
    }

    fn is_suffix(&self) -> bool {
	false
    }

    fn is_infix(&self) -> bool {
	match self {
	    Self::And |
	    Self::Or |
	    Self::Xor |
	    Self::Less |
	    Self::LessEq |
	    Self::More |
	    Self::MoreEq |
	    Self::Eq |
	    Self::NotEq |
	    Self::Plus |
	    Self::Minus |
	    Self::Times |
	    Self::Exponential |
	    Self::Quotient |
	    Self::Reminder |
	    Self::Concat => true,
	    Self::SubStr |
	    Self::Min |
	    Self::Max |
	    Self::CommonSubStr => false,
	}
    }

    fn associate(
	&self,
	t1: &AssertionTypes,
	t2: &AssertionTypes
    ) -> Result<AssertionTypes, String> {
	match (self, t1, t2) {
	    (Self::And, AssertionTypes::Boolean, AssertionTypes::Boolean) |
	    (Self::Or, AssertionTypes::Boolean, AssertionTypes::Boolean) => {
		Ok(AssertionTypes::Boolean)
	    },
	    (Self::Xor, AssertionTypes::Boolean, AssertionTypes::Boolean) => {
		Ok(AssertionTypes::Boolean)
	    },
	    (Self::Xor, AssertionTypes::Set, AssertionTypes::Set)=> {
		Ok(AssertionTypes::Set)
	    },
	    (Self::Less,   AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::Less,   AssertionTypes::Set,     AssertionTypes::Set)     |
	    (Self::LessEq, AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::LessEq, AssertionTypes::Set,     AssertionTypes::Set)     |
	    (Self::More,   AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::More,   AssertionTypes::Set,     AssertionTypes::Set)     |
	    (Self::MoreEq, AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::MoreEq, AssertionTypes::Set,     AssertionTypes::Set ) => {
		Ok(AssertionTypes::Boolean)
	    },
	    (Self::Eq,    AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::Eq,    AssertionTypes::Boolean, AssertionTypes::Boolean) |
	    (Self::Eq,    AssertionTypes::Element, AssertionTypes::Element) |
	    (Self::Eq,    AssertionTypes::Label,   AssertionTypes::Label)   |
	    (Self::Eq,    AssertionTypes::String,  AssertionTypes::String)  |
	    (Self::Eq,    AssertionTypes::Set,     AssertionTypes::Set)     |
	    (Self::NotEq, AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::NotEq, AssertionTypes::Boolean, AssertionTypes::Boolean) |
	    (Self::NotEq, AssertionTypes::Element, AssertionTypes::Element) |
	    (Self::NotEq, AssertionTypes::Label,   AssertionTypes::Label)   |
	    (Self::NotEq, AssertionTypes::String,  AssertionTypes::String)  |
	    (Self::NotEq, AssertionTypes::Set,     AssertionTypes::Set) => {
		Ok(AssertionTypes::Boolean)
	    },
	    (Self::Plus, AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::Minus, AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::Times, AssertionTypes::Integer, AssertionTypes::Integer) => {
		Ok(AssertionTypes::Integer)
	    },
	    (Self::Plus, AssertionTypes::Set, AssertionTypes::Set) |
	    (Self::Minus, AssertionTypes::Set, AssertionTypes::Set) |
	    (Self::Times, AssertionTypes::Set, AssertionTypes::Set) => {
		Ok(AssertionTypes::Set)
	    },
	    (Self::Exponential, AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::Quotient, AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::Reminder, AssertionTypes::Integer, AssertionTypes::Integer) => {
		Ok(AssertionTypes::Integer)
	    },
	    (Self::Concat, AssertionTypes::String, AssertionTypes::String) => {
		Ok(AssertionTypes::String)
	    },
	    (Self::SubStr, AssertionTypes::String, AssertionTypes::String) => {
		Ok(AssertionTypes::Boolean)
	    },
	    (Self::Min, AssertionTypes::Integer, AssertionTypes::Integer) |
	    (Self::Max, AssertionTypes::Integer, AssertionTypes::Integer) => {
		Ok(AssertionTypes::Integer)
	    },
	    (Self::CommonSubStr, AssertionTypes::String, AssertionTypes::String) => {
		Ok(AssertionTypes::String)
	    },
	    _ => {
		Err(format!("Expressions have incompatible types: {t1} and \
			     {t2} with operation {self}."))
	    }
	}
    }
}


// -----------------------------------------------------------------------------
use std::collections::HashMap;
use std::fmt;

use petgraph::visit::EdgeRef;

struct TypeContext {
    data: HashMap<String, AssertionTypes>,
    return_ty: Option<AssertionTypes>
}

impl TypeContext {
    pub fn new() -> Self {
	TypeContext {
	    data: HashMap::new(),
	    return_ty: None
	}
    }

    pub fn assign(
	&mut self,
	v: &AssignmentVariable,
	ty: AssertionTypes
    ) -> Result<(), String> {
	match v {
	    AssignmentVariable::Var(Variable::Id(v)) => {
		self.data.insert(v.clone(), ty);
		Ok(())
	    },
	    AssignmentVariable::Var(Variable::Label) |
	    AssignmentVariable::Var(Variable::Edge) => {
		Ok(())
	    }
	    AssignmentVariable::QualifiedVar(Variable::Label, _) => {
		Ok(())
	    },
	    AssignmentVariable::QualifiedVar(Variable::Edge, q) =>
		Err(format!("Variable does not have type label while trying to \
			     assign to qualification {q}, it's an edge instead.\
			     ")),
	    AssignmentVariable::QualifiedVar(Variable::Id(v), q) => {
		if let Some(AssertionTypes::Label) = self.data.get(v) {
		    Ok(())
		} else {
		    Err(format!("Variable {v} does not have type label while \
				 trying to assign to qualification {q}"))
		}
	    }
	}
    }

    pub fn return_type(
	&mut self,
	ty: AssertionTypes
    ) -> Result<(), String> {
	if let Some(ty_return) = self.return_ty {
	    if ty_return == ty {
		Ok(())
	    } else {
		Err(format!("Return statements don't agree: {ty_return} and \
			     {ty} found."))
	    }
	} else {
	    self.return_ty = Some(ty);
	    Ok(())
	}
    }

    pub fn assign_range(
	&mut self,
	v: &Variable,
	ty: AssertionTypes
    ) -> Result<(), String> {
	let v = match v {
	    Variable::Label |
	    Variable::Edge => return Err("Protected word label used in for \
					  assignment.".into()),
	    Variable::Id(v) => v
	};
	match ty {
	    AssertionTypes::RangeSet => {
		self.data.insert(v.clone(), AssertionTypes::Element);
		Ok(())
	    },
	    AssertionTypes::RangeInteger => {
		self.data.insert(v.clone(), AssertionTypes::Integer);
		Ok(())
	    },
	    AssertionTypes::RangeNeighbours => {
		self.data.insert(v.clone(), AssertionTypes::Edge);
		Ok(())
	    },
	    _ => {
		Err(format!("Range has incorrect type {ty}."))
	    },
	}
    }

    pub fn get(
	&self,
	v: &AssignmentVariable,
    ) -> Result<AssertionTypes, String> {
	match v {
	    AssignmentVariable::Var(Variable::Label) => {
		Ok(AssertionTypes::Label)
	    },
	    AssignmentVariable::Var(Variable::Edge) => {
		Ok(AssertionTypes::Edge)
	    },
	    AssignmentVariable::Var(Variable::Id(v)) => {
		if let Some(ty) = self.data.get(v) {
		    Ok(*ty)
		} else {
		    Err(format!("Could not find variable {v}."))
		}
	    },
	    AssignmentVariable::QualifiedVar(Variable::Label, _) => {
		Ok(AssertionTypes::Set)
	    },
	    AssignmentVariable::QualifiedVar(Variable::Edge, _) => {
		Err("Variable edge is not a label.".into())
	    },
	    AssignmentVariable::QualifiedVar(Variable::Id(var), _) => {
		if let Some(ty) = self.data.get(var) {
		    if *ty == AssertionTypes::Label {
			Ok(AssertionTypes::Set)
		    } else {
			Err(format!("Variable {var} is not a label."))
		    }
		} else {
		    Err(format!("Could not find variable {v}."))
		}
	    }
	}
    }
}

struct Context<'a> {
    data: HashMap<String, AssertReturnValue>,
    label: &'a super::structure::RSlabel,
    edge: &'a petgraph::graph::EdgeIndex,
}

impl<'a> Context<'a> {
    pub fn new(
	label: &'a super::structure::RSlabel,
	edge: &'a petgraph::graph::EdgeIndex,
    ) -> Self {
	Self { data: HashMap::new(), label, edge }
    }

    pub fn assign(
	&mut self,
	v: &AssignmentVariable,
	val: AssertReturnValue,
    ) -> Result<(), String> {
	match v {
	    AssignmentVariable::Var(v) => {
		if let Variable::Id(v) = v {
		    self.data.insert(v.clone(), val);
		}
		Ok(())
	    },
	    AssignmentVariable::QualifiedVar(Variable::Label, _) => Ok(()),
	    AssignmentVariable::QualifiedVar(Variable::Edge, q) =>
		Err(format!("Variable edge does not have type label while \
			     trying to assign to qualification {q}")),
	    AssignmentVariable::QualifiedVar(Variable::Id(v), q) => {
		match self.data.entry(v.clone()) {
		    std::collections::hash_map::Entry::Vacant(_ve) =>
			Err(format!("Variable {v} has no value while trying to \
				     assign a value to the field {q} of a label\
				     ")),
		    std::collections::hash_map::Entry::Occupied(mut oe) => {
			if let AssertReturnValue::Set(s) = val {
			    let l = oe.get_mut();
			    match l {
				AssertReturnValue::Label(l) => {
				    *q.referenced_mut(l) = s;
				    // if we modified available entities or the
				    // context we need to sync it with t
				    match q {
					QualifierRestricted::Context |
					QualifierRestricted::Entities => {
					    l.t = l.available_entities.union(
						&l.context
					    );
					},
					_ => {},
				    }
				    Ok(())
				},
				_ => {
				    Err(format!("Variable {v} does not have \
						 type label while trying to \
						 assign to qualification {q}"))
				}
			    }
			} else {
			    Err(format!("Value {val} is not a set while trying \
					 to assign to qualification {q} of \
					 variable {v}"))
			}
		    }
		}
	    }
	}
    }

    pub fn get(
	&self,
	v: &AssignmentVariable
    ) -> Result<AssertReturnValue, String> {
	match v {
	    AssignmentVariable::Var(Variable::Id(var)) => {
		self.data.get(var)
		    .cloned()
		    .ok_or(format!("Variable {v} used, but no value assigned."))
	    },
	    AssignmentVariable::QualifiedVar(Variable::Id(var), q) => {
		let val = self.data.get(var)
		    .ok_or(format!("Variable {v} used, but no value assigned."
		    ))?;
		match val {
		    AssertReturnValue::Label(l) => {
			Ok(AssertReturnValue::Set(q.referenced(l).clone()))
		    },
		    _ => {Err(format!("Variable {v} is not a label but was \
				       quantified with {q}."))}
		}
	    },
	    AssignmentVariable::Var(Variable::Label) => {
		Ok(AssertReturnValue::Label(self.label.clone()))
	    },
	    AssignmentVariable::QualifiedVar(Variable::Label, q) => {
		Ok(AssertReturnValue::Set(q.referenced(self.label).clone()))
	    },
	    AssignmentVariable::Var(Variable::Edge) => {
		Ok(AssertReturnValue::Edge(*self.edge))
	    },
	    AssignmentVariable::QualifiedVar(Variable::Edge, q) => {
		Err(format!("Variable edge is not a label but was quantified \
			     with {q}."))
	    },
	}
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum AssertionTypes {
    Boolean,
    Integer,
    String,
    Label,
    Set,
    Element,
    System,
    Context,
    NoType,
    RangeInteger,
    RangeSet,
    RangeNeighbours,

    Node,
    Edge,
}

#[derive(Debug, Clone)]
pub enum AssertReturnValue {
    Boolean(bool),
    Integer(IntegerType),
    String(String),
    Label(super::structure::RSlabel),
    Set(super::structure::RSset),
    Element(super::translator::IdType),
    Node(petgraph::graph::NodeIndex),
    Edge(petgraph::graph::EdgeIndex),
    Neighbours(petgraph::graph::NodeIndex),
    System(super::structure::RSsystem),
    Context(super::structure::RSprocess),
}

impl AssertReturnValue {
    pub fn unary(
	self,
	u: &Unary,
	translator: &mut super::translator::Translator,
	graph: &super::graph::RSgraph,
    ) -> Result<AssertReturnValue, String> {
	match (self, u) {
	    (AssertReturnValue::Boolean(b), Unary::Not) => {
		Ok(AssertReturnValue::Boolean(!b))
	    },
	    (AssertReturnValue::Integer(i), Unary::Rand) => {
		Ok(AssertReturnValue::Integer(rand::random_range(0..i)))
	    },
	    (AssertReturnValue::Set(set), Unary::Empty) => {
		Ok(AssertReturnValue::Boolean(set.is_empty()))
	    },
	    (AssertReturnValue::String(s), Unary::Empty) => {
		Ok(AssertReturnValue::Boolean(s.is_empty()))
	    },
	    (AssertReturnValue::Set(set), Unary::Length) => {
		Ok(AssertReturnValue::Integer(set.len() as i64))
	    },
	    (AssertReturnValue::String(s), Unary::Length) => {
		Ok(AssertReturnValue::Integer(s.len() as i64))
	    },
	    (AssertReturnValue::Boolean(b), Unary::ToStr) => {
		Ok(AssertReturnValue::String(format!("{b}")))
	    },
	    (AssertReturnValue::Integer(i), Unary::ToStr) => {
		Ok(AssertReturnValue::String(format!("{i}")))
	    },
	    (AssertReturnValue::Element(el), Unary::ToStr) => {
		Ok(AssertReturnValue::String(
		    translator.decode(el).ok_or(
			format!("Could not find element {el}.")
		    )?
		))
	    },
	    (AssertReturnValue::Label(l), Unary::QualifierLabel(q)) => {
		Ok(q.get(&l))
	    },
	    (AssertReturnValue::String(s), Unary::ToEl) => {
		Ok(AssertReturnValue::Element(translator.encode(s)))
	    },
	    (AssertReturnValue::Edge(edge), Unary::Source) => {
		Ok(AssertReturnValue::Node(
		    graph.edge_endpoints(edge).unwrap().0))
	    },
	    (AssertReturnValue::Edge(edge), Unary::Target) => {
		Ok(AssertReturnValue::Node(
		    graph.edge_endpoints(edge).unwrap().1))
	    },
	    (AssertReturnValue::Node(node), Unary::Neighbours) => {
		Ok(AssertReturnValue::Neighbours(node))
	    },
	    (AssertReturnValue::Node(node), Unary::System) => {
		Ok(AssertReturnValue::System(
		    graph.node_weight(node).unwrap().clone()
		))
	    },
	    (AssertReturnValue::System(sys), Unary::QualifierSystem(q)) => {
		Ok(q.get(&sys))
	    }
	    (val, u) => {
		Err(format!("Incompatible unary operation {u} on value {val}."))
	    }
	}
    }

    pub fn binary(
	self,
	b: &Binary,
	other: AssertReturnValue,
	_translator: &mut super::translator::Translator
    ) -> Result<AssertReturnValue, String> {
	use AssertReturnValue::*;
	Ok(match (b, self, other) {
	    (Binary::And, Boolean(b1), Boolean(b2)) => {Boolean(b1 && b2)},
	    (Binary::Or, Boolean(b1), Boolean(b2)) => {Boolean(b1 || b2)},
	    (Binary::Xor, Boolean(b1), Boolean(b2)) => {Boolean(b1 ^ b2)},
	    (Binary::Xor, Set(s1), Set(s2)) => {
		Set(s1.union(&s2).subtraction(&s1.intersection(&s2)))
	    },
	    (Binary::Less, Integer(i1), Integer(i2)) => {Boolean(i1 < i2)},
	    (Binary::Less, Set(s1), Set(s2)) => {
		Boolean(s1.is_subset(&s2) && !s2.is_subset(&s1))},
	    (Binary::LessEq, Integer(i1), Integer(i2)) => {Boolean(i1 <= i2)},
	    (Binary::LessEq, Set(s1), Set(s2)) => {
		Boolean(s1.is_subset(&s2))},
	    (Binary::More, Integer(i1), Integer(i2)) => {Boolean(i1 > i2)},
	    (Binary::More, Set(s1), Set(s2)) => {
		Boolean(s2.is_subset(&s1) && !s1.is_subset(&s2))},
	    (Binary::MoreEq, Integer(i1), Integer(i2)) => {Boolean(i1 >= i2)},
	    (Binary::MoreEq, Set(s1), Set(s2)) => {
		Boolean(s1.is_subset(&s2))},
	    (Binary::Eq, Integer(i1), Integer(i2)) => {Boolean(i1 == i2)},
	    (Binary::Eq, Boolean(b1), Boolean(b2)) => {Boolean(b1 == b2)},
	    (Binary::Eq, Element(el1), Element(el2)) => {Boolean(el1 == el2)},
	    (Binary::Eq, Label(l1), Label(l2)) => {Boolean(l1 == l2)},
	    (Binary::Eq, String(s1), String(s2)) => {Boolean(s1 == s2)},
	    (Binary::Eq, Set(set1), Set(set2)) => {Boolean(set1 == set2)},
	    (Binary::NotEq, Integer(i1), Integer(i2)) => {Boolean(i1 != i2)},
	    (Binary::NotEq, Boolean(b1), Boolean(b2)) => {Boolean(b1 != b2)},
	    (Binary::NotEq, Element(el1), Element(el2)) => {
		Boolean(el1 != el2)},
	    (Binary::NotEq, Label(l1), Label(l2)) => {Boolean(l1 != l2)},
	    (Binary::NotEq, String(s1), String(s2)) => {Boolean(s1 != s2)},
	    (Binary::NotEq, Set(set1), Set(set2)) => {Boolean(set1 != set2)},
	    (Binary::Plus, Integer(i1), Integer(i2)) => {Integer(i1 + i2)},
	    (Binary::Plus, Set(set1), Set(set2)) => {Set(set1.union(&set2))},
	    (Binary::Minus, Integer(i1), Integer(i2)) => {Integer(i1 - i2)},
	    (Binary::Minus, Set(set1), Set(set2)) => {
		Set(set1.subtraction(&set2))},
	    (Binary::Times, Integer(i1), Integer(i2)) => {Integer(i1 * i2)},
	    (Binary::Times, Set(set1), Set(set2)) => {
		Set(set1.intersection(&set2))},
	    (Binary::Exponential, Integer(i1), Integer(i2)) => {
		if i2 < 0 {
		    Integer(0)
		} else {
		    Integer(i1.pow(i2 as u32))}
	    },
	    (Binary::Quotient, Integer(i1), Integer(i2)) => {
		Integer(i1.div_euclid(i2))},
	    (Binary::Reminder, Integer(i1), Integer(i2)) => {
		Integer(i1.rem_euclid(i2))},
	    (Binary::Concat, String(s1), String(s2)) => {String(s1 + &s2)},
	    (Binary::SubStr, String(s1), String(s2)) => {
		let mut len = s1.len() as i64;
		for (p, c) in s1.chars().enumerate() {
		    if s2.chars().nth(p) != Some(c) {
			len = p as i64;
			break;
		    }
		}
		Integer(len)
	    },
	    (Binary::Min, Integer(i1), Integer(i2)) => {Integer(i1.min(i2))},
	    (Binary::Max, Integer(i1), Integer(i2)) => {Integer(i1.max(i2))},
	    (Binary::CommonSubStr, String(s1), String(s2)) => {
		let mut s = std::string::String::new();
		for (p, c) in s1.chars().enumerate() {
		    if s2.chars().nth(p) != Some(c) {
			break;
		    }
		    s.push(c);
		}
		String(s)
	    },
	    (b, val1, val2) =>
		return Err(format!("Operation {b} on values {val1} and {val2} \
				    could not be executed."))
	})
    }
}

fn typecheck(
    tree: &Tree,
    c: &mut TypeContext
) -> Result<AssertionTypes, String>
{
    match tree {
	Tree::Concat(t1, t2) => {
	    typecheck(t1, c)?;
	    typecheck(t2, c)
	},
	Tree::If(exp, t) => {
	    match typecheck_expression(exp, c)? {
		AssertionTypes::Boolean => {},
		_ => {return Err("Expression in if statement doesn't return a \
				  boolean.".to_string())}
	    };
	    typecheck(t, c)
	},
	Tree::IfElse(exp, t1, t2) => {
	    match typecheck_expression(exp, c)? {
		AssertionTypes::Boolean => {},
		_ => {return Err("Expression in if statement doesn't return a \
				  boolean.".into())}
	    };
	    let type_t1 = typecheck(t1, c)?;
	    let type_t2 = typecheck(t2, c)?;
	    if type_t1 == type_t2 {
		Ok(type_t1)
	    } else {
		Err("Branches of if statement do not match.".into())
	    }
	},
	Tree::Assignment(assignvar, exp) => {
	    let type_exp = typecheck_expression(exp, c)?;
	    c.assign(assignvar, type_exp)?;
	    Ok(AssertionTypes::NoType)
	},
	Tree::Return(exp) => {
	    let type_exp = typecheck_expression(exp, c)?;
	    c.return_type(type_exp)?;
	    Ok(AssertionTypes::NoType)
	},
	Tree::For(var, range, t) => {
	    let type_range = typecheck_range(range, c)?;
	    c.assign_range(var, type_range)?;
	    typecheck(t, c)
	},
    }
}

fn typecheck_expression(
    exp: &Expression,
    c: &TypeContext
) -> Result<AssertionTypes, String> {
    match exp {
	Expression::True |
	Expression::False => Ok(AssertionTypes::Boolean),
	Expression::Integer(_) => Ok(AssertionTypes::Integer),
	Expression::Label(_) => Ok(AssertionTypes::Label),
	Expression::Set(_) => Ok(AssertionTypes::Set),
	Expression::Element(_) => Ok(AssertionTypes::Element),
	Expression::String(_) => Ok(AssertionTypes::String),
	Expression::Var(v) => c.get(v),

	Expression::Unary(u, exp) => {
	    let type_exp = typecheck_expression(exp, c)?;
	    u.associate(&type_exp)
	},

	Expression::Binary(b, exp1, exp2) => {
	    let type_exp1 = typecheck_expression(exp1, c)?;
	    let type_exp2 = typecheck_expression(exp2, c)?;
	    b.associate(&type_exp1, &type_exp2)
	}
    }
}


fn typecheck_range(
    range: &Range,
    c: &mut TypeContext
) -> Result<AssertionTypes, String> {
    match range {
	Range::IterateInRange(exp1, exp2) => {
	    let type_exp1 = typecheck_expression(exp1, c)?;
	    let type_exp2 = typecheck_expression(exp2, c)?;
	    if let (AssertionTypes::Integer, AssertionTypes::Integer) =
		(type_exp1, type_exp2)
	    {
		Ok(AssertionTypes::RangeInteger)
	    } else {
		Err(format!("Expressions in range are not integers, but are: \
			     {type_exp1} and {type_exp2}."))
	    }
	},
	Range::IterateOverSet(exp) => {
	    let type_exp = typecheck_expression(exp, c)?;
	    match type_exp {
		AssertionTypes::Set =>
		    Ok(AssertionTypes::RangeSet),
		AssertionTypes::RangeNeighbours =>
		    Ok(AssertionTypes::RangeNeighbours),
		_ => Err(format!("Expressions in range is not a set or \
				  neighbours of a node, but is: {type_exp}."))
	    }
	}
    }
}

fn execute(
    tree: &Tree,
    c: &mut Context,
    translator: &mut super::translator::Translator,
    graph: &super::graph::RSgraph,
) -> Result<Option<AssertReturnValue>, String> {
    match tree {
	Tree::Concat(t1, t2) => {
	    if let Some(val) = execute(t1, c, translator, graph)? {
		Ok(Some(val))
	    } else {
		execute(t2, c, translator, graph)
	    }
	},
	Tree::If(exp, t) => {
	    let guard = execute_exp(exp, c, translator, graph)?;
	    if let AssertReturnValue::Boolean(true) = guard {
		execute(t, c, translator, graph)
	    } else {
		Ok(None)
	    }
	},
	Tree::IfElse(exp, t1, t2) => {
	    let guard = execute_exp(exp, c, translator, graph)?;
	    if let AssertReturnValue::Boolean(true) = guard {
		execute(t1, c, translator, graph)
	    } else {
		execute(t2, c, translator, graph)
	    }
	},
	Tree::Assignment(v, exp) => {
	    let val = execute_exp(exp, c, translator, graph)?;
	    c.assign(v, val)?;
	    Ok(None)
	},
	Tree::Return(exp) => {
	    Ok(Some(execute_exp(exp, c, translator, graph)?))
	},
	Tree::For(v, r, t) => {
	    let range = range_into_iter(r, c, translator, graph)?;
	    for val in range {
		c.assign(&AssignmentVariable::Var(v.clone()), val)?;
		if let Some(v) = execute(t, c, translator, graph)? {
		    return Ok(Some(v))
		}
	    }
	    Ok(None)
	},
    }
}

type RangeIterator = std::vec::IntoIter<AssertReturnValue>;

fn range_into_iter(
    range: &Range,
    c: &mut Context,
    translator: &mut super::translator::Translator,
    graph: &super::graph::RSgraph,
) -> Result<RangeIterator, String> {
    match range {
	Range::IterateOverSet(exp) => {
	    let val = execute_exp(exp, c, translator, graph)?;
	    match val {
		AssertReturnValue::Set(set) => {
		    Ok(set
		       .into_iter()
		       .map(AssertReturnValue::Element)
		       .collect::<Vec<_>>()
		       .into_iter())
		},
		AssertReturnValue::Neighbours(node) => {
		    Ok(graph
		       .edges(node)
		       .map(|x| AssertReturnValue::Edge(x.id()))
		       .collect::<Vec<_>>()
		       .into_iter())
		}
		_ => Err(format!("{val} is not a set in for cycle."))
	    }
	},
	Range::IterateInRange(exp1, exp2) => {
	    let val1 = execute_exp(exp1, c, translator, graph)?;
	    let val2 = execute_exp(exp2, c, translator, graph)?;
	    match (val1, val2) {
		(AssertReturnValue::Integer(i1),
		 AssertReturnValue::Integer(i2)) =>
		    Ok((i1..i2).map(AssertReturnValue::Integer)
		       .collect::<Vec<_>>().into_iter()),
		(val1, val2) =>
		    Err(format!("{val1}..{val2} is not a valid integer range \
				 in for cycle."))
	    }
	}
    }
}

fn execute_exp(
    exp: &Expression,
    c: &Context,
    translator: &mut super::translator::Translator,
    graph: &super::graph::RSgraph,
) -> Result<AssertReturnValue, String> {
    match exp {
	Expression::True => Ok(AssertReturnValue::Boolean(true)),
	Expression::False => Ok(AssertReturnValue::Boolean(false)),
	Expression::Integer(i) => Ok(AssertReturnValue::Integer(*i)),
	Expression::Label(l) => Ok(AssertReturnValue::Label(*l.clone())),
	Expression::Set(set) => Ok(AssertReturnValue::Set(set.clone())),
	Expression::Element(el) =>
	    Ok(AssertReturnValue::Element(*el)),
	Expression::String(s) => Ok(AssertReturnValue::String(s.clone())),
	Expression::Var(var) => c.get(var),
	Expression::Unary(u, exp) => {
	    let val = execute_exp(exp, c, translator, graph)?;
	    val.unary(u, translator, graph)
	},
	Expression::Binary(b, exp1, exp2) => {
	    let val1 = execute_exp(exp1, c, translator, graph)?;
	    let val2 = execute_exp(exp2, c, translator, graph)?;
	    val1.binary(b, val2, translator)
	},
    }
}

impl RSassert {
    pub fn typecheck(&self) -> Result<(), String> {
	let mut context = TypeContext::new();
	typecheck(&self.tree, &mut context)?;
	let ty = context.return_ty.unwrap_or(AssertionTypes::NoType);
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
	    AssertionTypes::NoType |
	    AssertionTypes::RangeInteger |
	    AssertionTypes::RangeSet |
	    AssertionTypes::RangeNeighbours =>
		Err(format!("Returned type {ty} is not a valid return type.")),
	}
    }

    pub fn execute(
	&self,
	graph: &super::graph::RSgraph,
	edge: &<super::graph::RSgraph as petgraph::visit::GraphBase>::EdgeId,
	translator: &mut super::translator::Translator,
    ) -> Result<AssertReturnValue, String> {
	let label = graph.edge_weight(*edge).unwrap();
	let mut context = Context::new(label, edge);
	if let Some(v) = execute(&self.tree, &mut context, translator, graph)? {
	    Ok(v)
	} else {
	    Err("No value returned.".into())
	}
    }
}


// -----------------------------------------------------------------------------
//                    Display Implementation for all types
// -----------------------------------------------------------------------------

impl fmt::Display for RSassert {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	write!(f, "label {{\n{}\n}}", self.tree)
    }
}

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Concat(t1, t2) => {write!(f, "{t1};\n{t2}")},
	    Self::If(exp, t) => {write!(f, "if {exp} {{\n{t}\n}}")},
	    Self::IfElse(exp, t1, t2) => {
		write!(f, "if {exp} {{\n{t1}\n}} else {{\n{t2}\n}}")
	    },
	    Self::Assignment(v, exp) => {write!(f, "{v} = {exp}")},
	    Self::Return(exp) => {write!(f, "return {exp}")},
	    Self::For(v, r, t) => {write!(f, "for {v} in {r} {{\n{t}\n}}")},
	}
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Label => {write!(f, "label")},
	    Self::Edge => {write!(f, "edge")},
	    Self::Id(s) => {write!(f, "{s}")}
	}
    }
}

impl fmt::Display for AssignmentVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Var(v) => write!(f, "{v}"),
	    Self::QualifiedVar(v, q) => write!(f, "{v}.{q}"),
	}
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::True => {write!(f, "True")},
	    Self::False => {write!(f, "False")},
	    Self::Integer(i) => {write!(f, "{i}")},
	    Self::Label(rslabel) => {write!(f, "{{debug: {rslabel:?}}}")},
	    Self::Set(set) => {write!(f, "{{debug: {set:?}}}")},
	    Self::Element(el) => {write!(f, "'{{debug: {el:?}}}'")},
	    Self::String(s) => {write!(f, r#""{s}""#)},
	    Self::Var(v) => {write!(f, "{v}")},
	    Self::Unary(u, exp) => {
		if u.is_prefix() {
		    write!(f, "{u}({exp})")
		} else if u.is_suffix() {
		    write!(f, "{exp}{u}")
		} else {
		    unreachable!()
		}
	    },
	    Self::Binary(b, exp1, exp2) => {
		if b.is_prefix() {
		    write!(f, "{b}({exp1}, {exp2})")
		} else if b.is_suffix() {
		    write!(f, "({exp1}, {exp2}){b}")
		} else if b.is_infix() {
		    write!(f, "({exp1} {b} {exp2})")
		} else {
		    unreachable!()
		}
	    },
	}
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::IterateOverSet(exp) => {write!(f, "{{{exp}}}")},
	    Self::IterateInRange(exp1, exp2) => {write!(f, "{exp1}..{exp2}")}
	}
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Not => write!(f, "not"),
	    Self::Rand => write!(f, "rand"),
	    Self::Empty => write!(f, ".empty"),
	    Self::Length => write!(f, ".length"),
	    Self::ToStr => write!(f, ".tostr"),
	    Self::QualifierLabel(q) => write!(f, ".{q}"),
	    Self::QualifierSystem(q) => write!(f, ".{q}"),
	    Self::ToEl => write!(f, ".toel"),
	    Self::Source => write!(f, ".source"),
	    Self::Target => write!(f, ".target"),
	    Self::Neighbours => write!(f, ".neighbours"),
	    Self::System => write!(f, ".system"),
	}
    }
}

impl fmt::Display for QualifierRestricted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Entities => write!(f, "Entities"),
	    Self::Context => write!(f, "Context"),
	    Self::Reactants => write!(f, "Reactants"),
	    Self::ReactantsAbsent => write!(f, "ReactantsAbsent"),
	    Self::Inhibitors => write!(f, "Inhibitors"),
	    Self::InhibitorsPresent => write!(f, "InhibitorsPresent"),
	    Self::Products => write!(f, "Products"),
	}
    }
}

impl fmt::Display for QualifierLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::AvailableEntities => write!(f, "AvailableEntities"),
	    Self::AllReactants => write!(f, "AllReactants"),
	    Self::AllInhibitors => write!(f, "AllInhibitors"),
	    Self::Restricted(q) => write!(f, "{q}"),
	}
    }
}

impl fmt::Display for QualifierSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match self {
	    Self::Context => write!(f, "context"),
	    Self::Entities => write!(f, "entities"),
	}
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::And => write!(f, "&&"),
	    Self::Or => write!(f, "||"),
	    Self::Xor => write!(f, "^^"),
	    Self::Less => write!(f, "<"),
	    Self::LessEq => write!(f, "<="),
	    Self::More => write!(f, ">"),
	    Self::MoreEq => write!(f, ">="),
	    Self::Eq => write!(f, "=="),
	    Self::NotEq => write!(f, "!="),
	    Self::Plus => write!(f, "+"),
	    Self::Minus => write!(f, "-"),
	    Self::Times => write!(f, "*"),
	    Self::Exponential => write!(f, "^"),
	    Self::Quotient => write!(f, "/"),
	    Self::Reminder => write!(f, "%"),
	    Self::Concat => write!(f, "::"),
	    Self::SubStr => write!(f, "substr"),
	    Self::Min => write!(f, "min"),
	    Self::Max => write!(f, "max"),
	    Self::CommonSubStr => write!(f, "commonsubstr"),
	}
    }
}

impl fmt::Display for AssertReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Boolean(b) => write!(f, "{b}"),
	    Self::Integer(i) => write!(f, "{i}"),
	    Self::String(s) => write!(f, r#""{s}""#),
	    Self::Label(l) => write!(f, "{{debug: {l:?}}}"),
	    Self::Set(set) => write!(f, "{{debug: {set:?}}}"),
	    Self::Element(el) => write!(f, "{{debug: {el:?}}}"),
	    Self::Edge(edge) => write!(f, "{{debug: {edge:?}}}"),
	    Self::Node(node) => write!(f, "{{debug: {node:?}}}"),
	    Self::Neighbours(node) =>
		write!(f, "{{debug: {node:?}}}.neighbours"),
	    Self::System(sys) => write!(f, "{{debug: {sys:?}}}"),
	    Self::Context(ctx) => write!(f, "{{debug: {ctx:?}}}"),
	}
    }
}

impl fmt::Display for AssertionTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Boolean => write!(f, "boolean"),
	    Self::Integer => write!(f, "integer"),
	    Self::String => write!(f, "string"),
	    Self::Label => write!(f, "label"),
	    Self::Set => write!(f, "set"),
	    Self::Element => write!(f, "element"),
	    Self::System => write!(f, "system"),
	    Self::Context => write!(f, "context"),
	    Self::NoType => write!(f, "no type"),
	    Self::RangeInteger => write!(f, "range of integers"),
	    Self::RangeSet => write!(f, "range of set"),
	    Self::RangeNeighbours => write!(f, "range of edges"),
	    Self::Edge => write!(f, "edge"),
	    Self::Node => write!(f, "node"),
	}
    }
}

// -----------------------------------------------------------------------------
//                                  Testing
// -----------------------------------------------------------------------------

#[test]
fn assert_tycheck_true() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {tree: Tree::Return(Box::new(Expression::True))};
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_concat_1() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Assignment(
		AssignmentVariable::Var(Variable::Id("a".into())),
		Box::new(Expression::True))),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_concat_2() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(Tree::Assignment(
		AssignmentVariable::Var(Variable::Id("a".into())),
		Box::new(Expression::Integer(10)))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_return_1() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(Tree::Return(Box::new(Expression::False))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_return_incompatible_1() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(Tree::Return(Box::new(Expression::Integer(10)))),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_return_incompatible_2() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(
		Tree::Concat(
		    Box::new(Tree::Assignment(
			AssignmentVariable::Var(Variable::Id("a".into())),
			Box::new(Expression::Integer(10)))),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::Var(AssignmentVariable::Var(
				Variable::Id("a".into())
			    ))
			)
		    ))),
	    )
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_return_2() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(
		Tree::Concat(
		    Box::new(Tree::Assignment(
			AssignmentVariable::Var(Variable::Id("a".into())),
			Box::new(Expression::False))),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::Var(AssignmentVariable::Var(
				Variable::Id("a".into())
			    ))
			)
		    ))),
	    )
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}


#[test]
fn assert_tycheck_return_3() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::Concat(
		    Box::new(Tree::Assignment(
			AssignmentVariable::Var(Variable::Id("a".into())),
			Box::new(Expression::False))),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::Var(AssignmentVariable::Var(
				Variable::Id("a".into())
			    ))
			)
		    ))),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(false))));
}

#[test]
fn assert_tycheck_if_1() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(
		Tree::If(
		    Box::new(
			Expression::True
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )
		    )
		),
	    )
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_if_2() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::If(
		    Box::new(
			Expression::True
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )
		    )
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());

    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(false))));
}

#[test]
fn assert_tycheck_if_3() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::If(
		    Box::new(
			Expression::False
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )
		    )
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_if_4() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::If(
		    Box::new(
			Expression::Integer(10)
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )
		    )
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_if_else_1() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(
		Tree::IfElse(
		    Box::new(
			Expression::True
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )),
		),
	    )
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_if_else_2() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::IfElse(
		    Box::new(
			Expression::False
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )),
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(false))));
}

#[test]
fn assert_tycheck_if_else_3() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::IfElse(
		    Box::new(
			Expression::Integer(10)
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )),
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_if_else_4() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::IfElse(
		    Box::new(
			Expression::True
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::Integer(10))
		    )),
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_assignment_1() {
    let tree = RSassert {
	tree: Tree::Assignment(
	    AssignmentVariable::Var(Variable::Id("a".into())),
	    Box::new(Expression::True)
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_assignment_2() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::True)
		)
	    ),
	    Box::new(
		Tree::Return(
		    Box::new(
			Expression::True
		    )
		)
	    ),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_assignment_3() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::False)
		)
	    ),
	    Box::new(
		Tree::Return(
		    Box::new(
			Expression::Var(
			    AssignmentVariable::Var(Variable::Id("a".into()))
			)
		    )
		)
	    ),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(false))));
}

#[test]
fn assert_tycheck_assignment_4() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::True)
		)
	    ),
	    Box::new(
		Tree::Return(
		    Box::new(
			Expression::Var(
			    AssignmentVariable::Var(Variable::Id("b".into()))
			)
		    )
		)
	    ),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_assignment_5() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Assignment(
		AssignmentVariable::Var(Variable::Id("a".into())),
		Box::new(Expression::Integer(10))
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(10))));
}

#[test]
fn assert_tycheck_assignment_6() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Integer(200))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into()))
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(10))));
}

#[test]
fn assert_tycheck_assignment_7() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::True)
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::False)
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_assignment_8() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    ))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into()))
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(10))));
}

#[test]
fn assert_tycheck_assignment_9() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    ))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into()))
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Integer(200))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(10))));
}

#[test]
fn assert_tycheck_assignment_10() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    ))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Integer(200))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into()))
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(10))));
}


#[test]
fn assert_tycheck_for_1() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Set(RSset::new()))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Set(RSset::new()))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateOverSet(
		    Box::new(
			Expression::Var(
			    AssignmentVariable::Var(Variable::Id("a".into()))
			)
		    )
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Integer(200))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(tree.execute(&graph, &edge, &mut translator).is_err());
}


#[test]
fn assert_tycheck_for_2() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Set(RSset::from([1, 2])))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Set(RSset::new()))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateOverSet(
		    Box::new(
			Expression::Var(
			    AssignmentVariable::Var(Variable::Id("a".into()))
			)
		    )
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into()))
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();
    translator.encode("one");
    translator.encode("two");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Set(_))));
}

#[test]
fn assert_tycheck_for_3() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Label(
			Box::new(RSlabel::from(RSset::from([1, 2]),
					       RSset::new(),
					       RSset::from([1, 2]),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new()))
		    ))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Set(RSset::new()))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateOverSet(
		    Box::new(
			Expression::Var(
			    AssignmentVariable::QualifiedVar(
				Variable::Id("a".into()),
				QualifierRestricted::Entities
			    )
			)
		    )
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into()))
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();
    translator.encode("one");
    translator.encode("two");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Set(_))));
}

#[test]
fn assert_tycheck_for_4() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Label(
			Box::new(RSlabel::from(RSset::from([1, 2]),
					       RSset::from([3]),
					       RSset::from([1, 2, 3]),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new()))
		    ))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Set(RSset::new()))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateOverSet(
		    Box::new(
			Expression::Unary(
			    Unary::QualifierLabel(QualifierLabel::AvailableEntities),
			    Box::new(Expression::Var(
				AssignmentVariable::Var(
				    Variable::Id("a".into())
				)
			    ))
			)
		    )
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("c".into()))
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();
    translator.encode("one");
    translator.encode("two");
    translator.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Element(_))));
}

#[test]
fn assert_tycheck_for_5() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Label(
			Box::new(RSlabel::from(RSset::from([1, 2]),
					       RSset::from([3]),
					       RSset::from([1, 2, 3]),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new()))
		    ))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Integer(0))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::For(
		    Variable::Id("c".into()),
		    Range::IterateOverSet(
			Box::new(
			    Expression::Unary(
				Unary::QualifierLabel(QualifierLabel::AvailableEntities),
				Box::new(Expression::Var(
				    AssignmentVariable::Var(
					Variable::Id("a".into())
				    )
				))
			    )
			)
		    ),
		    Box::new(Tree::Assignment(
			AssignmentVariable::Var(Variable::Id("b".into())),
			Box::new(Expression::Binary(
			    Binary::Plus,
			    Box::new(Expression::Var(
				AssignmentVariable::Var(Variable::Id("b".into()))
			    )),
			    Box::new(Expression::Integer(1))
			))
		    )),
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into()))
		    ))
		))
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();
    translator.encode("one");
    translator.encode("two");
    translator.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(3))));
}

#[test]
fn assert_tycheck_for_6() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Label(
			Box::new(RSlabel::from(RSset::from([1, 2]),
					       RSset::from([3]),
					       RSset::from([1, 2, 3]),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new()))
		    ))
		)),
		Box::new(Tree::Concat(
		    Box::new(Tree::Assignment(
			AssignmentVariable::Var(Variable::Id("b".into())),
			Box::new(Expression::Set(RSset::from([2])))
		    )),
		    Box::new(Tree::Assignment(
			AssignmentVariable::Var(Variable::Id("c".into())),
			Box::new(Expression::Integer(0))
		    )),
		))
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::For(
		    Variable::Id("d".into()),
		    Range::IterateOverSet(
			Box::new(
			    Expression::Binary(
				Binary::Plus,
				Box::new(Expression::Var(
				    AssignmentVariable::QualifiedVar(
					Variable::Id("a".into()),
					QualifierRestricted::Context
				    )
				)),
				Box::new(Expression::Var(
				    AssignmentVariable::Var(
					Variable::Id("b".into())
				    )
				))
			    )
			)
		    ),
		    Box::new(Tree::Assignment(
			AssignmentVariable::Var(Variable::Id("c".into())),
			Box::new(Expression::Binary(
			    Binary::Plus,
			    Box::new(Expression::Var(
				AssignmentVariable::Var(Variable::Id("c".into()))
			    )),
			    Box::new(Expression::Integer(1))
			))
		    )),
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("c".into()))
		    ))
		))
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();
    translator.encode("one");
    translator.encode("two");
    translator.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());


    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(2))));
}

#[test]
fn assert_tycheck_for_7() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Integer(0))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Integer(10))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateInRange(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    )),
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into())),
		    )),
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("c".into()))
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());


    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(0))));
}

#[test]
fn assert_tycheck_for_8() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("a".into())),
		    Box::new(Expression::Unary(
			Unary::Neighbours,
			Box::new(Expression::Unary(
			    Unary::Source,
			    Box::new(Expression::Var(
				AssignmentVariable::Var(Variable::Edge)
			    ))
			))
		    ))
		)),
		Box::new(Tree::Assignment(
		    AssignmentVariable::Var(Variable::Id("b".into())),
		    Box::new(Expression::Integer(0))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::For(
		    Variable::Id("c".into()),
		    Range::IterateOverSet(
			Box::new(Expression::Var(
			    AssignmentVariable::Var(Variable::Id("a".into()))
			)),
		    ),
		    Box::new(Tree::Assignment(
			AssignmentVariable::Var(Variable::Id("b".into())),
			Box::new(Expression::Binary(
			    Binary::Plus,
			    Box::new(Expression::Var(
				AssignmentVariable::Var(
				    Variable::Id("b".into())
				)
			    )),
			    Box::new(Expression::Integer(1))
			))
		    )),
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("b".into()))
		    ))
		))
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(1))));


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());
    let node_3 = graph.add_node(RSsystem::new());
    graph.add_edge(node_1, node_3, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Integer(2))));
}

#[test]
fn assert_tycheck_system() {
    use super::translator::Translator;
    use super::structure::{RSsystem, RSlabel, RSset, RSenvironment, RSprocess};
    use std::rc::Rc;

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Assignment(
		AssignmentVariable::Var(Variable::Id("a".into())),
		Box::new(Expression::Unary(
		    Unary::QualifierSystem(QualifierSystem::Entities),
		    Box::new(Expression::Unary(
			Unary::System,
			Box::new(Expression::Unary(
			    Unary::Target,
			    Box::new(Expression::Var(
				AssignmentVariable::Var(Variable::Edge)
			    ))
			))
		    ))
		))
	    )),
	    Box::new(Tree::Return(
		Box::new(Expression::Binary(
		    Binary::Less,
		    Box::new(Expression::Var(
			AssignmentVariable::Var(Variable::Id("a".into()))
		    )),
		    Box::new(Expression::Set(RSset::from([1, 2])))
		))
	    )),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut translator = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(
	RSsystem::from(
	    Rc::new(RSenvironment::new()),
	    RSset::from([2]),
	    RSprocess::Nill,
	    Rc::new(vec![])
	)
    );
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    println!("{:?}", tree.execute(&graph, &edge, &mut translator));

    assert!(matches!(tree.execute(&graph, &edge, &mut translator),
		     Ok(AssertReturnValue::Boolean(true))));
}

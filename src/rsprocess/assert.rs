// If changing IntegerType in assert.rs, also change from Num to another
// similar parser with different return type in grammar.lalrpop in
// AssertExpression
type IntegerType = i64;

#[derive(Debug, Clone)]
pub struct RSassert<S> {
    pub tree: Tree<S>
}

#[derive(Debug, Clone)]
pub enum Tree<S> {
    Concat(Box<Tree<S>>, Box<Tree<S>>),
    If(Box<Expression<S>>, Box<Tree<S>>),
    IfElse(Box<Expression<S>>, Box<Tree<S>>, Box<Tree<S>>),
    Assignment(Variable<S>, Option<Qualifier>, Box<Expression<S>>),
    Return(Box<Expression<S>>),
    For(Variable<S>, Range<S>, Box<Tree<S>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variable<S> {
    Id(String),
    Special(S)
}

trait SpecialVariables<G>: Display + std::fmt::Debug + Sized + Eq + Copy
    + std::hash::Hash
{
    fn type_of(&self) -> AssertionTypes;
    fn type_qualified(&self, q: &Qualifier) -> Result<AssertionTypes, String>;
    fn new_context(input: HashMap<Self, G>)
		   -> HashMap<Self, AssertReturnValue>;
    fn correct_type(&self, other: &AssertReturnValue) -> bool;
    // fn correct_type_qualified(&self, q: &Qualifier, other: &AssertReturnValue)
    //			      -> bool;
}

#[derive(Debug, Clone)]
pub enum Expression<S> {
    True,
    False,
    Integer(IntegerType),
    Label(Box<super::structure::RSlabel>),
    Set(super::structure::RSset),
    Element(super::translator::IdType),
    String(String),
    Var(Variable<S>),

    Unary(Unary, Box<Expression<S>>),
    Binary(Binary, Box<Expression<S>>, Box<Expression<S>>),
}

#[derive(Debug, Clone)]
pub enum Range<S> {
    IterateOverSet(Box<Expression<S>>),
    IterateInRange(Box<Expression<S>>, Box<Expression<S>>),
}

#[derive(Debug, Clone, Copy)]
pub enum Unary {
    Not,
    Rand,

    Empty,
    Length,
    ToStr,
    Qualifier(Qualifier),
    ToEl,
    Source,
    Target,
    Neighbours,
    System,
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

#[derive(Debug, Clone, Copy)]
pub enum QualifierLabel {
    AvailableEntities,
    AllReactants,
    AllInhibitors,
}

#[derive(Debug, Clone, Copy)]
pub enum QualifierSystem {
    Entities,
    Context,
}

#[derive(Debug, Clone, Copy)]
pub enum Qualifier {
    System(QualifierSystem),
    Label(QualifierLabel),
    Restricted(QualifierRestricted),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
    pub fn assign_qualified(&mut self, q: Qualifier, val: AssertReturnValue)
			-> Result<(), String> {
	match (self, q, val) {
	    (Self::Label(l),
	     Qualifier::Restricted(q),
	     AssertReturnValue::Set(set)) => {
		*q.referenced_mut(l) = set;
		Ok(())
	    },
	    (s, q, val) => {
		Err(format!("Cannot assign {val} to {s} with qualifier {q}"))
	    }
	}
    }
}



// -----------------------------------------------------------------------------
//                         Implementations for types
// -----------------------------------------------------------------------------

impl QualifierRestricted {
    pub(super) fn referenced_mut<'a>(
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

    pub(super) fn referenced<'a>(
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

    pub(super) fn get(
	&self,
	label: &super::structure::RSlabel,
    ) -> AssertReturnValue {
	AssertReturnValue::Set(self.referenced(label).clone())
    }
}

impl QualifierLabel {
    pub(super) fn get(
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
	}
    }
}

impl QualifierSystem {
    pub(super) fn get(
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

impl Unary {
    fn is_prefix(&self) -> bool {
	match self {
	    Self::Not |
	    Self::Rand => true,
	    Self::Empty |
	    Self::Length |
	    Self::ToStr |
	    Self::Qualifier(_) |
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
	    (Self::Qualifier(Qualifier::Label(_)), AssertionTypes::Label) => {
		Ok(AssertionTypes::Set)
	    },
	    (Self::Qualifier(Qualifier::Restricted(_)),
	     AssertionTypes::Label) => {
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
	    (Self::Qualifier(
		Qualifier::System(QualifierSystem::Entities)),
	     AssertionTypes::System) => {
		Ok(AssertionTypes::Set)
	    },
	    (Self::Qualifier(
		Qualifier::System(QualifierSystem::Context)),
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
	    (Self::Exponential,
	     AssertionTypes::Integer,
	     AssertionTypes::Integer) |
	    (Self::Quotient,
	     AssertionTypes::Integer,
	     AssertionTypes::Integer) |
	    (Self::Reminder,
	     AssertionTypes::Integer,
	     AssertionTypes::Integer) => {
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
	    (Self::CommonSubStr,
	     AssertionTypes::String,
	     AssertionTypes::String) => {
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
use std::fmt::{self, Display};

use petgraph::visit::EdgeRef;

struct TypeContext {
    data: HashMap<String, AssertionTypes>,
    return_ty: Option<AssertionTypes>
}

struct Context<S> {
    data: HashMap<String, AssertReturnValue>,
    special: HashMap<S, AssertReturnValue>,
}

impl TypeContext {
    fn new() -> Self {
	TypeContext {
	    data: HashMap::new(),
	    return_ty: None
	}
    }

    fn return_type(
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
}

impl TypeContext {
    fn assign<S, G>(
	&mut self,
	v: &Variable<S>,
	q: Option<&Qualifier>,
	ty: AssertionTypes
    ) -> Result<(), String>
    where S: SpecialVariables<G> {
	match (v, q) {
	    (Variable::Id(v), None) => {
		self.data.insert(v.clone(), ty);
		Ok(())
	    },
	    (Variable::Id(v), Some(q)) => {
		match self.data.entry(v.clone()) {
		    std::collections::hash_map::Entry::Vacant(_ve) => {
			Err(format!("Variable {v} as no assignment while \
				     trying to assign to qualification {q}, \
				     assign first a value."))
		    },
		    std::collections::hash_map::Entry::Occupied(oe) => {
			match (oe.get(), q, ty) {
			    (AssertionTypes::Label,
			     Qualifier::Restricted(_),
			     AssertionTypes::Set) => {
				Ok(())
			    },
			    (t, q, ty) => {
				Err(format!("Variable {v} has type {t}, but \
					     was assigned with qualifier {q} \
					     value with type {ty}"))
			    }
			}
		    }
		}
	    },
	    (Variable::Special(s), None) => {
		if s.type_of() == ty {
		    Ok(())
		} else {
		    Err(format!("Variable {s} has type {} but was \
				 assigned a value of type {ty}.", s.type_of()))
		}
	    },
	    (Variable::Special(s), Some(q)) => {
		if s.type_qualified(q)? == ty {
		    Ok(())
		} else {
		    Err(format!("Variable {s} has type {} but was \
				 assigned a value of type {ty} with qualifier \
				 {q}.", s.type_of()))
		}
	    }
	}
    }

    fn assign_range<S, G>(
	&mut self,
	v: &Variable<S>,
	ty: AssertionTypes
    ) -> Result<(), String>
    where S: SpecialVariables<G> {
	let v = match v {
	    Variable::Special(s) =>
		return Err(format!("Protected word {s} used in for \
				    assignment.")),
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

    fn get<S, G>(
	&self,
	v: &Variable<S>,
    ) -> Result<AssertionTypes, String>
    where S: SpecialVariables<G> {
	match v {
	    Variable::Special(s) => {
		Ok(s.type_of())
	    },
	    Variable::Id(v) => {
		if let Some(ty) = self.data.get(v) {
		    Ok(*ty)
		} else {
		    Err(format!("Could not find variable {v}."))
		}
	    },
	}
    }
}

impl<S> Context<S> {
    fn new<G>(
	input: HashMap<S, G>,
    ) -> Self
    where S: SpecialVariables<G> {
	Self { data: HashMap::new(),
	       special: S::new_context(input) }
    }

    fn assign<G>(
	&mut self,
	v: &Variable<S>,
	q: Option<&Qualifier>,
	val: AssertReturnValue,
    ) -> Result<(), String>
    where S: SpecialVariables<G> {
	match (v, q) {
	    (Variable::Id(v), None) => {
		self.data.insert(v.clone(), val);
		Ok(())
	    },
	    (Variable::Id(v), Some(q)) => {
		match self.data.entry(v.clone()) {
		    std::collections::hash_map::Entry::Vacant(_ve) => {
			Err(format!("Variable {v} as no assignment while \
				     trying to assign to qualification {q}, \
				     assign first a value."))
		    },
		    std::collections::hash_map::Entry::Occupied(mut oe) => {
			match (oe.get_mut(), q, val) {
			    (&mut AssertReturnValue::Label(ref mut l),
			     Qualifier::Restricted(q),
			     AssertReturnValue::Set(set)) => {
				*q.referenced_mut(l) = set;
				Ok(())
			    },
			    (val, q, newval) => {
				Err(format!("Variable {v} has value {val}, but \
					     was assigned with qualifier {q} \
					     new value {newval}."))
			    }
			}
		    }
		}
	    },
	    (Variable::Special(s), None) => {
		if s.correct_type(&val) {
		    if let Some(s) = self.special.get_mut(s) {
			*s = val;
		    } else {
			self.special.insert(*s, val);
		    }
		    Ok(())
		} else {
		    Err(format!("Trying to assign {val} to variable {s}."))
		}
	    },
	    (Variable::Special(s), Some(q)) => {
		if let Some(s) = self.special.get_mut(s) {
		    s.assign_qualified(*q, val)
		} else {
		    Err(format!("Trying to assign {val} to variable {s} with \
				 qualifier {q} but no value for {val} was found\
				 ."))
		}
	    }
	}
    }

    fn get<G>(
	&self,
	v: &Variable<S>,
    ) -> Result<AssertReturnValue, String>
    where S: SpecialVariables<G> {
	match v {
	    Variable::Id(var) => {
		self.data.get(var)
		    .cloned()
		    .ok_or(format!("Variable {v} used, but no value assigned."))
	    },
	    Variable::Special(s) => {
		self.special.get(s)
		    .cloned()
		    .ok_or(format!("Variable {v} used but no value assigned."))
	    },
	}
    }
}

impl AssertReturnValue {
    fn unary(
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
	    (AssertReturnValue::Label(l),
	     Unary::Qualifier(Qualifier::Label(q))) => {
		Ok(q.get(&l))
	    },
	    (AssertReturnValue::Label(l),
	     Unary::Qualifier(Qualifier::Restricted(q))) => {
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
	    (AssertReturnValue::System(sys),
	     Unary::Qualifier(Qualifier::System(q))) => {
		Ok(q.get(&sys))
	    }
	    (val, u) => {
		Err(format!("Incompatible unary operation {u} on value {val}."))
	    }
	}
    }

    fn binary(
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

fn typecheck<S, G>(
    tree: &Tree<S>,
    c: &mut TypeContext
) -> Result<AssertionTypes, String>
where S: SpecialVariables<G>
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
	Tree::Assignment(assignvar, q, exp) => {
	    let type_exp = typecheck_expression(exp, c)?;
	    c.assign(assignvar, q.into(), type_exp)?;
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

fn typecheck_expression<S, G>(
    exp: &Expression<S>,
    c: &TypeContext
) -> Result<AssertionTypes, String>
where S: SpecialVariables<G> {
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


fn typecheck_range<S, G>(
    range: &Range<S>,
    c: &mut TypeContext
) -> Result<AssertionTypes, String>
where S: SpecialVariables<G> {
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

fn execute<S, G>(
    tree: &Tree<S>,
    c: &mut Context<S>,
    translator: &mut super::translator::Translator,
    graph: &super::graph::RSgraph,
) -> Result<Option<AssertReturnValue>, String>
where S: SpecialVariables<G> {
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
	Tree::Assignment(v, q, exp) => {
	    let val = execute_exp(exp, c, translator, graph)?;
	    c.assign(v, q.into(), val)?;
	    Ok(None)
	},
	Tree::Return(exp) => {
	    Ok(Some(execute_exp(exp, c, translator, graph)?))
	},
	Tree::For(v, r, t) => {
	    let range = range_into_iter(r, c, translator, graph)?;
	    for val in range {
		c.assign(v, None, val)?;
		if let Some(v) = execute(t, c, translator, graph)? {
		    return Ok(Some(v))
		}
	    }
	    Ok(None)
	},
    }
}

type RangeIterator = std::vec::IntoIter<AssertReturnValue>;

fn range_into_iter<S, G>(
    range: &Range<S>,
    c: &mut Context<S>,
    translator: &mut super::translator::Translator,
    graph: &super::graph::RSgraph,
) -> Result<RangeIterator, String>
where S: SpecialVariables<G> {
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

fn execute_exp<S, G>(
    exp: &Expression<S>,
    c: &Context<S>,
    translator: &mut super::translator::Translator,
    graph: &super::graph::RSgraph,
) -> Result<AssertReturnValue, String>
where S: SpecialVariables<G> {
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






// ----------------------------------------------------------------------------
//                       Specific Assert Implementation
// ----------------------------------------------------------------------------

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum EdgeRelablerInput {
    Label,
    Edge,
}

#[derive(Debug, Clone)]
pub enum EdgeRelablerInputValues {
    Label(super::structure::RSlabel),
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
		Err(format!("Wrong use of qualifier {q} on value {s}"))
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

    // fn correct_type_qualified(&self, q: &Qualifier, other: &AssertReturnValue)
    //			      -> bool {
    //	match (self, q, other) {
    //	    (Self::Label, Qualifier::Label(_), AssertReturnValue::Set(_)) |
    //	    (Self::Label, Qualifier::Restricted(_), AssertReturnValue::Set(_))
    //		=> true,
    //	    (_, _, _) => false
    //	}
    // }
}

impl Display for EdgeRelablerInput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match self {
	    Self::Label => write!(f, "label"),
	    Self::Edge => write!(f, "edge"),
	}
    }
}





impl RSassert<EdgeRelablerInput> {
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

include!("assert_fmt.rs");

include!("assert_tests.rs");

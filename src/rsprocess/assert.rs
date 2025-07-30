#![allow(dead_code)]

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

pub type Variable = String;

#[derive(Debug, Clone)]
pub enum AssignmentVariable {
    Var(Variable),
    QualifiedVar(Variable, QualifierRestricted)
}

impl fmt::Display for AssignmentVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Var(v) => write!(f, "{v}"),
	    Self::QualifiedVar(v, q) => write!(f, "{v}.{q}"),
	}
    }
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
    Qualifier(Qualifier),
}

impl Unary {
    fn associated_types_unary(&self) -> Vec<AssertionTypes> {
	match self {
	    Unary::Not          => vec![AssertionTypes::Boolean],
	    Unary::Rand         => vec![AssertionTypes::Integer],
	    Unary::Empty        => vec![AssertionTypes::Set],
	    Unary::Length       => vec![AssertionTypes::String,
					AssertionTypes::Set],
	    Unary::ToStr        => vec![AssertionTypes::Boolean,
					AssertionTypes::Integer,
					AssertionTypes::Element],
	    Unary::Qualifier(_) => vec![AssertionTypes::Set]
	}
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Unary::Not => write!(f, "not"),
	    Unary::Rand => write!(f, "rand"),
	    Unary::Empty => write!(f, ".empty"),
	    Unary::Length => write!(f, ".length"),
	    Unary::ToStr => write!(f, ".tostr"),
	    Unary::Qualifier(q) => write!(f, ".{q}"),
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

impl fmt::Display for QualifierRestricted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    QualifierRestricted::Entities =>
		write!(f, "Entities"),
	    QualifierRestricted::Context =>
		write!(f, "Context"),
	    QualifierRestricted::Reactants =>
		write!(f, "Reactants"),
	    QualifierRestricted::ReactantsAbsent =>
		write!(f, "ReactantsAbsent"),
	    QualifierRestricted::Inhibitors =>
		write!(f, "Inhibitors"),
	    QualifierRestricted::InhibitorsPresent =>
		write!(f, "InhibitorsPresent"),
	    QualifierRestricted::Products =>
		write!(f, "Products"),
	}
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Qualifier {
    AvailableEntities,
    AllReactants,
    AllInhibitors,
    Restricted(QualifierRestricted),
}

impl fmt::Display for Qualifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::AvailableEntities => write!(f, "AvailableEntities"),
	    Self::AllReactants => write!(f, "AllReactants"),
	    Self::AllInhibitors => write!(f, "AllInhibitors"),
	    Self::Restricted(q) => write!(f, "{q}"),
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
    fn associate(
	&self,
	t1: &AssertionTypes,
	t2: &AssertionTypes
    ) -> Result<AssertionTypes, String> {
	//
	match self {
	    Self::And |
	    Self::Or => {
		if let (AssertionTypes::Boolean, AssertionTypes::Boolean) =
		    (t1, t2)
		{
		    return Ok(AssertionTypes::Boolean)
		}
	    },
	    Self::Xor => {
		match (t1, t2) {
		    (AssertionTypes::Boolean, AssertionTypes::Boolean) =>
			return Ok(AssertionTypes::Boolean),
		    (AssertionTypes::Set, AssertionTypes::Set) =>
			return Ok(AssertionTypes::Set),
		    _ => {}
		}
	    }
	    Self::Less |
	    Self::LessEq |
	    Self::More |
	    Self::MoreEq =>
		match (t1, t2) {
		    (AssertionTypes::Integer, AssertionTypes::Integer) |
		    (AssertionTypes::Set,     AssertionTypes::Set    ) =>
			return Ok(AssertionTypes::Boolean),
		    _ => {}
		}
	    Self::Eq |
	    Self::NotEq => {
		match (t1, t2) {
		    (AssertionTypes::Integer, AssertionTypes::Integer) |
		    (AssertionTypes::Boolean, AssertionTypes::Boolean) |
		    (AssertionTypes::Element, AssertionTypes::Element) |
		    (AssertionTypes::Label,   AssertionTypes::Label)   |
		    (AssertionTypes::String,  AssertionTypes::String)  |
		    (AssertionTypes::Set,     AssertionTypes::Set    ) =>
			return Ok(AssertionTypes::Boolean),
		    _ => {}
		}
	    },
	    Self::Plus |
	    Self::Minus |
	    Self::Times => {
		match (t1, t2) {
		    (AssertionTypes::Integer, AssertionTypes::Integer) =>
			return Ok(AssertionTypes::Integer),
		    (AssertionTypes::Set,     AssertionTypes::Set    ) =>
			return Ok(AssertionTypes::Set),
		    _ => {}
		}
	    },
	    Self::Exponential |
	    Self::Quotient |
	    Self::Reminder => {
		if let (AssertionTypes::Integer, AssertionTypes::Integer) =
		    (t1, t2) {
		    return Ok(AssertionTypes::Integer)
		}
	    },
	    Self::Concat => {
		if let (AssertionTypes::String, AssertionTypes::String) =
		    (t1, t2) {
		    return Ok(AssertionTypes::String)
		}
	    },
	    Self::SubStr => {
		if let (AssertionTypes::String, AssertionTypes::String) =
		    (t1, t2) {
		    return Ok(AssertionTypes::Boolean)
		}
	    },
	    Self::Min |
	    Self::Max => {
		if let (AssertionTypes::Integer, AssertionTypes::Integer) =
		    (t1, t2) {
		    return Ok(AssertionTypes::Integer)
		}
	    },
	    Self::CommonSubStr => {
		if let (AssertionTypes::String, AssertionTypes::String) =
		    (t1, t2) {
		    return Ok(AssertionTypes::String)
		}
	    },
	}
	Err(format!("Expressions have incompatible types: {t1} and {t2} with \
		     operation {self}."))
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


use core::fmt;
// -----------------------------------------------------------------------------
use std::collections::HashMap;

struct Context {
    data: HashMap<Variable, AssertionTypes>,
    ty_return: Option<AssertionTypes>
}

impl Context {
    pub fn new() -> Self {
	Context {
	    data: HashMap::new(),
	    ty_return: None
	}
    }

    pub fn assign(
	&mut self,
	v: &AssignmentVariable,
	ty: AssertionTypes
    ) -> Result<(), String> {
	match v {
	    AssignmentVariable::Var(v) => {
		self.data.insert(v.clone(), ty);
		Ok(())
	    },
	    AssignmentVariable::QualifiedVar(v, q) => {
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
	if let Some(ty_return) = self.ty_return {
	    if ty_return == ty {
		Ok(())
	    } else {
		Err(format!("Return statements don't agree: {ty_return} and \
			     {ty} found."))
	    }
	} else {
	    self.ty_return = Some(ty);
	    Ok(())
	}
    }

    pub fn assign_range(
	&mut self,
	v: &Variable,
	ty: AssertionTypes
    ) -> Result<(), String> {
	match ty {
	    AssertionTypes::RangeSet => {
		self.data.insert(v.clone(), AssertionTypes::Element);
		Ok(())
	    },
	    AssertionTypes::RangeInteger => {
		self.data.insert(v.clone(), AssertionTypes::Integer);
		Ok(())
	    },
	    _ => {
		Err(format!("Range has incorrect type {ty}."))
	    }
	}
    }

    pub fn get(
	&self,
	v: &AssignmentVariable,
    ) -> Result<AssertionTypes, String> {
	match v {
	    AssignmentVariable::Var(v) => {
		if let Some(ty) = self.data.get(v) {
		    Ok(*ty)
		} else {
		    Err(format!("Could not find variable {v}."))
		}
	    },
	    AssignmentVariable::QualifiedVar(var, _) => {
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

#[derive(Copy, Clone, PartialEq, Eq)]
enum AssertionTypes {
    Boolean,
    Integer,
    String,
    Label,
    Set,
    Element,
    NoType,
    RangeInteger,
    RangeSet,
}

impl fmt::Display for AssertionTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    AssertionTypes::Boolean => write!(f, "boolean"),
	    AssertionTypes::Integer => write!(f, "integer"),
	    AssertionTypes::String => write!(f, "string"),
	    AssertionTypes::Label => write!(f, "label"),
	    AssertionTypes::Set => write!(f, "set"),
	    AssertionTypes::Element => write!(f, "element"),
	    AssertionTypes::NoType => write!(f, "no type"),
	    AssertionTypes::RangeInteger => write!(f, "range of integers"),
	    AssertionTypes::RangeSet => write!(f, "range of set"),
	}
    }
}

fn typecheck(tree: &Tree, c: &mut Context) -> Result<AssertionTypes, String> {
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
    c: &Context
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
	    if let Unary::Qualifier(_) = u {
		let type_exp = typecheck_expression(exp, c)?;
		if type_exp == AssertionTypes::Label {
		    Ok(AssertionTypes::Set)
		} else {
		    Err("Trying to get the field of a label, but value is not \
			 a label.".into())
		}
	    } else {
		let type_exp = typecheck_expression(exp, c)?;
		let possible_types = u.associated_types_unary();
		if possible_types.contains(&type_exp) {
		    Ok(type_exp)
		} else {
		    Err(format!("Cannot use operation {u} on type {type_exp}"))
		}
	    }
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
    c: &mut Context
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
	    if let AssertionTypes::Set = type_exp {
		Ok(AssertionTypes::RangeSet)
	    } else {
		Err(format!("Expressions in range is not a set, but is: \
			     {type_exp}."))
	    }
	}
    }
}

impl RSassert {
    pub fn typecheck(&self) -> Result<(), String> {
	let mut context = Context::new();
	typecheck(&self.tree, &mut context)?;
	Ok(())
    }
}

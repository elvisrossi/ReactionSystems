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

#[derive(Debug, Clone)]
pub struct Variable {
    data: String,
}

impl Variable {
    pub fn from(data: String) -> Self {
	Self { data }
    }
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
    Qualifier(Qualifier)
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
pub enum Qualifier {
    AvailableEntities,
    AllReactants,
    AllInhibitors,
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

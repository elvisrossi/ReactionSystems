#![allow(dead_code)]

type IntegerType = i64;

#[derive(Debug, Clone)]
pub struct RSassert {
    pub tree: Tree
}

#[derive(Debug, Clone)]
pub enum Tree {
    Concat(Box<Tree>, Box<Tree>),
    If(Box<Boolean>, Box<Tree>),
    IfElse(Box<Boolean>, Box<Tree>, Box<Tree>),
    Assignment(AssignmentVar, Box<Expression>),
    Return(Box<Expression>),
    For(Variable, Range, Box<Tree>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    B(Boolean),
    A(Arithmetic),
    Lab(Label),
    Set(Set),
    El(Element),
    Str(Str),

    Var(Variable), // for when the type should just be passed through
}

#[derive(Debug, Clone)]
pub enum Boolean {
    Var(Variable), // should be of type boolean
    True,
    False,
    Not(Box<Boolean>),
    And(Box<Boolean>, Box<Boolean>),
    Or(Box<Boolean>, Box<Boolean>),
    Xor(Box<Boolean>, Box<Boolean>),

    Less(Arithmetic, Arithmetic),
    LessEq(Arithmetic, Arithmetic),
    More(Arithmetic, Arithmetic),
    MoreEq(Arithmetic, Arithmetic),
    Eq(Arithmetic, Arithmetic),
    NotEq(Arithmetic, Arithmetic),

    LabelEq(Label, Label),
    LabelNotEq(Label, Label),

    SetSub(Set, Set),
    SetSubEq(Set, Set),
    SetSuper(Set, Set),
    SetSuperEq(Set, Set),
    SetEq(Set, Set),
    SetEmpty(Set),

    ElEq(Element, Element),
    ElNotEq(Element, Element),

    StrEq(Str, Str),
    StrNotEq(Str, Str),
    StrSubstring(Str, Str),
}

#[derive(Debug, Clone)]
pub enum Arithmetic {
    Var(Variable), // should be of type arithmetic
    Integer(IntegerType),
    Plus(Box<Arithmetic>, Box<Arithmetic>),
    Minus(Box<Arithmetic>, Box<Arithmetic>),
    Times(Box<Arithmetic>, Box<Arithmetic>),
    Quotient(Box<Arithmetic>, Box<Arithmetic>),
    Reminder(Box<Arithmetic>, Box<Arithmetic>),
    Exponential(Box<Arithmetic>, Box<Arithmetic>),
    Rand(Box<Arithmetic>, Box<Arithmetic>),
    Min(Box<Arithmetic>, Box<Arithmetic>),
    Max(Box<Arithmetic>, Box<Arithmetic>),

    SetLength(Set),
    StrLength(Str),
}

#[derive(Debug, Clone)]
pub enum Label {
    Var(Variable), // should be of type label
    Literal(Box<super::structure::RSlabel>),
}

#[derive(Debug, Clone)]
pub enum Set {
    Var(Variable), // should be of type set
    Literal(super::structure::RSset),
    Append(Box<Set>, Element),
    Union(Box<Set>, Box<Set>),
    Intersection(Box<Set>, Box<Set>),
    Difference(Box<Set>, Box<Set>),
    SimmetricDifference(Box<Set>, Box<Set>),
    ValueOfLabel(Label, Qualifier),
}

#[derive(Debug, Clone)]
pub enum Element {
    Var(Variable), // should be of type element
    Literal(super::translator::IdType),
}

#[derive(Debug, Clone)]
pub enum Str {
    Var(Variable), // should be of type string
    Literal(String),
    Concat(Box<Str>, Box<Str>),
    ArithmeticToString(Box<Arithmetic>),
    BooleanToString(Box<Boolean>),
    ElementToString(Element),
    CommonSubstring(Box<Str>, Box<Str>),
}

#[derive(Debug, Clone)]
pub enum AssignmentVar {
    Var(Variable),
    ValueOfVariable(Variable, QualifierAssignment),
}

#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
}

#[derive(Debug, Clone, Copy)]
pub enum QualifierAssignment {
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
    Entities,
    Context,
    AvailableEntities,

    Reactants,
    ReactantsAbsent,
    AllReactants,

    Inhibitors,
    InhibitorsPresent,
    AllInhibitors,

    Products,
}

#[derive(Debug, Clone)]
pub enum Range {
    IterateOverSet(Set),
    IterateInRange(Box<Arithmetic>, Box<Arithmetic>),
}

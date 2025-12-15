use std::collections::HashMap;

use rsprocess::set::BasicSet;
use rsprocess::{element, graph, label, process, set, system, translator};
use serde::{Deserialize, Serialize};

/// If changing IntegerType in assert.rs, also change from Num to another
/// similar parser with different return type in grammar.lalrpop in
/// AssertExpression
type IntegerType = i64;

#[derive(Clone, Serialize, Deserialize, Hash)]
pub struct PositiveAssert<S> {
    pub tree: PositiveTree<S>,
}

#[derive(Clone, Serialize, Deserialize, Hash)]
pub enum PositiveTree<S> {
    Concat(Box<PositiveTree<S>>, Box<PositiveTree<S>>),
    If(Box<PositiveExpression<S>>, Box<PositiveTree<S>>),
    IfElse(
        Box<PositiveExpression<S>>,
        Box<PositiveTree<S>>,
        Box<PositiveTree<S>>,
    ),
    Assignment(
        PositiveVariable<S>,
        Option<PositiveQualifier>,
        Box<PositiveExpression<S>>,
    ),
    Return(Box<PositiveExpression<S>>),
    For(PositiveVariable<S>, PositiveRange<S>, Box<PositiveTree<S>>),
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PositiveVariable<S> {
    Id(String),
    Special(S),
}

/// Trait needed for special variables.
pub(super) trait SpecialVariables<G>:
    translator::PrintableWithTranslator
    + std::fmt::Debug
    + Sized
    + Eq
    + Copy
    + std::hash::Hash
{
    /// Returns the type of the specific special variable.
    fn type_of(&self) -> PositiveAssertionTypes;

    /// Returns the type of the qualified special variable.
    fn type_qualified(
        &self,
        q: &PositiveQualifier,
    ) -> Result<PositiveAssertionTypes, String>;

    /// Creates a new context.
    fn new_context(
        input: HashMap<Self, G>,
    ) -> HashMap<Self, PositiveAssertReturnValue>;

    /// Returns true if
    fn correct_type(&self, other: &PositiveAssertReturnValue) -> bool;
}

#[derive(Clone, Serialize, Deserialize, Hash)]
pub enum PositiveExpression<S> {
    True,
    False,
    Integer(IntegerType),
    Label(Box<label::PositiveLabel>),
    Set(set::PositiveSet),
    PositiveElement(element::PositiveType),
    String(String),
    Var(PositiveVariable<S>),

    Unary(PositiveUnary, Box<PositiveExpression<S>>),
    Binary(
        PositiveBinary,
        Box<PositiveExpression<S>>,
        Box<PositiveExpression<S>>,
    ),
}

#[derive(Clone, Serialize, Deserialize, Hash)]
pub enum PositiveRange<S> {
    IterateOverSet(Box<PositiveExpression<S>>),
    IterateInRange(Box<PositiveExpression<S>>, Box<PositiveExpression<S>>),
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum PositiveUnary {
    Not,
    Rand,

    Empty,
    Length,
    ToStr,
    ToEl,

    First,
    Second,

    Qualifier(PositiveQualifier),
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum QualifierRestricted {
    Entities,
    Context,
    Reactants,
    ReactantsAbsent,
    Inhibitors,
    InhibitorsPresent,
    Products,
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum QualifierLabel {
    AvailableEntities,
    AllReactants,
    AllInhibitors,
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum QualifierSystem {
    Entities,
    Context,
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum QualifierContext {
    IsNill,
    IsIdentifier,
    IsSet,
    IsGuarded,
    IsRepeated,
    IsSummation,
    IsNondeterministicChoice,

    GetSet,
    GetGuardReactants,
    GetGuardProducts,
    GetRepeatedCounter,
    GetRepeatedProcess,

    GetNextProcesses,
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum QualifierEdge {
    Source,
    Target,
    Label,
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum QualifierNode {
    Neighbours,
    System,
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum PositiveQualifier {
    System(QualifierSystem),
    Context(QualifierContext),
    Label(QualifierLabel),
    Restricted(QualifierRestricted),
    Edge(QualifierEdge),
    Node(QualifierNode),
}

#[derive(Clone, Copy, Serialize, Deserialize, Hash)]
pub enum PositiveBinary {
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
    Tuple,

    SubStr,
    Min,
    Max,
    CommonSubStr,
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub(super) enum PositiveAssertionTypes {
    Boolean,
    Integer,
    String,
    Tuple((Box<PositiveAssertionTypes>, Box<PositiveAssertionTypes>)),
    Label,
    Set,
    PositiveElement,
    System,
    Context,
    NoType,
    RangeInteger,
    RangeSet,
    RangeNeighbours,
    RangeContexts,

    Node,
    Edge,
}

#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum PositiveAssertReturnValue {
    Boolean(bool),
    Integer(IntegerType),
    String(String),
    Tuple(
        (
            Box<PositiveAssertReturnValue>,
            Box<PositiveAssertReturnValue>,
        ),
    ),
    Label(label::PositiveLabel),
    Set(set::PositiveSet),
    PositiveElement(element::PositiveType),
    Node(petgraph::graph::NodeIndex),
    Edge(petgraph::graph::EdgeIndex),
    Neighbours(petgraph::graph::NodeIndex),
    System(system::PositiveSystem),
    Context(process::PositiveProcess),
    RangeContext(process::PositiveProcess),
}

// -----------------------------------------------------------------------------
//                         Implementations for types
// -----------------------------------------------------------------------------

impl<S> Default for PositiveAssert<S> {
    fn default() -> Self {
        Self {
            tree: PositiveTree::Return(Box::new(PositiveExpression::True)),
        }
    }
}

impl QualifierRestricted {
    pub(super) fn referenced_mut<'a>(
        &self,
        label: &'a mut label::PositiveLabel,
    ) -> &'a mut set::PositiveSet {
        match self {
            | Self::Entities => &mut label.available_entities,
            | Self::Context => &mut label.context,
            | Self::Reactants => &mut label.reactants,
            | Self::ReactantsAbsent => &mut label.reactants_absent,
            | Self::Inhibitors => &mut label.inhibitors,
            | Self::InhibitorsPresent => &mut label.inhibitors_present,
            | Self::Products => &mut label.products,
        }
    }

    pub(super) fn referenced<'a>(
        &self,
        label: &'a label::PositiveLabel,
    ) -> &'a set::PositiveSet {
        match self {
            | Self::Entities => &label.available_entities,
            | Self::Context => &label.context,
            | Self::Reactants => &label.reactants,
            | Self::ReactantsAbsent => &label.reactants_absent,
            | Self::Inhibitors => &label.inhibitors,
            | Self::InhibitorsPresent => &label.inhibitors_present,
            | Self::Products => &label.products,
        }
    }

    pub(super) fn get(
        &self,
        label: &label::PositiveLabel,
    ) -> PositiveAssertReturnValue {
        PositiveAssertReturnValue::Set(self.referenced(label).clone())
    }
}

impl QualifierLabel {
    pub(super) fn get(
        &self,
        l: &label::PositiveLabel,
    ) -> PositiveAssertReturnValue {
        match self {
            | QualifierLabel::AvailableEntities =>
                PositiveAssertReturnValue::Set(l.t.clone()),
            | QualifierLabel::AllReactants => PositiveAssertReturnValue::Set(
                l.reactants.union(&l.reactants_absent),
            ),
            | QualifierLabel::AllInhibitors => PositiveAssertReturnValue::Set(
                l.inhibitors.union(&l.inhibitors_present),
            ),
        }
    }
}

impl QualifierSystem {
    pub(super) fn get(
        &self,
        l: &system::PositiveSystem,
    ) -> PositiveAssertReturnValue {
        match self {
            | Self::Context =>
                PositiveAssertReturnValue::Context(l.context_process.clone()),
            | Self::Entities =>
                PositiveAssertReturnValue::Set(l.available_entities.clone()),
        }
    }
}

impl QualifierContext {
    pub(super) fn get(
        &self,
        l: &process::PositiveProcess,
    ) -> PositiveAssertReturnValue {
        use process::PositiveProcess::*;
        match self {
            | Self::IsNill =>
                PositiveAssertReturnValue::Boolean(matches!(l, Nill)),
            | Self::IsIdentifier => PositiveAssertReturnValue::Boolean(
                matches!(l, RecursiveIdentifier { identifier: _ }),
            ),
            | Self::IsSet =>
                PositiveAssertReturnValue::Boolean(matches!(l, EntitySet {
                    entities:     _,
                    next_process: _,
                })),
            | Self::IsGuarded =>
                PositiveAssertReturnValue::Boolean(matches!(l, Guarded {
                    reaction:     _,
                    next_process: _,
                })),
            | Self::IsRepeated =>
                PositiveAssertReturnValue::Boolean(matches!(l, WaitEntity {
                    repeat: _,
                    repeated_process: _,
                    next_process: _,
                })),
            | Self::IsSummation =>
                PositiveAssertReturnValue::Boolean(matches!(l, Summation {
                    children: _,
                })),
            | Self::IsNondeterministicChoice =>
                PositiveAssertReturnValue::Boolean(matches!(
                    l,
                    NondeterministicChoice { children: _ }
                )),

            | Self::GetSet => PositiveAssertReturnValue::Set(
                if let EntitySet {
                    entities,
                    next_process: _,
                } = l
                {
                    entities.clone()
                } else {
                    Default::default()
                },
            ),
            | Self::GetGuardReactants => PositiveAssertReturnValue::Set(
                if let Guarded {
                    reaction,
                    next_process: _,
                } = l
                {
                    reaction.reactants.clone()
                } else {
                    Default::default()
                },
            ),
            | Self::GetGuardProducts => PositiveAssertReturnValue::Set(
                if let Guarded {
                    reaction,
                    next_process: _,
                } = l
                {
                    reaction.products.clone()
                } else {
                    Default::default()
                },
            ),
            | Self::GetRepeatedCounter => PositiveAssertReturnValue::Integer(
                if let WaitEntity {
                    repeat,
                    repeated_process: _,
                    next_process: _,
                } = l
                {
                    *repeat
                } else {
                    0
                },
            ),
            | Self::GetRepeatedProcess => {
                PositiveAssertReturnValue::Context(
                    if let WaitEntity {
                        repeat: _,
                        repeated_process,
                        next_process: _,
                    } = l
                    {
                        (**repeated_process).clone()
                    } else {
                        Default::default() // nill
                    },
                )
            },

            | Self::GetNextProcesses =>
                PositiveAssertReturnValue::RangeContext(l.clone()),
        }
    }
}

impl PositiveUnary {
    pub(super) fn is_prefix(&self) -> bool {
        match self {
            | Self::Not | Self::Rand => true,
            | Self::First
            | Self::Second
            | Self::Empty
            | Self::Length
            | Self::ToStr
            | Self::Qualifier(_)
            | Self::ToEl => false,
        }
    }

    pub(super) fn is_suffix(&self) -> bool {
        !self.is_prefix()
    }

    pub(super) fn associate(
        &self,
        type_exp: &PositiveAssertionTypes,
    ) -> Result<PositiveAssertionTypes, String> {
        match (self, type_exp) {
            | (Self::Not, PositiveAssertionTypes::Boolean) =>
                Ok(PositiveAssertionTypes::Boolean),
            | (Self::Rand, PositiveAssertionTypes::Integer) =>
                Ok(PositiveAssertionTypes::Integer),
            | (Self::Empty, PositiveAssertionTypes::Set) =>
                Ok(PositiveAssertionTypes::Boolean),
            | (Self::Length, PositiveAssertionTypes::Set)
            | (Self::Length, PositiveAssertionTypes::String) =>
                Ok(PositiveAssertionTypes::Integer),
            | (Self::ToStr, PositiveAssertionTypes::Boolean)
            | (Self::ToStr, PositiveAssertionTypes::PositiveElement)
            | (Self::ToStr, PositiveAssertionTypes::Integer) =>
                Ok(PositiveAssertionTypes::String),
            | (
                Self::Qualifier(PositiveQualifier::Label(_)),
                PositiveAssertionTypes::Label,
            ) => Ok(PositiveAssertionTypes::Set),
            | (
                Self::Qualifier(PositiveQualifier::Restricted(_)),
                PositiveAssertionTypes::Label,
            ) => Ok(PositiveAssertionTypes::Set),
            | (Self::ToEl, PositiveAssertionTypes::String) =>
                Ok(PositiveAssertionTypes::PositiveElement),
            | (
                Self::Qualifier(PositiveQualifier::Edge(QualifierEdge::Source)),
                PositiveAssertionTypes::Edge,
            ) => Ok(PositiveAssertionTypes::Node),
            | (
                Self::Qualifier(PositiveQualifier::Edge(QualifierEdge::Target)),
                PositiveAssertionTypes::Edge,
            ) => Ok(PositiveAssertionTypes::Node),
            | (
                Self::Qualifier(PositiveQualifier::Edge(QualifierEdge::Label)),
                PositiveAssertionTypes::Edge,
            ) => Ok(PositiveAssertionTypes::Label),
            | (
                Self::Qualifier(PositiveQualifier::Node(
                    QualifierNode::Neighbours,
                )),
                PositiveAssertionTypes::Node,
            ) => Ok(PositiveAssertionTypes::RangeNeighbours),
            | (
                Self::Qualifier(PositiveQualifier::System(
                    QualifierSystem::Entities,
                )),
                PositiveAssertionTypes::System,
            ) => Ok(PositiveAssertionTypes::Set),
            | (
                Self::Qualifier(PositiveQualifier::Restricted(
                    QualifierRestricted::Entities,
                )),
                PositiveAssertionTypes::System,
            )
            | (
                Self::Qualifier(PositiveQualifier::Restricted(
                    QualifierRestricted::Context,
                )),
                PositiveAssertionTypes::System,
            ) => Err(format!(
                "Expression has incompatible type with operation: type \
                     system with operation \"{self:?}\" (be sure to use \
                     \".SystemEntities\" and \".SystemContext\" to refer to \
                     the system fields)."
            )),
            | (
                Self::Qualifier(PositiveQualifier::System(
                    QualifierSystem::Context,
                )),
                PositiveAssertionTypes::System,
            ) => Ok(PositiveAssertionTypes::Context),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::IsNill,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::IsIdentifier,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::IsSet,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::IsGuarded,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::IsRepeated,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::IsSummation,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::IsNondeterministicChoice,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::GetSet,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Set),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::GetGuardReactants,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Set),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::GetGuardProducts,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Set),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::GetRepeatedCounter,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Integer),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::GetRepeatedProcess,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::Context),
            | (
                Self::Qualifier(PositiveQualifier::Context(
                    QualifierContext::GetNextProcesses,
                )),
                PositiveAssertionTypes::Context,
            ) => Ok(PositiveAssertionTypes::RangeContexts),
            | (
                Self::Qualifier(PositiveQualifier::Node(QualifierNode::System)),
                PositiveAssertionTypes::Node,
            ) => Ok(PositiveAssertionTypes::System),
            | (Self::First, PositiveAssertionTypes::Tuple((a, _))) =>
                Ok(*a.clone()),
            | (Self::Second, PositiveAssertionTypes::Tuple((_, b))) =>
                Ok(*b.clone()),
            | (op, type_exp) => Err(format!(
                "Expression has incompatible type with operation: type \
                     {type_exp:?} with operation \"{op:?}\"."
            )),
        }
    }
}

impl PositiveBinary {
    pub(super) fn is_prefix(&self) -> bool {
        match self {
            | Self::And
            | Self::Or
            | Self::Xor
            | Self::Less
            | Self::LessEq
            | Self::More
            | Self::MoreEq
            | Self::Eq
            | Self::NotEq
            | Self::Plus
            | Self::Minus
            | Self::Times
            | Self::Exponential
            | Self::Quotient
            | Self::Reminder
            | Self::Concat => false,
            | Self::SubStr
            | Self::Min
            | Self::Max
            | Self::CommonSubStr
            | Self::Tuple => true,
        }
    }

    pub(super) fn is_suffix(&self) -> bool {
        false
    }

    pub(super) fn is_infix(&self) -> bool {
        match self {
            | Self::And
            | Self::Or
            | Self::Xor
            | Self::Less
            | Self::LessEq
            | Self::More
            | Self::MoreEq
            | Self::Eq
            | Self::NotEq
            | Self::Plus
            | Self::Minus
            | Self::Times
            | Self::Exponential
            | Self::Quotient
            | Self::Reminder
            | Self::Concat => true,
            | Self::SubStr
            | Self::Min
            | Self::Max
            | Self::CommonSubStr
            | Self::Tuple => false,
        }
    }

    pub(super) fn associate(
        &self,
        t1: &PositiveAssertionTypes,
        t2: &PositiveAssertionTypes,
    ) -> Result<PositiveAssertionTypes, String> {
        match (self, t1, t2) {
            | (
                Self::And,
                PositiveAssertionTypes::Boolean,
                PositiveAssertionTypes::Boolean,
            )
            | (
                Self::Or,
                PositiveAssertionTypes::Boolean,
                PositiveAssertionTypes::Boolean,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Xor,
                PositiveAssertionTypes::Boolean,
                PositiveAssertionTypes::Boolean,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Xor,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            ) => Ok(PositiveAssertionTypes::Set),
            | (
                Self::Less,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::Less,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            )
            | (
                Self::LessEq,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::LessEq,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            )
            | (
                Self::More,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::More,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            )
            | (
                Self::MoreEq,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::MoreEq,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Eq,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::Eq,
                PositiveAssertionTypes::Boolean,
                PositiveAssertionTypes::Boolean,
            )
            | (
                Self::Eq,
                PositiveAssertionTypes::PositiveElement,
                PositiveAssertionTypes::PositiveElement,
            )
            | (
                Self::Eq,
                PositiveAssertionTypes::Label,
                PositiveAssertionTypes::Label,
            )
            | (
                Self::Eq,
                PositiveAssertionTypes::String,
                PositiveAssertionTypes::String,
            )
            | (
                Self::Eq,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            )
            | (
                Self::NotEq,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::NotEq,
                PositiveAssertionTypes::Boolean,
                PositiveAssertionTypes::Boolean,
            )
            | (
                Self::NotEq,
                PositiveAssertionTypes::PositiveElement,
                PositiveAssertionTypes::PositiveElement,
            )
            | (
                Self::NotEq,
                PositiveAssertionTypes::Label,
                PositiveAssertionTypes::Label,
            )
            | (
                Self::NotEq,
                PositiveAssertionTypes::String,
                PositiveAssertionTypes::String,
            )
            | (
                Self::NotEq,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            ) => Ok(PositiveAssertionTypes::Boolean),
            | (
                Self::Plus,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::Minus,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::Times,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            ) => Ok(PositiveAssertionTypes::Integer),
            | (
                Self::Plus,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            )
            | (
                Self::Minus,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            )
            | (
                Self::Times,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::Set,
            ) => Ok(PositiveAssertionTypes::Set),
            | (
                Self::Exponential,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::Quotient,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::Reminder,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            ) => Ok(PositiveAssertionTypes::Integer),
            | (
                Self::Concat,
                PositiveAssertionTypes::String,
                PositiveAssertionTypes::String,
            ) => Ok(PositiveAssertionTypes::String),
            | (
                Self::Concat,
                PositiveAssertionTypes::Set,
                PositiveAssertionTypes::PositiveElement,
            ) => Ok(PositiveAssertionTypes::Set),
            | (
                Self::SubStr,
                PositiveAssertionTypes::String,
                PositiveAssertionTypes::String,
            ) => Ok(PositiveAssertionTypes::Integer),
            | (
                Self::Min,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            )
            | (
                Self::Max,
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            ) => Ok(PositiveAssertionTypes::Integer),
            | (
                Self::CommonSubStr,
                PositiveAssertionTypes::String,
                PositiveAssertionTypes::String,
            ) => Ok(PositiveAssertionTypes::String),
            | (Self::Tuple, a, b) => Ok(PositiveAssertionTypes::Tuple((
                Box::new(a.clone()),
                Box::new(b.clone()),
            ))),
            | _ => Err(format!(
                "Expressions have incompatible types: {t1:?} and \
                             {t2:?} with operation {self:?}."
            )),
        }
    }
}

impl PositiveAssertReturnValue {
    pub(super) fn assign_qualified(
        &mut self,
        q: PositiveQualifier,
        val: PositiveAssertReturnValue,
    ) -> Result<(), String> {
        match (self, q, val) {
            | (
                Self::Label(l),
                PositiveQualifier::Restricted(q),
                PositiveAssertReturnValue::Set(set),
            ) => {
                *q.referenced_mut(l) = set;
                Ok(())
            },
            | (s, q, val) => Err(format!(
                "Cannot assign {val:?} to {s:?} with qualifier \
                             {q:?}"
            )),
        }
    }
}

// -----------------------------------------------------------------------------
//                        Typechecking and Evaluation
// -----------------------------------------------------------------------------

pub(super) struct TypeContext {
    data:      HashMap<String, PositiveAssertionTypes>,
    return_ty: Option<PositiveAssertionTypes>,
}

pub(super) struct Context<S> {
    data:    HashMap<String, PositiveAssertReturnValue>,
    special: HashMap<S, PositiveAssertReturnValue>,
}

impl TypeContext {
    pub(super) fn new() -> Self {
        TypeContext {
            data:      HashMap::new(),
            return_ty: None,
        }
    }

    fn return_type(
        &mut self,
        ty: &PositiveAssertionTypes,
    ) -> Result<(), String> {
        if let Some(ty_return) = &self.return_ty {
            if ty_return == ty {
                Ok(())
            } else {
                Err(format!(
                    "Return statements don't agree: {ty_return:?} and \
                             {ty:?} found."
                ))
            }
        } else {
            self.return_ty = Some(ty.clone());
            Ok(())
        }
    }
}

impl TypeContext {
    fn assign<S, G>(
        &mut self,
        v: &PositiveVariable<S>,
        q: Option<&PositiveQualifier>,
        ty: PositiveAssertionTypes,
    ) -> Result<(), String>
    where
        S: SpecialVariables<G>,
    {
        match (v, q) {
            | (PositiveVariable::Id(v), None) => {
                self.data.insert(v.clone(), ty);
                Ok(())
            },
            | (PositiveVariable::Id(v), Some(q)) =>
                match self.data.entry(v.clone()) {
                    | std::collections::hash_map::Entry::Vacant(_ve) =>
                        Err(format!(
                            "Variable {v:?} as no assignment while \
                                     trying to assign to qualification {q:?}, \
                                     assign first a value."
                        )),
                    | std::collections::hash_map::Entry::Occupied(oe) =>
                        match (oe.get(), q, ty) {
                            | (
                                PositiveAssertionTypes::Label,
                                PositiveQualifier::Restricted(_),
                                PositiveAssertionTypes::Set,
                            ) => Ok(()),
                            | (t, q, ty) => Err(format!(
                                "Variable {v:?} has type {t:?}, \
                                             but was assigned with qualifier \
                                             {q:?} value with type {ty:?}."
                            )),
                        },
                },
            | (PositiveVariable::Special(s), None) =>
                if s.type_of() == ty {
                    Ok(())
                } else {
                    Err(format!(
                        "Variable {s:?} has type {:?} but was \
                                 assigned a value of type {ty:?}.",
                        s.type_of()
                    ))
                },
            | (PositiveVariable::Special(s), Some(q)) =>
                if s.type_qualified(q)? == ty {
                    Ok(())
                } else {
                    Err(format!(
                        "Variable {s:?} has type {:?} but was \
                                 assigned a value of type {ty:?} with \
                                 qualifier {q:?}.",
                        s.type_of()
                    ))
                },
        }
    }

    fn assign_range<S, G>(
        &mut self,
        v: &PositiveVariable<S>,
        ty: PositiveAssertionTypes,
    ) -> Result<(), String>
    where
        S: SpecialVariables<G>,
    {
        let v = match v {
            | PositiveVariable::Special(s) => {
                return Err(format!(
                    "Protected word {s:?} used in for \
                                    assignment."
                ));
            },
            | PositiveVariable::Id(v) => v,
        };
        match ty {
            | PositiveAssertionTypes::RangeSet => {
                self.data
                    .insert(v.clone(), PositiveAssertionTypes::PositiveElement);
                Ok(())
            },
            | PositiveAssertionTypes::RangeInteger => {
                self.data.insert(v.clone(), PositiveAssertionTypes::Integer);
                Ok(())
            },
            | PositiveAssertionTypes::RangeNeighbours => {
                self.data.insert(v.clone(), PositiveAssertionTypes::Edge);
                Ok(())
            },
            | PositiveAssertionTypes::RangeContexts => {
                self.data.insert(v.clone(), PositiveAssertionTypes::Context);
                Ok(())
            },
            | _ => Err(format!("Range has incorrect type {ty:?}.")),
        }
    }

    fn get<S, G>(
        &self,
        v: &PositiveVariable<S>,
    ) -> Result<PositiveAssertionTypes, String>
    where
        S: SpecialVariables<G>,
    {
        match v {
            | PositiveVariable::Special(s) => Ok(s.type_of()),
            | PositiveVariable::Id(v) =>
                if let Some(ty) = self.data.get(v) {
                    Ok(ty.clone())
                } else {
                    Err(format!("Could not find variable {v:?}."))
                },
        }
    }
}

impl<S> Context<S> {
    pub(super) fn new<G>(input: HashMap<S, G>) -> Self
    where
        S: SpecialVariables<G>,
    {
        Self {
            data:    HashMap::new(),
            special: S::new_context(input),
        }
    }

    fn assign<G>(
        &mut self,
        v: &PositiveVariable<S>,
        q: Option<&PositiveQualifier>,
        val: PositiveAssertReturnValue,
    ) -> Result<(), String>
    where
        S: SpecialVariables<G>,
    {
        match (v, q) {
            | (PositiveVariable::Id(v), None) => {
                self.data.insert(v.clone(), val);
                Ok(())
            },
            | (PositiveVariable::Id(v), Some(q)) => match self
                .data
                .entry(v.clone())
            {
                | std::collections::hash_map::Entry::Vacant(_ve) =>
                    Err(format!(
                        "Variable {v:?} as no assignment while \
                                     trying to assign to qualification {q:?}, \
                                     assign first a value."
                    )),
                | std::collections::hash_map::Entry::Occupied(mut oe) =>
                    match (oe.get_mut(), q, val) {
                        | (
                            &mut PositiveAssertReturnValue::Label(ref mut l),
                            PositiveQualifier::Restricted(q),
                            PositiveAssertReturnValue::Set(set),
                        ) => {
                            *q.referenced_mut(l) = set;
                            Ok(())
                        },
                        | (val, q, newval) => Err(format!(
                            "Variable {v:?} has value {val:?}, \
                                             but was assigned with qualifier \
                                             {q:?} new value {newval:?}."
                        )),
                    },
            },
            | (PositiveVariable::Special(s), None) =>
                if s.correct_type(&val) {
                    if let Some(s) = self.special.get_mut(s) {
                        *s = val;
                    } else {
                        self.special.insert(*s, val);
                    }
                    Ok(())
                } else {
                    Err(format!("Trying to assign {val:?} to variable {s:?}."))
                },
            | (PositiveVariable::Special(s), Some(q)) => {
                if let Some(s) = self.special.get_mut(s) {
                    s.assign_qualified(*q, val)
                } else {
                    Err(format!(
                        "Trying to assign {val:?} to variable {s:?} \
                                 with qualifier {q:?} but no value for {val:?} \
                                 was found."
                    ))
                }
            },
        }
    }

    fn get<G>(
        &self,
        v: &PositiveVariable<S>,
    ) -> Result<PositiveAssertReturnValue, String>
    where
        S: SpecialVariables<G>,
    {
        match v {
            | PositiveVariable::Id(var) =>
                self.data.get(var).cloned().ok_or(format!(
                    "Variable {v:?} used, but no value \
                                    assigned."
                )),
            | PositiveVariable::Special(s) =>
                self.special.get(s).cloned().ok_or(format!(
                    "Variable {v:?} used but no value \
                                    assigned."
                )),
        }
    }
}

impl PositiveAssertReturnValue {
    fn unary(
        self,
        u: &PositiveUnary,
        translator: &mut translator::Translator,
        graph: &graph::PositiveSystemGraph,
    ) -> Result<PositiveAssertReturnValue, String> {
        match (self, u) {
            | (PositiveAssertReturnValue::Boolean(b), PositiveUnary::Not) =>
                Ok(PositiveAssertReturnValue::Boolean(!b)),
            | (PositiveAssertReturnValue::Integer(i), PositiveUnary::Rand) =>
                Ok(PositiveAssertReturnValue::Integer(rand::random_range(0..i))),
            | (PositiveAssertReturnValue::Set(set), PositiveUnary::Empty) =>
                Ok(PositiveAssertReturnValue::Boolean(set.is_empty())),
            | (PositiveAssertReturnValue::String(s), PositiveUnary::Empty) =>
                Ok(PositiveAssertReturnValue::Boolean(s.is_empty())),
            | (PositiveAssertReturnValue::Set(set), PositiveUnary::Length) =>
                Ok(PositiveAssertReturnValue::Integer(set.len() as i64)),
            | (PositiveAssertReturnValue::String(s), PositiveUnary::Length) =>
                Ok(PositiveAssertReturnValue::Integer(s.len() as i64)),
            | (PositiveAssertReturnValue::Boolean(b), PositiveUnary::ToStr) =>
                Ok(PositiveAssertReturnValue::String(format!("{b}"))),
            | (PositiveAssertReturnValue::Integer(i), PositiveUnary::ToStr) =>
                Ok(PositiveAssertReturnValue::String(format!("{i}"))),
            | (
                PositiveAssertReturnValue::PositiveElement(el),
                PositiveUnary::ToStr,
            ) => Ok(PositiveAssertReturnValue::String(format!(
                "{}",
                translator::Formatter::from(translator, &el)
            ))),
            | (
                PositiveAssertReturnValue::Label(l),
                PositiveUnary::Qualifier(PositiveQualifier::Label(q)),
            ) => Ok(q.get(&l)),
            | (
                PositiveAssertReturnValue::Label(l),
                PositiveUnary::Qualifier(PositiveQualifier::Restricted(q)),
            ) => Ok(q.get(&l)),
            | (PositiveAssertReturnValue::String(s), PositiveUnary::ToEl) =>
                Ok(PositiveAssertReturnValue::PositiveElement(
                    match s.chars().nth(0) {
                        | Some('+') => (
                            translator
                                .encode(s.chars().skip(1).collect::<String>()),
                            element::IdState::Positive,
                        )
                            .into(),
                        | Some('-') => (
                            translator
                                .encode(s.chars().skip(1).collect::<String>()),
                            element::IdState::Negative,
                        )
                            .into(),
                        | _ => {
                            return Err(format!(
                                "Could not decode symbol from string {s}"
                            ));
                        },
                    },
                )),
            | (
                PositiveAssertReturnValue::Edge(edge),
                PositiveUnary::Qualifier(PositiveQualifier::Edge(
                    QualifierEdge::Source,
                )),
            ) => Ok(PositiveAssertReturnValue::Node(
                graph.edge_endpoints(edge).unwrap().0,
            )),
            | (
                PositiveAssertReturnValue::Edge(edge),
                PositiveUnary::Qualifier(PositiveQualifier::Edge(
                    QualifierEdge::Target,
                )),
            ) => Ok(PositiveAssertReturnValue::Node(
                graph.edge_endpoints(edge).unwrap().1,
            )),
            | (
                PositiveAssertReturnValue::Edge(edge),
                PositiveUnary::Qualifier(PositiveQualifier::Edge(
                    QualifierEdge::Label,
                )),
            ) => Ok(PositiveAssertReturnValue::Label(graph[edge].clone())),
            | (
                PositiveAssertReturnValue::Node(node),
                PositiveUnary::Qualifier(PositiveQualifier::Node(
                    QualifierNode::Neighbours,
                )),
            ) => Ok(PositiveAssertReturnValue::Neighbours(node)),
            | (
                PositiveAssertReturnValue::Node(node),
                PositiveUnary::Qualifier(PositiveQualifier::Node(
                    QualifierNode::System,
                )),
            ) => Ok(PositiveAssertReturnValue::System(
                graph.node_weight(node).unwrap().clone(),
            )),
            | (
                PositiveAssertReturnValue::System(sys),
                PositiveUnary::Qualifier(PositiveQualifier::System(q)),
            ) => Ok(q.get(&sys)),
            | (
                PositiveAssertReturnValue::Context(c),
                PositiveUnary::Qualifier(PositiveQualifier::Context(q)),
            ) => Ok(q.get(&c)),
            | (
                PositiveAssertReturnValue::Tuple((v1, _)),
                PositiveUnary::First,
            ) => Ok(*v1),
            | (
                PositiveAssertReturnValue::Tuple((_, v2)),
                PositiveUnary::Second,
            ) => Ok(*v2),
            | (val, u) => Err(format!(
                "Incompatible unary operation {u:?} on value \
                             {val:?}."
            )),
        }
    }

    fn binary(
        self,
        b: &PositiveBinary,
        other: PositiveAssertReturnValue,
        _translator: &mut translator::Translator,
    ) -> Result<PositiveAssertReturnValue, String> {
        use PositiveAssertReturnValue::*;
        Ok(match (b, self, other) {
            | (PositiveBinary::And, Boolean(b1), Boolean(b2)) =>
                Boolean(b1 && b2),
            | (PositiveBinary::Or, Boolean(b1), Boolean(b2)) =>
                Boolean(b1 || b2),
            | (PositiveBinary::Xor, Boolean(b1), Boolean(b2)) =>
                Boolean(b1 ^ b2),
            | (PositiveBinary::Xor, Set(s1), Set(s2)) =>
                Set(s1.union(&s2).subtraction(&s1.intersection(&s2))),
            | (PositiveBinary::Less, Integer(i1), Integer(i2)) =>
                Boolean(i1 < i2),
            | (PositiveBinary::Less, Set(s1), Set(s2)) =>
                Boolean(s1.is_subset(&s2) && !s2.is_subset(&s1)),
            | (PositiveBinary::LessEq, Integer(i1), Integer(i2)) =>
                Boolean(i1 <= i2),
            | (PositiveBinary::LessEq, Set(s1), Set(s2)) =>
                Boolean(s1.is_subset(&s2)),
            | (PositiveBinary::More, Integer(i1), Integer(i2)) =>
                Boolean(i1 > i2),
            | (PositiveBinary::More, Set(s1), Set(s2)) =>
                Boolean(s2.is_subset(&s1) && !s1.is_subset(&s2)),
            | (PositiveBinary::MoreEq, Integer(i1), Integer(i2)) =>
                Boolean(i1 >= i2),
            | (PositiveBinary::MoreEq, Set(s1), Set(s2)) =>
                Boolean(s2.is_subset(&s1)),
            | (PositiveBinary::Eq, Integer(i1), Integer(i2)) =>
                Boolean(i1 == i2),
            | (PositiveBinary::Eq, Boolean(b1), Boolean(b2)) =>
                Boolean(b1 == b2),
            | (
                PositiveBinary::Eq,
                PositiveElement(el1),
                PositiveElement(el2),
            ) => Boolean(el1 == el2),
            | (PositiveBinary::Eq, Label(l1), Label(l2)) => Boolean(l1 == l2),
            | (PositiveBinary::Eq, String(s1), String(s2)) => Boolean(s1 == s2),
            | (PositiveBinary::Eq, Set(set1), Set(set2)) =>
                Boolean(set1 == set2),
            | (PositiveBinary::NotEq, Integer(i1), Integer(i2)) =>
                Boolean(i1 != i2),
            | (PositiveBinary::NotEq, Boolean(b1), Boolean(b2)) =>
                Boolean(b1 != b2),
            | (
                PositiveBinary::NotEq,
                PositiveElement(el1),
                PositiveElement(el2),
            ) => Boolean(el1 != el2),
            | (PositiveBinary::NotEq, Label(l1), Label(l2)) =>
                Boolean(l1 != l2),
            | (PositiveBinary::NotEq, String(s1), String(s2)) =>
                Boolean(s1 != s2),
            | (PositiveBinary::NotEq, Set(set1), Set(set2)) =>
                Boolean(set1 != set2),
            | (PositiveBinary::Plus, Integer(i1), Integer(i2)) =>
                Integer(i1 + i2),
            | (PositiveBinary::Plus, Set(set1), Set(set2)) =>
                Set(set1.union(&set2)),
            | (PositiveBinary::Minus, Integer(i1), Integer(i2)) =>
                Integer(i1 - i2),
            | (PositiveBinary::Minus, Set(set1), Set(set2)) =>
                Set(set1.subtraction(&set2)),
            | (PositiveBinary::Times, Integer(i1), Integer(i2)) =>
                Integer(i1 * i2),
            | (PositiveBinary::Times, Set(set1), Set(set2)) =>
                Set(set1.intersection(&set2)),
            | (PositiveBinary::Exponential, Integer(i1), Integer(i2)) =>
                if i2 < 0 {
                    Integer(0)
                } else {
                    Integer(i1.pow(i2 as u32))
                },
            | (PositiveBinary::Quotient, Integer(i1), Integer(i2)) =>
                Integer(i1.div_euclid(i2)),
            | (PositiveBinary::Reminder, Integer(i1), Integer(i2)) =>
                Integer(i1.rem_euclid(i2)),
            | (PositiveBinary::Concat, String(s1), String(s2)) =>
                String(s1 + &s2),
            | (PositiveBinary::Concat, Set(s), PositiveElement(e)) =>
                Set(s.union(&set::PositiveSet::from([e]))),
            | (PositiveBinary::SubStr, String(s1), String(s2)) => {
                let mut len = s1.len() as i64;
                for (p, c) in s1.chars().enumerate() {
                    if s2.chars().nth(p) != Some(c) {
                        len = p as i64;
                        break;
                    }
                }
                Integer(len)
            },
            | (PositiveBinary::Min, Integer(i1), Integer(i2)) =>
                Integer(i1.min(i2)),
            | (PositiveBinary::Max, Integer(i1), Integer(i2)) =>
                Integer(i1.max(i2)),
            | (PositiveBinary::CommonSubStr, String(s1), String(s2)) => {
                let mut s = std::string::String::new();
                for (p, c) in s1.chars().enumerate() {
                    if s2.chars().nth(p) != Some(c) {
                        break;
                    }
                    s.push(c);
                }
                String(s)
            },
            | (PositiveBinary::Tuple, t1, t2) =>
                Tuple((Box::new(t1), Box::new(t2))),
            | (b, val1, val2) => {
                return Err(format!(
                    "Operation {b:?} on values {val1:?} and \
                                    {val2:?} could not be executed."
                ));
            },
        })
    }
}

fn typecheck_helper<S, G>(
    tree: &PositiveTree<S>,
    c: &mut TypeContext,
) -> Result<PositiveAssertionTypes, String>
where
    S: SpecialVariables<G>,
{
    match tree {
        | PositiveTree::Concat(t1, t2) => {
            typecheck_helper(t1, c)?;
            typecheck_helper(t2, c)
        },
        | PositiveTree::If(exp, t) => {
            match typecheck_expression(exp, c)? {
                | PositiveAssertionTypes::Boolean => {},
                | _ => {
                    return Err("Expression in if statement doesn't return a \
                                  boolean."
                        .to_string());
                },
            };
            typecheck_helper(t, c)
        },
        | PositiveTree::IfElse(exp, t1, t2) => {
            match typecheck_expression(exp, c)? {
                | PositiveAssertionTypes::Boolean => {},
                | _ => {
                    return Err("Expression in if statement doesn't return a \
                                  boolean."
                        .into());
                },
            };
            let type_t1 = typecheck_helper(t1, c)?;
            let type_t2 = typecheck_helper(t2, c)?;
            if type_t1 == type_t2 {
                Ok(type_t1)
            } else {
                Err("Branches of if statement do not match.".into())
            }
        },
        | PositiveTree::Assignment(assignvar, q, exp) => {
            let type_exp = typecheck_expression(exp, c)?;
            c.assign(assignvar, q.into(), type_exp)?;
            Ok(PositiveAssertionTypes::NoType)
        },
        | PositiveTree::Return(exp) => {
            let type_exp = typecheck_expression(exp, c)?;
            c.return_type(&type_exp)?;
            Ok(PositiveAssertionTypes::NoType)
        },
        | PositiveTree::For(var, range, t) => {
            let type_range = typecheck_range(range, c)?;
            c.assign_range(var, type_range)?;
            typecheck_helper(t, c)
        },
    }
}

pub(super) fn typecheck<S, G>(
    tree: &PositiveTree<S>,
    c: &mut TypeContext,
) -> Result<PositiveAssertionTypes, String>
where
    S: SpecialVariables<G>,
{
    typecheck_helper(tree, c)?;
    Ok(c.return_ty
        .clone()
        .unwrap_or(PositiveAssertionTypes::NoType))
}

fn typecheck_expression<S, G>(
    exp: &PositiveExpression<S>,
    c: &TypeContext,
) -> Result<PositiveAssertionTypes, String>
where
    S: SpecialVariables<G>,
{
    match exp {
        | PositiveExpression::True | PositiveExpression::False =>
            Ok(PositiveAssertionTypes::Boolean),
        | PositiveExpression::Integer(_) => Ok(PositiveAssertionTypes::Integer),
        | PositiveExpression::Label(_) => Ok(PositiveAssertionTypes::Label),
        | PositiveExpression::Set(_) => Ok(PositiveAssertionTypes::Set),
        | PositiveExpression::PositiveElement(_) =>
            Ok(PositiveAssertionTypes::PositiveElement),
        | PositiveExpression::String(_) => Ok(PositiveAssertionTypes::String),
        | PositiveExpression::Var(v) => c.get(v),

        | PositiveExpression::Unary(u, exp) => {
            let type_exp = typecheck_expression(exp, c)?;
            u.associate(&type_exp)
        },

        | PositiveExpression::Binary(b, exp1, exp2) => {
            let type_exp1 = typecheck_expression(exp1, c)?;
            let type_exp2 = typecheck_expression(exp2, c)?;
            b.associate(&type_exp1, &type_exp2)
        },
    }
}

fn typecheck_range<S, G>(
    range: &PositiveRange<S>,
    c: &mut TypeContext,
) -> Result<PositiveAssertionTypes, String>
where
    S: SpecialVariables<G>,
{
    match range {
        | PositiveRange::IterateInRange(exp1, exp2) => {
            let type_exp1 = typecheck_expression(exp1, c)?;
            let type_exp2 = typecheck_expression(exp2, c)?;
            if let (
                PositiveAssertionTypes::Integer,
                PositiveAssertionTypes::Integer,
            ) = (&type_exp1, &type_exp2)
            {
                Ok(PositiveAssertionTypes::RangeInteger)
            } else {
                Err(format!(
                    "Expressions in range are not integers, but are: \
                             {type_exp1:?} and {type_exp2:?}."
                ))
            }
        },
        | PositiveRange::IterateOverSet(exp) => {
            let type_exp = typecheck_expression(exp, c)?;
            match type_exp {
                | PositiveAssertionTypes::Set =>
                    Ok(PositiveAssertionTypes::RangeSet),
                | PositiveAssertionTypes::RangeNeighbours =>
                    Ok(PositiveAssertionTypes::RangeNeighbours),
                | PositiveAssertionTypes::RangeContexts =>
                    Ok(PositiveAssertionTypes::RangeContexts),
                | _ => Err(format!(
                    "Expressions in range is not a set or \
                                  neighbours of a node, but is: {type_exp:?}."
                )),
            }
        },
    }
}

pub(super) fn execute<S, G>(
    tree: &PositiveTree<S>,
    c: &mut Context<S>,
    translator: &mut translator::Translator,
    graph: &graph::PositiveSystemGraph,
) -> Result<Option<PositiveAssertReturnValue>, String>
where
    S: SpecialVariables<G>,
{
    match tree {
        | PositiveTree::Concat(t1, t2) => {
            if let Some(val) = execute(t1, c, translator, graph)? {
                Ok(Some(val))
            } else {
                execute(t2, c, translator, graph)
            }
        },
        | PositiveTree::If(exp, t) => {
            let guard = execute_exp(exp, c, translator, graph)?;
            if let PositiveAssertReturnValue::Boolean(true) = guard {
                execute(t, c, translator, graph)
            } else {
                Ok(None)
            }
        },
        | PositiveTree::IfElse(exp, t1, t2) => {
            let guard = execute_exp(exp, c, translator, graph)?;
            if let PositiveAssertReturnValue::Boolean(true) = guard {
                execute(t1, c, translator, graph)
            } else {
                execute(t2, c, translator, graph)
            }
        },
        | PositiveTree::Assignment(v, q, exp) => {
            let val = execute_exp(exp, c, translator, graph)?;
            c.assign(v, q.into(), val)?;
            Ok(None)
        },
        | PositiveTree::Return(exp) =>
            Ok(Some(execute_exp(exp, c, translator, graph)?)),
        | PositiveTree::For(v, r, t) => {
            let range = range_into_iter(r, c, translator, graph)?;
            for val in range {
                c.assign(v, None, val)?;
                if let Some(v) = execute(t, c, translator, graph)? {
                    return Ok(Some(v));
                }
            }
            Ok(None)
        },
    }
}

type RangeIterator = std::vec::IntoIter<PositiveAssertReturnValue>;

fn range_into_iter<S, G>(
    range: &PositiveRange<S>,
    c: &mut Context<S>,
    translator: &mut translator::Translator,
    graph: &graph::PositiveSystemGraph,
) -> Result<RangeIterator, String>
where
    S: SpecialVariables<G>,
{
    use petgraph::visit::EdgeRef;

    match range {
        | PositiveRange::IterateOverSet(exp) => {
            let val = execute_exp(exp, c, translator, graph)?;
            match val {
                | PositiveAssertReturnValue::Set(set) => Ok(set
                    .into_iter()
                    .map(|el| {
                        PositiveAssertReturnValue::PositiveElement(el.into())
                    })
                    .collect::<Vec<_>>()
                    .into_iter()),
                | PositiveAssertReturnValue::Neighbours(node) => Ok(graph
                    .edges(node)
                    .map(|x| PositiveAssertReturnValue::Edge(x.id()))
                    .collect::<Vec<_>>()
                    .into_iter()),
                | PositiveAssertReturnValue::RangeContext(ctxs) => {
                    use process::PositiveProcess::*;
                    Ok(match ctxs {
                        | Nill => vec![].into_iter(),
                        | RecursiveIdentifier { identifier: _ } =>
                            vec![].into_iter(),
                        | EntitySet {
                            entities: _,
                            next_process,
                        } => vec![PositiveAssertReturnValue::Context(
                            (*next_process).clone(),
                        )]
                        .into_iter(),
                        | Guarded {
                            reaction: _,
                            next_process,
                        } => vec![PositiveAssertReturnValue::Context(
                            (*next_process).clone(),
                        )]
                        .into_iter(),
                        | WaitEntity {
                            repeat: _,
                            repeated_process,
                            next_process: _,
                        } => vec![PositiveAssertReturnValue::Context(
                            (*repeated_process).clone(),
                        )]
                        .into_iter(),
                        | Summation { children } => children
                            .iter()
                            .map(|c| {
                                PositiveAssertReturnValue::Context(
                                    (**c).clone(),
                                )
                            })
                            .collect::<Vec<_>>()
                            .into_iter(),
                        | NondeterministicChoice { children } => children
                            .iter()
                            .map(|c| {
                                PositiveAssertReturnValue::Context(
                                    (**c).clone(),
                                )
                            })
                            .collect::<Vec<_>>()
                            .into_iter(),
                    })
                },
                | _ => Err(format!("{val:?} is not a set in for cycle.")),
            }
        },
        | PositiveRange::IterateInRange(exp1, exp2) => {
            let val1 = execute_exp(exp1, c, translator, graph)?;
            let val2 = execute_exp(exp2, c, translator, graph)?;
            match (val1, val2) {
                | (
                    PositiveAssertReturnValue::Integer(i1),
                    PositiveAssertReturnValue::Integer(i2),
                ) => Ok((i1..i2)
                    .map(PositiveAssertReturnValue::Integer)
                    .collect::<Vec<_>>()
                    .into_iter()),
                | (val1, val2) => Err(format!(
                    "{val1:?}..{val2:?} is not a valid integer \
                                 range in for cycle."
                )),
            }
        },
    }
}

fn execute_exp<S, G>(
    exp: &PositiveExpression<S>,
    c: &Context<S>,
    translator: &mut translator::Translator,
    graph: &graph::PositiveSystemGraph,
) -> Result<PositiveAssertReturnValue, String>
where
    S: SpecialVariables<G>,
{
    match exp {
        | PositiveExpression::True =>
            Ok(PositiveAssertReturnValue::Boolean(true)),
        | PositiveExpression::False =>
            Ok(PositiveAssertReturnValue::Boolean(false)),
        | PositiveExpression::Integer(i) =>
            Ok(PositiveAssertReturnValue::Integer(*i)),
        | PositiveExpression::Label(l) =>
            Ok(PositiveAssertReturnValue::Label(*l.clone())),
        | PositiveExpression::Set(set) =>
            Ok(PositiveAssertReturnValue::Set(set.clone())),
        | PositiveExpression::PositiveElement(el) =>
            Ok(PositiveAssertReturnValue::PositiveElement(*el)),
        | PositiveExpression::String(s) =>
            Ok(PositiveAssertReturnValue::String(s.clone())),
        | PositiveExpression::Var(var) => c.get(var),
        | PositiveExpression::Unary(u, exp) => {
            let val = execute_exp(exp, c, translator, graph)?;
            val.unary(u, translator, graph)
        },
        | PositiveExpression::Binary(b, exp1, exp2) => {
            let val1 = execute_exp(exp1, c, translator, graph)?;
            let val2 = execute_exp(exp2, c, translator, graph)?;
            val1.binary(b, val2, translator)
        },
    }
}

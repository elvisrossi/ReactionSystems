use std::collections::VecDeque;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use super::element::{IdState, IdType};
use super::reaction::{PositiveReaction, Reaction};
use super::set::{BasicSet, PositiveSet, Set};
use super::translator::{Formatter, PrintableWithTranslator, Translator};

pub trait BasicProcess
where
    Self: Clone
        + Debug
        + Default
        + PartialEq
        + Eq
        + Hash
        + Serialize
        + PrintableWithTranslator,
    for<'a> Self: Deserialize<'a>,
{
    type Id;
    type Set: BasicSet;

    fn concat(&self, new: &Self) -> Self;
    fn all_elements(&self) -> Self::Set;
    fn filter_delta(&self, id: &Self::Id) -> Option<&Self::Set>;
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub enum Process {
    #[default]
    Nill,
    RecursiveIdentifier {
        identifier: IdType,
    },
    EntitySet {
        entities:     Set,
        next_process: Arc<Process>,
    },
    Guarded {
        reaction:     Reaction,
        next_process: Arc<Process>,
    },
    WaitEntity {
        repeat: i64,
        repeated_process: Arc<Process>,
        next_process: Arc<Process>,
    },
    Summation {
        children: Vec<Arc<Process>>,
    },
    NondeterministicChoice {
        children: Vec<Arc<Process>>,
    },
}

impl BasicProcess for Process {
    type Id = IdType;
    type Set = Set;

    fn concat(&self, new: &Self) -> Self {
        match (self, new) {
            | (
                Self::NondeterministicChoice { children: c1 },
                Self::NondeterministicChoice { children: c2 },
            ) => Self::NondeterministicChoice {
                children: [c1.clone(), c2.clone()].concat(),
            },
            | (Self::NondeterministicChoice { children }, new)
            | (new, Self::NondeterministicChoice { children }) => {
                let mut new_children = children.clone();
                new_children.push(Arc::new(new.clone()));
                Self::NondeterministicChoice {
                    children: new_children,
                }
            },
            | (_, _) => Self::NondeterministicChoice {
                children: vec![Arc::new(self.clone()), Arc::new(new.clone())],
            },
        }
    }

    /// returns all elements used
    fn all_elements(&self) -> Self::Set {
        let mut queue = VecDeque::from([self]);
        let mut elements = Self::Set::default();

        while let Some(el) = queue.pop_front() {
            match el {
                | Self::Nill => {},
                | Self::RecursiveIdentifier { identifier: _ } => {},
                | Self::EntitySet {
                    entities,
                    next_process,
                } => {
                    elements.push(entities);
                    queue.push_back(next_process);
                },
                | Self::Guarded {
                    reaction,
                    next_process,
                } => {
                    elements.push(&reaction.reactants);
                    elements.push(&reaction.inhibitors);
                    elements.push(&reaction.products);
                    queue.push_back(next_process);
                },
                | Self::WaitEntity {
                    repeat: _,
                    repeated_process,
                    next_process,
                } => {
                    queue.push_back(repeated_process);
                    queue.push_back(next_process);
                },
                | Self::Summation { children } =>
                    for c in children {
                        queue.push_back(c);
                    },
                | Self::NondeterministicChoice { children } => {
                    for c in children {
                        queue.push_back(c);
                    }
                },
            }
        }
        elements
    }

    /// Finds only the rules X = pre(Q, rec(X)), but not only x = pre(Q, rec(x))
    /// to use in filter_map.
    fn filter_delta<'a>(&'a self, id: &Self::Id) -> Option<&'a Self::Set> {
        #[allow(clippy::collapsible_if)]
        if let Self::EntitySet {
            entities,
            next_process,
        } = self
        {
            if let Self::RecursiveIdentifier { identifier } = &**next_process {
                if identifier == id {
                    return Some(entities);
                }
            }
        }

        None
    }
}

impl PrintableWithTranslator for Process {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &Translator,
    ) -> std::fmt::Result {
        match self {
            | Self::Nill => {
                write!(f, "Nill")
            },
            | Self::RecursiveIdentifier { identifier } => {
                write!(
                    f,
                    "[{}]",
                    translator.decode(*identifier).unwrap_or("Missing".into())
                )
            },
            | Self::EntitySet {
                entities,
                next_process,
            } => {
                write!(
                    f,
                    "{}.{}",
                    Formatter::from(translator, entities),
                    Formatter::from(translator, &**next_process)
                )
            },
            | Self::Guarded {
                reaction,
                next_process,
            } => {
                write!(
                    f,
                    "?{}?.{}",
                    Formatter::from(translator, reaction),
                    Formatter::from(translator, &**next_process)
                )
            },
            | Self::WaitEntity {
                repeat,
                repeated_process,
                next_process,
            } => {
                write!(
                    f,
                    "({})^{repeat}.{}",
                    Formatter::from(translator, &**repeated_process),
                    Formatter::from(translator, &**next_process)
                )
            },
            | Self::Summation { children } => {
                write!(f, "[")?;
                let mut it = children.iter().peekable();
                while let Some(child) = it.next() {
                    if it.peek().is_none() {
                        write!(f, "{}", Formatter::from(translator, &**child))?;
                    } else {
                        write!(
                            f,
                            "{} + ",
                            Formatter::from(translator, &**child)
                        )?;
                    }
                }
                write!(f, "]")
            },
            | Self::NondeterministicChoice { children } => {
                write!(f, "[")?;
                let mut it = children.iter().peekable();
                while let Some(child) = it.next() {
                    if it.peek().is_none() {
                        write!(f, "{}", Formatter::from(translator, &**child))?;
                    } else {
                        write!(
                            f,
                            "{}, ",
                            Formatter::from(translator, &**child)
                        )?;
                    }
                }
                write!(f, "]")
            },
        }
    }
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub enum PositiveProcess {
    #[default]
    Nill,
    RecursiveIdentifier {
        identifier: IdType,
    },
    EntitySet {
        entities:     PositiveSet,
        next_process: Arc<PositiveProcess>,
    },
    Guarded {
        reaction:     PositiveReaction,
        next_process: Arc<PositiveProcess>,
    },
    WaitEntity {
        repeat: i64,
        repeated_process: Arc<PositiveProcess>,
        next_process: Arc<PositiveProcess>,
    },
    Summation {
        children: Vec<Arc<PositiveProcess>>,
    },
    NondeterministicChoice {
        children: Vec<Arc<PositiveProcess>>,
    },
}

impl BasicProcess for PositiveProcess {
    type Id = IdType;
    type Set = PositiveSet;

    fn concat(&self, new: &Self) -> Self {
        match (self, new) {
            | (
                Self::NondeterministicChoice { children: c1 },
                Self::NondeterministicChoice { children: c2 },
            ) => Self::NondeterministicChoice {
                children: [c1.clone(), c2.clone()].concat(),
            },
            | (Self::NondeterministicChoice { children }, new)
            | (new, Self::NondeterministicChoice { children }) => {
                let mut new_children = children.clone();
                new_children.push(Arc::new(new.clone()));
                Self::NondeterministicChoice {
                    children: new_children,
                }
            },
            | (_, _) => Self::NondeterministicChoice {
                children: vec![Arc::new(self.clone()), Arc::new(new.clone())],
            },
        }
    }

    /// returns all elements used
    fn all_elements(&self) -> Self::Set {
        let mut queue = VecDeque::from([self]);
        let mut elements = Self::Set::default();

        while let Some(el) = queue.pop_front() {
            match el {
                | Self::Nill => {},
                | Self::RecursiveIdentifier { identifier: _ } => {},
                | Self::EntitySet {
                    entities,
                    next_process,
                } => {
                    elements.push(entities);
                    queue.push_back(next_process);
                },
                | Self::Guarded {
                    reaction,
                    next_process,
                } => {
                    elements.push(&reaction.reactants);
                    elements.push(&reaction.products);
                    queue.push_back(next_process);
                },
                | Self::WaitEntity {
                    repeat: _,
                    repeated_process,
                    next_process,
                } => {
                    queue.push_back(repeated_process);
                    queue.push_back(next_process);
                },
                | Self::Summation { children } =>
                    for c in children {
                        queue.push_back(c);
                    },
                | Self::NondeterministicChoice { children } => {
                    for c in children {
                        queue.push_back(c);
                    }
                },
            }
        }
        elements
    }

    /// Finds only the rules X = pre(Q, rec(X)), but not only x = pre(Q, rec(x))
    /// to use in filter_map.
    fn filter_delta(&self, id: &Self::Id) -> Option<&Self::Set> {
        #[allow(clippy::collapsible_if)]
        if let Self::EntitySet {
            entities,
            next_process,
        } = self
        {
            if let Self::RecursiveIdentifier { identifier } = &**next_process {
                if identifier == id {
                    return Some(entities);
                }
            }
        }
        None
    }
}

impl PrintableWithTranslator for PositiveProcess {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &Translator,
    ) -> std::fmt::Result {
        match self {
            | Self::Nill => {
                write!(f, "Nill")
            },
            | Self::RecursiveIdentifier { identifier } => {
                write!(
                    f,
                    "[{}]",
                    translator.decode(*identifier).unwrap_or("Missing".into())
                )
            },
            | Self::EntitySet {
                entities,
                next_process,
            } => {
                write!(
                    f,
                    "{}.{}",
                    Formatter::from(translator, entities),
                    Formatter::from(translator, &**next_process)
                )
            },
            | Self::Guarded {
                reaction,
                next_process,
            } => {
                write!(
                    f,
                    "?{}?.{}",
                    Formatter::from(translator, reaction),
                    Formatter::from(translator, &**next_process)
                )
            },
            | Self::WaitEntity {
                repeat,
                repeated_process,
                next_process,
            } => {
                write!(
                    f,
                    "({})^{repeat}.{}",
                    Formatter::from(translator, &**repeated_process),
                    Formatter::from(translator, &**next_process)
                )
            },
            | Self::Summation { children } => {
                write!(f, "[")?;
                let mut it = children.iter().peekable();
                while let Some(child) = it.next() {
                    if it.peek().is_none() {
                        write!(f, "{}", Formatter::from(translator, &**child))?;
                    } else {
                        write!(
                            f,
                            "{} + ",
                            Formatter::from(translator, &**child)
                        )?;
                    }
                }
                write!(f, "]")
            },
            | Self::NondeterministicChoice { children } => {
                write!(f, "[")?;
                let mut it = children.iter().peekable();
                while let Some(child) = it.next() {
                    if it.peek().is_none() {
                        write!(f, "{}", Formatter::from(translator, &**child))?;
                    } else {
                        write!(
                            f,
                            "{}, ",
                            Formatter::from(translator, &**child)
                        )?;
                    }
                }
                write!(f, "]")
            },
        }
    }
}

impl From<&Process> for PositiveProcess {
    fn from(value: &Process) -> Self {
        match value {
            | Process::Nill => Self::Nill,
            | Process::EntitySet {
                entities,
                next_process,
            } => Self::EntitySet {
                entities:     entities.to_positive_set(IdState::Positive),
                next_process: Arc::new((&**next_process).into()),
            },
            | Process::RecursiveIdentifier { identifier } =>
                Self::RecursiveIdentifier {
                    identifier: *identifier,
                },
            | Process::Guarded {
                reaction,
                next_process,
            } =>
            // TODO: is this right?
                Self::Guarded {
                    reaction:     PositiveReaction {
                        reactants: reaction
                            .reactants
                            .to_positive_set(IdState::Positive)
                            .union(
                                &reaction
                                    .inhibitors
                                    .to_positive_set(IdState::Negative),
                            ),
                        products:  reaction
                            .products
                            .to_positive_set(IdState::Positive),
                    },
                    next_process: Arc::new((&**next_process).into()),
                },
            | Process::WaitEntity {
                repeat,
                repeated_process,
                next_process,
            } => Self::WaitEntity {
                repeat: *repeat,
                repeated_process: Arc::new((&**repeated_process).into()),
                next_process: Arc::new((&**next_process).into()),
            },
            | Process::Summation { children } => Self::Summation {
                children: children
                    .iter()
                    .map(|c| Arc::new((&**c).into()))
                    .collect(),
            },
            | Process::NondeterministicChoice { children } =>
                Self::NondeterministicChoice {
                    children: children
                        .iter()
                        .map(|c| Arc::new((&**c).into()))
                        .collect(),
                },
        }
    }
}

impl From<Process> for PositiveProcess {
    fn from(value: Process) -> Self {
        (&value).into()
    }
}

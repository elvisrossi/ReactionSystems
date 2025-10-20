use std::cmp;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use serde::{Deserialize, Serialize};

use super::choices::{BasicChoices, Choices, PositiveChoices};
use super::element::IdType;
use super::process::{BasicProcess, PositiveProcess, Process};
use super::reaction::{
    BasicReaction, ExtensionReaction, PositiveReaction, Reaction,
};
use super::set::{BasicSet, PositiveSet, Set};
use super::translator::{Formatter, PrintableWithTranslator, Translator};

pub trait BasicEnvironment
where
    Self: Clone + Debug + Default + Serialize + PrintableWithTranslator,
    for<'a> Self: Deserialize<'a>,
{
    type Id;
    type Set: BasicSet;
    type Choices: BasicChoices;
    type Process: BasicProcess<Set = Self::Set, Id = Self::Id>;
    type Reaction: BasicReaction<Set = Self::Set>;

    fn get(&self, k: Self::Id) -> Option<&Self::Process>;
    fn all_elements(&self) -> Self::Set;
    fn unfold(
        &self,
        context: &Self::Process,
        entities: &Self::Set,
    ) -> Result<Self::Choices, String>;
}

pub trait ExtensionsEnvironment: BasicEnvironment {
    fn iter(&self) -> <&Self as IntoIterator>::IntoIter
    where
        for<'b> &'b Self: IntoIterator;
}

impl<T: BasicEnvironment> ExtensionsEnvironment for T {
    fn iter(&self) -> <&Self as IntoIterator>::IntoIter
    where
        for<'b> &'b Self: IntoIterator,
    {
        self.into_iter()
    }
}

// -----------------------------------------------------------------------------
//                                   Loops
// -----------------------------------------------------------------------------

pub trait LoopEnvironment: BasicEnvironment {
    #[allow(clippy::type_complexity)]
    fn lollipops_decomposed(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
    ) -> Vec<(Vec<Self::Set>, Vec<Self::Set>)>;

    fn lollipops_prefix_len_loop_decomposed(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
    ) -> Vec<(usize, Vec<Self::Set>)>;

    fn lollipops_only_loop_decomposed(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
    ) -> Vec<Vec<Self::Set>>;

    #[allow(clippy::type_complexity)]
    fn lollipops_decomposed_named(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
        symb: Self::Id,
    ) -> Option<(Vec<Self::Set>, Vec<Self::Set>)>;

    fn lollipops_prefix_len_loop_decomposed_named(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
        symb: Self::Id,
    ) -> Option<(usize, Vec<Self::Set>)>;

    fn lollipops_only_loop_decomposed_named(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
        symb: Self::Id,
    ) -> Option<Vec<Self::Set>>;
}

impl<T: BasicEnvironment + ExtensionsEnvironment> LoopEnvironment for T
where
    for<'a> &'a T: IntoIterator<Item = (&'a T::Id, &'a T::Process)>,
    T::Id: Eq,
{
    /// A special case of systems is when the context recursively provides
    /// always the same set of entities.  The corresponding computation is
    /// infinite.  It consists of a finite sequence of states followed by a
    /// looping sequence.  IMPORTANT: We return all loops for all X = Q.X, by
    /// varing X.  The set of reactions Rs and the context x are constant.  Each
    /// state of the computation is distinguished by the current entities E.
    /// Under these assumptions, the predicate lollipop finds the Prefixes and
    /// the Loops sequences of entities.
    /// see lollipop
    fn lollipops_decomposed(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
    ) -> Vec<(Vec<Self::Set>, Vec<Self::Set>)> {
        // FIXME: i think we are only interested in "x", not all symbols that
        // satisfy X = pre(Q, rec(X))
        let filtered = self.iter().filter_map(|l| l.1.filter_delta(l.0));

        let find_loop_fn = |q| {
            T::Reaction::find_loop(
                reaction_rules,
                available_entities.clone(),
                q,
            )
        };

        filtered.map(find_loop_fn).collect::<Vec<_>>()
    }

    fn lollipops_prefix_len_loop_decomposed(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
    ) -> Vec<(usize, Vec<Self::Set>)> {
        let filtered = self.iter().filter_map(|l| l.1.filter_delta(l.0));

        let find_loop_fn = |q| {
            T::Reaction::find_prefix_len_loop(
                reaction_rules,
                available_entities.clone(),
                q,
            )
        };

        filtered.map(find_loop_fn).collect::<Vec<_>>()
    }

    /// see loop
    fn lollipops_only_loop_decomposed(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
    ) -> Vec<Vec<Self::Set>> {
        let filtered = self.iter().filter_map(|l| l.1.filter_delta(l.0));

        let find_loop_fn = |q| {
            T::Reaction::find_only_loop(reaction_rules, available_entities, q)
        };

        filtered.map(find_loop_fn).collect::<Vec<_>>()
    }

    /// A special case of systems is when the context recursively provides
    /// always the same set of entities.  The corresponding computation is
    /// infinite.  It consists of a finite sequence of states followed by a
    /// looping sequence.  IMPORTANT: We return all loops for all X = Q.X, by
    /// varing X.  The set of reactions Rs and the context x are constant.  Each
    /// state of the computation is distinguished by the current entities E.
    /// Under these assumptions, the predicate lollipop finds the Prefixes and
    /// the Loops sequences of entities.
    /// see lollipop
    fn lollipops_decomposed_named(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
        symb: Self::Id,
    ) -> Option<(Vec<Self::Set>, Vec<Self::Set>)> {
        let filtered = self
            .iter()
            .filter_map(|l| {
                if *l.0 == symb {
                    l.1.filter_delta(&symb)
                } else {
                    None
                }
            })
            .next();

        let find_loop_fn = |q| {
            T::Reaction::find_loop(
                reaction_rules,
                available_entities.clone(),
                q,
            )
        };

        filtered.map(find_loop_fn)
    }

    fn lollipops_prefix_len_loop_decomposed_named(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
        symb: Self::Id,
    ) -> Option<(usize, Vec<Self::Set>)> {
        let filtered = self
            .iter()
            .filter_map(|l| {
                if *l.0 == symb {
                    l.1.filter_delta(&symb)
                } else {
                    None
                }
            })
            .next();

        let find_loop_fn = |q| {
            T::Reaction::find_prefix_len_loop(
                reaction_rules,
                available_entities.clone(),
                q,
            )
        };

        filtered.map(find_loop_fn)
    }

    /// see loop
    fn lollipops_only_loop_decomposed_named(
        &self,
        reaction_rules: &[Self::Reaction],
        available_entities: &Self::Set,
        symb: Self::Id,
    ) -> Option<Vec<Self::Set>> {
        let filtered = self
            .iter()
            .filter_map(|l| {
                if *l.0 == symb {
                    l.1.filter_delta(&symb)
                } else {
                    None
                }
            })
            .next();

        let find_loop_fn = |q| {
            T::Reaction::find_only_loop(reaction_rules, available_entities, q)
        };

        filtered.map(find_loop_fn)
    }
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Default, Serialize, Deserialize, Hash)]
pub struct Environment {
    definitions: BTreeMap<IdType, Process>,
}

impl BasicEnvironment for Environment {
    type Process = Process;
    type Set = Set;
    type Choices = Choices;
    type Id = IdType;
    type Reaction = Reaction;

    fn get(&self, k: IdType) -> Option<&Process> {
        self.definitions.get(&k)
    }

    fn all_elements(&self) -> Set {
        let mut acc = Set::default();
        for (_, process) in self.definitions.iter() {
            acc.push(&process.all_elements());
        }
        acc
    }

    /// unfold returns the list of choices for the context given the process
    /// definitions environment. choices::Choices is a list of context moves
    /// mapping a set of entities and the continuation.
    /// see unfold
    fn unfold(
        &self,
        context_process: &Process,
        current_entities: &Set,
    ) -> Result<Choices, String> {
        match context_process {
            | Process::Nill => Ok(Choices::default()),
            | Process::RecursiveIdentifier { identifier } => {
                let newprocess = self.get(*identifier);
                if let Some(newprocess) = newprocess {
                    self.unfold(newprocess, current_entities)
                } else {
                    Err(format!("Missing symbol in context: {identifier}"))
                }
            },
            | Process::EntitySet {
                entities,
                next_process,
            } => Ok(Choices::from([(
                Rc::new(entities.clone()),
                Rc::clone(next_process),
            )])),
            | Process::Guarded {
                reaction,
                next_process,
            } =>
                if reaction.enabled(current_entities) {
                    Ok(Choices::from([(
                        Rc::new(reaction.products.clone()),
                        Rc::clone(next_process),
                    )]))
                } else {
                    Ok(Choices::default())
                },
            | Process::WaitEntity {
                repeat,
                repeated_process: _,
                next_process,
            } if *repeat <= 0 => self.unfold(next_process, current_entities),
            | Process::WaitEntity {
                repeat,
                repeated_process,
                next_process,
            } if *repeat == 1 => {
                let mut choices1 =
                    self.unfold(repeated_process, current_entities)?;
                choices1.replace(Rc::clone(next_process));
                Ok(choices1)
            },
            | Process::WaitEntity {
                repeat,
                repeated_process,
                next_process,
            } => {
                let mut choices1 =
                    self.unfold(repeated_process, current_entities)?;
                choices1.replace(Rc::new(Process::WaitEntity {
                    repeat: (*repeat - 1),
                    repeated_process: Rc::clone(repeated_process),
                    next_process: Rc::clone(next_process),
                }));
                Ok(choices1)
            },
            | Process::Summation { children } => {
                // short-circuits with try_fold.
                children.iter().try_fold(Choices::default(), |mut acc, x| {
                    match self.unfold(x, current_entities) {
                        | Ok(mut choices) => {
                            acc.append(&mut choices);
                            Ok(acc)
                        },
                        | Err(e) => Err(e),
                    }
                })
            },
            | Process::NondeterministicChoice { children } => {
                // short-circuits with try_fold.
                if children.is_empty() {
                    Ok(Choices::from(vec![(
                        Rc::new(Set::default()),
                        Rc::new(Process::Nill),
                    )]))
                } else {
                    children.iter().try_fold(
                        Choices::default(),
                        |mut acc, x| {
                            acc.shuffle(self.unfold(x, current_entities)?);
                            Ok(acc)
                        },
                    )
                }
            },
        }
    }
}

impl PrintableWithTranslator for Environment {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &Translator,
    ) -> std::fmt::Result {
        write!(f, "{{env:")?;
        let mut it = self.iter().peekable();
        while let Some(el) = it.next() {
            if it.peek().is_none() {
                write!(
                    f,
                    "({} -> {})",
                    Formatter::from(translator, el.0),
                    Formatter::from(translator, el.1)
                )?;
            } else {
                write!(
                    f,
                    "({} -> {}), ",
                    Formatter::from(translator, el.0),
                    Formatter::from(translator, el.1)
                )?;
            }
        }
        write!(f, "}}")
    }
}

impl IntoIterator for Environment {
    type Item = (IdType, Process);
    type IntoIter = std::collections::btree_map::IntoIter<IdType, Process>;

    fn into_iter(self) -> Self::IntoIter {
        self.definitions.into_iter()
    }
}

impl<'a> IntoIterator for &'a Environment {
    type Item = (&'a IdType, &'a Process);
    type IntoIter = std::collections::btree_map::Iter<'a, IdType, Process>;

    fn into_iter(self) -> Self::IntoIter {
        self.definitions.iter()
    }
}

impl<const N: usize> From<[(IdType, Process); N]> for Environment {
    fn from(arr: [(IdType, Process); N]) -> Self {
        Environment {
            definitions: BTreeMap::from(arr),
        }
    }
}

impl From<&[(IdType, Process)]> for Environment {
    fn from(arr: &[(IdType, Process)]) -> Self {
        Environment {
            definitions: BTreeMap::from_iter(arr.to_vec()),
        }
    }
}

impl From<Vec<(IdType, Process)>> for Environment {
    fn from(arr: Vec<(IdType, Process)>) -> Self {
        Environment {
            definitions: BTreeMap::from_iter(arr),
        }
    }
}

// -----------------------------------------------------------------------------
//                                 Confluence
// -----------------------------------------------------------------------------

impl Environment {
    /// Two set of entities E1 and E2 are confluent w.r.t. the perpetual context
    /// delta iff they reach the same loop.
    /// confluent checks if all the sets of entities in ```entities``` are
    /// confluent and if so returns the maximal length of prefixes traversed
    /// to reached the loop, its dimension (length) and the loop.
    /// see confluent, confluents
    pub fn confluent(
        &self,
        reaction_rules: &[Reaction],
        entities: &[Set],
    ) -> Option<(usize, usize, Vec<Set>)> {
        let all_loops = self.lollipops_prefix_len_loop_decomposed(
            reaction_rules,
            entities.first()?,
        );
        let (prefix_len, hoop) = all_loops.first()?.clone();
        let dimension = hoop.len();
        let mut max_distance = prefix_len;

        for available_entities in entities.iter().skip(1) {
            let all_loops = self.lollipops_prefix_len_loop_decomposed(
                reaction_rules,
                available_entities,
            );
            let (prefix_len, new_hoop) = all_loops.first()?;

            if new_hoop.len() != dimension || !hoop.contains(new_hoop.first()?)
            {
                return None;
            }
            max_distance = cmp::max(max_distance, *prefix_len);
        }
        Some((max_distance, dimension, hoop))
    }

    /// Two set of entities E1 and E2 are confluent w.r.t. the perpetual context
    /// Q iff they reach the same loop.
    /// The predicate confluent(Rs,Q,Es,Loop,Distance,Dimension) checks if all
    /// the sets of entities in Es are confluent and if so returns the Loop,
    /// the maximal length of prefixes traversed to reached the loop and its
    /// dimension (length). see confluent, confluents
    pub fn confluent_named(
        &self,
        reaction_rules: &[Reaction],
        entities: &[Set],
        symb: IdType,
    ) -> Option<(usize, usize, Vec<Set>)> {
        let (prefix_len, first_hoop) = self
            .lollipops_prefix_len_loop_decomposed_named(
                reaction_rules,
                entities.first()?,
                symb,
            )?;
        let dimension = first_hoop.len();
        let mut max_distance = prefix_len;
        let hoop = first_hoop;

        for available_entities in entities.iter().skip(1) {
            let (prefix_len, new_hoop) = self
                .lollipops_prefix_len_loop_decomposed_named(
                    reaction_rules,
                    available_entities,
                    symb,
                )?;

            if new_hoop.len() != dimension || !hoop.contains(new_hoop.first()?)
            {
                return None;
            }
            max_distance = cmp::max(max_distance, prefix_len);
        }
        Some((max_distance, dimension, hoop))
    }

    /// invariant_named checks if all the sets of entities in ```entities``` are
    /// confluent and if so returns the set of all traversed states, together
    /// with the loop.
    /// see invariant
    pub fn invariant_named(
        &self,
        reaction_rules: &[Reaction],
        entities: &[Set],
        symb: IdType,
    ) -> Option<(Vec<Set>, Vec<Set>)> {
        let (prefix, hoop) = self.lollipops_decomposed_named(
            reaction_rules,
            entities.first()?,
            symb,
        )?;
        let mut invariant = vec![];
        invariant.append(&mut prefix.clone());
        invariant.append(&mut hoop.clone());
        let dimension = hoop.len();

        for available_entities in entities {
            let (new_prefix, new_hoop) = self.lollipops_decomposed_named(
                reaction_rules,
                available_entities,
                symb,
            )?;
            if new_hoop.len() != dimension || !hoop.contains(new_hoop.first()?)
            {
                return None;
            }
            invariant.append(&mut new_prefix.clone());
        }
        // remove duplicates, maybe better with sorting?
        invariant = invariant
            .iter()
            .cloned()
            .collect::<HashSet<_>>()
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        Some((invariant, hoop))
    }

    /// Suppose the context has the form
    /// Q1. ... Q1.Q2. ... Q2. ... Qn. ... Qn. ...
    /// and that each context Q1, Q2, ... , Q(n-1) is provided for a large
    /// number of times, enough to stabilize the system in a loop (while Qn
    /// is provided infinitely many times).  Then it can be the case that
    /// when the context switches from Qi to Q(i+1), no matter what is the
    /// current state of the loop for Qi at the moment of the switching, the
    /// system will stabilize in the same loop for Q(i+1): if this is the
    /// case the system is called "loop confluent". loop_confluent_named
    /// checks this property over the list of contexts [Q1,Q2,...,Qn] and
    /// returns the lists of Loops, Distances and Dimensions for all Qi's.
    /// see loop_confluent
    pub fn loop_confluent_named(
        deltas: &[Self],
        reaction_rules: &[Reaction],
        entities: &[Set],
        symb: IdType,
    ) -> Option<Vec<(usize, usize, Vec<Set>)>> {
        deltas
            .iter()
            .map(|q| q.confluent_named(reaction_rules, entities, symb))
            .collect::<Option<Vec<_>>>()
    }

    /// "strong confluence" requires loop confluence and additionally check
    /// that even if the context is switched BEFORE REACHING THE LOOP for Qi
    /// the traversed states are still confluent for Q(i+1)
    /// IMPORTANT: this notion of confluence assumes each context can be
    /// executed 0 or more times
    /// see strong_confluent
    #[allow(clippy::type_complexity)]
    pub fn strong_confluent_named(
        deltas: &[Self],
        reaction_rules: &[Reaction],
        entities: &[Set],
        symb: IdType,
    ) -> Option<Vec<(Vec<Set>, usize, Vec<Set>)>> {
        deltas
            .iter()
            .map(|q| {
                let (invariant, hoop) =
                    q.invariant_named(reaction_rules, entities, symb)?;
                let length = invariant.len();
                Some((invariant, length, hoop))
            })
            .collect::<Option<Vec<_>>>()
    }

    // TODO: weak confluence
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct PositiveEnvironment {
    definitions: HashMap<IdType, PositiveProcess>,
}

impl BasicEnvironment for PositiveEnvironment {
    type Id = IdType;
    type Set = PositiveSet;
    type Choices = PositiveChoices;
    type Process = PositiveProcess;
    type Reaction = PositiveReaction;

    fn get(&self, k: Self::Id) -> Option<&Self::Process> {
        self.definitions.get(&k)
    }

    fn all_elements(&self) -> Self::Set {
        let mut acc = Self::Set::default();
        for (_, process) in self.definitions.iter() {
            acc.push(&process.all_elements());
        }
        acc
    }

    fn unfold(
        &self,
        context: &Self::Process,
        entities: &Self::Set,
    ) -> Result<Self::Choices, String> {
        match context {
            | PositiveProcess::Nill => Ok(Self::Choices::default()),
            | PositiveProcess::RecursiveIdentifier { identifier } => {
                let newprocess = self.get(*identifier);
                if let Some(newprocess) = newprocess {
                    self.unfold(newprocess, entities)
                } else {
                    Err(format!("Missing symbol in context: {identifier}"))
                }
            },
            | PositiveProcess::EntitySet {
                entities,
                next_process,
            } => Ok(Self::Choices::from([(
                Rc::new(entities.clone()),
                Rc::clone(next_process),
            )])),
            | PositiveProcess::Guarded {
                reaction,
                next_process,
            } =>
                if reaction.enabled(entities) {
                    Ok(Self::Choices::from([(
                        Rc::new(reaction.products.clone()),
                        Rc::clone(next_process),
                    )]))
                } else {
                    Ok(Self::Choices::default())
                },
            | PositiveProcess::WaitEntity {
                repeat,
                repeated_process: _,
                next_process,
            } if *repeat <= 0 => self.unfold(next_process, entities),
            | PositiveProcess::WaitEntity {
                repeat,
                repeated_process,
                next_process,
            } if *repeat == 1 => {
                let mut choices1 = self.unfold(repeated_process, entities)?;
                choices1.replace(Rc::clone(next_process));
                Ok(choices1)
            },
            | PositiveProcess::WaitEntity {
                repeat,
                repeated_process,
                next_process,
            } => {
                let mut choices1 = self.unfold(repeated_process, entities)?;
                choices1.replace(Rc::new(PositiveProcess::WaitEntity {
                    repeat: (*repeat - 1),
                    repeated_process: Rc::clone(repeated_process),
                    next_process: Rc::clone(next_process),
                }));
                Ok(choices1)
            },
            | PositiveProcess::Summation { children } => {
                // short-circuits with try_fold.
                children.iter().try_fold(
                    Self::Choices::default(),
                    |mut acc, x| match self.unfold(x, entities) {
                        | Ok(mut choices) => {
                            acc.append(&mut choices);
                            Ok(acc)
                        },
                        | Err(e) => Err(e),
                    },
                )
            },
            | PositiveProcess::NondeterministicChoice { children } => {
                // short-circuits with try_fold.
                if children.is_empty() {
                    Ok(Self::Choices::from(vec![(
                        Rc::new(Self::Set::default()),
                        Rc::new(Self::Process::default()),
                    )]))
                } else {
                    children.iter().try_fold(
                        Self::Choices::default(),
                        |mut acc, x| {
                            acc.shuffle(self.unfold(x, entities)?);
                            Ok(acc)
                        },
                    )
                }
            },
        }
    }
}

impl PrintableWithTranslator for PositiveEnvironment {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &Translator,
    ) -> std::fmt::Result {
        write!(f, "{{env:")?;
        let mut it = self.iter().peekable();
        while let Some(el) = it.next() {
            if it.peek().is_none() {
                write!(
                    f,
                    "({} -> {})",
                    Formatter::from(translator, el.0),
                    Formatter::from(translator, el.1)
                )?;
            } else {
                write!(
                    f,
                    "({} -> {}), ",
                    Formatter::from(translator, el.0),
                    Formatter::from(translator, el.1)
                )?;
            }
        }
        write!(f, "}}")
    }
}

impl IntoIterator for PositiveEnvironment {
    type Item = (IdType, PositiveProcess);
    type IntoIter =
        std::collections::hash_map::IntoIter<IdType, PositiveProcess>;

    fn into_iter(self) -> Self::IntoIter {
        self.definitions.into_iter()
    }
}

impl<'a> IntoIterator for &'a PositiveEnvironment {
    type Item = (&'a IdType, &'a PositiveProcess);
    type IntoIter =
        std::collections::hash_map::Iter<'a, IdType, PositiveProcess>;

    fn into_iter(self) -> Self::IntoIter {
        self.definitions.iter()
    }
}

impl From<&Environment> for PositiveEnvironment {
    fn from(value: &Environment) -> Self {
        PositiveEnvironment {
            definitions: value
                .definitions
                .iter()
                .map(|(id, proc)| (*id, proc.into()))
                .collect::<HashMap<_, _>>(),
        }
    }
}

impl From<Environment> for PositiveEnvironment {
    fn from(value: Environment) -> Self {
        (&value).into()
    }
}

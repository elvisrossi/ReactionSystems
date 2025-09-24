use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use petgraph::graph::DiGraph;
use serde::{Deserialize, Serialize};

use super::choices::{BasicChoices, Choices, PositiveChoices};
use super::element::IdState;
use super::environment::{
    BasicEnvironment, Environment, ExtensionsEnvironment, LoopEnvironment,
    PositiveEnvironment,
};
use super::label::{BasicLabel, Label, PositiveLabel};
use super::process::{BasicProcess, PositiveProcess, Process};
use super::reaction::{
    BasicReaction, ExtensionReaction, PositiveReaction, Reaction,
};
use super::set::{BasicSet, PositiveSet, Set};
use super::trace::Trace;
use super::transitions::TransitionsIterator;
use super::translator::{Formatter, PrintableWithTranslator, Translator};

pub trait BasicSystem
where
    Self: Clone
        + Debug
        + Serialize
        + Default
        + Eq
        + Hash
        + PrintableWithTranslator,
    for<'de> Self: Deserialize<'de>,
{
    type Set: BasicSet;
    type Reaction: BasicReaction<Set = Self::Set>;
    type Label: BasicLabel<Set = Self::Set>;
    type Process: BasicProcess<Set = Self::Set>;
    type Environment: BasicEnvironment<
            Set = Self::Set,
            Process = Self::Process,
            Choices = Self::Choices,
        >;
    type Choices: BasicChoices;

    fn to_transitions_iterator(
        &self,
    ) -> Result<impl Iterator<Item = (Self::Label, Self)>, String>;

    fn environment(&self) -> &Self::Environment;
    fn available_entities(&self) -> &Self::Set;
    fn context(&self) -> &Self::Process;
    fn reactions(&self) -> &Vec<Self::Reaction>;

    fn context_elements(&self) -> &Self::Set;
    fn products_elements(&self) -> &Self::Set;
}

pub trait ExtensionsSystem: BasicSystem {
    fn unfold(&self) -> Result<Self::Choices, String>;

    fn one_transition(&self) -> Result<Option<(Self::Label, Self)>, String>;

    fn nth_transition(
        &self,
        n: usize,
    ) -> Result<Option<(Self::Label, Self)>, String>;

    fn all_transitions(&self) -> Result<Vec<(Self::Label, Self)>, String>;

    fn run(&self) -> Result<Vec<Rc<Self>>, String>;

    fn digraph(&self) -> Result<DiGraph<Self, Self::Label>, String>;

    fn target(&self) -> Result<(i64, Self::Set), String>;

    fn target_limit(&self, limit: usize) -> Result<(i64, Self::Set), String>;

    #[allow(clippy::type_complexity)]
    fn run_separated(
        &self,
    ) -> Result<Vec<(Self::Set, Self::Set, Self::Set)>, String>;

    #[allow(clippy::type_complexity)]
    fn run_separated_limit(
        &self,
        limit: usize,
    ) -> Result<Vec<(Self::Set, Self::Set, Self::Set)>, String>;

    fn traces(self, n: usize) -> Result<Vec<Trace<Self::Label, Self>>, String>;
}

impl<T: BasicSystem> ExtensionsSystem for T {
    fn unfold(&self) -> Result<Self::Choices, String> {
        self.environment()
            .unfold(self.context(), self.available_entities())
    }

    /// see oneTransition, transition, smartTransition, smartOneTransition
    fn one_transition(&self) -> Result<Option<(Self::Label, Self)>, String> {
        let mut tr = self.to_transitions_iterator()?;
        Ok(tr.next())
    }

    fn nth_transition(
        &self,
        n: usize,
    ) -> Result<Option<(Self::Label, Self)>, String> {
        let mut tr = self.to_transitions_iterator()?;
        Ok(tr.nth(n))
    }

    /// see allTransitions, smartAllTransitions
    fn all_transitions(&self) -> Result<Vec<(Self::Label, Self)>, String> {
        let tr = self.to_transitions_iterator()?;
        Ok(tr.collect::<Vec<_>>())
    }

    /// see oneRun, run, smartOneRunEK, smartRunEK
    fn run(&self) -> Result<Vec<Rc<Self>>, String> {
        let mut res = vec![Rc::new(self.clone())];
        while let Some((_, next_sys)) = res.last().unwrap().one_transition()? {
            res.push(Rc::new(next_sys));
        }
        Ok(res)
    }

    /// Creates a graph starting from a system as root node.
    fn digraph(&self) -> Result<DiGraph<Self, Self::Label>, String> {
        use petgraph::Graph;

        let mut graph: DiGraph<Self, Self::Label> = Graph::default();
        let node = graph.add_node(self.clone());

        let mut association = HashMap::new();
        association.insert(self.clone(), node);

        let mut stack = VecDeque::new();
        stack.push_back(self.clone());

        while let Some(current) = stack.pop_front() {
            // depth first
            let current_node = *association.get(&current).unwrap();

            for (label, next) in current.to_transitions_iterator()? {
                // if not already visited
                let next_node =
                    association.entry(next.clone()).or_insert_with(|| {
                        stack.push_back(next.clone());
                        graph.add_node(next)
                    });
                graph.add_edge(current_node, *next_node, label);
            }
        }
        Ok(graph)
    }

    /// Returns the state in one of the terminal states and the number of steps
    /// to arrive at the last state.
    /// see oneTarget, smartOneTarget, target, smartTarget
    fn target(&self) -> Result<(i64, Self::Set), String> {
        let current = self.one_transition()?;
        if current.is_none() {
            return Ok((0, self.available_entities().clone()));
        }
        let mut n = 1;
        let mut current = current.unwrap().1;
        while let Some((_, next)) = current.one_transition()? {
            current = next;
            n += 1;
        }
        Ok((n, current.available_entities().clone()))
    }

    fn target_limit(&self, limit: usize) -> Result<(i64, Self::Set), String> {
        let current = self.one_transition()?;
        if current.is_none() {
            return Ok((0, self.available_entities().clone()));
        }
        let mut n = 1;
        let mut current = current.unwrap().1;
        while let Some((_, next)) = current.one_transition()? {
            if n >= limit {
                break;
            }
            current = next;
            n += 1;
        }
        Ok((n as i64, current.available_entities().clone()))
    }

    /// see smartOneRunECT, smartRunECT
    fn run_separated(
        &self,
    ) -> Result<Vec<(Self::Set, Self::Set, Self::Set)>, String> {
        let mut res = vec![];
        let current = self.one_transition()?;
        if current.is_none() {
            return Ok(res);
        }
        let current = current.unwrap();
        let (available_entities, context, t) = current.0.get_context();
        res.push((available_entities.clone(), context.clone(), t.clone()));
        let mut current = current.1;
        while let Some((label, next)) = current.one_transition()? {
            current = next;
            let (available_entities, context, t) = label.get_context();
            res.push((available_entities.clone(), context.clone(), t.clone()));
        }
        Ok(res)
    }

    /// see smartOneRunECT, smartRunECT
    fn run_separated_limit(
        &self,
        limit: usize,
    ) -> Result<Vec<(Self::Set, Self::Set, Self::Set)>, String> {
        let mut limit = limit;
        let mut res = vec![];
        let current = self.one_transition()?;
        if current.is_none() {
            return Ok(res);
        }
        let current = current.unwrap();
        let (available_entities, context, t) = current.0.get_context();
        res.push((available_entities.clone(), context.clone(), t.clone()));
        let mut current = current.1;
        while let Some((label, next)) = current.one_transition()? {
            if limit <= 1 {
                break;
            }
            limit -= 1;
            current = next;
            let (available_entities, context, t) = label.get_context();
            res.push((available_entities.clone(), context.clone(), t.clone()));
        }
        Ok(res)
    }

    /// Return the first n traces. Equivalent to visiting the execution tree
    /// depth first and returning the first n leaf nodes and their path to the
    /// root.
    fn traces(self, n: usize) -> Result<Vec<Trace<Self::Label, Self>>, String> {
        if n == 0 {
            return Ok(vec![]);
        }
        let mut n = n;
        let mut res: Vec<Trace<Self::Label, Self>> = vec![];
        let mut current_trace: Trace<Self::Label, Self> = Trace::default();
        current_trace.push((None, Rc::new(self)));
        let mut branch = vec![0];
        let mut depth = 0;
        let mut new_branch = true;

        loop {
            let next_sys =
                current_trace[depth].1.nth_transition(branch[depth])?;

            if let Some((current_label, next_sys)) = next_sys {
                depth += 1;
                if depth >= branch.len() {
                    branch.push(0);
                    current_trace.push((
                        Some(Rc::new(current_label)),
                        Rc::new(next_sys),
                    ));
                } else {
                    branch[depth] = 0;
                    current_trace[depth] =
                        (Some(Rc::new(current_label)), Rc::new(next_sys));
                }
                new_branch = true;
            } else {
                // at the bottom of a trace, we save to res, then backtrack
                // until we find another possible path.
                if new_branch {
                    res.push(Trace::from(current_trace[0..depth].to_vec()));
                    new_branch = false;
                    n -= 1;
                }
                if n == 0 {
                    break;
                }
                if depth == 0 {
                    break;
                }
                depth -= 1;
                branch[depth] += 1;
            }
        }

        Ok(res)
    }
}

// -----------------------------------------------------------------------------
//                                    Loop
// -----------------------------------------------------------------------------

pub trait LoopSystem: BasicSystem {
    type Env: BasicEnvironment;

    #[allow(clippy::type_complexity)]
    fn lollipops(&self) -> Vec<(Vec<Self::Set>, Vec<Self::Set>)>;

    fn lollipops_only_loop(self) -> Vec<Vec<Self::Set>>;

    #[allow(clippy::type_complexity)]
    fn lollipops_named(
        &self,
        symb: <Self::Env as BasicEnvironment>::Id,
    ) -> Option<(Vec<Self::Set>, Vec<Self::Set>)>;

    fn lollipops_only_loop_named(
        &self,
        symb: <Self::Env as BasicEnvironment>::Id,
    ) -> Option<Vec<Self::Set>>;
}

impl<
    S,
    E,
    R: ExtensionReaction<Set = S>,
    T: BasicSystem<Reaction = R, Environment = E, Set = S>,
> LoopSystem for T
where
    E: BasicEnvironment<Reaction = R, Set = S> + ExtensionsEnvironment,
    for<'a> &'a E: IntoIterator<Item = (&'a E::Id, &'a E::Process)>,
    E::Id: Eq,
{
    type Env = E;

    /// A special case of systems is when the context recursively provides
    /// always the same set of entities.  The corresponding computation is
    /// infinite.  It consists of a finite sequence of states followed by a
    /// looping sequence.  IMPORTANT: We return all loops for all X = Q.X, by
    /// varing X.  The set of reactions Rs and the context x are constant.  Each
    /// state of the computation is distinguished by the current entities E.
    /// Under these assumptions, the predicate lollipop finds the Prefixes and
    /// the Loops sequences of entities.
    /// see lollipop
    fn lollipops(&self) -> Vec<(Vec<Self::Set>, Vec<Self::Set>)> {
        self.environment()
            .lollipops_decomposed(self.reactions(), self.available_entities())
    }

    /// Only returns the loop part of the lollipop, returns for all X, where
    /// X = Q.X
    /// see loop
    fn lollipops_only_loop(self) -> Vec<Vec<Self::Set>> {
        let filtered = self
            .environment()
            .iter()
            .filter_map(|l| l.1.filter_delta(l.0));

        let find_loop_fn = |q| {
            R::find_only_loop(self.reactions(), self.available_entities(), q)
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
    fn lollipops_named(
        &self,
        symb: E::Id,
    ) -> Option<(Vec<Self::Set>, Vec<Self::Set>)> {
        self.environment().lollipops_decomposed_named(
            self.reactions(),
            self.available_entities(),
            symb,
        )
    }

    /// Only returns the loop part of the lollipop, returns for all X, where
    /// X = Q.X
    /// see loop
    fn lollipops_only_loop_named(&self, symb: E::Id) -> Option<Vec<Self::Set>> {
        let filtered = self
            .environment()
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
            R::find_only_loop(self.reactions(), self.available_entities(), q)
        };

        filtered.map(find_loop_fn)
    }
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct System {
    pub delta: Rc<Environment>,
    pub available_entities: Set,
    pub context_process: Process,
    pub reaction_rules: Rc<Vec<Reaction>>,

    context_elements:  Rc<Set>,
    products_elements: Rc<Set>,
}

impl BasicSystem for System {
    type Set = Set;
    type Reaction = Reaction;
    type Label = Label;
    type Process = Process;
    type Environment = Environment;
    type Choices = Choices;

    fn to_transitions_iterator(
        &self,
    ) -> Result<impl Iterator<Item = (Self::Label, Self)>, String> {
        TransitionsIterator::<Self::Set, Self, Self::Process>::try_from(self)
    }

    fn environment(&self) -> &Self::Environment {
        &self.delta
    }

    fn available_entities(&self) -> &Self::Set {
        &self.available_entities
    }

    fn context(&self) -> &Self::Process {
        &self.context_process
    }

    fn reactions(&self) -> &Vec<Self::Reaction> {
        &self.reaction_rules
    }

    fn context_elements(&self) -> &Self::Set {
        &self.context_elements
    }

    fn products_elements(&self) -> &Self::Set {
        &self.products_elements
    }
}

/// Equality does not care about delta or reaction rules. Only entities and
/// context is compared
impl PartialEq for System {
    // we ignore delta and reaction rules
    fn eq(&self, other: &System) -> bool {
        self.available_entities == other.available_entities
            && self.context_process == other.context_process
    }
}

/// Equality does not care about delta or reaction rules. Only entities and
/// context is compared
impl Eq for System {}

/// Hash does not care about delta or reaction rules. Only entities and
/// context is hashed
impl Hash for System {
    // ignores delta and reaction rules
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.available_entities.hash(state);
        self.context_process.hash(state);
    }
}

impl Default for System {
    fn default() -> Self {
        Self {
            delta: Rc::new(Environment::default()),
            available_entities: Set::default(),
            context_process: Process::Nill,
            reaction_rules: Rc::new(Vec::default()),

            context_elements:  Rc::new(Set::default()),
            products_elements: Rc::new(Set::default()),
        }
    }
}

impl PrintableWithTranslator for System {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &Translator,
    ) -> std::fmt::Result {
        write!(
            f,
            "[delta: {}, available_entities: {}, context_process: {}, \
             reaction_rules: [",
            Formatter::from(translator, &*self.delta),
            Formatter::from(translator, &self.available_entities),
            Formatter::from(translator, &self.context_process)
        )?;
        let mut it = self.reaction_rules.iter().peekable();
        while let Some(el) = it.next() {
            if it.peek().is_none() {
                write!(f, "{}", Formatter::from(translator, el))?;
            } else {
                write!(f, "{}, ", Formatter::from(translator, el))?;
            }
        }
        write!(f, "] ]")
    }
}

impl System {
    pub fn from(
        delta: Rc<Environment>,
        available_entities: Set,
        context_process: Process,
        reaction_rules: Rc<Vec<Reaction>>,
    ) -> System {
        let products_elements = reaction_rules
            .iter()
            .fold(Set::default(), |acc: Set, r| acc.union(&r.products));
        let all_elements_context = delta
            .all_elements()
            .union(&context_process.all_elements())
            .subtraction(&products_elements);

        System {
            delta: Rc::clone(&delta),
            available_entities,
            context_process,
            reaction_rules: Rc::clone(&reaction_rules),

            context_elements: Rc::new(all_elements_context),
            products_elements: Rc::new(products_elements),
        }
    }
}

// -----------------------------------------------------------------------------
//                                 Statistics
// -----------------------------------------------------------------------------

impl System {
    /// Non simulated statistics of a system.
    /// Returns statistics about the system as a string.
    /// see main_do(stat,MissingE)
    pub fn statistics(&self, translator: &Translator) -> String {
        use super::translator::Formatter;

        let mut result: String = "Statistics:\n".into();
        result.push_str(
            "=============================================================\n",
        );
        result.push_str(&format!(
            "the initial state has {} entities:\n",
            self.available_entities.len()
        ));
        result.push_str(&format!(
            "{}\n",
            Formatter::from(translator, &self.available_entities)
        ));

        let reactants = self
            .reaction_rules
            .iter()
            .fold(Set::default(), |acc, new| acc.union(&new.reactants));
        result.push_str(&format!(
            "The reactants are {}:\n{}\n",
            reactants.len(),
            Formatter::from(translator, &reactants)
        ));

        let inhibitors = self
            .reaction_rules
            .iter()
            .fold(Set::default(), |acc, new| acc.union(&new.inhibitors));
        result.push_str(&format!(
            "The inhibitors are {}:\n{}\n",
            inhibitors.len(),
            Formatter::from(translator, &inhibitors)
        ));

        let products = self
            .reaction_rules
            .iter()
            .fold(Set::default(), |acc, new| acc.union(&new.products));
        result.push_str(&format!(
            "The products are {}:\n{}\n",
            products.len(),
            Formatter::from(translator, &products)
        ));

        let total = reactants.union(&inhibitors.union(&products));
        result.push_str(&format!(
            "The reactions involve {} entities:\n{}\n",
            total.len(),
            Formatter::from(translator, &total)
        ));

        let entities_env = self.delta.all_elements();
        result.push_str(&format!(
            "The environment involves {} entities:\n{}\n",
            entities_env.len(),
            Formatter::from(translator, &entities_env)
        ));

        let entities_context = self.context_process.all_elements();
        result.push_str(&format!(
            "The context involves {} entities:\n{}\n",
            entities_context.len(),
            Formatter::from(translator, &entities_context)
        ));

        let entities_all = total
            .union(&entities_env)
            .union(&entities_context)
            .union(&self.available_entities);

        result.push_str(&format!(
            "The whole RS involves {} entities:\n{}\n",
            entities_all.len(),
            Formatter::from(translator, &entities_all)
        ));

        let possible_e = products
            .union(&self.available_entities)
            .union(&entities_context);
        let missing_e = reactants.subtraction(&possible_e);
        result.push_str(&format!(
            "There are {} reactants that will never be available:\n{}\n",
            missing_e.len(),
            Formatter::from(translator, &missing_e)
        ));

        let entities_not_needed = entities_context.subtraction(&total);
        result.push_str(&format!(
            "The context can provide {} entities that will never be used:\
             \n{}\n",
            entities_not_needed.len(),
            Formatter::from(translator, &entities_not_needed)
        ));

        result.push_str(&format!(
            "There are {} reactions in total.\n",
            self.reaction_rules.len()
        ));

        let mut admissible_reactions = vec![];
        let mut nonadmissible_reactions = vec![];

        for reaction in self.reaction_rules.iter() {
            if reaction.reactants.is_disjoint(&missing_e) {
                admissible_reactions.push(reaction);
            } else {
                nonadmissible_reactions.push(reaction);
            }
        }

        result.push_str(&format!(
            "- the applicable reactions are {}.\n",
            admissible_reactions.len()
        ));

        result.push_str(&format!(
            "- there are {} reactions that will never be enabled.\n",
            nonadmissible_reactions.len()
        ));
        result.push_str(
            "=============================================================",
        );

        result
    }
}

// -----------------------------------------------------------------------------
//                              Positive System
// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PositiveSystem {
    pub delta: Rc<PositiveEnvironment>,
    pub available_entities: PositiveSet,
    pub context_process: PositiveProcess,
    pub reaction_rules: Rc<Vec<PositiveReaction>>,

    context_elements:  Rc<PositiveSet>,
    products_elements: Rc<PositiveSet>,
}

impl BasicSystem for PositiveSystem {
    type Set = PositiveSet;
    type Reaction = PositiveReaction;
    type Label = PositiveLabel;
    type Process = PositiveProcess;
    type Environment = PositiveEnvironment;
    type Choices = PositiveChoices;

    fn to_transitions_iterator(
        &self,
    ) -> Result<impl Iterator<Item = (Self::Label, Self)>, String> {
        TransitionsIterator::<Self::Set, Self, Self::Process>::try_from(self)
    }

    fn environment(&self) -> &Self::Environment {
        &self.delta
    }

    fn available_entities(&self) -> &Self::Set {
        &self.available_entities
    }

    fn context(&self) -> &Self::Process {
        &self.context_process
    }

    fn reactions(&self) -> &Vec<Self::Reaction> {
        &self.reaction_rules
    }

    fn context_elements(&self) -> &Self::Set {
        &self.context_elements
    }

    fn products_elements(&self) -> &Self::Set {
        &self.products_elements
    }
}

/// Equality does not care about delta or reaction rules. Only entities and
/// context is compared
impl PartialEq for PositiveSystem {
    // we ignore delta and reaction rules
    fn eq(&self, other: &PositiveSystem) -> bool {
        self.available_entities == other.available_entities
            && self.context_process == other.context_process
    }
}

/// Equality does not care about delta or reaction rules. Only entities and
/// context is compared
impl Eq for PositiveSystem {}

/// Hash does not care about delta or reaction rules. Only entities and
/// context is hashed
impl Hash for PositiveSystem {
    // ignores delta and reaction rules
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.available_entities.hash(state);
        self.context_process.hash(state);
    }
}

impl Default for PositiveSystem {
    fn default() -> Self {
        Self {
            delta: Rc::new(PositiveEnvironment::default()),
            available_entities: PositiveSet::default(),
            context_process: PositiveProcess::default(),
            reaction_rules: Rc::new(Vec::default()),

            context_elements:  Rc::new(PositiveSet::default()),
            products_elements: Rc::new(PositiveSet::default()),
        }
    }
}

impl PrintableWithTranslator for PositiveSystem {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &Translator,
    ) -> std::fmt::Result {
        write!(
            f,
            "[delta: {}, available_entities: {}, context_process: {}, \
             reaction_rules: [",
            Formatter::from(translator, &*self.delta),
            Formatter::from(translator, &self.available_entities),
            Formatter::from(translator, &self.context_process)
        )?;
        let mut it = self.reaction_rules.iter().peekable();
        while let Some(el) = it.next() {
            if it.peek().is_none() {
                write!(f, "{}", Formatter::from(translator, el))?;
            } else {
                write!(f, "{}, ", Formatter::from(translator, el))?;
            }
        }
        write!(f, "] ]")
    }
}

impl From<System> for PositiveSystem {
    /// Converts from a normal system to a positive one, replaces every reaction
    /// {r, i, p} with a positive reaction {r ∪ ¬i, p} and with {t, el} for each
    /// el ∈ p for p in some reaction and t ∈ prohibiting set of a with respect
    /// all reactions that contains el in the products.
    /// Should never fail.
    fn from(value: System) -> Self {
        let new_env = Rc::new((&*value.delta).into());
        let positive_entities =
            value.available_entities.to_positive_set(IdState::Positive);

        let negative_entities = value
            .context_process
            .all_elements()
            .union(&value.environment().all_elements())
            .union(&value.reactions().iter().fold(
                Set::default(),
                |acc: Set, el| {
                    acc.union(&el.inhibitors)
                        .union(&el.products)
                        .union(&el.reactants)
                },
            ))
            .subtraction(&value.available_entities)
            .to_positive_set(IdState::Negative);
        let new_available_entities =
            positive_entities.union(&negative_entities);

        let new_context = value.context_process.into();
        let new_reactions = {
            let mut res = vec![];
            let old_reactions = &value.reaction_rules;

            let all_products = Reaction::all_products(old_reactions);
            for el in all_products {
                let p =
                    Reaction::all_reactions_with_product(old_reactions, &el);
                let mut tmp = vec![];
                for r in p.iter() {
                    tmp.push(PositiveReaction::create(
                        r.reactants.clone(),
                        r.inhibitors.clone(),
                        Set::from([el]),
                    ))
                }
                tmp.sort_by(|r1, r2| r1.reactants.cmp(&r2.reactants));

                // remove reactions with only one element of opposite state
                // as intersection (same product ```el```)
                let mut pos = tmp.len() - 1;
                while pos > 0 {
                    if let Some(intersection) =
                        tmp[pos].differ_only_one_element(&tmp[pos - 1])
                    {
                        tmp[pos - 1].reactants = intersection;
                        tmp.remove(pos);
                    }
                    pos -= 1;
                }

                res.extend(tmp);
                let prohib_set = Set::prohibiting_set(
                    &p.iter().map(|p| p.reactants.clone()).collect::<Vec<_>>(),
                    &p.iter().map(|p| p.inhibitors.clone()).collect::<Vec<_>>(),
                )
                .unwrap(); // since we have in input a valid system
                for s in prohib_set {
                    res.push(PositiveReaction {
                        reactants: s,
                        products:  PositiveSet::from([(el, IdState::Negative)]),
                    })
                }
            }

            Rc::new(res)
        };

        Self::from(new_env, new_available_entities, new_context, new_reactions)
    }
}

impl PositiveSystem {
    pub fn from(
        delta: Rc<PositiveEnvironment>,
        available_entities: PositiveSet,
        context_process: PositiveProcess,
        reaction_rules: Rc<Vec<PositiveReaction>>,
    ) -> Self {
        let products_elements = reaction_rules
            .iter()
            .fold(PositiveSet::default(), |acc: PositiveSet, r| {
                acc.union(&r.products)
            });
        let all_elements_context = delta
            .all_elements()
            .union(&context_process.all_elements())
            .subtraction(&products_elements);

        Self {
            delta: Rc::clone(&delta),
            available_entities,
            context_process,
            reaction_rules: Rc::clone(&reaction_rules),

            context_elements: Rc::new(all_elements_context),
            products_elements: Rc::new(products_elements),
        }
    }
}

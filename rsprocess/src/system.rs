use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::{Arc, Mutex};

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
use super::set::{BasicSet, ExtensionsSet, PositiveSet, Set};
use super::trace::Trace;
use super::transitions::TransitionsIterator;
use super::translator::{Formatter, PrintableWithTranslator, Translator};
use crate::trace::{EnabledReactions, SlicingElement, SlicingTrace};
use crate::transitions::TraceIterator;

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

    #[allow(clippy::type_complexity)]
    fn to_slicing_iterator(
        &self,
    ) -> Result<
        impl Iterator<Item = (Self::Set, Self::Set, Vec<usize>, Self)>,
        String,
    >;

    fn environment(&self) -> &Self::Environment;
    fn available_entities(&self) -> &Self::Set;
    fn context(&self) -> &Self::Process;
    fn reactions(&self) -> &Vec<Self::Reaction>;

    fn context_elements(&self) -> Self::Set;
    fn products_elements(&self) -> Self::Set;
}

pub trait ExtensionsSystem: BasicSystem {
    fn unfold(&self) -> Result<Self::Choices, String>;

    fn one_transition(&self) -> Result<Option<(Self::Label, Self)>, String>;

    fn nth_transition(
        &self,
        n: usize,
    ) -> Result<Option<(Self::Label, Self)>, String>;

    fn all_transitions(&self) -> Result<Vec<(Self::Label, Self)>, String>;

    fn run(&self) -> Result<Vec<Arc<Self>>, String>;

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

    fn traces(&self, n: usize)
    -> Result<Vec<Trace<Self::Label, Self>>, String>;

    fn slice_trace(
        &self,
    ) -> Result<SlicingTrace<Self::Set, Self::Reaction, Self>, String>;

    fn slice_trace_limit(
        &self,
        limit: usize,
    ) -> Result<SlicingTrace<Self::Set, Self::Reaction, Self>, String>;
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
    fn run(&self) -> Result<Vec<Arc<Self>>, String> {
        let mut res = vec![Arc::new(self.clone())];
        while let Some((_, next_sys)) = res.last().unwrap().one_transition()? {
            res.push(Arc::new(next_sys));
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
    fn traces(
        &self,
        n: usize,
    ) -> Result<Vec<Trace<Self::Label, Self>>, String> {
        if n == 0 {
            return Ok(vec![]);
        }
        let sys = self.clone();
        let mut n = n;
        let mut res: Vec<Trace<Self::Label, Self>> = vec![];
        let mut current_trace: Trace<Self::Label, Self> = Trace::default();
        current_trace.push((None, Arc::new(sys)));
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
                        Some(Arc::new(current_label)),
                        Arc::new(next_sys),
                    ));
                } else {
                    branch[depth] = 0;
                    current_trace[depth] =
                        (Some(Arc::new(current_label)), Arc::new(next_sys));
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

    #[allow(clippy::field_reassign_with_default)]
    #[allow(clippy::type_complexity)]
    fn slice_trace(
        &self,
    ) -> Result<SlicingTrace<Self::Set, Self::Reaction, Self>, String> {
        let mut trace = SlicingTrace::default();

        trace.context_elements = Arc::new(self.context_elements());
        trace.products_elements = Arc::new(self.products_elements());
        trace.reactions = Arc::new(self.reactions().clone());
        trace.systems.push(Arc::new(self.clone()));
        trace.elements.push(SlicingElement::from((
            Self::Set::default(),
            self.available_entities().clone(),
        )));

        let current: Option<(Self::Set, Self::Set, Vec<usize>, Self)> =
            self.to_slicing_iterator()?.next();
        if current.is_none() {
            return Ok(trace);
        }
        let current = current.unwrap();

        let (context, products, enabled_reactions, mut current) = current;
        trace
            .elements
            .push(SlicingElement::from((context, products)));
        trace
            .enabled_reactions
            .push(EnabledReactions::from(enabled_reactions));
        trace.systems.push(Arc::new(current.clone()));

        loop {
            let t = current.to_slicing_iterator()?.next();
            if let Some((context, products, enabled_reactions, next_sys)) = t {
                current = next_sys;
                trace
                    .elements
                    .push(SlicingElement::from((context, products)));
                trace
                    .enabled_reactions
                    .push(EnabledReactions::from(enabled_reactions));
                trace.systems.push(Arc::new(current.clone()));
            } else {
                break;
            }
        }
        // trace.enabled_reactions.pop();
        Ok(trace)
    }

    #[allow(clippy::field_reassign_with_default)]
    #[allow(clippy::type_complexity)]
    fn slice_trace_limit(
        &self,
        limit: usize,
    ) -> Result<SlicingTrace<Self::Set, Self::Reaction, Self>, String> {
        let mut trace = SlicingTrace::default();

        trace.context_elements = Arc::new(self.context_elements());
        trace.products_elements = Arc::new(self.products_elements());
        trace.reactions = Arc::new(self.reactions().clone());
        trace.systems.push(Arc::new(self.clone()));
        trace.elements.push(SlicingElement::from((
            Self::Set::default(),
            self.available_entities().clone(),
        )));

        let current: Option<(Self::Set, Self::Set, Vec<usize>, Self)> =
            self.to_slicing_iterator()?.next();
        if current.is_none() {
            return Ok(trace);
        }
        let current = current.unwrap();

        let (context, products, enabled_reactions, mut current) = current;
        trace
            .elements
            .push(SlicingElement::from((context, products)));
        trace
            .enabled_reactions
            .push(EnabledReactions::from(enabled_reactions));
        trace.systems.push(Arc::new(current.clone()));

        let mut n = limit;
        loop {
            n -= 1;
            let t = current.to_slicing_iterator()?.next();
            if let Some((context, products, enabled_reactions, next_sys)) = t
                && n > 0
            {
                current = next_sys;
                trace
                    .elements
                    .push(SlicingElement::from((context, products)));
                trace
                    .enabled_reactions
                    .push(EnabledReactions::from(enabled_reactions));
                trace.systems.push(Arc::new(current.clone()));
            } else {
                break;
            }
        }
        // trace.enabled_reactions.pop();
        Ok(trace)
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
    pub delta: Arc<Environment>,
    pub available_entities: Set,
    pub context_process: Process,
    pub reaction_rules: Arc<Vec<Reaction>>,

    context_elements:  Arc<Mutex<Option<Set>>>,
    products_elements: Arc<Mutex<Option<Set>>>,
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

    #[allow(refining_impl_trait)] // otherwise impl cannot be resolved to
    // concrete type and the compiler complains
    fn to_slicing_iterator(
        &self,
    ) -> Result<TraceIterator<'_, Self::Set, Self, Self::Process>, String> {
        TraceIterator::<Self::Set, Self, Self::Process>::try_from(self)
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

    fn context_elements(&self) -> Self::Set {
        let mut c = self.context_elements.lock().unwrap();
        if c.is_some() {
            c.as_ref().unwrap().clone()
        } else {
            let all_elements_context = self
                .delta
                .all_elements()
                .union(&self.context_process.all_elements())
                .subtraction(&self.products_elements());

            *c = Some(all_elements_context.clone());
            all_elements_context
        }
    }

    fn products_elements(&self) -> Self::Set {
        let mut p = self.products_elements.lock().unwrap();
        if p.is_some() {
            p.as_ref().unwrap().clone()
        } else {
            let products_elements = self
                .reaction_rules
                .iter()
                .fold(Self::Set::default(), |acc: Self::Set, r| {
                    acc.union(&r.products)
                });

            *p = Some(products_elements.clone());
            products_elements
        }
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
            delta: Arc::new(Environment::default()),
            available_entities: Set::default(),
            context_process: Process::Nill,
            reaction_rules: Arc::new(Vec::default()),

            context_elements:  Arc::new(Mutex::new(None)),
            products_elements: Arc::new(Mutex::new(None)),
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
            "[\ndelta: {},\navailable_entities: {},\ncontext_process: {},\n\
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
        write!(f, "]\n]")
    }
}

impl System {
    pub fn from(
        delta: Arc<Environment>,
        available_entities: Set,
        context_process: Process,
        reaction_rules: Arc<Vec<Reaction>>,
    ) -> System {
        System {
            delta: Arc::clone(&delta),
            available_entities,
            context_process,
            reaction_rules: Arc::clone(&reaction_rules),

            context_elements: Arc::new(Mutex::new(None)),
            products_elements: Arc::new(Mutex::new(None)),
        }
    }

    pub fn overwrite_context_elements(&mut self, new_context_elements: Set) {
        self.context_elements =
            Arc::new(Mutex::new(Some(new_context_elements)));
    }

    pub fn overwrite_product_elements(&mut self, new_product_elements: Set) {
        // since context_elements depend on product elements we make sure that
        // its computed to ensure consistent behaviour
        self.context_elements();
        self.products_elements =
            Arc::new(Mutex::new(Some(new_product_elements)));
    }

    pub fn precomputed_context_elements(
        &mut self,
        new_context_elements: Option<Set>,
    ) {
        if let Some(nce) = new_context_elements {
            self.overwrite_context_elements(nce);
        }
    }

    pub fn precomputed_product_elements(
        &mut self,
        new_product_elements: Option<Set>,
    ) {
        if let Some(npe) = new_product_elements {
            self.overwrite_product_elements(npe);
        }
    }

    pub fn direct_get_product_elements(&self) -> Option<Set> {
        self.products_elements.lock().unwrap().clone()
    }

    pub fn direct_get_context_elements(&self) -> Option<Set> {
        self.context_elements.lock().unwrap().clone()
    }

    pub fn to_single_products(&self) -> Self {
        let mut new_sys = Self::default();
        new_sys
            .precomputed_context_elements(self.direct_get_context_elements());
        new_sys
            .precomputed_product_elements(self.direct_get_product_elements());

        new_sys.delta = Arc::clone(&self.delta);
        new_sys.available_entities = self.available_entities.clone();
        new_sys.context_process = self.context_process.clone();
        new_sys.reaction_rules = {
            let mut new_reactions = vec![];
            for r in self.reaction_rules.iter() {
                for el in r.products.iter() {
                    new_reactions.push(Reaction::from(
                        r.reactants.clone(),
                        r.inhibitors.clone(),
                        [*el].into(),
                    ))
                }
            }
            Arc::new(new_reactions)
        };

        new_sys
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
    pub delta: Arc<PositiveEnvironment>,
    pub available_entities: PositiveSet,
    pub context_process: PositiveProcess,
    pub reaction_rules: Arc<Vec<PositiveReaction>>,

    context_elements:  Arc<Mutex<Option<PositiveSet>>>,
    products_elements: Arc<Mutex<Option<PositiveSet>>>,
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

    fn to_slicing_iterator(
        &self,
    ) -> Result<
        impl Iterator<Item = (Self::Set, Self::Set, Vec<usize>, Self)>,
        String,
    > {
        TraceIterator::<Self::Set, Self, Self::Process>::try_from(self)
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

    fn context_elements(&self) -> Self::Set {
        let mut c = self.context_elements.lock().unwrap();
        if c.is_some() {
            c.as_ref().unwrap().clone()
        } else {
            let all_elements_context = self
                .delta
                .all_elements()
                .union(&self.context_process.all_elements())
                .subtraction(&self.products_elements())
                .iter()
                .map(|e| (*e.0, IdState::Positive))
                .collect::<Self::Set>();

            *c = Some(all_elements_context.clone());
            all_elements_context
        }
    }

    fn products_elements(&self) -> Self::Set {
        let mut p = self.products_elements.lock().unwrap();
        if p.is_some() {
            p.as_ref().unwrap().clone()
        } else {
            let products_elements = self
                .reaction_rules
                .iter()
                .fold(Self::Set::default(), |acc: Self::Set, r| {
                    acc.union(&r.products)
                })
                .iter()
                .map(|e| (*e.0, IdState::Positive))
                .collect::<Self::Set>();

            *p = Some(products_elements.clone());
            products_elements
        }
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
            delta: Arc::new(PositiveEnvironment::default()),
            available_entities: PositiveSet::default(),
            context_process: PositiveProcess::default(),
            reaction_rules: Arc::new(Vec::default()),

            context_elements:  Arc::new(Mutex::new(None)),
            products_elements: Arc::new(Mutex::new(None)),
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
            "[\ndelta: {},\navailable_entities: {},\ncontext_process: {},\n\
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
                writeln!(f, "{},", Formatter::from(translator, el))?;
            }
        }
        write!(f, "]\n]")
    }
}

impl From<System> for PositiveSystem {
    /// Converts from a normal system to a positive one, replaces every reaction
    /// {r, i, p} with a positive reaction {r ∪ ¬i, p} and with {t, el} for each
    /// el ∈ p for p in some reaction and t ∈ prohibiting set of a with respect
    /// all reactions that contains el in the products.
    /// Should never fail.
    fn from(value: System) -> Self {
        let new_env = Arc::new((&*value.delta).into());
        let positive_entities =
            value.available_entities.to_positive_set(IdState::Positive);

        // let negative_entities = value
        //     .context_process
        //     .all_elements()
        //     .union(&value.environment().all_elements())
        //     .union(&value.reactions().iter().fold(
        //         Set::default(),
        //         |acc: Set, el| {
        //             acc.union(&el.inhibitors)
        //                 .union(&el.products)
        //                 .union(&el.reactants)
        //         },
        //     ))
        //     .subtraction(&value.available_entities)
        //     .to_positive_set(IdState::Negative);
        let negative_entities =
            value.products_elements().to_positive_set(IdState::Negative);
        let new_available_entities =
            positive_entities.add_unique(&negative_entities);

        let new_reactions =
            Arc::new(PositiveReaction::from_reactions(value.reactions()));
        let new_context = value.context_process.into();

        Self::from(new_env, new_available_entities, new_context, new_reactions)
    }
}

impl PositiveSystem {
    pub fn from(
        delta: Arc<PositiveEnvironment>,
        available_entities: PositiveSet,
        context_process: PositiveProcess,
        reaction_rules: Arc<Vec<PositiveReaction>>,
    ) -> Self {
        Self {
            delta: Arc::clone(&delta),
            available_entities,
            context_process,
            reaction_rules: Arc::clone(&reaction_rules),

            context_elements: Arc::new(Mutex::new(None)),
            products_elements: Arc::new(Mutex::new(None)),
        }
    }

    pub fn negated_products_elements(&self) -> PositiveSet {
        self.products_elements()
            .iter()
            .map(|el| (*el.0, IdState::Negative))
            .collect::<PositiveSet>()
    }

    pub fn overwrite_context_elements(
        &mut self,
        new_context_elements: PositiveSet,
    ) {
        self.context_elements =
            Arc::new(Mutex::new(Some(new_context_elements)));
    }

    pub fn overwrite_product_elements(
        &mut self,
        new_product_elements: PositiveSet,
    ) {
        // since context_elements depend on product elements we make sure that
        // its computed to ensure consistent behaviour
        self.context_elements();
        self.products_elements =
            Arc::new(Mutex::new(Some(new_product_elements)));
    }

    pub fn precomputed_context_elements(
        &mut self,
        new_context_elements: Option<PositiveSet>,
    ) {
        if let Some(nce) = new_context_elements {
            self.overwrite_context_elements(nce);
        }
    }

    pub fn precomputed_product_elements(
        &mut self,
        new_product_elements: Option<PositiveSet>,
    ) {
        if let Some(npe) = new_product_elements {
            self.overwrite_product_elements(npe);
        }
    }

    pub fn direct_get_product_elements(&self) -> Option<PositiveSet> {
        self.products_elements.lock().unwrap().clone()
    }

    pub fn direct_get_context_elements(&self) -> Option<PositiveSet> {
        self.context_elements.lock().unwrap().clone()
    }
}

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use super::environment::Environment;
use super::graph::SystemGraph;
use super::label::Label;
use super::process::Process;
use super::reaction::Reaction;
use super::set::Set;
use super::transitions::TransitionsIterator;
use super::translator::{IdType, Translator, PrintableWithTranslator, Formatter};


#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct System {
    pub delta: Rc<Environment>,
    pub available_entities: Set,
    pub context_process: Process,
    pub reaction_rules: Rc<Vec<Reaction>>,
}

type Trace = Vec<(Option<Rc<Label>>, Rc<System>)>;

impl System {
    pub fn new() -> System {
	System {
	    delta: Rc::new(Environment::new()),
	    available_entities: Set::new(),
	    context_process: Process::Nill,
	    reaction_rules: Rc::new(vec![]),
	}
    }

    pub fn from(
	delta: Rc<Environment>,
	available_entities: Set,
	context_process: Process,
	reaction_rules: Rc<Vec<Reaction>>,
    ) -> System {
	System {
	    delta: Rc::clone(&delta),
	    available_entities,
	    context_process,
	    reaction_rules: Rc::clone(&reaction_rules),
	}
    }

    pub fn to_transitions_iterator<'a>(
	&'a self
    ) -> Result<TransitionsIterator<'a>, String> {
	TransitionsIterator::from(self)
    }


    /// see oneTransition, transition, smartTransition, smartOneTransition
    pub fn one_transition(
	&self
    ) -> Result<Option<(Label, System)>, String> {
	let mut tr = self.to_transitions_iterator()?;
	Ok(tr.next())
    }


    pub fn nth_transition(
	&self,
	n: usize,
    ) -> Result<Option<(Label, System)>, String> {
	let mut tr = self.to_transitions_iterator()?;
	Ok(tr.nth(n))
    }


    /// see allTransitions, smartAllTransitions
    pub fn all_transitions(
	&self
    ) -> Result<Vec<(Label, System)>, String> {
	let tr = self.to_transitions_iterator()?;
	Ok(tr.collect::<Vec<_>>())
    }


    /// see oneTarget, smartOneTarget, target, smartTarget
    pub fn target(
	&self
    ) -> Result<(i64, Set), String> {
	let current = self.one_transition()?;
	if current.is_none() {
	    return Ok((0, self.available_entities.clone()));
	}
	let mut n = 1;
	let mut current = current.unwrap().1;
	while let Some((_, next)) = current.one_transition()? {
	    current = next;
	    n += 1;
	}
	Ok((n, current.available_entities.clone()))
    }

    /// see oneRun, run, smartOneRunEK, smartRunEK
    pub fn run(
	self
    ) -> Result<Vec<Rc<Self>>, String> {
	let mut res = vec![Rc::new(self)];
	while let Some((_, next_sys)) = res.last().unwrap().one_transition()? {
	    res.push(Rc::new(next_sys));
	}
	Ok(res)
    }


    /// see smartOneRunECT, smartRunECT
    pub fn run_separated(
	&self
    ) -> Result<Vec<(Set, Set, Set)>, String> {
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

    pub fn traces(
	self,
	n: usize,
    ) -> Result<Vec<Trace>, String> {
	if n == 0 {
	    return Ok(vec![])
	}
	let mut n = n;
	let mut res : Vec<Trace> = vec![];
	let mut current_trace: Trace = vec![(None, Rc::new(self))];
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
		    current_trace.push((Some(Rc::new(current_label)),
					Rc::new(next_sys)));
		} else {
		    branch[depth] = 0;
		    current_trace[depth] = (Some(Rc::new(current_label)),
					    Rc::new(next_sys));
		}
		new_branch = true;
	    } else {
		// at the bottom of a trace, we save to res, then backtrack
		// until we find another possible path.
		if new_branch {
		    res.push(current_trace[0..depth].to_vec());
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

/// Equality does not care about delta or reaction rules. Only entities and
/// context is compared
impl PartialEq for System {
    // we ignore delta and reaction rules
    fn eq(&self, other: &System) -> bool {
	self.available_entities == other.available_entities &&
	    self.context_process == other.context_process
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
	System::new()
    }
}

impl PrintableWithTranslator for System {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator)
	     -> std::fmt::Result {
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


// -----------------------------------------------------------------------------
//                                   Loops
// -----------------------------------------------------------------------------

impl System {
    /// A special case of systems is when the context recursively provides
    /// always the same set of entities.  The corresponding computation is
    /// infinite.  It consists of a finite sequence of states followed by a
    /// looping sequence.  IMPORTANT: We return all loops for all X = Q.X, by
    /// varing X.  The set of reactions Rs and the context x are constant.  Each
    /// state of the computation is distinguished by the current entities E.
    /// Under these assumptions, the predicate lollipop finds the Prefixes and
    /// the Loops sequences of entities.
    /// see lollipop
    pub fn lollipops(
	&self
    ) -> Vec<(Vec<Set>, Vec<Set>)> {
	self.delta.lollipops_decomposed(
	    &self.reaction_rules,
	    &self.available_entities,
	)
    }


    /// Only returns the loop part of the lollipop, returns for all X, where
    /// X = Q.X
    /// see loop
    pub fn lollipops_only_loop(
	self
    ) -> Vec<Vec<Set>> {
	let filtered =
	    self.delta.iter().filter_map(
		|l|
		l.1.filter_delta(l.0)
	    );

	let find_loop_fn = |q| {
	    Reaction::find_only_loop(&self.reaction_rules,
				     self.available_entities.clone(),
				     q)
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
    pub fn lollipops_named(
	&self,
	symb: IdType
    ) -> Option<(Vec<Set>, Vec<Set>)> {
	self.delta.lollipops_decomposed_named(
	    &self.reaction_rules,
	    &self.available_entities,
	    symb,
	)
    }


    /// Only returns the loop part of the lollipop, returns for all X, where
    /// X = Q.X
    /// see loop
    pub fn lollipops_only_loop_named(
	&self,
	symb: IdType
    ) -> Option<Vec<Set>> {
	let filtered = self
	    .delta
	    .iter()
	    .filter_map(
		|l|
		if *l.0 == symb {
		    l.1.filter_delta(&symb)
		} else {
		    None
		}
	    )
	    .next();

	let find_loop_fn = |q| {
	    Reaction::find_only_loop(&self.reaction_rules,
				     self.available_entities.clone(),
				     q)
	};

	filtered.map(find_loop_fn)
    }
}

// -----------------------------------------------------------------------------
//                                   Graph
// -----------------------------------------------------------------------------

impl System {
    /// Creates a graph starting from a system as root node
    pub fn digraph(
	self
    ) -> Result<SystemGraph, String> {
	use petgraph::Graph;

	let mut graph = Graph::default();
	let node = graph.add_node(self.clone());

	let mut association = HashMap::new();
	association.insert(self.clone(), node);

	let mut stack = vec![self];

	while let Some(current) = stack.pop() {
	    // depth first
	    let current_node = *association.get(&current).unwrap();

	    for (label, next) in TransitionsIterator::from(&current)? {
		// if not already visited
		let next_node = association.entry(next.clone()).or_insert_with(|| {
		    stack.push(next.clone());
		    graph.add_node(next)
		});
		graph.add_edge(current_node, *next_node, label);
	    }
	}
	Ok(graph)
    }
}


// -----------------------------------------------------------------------------
//                                 Statistics
// -----------------------------------------------------------------------------

impl System {
    /// Non simulated statistics of a system.
    /// Returns statistics about the system as a string.
    /// see main_do(stat,MissingE)
    pub fn statistics(
	&self,
	translator: &Translator,
    ) -> String {
	use super::translator;

	let mut result: String = "Statistics:\n".into();
	result.push_str(
	    "=============================================================\n"
	);
	result.push_str(&format!(
	    "the initial state has {} entities:\n",
	    self.available_entities.len()
	));
	result.push_str(&format!(
	    "{}\n",
	    translator::Formatter::from(translator, &self.available_entities)
	));

	let reactants = self
	    .reaction_rules
	    .iter()
	    .fold(Set::new(), |acc, new| acc.union(&new.reactants));
	result.push_str(&format!(
	    "The reactants are {}:\n{}\n",
	    reactants.len(),
	    translator::Formatter::from(translator, &reactants)
	));

	let inhibitors = self
	    .reaction_rules
	    .iter()
	    .fold(Set::new(), |acc, new| acc.union(&new.inhibitors));
	result.push_str(&format!(
	    "The inhibitors are {}:\n{}\n",
	    inhibitors.len(),
	    translator::Formatter::from(translator, &inhibitors)
	));

	let products = self
	    .reaction_rules
	    .iter()
	    .fold(Set::new(), |acc, new| acc.union(&new.products));
	result.push_str(&format!(
	    "The products are {}:\n{}\n",
	    products.len(),
	    translator::Formatter::from(translator, &products)
	));

	let total = reactants.union(&inhibitors.union(&products));
	result.push_str(&format!(
	    "The reactions involve {} entities:\n{}\n",
	    total.len(),
	    translator::Formatter::from(translator, &total)
	));

	let entities_env = self.delta.all_elements();
	result.push_str(&format!(
	    "The environment involves {} entities:\n{}\n",
	    entities_env.len(),
	    translator::Formatter::from(translator, &entities_env)
	));

	let entities_context = self.context_process.all_elements();
	result.push_str(&format!(
	    "The context involves {} entities:\n{}\n",
	    entities_context.len(),
	    translator::Formatter::from(translator, &entities_context)
	));

	let entities_all = total
	    .union(&entities_env)
	    .union(&entities_context)
	    .union(&self.available_entities);

	result.push_str(&format!(
	    "The whole RS involves {} entities:\n{}\n",
	    entities_all.len(),
	    translator::Formatter::from(translator, &entities_all)
	));

	let possible_e = products
	    .union(&self.available_entities)
	    .union(&entities_context);
	let missing_e = reactants.subtraction(&possible_e);
	result.push_str(&format!(
	    "There are {} reactants that will never be available:\n{}\n",
	    missing_e.len(),
	    translator::Formatter::from(translator, &missing_e)
	));

	let entities_not_needed = entities_context.subtraction(&total);
	result.push_str(&format!(
	    "The context can provide {} entities that will never be used:\
	     \n{}\n",
	    entities_not_needed.len(),
	    translator::Formatter::from(translator, &entities_not_needed)
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
	    "============================================================="
	);

	result
    }
}

//! Definitions for simple simulation steps.

use super::structure::{RSchoices,
		       RSenvironment,
		       RSlabel,
		       RSprocess,
		       RSset,
		       RSsystem};
use super::support_structures::TransitionsIterator;
use std::rc::Rc;

/// unfold returns the list of choices for the context given the process
/// definitions environment. RSchoices is a list of context moves mapping a set
/// of entities and the continuation.
/// see unfold
pub fn unfold(
    environment: &RSenvironment,
    context_process: &RSprocess,
    current_entities: &RSset,
) -> Result<RSchoices, String> {
    match context_process {
	RSprocess::Nill => {
	    Ok(RSchoices::new())
	},
	RSprocess::RecursiveIdentifier { identifier } => {
	    let newprocess = environment.get(*identifier);
	    if let Some(newprocess) = newprocess {
		unfold(environment, newprocess, current_entities)
	    } else {
		Err(format!("Missing symbol in context: {identifier}"))
	    }
	}
	RSprocess::EntitySet { entities, next_process, } => {
	    Ok(RSchoices::from([(
		Rc::new(entities.clone()),
		Rc::clone(next_process),
	    )]))
	},
	RSprocess::Guarded { reaction, next_process } => {
	    if reaction.enabled(current_entities) {
		Ok(RSchoices::from([(Rc::new(reaction.products.clone()),
				     Rc::clone(next_process))]))
	    } else {
		Ok(RSchoices::new())
	    }
	}
	RSprocess::WaitEntity { repeat, repeated_process: _, next_process, }
	if *repeat <= 0 => {
	    unfold(environment, next_process, current_entities)
	},
	RSprocess::WaitEntity { repeat, repeated_process, next_process, }
	if *repeat == 1 => {
	    let mut choices1 = unfold(environment,
				      repeated_process,
				      current_entities)?;
	    choices1.replace(Rc::clone(next_process));
	    Ok(choices1)
	}
	RSprocess::WaitEntity { repeat, repeated_process, next_process, } => {
	    let mut choices1 = unfold(environment,
				      repeated_process,
				      current_entities)?;
	    choices1.replace(Rc::new(RSprocess::WaitEntity {
		repeat: (*repeat - 1),
		repeated_process: Rc::clone(repeated_process),
		next_process: Rc::clone(next_process),
	    }));
	    Ok(choices1)
	}
	RSprocess::Summation { children } => {
	    // short-circuits with try_fold.
	    children.iter().try_fold(RSchoices::new(), |mut acc, x| {
		match unfold(environment, x, current_entities) {
		    Ok(mut choices) => {
			acc.append(&mut choices);
			Ok(acc)
		    }
		    Err(e) => Err(e),
		}
	    })
	}
	RSprocess::NondeterministicChoice { children } => {
	    // short-circuits with try_fold.
	    if children.is_empty() {
		Ok(RSchoices::from(vec![(
		    Rc::new(RSset::new()),
		    Rc::new(RSprocess::Nill),
		)]))
	    } else {
		children.iter().try_fold(RSchoices::new(), |mut acc, x| {
		    acc.shuffle(unfold(environment, x, current_entities)?);
		    Ok(acc)
		})
	    }
	}
    }
}

pub fn iterator_transitions<'a>(
    system: &'a RSsystem
) -> Result<TransitionsIterator<'a>, String> {
    TransitionsIterator::from(system)
}

/// see oneTransition, transition, smartTransition, smartOneTransition
pub fn one_transition(
    system: &RSsystem
) -> Result<Option<(RSlabel, RSsystem)>, String> {
    let mut tr = TransitionsIterator::from(system)?;
    Ok(tr.next())
}

/// see allTransitions, smartAllTransitions
pub fn all_transitions(
    system: &RSsystem
) -> Result<Vec<(RSlabel, RSsystem)>, String> {
    let tr = TransitionsIterator::from(system)?;
    Ok(tr.collect::<Vec<_>>())
}

/// see oneTarget, smartOneTarget, target, smartTarget
pub fn target(
    system: &RSsystem
) -> Result<(i64, RSset), String> {
    let current = one_transition(system)?;
    if current.is_none() {
	return Ok((0, system.available_entities.clone()));
    }
    let mut n = 1;
    let mut current = current.unwrap().1;
    while let Some((_, next)) = one_transition(&current)? {
	current = next;
	n += 1;
    }
    Ok((n, current.available_entities.clone()))
}

/// see oneRun, run, smartOneRunEK, smartRunEK
pub fn run(system: RSsystem) -> Result<Vec<Rc<RSsystem>>, String> {
    let mut res = vec![Rc::new(system)];
    while let Some((_, next_sys)) = one_transition(res.last().unwrap())? {
	res.push(Rc::new(next_sys));
    }
    Ok(res)
}

/// see smartOneRunECT, smartRunECT
pub fn run_separated(
    system: &RSsystem
) -> Result<Vec<(RSset, RSset, RSset)>, String> {
    let mut res = vec![];
    let current = one_transition(system)?;
    if current.is_none() {
	return Ok(res);
    }
    let current = current.unwrap();
    let (available_entities, context, t) = current.0.get_context();
    res.push((available_entities.clone(), context.clone(), t.clone()));
    let mut current = current.1;
    while let Some((label, next)) = one_transition(&current)? {
	current = next;
	let (available_entities, context, t) = label.get_context();
	res.push((available_entities.clone(), context.clone(), t.clone()));
    }
    Ok(res)
}



fn nth_transition(
    system: &RSsystem,
    n: usize,
) -> Result<Option<(RSlabel, RSsystem)>, String> {
    let mut tr = TransitionsIterator::from(system)?;
    Ok(tr.nth(n))
}

type Trace = Vec<(Option<Rc<RSlabel>>, Rc<RSsystem>)>;

pub fn traces(
    system: RSsystem,
    n: usize,
) -> Result<Vec<Trace>, String> {
    if n == 0 {
	return Ok(vec![])
    }
    let mut n = n;
    let mut res : Vec<Trace> = vec![];
    let mut current_trace: Trace = vec![(None, Rc::new(system))];
    let mut branch = vec![0];
    let mut depth = 0;
    let mut new_branch = true;

    loop {
	let next_sys = nth_transition(&current_trace[depth].1,
				      branch[depth])?;

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
	    // at the bottom of a trace, we save to res, then backtrack until
	    // we find another possible path.
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

#[test]
fn traces_1() {
    use super::structure::RSreaction;

    let system = RSsystem {
	delta: Rc::new(RSenvironment::from([
	    (100, RSprocess::WaitEntity {
		repeat: 2,
		repeated_process: Rc::new(RSprocess::EntitySet {
		    entities: RSset::from([1]),
		    next_process: Rc::new(RSprocess::Nill)
		}),
		next_process: Rc::new(RSprocess::Nill) }),
	    (102, RSprocess::WaitEntity {
		repeat: 3,
		repeated_process: Rc::new(RSprocess::EntitySet {
		    entities: RSset::from([2]),
		    next_process: Rc::new(RSprocess::Nill) }),
		next_process: Rc::new(RSprocess::Nill) }),
	    (103, RSprocess::WaitEntity {
		repeat: 4,
		repeated_process: Rc::new(RSprocess::EntitySet {
		    entities: RSset::from([3]),
		    next_process: Rc::new(RSprocess::Nill) }),
		next_process: Rc::new(RSprocess::Nill) }),
	    (101, RSprocess::Summation { children: vec![
		Rc::new(RSprocess::EntitySet {
		    entities: RSset::from([10]),
		    next_process: Rc::new(RSprocess::RecursiveIdentifier {
			identifier: 100 }) }),
		Rc::new(RSprocess::EntitySet {
		    entities: RSset::from([11]),
		    next_process: Rc::new(RSprocess::RecursiveIdentifier {
			identifier: 102 }) }),
		Rc::new(RSprocess::EntitySet {
		    entities: RSset::from([11]),
		    next_process: Rc::new(RSprocess::RecursiveIdentifier {
			identifier: 103 }) })
	    ] }),
	])),
	available_entities: RSset::from([1, 2]),
	context_process: RSprocess::RecursiveIdentifier { identifier: 101 },
	reaction_rules:
	Rc::new(vec![RSreaction { reactants: RSset::from([1]),
				  inhibitors: RSset::from([3]),
				  products: RSset::from([3]), },
		     RSreaction { reactants: RSset::from([3]),
				  inhibitors: RSset::from([1]),
				  products: RSset::from([1]), },
		     RSreaction { reactants: RSset::from([2]),
				  inhibitors: RSset::new(),
				  products: RSset::from([4]), },
	])
    };

    // for (pos, trace) in res.iter().enumerate() {
    //	println!("trace {}:", pos);
    //	for (_, sy) in trace {
    //	    let ent = format!("{:?}", sy.available_entities);
    //	    let con = format!("{:?}", sy.context_process);
    //	    println!("\t({}, {})", ent, con);
    //	}
    // }

    let res = traces(system.clone(), 1).unwrap();
    assert_eq!(res.len(), 1);

    let res = traces(system.clone(), 2).unwrap();
    assert_eq!(res.len(), 2);
    assert_eq!(res[0].len() + 1, res[1].len());

    let res = traces(system.clone(), 3).unwrap();
    assert_eq!(res.len(), 3);

    let res = traces(system.clone(), 4).unwrap();
    assert_eq!(res.len(), 3);

    let res = traces(system.clone(), 0).unwrap();
    assert_eq!(res.len(), 0);
}

#[test]
fn traces_empty_env() {
    use super::structure::RSreaction;

    let system = RSsystem {
	delta: Rc::new(RSenvironment::from([])),
	available_entities: RSset::from([1, 2]),
	context_process: RSprocess::WaitEntity {
	    repeat: 10,
	    repeated_process: Rc::new(RSprocess::EntitySet {
		entities: RSset::from([1, 2]),
		next_process: Rc::new(RSprocess::Nill) }),
	    next_process: Rc::new(RSprocess::Nill) },
	reaction_rules:
	Rc::new(vec![RSreaction { reactants: RSset::from([1]),
				  inhibitors: RSset::from([3]),
				  products: RSset::from([3]), },
		     RSreaction { reactants: RSset::from([3]),
				  inhibitors: RSset::from([1]),
				  products: RSset::from([1]), },
		     RSreaction { reactants: RSset::from([2]),
				  inhibitors: RSset::new(),
				  products: RSset::from([4]), },
	])
    };

    let res = traces(system.clone(), 10).unwrap();
    assert_eq!(res.len(), 1);
    assert_eq!(res[0].len(), 10);
}

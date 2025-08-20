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
            let mut choices1 = unfold(environment, repeated_process, current_entities)?;
            choices1.replace(Rc::clone(next_process));
            Ok(choices1)
        }
        RSprocess::WaitEntity { repeat, repeated_process, next_process, } => {
            let mut choices1 = unfold(environment, repeated_process, current_entities)?;
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

#![allow(dead_code)]

use super::structure::{RSchoices,
		       RSenvironment,
		       RSlabel,
		       RSprocess,
		       RSset,
		       RSsystem};
use super::support_structures::TransitionsIterator;
use std::rc::Rc;

// see unfold
pub fn unfold(
    environment: &RSenvironment,
    context_process: &RSprocess,
) -> Result<RSchoices, String> {
    match context_process {
        RSprocess::Nill => Ok(RSchoices::new()),
        RSprocess::RecursiveIdentifier { identifier } => {
            let newprocess = environment.get(*identifier);
            if let Some(newprocess) = newprocess {
                unfold(environment, newprocess)
            } else {
                Err(format!("Recursive call to missing symbol: {identifier}"))
            }
        }
        RSprocess::EntitySet {
            entities,
            next_process,
        } => Ok(RSchoices::from(vec![(
            Rc::new(entities.clone()),
            Rc::clone(next_process),
        )])),
        RSprocess::WaitEntity {
            repeat,
            repeated_process: _,
            next_process,
        } if *repeat <= 0 => unfold(environment, next_process),
        RSprocess::WaitEntity {
            repeat,
            repeated_process,
            next_process,
        } if *repeat == 1 => {
            let mut choices1 = unfold(environment, repeated_process)?;
            choices1.replace(Rc::clone(next_process));
            Ok(choices1)
        }
        RSprocess::WaitEntity {
            repeat,
            repeated_process,
            next_process,
        } => {
            let mut choices1 = unfold(environment, repeated_process)?;
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
                match unfold(environment, x) {
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
                    acc.shuffle(unfold(environment, x)?);
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

// see oneTransition, transition, smartTransition, smartOneTransition
pub fn one_transition(
    system: &RSsystem
) -> Result<Option<(RSlabel, RSsystem)>, String> {
    let mut tr = TransitionsIterator::from(system)?;
    Ok(tr.next())
}

// see allTransitions, smartAllTransitions
pub fn all_transitions(
    system: &RSsystem
) -> Result<Vec<(RSlabel, RSsystem)>, String> {
    let tr = TransitionsIterator::from(system)?;
    Ok(tr.collect::<Vec<_>>())
}

// see oneTarget, smartOneTarget, target, smartTarget
pub fn target(
    system: &RSsystem
) -> Result<(i64, RSset), String> {
    let current = one_transition(system)?;
    if current.is_none() {
        return Ok((0, system.get_available_entities().clone()));
    }
    let mut n = 1;
    let mut current = current.unwrap().1;
    while let Some((_, next)) = one_transition(&current)? {
        current = next;
        n += 1;
    }
    Ok((n, current.get_available_entities().clone()))
}

// see oneRun, run, smartOneRunEK, smartRunEK
pub fn run(system: RSsystem) -> Result<Vec<Rc<RSsystem>>, String> {
    let mut res = vec![Rc::new(system)];
    while let Some((_, next_sys)) = one_transition(res.last().unwrap())? {
        res.push(Rc::new(next_sys));
    }
    Ok(res)
}

// see smartOneRunECT, smartRunECT
pub fn run_separated(
    system: &RSsystem
) -> Result<Vec<(RSset, RSset, RSset)>, String> {
    let mut res = vec![];
    let current = one_transition(system)?;
    if current.is_none() {
        return Ok(res);
    }
    let current = current.unwrap();
    res.push(current.0.get_context());
    let mut current = current.1;
    while let Some((label, next)) = one_transition(&current)? {
        current = next;
        res.push(label.get_context());
    }
    Ok(res)
}

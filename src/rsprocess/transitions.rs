#![allow(dead_code)]

use std::rc::Rc;
use super::structure::{RSset, RSChoices, RSenvironment, RSprocess, RSsystem, RSlabel};


pub fn unfold<'a>(environment: &'a RSenvironment<'a>, context_process: &'a RSprocess<'a>) -> Result<RSChoices<'a>, String> {
    match context_process {
	RSprocess::Nill => Ok(RSChoices::new()),
	RSprocess::RecursiveIdentifier{identifier} => {
	    let newprocess = environment.get(identifier);
	    if let Some(newprocess) = newprocess {
		unfold(environment, newprocess)
	    } else {
		Err(format!("Recursive call to missing symbol: {identifier}"))
	    }
	},
	RSprocess::EntitySet{entities, next_process} => {
	    Ok(RSChoices::from(vec![(Rc::new(entities.clone()), Rc::clone(next_process))]))
	},
	RSprocess::WaitEntity{repeat, repeated_process: _, next_process} if *repeat <= 0 => {
	    unfold(environment, next_process)
	},
	RSprocess::WaitEntity{repeat, repeated_process, next_process} if *repeat == 1 => {
	    let mut choices1 = unfold(environment, repeated_process)?;
	    choices1.replace(Rc::clone(next_process));
	    Ok(choices1)
	},
	RSprocess::WaitEntity{repeat, repeated_process, next_process} => {
	    let mut choices1 = unfold(environment, repeated_process)?;
	    choices1.replace(
		Rc::new(
		    RSprocess::WaitEntity{ repeat: (*repeat - 1),
					   repeated_process: Rc::clone(repeated_process),
					   next_process: Rc::clone(next_process) }
		)
	    );
	    Ok(choices1)
	},
	RSprocess::Summation{children} => {
	    // short-circuits with try_fold.
	    children.iter().try_fold(RSChoices::new(), |mut acc, x| {
		match unfold(environment, x) {
		    Ok(mut choices) => {acc.append(&mut choices); Ok(acc)},
		    Err(e) => Err(e)
		}
	    })
	},
	RSprocess::NondeterministicChoice{children} => {
	    // short-circuits with try_fold.
	    if children.is_empty() {
		Ok(RSChoices::from(vec![(Rc::new(RSset::new()), Rc::new(RSprocess::Nill))]))
	    } else {
		children.iter().try_fold(RSChoices::new(), |mut acc, x| {
		    acc.shuffle(unfold(environment, x)?);
		    Ok(acc)
		})
	    }
	}
    }
}

pub fn all_transitions<'a>(system: &'a RSsystem<'a>) -> Result<Vec<(RSlabel<'a>, RSsystem<'a>)>, String> {
    let choices = unfold(system.get_delta(), system.get_context_process())?;
    println!("choices: {:?}", choices);
    println!("\n\n");
    let mut results = vec![];

    for choice in choices {
	let t = system.get_available_entities().union(choice.0.as_ref());
	let (reactants, reactantsi, inihibitors, ireactants, products)
	    = system.get_reaction_rules().iter()
	    .fold((RSset::new(), RSset::new(), RSset::new(), RSset::new(), RSset::new()),
		  |acc, reaction| if reaction.enabled(&t) {
		      (acc.0.union(reaction.reactants()),
		       acc.1,
		       acc.2.union(reaction.inihibitors()),
		       acc.3,
		       acc.4.union(reaction.products()))
		  } else {
		      (acc.0,
		       acc.1.union(&reaction.inihibitors().intersection(&t)),
		       acc.2,
		       acc.3.union(&reaction.reactants().subtraction(&t)),
		       acc.4)
		  }

	    );

	let label = RSlabel::from(system.get_available_entities().clone(),
				  (*choice.0).clone(),
				  t,
				  reactants,
				  reactantsi,
				  inihibitors,
				  ireactants,
				  products.clone());
	let new_system = RSsystem::from(system.get_delta().clone(),
					products,
					(*choice.1).clone(),
					system.get_reaction_rules().clone());
	results.push((label, new_system))
    }

    Ok(results)
}

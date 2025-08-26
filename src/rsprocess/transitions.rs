//! Module for helper structure for simulation

use std::rc::Rc;

use super::label::Label;
use super::process::Process;
use super::set::{BasicSet, Set};
use super::system::System;
use super::reaction::BasicReaction;

#[derive(Clone, Debug)]
pub struct TransitionsIterator<'a> {
    choices_iterator: std::vec::IntoIter<(Rc<Set>, Rc<Process>)>,
    system: &'a System,
}

impl<'a> TransitionsIterator<'a> {
    pub fn from(
	system: &'a System
    ) -> Result<TransitionsIterator<'a>, String> {
        match system.delta.unfold(&system.context_process,
				  &system.available_entities) {
            Ok(o) => Ok(TransitionsIterator {
                choices_iterator: o.into_iter(),
                system,
            }),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Iterator for TransitionsIterator<'a> {
    type Item = (Label, System);

    /// Creates the next arc from the current system.
    fn next(&mut self) -> Option<(Label, System)> {
        let (c, k) = self.choices_iterator.next()?;
        let t = self.system.available_entities.union(c.as_ref());
        let (
	    reactants,
	    reactants_absent,
	    inhibitors,
	    inhibitors_present,
	    products
	) =
            self.system.reaction_rules.iter().fold(
                (
                    Set::default(), // reactants
                    Set::default(), // reactants_absent
                    Set::default(), // inhibitors
                    Set::default(), // inhibitors_present
                    Set::default(), // products
                ),
                |acc, reaction| {
                    if reaction.enabled(&t) {
			(
                            acc.0.union(&reaction.reactants),
                            acc.1,
                            acc.2.union(&reaction.inhibitors),
                            acc.3,
                            acc.4.union(&reaction.products),
			)
                    } else {
                        (
                            acc.0,
                            acc.1.union(&reaction.inhibitors.intersection(&t)),
                            acc.2,
                            acc.3.union(&reaction.reactants.subtraction(&t)),
                            acc.4,
                        )
                    }
                },
            );

        let label = Label::from(
            self.system.available_entities.clone(),
            (*c).clone(),
            t,
            reactants,
            reactants_absent,
            inhibitors,
            inhibitors_present,
            products.clone(),
        );
        let new_system = System::from(
            Rc::clone(&self.system.delta),
            products,
            (*k).clone(),
            Rc::clone(&self.system.reaction_rules),
        );
        Some((label, new_system))
    }
}

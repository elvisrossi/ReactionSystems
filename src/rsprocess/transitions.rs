//! Module for helper structure for simulation

use std::rc::Rc;

use super::label::{Label, PositiveLabel};
use super::process::{BasicProcess, PositiveProcess, Process};
use super::set::{BasicSet, PositiveSet, Set};
use super::system::{BasicSystem, PositiveSystem, System};
use super::reaction::BasicReaction;
use super::environment::BasicEnvironment;

#[derive(Clone, Debug)]
pub struct TransitionsIterator<'a, S: BasicSet,
			       Sys: BasicSystem<Set = S>,
			       Proc: BasicProcess<Set = S>> {
    choices_iterator: std::vec::IntoIter<(Rc<S>, Rc<Proc>)>,
    system: &'a Sys,
}

impl<'a> TransitionsIterator<'a, Set, System, Process> {
    pub fn from(
	system: &'a System
    ) -> Result<Self, String> {
        match system.environment().unfold(system.context(),
					  system.available_entities()) {
            Ok(o) => Ok(TransitionsIterator {
                choices_iterator: o.into_iter(),
                system,
            }),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Iterator for TransitionsIterator<'a, Set, System, Process> {
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

// -----------------------------------------------------------------------------

impl<'a> TransitionsIterator<'a, PositiveSet, PositiveSystem, PositiveProcess> {
    pub fn from(
	system: &'a PositiveSystem
    ) -> Result<Self, String> {
	match system.environment().unfold(system.context(),
					  system.available_entities()) {
            Ok(o) => Ok(TransitionsIterator {
                choices_iterator: o.into_iter(),
                system,
	    }),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Iterator for
    TransitionsIterator<'a, PositiveSet, PositiveSystem, PositiveProcess> {
    type Item = (PositiveLabel, PositiveSystem);

    /// Creates the next arc from the current system.
    fn next(&mut self) -> Option<Self::Item> {
	let (c, k) = self.choices_iterator.next()?;
        let t = self.system.available_entities.union(c.as_ref());
        let (
	    reactants,
	    reactants_absent,
	    products
	) =
            self.system.reaction_rules.iter().fold(
                (
                    PositiveSet::default(), // reactants
                    PositiveSet::default(), // reactants_absent
                    PositiveSet::default(), // products
                ),
                |acc, reaction| {
                    if reaction.enabled(&t) {
			(
                            acc.0.union(&reaction.reactants),
                            acc.1,
                            acc.2.union(&reaction.products),
			)
                    } else {
                        (
                            acc.0,
			    // TODO is this right?
                            acc.1.union(&reaction.reactants.intersection(&t)),
                            acc.2,
                        )
                    }
                },
            );

        let label = PositiveLabel::from(
            self.system.available_entities.clone(),
            (*c).clone(),
            t,
            reactants,
            reactants_absent,
            products.clone(),
        );
        let new_system = PositiveSystem::from(
            Rc::clone(&self.system.delta),
            products,
            (*k).clone(),
            Rc::clone(&self.system.reaction_rules),
        );
        Some((label, new_system))
    }
}

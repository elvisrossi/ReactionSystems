//! Module for helper structure for simulation

use super::structure::{RSlabel, RSprocess, RSset, RSsystem};
use super::transitions::unfold;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct TransitionsIterator<'a> {
    choices_iterator: std::vec::IntoIter<(Rc<RSset>, Rc<RSprocess>)>,
    system: &'a RSsystem,
}

impl<'a> TransitionsIterator<'a> {
    pub fn from(
	system: &'a RSsystem
    ) -> Result<TransitionsIterator<'a>, String> {
        match unfold(&system.delta, &system.context_process) {
            Ok(o) => Ok(TransitionsIterator {
                choices_iterator: o.into_iter(),
                system,
            }),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Iterator for TransitionsIterator<'a> {
    type Item = (RSlabel, RSsystem);

    /// Creates the next arc from the current system.
    fn next(&mut self) -> Option<(RSlabel, RSsystem)> {
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
                    RSset::new(), // reactants
                    RSset::new(), // reactants_absent
                    RSset::new(), // inhibitors
                    RSset::new(), // inhibitors_present
                    RSset::new(), // products
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

        let label = RSlabel::from(
            self.system.available_entities.clone(),
            (*c).clone(),
            t,
            reactants,
            reactants_absent,
            inhibitors,
            inhibitors_present,
            products.clone(),
        );
        let new_system = RSsystem::from(
            Rc::clone(&self.system.delta),
            products,
            (*k).clone(),
            Rc::clone(&self.system.reaction_rules),
        );
        Some((label, new_system))
    }
}

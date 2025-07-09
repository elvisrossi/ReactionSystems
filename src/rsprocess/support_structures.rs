#![allow(dead_code)]
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

    fn next(&mut self) -> Option<(RSlabel, RSsystem)> {
        let (c, k) = self.choices_iterator.next()?;
        let t = self.system.available_entities.union(c.as_ref());
        let (reactants, reactantsi, inihibitors, ireactants, products) =
            self.system.reaction_rules.iter().fold(
                (
                    RSset::new(), // reactants
                    RSset::new(), // reactantsi
                    RSset::new(), // inihibitors
                    RSset::new(), // ireactants
                    RSset::new(), // products
                ),
                |acc, reaction| {
                    if reaction.enabled(&t) {
			(
                            acc.0.union(&reaction.reactants),
                            acc.1,
                            acc.2.union(&reaction.inihibitors),
                            acc.3,
                            acc.4.union(&reaction.products),
			)
                    } else {
                        (
                            acc.0,
                            acc.1.union(&reaction.inihibitors.intersection(&t)),
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
            reactantsi,
            inihibitors,
            ireactants,
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

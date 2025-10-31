//! Module for helper structure for simulation

use std::fmt::Debug;
use std::sync::Arc;

use super::label::{Label, PositiveLabel};
use super::process::{BasicProcess, PositiveProcess, Process};
use super::reaction::BasicReaction;
use super::set::{BasicSet, PositiveSet, Set};
use super::system::{BasicSystem, ExtensionsSystem, PositiveSystem, System};

pub trait BasicTransition
where
    Self: Clone + Debug + Iterator,
{
}

#[derive(Clone, Debug)]
pub struct TransitionsIterator<
    'a,
    S: BasicSet,
    Sys: BasicSystem<Set = S>,
    Proc: BasicProcess<Set = S>,
> {
    choices_iterator: std::vec::IntoIter<(Arc<S>, Arc<Proc>)>,
    system: &'a Sys,
}

impl<'a> BasicTransition for TransitionsIterator<'a, Set, System, Process> {}

impl<'a> TryFrom<&'a System> for TransitionsIterator<'a, Set, System, Process> {
    type Error = String;

    fn try_from(value: &'a System) -> Result<Self, Self::Error> {
        match value.unfold() {
            | Ok(o) => Ok(Self {
                choices_iterator: o.into_iter(),
                system: value,
            }),
            | Err(e) => Err(e),
        }
    }
}

impl<'a> Iterator for TransitionsIterator<'a, Set, System, Process> {
    type Item = (Label, System);

    /// Creates the next arc from the current system.
    fn next(&mut self) -> Option<Self::Item> {
        let (c, k) = self.choices_iterator.next()?;
        let t = self.system.available_entities.union(c.as_ref());
        let (
            reactants,
            reactants_absent,
            inhibitors,
            inhibitors_present,
            products,
        ) = self.system.reactions().iter().fold(
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
            Arc::clone(&self.system.delta),
            products,
            (*k).clone(),
            Arc::clone(&self.system.reaction_rules),
        );
        Some((label, new_system))
    }
}

// -----------------------------------------------------------------------------

impl<'a> BasicTransition
    for TransitionsIterator<'a, PositiveSet, PositiveSystem, PositiveProcess>
{
}

impl<'a> TryFrom<&'a PositiveSystem>
    for TransitionsIterator<'a, PositiveSet, PositiveSystem, PositiveProcess>
{
    type Error = String;

    fn try_from(value: &'a PositiveSystem) -> Result<Self, Self::Error> {
        match value.unfold() {
            | Ok(o) => Ok(Self {
                choices_iterator: o.into_iter(),
                system: value,
            }),
            | Err(e) => Err(e),
        }
    }
}

impl<'a> Iterator
    for TransitionsIterator<'a, PositiveSet, PositiveSystem, PositiveProcess>
{
    type Item = (PositiveLabel, PositiveSystem);

    /// Creates the next arc from the current system.
    fn next(&mut self) -> Option<Self::Item> {
        let (c, k) = self.choices_iterator.next()?;
        let t = self.system.available_entities.union(c.as_ref());
        let (
            reactants,
            reactants_absent,
            inhibitors,
            inhibitors_present,
            products,
        ) = self.system.reaction_rules.iter().fold(
            (
                PositiveSet::default(), // reactants
                PositiveSet::default(), // reactants_absent
                PositiveSet::default(), // inhibitors
                PositiveSet::default(), // inhibitors_present
                PositiveSet::default(), // products
            ),
            |acc, reaction| {
                if reaction.enabled(&t) {
                    (
                        acc.0.union(&reaction.reactants.positives()),
                        acc.1,
                        acc.2.union(&reaction.reactants.negatives()),
                        acc.3,
                        acc.4.union(&reaction.products),
                    )
                } else {
                    (
                        acc.0,
                        acc.1.union(
                            &reaction.reactants.negatives().intersection(&t),
                        ),
                        acc.2,
                        acc.3.union(
                            &reaction.reactants.positives().subtraction(&t),
                        ),
                        acc.4,
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
            inhibitors,
            inhibitors_present,
            products.clone(),
        );
        let new_system = PositiveSystem::from(
            Arc::clone(&self.system.delta),
            // products.add_unique(&self.system.negated_products_elements()),
            products,
            (*k).clone(),
            Arc::clone(&self.system.reaction_rules),
        );
        Some((label, new_system))
    }
}

// -----------------------------------------------------------------------------

pub struct TraceIterator<
    'a,
    S: BasicSet,
    Sys: BasicSystem<Set = S, Process = Proc>,
    Proc: BasicProcess<Set = S>,
> {
    choices_iterator:
        <<Sys as BasicSystem>::Choices as std::iter::IntoIterator>::IntoIter,
    system: &'a Sys,
}

impl<'a> TryFrom<&'a PositiveSystem>
    for TraceIterator<'a, PositiveSet, PositiveSystem, PositiveProcess>
{
    type Error = String;

    fn try_from(value: &'a PositiveSystem) -> Result<Self, Self::Error> {
        match value.unfold() {
            | Ok(o) => Ok(Self {
                choices_iterator: o.into_iter(),
                system: value,
            }),
            | Err(e) => Err(e),
        }
    }
}

impl<'a> TryFrom<&'a System> for TraceIterator<'a, Set, System, Process> {
    type Error = String;

    fn try_from(value: &'a System) -> Result<Self, Self::Error> {
        match value.unfold() {
            | Ok(o) => Ok(Self {
                choices_iterator: o.into_iter(),
                system: value,
            }),
            | Err(e) => Err(e),
        }
    }
}

impl<'a> Iterator for TraceIterator<'a, Set, System, Process> {
    type Item = (Set, Set, Vec<usize>, System);

    fn next(&mut self) -> Option<Self::Item> {
        let (context, k) = self.choices_iterator.next()?;
        let total_entities =
            self.system.available_entities().union(context.as_ref());

        let (enabled_reaction_positions, all_products) =
            self.system.reactions().iter().enumerate().fold(
                (vec![], Set::default()),
                |mut acc, (pos, reaction)| {
                    if reaction.enabled(&total_entities) {
                        acc.0.push(pos);
                        (acc.0, acc.1.union(&reaction.products))
                    } else {
                        acc
                    }
                },
            );

        let new_system = System::from(
            Arc::clone(&self.system.delta),
            // all_products.add_unique(&self.system.
            // negated_products_elements()),
            all_products.clone(),
            (*k).clone(),
            Arc::clone(&self.system.reaction_rules),
        );

        Some((
            context.as_ref().clone(),
            all_products,
            enabled_reaction_positions,
            new_system,
        ))
    }
}

impl<'a> Iterator
    for TraceIterator<'a, PositiveSet, PositiveSystem, PositiveProcess>
{
    type Item = (PositiveSet, PositiveSet, Vec<usize>, PositiveSystem);

    fn next(&mut self) -> Option<Self::Item> {
        let (context, k) = self.choices_iterator.next()?;
        let total_entities =
            self.system.available_entities().union(context.as_ref());

        let (enabled_reaction_positions, all_products) =
            self.system.reactions().iter().enumerate().fold(
                (vec![], PositiveSet::default()),
                |mut acc, (pos, reaction)| {
                    if reaction.enabled(&total_entities) {
                        acc.0.push(pos);
                        (acc.0, acc.1.union(&reaction.products))
                    } else {
                        acc
                    }
                },
            );

        let new_system = PositiveSystem::from(
            Arc::clone(&self.system.delta),
            // all_products.add_unique(&self.system.
            // negated_products_elements()),
            all_products.clone(),
            (*k).clone(),
            Arc::clone(&self.system.reaction_rules),
        );

        Some((
            context.as_ref().clone(),
            all_products.mask(&self.system.products_elements()),
            enabled_reaction_positions,
            new_system,
        ))
    }
}

use std::collections::BTreeMap;
use std::sync::Arc;

use super::element::{IdState, PositiveType};
use super::environment::{Environment, PositiveEnvironment};
use super::process::{PositiveProcess, Process};
use super::reaction::{PositiveReaction, Reaction};
use super::set::{BasicSet, PositiveSet, Set};
use super::system::{BasicSystem, ExtensionsSystem, PositiveSystem, System};
use crate::boolean::{BooleanFunction, BooleanNetwork};

#[test]
fn one_transition() {
    let system = System::from(
        Arc::new(Environment::default()),
        Set::from([1, 2]),
        Process::EntitySet {
            entities:     Set::from([]),
            next_process: Arc::new(Process::Nill),
        },
        Arc::new(vec![Reaction::from(
            Set::from([1]),
            Set::from([3]),
            Set::from([3]),
        )]),
    );

    match system.one_transition() {
        | Ok(Some((
            _,
            System {
                available_entities, ..
            },
        ))) => assert!(
            available_entities.len() == 1 && available_entities.contains(&3)
        ),
        | _ => panic!(),
    }
}

#[test]
fn one_transition_2() {
    let system = PositiveSystem::from(
        Arc::new(PositiveEnvironment::default()),
        PositiveSet::from([
            (1, IdState::Positive),
            (2, IdState::Positive),
            (3, IdState::Negative),
        ]),
        PositiveProcess::WaitEntity {
            repeat: 2,
            repeated_process: Arc::new(PositiveProcess::EntitySet {
                entities:     PositiveSet::default(),
                next_process: Arc::new(PositiveProcess::Nill),
            }),
            next_process: Arc::new(PositiveProcess::Nill),
        },
        Arc::new(vec![
            PositiveReaction {
                reactants: PositiveSet::from([
                    (1, IdState::Positive),
                    (3, IdState::Negative),
                ]),
                products:  PositiveSet::from([(3, IdState::Positive)]),
            },
            PositiveReaction {
                reactants: PositiveSet::from([(3, IdState::Positive)]),
                products:  PositiveSet::from([(3, IdState::Negative)]),
            },
            PositiveReaction {
                reactants: PositiveSet::from([(1, IdState::Negative)]),
                products:  PositiveSet::from([(3, IdState::Negative)]),
            },
        ]),
    );

    let system = system.one_transition();

    let system = match system {
        | Ok(Some((_, system))) => {
            assert!(
                system.available_entities.len() == 1
                    && system.available_entities.contains(&PositiveType {
                        id:    3,
                        state: IdState::Positive,
                    })
            );
            system
        },
        | _ => panic!(),
    };

    match system.one_transition() {
        | Ok(Some((
            _,
            PositiveSystem {
                available_entities, ..
            },
        ))) => assert!(
            available_entities.len() == 1
                && available_entities.contains(&PositiveType {
                    id:    3,
                    state: IdState::Negative,
                })
        ),
        | _ => panic!(),
    }
}

#[test]
fn convertion() {
    let system = System::from(
        Arc::new(Environment::default()),
        Set::from([1, 2]),
        Process::EntitySet {
            entities:     Set::from([]),
            next_process: Arc::new(Process::Nill),
        },
        Arc::new(vec![Reaction::from(
            Set::from([1]),
            Set::from([3]),
            Set::from([3]),
        )]),
    );

    let system: PositiveSystem = system.into();

    assert_eq!(system.available_entities.len(), 3); // should be +1, +2, -3
    assert_eq!(system.reaction_rules.len(), 3);
}

#[test]
fn traces_1() {
    let system = System::from(
        Arc::new(Environment::from([
            (100, Process::WaitEntity {
                repeat: 2,
                repeated_process: Arc::new(Process::EntitySet {
                    entities:     Set::from([1]),
                    next_process: Arc::new(Process::Nill),
                }),
                next_process: Arc::new(Process::Nill),
            }),
            (102, Process::WaitEntity {
                repeat: 3,
                repeated_process: Arc::new(Process::EntitySet {
                    entities:     Set::from([2]),
                    next_process: Arc::new(Process::Nill),
                }),
                next_process: Arc::new(Process::Nill),
            }),
            (103, Process::WaitEntity {
                repeat: 4,
                repeated_process: Arc::new(Process::EntitySet {
                    entities:     Set::from([3]),
                    next_process: Arc::new(Process::Nill),
                }),
                next_process: Arc::new(Process::Nill),
            }),
            (101, Process::Summation {
                children: vec![
                    Arc::new(Process::EntitySet {
                        entities:     Set::from([10]),
                        next_process: Arc::new(Process::RecursiveIdentifier {
                            identifier: 100,
                        }),
                    }),
                    Arc::new(Process::EntitySet {
                        entities:     Set::from([11]),
                        next_process: Arc::new(Process::RecursiveIdentifier {
                            identifier: 102,
                        }),
                    }),
                    Arc::new(Process::EntitySet {
                        entities:     Set::from([11]),
                        next_process: Arc::new(Process::RecursiveIdentifier {
                            identifier: 103,
                        }),
                    }),
                ],
            }),
        ])),
        Set::from([1, 2]),
        Process::RecursiveIdentifier { identifier: 101 },
        Arc::new(vec![
            Reaction::from(Set::from([1]), Set::from([3]), Set::from([3])),
            Reaction::from(Set::from([3]), Set::from([1]), Set::from([1])),
            Reaction::from(Set::from([2]), Set::default(), Set::from([4])),
        ]),
    );

    let res = system.clone().traces(1).unwrap();
    assert_eq!(res.len(), 1);

    let res = system.clone().traces(2).unwrap();
    let mut res = res.iter().map(|x| x.len()).collect::<Vec<_>>();
    res.sort();
    assert_eq!(res, [3, 4]);

    let res = system.clone().traces(3).unwrap();
    let mut res = res.iter().map(|x| x.len()).collect::<Vec<_>>();
    res.sort();
    assert_eq!(res, [3, 4, 5]);

    let res = system.clone().traces(4).unwrap();
    assert_eq!(res.len(), 3);

    let res = system.clone().traces(0).unwrap();
    assert_eq!(res.len(), 0);
}

#[test]
fn traces_empty_env() {
    let system = System::from(
        Arc::new(Environment::from([])),
        Set::from([1, 2]),
        Process::WaitEntity {
            repeat: 10,
            repeated_process: Arc::new(Process::EntitySet {
                entities:     Set::from([1, 2]),
                next_process: Arc::new(Process::Nill),
            }),
            next_process: Arc::new(Process::Nill),
        },
        Arc::new(vec![
            Reaction::from(Set::from([1]), Set::from([3]), Set::from([3])),
            Reaction::from(Set::from([3]), Set::from([1]), Set::from([1])),
            Reaction::from(Set::from([2]), Set::default(), Set::from([4])),
        ]),
    );

    let res = system.clone().traces(10).unwrap();
    assert_eq!(res.len(), 1);
    assert_eq!(res[0].len(), 10);
}

#[test]
fn conversion_reactions() {
    use super::element::IdState::*;

    let system = System::from(
        Arc::new(Environment::from([])),
        Set::from([1, 2]),
        Process::Nill,
        Arc::new(vec![
            Reaction::from(Set::from([2]), Set::from([1, 3]), Set::from([5])),
            Reaction::from(Set::from([1, 2]), Set::from([3]), Set::from([5])),
        ]),
    );
    let converted_system: PositiveSystem = system.into();
    let mut reactions = converted_system.reactions().clone();
    reactions.sort_by(|a, b| {
        a.products
            .cmp(&b.products)
            .then(a.reactants.cmp(&b.reactants))
    });

    assert_eq!(reactions, vec![
        PositiveReaction::from(
            PositiveSet::from([(2, Positive), (3, Negative)]),
            PositiveSet::from([(5, Positive)]),
        ),
        PositiveReaction::from(
            PositiveSet::from([(2, Negative)]),
            PositiveSet::from([(5, Negative)]),
        ),
        PositiveReaction::from(
            PositiveSet::from([(3, Positive)]),
            PositiveSet::from([(5, Negative)]),
        ),
    ]);
}

#[test]
fn conversion_entities() {
    use super::element::IdState::*;

    let system = System::from(
        Arc::new(Environment::from([])),
        Set::from([1, 2]),
        Process::Nill,
        Arc::new(vec![
            Reaction::from(Set::from([2]), Set::from([1, 3]), Set::from([5])),
            Reaction::from(Set::from([1, 2]), Set::from([3]), Set::from([5])),
        ]),
    );
    let converted_system: PositiveSystem = system.into();
    let entities = converted_system.available_entities().clone();

    assert_eq!(
        entities,
        PositiveSet::from([(1, Positive), (2, Positive), (5, Negative)])
    );
}

#[test]
fn slice_trace() {
    let mut translator = crate::translator::Translator::new();

    let mut tr = |a| translator.encode(a);

    let system = System::from(
        Arc::new(Environment::from([])),
        Set::from([tr("a"), tr("b")]),
        Process::EntitySet {
            entities:     Set::from([tr("c")]),
            next_process: Arc::new(Process::EntitySet {
                entities:     Set::from([]),
                next_process: Arc::new(Process::Nill),
            }),
        },
        Arc::new(vec![Reaction::from(
            Set::from([tr("a")]),
            Set::from([]),
            Set::from([tr("b")]),
        )]),
    );

    let system: PositiveSystem = system.into();

    let res_slice = system.slice_trace().unwrap();
    let res_run = system.run().unwrap();

    assert_eq!(res_slice.systems, res_run);
}

#[test]
fn slice_trace_2() {
    let mut translator = crate::translator::Translator::new();

    let mut tr = |a| translator.encode(a);

    let system = System::from(
        Arc::new(Environment::from([])),
        Set::from([tr("a"), tr("b")]),
        Process::EntitySet {
            entities:     Set::from([tr("c")]),
            next_process: Arc::new(Process::EntitySet {
                entities:     Set::from([]),
                next_process: Arc::new(Process::Nill),
            }),
        },
        Arc::new(vec![
            Reaction::from(
                Set::from([tr("a")]),
                Set::from([tr("b")]),
                Set::from([tr("b")]),
            ),
            Reaction::from(
                Set::from([tr("b")]),
                Set::from([tr("a")]),
                Set::from([tr("a")]),
            ),
        ]),
    );

    let system: PositiveSystem = system.into();

    let res_slice = system.slice_trace().unwrap();
    let res_run = system.run().unwrap();

    assert_eq!(res_slice.systems, res_run);
}

#[test]
fn from_boolean_positive() {
    let bn = BooleanNetwork {
        initial_state: BTreeMap::from([(1, true), (2, true), (3, true)]),
        update_rules:  BTreeMap::from([
            (
                1,
                BooleanFunction::Or(
                    Box::new(BooleanFunction::Not(Box::new(
                        BooleanFunction::Variable(1),
                    ))),
                    Box::new(BooleanFunction::And(
                        Box::new(BooleanFunction::Variable(2)),
                        Box::new(BooleanFunction::Variable(3)),
                    )),
                ),
            ),
            (2, BooleanFunction::Variable(3)),
            (
                3,
                BooleanFunction::Not(Box::new(BooleanFunction::Variable(2))),
            ),
        ]),
    };
    let rs: PositiveSystem = bn.into();

    assert_eq!(rs.reaction_rules.len(), 8);
}

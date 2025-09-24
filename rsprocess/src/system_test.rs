use super::set::PositiveSet;
use super::system::BasicSystem;

#[test]
fn one_transition() {
    use std::rc::Rc;

    use super::environment::Environment;
    use super::process::Process;
    use super::reaction::Reaction;
    use super::set::{BasicSet, Set};
    use super::system::{ExtensionsSystem, System};

    let system = System::from(
        Rc::new(Environment::default()),
        Set::from([1, 2]),
        Process::EntitySet {
            entities:     Set::from([]),
            next_process: Rc::new(Process::Nill),
        },
        Rc::new(vec![Reaction::from(
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
    use std::rc::Rc;

    use super::element::{IdState, PositiveType};
    use super::environment::PositiveEnvironment;
    use super::process::PositiveProcess;
    use super::reaction::PositiveReaction;
    use super::set::{BasicSet, PositiveSet};
    use super::system::{ExtensionsSystem, PositiveSystem};

    let system = PositiveSystem::from(
        Rc::new(PositiveEnvironment::default()),
        PositiveSet::from([
            (1, IdState::Positive),
            (2, IdState::Positive),
            (3, IdState::Negative),
        ]),
        PositiveProcess::WaitEntity {
            repeat: 2,
            repeated_process: Rc::new(PositiveProcess::EntitySet {
                entities:     PositiveSet::default(),
                next_process: Rc::new(PositiveProcess::Nill),
            }),
            next_process: Rc::new(PositiveProcess::Nill),
        },
        Rc::new(vec![
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
    use std::rc::Rc;

    use super::environment::Environment;
    use super::process::Process;
    use super::reaction::Reaction;
    use super::set::{BasicSet, Set};
    use super::system::{PositiveSystem, System};

    let system = System::from(
        Rc::new(Environment::default()),
        Set::from([1, 2]),
        Process::EntitySet {
            entities:     Set::from([]),
            next_process: Rc::new(Process::Nill),
        },
        Rc::new(vec![Reaction::from(
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
    use std::rc::Rc;

    use super::environment::Environment;
    use super::process::Process;
    use super::reaction::Reaction;
    use super::set::Set;
    use super::system::{ExtensionsSystem, System};

    let system = System::from(
        Rc::new(Environment::from([
            (100, Process::WaitEntity {
                repeat: 2,
                repeated_process: Rc::new(Process::EntitySet {
                    entities:     Set::from([1]),
                    next_process: Rc::new(Process::Nill),
                }),
                next_process: Rc::new(Process::Nill),
            }),
            (102, Process::WaitEntity {
                repeat: 3,
                repeated_process: Rc::new(Process::EntitySet {
                    entities:     Set::from([2]),
                    next_process: Rc::new(Process::Nill),
                }),
                next_process: Rc::new(Process::Nill),
            }),
            (103, Process::WaitEntity {
                repeat: 4,
                repeated_process: Rc::new(Process::EntitySet {
                    entities:     Set::from([3]),
                    next_process: Rc::new(Process::Nill),
                }),
                next_process: Rc::new(Process::Nill),
            }),
            (101, Process::Summation {
                children: vec![
                    Rc::new(Process::EntitySet {
                        entities:     Set::from([10]),
                        next_process: Rc::new(Process::RecursiveIdentifier {
                            identifier: 100,
                        }),
                    }),
                    Rc::new(Process::EntitySet {
                        entities:     Set::from([11]),
                        next_process: Rc::new(Process::RecursiveIdentifier {
                            identifier: 102,
                        }),
                    }),
                    Rc::new(Process::EntitySet {
                        entities:     Set::from([11]),
                        next_process: Rc::new(Process::RecursiveIdentifier {
                            identifier: 103,
                        }),
                    }),
                ],
            }),
        ])),
        Set::from([1, 2]),
        Process::RecursiveIdentifier { identifier: 101 },
        Rc::new(vec![
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
    use std::rc::Rc;

    use super::environment::Environment;
    use super::process::Process;
    use super::reaction::Reaction;
    use super::set::Set;
    use super::system::{ExtensionsSystem, System};

    let system = System::from(
        Rc::new(Environment::from([])),
        Set::from([1, 2]),
        Process::WaitEntity {
            repeat: 10,
            repeated_process: Rc::new(Process::EntitySet {
                entities:     Set::from([1, 2]),
                next_process: Rc::new(Process::Nill),
            }),
            next_process: Rc::new(Process::Nill),
        },
        Rc::new(vec![
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
    use std::rc::Rc;

    use super::element::IdState::*;
    use super::environment::Environment;
    use super::process::Process;
    use super::reaction::{PositiveReaction, Reaction};
    use super::set::Set;
    use super::system::{PositiveSystem, System};

    let system = System::from(
        Rc::new(Environment::from([])),
        Set::from([1, 2]),
        Process::Nill,
        Rc::new(vec![
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
    use std::rc::Rc;

    use super::element::IdState::*;
    use super::environment::Environment;
    use super::process::Process;
    use super::reaction::Reaction;
    use super::set::Set;
    use super::system::{PositiveSystem, System};

    let system = System::from(
        Rc::new(Environment::from([])),
        Set::from([1, 2]),
        Process::Nill,
        Rc::new(vec![
            Reaction::from(Set::from([2]), Set::from([1, 3]), Set::from([5])),
            Reaction::from(Set::from([1, 2]), Set::from([3]), Set::from([5])),
        ]),
    );
    let converted_system: PositiveSystem = system.into();
    let entities = converted_system.available_entities().clone();

    assert_eq!(
        entities,
        PositiveSet::from([
            (1, Positive),
            (2, Positive),
            (3, Negative),
            (5, Negative)
        ])
    );
}

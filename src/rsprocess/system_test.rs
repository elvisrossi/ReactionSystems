#[test]
fn one_transition() {
    use super::system::{System, ExtensionsSystem};
    use super::reaction::Reaction;
    use super::process::Process;
    use super::environment::Environment;
    use super::set::{Set, BasicSet};
    use std::rc::Rc;

    let system = System::from(
	Rc::new(Environment::default()),
	Set::from([1, 2]),
	Process::EntitySet { entities: Set::from([]),
			     next_process: Rc::new(Process::Nill) },
	Rc::new(vec![Reaction { reactants: Set::from([1]),
				inhibitors: Set::from([3]),
				products: Set::from([3]), }, ])
    );

    match system.one_transition() {
	Ok(Some((_, System { available_entities, .. }))) =>
	    assert!(available_entities.len() == 1 &&
		    available_entities.contains(&3)),
	_ => panic!()
    }
}

#[test]
fn one_transition_2() {
    use super::system::{PositiveSystem, ExtensionsSystem};
    use super::reaction::PositiveReaction;
    use super::process::PositiveProcess;
    use super::environment::PositiveEnvironment;
    use super::set::{PositiveSet, BasicSet};
    use super::element::{IdState, PositiveType};
    use std::rc::Rc;

    let system = PositiveSystem::from(
	Rc::new(PositiveEnvironment::default()),
	PositiveSet::from([(1, IdState::Positive),
			   (2, IdState::Positive),
			   (3, IdState::Negative)]),
	PositiveProcess::WaitEntity { repeat: 2,
				      repeated_process: Rc::new(
					  PositiveProcess::EntitySet {
					      entities: PositiveSet::default(),
					      next_process: Rc::new(
						  PositiveProcess::Nill) }
				      ),
				      next_process: Rc::new(PositiveProcess::Nill) },
	Rc::new(vec![
	    PositiveReaction {
		reactants: PositiveSet::from([(1, IdState::Positive),
					      (3, IdState::Negative)]),
		products: PositiveSet::from([(3, IdState::Positive)]), },
	    PositiveReaction {
		reactants: PositiveSet::from([(3, IdState::Positive)]),
		products: PositiveSet::from([(3, IdState::Negative)]), },
	    PositiveReaction {
		reactants: PositiveSet::from([(1, IdState::Negative)]),
		products: PositiveSet::from([(3, IdState::Negative)]), }, ])
    );

    let system = system.one_transition();

    let system =
	match system {
	    Ok(Some((_, system))) => {
		assert!(system.available_entities.len() == 1 &&
			system.available_entities.contains(
			    &PositiveType { id: 3,
					    state: IdState::Positive }));
		system
	    },
	    _ => panic!()
	};

    match system.one_transition() {
	Ok(Some((_, PositiveSystem { available_entities, .. }))) =>
	    assert!(available_entities.len() == 1 &&
		    available_entities.contains(
			&PositiveType { id: 3,
					state: IdState::Negative })),
	_ => panic!()
    }
}

#[test]
fn convertion() {
    use super::system::{System, PositiveSystem};
    use super::reaction::Reaction;
    use super::process::Process;
    use super::environment::Environment;
    use super::set::{Set, BasicSet};
    use std::rc::Rc;

    let system = System::from(
	Rc::new(Environment::default()),
	Set::from([1, 2]),
	Process::EntitySet { entities: Set::from([]),
			     next_process: Rc::new(Process::Nill) },
	Rc::new(vec![Reaction { reactants: Set::from([1]),
				inhibitors: Set::from([3]),
				products: Set::from([3]), }, ])
    );

    let system: PositiveSystem = system.into();

    assert_eq!(system.available_entities.len(), 2);
    assert_eq!(system.reaction_rules.len(), 3);
}

#[test]
fn traces_1() {
    use super::system::{System, ExtensionsSystem};
    use super::reaction::Reaction;
    use super::process::Process;
    use super::environment::Environment;
    use super::set::Set;
    use std::rc::Rc;

    let system = System {
	delta: Rc::new(Environment::from([
	    (100, Process::WaitEntity {
		repeat: 2,
		repeated_process: Rc::new(Process::EntitySet {
		    entities: Set::from([1]),
		    next_process: Rc::new(Process::Nill)
		}),
		next_process: Rc::new(Process::Nill) }),
	    (102, Process::WaitEntity {
		repeat: 3,
		repeated_process: Rc::new(Process::EntitySet {
		    entities: Set::from([2]),
		    next_process: Rc::new(Process::Nill) }),
		next_process: Rc::new(Process::Nill) }),
	    (103, Process::WaitEntity {
		repeat: 4,
		repeated_process: Rc::new(Process::EntitySet {
		    entities: Set::from([3]),
		    next_process: Rc::new(Process::Nill) }),
		next_process: Rc::new(Process::Nill) }),
	    (101, Process::Summation { children: vec![
		Rc::new(Process::EntitySet {
		    entities: Set::from([10]),
		    next_process: Rc::new(Process::RecursiveIdentifier {
			identifier: 100 }) }),
		Rc::new(Process::EntitySet {
		    entities: Set::from([11]),
		    next_process: Rc::new(Process::RecursiveIdentifier {
			identifier: 102 }) }),
		Rc::new(Process::EntitySet {
		    entities: Set::from([11]),
		    next_process: Rc::new(Process::RecursiveIdentifier {
			identifier: 103 }) })
	    ] }),
	])),
	available_entities: Set::from([1, 2]),
	context_process: Process::RecursiveIdentifier { identifier: 101 },
	reaction_rules:
	Rc::new(vec![Reaction { reactants: Set::from([1]),
				  inhibitors: Set::from([3]),
				  products: Set::from([3]), },
		     Reaction { reactants: Set::from([3]),
				  inhibitors: Set::from([1]),
				  products: Set::from([1]), },
		     Reaction { reactants: Set::from([2]),
				  inhibitors: Set::default(),
				  products: Set::from([4]), },
	])
    };

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
    use super::system::{System, ExtensionsSystem};
    use super::reaction::Reaction;
    use super::process::Process;
    use super::environment::Environment;
    use super::set::Set;
    use std::rc::Rc;

    let system = System {
	delta: Rc::new(Environment::from([])),
	available_entities: Set::from([1, 2]),
	context_process: Process::WaitEntity {
	    repeat: 10,
	    repeated_process: Rc::new(Process::EntitySet {
		entities: Set::from([1, 2]),
		next_process: Rc::new(Process::Nill) }),
	    next_process: Rc::new(Process::Nill) },
	reaction_rules:
	Rc::new(vec![Reaction { reactants: Set::from([1]),
				  inhibitors: Set::from([3]),
				  products: Set::from([3]), },
		     Reaction { reactants: Set::from([3]),
				  inhibitors: Set::from([1]),
				  products: Set::from([1]), },
		     Reaction { reactants: Set::from([2]),
				  inhibitors: Set::default(),
				  products: Set::from([4]), },
	])
    };

    let res = system.clone().traces(10).unwrap();
    assert_eq!(res.len(), 1);
    assert_eq!(res[0].len(), 10);
}

#[test]
fn traces_1() {
    use super::system::System;
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
				  inhibitors: Set::new(),
				  products: Set::from([4]), },
	])
    };

    // for (pos, trace) in res.iter().enumerate() {
    //	println!("trace {}:", pos);
    //	for (_, sy) in trace {
    //	    let ent = format!("{:?}", sy.available_entities);
    //	    let con = format!("{:?}", sy.context_process);
    //	    println!("\t({}, {})", ent, con);
    //	}
    // }

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
    use super::system::System;
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
				  inhibitors: Set::new(),
				  products: Set::from([4]), },
	])
    };

    let res = system.clone().traces(10).unwrap();
    assert_eq!(res.len(), 1);
    assert_eq!(res[0].len(), 10);
}

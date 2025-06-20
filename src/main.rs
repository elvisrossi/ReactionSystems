#![allow(unused_imports)]
mod rsprocess;
use lalrpop_util::lalrpop_mod;
use std::rc::Rc;
use rsprocess::translator::WithTranslator;
// use std::io;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    lalrpop_mod!(grammar, "/rsprocess/grammar.rs");

    let mut translator = rsprocess::translator::Translator::new();

    // let mut buffer = String::new();
    // let i = io::stdin();
    // i.read_line(&mut buffer).expect("Can't read stdin");

    // let result = grammar::SetParser::new().parse(&mut translator, &buffer).unwrap();


    // -------------------------------------------------------------------------
    // let reactants = grammar::SetParser::new().parse(&mut translator, "{a}").unwrap();
    // let inihibitors = grammar::SetParser::new().parse(&mut translator, "{c}").unwrap();
    // let products = grammar::SetParser::new().parse(&mut translator, "{a,c}").unwrap();

    // let process1 = rsprocess::structure::RSreaction::from(reactants, inihibitors, products);

    // let reactants = grammar::SetParser::new().parse(&mut translator, "{b}").unwrap();
    // let inihibitors = grammar::SetParser::new().parse(&mut translator, "{c}").unwrap();
    // let products = grammar::SetParser::new().parse(&mut translator, "{b,c}").unwrap();

    // let process2 = rsprocess::structure::RSreaction::from(reactants, inihibitors, products);

    // let current_state = grammar::SetParser::new().parse(&mut translator, "{b}").unwrap();

    // println!("{:?}", rsprocess::classical::compute_all(&current_state, vec![&process1, &process2]));


    // -------------------------------------------------------------------------
    // let env = grammar::EnvironmentParser::new().parse(&mut translator, "[x = {a}.{b}.x , y = ({a,c}.y + {b,c}.y)]").unwrap();
    // let process = grammar::ContextParser::new().parse(&mut translator, "[({a}.nil + x + y)]").unwrap();

    // println!("{:?}", rsprocess::transitions::unfold(&env, &process));

    // println!("--------------------");

    // println!("{:?}", env);


    // -------------------------------------------------------------------------
    // allTransitions(sys([],[a,b],[],[react([a],[c],[a]),react([b],[d],[b])]) , Moves).

    // let env = grammar::EnvironmentParser::new().parse(&mut translator, "[]").unwrap();
    // let process = grammar::ContextParser::new().parse(&mut translator, "[]").unwrap();

    // let sys = rsprocess::structure::RSsystem::from(Rc::new(*env),
    //						   rsprocess::structure::RSset::from(vec![translator.encode("a"),
    //											  translator.encode("b")]),
    //						   *process,
    //						   Rc::new(vec![
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec![translator.encode("a")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("c")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("a")])
    //						       ),
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec![translator.encode("b")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("d")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("b")])
    //						       )
    //						   ]));


    // println!("all_transitions: {:?}", rsprocess::transitions::all_transitions(&sys));
    // -------------------------------------------------------------------------
    // parse_ctx("[({a}.nil + {b}.nil)]",Ks) , allTransitions(sys([],[],Ks,[react([a],[c],[a]),react([b],[d],[b])]),Moves).
    // let env = grammar::EnvironmentParser::new().parse(&mut translator, "[]").unwrap();
    // let process = grammar::ContextParser::new().parse(&mut translator, "[({a}.nil + {b}.nil)]").unwrap();

    // let sys = rsprocess::structure::RSsystem::from(Rc::new(*env),
    //						   rsprocess::structure::RSset::from(vec![]),
    //						   *process,
    //						   Rc::new(vec![
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec![translator.encode("a")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("c")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("a")])
    //						       ),
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec![translator.encode("b")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("d")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("b")])
    //						       )
    //						   ]));


    // println!("all_transitions: {:?}", rsprocess::transitions::all_transitions(&sys));

    // -------------------------------------------------------------------------
    // parse_ctx("[({a}.nil + {b}.nil),({c}.nil + {d}.nil)]",Ks) , allTransitions(sys([],[],Ks,[react([a],[c],[a]),react([b],[d],[b])]),Moves).
    // let env = grammar::EnvironmentParser::new().parse(&mut translator, "[]").unwrap();
    // let process = grammar::ContextParser::new().parse(&mut translator, "[({a}.nil + {b}.nil),({c}.nil + {d}.nil)]").unwrap();

    // let sys = rsprocess::structure::RSsystem::from(Rc::new(*env),
    //						   rsprocess::structure::RSset::from(vec![]),
    //						   *process,
    //						   Rc::new(vec![
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec![translator.encode("a")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("c")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("a")])
    //						       ),
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec![translator.encode("b")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("d")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("b")])
    //						       )
    //						   ]));

    // let it = rsprocess::transitions::iterator_transitions(&sys)?;

    // for (n, i) in it.into_iter().enumerate() {
    //	println!("next i - {n}:  {:?}", i);
    //	println!("--------------------");
    // }

    // -------------------------------------------------------------------------
    // let env = grammar::EnvironmentParser::new().parse(&mut translator, "[]").unwrap();
    // let process = grammar::ContextParser::new().parse(&mut translator, "[({a}.nil + {b}.nil),({c}.nil + {d}.nil)]").unwrap();

    // let sys = rsprocess::structure::RSsystem::from(Rc::new(*env),
    //						   rsprocess::structure::RSset::from(vec![]),
    //						   *process,
    //						   Rc::new(vec![
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec![translator.encode("a")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("c")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("a")])
    //						       ),
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec![translator.encode("b")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("d")]),
    //							   rsprocess::structure::RSset::from(vec![translator.encode("b")])
    //						       )
    //						   ]));
    // println!("{:?}", rsprocess::transitions::run_separated(&sys));

    // -------------------------------------------------------------------------
    // list_to_assoc([x-pre([a, b], rec(x)), y-pre([d, e], rec(y))], Ass),
    // lollipop(sys(Ass, [a, b, c, d], [], [react([a], [c], [a]), react([d], [f], [d])]), Prefix, Loop).

    let env = grammar::EnvironmentParser::new().parse(&mut translator, "[x = {a,b}.x]").unwrap();
    let process = grammar::ContextParser::new().parse(&mut translator, "[]").unwrap();

    let sys = rsprocess::structure::RSsystem::from(Rc::new(*env),
						   rsprocess::structure::RSset::from(vec![translator.encode("a"),
											  translator.encode("b"),
											  translator.encode("c"),
											  translator.encode("d")]),
						   *process,
						   Rc::new(vec![
						       rsprocess::structure::RSreaction::from(
							   rsprocess::structure::RSset::from(vec![translator.encode("a")]),
							   rsprocess::structure::RSset::from(vec![translator.encode("c")]),
							   rsprocess::structure::RSset::from(vec![translator.encode("a")])
						       ),
						       rsprocess::structure::RSreaction::from(
							   rsprocess::structure::RSset::from(vec![translator.encode("d")]),
							   rsprocess::structure::RSset::from(vec![translator.encode("f")]),
							   rsprocess::structure::RSset::from(vec![translator.encode("d")])
						       )
						   ]));

    let res = rsprocess::perpetual::lollipops(sys.clone());

    println!("res:");
    for (prefix, hoop) in res {
	print!("prefix: ");
	for p in prefix {
	    print!("{}, ", WithTranslator::from_RSset(&translator, &p));
	}
	print!("\nhoop: ");
	for l in hoop {
	    print!("{}, ", WithTranslator::from_RSset(&translator, &l));
	}
	println!();
    }

    let res = rsprocess::perpetual::lollipops_only_loop(sys);

    println!("res:");
    for hoop in res {
	print!("hoop: ");
	for l in hoop {
	    print!("{}, ", WithTranslator::from_RSset(&translator, &l));
	}
	println!();
    }
    Ok(())
}

mod rsprocess;
use lalrpop_util::lalrpop_mod;
use rsprocess::translator::WithTranslator;
// use std::rc::Rc;
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

    let tmp = rsprocess::structure::RSreaction::from(
	rsprocess::structure::RSset::from(vec![translator.encode("a"), translator.encode("c")]),
	rsprocess::structure::RSset::from(vec![translator.encode("c")]),
	rsprocess::structure::RSset::from(vec![translator.encode("a")])
    );
    println!("{}", WithTranslator::from_RSreaction(&translator, &tmp));
    Ok(())
}

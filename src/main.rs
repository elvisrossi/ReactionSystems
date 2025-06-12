mod rsprocess;
use lalrpop_util::lalrpop_mod;
// use std::io;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    lalrpop_mod!(grammar, "/rsprocess/grammar.rs");

    // let mut buffer = String::new();
    // let i = io::stdin();
    // i.read_line(&mut buffer).expect("Can't read stdin");

    // let result = grammar::SetParser::new().parse(&buffer).unwrap();


    // -------------------------------------------------------------------------
    // let reactants = grammar::SetParser::new().parse("{a}").unwrap();
    // let inihibitors = grammar::SetParser::new().parse("{c}").unwrap();
    // let products = grammar::SetParser::new().parse("{a,c}").unwrap();

    // let process1 = rsprocess::structure::RSreaction::from(reactants, inihibitors, products);

    // let reactants = grammar::SetParser::new().parse("{b}").unwrap();
    // let inihibitors = grammar::SetParser::new().parse("{c}").unwrap();
    // let products = grammar::SetParser::new().parse("{b,c}").unwrap();

    // let process2 = rsprocess::structure::RSreaction::from(reactants, inihibitors, products);

    // let current_state = grammar::SetParser::new().parse("{b}").unwrap();

    // println!("{:?}", rsprocess::classical::compute_all(&current_state, vec![&process1, &process2]));


    // -------------------------------------------------------------------------
    // let env = grammar::EnvironmentParser::new().parse("[x = {a}.{b}.x , y = ({a,c}.y + {b,c}.y)]").unwrap();
    // let process = grammar::ContextParser::new().parse("[({a}.nil + x + y)]").unwrap();

    // println!("{:?}", rsprocess::transitions::unfold(&env, &process));

    // println!("--------------------");

    // println!("{:?}", env);


    // -------------------------------------------------------------------------
    // allTransitions(sys([],[a,b],[],[react([a],[c],[a]),react([b],[d],[b])]) , Moves).

    // let env = grammar::EnvironmentParser::new().parse("[]").unwrap();
    // let process = grammar::ContextParser::new().parse("[]").unwrap();

    // let sys = rsprocess::structure::RSsystem::from(*env,
    //						   rsprocess::structure::RSset::from(vec!["a", "b"]),
    //						   *process,
    //						   vec![
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec!["a"]),
    //							   rsprocess::structure::RSset::from(vec!["c"]),
    //							   rsprocess::structure::RSset::from(vec!["a"])
    //						       ),
    //						       rsprocess::structure::RSreaction::from(
    //							   rsprocess::structure::RSset::from(vec!["b"]),
    //							   rsprocess::structure::RSset::from(vec!["d"]),
    //							   rsprocess::structure::RSset::from(vec!["b"])
    //						       )
    //						   ]);


    // println!("all_transitions: {:?}", rsprocess::transitions::all_transitions(&sys));
    // -------------------------------------------------------------------------
    // parse_ctx("[({a}.nil + {b}.nil)]",Ks) , allTransitions(sys([],[],Ks,[react([a],[c],[a]),react([b],[d],[b])]),Moves).
    let env = grammar::EnvironmentParser::new().parse("[]").unwrap();
    let process = grammar::ContextParser::new().parse("[({a}.nil + {b}.nil)]").unwrap();

    let sys = rsprocess::structure::RSsystem::from(*env,
						   rsprocess::structure::RSset::from(vec![]),
						   *process,
						   vec![
						       rsprocess::structure::RSreaction::from(
							   rsprocess::structure::RSset::from(vec!["a"]),
							   rsprocess::structure::RSset::from(vec!["c"]),
							   rsprocess::structure::RSset::from(vec!["a"])
						       ),
						       rsprocess::structure::RSreaction::from(
							   rsprocess::structure::RSset::from(vec!["b"]),
							   rsprocess::structure::RSset::from(vec!["d"]),
							   rsprocess::structure::RSset::from(vec!["b"])
						       )
						   ]);


    println!("all_transitions: {:?}", rsprocess::transitions::all_transitions(&sys));

    // -------------------------------------------------------------------------
    // use std::rc::Rc;
    // let mut a = rsprocess::structure::RSChoices::from(vec![
    //	(Rc::new(rsprocess::structure::RSset::from(vec!["a"])),
    //	 Rc::new(rsprocess::structure::RSprocess::Nill)),
    // ]);

    // let b = rsprocess::structure::RSChoices::from(vec![
    //	(Rc::new(rsprocess::structure::RSset::from(vec!["b"])),
    //	 Rc::new(rsprocess::structure::RSprocess::Nill)),
    // ]);

    // a.shuffle(b);
    // println!("shuffle: {:?}", a);

    println!("--------------------");
    Ok(())
}

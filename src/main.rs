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
    let env = grammar::EnvironmentParser::new().parse("[x = {a}.{b}.x , y = ({a,c}.y + {b,c}.y)]").unwrap();
    let process = grammar::ContextParser::new().parse("[({a}.nil + x + y)]").unwrap();

    println!("{:?}", rsprocess::transitions::unfold(&env, &process));
    Ok(())
}

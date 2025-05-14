mod rsprocess;
use lalrpop_util::lalrpop_mod;
use std::io;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    lalrpop_mod!(grammar, "/rsprocess/grammar.rs");

    let mut buffer = String::new();
    let i = io::stdin();
    i.read_line(&mut buffer).expect("Can't read stdin");

    let result = grammar::BHMLParser::new().parse(&buffer).unwrap();
    println!("{:?}", result);

    Ok(())
}

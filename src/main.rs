fn main() {
    let now = std::time::Instant::now();

    // use reactionsystems::rsprocess::presets;
    // match presets::run("testing/first.system".into()) {
    //	Ok(_) => {},
    //	Err(e) => {println!("{e}")}
    // }

    use reactionsystems::grammar::AssertParser;
    use reactionsystems::rsprocess::translator::Translator;

    let contents = r#"label { if (substr("e", ('e').tostr)) then {return 'e'} else {return (("e").toel)} }"#;
    let mut translator = Translator::new();
    let tree = AssertParser::new().parse(&mut translator, contents).unwrap();

    println!("{tree}");
    match tree.typecheck() {
	Ok(_) => println!("ok"),
	Err(e) => println!("error: {e}")
    }
    match tree.execute(&reactionsystems::rsprocess::structure::RSlabel::new(), &mut translator) {
	Ok(val) => println!("ok: {val}"),
	Err(e) => println!("error: {e}")
    }

    println!("{} milliseconds elapsed", now.elapsed().as_millis());
}

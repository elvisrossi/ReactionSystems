use super::rsprocess::structure::{RSset, RSsystem};
use super::rsprocess::{graph, rsdot, translator};
use super::rsprocess::translator::Translator;
use super::rsprocess::{frequency, perpetual, statistics, transitions};
use super::rsprocess::serialize;

use std::env;
use std::fs::{self, File};
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

// grammar is defined in lib.rs, calling lalrpop_mod! twice, generates twice
// the code
use super::grammar;

fn read_file<T, F>(
    translator: &mut Translator,
    path: std::path::PathBuf,
    parser: F
)  -> std::io::Result<T>
where
    F: Fn(&mut Translator, String) -> T
{
    // we read the file with a buffer
    let f = fs::File::open(path.clone())?;
    let mut buf_reader = io::BufReader::new(f);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;

    // parse
    let result = parser(translator, contents);

    Ok(result)
}

fn save_file(
    contents: String,
    path: std::path::PathBuf
)  -> std::io::Result<()>
{
    let mut f = fs::File::create(path.clone())?;
    write!(f, "{contents}")?;
    Ok(())
}

fn parser_system(translator: &mut Translator, contents: String) -> RSsystem {
    grammar::SystemParser::new()
        .parse(translator, &contents)
        .unwrap()
}

fn parser_experiment(translator: &mut Translator, contents: String) -> (Vec<u32>, Vec<RSset>) {
    grammar::ExperimentParser::new()
        .parse(translator, &contents)
        .unwrap()
}



// equivalent main_do(stat) or main_do(stat, MissingE)
pub fn stats() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path, parser_system)?;

    // print statistics to screan
    println!("{}", statistics::of_RSsystem(&translator, &system));

    // to write to file:
    //// path = path.with_extension("stats");
    //// let mut f = std::fs::File::create(path)?;
    //// writeln!(f, "{}", statistics::of_RSsystem(translator, &result))?;

    Ok(())
}

// equivalent to main_do(target, E)
pub fn target() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path, parser_system)?;

    // the system needs to terminate to return
    let res = match transitions::target(&system) {
        Ok(o) => o,
        Err(e) => {
            println!("Error computing target: {e}");
            return Ok(());
        }
    };

    println!(
        "After {} steps we arrive at state:\n{}",
        res.0,
        translator::RSsetDisplay::from(&translator, &res.1)
    );

    Ok(())
}

// equivalent to main_do(run,Es)
pub fn run() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path, parser_system)?;

    // the system needs to terminate to return
    let res = match transitions::run_separated(&system) {
        Ok(o) => o,
        Err(e) => {
            println!("Error computing target: {e}");
            return Ok(());
        }
    };

    println!("The trace is composed by the set of entities:");

    for (e, _c, _t) in res {
        println!("{}", translator::RSsetDisplay::from(&translator, &e));
    }

    Ok(())
}

// equivalent to main_do(loop,Es)
pub fn hoop() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path, parser_system)?;

    // we retrieve the id for "x" and use it to find the corresponding loop
    let res =
	match perpetual::lollipops_only_loop_named(system,
						   translator.encode("x")) {
        Some(o) => o,
        None => {
            println!("No loop found.");
            return Ok(());
        }
    };

    println!("The loop is composed by the sets:");

    for e in res {
        println!("{}", translator::RSsetDisplay::from(&translator, &e));
    }

    Ok(())
}

// equivalent to main_do(freq, PairList)
pub fn freq() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path, parser_system)?;

    let res = match frequency::naive_frequency(&system) {
        Ok(f) => f,
        Err(e) => {
            println!("Error computing target: {e}");
            return Ok(());
        }
    };

    println!(
        "Frequency of encountered symbols:\n{}",
        translator::FrequencyDisplay::from(&translator, &res)
    );

    Ok(())
}

// equivalent to main_do(limitfreq, PairList)
pub fn limit_freq() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path, parser_system)?;

    path = env::current_dir()?;
    path = path.join("testing/first.experiment");
    let (_, sets) = read_file(&mut translator, path, parser_experiment)?;

    let res = match frequency::limit_frequency(&sets,
					       &system.reaction_rules,
					       &system.available_entities) {
	Some(e) => e,
	None => {return Ok(());}
    };

    println!(
        "Frequency of encountered symbols:\n{}",
        translator::FrequencyDisplay::from(&translator, &res)
    );

    Ok(())
}

// equivalent to main_do(fastfreq, PairList)
pub fn fast_freq() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path, parser_system)?;

    path = env::current_dir()?;
    path = path.join("testing/first.experiment");
    let (weights, sets) = read_file(&mut translator, path, parser_experiment)?;

    let res = match frequency::fast_frequency(&sets,
					      &system.reaction_rules,
					      &system.available_entities,
					      &weights) {
	Some(e) => e,
	None => {return Ok(());}
    };

    println!(
        "Frequency of encountered symbols:\n{}",
        translator::FrequencyDisplay::from(&translator, &res)
    );

    Ok(())
}

// equivalent to main_do(digraph, Arcs)
pub fn digraph() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path, parser_system)?;

    // the system needs to terminate to return
    let res = match graph::digraph(system) {
        Ok(o) => o,
        Err(e) => {
            println!("Error computing target: {e}");
            return Ok(());
        }
    };

    let rc_translator = Rc::new(translator);

    let old_res = Rc::new(res.clone());

    // map each value to the corresponding value we want to display
    let res = res.map(
	|id, node|
	graph::GraphMapNodesTy::from(graph::GraphMapNodes::Entities,
				     Rc::clone(&rc_translator)).get()(id, node)
	    + "; " +
	    &graph::GraphMapNodesTy::from(graph::GraphMapNodes::Context,
					  Rc::clone(&rc_translator)).get()(id, node),
	graph::GraphMapEdgesTy::from(graph::GraphMapEdges::EntitiesAdded,
					 Rc::clone(&rc_translator)).get()
    );

    println!("Generated graph in dot notation:\n{}",
	     rsdot::RSDot::with_attr_getters(
		 &res,
		 &[],
		 &graph::default_edge_formatter(Rc::clone(&old_res)),
		 &graph::default_node_formatter(Rc::clone(&old_res)),
	     )
    );

    Ok(())
}

pub fn graphml() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_file(&mut translator, path.clone(), parser_system)?;

    // the system needs to terminate to return
    let res = match graph::digraph(system) {
        Ok(o) => o,
        Err(e) => {
            println!("Error computing target: {e}");
            return Ok(());
        }
    };

    let rc_translator = Rc::new(translator);

    //let old_res = Rc::new(res.clone());

    // map each value to the corresponding value we want to display
    let res = res.map(
	|id, node|
	graph::GraphMapNodesTy::from(graph::GraphMapNodes::Entities,
				     Rc::clone(&rc_translator)).get()(id, node)
	    + "; " +
	    &graph::GraphMapNodesTy::from(graph::GraphMapNodes::Context,
					  Rc::clone(&rc_translator)).get()(id, node),
	graph::GraphMapEdgesTy::from(graph::GraphMapEdges::EntitiesAdded,
					 Rc::clone(&rc_translator)).get()
    );

    path.set_extension("graphml");

    use petgraph_graphml::GraphMl;
    let graphml = GraphMl::new(&res)
	.pretty_print(true)
	.export_node_weights_display()
	.export_edge_weights_display();
    println!("Generated graph in graphml notation:\n{graphml}");
    save_file(graphml.to_string(), path)?;

    Ok(())
}

// equivalent to main_do(advdigraph, Arcs)
pub fn adversarial() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/adversarial.system");
    let system = read_file(&mut translator, path, parser_system)?;

    // the system needs to terminate to return
    let res = match graph::digraph(system) {
        Ok(o) => o,
        Err(e) => {
            println!("Error computing target: {e}");
            return Ok(());
        }
    };

    let rc_translator = Rc::new(translator);

    let old_res = Rc::new(res.clone());

    // map each value to the corresponding value we want to display
    let res = res.map(
	|id, node|
	graph::GraphMapNodesTy::from(graph::GraphMapNodes::Entities,
				     Rc::clone(&rc_translator)).get()(id, node)
	    + "; " +
	    &graph::GraphMapNodesTy::from(graph::GraphMapNodes::Context,
					  Rc::clone(&rc_translator)).get()(id, node),
	graph::GraphMapEdgesTy::from(graph::GraphMapEdges::EntitiesAdded,
					 Rc::clone(&rc_translator)).get()
    );

    println!("Generated graph in dot notation:\n{}",
	     rsdot::RSDot::with_attr_getters(
		 &res,
		 &[],
		 &graph::default_edge_formatter(Rc::clone(&old_res)),
		 &graph::default_node_formatter(Rc::clone(&old_res)),
	     )
    );

    Ok(())
}

pub fn serialize() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/adversarial.system");
    let system = read_file(&mut translator, path, parser_system)?;

    // the system needs to terminate to return
    let graph = match graph::digraph(system) {
        Ok(o) => o,
        Err(e) => {
            println!("Error computing target: {e}");
            return Ok(());
        }
    };

    let mut path = env::current_dir()?;
    path = path.join("testing/adversarial.cbor");

    let file = File::create(path)?;

    serialize::sr(file, &graph, &translator).unwrap();

    Ok(())
}

pub fn deserialize() -> std::io::Result<()> {
    let mut path = env::current_dir()?;
    path = path.join("testing/adversarial.cbor");

    let file = File::open(path)?;

    let (graph, translator) = serialize::dsr(file).unwrap();

    let rc_translator = Rc::new(translator);

    let old_res = Rc::new(graph.clone());

    // map each value to the corresponding value we want to display
    let graph = graph.map(
	|id, node|
	graph::GraphMapNodesTy::from(graph::GraphMapNodes::Entities,
				     Rc::clone(&rc_translator)).get()(id, node)
	    + "; " +
	    &graph::GraphMapNodesTy::from(graph::GraphMapNodes::Context,
					  Rc::clone(&rc_translator)).get()(id, node),
	graph::GraphMapEdgesTy::from(graph::GraphMapEdges::EntitiesAdded,
					 Rc::clone(&rc_translator)).get()
    );

    println!("Generated graph in dot notation:\n{}",
	     rsdot::RSDot::with_attr_getters(
		 &graph,
		 &[],
		 &graph::default_edge_formatter(Rc::clone(&old_res)),
		 &graph::default_node_formatter(Rc::clone(&old_res)),
	     )
    );

    Ok(())
}

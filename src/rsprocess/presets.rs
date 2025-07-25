//! Module that holds useful presets for interacting with other modules.

use std::env;
use std::fmt::Display;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

use lalrpop_util::ParseError;

// grammar is defined in lib.rs, calling lalrpop_mod! twice, generates twice
// the code
use crate::grammar;

use super::structure::RSlabel;
use super::structure::{RSset, RSsystem};
use super::translator::Translator;
use super::*;

// -----------------------------------------------------------------------------
//                                 Structures
// -----------------------------------------------------------------------------

/// Describes how the result of some computation has to be saved.
pub struct SaveOptions {
    pub print: bool,
    pub save: Option<Vec<String>>,
}

impl SaveOptions {
    pub fn combine(&mut self, other: &mut Self) {
        self.print = self.print || other.print;
        match (self.save.is_some(), other.save.is_some()) {
            (false, false) | (true, false) => {}
            (false, true) => {
                self.save = other.save.to_owned();
            }
            (true, true) => {
                self.save
                    .as_mut()
                    .unwrap()
                    .append(other.save.as_mut().unwrap());
            }
        }
    }
    pub fn new() -> Self {
        SaveOptions {
            print: false,
            save: None,
        }
    }
}

impl Default for SaveOptions {
    fn default() -> Self {
        SaveOptions::new()
    }
}

// Describes display options for edges (RSlabels).
#[derive(Clone)]
pub enum EdgeDisplay {
    Separator(String),
    Display(graph::GraphMapEdges),
}

// Describes output options for a graph.
pub enum GraphSaveOptions {
    Dot {
        node_display: graph::NodeDisplay,
        edge_display: Vec<EdgeDisplay>,
	node_color: graph::NodeColor,
	edge_color: graph::EdgeColor,
        so: SaveOptions,
    },
    GraphML {
        node_display: graph::NodeDisplay,
        edge_display: Vec<EdgeDisplay>,
        so: SaveOptions,
    },
    Serialize {
        path: String,
    },
}

/// Describes the computation to apply to the input system or graph.
pub enum Instruction {
    Stats { so: SaveOptions },
    Target { so: SaveOptions },
    Run { so: SaveOptions },
    Loop { symbol: String, so: SaveOptions },
    Frequency { so: SaveOptions },
    LimitFrequency { experiment: String, so: SaveOptions },
    FastFrequency { experiment: String, so: SaveOptions },
    Digraph { gso: Vec<GraphSaveOptions> },
    Bisimilarity { system_b: String, so: SaveOptions }
}

/// Describes a system or a graph.
pub enum System {
    Deserialize { path: String },
    RSsystem { sys: RSsystem },
}

impl System {
    /// Deserialize the graph if applicable.
    pub fn compute(
	&self,
	translator: Translator
    ) -> Result<EvaluatedSystem, String>
    {
        match self {
            Self::RSsystem { sys } => Ok(EvaluatedSystem::System {
                sys: sys.to_owned(),
                translator,
            }),
            Self::Deserialize { path } => {
                let (graph, translator) = deserialize(path.into())?;
                Ok(EvaluatedSystem::Graph { graph, translator })
            }
        }
    }
}

pub enum EvaluatedSystem {
    Graph {
        graph: graph::RSgraph,
        translator: Translator,
    },
    System {
        sys: RSsystem,
        translator: Translator,
    },
}

impl EvaluatedSystem {
    pub fn get_translator(&mut self) -> &mut Translator {
	match self {
	    EvaluatedSystem::Graph { graph: _, translator } => {
		translator
	    },
	    EvaluatedSystem::System { sys: _, translator } => {
		translator
	    }
	}
    }
}

/// Holds all the computations to be done on the system
pub struct Instructions {
    pub system: System,
    pub instructions: Vec<Instruction>,
}

// -----------------------------------------------------------------------------
//                            IO Helper Functions
// -----------------------------------------------------------------------------

fn read_file<T, F>(
    translator: &mut Translator,
    path_string: String,
    parser: F
) -> Result<T, String>
where
    F: Fn(&mut Translator, String) -> Result<T, String>,
{
    // relative path
    let mut path = match env::current_dir() {
        Ok(p) => p,
        Err(_) => return Err("Error getting current directory.".into()),
    };
    path = path.join(path_string);

    // we read the file with a buffer
    let f = match fs::File::open(path) {
        Ok(f) => f,
        Err(_) => return Err("Error opening file.".into()),
    };
    let mut buf_reader = io::BufReader::new(f);
    let mut contents = String::new();
    match buf_reader.read_to_string(&mut contents) {
        Ok(_) => {}
        Err(_) => return Err("Error reading file.".into()),
    }

    // parse
    let result = parser(translator, contents)?;

    Ok(result)
}

fn reformat_error<T, S>(
    e: ParseError<usize, T, &'static str>
) -> Result<S, String>
where
    T: Display,
{
    match e {
        ParseError::ExtraToken { token: (l, t, r) } => Err(format!(
            "Unexpected token \"{t}\" \
		 between positions {l} and {r}."
        )),
        ParseError::UnrecognizedEof {
            location: _,
            expected: _,
        } => Err("End of file encountered while parsing.".into()),
        ParseError::InvalidToken { location } => {
            Err(format!("Invalid token at position {location}."))
        }
        ParseError::UnrecognizedToken {
            token: (l, t, r),
            expected,
        } => {
	    let mut err = format!(
		"Unrecognized token \"{t}\" \
		 between positions {l} and {r}."
	    );

	    // Temporary debug.
	    err.push_str("Expected: ");
	    let mut it = expected.iter().peekable();
	    while let Some(s) = it.next() {
		err.push('(');
		err.push_str(s);
		err.push(')');
		if it.peek().is_some() {
		    err.push(',');
		    err.push(' ');		    
		}

	    }
	    Err(err)
	},
        ParseError::User { error } => Err(error.to_string()),
    }
}

fn parser_experiment(
    translator: &mut Translator,
    contents: String,
) -> Result<(Vec<u32>, Vec<RSset>), String> {
    match grammar::ExperimentParser::new().parse(translator, &contents) {
        Ok(sys) => Ok(sys),
        Err(e) => reformat_error(e),
    }
}

fn parser_instructions(
    translator: &mut Translator,
    contents: String,
) -> Result<Instructions, String> {
    match grammar::RunParser::new().parse(translator, &contents) {
        Ok(sys) => Ok(sys),
        Err(e) => reformat_error(e),
    }
}

fn save_file(contents: &String, path_string: String) -> Result<(), String> {
    // relative path
    let mut path = match env::current_dir() {
        Ok(p) => p,
        Err(_) => return Err("Error getting current directory.".into()),
    };
    path = path.join(path_string);

    let mut f = match fs::File::create(&path) {
        Ok(f) => f,
        Err(_) =>
	    return Err(
		format!("Error creating file {}.", path.to_str().unwrap())
	    ),
    };
    match write!(f, "{contents}") {
        Ok(_) => {}
        Err(_) => return Err("Error writing to file.".into()),
    }
    Ok(())
}

// -----------------------------------------------------------------------------
//                                  main_do
// -----------------------------------------------------------------------------

/// Prints statistics of the system.
/// Equivalent main_do(stat) or main_do(stat, MissingE)
pub fn stats(system: &EvaluatedSystem) -> Result<String, String> {
    match system {
        EvaluatedSystem::System { sys, translator } =>
	    Ok(statistics::of_RSsystem(translator, sys)),
        EvaluatedSystem::Graph { graph, translator } => {
            let Some(sys) = graph.node_weights().next() else {
                return Err("No node found in graph".into());
            };
            Ok(statistics::of_RSsystem(translator, sys))
        }
    }
}

/// Prints a final set of entities in a terminating Reaction System.
/// The system needs to terminate to return.
/// Equivalent to main_do(target, E)
pub fn target(system: &EvaluatedSystem) -> Result<String, String> {
    let (res, translator) = match system {
        EvaluatedSystem::System { sys, translator } =>
	    (transitions::target(sys)?, translator),
        EvaluatedSystem::Graph { graph, translator } => {
            let Some(sys) = graph.node_weights().next() else {
                return Err("No node found in graph".into());
            };
            (transitions::target(sys)?, translator)
        }
    };
    Ok(format!(
        "After {} steps we arrive at state:\n{}",
        res.0,
        translator::RSsetDisplay::from(translator, &res.1)
    ))
}

/// Finds the list of traversed states in a (deterministic) terminating
/// reaction.
/// The system needs to terminate to return.
/// equivalent to main_do(run,Es)
pub fn traversed(system: &EvaluatedSystem) -> Result<String, String> {
    let (res, translator) = match system {
        EvaluatedSystem::System { sys, translator } => {
            (transitions::run_separated(sys)?, translator)
        }
        EvaluatedSystem::Graph { graph, translator } => {
            let Some(sys) = graph.node_weights().next() else {
                return Err("No node found in graph".into());
            };
            (transitions::run_separated(sys)?, translator)
        }
    };

    let mut output = String::new();

    output.push_str("The trace is composed by the set of entities:");
    for (e, _c, _t) in res {
        output.push_str(&format!(
            "{}",
            translator::RSsetDisplay::from(translator, &e)
        ));
    }
    Ok(output)
}

/// Finds the looping list of states in a reaction system with a perpetual
/// context. IMPORTANT: for loops, we assume Delta defines the process constant
/// x = Q.x and the context process is x .
/// equivalent to main_do(loop,Es)
pub fn hoop(
    system: &EvaluatedSystem,
    symbol: String
) -> Result<String, String> {
    let (res, translator) = match system {
        EvaluatedSystem::System { sys, translator } => (sys, translator),
        EvaluatedSystem::Graph { graph, translator } => {
            let Some(sys) = graph.node_weights().next() else {
                return Err("No node found in graph".into());
            };
            (sys, translator)
        }
    };
    // we retrieve the id for "x" and use it to find the corresponding loop
    let Some(id) = translator.encode_not_mut(&symbol) else {
        return Err(format!("Symbol {symbol} not found"));
    };
    let res = match perpetual::lollipops_only_loop_named(res, id) {
        Some(o) => o,
        None => {
            return Err("No loop found.".into());
        }
    };

    let mut output = String::new();

    output.push_str("The loop is composed by the sets:");
    for e in res {
        output.push_str(&format!(
            "{}",
            translator::RSsetDisplay::from(translator, &e)
        ));
    }

    Ok(output)
}

/// Finds the frequency of each entity in the traversed states for a
/// (deterministic) terminating Reaction System.
/// equivalent to main_do(freq, PairList)
pub fn freq(system: &EvaluatedSystem) -> Result<String, String> {
    let (sys, translator) = match system {
        EvaluatedSystem::System { sys, translator } => (sys, translator),
        EvaluatedSystem::Graph { graph, translator } => {
            let Some(sys) = graph.node_weights().next() else {
                return Err("No node found in graph".into());
            };
            (sys, translator)
        }
    };

    let res = frequency::naive_frequency(sys)?;

    Ok(format!(
        "Frequency of encountered symbols:\n{}",
        translator::FrequencyDisplay::from(translator, &res)
    ))
}

/// Finds the frequency of each entity in the limit loop of a nonterminating
/// Reaction System whose context has the form Q1 ... Q1.Q2 ... Q2 ... Qn ...
/// equivalent to main_do(limitfreq, PairList)
pub fn limit_freq(
    system: &mut EvaluatedSystem,
    experiment: String
) -> Result<String, String> {
    let (sys, translator): (&RSsystem, &mut Translator) = match system {
        EvaluatedSystem::System { sys, translator } => (sys, translator),
        EvaluatedSystem::Graph { graph, translator } => {
            let Some(sys) = graph.node_weights().next() else {
                return Err("No node found in graph".into());
            };
            (sys, translator)
        }
    };

    let (_, sets) = read_file(translator, experiment, parser_experiment)?;

    let res =
	match frequency::limit_frequency(
	    &sets,
	    &sys.reaction_rules,
	    &sys.available_entities)
    {
        Some(e) => e,
        None => {
            return Err("Error calculating frequency.".into());
        }
    };

    Ok(format!(
        "Frequency of encountered symbols:\n{}",
        translator::FrequencyDisplay::from(translator, &res)
    ))
}

/// Finds the frequency of each entity in the traversed loops of a terminating
/// reaction system whose context has the form
/// Q1 ... Q1.Q2 ... Q2 ... Qn ... Qn.nil and each Qi is repeated Wi times
/// read from a corresponding file.
/// equivalent to main_do(fastfreq, PairList)
pub fn fast_freq(
    system: &mut EvaluatedSystem,
    experiment: String
) -> Result<String, String>
{
    let (sys, translator): (&RSsystem, &mut Translator) = match system {
        EvaluatedSystem::System { sys, translator } => (sys, translator),
        EvaluatedSystem::Graph { graph, translator } => {
            let Some(sys) = graph.node_weights().next() else {
                return Err("No node found in graph".into());
            };
            (sys, translator)
        }
    };

    let (weights, sets) = read_file(translator, experiment, parser_experiment)?;

    let res = match frequency::fast_frequency(
        &sets,
        &sys.reaction_rules,
        &sys.available_entities,
        &weights,
    ) {
        Some(e) => e,
        None => {
            return Err("Error calculating frequency.".into());
        }
    };

    Ok(format!(
        "Frequency of encountered symbols:\n{}",
        translator::FrequencyDisplay::from(translator, &res)
    ))
}

/// Computes the LTS.
/// equivalent to main_do(digraph, Arcs) or to main_do(advdigraph, Arcs)
pub fn digraph(system: &mut EvaluatedSystem) -> Result<(), String> {
    if let EvaluatedSystem::System { sys, translator } = system {
	*system =
            EvaluatedSystem::Graph {
		graph: graph::digraph(sys.clone())?,
		translator: translator.to_owned(),
            };
    }
    Ok(())
}

/// Computes bisimularity of two provided systems
pub fn bisimilar(
    system_a: &mut EvaluatedSystem,
    system_b: String
) -> Result<String, String>
{
    let system_b = read_file(system_a.get_translator(),
				 system_b.to_string(),
				 parser_instructions)?;

    let mut system_b =
	match system_b.system.compute(system_a.get_translator().clone())? {
	    EvaluatedSystem::System { sys, translator } =>
		EvaluatedSystem::System { sys, translator },
	    EvaluatedSystem::Graph { graph, translator } => {
		if translator != *system_a.get_translator() {
		    return Err("Bisimilarity not implemented for systems with \
				different encodings. Serialize the systems \
				with the same translator.".into());
		}
		EvaluatedSystem::Graph { graph, translator }
	    }
	};

    digraph(system_a)?;
    digraph(&mut system_b)?;

    // since we ran digraph on both they have to be graphs
    match (system_a, &system_b) {
	(EvaluatedSystem::Graph { graph: a, translator: _ },
	 EvaluatedSystem::Graph { graph: b, translator: _ }) => {
	    // needed because rust cant see that we want to use &graph::RSgraph
	    // and not &mut graph::RSgraph
	    let a: &graph::RSgraph = a;
	    let b: &graph::RSgraph = b;
	    Ok(format!(
		"{}",
		// super::bisimilarity::bisimilarity_kanellakis_smolka(&a, &b)
		// super::bisimilarity::bisimilarity_paige_tarjan_ignore_labels(&a, &b)
		super::bisimilarity::bisimilarity_paige_tarjan(&a, &b)
	    ))
	},
	_ => { unreachable!() }
    }
}





// -----------------------------------------------------------------------------
//                              Output Functions
// -----------------------------------------------------------------------------

type GraphMapEdgesFnTy<'a> =
    dyn Fn(petgraph::prelude::EdgeIndex, &'a RSlabel) -> String + 'a;
fn generate_edge_printing_fn<'a>(
    edge_display: &[EdgeDisplay],
    translator: Rc<Translator>,
) -> Box<GraphMapEdgesFnTy<'a>> {
    // The type cannot be aliased since rust doesnt like generics.
    // We are iterating over the edge_display and constructing a function
    // (accumulator) that prints out our formatted nodes. So at each step we
    // call the previous function and add the next string or function.
    let edge_display = edge_display.iter().map(
	|e| {
	    match e {
		EdgeDisplay::Display(d) => {
		    d.clone()
		},
		EdgeDisplay::Separator(s) => {
		    graph::GraphMapEdges::String { string: s.clone() }
		}
	    }
	}
    ).collect::<Vec<_>>();

    let gmet = graph::GraphMapEdgesTy::from(
	(edge_display, Rc::clone(&translator))
    );

    gmet.generate()
}

/// Writes the specified graph to a file in .dot format.
pub fn dot(
    system: &EvaluatedSystem,
    node_display: graph::NodeDisplay,
    edge_display: Vec<EdgeDisplay>,
    node_color: graph::NodeColor,
    edge_color: graph::EdgeColor
) -> Result<String, String> {
    match system {
        EvaluatedSystem::System {
            sys: _,
            translator: _,
        } => Err("Supplied system is not a graph".into()),
        EvaluatedSystem::Graph { graph, translator } => {
            let rc_translator = Rc::new(translator.clone());
            let modified_graph = graph.map(
                node_display.generate(Rc::clone(&rc_translator), graph),
                generate_edge_printing_fn(&edge_display,
					   Rc::clone(&rc_translator)),
            );

            let graph = Rc::new(graph.to_owned());

            let node_formatter =
	     	node_color.generate(Rc::clone(&graph),
				    translator.encode_not_mut("*"));
	    let edge_formatter =
		edge_color.generate(Rc::clone(&graph));

            let dot = rsdot::RSDot::with_attr_getters(
                &modified_graph,
                &[],
		&edge_formatter,
                &node_formatter,
            );

            Ok(format!("{dot}"))
        }
    }
}

/// Writes the specified graph to a file in .graphml format.
pub fn graphml(
    system: &EvaluatedSystem,
    node_display: graph::NodeDisplay,
    edge_display: Vec<EdgeDisplay>,
) -> Result<String, String> {
    match system {
        EvaluatedSystem::System {
            sys: _,
            translator: _,
        } => Err("Supplied system is not a graph".into()),
        EvaluatedSystem::Graph { graph, translator } => {
            let rc_translator = Rc::new(translator.to_owned());

            // map each value to the corresponding value we want to display
            let modified_graph = graph.map(
                node_display.generate(Rc::clone(&rc_translator),
				      graph),
                generate_edge_printing_fn(&edge_display,
					   rc_translator),
            );

            use petgraph_graphml::GraphMl;
            let graphml = GraphMl::new(&modified_graph)
                .pretty_print(true)
                .export_node_weights_display()
                .export_edge_weights_display();

            Ok(format!("{graphml}"))
        }
    }
}

/// Writes the specified graph, translator tuple to file.
/// N.B. graph size in memory might be much larger after serialization and
/// deserialization.
pub fn serialize(system: &EvaluatedSystem, path: String) -> Result<(), String> {
    match system {
        EvaluatedSystem::System {
            sys: _,
            translator: _,
        } => Err("Supplied system is not a graph".into()),
        EvaluatedSystem::Graph { graph, translator } => {
            // relative path
            let mut path = std::path::PathBuf::from(path);
            path.set_extension("cbor");

            let f = match fs::File::create(&path) {
                Ok(f) => f,
                Err(_) => return Err(format!("Error creating file {}.",
					     path.to_str().unwrap())),
            };

            match serialize::ser(f, graph, translator) {
                Ok(_) => Ok(()),
                Err(_) => Err("Error during serialization.".into()),
            }
        }
    }
}

/// Reads the specified serialized system from a file.
/// N.B. graph size in memory might be much larger after serialization and
/// deserialization
pub fn deserialize(
    input_path: String,
) -> Result<(graph::RSgraph, Translator), String>
{
    // relative path
    let mut path = match env::current_dir() {
        Ok(p) => p,
        Err(_) => return Err("Error getting current directory.".into()),
    };
    path = path.join(input_path);
    path.set_extension("cbor");

    let f = match fs::File::open(&path) {
        Ok(f) => f,
        Err(_) => return Err(format!("Error opening file {}.",
				     path.to_str().unwrap())),
    };

    match serialize::de(f) {
        Ok(a) => Ok(a),
        Err(_) => Err("Error during deserialization.".into()),
    }
}

//------------------------------------------------------------------------------
//                         Interpreting Instructions
//------------------------------------------------------------------------------

macro_rules! save_options {
    ($assignment: expr, $so: ident) => {
        let SaveOptions { print, save } = $so;
        let output = $assignment;
        if print {
            println!("{output}");
        }
        if let Some(save) = save {
            for file in save {
                save_file(&output, file)?;
            }
        }
    };
}

fn execute(
    instruction: Instruction,
    system: &mut EvaluatedSystem
) -> Result<(), String> {
    match instruction {
        Instruction::Stats { so } => {
            save_options!(stats(system)?, so);
        }
        Instruction::Target { so } => {
            save_options!(target(system)?, so);
        }
        Instruction::Run { so } => {
            save_options!(traversed(system)?, so);
        }
        Instruction::Loop { symbol, so } => {
            save_options!(hoop(system, symbol)?, so);
        }
        Instruction::Frequency { so } => {
            save_options!(freq(system)?, so);
        }
        Instruction::LimitFrequency { experiment, so } => {
            save_options!(limit_freq(system, experiment)?, so);
        }
        Instruction::FastFrequency { experiment, so } => {
            save_options!(fast_freq(system, experiment)?, so);
        }
        Instruction::Digraph { gso } => {
            for save in gso {
                digraph(system)?;
                match save {
                    GraphSaveOptions::Dot {
                        node_display: nd,
                        edge_display: ed,
			node_color: nc,
			edge_color: ec,
                        so,
                    } => {
                        save_options!(dot(system, nd, ed, nc, ec)?, so);
                    }
                    GraphSaveOptions::GraphML {
                        node_display: nd,
                        edge_display: ed,
                        so,
                    } => {
                        save_options!(graphml(system, nd, ed)?, so);
                    }
                    GraphSaveOptions::Serialize { path } => {
                        serialize(system, path)?;
                    }
                }
            }
        }
	Instruction::Bisimilarity { system_b, so } => {
	    save_options!(bisimilar(system, system_b)?, so);
	}
    }
    Ok(())
}

/// Interprets file at supplied path, then executes the code specified as
/// instructions inside the file.
pub fn run(path: String) -> Result<(), String> {
    let mut translator = Translator::new();

    let Instructions {
        system,
        instructions,
    } = read_file(&mut translator, path, parser_instructions)?;

    let mut system = system.compute(translator)?;

    for instr in instructions {
        execute(instr, &mut system)?;
    }

    Ok(())
}

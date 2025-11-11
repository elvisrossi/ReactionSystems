//! Module that holds useful presets for interacting with other modules.
use std::io::prelude::*;
use std::sync::Arc;
use std::{env, fs, io};

use petgraph::Graph;
use rsprocess::set::Set;
use rsprocess::system::ExtensionsSystem;
use rsprocess::translator::Translator;
use rsprocess::*;

use crate::data::MapEdges;

// -----------------------------------------------------------------------------

pub trait FileParsers {
    fn parse_experiment(
        translator: &mut Translator,
        contents: String,
    ) -> Result<(Vec<u32>, Vec<Set>), String>;

    fn parse_instructions(
        translator: &mut Translator,
        contents: String,
    ) -> Result<Instructions, String>;
}

// -----------------------------------------------------------------------------
//                                 Structures
// -----------------------------------------------------------------------------

/// Describes how the result of some computation has to be saved.
pub struct SaveOptions {
    pub print: bool,
    pub save:  Option<Vec<String>>,
}

impl SaveOptions {
    pub fn combine(&mut self, other: &mut Self) {
        self.print = self.print || other.print;
        match (self.save.is_some(), other.save.is_some()) {
            | (false, false) | (true, false) => {},
            | (false, true) => {
                self.save = other.save.to_owned();
            },
            | (true, true) => {
                self.save
                    .as_mut()
                    .unwrap()
                    .append(other.save.as_mut().unwrap());
            },
        }
    }
    pub fn new() -> Self {
        SaveOptions {
            print: false,
            save:  None,
        }
    }
}

impl Default for SaveOptions {
    fn default() -> Self {
        SaveOptions::new()
    }
}

// Describes output options for a graph.
pub enum GraphSaveOptions {
    Dot {
        node_display: graph::NodeDisplay,
        edge_display: graph::EdgeDisplay,
        node_color: graph::NodeColor,
        edge_color: graph::EdgeColor,
        so: SaveOptions,
    },
    GraphML {
        node_display: graph::NodeDisplay,
        edge_display: graph::EdgeDisplay,
        so: SaveOptions,
    },
    Serialize {
        path: String,
    },
}

/// Describes the computation to apply to the input system or graph.
pub enum Instruction {
    Stats {
        so: SaveOptions,
    },
    Target {
        so:    SaveOptions,
        limit: Option<usize>,
    },
    Run {
        so:    SaveOptions,
        limit: Option<usize>,
    },
    Loop {
        symbol: String,
        so:     SaveOptions,
    },
    Frequency {
        so: SaveOptions,
    },
    LimitFrequency {
        experiment: String,
        so: SaveOptions,
    },
    FastFrequency {
        experiment: String,
        so: SaveOptions,
    },
    Digraph {
        group: Option<Box<assert::grouping::Assert>>,
        gso:   Vec<GraphSaveOptions>,
    },
    Bisimilarity {
        system_b: String,
        edge_relabeler: Box<assert::relabel::Assert>,
        so: SaveOptions,
    },
}

/// Describes a system or a graph.
#[derive(Clone)]
pub enum System {
    Deserialize { path: String },
    System { sys: system::System },
}

impl System {
    /// Deserialize the graph if applicable.
    pub fn compute(
        self,
        translator: Translator,
    ) -> Result<EvaluatedSystem, String> {
        match self {
            | Self::System { sys } =>
                Ok(EvaluatedSystem::from_sys(sys, translator)),
            | Self::Deserialize { path } => {
                let (graph, translator) = deserialize(path)?;
                Ok(EvaluatedSystem::from_graph(graph, translator))
            },
        }
    }
}

#[derive(Clone)]
pub struct EvaluatedSystem {
    sys: Option<system::System>,
    graph: Option<graph::SystemGraph>,
    positive: Option<system::PositiveSystem>,
    translator: Translator,
}

impl EvaluatedSystem {
    pub fn get_translator(&mut self) -> &mut Translator {
        &mut self.translator
    }

    pub fn from_graph(
        graph: graph::SystemGraph,
        translator: Translator,
    ) -> Self {
        Self {
            sys: None,
            graph: Some(graph),
            positive: None,
            translator,
        }
    }

    pub fn from_sys(sys: system::System, translator: Translator) -> Self {
        Self {
            sys: Some(sys),
            graph: None,
            positive: None,
            translator,
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
    parser: F,
) -> Result<T, String>
where
    F: Fn(&mut Translator, String) -> Result<T, String>,
{
    // relative path
    let mut path = match env::current_dir() {
        | Ok(p) => p,
        | Err(e) => {
            return Err(format!("Error getting current directory: {e}"));
        },
    };
    path = path.join(path_string);

    // we read the file with a buffer
    let f = match fs::File::open(path) {
        | Ok(f) => f,
        | Err(e) => return Err(format!("Error opening file: {e}.")),
    };
    let mut buf_reader = io::BufReader::new(f);
    let mut contents = String::new();
    match buf_reader.read_to_string(&mut contents) {
        | Ok(_) => {},
        | Err(e) => return Err(format!("Error reading file: {e}")),
    }

    // parse
    let result = parser(translator, contents)?;

    Ok(result)
}

fn save_file(contents: &String, path_string: String) -> Result<(), String> {
    // relative path
    let mut path = match env::current_dir() {
        | Ok(p) => p,
        | Err(_) => return Err("Error getting current directory.".into()),
    };
    path = path.join(path_string);

    let mut f = match fs::File::create(&path) {
        | Ok(f) => f,
        | Err(_) => {
            return Err(format!(
                "Error creating file {}.",
                path.to_str().unwrap()
            ));
        },
    };
    match write!(f, "{contents}") {
        | Ok(_) => {},
        | Err(_) => return Err("Error writing to file.".into()),
    }
    Ok(())
}

// -----------------------------------------------------------------------------
//                                  main_do
// -----------------------------------------------------------------------------

/// Prints statistics of the system.
/// Equivalent main_do(stat) or main_do(stat, MissingE)
pub fn stats(system: &EvaluatedSystem) -> Result<String, String> {
    if let Some(sys) = &system.sys {
        Ok(sys.statistics(&system.translator))
    } else if let Some(graph) = &system.graph {
        let Some(sys) = graph.node_weights().next() else {
            return Err("No node found in graph.".into());
        };
        Ok(sys.statistics(&system.translator))
    } else {
        Err("Statistics not available for supplied system.".into())
    }
}

/// Prints a final set of entities in a terminating Reaction System.
/// The system needs to terminate to return.
/// Equivalent to main_do(target, E)
pub fn target(
    system: &EvaluatedSystem,
    limit: Option<usize>,
) -> Result<String, String> {
    if let Some(sys) = &system.sys {
        let res = if let Some(limit) = limit {
            sys.target_limit(limit)?
        } else {
            sys.target()?
        };
        Ok(format!(
            "After {} steps we arrive at state:\n{}",
            res.0,
            translator::Formatter::from(&system.translator, &res.1)
        ))
    } else if let Some(graph) = &system.graph {
        let Some(sys) = graph.node_weights().next() else {
            return Err("No node found in graph.".into());
        };
        let res = if let Some(limit) = limit {
            sys.target_limit(limit)?
        } else {
            sys.target()?
        };
        Ok(format!(
            "After {} steps we arrive at state:\n{}",
            res.0,
            translator::Formatter::from(&system.translator, &res.1)
        ))
    } else if let Some(positive) = &system.positive {
        let res = if let Some(limit) = limit {
            positive.target_limit(limit)?
        } else {
            positive.target()?
        };
        Ok(format!(
            "After {} steps we arrive at state:\n{}",
            res.0,
            translator::Formatter::from(&system.translator, &res.1)
        ))
    } else {
        Err("Target not available for supplied system.".into())
    }
}

/// Finds the list of traversed states in a (deterministic) terminating
/// reaction.
/// The system needs to terminate to return.
/// equivalent to main_do(run,Es)
pub fn traversed(
    system: &EvaluatedSystem,
    limit: Option<usize>,
) -> Result<String, String> {
    let mut output = String::new();

    if let Some(sys) = &system.sys {
        let res = if let Some(limit) = limit {
            sys.run_separated_limit(limit)?
        } else {
            sys.run_separated()?
        };

        output.push_str("The trace is composed by the set of entities: ");
        for (e, _c, _t) in res {
            output.push_str(&format!(
                "{}",
                translator::Formatter::from(&system.translator, &e)
            ));
        }
        Ok(output)
    } else if let Some(graph) = &system.graph {
        let Some(sys) = graph.node_weights().next() else {
            return Err("No node found in graph.".into());
        };
        let res = if let Some(limit) = limit {
            sys.run_separated_limit(limit)?
        } else {
            sys.run_separated()?
        };

        output.push_str("The trace is composed by the set of entities: ");
        for (e, _c, _t) in res {
            output.push_str(&format!(
                "{}",
                translator::Formatter::from(&system.translator, &e)
            ));
        }
        Ok(output)
    } else if let Some(positive) = &system.positive {
        let res = if let Some(limit) = limit {
            positive.run_separated_limit(limit)?
        } else {
            positive.run_separated()?
        };

        output.push_str("The trace is composed by the set of entities: ");
        for (e, _c, _t) in res {
            output.push_str(&format!(
                "{}",
                translator::Formatter::from(&system.translator, &e)
            ));
        }
        Ok(output)
    } else {
        Err("Run not available for supplied system.".into())
    }
}

/// Finds the looping list of states in a reaction system with a perpetual
/// context. IMPORTANT: for loops, we assume Delta defines the process constant
/// x = Q.x and the context process is x .
/// equivalent to main_do(loop,Es)
pub fn hoop(
    system: &EvaluatedSystem,
    symbol: String,
) -> Result<String, String> {
    use system::LoopSystem;
    // we retrieve the id for the input symbol, error if not found.
    let Some(id) = system.translator.encode_not_mut(&symbol) else {
        return Err(format!("Symbol {symbol} not found."));
    };
    let mut output = String::new();

    if let Some(sys) = &system.sys {
        let res = match sys.lollipops_only_loop_named(id) {
            | Some(o) => o,
            | None => {
                return Err("No loop found.".into());
            },
        };
        output.push_str("The loop is composed by the sets: ");
        for e in res {
            output.push_str(&format!(
                "{}",
                translator::Formatter::from(&system.translator, &e)
            ));
        }

        Ok(output)
    } else if let Some(graph) = &system.graph {
        let Some(sys) = graph.node_weights().next() else {
            return Err("No node found in graph.".into());
        };
        let res = match sys.lollipops_only_loop_named(id) {
            | Some(o) => o,
            | None => {
                return Err("No loop found.".into());
            },
        };

        output.push_str("The loop is composed by the sets: ");
        for e in res {
            output.push_str(&format!(
                "{}",
                translator::Formatter::from(&system.translator, &e)
            ));
        }

        Ok(output)
    } else if let Some(positive) = &system.positive {
        let res = match positive.lollipops_only_loop_named(id) {
            | Some(o) => o,
            | None => {
                return Err("No loop found.".into());
            },
        };

        output.push_str("The loop is composed by the sets: ");
        for e in res {
            output.push_str(&format!(
                "{}",
                translator::Formatter::from(&system.translator, &e)
            ));
        }

        Ok(output)
    } else {
        Err("Loop not available for supplied system.".into())
    }
}

/// Finds the frequency of each entity in the traversed states for a
/// (deterministic) terminating Reaction System.
/// equivalent to main_do(freq, PairList)
pub fn freq(system: &EvaluatedSystem) -> Result<String, String> {
    use frequency::BasicFrequency;

    if let Some(sys) = &system.sys {
        let res = frequency::Frequency::naive_frequency(sys)?;

        Ok(format!(
            "Frequency of encountered symbols:\n{}",
            translator::Formatter::from(&system.translator, &res)
        ))
    } else if let Some(graph) = &system.graph {
        let Some(sys) = graph.node_weights().next() else {
            return Err("No node found in graph.".into());
        };
        let res = frequency::Frequency::naive_frequency(sys)?;

        Ok(format!(
            "Frequency of encountered symbols:\n{}",
            translator::Formatter::from(&system.translator, &res)
        ))
    } else if let Some(positive) = &system.positive {
        let res = frequency::PositiveFrequency::naive_frequency(positive)?;

        Ok(format!(
            "Frequency of encountered symbols:\n{}",
            translator::Formatter::from(&system.translator, &res)
        ))
    } else {
        Err("Frequency not available for supplied system.".into())
    }
}

/// Finds the frequency of each entity in the limit loop of a nonterminating
/// Reaction System whose context has the form Q1 ... Q1.Q2 ... Q2 ... Qn ...
/// equivalent to main_do(limitfreq, PairList)
pub fn limit_freq<F>(
    system: &mut EvaluatedSystem,
    experiment: String,
    parser_experiment: F,
) -> Result<String, String>
where
    F: Fn(&mut Translator, String) -> Result<(Vec<u32>, Vec<Set>), String>,
{
    use frequency::BasicFrequency;

    let (_, sets) =
        read_file(&mut system.translator, experiment, parser_experiment)?;

    if let Some(sys) = &system.sys {
        let res = match frequency::Frequency::limit_frequency(
            &sets,
            &sys.reaction_rules,
            &sys.available_entities,
        ) {
            | Some(e) => e,
            | None => {
                return Err("Error calculating frequency.".into());
            },
        };

        Ok(format!(
            "Frequency of encountered symbols:\n{}",
            translator::Formatter::from(&system.translator, &res)
        ))
    } else if let Some(graph) = &system.graph {
        let Some(sys) = graph.node_weights().next() else {
            return Err("No node found in graph.".into());
        };

        let res = match frequency::Frequency::limit_frequency(
            &sets,
            &sys.reaction_rules,
            &sys.available_entities,
        ) {
            | Some(e) => e,
            | None => {
                return Err("Error calculating frequency.".into());
            },
        };

        Ok(format!(
            "Frequency of encountered symbols:\n{}",
            translator::Formatter::from(&system.translator, &res)
        ))
    } else if let Some(_positive) = &system.positive {
        todo!()
        // let res = match frequency::PositiveFrequency::limit_frequency(
        //     &sets,
        //     &positive.reaction_rules,
        //     &positive.available_entities,
        // ) {
        //     | Some(e) => e,
        //     | None => {
        //         return Err("Error calculating frequency.".into());
        //     },
        // };

        // Ok(format!(
        //     "Frequency of encountered symbols:\n{}",
        //     translator::Formatter::from(&system.translator, &res)
        // ))
    } else {
        Err("LimitFrequency not available for supplied system.".into())
    }
}

/// Finds the frequency of each entity in the traversed loops of a terminating
/// reaction system whose context has the form
/// Q1 ... Q1.Q2 ... Q2 ... Qn ... Qn.nil and each Qi is repeated Wi times
/// read from a corresponding file.
/// equivalent to main_do(fastfreq, PairList)
pub fn fast_freq<F>(
    system: &mut EvaluatedSystem,
    experiment: String,
    parser_experiment: F,
) -> Result<String, String>
where
    F: Fn(&mut Translator, String) -> Result<(Vec<u32>, Vec<Set>), String>,
{
    use frequency::BasicFrequency;

    let (weights, sets) =
        read_file(&mut system.translator, experiment, parser_experiment)?;

    if let Some(sys) = &system.sys {
        let res = match frequency::Frequency::fast_frequency(
            &sets,
            &sys.reaction_rules,
            &sys.available_entities,
            &weights,
        ) {
            | Some(e) => e,
            | None => {
                return Err("Error calculating frequency.".into());
            },
        };

        Ok(format!(
            "Frequency of encountered symbols:\n{}",
            translator::Formatter::from(&system.translator, &res)
        ))
    } else if let Some(graph) = &system.graph {
        let Some(sys) = graph.node_weights().next() else {
            return Err("No node found in graph".into());
        };

        let res = match frequency::Frequency::fast_frequency(
            &sets,
            &sys.reaction_rules,
            &sys.available_entities,
            &weights,
        ) {
            | Some(e) => e,
            | None => {
                return Err("Error calculating frequency.".into());
            },
        };

        Ok(format!(
            "Frequency of encountered symbols:\n{}",
            translator::Formatter::from(&system.translator, &res)
        ))
    } else if let Some(_positive) = &system.positive {
        todo!()
    } else {
        Err("FastFrequency not available for supplied system.".into())
    }
}

/// Computes the LTS.
/// equivalent to main_do(digraph, Arcs) or to main_do(advdigraph, Arcs)
pub fn digraph(system: &mut EvaluatedSystem) -> Result<(), String> {
    if let (Some(sys), true) = (&system.sys, system.graph.is_none()) {
        let graph = sys.digraph()?;
        system.graph = Some(graph);
    } else if let (Some(positive), true) =
        (&system.positive, system.graph.is_none())
    {
        let _graph = positive.digraph()?;
        todo!()
    }
    Ok(())
}

pub fn grouping(
    system: &mut EvaluatedSystem,
    group: &assert::grouping::Assert,
) -> Result<(), String> {
    if system.graph.is_some() {
        super::data::grouping(
            system.graph.as_mut().unwrap(),
            group,
            &mut system.translator,
        )
    } else {
        Err("Grouping can be done only on graphs.".into())
    }
}

/// Computes bisimularity of two provided systems
pub fn bisimilar<F>(
    system_a: &mut EvaluatedSystem,
    edge_relabeler: &assert::relabel::Assert,
    system_b: String,
    parser_instructions: F,
) -> Result<String, String>
where
    F: Fn(&mut Translator, String) -> Result<Instructions, String>,
{
    use assert::relabel::AssertReturnValue;

    let system_b = read_file(
        &mut system_a.translator,
        system_b.to_string(),
        parser_instructions,
    )?;

    let mut system_b =
        system_b.system.compute(system_a.get_translator().clone())?;

    if system_b.translator != system_a.translator {
        return Err("Bisimilarity not implemented for systems with different \
                    encodings. Serialize the systems with the same translator."
            .into());
    }

    digraph(system_a)?;
    digraph(&mut system_b)?;

    // since we ran digraph on both they have to have valid graphs

    let a: Graph<system::System, AssertReturnValue> = system_a
        .graph
        .as_ref()
        .unwrap()
        .map_edges(edge_relabeler, &mut system_a.translator)?;
    let b: Graph<system::System, AssertReturnValue> =
        system_b
            .graph
            .unwrap()
            .map_edges(edge_relabeler, &mut system_b.translator)?;
    Ok(format!(
        "{}",
        // bisimilarity::bisimilarity_kanellakis_smolka::bisimilarity(&&a, &&b)
        // bisimilarity::bisimilarity_paige_tarjan::bisimilarity_ignore_labels(&&a, &&b)
        bisimilarity::bisimilarity_paige_tarjan::bisimilarity(&&a, &&b)
    ))
}

// -----------------------------------------------------------------------------
//                              Output Functions
// -----------------------------------------------------------------------------

/// Writes the specified graph to a file in .dot format.
pub fn dot(
    system: &EvaluatedSystem,
    node_display: graph::NodeDisplay,
    edge_display: graph::EdgeDisplay,
    node_color: graph::NodeColor,
    edge_color: graph::EdgeColor,
) -> Result<String, String> {
    if let Some(graph) = &system.graph {
        let rc_translator = Arc::new(system.translator.clone());
        let modified_graph = graph.map(
            node_display.generate(Arc::clone(&rc_translator), graph),
            edge_display.generate(Arc::clone(&rc_translator), graph),
        );

        let graph = Arc::new(graph.to_owned());

        let node_formatter = node_color
            .generate(Arc::clone(&graph), system.translator.encode_not_mut("*"));
        let edge_formatter = edge_color.generate(Arc::clone(&graph));

        let dot = dot::Dot::with_attr_getters(
            &modified_graph,
            &[],
            &edge_formatter,
            &node_formatter,
        );

        Ok(format!("{dot}"))
    } else {
        Err("Supplied system is not a graph".into())
    }
}

/// Writes the specified graph to a file in .graphml format.
pub fn graphml(
    system: &EvaluatedSystem,
    node_display: graph::NodeDisplay,
    edge_display: graph::EdgeDisplay,
) -> Result<String, String> {
    if let Some(graph) = &system.graph {
        let rc_translator = Arc::new(system.translator.to_owned());

        // map each value to the corresponding value we want to display
        let modified_graph = graph.map(
            node_display.generate(Arc::clone(&rc_translator), graph),
            edge_display.generate(rc_translator, graph),
        );

        use petgraph_graphml::GraphMl;
        let graphml = GraphMl::new(&modified_graph)
            .pretty_print(true)
            .export_node_weights_display()
            .export_edge_weights_display();

        Ok(format!("{graphml}"))
    } else {
        Err("Supplied system is not a graph".into())
    }
}

/// Writes the specified graph and translator to file.
/// N.B. graph size in memory might be much larger after serialization and
/// deserialization.
pub fn serialize(system: &EvaluatedSystem, path: String) -> Result<(), String> {
    if let Some(graph) = &system.graph {
        // relative path
        let mut path = std::path::PathBuf::from(path);
        path.set_extension("cbor");

        let f = match fs::File::create(&path) {
            | Ok(f) => f,
            | Err(_) => {
                return Err(format!(
                    "Error creating file {}.",
                    path.to_str().unwrap()
                ));
            },
        };

        match serialize::ser(f, graph, &system.translator) {
            | Ok(_) => Ok(()),
            | Err(_) => Err("Error during serialization.".into()),
        }
    } else {
        Err("Supplied system is not a graph".into())
    }
}

/// Reads the specified serialized system from a file.
/// N.B. graph size in memory might be much larger after serialization and
/// deserialization
pub fn deserialize(
    input_path: String,
) -> Result<(graph::SystemGraph, Translator), String> {
    // relative path
    let mut path = match env::current_dir() {
        | Ok(p) => p,
        | Err(_) => return Err("Error getting current directory.".into()),
    };
    path = path.join(input_path);
    path.set_extension("cbor");

    let f = match fs::File::open(&path) {
        | Ok(f) => f,
        | Err(_) => {
            return Err(format!(
                "Error opening file {}.",
                path.to_str().unwrap()
            ));
        },
    };

    match serialize::de(f) {
        | Ok(a) => Ok(a),
        | Err(_) => Err("Error during deserialization.".into()),
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

fn execute<P: FileParsers>(
    instruction: Instruction,
    system: &mut EvaluatedSystem,
) -> Result<(), String> {
    match instruction {
        | Instruction::Stats { so } => {
            save_options!(stats(system)?, so);
        },
        | Instruction::Target { so, limit } => {
            save_options!(target(system, limit)?, so);
        },
        | Instruction::Run { so, limit } => {
            save_options!(traversed(system, limit)?, so);
        },
        | Instruction::Loop { symbol, so } => {
            save_options!(hoop(system, symbol)?, so);
        },
        | Instruction::Frequency { so } => {
            save_options!(freq(system)?, so);
        },
        | Instruction::LimitFrequency { experiment, so } => {
            save_options!(
                limit_freq(system, experiment, P::parse_experiment)?,
                so
            );
        },
        | Instruction::FastFrequency { experiment, so } => {
            save_options!(
                fast_freq(system, experiment, P::parse_experiment)?,
                so
            );
        },
        | Instruction::Digraph { group, gso } => {
            digraph(system)?;
            let mut graph = system.clone();

            if let Some(group) = group {
                group.typecheck()?;
                grouping(&mut graph, &group)?;
            }
            for save in gso {
                match save {
                    | GraphSaveOptions::Dot {
                        node_display: nd,
                        edge_display: ed,
                        node_color: nc,
                        edge_color: ec,
                        so,
                    } => {
                        save_options!(dot(&graph, nd, ed, nc, ec)?, so);
                    },
                    | GraphSaveOptions::GraphML {
                        node_display: nd,
                        edge_display: ed,
                        so,
                    } => {
                        save_options!(graphml(&graph, nd, ed)?, so);
                    },
                    | GraphSaveOptions::Serialize { path } => {
                        serialize(&graph, path)?;
                    },
                }
            }
        },
        | Instruction::Bisimilarity {
            system_b,
            edge_relabeler,
            so,
        } => {
            edge_relabeler.typecheck()?;
            save_options!(
                bisimilar(
                    system,
                    &edge_relabeler,
                    system_b,
                    P::parse_instructions
                )?,
                so
            );
        },
    }
    Ok(())
}

/// Interprets file at supplied path, then executes the code specified as
/// instructions inside the file.
pub fn run<P: FileParsers>(path: String) -> Result<(), String> {
    let mut translator = Translator::new();

    let Instructions {
        system,
        instructions,
    } = read_file(&mut translator, path, P::parse_instructions)?;

    let mut system = system.compute(translator)?;

    for instr in instructions {
        execute::<P>(instr, &mut system)?;
    }

    Ok(())
}

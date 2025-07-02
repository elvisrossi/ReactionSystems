#![allow(dead_code)]

use crate::rsprocess::structure::RSsystem;
use crate::rsprocess::translator;
use crate::rsprocess::translator::Translator;
use crate::rsprocess::{frequency, perpetual, statistics, transitions};

use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;

// grammar is defined in main.rs, calling lalrpop_mod! twice, generates twice
// the code
use super::grammar;

fn read_system(translator: &mut Translator, path: std::path::PathBuf) -> std::io::Result<RSsystem> {
    // we read the file with a buffer
    let f = fs::File::open(path.clone())?;
    let mut buf_reader = io::BufReader::new(f);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;

    // parse
    let result = grammar::SystemParser::new()
        .parse(translator, &contents)
        .unwrap();

    Ok(result)
}

// equivalent main_do(stat) or main_do(stat, MissingE)
pub fn stats() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_system(&mut translator, path)?;

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
    let system = read_system(&mut translator, path)?;

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
    let system = read_system(&mut translator, path)?;

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
    let system = read_system(&mut translator, path)?;

    // we retrieve the id for "x" and use it to find the corresponding loop
    let res = match perpetual::lollipops_only_loop_named(system, translator.encode("x")) {
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

pub fn freq() -> std::io::Result<()> {
    let mut translator = Translator::new();

    let mut path = env::current_dir()?;
    // file to read is inside testing/
    path = path.join("testing/first.system");
    let system = read_system(&mut translator, path)?;

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

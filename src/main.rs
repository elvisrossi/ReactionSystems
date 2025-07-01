#![allow(unused_imports)]
mod rsprocess;
mod examples;

use std::{hash::Hash, rc::Rc};
use rsprocess::translator::WithTranslator;

lalrpop_util::lalrpop_mod!(#[allow(clippy::uninlined_format_args)] pub grammar, "/rsprocess/grammar.rs");

fn main() -> std::io::Result<()> {

    // let now = std::time::Instant::now();
    // std::thread::sleep(std::time::Duration::new(2, 0));
    // println!("{}", now.elapsed().as_micros());

    examples::stats()?;

    examples::hoop()?;

    examples::target()?;

    examples::run()?;

    Ok(())
}

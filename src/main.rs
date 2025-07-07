mod examples;
mod rsprocess;

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::uninlined_format_args)] pub grammar, // name of module
    "/rsprocess/grammar.rs" // location of parser
);

fn main() -> std::io::Result<()> {
    // let now = std::time::Instant::now();
    // std::thread::sleep(std::time::Duration::new(2, 0));
    // println!("{}", now.elapsed().as_micros());

    // examples::stats()?;

    // examples::freq()?;

    // examples::hoop()?;

    // examples::target()?;

    // examples::run()?;

    // examples::limit_freq()?;

    // examples::fast_freq()?;

    examples::digraph()?;

    examples::adversarial()?;

    Ok(())
}

fn main() {
    let now = std::time::Instant::now();

    use reactionsystems::rsprocess::presets;
    match presets::run("testing/first.system".into()) {
	Ok(()) => {},
	Err(e) => {println!("{e}")}
    }

    println!("{} milliseconds elapsed", now.elapsed().as_millis());
}

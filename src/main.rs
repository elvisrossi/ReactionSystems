fn main() {
    let now = std::time::Instant::now();

    use reactionsystems::rsprocess::presets;
    match presets::run("testing/medical.system".into()) {
	Ok(()) => {},
	Err(e) => {println!("{e}")}
    }

    println!("{} milliseconds elapsed", now.elapsed().as_millis());
}

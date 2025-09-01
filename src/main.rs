fn main() {
    use reactionsystems::rsprocess::presets;

    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    input = input.trim().into();

    let now = std::time::Instant::now();

    match presets::run(input) {
	Ok(()) => {},
	Err(e) => {println!("{e}")}
    }

    println!("{} milliseconds elapsed", now.elapsed().as_millis());
}

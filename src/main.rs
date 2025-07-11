use reactionsystems::rsprocess::presets;

fn main() {
    // let now = std::time::Instant::now();
    // println!("{}", now.elapsed().as_micros());

    match presets::run("testing/first.system".into()) {
	Ok(_) => {},
	Err(e) => {println!("{e}")}
    }
}

mod helper;

fn main() {
    use execution::presets;

    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    input = input.trim().into();

    let now = std::time::Instant::now();

    match presets::run::<helper::Parsers>(input) {
        | Ok(()) => {},
        | Err(e) => println!("{e}"),
    }

    let now = now.elapsed();
    println!(
        "{}.{:0>3} milliseconds elapsed",
        now.as_millis(),
        now.as_micros() - now.as_millis() * 1000
    );
}

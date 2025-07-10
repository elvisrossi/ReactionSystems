fn main() -> std::io::Result<()> {
    // let now = std::time::Instant::now();
    // println!("{}", now.elapsed().as_micros());

    reactionsystems::examples::graphml()?;

    Ok(())
}

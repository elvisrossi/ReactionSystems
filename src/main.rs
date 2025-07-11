fn main() {
    // let now = std::time::Instant::now();
    // println!("{}", now.elapsed().as_micros());

    let (g, t) = reactionsystems::rsprocess::presets::digraph("testing/first.system".into()).unwrap();

    reactionsystems::rsprocess::presets::dot(&g, &t, "testing/first.dot".into()).unwrap();
}

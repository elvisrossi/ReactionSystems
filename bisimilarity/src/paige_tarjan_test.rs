use super::paige_tarjan::{bisimilarity, bisimilarity_ignore_labels};

#[test]
fn identity_paige_tarjan() {
    use petgraph::Graph;
    let mut graph_a = Graph::new();

    let node_a_1 = graph_a.add_node(1);
    let node_a_2 = graph_a.add_node(2);
    graph_a.add_edge(node_a_1, node_a_2, 1);

    assert!(bisimilarity(&&graph_a, &&graph_a));
    assert!(bisimilarity_ignore_labels(&&graph_a, &&graph_a))
}

#[test]
fn identity_paige_tarjan_2() {
    use petgraph::Graph;
    let mut graph_a = Graph::new();

    let node_a_1 = graph_a.add_node(1);
    let node_a_2 = graph_a.add_node(2);
    graph_a.add_edge(node_a_1, node_a_2, 1);
    let node_a_3 = graph_a.add_node(3);
    graph_a.add_edge(node_a_1, node_a_3, 1);
    let node_a_6 = graph_a.add_node(6);
    graph_a.add_edge(node_a_2, node_a_6, 2);
    let node_a_4 = graph_a.add_node(4);
    graph_a.add_edge(node_a_3, node_a_4, 2);
    let node_a_7 = graph_a.add_node(7);
    graph_a.add_edge(node_a_6, node_a_7, 2);
    let node_a_5 = graph_a.add_node(5);
    graph_a.add_edge(node_a_4, node_a_5, 2);
    let node_a_8 = graph_a.add_node(8);
    graph_a.add_edge(node_a_7, node_a_8, 3);
    graph_a.add_edge(node_a_8, node_a_7, 3);
    graph_a.add_edge(node_a_8, node_a_8, 3);

    assert!(bisimilarity(&&graph_a, &&graph_a));
    assert!(bisimilarity_ignore_labels(&&graph_a, &&graph_a))
}

#[test]
fn identity_different_weights_paige_tarjan() {
    use petgraph::Graph;
    let mut graph_a = Graph::new();

    let node_a_1 = graph_a.add_node(1);
    let node_a_2 = graph_a.add_node(2);
    graph_a.add_edge(node_a_1, node_a_2, 1);

    let mut graph_b = Graph::new();

    let node_b_1 = graph_b.add_node(1);
    let node_b_2 = graph_b.add_node(2);
    graph_b.add_edge(node_b_1, node_b_2, 2);

    assert!(!bisimilarity(&&graph_a, &&graph_b));
    assert!(bisimilarity_ignore_labels(&&graph_a, &&graph_b))
}

#[test]
fn not_bisimilar_paige_tarjan() {
    use petgraph::Graph;
    let mut graph_a = Graph::new();

    let node_a_1 = graph_a.add_node(1);
    let node_a_2 = graph_a.add_node(2);
    graph_a.add_edge(node_a_1, node_a_2, 1);

    let mut graph_b = Graph::new();

    let node_b_1 = graph_b.add_node(1);
    let node_b_2 = graph_b.add_node(2);
    graph_b.add_edge(node_b_1, node_b_2, 1);
    let node_b_3 = graph_b.add_node(3);
    graph_b.add_edge(node_b_1, node_b_3, 2);

    assert!(!bisimilarity(&&graph_a, &&graph_b));
    assert!(bisimilarity_ignore_labels(&&graph_a, &&graph_b))
}

#[test]
fn not_bisimilar_paige_tarjan_2() {
    use petgraph::Graph;
    let mut graph_a = Graph::new();

    let node_a_1 = graph_a.add_node(1);
    let node_a_2 = graph_a.add_node(2);
    graph_a.add_edge(node_a_1, node_a_2, 1);
    let node_a_3 = graph_a.add_node(3);
    graph_a.add_edge(node_a_1, node_a_3, 1);
    let node_a_6 = graph_a.add_node(6);
    graph_a.add_edge(node_a_2, node_a_6, 2);
    let node_a_4 = graph_a.add_node(4);
    graph_a.add_edge(node_a_3, node_a_4, 2);
    let node_a_7 = graph_a.add_node(7);
    graph_a.add_edge(node_a_6, node_a_7, 2);
    let node_a_5 = graph_a.add_node(5);
    graph_a.add_edge(node_a_4, node_a_5, 2);
    let node_a_8 = graph_a.add_node(8);
    graph_a.add_edge(node_a_7, node_a_8, 3);
    graph_a.add_edge(node_a_8, node_a_7, 3);
    graph_a.add_edge(node_a_8, node_a_8, 3);

    let mut graph_b = Graph::new();

    let node_b_1 = graph_b.add_node(1);
    let node_b_2 = graph_b.add_node(2);
    graph_b.add_edge(node_b_1, node_b_2, 1);
    let node_b_3 = graph_b.add_node(3);
    graph_b.add_edge(node_b_2, node_b_3, 2);
    let node_b_4 = graph_b.add_node(4);
    graph_b.add_edge(node_b_3, node_b_4, 2);

    assert!(!bisimilarity(&&graph_a, &&graph_b));
    assert!(!bisimilarity_ignore_labels(&&graph_a, &&graph_b))
}

#[test]
fn not_bisimilar_paige_tarjan_3() {
    use petgraph::Graph;
    let mut graph_b = Graph::new();

    let node_b_1 = graph_b.add_node(1);
    let node_b_2 = graph_b.add_node(2);
    graph_b.add_edge(node_b_1, node_b_2, 1);
    let node_b_3 = graph_b.add_node(3);
    graph_b.add_edge(node_b_2, node_b_3, 2);
    let node_b_4 = graph_b.add_node(4);
    graph_b.add_edge(node_b_3, node_b_4, 2);

    let mut graph_c = Graph::new();

    let node_c_1 = graph_c.add_node(1);
    let node_c_2 = graph_c.add_node(2);
    graph_c.add_edge(node_c_1, node_c_2, 1);
    let node_c_3 = graph_c.add_node(3);
    graph_c.add_edge(node_c_1, node_c_3, 2);
    graph_c.add_edge(node_c_2, node_c_3, 2);

    assert!(!bisimilarity(&&graph_b, &&graph_c));
    assert!(!bisimilarity_ignore_labels(&&graph_b, &&graph_c))
}

#[test]
fn bisimilar_paige_tarjan() {
    use petgraph::Graph;
    let mut graph_b = Graph::new();

    let node_b_1 = graph_b.add_node(1);
    let node_b_2 = graph_b.add_node(2);
    graph_b.add_edge(node_b_1, node_b_2, 1);
    let node_b_3 = graph_b.add_node(3);
    graph_b.add_edge(node_b_2, node_b_3, 2);
    let node_b_4 = graph_b.add_node(4);
    graph_b.add_edge(node_b_3, node_b_4, 2);

    let mut graph_c = Graph::new();

    let node_c_1 = graph_c.add_node(1);
    let node_c_2 = graph_c.add_node(2);
    graph_c.add_edge(node_c_1, node_c_2, 1);
    let node_c_3 = graph_c.add_node(3);
    graph_c.add_edge(node_c_2, node_c_3, 2);
    let node_c_4 = graph_c.add_node(4);
    graph_c.add_edge(node_c_3, node_c_4, 2);
    let node_c_5 = graph_c.add_node(5);
    graph_c.add_edge(node_c_1, node_c_5, 1);
    let node_c_6 = graph_c.add_node(6);
    graph_c.add_edge(node_c_5, node_c_6, 2);
    let node_c_7 = graph_c.add_node(7);
    graph_c.add_edge(node_c_6, node_c_7, 2);

    assert!(bisimilarity(&&graph_b, &&graph_c));
    assert!(bisimilarity_ignore_labels(&&graph_b, &&graph_c))
}

#[test]
fn bisimilar_paige_tarjan_2() {
    use petgraph::Graph;
    let mut graph_a = Graph::new();

    let node_a_1 = graph_a.add_node(1);
    let node_a_2 = graph_a.add_node(2);
    graph_a.add_edge(node_a_1, node_a_2, 1);
    let node_a_3 = graph_a.add_node(3);
    graph_a.add_edge(node_a_1, node_a_3, 1);
    graph_a.add_edge(node_a_2, node_a_3, 2);
    graph_a.add_edge(node_a_3, node_a_3, 2);

    let mut graph_b = Graph::new();

    let node_b_1 = graph_b.add_node(1);
    let node_b_2 = graph_b.add_node(2);
    graph_b.add_edge(node_b_1, node_b_2, 1);
    graph_b.add_edge(node_b_2, node_b_2, 2);

    assert!(bisimilarity(&&graph_a, &&graph_b));
    assert!(bisimilarity_ignore_labels(&&graph_a, &&graph_b))
}

#[test]
fn bisimilar_paige_tarjan_3() {
    use petgraph::Graph;
    let mut graph_b = Graph::new();

    let node_b_1 = graph_b.add_node(1);
    let node_b_2 = graph_b.add_node(2);
    graph_b.add_edge(node_b_1, node_b_2, 1);
    let node_b_3 = graph_b.add_node(3);
    graph_b.add_edge(node_b_2, node_b_3, 2);
    let node_b_4 = graph_b.add_node(4);
    graph_b.add_edge(node_b_3, node_b_4, 2);

    let mut graph_c = Graph::new();

    let node_c_1 = graph_c.add_node(1);
    let node_c_2 = graph_c.add_node(2);
    graph_c.add_edge(node_c_1, node_c_2, 1);
    let node_c_3 = graph_c.add_node(3);
    graph_c.add_edge(node_c_2, node_c_3, 2);
    let node_c_4 = graph_c.add_node(4);
    graph_c.add_edge(node_c_3, node_c_4, 2);
    let node_c_5 = graph_c.add_node(5);
    graph_c.add_edge(node_c_1, node_c_5, 1);
    graph_c.add_edge(node_c_5, node_c_3, 2);

    assert!(bisimilarity(&&graph_b, &&graph_c));
    assert!(bisimilarity_ignore_labels(&&graph_b, &&graph_c))
}

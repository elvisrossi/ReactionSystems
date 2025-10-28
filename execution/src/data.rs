use petgraph::{Directed, Graph};
use rsprocess::graph::{PositiveSystemGraph, SystemGraph};
use rsprocess::label::{Label, PositiveLabel};
use rsprocess::system::{PositiveSystem, System};
use rsprocess::translator;

// -----------------------------------------------------------------------------
//                              helper functions
// -----------------------------------------------------------------------------

/// Very inelegant way to provide our graph with a map method where the edges
/// are mapped until the first error.
pub trait MapEdges<'a, N: 'a, E, Ty, Ix>
where
    Ty: petgraph::EdgeType,
    Ix: petgraph::graph::IndexType,
{
    fn map_edges(
        &self,
        edge_map: &assert::relabel::Assert,
        translator: &mut translator::Translator,
    ) -> Result<Graph<System, assert::relabel::AssertReturnValue, Ty, Ix>, String>;
}

impl<'a> MapEdges<'a, System, Label, Directed, u32> for SystemGraph {
    fn map_edges(
        &self,
        edge_map: &assert::relabel::Assert,
        translator: &mut translator::Translator,
    ) -> Result<
        Graph<System, assert::relabel::AssertReturnValue, Directed, u32>,
        String,
    > {
        use petgraph::graph::EdgeIndex;

        let mut g = Graph::with_capacity(self.node_count(), self.edge_count());
        let nodes = self.raw_nodes();
        let edges = self.raw_edges();

        let edges = edges
            .iter()
            .enumerate()
            .map(|(i, edge)| {
                match edge_map.execute(self, &EdgeIndex::new(i), translator) {
                    | Err(e) => Err(e),
                    | Ok(val) => Ok((edge.source(), edge.target(), val)),
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        nodes.iter().for_each(|node| {
            g.add_node(node.weight.clone());
        });

        edges.into_iter().for_each(|(source, target, v)| {
            g.add_edge(source, target, v);
        });

        Ok(g)
    }
}

/// Very inelegant way to provide our graph with a map method where the edges
/// are mapped until the first error.
pub trait PositiveMapEdges<'a, N: 'a, E, Ty, Ix>
where
    Ty: petgraph::EdgeType,
    Ix: petgraph::graph::IndexType,
{
    fn map_edges(
        &self,
        edge_map: &assert::positive_relabel::PositiveAssert,
        translator: &mut translator::Translator,
    ) -> Result<
        Graph<
            PositiveSystem,
            assert::positive_relabel::PositiveAssertReturnValue,
            Ty,
            Ix,
        >,
        String,
    >;
}

impl<'a> PositiveMapEdges<'a, PositiveSystem, PositiveLabel, Directed, u32>
    for PositiveSystemGraph
{
    fn map_edges(
        &self,
        edge_map: &assert::positive_relabel::PositiveAssert,
        translator: &mut translator::Translator,
    ) -> Result<
        Graph<
            PositiveSystem,
            assert::positive_relabel::PositiveAssertReturnValue,
            Directed,
            u32,
        >,
        String,
    > {
        use petgraph::graph::EdgeIndex;

        let mut g = Graph::with_capacity(self.node_count(), self.edge_count());
        let nodes = self.raw_nodes();
        let edges = self.raw_edges();

        let edges = edges
            .iter()
            .enumerate()
            .map(|(i, edge)| {
                match edge_map.execute(self, &EdgeIndex::new(i), translator) {
                    | Err(e) => Err(e),
                    | Ok(val) => Ok((edge.source(), edge.target(), val)),
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        nodes.iter().for_each(|node| {
            g.add_node(node.weight.clone());
        });

        edges.into_iter().for_each(|(source, target, v)| {
            g.add_edge(source, target, v);
        });

        Ok(g)
    }
}

/// Given a graph and a function that identifies nodes of the graph, merges
/// nodes with the same identifier.
pub fn grouping(
    graph: &mut Graph<System, Label>,
    group: &assert::grouping::Assert,
    translator: &mut rsprocess::translator::Translator,
) -> Result<(), String> {
    let mut buckets = std::collections::HashMap::new();
    let mut leader: std::collections::HashMap<petgraph::prelude::NodeIndex, _> =
        std::collections::HashMap::new();

    for node in graph.node_indices() {
        let val = group.execute(graph, &node, translator)?;
        buckets.entry(val.clone()).or_insert(vec![]).push(node);
        let l = buckets.get(&val).unwrap().first().unwrap();
        leader.insert(node, (*l, val));
    }

    for node in graph.node_indices().rev() {
        let (origin, val) = leader.get(&node).unwrap();
        if *origin == node {
            continue;
        }
        if buckets.get(val).unwrap().len() <= 1 {
            continue;
        }

        let mut edges =
            graph.neighbors_directed(node, petgraph::Outgoing).detach();
        while let Some(edge) = edges.next_edge(graph) {
            graph.add_edge(
                *origin,
                graph.edge_endpoints(edge).unwrap().1,
                graph.edge_weight(edge).unwrap().clone(),
            );
        }
        let mut edges =
            graph.neighbors_directed(node, petgraph::Incoming).detach();
        while let Some(edge) = edges.next_edge(graph) {
            graph.add_edge(
                graph.edge_endpoints(edge).unwrap().0,
                *origin,
                graph.edge_weight(edge).unwrap().clone(),
            );
        }
        graph
            .remove_node(node)
            .ok_or(format!("Could not remove node {node:?}"))?;
    }

    Ok(())
}

/// Given a graph and a function that identifies nodes of the graph, merges
/// nodes with the same identifier.
pub fn positive_grouping(
    graph: &mut Graph<PositiveSystem, PositiveLabel>,
    group: &assert::positive_grouping::PositiveAssert,
    translator: &mut rsprocess::translator::Translator,
) -> Result<(), String> {
    let mut buckets = std::collections::HashMap::new();
    let mut leader: std::collections::HashMap<petgraph::prelude::NodeIndex, _> =
        std::collections::HashMap::new();

    for node in graph.node_indices() {
        let val = group.execute(graph, &node, translator)?;
        buckets.entry(val.clone()).or_insert(vec![]).push(node);
        let l = buckets.get(&val).unwrap().first().unwrap();
        leader.insert(node, (*l, val));
    }

    for node in graph.node_indices().rev() {
        let (origin, val) = leader.get(&node).unwrap();
        if *origin == node {
            continue;
        }
        if buckets.get(val).unwrap().len() <= 1 {
            continue;
        }

        let mut edges =
            graph.neighbors_directed(node, petgraph::Outgoing).detach();
        while let Some(edge) = edges.next_edge(graph) {
            graph.add_edge(
                *origin,
                graph.edge_endpoints(edge).unwrap().1,
                graph.edge_weight(edge).unwrap().clone(),
            );
        }
        let mut edges =
            graph.neighbors_directed(node, petgraph::Incoming).detach();
        while let Some(edge) = edges.next_edge(graph) {
            graph.add_edge(
                graph.edge_endpoints(edge).unwrap().0,
                *origin,
                graph.edge_weight(edge).unwrap().clone(),
            );
        }
        graph
            .remove_node(node)
            .ok_or(format!("Could not remove node {node:?}"))?;
    }

    Ok(())
}

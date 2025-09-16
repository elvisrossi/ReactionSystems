use petgraph::{Directed, Graph};
use rsprocess::graph::SystemGraph;
use rsprocess::label::Label;
use rsprocess::system::System;
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

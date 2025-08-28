//! Bisimilarity by Kanellakis and Smolka from The algorithmics of bisimilarity
//! by Luca Aceto, Anna Ingolfsdottir and Jirí Srba; pages 105 to 110
//! <https://doi.org/10.1017/CBO9780511792588.004>

use std::collections::{BTreeSet, HashMap, HashSet};

use petgraph::visit::{ EdgeRef, GraphBase, IntoEdgeReferences,
		       IntoEdges, IntoNodeIdentifiers,
		       IntoNodeReferences };

// -----------------------------------------------------------------------------
//                              Helper Functions
// -----------------------------------------------------------------------------

#[inline(always)]
fn equal_vectors<T>(a: &Vec<T>, b: &Vec<T>) -> bool
where
    T: PartialEq
{
    for el in a {
	if !b.contains(el) {
	    return false;
	}
    }

    for el in b {
	if !a.contains(el) {
	    return false;
	}
    }

    true
}

// -----------------------------------------------------------------------------
//                                Bisimilarity
// -----------------------------------------------------------------------------

struct GraphPartition<'a, G>
where
    G: GraphBase,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
{
    pub node_to_block: HashMap<(usize, G::NodeId), u32>,
    pub block_to_node: HashMap<u32, Vec<(usize, G::NodeId)>>,
    pub graphs: [&'a G; 2],
    last_block: u32,
    blocks: BTreeSet<u32>
}

impl<'a, G> GraphPartition<'a, G>
where
    G: GraphBase,
    G::NodeId: std::cmp::Eq + std::hash::Hash
{
    pub fn new(graph_a: &'a G, graph_b: &'a G) -> Self {
	GraphPartition { node_to_block: HashMap::new(),
			 block_to_node:  HashMap::new(),
			 graphs: [graph_a, graph_b],
			 last_block: 0,
			 blocks: BTreeSet::new() }
    }

    #[inline(always)]
    pub fn add_node_last_partition(&mut self, node: G::NodeId, graph: usize) {
	self.node_to_block.insert((graph, node), self.last_block);
	self.block_to_node
	    .entry(self.last_block)
	    .or_default()
	    .push((graph, node));
	self.blocks.insert(self.last_block);
    }

    #[inline(always)]
    pub fn iterate_blocks(&self) -> Vec<u32> {
	self.blocks.iter().cloned().collect::<Vec<_>>()
    }

    pub fn bisimilar(&self) -> bool {
	// if there is a block that has only elements from one graph then they
	// are not bisimilar
	for (_block, node_list) in self.block_to_node.iter() {
	    let graph_id = node_list.first().unwrap().0;
	    if node_list.iter().all(|el| el.0 == graph_id) {
		return false;
	    }
	}
	true
    }
}

impl<'a, G> GraphPartition<'a, G>
where
    G: IntoEdges,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
{
    fn reachable_blocks(
	&self,
	label: &G::EdgeWeight,
	s: &(usize, G::NodeId)
    ) -> Vec<u32>
    where G::EdgeWeight: PartialEq
    {
	let mut val = vec![];
	for el in self.graphs[s.0].edges(s.1).filter(|x| x.weight() == label) {
	    let tmp = (s.0, el.target());
	    val.push(*self.node_to_block.get(&tmp).unwrap());
	}
	val
    }

    pub fn split(&mut self, block: u32, label: &G::EdgeWeight) -> bool
    where G::EdgeWeight: PartialEq
    {
	let Some(nodes) = self.block_to_node.get(&block)
	else {
	    return true
	};
	let mut nodes = nodes.iter();
	let s = nodes.next().unwrap();

	let mut b1 = vec![s];
	let mut b2 = vec![];

	let reachable_blocks_s = self.reachable_blocks(label, s);

	for t in nodes {
	    let reachable_blocks_t = self.reachable_blocks(label, t);
	    if equal_vectors(&reachable_blocks_s, &reachable_blocks_t) {
		b1.push(t);
	    } else {
		b2.push(*t);
	    }
	}

	if b2.is_empty() {
	    // all elements go to the same block with label label, so no change
	    false
	} else {
	    // some elements need to be split into a different block
	    self.last_block += 1;
	    let new_block = self.last_block;
	    self.blocks.insert(new_block);

	    for b in b2 {
		self.node_to_block.entry(b).and_modify(|e| *e = new_block);
		self.block_to_node.entry(new_block).or_default().push(b);
		self.block_to_node.entry(block).and_modify(|e| {
		    let index = e.iter().position(|x| *x == b).unwrap();
		    e.remove(index);
		});
	    }
	    true
	}
    }
}

pub fn bisimilarity<'a, G>(
    graph_a: &'a G,
    graph_b: &'a G
) -> bool
where
    G: IntoNodeReferences + IntoEdges,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
    G::EdgeWeight: std::cmp::Eq + std::hash::Hash + Clone
{
    let graphs = [graph_a, graph_b];

    let mut partition: GraphPartition<G> =
	GraphPartition::new(graph_a, graph_b);
    for (p, graph) in graphs.iter().enumerate() {
	for node in graph.node_identifiers() {
	    partition.add_node_last_partition(node, p);
	}
    }

    let labels =
	graph_a.edge_references()
	.chain(graph_b.edge_references())
	.map(|e| e.weight().clone())
	.collect::<HashSet<_>>();

    let mut changed = true;

    while changed {
	changed = false;

	for block in partition.iterate_blocks() {
	    for label in labels.iter() {
		if partition.split(block, label) {
		    changed = true;
		}
	    }
	}
    }

    partition.bisimilar()
}

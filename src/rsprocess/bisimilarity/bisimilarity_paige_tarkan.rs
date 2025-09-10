//! Bisimilarity by Paige and Tarjan from Three Partition Refinement Algorithms
//! by Robert Paige L., Robert Endre Tarjan; pages 977 to 983
//! <https://doi.org/10.1137/0216062>

use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use petgraph::Direction::{Incoming, Outgoing};
use petgraph::visit::{
    EdgeCount, EdgeRef, GraphBase, IntoEdgeReferences, IntoNeighborsDirected,
    IntoNodeIdentifiers, IntoNodeReferences, NodeCount,
};

type NodeIdType = u32;
type GraphIdType = u32;

type NodeType = (GraphIdType, NodeIdType);

trait NextId<From, T> {
    fn next_id_of_graph(&mut self, val: From) -> T;
}

struct Translator<From, To, State>
where
    State: NextId<From, To>,
{
    data: HashMap<From, To>,
    reverse_data: HashMap<To, From>,
    last_id: State,
}

impl<From, To, State> Translator<From, To, State>
where
    To: std::hash::Hash + std::cmp::Eq + Copy,
    From: std::hash::Hash + std::cmp::Eq + Clone,
    State: NextId<From, To>,
{
    pub fn new() -> Self
    where
        State: Default,
    {
        Translator {
            data: HashMap::new(),
            reverse_data: HashMap::new(),
            last_id: State::default(),
        }
    }

    pub fn encode(&mut self, val: From) -> To {
        let id = *(self
            .data
            .entry(val.clone())
            .or_insert(self.last_id.next_id_of_graph(val.clone())));
        self.reverse_data.insert(id, val);
        id
    }

    pub fn get(&self, val: &From) -> Option<&To> {
        self.data.get(val)
    }

    pub fn decode(&self, val: &To) -> Option<&From> {
        self.reverse_data.get(val)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct NodeState<const N: usize> {
    last_ids: [u32; N],
}

impl<const N: usize> NodeState<N> {
    fn new() -> Self {
        NodeState { last_ids: [0; N] }
    }
}

impl<const N: usize> Default for NodeState<N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const N: usize, T> NextId<(T, GraphIdType), NodeType> for NodeState<N> {
    fn next_id_of_graph(&mut self, val: (T, GraphIdType)) -> NodeType {
        let graph_id_usize = val.1 as usize;
        assert!(graph_id_usize < N);
        self.last_ids[graph_id_usize] += 1;
        (val.1, self.last_ids[graph_id_usize])
    }
}

type MyNodeTranslator<From, const N: usize> =
    Translator<(From, GraphIdType), NodeType, NodeState<N>>;

type EdgeIdType = u32;
type EdgeType = EdgeIdType;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct EdgeState {
    last_ids: u32,
}

impl EdgeState {
    fn new() -> Self {
        EdgeState { last_ids: 0 }
    }
}

impl Default for EdgeState {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> NextId<T, EdgeType> for EdgeState {
    fn next_id_of_graph(&mut self, _val: T) -> EdgeType {
        self.last_ids += 1;
        self.last_ids
    }
}

type MyEdgeTranslator<From> = Translator<From, EdgeType, EdgeState>;

type Block = Vec<NodeType>;
type BackEdges = HashMap<NodeType, Vec<NodeType>>;
type NodeToBlock = HashMap<NodeType, Rc<RefCell<SimpleBlock>>>;
type CompoundPartition = Vec<Rc<CompoundBlock>>;
type SimpleBlockPointer = Rc<RefCell<SimpleBlock>>;
type CompoundBlockPointer = Rc<CompoundBlock>;
type BackEdgesGrouped = HashMap<Block, BackEdgesGroup>;

struct SimpleBlock {
    block: Block,
    coarse_block_that_supersets_self: Rc<CompoundBlock>,
}

#[derive(Clone)]
struct CompoundBlock {
    block: Block,
    simple_blocks_subsets_of_self: RefCell<Vec<Rc<RefCell<SimpleBlock>>>>,
}

impl CompoundBlock {
    fn add_simple_block(&self, fine_block: Rc<RefCell<SimpleBlock>>) {
        self.simple_blocks_subsets_of_self
            .borrow_mut()
            .push(fine_block);
    }
    fn remove_simple_block(&self, fine_block: &Rc<RefCell<SimpleBlock>>) {
        self.simple_blocks_subsets_of_self
            .borrow_mut()
            .retain(|x| !Rc::ptr_eq(x, fine_block));
    }
    fn simple_block_count(&self) -> usize {
        self.simple_blocks_subsets_of_self.borrow().len()
    }
}

struct BackEdgesGroup {
    block: Rc<RefCell<SimpleBlock>>,
    subblock: Block,
}

trait HasBlock {
    fn block(&self) -> Block;
}

impl HasBlock for SimpleBlockPointer {
    fn block(&self) -> Block {
        (**self).borrow().block.clone()
    }
}

impl HasBlock for CompoundBlock {
    fn block(&self) -> Block {
        self.block.clone()
    }
}

#[allow(clippy::type_complexity)]
fn initialization<const N: usize, G>(
    graphs: &[&G; N],
) -> (
    (SimpleBlockPointer, SimpleBlockPointer),
    CompoundPartition,
    NodeToBlock,
    MyNodeTranslator<G::NodeId, N>,
)
where
    G: IntoNodeReferences + IntoNeighborsDirected,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
{
    // translate into unique ids
    let mut convert_nodes: MyNodeTranslator<G::NodeId, N> = Translator::new();

    let graph_node_indices = {
        let mut tmp: Block = vec![];

        for (pos, graph) in graphs.iter().enumerate() {
            tmp.extend(
                graph
                    .node_identifiers()
                    .map(|val| convert_nodes.encode((val, pos as u32)))
                    .collect::<Vec<_>>(),
            );
        }

        tmp
    };
    let compound_initial_block_pointer: Rc<CompoundBlock> = {
        let compound_initial_block = CompoundBlock {
            block: graph_node_indices.clone(),
            simple_blocks_subsets_of_self: RefCell::new(vec![]),
        };

        Rc::new(compound_initial_block)
    };

    // minor optimization: split nodes between those that have outgoing edges
    // and those that dont
    let (leaf_node_block_pointer, non_leaf_node_block_pointer) = {
        let (leaf_node_indices, non_leaf_node_indices): (Block, Block) =
            graph_node_indices.clone().into_iter().partition(|x| {
                let (node_id, graph_id) = convert_nodes.decode(x).unwrap();
                graphs[*graph_id as usize]
                    .neighbors_directed(*node_id, Outgoing)
                    .count()
                    == 0
            });

        let leaf_node_block = SimpleBlock {
            block: leaf_node_indices,

            coarse_block_that_supersets_self: Rc::clone(
                &compound_initial_block_pointer,
            ),
        };
        let non_leaf_node_block = SimpleBlock {
            block: non_leaf_node_indices,

            coarse_block_that_supersets_self: Rc::clone(
                &compound_initial_block_pointer,
            ),
        };

        (
            Rc::new(RefCell::new(leaf_node_block)),
            Rc::new(RefCell::new(non_leaf_node_block)),
        )
    };

    compound_initial_block_pointer
        .simple_blocks_subsets_of_self
        .borrow_mut()
        .extend([
            Rc::clone(&leaf_node_block_pointer),
            Rc::clone(&non_leaf_node_block_pointer),
        ]);

    let node_to_block = {
        let mut node_to_block = HashMap::new();

        (*non_leaf_node_block_pointer)
            .borrow()
            .block
            .iter()
            .copied()
            .for_each(|value| {
                node_to_block
                    .insert(value, Rc::clone(&non_leaf_node_block_pointer));
            });

        (*leaf_node_block_pointer)
            .borrow()
            .block
            .iter()
            .copied()
            .for_each(|value| {
                node_to_block
                    .insert(value, Rc::clone(&leaf_node_block_pointer));
            });
        node_to_block
    };

    (
        (leaf_node_block_pointer, non_leaf_node_block_pointer),
        vec![compound_initial_block_pointer],
        node_to_block,
        convert_nodes,
    )
}

fn build_backedges<IndexHolder: HasBlock, const N: usize, G>(
    graphs: &[&G; N],
    block: IndexHolder,
    convert_nodes: &MyNodeTranslator<G::NodeId, N>,
) -> BackEdges
where
    G: IntoNeighborsDirected,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
{
    let mut backedges = HashMap::new();

    block.block().iter().for_each(|node_index_pointer| {
        backedges.insert(*node_index_pointer, {
            let (node_id, graph_id) =
                convert_nodes.decode(node_index_pointer).unwrap();
            graphs[*graph_id as usize]
                .neighbors_directed(*node_id, Incoming)
                .collect::<HashSet<_>>()
                .into_iter()
                // the back edges should be all in the same graph
                .map(|e| convert_nodes.get(&(e, *graph_id)).unwrap())
                .copied()
                .collect::<Vec<_>>()
        });
    });

    backedges
}

fn group_by_backedges(
    backedges: BackEdges,
    node_to_block: &NodeToBlock,
) -> BackEdgesGrouped {
    let mut backedges_grouped: BackEdgesGrouped = HashMap::new();

    for incoming_neighbor_group in backedges.values() {
        for node in incoming_neighbor_group {
            let block = Rc::clone(node_to_block.get(node).unwrap());
            let key = (*block).borrow().block.clone();

            match backedges_grouped.entry(key) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().subblock.push(*node)
                }
                Entry::Vacant(entry) => {
                    entry.insert(BackEdgesGroup {
                        block: Rc::clone(&block),
                        subblock: Vec::from([*node]),
                    });
                }
            }
        }
    }

    backedges_grouped
}

fn split_blocks_with_grouped_backedges(
    mut backedges_grouped: BackEdgesGrouped,
    node_to_block: &mut NodeToBlock,
) -> (
    (Vec<SimpleBlockPointer>, Vec<SimpleBlockPointer>),
    Vec<CompoundBlockPointer>,
) {
    let mut all_new_simple_blocks: Vec<Rc<RefCell<SimpleBlock>>> = vec![];
    let mut all_removed_simple_blocks: Vec<Rc<RefCell<SimpleBlock>>> = vec![];
    let mut new_compound_blocks: Vec<Rc<CompoundBlock>> = vec![];

    for (block, back_edges_group) in backedges_grouped.iter_mut() {
        let borrowed_compound_block = Rc::clone(
            &(*back_edges_group.block)
                .borrow()
                .coarse_block_that_supersets_self,
        );

        let proper_subblock = {
            let simple_block = SimpleBlock {
                block: back_edges_group.subblock.clone(),

                coarse_block_that_supersets_self: Rc::clone(
                    &borrowed_compound_block,
                ),
            };

            Rc::new(RefCell::new(simple_block))
        };
        let prior_count = borrowed_compound_block.simple_block_count();
        borrowed_compound_block.add_simple_block(Rc::clone(&proper_subblock));

        if prior_count == 1 {
            new_compound_blocks.push(Rc::clone(&borrowed_compound_block));
        }

        for node in back_edges_group.subblock.iter() {
            node_to_block.insert(*node, Rc::clone(&proper_subblock));
        }

        // subtract subblock from block
        (*back_edges_group.block).borrow_mut().block = block
            .iter()
            .filter(|x| !(*proper_subblock).borrow().block.contains(x))
            .copied()
            .collect();

        if (*back_edges_group.block).borrow().block.is_empty() {
            borrowed_compound_block
                .remove_simple_block(&back_edges_group.block);
            all_removed_simple_blocks.push(Rc::clone(&back_edges_group.block));
        }
        all_new_simple_blocks.push(Rc::clone(&proper_subblock));
    }
    (
        (all_new_simple_blocks, all_removed_simple_blocks),
        new_compound_blocks,
    )
}

fn maximum_bisimulation<const N: usize, G>(
    graphs: &[&G; N],
) -> (Option<Vec<Block>>, MyNodeTranslator<G::NodeId, N>)
where
    G: IntoNodeReferences + IntoNeighborsDirected,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
{
    let (
        (simple_block_0, simple_block_1),
        initial_compound_partition,
        mut node_to_block,
        converter,
    ) = initialization(graphs);

    let mut queue: CompoundPartition = initial_compound_partition;
    let mut all_simple_blocks = vec![simple_block_0, simple_block_1];

    loop {
        let (smaller_component, simple_splitter_block) = {
            let splitter_block = match queue.pop() {
                Some(coarse_block) => coarse_block,
                None => break,
            };
            let mut simple_blocks_in_splitter_block = splitter_block
                .simple_blocks_subsets_of_self
                .borrow()
                .clone();

            let smaller_component_index = {
                match simple_blocks_in_splitter_block
                    .iter()
                    .enumerate()
                    .min_by(|(_, x), (_, y)| {
                        (***x)
                            .borrow()
                            .block
                            .len()
                            .cmp(&(***y).borrow().block.len())
                    })
                    .map(|(index, _)| index)
                {
                    Some(v) => v,
                    None => return (None, converter),
                }
            };

            let smaller_component =
                simple_blocks_in_splitter_block.remove(smaller_component_index);

            let simple_splitter_block_values: Block = splitter_block
                .block
                .clone()
                .iter()
                .filter(|x| !(*smaller_component).borrow().block.contains(x))
                .copied()
                .collect();

            let simple_splitter_block = CompoundBlock {
                block: simple_splitter_block_values,

                simple_blocks_subsets_of_self: RefCell::new(
                    simple_blocks_in_splitter_block,
                ),
            };
            let simple_splitter_block_pointer = Rc::new(simple_splitter_block);

            if simple_splitter_block_pointer
                .simple_blocks_subsets_of_self
                .borrow()
                .len()
                > 1
            {
                queue.push(Rc::clone(&simple_splitter_block_pointer));
            }

            (smaller_component, simple_splitter_block_pointer)
        };
        simple_splitter_block
            .simple_blocks_subsets_of_self
            .borrow()
            .iter()
            .for_each(|x| {
                (*x).borrow_mut().coarse_block_that_supersets_self =
                    Rc::clone(&simple_splitter_block);
            });

        let mut back_edges =
            build_backedges(graphs, smaller_component, &converter);

        let back_edges_group =
            group_by_backedges(back_edges.clone(), &node_to_block);
        let (
            (new_simple_blocks, removeable_simple_blocks),
            compound_block_that_are_now_compound,
        ) = split_blocks_with_grouped_backedges(
            back_edges_group,
            &mut node_to_block,
        );

        all_simple_blocks.extend(new_simple_blocks);
        all_simple_blocks.retain(|x| {
            !removeable_simple_blocks.iter().any(|y| Rc::ptr_eq(x, y))
        });
        queue.extend(compound_block_that_are_now_compound);

        // back edges = E^{-1}(B) - E^{-1}(S-B)
        {
            let back_edges_splitter_complement = build_backedges(
                graphs,
                (*simple_splitter_block).clone(),
                &converter,
            );

            back_edges_splitter_complement.keys().for_each(|node| {
                back_edges.remove(node);
            });
        }

        let back_edges_group = group_by_backedges(back_edges, &node_to_block);
        let (
            (new_fine_blocks, removeable_fine_blocks),
            coarse_block_that_are_now_compound,
        ) = split_blocks_with_grouped_backedges(
            back_edges_group,
            &mut node_to_block,
        );

        all_simple_blocks.extend(new_fine_blocks);
        all_simple_blocks.retain(|x| {
            !removeable_fine_blocks.iter().any(|y| Rc::ptr_eq(x, y))
        });
        queue.extend(coarse_block_that_are_now_compound);
    }

    (
        Some(
            all_simple_blocks
                .iter()
                .map(|x| (**x).borrow().block.clone())
                // remove leaf block when there are no leaves
                .filter(|x| !x.is_empty())
                .collect(),
        ),
        converter,
    )
}

/// Creates a new graph with nodes as signifiers instead of different weights on
/// the edges.
fn create_modified_graph<G>(
    graph: &G,
    converter_edges: &MyEdgeTranslator<G::EdgeWeight>,
) -> (petgraph::Graph<u32, u32>, HashSet<u32>)
where
    G: NodeCount + EdgeCount + IntoEdgeReferences + IntoNodeIdentifiers,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
    G::EdgeWeight: std::cmp::Eq + std::hash::Hash + Clone,
{
    let mut new_graph_a: petgraph::Graph<_, u32> =
        petgraph::Graph::with_capacity(
            graph.node_count() * 4,
            graph.edge_count() * 4,
        );
    let mut association_weight_id = HashMap::new();
    let mut original_nodes = HashSet::new();
    let mut last_id = 0;

    for edge in graph.edge_references() {
        let source_id = match association_weight_id.get(&edge.source()) {
            Some(id) => *id,
            None => {
                let id = new_graph_a.add_node(last_id);
                original_nodes.insert(last_id);
                last_id += 1;
                association_weight_id.insert(edge.source(), id);
                id
            }
        };
        let target_id = match association_weight_id.get(&edge.target()) {
            Some(id) => *id,
            None => {
                let id = new_graph_a.add_node(last_id);
                original_nodes.insert(last_id);
                last_id += 1;
                association_weight_id.insert(edge.target(), id);
                id
            }
        };
        let weight = *converter_edges.get(edge.weight()).unwrap();

        let middle_node_id = new_graph_a.add_node(0);

        new_graph_a.add_edge(source_id, middle_node_id, weight);
        new_graph_a.add_edge(middle_node_id, target_id, weight);

        let mut previous = middle_node_id;
        for _ in 0..weight {
            let path = new_graph_a.add_node(0);
            new_graph_a.add_edge(previous, path, weight);
            previous = path;
        }
    }

    for node in graph.node_identifiers() {
        let mut previous = *association_weight_id.get(&node).unwrap();
        for _ in 0..converter_edges.last_id.last_ids + 2 {
            let path = new_graph_a.add_node(0);
            new_graph_a.add_edge(previous, path, 0);
            previous = path;
        }
    }

    (new_graph_a, original_nodes)
}

#[allow(clippy::type_complexity)]
fn modify_graph<G>(
    graph_a: &G,
    graph_b: &G,
) -> (
    (petgraph::Graph<u32, u32>, HashSet<u32>),
    (petgraph::Graph<u32, u32>, HashSet<u32>),
)
where
    G: IntoNodeReferences + IntoNeighborsDirected + NodeCount + EdgeCount,
    G: IntoEdgeReferences,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
    G::EdgeWeight: std::cmp::Eq + std::hash::Hash + Clone,
{
    let converter_edges: MyEdgeTranslator<G::EdgeWeight> = {
        let mut converter_edges = Translator::new();
        let mut labels: HashMap<G::EdgeWeight, u32> = HashMap::new();

        for edge in graph_a.edge_references() {
            *labels.entry(edge.weight().clone()).or_default() += 1;
        }
        for edge in graph_b.edge_references() {
            *labels.entry(edge.weight().clone()).or_default() += 1;
        }
        // slight optimization: we reorder the edges such that edges with the
        // most occurrences have smaller index
        let mut labels: Vec<(G::EdgeWeight, u32)> =
            labels.into_iter().collect();
        labels.sort_by(|a, b| b.1.cmp(&a.1));

        for (label, _) in labels.into_iter() {
            let _ = converter_edges.encode(label);
        }
        converter_edges
    };

    let new_graph_a = create_modified_graph(graph_a, &converter_edges);
    let new_graph_b = create_modified_graph(graph_b, &converter_edges);

    (new_graph_a, new_graph_b)
}

/// check if every block contains either no original nodes or nodes from both
/// graphs
fn check_bisimilarity<G>(
    val: Vec<Vec<NodeType>>,
    converter_bisimulated_graph: &MyNodeTranslator<
        <petgraph::Graph<u32, u32> as GraphBase>::NodeId,
        2,
    >,
    original_nodes: [HashSet<u32>; 2],
) -> bool
where
    G: IntoEdgeReferences,
    G::EdgeWeight: std::cmp::Eq + std::hash::Hash + Clone,
{
    val.into_iter().all(|el| {
        let mut keep_track = [false, false];
        for e in el {
            let (_node_id, graph_id) =
                converter_bisimulated_graph.decode(&e).unwrap();
            if original_nodes[*graph_id as usize].contains(&e.0) {
                keep_track[*graph_id as usize] = true;
            }
        }
        !(keep_track[0] ^ keep_track[1])
    })
}

// -----------------------------------------------------------------------------

pub fn bisimilarity<G>(graph_a: &G, graph_b: &G) -> bool
where
    G: IntoNodeReferences + IntoNeighborsDirected + NodeCount + EdgeCount,
    G: IntoEdgeReferences,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
    G::EdgeWeight: std::cmp::Eq + std::hash::Hash + Clone,
{
    if graph_a.node_count() == 0 && graph_b.node_count() == 0 {
        return true;
    }
    if graph_a.node_count() == 0 || graph_b.node_count() == 0 {
        return false;
    }

    let ((new_graph_a, original_nodes_a), (new_graph_b, original_nodes_b)) =
        modify_graph(graph_a, graph_b);

    let (result, _converter) =
        match maximum_bisimulation(&[&&new_graph_a, &&new_graph_b]) {
            (None, _) => return false,
            (Some(val), converter) => (
                check_bisimilarity::<G>(
                    val,
                    &converter,
                    [original_nodes_a, original_nodes_b],
                ),
                converter,
            ),
        };

    result
}

pub fn bisimilarity_ignore_labels<G>(graph_a: &G, graph_b: &G) -> bool
where
    G: IntoNodeReferences + IntoNeighborsDirected + NodeCount,
    G::NodeId: std::cmp::Eq + std::hash::Hash,
{
    if graph_a.node_count() == 0 && graph_b.node_count() == 0 {
        return true;
    }
    if graph_a.node_count() == 0 || graph_b.node_count() == 0 {
        return false;
    }

    let (result, _converter) = match maximum_bisimulation(&[graph_a, graph_b]) {
        (None, _) => return false,
        (Some(val), converter) => (
            val.into_iter().find(|el| {
                let mut keep_track = [false, false];
                for e in el {
                    let (_node_id, graph_id) = converter.decode(e).unwrap();
                    keep_track[*graph_id as usize] = true;
                }
                !keep_track[0] || !keep_track[1]
            }),
            converter,
        ),
    };

    result.is_none()
}

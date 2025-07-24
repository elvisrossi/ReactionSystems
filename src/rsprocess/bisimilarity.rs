use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Debug;
use std::rc::Rc;

use petgraph::visit::{ EdgeRef, GraphBase, IntoEdgeReferences, IntoEdges, IntoNeighborsDirected, IntoNodeReferences, NodeCount };
use petgraph::Direction::{Incoming, Outgoing};

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



// -----------------------------------------------------------------------------
// Bisimilarity by Kanellakis and Smolka from The algorithmics of bisimilarity
// by Luca Aceto, Anna Ingolfsdottir and Jirí Srba; pages 105 to 110
// https://doi.org/10.1017/CBO9780511792588.004
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
		label: &G::EdgeRef,
		s: &(usize, G::NodeId)
	) -> Vec<u32>
	where <G as IntoEdgeReferences>::EdgeRef: PartialEq
	{
		let mut val = vec![];
		for el in self.graphs[s.0].edges(s.1).filter(|x| x == label) {
			let tmp = (s.0, el.target());
			val.push(*self.node_to_block.get(&tmp).unwrap());
		}
		val
	}

	pub fn split(&mut self, block: u32, label: &G::EdgeRef) -> bool
	where <G as IntoEdgeReferences>::EdgeRef: PartialEq
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

pub fn bisimilarity_kanellakis_smolka<'a, G>(
	graph_a: &'a G,
	graph_b: &'a G
) -> bool
where
	G: IntoNodeReferences + IntoEdges,
	G::NodeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeRef: PartialEq
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
		.collect::<Vec<_>>();

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

// -----------------------------------------------------------------------------
// Bisimilarity by Paige and Tarjan from Three Partition Refinement Algorithms
// by Robert Paige L., Robert Endre Tarjan; pages 977 to 983
// https://doi.org/10.1137/0216062
// -----------------------------------------------------------------------------

type NodeIdType = u32;
type GraphIdType = u32;

type NodeType = (GraphIdType, NodeIdType);

trait NextId<From, T> {
	fn next_id_of_graph(&mut self, val: From) -> T;
}

#[derive(Debug)]
struct Translator<From, To, State>
where
	State: NextId<From, To>
{
	data: HashMap<From, To>,
	reverse_data: HashMap<To, From>,
	last_id: State,
}

impl<From, To, State> Translator<From, To, State>
where
	To: std::hash::Hash + std::cmp::Eq + Copy,
	From: std::hash::Hash + std::cmp::Eq + Clone,
	State: NextId<From, To>
{
	pub fn new() -> Self
	where
		State: Default
	{
		Translator { data: HashMap::new(),
					 reverse_data: HashMap::new(),
					 last_id: State::default() }
	}

	pub fn encode(&mut self, val: From) -> To
	{
		let id = *(self.data.entry(val.clone())
				   .or_insert(
					   self.last_id.next_id_of_graph(val.clone())
				   ));
		self.reverse_data.insert(id, val);
		id
	}

	pub fn get(&self, val: &From) -> Option<&To>
	{
		self.data.get(val)
	}

	pub fn decode(&self, val: &To) -> Option<&From> {
		self.reverse_data.get(val)
	}
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
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
		if graph_id_usize > self.last_ids.len() {
			panic!()
		}
		self.last_ids[graph_id_usize] += 1;
		(val.1, self.last_ids[graph_id_usize])
	}
}

type MyTranslator<From, const N: usize> = Translator<(From, GraphIdType), NodeType, NodeState<N>>;





type Block = Vec<NodeType>;
type BackEdges = HashMap<NodeType, Vec<NodeType>>;
type NodeToBlock = HashMap<NodeType, Rc<RefCell<SimpleBlock>>>;
type CompoundPartition = Vec<Rc<CompoundBlock>>;
type FineBlockPointer = Rc<RefCell<SimpleBlock>>;
type CompoundBlockPointer = Rc<CompoundBlock>;
type BackEdgesGrouped = HashMap<Block, BackEdgesGroup>;

struct SimpleBlock {
	block: Block,
	coarse_block_that_supersets_self: Rc<CompoundBlock>
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

impl HasBlock for FineBlockPointer {
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
	graphs: &[&G; N]
) -> ( (FineBlockPointer, FineBlockPointer),
	   CompoundPartition,
	   NodeToBlock,
	   MyTranslator<G::NodeId, N> )
where
	G: IntoNodeReferences + IntoEdges + IntoNeighborsDirected,
	G::NodeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeRef: PartialEq,
	G::NodeId: Debug
{
	// translate into unique ids
	let mut convert_nodes: MyTranslator<G::NodeId, N>
		= Translator::new();

	let graph_node_indices = {
		let mut tmp: Block = vec![];

		for (pos, graph) in graphs.iter().enumerate() {
			tmp.extend(
				graph.node_identifiers()
					.map(|val| convert_nodes.encode((val, pos as u32)))
					.collect::<Vec<_>>()
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
			graph_node_indices
			.clone()
			.into_iter()
			.partition(
				|x| {
					let (node_id, graph_id) = convert_nodes.decode(x).unwrap();
					graphs[*graph_id as usize]
						.neighbors_directed(
							*node_id,
							Outgoing)
						.count() == 0
				}
			);

		let leaf_node_block = SimpleBlock {
			block: leaf_node_indices,

			coarse_block_that_supersets_self:
			Rc::clone(&compound_initial_block_pointer),
		};
		let non_leaf_node_block = SimpleBlock {
			block: non_leaf_node_indices,

			coarse_block_that_supersets_self:
			Rc::clone(&compound_initial_block_pointer),
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
		let mut tmp = HashMap::new();

		(*non_leaf_node_block_pointer)
			.borrow()
			.block
			.iter()
			.copied()
			.for_each(
				|value|
				{ tmp.insert(value, Rc::clone(&non_leaf_node_block_pointer)); }
			);

		(*leaf_node_block_pointer)
			.borrow()
			.block
			.iter()
			.copied()
			.for_each(
				|value|
				{ tmp.insert(value, Rc::clone(&leaf_node_block_pointer)); }
			);
		tmp
	};

	(
		(leaf_node_block_pointer, non_leaf_node_block_pointer),
		vec![compound_initial_block_pointer],
		node_to_block,
		convert_nodes
	)
}

fn build_backedges<IndexHolder: HasBlock, const N:usize, G>(
	graphs: &[&G; N],
	block: IndexHolder,
	convert_nodes: &MyTranslator<G::NodeId, N>
) -> BackEdges
where
	G: IntoNodeReferences + IntoEdges + IntoNeighborsDirected,
	G::NodeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeRef: PartialEq,
{
	let mut backedges = HashMap::new();

	block.block().iter().for_each(|node_index_pointer| {
		backedges.insert(
			*node_index_pointer,
			{
				let (node_id, graph_id) =
					convert_nodes.decode(node_index_pointer).unwrap();
				graphs[*graph_id as usize]
					.neighbors_directed(
						*node_id,
						Incoming)
					.collect::<HashSet<_>>()
					.into_iter()
					// the back edges should be all in the same graph
					.map(|e| convert_nodes.get(&(e, *graph_id)).unwrap())
					.copied()
					.collect::<Vec<_>>()
			}
		);
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
				Entry::Occupied(mut entry) =>
					entry.get_mut().subblock.push(*node),
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
	(Vec<FineBlockPointer>, Vec<FineBlockPointer>),
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

				coarse_block_that_supersets_self:
				Rc::clone(&borrowed_compound_block),
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
		(*back_edges_group.block).borrow_mut().block =
			block
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
	graphs: &[&G; N]
) -> (Option<Vec<Block>>, MyTranslator<G::NodeId, N>)
where
	G: IntoNodeReferences + IntoEdges + IntoNeighborsDirected,
	G::NodeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeRef: PartialEq,
	G::NodeId: Debug
{
	let ((simple_block_0, simple_block_1),
		 initial_compound_partition,
		 mut node_to_block,
		 converter) = initialization(graphs);

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
					.map(|(index, _)| index) {
						Some(v) => v,
						None => {return (None, converter)}
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

				simple_blocks_subsets_of_self:
				RefCell::new(simple_blocks_in_splitter_block),
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
		let ((new_simple_blocks, removeable_simple_blocks),
			 compound_block_that_are_now_compound) =
			split_blocks_with_grouped_backedges(back_edges_group,
												&mut node_to_block);

		all_simple_blocks.extend(new_simple_blocks);
		all_simple_blocks.retain(
			|x| !removeable_simple_blocks.iter().any(|y| Rc::ptr_eq(x, y)) );
		queue.extend(compound_block_that_are_now_compound);

		// counterimage = E^{-1}(B) - E^{-1}(S-B)
		{
			let counterimage_splitter_complement =
				build_backedges(graphs,
								(*simple_splitter_block).clone(),
								&converter);

			counterimage_splitter_complement.keys().for_each(|node| {
				back_edges.remove(node);
			});
		}

		let counterimage_group = group_by_backedges(back_edges, &node_to_block);
		let ((new_fine_blocks, removeable_fine_blocks),
			 coarse_block_that_are_now_compound) =
			split_blocks_with_grouped_backedges(counterimage_group,
												&mut node_to_block);

		all_simple_blocks.extend(new_fine_blocks);
		all_simple_blocks.retain(
			|x| !removeable_fine_blocks.iter().any(|y| Rc::ptr_eq(x, y)) );
		queue.extend(coarse_block_that_are_now_compound);
	}

	(Some(
		all_simple_blocks
			.iter()
			.map(|x| (**x).borrow().block.clone())
		// remove leaf block when there are no leaves
			.filter(|x| !x.is_empty())
			.collect(),
	), converter)
}


pub fn bisimilarity_paige_tarjan<G>(
	graph_a: &G,
	graph_b: &G
) -> bool
where
	G: IntoNodeReferences + IntoEdges + IntoNeighborsDirected + NodeCount,
	G::NodeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeRef: PartialEq,
	G::NodeId: Debug,
{
	if graph_a.node_count() == 0 && graph_b.node_count() == 0 {
		return true
	}
	if graph_a.node_count() == 0 || graph_b.node_count() == 0 {
		return false
	}

	let (result, _converter) =
		match maximum_bisimulation(&[graph_a, graph_b]) {
			(None, _) => {return false},
			(Some(val), converter) => {
				(val.into_iter()
					.find(
						|el| {
							let mut keep_track = [false, false];
							for e in el {
								let (_node_id, graph_id) =
									converter.decode(e).unwrap();
								keep_track[*graph_id as usize] = true;
							}
							!keep_track[0] || !keep_track[1]
						}
					), converter)

			}
		};

	result.is_none()
}

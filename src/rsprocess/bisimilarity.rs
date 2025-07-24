use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::{BTreeSet, HashMap, HashSet};
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

trait NodeTrait {
	fn graph(&self) -> GraphIdType;
}

impl NodeTrait for NodeType {
	fn graph(&self) -> GraphIdType {
		self.0
	}
}

trait NextId<T> {
	fn next_id_of_graph(&mut self, graph_id: GraphIdType) -> T;
}

struct Translator<From, To, State>
where
	State: NextId<To>
{
	data: HashMap<From, To>,
	reverse_data: HashMap<To, From>,
	last_id: State,
}

impl<From, To, State> Translator<From, To, State>
where
	To: std::hash::Hash + std::cmp::Eq + Copy,
	From: std::hash::Hash + std::cmp::Eq + Clone,
	State: NextId<To>
{
	pub fn new() -> Self
	where
		State: Default
	{
		Translator { data: HashMap::new(),
					 reverse_data: HashMap::new(),
					 last_id: State::default() }
	}

	pub fn encode(&mut self, val: From, graph_id: GraphIdType) -> To
	{
		let id = *(self.data.entry(val.clone())
				   .or_insert(
					   self.last_id.next_id_of_graph(graph_id)
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

impl<const N: usize> NextId<NodeType> for NodeState<N> {
	fn next_id_of_graph(&mut self, graph_id: u32) -> NodeType {
		let graph_id_usize = graph_id as usize;
		if graph_id_usize > self.last_ids.len() {
			panic!()
		}
		self.last_ids[graph_id_usize] += 1;
		(graph_id, self.last_ids[graph_id_usize])
	}
}





type Block = Vec<NodeType>;
type CounterImage = HashMap<NodeType, Vec<NodeType>>;
type NodeToBlockVec = HashMap<NodeType, Rc<RefCell<FineBlock>>>;
type CoarsePartition = Vec<Rc<CoarseBlock>>;
type FineBlockPointer = Rc<RefCell<FineBlock>>;
type CoarseBlockPointer = Rc<CoarseBlock>;
type CounterimageGrouped = HashMap<Block, CounterImageGroup>;

struct FineBlock {
	values: Block,
	coarse_block_that_supersets_self: Rc<CoarseBlock>
}

#[derive(Clone)]
struct CoarseBlock {
	values: Block,
	fine_blocks_that_are_subsets_of_self: RefCell<Vec<Rc<RefCell<FineBlock>>>>,
}

impl CoarseBlock {
	fn add_fine_block(&self, fine_block: Rc<RefCell<FineBlock>>) {
		self.fine_blocks_that_are_subsets_of_self
			.borrow_mut()
			.push(fine_block);
	}
	fn remove_fine_block(&self, fine_block: &Rc<RefCell<FineBlock>>) {
		self.fine_blocks_that_are_subsets_of_self
			.borrow_mut()
			.retain(|x| !Rc::ptr_eq(x, fine_block));
	}
	fn fine_block_count(&self) -> usize {
		self.fine_blocks_that_are_subsets_of_self.borrow().len()
	}
}

struct CounterImageGroup {
	block: Rc<RefCell<FineBlock>>,
	subblock: Block,
}

trait HasValues {
	fn values(&self) -> Block;
}

impl HasValues for FineBlockPointer {
	fn values(&self) -> Block {
		(**self).borrow().values.clone()
	}
}

impl HasValues for CoarseBlock {
	fn values(&self) -> Block {
		self.values.clone()
	}
}

fn initialization<const N: usize, G>(
	graphs: &[&G; N]
) -> ( (FineBlockPointer, FineBlockPointer),
	   CoarsePartition,
	   NodeToBlockVec,
	   Translator<G::NodeId, NodeType, NodeState<N>> )
where
	G: IntoNodeReferences + IntoEdges + IntoNeighborsDirected,
	G::NodeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeRef: PartialEq,
{
	// we translate into unique ids
	let mut convert_nodes: Translator<G::NodeId, NodeType, NodeState<N>>
		= Translator::new();

	let graph_node_indices = {
		let mut tmp: Block = vec![];

		for (pos, graph) in graphs.iter().enumerate() {
			tmp.extend(
				graph.node_identifiers()
					.map(|val| convert_nodes.encode(val, pos as u32))
					.collect::<Vec<_>>()
			);
		}

		tmp
	};

	let coarse_initial_block_pointer: Rc<CoarseBlock> = {
		let coarse_initial_block = CoarseBlock {
			values: graph_node_indices.clone(),
			fine_blocks_that_are_subsets_of_self: RefCell::new(vec![]),
		};

		Rc::new(coarse_initial_block)
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
					graphs[x.graph() as usize]
						.neighbors_directed(
							*convert_nodes.decode(x).unwrap(),
							Outgoing)
						.count() == 0
				}
			);

		let leaf_node_block = FineBlock {
			values: leaf_node_indices,
			coarse_block_that_supersets_self: Rc::clone(&coarse_initial_block_pointer),
		};
		let non_leaf_node_block = FineBlock {
			values: non_leaf_node_indices,
			coarse_block_that_supersets_self: Rc::clone(&coarse_initial_block_pointer),
		};

		(
			Rc::new(RefCell::new(leaf_node_block)),
			Rc::new(RefCell::new(non_leaf_node_block)),
		)
	};

	coarse_initial_block_pointer
		.fine_blocks_that_are_subsets_of_self
		.borrow_mut()
		.extend([
			Rc::clone(&leaf_node_block_pointer),
			Rc::clone(&non_leaf_node_block_pointer),
		]);

	let node_to_block_vec = {
		let mut tmp = HashMap::new();

		(*non_leaf_node_block_pointer)
			.borrow()
			.values
			.iter()
			.copied()
			.for_each(
				|value|
				{ tmp.insert(value, Rc::clone(&non_leaf_node_block_pointer)); }
			);

		(*leaf_node_block_pointer)
			.borrow()
			.values
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
		vec![coarse_initial_block_pointer],
		node_to_block_vec,
		convert_nodes
	)
}

fn build_counterimage<IndexHolder: HasValues, const N:usize, G>(
	graphs: &[&G; N],
	fine_block: IndexHolder,
	convert_nodes: &Translator<G::NodeId, NodeType, NodeState<N>>
) -> CounterImage
where
	G: IntoNodeReferences + IntoEdges + IntoNeighborsDirected,
	G::NodeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeRef: PartialEq,
{
	let mut counterimage = HashMap::new();

	fine_block.values().iter().for_each(|node_index_pointer| {
		counterimage.insert(
			*node_index_pointer,
			graphs[node_index_pointer.graph() as usize]
				.neighbors_directed(
					*convert_nodes.decode(node_index_pointer).unwrap(),
					Incoming)
				.collect::<HashSet<_>>()
				.into_iter()
				.map(|e| convert_nodes.get(&e).unwrap())
				.copied()
				.collect::<Vec<_>>(),
		);
	});

	counterimage
}

fn group_by_counterimage(
	counterimage: CounterImage,
	node_to_block: &NodeToBlockVec,
) -> CounterimageGrouped {
	let mut counterimage_grouped: CounterimageGrouped = HashMap::new();

	for incoming_neighbor_group in counterimage.values() {
		for node in incoming_neighbor_group {
			let block = Rc::clone(node_to_block.get(node).unwrap());
			let key = (*block).borrow().values.clone();

			match counterimage_grouped.entry(key) {
				Entry::Occupied(mut entry) => entry.get_mut().subblock.push(*node),
				Entry::Vacant(entry) => {
					entry.insert(CounterImageGroup {
						block: Rc::clone(&block),
						subblock: Vec::from([*node]),
					});
				}
			}
		}
	}

	counterimage_grouped
}

fn split_blocks_with_grouped_counterimage(
	mut counterimage_grouped: CounterimageGrouped,
	node_to_block_vec: &mut NodeToBlockVec,
) -> (
	(Vec<FineBlockPointer>, Vec<FineBlockPointer>),
	Vec<CoarseBlockPointer>,
) {
	let mut all_new_fine_blocks: Vec<Rc<RefCell<FineBlock>>> = vec![];
	let mut all_removed_fine_blocks: Vec<Rc<RefCell<FineBlock>>> = vec![];
	let mut new_compound_coarse_blocks: Vec<Rc<CoarseBlock>> = vec![];

	for (block, counter_image_group) in counterimage_grouped.iter_mut() {
		let borrowed_coarse_block = Rc::clone(
			&(*counter_image_group.block)
				.borrow()
				.coarse_block_that_supersets_self,
		);

		let proper_subblock = {
			let fine_block = FineBlock {
				values: counter_image_group.subblock.clone(),
				coarse_block_that_supersets_self: Rc::clone(&borrowed_coarse_block),
			};

			Rc::new(RefCell::new(fine_block))
		};
		let prior_count = borrowed_coarse_block.fine_block_count();
		borrowed_coarse_block.add_fine_block(Rc::clone(&proper_subblock));

		if prior_count == 1 {
			new_compound_coarse_blocks.push(Rc::clone(&borrowed_coarse_block));
		}

		for node_index in counter_image_group.subblock.iter() {
			node_to_block_vec.insert(*node_index, Rc::clone(&proper_subblock));
		}

		// subtract subblock from block
		(*counter_image_group.block).borrow_mut().values = block
			.iter()
			.filter(|x| !(*proper_subblock).borrow().values.contains(x))
			.copied()
			.collect();

		if (*counter_image_group.block).borrow().values.is_empty() {
			borrowed_coarse_block.remove_fine_block(&counter_image_group.block);
			all_removed_fine_blocks.push(Rc::clone(&counter_image_group.block));
		}
		all_new_fine_blocks.push(Rc::clone(&proper_subblock));
	}
	(
		(all_new_fine_blocks, all_removed_fine_blocks),
		new_compound_coarse_blocks,
	)
}

fn maximum_bisimulation<const N: usize, G>(
	graphs: &[&G; N]
) -> Option<Vec<Block>>
where
	G: IntoNodeReferences + IntoEdges + IntoNeighborsDirected,
	G::NodeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeId: std::cmp::Eq + std::hash::Hash,
	G::EdgeRef: PartialEq,
{
	let (fine_block_tuple,
		 initial_coarse_partition,
		 mut node_to_block_vec,
		 converter) = initialization(graphs);

	let mut queue: CoarsePartition = initial_coarse_partition;
	let mut all_fine_blocks = vec![fine_block_tuple.0, fine_block_tuple.1];

	loop {
		let (smaller_component, simple_splitter_block) = {
			let splitter_block = match queue.pop() {
				Some(coarse_block) => coarse_block,
				None => break,
			};
			let mut fine_blocks_in_splitter_block = splitter_block
				.fine_blocks_that_are_subsets_of_self
				.borrow()
				.clone();

			let smaller_component_index = fine_blocks_in_splitter_block
				.iter()
				.enumerate()
				.min_by(|(_, x), (_, y)| {
					(***x)
						.borrow()
						.values
						.len()
						.cmp(&(***y).borrow().values.len())
				})
				.map(|(index, _)| index)?;

			let smaller_component = fine_blocks_in_splitter_block.remove(smaller_component_index);

			let simple_splitter_block_values: Block = splitter_block
				.values
				.clone()
				.iter()
				.filter(|x| !(*smaller_component).borrow().values.contains(x))
				.copied()
				.collect();

			let simple_splitter_block = CoarseBlock {
				values: simple_splitter_block_values,
				fine_blocks_that_are_subsets_of_self: RefCell::new(fine_blocks_in_splitter_block),
			};
			let simple_splitter_block_pointer = Rc::new(simple_splitter_block);

			if simple_splitter_block_pointer
				.fine_blocks_that_are_subsets_of_self
				.borrow()
				.len()
				> 1
			{
				queue.push(Rc::clone(&simple_splitter_block_pointer));
			}

			(smaller_component, simple_splitter_block_pointer)
		};
		simple_splitter_block
			.fine_blocks_that_are_subsets_of_self
			.borrow()
			.iter()
			.for_each(|x| {
				(*x).borrow_mut().coarse_block_that_supersets_self =
					Rc::clone(&simple_splitter_block);
			});

		let mut counterimage = build_counterimage(graphs, smaller_component, &converter);

		let counterimage_group = group_by_counterimage(counterimage.clone(), &node_to_block_vec);
		let ((new_fine_blocks, removeable_fine_blocks), coarse_block_that_are_now_compound) =
			split_blocks_with_grouped_counterimage(counterimage_group, &mut node_to_block_vec);

		all_fine_blocks.extend(new_fine_blocks);
		all_fine_blocks.retain(|x| !removeable_fine_blocks.iter().any(|y| Rc::ptr_eq(x, y)));
		queue.extend(coarse_block_that_are_now_compound);

		// counterimage = E^{-1}(B) - E^{-1}(S-B)
		{
			let counterimage_splitter_complement =
				build_counterimage(graphs, (*simple_splitter_block).clone(), &converter);

			counterimage_splitter_complement.keys().for_each(|node| {
				counterimage.remove(node);
			});
		}

		let counterimage_group = group_by_counterimage(counterimage, &node_to_block_vec);
		let ((new_fine_blocks, removeable_fine_blocks), coarse_block_that_are_now_compound) =
			split_blocks_with_grouped_counterimage(counterimage_group, &mut node_to_block_vec);

		all_fine_blocks.extend(new_fine_blocks);
		all_fine_blocks.retain(|x| !removeable_fine_blocks.iter().any(|y| Rc::ptr_eq(x, y)));
		queue.extend(coarse_block_that_are_now_compound);
	}

	Some(
		all_fine_blocks
			.iter()
			.map(|x| (**x).borrow().values.clone())
			.filter(|x| !x.is_empty()) // remove leaf block when there are no leaves
			.collect(),
	)
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
{
	if graph_a.node_count() == 0 && graph_b.node_count() == 0 {
		return true
	}
	if graph_a.node_count() == 0 || graph_b.node_count() == 0 {
		return false
	}

	let result =
		match maximum_bisimulation(&[graph_a, graph_b]) {
			None => {return false},
			Some(val) => {
				val.into_iter()
					.find(
						|el| {
							let mut keep_track = [false, false];
							for e in el {
								keep_track[e.graph() as usize] = true;
							}
							!keep_track[0] || !keep_track[1]
						}
					)

			}
		};

	println!("{:?}", result);

	result.is_none()
}

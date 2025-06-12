#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

// -----------------------------------------------------------------------------
// RSset
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSset<'a> {
    identifiers: HashSet<&'a str>
}

impl<'a, const N: usize> From<[&'a str; N]> for RSset<'a> {
    fn from(arr: [&'a str; N]) -> Self {
	RSset{identifiers: HashSet::from(arr)}
    }
}

impl<'a> From<&[&'a str]> for RSset<'a> {
    fn from(arr: &[&'a str]) -> Self {
	RSset{identifiers: HashSet::from_iter(arr.to_vec())}
    }
}

impl<'a> From<Vec<&'a str>> for RSset<'a> {
    fn from(arr: Vec<&'a str>) -> Self {
	RSset{identifiers: HashSet::from_iter(arr)}
    }
}

impl<'a> RSset<'a> {
    pub fn new() -> Self {
	RSset{identifiers: HashSet::new()}
    }

    pub fn is_subset(&self, b: &RSset<'a>) -> bool {
	self.identifiers.is_subset(&b.identifiers)
    }

    pub fn is_disjoint(&self, b: &RSset<'a>) -> bool {
	self.identifiers.is_disjoint(&b.identifiers)
    }

    pub fn union(&self, b: &RSset<'a>) -> RSset<'a> {
	// TODO maybe find more efficient way without copy/clone
	let mut ret: RSset = b.clone();
	ret.identifiers.extend(self.identifiers.iter());
	ret
    }

    pub fn union_option(&self, b: Option<&RSset<'a>>) -> RSset<'a> {
	if let Some(b) = b {
	    self.union(b)
	} else {
	    self.clone()
	}
    }

    pub fn intersection(&self, b: &RSset<'a>) -> RSset<'a> {
	// TODO maybe find more efficient way without copy/clone
	let res: HashSet<_> = b.identifiers.intersection(&self.identifiers)
					   .copied()
					   .collect();
	RSset { identifiers: res }
    }

    pub fn subtraction(&self, b: &RSset<'a>) -> RSset<'a> {
	// TODO maybe find more efficient way without copy/clone
	let res: HashSet<_> = self.identifiers.difference(&b.identifiers)
					      .copied()
					      .collect();
	RSset { identifiers: res }
    }
}


// -----------------------------------------------------------------------------
// RSreaction
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSreaction<'a> {
    reactants: RSset<'a>,
    inihibitors: RSset<'a>,
    products: RSset<'a>
}

impl<'a> RSreaction<'a> {
    pub fn new() -> Self {
	RSreaction{ reactants:   RSset::new(),
		    inihibitors: RSset::new(),
		    products:    RSset::new(), }
    }

    pub fn from(reactants: RSset<'a>, inihibitors: RSset<'a>, products: RSset<'a>) -> Self {
	RSreaction{ reactants,
		    inihibitors,
		    products }
    }

    pub fn enabled(&self, current_state: &RSset<'a>) -> bool {
	self.reactants.is_subset(current_state)
	    && self.inihibitors.is_disjoint(current_state)
    }

    pub fn products_clone(&self) -> RSset<'a> {
	self.products.clone()
    }

    pub fn reactants(&self) -> &RSset<'a> {
	&self.reactants
    }

    pub fn inihibitors(&self) -> &RSset<'a> {
	&self.inihibitors
    }

    pub fn products(&self) -> &RSset<'a> {
	&self.products
    }
}



// -----------------------------------------------------------------------------
// RSprocess
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum RSprocess<'a> {
    Nill,
    RecursiveIdentifier{identifier: &'a str},
    EntitySet{entities: RSset<'a>, next_process: Rc<RSprocess<'a>>},
    WaitEntity{repeat: i64, repeated_process: Rc<RSprocess<'a>>, next_process: Rc<RSprocess<'a>>},
    Summation{children: Vec<RSprocess<'a>>},
    NondeterministicChoice{children: Vec<Rc<RSprocess<'a>>>}
}

impl<'a> RSprocess<'a> {
    // TODO: remove all the clone()
    pub fn concat(&self, new: &RSprocess<'a>) -> RSprocess<'a>{
	match (self, new) {
	    (RSprocess::NondeterministicChoice{children: c1}, RSprocess::NondeterministicChoice{children: c2}) => {
		RSprocess::NondeterministicChoice { children: [c1.clone(), c2.clone()].concat() }
	    },
	    (RSprocess::NondeterministicChoice{children}, new) |
	    (new, RSprocess::NondeterministicChoice{children}) => {
		let mut new_children = children.clone();
		new_children.push(Rc::new(new.clone()));
		RSprocess::NondeterministicChoice{ children: new_children }
	    },
	    (_, _) => {
		RSprocess::NondeterministicChoice { children: vec![Rc::new(self.clone()), Rc::new(new.clone())] }
	    }
	}
    }
}

#[derive(Clone, Debug)]
pub struct RSChoices<'a> {
    context_moves: Vec<(Rc<RSset<'a>>, Rc<RSprocess<'a>>)>
}

impl<'a> RSChoices<'a> {
    pub fn new() -> Self {
	RSChoices{ context_moves: vec![] }
    }

    pub fn new_not_empty() -> Self {
	RSChoices{ context_moves: vec![(Rc::new(RSset::new()), Rc::new(RSprocess::Nill))] }
    }

    pub fn append(&mut self, a: &mut RSChoices<'a>) {
	self.context_moves.append(&mut a.context_moves);
    }

    pub fn replace(&mut self, a: Rc<RSprocess<'a>>) {
	self.context_moves =
	    self.context_moves
	    .iter_mut()
	    .map(|(c1, _)| (Rc::clone(c1), Rc::clone(&a))).collect::<Vec<_>>();
    }

    pub fn shuffle(&mut self, choices: RSChoices<'a>) {
	match (self.context_moves.is_empty(), choices.context_moves.is_empty()) {
	    (true, true) => {}
	    (true, false) => { self.context_moves = choices.context_moves }
	    (false, true) => {}
	    (false, false) => {
		let mut new_self = vec![];
		for item_self in &self.context_moves {
		    for item_choices in &choices.context_moves {
			new_self.push((Rc::new(item_self.0.union(&item_choices.0)),
				       Rc::new(item_self.1.concat(&item_choices.1))));
		    }
		}
		self.context_moves = new_self;
	    }
	}
    }
}

impl<'a> IntoIterator for RSChoices<'a> {
    type Item = (Rc<RSset<'a>>, Rc<RSprocess<'a>>);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
	self.context_moves.into_iter()
    }
}

impl<'a, const N: usize> From<[(Rc<RSset<'a>>, Rc<RSprocess<'a>>); N]> for RSChoices<'a> {
    fn from(arr: [(Rc<RSset<'a>>, Rc<RSprocess<'a>>); N]) -> Self {
	RSChoices{context_moves: arr.to_vec()}
    }
}

impl<'a> From<&[(Rc<RSset<'a>>, Rc<RSprocess<'a>>)]> for RSChoices<'a> {
    fn from(arr: &[(Rc<RSset<'a>>, Rc<RSprocess<'a>>)]) -> Self {
	RSChoices{context_moves: arr.to_vec()}
    }
}

impl<'a> From<Vec<(Rc<RSset<'a>>, Rc<RSprocess<'a>>)>> for RSChoices<'a> {
    fn from(arr: Vec<(Rc<RSset<'a>>, Rc<RSprocess<'a>>)>) -> Self {
	RSChoices{context_moves: arr}
    }
}

// -----------------------------------------------------------------------------
// RSenvironment
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSenvironment<'a> {
    definitions: HashMap<&'a str, RSprocess<'a>>,
}

impl<'a> RSenvironment<'a> {
    pub fn new() -> RSenvironment<'a> {
	RSenvironment{definitions: HashMap::new()}
    }

    pub fn get(&self, k: &'a str) -> Option<&RSprocess<'a>> {
	self.definitions.get(k)
    }
}

impl<'a, const N: usize> From<[(&'a str, RSprocess<'a>); N]> for RSenvironment<'a> {
    fn from(arr: [(&'a str, RSprocess<'a>); N]) -> Self {
	RSenvironment{definitions: HashMap::from(arr)}
    }
}

impl<'a> From<&[(&'a str, RSprocess<'a>)]> for RSenvironment<'a> {
    fn from(arr: &[(&'a str, RSprocess<'a>)]) -> Self {
	RSenvironment{definitions: HashMap::from_iter(arr.to_vec())}
    }
}

impl<'a> From<Vec<(&'a str, RSprocess<'a>)>> for RSenvironment<'a> {
    fn from(arr: Vec<(&'a str, RSprocess<'a>)>) -> Self {
	RSenvironment{definitions: HashMap::from_iter(arr)}
    }
}

// -----------------------------------------------------------------------------
// RSsystem
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSsystem<'a> {
    delta: RSenvironment<'a>,
    available_entities: RSset<'a>,
    context_process: RSprocess<'a>,
    reaction_rules: Vec<RSreaction<'a>>,
}

impl<'a> RSsystem<'a> {
    pub fn new() -> RSsystem<'a> {
	RSsystem {
	    delta: RSenvironment::new(),
	    available_entities: RSset::new(),
	    context_process: RSprocess::Nill,
	    reaction_rules: vec![],
	}
    }

    pub fn from(delta: RSenvironment<'a>,
		available_entities: RSset<'a>,
		context_process: RSprocess<'a>,
		reaction_rules: Vec<RSreaction<'a>>) -> RSsystem<'a> {
	RSsystem { delta, available_entities, context_process, reaction_rules }
    }

    pub fn get_delta(&self) -> &RSenvironment<'a> {
	&self.delta
    }

    pub fn get_available_entities(&self) -> &RSset<'a> {
	&self.available_entities
    }

    pub fn get_context_process(&self) -> &RSprocess<'a> {
	&self.context_process
    }

    pub fn get_reaction_rules(&self) -> &Vec<RSreaction<'a>> {
	&self.reaction_rules
    }
}

// -----------------------------------------------------------------------------
// RSsystem
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSlabel<'a> {
    available_entities: RSset<'a>,
    c: RSset<'a>, /// TODO: what is c? what is t? (c comes from choices, t is
    t: RSset<'a>, ///       the union of available_entities and c)
    reactants: RSset<'a>,
    reactantsi: RSset<'a>,
    inihibitors: RSset<'a>,
    ireactants: RSset<'a>,
    products: RSset<'a>,
}

impl<'a> RSlabel<'a> {
    pub fn new() -> Self {
	RSlabel { available_entities: RSset::new(),
		  c: RSset::new(),
		  t: RSset::new(),
		  reactants: RSset::new(),
		  reactantsi: RSset::new(),
		  inihibitors: RSset::new(),
		  ireactants: RSset::new(),
		  products: RSset::new() }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn from(available_entities: RSset<'a>,
		c: RSset<'a>,
		t: RSset<'a>,
		reactants: RSset<'a>,
		reactantsi: RSset<'a>,
		inihibitors: RSset<'a>,
		ireactants: RSset<'a>,
		products: RSset<'a>,) -> Self {
	RSlabel { available_entities,
		  c,
		  t,
		  reactants,
		  reactantsi,
		  inihibitors,
		  ireactants,
		  products }
    }
}


// -----------------------------------------------------------------------------
// RSassertOp
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum RSassertOp {
    InW,
    InR,
    InI,
    InP
}

// -----------------------------------------------------------------------------
// RSassert
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum RSassert<'a> {
    Not(Box<RSassert<'a>>),
    Xor(Box<RSassert<'a>>, Box<RSassert<'a>>),
    Or(Vec<RSassert<'a>>),
    And(Vec<RSassert<'a>>),
    Sub(RSset<'a>, RSassertOp),
    NonEmpty(RSassertOp)
}

// -----------------------------------------------------------------------------
// RSBHML
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum RSBHML<'a> {
    True,
    False,
    Or(Vec<RSBHML<'a>>),
    And(Vec<RSBHML<'a>>),
    Diamond(Box<RSassert<'a>>, Box<RSBHML<'a>>),
    Box(Box<RSassert<'a>>, Box<RSBHML<'a>>)
}

use std::collections::{BTreeMap, HashSet};

// -----------------------------------------------------------------------------
// RSset
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
#[allow(dead_code)]
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
	// TODO maybe find more efficient way
	let mut ret: RSset = b.clone();
	ret.identifiers.extend(self.identifiers.iter());
	ret
    }
}


// -----------------------------------------------------------------------------
// RSreaction
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
#[allow(dead_code)]
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
}



// -----------------------------------------------------------------------------
// RSprocess
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum RSprocess<'a> {
    Nill,
    ConstantIdentifier{identifier: &'a str},
    EntitySet{entities: RSset<'a>, next_process: Box<RSprocess<'a>>},
    WaitEntity{repeat: i64, repeated_process: Box<RSprocess<'a>>, next_process: Box<RSprocess<'a>>},
    NondeterministicChoice{children: Vec<RSprocess<'a>>},
    Summation{children: Vec<RSprocess<'a>>}
}


// -----------------------------------------------------------------------------
// RSenvironment
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct RSenvironment<'a> {
    definitions: BTreeMap<&'a str, Box<RSprocess<'a>>>,
}

impl<'a> RSenvironment<'a> {
    pub fn new() -> RSenvironment<'a> {
	RSenvironment{definitions: BTreeMap::new()}
    }
}

impl<'a, const N: usize> From<[(&'a str, Box<RSprocess<'a>>); N]> for RSenvironment<'a> {
    fn from(arr: [(&'a str, Box<RSprocess<'a>>); N]) -> Self {
	RSenvironment{definitions: BTreeMap::from(arr)}
    }
}

impl<'a> From<&[(&'a str, Box<RSprocess<'a>>)]> for RSenvironment<'a> {
    fn from(arr: &[(&'a str, Box<RSprocess<'a>>)]) -> Self {
	RSenvironment{definitions: BTreeMap::from_iter(arr.to_vec())}
    }
}

impl<'a> From<Vec<(&'a str, Box<RSprocess<'a>>)>> for RSenvironment<'a> {
    fn from(arr: Vec<(&'a str, Box<RSprocess<'a>>)>) -> Self {
	RSenvironment{definitions: BTreeMap::from_iter(arr)}
    }
}

// -----------------------------------------------------------------------------
// RSassertOp
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
#[allow(dead_code)]
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
#[allow(dead_code)]
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
#[allow(dead_code)]
#[allow(clippy::upper_case_acronyms)]
pub enum RSBHML<'a> {
    True,
    False,
    Or(Vec<RSBHML<'a>>),
    And(Vec<RSBHML<'a>>),
    Diamond(Box<RSassert<'a>>, Box<RSBHML<'a>>),
    Box(Box<RSassert<'a>>, Box<RSBHML<'a>>)
}

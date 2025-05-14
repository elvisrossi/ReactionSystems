use std::collections::BTreeMap;

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct RSset<'a> {
    identifiers: Vec<&'a str>
}

impl<'a, const N: usize> From<[&'a str; N]> for RSset<'a> {
    fn from(mut arr: [&'a str; N]) -> Self {
	arr.sort();
	RSset{identifiers: arr.to_vec()}
    }
}

impl<'a> From<&[&'a str]> for RSset<'a> {
    fn from(arr: &[&'a str]) -> Self {
	let mut tmp = arr.to_vec();
	tmp.sort();
	RSset{identifiers: tmp}
    }
}

impl<'a> From<Vec<&'a str>> for RSset<'a> {
    fn from(mut arr: Vec<&'a str>) -> Self {
	arr.sort();
	RSset{identifiers: arr}
    }
}


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

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum RSassertOp {
    InW,
    InR,
    InI,
    InP
}


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

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum RSBHML<'a> {
    True,
    False,
    Or(Vec<RSBHML<'a>>),
    And(Vec<RSBHML<'a>>),
    Diamond(Box<RSassert<'a>>, Box<RSBHML<'a>>),
    Box(Box<RSassert<'a>>, Box<RSBHML<'a>>)
}

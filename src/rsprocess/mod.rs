use std::collections::BTreeMap;

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum RSprocess<'a> {
    Nill,
    ConstantIdentifier{identifier: &'a str},
    EntitySet{entities: Vec<&'a str>, next_process: Box<RSprocess<'a>>},
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
    Sub(Vec<&'a str>, RSassertOp),
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

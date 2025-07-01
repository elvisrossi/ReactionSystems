#![allow(dead_code)]

use super::translator::IdType;
use std::collections::{BTreeSet, HashMap, VecDeque};
use std::hash::Hash;
use std::rc::Rc;

// -----------------------------------------------------------------------------
// RSset
// -----------------------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RSset {
    identifiers: BTreeSet<IdType>,
}

impl<const N: usize> From<[IdType; N]> for RSset {
    fn from(arr: [IdType; N]) -> Self {
        RSset {
            identifiers: BTreeSet::from(arr),
        }
    }
}

impl From<&[IdType]> for RSset {
    fn from(arr: &[IdType]) -> Self {
        RSset {
            identifiers: BTreeSet::from_iter(arr.to_vec()),
        }
    }
}

impl From<Vec<IdType>> for RSset {
    fn from(arr: Vec<IdType>) -> Self {
        RSset {
            identifiers: BTreeSet::from_iter(arr),
        }
    }
}

impl RSset {
    pub fn new() -> Self {
        RSset {
            identifiers: BTreeSet::new(),
        }
    }

    pub fn is_subset(&self, b: &RSset) -> bool {
        self.identifiers.is_subset(&b.identifiers)
    }

    pub fn is_disjoint(&self, b: &RSset) -> bool {
        self.identifiers.is_disjoint(&b.identifiers)
    }

    pub fn union(&self, b: &RSset) -> RSset {
        // TODO maybe find more efficient way without copy/clone
        let mut ret: RSset = b.clone();
        ret.identifiers.extend(self.identifiers.iter());
        ret
    }

    pub fn union_option(&self, b: Option<&RSset>) -> RSset {
        if let Some(b) = b {
            self.union(b)
        } else {
            self.clone()
        }
    }

    pub fn intersection(&self, b: &RSset) -> RSset {
        // TODO maybe find more efficient way without copy/clone
        let res: BTreeSet<_> = b
            .identifiers
            .intersection(&self.identifiers)
            .copied()
            .collect();
        RSset { identifiers: res }
    }

    pub fn subtraction(&self, b: &RSset) -> RSset {
        // TODO maybe find more efficient way without copy/clone
        let res: BTreeSet<_> = self
            .identifiers
            .difference(&b.identifiers)
            .copied()
            .collect();
        RSset { identifiers: res }
    }

    pub fn set(&self) -> &BTreeSet<IdType> {
        &self.identifiers
    }

    pub fn iter(&self) -> std::collections::btree_set::Iter<'_, IdType> {
        self.identifiers.iter()
    }

    pub fn len(&self) -> usize {
        self.identifiers.len()
    }

    pub fn insert(&mut self, el: IdType) -> bool {
        self.identifiers.insert(el)
    }

    pub fn push(&mut self, b: &RSset) {
        self.identifiers.extend(b.iter())
    }
}

impl Default for RSset {
    fn default() -> Self {
        RSset::new()
    }
}

impl IntoIterator for RSset {
    type Item = IdType;
    type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.identifiers.into_iter()
    }
}

// -----------------------------------------------------------------------------
// RSreaction
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSreaction {
    reactants: RSset,
    inihibitors: RSset,
    products: RSset,
}

impl RSreaction {
    pub fn new() -> Self {
        RSreaction {
            reactants: RSset::new(),
            inihibitors: RSset::new(),
            products: RSset::new(),
        }
    }

    pub fn from(reactants: RSset, inihibitors: RSset, products: RSset) -> Self {
        RSreaction {
            reactants,
            inihibitors,
            products,
        }
    }

    // see enable
    pub fn enabled(&self, current_state: &RSset) -> bool {
        self.reactants.is_subset(current_state)
	    && self.inihibitors.is_disjoint(current_state)
    }

    pub fn products_clone(&self) -> RSset {
        self.products.clone()
    }

    pub fn reactants(&self) -> &RSset {
        &self.reactants
    }

    pub fn inihibitors(&self) -> &RSset {
        &self.inihibitors
    }

    pub fn products(&self) -> &RSset {
        &self.products
    }
}

impl Default for RSreaction {
    fn default() -> Self {
        RSreaction::new()
    }
}

// -----------------------------------------------------------------------------
// RSprocess
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum RSprocess {
    Nill,
    RecursiveIdentifier {
        identifier: IdType,
    },
    EntitySet {
        entities: RSset,
        next_process: Rc<RSprocess>,
    },
    WaitEntity {
        repeat: i64,
        repeated_process: Rc<RSprocess>,
        next_process: Rc<RSprocess>,
    },
    Summation {
        children: Vec<Rc<RSprocess>>,
    },
    NondeterministicChoice {
        children: Vec<Rc<RSprocess>>,
    },
}

impl RSprocess {
    // TODO: remove all the clone()
    pub fn concat(&self, new: &RSprocess) -> RSprocess {
        match (self, new) {
            (
                RSprocess::NondeterministicChoice { children: c1 },
                RSprocess::NondeterministicChoice { children: c2 },
            ) => RSprocess::NondeterministicChoice {
                children: [c1.clone(), c2.clone()].concat(),
            },
            (RSprocess::NondeterministicChoice { children }, new)
            | (new, RSprocess::NondeterministicChoice { children }) => {
                let mut new_children = children.clone();
                new_children.push(Rc::new(new.clone()));
                RSprocess::NondeterministicChoice {
                    children: new_children,
                }
            }
            (_, _) => RSprocess::NondeterministicChoice {
                children: vec![Rc::new(self.clone()), Rc::new(new.clone())],
            },
        }
    }

    pub fn all_elements(&self) -> RSset {
        let mut queue = VecDeque::from([self]);
        let mut elements = RSset::new();

        while let Some(el) = queue.pop_front() {
            match el {
                Self::Nill => {}
                Self::RecursiveIdentifier { identifier: _ } => {}
                Self::EntitySet {
                    entities,
                    next_process,
                } => {
                    elements.push(entities);
                    queue.push_back(next_process);
                }
                Self::WaitEntity {
                    repeat: _,
                    repeated_process,
                    next_process,
                } => {
                    queue.push_back(repeated_process);
                    queue.push_back(next_process);
                }
                Self::Summation { children } => {
                    for c in children {
                        queue.push_back(c);
                    }
                }
                Self::NondeterministicChoice { children } => {
                    for c in children {
                        queue.push_back(c);
                    }
                }
            }
        }
        elements
    }
}

// -----------------------------------------------------------------------------
// RSchoices
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSchoices {
    context_moves: Vec<(Rc<RSset>, Rc<RSprocess>)>,
}

impl RSchoices {
    pub fn new() -> Self {
        RSchoices {
            context_moves: vec![],
        }
    }

    pub fn new_not_empty() -> Self {
        RSchoices {
            context_moves: vec![(Rc::new(RSset::new()),
				 Rc::new(RSprocess::Nill))],
        }
    }

    pub fn append(&mut self, a: &mut RSchoices) {
        self.context_moves.append(&mut a.context_moves);
    }

    pub fn replace(&mut self, a: Rc<RSprocess>) {
        self.context_moves = self
            .context_moves
            .iter_mut()
            .map(|(c1, _)| (Rc::clone(c1), Rc::clone(&a)))
            .collect::<Vec<_>>();
    }

    pub fn shuffle(&mut self, choices: RSchoices) {
        match (
            self.context_moves.is_empty(),
            choices.context_moves.is_empty(),
        ) {
            (true, true) => {}
            (true, false) => self.context_moves = choices.context_moves,
            (false, true) => {}
            (false, false) => {
                let mut new_self = vec![];
                for item_self in &self.context_moves {
                    for item_choices in &choices.context_moves {
                        new_self.push((
                            Rc::new(item_self.0.union(&item_choices.0)),
                            Rc::new(item_self.1.concat(&item_choices.1)),
                        ));
                    }
                }
                self.context_moves = new_self;
            }
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, (Rc<RSset>, Rc<RSprocess>)> {
        self.context_moves.iter()
    }
}

impl IntoIterator for RSchoices {
    type Item = (Rc<RSset>, Rc<RSprocess>);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.context_moves.into_iter()
    }
}

impl<const N: usize> From<[(Rc<RSset>, Rc<RSprocess>); N]> for RSchoices {
    fn from(arr: [(Rc<RSset>, Rc<RSprocess>); N]) -> Self {
        RSchoices {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<&[(Rc<RSset>, Rc<RSprocess>)]> for RSchoices {
    fn from(arr: &[(Rc<RSset>, Rc<RSprocess>)]) -> Self {
        RSchoices {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<Vec<(Rc<RSset>, Rc<RSprocess>)>> for RSchoices {
    fn from(arr: Vec<(Rc<RSset>, Rc<RSprocess>)>) -> Self {
        RSchoices { context_moves: arr }
    }
}

// -----------------------------------------------------------------------------
// RSenvironment
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSenvironment {
    definitions: HashMap<IdType, RSprocess>,
}

impl RSenvironment {
    pub fn new() -> RSenvironment {
        RSenvironment {
            definitions: HashMap::new(),
        }
    }

    pub fn get(&self, k: IdType) -> Option<&RSprocess> {
        self.definitions.get(&k)
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, u32, RSprocess> {
        self.definitions.iter()
    }

    pub fn all_elements(&self) -> RSset {
        let mut acc = RSset::new();
        for (_, process) in self.definitions.iter() {
            acc.push(&process.all_elements());
        }
        acc
    }
}

impl Default for RSenvironment {
    fn default() -> Self {
        RSenvironment::new()
    }
}

impl<const N: usize> From<[(IdType, RSprocess); N]> for RSenvironment {
    fn from(arr: [(IdType, RSprocess); N]) -> Self {
        RSenvironment {
            definitions: HashMap::from(arr),
        }
    }
}

impl From<&[(IdType, RSprocess)]> for RSenvironment {
    fn from(arr: &[(IdType, RSprocess)]) -> Self {
        RSenvironment {
            definitions: HashMap::from_iter(arr.to_vec()),
        }
    }
}

impl From<Vec<(IdType, RSprocess)>> for RSenvironment {
    fn from(arr: Vec<(IdType, RSprocess)>) -> Self {
        RSenvironment {
            definitions: HashMap::from_iter(arr),
        }
    }
}

// -----------------------------------------------------------------------------
// RSsystem
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSsystem {
    delta: Rc<RSenvironment>,
    available_entities: RSset,
    context_process: RSprocess,
    reaction_rules: Rc<Vec<RSreaction>>,
}

impl RSsystem {
    pub fn new() -> RSsystem {
        RSsystem {
            delta: Rc::new(RSenvironment::new()),
            available_entities: RSset::new(),
            context_process: RSprocess::Nill,
            reaction_rules: Rc::new(vec![]),
        }
    }

    pub fn from(
        delta: Rc<RSenvironment>,
        available_entities: RSset,
        context_process: RSprocess,
        reaction_rules: Rc<Vec<RSreaction>>,
    ) -> RSsystem {
        RSsystem {
            delta: Rc::clone(&delta),
            available_entities,
            context_process,
            reaction_rules: Rc::clone(&reaction_rules),
        }
    }

    pub fn get_delta(&self) -> &Rc<RSenvironment> {
        &self.delta
    }

    pub fn get_available_entities(&self) -> &RSset {
        &self.available_entities
    }

    pub fn get_context_process(&self) -> &RSprocess {
        &self.context_process
    }

    pub fn get_reaction_rules(&self) -> &Rc<Vec<RSreaction>> {
        &self.reaction_rules
    }
}

impl Default for RSsystem {
    fn default() -> Self {
        RSsystem::new()
    }
}

// -----------------------------------------------------------------------------
// RSsystem
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSlabel {
    pub available_entities: RSset,
    pub context: RSset,
    pub t: RSset,
    ///      union of available_entities and context
    pub reactants: RSset,
    pub reactantsi: RSset, // reactants absent
    pub inihibitors: RSset,
    pub ireactants: RSset, // inhibitors present
    pub products: RSset,
}

impl RSlabel {
    pub fn new() -> Self {
        RSlabel {
            available_entities: RSset::new(),
            context: RSset::new(),
            t: RSset::new(),
            reactants: RSset::new(),
            reactantsi: RSset::new(),
            inihibitors: RSset::new(),
            ireactants: RSset::new(),
            products: RSset::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn from(
        available_entities: RSset,
        context: RSset,
        t: RSset,
        reactants: RSset,
        reactantsi: RSset,
        inihibitors: RSset,
        ireactants: RSset,
        products: RSset,
    ) -> Self {
        RSlabel {
            available_entities,
            context,
            t,
            reactants,
            reactantsi,
            inihibitors,
            ireactants,
            products,
        }
    }

    pub fn get_context(&self) -> (RSset, RSset, RSset) {
        // TODO remove clone?
        (
            self.available_entities.clone(),
            self.context.clone(),
            self.t.clone(),
        )
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
    InP,
}

// -----------------------------------------------------------------------------
// RSassert
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum RSassert {
    Not(Box<RSassert>),
    Xor(Box<RSassert>, Box<RSassert>),
    Or(Vec<RSassert>),
    And(Vec<RSassert>),
    Sub(RSset, RSassertOp),
    NonEmpty(RSassertOp),
}

// -----------------------------------------------------------------------------
// RSBHML
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum RSBHML {
    True,
    False,
    Or(Vec<RSBHML>),
    And(Vec<RSBHML>),
    Diamond(Box<RSassert>, Box<RSBHML>),
    Box(Box<RSassert>, Box<RSBHML>),
}

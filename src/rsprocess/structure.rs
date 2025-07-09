use super::translator::IdType;
use std::collections::{BTreeSet, HashMap, VecDeque};
use std::hash::Hash;
use std::rc::Rc;

// -----------------------------------------------------------------------------
// RSset
// -----------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RSset {
    pub identifiers: BTreeSet<IdType>,
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

    // returns the new set a \cup b
    pub fn union(&self, b: &RSset) -> RSset {
        let mut ret: RSset = b.clone();
        ret.identifiers.extend(self.identifiers.iter());
        ret
    }

    pub fn union_option(&mut self, b: Option<&RSset>) {
        if let Some(b) = b {
            self.identifiers.extend(b.iter());
        }
    }

    /// returns the new set a \cap b
    pub fn intersection(&self, b: &RSset) -> RSset {
        // TODO maybe find more efficient way without copy/clone
        let res: BTreeSet<_> = b
            .identifiers
            .intersection(&self.identifiers)
            .copied()
            .collect();
        RSset { identifiers: res }
    }

    /// returns the new set a \ b
    pub fn subtraction(&self, b: &RSset) -> RSset {
        // TODO maybe find more efficient way without copy/clone
        let res: BTreeSet<_> = self
            .identifiers
            .difference(&b.identifiers)
            .copied()
            .collect();
        RSset { identifiers: res }
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

    pub fn is_empty(&self) -> bool {
	self.identifiers.is_empty()
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
    pub reactants: RSset,
    pub inihibitors: RSset,
    pub products: RSset,
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

    /// returns true if ```current_state``` enables the reaction
    /// see enable
    pub fn enabled(&self, current_state: &RSset) -> bool {
        self.reactants.is_subset(current_state)
	    && self.inihibitors.is_disjoint(current_state)
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

    /// returns all elements used
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

impl Default for RSchoices {
    fn default() -> Self {
	Self::new()
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
    pub delta: Rc<RSenvironment>,
    pub available_entities: RSset,
    pub context_process: RSprocess,
    pub reaction_rules: Rc<Vec<RSreaction>>,
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
}

/// Equality does not care about delta or reaction rules, only entities and
/// context is compared
impl PartialEq for RSsystem {
    // we ignore delta and reaction rules
    fn eq(&self, other: &RSsystem) -> bool {
        self.available_entities == other.available_entities &&
	    self.context_process == other.context_process
    }
}

/// Equality does not care about delta or reaction rules, only entities and
/// context is compared
impl Eq for RSsystem {}

/// Hash does not care about delta or reaction rules, only entities and
/// context is hashed
impl Hash for RSsystem {
    // ignores delta and reaction rules
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.available_entities.hash(state);
        self.context_process.hash(state);
    }
}

impl Default for RSsystem {
    fn default() -> Self {
        RSsystem::new()
    }
}

// -----------------------------------------------------------------------------
// RSlabel
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct RSlabel {
    pub available_entities: RSset,
    pub context: RSset,
    pub t: RSset,
    pub reactants: RSset,
    pub reactants_absent: RSset,
    pub inihibitors: RSset,
    pub inihibitors_present: RSset,
    pub products: RSset,
}

impl RSlabel {
    pub fn new() -> Self {
        RSlabel {
            available_entities: RSset::new(),
            context: RSset::new(),
            t: RSset::new(),
            reactants: RSset::new(),
            reactants_absent: RSset::new(),
            inihibitors: RSset::new(),
            inihibitors_present: RSset::new(),
            products: RSset::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn from(
        available_entities: RSset,
        context: RSset,
        t: RSset,
        reactants: RSset,
        reactants_absent: RSset,
        inihibitors: RSset,
        inihibitors_present: RSset,
        products: RSset,
    ) -> Self {
        RSlabel {
            available_entities,
            context,
            t,
            reactants,
            reactants_absent,
            inihibitors,
            inihibitors_present,
            products,
        }
    }

    pub fn get_context(&self) -> (&RSset, &RSset, &RSset) {
        (
            &self.available_entities,
            &self.context,
            &self.t,
        )
    }
}

impl Default for RSlabel {
    fn default() -> Self {
	Self::new()
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

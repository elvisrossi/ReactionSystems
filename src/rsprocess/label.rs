use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use std::hash::Hash;

use super::set::{BasicSet, PositiveSet, Set};
use super::translator::{Formatter, PrintableWithTranslator, Translator};

pub trait BasicLabel
where
    Self: Default + Clone + Debug + Serialize + Eq + Ord + Hash + PrintableWithTranslator,
    for<'a> Self: Deserialize<'a>,
{
    type Set: BasicSet;

    fn get_context(&self) -> (&Self::Set, &Self::Set, &Self::Set);
}

// -----------------------------------------------------------------------------

#[derive(Default, Clone, Debug, Serialize, Deserialize, Eq, PartialOrd, Ord)]
pub struct Label {
    pub available_entities: Set,
    pub context: Set,
    pub t: Set,
    pub reactants: Set,
    pub reactants_absent: Set,
    pub inhibitors: Set,
    pub inhibitors_present: Set,
    pub products: Set,
}

impl BasicLabel for Label {
    type Set = Set;

    fn get_context(&self) -> (&Set, &Set, &Set) {
        (&self.available_entities, &self.context, &self.t)
    }
}

impl Label {
    #[allow(clippy::too_many_arguments)]
    pub fn from(
        available_entities: Set,
        context: Set,
        t: Set,
        reactants: Set,
        reactants_absent: Set,
        inhibitors: Set,
        inhibitors_present: Set,
        products: Set,
    ) -> Self {
        Label {
            available_entities,
            context,
            t,
            reactants,
            reactants_absent,
            inhibitors,
            inhibitors_present,
            products,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn create(
        available_entities: Set,
        context: Set,
        reactants: Set,
        reactants_absent: Set,
        inhibitors: Set,
        inhibitors_present: Set,
        products: Set,
    ) -> Self {
        Label {
            available_entities: available_entities.clone(),
            context: context.clone(),
            t: available_entities.union(&context),
            reactants,
            reactants_absent,
            inhibitors,
            inhibitors_present,
            products,
        }
    }
}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        self.available_entities == other.available_entities &&
	    self.context == other.context &&
	//  self.t == other.t && // no need since its the union of the above
	//  // elements
	    self.reactants == other.reactants &&
	    self.reactants_absent == other.reactants_absent &&
	    self.inhibitors == other.inhibitors &&
	    self.inhibitors_present == other.inhibitors_present &&
	    self.products == other.products
    }
}

impl Hash for Label {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.available_entities.hash(state);
        self.context.hash(state);
        // self.t.hash(state);
        self.reactants.hash(state);
        self.reactants_absent.hash(state);
        self.inhibitors.hash(state);
        self.inhibitors_present.hash(state);
        self.products.hash(state);
    }
}

impl PrintableWithTranslator for Label {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator) -> std::fmt::Result {
        write!(
            f,
            "{{available_entities: {}, \
	     context: {}, \
	     t: {}, \
	     reactants: {}, \
	     reactantsi: {}, \
	     inihibitors: {}, \
	     ireactants: {}, \
	     products: {}}}",
            Formatter::from(translator, &self.available_entities),
            Formatter::from(translator, &self.context),
            Formatter::from(translator, &self.t),
            Formatter::from(translator, &self.reactants),
            Formatter::from(translator, &self.reactants_absent),
            Formatter::from(translator, &self.inhibitors),
            Formatter::from(translator, &self.inhibitors_present),
            Formatter::from(translator, &self.products),
        )
    }
}

// -----------------------------------------------------------------------------

#[derive(Default, Clone, Debug, Serialize, Deserialize, Eq, PartialOrd, Ord)]
pub struct PositiveLabel {
    pub available_entities: PositiveSet,
    pub context: PositiveSet,
    pub t: PositiveSet,
    pub reactants: PositiveSet,
    pub reactants_absent: PositiveSet,
    pub products: PositiveSet,
}

impl BasicLabel for PositiveLabel {
    type Set = PositiveSet;

    fn get_context(&self) -> (&PositiveSet, &PositiveSet, &PositiveSet) {
        (&self.available_entities, &self.context, &self.t)
    }
}

impl PartialEq for PositiveLabel {
    fn eq(&self, other: &Self) -> bool {
        self.available_entities == other.available_entities &&
	    self.context == other.context &&
	//  self.t == other.t && // no need since its the union of the above
	//  // elements
	    self.reactants == other.reactants &&
	    self.reactants_absent == other.reactants_absent &&
	    self.products == other.products
    }
}

impl Hash for PositiveLabel {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.available_entities.hash(state);
        self.context.hash(state);
        // self.t.hash(state);
        self.reactants.hash(state);
        self.reactants_absent.hash(state);
        self.products.hash(state);
    }
}

impl PrintableWithTranslator for PositiveLabel {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator) -> std::fmt::Result {
        write!(
            f,
            "{{available_entities: {}, \
	     context: {}, \
	     t: {}, \
	     reactants: {}, \
	     reactantsi: {}, \
	     products: {}}}",
            Formatter::from(translator, &self.available_entities),
            Formatter::from(translator, &self.context),
            Formatter::from(translator, &self.t),
            Formatter::from(translator, &self.reactants),
            Formatter::from(translator, &self.reactants_absent),
            Formatter::from(translator, &self.products),
        )
    }
}

impl PositiveLabel {
    #[allow(clippy::too_many_arguments)]
    pub fn from(
        available_entities: PositiveSet,
        context: PositiveSet,
        t: PositiveSet,
        reactants: PositiveSet,
        reactants_absent: PositiveSet,
        products: PositiveSet,
    ) -> Self {
        Self {
            available_entities,
            context,
            t,
            reactants,
            reactants_absent,
            products,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn create(
        available_entities: PositiveSet,
        context: PositiveSet,
        reactants: PositiveSet,
        reactants_absent: PositiveSet,
        products: PositiveSet,
    ) -> Self {
        Self {
            available_entities: available_entities.clone(),
            context: context.clone(),
            t: available_entities.union(&context),
            reactants,
            reactants_absent,
            products,
        }
    }
}

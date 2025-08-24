use serde::{Deserialize, Serialize};
use std::hash::Hash;

use super::set::Set;
use super::translator::{Translator, PrintableWithTranslator, Formatter};

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialOrd)]
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

impl Label {
    pub fn new() -> Self {
	Label {
	    available_entities: Set::new(),
	    context: Set::new(),
	    t: Set::new(),
	    reactants: Set::new(),
	    reactants_absent: Set::new(),
	    inhibitors: Set::new(),
	    inhibitors_present: Set::new(),
	    products: Set::new(),
	}
    }

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

    pub fn get_context(&self) -> (&Set, &Set, &Set) {
	(
	    &self.available_entities,
	    &self.context,
	    &self.t,
	)
    }
}

impl Default for Label {
    fn default() -> Self {
	Self::new()
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
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator)
	     -> std::fmt::Result {
	write!(
	    f,
	    "{{available_entities: {}, context: {}, t: {}, reactants: {}, reactantsi: {}, inihibitors: {}, ireactants: {}, products: {}}}",
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

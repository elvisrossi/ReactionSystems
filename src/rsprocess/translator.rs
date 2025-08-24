//! Module for translation and keeping track of strings.

use std::collections::HashMap;
use serde::{Serialize, Deserialize};

/// precision for printing frequencies
pub static PRECISION: &usize = &2;

pub type IdType = u32;

/// Structure that keeps track of association string and id. Ids given
/// sequentially from 0.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Translator {
    strings: HashMap<String, IdType>,
    reverse: HashMap<IdType, String>,
    last_id: IdType,
}

impl Translator {
    pub fn new() -> Self {
        Translator {
            strings: HashMap::from([("*".into(), 0)]),
            reverse: HashMap::from([(0, "*".into())]),
            last_id: 1,
        }
    }

    /// converts a string into an id
    pub fn encode(&mut self, s: impl Into<String>) -> IdType {
        let s = s.into();
        let id = *(self.strings.entry(s.clone()).or_insert({
            self.last_id += 1;
            self.last_id
        }));
        self.reverse.insert(id, s.clone());
        id
    }

    pub fn encode_not_mut(&self, s: impl Into<String>) -> Option<IdType> {
	self.strings.get(&s.into()).copied()
    }

    /// converts an id into the corresponding string
    pub fn decode(&self, el: IdType) -> Option<String> {
        self.reverse
            .get(&el)
            .map(|x| x.to_string())
    }
}

impl Default for Translator {
    fn default() -> Self {
        Translator::new()
    }
}

impl PartialEq for Translator {
    fn eq(&self, other: &Self) -> bool {
	for (s, id) in self.strings.iter() {
	    match other.strings.get(s) {
		None => return false,
		Some(id2) if id != id2 => return false,
		_ => {}
	    }
	}
	true
    }
}


// -----------------------------------------------------------------------------
//                              print structures
// -----------------------------------------------------------------------------

pub trait PrintableWithTranslator {
    fn print(&self, f: &mut fmt::Formatter, translator: &Translator)
	     -> fmt::Result;
}

pub struct Formatter<'a, T> {
    data: &'a T,
    translator: &'a Translator,
}

impl<'a, T> fmt::Display for Formatter<'a, T>
where
    T: PrintableWithTranslator
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	self.data.print(f, self.translator)
    }
}

impl<'a, T> Formatter<'a, T> {
    pub fn from(
	translator: &'a Translator,
	data: &'a T
    ) -> Self {
	Self { data, translator }
    }
}






use super::{
    structure::{
        RSassert
    },
};
use std::fmt;

macro_rules! translator_structure {
    ($name:ident, $type:ty, $dataname:ident, $print_func:ident) => {
	#[derive(Clone, Debug)]
	#[allow(dead_code)]
        pub struct $name<'a> {
            translator: &'a Translator,
	    $dataname: &'a $type,
        }

	#[allow(dead_code)]
	impl <'a>$name<'a> {
	    pub fn from(translator: &'a Translator, $dataname: &'a $type) -> Self {
		$name { translator, $dataname }
	    }
	}

	impl<'a> fmt::Display for $name<'a> {
	    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		$print_func(f, self.translator, self.$dataname)
	    }
	}
    };
}


// RSassert

#[allow(unused_variables)]
fn print_assert(
    f: &mut fmt::Formatter,
    translator: &Translator,
    assert: &RSassert
) -> fmt::Result {
    todo!()
}

translator_structure!(RSassertDisplay, RSassert, assert, print_assert);

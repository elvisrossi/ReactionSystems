//! Module for translation and keeping track of strings.

use std::{cmp::max, collections::HashMap};
use serde::{Serialize, Deserialize};

/// precision for printing frequencies
static PRECISION: &usize = &2;

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

impl Translator {
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

// -----------------------------------------------------------------------------
//                              print structures
// -----------------------------------------------------------------------------
use super::{
    frequency::Frequency,
    structure::{
        RSBHML, RSassert, RSchoices, RSenvironment, RSlabel,
	RSprocess, RSreaction, RSset, RSsystem,
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



// RSset

fn print_set(
    f: &mut fmt::Formatter,
    translator: &Translator,
    set: &RSset
) -> fmt::Result {
    write!(f, "{{")?;
    let mut it = set.iter().peekable();
    while let Some(el) = it.next() {
        if it.peek().is_none() {
            write!(f,
		   "{}",
		   translator.decode(*el).unwrap_or("Missing".into()))?;
        } else {
            write!(f,
		   "{}, ",
		   translator.decode(*el).unwrap_or("Missing".into()))?;
        }
    }
    write!(f, "}}")
}

translator_structure!(RSsetDisplay, RSset, set, print_set);



// RSreaction

fn print_reaction(
    f: &mut fmt::Formatter,
    translator: &Translator,
    reaction: &RSreaction,
) -> fmt::Result {
    write!(
        f,
        "(r: {}, i: {}, p: {})",
        RSsetDisplay::from(translator, &reaction.reactants),
        RSsetDisplay::from(translator, &reaction.inhibitors),
        RSsetDisplay::from(translator, &reaction.products)
    )
}

translator_structure!(RSreactionDisplay, RSreaction, reaction, print_reaction);



// RSprocess

fn print_process(
    f: &mut fmt::Formatter,
    translator: &Translator,
    process: &RSprocess,
) -> fmt::Result {
    use super::structure::RSprocess::*;
    match process {
        Nill => {
            write!(f, "Nill")
        },
        RecursiveIdentifier { identifier } => {
            write!(f,
		   "[{}]",
		   translator.decode(*identifier).unwrap_or("Missing".into()))
        },
        EntitySet { entities, next_process, } => {
            write!(
                f,
                "{}.{}",
                RSsetDisplay::from(translator, entities),
                RSprocessDisplay::from(translator, next_process)
            )
        },
	Guarded { reaction, next_process } => {
	    write!(f,
		   "?{}?.{}",
		   RSreactionDisplay::from(translator, reaction),
		   RSprocessDisplay::from(translator, next_process))
	},
        WaitEntity { repeat, repeated_process, next_process, } => {
            write!(
                f,
                "({})^{repeat}.{}",
                RSprocessDisplay::from(translator, repeated_process),
                RSprocessDisplay::from(translator, next_process)
            )
        }
        Summation { children } => {
            write!(f, "[")?;
            let mut it = children.iter().peekable();
            while let Some(child) = it.next() {
                if it.peek().is_none() {
                    write!(
			f,
			"{}",
			RSprocessDisplay::from(translator, child)
		    )?;
                } else {
                    write!(
                        f,
                        "{} + ",
                        RSprocessDisplay::from(translator, child)
                    )?;
                }
            }
            write!(f, "]")
        }
        NondeterministicChoice { children } => {
            write!(f, "[")?;
            let mut it = children.iter().peekable();
            while let Some(child) = it.next() {
                if it.peek().is_none() {
                    write!(
			f,
			"{}",
			RSprocessDisplay::from(translator, child)
		    )?;
                } else {
                    write!(
			f,
			"{}, ",
			RSprocessDisplay::from(translator, child)
		    )?;
                }
            }
            write!(f, "]")
        }
    }
}

translator_structure!(RSprocessDisplay, RSprocess, process, print_process);



// RSchoices

fn print_choices(
    f: &mut fmt::Formatter,
    translator: &Translator,
    choices: &RSchoices,
) -> fmt::Result {
    write!(f, "[")?;
    let mut it = choices.iter().peekable();
    while let Some(el) = it.next() {
        if it.peek().is_none() {
            write!(
                f,
                "[set: {}, process: {}]",
                RSsetDisplay::from(translator, &el.0),
                RSprocessDisplay::from(translator, &el.1)
            )?;
        } else {
            write!(
                f,
                "[set: {}, process: {}], ",
                RSsetDisplay::from(translator, &el.0),
                RSprocessDisplay::from(translator, &el.1)
            )?;
        }
    }
    write!(f, "]")
}

translator_structure!(RSchoicesDisplay, RSchoices, choices, print_choices);



// RSenvironment

fn print_environment(
    f: &mut fmt::Formatter,
    translator: &Translator,
    environment: &RSenvironment,
) -> fmt::Result {
    write!(f, "{{env:")?;
    let mut it = environment.iter().peekable();
    while let Some(el) = it.next() {
        if it.peek().is_none() {
            write!(
                f,
                "({} -> {})",
                translator.decode(*el.0).unwrap_or("Missing".into()),
                RSprocessDisplay::from(translator, el.1)
            )?;
        } else {
            write!(
                f,
                "({} -> {}), ",
                translator.decode(*el.0).unwrap_or("Missing".into()),
                RSprocessDisplay::from(translator, el.1)
            )?;
        }
    }
    write!(f, "}}")
}

translator_structure!(RSenvironmentDisplay, RSenvironment, environment, print_environment);



// RSsystem

fn print_system(
    f: &mut fmt::Formatter,
    translator: &Translator,
    system: &RSsystem
) -> fmt::Result {
    write!(
        f,
        "[delta: {}, available_entities: {}, context_process: {}, reaction_rules: [",
        RSenvironmentDisplay::from(translator, &system.delta),
        RSsetDisplay::from(translator, &system.available_entities),
        RSprocessDisplay::from(translator, &system.context_process)
    )?;
    let mut it = system.reaction_rules.iter().peekable();
    while let Some(el) = it.next() {
        if it.peek().is_none() {
            write!(f, "{}", RSreactionDisplay::from(translator, el))?;
        } else {
            write!(f, "{}, ", RSreactionDisplay::from(translator, el))?;
        }
    }
    write!(f, "] ]")
}

translator_structure!(RSsystemDisplay, RSsystem, system, print_system);



// RSlabel

fn print_label(
    f: &mut fmt::Formatter,
    translator: &Translator,
    label: &RSlabel
) -> fmt::Result {
    write!(
        f,
        "{{available_entities: {}, context: {}, t: {}, reactants: {}, reactantsi: {}, inihibitors: {}, ireactants: {}, products: {}}}",
        RSsetDisplay::from(translator, &label.available_entities),
        RSsetDisplay::from(translator, &label.context),
        RSsetDisplay::from(translator, &label.t),
        RSsetDisplay::from(translator, &label.reactants),
        RSsetDisplay::from(translator, &label.reactants_absent),
        RSsetDisplay::from(translator, &label.inhibitors),
        RSsetDisplay::from(translator, &label.inhibitors_present),
        RSsetDisplay::from(translator, &label.products),
    )
}

translator_structure!(RSlabelDisplay, RSlabel, label, print_label);

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



// RSBHML

#[allow(unused_variables)]
fn print_bhml(
    f: &mut fmt::Formatter,
    translator: &Translator,
    bhml: &RSBHML
) -> fmt::Result {
    todo!()
}

translator_structure!(RSBHMLDisplay, RSBHML, bhml, print_bhml);


// Frequency

fn print_frequency(
    f: &mut fmt::Formatter,
    translator: &Translator,
    frequency: &Frequency,
) -> fmt::Result {
    write!(f, "[")?;
    let mut freq_it = frequency.frequency_map.iter().peekable();

    let totals = &frequency.totals;
    let weights = &frequency.weights;

    while let Some((e, freq)) = freq_it.next() {
        write!(f, "{} -> ", translator.decode(*e).unwrap_or("Missing".into()))?;

        let mut total_freq = 0.;

	let end = max(freq.len(), max(totals.len(), weights.len()));

	for pos in 0..end {
	    let freq_e = freq.get(pos).copied().unwrap_or(0) as f32;
	    let weight = weights.get(pos).copied().unwrap_or(1) as f32;
	    let total = totals.get(pos).copied().unwrap_or(1) as f32;

	    let weighted_freq = (freq_e * weight * 100.) / (total);
	    if pos == end-1 {
		#[allow(clippy::uninlined_format_args)]
		write!(f, "{weighted_freq:.*}", PRECISION)?;
	    } else {
		#[allow(clippy::uninlined_format_args)]
		write!(f, "{weighted_freq:.*}, ", PRECISION)?;
	    }
	    total_freq += weighted_freq;
	}

        total_freq /= frequency.total_weights() as f32;

	#[allow(clippy::uninlined_format_args)]
        write!(f, " (total: {total_freq:.*})", PRECISION)?;

        if freq_it.peek().is_some() {
            writeln!(f, ",")?;
        }
    }
    write!(f, "]")
}

translator_structure!(FrequencyDisplay, Frequency, frequency, print_frequency);


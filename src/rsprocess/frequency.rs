//! Definitions and structure for frequency of elements in a simulation

use std::collections::HashMap;

use super::reaction::{Reaction, ExtensionReaction};
use super::set::{Set, ExtensionsSet};
use super::system::System;
use super::translator::{IdType, Translator, PrintableWithTranslator, PRECISION};

/// structure that holds the frequency of elements of a run or multiple runs,
/// weighted.  To print use ```translator::FrequencyDisplay```.
#[derive(Debug, Clone)]
pub struct Frequency {
    pub frequency_map: HashMap<IdType, Vec<u32>>,
    pub totals: Vec<usize>,
    pub weights: Vec<u32>,
}

impl Frequency {
    pub fn new() -> Self {
        Frequency {
            frequency_map: HashMap::new(),
            totals: vec![],
            weights: vec![],
        }
    }

    pub fn add(&mut self, e: &Set, run: usize) {
        for &el in e.iter() {
	    let entry =
		self.frequency_map.entry(el).or_insert(vec![0; run + 1]);
	    if entry.len() < run +1 {
		entry.resize(run + 1, 0);
	    }
            entry[run] += 1
        }
	// TODO resize clones all prev values, replace with in place method
	if self.totals.len() < run + 1 {
	    self.totals.resize(run + 1, 0);
	}
        self.totals[run] += 1
    }

    pub fn append_weight(&mut self, new_weight: u32) {
        self.weights.push(new_weight)
    }

    pub fn total_weights(&self) -> u32 {
        self.weights.iter().sum()
    }
}

impl Default for Frequency {
    fn default() -> Self {
        Frequency::new()
    }
}

impl PrintableWithTranslator for Frequency {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator)
	     -> std::fmt::Result {
	use std::cmp::max;
	
	write!(f, "[")?;
	let mut freq_it = self.frequency_map.iter().peekable();

	let totals = &self.totals;
	let weights = &self.weights;

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

            total_freq /= self.total_weights() as f32;

	    #[allow(clippy::uninlined_format_args)]
            write!(f, " (total: {total_freq:.*})", PRECISION)?;

            if freq_it.peek().is_some() {
		writeln!(f, ",")?;
            }
	}
	write!(f, "]")
    }
}

// -----------------------------------------------------------------------------


impl Frequency {
    /// Assuming the system is finite, calculates the frequency of each symbol
    /// in all traversed states.
    /// see naiveFreq
    pub fn naive_frequency(
	system: &System
    ) -> Result<Self, String> {
	let ect = system.run_separated()?;
	let es = ect.iter().map(|(e, _, _)| e).collect::<Vec<_>>();

	let mut freq = Frequency::new();
	freq.append_weight(1);

	es.iter().for_each(|e| freq.add(e, 0));

	Ok(freq)
    }

    /// Assume the system stabilizes in a loop, calculates the frequency of each
    /// symbol in all states of the loop.
    /// see loopFreq
    pub fn loop_frequency(
	system: &System,
	symb: IdType
    ) -> Self {
	let mut freq = Frequency::new();
	freq.append_weight(1);

	if let Some(hoop) = system.lollipops_only_loop_named(symb) {
            hoop.iter().for_each(|e| freq.add(e, 0));
	}
	freq
    }

    /// Assuming ```q[i]``` is given enough times such that the system
    /// stabilizes in a loop, calculates the frequency of the symbols in any
    /// state in the last loop.
    /// see limitFreq
    pub fn limit_frequency(
	q: &[Set],
	reaction_rules: &[Reaction],
	available_entities: &Set,
    ) -> Option<Self> {
	let mut available_entities = available_entities.clone();

	for q in q.iter().rev().skip(1).rev() {
            let res =
		Reaction::lollipops_only_loop_decomposed_q(reaction_rules,
							   q,
							   &available_entities);
            available_entities = res.into_iter().next()?;
	}

	let mut freq = Frequency::new();
	freq.append_weight(1);

	Reaction::lollipops_only_loop_decomposed_q(reaction_rules,
						   q.last().unwrap(),
						   &available_entities)
            .iter()
            .for_each(|e| freq.add(e, 0));
	Some(freq)
    }

    /// Assuming ```q[i]``` is given enough times such that the system
    /// stabilizes in a loop, calculates the frequency of the symbols in any
    /// state in any loop, weighted.
    /// see fastFreq
    pub fn fast_frequency(
	q: &[Set],
	reaction_rules: &[Reaction],
	available_entities: &Set,
	weights: &[u32],
    ) -> Option<Self> {
	// FIXME: we return the empty frequency or do we not return anything?
	let mut available_entities = available_entities.clone();

	let mut freq = Frequency::new();

	for (pos, (q, &w)) in q.iter().zip(weights).enumerate() {
            freq.append_weight(w);
            let hoop =
		Reaction::lollipops_only_loop_decomposed_q(reaction_rules,
							   q,
							   &available_entities);
            hoop.iter().for_each(|e| freq.add(e, pos));
            available_entities = hoop.into_iter().next()?;
	}
	Some(freq)
    }
}

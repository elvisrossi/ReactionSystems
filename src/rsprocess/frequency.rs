//! Definitions and structure for frequency of elements in a simulation

use std::collections::HashMap;

use super::reaction::Reaction;
use super::set::Set;
use super::system::System;
use super::translator::IdType;

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

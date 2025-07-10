//! Definitions and structure for frequency of elements in a simulation

use crate::rsprocess::perpetual::lollipops_only_loop_decomposed_q;
use std::collections::HashMap;

use super::perpetual::lollipops_only_loop_named;
use super::structure::{RSreaction, RSset, RSsystem};
use super::transitions::run_separated;
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

    pub fn add(&mut self, e: &RSset, run: usize) {
        for &el in e.iter() {
	    let entry = self.frequency_map.entry(el).or_insert(vec![0; run + 1]);
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

/// assume the system is finite, calculate the frequency of each symbol in all
/// traversed states
/// see naiveFreq
pub fn naive_frequency(system: &RSsystem) -> Result<Frequency, String> {
    let ect = run_separated(system)?;
    let es = ect.iter().map(|(e, _, _)| e).collect::<Vec<_>>();

    let mut freq = Frequency::new();
    freq.append_weight(1);

    es.iter().for_each(|e| freq.add(e, 0));

    Ok(freq)
}

/// assume the system stabilizes in a loop, calculate the frequency of each
/// symbol in all states of the loop
/// see loopFreq
pub fn loop_frequency(system: &RSsystem, symb: IdType) -> Frequency {
    let mut freq = Frequency::new();
    freq.append_weight(1);

    if let Some(hoop) = lollipops_only_loop_named(system.clone(), symb) {
        hoop.iter().for_each(|e| freq.add(e, 0));
    }
    freq
}

/// ```q[i]``` is given enough times such that the stabilizes in a loop, calculate the
/// frequency of the symbols in any state in the last loop
/// see limitFreq
pub fn limit_frequency(
    q: &[RSset],
    reaction_rules: &[RSreaction],
    available_entities: &RSset,
) -> Option<Frequency> {
    let mut available_entities = available_entities.clone();

    for q in q.iter().rev().skip(1).rev() {
        let res = lollipops_only_loop_decomposed_q(q,
						   reaction_rules,
						   &available_entities);
        available_entities = res.into_iter().next()?;
    }

    let mut freq = Frequency::new();
    freq.append_weight(1);

    lollipops_only_loop_decomposed_q(q.last().unwrap(),
				     reaction_rules,
				     &available_entities)
        .iter()
        .for_each(|e| freq.add(e, 0));
    Some(freq)
}

/// ```q[i]``` is given enough times such that the stabilizes in a loop, calculate the
/// frequency of the symbols in any state in any loop, weighted.
/// see fastFreq
pub fn fast_frequency(
    q: &[RSset],
    reaction_rules: &[RSreaction],
    available_entities: &RSset,
    weights: &[u32],
) -> Option<Frequency> {
    // FIXME: we return the empty frequency or do we not return anything?
    let mut available_entities = available_entities.clone();

    let mut freq = Frequency::new();

    for (pos, (q, &w)) in q.iter().zip(weights).enumerate() {
        freq.append_weight(w);
        let hoop = lollipops_only_loop_decomposed_q(q,
						    reaction_rules,
						    &available_entities);
        hoop.iter().for_each(|e| freq.add(e, pos));
        available_entities = hoop.into_iter().next()?;
    }
    Some(freq)
}

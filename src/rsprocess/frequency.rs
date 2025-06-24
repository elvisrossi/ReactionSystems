#![allow(dead_code)]

use std::collections::HashMap;
use crate::rsprocess::perpetual::lollipops_only_loop_decomposed_q;

use super::perpetual::{lollipops_decomposed_named, lollipops_only_loop_decomposed_named, lollipops_only_loop_named};
use super::structure::{RSenvironment, RSreaction, RSset, RSsystem};
use super::transitions::run_separated;
use super::translator::IdType;

#[derive(Debug, Clone)]
pub struct Frequency {
    pub frequency_map: HashMap<IdType, u32>,
    pub total_runs: usize,
    pub weight: u32
}

impl Frequency {
    pub fn new(weight: u32) -> Self {
	Frequency { frequency_map: HashMap::new(), total_runs: 0, weight }
    }

    pub fn add(&mut self, e: &RSset) {
	for &el in e.iter() {
	    *self.frequency_map.entry(el).or_default() += 1
	}
	self.total_runs += 1
    }

    pub fn merge(&mut self, other: Frequency) {
	let mut count = 0;
	other.frequency_map.iter().for_each(
	    |(&k, &v)| {
		*self.frequency_map.entry(k)
		    .or_insert_with(|| {count+=1; 0}) += v;
	    }
	);
	self.total_runs += count;
	// FIXME: ???
	self.weight += other.weight;
    }
}


// -----------------------------------------------------------------------------

// see naiveFreq
pub fn naive_frequency(system: &RSsystem) -> Result<Frequency, String> {
    let ect = run_separated(system)?;
    let es = ect.iter().map(|(e, _, _)| e).collect::<Vec<_>>();

    let mut freq = Frequency::new(1);

    es.iter().for_each(|e| freq.add(e));

    Ok(freq)
}

// see naiveFreq
pub fn naive_frequency_weighted(system: &RSsystem, weight: u32) -> Result<Frequency, String> {
    let ect = run_separated(system)?;
    let es = ect.iter().map(|(e, _, _)| e).collect::<Vec<_>>();

    let mut freq = Frequency::new(weight);

    es.iter().for_each(|e| freq.add(e));

    Ok(freq)
}

// see loopFreq
pub fn loop_frequency(system: &RSsystem, symb: IdType, weight: u32) -> Frequency {
    // FIXME: we return the empty frequency or do we not return anything?
    let mut freq = Frequency::new(weight);

    if let Some(hoop) = lollipops_only_loop_named(system.clone(), symb) {
	hoop.iter().for_each(|e| freq.add(e));
    }
    freq
}

// see limitFreq
pub fn limit_frequency(
    q: &[RSset],
    reaction_rules: &[RSreaction],
    available_entities: &RSset,
    weight: u32
) -> Option<Frequency> {
    let mut available_entities = available_entities.clone();
    for q in q.iter().rev().skip(1).rev() {
	let res = lollipops_only_loop_decomposed_q(q,
						   reaction_rules,
						   &available_entities);
	available_entities = res.into_iter().next()?;
    }

    let mut freq = Frequency::new(weight);

    lollipops_only_loop_decomposed_q(q.last().unwrap(),
				     reaction_rules,
				     &available_entities)
	.iter().for_each(|e| freq.add(e));
    Some(freq)
}


// see fastFreq
pub fn fast_frequency(
    q: &[&[RSset]],
    reaction_rules: &[RSreaction],
    available_entities: &RSset,
    weights: &[u32]
) -> Option<Frequency> {
    // FIXME: we return the empty frequency or do we not return anything?
    let mut available_entities = available_entities.clone();

    let mut freq = Frequency::new(1);

    for (&q, &w) in q.iter().zip(weights) {
	let mut new_freq = Frequency::new(w);
	for q in q.iter().rev().skip(1).rev() {
	    let hoop = lollipops_only_loop_decomposed_q(q,
							reaction_rules,
							&available_entities);
	    hoop.iter().for_each(|e| freq.add(e));
	    available_entities = hoop.into_iter().next()?;
	}

	lollipops_only_loop_decomposed_q(q.last().unwrap(),
					 reaction_rules,
					 &available_entities)
	    .iter().for_each(|e| new_freq.add(e));

	freq.merge(new_freq);
    }
    Some(freq)
}

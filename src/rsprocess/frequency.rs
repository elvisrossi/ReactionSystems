//! Definitions and structure for frequency of elements in a simulation

use std::collections::HashMap;
use std::fmt::Debug;

use super::element::{IdType, PositiveType};
use super::environment::{BasicEnvironment, Environment, PositiveEnvironment};
use super::reaction::{BasicReaction, ExtensionReaction, PositiveReaction, Reaction};
use super::set::{BasicSet, ExtensionsSet, PositiveSet, Set};
use super::system::{BasicSystem, ExtensionsSystem, LoopSystem, PositiveSystem, System};
use super::translator::{Formatter, PRECISION, PrintableWithTranslator, Translator};

pub trait BasicFrequency: Debug + Clone + Default + PrintableWithTranslator {
    type Set: BasicSet;
    type Sys: BasicSystem<Set = Self::Set> + LoopSystem<Env = Self::Env, Reaction = Self::R>;
    type Env: BasicEnvironment<Set = Self::Set, Reaction = Self::R, Id = Self::Id>;
    type R: BasicReaction<Set = Self::Set>;
    type Id;

    fn naive_frequency(system: &Self::Sys) -> Result<Self, String>;

    fn loop_frequency(system: &Self::Sys, symb: Self::Id) -> Self;

    fn limit_frequency(
        q: &[Self::Set],
        reactions: &[Self::R],
        available_entities: &Self::Set,
    ) -> Option<Self>;

    fn fast_frequency(
        q: &[Self::Set],
        reactions: &[Self::R],
        available_entities: &Self::Set,
        weights: &[u32],
    ) -> Option<Self>;
}

/// Structure that holds the frequency of elements of a run or multiple runs,
/// weighted.  To print use ```translator::FrequencyDisplay```.
#[derive(Debug, Clone, Default)]
pub struct Frequency {
    pub frequency_map: HashMap<IdType, Vec<u32>>,
    pub totals: Vec<usize>,
    pub weights: Vec<u32>,
}

impl Frequency {
    fn add(&mut self, e: Set, run: usize) {
        for el in e.iter() {
            let entry = self.frequency_map.entry(*el).or_insert(vec![0; run + 1]);
            if entry.len() < run + 1 {
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

    fn append_weight(&mut self, new_weight: u32) {
        self.weights.push(new_weight)
    }

    fn total_weights(&self) -> u32 {
        self.weights.iter().sum()
    }
}

impl PrintableWithTranslator for Frequency {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator) -> std::fmt::Result {
        use std::cmp::max;

        write!(f, "[")?;
        let mut freq_it = self.frequency_map.iter().peekable();

        let totals = &self.totals;
        let weights = &self.weights;

        while let Some((e, freq)) = freq_it.next() {
            write!(f, "{} -> ", Formatter::from(translator, e))?;

            let mut total_freq = 0.;

            let end = max(freq.len(), max(totals.len(), weights.len()));

            for pos in 0..end {
                let freq_e = freq.get(pos).copied().unwrap_or(0) as f32;
                let weight = weights.get(pos).copied().unwrap_or(1) as f32;
                let total = totals.get(pos).copied().unwrap_or(1) as f32;

                let weighted_freq = (freq_e * weight * 100.) / (total);
                if pos == end - 1 {
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

impl BasicFrequency for Frequency {
    type Set = Set;
    type Sys = System;
    type Env = Environment;
    type R = Reaction;
    type Id = IdType;

    /// Assuming the system is finite, calculates the frequency of each symbol
    /// in all traversed states.
    /// see naiveFreq
    fn naive_frequency(system: &Self::Sys) -> Result<Self, String> {
        let ect = system.run_separated()?;

        let mut freq = Self::default();
        freq.append_weight(1);

        ect.into_iter().for_each(|(e, _, _)| freq.add(e, 0));

        Ok(freq)
    }

    /// Assume the system stabilizes in a loop, calculates the frequency of each
    /// symbol in all states of the loop.
    /// see loopFreq
    fn loop_frequency(system: &Self::Sys, symb: Self::Id) -> Self {
        let mut freq = Self::default();
        freq.append_weight(1);

        if let Some(hoop) = system.lollipops_only_loop_named(symb) {
            hoop.iter().for_each(|e| freq.add(e.clone(), 0));
        }
        freq
    }

    /// Assuming ```q[i]``` is given enough times such that the system
    /// stabilizes in a loop, calculates the frequency of the symbols in any
    /// state in the last loop.
    /// see limitFreq
    fn limit_frequency(
        q: &[Self::Set],
        reaction_rules: &[Self::R],
        available_entities: &Self::Set,
    ) -> Option<Self> {
        let mut available_entities = available_entities.clone();

        for q in q.iter().rev().skip(1).rev() {
            let res =
                Self::R::lollipops_only_loop_decomposed_q(reaction_rules, q, &available_entities);
            available_entities = res.into_iter().next()?;
        }

        let mut freq = Self::default();
        freq.append_weight(1);

        Self::R::lollipops_only_loop_decomposed_q(
            reaction_rules,
            q.last().unwrap(),
            &available_entities,
        )
        .iter()
        .cloned()
        .for_each(|e| freq.add(e, 0));
        Some(freq)
    }

    /// Assuming ```q[i]``` is given enough times such that the system
    /// stabilizes in a loop, calculates the frequency of the symbols in any
    /// state in any loop, weighted.
    /// see fastFreq
    fn fast_frequency(
        q: &[Self::Set],
        reaction_rules: &[Self::R],
        available_entities: &Self::Set,
        weights: &[u32],
    ) -> Option<Self> {
        // FIXME: we return the empty frequency or do we not return anything?
        let mut available_entities = available_entities.clone();

        let mut freq = Self::default();

        for (pos, (q, &w)) in q.iter().zip(weights).enumerate() {
            freq.append_weight(w);
            let hoop =
                Self::R::lollipops_only_loop_decomposed_q(reaction_rules, q, &available_entities);
            hoop.iter().cloned().for_each(|e| freq.add(e, pos));
            available_entities = hoop.into_iter().next()?;
        }
        Some(freq)
    }
}

// -----------------------------------------------------------------------------

/// Structure that holds the frequency of positive or negative elements of a run
/// or multiple runs, weighted. To print use ```translator::FrequencyDisplay```.
#[derive(Debug, Clone, Default)]
pub struct PositiveFrequency {
    pub frequency_map: HashMap<PositiveType, Vec<u32>>,
    pub totals: Vec<usize>,
    pub weights: Vec<u32>,
}

impl PositiveFrequency {
    fn add(&mut self, e: PositiveSet, run: usize) {
        for (&id, &state) in e.iter() {
            let entry = self
                .frequency_map
                .entry(PositiveType { id, state })
                .or_insert(vec![0; run + 1]);
            if entry.len() < run + 1 {
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

    fn append_weight(&mut self, new_weight: u32) {
        self.weights.push(new_weight)
    }

    fn total_weights(&self) -> u32 {
        self.weights.iter().sum()
    }
}

impl PrintableWithTranslator for PositiveFrequency {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator) -> std::fmt::Result {
        use std::cmp::max;

        write!(f, "[")?;
        let mut freq_it = self.frequency_map.iter().peekable();

        let totals = &self.totals;
        let weights = &self.weights;

        while let Some((e, freq)) = freq_it.next() {
            write!(f, "{} -> ", Formatter::from(translator, e))?;

            let mut total_freq = 0.;

            let end = max(freq.len(), max(totals.len(), weights.len()));

            for pos in 0..end {
                let freq_e = freq.get(pos).copied().unwrap_or(0) as f32;
                let weight = weights.get(pos).copied().unwrap_or(1) as f32;
                let total = totals.get(pos).copied().unwrap_or(1) as f32;

                let weighted_freq = (freq_e * weight * 100.) / (total);
                if pos == end - 1 {
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

impl BasicFrequency for PositiveFrequency {
    type Set = PositiveSet;
    type Sys = PositiveSystem;
    type Env = PositiveEnvironment;
    type R = PositiveReaction;
    type Id = IdType;

    /// Assuming the system is finite, calculates the frequency of each symbol
    /// in all traversed states.
    /// see naiveFreq
    fn naive_frequency(system: &Self::Sys) -> Result<Self, String> {
        let ect = system.run_separated()?;

        let mut freq = Self::default();
        freq.append_weight(1);

        ect.into_iter().for_each(|(e, _, _)| freq.add(e, 0));

        Ok(freq)
    }

    /// Assume the system stabilizes in a loop, calculates the frequency of each
    /// symbol in all states of the loop.
    /// see loopFreq
    fn loop_frequency(system: &Self::Sys, symb: Self::Id) -> Self {
        let mut freq = Self::default();
        freq.append_weight(1);

        if let Some(hoop) = system.lollipops_only_loop_named(symb) {
            hoop.iter().for_each(|e| freq.add(e.clone(), 0));
        }
        freq
    }

    /// Assuming ```q[i]``` is given enough times such that the system
    /// stabilizes in a loop, calculates the frequency of the symbols in any
    /// state in the last loop.
    /// see limitFreq
    fn limit_frequency(
        q: &[Self::Set],
        reaction_rules: &[Self::R],
        available_entities: &Self::Set,
    ) -> Option<Self> {
        let mut available_entities = available_entities.clone();

        for q in q.iter().rev().skip(1).rev() {
            let res =
                Self::R::lollipops_only_loop_decomposed_q(reaction_rules, q, &available_entities);
            available_entities = res.into_iter().next()?;
        }

        let mut freq = Self::default();
        freq.append_weight(1);

        Self::R::lollipops_only_loop_decomposed_q(
            reaction_rules,
            q.last().unwrap(),
            &available_entities,
        )
        .iter()
        .cloned()
        .for_each(|e| freq.add(e, 0));
        Some(freq)
    }

    /// Assuming ```q[i]``` is given enough times such that the system
    /// stabilizes in a loop, calculates the frequency of the symbols in any
    /// state in any loop, weighted.
    /// see fastFreq
    fn fast_frequency(
        q: &[Self::Set],
        reaction_rules: &[Self::R],
        available_entities: &Self::Set,
        weights: &[u32],
    ) -> Option<Self> {
        // FIXME: we return the empty frequency or do we not return anything?
        let mut available_entities = available_entities.clone();

        let mut freq = Self::default();

        for (pos, (q, &w)) in q.iter().zip(weights).enumerate() {
            freq.append_weight(w);
            let hoop =
                Self::R::lollipops_only_loop_decomposed_q(reaction_rules, q, &available_entities);
            hoop.iter().cloned().for_each(|e| freq.add(e, pos));
            available_entities = hoop.into_iter().next()?;
        }
        Some(freq)
    }
}

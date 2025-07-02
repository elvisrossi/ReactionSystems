// translate and keeps track of strings
use std::collections::HashMap;

pub type IdType = u32;

#[derive(Clone, Debug)]
pub struct Translator {
    strings: HashMap<String, IdType>,
    reverse: HashMap<IdType, String>,
    last_id: IdType,
}

impl Translator {
    pub fn new() -> Self {
        Translator {
            strings: HashMap::new(),
            reverse: HashMap::new(),
            last_id: 0,
        }
    }
}

impl Default for Translator {
    fn default() -> Self {
        Translator::new()
    }
}

impl Translator {
    pub fn encode(&mut self, s: impl Into<String>) -> IdType {
        let s = s.into();
        let id = *(self.strings.entry(s.clone()).or_insert({
            self.last_id += 1;
            self.last_id
        }));
        self.reverse.insert(id, s.clone());
        id
    }

    pub fn decode(&self, el: IdType) -> Option<String> {
        self.reverse
            .get(&el)
            .map(|x| x.to_string())
    }
}

// -----------------------------------------------------------------------------
use super::{
    frequency::Frequency,
    structure::{
        RSBHML, RSassert, RSassertOp, RSchoices, RSenvironment, RSlabel,
	RSprocess, RSreaction, RSset, RSsystem,
    },
};
use std::fmt;

#[allow(clippy::large_enum_variant)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Debug)]
pub enum WithTranslator<'a> {
    RSset {
        translator: &'a Translator,
        set: &'a RSset,
    },
    RSreaction {
        translator: &'a Translator,
        reaction: &'a RSreaction,
    },
    RSprocess {
        translator: &'a Translator,
        process: &'a RSprocess,
    },
    RSchoices {
        translator: &'a Translator,
        choices: &'a RSchoices,
    },
    RSenvironment {
        translator: &'a Translator,
        environment: &'a RSenvironment,
    },
    RSsystem {
        translator: &'a Translator,
        system: &'a RSsystem,
    },
    RSlabel {
        translator: &'a Translator,
        label: &'a RSlabel,
    },
    RSassertOp {
        translator: &'a Translator,
        assert_op: &'a RSassertOp,
    },
    RSassert {
        translator: &'a Translator,
        assert: &'a RSassert,
    },
    RSBHML {
        translator: &'a Translator,
        bhml: &'a RSBHML,
    },
    Frequency {
        translator: &'a Translator,
        frequency: &'a Frequency,
    },
}

macro_rules! from_RS {
    ($name:ident, $type:ty, $dataname:ident, $type2: ident) => {
        pub fn $name(translator: &'a Translator, $dataname: &'a $type) -> Self {
            WithTranslator::$type2 {
                translator,
                $dataname,
            }
        }
    };
}

#[allow(non_snake_case)]
#[allow(dead_code)]
impl<'a> WithTranslator<'a> {
    from_RS!(from_RSset, RSset, set, RSset);

    from_RS!(from_RSreaction, RSreaction, reaction, RSreaction);

    from_RS!(from_RSprocess, RSprocess, process, RSprocess);

    from_RS!(from_RSchoices, RSchoices, choices, RSchoices);

    from_RS!(
        from_RSenvironment,
        RSenvironment,
        environment,
        RSenvironment
    );

    from_RS!(from_RSsystem, RSsystem, system, RSsystem);

    from_RS!(from_RSlabel, RSlabel, label, RSlabel);

    from_RS!(from_RSassertOp, RSassertOp, assert_op, RSassertOp);

    from_RS!(from_RSassert, RSassert, assert, RSassert);

    from_RS!(from_RSBHML, RSBHML, bhml, RSBHML);

    from_RS!(from_Frequency, Frequency, frequency, Frequency);
}

// -----------------------------------------------------------------------------
// Printing functions
// -----------------------------------------------------------------------------

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

fn print_reaction(
    f: &mut fmt::Formatter,
    translator: &Translator,
    reaction: &RSreaction,
) -> fmt::Result {
    write!(
        f,
        "(r: {}, i: {}, p: {})",
        WithTranslator::from_RSset(translator, reaction.reactants()),
        WithTranslator::from_RSset(translator, reaction.inihibitors()),
        WithTranslator::from_RSset(translator, reaction.products())
    )
}

fn print_process(
    f: &mut fmt::Formatter,
    translator: &Translator,
    process: &RSprocess,
) -> fmt::Result {
    use super::structure::RSprocess::*;
    match process {
        Nill => {
            write!(f, "[Nill]")
        }
        RecursiveIdentifier { identifier } => {
            write!(f,
		   "[{}]",
		   translator.decode(*identifier).unwrap_or("Missing".into()))
        }
        EntitySet {
            entities,
            next_process,
        } => {
            write!(
                f,
                "[entities: {}, next_process: {}]",
                WithTranslator::from_RSset(translator, entities),
                WithTranslator::from_RSprocess(translator, next_process)
            )
        }
        WaitEntity {
            repeat,
            repeated_process,
            next_process,
        } => {
            write!(
                f,
                "[repeat: {repeat}, repeated_process: {}, next_process: {}]",
                WithTranslator::from_RSprocess(translator, repeated_process),
                WithTranslator::from_RSprocess(translator, next_process)
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
			WithTranslator::from_RSprocess(translator, child)
		    )?;
                } else {
                    write!(
                        f,
                        "{} + ",
                        WithTranslator::from_RSprocess(translator, child)
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
			WithTranslator::from_RSprocess(translator, child)
		    )?;
                } else {
                    write!(
			f,
			"{}, ",
			WithTranslator::from_RSprocess(translator, child)
		    )?;
                }
            }
            write!(f, "]")
        }
    }
}

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
                WithTranslator::from_RSset(translator, &el.0),
                WithTranslator::from_RSprocess(translator, &el.1)
            )?;
        } else {
            write!(
                f,
                "[set: {}, process: {}], ",
                WithTranslator::from_RSset(translator, &el.0),
                WithTranslator::from_RSprocess(translator, &el.1)
            )?;
        }
    }
    write!(f, "]")
}

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
                WithTranslator::from_RSprocess(translator, el.1)
            )?;
        } else {
            write!(
                f,
                "({} -> {}), ",
                translator.decode(*el.0).unwrap_or("Missing".into()),
                WithTranslator::from_RSprocess(translator, el.1)
            )?;
        }
    }
    write!(f, "}}")
}

fn print_system(
    f: &mut fmt::Formatter,
    translator: &Translator,
    system: &RSsystem
) -> fmt::Result {
    write!(
        f,
        "[delta: {}, available_entities: {}, context_process: {}, reaction_rules: [",
        WithTranslator::from_RSenvironment(translator, system.get_delta()),
        WithTranslator::from_RSset(translator, system.get_available_entities()),
        WithTranslator::from_RSprocess(translator, system.get_context_process())
    )?;
    let mut it = system.get_reaction_rules().iter().peekable();
    while let Some(el) = it.next() {
        if it.peek().is_none() {
            write!(f, "{}", WithTranslator::from_RSreaction(translator, el))?;
        } else {
            write!(f, "{}, ", WithTranslator::from_RSreaction(translator, el))?;
        }
    }
    write!(f, "] ]")
}

fn print_label(
    f: &mut fmt::Formatter,
    translator: &Translator,
    label: &RSlabel
) -> fmt::Result {
    write!(
        f,
        "{{available_entities: {}, context: {}, t: {}, reactants: {}, reactantsi: {}, inihibitors: {}, ireactants: {}, products: {}}}",
        WithTranslator::from_RSset(translator, &label.available_entities),
        WithTranslator::from_RSset(translator, &label.context),
        WithTranslator::from_RSset(translator, &label.t),
        WithTranslator::from_RSset(translator, &label.reactants),
        WithTranslator::from_RSset(translator, &label.reactantsi),
        WithTranslator::from_RSset(translator, &label.inihibitors),
        WithTranslator::from_RSset(translator, &label.ireactants),
        WithTranslator::from_RSset(translator, &label.products),
    )
}

fn print_assert_op(
    f: &mut fmt::Formatter,
    _translator: &Translator,
    assert_op: &RSassertOp,
) -> fmt::Result {
    use super::structure::RSassertOp::*;
    match assert_op {
        InW => {
            write!(f, "InW")
        }
        InR => {
            write!(f, "InR")
        }
        InI => {
            write!(f, "InI")
        }
        InP => {
            write!(f, "InP")
        }
    }
}

#[allow(unused_variables)]
fn print_assert(
    f: &mut fmt::Formatter,
    translator: &Translator,
    assert: &RSassert
) -> fmt::Result {
    todo!()
}

#[allow(unused_variables)]
fn print_bhml(
    f: &mut fmt::Formatter,
    translator: &Translator,
    bhml: &RSBHML
) -> fmt::Result {
    todo!()
}

fn print_frequency(
    f: &mut fmt::Formatter,
    translator: &Translator,
    frequency: &Frequency,
) -> fmt::Result {
    write!(f, "[")?;
    let mut freq_it = frequency.frequency_map.iter().peekable();

    while let Some((e, freq)) = freq_it.next() {
        write!(f, "{} -> ", translator.decode(*e).unwrap_or("Missing".into()))?;

        let mut iter = freq
            .iter()
            .zip(frequency.totals.iter().zip(frequency.weights.iter()))
            .peekable();

        let mut total_freq = 0.;

        while let Some((freq_e, (total, weight))) = iter.next() {
            let weighted_freq = (*freq_e as f32 * *weight as f32 * 100.) / (*total as f32);

            if iter.peek().is_none() {
                write!(f, "{weighted_freq:.2}")?;
            } else {
                write!(f, "{weighted_freq:.2}, ")?;
            }
            total_freq += weighted_freq;
        }

        total_freq /= frequency.total_weights() as f32;

        write!(f, "(total: {total_freq:.2})")?;

        if freq_it.peek().is_some() {
            writeln!(f, ",")?;
        }
    }
    write!(f, "]")
}

impl<'a> fmt::Display for WithTranslator<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            WithTranslator::RSset {
		translator,
		set
	    } => print_set(f, translator, set),
            WithTranslator::RSreaction {
                translator,
                reaction,
            } => print_reaction(f, translator, reaction),
            WithTranslator::RSprocess {
                translator,
                process,
            } => print_process(f, translator, process),
            WithTranslator::RSchoices {
                translator,
                choices,
            } => print_choices(f, translator, choices),
            WithTranslator::RSenvironment {
                translator,
                environment,
            } => print_environment(f, translator, environment),
            WithTranslator::RSsystem {
		translator,
		system
	    } => print_system(f, translator, system),
            WithTranslator::RSlabel {
		translator,
		label
	    } => print_label(f, translator, label),
            WithTranslator::RSassertOp {
                translator,
                assert_op,
            } => print_assert_op(f, translator, assert_op),
            WithTranslator::RSassert {
		translator,
		assert
	    } => print_assert(f, translator, assert),
            WithTranslator::RSBHML {
		translator,
		bhml
	    } => print_bhml(f, translator, bhml),
            WithTranslator::Frequency {
                translator,
                frequency,
            } => print_frequency(f, translator, frequency),
        }
    }
}

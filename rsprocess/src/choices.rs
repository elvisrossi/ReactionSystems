use std::fmt::Debug;
use std::sync::Arc;

use super::process::{BasicProcess, PositiveProcess, Process};
use super::set::{BasicSet, PositiveSet, Set};
use super::translator::{Formatter, PrintableWithTranslator, Translator};

pub trait BasicChoices
where
    Self: Clone + Debug + Default + IntoIterator + PrintableWithTranslator,
{
    type Process: BasicProcess;

    fn append(&mut self, other: &mut Self);
    fn replace(&mut self, other: Arc<Self::Process>);
    fn shuffle(&mut self, choices: Self);
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct Choices {
    context_moves: Vec<(Arc<Set>, Arc<Process>)>,
}

impl BasicChoices for Choices {
    type Process = Process;

    fn append(&mut self, a: &mut Self) {
        self.context_moves.append(&mut a.context_moves);
    }

    fn replace(&mut self, a: Arc<Self::Process>) {
        self.context_moves = self
            .context_moves
            .iter_mut()
            .map(|(c1, _)| (Arc::clone(c1), Arc::clone(&a)))
            .collect::<Vec<_>>();
    }

    fn shuffle(&mut self, choices: Self) {
        match (
            self.context_moves.is_empty(),
            choices.context_moves.is_empty(),
        ) {
            | (true, true) => {},
            | (true, false) => self.context_moves = choices.context_moves,
            | (false, true) => {},
            | (false, false) => {
                let mut new_self = vec![];
                for item_self in &self.context_moves {
                    for item_choices in &choices.context_moves {
                        new_self.push((
                            Arc::new(item_self.0.union(&item_choices.0)),
                            Arc::new(item_self.1.concat(&item_choices.1)),
                        ));
                    }
                }
                self.context_moves = new_self;
            },
        }
    }
}

impl Choices {
    fn iter(&self) -> std::slice::Iter<'_, (Arc<Set>, Arc<Process>)> {
        self.context_moves.iter()
    }
}

impl IntoIterator for Choices {
    type Item = (Arc<Set>, Arc<Process>);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.context_moves.into_iter()
    }
}

impl<const N: usize> From<[(Arc<Set>, Arc<Process>); N]> for Choices {
    fn from(arr: [(Arc<Set>, Arc<Process>); N]) -> Self {
        Choices {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<&[(Arc<Set>, Arc<Process>)]> for Choices {
    fn from(arr: &[(Arc<Set>, Arc<Process>)]) -> Self {
        Choices {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<Vec<(Arc<Set>, Arc<Process>)>> for Choices {
    fn from(arr: Vec<(Arc<Set>, Arc<Process>)>) -> Self {
        Choices { context_moves: arr }
    }
}

impl PrintableWithTranslator for Choices {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &Translator,
    ) -> std::fmt::Result {
        write!(f, "[")?;
        let mut it = self.iter().peekable();
        while let Some(el) = it.next() {
            if it.peek().is_none() {
                write!(
                    f,
                    "[set: {}, process: {}]",
                    Formatter::from(translator, &*el.0),
                    Formatter::from(translator, &*el.1)
                )?;
            } else {
                write!(
                    f,
                    "[set: {}, process: {}], ",
                    Formatter::from(translator, &*el.0),
                    Formatter::from(translator, &*el.1)
                )?;
            }
        }
        write!(f, "]")
    }
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct PositiveChoices {
    context_moves: Vec<(Arc<PositiveSet>, Arc<PositiveProcess>)>,
}

impl BasicChoices for PositiveChoices {
    type Process = PositiveProcess;

    fn append(&mut self, a: &mut Self) {
        self.context_moves.append(&mut a.context_moves);
    }

    fn replace(&mut self, a: Arc<Self::Process>) {
        self.context_moves = self
            .context_moves
            .iter_mut()
            .map(|(c1, _)| (Arc::clone(c1), Arc::clone(&a)))
            .collect::<Vec<_>>();
    }

    fn shuffle(&mut self, choices: Self) {
        match (
            self.context_moves.is_empty(),
            choices.context_moves.is_empty(),
        ) {
            | (true, true) => {},
            | (true, false) => self.context_moves = choices.context_moves,
            | (false, true) => {},
            | (false, false) => {
                let mut new_self = vec![];
                for item_self in &self.context_moves {
                    for item_choices in &choices.context_moves {
                        new_self.push((
                            Arc::new(item_self.0.union(&item_choices.0)),
                            Arc::new(item_self.1.concat(&item_choices.1)),
                        ));
                    }
                }
                self.context_moves = new_self;
            },
        }
    }
}

impl IntoIterator for PositiveChoices {
    type Item = (Arc<PositiveSet>, Arc<PositiveProcess>);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.context_moves.into_iter()
    }
}

impl PrintableWithTranslator for PositiveChoices {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &Translator,
    ) -> std::fmt::Result {
        write!(f, "[")?;
        let mut it = self.iter().peekable();
        while let Some(el) = it.next() {
            if it.peek().is_none() {
                write!(
                    f,
                    "[set: {}, process: {}]",
                    Formatter::from(translator, &*el.0),
                    Formatter::from(translator, &*el.1)
                )?;
            } else {
                write!(
                    f,
                    "[set: {}, process: {}], ",
                    Formatter::from(translator, &*el.0),
                    Formatter::from(translator, &*el.1)
                )?;
            }
        }
        write!(f, "]")
    }
}

impl PositiveChoices {
    fn iter(
        &self,
    ) -> std::slice::Iter<'_, (Arc<PositiveSet>, Arc<PositiveProcess>)> {
        self.context_moves.iter()
    }
}

impl<const N: usize> From<[(Arc<PositiveSet>, Arc<PositiveProcess>); N]>
    for PositiveChoices
{
    fn from(arr: [(Arc<PositiveSet>, Arc<PositiveProcess>); N]) -> Self {
        Self {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<&[(Arc<PositiveSet>, Arc<PositiveProcess>)]> for PositiveChoices {
    fn from(arr: &[(Arc<PositiveSet>, Arc<PositiveProcess>)]) -> Self {
        Self {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<Vec<(Arc<PositiveSet>, Arc<PositiveProcess>)>> for PositiveChoices {
    fn from(arr: Vec<(Arc<PositiveSet>, Arc<PositiveProcess>)>) -> Self {
        Self { context_moves: arr }
    }
}

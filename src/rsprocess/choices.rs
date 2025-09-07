use std::fmt::Debug;
use std::rc::Rc;

use super::process::{BasicProcess, PositiveProcess, Process};
use super::set::{BasicSet, PositiveSet, Set};
use super::translator::{Formatter, PrintableWithTranslator, Translator};

pub trait BasicChoices
where
    Self: Clone + Debug + Default + IntoIterator + PrintableWithTranslator,
{
    type Process: BasicProcess;

    fn append(&mut self, other: &mut Self);
    fn replace(&mut self, other: Rc<Self::Process>);
    fn shuffle(&mut self, choices: Self);
}

// -----------------------------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct Choices {
    context_moves: Vec<(Rc<Set>, Rc<Process>)>,
}

impl BasicChoices for Choices {
    type Process = Process;

    fn append(&mut self, a: &mut Self) {
        self.context_moves.append(&mut a.context_moves);
    }

    fn replace(&mut self, a: Rc<Self::Process>) {
        self.context_moves = self
            .context_moves
            .iter_mut()
            .map(|(c1, _)| (Rc::clone(c1), Rc::clone(&a)))
            .collect::<Vec<_>>();
    }

    fn shuffle(&mut self, choices: Self) {
        match (
            self.context_moves.is_empty(),
            choices.context_moves.is_empty(),
        ) {
            (true, true) => {}
            (true, false) => self.context_moves = choices.context_moves,
            (false, true) => {}
            (false, false) => {
                let mut new_self = vec![];
                for item_self in &self.context_moves {
                    for item_choices in &choices.context_moves {
                        new_self.push((
                            Rc::new(item_self.0.union(&item_choices.0)),
                            Rc::new(item_self.1.concat(&item_choices.1)),
                        ));
                    }
                }
                self.context_moves = new_self;
            }
        }
    }
}

impl Choices {
    fn iter(&self) -> std::slice::Iter<'_, (Rc<Set>, Rc<Process>)> {
        self.context_moves.iter()
    }
}

impl IntoIterator for Choices {
    type Item = (Rc<Set>, Rc<Process>);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.context_moves.into_iter()
    }
}

impl<const N: usize> From<[(Rc<Set>, Rc<Process>); N]> for Choices {
    fn from(arr: [(Rc<Set>, Rc<Process>); N]) -> Self {
        Choices {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<&[(Rc<Set>, Rc<Process>)]> for Choices {
    fn from(arr: &[(Rc<Set>, Rc<Process>)]) -> Self {
        Choices {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<Vec<(Rc<Set>, Rc<Process>)>> for Choices {
    fn from(arr: Vec<(Rc<Set>, Rc<Process>)>) -> Self {
        Choices { context_moves: arr }
    }
}

impl PrintableWithTranslator for Choices {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator) -> std::fmt::Result {
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
    context_moves: Vec<(Rc<PositiveSet>, Rc<PositiveProcess>)>,
}

impl BasicChoices for PositiveChoices {
    type Process = PositiveProcess;

    fn append(&mut self, a: &mut Self) {
        self.context_moves.append(&mut a.context_moves);
    }

    fn replace(&mut self, a: Rc<Self::Process>) {
        self.context_moves = self
            .context_moves
            .iter_mut()
            .map(|(c1, _)| (Rc::clone(c1), Rc::clone(&a)))
            .collect::<Vec<_>>();
    }

    fn shuffle(&mut self, choices: Self) {
        match (
            self.context_moves.is_empty(),
            choices.context_moves.is_empty(),
        ) {
            (true, true) => {}
            (true, false) => self.context_moves = choices.context_moves,
            (false, true) => {}
            (false, false) => {
                let mut new_self = vec![];
                for item_self in &self.context_moves {
                    for item_choices in &choices.context_moves {
                        new_self.push((
                            Rc::new(item_self.0.union(&item_choices.0)),
                            Rc::new(item_self.1.concat(&item_choices.1)),
                        ));
                    }
                }
                self.context_moves = new_self;
            }
        }
    }
}

impl IntoIterator for PositiveChoices {
    type Item = (Rc<PositiveSet>, Rc<PositiveProcess>);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.context_moves.into_iter()
    }
}

impl PrintableWithTranslator for PositiveChoices {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator) -> std::fmt::Result {
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
    fn iter(&self) -> std::slice::Iter<'_, (Rc<PositiveSet>, Rc<PositiveProcess>)> {
        self.context_moves.iter()
    }
}

impl<const N: usize> From<[(Rc<PositiveSet>, Rc<PositiveProcess>); N]> for PositiveChoices {
    fn from(arr: [(Rc<PositiveSet>, Rc<PositiveProcess>); N]) -> Self {
        Self {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<&[(Rc<PositiveSet>, Rc<PositiveProcess>)]> for PositiveChoices {
    fn from(arr: &[(Rc<PositiveSet>, Rc<PositiveProcess>)]) -> Self {
        Self {
            context_moves: arr.to_vec(),
        }
    }
}

impl From<Vec<(Rc<PositiveSet>, Rc<PositiveProcess>)>> for PositiveChoices {
    fn from(arr: Vec<(Rc<PositiveSet>, Rc<PositiveProcess>)>) -> Self {
        Self { context_moves: arr }
    }
}

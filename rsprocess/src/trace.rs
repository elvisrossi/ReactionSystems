use std::fmt::Debug;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use std::slice::SliceIndex;

use serde::{Deserialize, Serialize};

use crate::reaction::{BasicReaction, PositiveReaction, Reaction};
use crate::set::{BasicSet, PositiveSet, Set};
use crate::system::{BasicSystem, PositiveSystem, System};
use crate::translator::{Formatter, PrintableWithTranslator};

type TraceElement<L, Sys> = (Option<Rc<L>>, Rc<Sys>);

#[derive(Clone, Default)]
pub struct Trace<L, Sys> {
    pub values: Vec<TraceElement<L, Sys>>,
}

impl<L, Sys> Trace<L, Sys> {
    pub fn push(&mut self, val: TraceElement<L, Sys>) {
        self.values.push(val)
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

impl<L, Sys> From<&[TraceElement<L, Sys>]> for Trace<L, Sys> {
    fn from(value: &[TraceElement<L, Sys>]) -> Self {
        Self {
            values: value.to_vec(),
        }
    }
}

impl<L, Sys, const N: usize> From<&[TraceElement<L, Sys>; N]>
    for Trace<L, Sys>
{
    fn from(value: &[TraceElement<L, Sys>; N]) -> Self {
        Self {
            values: value.to_vec(),
        }
    }
}

impl<'a, L, Sys> From<&'a Vec<TraceElement<L, Sys>>> for Trace<L, Sys> {
    fn from(value: &'a Vec<TraceElement<L, Sys>>) -> Self {
        Self {
            values: value.to_vec(),
        }
    }
}

impl<L, Sys> From<Vec<TraceElement<L, Sys>>> for Trace<L, Sys> {
    fn from(value: Vec<TraceElement<L, Sys>>) -> Self {
        Self { values: value }
    }
}

impl<L, Sys, I: SliceIndex<[TraceElement<L, Sys>]>> Index<I> for Trace<L, Sys> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.values[index]
    }
}

impl<L, Sys, I: SliceIndex<[TraceElement<L, Sys>]>> IndexMut<I>
    for Trace<L, Sys>
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.values[index]
    }
}

// -----------------------------------------------------------------------------
//                               Slicing Trace
// -----------------------------------------------------------------------------
#[derive(Clone, Default, Hash, Serialize, Deserialize)]
pub struct SlicingElement<S> {
    pub context: S,
    pub reaction_products: S,
}

impl<S> From<(S, S)> for SlicingElement<S> {
    fn from(value: (S, S)) -> Self {
        Self {
            context: value.0,
            reaction_products: value.1,
        }
    }
}

impl<S> Debug for SlicingElement<S>
where
    S: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{context: {:?} - reaction_products: {:?}}}",
            self.context, self.reaction_products
        )
    }
}

impl<S> PrintableWithTranslator for SlicingElement<S>
where
    S: PrintableWithTranslator,
{
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        write!(
            f,
            "{{context: {}, ",
            Formatter::from(translator, &self.context)
        )?;
        write!(
            f,
            "reaction products: {}}}",
            Formatter::from(translator, &self.reaction_products)
        )
    }
}

#[derive(Clone, Default, Debug, Hash, Serialize, Deserialize)]
pub struct EnabledReactions {
    pub data: Vec<usize>,
}

impl From<Vec<usize>> for EnabledReactions {
    fn from(value: Vec<usize>) -> Self {
        Self { data: value }
    }
}

impl From<&[usize]> for EnabledReactions {
    fn from(value: &[usize]) -> Self {
        Self {
            data: value.to_vec(),
        }
    }
}

impl<const N: usize> From<[usize; N]> for EnabledReactions {
    fn from(value: [usize; N]) -> Self {
        Self {
            data: value.to_vec(),
        }
    }
}

impl PrintableWithTranslator for EnabledReactions {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        _translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        let mut elements = self.data.iter().peekable();
        while let Some(e) = elements.next() {
            if elements.peek().is_some() {
                write!(f, "{e}, ")?;
            } else {
                write!(f, "{e}")?;
            }
        }
        Ok(())
    }
}

impl<'a> IntoIterator for &'a EnabledReactions {
    type Item = &'a usize;
    type IntoIter = std::slice::Iter<'a, usize>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

impl EnabledReactions {
    pub fn iter(&self) -> std::slice::Iter<'_, usize> {
        self.into_iter()
    }
}

#[derive(Clone, Debug, Hash, Serialize, Deserialize)]
pub struct SlicingTrace<S, R, Sys> {
    pub elements: Vec<SlicingElement<S>>,
    pub enabled_reactions: Vec<EnabledReactions>,

    pub reactions: Rc<Vec<R>>,
    pub systems:   Vec<Rc<Sys>>,

    pub context_elements:  Rc<S>,
    pub products_elements: Rc<S>,
}

impl<S: Default, R, Sys> Default for SlicingTrace<S, R, Sys> {
    fn default() -> Self {
        Self {
            elements: Vec::default(),
            enabled_reactions: Vec::default(),
            reactions: Rc::new(Vec::default()),
            systems: Vec::default(),
            context_elements: Rc::new(S::default()),
            products_elements: Rc::new(S::default()),
        }
    }
}

impl<S, R, Sys> SlicingTrace<S, R, Sys> {
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }
}

impl SlicingTrace<Set, Reaction, System> {
    pub fn slice(&self, marking: Set) -> Result<Self, String> {
        let mut reversed_elements = Vec::with_capacity(self.elements.len());
        reversed_elements.push(SlicingElement {
            context: self
                .elements
                .last()
                .ok_or("Trace with length zero.")?
                .context
                .clone(),
            reaction_products: marking,
        });
        let mut reversed_enabled_reactions: Vec<EnabledReactions> =
            Vec::with_capacity(self.enabled_reactions.len());

        for i in (1..(self.len())).rev() {
            let reverse_i: usize = self.len() - i;
            let i = i - 1;
            if reversed_elements.len() <= reverse_i {
                reversed_elements.push(SlicingElement::default());
            }
            if reversed_enabled_reactions.len() <= reverse_i {
                reversed_enabled_reactions.push(EnabledReactions::default());
            }

            for r in self.enabled_reactions[i].iter() {
                if !reversed_elements[reverse_i - 1]
                    .reaction_products
                    .is_disjoint(self.reactions[*r].products())
                {
                    reversed_enabled_reactions[reverse_i - 1].data.push(*r);
                    reversed_elements[reverse_i].context.push(
                        &self.reactions[*r]
                            .reactants()
                            .intersection(&self.context_elements),
                    );
                    reversed_elements[reverse_i].reaction_products.push(
                        &self.reactions[*r]
                            .reactants()
                            .intersection(&self.products_elements),
                    )
                }
            }
        }

        reversed_elements.reverse();
        reversed_enabled_reactions.reverse();

        let new_trace = Self {
            elements: reversed_elements,
            enabled_reactions: reversed_enabled_reactions,
            reactions: Rc::clone(&self.reactions),
            systems: self.systems.to_vec(),
            context_elements: Rc::clone(&self.context_elements),
            products_elements: Rc::clone(&self.products_elements),
        };

        Ok(new_trace)
    }
}

impl SlicingTrace<PositiveSet, PositiveReaction, PositiveSystem> {
    pub fn slice(&self, marking: PositiveSet) -> Result<Self, String> {
        let mut reversed_elements = Vec::with_capacity(self.elements.len());
        reversed_elements.push(SlicingElement {
            context: self
                .elements
                .last()
                .ok_or("Trace with length zero.")?
                .context
                .clone(),
            reaction_products: marking,
        });
        let mut reversed_enabled_reactions: Vec<EnabledReactions> =
            Vec::with_capacity(self.enabled_reactions.len());

        for i in (1..(self.len())).rev() {
            let reverse_i: usize = self.len() - i;
            let i = i - 1;
            if reversed_elements.len() <= reverse_i {
                reversed_elements.push(SlicingElement::default());
            }
            if reversed_enabled_reactions.len() <= reverse_i {
                reversed_enabled_reactions.push(EnabledReactions::default());
            }

            for r in self.enabled_reactions[i].iter() {
                if !reversed_elements[reverse_i - 1]
                    .reaction_products
                    .intersection(self.reactions[*r].products())
                    .is_empty()
                {
                    reversed_enabled_reactions[reverse_i - 1].data.push(*r);
                    reversed_elements[reverse_i].context.push(
                        &self.reactions[*r]
                            .reactants()
                            .mask(&self.context_elements),
                    );
                    reversed_elements[reverse_i].reaction_products.push(
                        &self.reactions[*r]
                            .reactants()
                            .mask(&self.products_elements),
                    )
                }
            }
        }

        reversed_elements.reverse();
        reversed_enabled_reactions.reverse();

        let new_trace = Self {
            elements: reversed_elements,
            enabled_reactions: reversed_enabled_reactions,
            reactions: Rc::clone(&self.reactions),
            systems: self.systems.to_vec(),
            context_elements: Rc::clone(&self.context_elements),
            products_elements: Rc::clone(&self.products_elements),
        };

        Ok(new_trace)
    }
}

impl<
    S: BasicSet,
    R: BasicReaction<Set = S>,
    Sys: BasicSystem<Set = S, Reaction = R>,
> PrintableWithTranslator for SlicingTrace<S, R, Sys>
{
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        // let mut systems = self.systems.iter().peekable();
        // writeln!(f, "Systems:")?;
        // while let Some(system) = systems.next() {
        //     if systems.peek().is_some() {
        //         write!(f, "{} --> ", Formatter::from(translator,
        // &**system))?;     } else {
        //         writeln!(f, "{}", Formatter::from(translator, &**system))?;
        //     }
        // }

        let mut reactions = self.reactions.iter().enumerate().peekable();
        writeln!(f, "Reactions:")?;
        while let Some((pos, reaction)) = reactions.next() {
            if reactions.peek().is_some() {
                writeln!(
                    f,
                    "\t({pos}) {},",
                    Formatter::from(translator, reaction)
                )?;
            } else {
                writeln!(
                    f,
                    "\t({pos}) {}.",
                    Formatter::from(translator, reaction)
                )?;
            }
        }

        writeln!(
            f,
            "Context Elements: {}",
            Formatter::from(translator, &*self.context_elements)
        )?;
        writeln!(
            f,
            "Product Elements: {}",
            Formatter::from(translator, &*self.products_elements)
        )?;

        writeln!(f, "Trace:")?;

        let mut elements = self.elements.iter().peekable();
        let mut enabled_reactions = self.enabled_reactions.iter();

        while let Some(el) = elements.next() {
            if let Some(r) = enabled_reactions.next() {
                let reaction_string = format!("{}", Formatter::from(translator, r));
                let reaction_string = if reaction_string.is_empty() {
                    "( )"
                } else {
                    &format!("({})", reaction_string)
                };
                if elements.peek().is_some() {
                    writeln!(
                        f,
                        "{}\n        |\n{: ^18}\n        |\n        v",
                        Formatter::from(translator, el),
                        reaction_string,
                    )?;
                } else {
                    writeln!(
                        f,
                        "{}\n        |\n{: ^18}\n        |\n        ?",
                        Formatter::from(translator, el),
                        reaction_string,
                    )?;
                }
            } else if elements.peek().is_some() {
                writeln!(
                    f,
                    "{}\n        |\n        |\n        |\n        v",
                    Formatter::from(translator, el)
                )?;
            } else {
                writeln!(f, "{}", Formatter::from(translator, el))?;
            }
        }

        Ok(())
    }
}

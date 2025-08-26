use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::hash::Hash;
use std::rc::Rc;

use super::reaction::Reaction;
use super::set::{Set, BasicSet};
use super::translator::{IdType, Translator, PrintableWithTranslator, Formatter};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Process {
    Nill,
    RecursiveIdentifier {
	identifier: IdType,
    },
    EntitySet {
	entities: Set,
	next_process: Rc<Process>,
    },
    Guarded {
	reaction: Reaction,
	next_process: Rc<Process>,
    },
    WaitEntity {
	repeat: i64,
	repeated_process: Rc<Process>,
	next_process: Rc<Process>,
    },
    Summation {
	children: Vec<Rc<Process>>,
    },
    NondeterministicChoice {
	children: Vec<Rc<Process>>,
    },
}

impl Process {
    // TODO: remove all the clone()
    pub fn concat(&self, new: &Process) -> Process {
	match (self, new) {
	    (
		Process::NondeterministicChoice { children: c1 },
		Process::NondeterministicChoice { children: c2 },
	    ) => Process::NondeterministicChoice {
		children: [c1.clone(), c2.clone()].concat(),
	    },
	    (Process::NondeterministicChoice { children }, new)
	    | (new, Process::NondeterministicChoice { children }) => {
		let mut new_children = children.clone();
		new_children.push(Rc::new(new.clone()));
		Process::NondeterministicChoice {
		    children: new_children,
		}
	    }
	    (_, _) => Process::NondeterministicChoice {
		children: vec![Rc::new(self.clone()), Rc::new(new.clone())],
	    },
	}
    }

    /// returns all elements used
    pub fn all_elements(&self) -> Set {
	let mut queue = VecDeque::from([self]);
	let mut elements = Set::default();

	while let Some(el) = queue.pop_front() {
	    match el {
		Self::Nill => {}
		Self::RecursiveIdentifier { identifier: _ } => {}
		Self::EntitySet {
		    entities,
		    next_process,
		} => {
		    elements.push(entities);
		    queue.push_back(next_process);
		}
		Self::Guarded { reaction, next_process } => {
		    elements.push(&reaction.reactants);
		    elements.push(&reaction.inhibitors);
		    elements.push(&reaction.products);
		    queue.push_back(next_process);
		}
		Self::WaitEntity {
		    repeat: _,
		    repeated_process,
		    next_process,
		} => {
		    queue.push_back(repeated_process);
		    queue.push_back(next_process);
		}
		Self::Summation { children } => {
		    for c in children {
			queue.push_back(c);
		    }
		}
		Self::NondeterministicChoice { children } => {
		    for c in children {
			queue.push_back(c);
		    }
		}
	    }
	}
	elements
    }


    /// Finds only the rules X = pre(Q, rec(X)), but not only x = pre(Q, rec(x))
    /// to use in filter_map.
    pub fn filter_delta<'a>(
	&'a self,
	id: &IdType
    ) -> Option<&'a Set> {
	if let Self::EntitySet { entities, next_process } = self
	    && let Self::RecursiveIdentifier { identifier } = &**next_process
	    && identifier == id
	{
	    return Some(entities);
	}

	None
    }
}

impl PrintableWithTranslator for Process {
    fn print(&self, f: &mut std::fmt::Formatter, translator: &Translator)
	     -> std::fmt::Result {
	match self {
	    Self::Nill => {
		write!(f, "Nill")
	    },
	    Self::RecursiveIdentifier { identifier } => {
		write!(f,
		       "[{}]",
		       translator
		       .decode(*identifier)
		       .unwrap_or("Missing".into()))
	    },
	    Self::EntitySet { entities, next_process, } => {
		write!(
		    f,
		    "{}.{}",
		    Formatter::from(translator, entities),
		    Formatter::from(translator, &**next_process)
		)
	    },
	    Self::Guarded { reaction, next_process } => {
		write!(f,
		       "?{}?.{}",
		       Formatter::from(translator, reaction),
		       Formatter::from(translator, &**next_process))
	    },
	    Self::WaitEntity { repeat, repeated_process, next_process, } => {
		write!(
		    f,
		    "({})^{repeat}.{}",
		    Formatter::from(translator, &**repeated_process),
		    Formatter::from(translator, &**next_process)
		)
	    }
	    Self::Summation { children } => {
		write!(f, "[")?;
		let mut it = children.iter().peekable();
		while let Some(child) = it.next() {
		    if it.peek().is_none() {
			write!(
			    f,
			    "{}",
			    Formatter::from(translator, &**child)
			)?;
		    } else {
			write!(
			    f,
			    "{} + ",
			    Formatter::from(translator, &**child)
			)?;
		    }
		}
		write!(f, "]")
	    }
	    Self::NondeterministicChoice { children } => {
		write!(f, "[")?;
		let mut it = children.iter().peekable();
		while let Some(child) = it.next() {
		    if it.peek().is_none() {
			write!(
			    f,
			    "{}",
			    Formatter::from(translator, &**child)
			)?;
		    } else {
			write!(
			    f,
			    "{}, ",
			    Formatter::from(translator, &**child)
			)?;
		    }
		}
		write!(f, "]")
	    }
	}
    }
}

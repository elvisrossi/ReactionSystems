use std::rc::Rc;

use super::set::Set;
use super::process::Process;

#[derive(Clone, Debug)]
pub struct Choices {
    context_moves: Vec<(Rc<Set>, Rc<Process>)>,
}

impl Choices {
    pub fn new() -> Self {
	Choices {
	    context_moves: vec![],
	}
    }

    pub fn new_not_empty() -> Self {
	Choices {
	    context_moves: vec![(Rc::new(Set::new()),
				 Rc::new(Process::Nill))],
	}
    }

    pub fn append(&mut self, a: &mut Choices) {
	self.context_moves.append(&mut a.context_moves);
    }

    pub fn replace(&mut self, a: Rc<Process>) {
	self.context_moves = self
	    .context_moves
	    .iter_mut()
	    .map(|(c1, _)| (Rc::clone(c1), Rc::clone(&a)))
	    .collect::<Vec<_>>();
    }

    pub fn shuffle(&mut self, choices: Choices) {
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

    pub fn iter(&self) -> std::slice::Iter<'_, (Rc<Set>, Rc<Process>)> {
	self.context_moves.iter()
    }
}

impl Default for Choices {
    fn default() -> Self {
	Self::new()
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

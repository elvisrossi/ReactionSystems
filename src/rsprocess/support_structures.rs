#![allow(dead_code)]
use std::rc::Rc;
use super::structure::{RSsystem, RSlabel, RSset, RSprocess};
use super::transitions::unfold;

#[derive(Clone, Debug)]
pub struct TransitionsIterator<'a> {
    choices_iterator: std::vec::IntoIter<(Rc<RSset<'a>>, Rc<RSprocess<'a>>)>,
    system: &'a RSsystem<'a>
}

impl<'a> TransitionsIterator<'a> {
    pub fn from(system: &'a RSsystem<'a>) -> Result<TransitionsIterator<'a>, String> {
	match unfold(system.get_delta(), system.get_context_process()) {
	    Ok(o) => Ok(
		TransitionsIterator {
		    choices_iterator: o.into_iter(),
		    system
		}
	    ),
	    Err(e) => Err(e)
	}
    }
}

impl<'a> Iterator for TransitionsIterator<'a> {
    type Item = (RSlabel<'a>, RSsystem<'a>);

    fn next(&mut self) -> Option<(RSlabel<'a>, RSsystem<'a>)> {
	let choice = self.choices_iterator.next()?;
	let t = self.system.get_available_entities().union(choice.0.as_ref());
	let (reactants, reactantsi, inihibitors, ireactants, products)
	    = self.system.get_reaction_rules().iter()
	    .fold((RSset::new(), RSset::new(), RSset::new(), RSset::new(), RSset::new()),
		  |acc, reaction| if reaction.enabled(&t) {
		      (acc.0.union(reaction.reactants()),
		       acc.1,
		       acc.2.union(reaction.inihibitors()),
		       acc.3,
		       acc.4.union(reaction.products()))
		  } else {
		      (acc.0,
		       acc.1.union(&reaction.inihibitors().intersection(&t)),
		       acc.2,
		       acc.3.union(&reaction.reactants().subtraction(&t)),
		       acc.4)
		  }

	    );

	let label = RSlabel::from(self.system.get_available_entities().clone(),
				  (*choice.0).clone(),
				  t,
				  reactants,
				  reactantsi,
				  inihibitors,
				  ireactants,
				  products.clone());
	let new_system = RSsystem::from(self.system.get_delta().clone(),
					products,
					(*choice.1).clone(),
					self.system.get_reaction_rules().clone());
	Some((label, new_system))
    }
}

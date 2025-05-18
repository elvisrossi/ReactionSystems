use super::structure::{RSset, RSreaction};

pub fn compute_step<'a>(current_state: &RSset<'a>, reaction: &RSreaction<'a>) -> RSset<'a> {
    if reaction.enabled(current_state) {
	reaction.products_clone()
    } else {
	RSset::new()
    }
}

pub fn compute_all<'a>(current_state: &RSset<'a>, reactions: Vec<&RSreaction<'a>>) -> RSset<'a> {
    reactions.iter().fold(RSset::new(), |acc, r| acc.union(&compute_step(current_state, r)))
}

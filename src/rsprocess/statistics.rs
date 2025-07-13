//! Non simulated statistics of a system.

use super::structure::RSset;
use super::structure::RSsystem;
use super::translator;
use super::translator::Translator;

/// Returns statistics about the system.
/// see main_do(stat,MissingE)
#[allow(non_snake_case)]
pub fn of_RSsystem<'a>(translator: &'a Translator, system: &'a RSsystem) -> String {
    let mut result: String = "Statistics:\n".into();
    result.push_str(
	"=============================================================\n"
    );
    result.push_str(&format!(
        "the initial state has {} entities:\n",
        system.available_entities.len()
    ));
    result.push_str(&format!(
        "{}\n",
        translator::RSsetDisplay::from(translator, &system.available_entities)
    ));

    let reactants = system
        .reaction_rules
        .iter()
        .fold(RSset::new(), |acc, new| acc.union(&new.reactants));
    result.push_str(&format!(
        "The reactants are {}:\n{}\n",
        reactants.len(),
        translator::RSsetDisplay::from(translator, &reactants)
    ));

    let inhibitors = system
        .reaction_rules
        .iter()
        .fold(RSset::new(), |acc, new| acc.union(&new.inhibitors));
    result.push_str(&format!(
        "The inhibitors are {}:\n{}\n",
        inhibitors.len(),
        translator::RSsetDisplay::from(translator, &inhibitors)
    ));

    let products = system
        .reaction_rules
        .iter()
        .fold(RSset::new(), |acc, new| acc.union(&new.products));
    result.push_str(&format!(
        "The products are {}:\n{}\n",
        products.len(),
        translator::RSsetDisplay::from(translator, &products)
    ));

    let total = reactants.union(&inhibitors.union(&products));
    result.push_str(&format!(
        "The reactions involve {} entities:\n{}\n",
        total.len(),
        translator::RSsetDisplay::from(translator, &total)
    ));

    let entities_env = system.delta.all_elements();
    result.push_str(&format!(
        "The environment involves {} entities:\n{}\n",
        entities_env.len(),
        translator::RSsetDisplay::from(translator, &entities_env)
    ));

    let entities_context = system.context_process.all_elements();
    result.push_str(&format!(
        "The context involves {} entities:\n{}\n",
        entities_context.len(),
        translator::RSsetDisplay::from(translator, &entities_context)
    ));

    let entities_all = total
        .union(&entities_env)
        .union(&entities_context)
        .union(&system.available_entities);

    result.push_str(&format!(
        "The whole RS involves {} entities:\n{}\n",
        entities_all.len(),
        translator::RSsetDisplay::from(translator, &entities_all)
    ));

    let possible_e = products
        .union(&system.available_entities)
        .union(&entities_context);
    let missing_e = reactants.subtraction(&possible_e);
    result.push_str(&format!(
        "There are {} reactants that will never be available:\n{}\n",
        missing_e.len(),
        translator::RSsetDisplay::from(translator, &missing_e)
    ));

    let entities_not_needed = entities_context.subtraction(&total);
    result.push_str(&format!(
        "The context can provide {} entities that will never be used:\n{}\n",
        entities_not_needed.len(),
        translator::RSsetDisplay::from(translator, &entities_not_needed)
    ));

    result.push_str(&format!(
        "There are {} reactions in total.\n",
        system.reaction_rules.len()
    ));

    let mut admissible_reactions = vec![];
    let mut nonadmissible_reactions = vec![];

    for reaction in system.reaction_rules.iter() {
        if reaction.reactants.is_disjoint(&missing_e) {
            admissible_reactions.push(reaction);
        } else {
            nonadmissible_reactions.push(reaction);
        }
    }

    result.push_str(&format!(
        "- the applicable reactions are {}.\n",
        admissible_reactions.len()
    ));

    result.push_str(&format!(
        "- there are {} reactions that will never be enabled.\n",
        nonadmissible_reactions.len()
    ));
    result.push_str(
	"============================================================="
    );

    result
}

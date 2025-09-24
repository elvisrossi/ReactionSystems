use std::rc::Rc;

use crate::element::IdState;
use crate::reaction::{BasicReaction, PositiveReaction, Reaction};
use crate::set::{ExtensionsSet, PositiveSet, Set, BasicSet};
use crate::system::{PositiveSystem, System};
use crate::trace::*;
use crate::translator::Translator;

#[test]
fn slice_atoi() {
    let mut translator = Translator::new();

    let reactions = vec![
        (
            vec!["tgfbr", "stat3", "il6r"],
            vec!["tbet", "gata3", "foxp3"],
            vec!["rorgt"],
        ),
        (
            vec!["tgfbr", "stat3", "il21r"],
            vec!["tbet", "gata3", "foxp3"],
            vec!["rorgt"],
        ),
        (vec!["il23r"], vec![], vec!["stat3"]),
        (vec!["il21"], vec![], vec!["il21r"]),
        (vec!["il6"], vec![], vec!["il6r"]),
        (vec!["tcr"], vec!["foxp3"], vec!["nfat"]),
        (vec!["il27", "nfat"], vec![], vec!["stat1"]),
        (vec!["stat1"], vec!["rorgt", "foxp3"], vec!["tbet"]),
    ]
    .iter()
    .map(|r| {
        Reaction::from(
            r.0.iter()
                .map(|el| translator.encode(*el))
                .collect::<Vec<_>>()
                .into(),
            r.1.iter()
                .map(|el| translator.encode(*el))
                .collect::<Vec<_>>()
                .into(),
            r.2.iter()
                .map(|el| translator.encode(*el))
                .collect::<Vec<_>>()
                .into(),
        )
    })
    .collect::<Vec<_>>();

    let elements = [
        (vec!["il23r", "il21"], vec!["tcr"]),
        (vec!["il21r", "stat3", "nfat"], vec!["il27"]),
        (vec!["stat1"], vec![]),
        (vec!["tbet"], vec![]),
    ]
    .iter()
    .map(|elements| SlicingElement {
        context: elements
            .1
            .iter()
            .map(|el| translator.encode(*el))
            .collect::<Vec<_>>()
            .into(),
        reaction_products: elements
            .0
            .iter()
            .map(|el| translator.encode(*el))
            .collect::<Vec<_>>()
            .into(),
    })
    .collect::<Vec<_>>();
    let enabled_reactions = vec![vec![3, 4, 6], vec![7], vec![8]]
        .into_iter()
        .map(|r| EnabledReactions {
            data: r.iter().map(|i| i - 1).collect::<Vec<_>>(),
        })
        .collect::<Vec<_>>();

    let context_elements = ["tgfb", "il6", "tcr", "il27"]
        .iter()
        .map(|el| translator.encode(*el))
        .collect::<Vec<_>>()
        .into();
    let products_elements =
        ["rorgt", "stat3", "il21r", "il6r", "nfat", "stat1", "tbet", "il21",
         "il23r"]
            .iter()
            .map(|el| translator.encode(*el))
            .collect::<Vec<_>>()
            .into();

    let trace: SlicingTrace<Set, Reaction, System> = SlicingTrace {
        elements,
        enabled_reactions,

        reactions: Rc::new(reactions),
        systems: vec![],
        context_elements: Rc::new(context_elements),
        products_elements: Rc::new(products_elements),
    };

    let marking = ["tbet"]
        .iter()
        .map(|el| translator.encode(*el))
        .collect::<Vec<_>>()
        .into();

    let sliced = trace.slice(marking).unwrap();

    let mut reaction_products = sliced
        .elements
        .iter()
        .map(|elements| {
            elements
                .reaction_products
                .iter()
                .map(|el| translator.decode(*el).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    reaction_products.iter_mut().for_each(|x| x.sort());
    let mut correct_reaction_products =
        vec![vec![], vec!["nfat"], vec!["stat1"], vec!["tbet"]];
    correct_reaction_products.iter_mut().for_each(|x| x.sort());

    assert_eq!(reaction_products, correct_reaction_products);

    let mut context = sliced
        .elements
        .iter()
        .map(|elements| {
            elements
                .context
                .iter()
                .map(|el| translator.decode(*el).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    context.iter_mut().for_each(|x| x.sort());
    let mut correct_context = vec![vec!["tcr"], vec!["il27"], vec![], vec![]];
    correct_context.iter_mut().for_each(|x| x.sort());

    assert_eq!(context, correct_context);

    let mut enabled_reactions = sliced
        .enabled_reactions
        .iter()
        .map(|elements| elements.data.clone())
        .collect::<Vec<_>>();
    enabled_reactions.iter_mut().for_each(|x| x.sort());
    let mut correct_enabled_reactions = vec![vec![5], vec![6], vec![7]];
    correct_enabled_reactions.iter_mut().for_each(|x| x.sort());

    assert_eq!(enabled_reactions, correct_enabled_reactions);
}

#[test]
fn slice_positive_atoi() {
    let mut translator = Translator::new();

    let reactions = {
        let reactions = vec![
            (
                vec!["tgfbr", "stat3", "il6r"],
                vec!["tbet", "gata3", "foxp3"],
                vec!["rorgt"],
            ),
            (
                vec!["tgfbr", "stat3", "il21r"],
                vec!["tbet", "gata3", "foxp3"],
                vec!["rorgt"],
            ),
            (vec!["il23r"], vec![], vec!["stat3"]),
            (vec!["il21"], vec![], vec!["il21r"]),
            (vec!["il6"], vec![], vec!["il6r"]),
            (vec!["tcr"], vec!["foxp3"], vec!["nfat"]),
            (vec!["il27", "nfat"], vec![], vec!["stat1"]),
            (vec!["stat1"], vec!["rorgt", "foxp3"], vec!["tbet"]),
        ]
        .iter()
        .map(|r| {
            Reaction::from(
                r.0.iter()
                    .map(|el| translator.encode(*el))
                    .collect::<Vec<_>>()
                    .into(),
                r.1.iter()
                    .map(|el| translator.encode(*el))
                    .collect::<Vec<_>>()
                    .into(),
                r.2.iter()
                    .map(|el| translator.encode(*el))
                    .collect::<Vec<_>>()
                    .into(),
            )
        })
        .collect::<Vec<_>>();

        let system = System::from(
            Rc::new(crate::environment::Environment::from([])),
            Set::from([]),
            crate::process::Process::Nill,
            Rc::new(reactions),
        );

        let converted_system: PositiveSystem = system.into();
        let mut reactions = Rc::try_unwrap(converted_system.reaction_rules).unwrap();
        reactions.sort_by(|a, b| a.reactants.cmp(&b.reactants)
                          .then(a.products.cmp(&b.products)));

        println!("Computed Reactions:");
        for (pos, r) in reactions.iter().enumerate() {
            println!("\t({pos}) {},", crate::translator::Formatter::from(&translator, r));
        }
        reactions
    };

    let elements = [
        (vec![("il23r", IdState::Positive), ("il21", IdState::Positive),
              ("rorgt", IdState::Negative), ("stat3", IdState::Negative),
              ("il21r", IdState::Negative), ("il6r", IdState::Negative),
              ("nfat", IdState::Negative), ("stat1", IdState::Negative),
              ("tbet", IdState::Negative), ("tgfbr", IdState::Negative),
              ("foxp3", IdState::Negative)],
         vec![("tcr", IdState::Positive), ("tgfb", IdState::Negative),
              ("il6", IdState::Negative), ("il27", IdState::Negative)]),
        (vec![("il21r", IdState::Positive), ("stat3", IdState::Positive),
              ("nfat", IdState::Positive), ("rorgt", IdState::Negative),
              ("il6r", IdState::Negative), ("stat1", IdState::Negative),
              ("tbet", IdState::Negative), ("il21", IdState::Negative),
              ("il23r", IdState::Negative), ("tgfbr", IdState::Negative),
              ("foxp3", IdState::Negative)],
         vec![("il27", IdState::Positive), ("tgfb", IdState::Negative),
              ("il6", IdState::Negative), ("tcr", IdState::Negative)]),
        (vec![("stat1", IdState::Positive), ("rorgt", IdState::Negative),
              ("stat3", IdState::Negative), ("il21r", IdState::Negative),
              ("il6r", IdState::Negative), ("nfat", IdState::Negative),
              ("tbet", IdState::Negative), ("il21", IdState::Negative),
              ("il23r", IdState::Negative), ("tgfbr", IdState::Negative),
              ("foxp3", IdState::Negative)],
         vec![("il27", IdState::Negative), ("tgfb", IdState::Negative),
              ("il6", IdState::Negative), ("tcr", IdState::Negative)]),
        (vec![("tbet", IdState::Positive), ("rorgt", IdState::Negative),
              ("stat3", IdState::Negative), ("il21r", IdState::Negative),
              ("il6r", IdState::Negative), ("nfat", IdState::Negative),
              ("stat1", IdState::Negative), ("il21", IdState::Negative),
              ("il23r", IdState::Negative), ("tgfbr", IdState::Negative),
              ("foxp3", IdState::Negative)],
         vec![]),
    ]
        .iter()
        .map(|elements| SlicingElement {
            context: Into::<PositiveSet>::into(
                elements
                    .1
                    .iter()
                    .map(|el| (translator.encode(el.0), el.1))
                    .collect::<Vec<_>>()),
            reaction_products: Into::<PositiveSet>::into(
                elements
                    .0
                    .iter()
                    .map(|el| (translator.encode(el.0), el.1))
                    .collect::<Vec<_>>()),
        })
        .collect::<Vec<_>>();


    let enabled_reactions = {
        let mut enabled_reactions = vec![];
        for slice_el in elements.iter().rev().skip(1).rev() {
            let available_enteties = slice_el.context.union(&slice_el.reaction_products);
            enabled_reactions.push(vec![]);
            for (pos, r) in reactions.iter().enumerate() {
                if r.enabled(&available_enteties) {
                    enabled_reactions.last_mut().unwrap().push(pos);
                }
            }
        }
        enabled_reactions.into_iter()
            .map(|r| EnabledReactions {
                data: r,
            })
            .collect::<Vec<_>>()
    };

    let context_elements =
        Into::<Set>::into(
            ["tgfb", "il6", "tcr", "il27"]
                .iter()
                .map(|el| translator.encode(*el))
                .collect::<Vec<_>>()
        ).to_positive_set(IdState::Positive);
    let products_elements =
        Into::<Set>::into(
            ["rorgt", "stat3", "il21r", "il6r", "nfat", "stat1", "tbet", "il21",
             "il23r", "tgfbr", "foxp3"]
                .iter()
                .map(|el| translator.encode(*el))
                .collect::<Vec<_>>()
        ).to_positive_set(IdState::Positive);

    let trace: SlicingTrace<PositiveSet, PositiveReaction, PositiveSystem> =
        SlicingTrace {
            elements,
            enabled_reactions,

            reactions: Rc::new(reactions),
            systems: vec![],
            context_elements: Rc::new(context_elements),
            products_elements: Rc::new(products_elements),
        };

    let marking =
        Into::<Set>::into(
            ["tbet"]
                .iter()
                .map(|el| translator.encode(*el))
                .collect::<Vec<_>>()
        ).to_positive_set(IdState::Positive);

    let sliced = trace.slice(marking).unwrap();

    let mut reaction_products = sliced
        .elements
        .iter()
        .map(|elements| {
            elements
                .reaction_products
                .iter()
                .map(|el| (translator.decode(*el.0).unwrap(), *el.1))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    reaction_products.iter_mut().for_each(|x| x.sort());
    let mut correct_reaction_products: Vec<Vec<(String, IdState)>> =
        [vec![("foxp3", IdState::Negative)],
         vec![("nfat", IdState::Positive), ("tgfbr", IdState::Negative)],
         vec![("stat1", IdState::Positive), ("foxp3", IdState::Negative),
              ("rorgt", IdState::Negative)],
         vec![("tbet", IdState::Positive)]]
        .iter().map(|x| x.iter().map(|y| (y.0.to_string(), y.1))
                    .collect::<Vec<_>>()).collect::<Vec<_>>();
    correct_reaction_products.iter_mut().for_each(|x| x.sort());

    assert_eq!(reaction_products, correct_reaction_products);

    let mut context = sliced
        .elements
        .iter()
        .map(|elements| {
            elements
                .context
                .iter()
                .map(|el| (translator.decode(*el.0).unwrap(), *el.1))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    context.iter_mut().for_each(|x| x.sort());
    let mut correct_context =
        [vec![("tcr", IdState::Positive)],
         vec![("il27", IdState::Positive)],
         vec![],
         vec![]]
        .iter().map(|x| x.iter().map(|y| (y.0.to_string(), y.1))
                    .collect::<Vec<_>>()).collect::<Vec<_>>();
    correct_context.iter_mut().for_each(|x| x.sort());

    assert_eq!(context, correct_context);

    let mut enabled_reactions = sliced
        .enabled_reactions
        .iter()
        .map(|elements| elements.data.clone())
        .collect::<Vec<_>>();
    enabled_reactions.iter_mut().for_each(|x| x.sort());
    let mut correct_enabled_reactions = vec![vec![11], vec![2, 20], vec![10]];
    correct_enabled_reactions.iter_mut().for_each(|x| x.sort());

    assert_eq!(enabled_reactions, correct_enabled_reactions);
}

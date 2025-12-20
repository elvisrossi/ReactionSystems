use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::element::IdType;
use crate::translator::{Formatter, PrintableWithTranslator};

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BooleanFunction {
    #[default]
    False,
    True,
    Not(Box<BooleanFunction>),
    Variable(IdType),
    And(Box<BooleanFunction>, Box<BooleanFunction>),
    Or(Box<BooleanFunction>, Box<BooleanFunction>),
}

impl BooleanFunction {
    pub fn evaluate(&self, assignments: &BTreeMap<IdType, bool>) -> bool {
        match self {
            | Self::False => false,
            | Self::True => true,
            | Self::Not(bf) => !bf.evaluate(assignments),
            | Self::Variable(i) => *assignments.get(i).unwrap_or(&false),
            | Self::And(bf1, bf2) =>
                bf1.evaluate(assignments) && bf2.evaluate(assignments),
            | Self::Or(bf1, bf2) =>
                bf1.evaluate(assignments) || bf2.evaluate(assignments),
        }
    }

    pub fn remove_literals(&self) -> Self {
        match self {
            | Self::False => Self::False,
            | Self::True => Self::True,
            | Self::Not(bf) => match &**bf {
                | Self::False => Self::True,
                | Self::True => Self::False,
                | Self::Not(bf) => bf.remove_literals(),
                | _ => bf.remove_literals(),
            },
            | Self::Variable(i) => Self::Variable(*i),
            | Self::And(bf1, bf2) => match (&**bf1, &**bf2) {
                | (Self::False, _) => Self::False,
                | (_, Self::False) => Self::False,
                | (Self::True, Self::True) => Self::True,
                | (Self::True, bf) => bf.remove_literals(),
                | (bf, Self::True) => bf.remove_literals(),
                | (bf1, bf2) => Self::And(
                    Box::new(bf1.remove_literals()),
                    Box::new(bf2.remove_literals()),
                ),
            },
            | Self::Or(bf1, bf2) => match (&**bf1, &**bf2) {
                | (Self::True, _) => Self::True,
                | (_, Self::True) => Self::True,
                | (Self::False, Self::False) => Self::False,
                | (Self::False, bf) => bf.remove_literals(),
                | (bf, Self::False) => bf.remove_literals(),
                | (bf1, bf2) => Self::Or(
                    Box::new(bf1.remove_literals()),
                    Box::new(bf2.remove_literals()),
                ),
            },
        }
    }
}

impl PrintableWithTranslator for BooleanFunction {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        use BooleanFunction::*;
        match self {
            | False => write!(f, "False"),
            | True => write!(f, "True"),
            | Not(next) =>
                write!(f, "Not({})", Formatter::from(translator, &**next)),
            | Variable(x) => write!(f, "{})", Formatter::from(translator, x)),
            | And(next1, next2) => write!(
                f,
                "And({}, {})",
                Formatter::from(translator, &**next1),
                Formatter::from(translator, &**next2)
            ),
            | Or(next1, next2) => write!(
                f,
                "Or({}, {})",
                Formatter::from(translator, &**next1),
                Formatter::from(translator, &**next2)
            ),
        }
    }
}

// -----------------------------------------------------------------------------
// CNF
// -----------------------------------------------------------------------------

#[derive(
    Default, Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize,
)]
pub enum CNFLiteral {
    #[default]
    False,
    True,
    Variable {
        positive: bool,
        variable: IdType,
    },
}

impl std::ops::Not for CNFLiteral {
    type Output = CNFLiteral;

    fn not(self) -> Self::Output {
        match self {
            | Self::False => Self::True,
            | Self::True => Self::False,
            | Self::Variable { positive, variable } => Self::Variable {
                positive: !positive,
                variable,
            },
        }
    }
}

impl CNFLiteral {
    pub fn evaluate(&self, assignments: &BTreeMap<IdType, bool>) -> bool {
        match self {
            | Self::False => false,
            | Self::True => true,
            | Self::Variable { positive, variable } =>
                if *positive {
                    *assignments.get(variable).unwrap_or(&false)
                } else {
                    !*assignments.get(variable).unwrap_or(&false)
                },
        }
    }
}

impl PrintableWithTranslator for CNFLiteral {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        use CNFLiteral::*;
        match self {
            | False => write!(f, "F"),
            | True => write!(f, "T"),
            | Variable { positive, variable } =>
                if *positive {
                    write!(f, "{}", Formatter::from(translator, variable))
                } else {
                    write!(f, "-{}", Formatter::from(translator, variable))
                },
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
// first vec is And, second in Or
pub struct CNFBooleanFunction {
    pub formula: Vec<Vec<CNFLiteral>>,
}

impl PrintableWithTranslator for CNFBooleanFunction {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        let mut it = self.formula.iter().peekable();
        while let Some(or_formula) = it.next() {
            let mut or_it = or_formula.iter().peekable();
            write!(f, "(")?;
            while let Some(or) = or_it.next() {
                if or_it.peek().is_none() {
                    write!(f, "{}", Formatter::from(translator, or))?;
                } else {
                    write!(f, "{} v ", Formatter::from(translator, or))?;
                }
            }
            if it.peek().is_none() {
                write!(f, ")")?;
            } else {
                writeln!(f, ") ^")?;
            }
        }
        Ok(())
    }
}

impl From<BooleanFunction> for CNFBooleanFunction {
    fn from(source: BooleanFunction) -> Self {
        fn morgan(source: CNFBooleanFunction) -> CNFBooleanFunction {
            let temp: Vec<Vec<_>> = source
                .formula
                .into_iter()
                .map(|f| f.into_iter().map(|l| !l).collect())
                .collect();

            let lenghts: Vec<usize> = temp.iter().map(|f| f.len()).collect();
            let mut position = vec![0; temp.len()];

            let add_one = |position: &mut Vec<usize>| -> bool {
                let mut location: usize = 0;
                loop {
                    if location >= position.len() {
                        return true;
                    }
                    position[location] += 1;
                    if position[location] >= lenghts[location] {
                        position[location] = 0;
                    } else {
                        return false;
                    }
                    location += 1;
                }
            };

            let mut ret_val = vec![];
            loop {
                ret_val.push(
                    position
                        .iter()
                        .enumerate()
                        .map(|(pos, p)| temp[pos][*p])
                        .collect(),
                );

                if add_one(&mut position) {
                    break;
                }
            }
            CNFBooleanFunction { formula: ret_val }
        }

        fn helper_normalize(source: BooleanFunction) -> CNFBooleanFunction {
            match &source {
                | BooleanFunction::False => CNFBooleanFunction {
                    formula: vec![vec![CNFLiteral::False]],
                },
                | BooleanFunction::True => CNFBooleanFunction {
                    formula: vec![vec![CNFLiteral::True]],
                },
                | BooleanFunction::Variable(v) => CNFBooleanFunction {
                    formula: vec![vec![CNFLiteral::Variable {
                        positive: true,
                        variable: *v,
                    }]],
                },
                | BooleanFunction::And(n1, n2) => {
                    let n1 = helper_normalize(*n1.clone());
                    let n2 = helper_normalize(*n2.clone());

                    CNFBooleanFunction {
                        formula: n1
                            .formula
                            .into_iter()
                            .chain(n2.formula)
                            .collect(),
                    }
                },
                | BooleanFunction::Or(n1, n2) => {
                    let n1 = helper_normalize(*n1.clone());
                    let n2 = helper_normalize(*n2.clone());

                    let mut formulas = vec![];
                    for formula1 in n1.formula {
                        for formula2 in &n2.formula {
                            formulas.push(
                                formula1
                                    .iter()
                                    .chain(formula2.iter())
                                    .cloned()
                                    .collect(),
                            );
                        }
                    }
                    CNFBooleanFunction { formula: formulas }
                },
                | BooleanFunction::Not(n) => {
                    if let BooleanFunction::Not(n) = &**n {
                        helper_normalize(*n.clone())
                    } else {
                        morgan(helper_normalize(*n.clone()))
                    }
                },
            }
        }

        helper_normalize(source)
    }
}

impl CNFBooleanFunction {
    pub fn evaluate(&self, assignments: &BTreeMap<IdType, bool>) -> bool {
        self.formula
            .iter()
            .all(|or_f| or_f.iter().any(|l| l.evaluate(assignments)))
    }
}

// -----------------------------------------------------------------------------
// DNF
// -----------------------------------------------------------------------------

#[derive(
    Default, Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize,
)]
pub enum DNFLiteral {
    #[default]
    False,
    True,
    Variable {
        positive: bool,
        variable: IdType,
    },
}

impl std::ops::Not for DNFLiteral {
    type Output = DNFLiteral;

    fn not(self) -> Self::Output {
        match self {
            | Self::False => Self::True,
            | Self::True => Self::False,
            | Self::Variable { positive, variable } => Self::Variable {
                positive: !positive,
                variable,
            },
        }
    }
}

impl DNFLiteral {
    pub fn evaluate(&self, assignments: &BTreeMap<IdType, bool>) -> bool {
        match self {
            | Self::False => false,
            | Self::True => true,
            | Self::Variable { positive, variable } =>
                if *positive {
                    *assignments.get(variable).unwrap_or(&false)
                } else {
                    !*assignments.get(variable).unwrap_or(&false)
                },
        }
    }
}

impl PrintableWithTranslator for DNFLiteral {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        use DNFLiteral::*;
        match self {
            | False => write!(f, "F"),
            | True => write!(f, "T"),
            | Variable { positive, variable } =>
                if *positive {
                    write!(f, "{}", Formatter::from(translator, variable))
                } else {
                    write!(f, "-{}", Formatter::from(translator, variable))
                },
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
// first vec is Or, second in And
pub struct DNFBooleanFunction {
    pub formula: Vec<Vec<DNFLiteral>>,
}

impl PrintableWithTranslator for DNFBooleanFunction {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        let mut it = self.formula.iter().peekable();
        while let Some(or_formula) = it.next() {
            let mut or_it = or_formula.iter().peekable();
            write!(f, "(")?;
            while let Some(or) = or_it.next() {
                if or_it.peek().is_none() {
                    write!(f, "{}", Formatter::from(translator, or))?;
                } else {
                    write!(f, "{} ^ ", Formatter::from(translator, or))?;
                }
            }
            if it.peek().is_none() {
                write!(f, ")")?;
            } else {
                writeln!(f, ") v")?;
            }
        }
        Ok(())
    }
}

impl From<BooleanFunction> for DNFBooleanFunction {
    fn from(source: BooleanFunction) -> Self {
        fn morgan(source: DNFBooleanFunction) -> DNFBooleanFunction {
            let temp: Vec<Vec<_>> = source
                .formula
                .into_iter()
                .map(|f| f.into_iter().map(|l| !l).collect())
                .collect();

            let lenghts: Vec<usize> = temp.iter().map(|f| f.len()).collect();
            let mut position = vec![0; temp.len()];

            let add_one = |position: &mut Vec<usize>| -> bool {
                let mut location: usize = 0;
                loop {
                    if location >= position.len() {
                        return true;
                    }
                    position[location] += 1;
                    if position[location] >= lenghts[location] {
                        position[location] = 0;
                    } else {
                        return false;
                    }
                    location += 1;
                }
            };

            let mut ret_val = vec![];
            loop {
                ret_val.push(
                    position
                        .iter()
                        .enumerate()
                        .map(|(pos, p)| temp[pos][*p])
                        .collect(),
                );

                if add_one(&mut position) {
                    break;
                }
            }
            DNFBooleanFunction { formula: ret_val }
        }

        fn helper_normalize(source: BooleanFunction) -> DNFBooleanFunction {
            match &source {
                | BooleanFunction::False => DNFBooleanFunction {
                    formula: vec![vec![DNFLiteral::False]],
                },
                | BooleanFunction::True => DNFBooleanFunction {
                    formula: vec![vec![DNFLiteral::True]],
                },
                | BooleanFunction::Variable(v) => DNFBooleanFunction {
                    formula: vec![vec![DNFLiteral::Variable {
                        positive: true,
                        variable: *v,
                    }]],
                },
                | BooleanFunction::And(n1, n2) => {
                    let n1 = helper_normalize(*n1.clone());
                    let n2 = helper_normalize(*n2.clone());

                    let mut formulas = vec![];
                    for formula1 in n1.formula {
                        for formula2 in &n2.formula {
                            formulas.push(
                                formula1
                                    .iter()
                                    .chain(formula2.iter())
                                    .cloned()
                                    .collect(),
                            );
                        }
                    }
                    DNFBooleanFunction { formula: formulas }
                },
                | BooleanFunction::Or(n1, n2) => {
                    let n1 = helper_normalize(*n1.clone());
                    let n2 = helper_normalize(*n2.clone());

                    DNFBooleanFunction {
                        formula: n1
                            .formula
                            .into_iter()
                            .chain(n2.formula)
                            .collect(),
                    }
                },
                | BooleanFunction::Not(n) => {
                    if let BooleanFunction::Not(n) = &**n {
                        helper_normalize(*n.clone())
                    } else {
                        morgan(helper_normalize(*n.clone()))
                    }
                },
            }
        }

        helper_normalize(source)
    }
}

impl DNFBooleanFunction {
    pub fn evaluate(&self, assignments: &BTreeMap<IdType, bool>) -> bool {
        self.formula
            .iter()
            .any(|or_f| or_f.iter().all(|l| l.evaluate(assignments)))
    }
}

// -----------------------------------------------------------------------------
// Boolean Networks
// -----------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BooleanNetwork {
    initial_state: BTreeMap<IdType, bool>,
    update_rules:  BTreeMap<IdType, BooleanFunction>,
}

impl BooleanNetwork {
    pub fn step(&self) -> BooleanNetwork {
        BooleanNetwork {
            initial_state: {
                BTreeMap::from_iter(
                    self.update_rules
                        .iter()
                        .map(|(el, bf)| (*el, bf.evaluate(&self.initial_state)))
                        .collect::<Vec<_>>(),
                )
            },
            update_rules:  self.update_rules.clone(),
        }
    }
}

impl PrintableWithTranslator for BooleanNetwork {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &crate::translator::Translator,
    ) -> std::fmt::Result {
        write!(f, "Initial State: ")?;
        let mut it = self.initial_state.iter().peekable();
        while let Some((x, b)) = it.next() {
            write!(f, "({} -> {})", Formatter::from(translator, x), b)?;
            if it.peek().is_some() {
                write!(f, ",")?;
            }
            write!(f, " ")?;
        }

        writeln!(f)?;
        writeln!(f, "Update Rules:")?;

        for (x, bf) in self.update_rules.iter() {
            write!(
                f,
                "\t{} -> {}",
                Formatter::from(translator, x),
                Formatter::from(translator, bf)
            )?;
        }
        Ok(())
    }
}

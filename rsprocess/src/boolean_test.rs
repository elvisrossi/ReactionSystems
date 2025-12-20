use std::collections::BTreeMap;

use crate::boolean::{BooleanFunction, CNFBooleanFunction, CNFLiteral};
use crate::element::IdType;

macro_rules! cnfl {
    ($i:ident) => {
        CNFLiteral::$i
    };
    ($p:literal, $i:literal) => {
        CNFLiteral::Variable {
            positive: $p,
            variable: $i,
        }
    };
}

macro_rules! boolean {
    (False) => (BooleanFunction::False);
    (True) => (BooleanFunction::True);
    (Variable($i:literal)) => (BooleanFunction::Variable($i));
    (Not($($tail:tt)+)) => (BooleanFunction::Not(Box::new(boolean!($($tail)*))));
    (And(($($tail1:tt)+ ), ($($tail2:tt)+ ))) => (
        BooleanFunction::And(
            Box::new(boolean!($($tail1)+)),
            Box::new(boolean!($($tail2)+))
        )
    );
    (Or(($($tail1:tt)+ ), ($($tail2:tt)+ ))) => (
        BooleanFunction::Or(
            Box::new(boolean!($($tail1)+)),
            Box::new(boolean!($($tail2)+))
        )
    );
}

#[test]
fn boolean_1() {
    let bf = boolean!(False);
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(False)]]);
}

#[test]
fn boolean_2() {
    let bf = boolean!(True);
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)]]);
}

#[test]
fn boolean_3() {
    let bf = boolean!(Variable(1));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(true, 1)]]);
}

#[test]
fn boolean_4() {
    let bf = boolean!(Not(True));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(False)]]);
}

#[test]
fn boolean_5() {
    let bf = boolean!(Not(False));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)]]);
}

#[test]
fn boolean_6() {
    let bf = boolean!(Not(False));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)]]);
}

#[test]
fn boolean_7() {
    let bf = boolean!(Not(Not(False)));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(False)]]);
}

#[test]
fn boolean_8() {
    let bf = boolean!(Not(Not(True)));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)]]);
}

#[test]
fn boolean_9() {
    let bf = boolean!(Not(Variable(0)));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(false, 0)]]);
}

#[test]
fn boolean_10() {
    let bf = boolean!(Not(Not(Variable(0))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(true, 0)]]);
}

#[test]
fn boolean_11() {
    let bf = boolean!(And((True), (True)));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)], [cnfl!(True)]]);
}

#[test]
fn boolean_12() {
    let bf = boolean!(And((True), (False)));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)], [cnfl!(False)]]);
}

#[test]
fn boolean_13() {
    let bf = boolean!(And((Variable(0)), (False)));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(true, 0)], [cnfl!(False)]]);
}

#[test]
fn boolean_14() {
    let bf = boolean!(And((Not(Variable(0))), (False)));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(false, 0)], [cnfl!(False)]]);
}

#[test]
fn boolean_15() {
    let bf = boolean!(And((Not(Variable(0))), (Not(Not(Variable(1))))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(false, 0)], [cnfl!(true, 1)]]);
}

#[test]
fn boolean_16() {
    let bf = boolean!(And((And((True), (True))), (Not(Not(Variable(1))))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)], [cnfl!(True)], [cnfl!(
        true, 1
    )]]);
}

#[test]
fn boolean_17() {
    let bf = boolean!(Not(And((False), (False))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True), cnfl!(True)]]);
}

#[test]
fn boolean_18() {
    let bf = boolean!(Not(And((False), (True))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True), cnfl!(False)]]);
}

#[test]
fn boolean_19() {
    let bf = boolean!(Not(And((False), (Not(True)))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True), cnfl!(True)]]);
}

#[test]
fn boolean_20() {
    let bf = boolean!(Not(And((Variable(0)), (Not(True)))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(false, 0), cnfl!(True)]]);
}

#[test]
fn boolean_21() {
    let bf = boolean!(Not(And((Variable(0)), (Not(And((True), (False)))))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(false, 0), cnfl!(True)], [
        cnfl!(false, 0),
        cnfl!(False)
    ]]);
}

#[test]
fn boolean_22() {
    let bf = boolean!(And(
        (Or((Variable(0)), (Or((Not(Variable(1))), (Variable(2)))))),
        (Or((Not(Variable(3))), (Or((Variable(4)), (Variable(5))))))
    ));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [
        [cnfl!(true, 0), cnfl!(false, 1), cnfl!(true, 2)],
        [cnfl!(false, 3), cnfl!(true, 4), cnfl!(true, 5)]
    ]);
}

#[test]
fn boolean_23() {
    let bf = boolean!(And((Or((Variable(0)), (Variable(1)))), (Variable(2))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [vec![cnfl!(true, 0), cnfl!(true, 1)], vec![
        cnfl!(true, 2)
    ]]);
}

#[test]
fn boolean_24() {
    let bf = boolean!(Or((Variable(0)), (Variable(1))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(true, 0), cnfl!(true, 1)]]);
}

#[test]
fn boolean_25() {
    let bf = boolean!(Or((Variable(0)), (Or((Variable(1)), (Variable(2))))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[
        cnfl!(true, 0),
        cnfl!(true, 1),
        cnfl!(true, 2)
    ]]);
}

#[test]
fn boolean_26() {
    let bf = boolean!(Or(
        (Variable(0)),
        (Or((Variable(1)), (Or((Variable(2)), (Variable(3))))))
    ));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[
        cnfl!(true, 0),
        cnfl!(true, 1),
        cnfl!(true, 2),
        cnfl!(true, 3)
    ]]);
}

#[test]
fn boolean_27() {
    let bf = boolean!(Or(
        (Variable(0)),
        (Or(
            (Or((Variable(1)), (Variable(2)))),
            (Or((Variable(3)), (Variable(4))))
        ))
    ));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[
        cnfl!(true, 0),
        cnfl!(true, 1),
        cnfl!(true, 2),
        cnfl!(true, 3),
        cnfl!(true, 4)
    ]]);
}

#[test]
fn boolean_28() {
    let bf = boolean!(Or((Not(Variable(0))), (Variable(1))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(false, 0), cnfl!(true, 1)]]);
}

#[test]
fn boolean_29() {
    let bf = boolean!(Or((Not(Variable(0))), (Not(Not(Variable(1))))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(false, 0), cnfl!(true, 1)]]);
}

#[test]
fn boolean_30() {
    let bf = boolean!(Not(Or((False), (False))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)], [cnfl!(True)]]);
}

#[test]
fn boolean_31() {
    let bf = boolean!(Not(Or((False), (True))));
    let cnf: CNFBooleanFunction = bf.into();

    assert_eq!(cnf.formula, [[cnfl!(True)], [cnfl!(False)]]);
}

#[test]
fn boolean_32() {
    let bf = boolean!(Not(Or((Variable(0)), (Not(Or((True), (False)))))));
    let cnf: CNFBooleanFunction = bf.into();

    let assignments = BTreeMap::from([(0, false)]);

    assert!(cnf.evaluate(&assignments));

    let assignments = BTreeMap::from([(0, true)]);

    assert!(!cnf.evaluate(&assignments));
}

#[test]
fn boolean_33() {
    let bf = boolean!(Not(Or((Variable(0)), (Not(And((True), (False)))))));
    let cnf: CNFBooleanFunction = bf.into();

    let assignments = BTreeMap::from([(0, false)]);

    assert!(!cnf.evaluate(&assignments));

    let assignments = BTreeMap::from([(0, true)]);

    assert!(!cnf.evaluate(&assignments));
}

#[test]
fn boolean_34() {
    let bf = boolean!(Or(
        (And((Variable(0)), (Variable(1)))),
        (Or(
            (Or((Variable(2)), (Not(Variable(3))))),
            (Not(And((And((Variable(4)), (Not(Variable(5))))), (Variable(6)))))
        ))
    ));
    let cnf: CNFBooleanFunction = bf.into();

    let assignments: Vec<BTreeMap<IdType, bool>> = (0_u32..128)
        .map(|p| {
            BTreeMap::from_iter(
                (0..8)
                    .map(|pos| (pos, p >> pos & 1 == 1))
                    .collect::<Vec<_>>(),
            )
        })
        .collect();

    let correct_results = [
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, false, false, false, true, true, true, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true, true,
    ];

    for (assignment, res) in assignments.iter().zip(correct_results) {
        assert_eq!(cnf.evaluate(assignment), res);
    }
}

#[test]
fn boolean_35() {
    let bf = boolean!(Or(
        (And(
            (And(
                (Or((Variable(0)), (Or((Variable(1)), (Variable(2)))))),
                (Or((And((Variable(0)), (Not(Variable(1))))), (Variable(2))))
            )),
            (Not(Variable(2)))
        )),
        (And((Variable(2)), (Not(Or((Variable(0)), (Variable(1)))))))
    ));
    let cnf: CNFBooleanFunction = bf.into();

    let assignments: Vec<BTreeMap<IdType, bool>> = (0_u32..8)
        .map(|p| {
            BTreeMap::from_iter(
                (0..3)
                    .map(|pos| (pos, p >> pos & 1 == 1))
                    .collect::<Vec<_>>(),
            )
        })
        .collect();

    let correct_results =
        [false, true, false, false, true, false, false, false];

    for (assignment, res) in assignments.iter().zip(correct_results) {
        assert_eq!(cnf.evaluate(assignment), res);
    }
}

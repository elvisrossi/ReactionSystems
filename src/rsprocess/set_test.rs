#[test]
fn prohibiting_set_1() {
    use super::element::IdState::*;
    use super::set::{PositiveSet, Set};

    let r1r = Set::from(vec![2, 3, 4]);
    let r1i = Set::from(vec![5, 6, 7]);

    let r2r = Set::from(vec![2, 3, 11]);
    let r2i = Set::from(vec![5, 6, 7]);

    let mut prohibiting_set =
        Set::prohibiting_set(&[r1r, r2r], &[r1i, r2i]).unwrap();
    prohibiting_set.sort();

    assert_eq!(
        prohibiting_set,
        vec![
            PositiveSet::from([(2, Negative)]),
            PositiveSet::from([(3, Negative)]),
            PositiveSet::from([(4, Negative), (11, Negative)]),
            PositiveSet::from([(5, Positive)]),
            PositiveSet::from([(6, Positive)]),
            PositiveSet::from([(7, Positive)]),
        ]
    )
}

#[test]
fn prohibiting_set_2() {
    use super::element::IdState::*;
    use super::set::{PositiveSet, Set};

    let r1r = Set::from(vec![1]);
    let r1i = Set::from(vec![2]);

    let r2r = Set::from(vec![1]);
    let r2i = Set::from(vec![3]);

    let mut prohibiting_set =
        Set::prohibiting_set(&[r1r, r2r], &[r1i, r2i]).unwrap();
    prohibiting_set.sort();

    assert_eq!(
        prohibiting_set,
        vec![
            PositiveSet::from([(1, Negative)]),
            PositiveSet::from([(2, Positive), (3, Positive)]),
        ]
    )
}

#[test]
fn prohibiting_set_3() {
    use super::element::IdState::*;
    use super::set::{PositiveSet, Set};

    let r1r = Set::from(vec![1]);
    let r1i = Set::from(vec![2]);

    let r2r = Set::from(vec![3]);
    let r2i = Set::from(vec![1]);

    let mut prohibiting_set =
        Set::prohibiting_set(&[r1r, r2r], &[r1i, r2i]).unwrap();
    prohibiting_set.sort();

    assert_eq!(
        prohibiting_set,
        vec![
            PositiveSet::from([(1, Positive), (2, Positive)]),
            PositiveSet::from([(1, Negative), (3, Negative)]),
            PositiveSet::from([(2, Positive), (3, Negative)]),
        ]
    )
}

#[test]
fn prohibiting_set_4() {
    use super::element::IdState::*;
    use super::set::{PositiveSet, Set};

    let r1r = Set::from(vec![1]);
    let r1i = Set::from(vec![2]);

    let r2r = Set::from(vec![3]);
    let r2i = Set::from(vec![4]);

    let r3r = Set::from(vec![5]);
    let r3i = Set::from(vec![6]);

    let mut prohibiting_set =
        Set::prohibiting_set(&[r1r, r2r, r3r], &[r1i, r2i, r3i]).unwrap();
    prohibiting_set.sort();

    assert_eq!(
        prohibiting_set,
        vec![
            PositiveSet::from([(1, Negative), (3, Negative), (5, Negative)]),
            PositiveSet::from([(1, Negative), (3, Negative), (6, Positive)]),
            PositiveSet::from([(1, Negative), (4, Positive), (5, Negative)]),
            PositiveSet::from([(1, Negative), (4, Positive), (6, Positive)]),
            PositiveSet::from([(2, Positive), (3, Negative), (5, Negative)]),
            PositiveSet::from([(2, Positive), (3, Negative), (6, Positive)]),
            PositiveSet::from([(2, Positive), (4, Positive), (5, Negative)]),
            PositiveSet::from([(2, Positive), (4, Positive), (6, Positive)]),
        ]
    )
}

#[test]
fn prohibiting_set_5() {
    use super::element::IdState::*;
    use super::set::{PositiveSet, Set};

    let r1r = Set::from(vec![1]);
    let r1i = Set::from(vec![2]);

    let r2r = Set::from(vec![1, 2]);
    let r2i = Set::from(vec![]);

    let mut prohibiting_set =
        Set::prohibiting_set(&[r1r, r2r], &[r1i, r2i]).unwrap();
    prohibiting_set.sort();

    assert_eq!(prohibiting_set, vec![PositiveSet::from([(1, Negative)]),])
}

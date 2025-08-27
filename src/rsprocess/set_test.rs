#[test]
fn prohibiting_set_1() {
    use std::collections::BTreeMap;

    use super::set::{Set, PositiveSet};
    use super::element::IdState;

    let r1r = Set::from(vec![2, 3, 4]);
    let r1i = Set::from(vec![5, 6, 7]);

    let r2r = Set::from(vec![2, 3, 11]);
    let r2i = Set::from(vec![5, 6, 7]);

    let prohibiting_set = Set::prohibiting_set(&[r1r, r2r], &[r1i, r2i]).unwrap();

    assert_eq!(prohibiting_set,
	       vec![PositiveSet { identifiers:
				  BTreeMap::from([(2, IdState::Negative)]) },
		    PositiveSet { identifiers:
				  BTreeMap::from([(3, IdState::Negative)]) },
		    PositiveSet { identifiers:
				  BTreeMap::from([(5, IdState::Positive)]) },
		    PositiveSet { identifiers:
				  BTreeMap::from([(6, IdState::Positive)]) },
		    PositiveSet { identifiers:
				  BTreeMap::from([(7, IdState::Positive)]) },
		    PositiveSet { identifiers:
				  BTreeMap::from([(4, IdState::Negative),
						  (11, IdState::Negative)]) }])
}

#[test]
fn prohibiting_set_2() {
    use std::collections::BTreeMap;

    use super::set::{Set, PositiveSet};
    use super::element::IdState;

    let r1r = Set::from(vec![1]);
    let r1i = Set::from(vec![2]);

    let r2r = Set::from(vec![1]);
    let r2i = Set::from(vec![3]);

    let prohibiting_set = Set::prohibiting_set(&[r1r, r2r], &[r1i, r2i]).unwrap();

    assert_eq!(prohibiting_set,
	       vec![PositiveSet { identifiers:
				  BTreeMap::from([(1, IdState::Negative)]) },
		    PositiveSet { identifiers:
				  BTreeMap::from([(2, IdState::Positive),
						  (3, IdState::Positive)]) }])
}


#[test]
fn prohibiting_set_3() {
    use std::collections::BTreeMap;

    use super::set::{Set, PositiveSet};
    use super::element::IdState;

    let r1r = Set::from(vec![1]);
    let r1i = Set::from(vec![2]);

    let r2r = Set::from(vec![3]);
    let r2i = Set::from(vec![1]);

    let prohibiting_set = Set::prohibiting_set(&[r1r, r2r], &[r1i, r2i]).unwrap();

    assert_eq!(prohibiting_set,
	       vec![PositiveSet { identifiers:
				  BTreeMap::from([(1, IdState::Negative),
						  (3, IdState::Negative)]) },
		    PositiveSet { identifiers:
				  BTreeMap::from([(2, IdState::Positive),
						  (3, IdState::Negative)]) }])
}

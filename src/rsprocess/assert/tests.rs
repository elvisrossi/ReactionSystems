use super::super::{environment, label, process, set, system, translator};
use super::dsl::*;
use super::rsassert::*;

type LocalAssert = Assert<EdgeRelablerInput>;

#[test]
fn return_true() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Return(Box::new(Expression::True)),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn concat_1() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Assignment(
                Variable::Id("a".into()),
                None,
                Box::new(Expression::True),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn concat_2() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Return(Box::new(Expression::True))),
            Box::new(Tree::Assignment(
                Variable::Id("a".into()),
                None,
                Box::new(Expression::Integer(10)),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn return_1() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Return(Box::new(Expression::True))),
            Box::new(Tree::Return(Box::new(Expression::False))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn return_incompatible_1() {
    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Return(Box::new(Expression::True))),
            Box::new(Tree::Return(Box::new(Expression::Integer(10)))),
        ),
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn return_incompatible_2() {
    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Return(Box::new(Expression::True))),
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Integer(10)),
                )),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("a".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn return_2() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Return(Box::new(Expression::True))),
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::False),
                )),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("a".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn return_3() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::False),
                )),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("a".into()),
                )))),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(false))
    ));
}

#[test]
fn if_1() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Return(Box::new(Expression::True))),
            Box::new(Tree::If(
                Box::new(Expression::True),
                Box::new(Tree::Return(Box::new(Expression::True))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn if_2() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::If(
                Box::new(Expression::True),
                Box::new(Tree::Return(Box::new(Expression::False))),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(false))
    ));
}

#[test]
fn if_3() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::If(
                Box::new(Expression::False),
                Box::new(Tree::Return(Box::new(Expression::False))),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn if_4() {
    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::If(
                Box::new(Expression::Integer(10)),
                Box::new(Tree::Return(Box::new(Expression::True))),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn if_else_1() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Return(Box::new(Expression::True))),
            Box::new(Tree::IfElse(
                Box::new(Expression::True),
                Box::new(Tree::Return(Box::new(Expression::True))),
                Box::new(Tree::Return(Box::new(Expression::False))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn if_else_2() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::IfElse(
                Box::new(Expression::False),
                Box::new(Tree::Return(Box::new(Expression::True))),
                Box::new(Tree::Return(Box::new(Expression::False))),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(false))
    ));
}

#[test]
fn if_else_3() {
    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::IfElse(
                Box::new(Expression::Integer(10)),
                Box::new(Tree::Return(Box::new(Expression::True))),
                Box::new(Tree::Return(Box::new(Expression::False))),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn if_else_4() {
    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::IfElse(
                Box::new(Expression::True),
                Box::new(Tree::Return(Box::new(Expression::True))),
                Box::new(Tree::Return(Box::new(Expression::Integer(10)))),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assignment_1() {
    let tree = LocalAssert {
        tree: Tree::Assignment(
            Variable::Id("a".into()),
            None,
            Box::new(Expression::True),
        ),
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assignment_2() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Assignment(
                Variable::Id("a".into()),
                None,
                Box::new(Expression::True),
            )),
            Box::new(Tree::Return(Box::new(Expression::True))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn assignment_3() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Assignment(
                Variable::Id("a".into()),
                None,
                Box::new(Expression::False),
            )),
            Box::new(Tree::Return(Box::new(Expression::Var(Variable::Id(
                "a".into(),
            ))))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(false))
    ));
}

#[test]
fn assignment_4() {
    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Assignment(
                Variable::Id("a".into()),
                None,
                Box::new(Expression::True),
            )),
            Box::new(Tree::Return(Box::new(Expression::Var(Variable::Id(
                "b".into(),
            ))))),
        ),
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assignment_5() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Assignment(
                Variable::Id("a".into()),
                None,
                Box::new(Expression::Integer(10)),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("a".into()),
                )))),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("a".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(10))
    ));
}

#[test]
fn assignment_6() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Integer(10)),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Integer(200)),
                )),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("a".into()),
                )))),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("b".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(10))
    ));
}

#[test]
fn assignment_7() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Integer(10)),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::True),
                )),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("a".into()),
                )))),
                Box::new(Tree::Return(Box::new(Expression::False))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

#[test]
fn assignment_8() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Integer(10)),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Var(Variable::Id("a".into()))),
                )),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("a".into()),
                )))),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("b".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(10))
    ));
}

#[test]
fn assignment_9() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Integer(10)),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Var(Variable::Id("a".into()))),
                )),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("b".into()),
                )))),
                Box::new(Tree::Return(Box::new(Expression::Integer(200)))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(10))
    ));
}

#[test]
fn assignment_10() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Integer(10)),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Var(Variable::Id("a".into()))),
                )),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Integer(200)),
                )),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("b".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(10))
    ));
}

#[test]
fn for_1() {
    use label::Label;
    use set::Set;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Set(Set::default())),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Set(Set::default())),
                )),
            )),
            Box::new(Tree::For(
                Variable::Id("c".into()),
                Range::IterateOverSet(Box::new(Expression::Var(Variable::Id(
                    "a".into(),
                )))),
                Box::new(Tree::Return(Box::new(Expression::Integer(200)))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(tree.execute(&graph, &edge, &mut tr).is_err());
}

#[test]
fn for_2() {
    use label::Label;
    use set::Set;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Set(Set::from([1, 2]))),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Set(Set::default())),
                )),
            )),
            Box::new(Tree::For(
                Variable::Id("c".into()),
                Range::IterateOverSet(Box::new(Expression::Var(Variable::Id(
                    "a".into(),
                )))),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("b".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Set(_))
    ));
}

#[test]
fn for_3() {
    use label::Label;
    use set::Set;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Label(Box::new(Label::from(
                        Set::from([1, 2]),
                        Set::default(),
                        Set::from([1, 2]),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                    )))),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Set(Set::default())),
                )),
            )),
            Box::new(Tree::For(
                Variable::Id("c".into()),
                Range::IterateOverSet(Box::new(Expression::Unary(
                    Unary::Qualifier(Qualifier::Restricted(
                        QualifierRestricted::Entities,
                    )),
                    Box::new(Expression::Var(Variable::Id("a".into()))),
                ))),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("b".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Set(_))
    ));
}

#[test]
fn for_4() {
    use label::Label;
    use set::Set;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Label(Box::new(Label::from(
                        Set::from([1, 2]),
                        Set::from([3]),
                        Set::from([1, 2, 3]),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                    )))),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Set(Set::default())),
                )),
            )),
            Box::new(Tree::For(
                Variable::Id("c".into()),
                Range::IterateOverSet(Box::new(Expression::Unary(
                    Unary::Qualifier(Qualifier::Label(
                        QualifierLabel::AvailableEntities,
                    )),
                    Box::new(Expression::Var(Variable::Id("a".into()))),
                ))),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("c".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");
    tr.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Element(_))
    ));
}

#[test]
fn for_5() {
    use label::Label;
    use set::Set;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Label(Box::new(Label::from(
                        Set::from([1, 2]),
                        Set::from([3]),
                        Set::from([1, 2, 3]),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                    )))),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Integer(0)),
                )),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::For(
                    Variable::Id("c".into()),
                    Range::IterateOverSet(Box::new(Expression::Unary(
                        Unary::Qualifier(Qualifier::Label(
                            QualifierLabel::AvailableEntities,
                        )),
                        Box::new(Expression::Var(Variable::Id("a".into()))),
                    ))),
                    Box::new(Tree::Assignment(
                        Variable::Id("b".into()),
                        None,
                        Box::new(Expression::Binary(
                            Binary::Plus,
                            Box::new(Expression::Var(Variable::Id("b".into()))),
                            Box::new(Expression::Integer(1)),
                        )),
                    )),
                )),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("b".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");
    tr.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(3))
    ));
}

#[test]
fn for_6() {
    use label::Label;
    use set::Set;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Label(Box::new(Label::from(
                        Set::from([1, 2]),
                        Set::from([3]),
                        Set::from([1, 2, 3]),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                        Set::default(),
                    )))),
                )),
                Box::new(Tree::Concat(
                    Box::new(Tree::Assignment(
                        Variable::Id("b".into()),
                        None,
                        Box::new(Expression::Set(Set::from([2]))),
                    )),
                    Box::new(Tree::Assignment(
                        Variable::Id("c".into()),
                        None,
                        Box::new(Expression::Integer(0)),
                    )),
                )),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::For(
                    Variable::Id("d".into()),
                    Range::IterateOverSet(Box::new(Expression::Binary(
                        Binary::Plus,
                        Box::new(Expression::Unary(
                            Unary::Qualifier(Qualifier::Restricted(
                                QualifierRestricted::Context,
                            )),
                            Box::new(Expression::Var(Variable::Id("a".into()))),
                        )),
                        Box::new(Expression::Var(Variable::Id("b".into()))),
                    ))),
                    Box::new(Tree::Assignment(
                        Variable::Id("c".into()),
                        None,
                        Box::new(Expression::Binary(
                            Binary::Plus,
                            Box::new(Expression::Var(Variable::Id("c".into()))),
                            Box::new(Expression::Integer(1)),
                        )),
                    )),
                )),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("c".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");
    tr.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(2))
    ));
}

#[test]
fn for_7() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Integer(0)),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Integer(10)),
                )),
            )),
            Box::new(Tree::For(
                Variable::Id("c".into()),
                Range::IterateInRange(
                    Box::new(Expression::Var(Variable::Id("a".into()))),
                    Box::new(Expression::Var(Variable::Id("b".into()))),
                ),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("c".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(0))
    ));
}

#[test]
fn for_8() {
    use label::Label;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Concat(
                Box::new(Tree::Assignment(
                    Variable::Id("a".into()),
                    None,
                    Box::new(Expression::Unary(
                        Unary::Qualifier(Qualifier::Node(
                            QualifierNode::Neighbours,
                        )),
                        Box::new(Expression::Unary(
                            Unary::Qualifier(Qualifier::Edge(
                                QualifierEdge::Source,
                            )),
                            Box::new(Expression::Var(Variable::Special(
                                EdgeRelablerInput::Edge,
                            ))),
                        )),
                    )),
                )),
                Box::new(Tree::Assignment(
                    Variable::Id("b".into()),
                    None,
                    Box::new(Expression::Integer(0)),
                )),
            )),
            Box::new(Tree::Concat(
                Box::new(Tree::For(
                    Variable::Id("c".into()),
                    Range::IterateOverSet(Box::new(Expression::Var(
                        Variable::Id("a".into()),
                    ))),
                    Box::new(Tree::Assignment(
                        Variable::Id("b".into()),
                        None,
                        Box::new(Expression::Binary(
                            Binary::Plus,
                            Box::new(Expression::Var(Variable::Id("b".into()))),
                            Box::new(Expression::Integer(1)),
                        )),
                    )),
                )),
                Box::new(Tree::Return(Box::new(Expression::Var(
                    Variable::Id("b".into()),
                )))),
            )),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(1))
    ));

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::default());
    let edge = graph.add_edge(node_1, node_2, Label::default());
    let node_3 = graph.add_node(System::default());
    graph.add_edge(node_1, node_3, Label::default());

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Integer(2))
    ));
}

#[test]
fn nodes() {
    use std::rc::Rc;

    use environment::Environment;
    use label::Label;
    use process::Process;
    use set::Set;
    use system::System;
    use translator::Translator;

    let tree = LocalAssert {
        tree: Tree::Concat(
            Box::new(Tree::Assignment(
                Variable::Id("a".into()),
                None,
                Box::new(Expression::Unary(
                    Unary::Qualifier(Qualifier::System(
                        QualifierSystem::Entities,
                    )),
                    Box::new(Expression::Unary(
                        Unary::Qualifier(Qualifier::Node(
                            QualifierNode::System,
                        )),
                        Box::new(Expression::Unary(
                            Unary::Qualifier(Qualifier::Edge(
                                QualifierEdge::Target,
                            )),
                            Box::new(Expression::Var(Variable::Special(
                                EdgeRelablerInput::Edge,
                            ))),
                        )),
                    )),
                )),
            )),
            Box::new(Tree::Return(Box::new(Expression::Binary(
                Binary::Less,
                Box::new(Expression::Var(Variable::Id("a".into()))),
                Box::new(Expression::Set(Set::from([1, 2]))),
            )))),
        ),
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(System::default());
    let node_2 = graph.add_node(System::from(
        Rc::new(Environment::default()),
        Set::from([2]),
        Process::Nill,
        Rc::new(vec![]),
    ));
    let edge = graph.add_edge(node_1, node_2, Label::default());

    println!("{:?}", tree.execute(&graph, &edge, &mut tr));

    assert!(matches!(
        tree.execute(&graph, &edge, &mut tr),
        Ok(AssertReturnValue::Boolean(true))
    ));
}

// -----------------------------------------------------------------------------
//                                  Testing
// -----------------------------------------------------------------------------

use super::dsl::*;
use super::super::{translator, structure};

#[test]
fn assert_tycheck_true() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {tree: Tree::Return(Box::new(Expression::True))};
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_concat_1() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Assignment(
		Variable::Id("a".into()),
		None,
		Box::new(Expression::True))),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_concat_2() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(Tree::Assignment(
		Variable::Id("a".into()),
		None,
		Box::new(Expression::Integer(10)))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_return_1() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(Tree::Return(Box::new(Expression::False))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_return_incompatible_1() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(Tree::Return(Box::new(Expression::Integer(10)))),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_return_incompatible_2() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(
		Tree::Concat(
		    Box::new(Tree::Assignment(
			Variable::Id("a".into()),
			None,
			Box::new(Expression::Integer(10)))),
		    Box::new(Tree::Return(
			Box::new(Expression::Var(Variable::Id("a".into())))
		    ))),
	    )
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_return_2() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(
		Tree::Concat(
		    Box::new(Tree::Assignment(
			Variable::Id("a".into()),
			None,
			Box::new(Expression::False))),
		    Box::new(Tree::Return(
			Box::new(Expression::Var(Variable::Id("a".into())))
		    ))),
	    )
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}


#[test]
fn assert_tycheck_return_3() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::Concat(
		    Box::new(Tree::Assignment(
			Variable::Id("a".into()),
			None,
			Box::new(Expression::False))),
		    Box::new(Tree::Return(
			Box::new(Expression::Var(Variable::Id("a".into())))
		    ))),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(false))));
}

#[test]
fn assert_tycheck_if_1() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(
		Tree::If(
		    Box::new(
			Expression::True
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )
		    )
		),
	    )
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_if_2() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::If(
		    Box::new(
			Expression::True
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )
		    )
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());

    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(false))));
}

#[test]
fn assert_tycheck_if_3() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::If(
		    Box::new(
			Expression::False
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )
		    )
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_if_4() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::If(
		    Box::new(
			Expression::Integer(10)
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )
		    )
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_if_else_1() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Return(Box::new(Expression::True))),
	    Box::new(
		Tree::IfElse(
		    Box::new(
			Expression::True
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )),
		),
	    )
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_if_else_2() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::IfElse(
		    Box::new(
			Expression::False
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )),
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(false))));
}

#[test]
fn assert_tycheck_if_else_3() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::IfElse(
		    Box::new(
			Expression::Integer(10)
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::False)
		    )),
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_if_else_4() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::IfElse(
		    Box::new(
			Expression::True
		    ),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::True)
		    )),
		    Box::new(Tree::Return(
			Box::new(
			    Expression::Integer(10))
		    )),
		),
	    ),
	    Box::new(Tree::Return(Box::new(Expression::True))),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_assignment_1() {
    let tree = RSassert {
	tree: Tree::Assignment(
	    Variable::Id("a".into()),
	    None,
	    Box::new(Expression::True)
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_assignment_2() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::True)
		)
	    ),
	    Box::new(
		Tree::Return(
		    Box::new(
			Expression::True
		    )
		)
	    ),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_assignment_3() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::False)
		)
	    ),
	    Box::new(
		Tree::Return(
		    Box::new(
			Expression::Var(
			    Variable::Id("a".into())
			)
		    )
		)
	    ),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(false))));
}

#[test]
fn assert_tycheck_assignment_4() {
    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(
		Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::True)
		)
	    ),
	    Box::new(
		Tree::Return(
		    Box::new(
			Expression::Var(
			    Variable::Id("b".into())
			)
		    )
		)
	    ),
	)
    };
    assert!(tree.typecheck().is_err());
}

#[test]
fn assert_tycheck_assignment_5() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Assignment(
		Variable::Id("a".into()),
		None,
		Box::new(Expression::Integer(10))
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(10))));
}

#[test]
fn assert_tycheck_assignment_6() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Integer(200))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("b".into())
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(10))));
}

#[test]
fn assert_tycheck_assignment_7() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::True)
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::False)
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

#[test]
fn assert_tycheck_assignment_8() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    ))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("b".into())
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(10))));
}

#[test]
fn assert_tycheck_assignment_9() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    ))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("b".into())
		    ))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Integer(200))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(10))));
}

#[test]
fn assert_tycheck_assignment_10() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Integer(10))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    ))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Integer(200))
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("b".into())
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(10))));
}


#[test]
fn assert_tycheck_for_1() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Set(RSset::new()))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Set(RSset::new()))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateOverSet(
		    Box::new(
			Expression::Var(
			    Variable::Id("a".into()),
			)
		    )
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Integer(200))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(tree.execute(&graph, &edge, &mut tr).is_err());
}


#[test]
fn assert_tycheck_for_2() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Set(RSset::from([1, 2])))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Set(RSset::new()))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateOverSet(
		    Box::new(
			Expression::Var(
			    Variable::Id("a".into())
			)
		    )
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("b".into())
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Set(_))));
}

#[test]
fn assert_tycheck_for_3() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Label(
			Box::new(RSlabel::from(RSset::from([1, 2]),
					       RSset::new(),
					       RSset::from([1, 2]),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new()))
		    ))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Set(RSset::new()))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateOverSet(
		    Box::new(
			Expression::Unary(
			    Unary::Qualifier(Qualifier::Restricted(
				QualifierRestricted::Entities
			    )),
			    Box::new(Expression::Var(
				Variable::Id("a".into())
			    ))
			)
		    )
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("b".into())
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Set(_))));
}

#[test]
fn assert_tycheck_for_4() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Label(
			Box::new(RSlabel::from(RSset::from([1, 2]),
					       RSset::from([3]),
					       RSset::from([1, 2, 3]),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new()))
		    ))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Set(RSset::new()))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateOverSet(
		    Box::new(
			Expression::Unary(
			    Unary::Qualifier(Qualifier::Label(
				QualifierLabel::AvailableEntities
			    )),
			    Box::new(Expression::Var(
				Variable::Id("a".into())
			    ))
			)
		    )
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("c".into())
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");
    tr.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Element(_))));
}

#[test]
fn assert_tycheck_for_5() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Label(
			Box::new(RSlabel::from(RSset::from([1, 2]),
					       RSset::from([3]),
					       RSset::from([1, 2, 3]),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new()))
		    ))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Integer(0))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::For(
		    Variable::Id("c".into()),
		    Range::IterateOverSet(
			Box::new(
			    Expression::Unary(
				Unary::Qualifier(Qualifier::Label(
				    QualifierLabel::AvailableEntities
				)),
				Box::new(Expression::Var(
				    Variable::Id("a".into())
				))
			    )
			)
		    ),
		    Box::new(Tree::Assignment(
			Variable::Id("b".into()),
			None,
			Box::new(Expression::Binary(
			    Binary::Plus,
			    Box::new(Expression::Var(
				Variable::Id("b".into())
			    )),
			    Box::new(Expression::Integer(1))
			))
		    )),
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("b".into())
		    ))
		))
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");
    tr.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(3))));
}

#[test]
fn assert_tycheck_for_6() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel, RSset};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Label(
			Box::new(RSlabel::from(RSset::from([1, 2]),
					       RSset::from([3]),
					       RSset::from([1, 2, 3]),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new(),
					       RSset::new()))
		    ))
		)),
		Box::new(Tree::Concat(
		    Box::new(Tree::Assignment(
			Variable::Id("b".into()),
			None,
			Box::new(Expression::Set(RSset::from([2])))
		    )),
		    Box::new(Tree::Assignment(
			Variable::Id("c".into()),
			None,
			Box::new(Expression::Integer(0))
		    )),
		))
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::For(
		    Variable::Id("d".into()),
		    Range::IterateOverSet(
			Box::new(
			    Expression::Binary(
				Binary::Plus,
				Box::new(Expression::Unary(
				    Unary::Qualifier(Qualifier::Restricted(
					QualifierRestricted::Context
				    )),
				    Box::new(Expression::Var(
					Variable::Id("a".into())
				    ))
				)
				),
				Box::new(Expression::Var(
				    Variable::Id("b".into())
				))
			    )
			)
		    ),
		    Box::new(Tree::Assignment(
			Variable::Id("c".into()),
			None,
			Box::new(Expression::Binary(
			    Binary::Plus,
			    Box::new(Expression::Var(Variable::Id("c".into()))),
			    Box::new(Expression::Integer(1))
			))
		    )),
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(Variable::Id("c".into())))
		))
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();
    tr.encode("one");
    tr.encode("two");
    tr.encode("three");

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());


    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(2))));
}

#[test]
fn assert_tycheck_for_7() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Integer(0))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Integer(10))
		)),
	    )),
	    Box::new(Tree::For(
		Variable::Id("c".into()),
		Range::IterateInRange(
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    )),
		    Box::new(Expression::Var(
			Variable::Id("b".into()),
		    )),
		),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(
			Variable::Id("c".into())
		    ))
		)),
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());


    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(0))));
}

#[test]
fn assert_tycheck_for_8() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel};

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Concat(
		Box::new(Tree::Assignment(
		    Variable::Id("a".into()),
		    None,
		    Box::new(Expression::Unary(
			Unary::Qualifier(Qualifier::Node(
			    QualifierNode::Neighbours)),
			Box::new(Expression::Unary(
			    Unary::Qualifier(Qualifier::Edge(
				QualifierEdge::Source)),
			    Box::new(Expression::Var(
				Variable::Special(EdgeRelablerInput::Edge)
			    ))
			))
		    ))
		)),
		Box::new(Tree::Assignment(
		    Variable::Id("b".into()),
		    None,
		    Box::new(Expression::Integer(0))
		)),
	    )),
	    Box::new(Tree::Concat(
		Box::new(Tree::For(
		    Variable::Id("c".into()),
		    Range::IterateOverSet(
			Box::new(Expression::Var(
			    Variable::Id("a".into())
			)),
		    ),
		    Box::new(Tree::Assignment(
			Variable::Id("b".into()),
			None,
			Box::new(Expression::Binary(
			    Binary::Plus,
			    Box::new(Expression::Var(Variable::Id("b".into()))),
			    Box::new(Expression::Integer(1))
			))
		    )),
		)),
		Box::new(Tree::Return(
		    Box::new(Expression::Var(Variable::Id("b".into())))
		))
	    ))
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(1))));


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(RSsystem::new());
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());
    let node_3 = graph.add_node(RSsystem::new());
    graph.add_edge(node_1, node_3, RSlabel::new());

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Integer(2))));
}

#[test]
fn assert_tycheck_system() {
    use translator::Translator;
    use structure::{RSsystem, RSlabel, RSset, RSenvironment, RSprocess};
    use std::rc::Rc;

    let tree = RSassert {
	tree: Tree::Concat(
	    Box::new(Tree::Assignment(
		Variable::Id("a".into()),
		None,
		Box::new(Expression::Unary(
		    Unary::Qualifier(
			Qualifier::System(QualifierSystem::Entities)
		    ),
		    Box::new(Expression::Unary(
			Unary::Qualifier(Qualifier::Node(
			    QualifierNode::System)),
			Box::new(Expression::Unary(
			    Unary::Qualifier(Qualifier::Edge(
				QualifierEdge::Target)),
			    Box::new(Expression::Var(
				Variable::Special(EdgeRelablerInput::Edge)
			    ))
			))
		    ))
		))
	    )),
	    Box::new(Tree::Return(
		Box::new(Expression::Binary(
		    Binary::Less,
		    Box::new(Expression::Var(
			Variable::Id("a".into())
		    )),
		    Box::new(Expression::Set(RSset::from([1, 2])))
		))
	    )),
	)
    };
    assert!(tree.typecheck().is_ok());


    let mut tr = Translator::new();

    let mut graph = petgraph::Graph::new();
    let node_1 = graph.add_node(RSsystem::new());
    let node_2 = graph.add_node(
	RSsystem::from(
	    Rc::new(RSenvironment::new()),
	    RSset::from([2]),
	    RSprocess::Nill,
	    Rc::new(vec![])
	)
    );
    let edge = graph.add_edge(node_1, node_2, RSlabel::new());

    println!("{:?}", tree.execute(&graph, &edge, &mut tr));

    assert!(matches!(tree.execute(&graph, &edge, &mut tr),
		     Ok(AssertReturnValue::Boolean(true))));
}

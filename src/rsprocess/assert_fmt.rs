// -----------------------------------------------------------------------------
//                    Display Implementation for all types
// -----------------------------------------------------------------------------

impl fmt::Display for RSassert {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	write!(f, "label {{\n{}\n}}", self.tree)
    }
}

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Concat(t1, t2) => {write!(f, "{t1};\n{t2}")},
	    Self::If(exp, t) => {write!(f, "if {exp} {{\n{t}\n}}")},
	    Self::IfElse(exp, t1, t2) => {
		write!(f, "if {exp} {{\n{t1}\n}} else {{\n{t2}\n}}")
	    },
	    Self::Assignment(v, q, exp) => {
		if let Some(q) = q {
		    write!(f, "{v}.{q} = {exp}")
		} else {
		    write!(f, "{v} = {exp}")
		}
	    },
	    Self::Return(exp) => {write!(f, "return {exp}")},
	    Self::For(v, r, t) => {write!(f, "for {v} in {r} {{\n{t}\n}}")},
	}
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Label => {write!(f, "label")},
	    Self::Edge => {write!(f, "edge")},
	    Self::Id(s) => {write!(f, "{s}")}
	}
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::True => {write!(f, "True")},
	    Self::False => {write!(f, "False")},
	    Self::Integer(i) => {write!(f, "{i}")},
	    Self::Label(rslabel) => {write!(f, "{{debug: {rslabel:?}}}")},
	    Self::Set(set) => {write!(f, "{{debug: {set:?}}}")},
	    Self::Element(el) => {write!(f, "'{{debug: {el:?}}}'")},
	    Self::String(s) => {write!(f, r#""{s}""#)},
	    Self::Var(v) => {write!(f, "{v}")},
	    Self::Unary(u, exp) => {
		if u.is_prefix() {
		    write!(f, "{u}({exp})")
		} else if u.is_suffix() {
		    write!(f, "{exp}{u}")
		} else {
		    unreachable!()
		}
	    },
	    Self::Binary(b, exp1, exp2) => {
		if b.is_prefix() {
		    write!(f, "{b}({exp1}, {exp2})")
		} else if b.is_suffix() {
		    write!(f, "({exp1}, {exp2}){b}")
		} else if b.is_infix() {
		    write!(f, "({exp1} {b} {exp2})")
		} else {
		    unreachable!()
		}
	    },
	}
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::IterateOverSet(exp) => {write!(f, "{{{exp}}}")},
	    Self::IterateInRange(exp1, exp2) => {write!(f, "{exp1}..{exp2}")}
	}
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Not => write!(f, "not"),
	    Self::Rand => write!(f, "rand"),
	    Self::Empty => write!(f, ".empty"),
	    Self::Length => write!(f, ".length"),
	    Self::ToStr => write!(f, ".tostr"),
	    Self::Qualifier(q) => write!(f, ".{q}"),
	    Self::ToEl => write!(f, ".toel"),
	    Self::Source => write!(f, ".source"),
	    Self::Target => write!(f, ".target"),
	    Self::Neighbours => write!(f, ".neighbours"),
	    Self::System => write!(f, ".system"),
	}
    }
}

impl fmt::Display for QualifierRestricted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Entities => write!(f, "Entities"),
	    Self::Context => write!(f, "Context"),
	    Self::Reactants => write!(f, "Reactants"),
	    Self::ReactantsAbsent => write!(f, "ReactantsAbsent"),
	    Self::Inhibitors => write!(f, "Inhibitors"),
	    Self::InhibitorsPresent => write!(f, "InhibitorsPresent"),
	    Self::Products => write!(f, "Products"),
	}
    }
}

impl fmt::Display for QualifierLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::AvailableEntities => write!(f, "AvailableEntities"),
	    Self::AllReactants => write!(f, "AllReactants"),
	    Self::AllInhibitors => write!(f, "AllInhibitors"),
	}
    }
}

impl fmt::Display for QualifierSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match self {
	    Self::Context => write!(f, "context"),
	    Self::Entities => write!(f, "entities"),
	}
    }
}

impl fmt::Display for Qualifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match self {
	    Self::Label(q) => write!(f, "{q}"),
	    Self::Restricted(q) => write!(f, "{q}"),
	    Self::System(q) => write!(f, "{q}"),
	}
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::And => write!(f, "&&"),
	    Self::Or => write!(f, "||"),
	    Self::Xor => write!(f, "^^"),
	    Self::Less => write!(f, "<"),
	    Self::LessEq => write!(f, "<="),
	    Self::More => write!(f, ">"),
	    Self::MoreEq => write!(f, ">="),
	    Self::Eq => write!(f, "=="),
	    Self::NotEq => write!(f, "!="),
	    Self::Plus => write!(f, "+"),
	    Self::Minus => write!(f, "-"),
	    Self::Times => write!(f, "*"),
	    Self::Exponential => write!(f, "^"),
	    Self::Quotient => write!(f, "/"),
	    Self::Reminder => write!(f, "%"),
	    Self::Concat => write!(f, "::"),
	    Self::SubStr => write!(f, "substr"),
	    Self::Min => write!(f, "min"),
	    Self::Max => write!(f, "max"),
	    Self::CommonSubStr => write!(f, "commonsubstr"),
	}
    }
}

impl fmt::Display for AssertReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Boolean(b) => write!(f, "{b}"),
	    Self::Integer(i) => write!(f, "{i}"),
	    Self::String(s) => write!(f, r#""{s}""#),
	    Self::Label(l) => write!(f, "{{debug: {l:?}}}"),
	    Self::Set(set) => write!(f, "{{debug: {set:?}}}"),
	    Self::Element(el) => write!(f, "{{debug: {el:?}}}"),
	    Self::Edge(edge) => write!(f, "{{debug: {edge:?}}}"),
	    Self::Node(node) => write!(f, "{{debug: {node:?}}}"),
	    Self::Neighbours(node) =>
		write!(f, "{{debug: {node:?}}}.neighbours"),
	    Self::System(sys) => write!(f, "{{debug: {sys:?}}}"),
	    Self::Context(ctx) => write!(f, "{{debug: {ctx:?}}}"),
	}
    }
}

impl fmt::Display for AssertionTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    Self::Boolean => write!(f, "boolean"),
	    Self::Integer => write!(f, "integer"),
	    Self::String => write!(f, "string"),
	    Self::Label => write!(f, "label"),
	    Self::Set => write!(f, "set"),
	    Self::Element => write!(f, "element"),
	    Self::System => write!(f, "system"),
	    Self::Context => write!(f, "context"),
	    Self::NoType => write!(f, "no type"),
	    Self::RangeInteger => write!(f, "range of integers"),
	    Self::RangeSet => write!(f, "range of set"),
	    Self::RangeNeighbours => write!(f, "range of edges"),
	    Self::Edge => write!(f, "edge"),
	    Self::Node => write!(f, "node"),
	}
    }
}

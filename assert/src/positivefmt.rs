// -----------------------------------------------------------------------------
//                    Display Implementation for all types
// -----------------------------------------------------------------------------
use std::fmt;

use rsprocess::translator::{Formatter, PrintableWithTranslator, Translator};

use super::positivedsl::*;

impl<S> fmt::Debug for PositiveAssert<S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "label {{\n{:?}\n}}", self.tree)
    }
}

impl<S> fmt::Debug for PositiveTree<S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Concat(t1, t2) => {
                write!(f, "{t1:?};\n{t2:?}")
            },
            | Self::If(exp, t) => {
                write!(f, "if {exp:?} {{\n{t:?}\n}}")
            },
            | Self::IfElse(exp, t1, t2) => {
                write!(f, "if {exp:?} {{\n{t1:?}\n}} else {{\n{t2:?}\n}}")
            },
            | Self::Assignment(v, q, exp) =>
                if let Some(q) = q {
                    write!(f, "{v:?}.{q:?} = {exp:?}")
                } else {
                    write!(f, "{v:?} = {exp:?}")
                },
            | Self::Return(exp) => {
                write!(f, "return {exp:?}")
            },
            | Self::For(v, r, t) => {
                write!(f, "for {v:?} in {r:?} {{\n{t:?}\n}}")
            },
        }
    }
}

impl<S> fmt::Debug for PositiveVariable<S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Special(s) => {
                write!(f, "{s:?}")
            },
            | Self::Id(s) => {
                write!(f, "{s:?}")
            },
        }
    }
}

impl<S> fmt::Debug for PositiveExpression<S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::True => {
                write!(f, "True")
            },
            | Self::False => {
                write!(f, "False")
            },
            | Self::Integer(i) => {
                write!(f, "{i}")
            },
            | Self::Label(rslabel) => {
                write!(f, "{{debug: {rslabel:?}}}")
            },
            | Self::Set(set) => {
                write!(f, "{{debug: {set:?}}}")
            },
            | Self::PositiveElement(el) => {
                write!(f, "'{{debug: {el:?}}}'")
            },
            | Self::String(s) => {
                write!(f, r#""{s:?}""#)
            },
            | Self::Var(v) => {
                write!(f, "{v:?}")
            },
            | Self::Unary(u, exp) =>
                if u.is_prefix() {
                    write!(f, "{u:?}({exp:?})")
                } else if u.is_suffix() {
                    write!(f, "{exp:?}{u:?}")
                } else {
                    unreachable!()
                },
            | Self::Binary(b, exp1, exp2) =>
                if b.is_prefix() {
                    write!(f, "{b:?}({exp1:?}, {exp2:?})")
                } else if b.is_suffix() {
                    write!(f, "({exp1:?}, {exp2:?}){b:?}")
                } else if b.is_infix() {
                    write!(f, "({exp1:?} {b:?} {exp2:?})")
                } else {
                    unreachable!()
                },
        }
    }
}

impl<S> fmt::Debug for PositiveRange<S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::IterateOverSet(exp) => write!(f, "{{{exp:?}}}"),
            | Self::IterateInRange(exp1, exp2) => {
                write!(f, "{exp1:?}..{exp2:?}")
            },
        }
    }
}

impl fmt::Debug for PositiveUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Not => write!(f, "not"),
            | Self::Rand => write!(f, "rand"),
            | Self::Empty => write!(f, ".empty"),
            | Self::Length => write!(f, ".length"),
            | Self::ToStr => write!(f, ".tostr"),
            | Self::ToEl => write!(f, ".toel"),
            | Self::Qualifier(q) => write!(f, ".{q:?}"),
        }
    }
}

impl fmt::Debug for QualifierRestricted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Entities => write!(f, "Entities"),
            | Self::Context => write!(f, "Context"),
            | Self::Reactants => write!(f, "Reactants"),
            | Self::ReactantsAbsent => write!(f, "ReactantsAbsent"),
            | Self::Inhibitors => write!(f, "Inhibitors"),
            | Self::InhibitorsPresent => write!(f, "InhibitorsPresent"),
            | Self::Products => write!(f, "Products"),
        }
    }
}

impl fmt::Debug for QualifierLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::AvailableEntities => write!(f, "AvailableEntities"),
            | Self::AllReactants => write!(f, "AllReactants"),
            | Self::AllInhibitors => write!(f, "AllInhibitors"),
        }
    }
}

impl fmt::Debug for QualifierSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Context => write!(f, "context"),
            | Self::Entities => write!(f, "entities"),
        }
    }
}

impl fmt::Debug for QualifierEdge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Source => write!(f, "source"),
            | Self::Target => write!(f, "target"),
            | Self::Label => write!(f, "label"),
        }
    }
}

impl fmt::Debug for QualifierNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Neighbours => write!(f, "neighbours"),
            | Self::System => write!(f, "system"),
        }
    }
}

impl fmt::Debug for PositiveQualifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Label(q) => write!(f, "{q:?}"),
            | Self::Restricted(q) => write!(f, "{q:?}"),
            | Self::System(q) => write!(f, "{q:?}"),
            | Self::Edge(q) => write!(f, "{q:?}"),
            | Self::Node(q) => write!(f, "{q:?}"),
        }
    }
}

impl fmt::Debug for PositiveBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::And => write!(f, "&&"),
            | Self::Or => write!(f, "||"),
            | Self::Xor => write!(f, "^^"),
            | Self::Less => write!(f, "<"),
            | Self::LessEq => write!(f, "<="),
            | Self::More => write!(f, ">"),
            | Self::MoreEq => write!(f, ">="),
            | Self::Eq => write!(f, "=="),
            | Self::NotEq => write!(f, "!="),
            | Self::Plus => write!(f, "+"),
            | Self::Minus => write!(f, "-"),
            | Self::Times => write!(f, "*"),
            | Self::Exponential => write!(f, "^"),
            | Self::Quotient => write!(f, "/"),
            | Self::Reminder => write!(f, "%"),
            | Self::Concat => write!(f, "::"),
            | Self::SubStr => write!(f, "substr"),
            | Self::Min => write!(f, "min"),
            | Self::Max => write!(f, "max"),
            | Self::CommonSubStr => write!(f, "commonsubstr"),
        }
    }
}

impl fmt::Debug for PositiveAssertReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Boolean(b) => write!(f, "{b:?}"),
            | Self::Integer(i) => write!(f, "{i:?}"),
            | Self::String(s) => write!(f, r#""{s:?}""#),
            | Self::Label(l) => write!(f, "{{debug: {l:?}}}"),
            | Self::Set(set) => write!(f, "{{debug: {set:?}}}"),
            | Self::PositiveElement(el) => write!(f, "{{debug: {el:?}}}"),
            | Self::Edge(edge) => write!(f, "{{debug: {edge:?}}}"),
            | Self::Node(node) => write!(f, "{{debug: {node:?}}}"),
            | Self::Neighbours(node) => {
                write!(f, "{{debug: {node:?}}}.neighbours")
            },
            | Self::System(sys) => write!(f, "{{debug: {sys:?}}}"),
            | Self::Context(ctx) => write!(f, "{{debug: {ctx:?}}}"),
        }
    }
}

impl fmt::Debug for PositiveAssertionTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Boolean => write!(f, "boolean"),
            | Self::Integer => write!(f, "integer"),
            | Self::String => write!(f, "string"),
            | Self::Label => write!(f, "label"),
            | Self::Set => write!(f, "set"),
            | Self::PositiveElement => write!(f, "element"),
            | Self::System => write!(f, "system"),
            | Self::Context => write!(f, "context"),
            | Self::NoType => write!(f, "no type"),
            | Self::RangeInteger => write!(f, "range of integers"),
            | Self::RangeSet => write!(f, "range of set"),
            | Self::RangeNeighbours => write!(f, "range of edges"),
            | Self::Edge => write!(f, "edge"),
            | Self::Node => write!(f, "node"),
        }
    }
}

// -----------------------------------------------------------------------------

impl<S> PrintableWithTranslator for PositiveAssert<S>
where
    S: PrintableWithTranslator,
{
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &Translator,
    ) -> fmt::Result {
        write!(
            f,
            "label {{\n{}\n}}",
            Formatter::from(translator, &self.tree)
        )
    }
}

impl<S> PrintableWithTranslator for PositiveTree<S>
where
    S: PrintableWithTranslator,
{
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &Translator,
    ) -> fmt::Result {
        match self {
            | Self::Concat(t1, t2) => write!(
                f,
                "{};\n{}",
                Formatter::from(translator, &**t1),
                Formatter::from(translator, &**t2)
            ),
            | Self::If(exp, t) => write!(
                f,
                "if {} {{\n{}\n}}",
                Formatter::from(translator, &**exp),
                Formatter::from(translator, &**t)
            ),
            | Self::IfElse(exp, t1, t2) => {
                write!(
                    f,
                    "if {} {{\n{}\n}} else {{\n{}\n}}",
                    Formatter::from(translator, &**exp),
                    Formatter::from(translator, &**t1),
                    Formatter::from(translator, &**t2)
                )
            },
            | Self::Assignment(v, q, exp) =>
                if let Some(q) = q {
                    write!(
                        f,
                        "{}.{} = {}",
                        Formatter::from(translator, v),
                        Formatter::from(translator, q),
                        Formatter::from(translator, &**exp)
                    )
                } else {
                    write!(
                        f,
                        "{} = {}",
                        Formatter::from(translator, v),
                        Formatter::from(translator, &**exp)
                    )
                },
            | Self::Return(exp) => {
                write!(f, "return {}", Formatter::from(translator, &**exp))
            },
            | Self::For(v, r, t) => {
                write!(
                    f,
                    "for {} in {} {{\n{}\n}}",
                    Formatter::from(translator, v),
                    Formatter::from(translator, r),
                    Formatter::from(translator, &**t)
                )
            },
        }
    }
}

impl<S> PrintableWithTranslator for PositiveVariable<S>
where
    S: PrintableWithTranslator,
{
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &Translator,
    ) -> fmt::Result {
        match self {
            | Self::Special(s) => {
                write!(f, "{}", Formatter::from(translator, s))
            },
            | Self::Id(s) => write!(f, "{s}"),
        }
    }
}

impl<S> PrintableWithTranslator for PositiveExpression<S>
where
    S: PrintableWithTranslator,
{
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &Translator,
    ) -> fmt::Result {
        match self {
            | Self::True => write!(f, "True"),
            | Self::False => write!(f, "False"),
            | Self::Integer(i) => write!(f, "{i}"),
            | Self::Label(l) => {
                write!(f, "{}", Formatter::from(translator, &**l))
            },
            | Self::Set(set) => {
                write!(f, "{}", Formatter::from(translator, set))
            },
            | Self::PositiveElement(el) => {
                write!(f, "'{}'", Formatter::from(translator, el))
            },
            | Self::String(s) => write!(f, r#""{s}""#),
            | Self::Var(v) => write!(f, "{}", Formatter::from(translator, v)),
            | Self::Unary(u, exp) =>
                if u.is_prefix() {
                    write!(
                        f,
                        "{}({})",
                        Formatter::from(translator, u),
                        Formatter::from(translator, &**exp)
                    )
                } else if u.is_suffix() {
                    write!(
                        f,
                        "{}{}",
                        Formatter::from(translator, &**exp),
                        Formatter::from(translator, u)
                    )
                } else {
                    unreachable!()
                },
            | Self::Binary(b, exp1, exp2) =>
                if b.is_prefix() {
                    write!(
                        f,
                        "{}({}, {})",
                        Formatter::from(translator, b),
                        Formatter::from(translator, &**exp1),
                        Formatter::from(translator, &**exp2)
                    )
                } else if b.is_suffix() {
                    write!(
                        f,
                        "({}, {}){}",
                        Formatter::from(translator, &**exp1),
                        Formatter::from(translator, &**exp2),
                        Formatter::from(translator, b)
                    )
                } else if b.is_infix() {
                    write!(
                        f,
                        "({} {} {})",
                        Formatter::from(translator, &**exp1),
                        Formatter::from(translator, b),
                        Formatter::from(translator, &**exp2)
                    )
                } else {
                    unreachable!()
                },
        }
    }
}

impl<S> PrintableWithTranslator for PositiveRange<S>
where
    S: PrintableWithTranslator,
{
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &Translator,
    ) -> fmt::Result {
        match self {
            | Self::IterateOverSet(exp) => {
                write!(f, "{}", Formatter::from(translator, &**exp))
            },
            | Self::IterateInRange(exp1, exp2) => write!(
                f,
                "{}..{}",
                Formatter::from(translator, &**exp1),
                Formatter::from(translator, &**exp2)
            ),
        }
    }
}

impl PrintableWithTranslator for PositiveUnary {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &Translator,
    ) -> fmt::Result {
        match self {
            | Self::Not => write!(f, "not"),
            | Self::Rand => write!(f, "rand"),
            | Self::Empty => write!(f, ".empty"),
            | Self::Length => write!(f, ".length"),
            | Self::ToStr => write!(f, ".tostr"),
            | Self::ToEl => write!(f, ".toel"),
            | Self::Qualifier(q) => {
                write!(f, ".{}", Formatter::from(translator, q))
            },
        }
    }
}

impl PrintableWithTranslator for QualifierRestricted {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PrintableWithTranslator for QualifierLabel {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PrintableWithTranslator for QualifierSystem {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PrintableWithTranslator for QualifierEdge {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PrintableWithTranslator for QualifierNode {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PrintableWithTranslator for PositiveQualifier {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &Translator,
    ) -> fmt::Result {
        match self {
            | Self::Label(q) => write!(f, "{}", Formatter::from(translator, q)),
            | Self::Restricted(q) => {
                write!(f, "{}", Formatter::from(translator, q))
            },
            | Self::System(q) => {
                write!(f, "{}", Formatter::from(translator, q))
            },
            | Self::Edge(q) => write!(f, "{}", Formatter::from(translator, q)),
            | Self::Node(q) => write!(f, "{}", Formatter::from(translator, q)),
        }
    }
}

impl PrintableWithTranslator for PositiveBinary {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PrintableWithTranslator for PositiveAssertReturnValue {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &Translator,
    ) -> fmt::Result {
        match self {
            | Self::Boolean(b) => write!(f, "{b}"),
            | Self::Integer(i) => write!(f, "{i}"),
            | Self::String(s) => write!(f, r#""{s}""#),
            | Self::Label(l) => write!(f, "{}", Formatter::from(translator, l)),
            | Self::Set(set) => {
                write!(f, "{}", Formatter::from(translator, set))
            },
            | Self::PositiveElement(el) => {
                write!(f, "{}", Formatter::from(translator, el))
            },
            | Self::Edge(edge) => write!(f, "{{edge: {edge:?}}}"),
            | Self::Node(node) => write!(f, "{{node: {node:?}}}"),
            | Self::Neighbours(node) => {
                write!(f, "{{node: {node:?}}}.neighbours")
            },
            | Self::System(sys) => {
                write!(f, "{}", Formatter::from(translator, sys))
            },
            | Self::Context(ctx) => {
                write!(f, "{}", Formatter::from(translator, ctx))
            },
        }
    }
}

impl PrintableWithTranslator for PositiveAssertionTypes {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

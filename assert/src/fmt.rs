// -----------------------------------------------------------------------------
//                    Display Implementation for all types
// -----------------------------------------------------------------------------
use std::fmt;

use rsprocess::translator::{Formatter, PrintableWithTranslator, Translator};

use super::dsl::*;

impl<S> fmt::Debug for Assert<S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "label {{\n{:?}\n}}", self.tree)
    }
}

impl<S> fmt::Debug for Tree<S>
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

impl<S> fmt::Debug for Variable<S>
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

impl<S> fmt::Debug for Expression<S>
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
            | Self::Element(el) => {
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

impl<S> fmt::Debug for Range<S>
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

impl fmt::Debug for Unary {
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

impl fmt::Debug for QualifierContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::IsNill => write!(f, "isNill"),
            | Self::IsIdentifier => write!(f, "isIdentifier"),
            | Self::IsSet => write!(f, "isSet"),
            | Self::IsGuarded => write!(f, "isGuarded"),
            | Self::IsRepeated => write!(f, "isRepeated"),
            | Self::IsSummation => write!(f, "isSummation"),
            | Self::IsNondeterministicChoice =>
                write!(f, "isNondeterministicChoice"),

            | Self::GetIdentifier => write!(f, "getIdentifier"),
            | Self::GetSet => write!(f, "getSet"),
            | Self::GetGuardReactants => write!(f, "getGuardReactants"),
            | Self::GetGuardInhibitors => write!(f, "getInhibitors"),
            | Self::GetGuardProducts => write!(f, "getGuardProducts"),
            | Self::GetRepeatedCounter => write!(f, "getRepeatedCounter"),
            | Self::GetRepeatedProcess => write!(f, "getRepeatedProcess"),

            | Self::GetNextProcesses => write!(f, "getNextProcesses"),
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

impl fmt::Debug for Qualifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Label(q) => write!(f, "{q:?}"),
            | Self::Restricted(q) => write!(f, "{q:?}"),
            | Self::System(q) => write!(f, "{q:?}"),
            | Self::Context(q) => write!(f, "{q:?}"),
            | Self::Edge(q) => write!(f, "{q:?}"),
            | Self::Node(q) => write!(f, "{q:?}"),
        }
    }
}

impl fmt::Debug for Binary {
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

impl fmt::Debug for AssertReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Boolean(b) => write!(f, "{b:?}"),
            | Self::Integer(i) => write!(f, "{i:?}"),
            | Self::String(s) => write!(f, r#""{s:?}""#),
            | Self::Label(l) => write!(f, "{{debug: {l:?}}}"),
            | Self::Set(set) => write!(f, "{{debug: {set:?}}}"),
            | Self::Element(el) => write!(f, "{{debug: {el:?}}}"),
            | Self::Edge(edge) => write!(f, "{{debug: {edge:?}}}"),
            | Self::Node(node) => write!(f, "{{debug: {node:?}}}"),
            | Self::Neighbours(node) => {
                write!(f, "{{debug: {node:?}}}.neighbours")
            },
            | Self::System(sys) => write!(f, "{{debug: {sys:?}}}"),
            | Self::Context(ctx) => write!(f, "{{debug: {ctx:?}}}"),
            | Self::RangeContext(ctx) => write!(f, "{{debug: {ctx:?}}}"),
        }
    }
}

impl fmt::Debug for AssertionTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Boolean => write!(f, "boolean"),
            | Self::Integer => write!(f, "integer"),
            | Self::String => write!(f, "string"),
            | Self::Label => write!(f, "label"),
            | Self::Set => write!(f, "set"),
            | Self::Element => write!(f, "element"),
            | Self::System => write!(f, "system"),
            | Self::Context => write!(f, "context"),
            | Self::NoType => write!(f, "no type"),
            | Self::RangeInteger => write!(f, "range of integers"),
            | Self::RangeSet => write!(f, "range of set"),
            | Self::RangeNeighbours => write!(f, "range of edges"),
            | Self::RangeContexts => write!(f, "range of contexts"),
            | Self::Edge => write!(f, "edge"),
            | Self::Node => write!(f, "node"),
        }
    }
}

// -----------------------------------------------------------------------------

impl<S> PrintableWithTranslator for Assert<S>
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

impl<S> PrintableWithTranslator for Tree<S>
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

impl<S> PrintableWithTranslator for Variable<S>
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

impl<S> PrintableWithTranslator for Expression<S>
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
            | Self::Element(el) => {
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

impl<S> PrintableWithTranslator for Range<S>
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

impl PrintableWithTranslator for Unary {
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

impl PrintableWithTranslator for QualifierContext {
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

impl PrintableWithTranslator for Qualifier {
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
            | Self::Context(q) => {
                write!(f, "{}", Formatter::from(translator, q))
            },
            | Self::Edge(q) => write!(f, "{}", Formatter::from(translator, q)),
            | Self::Node(q) => write!(f, "{}", Formatter::from(translator, q)),
        }
    }
}

impl PrintableWithTranslator for Binary {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PrintableWithTranslator for AssertReturnValue {
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
            | Self::Element(el) => {
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
            | Self::RangeContext(ctx) => {
                write!(
                    f,
                    "{{next processes of: {}}}",
                    Formatter::from(translator, ctx)
                )
            },
        }
    }
}

impl PrintableWithTranslator for AssertionTypes {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        _translator: &Translator,
    ) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

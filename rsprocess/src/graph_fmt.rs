use crate::{graph::{ContextColorConditional, EdgeColor, EdgeColorConditional, EdgeDisplay, EdgeDisplayBase, NodeColor, NodeColorConditional, NodeDisplay, NodeDisplayBase, OperationType}, translator::{self, Formatter, PrintableWithTranslator}};

impl PrintableWithTranslator for NodeDisplayBase {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        match self {
            Self::String { string } =>
                write!(f, "\"{string}\""),
            Self::Hide =>
                write!(f, "Hide"),
            Self::Entities =>
                write!(f, "Entities"),
            Self::MaskEntities { mask } =>
                write!(f, "{}", Formatter::from(translator, mask)),
            Self::ExcludeEntities { mask } =>
                write!(f, "{}", Formatter::from(translator, mask)),
            Self::Context =>
                write!(f, "Context"),
            Self::UncommonEntities =>
                write!(f, "UncommonEntities"),
            Self::MaskUncommonEntities { mask } =>
                write!(f, "{}", Formatter::from(translator, mask)),
        }
    }
}

impl PrintableWithTranslator for NodeDisplay {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        let mut it = self.base.iter().peekable();

        write!(f, "[")?;
        while let Some(el) = it.next() {
            if it.peek().is_none() {
                write!(f, "{}", Formatter::from(translator, el))?;
            } else {
                write!(f, "{}, ", Formatter::from(translator, el))?;
            }
        }
        write!(f, "]")
    }
}


impl PrintableWithTranslator for EdgeDisplayBase {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        match self {
            Self::String { string } =>
                write!(f, "\"{string}\""),
            Self::Hide =>
                write!(f, "Hide"),
            Self::Products { mask, filter_common } =>
                match (mask, filter_common) {
                    (Some(mask), true) =>
                        write!(f, "UncommonMaskProducts {}", Formatter::from(translator, mask)),
                    (Some(mask), false) =>
                        write!(f, "MaskProducts {}", Formatter::from(translator, mask)),
                    (None, true) =>
                        write!(f, "UncommonProducts"),
                    (None, false) =>
                        write!(f, "Products"),
                },
            Self::Entities { mask, filter_common } =>
                match (mask, filter_common) {
                    (Some(mask), true) =>
                        write!(f, "UncommonMaskEntities {}", Formatter::from(translator, mask)),
                    (Some(mask), false) =>
                        write!(f, "MaskEntities {}", Formatter::from(translator, mask)),
                    (None, true) =>
                        write!(f, "UncommonEntities"),
                    (None, false) =>
                        write!(f, "Entities"),
                },
            Self::Context { mask, filter_common } =>
                match (mask, filter_common) {
                    (Some(mask), true) =>
                        write!(f, "UncommonMaskContext {}", Formatter::from(translator, mask)),
                    (Some(mask), false) =>
                        write!(f, "MaskContext {}", Formatter::from(translator, mask)),
                    (None, true) =>
                        write!(f, "UncommonContext"),
                    (None, false) =>
                        write!(f, "Context"),
                },
            Self::Union { mask, filter_common } =>
                match (mask, filter_common) {
                    (Some(mask), true) =>
                        write!(f, "UncommonMaskUnion {}", Formatter::from(translator, mask)),
                    (Some(mask), false) =>
                        write!(f, "MaskUnion {}", Formatter::from(translator, mask)),
                    (None, true) =>
                        write!(f, "UncommonUnion"),
                    (None, false) =>
                        write!(f, "Union"),
                },
            Self::Difference { mask, filter_common } =>
                match (mask, filter_common) {
                    (Some(mask), true) =>
                        write!(f, "UncommonMaskDifference {}", Formatter::from(translator, mask)),
                    (Some(mask), false) =>
                        write!(f, "MaskDifference {}", Formatter::from(translator, mask)),
                    (None, true) =>
                        write!(f, "UncommonDifference"),
                    (None, false) =>
                        write!(f, "Difference"),
                },
            Self::EntitiesDeleted { mask, filter_common } =>
                match (mask, filter_common) {
                    (Some(mask), true) =>
                        write!(f, "UncommonMaskEntitiesDeleted {}", Formatter::from(translator, mask)),
                    (Some(mask), false) =>
                        write!(f, "MaskEntitiesDeleted {}", Formatter::from(translator, mask)),
                    (None, true) =>
                        write!(f, "UncommonEntitiesDeleted"),
                    (None, false) =>
                        write!(f, "EntitiesDeleted"),
                },
            Self::EntitiesAdded { mask, filter_common } =>
                match (mask, filter_common) {
                    (Some(mask), true) =>
                        write!(f, "UncommonMaskEntitiesAdded {}", Formatter::from(translator, mask)),
                    (Some(mask), false) =>
                        write!(f, "MaskEntitiesAdded {}", Formatter::from(translator, mask)),
                    (None, true) =>
                        write!(f, "UncommonEntitiesAdded"),
                    (None, false) =>
                        write!(f, "EntitiesAdded"),
                },
        }
    }
}

impl PrintableWithTranslator for EdgeDisplay {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        let mut it = self.base.iter().peekable();

        write!(f, "[")?;
        while let Some(el) = it.next() {
            if it.peek().is_none() {
                write!(f, "{}", Formatter::from(translator, el))?;
            } else {
                write!(f, "{}, ", Formatter::from(translator, el))?;
            }
        }
        write!(f, "]")
    }
}


impl PrintableWithTranslator for OperationType {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        _translator: &translator::Translator,
    ) -> std::fmt::Result {
        match self {
            Self::Equals => write!(f, "=="),
            Self::Subset => write!(f, "⊂"),
            Self::SubsetEqual => write!(f, "⊆"),
            Self::Superset => write!(f, "⊃"),
            Self::SupersetEqual => write!(f, "⊇"),
        }
    }
}

impl PrintableWithTranslator for ContextColorConditional {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        match self {
            Self::Nill => write!(f, "Context.Nill"),
            Self::RecursiveIdentifier(id) => write!(f, "Context.RecursiveIdentifier ({})", Formatter::from(translator, id)),
            Self::EntitySet(ot, s) => write!(f, "Context.EntitySet {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
            Self::NonDeterministicChoice => write!(f, "Context.NonDeterministicChoice"),
            Self::Summation => write!(f, "Context.Summation"),
            Self::WaitEntity => write!(f, "Context.WaitEntity"),
        }
    }
}

impl PrintableWithTranslator for NodeColorConditional {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        match self {
            Self::ContextConditional(ccc) => {
                write!(f, "{}", Formatter::from(translator, ccc))
            },
            Self::EntitiesConditional(ot, s) => {
                write!(f, "Entities {} {}",
                       Formatter::from(translator, ot),
                       Formatter::from(translator, s))
            },
        }
    }
}

impl PrintableWithTranslator for NodeColor {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        let mut it = self.conditionals.iter().peekable();

        write!(f, "[")?;
        while let Some(el) = it.next() {
            write!(f, "{} ? {}", Formatter::from(translator, &el.0), el.1)?;
            if it.peek().is_some() {
                write!(f, "||")?;
            }
        }
        write!(f, "! \"{}\"", self.base_color)?;
        write!(f, "]")
    }
}


impl PrintableWithTranslator for EdgeColorConditional {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        match self {
            Self::Entities(ot, s) => write!(f, "Entities {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
            Self::Context(ot, s) => write!(f, "Entities {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
            Self::T(ot, s) => write!(f, "Entities {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
            Self::Reactants(ot, s) => write!(f, "Entities {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
            Self::ReactantsAbsent(ot, s) => write!(f, "Entities {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
            Self::Inhibitors(ot, s) => write!(f, "Entities {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
            Self::InhibitorsPresent(ot, s) => write!(f, "Entities {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
            Self::Products(ot, s) => write!(f, "Entities {} {}", Formatter::from(translator, ot), Formatter::from(translator, s)),
        }
    }
}

impl PrintableWithTranslator for EdgeColor {
    fn print(
        &self,
        f: &mut std::fmt::Formatter,
        translator: &translator::Translator,
    ) -> std::fmt::Result {
        let mut it = self.conditionals.iter().peekable();

        write!(f, "[")?;
        while let Some(el) = it.next() {
            write!(f, "{} ? {}", Formatter::from(translator, &el.0), el.1)?;
            if it.peek().is_some() {
                write!(f, "||")?;
            }
        }
        write!(f, "! \"{}\"", self.base_color)?;
        write!(f, "]")
    }
}

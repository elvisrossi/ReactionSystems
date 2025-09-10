use serde::{Deserialize, Serialize};
use std::fmt;

use super::translator::PrintableWithTranslator;

pub type IdType = u32;

impl PrintableWithTranslator for IdType {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &super::translator::Translator,
    ) -> fmt::Result {
        write!(
            f,
            "{}",
            translator.decode(*self).unwrap_or("Missing".into())
        )
    }
}

// -----------------------------------------------------------------------------

#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum IdState {
    Positive,
    Negative,
}

impl fmt::Display for IdState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Positive => write!(f, "+"),
            Self::Negative => write!(f, "-"),
        }
    }
}

impl std::ops::Not for IdState {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Positive => Self::Negative,
            Self::Negative => Self::Positive,
        }
    }
}

#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct PositiveType {
    pub id: IdType,
    pub state: IdState,
}

impl PrintableWithTranslator for PositiveType {
    fn print(
        &self,
        f: &mut fmt::Formatter,
        translator: &super::translator::Translator,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.state,
            translator.decode(self.id).unwrap_or("Missing".into())
        )
    }
}

impl From<(IdType, IdState)> for PositiveType {
    fn from(value: (IdType, IdState)) -> Self {
        Self {
            id: value.0,
            state: value.1,
        }
    }
}

impl From<(&IdType, &IdState)> for PositiveType {
    fn from(value: (&IdType, &IdState)) -> Self {
        Self {
            id: *value.0,
            state: *value.1,
        }
    }
}

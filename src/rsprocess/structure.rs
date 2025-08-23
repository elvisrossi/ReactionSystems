//! Module for all basic structures.

// -----------------------------------------------------------------------------
// RSset
// -----------------------------------------------------------------------------

pub type RSset = super::set::Set;

// -----------------------------------------------------------------------------
// RSreaction
// -----------------------------------------------------------------------------

pub type RSreaction = super::reaction::Reaction;

// -----------------------------------------------------------------------------
// RSprocess
// -----------------------------------------------------------------------------

pub type RSprocess = super::process::Process;

// -----------------------------------------------------------------------------
// RSchoices
// -----------------------------------------------------------------------------

pub type RSchoices = super::choices::Choices;

// -----------------------------------------------------------------------------
// RSenvironment
// -----------------------------------------------------------------------------

pub type RSenvironment = super::environment::Environment;

// -----------------------------------------------------------------------------
// RSsystem
// -----------------------------------------------------------------------------

pub type RSsystem = super::system::System;

// -----------------------------------------------------------------------------
// RSlabel
// -----------------------------------------------------------------------------

pub type RSlabel = super::label::Label;

// -----------------------------------------------------------------------------
// RSassert
// -----------------------------------------------------------------------------

pub type RSassert =
    crate::rsprocess::assert::rsassert::useful_types_edge_relabeler::RSassert;

pub mod assert {
    pub use crate::rsprocess::assert::dsl::*;
}

/// Export of useful values from submodule of submodule
pub mod rsassert {
    pub use crate::rsprocess::assert::rsassert::useful_types_edge_relabeler::*;
}

// -----------------------------------------------------------------------------
// RSBHML
// -----------------------------------------------------------------------------
#[derive(Clone, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum RSBHML {
    True,
    False,
    Or(Vec<RSBHML>),
    And(Vec<RSBHML>),
    // Diamond(Box<RSassert>, Box<RSBHML>),
    // Box(Box<RSassert>, Box<RSBHML>),
}

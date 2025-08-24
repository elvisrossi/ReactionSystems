//! Module for all basic structures.

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

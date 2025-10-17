mod custom_error;

pub mod user_error {
    pub use crate::custom_error::*;
}

#[rustfmt::skip]
#[allow(clippy::extra_unused_lifetimes)]
#[allow(clippy::needless_lifetimes)]
#[allow(clippy::let_unit_value)]
#[allow(clippy::just_underscores_and_digits)]
#[allow(clippy::uninlined_format_args)]
#[allow(clippy::type_complexity)]
pub mod grammar {
    include!(concat!(env!("OUT_DIR"), "/src/grammar.rs"));
}

#[rustfmt::skip]
#[allow(clippy::extra_unused_lifetimes)]
#[allow(clippy::needless_lifetimes)]
#[allow(clippy::let_unit_value)]
#[allow(clippy::just_underscores_and_digits)]
#[allow(clippy::uninlined_format_args)]
#[allow(clippy::type_complexity)]
pub mod assert {
    include!(concat!(env!("OUT_DIR"), "/src/assert.rs"));
}

#[rustfmt::skip]
#[allow(clippy::extra_unused_lifetimes)]
#[allow(clippy::needless_lifetimes)]
#[allow(clippy::let_unit_value)]
#[allow(clippy::just_underscores_and_digits)]
#[allow(clippy::uninlined_format_args)]
#[allow(clippy::type_complexity)]
pub mod instructions {
    include!(concat!(env!("OUT_DIR"), "/src/instructions.rs"));
}

mod custom_error;

pub mod user_error {
    pub use crate::custom_error::*;
}

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::uninlined_format_args)] pub grammar, // name of module
    "/grammar.rs" // location of parser
);

pub mod rsprocess;
pub mod examples;

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::uninlined_format_args)] pub grammar, // name of module
    "/rsprocess/grammar.rs" // location of parser
);

use execution::presets;
use lalrpop_util::ParseError;
use std::fmt::Display;
use grammar::grammar;
use ::grammar::user_error::{UserError, UserErrorTypes};

pub struct Parsers {}

impl presets::FileParsers for Parsers {
    fn parse_experiment(
        translator: &mut rsprocess::translator::Translator,
        contents: String,
    ) -> Result<(Vec<u32>, Vec<rsprocess::set::Set>), String> {
        match grammar::ExperimentParser::new().parse(translator, &contents) {
            | Ok(sys) => Ok(sys),
            | Err(e) => reformat_error(e, &contents),
        }
    }

    fn parse_instructions(
        translator: &mut rsprocess::translator::Translator,
        contents: String,
    ) -> Result<presets::Instructions, String> {
        match grammar::RunParser::new().parse(translator, &contents) {
            | Ok(sys) => Ok(sys),
            | Err(e) => reformat_error(e, &contents),
        }
    }
}

fn create_error<S, T>(
    input_str: &str,
    l: usize,
    t: T,
    r: usize,
    expected: Option<Vec<String>>,
    error: Option<UserErrorTypes>,
) -> Result<S, String>
where
    T: Display,
{
    use colored::Colorize;

    let mut err = {
        if let Some(error) = error {
            format!(
                "{error} {}{}{} \
                 between positions {l} and {r}.",
                "\"".red(),
                t.to_string().red(),
                "\"".red(),
            )
        } else {
            format!(
                "Unrecognized token {}{}{} \
                 between positions {l} and {r}.",
                "\"".red(),
                t.to_string().red(),
                "\"".red(),
            )
        }
    };

    {
        if let Some(expected) = expected {
            // Temporary debug.
            err.push_str("\nExpected: ");
            let mut it = expected.iter().peekable();
            while let Some(s) = it.next() {
                err.push('(');
                err.push_str(&format!("{}", s.green()));
                err.push(')');
                if it.peek().is_some() {
                    err.push(',');
                    err.push(' ');
                }
            }
        }
    }
    let right_new_line = input_str[l..]
        .find("\n")
        .map(|pos| pos + l)
        .unwrap_or(input_str.len());
    let left_new_line = input_str[..r]
        .rfind("\n")
        .map(|pos| pos + 1)
        .unwrap_or_default();

    let line_number = input_str[..l].match_indices('\n').count() + 1;
    let pre_no_color = format!("{line_number} |");
    let pre = format!("{}", pre_no_color.blue());

    let line_pos_l = l - left_new_line;
    let line_pos_r = r - left_new_line;

    err.push_str(&format!(
        "\nLine {} position {} to {}:\n{}{}{}{}",
        line_number,
        line_pos_l,
        line_pos_r,
        &pre,
        &input_str[left_new_line..l].green(),
        &input_str[l..r].red(),
        &input_str[r..right_new_line],
    ));
    err.push('\n');
    err.push_str(&" ".repeat(pre_no_color.len() - 1));
    err.push_str(&format!("{}", "|".blue()));
    err.push_str(&" ".repeat(l - left_new_line));
    err.push_str(&format!("{}", &"↑".red()));
    if r - l > 2 {
        err.push_str(&" ".repeat(r - l - 2));
        err.push_str(&format!("{}", &"↑".red()));
    }

    Err(err)
}

fn reformat_error<T, S>(
    e: ParseError<usize, T, UserError>,
    input_str: &str,
) -> Result<S, String>
where
    T: Display,
{
    match e {
        | ParseError::ExtraToken { token: (l, t, r) } =>
            Err(format!("Unexpected extra token \"{t}\" between positions {l} \
                         and {r}.")),
        | ParseError::UnrecognizedEof {
            location: _,
            expected: _,
        } => Err("End of file encountered while parsing.".into()),
        | ParseError::InvalidToken { location } =>
            Err(format!("Invalid token at position {location}.")),
        | ParseError::UnrecognizedToken {
            token: (l, t, r),
            expected,
        } => {
            create_error(input_str, l, t, r, Some(expected), None)
        },
        | ParseError::User {
            error: UserError { token: (l, t, r), error }
        } => {
            create_error(input_str, l, t, r, None, Some(error))
        },
    }
}

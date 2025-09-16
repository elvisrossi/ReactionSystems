use std::fmt::Display;

pub enum UserErrorTypes {
    NumberTooBigUsize,
    NumberTooBigi64,
}

impl Display for UserErrorTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::NumberTooBigUsize => write!(
                f,
                "Specified number is too big (greater than {})",
                usize::MAX
            ),
            | Self::NumberTooBigi64 => write!(
                f,
                "Specified number is too big (lesser than {} or \
                           greater than {})",
                i64::MIN,
                i64::MAX
            ),
        }
    }
}

pub struct UserError {
    pub token: (usize, String, usize),
    pub error: UserErrorTypes,
}

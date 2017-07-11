use std::fmt;
use std::error::Error;

use std::num::ParseFloatError;

#[derive(Debug, PartialEq)]
pub enum CalcError {
    DivideByZero,
    InvalidNumber(String),
    InvalidOperator(char),
    UnrecognizedToken(String),
    UnexpectedToken(String, &'static str),
    UnknownAtom(String),
    UnexpectedEndOfInput,
    UnmatchedParenthesis,
    NoFunctionArgument,
}

use CalcError::*;

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DivideByZero => write!(f, "attempted to divide by zero"),
            InvalidNumber(ref number) => {
                write!(f, "invalid number: {}", number)
            }
            InvalidOperator(ref c) => write!(f, "invalid operator: {}", c),
            UnrecognizedToken(ref token) => {
                write!(f, "unrecognized token: {}", token)
            }
            UnexpectedToken(ref token, ref kind) => {
                write!(f, "expected {} token, got {} instead", kind, token)
            }
            UnknownAtom(ref atom) => {
                write!(f, "unknown variable or function '{}'", atom)
            }
            UnexpectedEndOfInput => write!(f, "unexpected end of input"),
            UnmatchedParenthesis => write!(f, "unmatched patenthesis"),
            NoFunctionArgument => write!(f, "no bracketed function argument"),
        }
    }
}

impl From<ParseFloatError> for CalcError {
    fn from(data: ParseFloatError) -> CalcError {
        CalcError::InvalidNumber(data.description().into())
    }
}

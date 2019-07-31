use num::bigint::ParseBigIntError;
use std::error::Error;
use std::fmt;

/// Represents a partial computation that can be captured as part of an
/// error message.
#[derive(Debug, PartialEq)]
pub enum PartialComp {
    ToFloat(String),
    Unary {
        op: String,
        arg: String,
    },
    Binary {
        op: String,
        lhs: String,
        rhs: String,
    },
}

impl PartialComp {
    pub fn unary<T, U>(op: T, arg: U) -> Self
    where
        T: ToString,
        U: ToString,
    {
        PartialComp::Unary {
            op: op.to_string(),
            arg: arg.to_string(),
        }
    }

    pub fn binary<T, U, V>(op: T, lhs: &U, rhs: &V) -> Self
    where
        T: ToString,
        U: ToString,
        V: ToString,
    {
        PartialComp::Binary {
            op: op.to_string(),
            lhs: lhs.to_string(),
            rhs: rhs.to_string(),
        }
    }
}

impl fmt::Display for PartialComp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PartialComp::ToFloat(ref arg) => write!(f, "{} as float", arg),
            PartialComp::Unary { ref op, ref arg } => {
                write!(f, "{} {}", op, arg)
            }
            PartialComp::Binary {
                ref op,
                ref lhs,
                ref rhs,
            } => write!(f, "{} {} {}", lhs, op, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CalcError {
    BadTypes(PartialComp),
    DivideByZero,
    InvalidNumber(String),
    InvalidOperator(char),
    UnrecognizedToken(String),
    UnexpectedToken(String, &'static str),
    UnknownAtom(String),
    UnexpectedEndOfInput,
    UnmatchedParenthesis,
    WouldOverflow(PartialComp),
    WouldTruncate(PartialComp),
    RecursionLimitReached,
    ImpossibleDice,
    MissingAns,
}

use CalcError::*;

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BadTypes(ref comp) => {
                write!(f, "expression '{}' is not well typed", comp)
            }
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
            WouldOverflow(ref comp) => {
                write!(f, "expression '{}' would overflow", comp)
            }
            WouldTruncate(ref comp) => {
                write!(f, "expression '{}' would be truncated", comp)
            }
            UnexpectedEndOfInput => write!(f, "unexpected end of input"),
            UnmatchedParenthesis => write!(f, "unmatched patenthesis"),
            RecursionLimitReached => write!(f, "recursion limit reached"),
            ImpossibleDice => write!(f, "impossible dice"),
            MissingAns => write!(f, "no `ans` from a previous computation"),
        }
    }
}

impl From<ParseBigIntError> for CalcError {
    fn from(data: ParseBigIntError) -> CalcError {
        CalcError::InvalidNumber(data.description().into())
    }
}

impl From<CalcError> for String {
    fn from(data: CalcError) -> String {
        format!("{}", data)
    }
}

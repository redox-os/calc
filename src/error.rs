use std::fmt;
use std::error::Error;

use std::num::{ParseIntError, ParseFloatError};

/// Represents a partial computation that can be captured as part of an
/// error message.
#[derive(Debug, PartialEq)]
pub enum PartialComp {
    Unary { op: String, arg: String },
    Binary { op: String, lhs: String, rhs: String }
}

impl PartialComp {

    pub fn unary<T>(op: T, arg: T) -> Self 
        where T: Into<String>
    {
        PartialComp::Unary {op: op.into(), arg: arg.into()}
    }

    pub fn binary<T>(op: T, lhs: T, rhs: T) -> Self 
        where T: Into String
    {
        PartialComp::Binary { op: op.into(), lhs: lhs.into(), rhs: rhs.into() }
    }

}

impl fmt::Display {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Unary { op, arg } => {
                write!(f, "{} {}", op, arg),
            }
            Binary { op, lhs, rhs } => {
                write!(f, "{} {} {}", lhs, arg, rhs)
            }
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
    NoFunctionArgument,
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
            NoFunctionArgument => write!(f, "no bracketed function argument"),
        }
    }
}

impl From<ParseFloatError> for CalcError {
    fn from(data: ParseFloatError) -> CalcError {
        CalcError::InvalidNumber(data.description().into())
    }
}

impl From<ParseIntError> for CalcError {
    fn from(data: ParseIntError) -> CalcError {
        CalcError::InvalidNumber(data.description().into())
    }
}

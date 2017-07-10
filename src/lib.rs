#![cfg_attr(test, feature(test))]

#[cfg(test)]
extern crate test;

use self::CalcError::*;

use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use std::num::ParseFloatError;

#[cfg(test)]
mod bench;

#[cfg(test)]
mod tests;

/// Tokens used for
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Plus,
    Minus,
    Divide,
    Multiply,
    Exponent,
    Square,
    Cube,
    BitWiseAnd,
    BitWiseOr,
    BitWiseXor,
    BitWiseNot,
    BitWiseRShift,
    BitWiseLShift,
    Modulo,
    OpenParen,
    CloseParen,
    Number(f64),
    Atom(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Plus => write!(f, "Plus"),
            Token::Minus => write!(f, "Minus"),
            Token::Divide => write!(f, "Divide"),
            Token::Multiply => write!(f, "Multiply"),
            Token::Exponent => write!(f, "Exponent"),
            Token::Square => write!(f, "Square"),
            Token::Cube => write!(f, "Cube"),
            Token::BitWiseAnd => write!(f, "And"),
            Token::BitWiseOr => write!(f, "Or"),
            Token::BitWiseXor => write!(f, "Xor"),
            Token::BitWiseNot => write!(f, "Not"),
            Token::BitWiseRShift => write!(f, "RShift"),
            Token::BitWiseLShift => write!(f, "LShift"),
            Token::Modulo => write!(f, "Modulo"),
            Token::OpenParen => write!(f, "OpenParen"),
            Token::CloseParen => write!(f, "CloseParen"),
            Token::Number(n) => write!(f, "'{}'", n),
            Token::Atom(ref s) => write!(f, "'{}'", s),
        }
    }
}

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

#[derive(Clone, Debug)]
pub struct IntermediateResult {
    value: f64,
    tokens_read: usize,
}

impl IntermediateResult {
    pub fn new(value: f64, tokens_read: usize) -> Self {
        IntermediateResult { value, tokens_read }
    }

    /// Determines if the underlying value can be represented as an integer.
    /// This is used for typechecking of sorts: we can only do bitwise
    /// operations on integers.
    pub fn is_whole(&self) -> bool {
        self.value == self.value.floor()
    }
}

enum OperatorState {
    PotentiallyIncomplete,
    Complete,
    NotAnOperator,
}

trait IsOperator {
    fn is_operator(self) -> bool;
}

impl IsOperator for char {
    fn is_operator(self) -> bool {
        match self {
            '+' | '-' | '/' | '^' | '²' | '³' | '&' | '|' | '~' | '>' |
            '%' | '(' | ')' | '*' | '<' => true,
            _ => false,
        }
    }
}

trait CheckOperator {
    fn check_operator(self) -> OperatorState;
}

impl CheckOperator for char {
    fn check_operator(self) -> OperatorState {
        match self {
            '+' | '-' | '/' | '^' | '²' | '³' | '&' | '|' | '~' | '%' |
            '(' | ')' => OperatorState::Complete,
            '*' | '<' | '>' => OperatorState::PotentiallyIncomplete,
            _ => OperatorState::NotAnOperator,
        }
    }
}

trait OperatorMatch {
    fn operator_type(self) -> Option<Token>;
}

impl OperatorMatch for [char; 2] {
    fn operator_type(self) -> Option<Token> {
        if self == ['*', '*'] {
            Some(Token::Exponent)
        } else if self == ['<', '<'] {
            Some(Token::BitWiseLShift)
        } else if self == ['>', '>'] {
            Some(Token::BitWiseRShift)
        } else {
            None
        }
    }
}

impl OperatorMatch for char {
    fn operator_type(self) -> Option<Token> {
        match self {
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '/' => Some(Token::Divide),
            '*' => Some(Token::Multiply),
            '^' => Some(Token::BitWiseXor),
            '²' => Some(Token::Square),
            '³' => Some(Token::Cube),
            '&' => Some(Token::BitWiseAnd),
            '|' => Some(Token::BitWiseOr),
            '~' => Some(Token::BitWiseNot),
            '%' => Some(Token::Modulo),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            _ => None,
        }
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, CalcError> {
    let mut tokens = Vec::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        if c.is_alphabetic() {
            tokens.push(Token::Atom(consume_atom(&mut chars)));
        } else if c.is_digit(10) || c == '.' {
            let token_string = consume_number(&mut chars);
            tokens.push(Token::Number(token_string.parse()?));
        } else {
            match c.check_operator() {
                OperatorState::Complete => {
                    tokens.push(
                        c.operator_type().ok_or_else(|| InvalidOperator(c))?,
                    );
                    chars.next();
                }
                OperatorState::PotentiallyIncomplete => {
                    chars.next();
                    match chars.peek() {
                        Some(&next_char) if next_char.is_operator() => {
                            tokens.push(
                                [c, next_char].operator_type().ok_or_else(
                                    || {
                                        InvalidOperator(c)
                                    },
                                )?,
                            );
                            chars.next();
                        }
                        _ => {
                            tokens.push(c.operator_type().ok_or_else(
                                || InvalidOperator(c),
                            )?);
                        }
                    }
                }
                OperatorState::NotAnOperator => {
                    if c.is_whitespace() {
                        chars.next();
                    } else {
                        let token_string = consume_until_new_token(&mut chars);
                        return Err(CalcError::UnrecognizedToken(token_string));
                    }
                }
            }
        }
    }
    Ok(tokens)
}

fn consume_number<I: Iterator<Item = char>>(input: &mut Peekable<I>) -> String {
    let mut number = String::new();
    let mut has_decimal_point = false;
    while let Some(&c) = input.peek() {
        if c == '.' {
            if has_decimal_point {
                break;
            } else {
                number.push(c);
                has_decimal_point = true;
            }
        } else if c.is_digit(10) {
            number.push(c);
        } else {
            break;
        }
        input.next();
    }
    number
}

/// Consume a valid atom. An atom is defined by:
/// - Starting with an alphabetic character
/// - Consisting of alphanumeric characters or underscores
fn consume_atom<I: Iterator<Item = char>>(input: &mut Peekable<I>) -> String {
    let mut atom = String::new();
    while let Some(&c) = input.peek() {
        if c.is_alphanumeric() || c == '_' {
            atom.push(c);
            input.next();
        } else {
            break;
        }
    }
    atom
}

fn consume_until_new_token<I: Iterator<Item = char>>(input: &mut I) -> String {
    input
        .take_while(
            |c| !(c.is_whitespace() || c.is_operator() || c.is_digit(10)),
        )
        .collect()
}

/// Represents an environment for evaluating a mathematical expression
pub trait Environment {
    /// Look up the arity of an atom:
    /// - Variables have an implicit arity of zero
    /// - This library currently does not support varadic functions
    /// - If a symbol is not defined, return None
    fn arity(&self, atom: &str) -> Option<usize>;

    /// Resolve an atom given the name of the atom and some number of
    /// arguments
    /// Precondition: `args.len() == self.arity(atom)`
    fn resolve(
        &mut self,
        atom: &str,
        args: &[IntermediateResult],
    ) -> Result<f64, CalcError>;
}


fn d_expr<E>(
    token_list: &[Token],
    env: &mut E,
) -> Result<IntermediateResult, CalcError>
    where E: Environment
{
    let mut e1 = e_expr(token_list, env)?;
    let mut index = e1.tokens_read;

    while index < token_list.len() {
        match token_list[index] {
            Token::BitWiseAnd => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                if e1.is_whole() && e2.is_whole() {
                    let mut int_f = e1.value.floor() as i64;
                    let int_s = e2.value.floor() as i64;
                    int_f &= int_s;
                    e1.value = int_f as f64;
                    e1.tokens_read += e2.tokens_read + 1;
                } else {
                    return Err(CalcError::UnexpectedToken(
                        (if e1.is_whole() { e2.value } else { e1.value })
                            .to_string(),
                        "Not an integer number!",
                    ));
                }
            }
            Token::BitWiseOr => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                if e1.is_whole() && e2.is_whole() {
                    let mut int_f = e1.value.floor() as i64;
                    let int_s = e2.value.floor() as i64;
                    int_f |= int_s;
                    e1.value = int_f as f64;
                    e1.tokens_read += e2.tokens_read + 1;
                } else {
                    return Err(CalcError::UnexpectedToken(
                        (if e1.is_whole() { e2.value } else { e1.value })
                            .to_string(),
                        "Not an integer number!",
                    ));
                }
            }
            Token::BitWiseNot => {
                if e1.is_whole() {
                    let mut int_f = e1.value.floor() as i64;
                    // magic number: bigest integer representable by f64 is 2^53, which is 0b1<<54 according to https://stackoverflow.com/questions/1848700/biggest-integer-that-can-be-stored-in-a-double
                    // make a mask by shifting 11... between the sign bit and
                    // the number to effectively get a 55 bit signed number
                    // let mask = 0b111111111 << 54;
                    int_f = !(int_f);
                    e1.value = int_f as f64;
                    e1.tokens_read += 1;
                } else {
                    return Err(CalcError::UnexpectedToken(
                        e1.value.to_string(),
                        "Not an integer number!",
                    ));
                }
            }
            Token::BitWiseXor => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                if e1.is_whole() && e2.is_whole() {
                    let mut int_f = e1.value.floor() as i64;
                    let int_s = e2.value.floor() as i64;
                    int_f ^= int_s;
                    e1.value = int_f as f64;
                    e1.tokens_read += e2.tokens_read + 1;
                } else {
                    return Err(CalcError::UnexpectedToken(
                        (if e1.is_whole() { e2.value } else { e1.value })
                            .to_string(),
                        "Not an integer number!",
                    ));
                }
            }
            Token::BitWiseLShift => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                if e1.is_whole() && e2.is_whole() {
                    let mut int_f = e1.value.floor() as i64;
                    let int_s = e2.value.floor() as i64;
                    int_f <<= int_s;
                    e1.value = int_f as f64;
                    e1.tokens_read += e2.tokens_read + 1;
                } else {
                    return Err(CalcError::UnexpectedToken(
                        (if e1.is_whole() { e2.value } else { e1.value })
                            .to_string(),
                        "Not an integer number!",
                    ));
                }
            }
            Token::BitWiseRShift => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                if e1.is_whole() && e2.is_whole() {
                    let mut int_f = e1.value.floor() as i64;
                    let int_s = e2.value.floor() as i64;
                    int_f >>= int_s;
                    e1.value = int_f as f64;
                    e1.tokens_read += e2.tokens_read + 1;
                } else {
                    return Err(CalcError::UnexpectedToken(
                        (if e1.is_whole() { e2.value } else { e1.value })
                            .to_string(),
                        "Not an integer number!",
                    ));
                }
            }
            Token::Number(ref n) => {
                return Err(
                    CalcError::UnexpectedToken(n.to_string(), "operator"),
                );
            }
            _ => break,
        };
        index = e1.tokens_read;
    }
    Ok(e1)
}
// Addition and subtraction
fn e_expr<E>(
    token_list: &[Token],
    env: &mut E,
) -> Result<IntermediateResult, CalcError>
    where E: Environment
{
    let mut t1 = t_expr(token_list, env)?;
    let mut index = t1.tokens_read;

    while index < token_list.len() {
        match token_list[index] {
            Token::Plus => {
                let t2 = t_expr(&token_list[index + 1..], env)?;
                t1.value += t2.value;
                t1.tokens_read += t2.tokens_read + 1;
            }
            Token::Minus => {
                let t2 = t_expr(&token_list[index + 1..], env)?;
                t1.value -= t2.value;
                t1.tokens_read += t2.tokens_read + 1;
            }
            Token::Number(n) => {
                return Err(
                    CalcError::UnexpectedToken(n.to_string(), "operator"),
                )
            }
            _ => break,
        };
        index = t1.tokens_read;
    }
    Ok(t1)
}

// Multiplication and division
fn t_expr<E>(
    token_list: &[Token],
    env: &mut E,
) -> Result<IntermediateResult, CalcError>
    where E: Environment
{
    let mut f1 = f_expr(token_list, env)?;
    let mut index = f1.tokens_read;

    while index < token_list.len() {
        match token_list[index] {
            Token::Multiply => {
                let f2 = f_expr(&token_list[index + 1..], env)?;
                f1.value *= f2.value;
                f1.tokens_read += f2.tokens_read + 1;
            }
            Token::Divide => {
                let f2 = f_expr(&token_list[index + 1..], env)?;
                if f2.value == 0.0 {
                    return Err(CalcError::DivideByZero);
                } else {
                    f1.value /= f2.value;
                    f1.tokens_read += f2.tokens_read + 1;
                }
            }
            Token::Modulo => {
                let f2 = f_expr(&token_list[index + 1..], env)?;
                if f2.value == 0.0 {
                    return Err(CalcError::DivideByZero);
                } else {
                    f1.value %= f2.value;
                    f1.tokens_read += f2.tokens_read + 1;
                }
            }
            Token::Number(n) => {
                return Err(
                    CalcError::UnexpectedToken(n.to_string(), "operator"),
                );
            }
            _ => break,
        }
        index = f1.tokens_read;
    }
    Ok(f1)
}

// Exponentiation
fn f_expr<E>(
    token_list: &[Token],
    env: &mut E,
) -> Result<IntermediateResult, CalcError>
    where E: Environment
{
    let mut g1 = g_expr(token_list, env)?; // was g1
    let mut index = g1.tokens_read;
    let token_len = token_list.len();
    while index < token_len {
        match token_list[index] {
            Token::Exponent => {
                let f = f_expr(&token_list[index + 1..], env)?;
                g1.value = g1.value.powf(f.value);
                g1.tokens_read += f.tokens_read + 1;
            }
            Token::Square => {
                g1.value = g1.value * g1.value;
                g1.tokens_read += 1;
            }
            Token::Cube => {
                g1.value = g1.value * g1.value * g1.value;
                g1.tokens_read += 1;
            }
            Token::Number(n) => {
                return Err(
                    CalcError::UnexpectedToken(n.to_string(), "operator"),
                );
            }
            _ => break,
        }
        index = g1.tokens_read;
    }
    Ok(g1)
}

// Numbers, parenthesized expressions, and atoms
fn g_expr<E>(
    token_list: &[Token],
    env: &mut E,
) -> Result<IntermediateResult, CalcError>
    where E: Environment
{
    if !token_list.is_empty() {
        match token_list[0] {
            Token::Number(n) => Ok(IntermediateResult::new(n, 1)),
            Token::Atom(ref s) => {
                if let Some(nargs) = env.arity(s) {
                    let mut args = Vec::new();
                    let mut start = 1;
                    for _ in 0..nargs {
                        let ir = g_expr(&token_list[start..], env)?;
                        start += ir.tokens_read;
                        args.push(ir);
                    }
                    let res = env.resolve(s, &args);
                    Ok(IntermediateResult::new(res?, start))
                } else {
                    Err(CalcError::UnknownAtom(s.clone()))
                }
            }
            Token::Minus => {
                if token_list.len() > 1 {
                    if let Token::Number(ref n) = token_list[1] {
                        Ok(IntermediateResult::new(-n, 2))
                    } else {
                        Err(CalcError::UnexpectedToken(
                            token_list[1].to_string(),
                            "number",
                        ))
                    }
                } else {
                    Err(CalcError::UnexpectedEndOfInput)
                }
            }
            Token::OpenParen => {
                let ir = d_expr(&token_list[1..], env)?;
                let close_paren = ir.tokens_read + 1;
                if close_paren < token_list.len() {
                    match token_list[close_paren] {
                        Token::CloseParen => Ok(IntermediateResult::new(
                            ir.value,
                            close_paren + 1,
                        )),
                        _ => Err(CalcError::UnexpectedToken(
                            token_list[close_paren].to_string(),
                            ")",
                        )),
                    }
                } else {
                    Err(CalcError::UnmatchedParenthesis)
                }
            }
            _ => Err(CalcError::UnexpectedToken(
                token_list[0].to_string(),
                "number",
            )),
        }
    } else {
        Err(CalcError::UnexpectedEndOfInput)
    }
}

pub struct DefaultEnvironment;

impl Environment for DefaultEnvironment {
    fn arity(&self, atom: &str) -> Option<usize> {
        match atom {
            "pi" => Some(0),
            "log" | "sin" | "cos" | "tan" => Some(1),
            _ => None,
        }
    }

    fn resolve(
        &mut self,
        atom: &str,
        args: &[IntermediateResult],
    ) -> Result<f64, CalcError> {
        match atom {
            "pi" => Ok(std::f64::consts::PI),
            "tau" => Ok(std::f64::consts::PI * 2.0),
            "log" => Ok(args[0].value.log(10.0)),
            "sin" => Ok(args[0].value.sin()),
            "cos" => Ok(args[0].value.cos()),
            "tan" => Ok(args[0].value.tan()),
            _ => Err(CalcError::UnknownAtom(atom.to_owned())),
        }
    }
}

fn parse<E>(tokens: &[Token], env: &mut E) -> Result<f64, CalcError>
    where E: Environment
{
    d_expr(tokens, env).map(|answer| answer.value)
}

pub fn eval(input: &str) -> Result<f64, CalcError> {
    let mut env = DefaultEnvironment;
    tokenize(input).and_then(|x| parse(&x, &mut env))
}

pub fn eval_with_env<E>(input: &str, env: &mut E) -> Result<f64, CalcError>
    where E: Environment
{
    tokenize(input).and_then(|x| parse(&x, env))
}

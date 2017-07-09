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
    Logarithm,
    Cosine,
    Sine,
    Tangent,
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
}


impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let tok = match *self {
            Token::Plus => "Plus",
            Token::Minus => "Minus",
            Token::Divide => "Divide",
            Token::Multiply => "Multiply",
            Token::Exponent => "Exponent",
            Token::Square => "Square",
            Token::Cube => "Cube",
            Token::BitWiseAnd => "And",
            Token::BitWiseOr => "Or",
            Token::BitWiseXor => "Xor",
            Token::BitWiseNot => "Not",
            Token::BitWiseRShift => "RShift",
            Token::BitWiseLShift => "LShift",
            Token::Logarithm => "Logarithm",
            Token::Cosine => "Cosine",
            Token::Sine => "Sine",
            Token::Tangent => "Tangent",
            Token::Modulo => "Modulo",
            Token::OpenParen => "OpenParen",
            Token::CloseParen => "CloseParen",
            Token::Number(_) => "Number",
        };
        write!(f, "{}", tok)
    }
}

#[derive(Debug, PartialEq)]
pub enum CalcError {
    DivideByZero,
    InvalidNumber(String),
    InvalidOperator(char),
    UnrecognizedToken(String),
    UnexpectedToken(String, &'static str),
    UnexpectedEndOfInput,
    UnmatchedParenthesis,
    NoFunctionArgument,
}

impl From<CalcError> for String {
    fn from(data: CalcError) -> String {
        match data {
            DivideByZero => "calc: attempted to divide by zero".into(),
            InvalidNumber(number) => {
                ["calc: invalid number: ", &number].concat()
            }
            InvalidOperator(character) => {
                format!("calc: invalid operator: {}", character)
            }
            //IO(error) => error.description().into(),
            UnrecognizedToken(token) => {
                ["calc: unrecognized token: ", &token].concat()
            }
            UnexpectedToken(token, kind) => {
                ["calc: unexpected ", kind, " token: ", &token].concat()
            }
            UnexpectedEndOfInput => "calc: unexpected end of input".into(),
            UnmatchedParenthesis => "calc: unmatched parenthesis".into(),
            NoFunctionArgument => "calc: no bracketed function argument".into(),
        }
    }
}

impl From<ParseFloatError> for CalcError {
    fn from(data: ParseFloatError) -> CalcError {
        CalcError::InvalidNumber(data.description().into())
    }
}

#[derive(Clone, Debug)]
struct IntermediateResult {
    value: f64,
    tokens_read: usize,
}

impl IntermediateResult {
    fn new(value: f64, tokens_read: usize) -> Self {
        IntermediateResult { value, tokens_read }
    }

    /// Determines if the underlying value can be represented as an integer.
    /// This is used for typechecking of sorts: we can only do bitwise operations
    /// on integers.
    pub fn is_whole(&self) -> bool {
        self.value == self.value.floor()
    }
}

enum OperatorState {
    PotentiallyIncomplete,
    Complete,
    NotAnOperator,
    Function
}

trait IsOperator {
    fn is_operator(self) -> bool;
}

impl IsOperator for char {
    fn is_operator(self) -> bool {
        match self {
            '+' | '-' | '/' | '^' | '²' | '³' | '&' | '|' | '~' | '>' |
            '%' | '(' | ')' | '*' | '<' | 'l' | 'o' | 'g'|'c' | 's' | 'i' |'n'| 'a' | 't'=> true,
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
            '*' | '<' | '>'  => OperatorState::PotentiallyIncomplete,
            'l' | 'o' | 'g'|'c' | 's' | 'i' |'n'| 'a' | 't' => OperatorState::Function,
            _ => OperatorState::NotAnOperator,
        }
    }
}

trait OperatorMatch {
    fn operator_type(self) -> Option<Token>;
}

impl OperatorMatch for [char; 3] {
    fn operator_type(self) -> Option<Token> {
        if self == ['l', 'o','g'] {
            Some(Token::Logarithm)
        } else if self == ['c', 'o', 's'] {
            Some(Token::Cosine)
        } else if self == ['s', 'i', 'n'] {
            Some(Token::Sine)
        } else if self == ['t', 'a', 'n'] {
            Some(Token::Tangent)
        } else {
            None
        }
    }
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
        if c.is_digit(10) || c == '.' {
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
                OperatorState::Function => {
                    chars.next();
                    match chars.peek() {
                        Some(&next_char) if next_char.is_operator() => {
                            chars.next();
                            match chars.peek() {
                                Some(&sec_next_char) if sec_next_char.is_operator() => {
                                    tokens.push(
                                        [c, next_char,sec_next_char].operator_type().ok_or_else(
                                        || {
                                        InvalidOperator(c)
                                        },
                                        )?,
                                    );
                                chars.next();
                                },
                                _ => {
                                    tokens.push(c.operator_type().ok_or_else(
                                    || InvalidOperator(c),
                                    )?);
                                },
                            }
                        }
                        _ => {
                                    tokens.push(c.operator_type().ok_or_else(
                                    || InvalidOperator(c),
                                    )?);
                                }
                    }
                } //TODO: add functions to calculate!!!
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

fn consume_until_new_token<I: Iterator<Item = char>>(input: &mut I) -> String {
    input
        .take_while(|c| {
            !(c.is_whitespace() || c.is_operator() || c.is_digit(10))
        })
        .collect()
}

fn d_expr(token_list: &[Token]) -> Result<IntermediateResult, CalcError> {
    let mut e1 = e_expr(token_list)?;
    let mut index = e1.tokens_read;

    while index < token_list.len() {
        match token_list[index] {
            Token::BitWiseAnd => {
                let e2 = e_expr(&token_list[index + 1..])?;
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
                let e2 = e_expr(&token_list[index + 1..])?;
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
                    //magic number: bigest integer representable by f64 is 2^53, which is 0b1<<54 according to https://stackoverflow.com/questions/1848700/biggest-integer-that-can-be-stored-in-a-double
                    // make a mask by shifting 11... between the sign bit and the number to effectively get a 55 bit signed number
                    //let mask = 0b111111111 << 54;
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
                let e2 = e_expr(&token_list[index + 1..])?;
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
                let e2 = e_expr(&token_list[index + 1..])?;
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
                let e2 = e_expr(&token_list[index + 1..])?;
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
fn e_expr(token_list: &[Token]) -> Result<IntermediateResult, CalcError> {
    let mut t1 = t_expr(token_list)?;
    let mut index = t1.tokens_read;

    while index < token_list.len() {
        match token_list[index] {
            Token::Plus => {
                let t2 = t_expr(&token_list[index + 1..])?;
                t1.value += t2.value;
                t1.tokens_read += t2.tokens_read + 1;
            }
            Token::Minus => {
                let t2 = t_expr(&token_list[index + 1..])?;
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
fn t_expr(token_list: &[Token]) -> Result<IntermediateResult, CalcError> {
    let mut f1 = f_expr(token_list)?;
    let mut index = f1.tokens_read;

    while index < token_list.len() {
        match token_list[index] {
            Token::Multiply => {
                let f2 = f_expr(&token_list[index + 1..])?;
                f1.value *= f2.value;
                f1.tokens_read += f2.tokens_read + 1;
            }
            Token::Divide => {
                let f2 = f_expr(&token_list[index + 1..])?;
                if f2.value == 0.0 {
                    return Err(CalcError::DivideByZero);
                } else {
                    f1.value /= f2.value;
                    f1.tokens_read += f2.tokens_read + 1;
                }
            }
            Token::Modulo => {
                let f2 = f_expr(&token_list[index + 1..])?;
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
fn f_expr(token_list: &[Token]) -> Result<IntermediateResult, CalcError> {
    let mut g1 = g_expr(token_list)?; //was g1
    let mut index = g1.tokens_read;
    let token_len = token_list.len();
    while index < token_len {
        match token_list[index] {
            Token::Exponent => {
                let f = f_expr(&token_list[index + 1..])?;
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

// Numbers and parenthesized expressions
fn g_expr(token_list: &[Token]) -> Result<IntermediateResult, CalcError> {
    if !token_list.is_empty() {
        match token_list[0] {
            Token::Number(n) => Ok(IntermediateResult::new(n, 1)),
            Token::Cosine | Token::Sine | Token::Tangent | Token::Logarithm => {
                if token_list[1] == Token::OpenParen {
                    let ir = d_expr(&token_list[2..])?;
                    let close_paren = ir.tokens_read + 2;
                    if close_paren < token_list.len() {
                        match token_list[close_paren] {
                            Token::CloseParen => {
                                let mut val = 0.0; //TODO: not declare this val here like the warningwants
                                match token_list[0]{
                                    Token::Cosine => val=ir.value.cos(),
                                    Token::Sine => val=ir.value.sin(),
                                    Token::Tangent => val=ir.value.tan(),
                                    Token::Logarithm => val=ir.value.log(10.0),
                                    _ => val=0.0, //TODO: maybe get this outta the way too
                                }
                                Ok(
                                    IntermediateResult::new(
                                        val,
                                        close_paren + 1,
                                    ),
                                )
                            },
                            _ => Err(CalcError::UnexpectedToken(
                             token_list[close_paren].to_string(),
                                ")",
                            )),
                        }
                    } else {
                        Err(CalcError::UnmatchedParenthesis)
                    }
                }
                else {
                    Err(CalcError::NoFunctionArgument) //actually lack of an argument
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
                let ir = d_expr(&token_list[1..])?;
                let close_paren = ir.tokens_read + 1;
                if close_paren < token_list.len() {
                    match token_list[close_paren] {
                        Token::CloseParen => Ok(
                            IntermediateResult::new(
                                ir.value,
                                close_paren + 1,
                            ),
                        ),
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


fn parse(tokens: &[Token]) -> Result<f64, CalcError> {
    d_expr(tokens).map(|answer| answer.value)
}

pub fn eval(input: &str) -> Result<f64, CalcError> {
    tokenize(input).and_then(|x| parse(&x))
}

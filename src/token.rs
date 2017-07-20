use std::i64;
use std::fmt;
use std::iter::Peekable;

use error::CalcError;
use error::CalcError::*;
use value::Value;

/// Tokens used for parsing an arithmetic expression
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
    Number(Value),
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
            Token::Number(ref n) => write!(f, "'{}'", n),
            Token::Atom(ref s) => write!(f, "'{}'", s),
        }
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
        } else if c.is_digit(16) || c == '.' {
            tokens.push(consume_number(&mut chars)?);
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

fn digits<I>(input: &mut Peekable<I>, radix: u32) -> String
    where I: Iterator<Item = char>
{
    let mut number = String::new();
    while let Some(&c) = input.peek() {
        if c.is_digit(radix) {
            number.push(c);
        } else {
            break;
        }
        input.next();
    }
    number
}

fn consume_number<I>(input: &mut Peekable<I>) -> Result<Token, CalcError>
    where I: Iterator<Item = char>
{
    match input.peek() {
        Some(&'0') => {
            input.next();
            match input.peek() {
                Some(&'x') | Some(&'X') => {
                    input.next();
                    let digits = digits(input, 16);
                    let num = i64::from_str_radix(&digits, 16)?;
                    return Ok(Token::Number(Value::Hex(num)));
                }
                Some(&_) => (),
                None => return Ok(Token::Number(Value::Dec(0))),
            }
        }
        Some(_) => (),
        None => return Err(CalcError::UnexpectedEndOfInput),
    }
    let whole = digits(input, 10);
    if let Some(&'.') = input.peek() {
        input.next();
        let frac = digits(input, 10);
        let num: f64 = [whole, ".".into(), frac].concat().parse()?;
        Ok(Token::Number(Value::Float(num)))
    } else {
        Ok(Token::Number(Value::Dec(whole.parse()?)))
    }
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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normal() {
        let line = "(3 + 7) >> 10 * (7 % 2)";
        let expected = vec![
            Token::OpenParen,
            Token::Number(Value::Dec(3)),
            Token::Plus,
            Token::Number(Value::Dec(7)),
            Token::CloseParen,
            Token::BitWiseRShift,
            Token::Number(Value::Dec(10)),
            Token::Multiply,
            Token::OpenParen,
            Token::Number(Value::Dec(7)),
            Token::Modulo,
            Token::Number(Value::Dec(2)),
            Token::CloseParen,
        ];
        assert_eq!(tokenize(line), Ok(expected));
    }

    #[test]
    fn function_chaining() {
        let line = "log 4 / log 2";
        let expected = vec![
            Token::Atom("log".into()),
            Token::Number(Value::Dec(4)),
            Token::Divide,
            Token::Atom("log".into()),
            Token::Number(Value::Dec(2)),
        ];
        assert_eq!(tokenize(line), Ok(expected));
    }

    #[test]
    fn hexadecimals() {
        let line = "0xDEADBEEF | 0xC0FFEE";
        let expected = vec![
            Token::Number(Value::Hex(0xDEADBEEF)),
            Token::BitWiseOr,
            Token::Number(Value::Hex(0xC0FFEE)),
        ];
        assert_eq!(tokenize(line), Ok(expected));
    }

}

use crate::error::CalcError;
use crate::error::CalcError::*;
use crate::value::{Integral, Value};
use decimal::d128;
use num::Num;
use std::fmt;
use std::iter::Peekable;

/// Tokens used for parsing an arithmetic expression
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ans,
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
    Dice,
    Number(Value),
    Atom(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Ans => write!(f, "Ans"),
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
            Token::Dice => write!(f, "Dice"),
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
            '+' | '-' | '/' | '^' | '²' | '³' | '&' | '|' | '~' | '>' | '%'
            | '(' | ')' | '*' | '<' | 'd' => true,
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
            '+' | '-' | '/' | '^' | '²' | '³' | '&' | '|' | '~' | '%' | '('
            | ')' | 'd' => OperatorState::Complete,
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
            'd' => Some(Token::Dice),
            _ => None,
        }
    }
}

/// Tokenizes a mathematical expression written written with the standard infix
/// notation.
///
/// Returns a vector of `Token`s in the infix format, if the supplied
/// expression is valid. This vector can then be passed into the `parse`
/// function to be evaluated.
pub fn tokenize(input: &str) -> Result<Vec<Token>, CalcError> {
    let mut tokens = Vec::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c.check_operator() {
            OperatorState::Complete => {
                tokens
                    .push(c.operator_type().ok_or_else(|| InvalidOperator(c))?);
                chars.next();
            }
            OperatorState::PotentiallyIncomplete => {
                chars.next();
                match chars.peek() {
                    Some(&next_char) if next_char.is_operator() => {
                       match [c, next_char].operator_type() {
                           Some(t) => {
                               tokens.push(t);
                               chars.next();
                           },
                           _=> tokens.push(c.operator_type().ok_or_else(|| InvalidOperator(c))?)
                       }
                    }
                    _ => {
                        tokens.push(
                            c.operator_type()
                                .ok_or_else(|| InvalidOperator(c))?,
                        );
                    }
                }
            }
            OperatorState::NotAnOperator => {
                if c.is_whitespace() {
                    chars.next();
                } else if c.is_alphabetic() {
                    tokens.push(consume_ans_or_atom(&mut chars).into());
                } else if c.is_digit(16) || c == '.' {
                    tokens.push(Token::Number(consume_number(&mut chars)?));
                } else {
                    let token_string = consume_until_new_token(&mut chars);
                    return Err(CalcError::UnrecognizedToken(token_string));
                }
            }
        }
    }
    Ok(tokens)
}

/// Tokenizes a mathematical expression written with a polish (prefix) notation.
///
/// Returns a vector of `Token`s in the infix format, if the supplied
/// expression is valid. This
/// vector can then be pased into the `parse` function to be evaluated.
pub fn tokenize_polish(input: &str) -> Result<Vec<Token>, CalcError> {
    // NOTE: This function isn't as efficient as it could be. For sake of
    // compatibility with the
    // existing infix parser, this function stores and re-arranges tokens in
    // the infix format;
    // adding extra parenthesis so that the order of operations aren't followed.

    // A temporary enum that is used for collecting polish values before they
    // are converted into
    // tokens for the infix format.
    #[derive(Debug)]
    enum PolishValue {
        Ans,
        Atom(String),
        Number(Value),
    }
    impl From<PolishValue> for Token {
        fn from(polish: PolishValue) -> Token {
            match polish {
                PolishValue::Ans => Token::Ans,
                PolishValue::Atom(atom) => Token::Atom(atom),
                PolishValue::Number(val) => Token::Number(val),
            }
        }
    }
    impl From<Token> for PolishValue {
        fn from(token: Token) -> PolishValue {
            match token {
                Token::Ans => PolishValue::Ans,
                Token::Atom(atom) => PolishValue::Atom(atom),
                Token::Number(val) => PolishValue::Number(val),
                _ => unreachable!(),
            }
        }
    }

    let mut chars = input.chars().peekable();
    let mut operators: Vec<Token> = Vec::with_capacity(input.len() / 4);
    let mut values: Vec<PolishValue> = Vec::with_capacity(input.len() / 3);
    let mut tokens = Vec::with_capacity(input.len() / 2);
    let mut parens = 0;

    'outer: loop {
        // Collect the operators until a number is found.
        while let Some(&c) = chars.peek() {
            match c.check_operator() {
                OperatorState::Complete => {
                    let token =
                        c.operator_type().ok_or_else(|| InvalidOperator(c))?;
                    if token != Token::OpenParen && token != Token::CloseParen {
                        operators.push(token);
                    }
                    chars.next();
                }
                OperatorState::PotentiallyIncomplete => {
                    chars.next();
                    match chars.peek() {
                        Some(&next_char) if next_char.is_operator() => {
                            let token = [c, next_char]
                                .operator_type()
                                .ok_or_else(|| InvalidOperator(c))?;
                            if token != Token::OpenParen
                                && token != Token::CloseParen
                            {
                                operators.push(token);
                            }
                            chars.next();
                        }
                        _ => {
                            let token = c
                                .operator_type()
                                .ok_or_else(|| InvalidOperator(c))?;
                            if token != Token::OpenParen
                                && token != Token::CloseParen
                            {
                                operators.push(token);
                            }
                        }
                    }
                }
                OperatorState::NotAnOperator => {
                    if c.is_whitespace() {
                        chars.next();
                    } else if c.is_alphabetic() {
                        values.push(consume_ans_or_atom(&mut chars).into());
                        break;
                    } else if c.is_digit(16) || c == '.' {
                        values.push(PolishValue::Number(consume_number(
                            &mut chars,
                        )?));
                        break;
                    } else {
                        let token_string = consume_until_new_token(&mut chars);
                        return Err(CalcError::UnrecognizedToken(token_string));
                    }
                }
            }
        }

        // Then collect the atoms/numbers, resetting the looping if more
        // operators are found.
        while let Some(&c) = chars.peek() {
            if c.is_alphabetic() {
                values.push(consume_ans_or_atom(&mut chars).into());
            } else if c.is_digit(16) || c == '.' {
                values.push(PolishValue::Number(consume_number(&mut chars)?));
            } else if c.is_whitespace() || c == ')' {
                let _ = chars.next();
            } else if operators.len() != values.len() {
                // Either too many or too few values were supplied
                return Err(CalcError::UnexpectedEndOfInput);
            } else {
                // This block executes when multiple sets of arithmetic
                // operations are entered.
                // Add parenthesis to work around the order of operations for
                // infix arithmetic.
                for _ in 1..operators.len() {
                    tokens.push(Token::OpenParen);
                }

                // Operators are processed in reverse, while numbers are
                // processed forwardly.
                let mut iterator = values
                    .drain(..)
                    .map(Token::from)
                    .zip(operators.drain(..).rev());

                // The first iteration will not include any closing parenthesis.
                if let Some((value, operator)) = iterator.next() {
                    tokens.push(value);
                    tokens.push(operator);
                }

                // Each following iteration will append the second number in
                // the operation,
                // followed by a closing parenthesis and the next operation in
                // the list.
                for (value, operator) in iterator {
                    tokens.push(value);
                    tokens.push(Token::CloseParen);
                    tokens.push(operator);
                }

                tokens.push(Token::OpenParen);
                parens += 1;
                continue 'outer;
            }
        }

        if values.len() == 0 || operators.len() != values.len() - 1 {
            return Err(CalcError::UnexpectedEndOfInput);
        } else {
            // Removes the last value from the values vector so that they are
            // even.
            // It will be re-added at the end once everything's been collected.
            let last_value: Token = values.pop().unwrap().into();
            // Add parenthesis to work around the order of operations for infix
            // arithmetic.
            for _ in 1..operators.len() {
                tokens.push(Token::OpenParen);
            }
            // Operators are processed in reverse, while numbers are processed
            // forwardly.
            let mut iterator = values
                .drain(..)
                .map(Token::from)
                .zip(operators.drain(..).rev());

            // The first iteration will not include any closing parenthesis.
            if let Some((value, operator)) = iterator.next() {
                tokens.push(value);
                tokens.push(operator);
            }

            // Each following iteration will append the second number in the
            // operation,
            // followed by a closing parenthesis and the next operation in the
            // list.
            for (value, operator) in iterator {
                tokens.push(value);
                tokens.push(Token::CloseParen);
                tokens.push(operator);
            }

            tokens.push(last_value);
            for _ in 0..parens {
                tokens.push(Token::CloseParen);
            }
            break 'outer;
        }
    }

    Ok(tokens)
}

fn consume_ans_or_atom<I>(input: &mut Peekable<I>) -> Token
where
    I: Iterator<Item = char>,
{
    let atom = consume_atom(input);
    if atom.eq_ignore_ascii_case("ans") {
        Token::Ans
    } else {
        Token::Atom(atom)
    }
}

fn digits<I>(input: &mut Peekable<I>, radix: u32) -> String
where
    I: Iterator<Item = char>,
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

fn consume_number<I>(input: &mut Peekable<I>) -> Result<Value, CalcError>
where
    I: Iterator<Item = char>,
{
    match input.peek() {
        Some(&'0') => {
            input.next();
            match input.peek().cloned() {
                Some('x') | Some('X') => {
                    input.next();
                    let digits = digits(input, 16);
                    let num = Integral::from_str_radix(&digits, 16)?;
                    return Ok(Value::hex(num));
                }
                Some(c) if c.is_digit(16) || c == '.' => (),
                _ => return Ok(Value::dec(0)),
            }
        }
        Some(_) => (),
        None => return Err(CalcError::UnexpectedEndOfInput),
    }
    let mut whole = digits(input, 10);
    if let Some(&'.') = input.peek() {
        input.next();
        let frac = digits(input, 10);
        if whole.is_empty() && frac.is_empty() {
            whole = "0".to_string();
        }
        let num = [whole, ".".into(), frac]
            .concat()
            .parse::<d128>()
            .map_err(|_| CalcError::InvalidNumber("invalid float".into()))?;
        Ok(Value::Float(num))
    } else {
        let res: Integral = whole.parse()?;
        Ok(Value::dec(res))
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
        .take_while(|c| {
            !(c.is_whitespace() || c.is_operator() || c.is_digit(10))
        })
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
            Token::Number(Value::dec(3)),
            Token::Plus,
            Token::Number(Value::dec(7)),
            Token::CloseParen,
            Token::BitWiseRShift,
            Token::Number(Value::dec(10)),
            Token::Multiply,
            Token::OpenParen,
            Token::Number(Value::dec(7)),
            Token::Modulo,
            Token::Number(Value::dec(2)),
            Token::CloseParen,
        ];
        assert_eq!(tokenize(line), Ok(expected));
    }

    #[test]
    fn zero() {
        let line = "0 + 0. + 0";
        let expected = vec![
            Token::Number(Value::dec(0)),
            Token::Plus,
            Token::Number(Value::Float(d128::from(0))),
            Token::Plus,
            Token::Number(Value::dec(0)),
        ];
        assert_eq!(tokenize(line), Ok(expected));
    }

    #[test]
    fn dice() {
        let line = "3d6";
        let expected = vec![
            Token::Number(Value::dec(3)),
            Token::Dice,
            Token::Number(Value::dec(6)),
        ];
        assert_eq!(tokenize(line), Ok(expected));
    }

    #[test]
    fn dice_polish() {
        let line = "d 3 6";
        let expected = vec![
            Token::Number(Value::dec(3)),
            Token::Dice,
            Token::Number(Value::dec(6)),
        ];
        assert_eq!(tokenize_polish(line), Ok(expected));
    }

    #[test]
    fn ans() {
        let line = "ans*3";
        let expected =
            vec![Token::Ans, Token::Multiply, Token::Number(Value::dec(3))];
        assert_eq!(tokenize(line), Ok(expected));
    }

    #[test]
    fn ans_polish() {
        let line = "* ans 3";
        let expected =
            vec![Token::Ans, Token::Multiply, Token::Number(Value::dec(3))];
        assert_eq!(tokenize_polish(line), Ok(expected));
    }

    #[test]
    fn ans_subtract_ans_polish() {
        let line = "- ans ans";
        let expected = vec![Token::Ans, Token::Minus, Token::Ans];
        assert_eq!(tokenize_polish(line), Ok(expected));
    }

    #[test]
    fn function_chaining() {
        let line = "log 4 / log 2";
        let expected = vec![
            Token::Atom("log".into()),
            Token::Number(Value::dec(4)),
            Token::Divide,
            Token::Atom("log".into()),
            Token::Number(Value::dec(2)),
        ];
        assert_eq!(tokenize(line), Ok(expected));
    }

    #[test]
    fn hexadecimals() {
        let line = "0xDEADBEEF | 0xC0FFEE";
        let expected = vec![
            Token::Number(Value::hex(0xDEADBEEF as i64)),
            Token::BitWiseOr,
            Token::Number(Value::hex(0xC0FFEE)),
        ];
        assert_eq!(tokenize(line), Ok(expected));
    }
}

#![cfg_attr(test, feature(test))]

#[macro_use]
extern crate decimal;
extern crate num;
#[cfg(test)]
extern crate test;

#[cfg(test)]
mod bench;

mod error;
pub mod parse;
mod token;
pub mod value;

pub use error::CalcError;
pub use value::Value;

/// Evalulates a regular mathematical expression.
pub fn eval(input: &str) -> Result<Value, CalcError> {
    let mut env = parse::DefaultEnvironment;
    token::tokenize(input).and_then(|x| parse::parse(&x, &mut env))
}

/// Evalulates a regular mathematical expression, with extra environment
/// variables.
pub fn eval_with_env<E>(input: &str, env: &mut E) -> Result<Value, CalcError>
where
    E: parse::Environment,
{
    token::tokenize(input).and_then(|x| parse::parse(&x, env))
}

/// Evalulates mathematical expressions that are written in Polish Notation.
///
/// Polish Notation defines that a string of operators are given at the
/// beginning of the
/// expression, as prefixes, and are followed by a string of numbers to apply
/// each operation
/// to. Polish Notation enables writing mathematical expressions that don't
/// require grouping
/// via parenthesis. It's also referred to as Prefix Notation, or Normal Polish
/// Notation (NPN).
///
/// # Examples
///
/// - `+ * 3 4 5` is equivalent to `3 * 4 + 5`
/// - `+ / * 5 3 2 * + 1 3 5` is equivalent to `((5 * 3) / 2) + ((1 + 3) * 5)`
pub fn eval_polish(input: &str) -> Result<Value, CalcError> {
    let mut env = parse::DefaultEnvironment;
    token::tokenize_polish(input).and_then(|x| parse::parse(&x, &mut env))
}

/// Evalulates mathematical expressions that are written in Polish Notation,
/// with extra
/// environment variables.
///
/// Polish Notation defines that a string of operators are given at the
/// beginning of the
/// expression, as prefixes, and are followed by a string of numbers to apply
/// each operation
/// to. Polish Notation enables writing mathematical expressions that don't
/// require grouping
/// via parenthesis. It's also referred to as prefix notation, or Normal Polish
/// Notation (NPN).
///
/// # Examples
///
/// - `+ * 3 4 5` is equivalent to `3 * 4 + 5`
/// - `+ / * 5 3 2 * + 1 3 5` is equivalent to `((5 * 3) / 2) + ((1 + 3) * 5)`
pub fn eval_polish_with_env<E>(
    input: &str,
    env: &mut E,
) -> Result<Value, CalcError>
where
    E: parse::Environment,
{
    token::tokenize_polish(input).and_then(|x| parse::parse(&x, env))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basics() {
        let cases = vec![
            ("  1 +   1", Value::Dec(2)),
            (" 4 * 7 - 14", Value::Dec(14)),
            (" 2 << 16 ", Value::Dec(131072)),
            (
                " ((4 * 18) % 17) / 3",
                Value::Float(d128!(4.0) / d128!(3.0)),
            ),
            ("2²³²", Value::Dec(4096)),
            ("4 ^ 3 ^ 2 ^ 3 ^ 4 ^ 2", Value::Dec(0)),
            ("3 << (4 >> 2)", Value::Dec(6)),
            ("~0", Value::Dec(-1)),
            // ("cos pi + sin (tau * (3 / 4))", Value::Float(d128!(-2.0))),
            ("~~5", Value::Dec(5)),
        ];
        for (input, expected) in cases {
            assert_eq!(eval(input), Ok(expected));
        }
    }

    #[test]
    fn polish() {
        let cases = vec![
            (" + 1 1", Value::Dec(2)),
            (" - * 4 7 14", Value::Dec(14)),
            (" << 2 16", Value::Dec(131072)),
            (
                " / % * 4 18 17 3",
                Value::Float(d128!(4.0) / d128!(3.0)),
            ),
            ("* + 1 3 5", Value::Dec(20)),
            (
                "+ / * 5 3 2 * + 1 3 5",
                Value::Float(d128!(27.5)),
            ),
            ("^ ^ ^ ^ ^ 4 3 2 3 4 2", Value::Dec(0)),
            ("<< 3 >> 4 2", Value::Dec(6)),
        ];
        for (input, expected) in cases {
            assert_eq!(eval_polish(input), Ok(expected));
        }
    }

    #[test]
    fn random() {
        let cases = vec![
            (
                "((15 * 10) - 26 * 19 - 30 / ((57 * 79 + 93 / 87 / 47))) / 8",
                Value::Float(d128!(-43.00083277394169075309321854399588)),
            ),
            (
                "(3 << 6) * 7 + (40 / 3)",
                Value::Float(d128!(1357.333333333333333333333333333333)),
            ),
            (
                "(21 & (5) ^ (20 & 81)) / (25 << 3)",
                Value::Float(d128!(0.105)),
            ),
            (
                "(79 & 14) * ((3) - 76 + 67 / (62) - (85 ^ (7 - (32) >> 52)))",
                Value::Float(d128!(197.1290322580645161290322580645161)),
            ),
        ];

        for (input, expected) in cases {
            assert_eq!(eval(input), Ok(expected));
        }
    }

}

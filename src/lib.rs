#![cfg_attr(test, feature(test))]

#[cfg(test)]
extern crate test;

#[cfg(test)]
mod bench;

mod error;
mod token;
mod parse;

pub use error::CalcError;

pub fn eval(input: &str) -> Result<f64, CalcError> {
    let mut env = parse::DefaultEnvironment;
    token::tokenize(input).and_then(|x| parse::parse(&x, &mut env))
}

pub fn eval_with_env<E>(input: &str, env: &mut E) -> Result<f64, CalcError>
    where E: parse::Environment
{
    token::tokenize(input).and_then(|x| parse::parse(&x, env))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basics() {
        let cases = vec![
            ("  1 +   1", 2.0),
            (" 4 * 7 - 14", 14.0),
            (" 2 << 16 ", 131072.0),
            (" ((4 * 18) % 17) / 3", 4.0 / 3.0),
            ("2²³²", 4096.0),
            ("4 ^ 3 ^ 2 ^ 3 ^ 4 ^ 2", 0.0),
            ("3 << (4 >> 2)", 6.0),
            ("~0", -1.0),
            ("cos pi + sin (tau * (3 / 4))", -2.0),
        ];
        for (input, expected) in cases {
            assert_eq!(eval(input), Ok(expected));
        }
    }


    #[test]
    fn random() {
        let cases = vec![
            (
                "((15 * 10) - 26 * 19 - 30 / ((57 * 79 + 93 / 87 / 47))) / 8",
                -43.00083277394169075309
            ),
            ("(3 << 6) * 7 + (40 / 3)", 1357.33333333333333333333),
            ("(21 & (5) ^ (20 & 81)) / (25 << 3)", 0.105),
            (
                "(79 & 14) * ((3) - 76 + 67 / (62) - (85 ^ (7 - (32) >> 52)))",
                197.12903225806448
            ),
        ];

        for (input, expected) in cases {
            assert_eq!(eval(input), Ok(expected));
        }
    }

}

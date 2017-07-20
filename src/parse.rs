use token::*;
use error::CalcError;
use value::{Value, IR, IntermediateResult};

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
    fn resolve(&mut self, atom: &str, args: &[IR]) -> Result<Value, CalcError>;
}


fn d_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
    where E: Environment
{

    if !token_list.is_empty() && token_list[0] == Token::BitWiseNot {
        let mut e = (!(e_expr(&token_list[1..], env)?))?;
        e.tokens += 1;
        return Ok(e);
    }

    let mut e1 = e_expr(token_list, env)?;
    let mut index = e1.tokens;

    while index < token_list.len() {
        match token_list[index] {
            Token::BitWiseAnd => {
                e1 = (e1 & e_expr(&token_list[index + 1..], env)?)?;
                e1.tokens += 1;
            }
            Token::BitWiseOr => {
                e1 = (e1 | e_expr(&token_list[index + 1..], env)?)?;
                e1.tokens += 1;
            }
            Token::BitWiseXor => {
                e1 = (e1 ^ e_expr(&token_list[index + 1..], env)?)?;
                e1.tokens += 1;
            }
            Token::BitWiseLShift => {
                e1 = (e1 << e_expr(&token_list[index + 1..], env)?)?;
                e1.tokens += 1;
            }
            Token::BitWiseRShift => {
                e1 = (e1 >> e_expr(&token_list[index + 1..], env)?)?;
                e1.tokens += 1;
            }
            Token::Number(ref n) => {
                return Err(
                    CalcError::UnexpectedToken(n.to_string(), "operator"),
                );
            }
            _ => break,
        };
        index = e1.tokens;
    }
    Ok(e1)
}
// Addition and subtraction
fn e_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
    where E: Environment
{
    let mut t1 = t_expr(token_list, env)?;
    let mut index = t1.tokens;

    while index < token_list.len() {
        match token_list[index] {
            Token::Plus => {
                t1 = t1 + t_expr(&token_list[index + 1..], env)?;
                t1.tokens += 1;
            }
            Token::Minus => {
                t1 = t1 - t_expr(&token_list[index + 1..], env)?;
                t1.tokens += 1;
            }
            Token::Number(n) => {
                return Err(
                    CalcError::UnexpectedToken(n.to_string(), "operator"),
                )
            }
            _ => break,
        };
        index = t1.tokens;
    }
    Ok(t1)
}

// Multiplication and division
fn t_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
    where E: Environment
{
    let mut f1 = f_expr(token_list, env)?;
    let mut index = f1.tokens;

    while index < token_list.len() {
        match token_list[index] {
            Token::Multiply => {
                f1 = f1 * f_expr(&token_list[index + 1..], env)?;
                f1.tokens += 1;
            }
            Token::Divide => {
                f1 = (f1 / f_expr(&token_list[index + 1..], env)?)?;
                f1.tokens += 1;
            }
            Token::Modulo => {
                f1 = (f1 % f_expr(&token_list[index + 1..], env)?)?;
                f1.tokens += 1;
            }
            Token::Number(n) => {
                return Err(
                    CalcError::UnexpectedToken(n.to_string(), "operator"),
                );
            }
            _ => break,
        }
        index = f1.tokens;
    }
    Ok(f1)
}

// Exponentiation
fn f_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
    where E: Environment
{
    let mut g1 = g_expr(token_list, env)?; // was g1
    let mut index = g1.tokens;
    let token_len = token_list.len();
    while index < token_len {
        match token_list[index] {
            Token::Exponent => {
                let f = f_expr(&token_list[index + 1..], env)?;
                g1 = g1.pow(f)?;
                g1.tokens += 1;
            }
            Token::Square => {
                g1 = g1.powu(2);
                g1.tokens += 1;
            }
            Token::Cube => {
                g1 = g1.powu(3);
                g1.tokens += 1;
            }
            Token::Number(n) => {
                return Err(
                    CalcError::UnexpectedToken(n.to_string(), "operator"),
                );
            }
            _ => break,
        }
        index = g1.tokens;
    }
    Ok(g1)
}

// Numbers, parenthesized expressions, and atoms
fn g_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
    where E: Environment
{
    if !token_list.is_empty() {
        match token_list[0] {
            Token::Number(n) => Ok(IR::floating(n, 1)),
            Token::Atom(ref s) => {
                if let Some(nargs) = env.arity(s) {
                    let mut args = Vec::new();
                    let mut start = 1;
                    for _ in 0..nargs {
                        let ir = g_expr(&token_list[start..], env)?;
                        start += ir.tokens;
                        args.push(ir);
                    }
                    let res = env.resolve(s, &args);
                    Ok(IR::new(res?, start))
                } else {
                    Err(CalcError::UnknownAtom(s.clone()))
                }
            }
            Token::Minus => {
                let mut ir = -(d_expr(&token_list[1..], env)?);
                ir.tokens += 1;
                Ok(ir)
            }
            Token::OpenParen => {
                let mut ir = d_expr(&token_list[1..], env)?;
                let close_paren = ir.tokens + 1;
                if close_paren < token_list.len() {
                    match token_list[close_paren] {
                        Token::CloseParen => {
                            ir.tokens += 1;
                            Ok(ir)
                        }
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
            "pi" | "tau" => Some(0),
            "log" | "sin" | "cos" | "tan" => Some(1),
            _ => None,
        }
    }

    fn resolve(&mut self, atom: &str, args: &[IR]) -> Result<Value, CalcError> {
        unimplemented!()
        // match atom {
        //    "pi" => Ok(::std::f64::consts::PI),
        //    "tau" => Ok(::std::f64::consts::PI * 2.0),
        //    "log" => Ok(args[0].value.log(10.0)),
        //    "sin" => Ok(args[0].value.sin()),
        //    "cos" => Ok(args[0].value.cos()),
        //    "tan" => Ok(args[0].value.tan()),
        //    _ => Err(CalcError::UnknownAtom(atom.to_owned())),
        // }
    }
}

pub fn parse<E>(tokens: &[Token], env: &mut E) -> Result<Value, CalcError>
    where E: Environment
{
    d_expr(tokens, env).map(|answer| answer.value())
}

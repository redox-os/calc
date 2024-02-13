use crate::error::CalcError;
use crate::token::*;
use crate::value::{Value, IR};
use decimal::d128;
use rand::Rng;

const RECURSION_LIMIT: usize = 10;

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
        args: &[Value],
    ) -> Result<Value, CalcError>;

    fn add_recursion_level(&mut self);
    fn subtract_recursion_level(&mut self);
    fn get_recursion_level(&self) -> usize;

    fn ans(&self) -> &Option<Value>;
}

fn d_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
where
    E: Environment,
{
    if !token_list.is_empty() && token_list[0] == Token::BitWiseNot {
        let mut e = d_expr(&token_list[1..], env)?;
        e.value = (!e.value)?;
        e.tokens += 1;
        return Ok(e);
    }

    let mut e1 = e_expr(token_list, env)?;
    let mut index = e1.tokens;

    while index < token_list.len() {
        match token_list[index] {
            Token::BitWiseAnd => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                e1.value = (e1.value & e2.value)?;
                e1.tokens += e2.tokens + 1;
            }
            Token::BitWiseOr => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                e1.value = (e1.value | e2.value)?;
                e1.tokens += e2.tokens + 1;
            }
            Token::BitWiseXor => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                e1.value = (e1.value ^ e2.value)?;
                e1.tokens += e2.tokens + 1;
            }
            Token::BitWiseLShift => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                e1.value = (e1.value << e2.value)?;
                e1.tokens += e2.tokens + 1;
            }
            Token::BitWiseRShift => {
                let e2 = e_expr(&token_list[index + 1..], env)?;
                e1.value = (e1.value >> e2.value)?;
                e1.tokens += e2.tokens + 1;
            }
            Token::Number(ref n) => {
                return Err(CalcError::UnexpectedToken(
                    n.to_string(),
                    "operator",
                ));
            }
            _ => break,
        };
        index = e1.tokens;
    }
    Ok(e1)
}
// Addition and subtraction
fn e_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
where
    E: Environment,
{
    let mut t1 = t_expr(token_list, env)?;
    let mut index = t1.tokens;

    while index < token_list.len() {
        match token_list[index] {
            Token::Plus => {
                let t2 = t_expr(&token_list[index + 1..], env)?;
                t1.value = (t1.value + t2.value)?;
                t1.tokens += t2.tokens + 1;
            }
            Token::Minus => {
                let t2 = t_expr(&token_list[index + 1..], env)?;
                t1.value = (t1.value - t2.value)?;
                t1.tokens += t2.tokens + 1;
            }
            Token::Number(ref n) => {
                return Err(CalcError::UnexpectedToken(
                    n.to_string(),
                    "operator",
                ))
            }
            _ => break,
        };
        index = t1.tokens;
    }
    Ok(t1)
}

// Multiplication and division
fn t_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
where
    E: Environment,
{
    let mut f1 = f_expr(token_list, env)?;
    let mut index = f1.tokens;

    while index < token_list.len() {
        match token_list[index] {
            Token::Multiply => {
                let f2 = f_expr(&token_list[index + 1..], env)?;
                f1.value = (f1.value * f2.value)?;
                f1.tokens += f2.tokens + 1;
            }
            Token::Divide => {
                let f2 = f_expr(&token_list[index + 1..], env)?;
                f1.value = (f1.value / f2.value)?;
                f1.tokens += f2.tokens + 1;
            }
            Token::Modulo => {
                let f2 = f_expr(&token_list[index + 1..], env)?;
                f1.value = (f1.value % f2.value)?;
                f1.tokens += f2.tokens + 1;
            }
            Token::Dice => {
                let f2 = f_expr(&token_list[index + 1..], env)?;
                let dice_rolls: i32 = f1.value.as_float()?.into();
                let dice_max: i32 = f2.value.as_float()?.into();
                if dice_rolls < 1 || dice_max < 1 {
                    return Err(CalcError::ImpossibleDice);
                }
                let mut dice_result = 0;
                let mut rng = rand::thread_rng();
                for _i in 0..dice_rolls {
                    dice_result += rng.gen_range(1, dice_max + 1);
                }
                f1.value = Value::dec(dice_result);
                f1.tokens += f2.tokens + 1;
            }
            Token::Number(ref n) => {
                return Err(CalcError::UnexpectedToken(
                    n.to_string(),
                    "operator",
                ));
            }
            _ => break,
        }
        index = f1.tokens;
    }
    Ok(f1)
}

// Exponentiation
fn f_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
where
    E: Environment,
{
    let mut g1 = g_expr(token_list, env)?; // was g1
    let mut index = g1.tokens;
    let token_len = token_list.len();
    while index < token_len {
        match token_list[index] {
            Token::Exponent => {
                let f = f_expr(&token_list[index + 1..], env)?;
                g1.value = g1.value.pow(f.value)?;
                g1.tokens += f.tokens + 1;
            }
            Token::Square => {
                g1.value = (g1.value.clone() * g1.value)?;
                g1.tokens += 1;
            }
            Token::Cube => {
                g1.value = ((g1.value.clone() * g1.value.clone())? * g1.value)?;
                g1.tokens += 1;
            }
            Token::Number(ref n) => {
                return Err(CalcError::UnexpectedToken(
                    n.to_string(),
                    "operator",
                ));
            }
            _ => break,
        }
        index = g1.tokens;
    }
    Ok(g1)
}

// Numbers, parenthesized expressions, and atoms
fn g_expr<E>(token_list: &[Token], env: &mut E) -> Result<IR, CalcError>
where
    E: Environment,
{
    if !token_list.is_empty() {
        match token_list[0] {
            Token::Ans => match env.ans() {
                Some(v) => Ok(IR::new(v.clone(), 1)),
                None => Err(CalcError::MissingAns),
            },
            Token::Number(ref n) => Ok(IR::new(n.clone(), 1)),
            Token::Atom(ref s) => {
                if let Some(nargs) = env.arity(s) {
                    let mut args: Vec<Value> = Vec::new();
                    let mut start = 1;
                    for _ in 0..nargs {
                        let ir = g_expr(&token_list[start..], env)?;
                        start += ir.tokens;
                        args.push(ir.value);
                    }
                    let res = env.resolve(s, &args);
                    Ok(IR::new(res?, start))
                } else {
                    Err(CalcError::UnknownAtom(s.clone()))
                }
            }
            Token::Minus => {
                if token_list.len() >= 2 {
                    if let Token::Number(ref n) = token_list[1] {
                        Ok(IR::new(-n.clone(), 2))
                    } else {
                        let mut ir = d_expr(&token_list[1..], env)?;
                        ir.value = -ir.value;
                        ir.tokens += 1;
                        Ok(ir)
                    }
                } else {
                    Err(CalcError::UnexpectedEndOfInput)
                }
            }
            Token::OpenParen => {
                env.add_recursion_level();
                if env.get_recursion_level() > RECURSION_LIMIT {
                    Err(CalcError::RecursionLimitReached)?
                }
                let mut ir = d_expr(&token_list[1..], env)?;
                let close_paren = ir.tokens + 1;
                if close_paren < token_list.len() {
                    match token_list[close_paren] {
                        Token::CloseParen => {
                            env.subtract_recursion_level();
                            ir.tokens = close_paren + 1;
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

pub struct DefaultEnvironment {
    recursion_level: usize,
    ans: Option<Value>,
}

impl DefaultEnvironment {
    pub fn new() -> DefaultEnvironment {
        DefaultEnvironment {
            recursion_level: 0,
            ans: None,
        }
    }

    pub fn with_ans(ans: Option<Value>) -> DefaultEnvironment {
        DefaultEnvironment {
            recursion_level: 0,
            ans,
        }
    }
}

impl Environment for DefaultEnvironment {
    fn arity(&self, atom: &str) -> Option<usize> {
        match atom {
            "pi" | "tau" => Some(0),
            "log" => Some(1),
            "ln" => Some(1),
            _ => None,
        }
    }

    fn resolve(
        &mut self,
        atom: &str,
        args: &[Value],
    ) -> Result<Value, CalcError> {
        match atom {
            "pi" => {
                Ok(Value::Float(d128!(3.1415926535897932384626433832795028)))
            }
            "tau" => Ok(Value::Float(
                d128!(3.1415926535897932384626433832795028) * d128!(2.0),
            )),
            // "sin" => Ok(Value::Float(args[0].as_float().sin())),
            // "cos" => Ok(Value::Float(args[0].as_float().cos())),
            "log" => args[0].log(),
            "ln" => args[0].ln(),
            // "tan" => Ok(Value::Float(args[0].as_float().tan())),
            _ => Err(CalcError::UnknownAtom(atom.to_owned())),
        }
    }

    fn get_recursion_level(&self) -> usize {
        self.recursion_level
    }

    fn add_recursion_level(&mut self) {
        self.recursion_level += 1;
    }
    fn subtract_recursion_level(&mut self) {
        self.recursion_level -= 1;
    }

    fn ans(&self) -> &Option<Value> {
        &self.ans
    }
}

pub fn parse<E>(tokens: &[Token], env: &mut E) -> Result<Value, CalcError>
where
    E: Environment,
{
    d_expr(tokens, env).map(|answer| answer.value)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn unary_minus() {
        let expr = [
            Token::Minus,
            Token::Number(Value::dec(1)),
            Token::Plus,
            Token::Number(Value::dec(1)),
        ];
        let expected = Value::dec(0);
        let mut env = DefaultEnvironment::new();
        assert_eq!(super::parse(&expr, &mut env), Ok(expected));
    }

    #[test]
    fn dice_parse() {
        let expr = [
            Token::Number(Value::dec(3)),
            Token::Dice,
            Token::Number(Value::dec(6)),
        ];
        let mut env = DefaultEnvironment::new();
        let out = super::parse(&expr, &mut env);
        let out_float = out.unwrap().as_float().unwrap();
        assert!(out_float >= d128!(3.0) && out_float <= d128!(18.0));
    }

    #[test]
    fn ans_calculation() {
        let expr = [Token::Ans, Token::Multiply, Token::Number(Value::dec(3))];
        let expected = Value::dec(12);
        let mut env = DefaultEnvironment::with_ans(Some(Value::dec(4)));
        assert_eq!(super::parse(&expr, &mut env), Ok(expected));
    }

    #[test]
    fn function_binding() {
        let expr = [
            Token::Atom("log".into()),
            Token::Number(Value::Float(d128!(4.0))),
            Token::Divide,
            Token::Atom("log".into()),
            Token::Number(Value::Float(d128!(2.0))),
        ];
        let expected = Value::Float(d128!(2.0));
        let mut env = DefaultEnvironment::new();
        assert_eq!(super::parse(&expr, &mut env), Ok(expected));
    }
}

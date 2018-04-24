use error::CalcError;
use token::*;
use value::{into_float, Value, IR};

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
                t1.value = t1.value + t2.value;
                t1.tokens += t2.tokens + 1;
            }
            Token::Minus => {
                let t2 = t_expr(&token_list[index + 1..], env)?;
                t1.value = t1.value - t2.value;
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
                f1.value = f1.value * f2.value;
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
                g1.value = g1.value * g1.value;
                g1.tokens += 1;
            }
            Token::Cube => {
                g1.value = g1.value * g1.value * g1.value;
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
                let mut ir = d_expr(&token_list[1..], env)?;
                let close_paren = ir.tokens + 1;
                if close_paren < token_list.len() {
                    match token_list[close_paren] {
                        Token::CloseParen => {
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

pub struct DefaultEnvironment;

impl Environment for DefaultEnvironment {
    fn arity(&self, atom: &str) -> Option<usize> {
        match atom {
            "pi" | "tau" => Some(0),
            "log" => Some(1),
            _ => None,
        }
    }

    fn resolve(
        &mut self,
        atom: &str,
        args: &[Value],
    ) -> Result<Value, CalcError> {
        match atom {
            "pi" => Ok(Value::Float(d128!(
                3.1415926535897932384626433832795028
            ))),
            "tau" => Ok(Value::Float(
                d128!(3.1415926535897932384626433832795028) * into_float(2),
            )),
            "log" => Ok(Value::Float(args[0].as_float()?.log10())),
            // "sin" => Ok(Value::Float(args[0].as_float().sin())),
            // "cos" => Ok(Value::Float(args[0].as_float().cos())),
            // "tan" => Ok(Value::Float(args[0].as_float().tan())),
            _ => Err(CalcError::UnknownAtom(atom.to_owned())),
        }
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
            Token::Number(Value::Dec(1)),
            Token::Plus,
            Token::Number(Value::Dec(1)),
        ];
        let expected = Value::Dec(0);
        let mut env = DefaultEnvironment;
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
        let mut env = DefaultEnvironment;
        assert_eq!(super::parse(&expr, &mut env), Ok(expected));
    }

}

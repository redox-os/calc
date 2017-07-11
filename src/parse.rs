use token::*;
use error::CalcError;


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

    if !token_list.is_empty() && token_list[0] == Token::BitWiseNot {
        let mut e = e_expr(&token_list[1..], env)?;
        if e.is_whole() {
            let mut int_f = e.value.floor() as i64;
            int_f = !(int_f);
            e.value = int_f as f64;
            e.tokens_read += 1;
            return Ok(e);
        } else {
            return Err(CalcError::UnexpectedToken(
                e.value.to_string(),
                "Not an integer number!",
            ));
        }
    }

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
                let mut ir = d_expr(&token_list[1..], env)?;
                ir.value = -ir.value;
                ir.tokens_read += 1;
                Ok(ir)
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
            "pi" | "tau" => Some(0),
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
            "pi" => Ok(::std::f64::consts::PI),
            "tau" => Ok(::std::f64::consts::PI * 2.0),
            "log" => Ok(args[0].value.log(10.0)),
            "sin" => Ok(args[0].value.sin()),
            "cos" => Ok(args[0].value.cos()),
            "tan" => Ok(args[0].value.tan()),
            _ => Err(CalcError::UnknownAtom(atom.to_owned())),
        }
    }
}

pub fn parse<E>(tokens: &[Token], env: &mut E) -> Result<f64, CalcError>
    where E: Environment
{
    d_expr(tokens, env).map(|answer| answer.value)
}

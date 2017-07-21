use std::fmt;
use std::ops::*;
use error::{CalcError, PartialComp};

/// Represents a canonical value that can be calculated by this library
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// An integral value in decimal form. This is generally interoperable
    /// with
    /// the `Hex` constructor, but will be overriden by the `Hex`
    /// constructor.
    Dec(i64),
    /// An integral value in hexadecimal form. This takes precedence over
    /// the
    /// `Dec` constructor.
    Hex(i64),
    /// A floating point number
    Float(f64),
}

impl Value {
    pub fn is_zero(&self) -> bool {
        match *self {
            Value::Dec(n) | Value::Hex(n) => n == 0,
            Value::Float(f) => f == 0.0,
        }
    }

    pub fn as_float(&self) -> f64 {
        match *self {
            Value::Dec(n) | Value::Hex(n) => n as f64,
            Value::Float(n) => n,
        }
    }

    /// Represents a computation that can only operate on, and return,
    /// integer values
    pub fn intmap<F, T>(
        self,
        that: Value,
        op: T,
        f: F,
    ) -> Result<Value, CalcError>
        where F: Fn(i64, i64) -> i64,
              T: ToString
    {
        match (self, that) {
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) |
            (Value::Dec(n), Value::Hex(m)) => Ok(Value::Hex(f(n, m))),
            (Value::Dec(n), Value::Dec(m)) => Ok(Value::Dec(f(n, m))),
            (v1 @ Value::Float(_), v2 @ _) |
            (v1 @ _, v2 @ Value::Float(_)) => {
                Err(CalcError::BadTypes(PartialComp::binary(op, v1, v2)))
            }
        }
    }

    /// Represents a computation that will cast integer types to floating
    /// point
    pub fn castmap<F, G>(self, that: Value, f: F, g: G) -> Value
        where F: Fn(i64, i64) -> i64,
              G: Fn(f64, f64) -> f64
    {
        match (self, that) {
            (Value::Float(n), Value::Float(m)) => Value::Float(g(n, m)),
            (Value::Float(n), Value::Hex(m)) |
            (Value::Float(n), Value::Dec(m)) => Value::Float(g(n, m as f64)),
            (Value::Hex(n), Value::Float(m)) |
            (Value::Dec(n), Value::Float(m)) => Value::Float(g(n as f64, m)),
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Dec(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) => Value::Hex(f(n, m)),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(f(n, m)),
        }
    }

    pub fn pow(self, that: Value) -> Result<Self, CalcError> {
        let value = match (&self, &that) {
            (&Value::Float(n), &Value::Float(m)) => Value::Float(n.powf(m)),
            (&Value::Float(n), &Value::Hex(m)) |
            (&Value::Float(n), &Value::Dec(m)) => {
                if m > i32::max_value() as i64 {
                    return Err(CalcError::WouldOverflow(
                        PartialComp::binary("**", self, that),
                    ));
                } else if m < i32::min_value() as i64 {
                    return Err(CalcError::WouldTruncate(
                        PartialComp::binary("**", self, that),
                    ));
                } else {
                    Value::Float(n.powi(m as i32))
                }
            }
            (&Value::Hex(n), &Value::Float(m)) |
            (&Value::Dec(n), &Value::Float(m)) => {
                Value::Float((n as f64).powf(m))
            }
            (&Value::Hex(n), &Value::Hex(m)) |
            (&Value::Dec(n), &Value::Hex(m)) |
            (&Value::Hex(n), &Value::Dec(m)) => {
                if m > i32::max_value() as i64 {
                    return Err(CalcError::WouldOverflow(
                        PartialComp::binary("**", self, that),
                    ));
                } else if m < i32::min_value() as i64 {
                    return Err(CalcError::WouldTruncate(
                        PartialComp::binary("**", self, that),
                    ));
                } else if m < 0 {
                    Value::Float((n as f64).powi(m as i32))
                } else {
                    Value::Hex(n.pow(m as u32))
                }
            }
            (&Value::Dec(n), &Value::Dec(m)) => {
                if m > i32::max_value() as i64 {
                    return Err(CalcError::WouldOverflow(
                        PartialComp::binary("**", self, that),
                    ));
                } else if m < i32::min_value() as i64 {
                    return Err(CalcError::WouldTruncate(
                        PartialComp::binary("**", self, that),
                    ));
                } else if m < 0 {
                    Value::Float((n as f64).powi(m as i32))
                } else {
                    Value::Dec(n.pow(m as u32))
                }
            }
        };
        Ok(value)
    }

    pub fn powu(self, i: u32) -> Self {
        match self {
            Value::Float(n) => Value::Float(n.powi(i as i32)),
            Value::Dec(n) => Value::Dec(n.pow(i)),
            Value::Hex(n) => Value::Hex(n.pow(i)),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Dec(n) => write!(f, "{}", n),
            Value::Hex(n) => write!(f, "0x{:X}", n),
            Value::Float(n) => write!(f, "{}", n),
        }
    }
}

/// An intermediate result that can be computed by this library.
/// - `value` represents the current computed data
/// - `tokens` represents the number of tokens that have been consumed
#[derive(Clone, Debug, PartialEq)]
pub struct IR {
    pub value: Value,
    pub tokens: usize,
}

impl IR {
    pub fn new<T: Into<Option<usize>>>(value: Value, tokens: T) -> Self {
        IR {
            value,
            tokens: tokens.into().unwrap_or(0),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, that: Value) -> Self::Output {
        self.castmap(that, |x, y| x + y, |x, y| x + y)
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, that: Value) -> Self::Output {
        self.castmap(that, |x, y| x - y, |x, y| x - y)
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, that: Value) -> Self::Output {
        self.castmap(that, |x, y| x * y, |x, y| x * y)
    }
}

impl Div for Value {
    type Output = Result<Self, CalcError>;

    fn div(self, that: Value) -> Self::Output {
        if that.is_zero() {
            return Err(CalcError::DivideByZero);
        }
        let value = match (self, that) {
            (Value::Float(n), Value::Float(m)) => Value::Float(n / m),
            (Value::Float(n), Value::Hex(m)) |
            (Value::Float(n), Value::Dec(m)) => Value::Float(n / m as f64),
            (Value::Hex(n), Value::Float(m)) |
            (Value::Dec(n), Value::Float(m)) => Value::Float(n as f64 / m),
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Dec(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) => {
                if n % m == 0 {
                    Value::Hex(n / m)
                } else {
                    Value::Float(n as f64 / m as f64)
                }
            }
            (Value::Dec(n), Value::Dec(m)) => {
                if n % m == 0 {
                    Value::Dec(n / m)
                } else {
                    Value::Float(n as f64 / m as f64)
                }
            }
        };
        Ok(value)
    }
}

impl BitAnd for Value {
    type Output = Result<Self, CalcError>;

    fn bitand(self, that: Value) -> Self::Output {
        self.intmap(that, "&", |n, m| n & m)
    }
}

impl BitOr for Value {
    type Output = Result<Self, CalcError>;

    fn bitor(self, that: Value) -> Self::Output {
        self.intmap(that, "|", |n, m| n | m)
    }
}

impl BitXor for Value {
    type Output = Result<Self, CalcError>;

    fn bitxor(self, that: Value) -> Self::Output {
        self.intmap(that, "^", |n, m| n ^ m)
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Hex(n) => Value::Hex(-n),
            Value::Dec(n) => Value::Dec(-n),
            Value::Float(f) => Value::Float(-f),
        }
    }
}

impl Not for Value {
    type Output = Result<Self, CalcError>;

    fn not(self) -> Self::Output {
        match self {
            Value::Hex(n) => Ok(Value::Hex(!n)),
            Value::Dec(n) => Ok(Value::Dec(!n)),
            Value::Float(f) => {
                return Err(CalcError::BadTypes(PartialComp::unary("~", f)))
            }
        }
    }
}


impl Rem for Value {
    type Output = Result<Self, CalcError>;

    fn rem(self, that: Value) -> Self::Output {
        if that.is_zero() {
            return Err(CalcError::DivideByZero);
        }
        Ok(self.castmap(that, |x, y| x % y, |x, y| x % y))
    }
}

impl Shl<Value> for Value {
    type Output = Result<Self, CalcError>;

    fn shl(self, that: Value) -> Self::Output {
        self.intmap(that, "<<", |n, m| n << m)
    }
}


impl Shr<Value> for Value {
    type Output = Result<Self, CalcError>;

    fn shr(self, that: Value) -> Self::Output {
        self.intmap(that, ">>", |n, m| n >> m)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn float_override() {
        let cases = vec![
            (Value::Float(3.0) + Value::Dec(1), Value::Float(4.0)),
            (Value::Hex(5) - Value::Float(4.5), Value::Float(0.5)),
            (
                Value::Hex(24) * Value::Dec(4) *
                    Value::Float(1.0 / 48.0),
                Value::Float(2.0)
            ),
        ];

        for (output, expected) in cases {
            assert_eq!(output, expected);
        }
    }

    #[test]
    fn hex_override() {
        let cases = vec![
            (Value::Hex(3) * Value::Dec(-2), Value::Hex(-6)),
            (
                (Value::Hex(0x100) >> Value::Hex(0x2)).unwrap(),
                Value::Hex(0x40)
            ),
        ];
        for (output, expected) in cases {
            assert_eq!(output, expected);
        }
    }

}

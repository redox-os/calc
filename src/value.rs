use std::fmt;
use std::ops::*;
use error::{CalcError, PartialComp};

#[derive(Clone, Debug)]
pub struct IntermediateResult {
    pub value: f64,
    pub tokens_read: usize,
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
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Dec(n) => write!(f, "{}", n),
            Value::Hex(n) => write!(f, "{:X}", n),
            Value::Float(n) => write!(f, "{}", n),
        }
    }
}

/// An intermediate result that can be computed by this library.
/// - `value` represents the current computed data
/// - `tokens` represents the number of tokens that have been consumed
#[derive(Clone, Debug, PartialEq)]
pub struct IR {
    value: Value,
    pub tokens: usize,
}

impl IR {
    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn new<T: Into<Option<usize>>>(value: Value, tokens: T) -> Self {
        IR {
            value,
            tokens: tokens.into().unwrap_or(0),
        }
    }

    /// Construct a new decimal result
    pub fn dec<T: Into<Option<usize>>>(v: i64, tokens: T) -> Self {
        IR {
            value: Value::Dec(v),
            tokens: tokens.into().unwrap_or(0),
        }
    }

    /// Construct a new hexadecimal result
    pub fn hex<T: Into<Option<usize>>>(v: i64, tokens: T) -> Self {
        IR {
            value: Value::Hex(v),
            tokens: tokens.into().unwrap_or(0),
        }
    }

    /// Construt a new floating point result
    pub fn floating<T: Into<Option<usize>>>(v: f64, tokens: T) -> Self {
        IR {
            value: Value::Float(v),
            tokens: tokens.into().unwrap_or(0),
        }
    }

    pub fn powu(self, i: u32) -> Self {
        match self.value {
            Value::Float(n) => {
                IR::new(Value::Float(n.powi(i as i32)), self.tokens)
            }
            Value::Dec(n) => IR::new(Value::Dec(n.pow(i)), self.tokens),
            Value::Hex(n) => IR::new(Value::Hex(n.pow(i)), self.tokens),
        }
    }

    pub fn pow(self, that: IR) -> Result<Self, CalcError> {
        let value = match (&self.value, &that.value) {
            (&Value::Float(n), &Value::Float(m)) => Value::Float(n.powf(m)),
            (&Value::Float(n), &Value::Hex(m)) |
            (&Value::Float(n), &Value::Dec(m)) => {
                if m > i32::max_value() as i64 {
                    return Err(CalcError::WouldOverflow(
                        PartialComp::binary("**", self.value, that.value),
                    ));
                } else if m < i32::min_value() as i64 {
                    return Err(CalcError::WouldTruncate(
                        PartialComp::binary("**", self.value, that.value),
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
                        PartialComp::binary("**", self.value, that.value),
                    ));
                } else if m < i32::min_value() as i64 {
                    return Err(CalcError::WouldTruncate(
                        PartialComp::binary("**", self.value, that.value),
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
                        PartialComp::binary("**", self.value, that.value),
                    ));
                } else if m < i32::min_value() as i64 {
                    return Err(CalcError::WouldTruncate(
                        PartialComp::binary("**", self.value, that.value),
                    ));
                } else if m < 0 {
                    Value::Float((n as f64).powi(m as i32))
                } else {
                    Value::Dec(n.pow(m as u32))
                }
            }
        };
        Ok(IR {
            value,
            tokens: self.tokens + that.tokens,
        })
    }
}

impl Add for IR {
    type Output = Self;

    fn add(self, that: IR) -> Self::Output {
        let value = self.value.castmap(that.value, |x, y| x + y, |x, y| x + y);
        IR {
            value,
            tokens: self.tokens + that.tokens,
        }
    }
}

impl Sub for IR {
    type Output = Self;

    fn sub(self, that: IR) -> Self::Output {
        let value = self.value.castmap(that.value, |x, y| x - y, |x, y| x - y);
        IR {
            value,
            tokens: self.tokens + that.tokens,
        }
    }
}

impl Mul for IR {
    type Output = Self;

    fn mul(self, that: IR) -> Self::Output {
        let value = self.value.castmap(that.value, |x, y| x * y, |x, y| x * y);
        IR {
            value,
            tokens: self.tokens + that.tokens,
        }
    }
}

impl Div for IR {
    type Output = Result<Self, CalcError>;

    fn div(self, that: IR) -> Self::Output {
        if that.value.is_zero() {
            return Err(CalcError::DivideByZero);
        }
        let value = match (self.value, that.value) {
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
        Ok(IR {
            value,
            tokens: self.tokens + that.tokens,
        })
    }
}

impl BitAnd for IR {
    type Output = Result<Self, CalcError>;

    fn bitand(self, that: IR) -> Self::Output {
        let value = self.value.intmap(that.value, "&", |n, m| n & m)?;
        Ok(IR {
            value,
            tokens: self.tokens + that.tokens,
        })
    }
}

impl BitOr for IR {
    type Output = Result<Self, CalcError>;

    fn bitor(self, that: IR) -> Self::Output {
        let value = self.value.intmap(that.value, "|", |n, m| n | m)?;
        Ok(IR {
            value,
            tokens: self.tokens + that.tokens,
        })
    }
}

impl BitXor for IR {
    type Output = Result<Self, CalcError>;

    fn bitxor(self, that: IR) -> Self::Output {
        let value = self.value.intmap(that.value, "^", |n, m| n ^ m)?;
        Ok(IR {
            value,
            tokens: self.tokens + that.tokens,
        })
    }
}

impl Neg for IR {
    type Output = Self;

    fn neg(self) -> Self::Output {
        let value = match self.value {
            Value::Hex(n) => Value::Hex(-n),
            Value::Dec(n) => Value::Dec(-n),
            Value::Float(f) => Value::Float(-f),
        };
        IR {
            value,
            tokens: self.tokens,
        }
    }
}

impl Not for IR {
    type Output = Result<Self, CalcError>;

    fn not(self) -> Self::Output {
        let value = match self.value {
            Value::Hex(n) => Value::Hex(!n),
            Value::Dec(n) => Value::Dec(!n),
            Value::Float(f) => {
                return Err(CalcError::BadTypes(PartialComp::unary("~", f)))
            }
        };
        Ok(IR {
            value,
            tokens: self.tokens,
        })
    }
}


impl Rem for IR {
    type Output = Result<Self, CalcError>;

    fn rem(self, that: IR) -> Self::Output {
        if that.value.is_zero() {
            return Err(CalcError::DivideByZero);
        }
        let value = self.value.castmap(that.value, |x, y| x % y, |x, y| x % y);
        Ok(IR {
            value,
            tokens: self.tokens + that.tokens,
        })
    }
}

impl Shl<IR> for IR {
    type Output = Result<Self, CalcError>;

    fn shl(self, that: IR) -> Self::Output {
        let value = self.value.intmap(that.value, "<<", |n, m| n << m)?;
        Ok(IR {
            value,
            tokens: self.tokens + that.tokens,
        })
    }
}


impl Shr<IR> for IR {
    type Output = Result<Self, CalcError>;

    fn shr(self, that: IR) -> Self::Output {
        let value = self.value.intmap(that.value, ">>", |n, m| n >> m)?;
        Ok(IR {
            value,
            tokens: self.tokens + that.tokens,
        })
    }
}

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
#[derive(Clone, Debug)]
pub enum Value {
    /// An integral value in decimal form. This is generally interoperable with
    /// the `Hex` constructor, but will be overriden by the `Hex` constructor.
    Dec(i64),
    /// An integral value in hexadecimal form. This takes precedence over the
    /// `Dec` constructor.
    Hex(i64),
    /// A floating point number
    Float(f64),
}

impl fmt::Display for Value {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Dec(n) => write!(f, "{}", n),
            Value::Hex(n) => write!(f, "{:X}", n),
            Value::Float(n) => write!(f, "{}", n)
        }
    }

}

/// An intermediate result that can be computed by this library.
/// - `value` represents the current computed data
/// - `tokens` represents the number of tokens that have been consumed
pub struct IR {
    value: Value,
    pub tokens: usize
}

impl IR {

    /// Construct a new decimal result
    pub fn dec<T: Into<Option<usize>>>(v: i64, tokens: T) -> Self {
        IR {value: Value::Dec(v), tokens: tokens.into().unwrap_or(0)}
    }

    /// Construct a new hexadecimal result
    pub fn hex<T: Into<Option<usize>>>(v: i64, tokens: T) -> Self {
        IR {value: Value::Hex(v), tokens: tokens.into().unwrap_or(0)}
    }

    /// Construt a new floating point result
    pub fn floating<T: Into<Option<usize>>>(v: f64, tokens: T) -> Self {
        IR {
            value: Value::Float(v),
            tokens: tokens.into().unwrap_or(0),
        }
    }

    pub fn pow(self, that: IR) -> Self {
        let value = match (self.value, that.value) {
            (Value::Float(n), Value::Float(m)) => Value::Float(n.powf(m)),
            (a @ Value::Float(n), b @ Value::Hex(m)) |
            (a @ Value::Float(n), b @ Value::Dec(m)) => {
                if m > std::i32::MAX {
                    return Err(CalcError::WouldOverflow(
                        PartialComp::binary("**", a, b)
                    ))
                } else if m < std::i32::MIN {
                    return Err(CalcError::WouldTruncate(
                        PartialComp::binary("**", a, b)
                    })
                } else {
                    Value::Float(n.powi(m as i32))
                }
            }
            (Value::Hex(n), Value::Float(m)) |
            (Value::Dec(n), Value::Float(m)) => {
                Value::Float((n as f64).powf(m))
            }
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Dec(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) => {
                if m > std::i32::MAX {
                    return Err(CalcError::WouldOverflow(
                        PartialComp::binary("**", a, b)
                    ))
                } else if m < std::i32::MIN {
                    return Err(CalcError::WouldTruncate(
                        PartialComp::binary("**", a, b)
                    })
                } else {
                    Value::Hex(n.pow(m))
                }
            }
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n + m),
        };
    }

}

impl Add for IR {
    type Output = Self;

    fn add(self, that: IR) -> Self::Output {
        let value = match (self.value, that.value) {
            (Value::Float(n), Value::Float(m)) => Value::Float(n + m),
            (Value::Float(n), Value::Hex(m)) |
            (Value::Float(n), Value::Dec(m)) => Value::Float(n + (m as f64)),
            (Value::Hex(n), Value::Float(m)) |
            (Value::Dec(n), Value::Float(m)) => Value::Float((n as f64) + m),
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Dec(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) => Value::Hex(n + m),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n + m),
        };
        IR { value, tokens: self.tokens + that.tokens }
    }

}

impl Sub for IR {
    type Output = Self;

    fn sub(self, that: IR) -> Self::Output {
        let value = match (self.value, that.value) {
            (Value::Float(n), Value::Float(m)) => Value::Float(n - m),
            (Value::Float(n), Value::Hex(m)) |
            (Value::Float(n), Value::Dec(m)) => Value::Float(n - (m as f64)),
            (Value::Hex(n), Value::Float(m)) |
            (Value::Dec(n), Value::Float(m)) => Value::Float((n as f64) - m),
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Dec(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) => Value::Hex(n - m),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n - m),
        };
        IR { value, tokens: self.tokens + that.tokens }
    }

}

impl Mul for IR {
    type Output = Self;

    fn mul(self, that: IR) -> Self::Output {
        let value = match (self.value, that.value) {
            (Value::Float(n), Value::Float(m)) => Value::Float(n * m),
            (Value::Float(n), Value::Hex(m)) |
            (Value::Float(n), Value::Dec(m)) => Value::Float(n * (m as f64)),
            (Value::Hex(n), Value::Float(m)) |
            (Value::Dec(n), Value::Float(m)) => Value::Float((n as f64) * m),
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Dec(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) => Value::Hex(n * m),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n * m),
        };
        IR { value, tokens: self.tokens + that.tokens }
    }

}

impl Div for IR {
    type Output = Result<Self, CalcError>;

    fn div(self, that: IR) -> Self::Output {

        // XXX: This whole construct can be replaced with `std::num::Zero` once
        // it is stable.
        macro_rules! safe {
            ($lhs:expr, $rhs:expr, $zero:expr) => {{
                if $rhs == $zero {
                    Err(CalcError::DivideByZero)
                } else {
                    Ok($lhs / $rhs)
                }
            }}
        }

        let value = match (self.value, that.value) {
            (Value::Float(n), Value::Float(m)) => Value::Float(safe!(n, m, 0.0)?),
            (Value::Float(n), Value::Hex(m)) |
            (Value::Float(n), Value::Dec(m)) => Value::Float(safe!(n, m as f64, 0.0)?),
            (Value::Hex(n), Value::Float(m)) |
            (Value::Dec(n), Value::Float(m)) => Value::Float(safe!(n as f64, m, 0.0)?),
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Dec(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) => Value::Hex(safe!(n, m, 0)?),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(safe!(n, m, 0)?),
        };
        Ok(IR { value, tokens: self.tokens + that.tokens })
    }

}

impl BitAnd for IR {
    type Output = Result<Self, CalcError>;

    fn bitand(self, that: IR) -> Self::Output {
        let value = match (self.value, that.value) {
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) |
            (Value::Dec(n), Value::Hex(m)) => Value::Hex(n & m),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n & m),
            (v1 @ Value::Float(_), v2 @ _) |
            (v1 @ _, v2 @ Value::Float(_)) => {
                return Err(CalcError::BadTypes(
                    PartialComp::binary("&", v1, v2)
                ))
            }
        };
        Ok(IR { value, tokens: self.tokens + that.tokens })
    }
}

impl BitOr for IR {
    type Output = Result<Self, CalcError>;

    fn bitor(self, that: IR) -> Self::Output {
        let value = match (self.value, that.value) {
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) |
            (Value::Dec(n), Value::Hex(m)) => Value::Hex(n | m),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n | m),
            (v1 @ Value::Float(_), v2 @ _) |
            (v1 @ _, v2 @ Value::Float(_)) => {
                return Err(CalcError::BadTypes(
                    PartialComp::binary("|", v1, v2)
                ))
            }
        };
        Ok(IR { value, tokens: self.tokens + that.tokens })
    }
}

impl BitXor for IR {
    type Output = Result<Self, CalcError>;

    fn bitxor(self, that: IR) -> Self::Output {
        let value = match (self.value, that.value) {
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) |
            (Value::Dec(n), Value::Hex(m)) => Value::Hex(n ^ m),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n ^ m),
            (v1 @ Value::Float(_), v2 @ _) |
            (v1 @ _, v2 @ Value::Float(_)) => {
                return Err(CalcError::BadTypes(
                    PartialComp::binary("^", v1, v2)
                ))
            }
        };
        Ok(IR { value, tokens: self.tokens + that.tokens })
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
        IR { value, tokens: self.tokens }
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
        Ok(IR { value, tokens: self.tokens })
    }
}


impl Rem for IR {
    type Output = Result<Self, CalcError>;

    fn rem(self, that: IR) -> Self::Output {

        // XXX: This whole construct can be replaced with `std::num::Zero` once
        // it is stable.
        macro_rules! safe {
            ($lhs:expr, $rhs:expr, $zero:expr) => {{
                if $rhs == $zero {
                    Err(CalcError::DivideByZero)
                } else {
                    Ok($lhs % $rhs)
                }
            }}
        }

        let value = match (self.value, that.value) {
            (Value::Float(n), Value::Float(m)) => Value::Float(safe!(n, m, 0.0)?),
            (Value::Float(n), Value::Hex(m)) |
            (Value::Float(n), Value::Dec(m)) => Value::Float(safe!(n, m as f64, 0.0)?),
            (Value::Hex(n), Value::Float(m)) |
            (Value::Dec(n), Value::Float(m)) => Value::Float(safe!(n as f64, m, 0.0)?),
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Dec(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) => Value::Hex(safe!(n, m, 0)?),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(safe!(n, m, 0)?),
        };
        Ok(IR { value, tokens: self.tokens + that.tokens })
    }

}

impl Shl<IR> for IR {
    type Output = Result<Self, CalcError>;

    fn shl(self, that: IR) -> Self::Output {
        let value = match (self.value, that.value) {
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) |
            (Value::Dec(n), Value::Hex(m)) => Value::Hex(n << m),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n << m),
            (v1 @ Value::Float(_), v2 @ _) |
            (v1 @ _, v2 @ Value::Float(_)) => {
                return Err(CalcError::BadTypes(
                    PartialComp::binary("<<", v1, v2)
                ))
            }
        };
        Ok(IR { value, tokens: self.tokens + that.tokens })
    }
}


impl Shr<IR> for IR {
    type Output = Result<Self, CalcError>;

    fn shr(self, that: IR) -> Self::Output {
        let value = match (self.value, that.value) {
            (Value::Hex(n), Value::Hex(m)) |
            (Value::Hex(n), Value::Dec(m)) |
            (Value::Dec(n), Value::Hex(m)) => Value::Hex(n >> m),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(n >> m),
            (v1 @ Value::Float(_), v2 @ _) |
            (v1 @ _, v2 @ Value::Float(_)) => {
                return Err(CalcError::BadTypes(
                    PartialComp::binary(">>", v1, v2)
                ))
            }
        };
        Ok(IR { value, tokens: self.tokens + that.tokens })
    }
}
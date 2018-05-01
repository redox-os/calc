use decimal::d128;
use error::{CalcError, PartialComp};
use num::Zero;
use std::fmt;
use std::ops::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntegralFmt {
    Dec,
    Hex,
}

impl Add for IntegralFmt {
    type Output = IntegralFmt;
    fn add(self, that: IntegralFmt) -> Self::Output {
        match (self, that) {
            (IntegralFmt::Hex, _) | (_, IntegralFmt::Hex) => IntegralFmt::Hex,
            _ => IntegralFmt::Dec,
        }
    }
}

impl<'a> Add<&'a IntegralFmt> for IntegralFmt {
    type Output = IntegralFmt;
    fn add(self, that: &IntegralFmt) -> Self::Output {
        match (self, *that) {
            (IntegralFmt::Hex, _) | (_, IntegralFmt::Hex) => IntegralFmt::Hex,
            _ => IntegralFmt::Dec,
        }
    }
}

impl IntegralFmt {
    pub fn merge(&self, that: &IntegralFmt) -> Self {
        match (*self, *that) {
            (IntegralFmt::Hex, _) | (_, IntegralFmt::Hex) => IntegralFmt::Hex,
            _ => IntegralFmt::Dec,
        }
    }
}

/// Represents a canonical value that can be calculated by this library
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// An integral value. The format of this value (hexadecimal versus
    /// decimal) is determined by the tag.
    Integral(i64, IntegralFmt),
    /// A 128-bit decimal floating point number
    Float(d128),
}

mod safe_ops {

    use super::{into_float, Value};

    fn highest_bit(mut n: u64) -> u64 {
        let mut bits = 0;
        while n != 0 {
            bits += 1;
            n >>= 1;
        }
        return bits;
    }

    pub fn int_pow(
        n: i64,
        m: i64,
        tag: super::IntegralFmt,
    ) -> Option<super::Value> {
        let bits = highest_bit(n as u64);
        match bits.checked_mul(m as u64) {
            Some(m) if m > 64 => None,
            Some(_) => {
                if m < 0 {
                    Some(Value::Float(into_float(n).pow(into_float(m))))
                } else {
                    Some(Value::Integral(n.pow(m as u32), tag))
                }
            }
            None => None,
        }
    }

}

impl Value {
    pub fn hex<T: Into<i64>>(n: T) -> Self {
        Value::Integral(n.into(), IntegralFmt::Hex)
    }

    pub fn dec<T: Into<i64>>(n: T) -> Self {
        Value::Integral(n.into(), IntegralFmt::Dec)
    }

    pub fn is_zero(&self) -> bool {
        match *self {
            Value::Integral(n, _) => n.is_zero(),
            Value::Float(f) => f == d128!(0),
        }
    }

    pub fn as_float(&self) -> d128 {
        match *self {
            Value::Integral(n, _) => into_float(n),
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
    where
        F: Fn(i64, i64) -> i64,
        T: ToString,
    {
        match (self, that) {
            (Value::Integral(n, t1), Value::Integral(m, t2)) => {
                Ok(Value::Integral(f(n, m), t1 + t2))
            }
            (v1 @ Value::Float(_), v2 @ _) | (v1 @ _, v2 @ Value::Float(_)) => {
                Err(CalcError::BadTypes(PartialComp::binary(
                    op, v1, v2,
                )))
            }
        }
    }

    /// Represents a computation that will cast integer types to floating
    /// point
    pub fn castmap<F, G>(self, that: Value, f: F, g: G) -> Value
    where
        F: Fn(i64, i64) -> i64,
        G: Fn(d128, d128) -> d128,
    {
        match (self, that) {
            (Value::Float(n), Value::Float(m)) => Value::Float(g(n, m)),
            (Value::Float(n), Value::Integral(m, _)) => {
                Value::Float(g(n, into_float(m)))
            }
            (Value::Integral(n, _), Value::Float(m)) => {
                Value::Float(g(into_float(n), m))
            }
            (Value::Integral(n, t1), Value::Integral(m, t2)) => {
                Value::Integral(f(n, m), t1 + t2)
            }
        }
    }

    pub fn pow(self, that: Value) -> Result<Self, CalcError> {
        let value = match (&self, &that) {
            (&Value::Float(n), &Value::Float(m)) => Value::Float(n.pow(m)),
            (&Value::Float(n), &Value::Integral(m, _)) => {
                if m > i32::max_value() as i64 {
                    return Err(CalcError::WouldOverflow(PartialComp::binary(
                        "**", self, that,
                    )));
                } else if m < i32::min_value().into() {
                    return Err(CalcError::WouldTruncate(PartialComp::binary(
                        "**", self, that,
                    )));
                } else {
                    Value::Float(n.pow(into_float(m)))
                }
            }
            (&Value::Integral(n, _), &Value::Float(m)) => {
                Value::Float(into_float(n).pow(m))
            }
            (&Value::Integral(n, t1), &Value::Integral(m, t2)) => {
                match safe_ops::int_pow(n, m, t1 + t2) {
                    Some(v) => v,
                    None => {
                        return Err(CalcError::WouldOverflow(
                            PartialComp::binary("**", self, that),
                        ))
                    }
                }
            }
        };
        Ok(value)
    }

    pub fn powu(self, i: u32) -> Self {
        match self {
            Value::Float(n) => Value::Float(n.pow(into_float(i))),
            Value::Integral(n, t) => Value::Integral(n.pow(i), t),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Integral(n, IntegralFmt::Dec) => write!(f, "{}", n),
            Value::Integral(n, IntegralFmt::Hex) => write!(f, "0x{:X}", n),
            Value::Float(n) => write!(f, "{}", n),
        }
    }
}

pub fn into_float<INT: Into<d128>>(dec: INT) -> d128 {
    dec.into()
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
            (Value::Float(n), Value::Integral(m, _)) => {
                Value::Float(n / into_float(m))
            }
            (Value::Integral(n, _), Value::Float(m)) => {
                Value::Float(into_float(n) / m)
            }
            (Value::Integral(n, t1), Value::Integral(m, t2)) => {
                if n % m == 0 {
                    Value::Integral(n / m, t1 + t2)
                } else {
                    Value::Float(into_float(n) / into_float(m))
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
            Value::Integral(n, t) => Value::Integral(-n, t),
            Value::Float(f) => Value::Float(-f),
        }
    }
}

impl Not for Value {
    type Output = Result<Self, CalcError>;

    fn not(self) -> Self::Output {
        match self {
            Value::Integral(n, t) => Ok(Value::Integral(!n, t)),
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
            (
                Value::Float(d128!(3)) + Value::dec(1),
                Value::Float(d128!(4)),
            ),
            (
                Value::hex(5) - Value::Float(d128!(4.5)),
                Value::Float(d128!(0.5)),
            ),
            (
                Value::hex(24) * Value::dec(4)
                    * Value::Float(d128!(1) / d128!(48)),
                Value::Float(d128!(2)),
            ),
        ];

        for (output, expected) in cases {
            assert_eq!(output, expected);
        }
    }

    #[test]
    fn hex_override() {
        let cases = vec![
            (Value::hex(3) * Value::dec(-2), Value::hex(-6)),
            (
                (Value::hex(0x100) >> Value::hex(0x2)).unwrap(),
                Value::hex(0x40),
            ),
        ];
        for (output, expected) in cases {
            assert_eq!(output, expected);
        }
    }

    #[test]
    fn pow_overflow() {
        let lhs = Value::dec(2);
        let rhs = Value::dec(63);
        let out = Err(CalcError::WouldOverflow(PartialComp::binary(
            "**", lhs, rhs,
        )));
        assert_eq!(out, Value::dec(2).pow(Value::dec(63)));
    }

}

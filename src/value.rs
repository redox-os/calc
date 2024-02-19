use crate::error::{CalcError, PartialComp};
use decimal::d128;
use num::{BigInt, BigUint, ToPrimitive, Zero};
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::ops::*;
use std::str::FromStr;

pub type Integral = BigInt;
type UIntegral = BigUint;

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
    Integral(Integral, IntegralFmt),
    /// A 128-bit decimal floating point number
    Float(d128),
}

pub mod ops {
    use super::{CalcError, Integral, PartialComp, UIntegral};
    use decimal::d128;
    use num::{Integer, ToPrimitive, Zero};
    use std::iter::repeat;

    macro_rules! bitwise_op {
        ($name:ident, $fun:expr) => {
            pub fn $name(n: &Integral, m: &Integral) -> Integral {
                bitwise(n, m, $fun)
            }
        };
    }

    fn equalize(left: &mut Vec<u8>, right: &mut Vec<u8>) {
        match left.len().cmp(&right.len()) {
            std::cmp::Ordering::Greater => {
                for it in repeat(Zero::zero()).take(left.len() - right.len()) {
                    right.push(it);
                }
            }
            std::cmp::Ordering::Equal => {}
            std::cmp::Ordering::Less => {
                for it in repeat(Zero::zero()).take(right.len() - left.len()) {
                    left.push(it);
                }
            }
        }
    }

    pub fn bitwise<F: Fn(u8, u8) -> u8>(
        n: &Integral,
        m: &Integral,
        fun: F,
    ) -> Integral {
        let mut n_bytes = n.to_signed_bytes_le();
        let mut m_bytes = m.to_signed_bytes_le();
        equalize(&mut n_bytes, &mut m_bytes);
        let res: Vec<u8> = n_bytes
            .iter()
            .zip(m_bytes)
            .map(|(n, m)| fun(*n, m))
            .collect();
        Integral::from_signed_bytes_le(&res)
    }

    bitwise_op!(and, |n, m| n & m);
    bitwise_op!(or, |n, m| n | m);
    bitwise_op!(xor, |n, m| n ^ m);

    pub fn not(n: Integral) -> Integral {
        let bytes = n.to_signed_bytes_le();
        let res: Vec<u8> = bytes.iter().map(|n| !n).collect();
        Integral::from_signed_bytes_le(&res)
    }

    pub fn int_powu(n: &Integral, m: &UIntegral) -> Integral {
        if m.is_zero() {
            1.into()
        } else if m.is_even() {
            int_powu(&(n * n), &(m / (2_u8)))
        } else {
            n * int_powu(&(n * n), &((m - (1_u8)) / (2_u8)))
        }
    }

    pub fn int_pow(n: &Integral, m: &Integral) -> Option<Integral> {
        if *m >= Zero::zero() {
            Some(int_powu(n, &m.to_biguint().unwrap()))
        } else {
            unimplemented!()
        }
    }

    pub fn to_float(n: &Integral) -> Result<d128, CalcError> {
        n.to_i64().map(Into::into).ok_or(CalcError::WouldTruncate(
            PartialComp::ToFloat(n.to_string()),
        ))
    }
}

impl Value {
    pub fn hex<T: Into<Integral>>(n: T) -> Self {
        Value::Integral(n.into(), IntegralFmt::Hex)
    }

    pub fn dec<T: Into<Integral>>(n: T) -> Self {
        Value::Integral(n.into(), IntegralFmt::Dec)
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Value::Integral(ref n, _) => n.is_zero(),
            Value::Float(ref f) => *f == d128!(0),
        }
    }

    // Check if Value is Not a Number
    pub fn is_nan(&self) -> bool {
        match self {
            // Integral is never NaN
            Value::Integral(_, _) => false,
            Value::Float(ref f) => f.is_nan(),
        }
    }

    pub fn as_float(&self) -> Result<d128, CalcError> {
        match self {
            Value::Integral(ref n, _) => ops::to_float(n),
            Value::Float(ref n) => Ok(*n),
        }
    }

    /// Represents a computation that can only operate on, and return,
    /// integer values
    pub fn intmap<F, T>(
        &self,
        that: &Value,
        op: T,
        f: F,
    ) -> Result<Value, CalcError>
    where
        F: Fn(&Integral, &Integral) -> Result<Integral, CalcError>,
        T: ToString,
    {
        match (self, that) {
            (Value::Integral(n, t1), Value::Integral(m, t2)) => {
                Ok(Value::Integral(f(n, m)?, *t1 + t2))
            }
            (v1 @ Value::Float(_), v2) | (v1, v2 @ Value::Float(_)) => {
                Err(CalcError::BadTypes(PartialComp::binary(op, v1, v2)))
            }
        }
    }

    /// Represents a computation that will cast integer types to floating
    /// point. There might be a possible truncation when we convert a BigInt
    /// into a floating point, so we have to be careful here.
    pub fn castmap<F, G>(
        self,
        that: Value,
        f: F,
        g: G,
    ) -> Result<Value, CalcError>
    where
        F: Fn(Integral, Integral) -> Integral,
        G: Fn(d128, d128) -> d128,
    {
        let ret = match (self, that) {
            (Value::Float(n), Value::Float(m)) => Value::Float(g(n, m)),
            (Value::Float(n), Value::Integral(m, _)) => {
                Value::Float(g(n, ops::to_float(&m)?))
            }
            (Value::Integral(n, _), Value::Float(m)) => {
                Value::Float(g(ops::to_float(&n)?, m))
            }
            (Value::Integral(n, t1), Value::Integral(m, t2)) => {
                Value::Integral(f(n, m), t1 + t2)
            }
        };
        Ok(ret)
    }

    pub fn pow(self, that: Value) -> Result<Self, CalcError> {
        let value = match (&self, &that) {
            (&Value::Float(n), &Value::Float(m)) => Value::Float(n.pow(m)),
            (&Value::Float(n), Value::Integral(m, _)) => {
                Value::Float(n.pow(ops::to_float(m)?))
            }
            (Value::Integral(n, _), &Value::Float(m)) => {
                Value::Float(ops::to_float(n)?.pow(m))
            }
            (&Value::Integral(ref n, t1), &Value::Integral(ref m, t2)) => {
                match ops::int_pow(n, m) {
                    Some(v) => Value::Integral(v, t1 + t2),
                    None => {
                        return Err(CalcError::WouldOverflow(
                            PartialComp::binary("**", &self, &that),
                        ))
                    }
                }
            }
        };
        Ok(value)
    }

    /// Computes the absolute value of a number
    pub fn abs(&self) -> Self {
        use num::Signed;
        match self {
            Value::Float(n) => Value::Float(n.abs()),
            Value::Integral(n, t) => Value::Integral(Signed::abs(n), *t),
        }
    }

    /// Computes the natural logarithm of a number
    pub fn ln(&self) -> Result<Self, CalcError> {
        match self {
            Value::Float(n) => Ok(Value::Float(n.ln())),
            Value::Integral(_, _) => Ok(Value::Float(self.as_float()?.ln())),
        }
    }

    /// Computes the logarithm base 10 of a number
    pub fn log(&self) -> Result<Self, CalcError> {
        match self {
            Value::Float(n) => Ok(Value::Float(n.log10())),
            Value::Integral(_, _) => Ok(Value::Float(self.as_float()?.log10())),
        }
    }

    /// Computes the sine of a number in radians
    pub fn sin(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.sin().try_into()
    }

    /// Computes the cosine of a number in radians
    pub fn cos(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.cos().try_into()
    }

    /// Computes the tangent of a number in radians
    pub fn tan(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.tan().try_into()
    }

    /// Computes the arc sine of a number in radians
    pub fn asin(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.asin().try_into()
    }

    /// Computes the arc cosine of a number in radians
    pub fn acos(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.acos().try_into()
    }

    /// Computes the arc tanget of a number in radians
    pub fn atan(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.atan().try_into()
    }

    /// Computes the hyperbolic sine of a number in radians
    pub fn sinh(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.sinh().try_into()
    }

    /// Computes the hyperbolic cosine of a number in radians
    pub fn cosh(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.cosh().try_into()
    }

    /// Computes the hyperbolic tangent of a number in radians
    pub fn tanh(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.tanh().try_into()
    }

    /// Computes the inverse hyperbolic sine of a number in radians
    pub fn asinh(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.asinh().try_into()
    }

    /// Computes the inverse hyperbolic cosine of a number in radians
    pub fn acosh(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.acosh().try_into()
    }

    /// Computes the inverse hyperbolic tanget of a number in radians
    pub fn atanh(&self) -> Result<Self, CalcError> {
        let float_val: f64 = self.try_into()?;
        float_val.atanh().try_into()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Integral(ref n, IntegralFmt::Dec) => write!(f, "{}", n),
            Value::Integral(ref n, IntegralFmt::Hex) => write!(f, "0x{:X}", n),
            Value::Float(ref n) => write!(f, "{}", n),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Float(d128!(0.0))
    }
}

impl TryFrom<&Value> for f64 {
    type Error = CalcError;

    fn try_from(v: &Value) -> Result<f64, CalcError> {
        match v {
            Value::Float(n) => n
                .to_string()
                .parse::<f64>()
                .map_err(|err| CalcError::InvalidNumber(err.to_string())),
            Value::Integral(n, _) => {
                n.to_f64().ok_or(CalcError::InvalidNumber(n.to_string()))
            }
        }
    }
}

impl TryFrom<f64> for Value {
    type Error = CalcError;

    fn try_from(v: f64) -> Result<Value, CalcError> {
        let n = d128::from_str(&v.to_string())
            .map_err(|err| CalcError::InvalidNumber(format!("{:?}", err)))?;
        Ok(Value::Float(n))
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
    type Output = Result<Self, CalcError>;

    fn add(self, that: Value) -> Self::Output {
        self.castmap(that, |x, y| x + y, |x, y| x + y)
    }
}

impl Sub for Value {
    type Output = Result<Self, CalcError>;

    fn sub(self, that: Value) -> Self::Output {
        self.castmap(that, |x, y| x - y, |x, y| x - y)
    }
}

impl Mul for Value {
    type Output = Result<Self, CalcError>;

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
                Value::Float(n / ops::to_float(&m)?)
            }
            (Value::Integral(n, _), Value::Float(m)) => {
                Value::Float(ops::to_float(&n)? / m)
            }
            (Value::Integral(ref n, t1), Value::Integral(ref m, t2)) => {
                if (n % m).is_zero() {
                    Value::Integral(n / m, t1 + t2)
                } else {
                    Value::Float(ops::to_float(n)? / ops::to_float(m)?)
                }
            }
        };
        Ok(value)
    }
}

impl BitAnd for Value {
    type Output = Result<Self, CalcError>;

    fn bitand(self, that: Value) -> Self::Output {
        self.intmap(&that, "&", |n, m| Ok(ops::and(n, m)))
    }
}

impl BitOr for Value {
    type Output = Result<Self, CalcError>;

    fn bitor(self, that: Value) -> Self::Output {
        self.intmap(&that, "|", |n, m| Ok(ops::or(n, m)))
    }
}

impl BitXor for Value {
    type Output = Result<Self, CalcError>;

    fn bitxor(self, that: Value) -> Self::Output {
        self.intmap(&that, "^", |n, m| Ok(ops::xor(n, m)))
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
            Value::Integral(n, t) => Ok(Value::Integral(ops::not(n), t)),
            Value::Float(f) => {
                Err(CalcError::BadTypes(PartialComp::unary("~", f)))
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
        self.castmap(that, |x, y| x % y, |x, y| x % y)
    }
}

impl Shl<Value> for Value {
    type Output = Result<Self, CalcError>;

    fn shl(self, that: Value) -> Self::Output {
        self.intmap(&that, "<<", |n, m| {
            m.to_i64()
                .map(|m| {
                    if m < 0 {
                        n >> ((-m) as usize)
                    } else {
                        n << (m as usize)
                    }
                })
                .ok_or(CalcError::WouldOverflow(PartialComp::binary(
                    "<<", &self, &that,
                )))
        })
    }
}

impl Shr<Value> for Value {
    type Output = Result<Self, CalcError>;

    fn shr(self, that: Value) -> Self::Output {
        self.intmap(&that, "<<", |n, m| {
            m.to_i64()
                .map(|m| {
                    if m < 0 {
                        n << ((-m) as usize)
                    } else {
                        n >> (m as usize)
                    }
                })
                .ok_or(CalcError::WouldOverflow(PartialComp::binary(
                    ">>", &self, &that,
                )))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn float_override() {
        let cases = vec![
            (
                (Value::Float(d128!(3)) + Value::dec(1)).unwrap(),
                Value::Float(d128!(4)),
            ),
            (
                (Value::hex(5) - Value::Float(d128!(4.5))).unwrap(),
                Value::Float(d128!(0.5)),
            ),
            (
                ((Value::hex(24) * Value::dec(4)).unwrap()
                    * Value::Float(d128!(1) / d128!(48)))
                .unwrap(),
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
            ((Value::hex(3) * Value::dec(-2)).unwrap(), Value::hex(-6)),
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
    fn try_from_f64() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Float(d128!(0.0)),
                (0.0).try_into().unwrap(),
            ),
            (
                Value::Float(d128!(0.0000001)),
                (0.0000001).try_into().unwrap(),
            ),
            (
                Value::Float(d128!(-0.0000001)),
                (-0.0000001).try_into().unwrap(),
            ),
            (
                Value::Float(d128!(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001)),
                (-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001).try_into().unwrap(),
            ),
            (
                Value::Float(d128!(179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)),
                (f64::MAX).try_into().unwrap(),
            ),
            (
                Value::Float(d128!(-179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)),
                (f64::MIN).try_into().unwrap(),
            ),
        ];

        for (output, expected) in cases {
            assert_eq!(output, expected);
        }
    }

    #[test]
    fn try_into_f64() {
        let cases: Vec<(f64, f64)> = vec![
            (
                (&Value::Integral(BigInt::from(0), IntegralFmt::Dec)).try_into().unwrap(),
                0.0f64,
            ),
            (
                (&Value::Integral(BigInt::from(1), IntegralFmt::Dec)).try_into().unwrap(),
                1.0f64,
            ),
            (
                (&Value::Integral(BigInt::from(-1), IntegralFmt::Dec)).try_into().unwrap(),
                -1.0f64,
            ),
            (
                (&Value::Float(d128!(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))).try_into().unwrap(),
                -0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001f64,
            ),
            (
                (&Value::Float(d128!(179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))).try_into().unwrap(),
                f64::MAX,
            ),
            (
                (&Value::Float(d128!(-179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))).try_into().unwrap(),
                f64::MIN,
            ),
            (
                (&Value::Float(d128!(0.0))).try_into().unwrap(),
                0.0,
            ),
            (
                (&Value::Float(d128!(0.0000001))).try_into().unwrap(),
                0.0000001,
            ),
            (
                (&Value::Float(d128!(-0.0000001))).try_into().unwrap(),
                -0.0000001,
            ),
            (
                (&Value::Float(d128!(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))).try_into().unwrap(),
                -0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001f64,
            ),
            (
                (&Value::Float(d128!(179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))).try_into().unwrap(),
                f64::MAX,
            ),
            (
                (&Value::Float(d128!(-179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))).try_into().unwrap(),
                f64::MIN,
            ),
        ];

        for (output, expected) in cases {
            assert_eq!(output, expected);
        }
    }

    #[test]
    fn function_abs() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec).abs(),
                Value::Integral(BigInt::from(0), IntegralFmt::Dec),
            ),
            (
                Value::Integral(BigInt::from(1), IntegralFmt::Dec).abs(),
                Value::Integral(BigInt::from(1), IntegralFmt::Dec),
            ),
            (
                Value::Integral(BigInt::from(-1), IntegralFmt::Dec).abs(),
                Value::Integral(BigInt::from(1), IntegralFmt::Dec),
            ),
            (Value::Float(d128!(0)).abs(), Value::Float(d128!(0))),
            (Value::Float(d128!(1.0)).abs(), Value::Float(d128!(1.0))),
            (Value::Float(d128!(0.1)).abs(), Value::Float(d128!(0.1))),
            (Value::Float(d128!(-0.0)).abs(), Value::Float(d128!(0.0))),
            (Value::Float(d128!(-1.0)).abs(), Value::Float(d128!(1.0))),
            (Value::Float(d128!(-0.1)).abs(), Value::Float(d128!(0.1))),
        ];

        for (output, expected) in cases {
            assert_eq!(output, expected);
        }
    }

    #[test]
    fn function_log() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .log()
                    .unwrap(),
                Value::Float(d128::neg_infinity()),
            ),
            (
                Value::Integral(BigInt::from(1), IntegralFmt::Dec)
                    .log()
                    .unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Integral(BigInt::from(10), IntegralFmt::Dec)
                    .log()
                    .unwrap(),
                Value::Float(d128!(1)),
            ),
            (
                Value::Integral(BigInt::from(100), IntegralFmt::Dec)
                    .log()
                    .unwrap(),
                Value::Float(d128!(2)),
            ),
            (
                Value::Float(d128!(0.0)).log().unwrap(),
                Value::Float(d128::neg_infinity()),
            ),
            (
                Value::Float(d128!(-0.0)).log().unwrap(),
                Value::Float(d128::neg_infinity()),
            ),
            (
                Value::Float(d128!(0.1)).log().unwrap(),
                Value::Float(d128!(-1.0)),
            ),
            (
                Value::Float(d128!(1.0)).log().unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(10.0)).log().unwrap(),
                Value::Float(d128!(1.0)),
            ),
            (
                Value::Float(d128!(1000.0)).log().unwrap(),
                Value::Float(d128!(3.0)),
            ),
        ];

        for (output, expected) in cases {
            assert_eq!(output, expected);
        }

        assert!(Value::Integral(BigInt::from(-1), IntegralFmt::Dec)
            .log()
            .unwrap()
            .is_nan());
        assert!(Value::Float(d128!(-0.1)).log().unwrap().is_nan());
        assert!(Value::Float(d128!(-1.0)).log().unwrap().is_nan());
    }

    #[test]
    fn function_ln() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .ln()
                    .unwrap(),
                Value::Float(d128::neg_infinity()),
            ),
            (
                Value::Integral(BigInt::from(1), IntegralFmt::Dec)
                    .ln()
                    .unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Integral(BigInt::from(2), IntegralFmt::Dec)
                    .ln()
                    .unwrap(),
                Value::Float(d128!(0.6931471805599453094172321214581766)),
            ),
            (
                Value::Integral(BigInt::from(7), IntegralFmt::Dec)
                    .ln()
                    .unwrap(),
                Value::Float(d128!(1.945910149055313305105352743443180)),
            ),
            (
                Value::Float(d128!(0.0)).ln().unwrap(),
                Value::Float(d128::neg_infinity()),
            ),
            (
                Value::Float(d128!(-0.0)).ln().unwrap(),
                Value::Float(d128::neg_infinity()),
            ),
            (
                Value::Float(d128!(0.36787944117144232159552377016146086))
                    .ln()
                    .unwrap(), // 1/e
                Value::Float(d128!(-0.9999999999999999999999999999999999)),
            ),
            (
                Value::Float(d128!(1.0)).ln().unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(2.71828182845904523536028747135266249))
                    .ln()
                    .unwrap(), // e
                Value::Float(d128!(0.9999999999999999999999999999999998)),
            ),
            (
                Value::Float(d128!(7.38905609893065022723042746057500781))
                    .ln()
                    .unwrap(), // e²
                Value::Float(d128!(2.0)),
            ),
        ];

        for (output, expected) in cases {
            assert_eq!(output, expected);
        }

        assert!(Value::Integral(BigInt::from(-1), IntegralFmt::Dec)
            .ln()
            .unwrap()
            .is_nan());
        assert!(Value::Float(d128!(-0.1)).ln().unwrap().is_nan());
        assert!(Value::Float(d128!(-1.0)).ln().unwrap().is_nan());
    }

    #[test]
    fn function_sin() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .sin()
                    .unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(0)).sin().unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .sin()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.5)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .sin()
                    .unwrap(), // pi/4
                Value::Float(d128!(0.7071067811865475)),
            ),
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .sin()
                    .unwrap(), // pi/3
                Value::Float(d128!(0.8660254037844387)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .sin()
                    .unwrap(), // pi/2
                Value::Float(d128!(1.0)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .sin()
                    .unwrap(), // pi
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .sin()
                    .unwrap(), // 2*pi
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .sin()
                    .unwrap(), // -pi/4
                Value::Float(d128!(-0.7071067811865475)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }
    }

    #[test]
    fn function_cos() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .cos()
                    .unwrap(),
                Value::Float(d128!(1.0)),
            ),
            (
                Value::Float(d128!(0)).cos().unwrap(),
                Value::Float(d128!(1)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .cos()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.8660254037844387)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .cos()
                    .unwrap(), // pi/4
                Value::Float(d128!(0.7071067811865475)),
            ),
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .cos()
                    .unwrap(), // pi/3
                Value::Float(d128!(0.5)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .cos()
                    .unwrap(), // pi/2
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .cos()
                    .unwrap(), // pi
                Value::Float(d128!(-1.0)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .cos()
                    .unwrap(), // 2*pi
                Value::Float(d128!(1.0)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .cos()
                    .unwrap(), // -pi/4
                Value::Float(d128!(0.7071067811865475)),
            ),
        ];

        for (output, expected) in cases {
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }
    }

    #[test]
    fn function_tan() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .tan()
                    .unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(0)).tan().unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .tan()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.5773502691896257)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .tan()
                    .unwrap(), // pi/4
                Value::Float(d128!(1.0)),
            ),
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .tan()
                    .unwrap(), // pi/3
                Value::Float(d128!(1.73205080756887729)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .tan()
                    .unwrap(), // pi/2
                Value::Float(d128!(16331239353195370)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .tan()
                    .unwrap(), // pi
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .tan()
                    .unwrap(), // 2*pi
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .tan()
                    .unwrap(), // -pi/4
                Value::Float(d128!(-1.0)),
            ),
        ];

        for (output, expected) in cases {
            print!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            println!(", verification: {}", &verification);
            assert!(verification);
        }
    }

    #[test]
    fn function_asin() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .asin()
                    .unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(0)).asin().unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .asin()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.5510695830994464)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .asin()
                    .unwrap(), // pi/4
                Value::Float(d128!(0.9033391107665127)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .asin()
                    .unwrap(), // -pi/4
                Value::Float(d128!(-0.9033391107665127)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }

        assert!(Value::Float(d128!(1.04719755119659774615421446109316762))
            .asin()
            .unwrap()
            .is_nan()); // pi/3
        assert!(Value::Float(d128!(1.57079632679489661923132169163975144))
            .asin()
            .unwrap()
            .is_nan()); // pi/2
        assert!(Value::Float(d128!(3.14159265358979323846264338327950288))
            .asin()
            .unwrap()
            .is_nan()); // pi
        assert!(Value::Float(d128!(6.28318530717958647692528676655900576))
            .asin()
            .unwrap()
            .is_nan()); // 2*pi
    }

    #[test]
    fn function_acos() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .acos()
                    .unwrap(),
                Value::Float(d128!(1.5707963267948966)),
            ),
            (
                Value::Float(d128!(0)).acos().unwrap(),
                Value::Float(d128!(1.5707963267948966)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .acos()
                    .unwrap(), // pi/6
                Value::Float(d128!(1.0197267436954500)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .acos()
                    .unwrap(), // pi/4
                Value::Float(d128!(0.6674572160283840)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .acos()
                    .unwrap(), // -pi/4
                Value::Float(d128!(2.4741354375614100)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }

        assert!(Value::Float(d128!(1.04719755119659774615421446109316762))
            .acos()
            .unwrap()
            .is_nan()); // pi/3
        assert!(Value::Float(d128!(1.57079632679489661923132169163975144))
            .acos()
            .unwrap()
            .is_nan()); // pi/2
        assert!(Value::Float(d128!(3.14159265358979323846264338327950288))
            .acos()
            .unwrap()
            .is_nan()); // pi
        assert!(Value::Float(d128!(6.28318530717958647692528676655900576))
            .acos()
            .unwrap()
            .is_nan()); // 2*pi
    }

    #[test]
    fn function_atan() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .atan()
                    .unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(0)).atan().unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .atan()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.4823479071010250)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .atan()
                    .unwrap(), // pi/4
                Value::Float(d128!(0.6657737500283540)),
            ),
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .atan()
                    .unwrap(), // pi/3
                Value::Float(d128!(0.8084487926300220)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .atan()
                    .unwrap(), // pi/2
                Value::Float(d128!(1.0038848218538872)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .atan()
                    .unwrap(), // pi
                Value::Float(d128!(1.2626272556789118)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .atan()
                    .unwrap(), // 2*pi
                Value::Float(d128!(1.4129651365067377)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .atan()
                    .unwrap(), // -pi/4
                Value::Float(d128!(-0.6657737500283540)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }
    }

    #[test]
    fn function_sinh() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .sinh()
                    .unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(0)).sinh().unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .sinh()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.5478534738880398)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .sinh()
                    .unwrap(), // pi/4
                Value::Float(d128!(0.8686709614860096)),
            ),
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .sinh()
                    .unwrap(), // pi/3
                Value::Float(d128!(1.2493670505239753)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .sinh()
                    .unwrap(), // pi/2
                Value::Float(d128!(2.3012989023072949)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .sinh()
                    .unwrap(), // pi
                Value::Float(d128!(11.5487393572577484)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .sinh()
                    .unwrap(), // 2*pi
                Value::Float(d128!(267.74489404101644)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .sinh()
                    .unwrap(), // -pi/4
                Value::Float(d128!(-0.8686709614860096)),
            ),
        ];

        for (output, expected) in cases {
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }
    }

    #[test]
    fn function_cosh() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .cosh()
                    .unwrap(),
                Value::Float(d128!(1.0)),
            ),
            (
                Value::Float(d128!(0)).cosh().unwrap(),
                Value::Float(d128!(1)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .cosh()
                    .unwrap(), // pi/6
                Value::Float(d128!(1.1402383210764289)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .cosh()
                    .unwrap(), // pi/4
                Value::Float(d128!(1.3246090892520057)),
            ),
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .cosh()
                    .unwrap(), // pi/3
                Value::Float(d128!(1.6002868577023863)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .cosh()
                    .unwrap(), // pi/2
                Value::Float(d128!(2.5091784786580567)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .cosh()
                    .unwrap(), // pi
                Value::Float(d128!(11.591953275521519)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .cosh()
                    .unwrap(), // 2*pi
                Value::Float(d128!(267.7467614837482)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .cosh()
                    .unwrap(), // -pi/4
                Value::Float(d128!(1.3246090892520057)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }
    }

    #[test]
    fn function_tanh() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .tanh()
                    .unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(0)).tanh().unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .tanh()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.4804727781564516)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .tanh()
                    .unwrap(), // pi/4
                Value::Float(d128!(0.6557942026326724)),
            ),
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .tanh()
                    .unwrap(), // pi/3
                Value::Float(d128!(0.7807144353592678)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .tanh()
                    .unwrap(), // pi/2
                Value::Float(d128!(0.9171523356672744)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .tanh()
                    .unwrap(), // pi
                Value::Float(d128!(0.99627207622075)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .tanh()
                    .unwrap(), // 2*pi
                Value::Float(d128!(0.9999930253396107)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .tanh()
                    .unwrap(), // -pi/4
                Value::Float(d128!(-0.6557942026326724)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }
    }

    #[test]
    fn function_asinh() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .asinh()
                    .unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(0)).asinh().unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .asinh()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.5022189850346117)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .asinh()
                    .unwrap(), // pi/4
                Value::Float(d128!(0.7212254887267798)),
            ),
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .asinh()
                    .unwrap(), // pi/3
                Value::Float(d128!(0.9143566553928861)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .asinh()
                    .unwrap(), // pi/2
                Value::Float(d128!(1.233403117511217)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .asinh()
                    .unwrap(), // pi
                Value::Float(d128!(1.8622957433108482)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .asinh()
                    .unwrap(), // 2*pi
                Value::Float(d128!(2.537297501373361)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .asinh()
                    .unwrap(), // -pi/4
                Value::Float(d128!(-0.7212254887267798)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }
    }

    #[test]
    fn function_acosh() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Float(d128!(1.047197551196597746154214461093167628))
                    .acosh()
                    .unwrap(), // pi/3
                Value::Float(d128!(0.30604210861326614)),
            ),
            (
                Value::Float(d128!(1.57079632679489661923132169163975144))
                    .acosh()
                    .unwrap(), // pi/2
                Value::Float(d128!(1.0232274785475506)),
            ),
            (
                Value::Float(d128!(3.14159265358979323846264338327950288))
                    .acosh()
                    .unwrap(), // pi
                Value::Float(d128!(1.8115262724608532)),
            ),
            (
                Value::Float(d128!(6.28318530717958647692528676655900576))
                    .acosh()
                    .unwrap(), // 2*pi
                Value::Float(d128!(2.524630659933467)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }

        assert!(Value::Integral(BigInt::from(0), IntegralFmt::Dec)
            .acosh()
            .unwrap()
            .is_nan());
        assert!(Value::Float(d128!(0.0)).acosh().unwrap().is_nan());
        assert!(Value::Float(d128!(0.52359877559829887307710723054658381))
            .acosh()
            .unwrap()
            .is_nan()); // pi/6
        assert!(Value::Float(d128!(0.785398163397448309615660845819875721))
            .acosh()
            .unwrap()
            .is_nan()); // pi/4
        assert!(Value::Float(d128!(-0.785398163397448309615660845819875721))
            .acosh()
            .unwrap()
            .is_nan()); // -pi/4
    }

    #[test]
    fn function_atanh() {
        let cases: Vec<(Value, Value)> = vec![
            (
                Value::Integral(BigInt::from(0), IntegralFmt::Dec)
                    .atanh()
                    .unwrap(),
                Value::Float(d128!(0.0)),
            ),
            (
                Value::Float(d128!(0)).atanh().unwrap(),
                Value::Float(d128!(0)),
            ),
            (
                Value::Float(d128!(0.52359877559829887307710723054658381))
                    .atanh()
                    .unwrap(), // pi/6
                Value::Float(d128!(0.5812850116947232)),
            ),
            (
                Value::Float(d128!(0.785398163397448309615660845819875721))
                    .atanh()
                    .unwrap(), // pi/4
                Value::Float(d128!(1.0593061708232432)),
            ),
            (
                Value::Float(d128!(-0.785398163397448309615660845819875721))
                    .atanh()
                    .unwrap(), // -pi/4
                Value::Float(d128!(-1.0593061708232432)),
            ),
        ];

        for (output, expected) in cases {
            println!("output: {}, expected: {}", &output, &expected);
            let diff: Value = (output - expected).unwrap().abs();
            let delta: Value = Value::Float(d128!(1e-15));
            let verification =
                diff.as_float().unwrap() < delta.as_float().unwrap();
            assert!(verification);
        }

        assert!(Value::Float(d128!(1.047197551196597746154214461093167628))
            .atanh()
            .unwrap()
            .is_nan()); // pi/6
        assert!(Value::Float(d128!(1.57079632679489661923132169163975144))
            .atanh()
            .unwrap()
            .is_nan()); // pi/6
        assert!(Value::Float(d128!(3.14159265358979323846264338327950288))
            .atanh()
            .unwrap()
            .is_nan()); // pi/6
        assert!(Value::Float(d128!(6.28318530717958647692528676655900576))
            .atanh()
            .unwrap()
            .is_nan()); // pi/6
    }
}

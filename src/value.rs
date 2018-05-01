use num::{Zero, BigInt, BigUint, ToPrimitive};
use decimal::d128;
use error::{CalcError, PartialComp};
use std::fmt;
use std::ops::*;

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
    use decimal::d128;
    use std::iter::repeat;
    use num::{Zero, Integer, ToPrimitive};
    use super::{UIntegral, Integral, CalcError, PartialComp};

    macro_rules! bitwise_op {
        ($name:ident, $fun:expr) => {
            pub fn $name(n: &Integral, m: &Integral) -> Integral {
                bitwise(n, m, $fun)
            }
        }
    }

    fn equalize(left: &mut Vec<u8>, right: &mut Vec<u8>) {
        if left.len() > right.len() {
            for it in repeat(Zero::zero()).take(left.len() - right.len()) {
                right.push(it);
            }
        } else if right.len() > left.len() {
            for it in repeat(Zero::zero()).take(right.len() - left.len()) {
                left.push(it);
            }
        }
    }

    pub fn bitwise<F: Fn(u8, u8) -> u8>(n: &Integral, m: &Integral, fun: F) -> Integral {
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
        let res: Vec<u8> = bytes
            .iter()
            .map(|n| !n)
            .collect();
        Integral::from_signed_bytes_le(&res)
    }

    pub fn int_powu(n: &Integral, m: &UIntegral) -> Integral {
        if m.is_zero() {
            1.into()
        } else if m.is_even() {
            int_powu(&(n * n), &(m / (2 as u8)))
        } else {
            n * int_powu(&(n * n), &((m - (1 as u8)) / (2 as u8)))
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
        n.to_i64()
         .map(Into::into)
         .ok_or(CalcError::WouldTruncate(PartialComp::ToFloat(n.to_string())))
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
            },
            (v1 @ Value::Float(_), v2 @ _) | (v1 @ _, v2 @ Value::Float(_)) => {
                Err(CalcError::BadTypes(PartialComp::binary(
                    op, v1, v2,
                )))
            }
        }
    }

    /// Represents a computation that will cast integer types to floating
    /// point. There might be a possible truncation when we convert a BigInt into a floating point,
    /// so we have to be careful here.
    pub fn castmap<F, G>(self, that: Value, f: F, g: G) -> Result<Value, CalcError>
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
            (&Value::Float(n), &Value::Integral(ref m, _)) => {
                Value::Float(n.pow(ops::to_float(m)?))
            }
            (&Value::Integral(ref n, _), &Value::Float(m)) => {
                Value::Float(ops::to_float(n)?.pow(m))
            }
            (&Value::Integral(ref n, t1), &Value::Integral(ref m, t2)) => {
                match ops::int_pow(&n, &m) {
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
                    Value::Float(ops::to_float(&n)? / ops::to_float(&m)?)
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
        self.castmap(that, |x, y| x % y, |x, y| x % y)
    }
}

impl Shl<Value> for Value {
    type Output = Result<Self, CalcError>;

    fn shl(self, that: Value) -> Self::Output {
        self.intmap(
            &that,
            "<<",
            |n, m| {
                m.to_i64()
                 .map(|m| {
                     if m < 0 {
                        n >> ((-m) as usize)
                     } else {
                        n << (m as usize)
                     }
                 })
                 .ok_or(CalcError::WouldOverflow(PartialComp::binary("<<", &self, &that)))
            }
        )
    }
}

impl Shr<Value> for Value {
    type Output = Result<Self, CalcError>;

    fn shr(self, that: Value) -> Self::Output {
        self.intmap(
            &that,
            "<<",
            |n, m| {
                m.to_i64()
                 .map(|m| {
                     if m < 0 {
                        n << ((-m) as usize)
                     } else {
                        n >> (m as usize)
                     }
                 })
                 .ok_or(
                     CalcError::WouldOverflow(PartialComp::binary(">>", &self, &that))
                 )
            }
        )
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
                    * Value::Float(d128!(1) / d128!(48))).unwrap(),
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

}

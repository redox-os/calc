use num::{Zero, BigInt, BigUint, ToPrimitive};
use decimal::d128;
use error::{CalcError, PartialComp};
use std::fmt;
use std::ops::*;

pub type Integral = BigInt;
pub type UIntegral = BigUint;

/// Represents a canonical value that can be calculated by this library
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// An integral value in decimal form. This is generally interoperable
    /// with the `Hex` constructor, but will be overriden by the `Hex`
    /// constructor.
    Dec(Integral),
    /// An integral value in hexadecimal form. This takes precedence over
    /// the `Dec` constructor.
    Hex(Integral),
    /// A 128-bit decimal floating point number
    Float(d128),
}

pub mod ops {
    use std::iter::repeat;
    use num::{Zero, Integer};
    use super::{Value, UIntegral, Integral};

    macro_rules! bitwise_op {
        ($name:ident, $fun:expr) => {
            pub fn $name(n: Integral, m: Integral) -> Integral {
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

    pub fn bitwise<F: Fn(u8, u8) -> u8>(n: Integral, m: Integral, fun: F) -> Integral {
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
        if *m >= 0.into() {
            Some(int_powu(n, &m.to_biguint().unwrap()))
        } else {
            None
        }
    }

}

impl Value {

    pub fn dec<T: Into<Integral>>(n: T) -> Self {
        Value::Dec(n.into())
    }

    pub fn hex<T: Into<Integral>>(n: T) -> Self {
        Value::Hex(n.into())
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Value::Dec(n) | Value::Hex(n) => n.is_zero(),
            Value::Float(f) => *f == d128!(0),
        }
    }

    pub fn as_float(&self) -> Result<d128, CalcError> {
        match self {
            Value::Dec(n) | Value::Hex(n) => {
                match n.to_i64() {
                    Some(n) => Ok(n.into()),
                    None => Err(
                        CalcError::WouldOverflow(PartialComp::unary("to float", self))
                    )
                }
            }
            Value::Float(n) => Ok(*n),
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
        F: Fn(Integral, Integral) -> Integral,
        T: ToString,
    {
        match (self, that) {
            (Value::Hex(n), Value::Hex(m))
            | (Value::Hex(n), Value::Dec(m))
            | (Value::Dec(n), Value::Hex(m)) => Ok(Value::Hex(f(n, m))),
            (Value::Dec(n), Value::Dec(m)) => Ok(Value::Dec(f(n, m))),
            (v1 @ Value::Float(_), v2 @ _) | (v1 @ _, v2 @ Value::Float(_)) => {
                Err(CalcError::BadTypes(PartialComp::binary(
                    op,
                    v1,
                    v2,
                )))
            }
        }
    }

    /// Represents a computation that will cast integer types to floating
    /// point
    pub fn castmap<F, G>(self, that: Value, f: F, g: G) -> Value
    where
        F: Fn(Integral, Integral) -> Integral,
        G: Fn(d128, d128) -> d128,
    {
        match (self, that) {
            (Value::Float(n), Value::Float(m)) => Value::Float(g(n, m)),
            (Value::Float(n), Value::Hex(m))
            | (Value::Float(n), Value::Dec(m)) => {
                Value::Float(g(n, into_float(m)))
            }
            (Value::Hex(n), Value::Float(m))
            | (Value::Dec(n), Value::Float(m)) => {
                Value::Float(g(into_float(n), m))
            }
            (Value::Hex(n), Value::Hex(m))
            | (Value::Dec(n), Value::Hex(m))
            | (Value::Hex(n), Value::Dec(m)) => Value::Hex(f(n, m)),
            (Value::Dec(n), Value::Dec(m)) => Value::Dec(f(n, m)),
        }
    }

    pub fn pow(&self, that: &Value) -> Result<Self, CalcError> {
        let value =
            match (self, that) {
                (&Value::Float(n), &Value::Float(m)) => Value::Float(n.pow(m)),
                (&Value::Float(ref n), &Value::Hex(ref m))
                | (&Value::Float(ref n), &Value::Dec(ref m)) => {
                    match m.to_i64() {
                        Some(_m) => unimplemented!(),
                        None => return Err(
                            CalcError::WouldTruncate(
                                PartialComp::binary("**", self, that)
                            )
                        )
                    }
                }
                (&Value::Hex(ref n), &Value::Float(ref m))
                | (&Value::Dec(ref n), &Value::Float(ref m)) => {
                    Value::Float(into_float(n).pow(m))
                }
                (&Value::Hex(ref n), &Value::Hex(ref m))
                | (&Value::Dec(ref n), &Value::Hex(ref m))
                | (&Value::Hex(ref n), &Value::Dec(ref m)) => {
                    match ops::int_pow(n, m) {
                        Some(v) => Value::hex(v),
                        None => {
                            return Err(CalcError::WouldOverflow(
                                PartialComp::binary("**", self, that),
                            ))
                        }
                    }
                }
                (&Value::Dec(ref n), &Value::Dec(ref m)) => {
                    match ops::int_pow(n, m) {
                        Some(v) => Value::dec(v),
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

}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Dec(n) => write!(f, "{}", n),
            Value::Hex(n) => write!(f, "0x{:X}", n),
            Value::Float(n) => write!(f, "{}", n),
        }
    }
}

// pub fn into_float<INT: Into<d128>>(dec: INT) -> d128 {
//     dec.into()
// }

pub fn into_float<T>(dec: T) -> d128 {
    unimplemented!()
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
            (Value::Float(n), Value::Hex(m))
            | (Value::Float(n), Value::Dec(m)) => {
                Value::Float(n / into_float(m))
            }
            (Value::Hex(n), Value::Float(m))
            | (Value::Dec(n), Value::Float(m)) => {
                Value::Float(into_float(n) / m)
            }
            (Value::Hex(ref n), Value::Hex(ref m))
            | (Value::Dec(ref n), Value::Hex(ref m))
            | (Value::Hex(ref n), Value::Dec(ref m)) => {
                if (n % m).is_zero() {
                    Value::Hex(n / m)
                } else {
                    Value::Float(into_float(n) / into_float(m))
                }
            }
            (Value::Dec(ref n), Value::Dec(ref m)) => {
                if (n % m).is_zero() {
                    Value::Dec(n / m)
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
        self.intmap(that, "&", |n, m| ops::and(n, m))
    }
}

impl BitOr for Value {
    type Output = Result<Self, CalcError>;

    fn bitor(self, that: Value) -> Self::Output {
        self.intmap(that, "|", |n, m| ops::or(n, m))
    }
}

impl BitXor for Value {
    type Output = Result<Self, CalcError>;

    fn bitxor(self, that: Value) -> Self::Output {
        self.intmap(that, "^", |n, m| ops::xor(n, m))
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
            Value::Hex(n) => Ok(Value::Hex(ops::not(n))),
            Value::Dec(n) => Ok(Value::Dec(ops::not(n))),
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
        self.intmap(
            that,
            "<<",
            |n, m| {
                if m < Zero::zero() {
                    n / ops::int_powu(&(2 as u8).into(), &(-m).to_biguint().unwrap())
                } else {
                    n * ops::int_powu(&(2 as u8).into(), &m.to_biguint().unwrap())
                }
            }
        )
    }
}

impl Shr<Value> for Value {
    type Output = Result<Self, CalcError>;

    fn shr(self, that: Value) -> Self::Output {
        self.intmap(that, "<<", |n, m| n / ops::int_powu(&m, &(2 as u8).into()))
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

}

use super::{Digits, FromFragment, UnsignedDecimal};
use num::{BigInt, BigUint};
use std::time::Duration;

use Error::*;

pub type Parent = UnsignedDecimal;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Error {
    Overflow,
    Underflow,
}

pub fn from_arg(arg: Parent) -> Result<Duration, Error> {
    use UnsignedDecimal::*;

    match arg {
        Zero => Ok(from_zero()),
        NonZero { base, exponent } => from_non_zero(base, exponent),
    }
}

fn from_zero() -> Duration {
    Duration::default()
}

fn from_non_zero(base: Digits, exponent: BigInt) -> Result<Duration, Error> {
    let (direction, exponent) = exponent.into_parts();
    if direction == ::num::bigint::Sign::Minus {
        from_negative_exponent(base, exponent)
    } else {
        from_non_negative_exponent(base, exponent).ok_or(Overflow)
    }
}

fn from_negative_exponent(base: Digits, exponent: BigUint) -> Result<Duration, Error> {
    const PRECISION: usize = 9;

    let base = base.into_inner();
    let exponent = usize::try_from(exponent).map_err(|_| Underflow)?;
    let radix = base.len().saturating_sub(exponent);
    let (whole, fraction) = base.split_at(radix);
    let fraction = u32::from_fragment(fraction).unwrap(); // `PRECISION` many digits should always fit into a `u32`
    let trailing = PRECISION.checked_sub(exponent).ok_or(Underflow)?;
    let exponent = u32::try_from(trailing).unwrap(); // `trailing` should always be ≤ `PRECISION`
    let scalar = 10u32.pow(exponent);
    let nanos = fraction * scalar;
    let whole = u64::from_fragment(whole).ok_or(Overflow)?;
    let secs = whole;
    Ok(Duration::new(secs, nanos))
}

fn from_non_negative_exponent(base: Digits, exponent: BigUint) -> Option<Duration> {
    let base = base.into_inner();
    let whole = u64::from_fragment(&base)?;
    let exponent = u32::try_from(exponent).ok()?;
    let scalar = u64::checked_pow(10, exponent)?;
    let secs = whole.checked_mul(scalar)?;
    Some(Duration::from_secs(secs))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::internal::parse;

    #[test]
    fn zero() {
        assert_eq!(Duration::new(0, 0), parse(["0"]).unwrap());
    }

    #[test]
    fn max() {
        assert_eq!(
            Duration::new(u64::MAX, 999999999),
            parse(["18446744073709551615.999999999"]).unwrap()
        );
    }
}

//! Utilities for parsing numeric types.
//!
//! If you're implement [`FromArgs`] for your own numeric types then you should check for underflow first and then check for overflow.

mod decimal;
mod duration;

use crate::internal::*;
use ::num::{BigInt, BigUint, Zero as _};
use std::{num, sync::atomic, time};

use Sign::*;

const TYPE_NAME: &str = "number";
const TYPE_ERROR: TypeError = TypeError(TYPE_NAME);

/// Sign, magnitude pair.
#[derive(Debug, Clone)]
pub struct Decimal {
    pub sign: Sign,
    pub magnitude: UnsignedDecimal,
}

/// Unsigned base, exponent pair.
///
/// If the variant is `NonZero` then the `base` is guaranteed to be non-empty.
#[derive(Debug, Clone)]
pub enum UnsignedDecimal {
    Zero,
    NonZero { base: Digits, exponent: BigInt },
}

/// Sign, non-negative magnitude pair.
#[derive(Debug, Clone)]
pub struct Integer {
    pub sign: Sign,
    pub magnitude: UnsignedInteger,
}

/// Unsigned base, non-negative exponent pair.
///
/// If the variant is `NonZero` then the `base` is guaranteed to be non-empty.
#[derive(Debug, Clone)]
pub enum UnsignedInteger {
    Zero,
    NonZero { base: Digits, exponent: BigUint },
}

/// Positive or negative sign (+, -).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign {
    Positive,
    Negative,
}

impl Sign {
    pub fn is_positive(self) -> bool {
        matches!(self, Positive)
    }

    pub fn is_negative(self) -> bool {
        matches!(self, Negative)
    }
}

/// Non-empty sequence of digits.
///
/// The inner digits are guaranteed to not have any leading or trailing zeros.
#[derive(Debug, Clone)]
pub struct Digits(Box<[Digit]>);

impl Digits {
    pub fn into_inner(self) -> Box<[Digit]> {
        let Self(inner) = self;

        inner
    }
}

/// Decimal digit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Digit {
    D0 = 0,
    D1 = 1,
    D2 = 2,
    D3 = 3,
    D4 = 4,
    D5 = 5,
    D6 = 6,
    D7 = 7,
    D8 = 8,
    D9 = 9,
}

impl FromArg for Decimal {
    type Parent = decimal::Parent;
    type Error = ();

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        decimal::from_arg(arg).ok_or(())
    }

    fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
        Box::new(TYPE_ERROR)
    }
}

impl FromArg for UnsignedDecimal {
    type Parent = Decimal;
    type Error = ();

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        use UnsignedDecimal::*;

        let Decimal { sign, magnitude } = arg;
        let (base, exponent) = match magnitude {
            Zero => return Ok(Zero),
            NonZero { base, exponent } => (base, exponent),
        };
        if sign.is_negative() {
            return Err(());
        }
        Ok(NonZero { base, exponent })
    }

    fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
        Box::new(IsNegativeError)
    }
}

impl FromArg for Integer {
    type Parent = Decimal;
    type Error = ();

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        let Decimal { sign, magnitude } = arg;
        let magnitude = match magnitude {
            UnsignedDecimal::Zero => UnsignedInteger::Zero,
            UnsignedDecimal::NonZero { base, exponent } => UnsignedInteger::NonZero {
                base,
                exponent: BigUint::try_from(exponent).map_err(|_| ())?,
            },
        };
        Ok(Self { sign, magnitude })
    }

    fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
        Box::new(IsDecimalError)
    }
}

impl FromArg for UnsignedInteger {
    type Parent = Integer;
    type Error = ();

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        use UnsignedInteger::*;

        let Integer { sign, magnitude } = arg;
        let (base, exponent) = match magnitude {
            Zero => return Ok(Zero),
            NonZero { base, exponent } => (base, exponent),
        };
        if sign.is_negative() {
            return Err(());
        }
        Ok(NonZero { base, exponent })
    }

    fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
        Box::new(IsNegativeError)
    }
}

/// Types that can be created from a slices of digits.
///
/// Returns `None` upon overflow.
pub trait FromFragment: Sized {
    fn from_fragment(fragment: &[Digit]) -> Option<Self>;
}

/// Types that can be created from a sign and a slice of digits.
///
/// Returns `Err(Positive)` upon positive overflow and `Err(Negative)` upon negative overflow.
pub trait FromSignedFragment: Sized {
    fn from_signed_fragment(sign: Sign, fragment: &[Digit]) -> Result<Self, Sign>;
}

impl<T: FromFragment> FromSignedFragment for T {
    fn from_signed_fragment(sign: Sign, fragment: &[Digit]) -> Result<Self, Sign> {
        if sign.is_negative() && !fragment.is_empty() {
            return Err(Negative);
        }
        T::from_fragment(fragment).ok_or(Positive)
    }
}

macro_rules! unsigned_primitive {
    ($target:ident) => {
        impl FromFragment for $target {
            fn from_fragment(fragment: &[Digit]) -> Option<Self> {
                let mut fragment = fragment.iter().map(|digit| *digit as $target);
                let first = loop {
                    match fragment.next() {
                        // ignore any leading zeros
                        Some(0) => (),
                        Some(digit) => break digit,
                        None => return Some(0),
                    }
                };
                // no `+ 1` to the RHS because we've already subtracted one from the LHS by taking the first digit
                if fragment.len() > <$target>::MAX.ilog10() as usize {
                    return None;
                }
                match fragment.next_back() {
                    None => Some(first),
                    Some(last) => fragment
                        .fold(first, |sum, digit| 10 * sum + digit)
                        .checked_mul(10)?
                        .checked_add(last),
                }
            }
        }

        impl FromArg for $target {
            type Parent = UnsignedInteger;
            type Error = ();

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                use UnsignedInteger::*;

                let (base, exponent) = match arg {
                    Zero => return Ok(0),
                    NonZero { base, exponent } => (base, exponent),
                };
                let value = <$target>::from_fragment(&base.into_inner()).ok_or(())?;
                let exponent = u32::try_from(exponent).map_err(|_| ())?;
                let scalar = <$target>::checked_pow(10, exponent).ok_or(())?;
                value.checked_mul(scalar).ok_or(())
            }

            fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
                Box::new(OverflowError(Constraint {
                    comparator: Comparator::Le,
                    value: <$target>::MAX,
                }))
            }
        }

        #[cfg(test)]
        mod $target {
            use super::*;

            #[test]
            fn max() {
                assert_eq!($target::MAX, parse([$target::MAX]).unwrap());
            }

            #[test]
            fn min() {
                assert_eq!($target::MIN, parse([$target::MIN]).unwrap());
            }
        }
    };
}

unsigned_primitive!(u8);
unsigned_primitive!(u16);
unsigned_primitive!(u32);
unsigned_primitive!(u64);
unsigned_primitive!(u128);
unsigned_primitive!(usize);

macro_rules! signed_primitive {
    ($signed:ident, $unsigned:ident) => {
        impl FromSignedFragment for $signed {
            fn from_signed_fragment(sign: Sign, fragment: &[Digit]) -> Result<Self, Sign> {
                const MAX: $unsigned = <$signed>::MAX.unsigned_abs();
                const MIN: $unsigned = <$signed>::MIN.unsigned_abs();

                match (sign, <$unsigned>::from_fragment(&fragment)) {
                    (Positive, Some(value @ 0..=MAX)) => Ok(value as $signed),
                    (Negative, Some(value @ 0..=MIN)) => Ok(value.wrapping_neg() as $signed),
                    (sign, _) => Err(sign),
                }
            }
        }

        impl FromArg for $signed {
            type Parent = Integer;
            type Error = Sign;

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                use UnsignedInteger::*;

                let Integer { sign, magnitude } = arg;
                let (base, exponent) = match magnitude {
                    Zero => return Ok(0),
                    NonZero { base, exponent } => (base, exponent),
                };
                let value = <$signed>::from_signed_fragment(sign, &base.into_inner())?;
                let exponent = u32::try_from(exponent).map_err(|_| sign)?;
                let scalar = <$signed>::checked_pow(10, exponent).ok_or(sign)?;
                value.checked_mul(scalar).ok_or(sign)
            }

            fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
                match error {
                    Negative => Box::new(OverflowError(Constraint {
                        comparator: Comparator::Ge,
                        value: <$signed>::MIN,
                    })),
                    Positive => Box::new(OverflowError(Constraint {
                        comparator: Comparator::Le,
                        value: <$signed>::MAX,
                    })),
                }
            }
        }

        #[cfg(test)]
        mod $signed {
            use super::*;

            #[test]
            fn max() {
                assert_eq!($signed::MAX, parse([$signed::MAX]).unwrap());
            }

            #[test]
            fn min() {
                assert_eq!($signed::MIN, parse([$signed::MIN]).unwrap());
            }
        }
    };
}

signed_primitive!(i8, u8);
signed_primitive!(i16, u16);
signed_primitive!(i32, u32);
signed_primitive!(i64, u64);
signed_primitive!(i128, u128);
signed_primitive!(isize, usize);

/// Booleans are numbers where `false` equals zero and `true` equals one.
impl FromArg for bool {
    type Parent = Decimal;
    type Error = ();

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        use UnsignedDecimal::*;

        let Decimal { sign, magnitude } = arg;
        let (base, exponent) = match magnitude {
            Zero => return Ok(false),
            NonZero { base, exponent } => (base, exponent),
        };
        if sign.is_positive() && *base.into_inner() == [Digit::D1] && exponent.is_zero() {
            return Ok(true);
        }
        Err(())
    }

    fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
        Box::new(NotBoolError)
    }
}

macro_rules! non_zero {
    ($non_zero:path, $primitive:path) => {
        impl FromArg for $non_zero {
            type Parent = $primitive;
            type Error = ();

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                Self::new(arg).ok_or(())
            }

            fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
                Box::new(IsZeroError)
            }
        }
    };
}

non_zero!(num::NonZeroI8, i8);
non_zero!(num::NonZeroI16, i16);
non_zero!(num::NonZeroI32, i32);
non_zero!(num::NonZeroI64, i64);
non_zero!(num::NonZeroI128, i128);
non_zero!(num::NonZeroIsize, isize);
non_zero!(num::NonZeroU8, u8);
non_zero!(num::NonZeroU16, u16);
non_zero!(num::NonZeroU32, u32);
non_zero!(num::NonZeroU64, u64);
non_zero!(num::NonZeroU128, u128);
non_zero!(num::NonZeroUsize, usize);

macro_rules! atomic {
    ($atomic:path, $primitive:path) => {
        impl FromArg for $atomic {
            type Parent = $primitive;
            type Error = Infallible;

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                Ok(Self::new(arg))
            }

            fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
                match error {}
            }
        }
    };
}

atomic!(atomic::AtomicBool, bool);
atomic!(atomic::AtomicI8, i8);
atomic!(atomic::AtomicI16, i16);
atomic!(atomic::AtomicI32, i32);
atomic!(atomic::AtomicI64, i64);
atomic!(atomic::AtomicIsize, isize);
atomic!(atomic::AtomicU8, u8);
atomic!(atomic::AtomicU16, u16);
atomic!(atomic::AtomicU32, u32);
atomic!(atomic::AtomicU64, u64);
atomic!(atomic::AtomicUsize, usize);

macro_rules! float {
    ($target:path) => {
        impl FromArg for $target {
            type Parent = Box<str>;
            type Error = ();

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                let unsigned = arg.strip_prefix(['+', '-']).unwrap_or(&arg);
                // handle nan and infinity
                if unsigned.starts_with(['N', 'n', 'I', 'i']) {
                    return Err(());
                }
                arg.parse().map_err(|_| ())
            }

            fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
                Box::new(TYPE_ERROR)
            }
        }
    };
}

float!(f32);
float!(f64);

impl FromArg for time::Duration {
    type Parent = duration::Parent;
    type Error = duration::Error;

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        duration::from_arg(arg)
    }

    fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
        use duration::Error::*;

        match error {
            Overflow => Box::new(OverflowError(Constraint {
                comparator: Comparator::Lt,
                value: "18446744073709551616", // todo remove hardcoded string
            })),
            Underflow => Box::new(UnderflowError(9)),
        }
    }
}

macro_rules! moment {
    ($target:path) => {
        impl FromArgs for $target {
            fn from_args<S: Source + ?Sized>(_: &mut Args<S>) -> FromArgsResult<Self> {
                Ok(Self::now())
            }
        }
    };
}

moment!(time::Instant);
moment!(time::SystemTime);

/// Error returned when a number is negative but it must be non-negative.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IsNegativeError;

impl fmt::Display for IsNegativeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "must be non-negative")
    }
}

impl Error for IsNegativeError {}

/// Error returned when a number has a fractional component when it must be an integer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IsDecimalError;

impl fmt::Display for IsDecimalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "must be an integer")
    }
}

impl Error for IsDecimalError {}

/// Error returned when a number did not satisfy a comparison constraint.
///
/// The wrapped [`Constraint<T>`] is the constraint that was exceeded.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OverflowError<T>(pub Constraint<T>);

impl<T: fmt::Display> fmt::Display for OverflowError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(bound) = self;

        write!(f, "must be {bound}")
    }
}

impl<T: fmt::Debug + fmt::Display> Error for OverflowError<T> {}

/// Comparator constraint.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint<T> {
    pub comparator: Comparator,
    pub value: T,
}

impl<T: fmt::Display> fmt::Display for Constraint<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { comparator, value } = self;

        write!(f, "{comparator} {value}")
    }
}

/// Binary comparator (&lt;, &le;, &gt;, &ge;).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Comparator {
    Lt,
    Le,
    Gt,
    Ge,
}

impl fmt::Display for Comparator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Comparator::*;

        let char = match self {
            Lt => '<',
            Le => '≤',
            Gt => '>',
            Ge => '≥',
        };
        write!(f, "{char}")
    }
}

/// Error returned when a number did not satisfy a precision constraint.
///
/// The wrapped `T` is the precision that was exceeded.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnderflowError<T>(pub T);

impl<T: fmt::Display> fmt::Display for UnderflowError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(precision) = self;

        write!(f, "precision must be ≤ {precision}")
    }
}

impl<T: fmt::Debug + fmt::Display> Error for UnderflowError<T> {}

#[derive(Debug)]
struct NotBoolError;

impl fmt::Display for NotBoolError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "must be equal to 0 or 1")
    }
}

impl Error for NotBoolError {}

/// Error returned when a number is zero when it must be non-zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IsZeroError;

impl fmt::Display for IsZeroError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cannot be equal to 0")
    }
}

impl Error for IsZeroError {}

use super::{Decimal, Digit, Digits, Sign, UnsignedDecimal};
use num::{BigInt, BigUint, Zero as _};

pub type Parent = String;

pub fn from_arg(mut arg: Parent) -> Option<Decimal> {
    use UnsignedDecimal::*;

    let mut exponent = take_exponent(&mut arg)?;
    let mut arg = arg.into_bytes();
    let sign = take_sign(&mut arg);
    let shift = take_radix(&mut arg);
    if arg.is_empty() {
        return None;
    }
    let trailing = remove_trailing_zeros(&mut arg);
    let magnitude = if arg.is_empty() {
        Zero
    } else {
        remove_leading_zeros(&mut arg);
        let adjust = trailing as isize - shift as isize;
        exponent += adjust;
        let digits = into_digits_inner(arg.into_boxed_slice())?;
        NonZero {
            base: Digits(digits),
            exponent,
        }
    };
    Some(Decimal { sign, magnitude })
}

fn take_exponent(arg: &mut String) -> Option<BigInt> {
    let (left, right) = match arg.split_once(['E', 'e']) {
        None => return Some(BigInt::zero()),
        Some(parts) => parts,
    };
    let exponent = parse_exponent(right)?;
    arg.truncate(left.len());
    Some(exponent)
}

fn parse_exponent(exponent: &str) -> Option<BigInt> {
    use num::bigint::Sign::*;

    let sign = if exponent.starts_with('-') {
        Minus
    } else {
        Plus
    };
    let magnitude = exponent.strip_prefix(['+', '-']).unwrap_or(exponent);
    if magnitude.is_empty() {
        return None;
    }
    let mut sum = BigUint::zero();
    for byte in magnitude.bytes() {
        let digit = match byte {
            b'0'..=b'9' => byte - b'0',
            _ => return None,
        };
        sum = 10u32 * sum + digit;
    }
    let magnitude = sum;
    let exponent = BigInt::from_biguint(sign, magnitude);
    Some(exponent)
}

fn take_sign(arg: &mut Vec<u8>) -> Sign {
    use Sign::*;

    let sign = match **arg {
        [b'+', ..] => Some(Positive),
        [b'-', ..] => Some(Negative),
        _ => None,
    };
    if sign.is_some() {
        arg.remove(0);
    }
    sign.unwrap_or(Positive)
}

fn take_radix(arg: &mut Vec<u8>) -> usize {
    let radix = match arg.iter().position(|byte| *byte == b'.') {
        None => return 0,
        Some(index) => index,
    };
    arg.remove(radix);
    arg.len() - radix
}

fn remove_trailing_zeros(arg: &mut Vec<u8>) -> usize {
    let length = arg.len();
    let last = arg
        .iter()
        .rposition(|byte| *byte != b'0')
        .map(|index| index + 1)
        .unwrap_or(0);
    arg.truncate(last);
    length - last
}

fn remove_leading_zeros(arg: &mut Vec<u8>) {
    let first = arg.iter().position(|byte| *byte != b'0').unwrap_or(0);
    let truncate = arg[first..].len();
    arg.copy_within(first.., 0);
    arg.truncate(truncate);
}

fn into_digits_inner(mut arg: Box<[u8]>) -> Option<Box<[Digit]>> {
    for byte in arg.iter_mut() {
        match byte {
            b'0'..=b'9' => *byte -= b'0',
            _ => return None,
        }
    }
    Some(unsafe { Box::from_raw(Box::into_raw(arg) as *mut [Digit]) })
}

#[cfg(test)]
mod tests {
    use super::*;
    use Digit::*;

    pub fn parse<A: ToString, const LEN: usize>(args: [A; LEN]) -> crate::ParseResult<Decimal> {
        crate::internal::parse(args)
    }

    #[test]
    fn empty() {
        parse([""]).unwrap_err();
    }

    #[test]
    fn plus() {
        parse(["+"]).unwrap_err();
    }

    #[test]
    fn minus() {
        parse(["-"]).unwrap_err();
    }

    #[test]
    fn period() {
        parse(["."]).unwrap_err();
    }

    #[test]
    fn plus_period() {
        parse(["+."]).unwrap_err();
    }

    #[test]
    fn minus_period() {
        parse(["-."]).unwrap_err();
    }

    #[test]
    fn e() {
        parse(["e"]).unwrap_err();
    }

    #[test]
    fn zero() {
        let Decimal { sign, magnitude } = parse(["0"]).unwrap();
        assert!(sign.is_positive());
        match magnitude {
            UnsignedDecimal::Zero => (),
            UnsignedDecimal::NonZero { .. } => panic!("`magnitude` should be zero"),
        }
    }

    #[test]
    fn plus_one() {
        let Decimal { sign, magnitude } = parse(["+1"]).unwrap();
        assert!(sign.is_positive());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1]);
        assert_eq!(exponent, BigInt::from(0));
    }

    #[test]
    fn minus_one() {
        let Decimal { sign, magnitude } = parse(["-1"]).unwrap();
        assert!(sign.is_negative());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1]);
        assert_eq!(exponent, BigInt::from(0));
    }

    #[test]
    fn radix_one() {
        let Decimal { sign, magnitude } = parse([".1"]).unwrap();
        assert!(sign.is_positive());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1]);
        assert_eq!(exponent, BigInt::from(-1));
    }

    #[test]
    fn radix_plus_one() {
        parse([".+1"]).unwrap_err();
    }

    #[test]
    fn radix_minus_one() {
        parse([".-1"]).unwrap_err();
    }

    #[test]
    fn one_e_one() {
        let Decimal { sign, magnitude } = parse(["1e1"]).unwrap();
        assert!(sign.is_positive());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1]);
        assert_eq!(exponent, BigInt::from(1));
    }

    #[test]
    fn one_e_plus_one() {
        let Decimal { sign, magnitude } = parse(["1e+1"]).unwrap();
        assert!(sign.is_positive());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1]);
        assert_eq!(exponent, BigInt::from(1));
    }

    #[test]
    fn one_e_minus_one() {
        let Decimal { sign, magnitude } = parse(["1e-1"]).unwrap();
        assert!(sign.is_positive());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1]);
        assert_eq!(exponent, BigInt::from(-1));
    }

    #[test]
    fn leading_zeros() {
        let Decimal { sign, magnitude } = parse(["+0001"]).unwrap();
        assert!(sign.is_positive());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1]);
        assert_eq!(exponent, BigInt::from(0));
    }

    #[test]
    fn trailing_zeros() {
        let Decimal { sign, magnitude } = parse(["1000.000e000"]).unwrap();
        assert!(sign.is_positive());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1]);
        assert_eq!(exponent, BigInt::from(3));
    }

    #[test]
    fn typical() {
        let Decimal { sign, magnitude } = parse(["123.456e789"]).unwrap();
        assert!(sign.is_positive());
        let (base, exponent) = match magnitude {
            UnsignedDecimal::Zero => panic!("`magnitude` should be non-zero"),
            UnsignedDecimal::NonZero { base, exponent } => (base, exponent),
        };
        assert_eq!(*base.into_inner(), [D1, D2, D3, D4, D5, D6]);
        assert_eq!(exponent, BigInt::from(786));
    }
}

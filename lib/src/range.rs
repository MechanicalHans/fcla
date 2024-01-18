use crate::internal::*;
use std::ops;

const START: Ident = Ident::Named("start");
const END: Ident = Ident::Named("end");
const BOUND: Ident = Ident::Named("bound");

impl<T: FromArgs> FromArgs for ops::Range<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(args.parse(START)?..args.parse(END)?)
    }
}

impl<T: FromArgs> FromArgs for ops::RangeFrom<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(args.parse(START)?..)
    }
}

impl<T: FromArgs> FromArgs for ops::RangeTo<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(..args.parse(END)?)
    }
}

impl FromArgs for ops::RangeFull {
    fn from_args<S: Source + ?Sized>(_: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(..)
    }
}

impl<T: FromArgs> FromArgs for ops::RangeInclusive<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(args.parse(START)?..=args.parse(END)?)
    }
}

impl<T: FromArgs> FromArgs for ops::RangeToInclusive<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(..=args.parse(END)?)
    }
}

impl<T: FromArgs> FromArgs for ops::Bound<T> {
    /// - `.` &rarr; `Unbounded`
    /// - `,` &rarr; `Excluded`
    /// - `,=` &rarr; `Included`
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        args.branch([
            (".", |_| Ok(Self::Unbounded)),
            (",", |args| Ok(Self::Excluded(args.parse(BOUND)?))),
            (",=", |args| Ok(Self::Included(args.parse(BOUND)?))),
        ])
    }
}

/// Dynamically bounded range.
///
/// This type uses a grammar similar to Rust's ranges with two key differences:
/// 1. The range's variant is the first argument.
/// 2. A bound's existence is communicated via fcla's [`Option`] grammar.
///
/// For example, the fcla equivalent of Rust's `0..10` would be `,, 0 10`.
/// The two commas indicate that both bounds are present and the lack of a trailing `=` indicates that the end bound is exclusive.
///
/// The fcla equivalent of Rust's `..=10` would be `.,= 10`.
/// The leading period indicates that the start bound is absent and the trailing `=` indicates that the end bound is inclusive.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RangeDynamic<T> {
    Range(ops::Range<T>),
    RangeTo(ops::RangeTo<T>),
    RangeFrom(ops::RangeFrom<T>),
    RangeFull(ops::RangeFull),
    RangeInclusive(ops::RangeInclusive<T>),
    RangeToInclusive(ops::RangeToInclusive<T>),
}

impl<T: FromArgs> FromArgs for RangeDynamic<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        args.branch([
            (",,", |args| Ok(Self::Range(from_args(args)?))),
            (",.", |args| Ok(Self::RangeFrom(from_args(args)?))),
            (".,", |args| Ok(Self::RangeTo(from_args(args)?))),
            ("..", |args| Ok(Self::RangeFull(from_args(args)?))),
            (",,=", |args| Ok(Self::RangeInclusive(from_args(args)?))),
            (".,=", |args| Ok(Self::RangeToInclusive(from_args(args)?))),
        ])
    }
}

impl<T> ops::RangeBounds<T> for RangeDynamic<T> {
    fn start_bound(&self) -> ops::Bound<&T> {
        match self {
            Self::Range(range) => range.start_bound(),
            Self::RangeTo(range) => range.start_bound(),
            Self::RangeFrom(range) => range.start_bound(),
            Self::RangeFull(range) => range.start_bound(),
            Self::RangeInclusive(range) => range.start_bound(),
            Self::RangeToInclusive(range) => range.start_bound(),
        }
    }

    fn end_bound(&self) -> ops::Bound<&T> {
        match self {
            Self::Range(range) => range.end_bound(),
            Self::RangeTo(range) => range.end_bound(),
            Self::RangeFrom(range) => range.end_bound(),
            Self::RangeFull(range) => range.end_bound(),
            Self::RangeInclusive(range) => range.end_bound(),
            Self::RangeToInclusive(range) => range.end_bound(),
        }
    }
}

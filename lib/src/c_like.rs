use crate::internal::*;

impl FromArgs for std::cmp::Ordering {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        args.branch([
            ("Less", |_| Ok(Self::Less)),
            ("Equal", |_| Ok(Self::Equal)),
            ("Greater", |_| Ok(Self::Greater)),
        ])
    }
}

impl FromArgs for std::fmt::Alignment {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        args.branch([
            ("Left", |_| Ok(Self::Left)),
            ("Right", |_| Ok(Self::Right)),
            ("Center", |_| Ok(Self::Center)),
        ])
    }
}

impl FromArgs for std::net::Shutdown {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        args.branch([
            ("Read", |_| Ok(Self::Read)),
            ("Write", |_| Ok(Self::Write)),
            ("Both", |_| Ok(Self::Both)),
        ])
    }
}

impl FromArgs for std::num::FpCategory {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        args.branch([
            ("Nan", |_| Ok(Self::Nan)),
            ("Infinite", |_| Ok(Self::Infinite)),
            ("Zero", |_| Ok(Self::Zero)),
            ("Subnormal", |_| Ok(Self::Subnormal)),
            ("Normal", |_| Ok(Self::Normal)),
        ])
    }
}

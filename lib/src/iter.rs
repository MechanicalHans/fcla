use crate::internal::*;
use std::iter;

impl<T> FromArgs for iter::Empty<T> {
    fn from_args<S: Source + ?Sized>(_: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(iter::empty())
    }
}

impl<T: FromArgs> FromArgs for iter::Once<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(iter::once(from_args(args)?))
    }
}

impl<T: FromArgs + Clone> FromArgs for iter::Repeat<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(iter::repeat(from_args(args)?))
    }
}

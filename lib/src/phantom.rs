use crate::internal::*;
use std::marker;

impl<T> FromArgs for marker::PhantomData<T> {
    fn from_args<S: Source + ?Sized>(_: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(Self)
    }
}

impl FromArgs for marker::PhantomPinned {
    fn from_args<S: Source + ?Sized>(_: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(Self)
    }
}

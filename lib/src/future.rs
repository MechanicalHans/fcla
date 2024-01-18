use crate::internal::*;
use std::future;

impl<T: FromArgs> FromArgs for future::Ready<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok(future::ready(from_args(args)?))
    }
}

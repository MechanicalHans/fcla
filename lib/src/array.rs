use crate::internal::*;

impl<T: FromArgs, const LENGTH: usize> FromArgs for [T; LENGTH] {
    // todo replace this with a non-allocating implementation
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        Ok((0..LENGTH)
            .map(|index| args.parse(Ident::Unnamed(index)))
            .collect::<Result<Vec<_>, _>>()?
            .try_into()
            // we don't use `expect` here because we're not concerned with the contents of the vec, only its length
            .unwrap_or_else(|_| unreachable!("there should always be `LENGTH` elements")))
    }
}

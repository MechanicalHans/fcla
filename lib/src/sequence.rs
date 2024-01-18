use crate::internal::*;
use std::collections;

macro_rules! sequence {
    ($($fragment:ident)::* $(, $constraint:path)*) => {
        impl<T> FromArgs for $($fragment)::*<T> where
            T: FromArgs,
            $(T: $constraint,)*
        {
            fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
                args.as_iter().collect()
            }
        }
    };
}

sequence!(Vec);
sequence!(collections::VecDeque);
sequence!(collections::LinkedList);
sequence!(collections::BTreeSet, Ord);
sequence!(collections::HashSet, Eq, std::hash::Hash);
sequence!(collections::BinaryHeap, Ord);

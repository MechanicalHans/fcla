use crate::internal::*;

// https://veykril.github.io/tlborm/decl-macros/patterns/tt-muncher.html
macro_rules! tuples {
    ([$($previous:tt),*], [                          ],) => {};
    ([$($previous:tt),*], [$current:tt $(, $next:tt)*],) => {
        tuple!($($previous,)* $current);
        tuples!([$($previous,)* $current], [$($next),*],);
    };
}

macro_rules! tuple {
    ($(($i:ident, $v:expr)),*) => {
        impl<$($i: FromArgs),*> FromArgs for ($($i,)*) {
            #[allow(unused_variables)]
            fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
                Ok(($(args.parse(Ident::Unnamed($v))?,)*))
            }
        }
    };
}

tuple!();
tuples!(
    [],
    [
        (A, 0),
        (B, 1),
        (C, 2),
        (D, 3),
        (E, 4),
        (F, 5),
        (G, 6),
        (H, 7),
        (I, 8),
        (J, 9),
        (K, 10),
        (L, 11)
    ],
);

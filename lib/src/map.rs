use crate::internal::*;
use std::collections;

/// Key value pair.
///
/// Used as the type param in [`Args::as_iter`] when parsing mappings.
/// If you're implementing [`FromArgs`] for your own mapping type then you should use this type too.
#[derive(Debug, Clone, FromArgs)]
pub struct KeyValue<K, V> {
    pub key: K,
    pub value: V,
}

impl<K, V> KeyValue<K, V> {
    /// Convert a key value pair into a tuple.
    ///
    /// The primary use for this method is for implementing [`FromArgs`] for mappings types.
    pub fn into_pair(self) -> (K, V) {
        let Self { key, value } = self;

        (key, value)
    }
}

macro_rules! map {
    ($($fragment:ident)::* $(, $constraint:path)*) => {
        impl<K, V> FromArgs for $($fragment)::*<K, V> where
            K: FromArgs,
            $(K: $constraint,)*
            V: FromArgs
        {
            fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
                args
                    .as_iter()
                    .map(|result| result.map(KeyValue::into_pair))
                    .collect()
            }
        }
    };
}

map!(collections::BTreeMap, Ord);
map!(collections::HashMap, Eq, std::hash::Hash);

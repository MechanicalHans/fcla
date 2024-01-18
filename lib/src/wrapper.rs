use crate::internal::*;
use std::{cell, num, sync};

macro_rules! opaque {
    ($($fragment:ident)::*) => {
        impl<T: FromArgs> FromArg for $($fragment)::*<T> {
            type Parent = T;
            type Error = Infallible;

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                Ok(Self::new(arg))
            }

            fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
                match error {}
            }
        }
    };
}

opaque!(Box);
opaque!(cell::Cell);
opaque!(cell::RefCell);
opaque!(cell::UnsafeCell);
opaque!(std::mem::ManuallyDrop);
opaque!(std::rc::Rc);
opaque!(sync::Arc);
opaque!(sync::Mutex);
opaque!(sync::RwLock);

macro_rules! transparent {
    ($($fragment:ident)::*) => {
        impl<T: FromArgs> FromArg for $($fragment)::*<T> {
            type Parent = T;
            type Error = Infallible;

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                Ok(Self(arg))
            }

            fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
                match error {}
            }
        }
    };
}

transparent!(num::Saturating);
transparent!(num::Wrapping);
transparent!(std::cmp::Reverse);
transparent!(std::panic::AssertUnwindSafe);

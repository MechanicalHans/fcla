use crate::internal::*;
use std::{ffi, path};

macro_rules! coerced {
    ($($fragment:ident)::*) => {
        impl FromArg for $($fragment)::*<ffi::OsStr> {
            type Parent = ffi::OsString;
            type Error = Infallible;

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                Ok(arg.into())
            }

            fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
                match error {}
            }
        }

        impl FromArg for $($fragment)::*<path::Path> {
            type Parent = path::PathBuf;
            type Error = Infallible;

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                Ok(arg.into())
            }

            fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
                match error {}
            }
        }

        impl FromArg for $($fragment)::*<str> {
            type Parent = String;
            type Error = Infallible;

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                Ok(arg.into())
            }

            fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
                match error {}
            }
        }

        impl<T: FromArgs> FromArg for $($fragment)::*<[T]> {
            type Parent = Vec<T>;
            type Error = Infallible;

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                Ok(arg.into())
            }

            fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
                match error {}
            }
        }
    };
}

coerced!(Box);
coerced!(std::rc::Rc);
coerced!(std::sync::Arc);

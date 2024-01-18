use crate::internal::*;
use std::sync;

impl FromArg for sync::Barrier {
    type Parent = usize;
    type Error = Infallible;

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        Ok(Self::new(arg))
    }

    fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
        match error {}
    }
}

use crate::internal::*;

impl FromArg for std::path::PathBuf {
    type Parent = std::ffi::OsString;
    type Error = Infallible;

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        Ok(arg.into())
    }

    fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync> {
        match error {}
    }
}

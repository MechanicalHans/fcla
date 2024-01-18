use crate::internal::*;

impl FromArg for String {
    type Parent = std::ffi::OsString;
    type Error = ();

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        arg.into_string().map_err(|_| ())
    }

    fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
        Box::new(TypeError("string"))
    }
}

use crate::internal::*;

impl FromArg for char {
    type Parent = Box<str>;
    type Error = ();

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        arg.parse().map_err(|_| ())
    }

    fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
        Box::new(TypeError("character"))
    }
}

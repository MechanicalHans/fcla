//! Fcla lets you create a "functional" command line argument parser in minutes.
//! Simply start by creating an `Args` type, then annotate your types with `#[derive(FromArgs)]`, before finally using the [`parse_cla`] function to parse your arguments.
//! Ideal for small projects and rapid development.
//!
//! # Example
//! The following example shows off most of what `fcla` can do:
//! ```no_run
//! use fcla::FromArgs;
//!
//! #[derive(Debug, FromArgs)]
//! enum Args {
//!     Simple { first: Box<str>, second: i32 },
//!     Nested { nested: Nested },
//!     Optional { optional: Option<Box<str>> },
//!     Sequence { sequence: Vec<i32> },
//!     Mapping { map: std::collections::HashMap<i32, Box<str>> },
//!     Range { range: fcla::RangeDynamic<i32> },
//!     Custom { tree: Tree<Box<str>> },
//!     Complex { sequence: Vec<Tree<Box<str>>> },
//! }
//!
//! #[derive(Debug, FromArgs)]
//! struct Nested {
//!     small: u8,
//!     medium: u16,
//!     large: u32,
//! }
//!
//! #[derive(Debug, FromArgs)]
//! enum Tree<T> {
//!     Root(T),
//!     Branch { left: Box<Self>, right: Box<Self> },
//! }
//!
//! fn main() -> fcla::MainResult<()> {
//!     let args = fcla::parse_cla::<Args>()?.args;
//!     println!("{:?}", args);
//!     Ok(())
//! }
//! ```
//!
//! You can run it with the following arguments:
//! - `Simple 'hello world' -1`
//! - `Nested 128 32768 2147483648`
//! - `Optional , word`
//! - `Optional .`
//! - `Sequence , 1 , -1 , 1E3 , -0.1E4 .`
//! - `Mapping , 4 four , 3 three , 2 two , 1 one .`
//! - `Range ,, 0 10`
//! - `Range .,= 10`
//! - `Custom Branch Branch Root hello Root world Branch Root from Branch Root fcla Root .`
//! - `Complex , Branch Root nested Root types , Root example .`
//!
//! # Grammar
//! Fcla's grammar can be broken into four main parts.
//!
//! ## Derives
//! Derived structs are parsed by parsing their fields in the order that they're declared in.
//!
//! Derived enums are parsed by matching the first argument against the enum's variant names and then parsing the rest of the arguments into the corresponding variant struct.
//!
//! ## Collections
//! Collections are parsed as a sequence of [`Option<T>`]s with zero or more `Some`s terminated by a `None`.
//! `Some` is parsed as `,` and `None` is parsed as `.`.
//! Lists and sets are composed of `T`s whilst mappings are composed of [`KeyValue<K, V>`].
//!
//! See the [`Args::as_iter`] method for more details.
//!
//! ## Numbers
//! Numbers are parsed using the same grammar as floats [`FromStr`] implementation but with the removal of the infinity and NaN cases.
//! This means that `0.1e1` will successfully parse into a [`u32`].
//!
//! See the [`num`] module for more details.
//!
//! ## Bespoke
//! Bespoke types (such as [`IpAddr`](std::net::IpAddr)) are parsed according to some external grammar (for standard library types this is usually their [`FromStr`] implementations).

extern crate self as fcla; // Required for `fcla_macros` to work within this crate.

/// [`FromArgs`] derive macro.
///
/// See the module root for more details about the derived implementation.
///
/// Note that if you're using this derive on a struct with macros in type position then you *may* run into some seemingly unrelated compilation errors later on.
/// It is therefore recommended to not do this.
pub use fcla_macros::FromArgs;

mod array;
mod c_like;
mod char;
mod coerced;
mod future;
mod iter;
mod map;
mod net;
pub mod num;
mod path;
mod phantom;
mod range;
mod sequence;
mod string;
mod sync;
mod tuple;
mod wrapper;

pub use map::*;
pub use range::*;

#[allow(unused_imports)] // needed for a link in the documentation.
use std::str::FromStr;
use std::{error::Error, ffi::OsString, fmt};

/// Alias for `Result<(), MainError<E>>`.
pub type MainResult<E> = Result<(), MainError<E>>;
/// Alias for `Result<T, ParseError>`.
pub type ParseResult<T> = Result<T, ParseError>;
/// Alias for `Result<T, FromArgsError>`.
pub type FromArgsResult<T> = Result<T, FromArgsError>;

type Branch<S, T> = (&'static str, Parser<S, T>);
type Parser<S, T> = fn(args: &mut Args<S>) -> FromArgsResult<T>;

/// Parse arguments from the environment assuming that they're from the command line.
///
/// Usually the first command line argument is the path to the executable.
/// This function takes account for that.
///
/// This is a convenience function for calling `parse_env::<Cla<T>>()`.
pub fn parse_cla<T: FromArgs>() -> Result<Cla<T>, ClaError> {
    parse_env().map_err(ClaError)
}

/// Parse arguments from the environment.
///
/// This is a convenience function for calling `parse(std::env::args_os())`.
pub fn parse_env<T: FromArgs>() -> ParseResult<T> {
    parse(std::env::args_os())
}

/// Parse arguments.
///
/// This will return an error if there is an error during parsing or if there are leftover arguments after parsing.
pub fn parse<I, T>(args: I) -> ParseResult<T>
where
    I: IntoIterator<Item = OsString>,
    I::IntoIter: ExactSizeIterator,
    T: FromArgs,
{
    let source = args.into_iter();
    let mut args = Args(source);
    let value = from_args(&mut args).map_err(ParseError::FromArgs)?;
    let Args(source) = args;
    if source.len() > 0 {
        return Err(ParseError::TooManyArguments);
    }
    Ok(value)
}

/// Convenience function for calling [`FromArgs::from_args`].
pub fn from_args<T, S>(args: &mut Args<S>) -> FromArgsResult<T>
where
    T: FromArgs,
    S: Source + ?Sized,
{
    T::from_args(args)
}

/// Types that can be parsed from [`Args`].
///
/// "Wrapper" types that place no restrictions on their inner values can be [derived](fcla_macros::FromArgs) instead of being manually implemented.
/// "Restriction" types (types that can be derived from [`FromArgs`] types) can implement [`FromArg`] instead of being manually implemented.
///
/// # Example
/// ```
/// use fcla::prelude::*;
///
/// enum Comparator {
///     Lt,
///     Le,
///     Gt,
///     Ge,
/// }
///
/// impl FromArgs for Comparator {
///     fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
///         args.branch([
///             ("lt", |_| Ok(Self::Lt)),
///             ("le", |_| Ok(Self::Le)),
///             ("gt", |_| Ok(Self::Gt)),
///             ("ge", |_| Ok(Self::Ge)),
///         ])
///     }
/// }
/// ```
pub trait FromArgs: Sized {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self>;
}

/// Types that can be used as a source for [`Args`].
pub trait Source: Iterator<Item = OsString> {}
impl<I: Iterator<Item = OsString>> Source for I {}

/// Types that can be parsed from types that can be parsed from [`Args`].
///
/// "Root" types have to implement [`FromArgs`] instead.
///
/// Implementing this trait for a type will also implement [`FromArgs`] for it.
///
/// # Example
/// ```
/// use fcla::prelude::*;
/// use std::{error::Error, fmt, ops::Range};
///
/// struct AscendingRange<T> {
///     start: T,
///     end: T,
/// }
///
/// impl<T: FromArgs + Ord> FromArg for AscendingRange<T> {
///     type Parent = Range<T>;
///     type Error = ();
///
///     fn from_arg(Range { start, end }: Self::Parent) -> Result<Self, Self::Error> {
///         if start > end {
///             return Err(());
///         }
///         Ok(Self { start, end })
///     }
///
///     fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
///         Box::new(DescendingError)
///     }
/// }
///
/// #[derive(Debug)]
/// struct DescendingError;
///
/// impl fmt::Display for DescendingError {
///     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
///         write!(f, "`start` must be less than or equal to `end`")
///     }
/// }
///
/// impl Error for DescendingError {}
/// ```
pub trait FromArg: Sized {
    type Parent: FromArgs;
    type Error;

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error>;
    fn box_error(error: Self::Error) -> Box<dyn Error + Send + Sync>;
}

impl<T: FromArg> FromArgs for T {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        let parent = from_args(args)?;
        T::from_arg(parent).map_err(|error| FromArgsError::new(Self::box_error(error)))
    }
}

// this type is hidden because it's useless to users of this library.
#[doc(hidden)]
pub struct Arg(Option<OsString>);

impl FromArgs for Arg {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        let Args(args) = args;
        Ok(Arg(args.next()))
    }
}

impl FromArg for OsString {
    type Parent = Arg;
    type Error = ();

    fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
        let Arg(arg) = arg;
        arg.ok_or(())
    }

    fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
        Box::new(MissingArgumentError)
    }
}

impl<T: FromArgs> FromArgs for Option<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        let Args(inner) = args;
        match inner.next().as_ref().and_then(|arg| arg.to_str()) {
            Some(".") => Ok(None),
            Some(",") => Ok(Some(from_args(args)?)),
            _ => Err(FromArgsError::new(MissingSeparator)),
        }
    }
}

/// Remaining unparsed arguments.
#[derive(Debug, Clone)]
pub struct Args<S: ?Sized>(S);

impl<S: Source + ?Sized> Args<S> {
    /// Parse the value of a field.
    ///
    /// This method is like [`FromArgs::from_args`] except that it attaches the field context to a returned error.
    pub fn parse<T: FromArgs>(&mut self, field: Ident) -> FromArgsResult<T> {
        from_args(self).map_err(|error| error.add_context(Context::Field(field)))
    }

    /// Parse the rest of args with the parser tupled with the string that matches the first arg.
    ///
    /// This method will return an error if the first arg does not match any of the variants or if parsing the rest of the args returns an error.
    pub fn branch<T, const LEN: usize>(
        &mut self,
        branches: [Branch<S, T>; LEN],
    ) -> FromArgsResult<T> {
        let variant = OsString::from_args(self)?;
        branches
            .iter()
            .find_map(|&(ident, parser)| {
                (variant == ident).then(|| {
                    parser(self).map_err(|error| error.add_context(Context::Variant(ident)))
                })
            })
            .ok_or(FromArgsError::new(UnmatchedVariantError))?
    }

    /// Create an iterator that repeatedly parses [`Option<T>`] from `self`.
    ///
    /// This iterator is like [`parse`] in that it attaches the iteration index context to a returned error.
    pub fn as_iter<T: FromArgs>(&mut self) -> AsIter<'_, S, T> {
        AsIter {
            args: self,
            parser: Option::<T>::from_args,
            index: 0,
        }
    }
}

/// Iterator that repeatedly parses [`Option<T>`] from [`Args`].
///
/// See the [`Args::as_iter`] method for more details.
pub struct AsIter<'a, S: ?Sized, T> {
    args: &'a mut Args<S>,
    parser: Parser<S, Option<T>>,
    index: usize,
}

impl<'a, S: Source + ?Sized, T> Iterator for AsIter<'a, S, T> {
    type Item = FromArgsResult<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let Self {
            ref mut args,
            parser,
            index,
        } = *self;

        self.index += 1;
        parser(args)
            .map_err(|error| error.add_context(Context::Field(Ident::Unnamed(index))))
            .transpose()
    }
}

/// Error parsing environment.
#[derive(Debug)]
pub enum ParseError {
    TooManyArguments,
    FromArgs(FromArgsError),
}

/// Error parsing a specific argument.
#[derive(Debug)]
pub struct FromArgsError {
    cause: Box<dyn Error + Send + Sync>,
    trace: Vec<Context>,
}

#[derive(Debug)]
enum Context {
    Field(Ident),
    Variant(&'static str),
}

/// Struct field identifier.
///
/// See the [`Args::parse`] method for more details.
#[derive(Debug, Clone, Copy)]
pub enum Ident {
    /// Index (for tuple structs).
    Unnamed(usize),
    /// String (for regular structs).
    Named(&'static str),
}

impl FromArgsError {
    fn new(cause: impl Into<Box<dyn Error + Send + Sync>>) -> Self {
        Self {
            cause: cause.into(),
            trace: Vec::new(),
        }
    }

    fn add_context(mut self, context: Context) -> Self {
        self.trace.push(context);
        self
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooManyArguments => write!(f, "too many arguments"),
            Self::FromArgs(error) => error.fmt(f),
        }
    }
}

impl fmt::Display for FromArgsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { cause, trace } = self;

        let trace = TraceDisplay { trace };
        write!(f, "{trace}: {cause}")
    }
}

struct TraceDisplay<'a> {
    trace: &'a [Context],
}

impl<'a> fmt::Display for TraceDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.trace
            .iter()
            .rev()
            .try_for_each(|context| context.fmt(f))
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Context::*;

        match self {
            Field(ident) => write!(f, ".{ident}"),
            Variant(ident) => write!(f, "::{ident}"),
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Ident::*;

        match self {
            Unnamed(index) => index.fmt(f),
            Named(ident) => ident.fmt(f),
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::TooManyArguments => None,
            Self::FromArgs(error) => error.source(),
        }
    }
}

impl Error for FromArgsError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.source()
    }
}

#[derive(Debug)]
struct MissingArgumentError;

impl fmt::Display for MissingArgumentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "argument missing")
    }
}

impl Error for MissingArgumentError {}

#[derive(Debug)]
struct MissingSeparator;

impl fmt::Display for MissingSeparator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "separator missing")
    }
}

impl Error for MissingSeparator {}

#[derive(Debug)]
struct UnmatchedVariantError;

impl fmt::Display for UnmatchedVariantError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "argument doesn't match any of the variants")
    }
}

impl Error for UnmatchedVariantError {}

/// Error parsing an unparsed argument into a type.
///
/// For example, this error is returned when parsing an invalid string into a number type.
///
/// The wrapped [`str`] is the ident for the expected type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeError(pub &'static str);

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(ty) = self;

        write!(f, "argument is not a valid {ty}")
    }
}

impl Error for TypeError {}

/// Wrapper around command line arguments.
///
/// See the [`parse_cla`] function for more details.
#[derive(Debug, Clone)]
pub struct Cla<T> {
    /// Path to the executable.
    pub path: Box<std::path::Path>,
    /// Rest of the args.
    pub args: T,
}

impl<T: FromArgs> FromArgs for Cla<T> {
    fn from_args<S: Source + ?Sized>(args: &mut Args<S>) -> FromArgsResult<Self> {
        let path = args.parse(Ident::Named("self"))?;
        let args = from_args(args)?;
        Ok(Self { path, args })
    }
}

/// Error parsing command line arguments.
pub struct ClaError(ParseError);

/// Error returned from `main`.
pub enum MainError<E> {
    Argument(ParseError),
    Runtime(E),
}

impl<E: fmt::Debug> fmt::Debug for MainError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Argument(error) => write!(f, "parsing command line arguments: {error}"),
            Self::Runtime(error) => error.fmt(f),
        }
    }
}

impl<E> From<ClaError> for MainError<E> {
    fn from(ClaError(inner): ClaError) -> Self {
        Self::Argument(inner)
    }
}

impl<E: fmt::Debug + Into<Box<E>>> From<E> for MainError<E> {
    fn from(value: E) -> Self {
        Self::Runtime(value)
    }
}

// Internal prelude for implementing `FromArgs` internally.
mod internal {
    pub use super::{from_args, Args, FromArg, FromArgs, FromArgsResult, Ident, Source, TypeError};
    pub use std::convert::Infallible;
    pub use std::error::Error;
    pub use std::fmt;

    #[cfg(test)]
    pub use super::ParseResult;

    #[cfg(test)]
    pub fn parse<A, T, const LEN: usize>(args: [A; LEN]) -> ParseResult<T>
    where
        A: ToString,
        T: FromArgs,
    {
        let args = args.iter().map(ToString::to_string).map(Into::into);
        super::parse(args)
    }
}

pub mod prelude {
    pub use super::{Args, FromArg, FromArgs, FromArgsResult, Source};
}

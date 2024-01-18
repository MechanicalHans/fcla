mod derive_from_args;

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

#[proc_macro_derive(FromArgs)]
pub fn from_args(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    let output = match derive_from_args::main(input) {
        Ok(implementation) => implementation.to_token_stream(),
        Err(error) => error.to_compile_error(),
    };
    output.into()
}

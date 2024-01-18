use proc_macro2::{Ident, TokenStream};
use quote::quote;
use std::collections::HashSet;
use syn::{
    parse_quote, punctuated::Punctuated, Block, Data, DeriveInput, Error, Expr, Field, Fields,
    Result, Token, Type, TypeParam, Variant,
};

type FieldList = Punctuated<Field, Token![,]>;
type VariantList = Punctuated<Variant, Token![,]>;

pub fn main(input: DeriveInput) -> Result<syn::ItemImpl> {
    let DeriveInput {
        ident,
        mut generics,
        data,
        ..
    } = input;
    let body = derive(&data)?;
    // We can't just add a `FromArgs` bound to every type parameter because it's possible for a type to implement `FromArgs` without all of its nested types implementing `FromArgs`.
    // For example `std::marker::PhantomData<T>` implements `FromArgs` for any type `T`.
    //
    // Additionally, we can't just add a `FromArgs` bound to every inner type because of https://github.com/rust-lang/rust/issues/48214.
    //
    // Therefore we only add a `FromArgs` bound to inner types that contain a type parameter.
    let type_params = generics.type_params().collect::<Vec<_>>();
    let predicates = inner_tys(&data)
        .into_iter()
        .filter(|ty| ty_contains_type_param(ty, &type_params))
        .map(|ty| -> syn::WherePredicate { parse_quote!(#ty: ::fcla::FromArgs) })
        .collect::<Vec<_>>();
    generics.make_where_clause().predicates.extend(predicates);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    Ok(parse_quote!(
        impl #impl_generics ::fcla::FromArgs for #ident #ty_generics #where_clause {
            #[allow(unused_variables)]
            fn from_args<S: ::fcla::Source + ?::std::marker::Sized>(
                args: &mut ::fcla::Args<S>
            ) -> ::fcla::FromArgsResult<Self>
            #body
        }
    ))
}

fn derive(data: &Data) -> Result<Block> {
    use Data::*;

    const UNION_ERROR: &str = "Cannot derive FromArgs for unions";

    match &data {
        Struct(data) => Ok(derive_struct(None, &data.fields)),
        Enum(data) => Ok(derive_enum(&data.variants)),
        Union(data) => Err(Error::new(data.union_token.span, UNION_ERROR)),
    }
}

fn derive_struct(variant: Option<&Ident>, fields: &Fields) -> Block {
    use Fields::*;

    let receiver = match variant {
        Some(ident) => quote!(Self::#ident),
        None => quote!(Self),
    };
    let body = match fields {
        Unit => TokenStream::new(),
        Unnamed(fields) => unnamed_struct_body(&fields.unnamed),
        Named(fields) => named_struct_body(&fields.named),
    };
    parse_quote!({ ::std::result::Result::Ok(#receiver #body) })
}

fn unnamed_struct_body(fields: &FieldList) -> TokenStream {
    let exprs = fields
        .iter()
        .enumerate()
        .map(|(index, field)| unnamed_struct_body_field(index, &field.ty));
    quote!((#(#exprs),*))
}

fn unnamed_struct_body_field(index: usize, ty: &Type) -> Expr {
    parse_quote!(
        ::fcla::Args::parse::<#ty>(
            args,
            ::fcla::Ident::Unnamed(#index),
        )?
    )
}

fn named_struct_body(fields: &FieldList) -> TokenStream {
    let fields = fields
        .iter()
        .map(|Field { ident, ty, .. }| named_struct_body_field(ident, ty));
    quote!({ #(#fields),* })
}

fn named_struct_body_field(ident: &Option<Ident>, ty: &Type) -> syn::FieldValue {
    parse_quote!(
        #ident: ::fcla::Args::parse::<#ty>(
            args,
            ::fcla::Ident::Named(::std::stringify!(#ident)),
        )?
    )
}

fn derive_enum(variants: &VariantList) -> Block {
    let arms = variants
        .iter()
        .map(|Variant { ident, fields, .. }| -> Expr {
            let body = derive_struct(Some(ident), fields);
            parse_quote!((::std::stringify!(#ident), |args| #body))
        });
    parse_quote!({ ::fcla::Args::branch(args, [#(#arms),*]) })
}

fn inner_tys(data: &Data) -> HashSet<&Type> {
    use Data::*;

    let mut inner_tys = HashSet::new();
    match &data {
        Struct(data) => add_struct_inner_tys(&mut inner_tys, &data.fields),
        Enum(data) => add_enum_inner_tys(&mut inner_tys, &data.variants),
        Union(data) => add_union_inner_tys(&mut inner_tys, &data.fields),
    }
    inner_tys
}

fn add_struct_inner_tys<'a>(inner_tys: &mut HashSet<&'a Type>, fields: &'a Fields) {
    use Fields::*;

    let fields = match fields {
        Unit => return,
        Unnamed(fields) => &fields.unnamed,
        Named(fields) => &fields.named,
    };
    add_fields_inner_tys(inner_tys, fields)
}

fn add_enum_inner_tys<'a>(inner_tys: &mut HashSet<&'a Type>, variants: &'a VariantList) {
    for variant in variants.iter() {
        add_struct_inner_tys(inner_tys, &variant.fields)
    }
}

fn add_union_inner_tys<'a>(inner_tys: &mut HashSet<&'a Type>, fields: &'a syn::FieldsNamed) {
    add_fields_inner_tys(inner_tys, &fields.named)
}

fn add_fields_inner_tys<'a>(inner_tys: &mut HashSet<&'a Type>, fields: &'a FieldList) {
    inner_tys.extend(fields.iter().map(|field| &field.ty))
}

fn ty_contains_type_param(ty: &Type, type_params: &[&TypeParam]) -> bool {
    use Type::*;

    match ty {
        Array(ty) => ty_contains_type_param(&ty.elem, type_params),
        BareFn(ty) => type_bare_fn_contains_type_param(ty, type_params),
        Group(ty) => ty_contains_type_param(&ty.elem, type_params),
        ImplTrait(_) => false,
        Infer(_) => false,
        // We can't know if a macro contains any type parameters.
        // Therefore we conservatively assume that they do.
        Macro(_) => true,
        Never(_) => false,
        Paren(ty) => ty_contains_type_param(&ty.elem, type_params),
        Path(ty) => type_type_path_contains_type_param(ty, type_params),
        Ptr(ty) => ty_contains_type_param(&ty.elem, type_params),
        Reference(ty) => ty_contains_type_param(&ty.elem, type_params),
        Slice(ty) => ty_contains_type_param(&ty.elem, type_params),
        TraitObject(_) => false,
        Tuple(ty) => ty_tuple_contains_type_param(ty, type_params),
        Verbatim(_) => todo!("when does this variant even show up?"),
        _ => todo!(),
    }
}

fn type_bare_fn_contains_type_param(bare_fn: &syn::TypeBareFn, type_params: &[&TypeParam]) -> bool {
    use syn::ReturnType::*;

    if bare_fn
        .inputs
        .iter()
        .any(|bare_fn_arg| ty_contains_type_param(&bare_fn_arg.ty, type_params))
    {
        return true;
    }
    match &bare_fn.output {
        Type(_, elem) => ty_contains_type_param(elem, type_params),
        Default => false,
    }
}

fn type_type_path_contains_type_param(path: &syn::TypePath, type_params: &[&TypeParam]) -> bool {
    type_params
        .iter()
        .any(|type_param| path.path.is_ident(&type_param.ident))
}

fn ty_tuple_contains_type_param(tuple: &syn::TypeTuple, type_params: &[&TypeParam]) -> bool {
    tuple
        .elems
        .iter()
        .any(|elem| ty_contains_type_param(elem, type_params))
}

//! `#[derive(Ad4mModel)]` — describe a mounted class's SHACL shape on
//! its Rust struct, the Rust counterpart of the TS model decorators
//! (`@Model`/`@Property`/`@HasOne`/`@HasMany` with `mount: true` in
//! `@coasys/ad4m/model`).
//!
//! The generated `Class::generate_shacl()` returns a
//! `ad4m_ldk::model::MountClassShape` (`{ name, shape }`) whose `shape`
//! is the same JSON `generateSHACL().shape.toJSON()` produces for the
//! equivalent decorated class — the mount form: no datatype inference,
//! `writable` only when a property really is writable, `call` actions
//! instead of link writes.
//!
//! ```ignore
//! #[derive(Ad4mModel)]
//! #[ad4m(
//!     namespace = "mounttest://",
//!     target_class = "mounttest://Thing",
//!     constructor = call("create", source = "{language}://class/MountThing"),
//!     destructor = call("delete"),
//! )]
//! struct MountThing {
//!     #[ad4m_property(through = "rdf://type", flag = "mounttest://Thing")]
//!     r#type: String,
//!     #[ad4m_property(through = "mounttest://title", setter = call("update"))]
//!     title: String,
//!     #[ad4m_property(through = "mounttest://tag", collection, setter = call("update"))]
//!     tags: Vec<String>,
//! }
//! ```
//!
//! Grammar — class `#[ad4m(...)]`: `name`, `namespace` (defaults to the
//! scheme of the first `through`, as the TS decorators infer it),
//! `target_class`, repeatable `constructor`/`destructor` `call(...)`s.
//! Property `#[ad4m_property(...)]`: `through` (required), `name`,
//! `required`, `collection`, `flag = "iri"` (a `has_value` constant,
//! implies `required`), `read_only`, `writable`, `local`, `datatype`,
//! `node_kind`, `relation_kind`, `alias`, `setter = call(...)`.
//! Relation `#[ad4m_relation(...)]`: `kind = one|many` (required —
//! `many` also sets `collection`), `through`, plus the property options
//! and `adder`/`remover = call(...)`. `call("interaction", source =
//! "...", params = "...")`: `source` defaults to `this` except on
//! constructors where it is required; `params` is the action's `target`
//! JSON verbatim and defaults to `{}`. The Rust struct's fields only
//! carry the shape — unlike `Ad4mModel` in the client SDK there is no
//! instance behaviour attached.

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    parenthesized, parse_macro_input, Data, DataStruct, DeriveInput, Error, Fields, Ident, LitStr,
    Token,
};

/// One parsed `call("interaction", source = "...", params = "...")`.
struct CallSpec {
    interaction: LitStr,
    source: Option<LitStr>,
    params: Option<LitStr>,
    span: Span,
}

impl Parse for CallSpec {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kw: Ident = input.parse()?;
        let span = kw.span();
        if kw != "call" {
            return Err(Error::new(span, "expected `call(\"interaction\", …)`"));
        }
        let inner;
        parenthesized!(inner in input);
        let interaction: LitStr = inner.parse()?;
        let mut source = None;
        let mut params = None;
        while inner.peek(Token![,]) {
            inner.parse::<Token![,]>()?;
            if inner.is_empty() {
                break;
            }
            let key: Ident = inner.parse()?;
            inner.parse::<Token![=]>()?;
            let value: LitStr = inner.parse()?;
            match key.to_string().as_str() {
                "source" => source = Some(value),
                "params" => params = Some(value),
                other => {
                    return Err(Error::new(
                        key.span(),
                        format!("unknown call option `{other}` — `source` or `params`"),
                    ))
                }
            }
        }
        Ok(Self {
            interaction,
            source,
            params,
            span,
        })
    }
}

/// One `key`, `key = "value"`, `key = word` or `key = call(...)` item of
/// an attribute's comma list.
enum AttrVal {
    Flag,
    Str(String),
    Call(CallSpec),
}

struct AttrItem {
    key: String,
    key_span: Span,
    val: AttrVal,
}

impl Parse for AttrItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let key: Ident = input.parse()?;
        let key_span = key.span();
        let val = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            // `call(...)` — a `call` ident followed by parens.
            if input.peek(Ident) && input.peek2(syn::token::Paren) {
                AttrVal::Call(input.parse::<CallSpec>()?)
            } else if input.peek(LitStr) {
                AttrVal::Str(input.parse::<LitStr>()?.value())
            } else if input.peek(Ident) {
                AttrVal::Str(input.parse::<Ident>()?.to_string())
            } else {
                return Err(Error::new(
                    input.span(),
                    "expected a string, a word, or `call(...)`",
                ));
            }
        } else {
            AttrVal::Flag
        };
        Ok(Self {
            key: key.to_string(),
            key_span,
            val,
        })
    }
}

struct Attrs(Vec<AttrItem>);

impl Parse for Attrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(
            Punctuated::<AttrItem, Token![,]>::parse_terminated(input)?
                .into_iter()
                .collect(),
        ))
    }
}

/// Emit `(interaction, source, target-params)` literals for a call
/// list. `default_source` applies per call when the spec names none —
/// `None` (a constructor) makes a missing `source` an authoring error,
/// as the TS generator does (`this` means nothing before the record
/// exists).
fn call_tuples(
    calls: &[&CallSpec],
    default_source: Option<&str>,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    calls
        .iter()
        .map(|c| {
            let source = match &c.source {
                Some(s) => s.value(),
                None => match default_source {
                    Some(d) => d.to_string(),
                    None => {
                        return Err(Error::new(
                            c.span,
                            "a constructor call needs a `source` expression URL — \
                             `this` means nothing before the record exists",
                        ))
                    }
                },
            };
            let target = c
                .params
                .as_ref()
                .map(|p| p.value())
                .unwrap_or_else(|| "{}".into());
            let interaction = &c.interaction;
            let source = LitStr::new(&source, c.span);
            let target = LitStr::new(&target, c.span);
            Ok(quote! { (#interaction, #source, #target) })
        })
        .collect()
}

/// The un-`r#`-prefixed field name — the property name the shape shows.
fn field_name(field: &syn::Field) -> String {
    let raw = field
        .ident
        .as_ref()
        .map(|i| i.to_string())
        .unwrap_or_default();
    raw.strip_prefix("r#").unwrap_or(&raw).to_string()
}

fn opt_lit(v: &Option<String>) -> proc_macro2::TokenStream {
    match v {
        Some(s) => {
            let lit = LitStr::new(s, Span::call_site());
            quote! { Some(#lit) }
        }
        None => quote! { None },
    }
}

fn derive_impl(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = input.ident.clone();

    // ---- class-level #[ad4m(...)] ----
    let mut name = struct_name.to_string();
    let mut namespace: Option<String> = None;
    let mut target_class: Option<String> = None;
    let mut constructors: Vec<&CallSpec> = Vec::new();
    let mut destructors: Vec<&CallSpec> = Vec::new();
    let mut class_attr: Option<Attrs> = None;
    for attr in &input.attrs {
        if attr.path().is_ident("ad4m") {
            class_attr = Some(attr.parse_args::<Attrs>()?);
        }
    }
    if let Some(attrs) = &class_attr {
        for item in &attrs.0 {
            match (item.key.as_str(), &item.val) {
                ("name", AttrVal::Str(v)) => name = v.clone(),
                ("namespace", AttrVal::Str(v)) => namespace = Some(v.clone()),
                ("target_class", AttrVal::Str(v)) => target_class = Some(v.clone()),
                ("constructor", AttrVal::Call(c)) => constructors.push(c),
                ("destructor", AttrVal::Call(c)) => destructors.push(c),
                (k, _) => {
                    return Err(Error::new(
                        item.key_span,
                        format!("unknown `#[ad4m]` option `{k}`"),
                    ))
                }
            }
        }
    }

    // ---- fields ----
    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(f),
            ..
        }) => &f.named,
        _ => {
            return Err(Error::new(
                struct_name.span(),
                "Ad4mModel derives only for structs with named fields",
            ))
        }
    };

    let mut props: Vec<proc_macro2::TokenStream> = Vec::new();
    for field in fields {
        let mut prop_attr = None;
        let mut rel_attr = None;
        for attr in &field.attrs {
            if attr.path().is_ident("ad4m_property") {
                prop_attr = Some(attr.parse_args::<Attrs>()?);
            } else if attr.path().is_ident("ad4m_relation") {
                rel_attr = Some(attr.parse_args::<Attrs>()?);
            }
        }
        let is_relation = rel_attr.is_some();
        let attrs = match (prop_attr, rel_attr) {
            (Some(a), None) | (None, Some(a)) => a,
            (None, None) => continue, // plain state field — not a shape property
            (Some(_), Some(_)) => {
                return Err(Error::new(
                    field.ident.as_ref().unwrap().span(),
                    "a field is either `ad4m_property` or `ad4m_relation`, not both",
                ))
            }
        };

        // Shared options.
        let mut prop_name = field_name(field);
        let mut through: Option<String> = None;
        let mut required = false;
        let mut collection = false;
        let mut flag: Option<String> = None;
        let mut read_only = false;
        let mut writable = false;
        let mut local = false;
        let mut node_kind: Option<String> = None;
        let mut relation_kind: Option<String> = None;
        let mut alias: Option<String> = None;
        let mut datatype: Option<String> = None;
        let mut setters: Vec<&CallSpec> = Vec::new();
        let mut adders: Vec<&CallSpec> = Vec::new();
        let mut removers: Vec<&CallSpec> = Vec::new();
        let mut kind: Option<String> = None;
        for item in &attrs.0 {
            match (item.key.as_str(), &item.val) {
                ("name", AttrVal::Str(v)) => prop_name = v.clone(),
                ("through" | "predicate", AttrVal::Str(v)) => through = Some(v.clone()),
                ("required", AttrVal::Flag) => required = true,
                ("collection", AttrVal::Flag) => collection = true,
                ("flag", AttrVal::Str(v)) => {
                    flag = Some(v.clone());
                    required = true;
                }
                ("read_only", AttrVal::Flag) => read_only = true,
                ("writable", AttrVal::Flag) => writable = true,
                ("local", AttrVal::Flag) => local = true,
                ("node_kind", AttrVal::Str(v)) => node_kind = Some(v.clone()),
                ("relation_kind", AttrVal::Str(v)) => relation_kind = Some(v.clone()),
                ("alias", AttrVal::Str(v)) => alias = Some(v.clone()),
                ("datatype", AttrVal::Str(v)) => datatype = Some(v.clone()),
                ("setter", AttrVal::Call(c)) => setters.push(c),
                ("adder", AttrVal::Call(c)) => adders.push(c),
                ("remover", AttrVal::Call(c)) => removers.push(c),
                ("kind", AttrVal::Str(v)) if is_relation => kind = Some(v.clone()),
                (k, _) => {
                    return Err(Error::new(
                        item.key_span,
                        format!("unknown attribute option `{k}`"),
                    ))
                }
            }
        }
        let through = through.ok_or_else(|| {
            Error::new(
                field.ident.as_ref().unwrap().span(),
                "a shape field needs `through` — the predicate IRI",
            )
        })?;

        // The first `through` also provides the namespace fallback, as
        // the TS decorators derive it: the scheme part (`x://`).
        if namespace.is_none() {
            if let Some(pos) = through.find("://") {
                namespace = Some(format!("{}://", &through[..pos]));
            }
        }

        // Cross-checks mirrored from the TS decorators: a read-only
        // member declares no writer.
        if read_only
            && (writable || !setters.is_empty() || !adders.is_empty() || !removers.is_empty())
        {
            return Err(Error::new(
                field.ident.as_ref().unwrap().span(),
                "`read_only` conflicts with `writable`/`setter`/`adder`/`remover`",
            ));
        }
        if is_relation && !setters.is_empty() {
            return Err(Error::new(
                field.ident.as_ref().unwrap().span(),
                "a relation writes through `adder`/`remover`, not `setter`",
            ));
        }

        if is_relation {
            // `kind = one` → a single `hasOne` member; `kind = many` →
            // a `hasMany` collection. Both are IRI node kinds by default.
            match kind.as_deref() {
                Some("one") => {
                    relation_kind.get_or_insert_with(|| "hasOne".into());
                }
                Some("many") => {
                    relation_kind.get_or_insert_with(|| "hasMany".into());
                    collection = true;
                }
                _ => {
                    return Err(Error::new(
                        field.ident.as_ref().unwrap().span(),
                        "a relation needs `kind = one` or `kind = many`",
                    ))
                }
            }
            node_kind.get_or_insert_with(|| "IRI".into());
        }
        // A declared setter marks the property writable — a writer the
        // mount exposes.
        if !setters.is_empty() {
            writable = true;
        }

        let name_lit = LitStr::new(&prop_name, field.ident.as_ref().unwrap().span());
        let path_lit = LitStr::new(&through, field.ident.as_ref().unwrap().span());
        let flag_lit = opt_lit(&flag);
        let nk_lit = opt_lit(&node_kind);
        let rk_lit = opt_lit(&relation_kind);
        let alias_lit = opt_lit(&alias);
        let dt_lit = opt_lit(&datatype);
        let setter_ts = call_tuples(&setters, Some("this"))?;
        let adder_ts = call_tuples(&adders, Some("this"))?;
        let remover_ts = call_tuples(&removers, Some("this"))?;
        props.push(quote! {
            shape.property(
                #name_lit, #path_lit,
                #required, #collection,
                #flag_lit, #writable, #local,
                #nk_lit, #rk_lit, #alias_lit, #dt_lit,
                &[ #(#setter_ts),* ], &[ #(#adder_ts),* ], &[ #(#remover_ts),* ],
            );
        });
    }

    let namespace = namespace.ok_or_else(|| {
        Error::new(
            struct_name.span(),
            "no `namespace` and no `through` to derive one from",
        )
    })?;
    let target_class = target_class.unwrap_or_else(|| format!("{namespace}{name}"));
    let shape_uri = format!("{namespace}{name}Shape");

    let ctor_ts = call_tuples(&constructors, None)?;
    let dtor_ts = call_tuples(&destructors, Some("this"))?;
    let name_lit = LitStr::new(&name, struct_name.span());
    let shape_uri_lit = LitStr::new(&shape_uri, struct_name.span());
    let target_class_lit = LitStr::new(&target_class, struct_name.span());

    Ok(quote! {
        impl #struct_name {
            /// `{ name, shape }` for `mount.classes` — the mount SHACL
            /// shape `generateSHACL()` emits for the equivalent
            /// TS-decorated class.
            pub fn generate_shacl() -> ::ad4m_ldk::model::MountClassShape {
                let mut shape = ::ad4m_ldk::model::MountShapeBuilder::new(
                    #shape_uri_lit, #target_class_lit,
                );
                shape.constructor_actions(&[ #(#ctor_ts),* ]);
                shape.destructor_actions(&[ #(#dtor_ts),* ]);
                #(#props)*
                ::ad4m_ldk::model::MountClassShape {
                    name: #name_lit.to_string(),
                    shape: shape.finish(),
                }
            }
        }
    })
}

#[proc_macro_derive(Ad4mModel, attributes(ad4m, ad4m_property, ad4m_relation))]
pub fn ad4m_model_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive_impl(input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

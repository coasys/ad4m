//! Proc macro crate for `#[rest_handler(...)]`.
//!
//! Each annotated handler becomes the single source of truth for its route path,
//! HTTP method, request type, and response type. The macro emits the original
//! function unchanged **plus** an `inventory::submit!` call that registers a
//! `RouteMetadata` entry at link-time.
//!
//! # Usage
//!
//! ```rust,ignore
//! #[rest_handler(GET, "/agent")]
//! // → request_type = "never", response inferred from return type
//!
//! #[rest_handler(POST, "/agent/generate")]
//! // → request inferred from Json<T> param, response from return type
//!
//! #[rest_handler(PUT, "/languages/:address/settings", request = "Record<string, unknown>")]
//! // → explicit TS request type override
//!
//! #[rest_handler(GET, "/runtime/friends/:did", response = "unknown")]
//! // → explicit TS response type override
//! ```

use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, FnArg, ItemFn, LitStr, Pat, ReturnType};

/// Attribute macro: `#[rest_handler(METHOD, "/path" [, request = "TsType"] [, response = "TsType"])]`
#[proc_macro_attribute]
pub fn rest_handler(attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);
    let attr_str = attr.to_string();

    // Parse attribute manually from token string
    // Format: METHOD, "/path" [, request = "..." ] [, response = "..." ]
    let (method, path, req_override, resp_override) = parse_attr(&attr_str);

    let request_type = req_override.unwrap_or_else(|| infer_request_type(&func));
    let response_type = resp_override.unwrap_or_else(|| infer_response_type(&func));

    let fn_name = &func.sig.ident;

    let expanded = quote! {
        #func

        inventory::submit! {
            crate::rest::route_meta::RouteMetadata {
                method: #method,
                path: #path,
                handler_name: stringify!(#fn_name),
                request_type: #request_type,
                response_type: #response_type,
            }
        }
    };

    expanded.into()
}

fn parse_attr(s: &str) -> (String, String, Option<String>, Option<String>) {
    // Tokenize roughly: METHOD , "path" [, request = "..." ] [, response = "..." ]
    let s = s.trim();

    // Find method (first ident)
    let comma_pos = s
        .find(',')
        .expect("rest_handler: missing comma after METHOD");
    let method = s[..comma_pos].trim().to_string();

    let rest = s[comma_pos + 1..].trim();

    // Find path (first string literal)
    let (path, rest) = extract_string_literal(rest).expect("rest_handler: missing path string");

    let mut req_override = None;
    let mut resp_override = None;

    // Parse remaining key = "value" pairs
    let mut remaining = rest.trim();
    while !remaining.is_empty() {
        // skip leading comma
        remaining = remaining.trim_start_matches(',').trim();
        if remaining.is_empty() {
            break;
        }
        if remaining.starts_with("request") {
            remaining = remaining["request".len()..].trim();
            remaining = remaining.trim_start_matches('=').trim();
            let (val, r) =
                extract_string_literal(remaining).expect("rest_handler: bad request value");
            req_override = Some(val);
            remaining = r.trim();
        } else if remaining.starts_with("response") {
            remaining = remaining["response".len()..].trim();
            remaining = remaining.trim_start_matches('=').trim();
            let (val, r) =
                extract_string_literal(remaining).expect("rest_handler: bad response value");
            resp_override = Some(val);
            remaining = r.trim();
        } else {
            panic!("rest_handler: unexpected token: {}", remaining);
        }
    }

    (method, path, req_override, resp_override)
}

fn extract_string_literal(s: &str) -> Option<(String, &str)> {
    let s = s.trim();
    if !s.starts_with('"') {
        return None;
    }
    let end = s[1..].find('"')? + 1;
    let val = s[1..end].to_string();
    Some((val, &s[end + 1..]))
}

/// Infer request type from `Json<T>` parameter.
fn infer_request_type(func: &ItemFn) -> String {
    for arg in &func.sig.inputs {
        if let FnArg::Typed(pat_type) = arg {
            let ty_str = pat_type.ty.to_token_stream().to_string();
            // Look for Json<SomeType>
            if let Some(inner) = extract_generic_inner(&ty_str, "Json") {
                return rust_type_to_ts(&inner);
            }
        }
    }
    "never".to_string()
}

/// Infer response type from `Result<Json<R>, _>` or SSE return types.
fn infer_response_type(func: &ItemFn) -> String {
    if let ReturnType::Type(_, ty) = &func.sig.output {
        let ty_str = ty.to_token_stream().to_string();
        // Sse<...> → void
        if ty_str.contains("Sse") {
            return "void".to_string();
        }
        // Result<Json<T>, _>
        if let Some(result_inner) = extract_generic_inner(&ty_str, "Result") {
            if let Some(json_inner) = extract_generic_inner(&result_inner, "Json") {
                return rust_type_to_ts(&json_inner);
            }
        }
    }
    "unknown".to_string()
}

/// Extract the inner type from `TypeName < ... >` handling nested angle brackets.
fn extract_generic_inner(s: &str, type_name: &str) -> Option<String> {
    let pos = s.find(type_name)?;
    let after = s[pos + type_name.len()..].trim();
    if !after.starts_with('<') {
        return None;
    }
    let mut depth = 0;
    let mut start = 0;
    for (i, ch) in after.char_indices() {
        match ch {
            '<' => {
                if depth == 0 {
                    start = i + 1;
                }
                depth += 1;
            }
            '>' => {
                depth -= 1;
                if depth == 0 {
                    return Some(after[start..i].trim().to_string());
                }
            }
            _ => {}
        }
    }
    None
}

/// Convert a Rust type string (from token stream) to a TypeScript type string.
fn rust_type_to_ts(s: &str) -> String {
    let s = s.trim();

    // Vec<T> → T[]
    if let Some(inner) = extract_generic_inner(s, "Vec") {
        return format!("{}[]", rust_type_to_ts(&inner));
    }
    // Option<T> → T | null
    if let Some(inner) = extract_generic_inner(s, "Option") {
        return format!("{} | null", rust_type_to_ts(&inner));
    }
    // serde_json::Value → unknown
    if s.contains("Value") {
        return "unknown".to_string();
    }
    // String → string
    if s == "String" || s == "& str" || s == "&str" {
        return "string".to_string();
    }
    // bool → boolean
    if s == "bool" {
        return "boolean".to_string();
    }
    // Strip the first part of Result<T, E> down to just the first generic arg
    // For cases like `Result < Json < T > , E >` — but this should be handled upstream

    // Clean up whitespace
    s.replace(" ", "")
}

//! Low-level utilities shared across the model query module.
//!
//! This module provides SPARQL injection prevention helpers
//! ([`escape_sparql_string`], [`validate_iri`]), typed parsing of AD4M's
//! `literal:` URI scheme ([`parse_literal_value`]), and numeric/timestamp
//! coercion helpers used by the filtering engine.

use deno_core::anyhow::{anyhow, Error};
#[cfg(test)]
use percent_encoding::utf8_percent_encode;
use serde_json::Value;

// ---------------------------------------------------------------------------
// SPARQL injection prevention helpers
// ---------------------------------------------------------------------------

/// Percent-encoding set matching JS `encodeRFC3986URIComponent`.
/// Leaves `A-Z a-z 0-9 - _ . ~` un-encoded — same characters the SDK
/// preserves, so `literal:*:` URLs round-trip without re-encoding drift.
#[cfg(test)]
const RFC3986_COMPONENT_ENCODE: percent_encoding::AsciiSet = percent_encoding::NON_ALPHANUMERIC
    .remove(b'-')
    .remove(b'_')
    .remove(b'.')
    .remove(b'~');

/// Encode a string for use in a `literal:string:…` IRI, matching
/// the JS SDK's `encodeRFC3986URIComponent` encoding. Test-only —
/// production paths construct typed RDF literals instead of synthesising
/// the URI form by hand.
#[cfg(test)]
pub(super) fn literal_percent_encode(s: &str) -> String {
    utf8_percent_encode(s, &RFC3986_COMPONENT_ENCODE).to_string()
}

/// Format a finite f64 for the `literal:number:` IRI tail, mirroring
/// `literal_encode` in `languages/literal.rs`: integers render without a
/// fractional part (e.g. `42`), floats use `{}` formatting (e.g. `3.14`).
///
/// Returns `None` if the value is non-finite (NaN or +/- infinity) — these
/// are rejected so we never emit a malformed IRI into the SPARQL.
pub(super) fn format_literal_number(n: f64) -> Option<String> {
    if !n.is_finite() {
        return None;
    }
    if n.fract() == 0.0 && n.abs() < (i64::MAX as f64) {
        Some(format!("{}", n as i64))
    } else {
        Some(format!("{n}"))
    }
}

/// Escape a string value for use inside a SPARQL string literal (double-quoted).
pub(super) fn escape_sparql_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Cheap heuristic for "this string is plausibly an absolute IRI" — passes
/// `validate_iri`, has a `:`, and starts with an ASCII letter. Used to decide
/// whether a where-value is safe to emit as a `<…>` IRIREF; rejects bare
/// strings like `"active"` that would produce un-parseable SPARQL.
pub(super) fn looks_like_absolute_iri(s: &str) -> bool {
    if validate_iri(s).is_err() {
        return false;
    }
    let Some(colon_idx) = s.find(':') else {
        return false;
    };
    if colon_idx == 0 {
        return false;
    }
    let first = s.as_bytes()[0];
    first.is_ascii_alphabetic()
}

/// Validate a value for use inside an IRI `<…>`.  Rejects characters that
/// would break or inject into a SPARQL IRI token, including all control and
/// whitespace characters (e.g. `\n`, `\r`, `\t`, U+00A0) which `validate_iri`
/// previously let through and which would emit malformed `<…>` IRIREFs.
pub(super) fn validate_iri(s: &str) -> Result<&str, Error> {
    if s.chars().any(|c| c.is_control() || c.is_whitespace())
        || s.contains('>')
        || s.contains('<')
        || s.contains('{')
        || s.contains('}')
        || s.contains('"')
    {
        return Err(anyhow!("Invalid IRI component: '{}'", s));
    }
    Ok(s)
}

/// Maximum recursion depth for include resolution to prevent stack overflow.
pub(super) const MAX_INCLUDE_DEPTH: u8 = 8;

// ---------------------------------------------------------------------------
// literal: URI parsing (typed)
// ---------------------------------------------------------------------------

/// Parse a `literal:` URI into a typed JSON value, or return the input as a
/// string when it is not a literal URI.
pub(super) fn parse_literal_value(uri: &str) -> Value {
    let body = if let Some(rest) = uri.strip_prefix("literal:") {
        rest
    } else {
        return Value::String(uri.to_string());
    };

    if let Some(rest) = body.strip_prefix("string:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        Value::String(decoded.into_owned())
    } else if let Some(rest) = body.strip_prefix("number:") {
        if let Ok(n) = rest.parse::<i64>() {
            Value::Number(n.into())
        } else if let Ok(f) = rest.parse::<f64>() {
            serde_json::Number::from_f64(f)
                .map(Value::Number)
                .unwrap_or(Value::String(rest.to_string()))
        } else {
            Value::String(rest.to_string())
        }
    } else if let Some(rest) = body.strip_prefix("boolean:") {
        match rest {
            "true" => Value::Bool(true),
            "false" => Value::Bool(false),
            _ => Value::String(rest.to_string()),
        }
    } else if let Some(rest) = body.strip_prefix("json:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        if let Ok(json_val) = serde_json::from_str::<Value>(&decoded) {
            // Unwrap signed-expression envelopes (`{author, timestamp, data, proof}`)
            // to the inner `.data` so consumers always see the underlying value
            // rather than the wrapper, regardless of whether the target was
            // written as a plain literal or as a signed expression.
            if let Some(data) = json_val.get("data") {
                if json_val.get("author").is_some() && json_val.get("proof").is_some() {
                    return data.clone();
                }
            }
            json_val
        } else {
            Value::String(decoded.into_owned())
        }
    } else {
        Value::String(uri.to_string())
    }
}

/// Extract numeric value from a JSON value for comparison.
pub(super) fn to_f64(val: &Value) -> Option<f64> {
    match val {
        Value::Number(n) => n.as_f64(),
        Value::String(s) => s.parse::<f64>().ok().or_else(|| iso_to_epoch_ms(s)),
        _ => None,
    }
}

/// Convert an ISO 8601 timestamp string to epoch milliseconds.
pub(super) fn iso_to_epoch_ms(s: &str) -> Option<f64> {
    chrono::DateTime::parse_from_rfc3339(s)
        .ok()
        .map(|dt| dt.timestamp_millis() as f64)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{json, Value};

    #[test]
    fn test_parse_literal_value_string() {
        assert_eq!(
            parse_literal_value("literal:string:hello%20world"),
            Value::String("hello world".to_string())
        );
    }

    #[test]
    fn test_parse_literal_value_number() {
        assert_eq!(
            parse_literal_value("literal:number:42"),
            Value::Number(42.into())
        );
        assert_eq!(
            parse_literal_value("literal:number:3.14"),
            serde_json::Number::from_f64(3.14)
                .map(Value::Number)
                .unwrap()
        );
    }

    #[test]
    fn test_parse_literal_value_boolean() {
        assert_eq!(
            parse_literal_value("literal:boolean:true"),
            Value::Bool(true)
        );
        assert_eq!(
            parse_literal_value("literal:boolean:false"),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_parse_literal_value_non_literal() {
        assert_eq!(
            parse_literal_value("recipe://Recipe"),
            Value::String("recipe://Recipe".to_string())
        );
    }

    #[test]
    fn test_parse_literal_value_plain_string() {
        let iri = format!("literal:string:{}", literal_percent_encode("active"));
        let parsed = parse_literal_value(&iri);
        assert_eq!(parsed, Value::String("active".to_string()));
    }

    #[test]
    fn test_parse_literal_value_plain_number() {
        let parsed = parse_literal_value("literal:number:42");
        assert_eq!(parsed, json!(42));
    }

    #[test]
    fn test_parse_literal_value_plain_json_object() {
        let obj = json!({"name": "Test", "count": 5});
        let obj_str = serde_json::to_string(&obj).unwrap();
        let encoded = literal_percent_encode(&obj_str);
        let iri = format!("literal:json:{encoded}");
        let parsed = parse_literal_value(&iri);
        assert_eq!(parsed, json!({"name": "Test", "count": 5}));
    }

    #[test]
    fn test_parse_literal_value_json_no_data_field() {
        let obj = json!({"name": "Test", "value": 123});
        let obj_str = serde_json::to_string(&obj).unwrap();
        let encoded = literal_percent_encode(&obj_str);
        let iri = format!("literal:json:{encoded}");
        let parsed = parse_literal_value(&iri);
        assert_eq!(parsed, obj, "Should return full JSON when no data field");
    }

    #[test]
    fn test_parse_literal_value_json_invalid_json() {
        let iri = "literal:json:not%20valid%20json";
        let parsed = parse_literal_value(iri);
        assert_eq!(parsed, Value::String("not valid json".to_string()));
    }

    #[test]
    fn test_parse_literal_value_string_with_spaces() {
        let iri = format!("literal:string:{}", literal_percent_encode("hello world"));
        let parsed = parse_literal_value(&iri);
        assert_eq!(
            parsed,
            Value::String("hello world".to_string()),
            "literal:string should decode percent-encoded content"
        );
    }

    #[test]
    fn test_parse_literal_value_number_unparseable() {
        assert_eq!(
            parse_literal_value("literal:number:not_a_number"),
            Value::String("not_a_number".to_string())
        );
    }

    #[test]
    fn test_parse_literal_value_boolean_invalid() {
        assert_eq!(
            parse_literal_value("literal:boolean:yes"),
            Value::String("yes".to_string())
        );
    }

    #[test]
    fn test_parse_literal_value_unknown_subtype() {
        assert_eq!(
            parse_literal_value("literal:unknown:foo"),
            Value::String("literal:unknown:foo".to_string())
        );
    }

    #[test]
    fn test_validate_iri_valid() {
        assert!(validate_iri("task://status").is_ok());
        assert!(validate_iri("literal:string:hello").is_ok());
        assert!(validate_iri("did:key:z6MkfR").is_ok());
    }

    #[test]
    fn test_validate_iri_rejects_injection() {
        assert!(validate_iri("task://status> . <injected://triple").is_err());
        assert!(validate_iri("<injected>").is_err());
        assert!(validate_iri("has spaces").is_err());
        assert!(validate_iri("has\"quotes").is_err());
        assert!(validate_iri("has{braces}").is_err());
    }
}

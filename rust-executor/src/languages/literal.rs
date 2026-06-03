use percent_encoding::{percent_decode_str, utf8_percent_encode, NON_ALPHANUMERIC};
use serde_json::Value as JsonValue;

use super::error::LanguageError;

/// Encode a JSON value into a literal URL expression part.
///
/// Mirrors the TypeScript `Literal.from(value).toUrl()` behavior.
/// For primitive types, uses the corresponding prefix (`string:`, `number:`, `boolean:`).
/// For objects/arrays, uses `json:` prefix with percent-encoded JSON serialization.
pub fn literal_encode(value: &JsonValue) -> String {
    match value {
        JsonValue::String(s) => {
            let encoded = utf8_percent_encode(s, NON_ALPHANUMERIC).to_string();
            format!("string:{}", encoded)
        }
        JsonValue::Number(n) => {
            format!("number:{}", n)
        }
        JsonValue::Bool(b) => {
            format!("boolean:{}", b)
        }
        _ => {
            // For objects, arrays, null — use json: prefix
            let json_str = serde_json::to_string(value).unwrap_or_default();
            let encoded = utf8_percent_encode(&json_str, NON_ALPHANUMERIC).to_string();
            format!("json:{}", encoded)
        }
    }
}

/// Decode a literal URL expression part back into a JSON value.
///
/// Mirrors the TypeScript `Literal.fromUrl("literal://<expression_part>").get()` behavior.
/// Primitives (`string:` / `number:` / `boolean:`) decode to their raw JSON value.
/// Objects decoded from `json:` payloads pass through unchanged — if they happen to
/// be a Channel-E signed-expression envelope (`{author, timestamp, data, proof}`)
/// the caller is responsible for interpreting that shape.
///
/// Note: this used to wrap primitives in a synthetic `{author: "<unknown>", …}`
/// envelope. That conflated Channel V (link-property values; provenance lives on
/// the link reifier) with Channel E (signed expressions). See
/// ~/.sovereign/membranes/coasys/research/literal-encoding-and-sparql-pushdown-2026-06-03.md
/// §9.1 (V6) for the audit.
pub fn literal_decode(expression_part: &str) -> Result<JsonValue, LanguageError> {
    let value = if let Some(rest) = expression_part.strip_prefix("string:") {
        let decoded = percent_decode_str(rest).decode_utf8().map_err(|e| {
            LanguageError::SerializationError {
                message: format!("Failed to decode literal string: {}", e),
            }
        })?;
        JsonValue::String(decoded.to_string())
    } else if let Some(rest) = expression_part.strip_prefix("number:") {
        let decoded = percent_decode_str(rest).decode_utf8().map_err(|e| {
            LanguageError::SerializationError {
                message: format!("Failed to decode literal number: {}", e),
            }
        })?;
        let num: serde_json::Number =
            decoded
                .parse()
                .map_err(|e: serde_json::Error| LanguageError::SerializationError {
                    message: format!("Failed to parse literal number: {}", e),
                })?;
        JsonValue::Number(num)
    } else if let Some(rest) = expression_part.strip_prefix("boolean:") {
        let decoded = percent_decode_str(rest).decode_utf8().map_err(|e| {
            LanguageError::SerializationError {
                message: format!("Failed to decode literal boolean: {}", e),
            }
        })?;
        let b: bool = decoded.parse().map_err(|e: std::str::ParseBoolError| {
            LanguageError::SerializationError {
                message: format!("Failed to parse literal boolean: {}", e),
            }
        })?;
        JsonValue::Bool(b)
    } else if let Some(rest) = expression_part.strip_prefix("json:") {
        let decoded = percent_decode_str(rest).decode_utf8().map_err(|e| {
            LanguageError::SerializationError {
                message: format!("Failed to decode literal JSON: {}", e),
            }
        })?;
        serde_json::from_str(&decoded).map_err(|e| LanguageError::SerializationError {
            message: format!("Failed to parse literal JSON: {}", e),
        })?
    } else {
        // Fallback: try to percent-decode and parse as JSON
        let decoded = percent_decode_str(expression_part)
            .decode_utf8()
            .map_err(|e| LanguageError::SerializationError {
                message: format!("Failed to decode literal: {}", e),
            })?;
        serde_json::from_str(&decoded).unwrap_or(JsonValue::String(decoded.to_string()))
    };

    // Return the decoded value as-is. Objects (which may be legitimate signed-expression
    // envelopes from Channel E callers) pass through unchanged; primitives are returned
    // raw — the synthetic-envelope wrapper that used to live here was removed as part of
    // the Channel V / Channel E separation.
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_decode_string() {
        let value = JsonValue::String("hello world".to_string());
        let encoded = literal_encode(&value);
        assert!(encoded.starts_with("string:"));
        let decoded = literal_decode(&encoded).unwrap();
        // Primitives now round-trip as raw values (no synthetic envelope).
        assert_eq!(decoded, value);
    }

    #[test]
    fn test_encode_decode_number() {
        let value = serde_json::json!(42);
        let encoded = literal_encode(&value);
        assert!(encoded.starts_with("number:"));
        let decoded = literal_decode(&encoded).unwrap();
        assert_eq!(decoded, value);
    }

    #[test]
    fn test_encode_decode_boolean() {
        let value = JsonValue::Bool(true);
        let encoded = literal_encode(&value);
        assert!(encoded.starts_with("boolean:"));
        let decoded = literal_decode(&encoded).unwrap();
        assert_eq!(decoded, value);
    }

    #[test]
    fn test_encode_decode_json_object() {
        let value = serde_json::json!({"key": "value", "num": 123});
        let encoded = literal_encode(&value);
        assert!(encoded.starts_with("json:"));
        let decoded = literal_decode(&encoded).unwrap();
        // Objects are returned as-is (not wrapped)
        assert_eq!(decoded, value);
    }
}

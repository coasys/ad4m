use percent_encoding::{percent_decode_str, utf8_percent_encode, AsciiSet, NON_ALPHANUMERIC};
use serde_json::Value as JsonValue;

use super::error::LanguageError;

/// Percent-encoding set matching JS `encodeRFC3986URIComponent`, which the
/// SDK's `Literal.toUrl()` uses: everything but `A-Z a-z 0-9 - _ . ~` is
/// escaped. The store renders `literal:string:` / `literal:json:` targets with
/// the same set.
pub(crate) const RFC3986_COMPONENT_ENCODE: AsciiSet = NON_ALPHANUMERIC
    .remove(b'-')
    .remove(b'_')
    .remove(b'.')
    .remove(b'~');

/// Encode a JSON value into a literal URL expression part.
///
/// Mirrors the TypeScript `Literal.from(value).toUrl()` behavior, including
/// its percent-encoding set.
/// For primitive types, uses the corresponding prefix (`string:`, `number:`, `boolean:`).
/// For objects/arrays, uses `json:` prefix with percent-encoded JSON serialization.
pub fn literal_encode(value: &JsonValue) -> String {
    match value {
        JsonValue::String(s) => {
            let encoded = utf8_percent_encode(s, &RFC3986_COMPONENT_ENCODE).to_string();
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
            let encoded = utf8_percent_encode(&json_str, &RFC3986_COMPONENT_ENCODE).to_string();
            format!("json:{}", encoded)
        }
    }
}

/// Percent-decode `input` and validate it as UTF-8. The decode error
/// becomes a `LanguageError::SerializationError` whose message is
/// `error_prefix` plus `: {error}` — each `literal_decode` branch passes
/// its own prefix.
fn decode_literal_component<'a>(
    input: &'a str,
    error_prefix: &'static str,
) -> Result<std::borrow::Cow<'a, str>, LanguageError> {
    percent_decode_str(input)
        .decode_utf8()
        .map_err(|e| LanguageError::SerializationError {
            message: format!("{}: {}", error_prefix, e),
        })
}

/// Decode a literal URL expression part back into a JSON value.
///
/// Mirrors the TypeScript `Literal.fromUrl("literal:<expression_part>").get()`.
/// `string:` / `number:` / `boolean:` decode to their typed primitive. `json:`
/// payloads are returned as the decoded JSON value verbatim — if the payload
/// happens to be a signed-expression envelope (`{author, timestamp, data, proof}`),
/// it is the caller's job to interpret that shape.
pub fn literal_decode(expression_part: &str) -> Result<JsonValue, LanguageError> {
    let value = if let Some(rest) = expression_part.strip_prefix("string:") {
        let decoded = decode_literal_component(rest, "Failed to decode literal string")?;
        JsonValue::String(decoded.to_string())
    } else if let Some(rest) = expression_part.strip_prefix("number:") {
        let decoded = decode_literal_component(rest, "Failed to decode literal number")?;
        let num: serde_json::Number =
            decoded
                .parse()
                .map_err(|e: serde_json::Error| LanguageError::SerializationError {
                    message: format!("Failed to parse literal number: {}", e),
                })?;
        JsonValue::Number(num)
    } else if let Some(rest) = expression_part.strip_prefix("boolean:") {
        let decoded = decode_literal_component(rest, "Failed to decode literal boolean")?;
        let b: bool = decoded.parse().map_err(|e: std::str::ParseBoolError| {
            LanguageError::SerializationError {
                message: format!("Failed to parse literal boolean: {}", e),
            }
        })?;
        JsonValue::Bool(b)
    } else if let Some(rest) = expression_part.strip_prefix("json:") {
        let decoded = decode_literal_component(rest, "Failed to decode literal JSON")?;
        serde_json::from_str(&decoded).map_err(|e| LanguageError::SerializationError {
            message: format!("Failed to parse literal JSON: {}", e),
        })?
    } else {
        // Fallback: try to percent-decode and parse as JSON
        let decoded = decode_literal_component(expression_part, "Failed to decode literal")?;
        serde_json::from_str(&decoded).unwrap_or(JsonValue::String(decoded.to_string()))
    };

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

    /// Same bytes as the SDK's `Literal.from(v).toUrl()`
    /// (`encodeRFC3986URIComponent`): `-_.~` stay bare.
    #[test]
    fn test_encode_matches_the_sdk_encoding() {
        assert_eq!(
            literal_encode(&JsonValue::String("board://status a-b_c.d~e".into())),
            "string:board%3A%2F%2Fstatus%20a-b_c.d~e"
        );
        assert_eq!(
            literal_encode(&serde_json::json!({ "a": "x-y" })),
            "json:%7B%22a%22%3A%22x-y%22%7D"
        );
    }

    /// `%FF` decodes to a lone invalid UTF-8 byte; every branch rejects it
    /// with its own message prefix (the `json:` branch capitalizes JSON)
    /// rather than falling back to a string value.
    #[test]
    fn test_decode_invalid_utf8_error_per_prefix() {
        let cases: [(&str, &str); 5] = [
            ("string:%FF", "Failed to decode literal string"),
            ("number:%FF", "Failed to decode literal number"),
            ("boolean:%FF", "Failed to decode literal boolean"),
            ("json:%FF", "Failed to decode literal JSON"),
            ("%FF", "Failed to decode literal"),
        ];
        for (input, prefix) in cases {
            let err = literal_decode(input).unwrap_err().to_string();
            assert_eq!(
                err,
                format!(
                    "Serialization error: {}: invalid utf-8 sequence of 1 bytes from index 0",
                    prefix
                ),
                "input {input:?}"
            );
        }
    }

    /// A malformed `%` escape is not an error: percent-decoding leaves it
    /// as literal text, so `string:` yields it and the unprefixed fallback
    /// keeps it as a string.
    #[test]
    fn test_decode_malformed_escape_stays_literal_text() {
        assert_eq!(
            literal_decode("string:%ZZ").unwrap(),
            JsonValue::String("%ZZ".to_string())
        );
        assert_eq!(
            literal_decode("%ZZ").unwrap(),
            JsonValue::String("%ZZ".to_string())
        );
    }

    /// The unprefixed fallback parses the decoded text as JSON when it can
    /// and keeps it as a string when it cannot.
    #[test]
    fn test_decode_unprefixed_json_or_string_fallback() {
        assert_eq!(
            literal_decode("%7B%22a%22%3A1%7D").unwrap(),
            serde_json::json!({"a": 1})
        );
        assert_eq!(
            literal_decode("hello%20world").unwrap(),
            JsonValue::String("hello world".to_string())
        );
    }

    /// Typed branches keep their own parse errors — they do not fall back
    /// to string values on undecodable-but-valid-UTF-8 input.
    #[test]
    fn test_decode_typed_parse_errors() {
        let err = literal_decode("number:nope").unwrap_err().to_string();
        assert!(
            err.starts_with("Serialization error: Failed to parse literal number: "),
            "{err}"
        );

        let err = literal_decode("boolean:TRUE").unwrap_err().to_string();
        assert_eq!(
            err,
            "Serialization error: Failed to parse literal boolean: \
             provided string was not `true` or `false`"
        );

        let err = literal_decode("json:nope").unwrap_err().to_string();
        assert!(
            err.starts_with("Serialization error: Failed to parse literal JSON: "),
            "{err}"
        );
    }
}

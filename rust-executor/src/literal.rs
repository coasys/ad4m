//! Literal URL encoding/decoding
//!
//! Mirrors the TypeScript `Literal` class from `core/src/Literal.ts`.
//! Handles `literal://string:X`, `literal://number:X`, `literal://json:X` URLs.

/// Parse a `literal://` URL and return the decoded value as a string.
///
/// Handles:
/// - `literal://string:X` → URL-decoded X
/// - `literal://number:X` → X as string
/// - `literal://json:X` → URL-decoded JSON string (may contain signed expressions)
/// - For signed expressions with a `data` field, extracts the inner data
/// - Non-literal URLs are returned as-is
pub fn decode_literal(url: &str) -> String {
    if !url.starts_with("literal://") {
        return url.to_string();
    }

    let body = &url[10..]; // strip "literal://"

    if let Some(value) = body.strip_prefix("string:") {
        return urlencoding::decode(value)
            .unwrap_or_else(|_| value.into())
            .to_string();
    }

    if let Some(value) = body.strip_prefix("number:") {
        return value.to_string();
    }

    if let Some(json_part) = body.strip_prefix("json:") {
        let decoded = urlencoding::decode(json_part)
            .unwrap_or_else(|_| json_part.into())
            .to_string();

        // Try to extract "data" field from signed expressions
        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&decoded) {
            if let Some(data) = parsed.get("data") {
                if let Some(s) = data.as_str() {
                    // Strip HTML tags for plain text extraction
                    let stripped = strip_html_tags(s);
                    return stripped;
                }
                return data.to_string();
            }
        }

        return decoded;
    }

    // Unknown literal type — return body as-is
    body.to_string()
}

/// Encode a string value as a `literal://string:X` URL
pub fn encode_literal_string(value: &str) -> String {
    format!("literal://string:{}", urlencoding::encode(value))
}

/// Simple HTML tag stripping (for extracting text from rich text fields)
fn strip_html_tags(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut in_tag = false;
    for c in s.chars() {
        match c {
            '<' => in_tag = true,
            '>' => in_tag = false,
            _ if !in_tag => result.push(c),
            _ => {}
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_string() {
        assert_eq!(decode_literal("literal://string:hello"), "hello");
        assert_eq!(
            decode_literal("literal://string:hello%20world"),
            "hello world"
        );
    }

    #[test]
    fn test_decode_number() {
        assert_eq!(decode_literal("literal://number:42"), "42");
    }

    #[test]
    fn test_non_literal() {
        assert_eq!(decode_literal("https://example.com"), "https://example.com");
    }

    #[test]
    fn test_encode_string() {
        assert_eq!(encode_literal_string("hello"), "literal://string:hello");
    }
}

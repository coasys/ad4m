//! RFC 8785 JSON Canonicalization Scheme (JCS), restricted to the values a chain entry may hold.
//!
//! A hashed or signed object holds no floating-point number and no integer outside
//! ±(2^53 − 1) (AR-01 4.2). With floats refused, JCS reduces to: no whitespace, object
//! members sorted by the UTF-16 code units of their names, strings escaped as ECMAScript's
//! `JSON.stringify` escapes them, integers in plain decimal.

use serde_json::{Map, Value};

/// The largest integer a canonical value may hold: 2^53 − 1 (`Number.MAX_SAFE_INTEGER`).
pub const MAX_SAFE_INTEGER: u64 = (1 << 53) - 1;

/// Why a value has no canonical form.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum CanonError {
    #[error("floating-point number {0} in a canonical object")]
    Float(String),
    #[error("integer {0} is outside ±(2^53 − 1)")]
    Integer(String),
    #[error("an entry must be a JSON object")]
    NotObject,
    #[error("serialisation failed: {0}")]
    Serde(String),
}

/// The JCS text of `value`.
pub fn canonical_json(value: &Value) -> Result<String, CanonError> {
    let mut out = String::new();
    write_value(&mut out, value)?;
    Ok(out)
}

/// The JCS text of an object, leaving out the top-level member `skip`.
pub(crate) fn canonical_object_without(
    map: &Map<String, Value>,
    skip: &str,
) -> Result<String, CanonError> {
    let mut out = String::new();
    write_object(&mut out, map, Some(skip))?;
    Ok(out)
}

fn write_value(out: &mut String, value: &Value) -> Result<(), CanonError> {
    match value {
        Value::Null => out.push_str("null"),
        Value::Bool(true) => out.push_str("true"),
        Value::Bool(false) => out.push_str("false"),
        Value::Number(n) => {
            if let Some(u) = n.as_u64() {
                if u > MAX_SAFE_INTEGER {
                    return Err(CanonError::Integer(n.to_string()));
                }
                out.push_str(&u.to_string());
            } else if let Some(i) = n.as_i64() {
                if i.unsigned_abs() > MAX_SAFE_INTEGER {
                    return Err(CanonError::Integer(n.to_string()));
                }
                out.push_str(&i.to_string());
            } else {
                return Err(CanonError::Float(n.to_string()));
            }
        }
        Value::String(s) => write_string(out, s),
        Value::Array(items) => {
            out.push('[');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(out, item)?;
            }
            out.push(']');
        }
        Value::Object(map) => write_object(out, map, None)?,
    }
    Ok(())
}

fn write_object(
    out: &mut String,
    map: &Map<String, Value>,
    skip: Option<&str>,
) -> Result<(), CanonError> {
    let mut members: Vec<(Vec<u16>, &String, &Value)> = map
        .iter()
        .filter(|(k, _)| Some(k.as_str()) != skip)
        .map(|(k, v)| (k.encode_utf16().collect(), k, v))
        .collect();
    members.sort_by(|a, b| a.0.cmp(&b.0));
    out.push('{');
    for (i, (_, key, value)) in members.into_iter().enumerate() {
        if i > 0 {
            out.push(',');
        }
        write_string(out, key);
        out.push(':');
        write_value(out, value)?;
    }
    out.push('}');
    Ok(())
}

fn write_string(out: &mut String, s: &str) {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0c}' => out.push_str("\\f"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                let b = c as u32 as usize;
                out.push_str("\\u00");
                out.push(HEX[b >> 4] as char);
                out.push(HEX[b & 0xf] as char);
            }
            c => out.push(c),
        }
    }
    out.push('"');
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn sorts_members_by_utf16_code_units() {
        // RFC 8785 3.2.3: the sort example (values replaced by integers).
        let v: Value = serde_json::from_str(
            r#"{"\u20ac":1,"\r":2,"\ufb33":3,"1":4,"\ud83d\ude00":5,"\u0080":6,"\u00f6":7}"#,
        )
        .unwrap();
        assert_eq!(
            canonical_json(&v).unwrap(),
            "{\"\\r\":2,\"1\":4,\"\u{80}\":6,\"\u{f6}\":7,\"\u{20ac}\":1,\"\u{1f600}\":5,\"\u{fb33}\":3}"
        );
    }

    #[test]
    fn escapes_strings_like_json_stringify() {
        // RFC 8785 3.2.2.2 string example.
        let v: Value =
            serde_json::from_str(r#""\u20ac$\u000F\u000aA'\u0042\u0022\u005c\\\"\/""#).unwrap();
        assert_eq!(
            canonical_json(&v).unwrap(),
            "\"\u{20ac}$\\u000f\\nA'B\\\"\\\\\\\\\\\"/\""
        );
        let v = json!("\u{8}\u{c}\t\r\u{1f}\u{7f}\u{2028}");
        assert_eq!(
            canonical_json(&v).unwrap(),
            "\"\\b\\f\\t\\r\\u001f\u{7f}\u{2028}\""
        );
    }

    #[test]
    fn writes_no_whitespace_and_nested_values() {
        let v = json!({"b": [1, true, null, {"z": -5, "a": "x"}], "a": {}});
        assert_eq!(
            canonical_json(&v).unwrap(),
            r#"{"a":{},"b":[1,true,null,{"a":"x","z":-5}]}"#
        );
    }

    #[test]
    fn refuses_floats_and_unsafe_integers() {
        assert!(matches!(
            canonical_json(&json!({"a": 1.5})),
            Err(CanonError::Float(_))
        ));
        assert!(matches!(
            canonical_json(&json!(1.0)),
            Err(CanonError::Float(_))
        ));
        assert_eq!(
            canonical_json(&json!(MAX_SAFE_INTEGER)).unwrap(),
            "9007199254740991"
        );
        assert!(matches!(
            canonical_json(&json!(MAX_SAFE_INTEGER + 1)),
            Err(CanonError::Integer(_))
        ));
        assert!(matches!(
            canonical_json(&json!(-(MAX_SAFE_INTEGER as i64) - 1)),
            Err(CanonError::Integer(_))
        ));
        assert_eq!(
            canonical_json(&json!(-(MAX_SAFE_INTEGER as i64))).unwrap(),
            "-9007199254740991"
        );
    }

    #[test]
    fn skips_only_the_top_level_member() {
        let v = json!({"proof": [1], "a": {"proof": 2}});
        assert_eq!(
            canonical_object_without(v.as_object().unwrap(), "proof").unwrap(),
            r#"{"a":{"proof":2}}"#
        );
    }
}

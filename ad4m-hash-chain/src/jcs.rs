//! RFC 8785 JSON Canonicalization Scheme (JCS), restricted to the values a chain entry may hold.
//!
//! A hashed or signed object holds no floating-point number and no integer outside
//! ±(2^53 − 1) (section 2 of `docs-src/identity/did-scid-coasys-1.md`). With floats refused,
//! JCS reduces to: no whitespace, object members sorted by the UTF-16 code units of their
//! names, strings escaped as ECMAScript's `JSON.stringify` escapes them, integers in plain
//! decimal. A received text is accepted only when it is already that form (`entry_from_json`).

use serde::de::DeserializeOwned;
use serde::ser::{self, Serialize};
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
    /// A received text is not the JCS text of the entry it parses into (whitespace, member
    /// order, a duplicate member name, a `null` the entry type drops, `1e2`, `-0`, …).
    #[error("the text is not the canonical form of the entry it parses into")]
    NotCanonical,
}

/// The JSON value of a typed entry. Refuses every float before the conversion, so a
/// non-finite float (which `serde_json::to_value` turns into `null`) is refused too.
pub(crate) fn to_value<T: Serialize + ?Sized>(value: &T) -> Result<Value, CanonError> {
    value.serialize(FloatCheck).map_err(|e| match e {
        Refused::Float(f) => CanonError::Float(f),
        Refused::Other(s) => CanonError::Serde(s),
    })?;
    serde_json::to_value(value).map_err(|e| CanonError::Serde(e.to_string()))
}

/// Parses `text` as an entry of type `T`, and refuses it unless `text` is exactly the JCS
/// text of the parsed value. So one accepted text has one parse and one hash, whatever the
/// target: no member the type drops, no float, no duplicate member name, no other spelling.
pub(crate) fn from_canonical_text<T: Serialize + DeserializeOwned>(
    text: &str,
) -> Result<T, CanonError> {
    let parsed: T = serde_json::from_str(text).map_err(|e| CanonError::Serde(e.to_string()))?;
    let value = to_value(&parsed)?;
    if !value.is_object() {
        return Err(CanonError::NotObject);
    }
    if canonical_json(&value)? != text {
        return Err(CanonError::NotCanonical);
    }
    Ok(parsed)
}

#[derive(Debug)]
enum Refused {
    Float(String),
    Other(String),
}

impl std::fmt::Display for Refused {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Refused::Float(x) | Refused::Other(x) => f.write_str(x),
        }
    }
}

impl std::error::Error for Refused {}

impl ser::Error for Refused {
    fn custom<M: std::fmt::Display>(msg: M) -> Self {
        Refused::Other(msg.to_string())
    }
}

/// A serializer that writes nothing and fails on the first `f32` or `f64`.
struct FloatCheck;

macro_rules! accept {
    ($($name:ident: $ty:ty),*) => {
        $(fn $name(self, _: $ty) -> Result<(), Refused> { Ok(()) })*
    };
}

impl ser::Serializer for FloatCheck {
    type Ok = ();
    type Error = Refused;
    type SerializeSeq = FloatCheck;
    type SerializeTuple = FloatCheck;
    type SerializeTupleStruct = FloatCheck;
    type SerializeTupleVariant = FloatCheck;
    type SerializeMap = FloatCheck;
    type SerializeStruct = FloatCheck;
    type SerializeStructVariant = FloatCheck;

    accept!(serialize_bool: bool, serialize_i8: i8, serialize_i16: i16, serialize_i32: i32,
        serialize_i64: i64, serialize_i128: i128, serialize_u8: u8, serialize_u16: u16,
        serialize_u32: u32, serialize_u64: u64, serialize_u128: u128, serialize_char: char,
        serialize_str: &str, serialize_bytes: &[u8], serialize_unit_struct: &'static str);

    fn serialize_f32(self, v: f32) -> Result<(), Refused> {
        Err(Refused::Float(v.to_string()))
    }
    fn serialize_f64(self, v: f64) -> Result<(), Refused> {
        Err(Refused::Float(v.to_string()))
    }
    fn serialize_none(self) -> Result<(), Refused> {
        Ok(())
    }
    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<(), Refused> {
        value.serialize(self)
    }
    fn serialize_unit(self) -> Result<(), Refused> {
        Ok(())
    }
    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
    ) -> Result<(), Refused> {
        Ok(())
    }
    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _: &'static str,
        value: &T,
    ) -> Result<(), Refused> {
        value.serialize(self)
    }
    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        value: &T,
    ) -> Result<(), Refused> {
        value.serialize(self)
    }
    fn serialize_seq(self, _: Option<usize>) -> Result<FloatCheck, Refused> {
        Ok(self)
    }
    fn serialize_tuple(self, _: usize) -> Result<FloatCheck, Refused> {
        Ok(self)
    }
    fn serialize_tuple_struct(self, _: &'static str, _: usize) -> Result<FloatCheck, Refused> {
        Ok(self)
    }
    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<FloatCheck, Refused> {
        Ok(self)
    }
    fn serialize_map(self, _: Option<usize>) -> Result<FloatCheck, Refused> {
        Ok(self)
    }
    fn serialize_struct(self, _: &'static str, _: usize) -> Result<FloatCheck, Refused> {
        Ok(self)
    }
    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<FloatCheck, Refused> {
        Ok(self)
    }
}

macro_rules! compound {
    ($($trait:ident :: $method:ident),*) => {
        $(impl ser::$trait for FloatCheck {
            type Ok = ();
            type Error = Refused;
            fn $method<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Refused> {
                value.serialize(FloatCheck)
            }
            fn end(self) -> Result<(), Refused> {
                Ok(())
            }
        })*
    };
}

compound!(
    SerializeSeq::serialize_element,
    SerializeTuple::serialize_element,
    SerializeTupleStruct::serialize_field,
    SerializeTupleVariant::serialize_field
);

impl ser::SerializeMap for FloatCheck {
    type Ok = ();
    type Error = Refused;
    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Refused> {
        key.serialize(FloatCheck)
    }
    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Refused> {
        value.serialize(FloatCheck)
    }
    fn end(self) -> Result<(), Refused> {
        Ok(())
    }
}

impl ser::SerializeStruct for FloatCheck {
    type Ok = ();
    type Error = Refused;
    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        _: &'static str,
        value: &T,
    ) -> Result<(), Refused> {
        value.serialize(FloatCheck)
    }
    fn end(self) -> Result<(), Refused> {
        Ok(())
    }
}

impl ser::SerializeStructVariant for FloatCheck {
    type Ok = ();
    type Error = Refused;
    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        _: &'static str,
        value: &T,
    ) -> Result<(), Refused> {
        value.serialize(FloatCheck)
    }
    fn end(self) -> Result<(), Refused> {
        Ok(())
    }
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

//! SHACL-derived validation for instance writes.
//!
//! Every write is checked against the class shape *before* anything is written; a rejection
//! names the property, the problem, the expected type, the cardinality and (for `sh:in`) the
//! allowed values, so the caller can fix its call in one round trip. The errors are data: MCP
//! and the RPC write their own text from them.

use super::{class_properties, find_property, PropView};
use crate::perspectives::interpretation::class_local_name;
use crate::perspectives::model_query::is_safe_iri_target;
use crate::perspectives::model_query::types::ModelShape;
use serde::Serialize;
use serde_json::{json, Map, Value};
use ts_rs::TS;

/// One rejected property.
#[derive(Debug, Clone, Serialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ValidationError {
    /// The property as the class names it, or as the caller wrote it when the class has none.
    pub property: String,
    pub problem: ValidationProblem,
    /// The property's type: `string`, `boolean`, `integer`, `number`, `datetime`, `reference`,
    /// or a custom datatype's local name. `None` for an unknown property.
    pub expected_type: Option<String>,
    /// `None` for an unknown property.
    pub cardinality: Option<Cardinality>,
    /// The property's `sh:in` values, in declared order, when it has a list.
    pub allowed_values: Option<Vec<Value>>,
    /// The value the caller sent, when there was one.
    pub received: Option<Value>,
    /// For a collection item, its index in the array the caller sent.
    pub item: Option<usize>,
}

/// What is wrong with a property.
#[derive(Debug, Clone, Serialize, PartialEq, Eq, TS)]
#[serde(
    tag = "kind",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
#[ts(export)]
pub enum ValidationProblem {
    /// The class has no such property; `available` lists the writable ones.
    UnknownProperty {
        class: String,
        available: Vec<String>,
    },
    /// The class has no such collection; `available` lists the writable ones.
    UnknownCollection {
        class: String,
        available: Vec<String>,
    },
    /// The property cannot be written.
    ReadOnly { reason: ReadOnlyReason },
    /// An update named a collection: its items change one by one.
    CollectionNotSettable,
    /// A collection write named a single-valued property.
    NotACollection,
    /// A single-valued property got an array.
    ArrayForScalar { length: usize },
    /// The value is `null`: omit the property instead.
    Null,
    /// A reference needs the URI of an instance of `targetClass`.
    NotAReference { target_class: String },
    /// The value is not of the property's type (`expectedType`).
    WrongType,
    /// The value is not one of `allowedValues` (`sh:in`).
    NotAllowed,
    /// A create left out a required property.
    MissingRequired,
}

/// Why a property cannot be written.
#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum ReadOnlyReason {
    /// A class marker, set by the constructor on create.
    ClassMarker,
    /// A reverse relation, stored on the target instance.
    ReverseRelation,
    /// Computed by a getter expression.
    Computed,
    /// The class declares no setter for it.
    NoSetter,
}

/// How many values a property takes: `min` 0 or 1, `max` 1 or none (a collection).
#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq, TS)]
#[ts(export)]
pub struct Cardinality {
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriteMode {
    /// All required properties must be present; collections may be given as arrays.
    Create,
    /// Partial patch of single-valued properties only.
    Update,
}

/// A validated, coerced write: canonical property names, values in the JSON type the property
/// declares.
#[derive(Debug, Default, PartialEq)]
pub struct ValidatedWrite {
    pub scalars: Map<String, Value>,
    pub collections: Vec<(String, Vec<Value>)>,
}

impl ValidationError {
    /// An error about `info`, carrying its type, cardinality and allowed values.
    fn on(info: &PropView<'_>, problem: ValidationProblem, received: Option<&Value>) -> Self {
        ValidationError {
            property: info.name().to_string(),
            problem,
            expected_type: Some(info.type_name()),
            cardinality: Some(info.cardinality()),
            allowed_values: info
                .in_values()
                .map(|entries| entries.iter().map(|e| allowed_value(e).clone()).collect()),
            received: received.cloned(),
            item: None,
        }
    }
}

/// The value an `sh:in` entry stands for: its `value` key when it is an object, else the entry
/// itself.
pub(crate) fn allowed_value(entry: &Value) -> &Value {
    entry.get("value").unwrap_or(entry)
}

/// A JSON value as the text `sh:in` compares on: strings by content, numbers and booleans by
/// their JSON text (`"todo"` → `todo`, `3` → `3`, `true` → `true`). Anything else (objects,
/// null) uses its JSON encoding.
pub(crate) fn value_text(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Number(n) => number_text(n),
        Value::Bool(b) => b.to_string(),
        other => serde_json::to_string(other).unwrap_or_default(),
    }
}

/// A number as JavaScript's `String(n)` writes it: a whole float is printed without a fraction
/// (`3.0` -> `3`), so a `number` property coerced from the text `"3"` matches the option `3`
/// that WE writes with `String(3)`.
fn number_text(n: &serde_json::Number) -> String {
    match n.as_f64() {
        Some(f)
            if n.is_f64()
                && f.is_finite()
                && f.fract() == 0.0
                && f.abs() < 9_007_199_254_740_992.0 =>
        {
            format!("{}", f as i64)
        }
        _ => n.to_string(),
    }
}

/// Compare as the SDK writes them: the coerced candidate against one allowed value, both turned
/// into text and compared exactly. No trimming, no case folding — WE writes option values with
/// `String(v)` (W1), and a number property may still be sent as a number.
fn matches_allowed(candidate: &Value, allowed: &Value) -> bool {
    value_text(candidate) == value_text(allowed)
}

/// Coerce one scalar value to the property's declared type, or explain why it can't be.
/// Lenient where the intent is unambiguous ("true" for a boolean, "42" for an integer, 3 for a
/// string), strict where it isn't.
pub(crate) fn coerce_scalar(info: &PropView<'_>, value: &Value) -> Result<Value, ValidationError> {
    let err = |problem: ValidationProblem| ValidationError::on(info, problem, Some(value));

    if let Value::Array(items) = value {
        return Err(err(ValidationProblem::ArrayForScalar {
            length: items.len(),
        }));
    }
    if value.is_null() {
        return Err(err(ValidationProblem::Null));
    }

    if let Some((_, target)) = info.relation() {
        return match value {
            Value::String(s) if is_safe_iri_target(s) => Ok(value.clone()),
            _ => Err(err(ValidationProblem::NotAReference {
                target_class: target.to_string(),
            })),
        };
    }

    // Stored through a language as an expression: the language takes the value as it is, and
    // checks it itself when the setter creates the expression.
    if info.stored_by_language().is_some() {
        return Ok(value.clone());
    }

    let wrong_type = || err(ValidationProblem::WrongType);
    let coerced = match info.type_name().as_str() {
        "boolean" => match value {
            Value::Bool(_) => Ok(value.clone()),
            Value::String(s) if s.eq_ignore_ascii_case("true") => Ok(json!(true)),
            Value::String(s) if s.eq_ignore_ascii_case("false") => Ok(json!(false)),
            _ => Err(wrong_type()),
        },
        "integer" => match value {
            Value::Number(n) if n.is_i64() || n.is_u64() => Ok(value.clone()),
            // A whole float in i64 range: `as` saturates, so 1e20 would become i64::MAX.
            // `i64::MAX as f64` is 2^63, itself out of range, hence `<`.
            Value::Number(n) => match n.as_f64() {
                Some(f) if f.fract() == 0.0 && f >= i64::MIN as f64 && f < i64::MAX as f64 => {
                    Ok(json!(f as i64))
                }
                _ => Err(wrong_type()),
            },
            Value::String(s) => s
                .trim()
                .parse::<i64>()
                .map(|i| json!(i))
                .map_err(|_| wrong_type()),
            _ => Err(wrong_type()),
        },
        "number" => match value {
            Value::Number(_) => Ok(value.clone()),
            Value::String(s) => s
                .trim()
                .parse::<f64>()
                .ok()
                .and_then(|f| serde_json::Number::from_f64(f).map(Value::Number))
                .ok_or_else(wrong_type),
            _ => Err(wrong_type()),
        },
        "string" | "datetime" => match value {
            Value::String(_) => Ok(value.clone()),
            Value::Number(n) => Ok(Value::String(n.to_string())),
            Value::Bool(b) => Ok(Value::String(b.to_string())),
            _ => Err(wrong_type()),
        },
        // Unknown / custom datatype: accept any single JSON value verbatim.
        _ => Ok(value.clone()),
    }?;

    // `sh:in`: the coerced value must match one declared entry. Runs after the type coercion so
    // "42" for an integer property matches entry `42`; the relation and language-stored early
    // returns above keep it off properties whose values are URIs or language expressions (no
    // class in WE puts `options` on those). An empty list allows nothing.
    if let Some(allowed) = info.in_values() {
        if !allowed
            .iter()
            .any(|entry| matches_allowed(&coerced, allowed_value(entry)))
        {
            return Err(err(ValidationProblem::NotAllowed));
        }
    }
    Ok(coerced)
}

/// Validate a `properties` map against a class shape.
///
/// On success returns the coerced scalar values (keyed by the property's canonical name) and,
/// for `Create`, the collection items to link. On failure returns *every* problem found, not
/// just the first, so the caller can fix its call in one round trip. Nothing is written by this
/// function.
pub fn validate_properties(
    shape: &ModelShape,
    props: &Map<String, Value>,
    mode: WriteMode,
) -> Result<ValidatedWrite, Vec<ValidationError>> {
    let infos = class_properties(shape);
    let mut errors: Vec<ValidationError> = Vec::new();
    let mut out = ValidatedWrite::default();

    let available = || -> Vec<String> {
        infos
            .iter()
            .filter(|i| i.writable())
            .map(|i| i.name().to_string())
            .collect()
    };

    for (given_name, value) in props {
        let Some(info) = find_property(&infos, given_name) else {
            errors.push(ValidationError {
                property: given_name.clone(),
                problem: ValidationProblem::UnknownProperty {
                    class: class_local_name(&shape.target_class).to_string(),
                    available: available(),
                },
                expected_type: None,
                cardinality: None,
                allowed_values: None,
                received: Some(value.clone()),
                item: None,
            });
            continue;
        };

        if let Some(reason) = info.read_only() {
            errors.push(ValidationError::on(
                info,
                ValidationProblem::ReadOnly { reason },
                Some(value),
            ));
            continue;
        }

        if info.collection() {
            match mode {
                WriteMode::Update => errors.push(ValidationError::on(
                    info,
                    ValidationProblem::CollectionNotSettable,
                    Some(value),
                )),
                WriteMode::Create => {
                    let items: Vec<Value> = match value {
                        Value::Array(items) => items.clone(),
                        Value::Null => vec![],
                        other => vec![other.clone()],
                    };
                    let mut coerced = Vec::with_capacity(items.len());
                    let mut bad = false;
                    for (index, item) in items.iter().enumerate() {
                        match coerce_scalar(info, item) {
                            Ok(v) => coerced.push(v),
                            Err(mut e) => {
                                e.item = Some(index);
                                errors.push(e);
                                bad = true;
                            }
                        }
                    }
                    if !bad && !coerced.is_empty() {
                        out.collections.push((info.name().to_string(), coerced));
                    }
                }
            }
            continue;
        }

        match coerce_scalar(info, value) {
            Ok(v) => {
                out.scalars.insert(info.name().to_string(), v);
            }
            Err(e) => errors.push(e),
        }
    }

    if mode == WriteMode::Create {
        for info in infos.iter().filter(|i| i.required() && i.writable()) {
            let supplied = if info.collection() {
                out.collections.iter().any(|(n, _)| n == info.name())
            } else {
                out.scalars.contains_key(info.name())
            };
            let already_reported = errors.iter().any(|e| e.property == info.name());
            if !supplied && !already_reported {
                errors.push(ValidationError::on(
                    info,
                    ValidationProblem::MissingRequired,
                    None,
                ));
            }
        }
    }

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

/// Resolve `collection` to a writable collection property of the class, or the error that says
/// why it is not one (unknown name, single-valued property, read-only relation).
pub(crate) fn resolve_collection<'a, 'p>(
    class_name: &str,
    infos: &'a [PropView<'p>],
    collection: &str,
    item_uri: &str,
) -> Result<&'a PropView<'p>, ValidationError> {
    let Some(info) = find_property(infos, collection) else {
        return Err(ValidationError {
            property: collection.to_string(),
            problem: ValidationProblem::UnknownCollection {
                class: class_name.to_string(),
                available: infos
                    .iter()
                    .filter(|i| i.collection() && i.writable())
                    .map(|i| i.name().to_string())
                    .collect(),
            },
            expected_type: None,
            cardinality: None,
            allowed_values: None,
            received: None,
            item: None,
        });
    };
    if !info.collection() {
        return Err(ValidationError::on(
            info,
            ValidationProblem::NotACollection,
            Some(&Value::String(item_uri.to_string())),
        ));
    }
    if let Some(reason) = info.read_only() {
        return Err(ValidationError::on(
            info,
            ValidationProblem::ReadOnly { reason },
            None,
        ));
    }
    Ok(info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// A whole float compares like JavaScript prints it: `3.0` matches `3` and `"3"`; a real
    /// fraction does not.
    #[test]
    fn whole_floats_compare_like_js_numbers() {
        let three_f = Value::Number(serde_json::Number::from_f64(3.0).unwrap());
        assert!(matches_allowed(&three_f, &json!(3)));
        assert!(matches_allowed(&three_f, &json!("3")));
        assert!(matches_allowed(&json!(3), &three_f));
        let frac = Value::Number(serde_json::Number::from_f64(3.5).unwrap());
        assert!(matches_allowed(&frac, &json!("3.5")));
        assert!(!matches_allowed(&frac, &json!(3)));
    }
}

//! SHACL-derived validation for the `instance_*` write tools.
//!
//! Every write is checked against the class shape *before* anything is
//! written; a rejection names the property, the expected type and the
//! cardinality so the agent can fix its call in one round trip.

use super::{class_properties, find_property, PropView};
use crate::perspectives::interpretation::class_local_name;
use crate::perspectives::model_query::is_safe_iri_target;
use crate::perspectives::model_query::types::ModelShape;
use serde::Serialize;
use serde_json::{json, Map, Value};

/// One rejected property. Serialized into the tool's error payload.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub(crate) struct ValidationError {
    pub property: String,
    pub problem: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expected_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardinality: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub received: Option<String>,
}

impl ValidationError {
    fn summary(&self) -> String {
        let mut s = format!("{}: {}", self.property, self.problem);
        match (&self.expected_type, &self.cardinality) {
            (Some(t), Some(c)) => s.push_str(&format!(" (expected {t}, {c})")),
            (Some(t), None) => s.push_str(&format!(" (expected {t})")),
            (None, Some(c)) => s.push_str(&format!(" ({c})")),
            (None, None) => {}
        }
        if let Some(r) = &self.received {
            s.push_str(&format!(", received {r}"));
        }
        s
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WriteMode {
    /// All required properties must be present; collections may be given as arrays.
    Create,
    /// Partial patch of single-valued properties only.
    Update,
}

/// A validated, coerced write: canonical property names, values in the
/// JSON type the property declares.
#[derive(Debug, Default, PartialEq)]
pub(crate) struct ValidatedWrite {
    pub scalars: Map<String, Value>,
    pub collections: Vec<(String, Vec<Value>)>,
}

/// Short description of a JSON value for error messages — type plus a
/// truncated preview, never the whole payload.
pub(crate) fn describe_value(v: &Value) -> String {
    match v {
        Value::Null => "null".to_string(),
        Value::Bool(b) => format!("boolean {b}"),
        Value::Number(n) => format!("number {n}"),
        Value::String(s) => {
            let preview: String = s.chars().take(40).collect();
            if s.chars().count() > 40 {
                format!("string \"{preview}…\"")
            } else {
                format!("string \"{preview}\"")
            }
        }
        Value::Array(a) => format!("array of {}", a.len()),
        Value::Object(_) => "object".to_string(),
    }
}

/// Coerce one scalar value to the property's declared type, or explain why
/// it can't be. Lenient where the intent is unambiguous ("true" for a
/// boolean, "42" for an integer, 3 for a string), strict where it isn't.
pub(crate) fn coerce_scalar(info: &PropView<'_>, value: &Value) -> Result<Value, ValidationError> {
    let err = |problem: String| ValidationError {
        property: info.name().to_string(),
        problem,
        expected_type: Some(info.type_name()),
        cardinality: Some(info.cardinality_text().to_string()),
        received: Some(describe_value(value)),
    };

    if let Value::Array(items) = value {
        return Err(err(format!(
            "expects a single value but received an array of {} — pass one value",
            items.len()
        )));
    }
    if value.is_null() {
        return Err(err(
            "null is not a value — omit the property instead of passing null".to_string(),
        ));
    }

    if let Some((_, target)) = info.relation() {
        return match value {
            Value::String(s) if is_safe_iri_target(s) => Ok(value.clone()),
            _ => Err(err(format!(
                "expects the URI of an existing {target} instance (as returned by instance_query)"
            ))),
        };
    }

    match info.type_name().as_str() {
        "boolean" => match value {
            Value::Bool(_) => Ok(value.clone()),
            Value::String(s) if s.eq_ignore_ascii_case("true") => Ok(json!(true)),
            Value::String(s) if s.eq_ignore_ascii_case("false") => Ok(json!(false)),
            _ => Err(err("expects true or false".to_string())),
        },
        "integer" => match value {
            Value::Number(n) if n.is_i64() || n.is_u64() => Ok(value.clone()),
            Value::Number(n) => match n.as_f64() {
                Some(f) if f.fract() == 0.0 => Ok(json!(f as i64)),
                _ => Err(err("expects a whole number".to_string())),
            },
            Value::String(s) => s
                .trim()
                .parse::<i64>()
                .map(|i| json!(i))
                .map_err(|_| err("expects a whole number".to_string())),
            _ => Err(err("expects a whole number".to_string())),
        },
        "number" => match value {
            Value::Number(_) => Ok(value.clone()),
            Value::String(s) => s
                .trim()
                .parse::<f64>()
                .ok()
                .and_then(|f| serde_json::Number::from_f64(f).map(Value::Number))
                .ok_or_else(|| err("expects a number".to_string())),
            _ => Err(err("expects a number".to_string())),
        },
        "string" | "datetime" => match value {
            Value::String(_) => Ok(value.clone()),
            Value::Number(n) => Ok(Value::String(n.to_string())),
            Value::Bool(b) => Ok(Value::String(b.to_string())),
            _ => Err(err(format!("expects a {}", info.type_name()))),
        },
        // Unknown / custom datatype: accept any single JSON value verbatim.
        _ => Ok(value.clone()),
    }
}

/// Validate a `properties` map against a class shape.
///
/// On success returns the coerced scalar values (keyed by the property's
/// canonical name) and, for `Create`, the collection items to link. On
/// failure returns *every* problem found, not just the first, so the agent
/// can fix its call in one round trip. Nothing is written by this function.
pub(crate) fn validate_properties(
    shape: &ModelShape,
    props: &Map<String, Value>,
    mode: WriteMode,
) -> Result<ValidatedWrite, Vec<ValidationError>> {
    let infos = class_properties(shape);
    let mut errors: Vec<ValidationError> = Vec::new();
    let mut out = ValidatedWrite::default();

    let available = || -> String {
        let names: Vec<&str> = infos
            .iter()
            .filter(|i| i.writable())
            .map(|i| i.name())
            .collect();
        names.join(", ")
    };

    for (given_name, value) in props {
        let Some(info) = find_property(&infos, given_name) else {
            errors.push(ValidationError {
                property: given_name.clone(),
                problem: format!(
                    "unknown property on class '{}'. Available: {}",
                    class_local_name(&shape.target_class),
                    available()
                ),
                expected_type: None,
                cardinality: None,
                received: Some(describe_value(value)),
            });
            continue;
        };

        if let Some(reason) = info.read_only_reason() {
            errors.push(ValidationError {
                property: info.name().to_string(),
                problem: format!("read-only: {reason}"),
                expected_type: Some(info.type_name()),
                cardinality: Some(info.cardinality_text().to_string()),
                received: Some(describe_value(value)),
            });
            continue;
        }

        if info.collection() {
            match mode {
                WriteMode::Update => errors.push(ValidationError {
                    property: info.name().to_string(),
                    problem: "is a collection — instance_update only sets single-valued \
                              properties; use instance_add_to_collection to add items"
                        .to_string(),
                    expected_type: Some(info.type_name()),
                    cardinality: Some(info.cardinality_text().to_string()),
                    received: Some(describe_value(value)),
                }),
                WriteMode::Create => {
                    let items: Vec<Value> = match value {
                        Value::Array(items) => items.clone(),
                        Value::Null => vec![],
                        other => vec![other.clone()],
                    };
                    let mut coerced = Vec::with_capacity(items.len());
                    let mut bad = false;
                    for item in &items {
                        match coerce_scalar(info, item) {
                            Ok(v) => coerced.push(v),
                            Err(mut e) => {
                                e.problem = format!("collection item {}", e.problem);
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
                errors.push(ValidationError {
                    property: info.name().to_string(),
                    problem: "missing required property".to_string(),
                    expected_type: Some(info.type_name()),
                    cardinality: Some(info.cardinality_text().to_string()),
                    received: None,
                });
            }
        }
    }

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

/// Render a validation failure as the tool's JSON error payload. The
/// top-level `error` string already carries property / type / cardinality
/// so a client that only surfaces `error` still gets an actionable message.
pub(crate) fn validation_failure(class_name: &str, errors: &[ValidationError]) -> String {
    let summary: Vec<String> = errors.iter().map(ValidationError::summary).collect();
    let noun = if errors.len() == 1 {
        "problem"
    } else {
        "problems"
    };
    serde_json::to_string_pretty(&json!({
        "error": format!(
            "Validation failed for class '{}' ({} {}): {}",
            class_name,
            errors.len(),
            noun,
            summary.join("; ")
        ),
        "class_name": class_name,
        "validation_errors": errors,
        "hint": "Call describe_perspective for the full schema of this class.",
    }))
    .unwrap_or_else(|e| format!("Error: {e}"))
}

/// Recursively check a `filter` (where clause) against the class shape:
/// unknown property names are rejected (a silent no-match is the worst
/// outcome for an agent), and property-name case is canonicalized so
/// `{"Name": …}` matches the declared `name`.
pub(crate) fn normalize_filter(
    shape: &ModelShape,
    filter: &Map<String, Value>,
) -> Result<Map<String, Value>, String> {
    let infos = class_properties(shape);
    let mut out = Map::new();
    for (key, value) in filter {
        match key.as_str() {
            "OR" | "AND" => {
                let Value::Array(branches) = value else {
                    return Err(format!("filter key '{key}' must be an array of clauses"));
                };
                let mut fixed = Vec::with_capacity(branches.len());
                for b in branches {
                    let Value::Object(m) = b else {
                        return Err(format!("filter key '{key}' entries must be objects"));
                    };
                    fixed.push(Value::Object(normalize_filter(shape, m)?));
                }
                out.insert(key.clone(), Value::Array(fixed));
            }
            "NOT" => {
                let Value::Object(m) = value else {
                    return Err("filter key 'NOT' must be an object".to_string());
                };
                out.insert(key.clone(), Value::Object(normalize_filter(shape, m)?));
            }
            "id" | "base" => {
                out.insert(key.clone(), value.clone());
            }
            _ => {
                let Some(info) = find_property(&infos, key) else {
                    let names: Vec<&str> = infos.iter().map(|i| i.name()).collect();
                    return Err(format!(
                        "unknown filter property '{}' on class '{}'. Available: id, {}",
                        key,
                        class_local_name(&shape.target_class),
                        names.join(", ")
                    ));
                };
                out.insert(info.name().to_string(), value.clone());
            }
        }
    }
    Ok(out)
}

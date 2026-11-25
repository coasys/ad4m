//! The text of the `instance_*` write tools' validation failures.
//!
//! The checks are [`crate::perspectives::instances::validate`], shared with the RPC; this file
//! words each [`shared::ValidationError`] as the tools always have: the property, the problem,
//! the expected type and the cardinality, so the agent can fix its call in one round trip.

use super::{cardinality_text, class_properties, find_property, read_only_text};
use crate::perspectives::instances::validate::{self as shared, value_text, ValidationProblem};
use crate::perspectives::interpretation::class_local_name;
use crate::perspectives::model_query::types::ModelShape;
use serde::Serialize;
use serde_json::{json, Map, Value};

pub(crate) use shared::{ValidatedWrite, WriteMode};

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

/// The allowed values for an error message: each value in declared order, comma separated, at
/// most 20 then "… and N more".
fn describe_allowed(allowed: &[Value]) -> String {
    let mut text = allowed
        .iter()
        .take(20)
        .map(value_text)
        .collect::<Vec<_>>()
        .join(", ");
    if allowed.len() > 20 {
        text.push_str(&format!(" … and {} more", allowed.len() - 20));
    }
    text
}

/// What is wrong, in words.
fn problem_text(e: &shared::ValidationError) -> String {
    let problem = match &e.problem {
        ValidationProblem::UnknownProperty { class, available } => {
            format!(
                "unknown property on class '{}'. Available: {}",
                class,
                available.join(", ")
            )
        }
        ValidationProblem::UnknownCollection { class, available } => format!(
            "unknown collection on class '{}'. Collections: {}",
            class,
            if available.is_empty() {
                "(none)".to_string()
            } else {
                available.join(", ")
            }
        ),
        ValidationProblem::ReadOnly { reason } => format!("read-only: {}", read_only_text(*reason)),
        ValidationProblem::NotACollection => "is a single-valued property, not a collection — use \
                                              instance_update to set it"
            .to_string(),
        ValidationProblem::ArrayForScalar { length } => {
            format!("expects a single value but received an array of {length} — pass one value")
        }
        ValidationProblem::Null => {
            "a collection item cannot be null — leave it out of the list".to_string()
        }
        ValidationProblem::NotAReference { target_class } => format!(
            "expects the URI of an existing {target_class} instance (as returned by \
             instance_query)"
        ),
        ValidationProblem::WrongType => match e.expected_type.as_deref() {
            Some("boolean") => "expects true or false".to_string(),
            Some("integer") => "expects a whole number".to_string(),
            Some("number") => "expects a number".to_string(),
            Some(other) => format!("expects a {other}"),
            None => "has the wrong type".to_string(),
        },
        ValidationProblem::NotAllowed => format!(
            "not one of the allowed values: {}",
            describe_allowed(e.allowed_values.as_deref().unwrap_or_default())
        ),
        ValidationProblem::MissingRequired => "missing required property".to_string(),
    };
    match e.item {
        Some(_) => format!("collection item {problem}"),
        None => problem,
    }
}

impl From<&shared::ValidationError> for ValidationError {
    fn from(e: &shared::ValidationError) -> Self {
        ValidationError {
            property: e.property.clone(),
            problem: problem_text(e),
            expected_type: e.expected_type.clone(),
            cardinality: e.cardinality.map(|c| cardinality_text(c).to_string()),
            received: e.received.as_ref().map(describe_value),
        }
    }
}

/// [`shared::validate_properties`], its errors worded. Nothing is written by this function.
pub(crate) fn validate_properties(
    shape: &ModelShape,
    props: &Map<String, Value>,
    mode: WriteMode,
) -> Result<ValidatedWrite, Vec<ValidationError>> {
    shared::validate_properties(shape, props, mode, false)
        .map_err(|errors| errors.iter().map(ValidationError::from).collect())
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

/// Evaluation of SHACL-AF Node Expressions for property transforms.
///
/// This module provides the `eval_transform` function, a pure evaluator for
/// `TransformExpression` trees. It maps resolved property values through declarative
/// transformations (path access, conditionals, concatenation, etc.) without
/// accessing any external state or storage.
use super::types::TransformExpression;
use serde_json::Value;

/// Evaluate a transform expression against a value.
///
/// # Arguments
/// * `expr` — The transform expression tree
/// * `value` — The current focus value (used for `focus()`)
/// * `obj` — The resolved object to query paths against
///
/// # Returns
/// The transformed `Value`, or `Null` if any step fails.
pub(super) fn eval_transform(expr: &TransformExpression, value: &Value, obj: &Value) -> Value {
    match expr {
        TransformExpression::Focus => value.clone(),

        TransformExpression::Literal { value: lit } => lit.clone(),

        TransformExpression::Path { predicate } => {
            // Extract local name from predicate (after last '/' or '://')
            let key = if let Some(idx) = predicate.rfind('/') {
                &predicate[idx + 1..]
            } else if let Some(idx) = predicate.rfind("://") {
                &predicate[idx + 3..]
            } else {
                predicate.as_str()
            };
            obj.get(key).cloned().unwrap_or(Value::Null)
        }

        TransformExpression::Exists { expr } => {
            let result = eval_transform(expr, value, obj);
            let is_truthy = match &result {
                Value::Null => false,
                Value::String(s) => !s.is_empty(),
                Value::Array(arr) => !arr.is_empty(),
                Value::Object(map) => !map.is_empty(),
                Value::Bool(b) => *b,
                Value::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(false),
            };
            Value::Bool(is_truthy)
        }

        TransformExpression::If {
            cond,
            then,
            else_expr,
        } => {
            let cond_result = eval_transform(cond, value, obj);
            let is_true = match &cond_result {
                Value::Bool(b) => *b,
                Value::Null => false,
                Value::String(s) => !s.is_empty(),
                Value::Array(arr) => !arr.is_empty(),
                Value::Object(map) => !map.is_empty(),
                _ => true,
            };
            if is_true {
                eval_transform(then, value, obj)
            } else {
                else_expr
                    .as_ref()
                    .map(|e| eval_transform(e, value, obj))
                    .unwrap_or(Value::Null)
            }
        }

        TransformExpression::Concat { args } => {
            let parts: Vec<String> = args
                .iter()
                .map(|a| eval_transform(a, value, obj))
                .filter_map(|v| match v {
                    Value::Null => None,
                    Value::String(s) => Some(s),
                    Value::Number(n) => Some(n.to_string()),
                    Value::Bool(b) => Some(b.to_string()),
                    _ => Some(v.to_string()),
                })
                .collect();
            Value::String(parts.join(""))
        }

        TransformExpression::Coalesce { args } => args
            .iter()
            .map(|a| eval_transform(a, value, obj))
            .find(|v| !matches!(v, Value::Null))
            .unwrap_or(Value::Null),

        TransformExpression::Function { iri, args: _ } => {
            match iri.as_str() {
                "ad4m://fn/defaultFileDecode" => {
                    // Apply the default file decode logic
                    default_file_decode(value)
                }
                _ => {
                    // Unknown function: passthrough
                    value.clone()
                }
            }
        }
    }
}

/// Default file decode: converts a FileData object to a data: URI.
fn default_file_decode(value: &Value) -> Value {
    if let Value::Object(map) = value {
        if let Some(data_base64) = map.get("data_base64").and_then(|v| v.as_str()) {
            let file_type = map
                .get("file_type")
                .and_then(|v| v.as_str())
                .unwrap_or("image/png");
            return Value::String(format!("data:{};base64,{}", file_type, data_base64));
        }
    }
    value.clone()
}

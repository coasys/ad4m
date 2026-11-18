//! What `instance_query` and `get_children` share to walk a graph (#1804):
//! the sort keys of `order`, the short `outline` row of `fields`, and the
//! rows whose declared forward relation points to a node (`via`).
//!
//! Everything here reads the SHACL of the perspective: a relation is in the
//! walk when a class declares it as a forward relation with a target class.
//! No class name and no predicate is known in advance.

use super::run_model_query;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::model_query::ACTIVITY_ORDER_KEY;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use serde_json::{json, Map, Value};
use std::sync::Arc;

/// The sort keys that are not properties. `activity` and the timestamps sort
/// newest first.
const TIME_KEYS: [&str; 3] = [ACTIVITY_ORDER_KEY, "createdAt", "updatedAt"];

/// The model-query `order` for the tool's `order` argument: `activity` and
/// `createdAt` / `updatedAt` newest first, a scalar property ascending. An
/// unknown key is an error that names the valid ones.
pub(crate) fn order_json(shape: &ModelShape, key: &str) -> Result<Value, String> {
    let key = key.trim();
    if TIME_KEYS.contains(&key) {
        let mut order = Map::new();
        order.insert(key.to_string(), json!("DESC"));
        return Ok(Value::Object(order));
    }
    if shape
        .properties
        .iter()
        .any(|p| p.name == key && !p.is_collection && !p.predicate.is_empty())
    {
        let mut order = Map::new();
        order.insert(key.to_string(), json!("ASC"));
        return Ok(Value::Object(order));
    }
    let mut valid: Vec<String> = TIME_KEYS.iter().map(|k| k.to_string()).collect();
    valid.extend(
        shape
            .properties
            .iter()
            .filter(|p| !p.is_collection && !p.predicate.is_empty() && !p.is_flag)
            .map(|p| p.name.clone()),
    );
    Err(format!(
        "Unknown order key '{key}'. Valid keys: {}.",
        valid.join(", ")
    ))
}

/// How much of each row a read returns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Fields {
    /// Every property, as `instance_get` gives it.
    Full,
    /// One short row: `id`, `title` (the identity value) and the forward
    /// relation values (the way up to a parent).
    Outline,
}

impl Fields {
    pub(crate) fn parse(value: Option<&str>, default: Fields) -> Result<Fields, String> {
        match value.map(str::trim) {
            None | Some("") => Ok(default),
            Some("full") => Ok(Fields::Full),
            Some("outline") => Ok(Fields::Outline),
            Some(other) => Err(format!(
                "Unknown fields '{other}'. Use 'outline' (id, title, parent links) or 'full'."
            )),
        }
    }
}

/// The name of the class's identity property (its title), if it declares one.
pub(crate) fn identity_name(shape: &ModelShape) -> Option<&str> {
    shape
        .properties
        .iter()
        .find(|p| p.identity)
        .map(|p| p.name.as_str())
}

/// The class's forward relations with a target class: (relation name,
/// target class name).
pub(crate) fn forward_relations(shape: &ModelShape) -> Vec<(&str, &str)> {
    shape
        .include_relations
        .iter()
        .filter(|r| r.direction == "forward" && !r.target_class_name.is_empty())
        .map(|r| (r.name.as_str(), r.target_class_name.as_str()))
        .collect()
}

/// The property names an outline row reads: the identity property and the
/// forward relations.
pub(crate) fn outline_properties(shape: &ModelShape) -> Vec<String> {
    let mut names: Vec<String> = identity_name(shape)
        .map(str::to_string)
        .into_iter()
        .collect();
    names.extend(
        forward_relations(shape)
            .into_iter()
            .map(|(name, _)| name.to_string()),
    );
    names
}

/// One outline row from a hydrated instance: `id`, `title` when the class has
/// an identity value, and each forward relation that has a value.
pub(crate) fn outline_row(shape: &ModelShape, instance: &Value) -> Value {
    let mut row = Map::new();
    row.insert("id".into(), instance["id"].clone());
    if let Some(title) = identity_name(shape).and_then(|name| instance.get(name)) {
        if !title.is_null() {
            row.insert("title".into(), title.clone());
        }
    }
    for (name, _) in forward_relations(shape) {
        match instance.get(name) {
            None | Some(Value::Null) => {}
            Some(Value::Array(items)) if items.is_empty() => {}
            Some(value) => {
                row.insert(name.to_string(), value.clone());
            }
        }
    }
    Value::Object(row)
}

/// Each registered class with its shape. A class whose shape does not load is
/// left out.
pub(crate) async fn class_shapes(
    perspective: &PerspectiveInstance,
) -> Vec<(String, Arc<ModelShape>)> {
    let mut names = perspective
        .get_subject_classes_from_shacl()
        .await
        .unwrap_or_default();
    names.sort();
    names.dedup();
    names
        .into_iter()
        .filter_map(|name| {
            let shape = perspective.get_shape(&name).ok()?;
            Some((name, shape))
        })
        .collect()
}

/// A row that points to a node through a declared relation, or that a node
/// holds.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Neighbour {
    pub id: String,
    pub class: Option<String>,
    pub title: Option<Value>,
    /// The relation that points to the node; `None` for a containment child.
    pub relation: Option<String>,
    pub timestamp: Value,
    pub author: Value,
}

impl Neighbour {
    pub(crate) fn to_json(&self) -> Value {
        let mut row = Map::new();
        row.insert("id".into(), json!(self.id));
        if let Some(class) = &self.class {
            row.insert("class".into(), json!(class));
        }
        if let Some(title) = &self.title {
            row.insert("title".into(), title.clone());
        }
        row.insert(
            "via".into(),
            json!(self.relation.as_deref().unwrap_or("containment")),
        );
        row.insert("timestamp".into(), self.timestamp.clone());
        row.insert("author".into(), self.author.clone());
        Value::Object(row)
    }
}

/// The rows whose declared forward relation points to `node`, in each class
/// of `classes`: one model query per (class, relation), filtered on the
/// relation value, as `instance_query(filter: {relation: node})` does.
pub(crate) async fn rows_pointing_to(
    perspective: &PerspectiveInstance,
    classes: &[(String, Arc<ModelShape>)],
    node: &str,
) -> Result<Vec<Neighbour>, String> {
    let mut out = Vec::new();
    for (class_name, shape) in classes {
        let title = identity_name(shape);
        for (relation, _) in forward_relations(shape) {
            let mut properties: Vec<String> = title.map(str::to_string).into_iter().collect();
            properties.push("createdAt".into());
            let query = json!({
                "where": { relation: node },
                "properties": properties,
            });
            let (rows, _) = run_model_query(perspective, class_name, &query).await?;
            for row in rows {
                let Some(id) = row["id"].as_str() else {
                    continue;
                };
                out.push(Neighbour {
                    id: id.to_string(),
                    class: Some(class_name.clone()),
                    title: title.and_then(|t| row.get(t)).cloned(),
                    relation: Some(relation.to_string()),
                    timestamp: row["timestamp"].clone(),
                    author: row["author"].clone(),
                });
            }
        }
    }
    Ok(out)
}

/// Fill in the class and title of the containment children in `rows` that
/// have none: the first class (by name) that has an instance at the id.
pub(crate) async fn name_children(
    perspective: &PerspectiveInstance,
    classes: &[(String, Arc<ModelShape>)],
    rows: &mut [Neighbour],
) -> Result<(), String> {
    let ids: Vec<String> = rows
        .iter()
        .filter(|r| r.class.is_none())
        .map(|r| r.id.clone())
        .collect();
    if ids.is_empty() {
        return Ok(());
    }
    for (class_name, shape) in classes {
        let title = identity_name(shape);
        let properties: Vec<String> = title.map(str::to_string).into_iter().collect();
        let query = json!({
            "where": { "id": ids },
            "properties": properties,
        });
        let (found, _) = run_model_query(perspective, class_name, &query).await?;
        for instance in found {
            let Some(id) = instance["id"].as_str() else {
                continue;
            };
            for row in rows.iter_mut().filter(|r| r.id == id && r.class.is_none()) {
                row.class = Some(class_name.clone());
                row.title = title.and_then(|t| instance.get(t)).cloned();
            }
        }
    }
    Ok(())
}

/// Milliseconds since the epoch of a timestamp a read returns (a number, a
/// string of digits, or RFC 3339); 0 when it is none of them.
pub(crate) fn millis(value: &Value) -> i64 {
    match value {
        Value::Number(n) => n.as_i64().unwrap_or(0),
        Value::String(s) => s.parse::<i64>().ok().unwrap_or_else(|| {
            chrono::DateTime::parse_from_rfc3339(s)
                .map(|t| t.timestamp_millis())
                .unwrap_or(0)
        }),
        _ => 0,
    }
}

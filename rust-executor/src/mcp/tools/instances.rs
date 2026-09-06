//! Static, class-agnostic subject-instance tools.
//!
//! `describe_perspective` plus the `instance_*` family give an MCP client the
//! same typed-instance operations as the dynamic per-class tools in
//! [`super::dynamic`], but with a **constant tool list**: the subject class is
//! a `class_name` argument instead of a tool-name prefix, and the schema an
//! agent needs in order to fill `properties` comes back *as data* from
//! `describe_perspective` instead of being baked into per-property tool
//! parameters.
//!
//! Why: the dynamic surface is linear in (classes × actions) — ~45 tools with
//! one social DNA loaded, ~85 with two — and it changes at runtime as
//! neighbourhoods are joined. That breaks every statically-declared
//! integration (plugin manifests, agent tool configs, docs) and degrades LLM
//! tool selection long before hard limits bite. Design and trade-offs:
//! `planning/mcp-tool-surface-redesign-2026-09-06.md`.
//!
//! Correctness is unchanged. Every write is validated against the class's
//! SHACL shape *before* anything is written ([`validate_properties`]), and a
//! rejection names the property, the expected type and the cardinality —
//! the agent gets less schema up front than with per-property tool
//! parameters, so the error has to carry it instead. Accepted writes go
//! through the same `create_subject` / `update_subject` / link paths the
//! per-class tools use. The dynamic generators themselves are untouched and
//! keep feeding the in-process interpretation harness.

use super::Ad4mMcpHandler;
use crate::mcp::shacl;
use crate::perspectives::flow_context::load_shacl_flows;
use crate::perspectives::model_query::is_safe_iri_target;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::{Link, LinkQuery, LinkStatus, PerspectiveHandle};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::sync::Arc;
use std::time::Duration;

/// Default page size for `instance_query` when the caller passes no `limit`.
/// Bounded so a class with thousands of instances doesn't flood the LLM
/// context; `total_count` in the response tells the caller there is more.
pub(crate) const DEFAULT_QUERY_LIMIT: usize = 100;

/// How long class resolution waits for a class's SHACL to arrive over
/// p-diff-sync on a freshly joined neighbourhood before reporting it as
/// unknown. Local perspectives never wait.
const CLASS_SYNC_WAIT: Duration = Duration::from_secs(5);

/// Predicate used for the optional `parent` argument on `instance_create` /
/// `instance_query` — the same child link the per-class `{class}_create` /
/// `{class}_list` tools use.
const HAS_CHILD: &str = "ad4m://has_child";

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for describing a perspective's data model
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct DescribePerspectiveParams {
    /// Perspective UUID
    pub perspective_id: String,
}

/// Parameters for creating a subject instance of any class
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceCreateParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name exactly as listed by describe_perspective (e.g. "Message")
    pub class_name: String,
    /// Property values keyed by property name. Scalar properties take one JSON
    /// value (string / number / boolean, matching the property's type);
    /// collection properties take an array of item URIs. Required properties
    /// must be present.
    pub properties: Option<Map<String, Value>>,
    /// Optional URI for the new instance. A random one is generated when omitted.
    pub base_uri: Option<String>,
    /// Optional parent URI. The new instance is additionally linked as an
    /// `ad4m://has_child` child of this node (e.g. a Message inside a Channel).
    pub parent: Option<String>,
}

/// Parameters for querying instances of a class
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceQueryParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name as listed by describe_perspective
    pub class_name: String,
    /// Optional filter on property values (a model-query `where` clause).
    /// Keys are property names. Values: an exact match (`{"status": "open"}`),
    /// an array for IN (`{"status": ["open", "doing"]}`), or an operator object
    /// (`{"count": {"gt": 5}}`, `{"title": {"contains": "mcp"}}`,
    /// `{"owner": {"not": "did:key:…"}}`). Combine with `"OR": [..]` /
    /// `"AND": [..]` / `"NOT": {..}`. `id` filters on the instance URI.
    pub filter: Option<Map<String, Value>>,
    /// Optional parent URI: only return instances that are `ad4m://has_child`
    /// children of this node (e.g. the messages of one channel).
    pub parent: Option<String>,
    /// Maximum number of instances to return (default 100).
    pub limit: Option<usize>,
    /// Number of instances to skip, for pagination.
    pub offset: Option<usize>,
}

/// Parameters for reading one instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceGetParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name as listed by describe_perspective
    pub class_name: String,
    /// URI of the instance (the `id` returned by instance_create / instance_query)
    pub base_uri: String,
}

/// Parameters for updating scalar properties of an instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceUpdateParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name as listed by describe_perspective
    pub class_name: String,
    /// URI of the instance to update
    pub base_uri: String,
    /// Property values to set, keyed by property name. Only the given
    /// properties change; each must be a single-valued property of the class.
    pub properties: Map<String, Value>,
}

/// Parameters for adding an item to a collection property
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceAddToCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name of the instance that owns the collection
    pub class_name: String,
    /// URI of the instance that owns the collection
    pub base_uri: String,
    /// Collection property name as listed under `collections` by describe_perspective
    pub collection: String,
    /// URI of the item to add (typically another instance's `id`)
    pub item_uri: String,
}

/// Parameters for removing an instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceRemoveParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name of the instance
    pub class_name: String,
    /// URI of the instance to remove
    pub base_uri: String,
}

// ============================================================================
// Schema projection — SHACL shape → data an agent can read
// ============================================================================

/// One property of a class as it is presented to (and validated for) MCP
/// clients. Derived from the `ModelShape` the query pipeline already uses,
/// so what `describe_perspective` says and what `instance_*` validates
/// against is one and the same source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PropertyInfo {
    pub name: String,
    pub predicate: String,
    /// Friendly type name: `string` / `boolean` / `integer` / `number` /
    /// `datetime` / `reference` (link to another instance) / raw xsd local
    /// name for anything else.
    pub type_name: String,
    pub required: bool,
    /// Multi-valued (collection or *Many relation).
    pub collection: bool,
    /// Relation kind (`hasOne`, `hasMany`, `belongsToOne`, `belongsToMany`)
    /// and the target class name, for link-typed properties.
    pub relation: Option<(String, String)>,
    /// `belongsTo*` — the link is stored on the *other* instance, so this
    /// side cannot write it.
    pub reverse: bool,
    /// Class marker (`sh:hasValue` + `minCount 1`), set by the constructor.
    /// Never supplied by clients.
    pub flag: bool,
    /// Derived via a getter expression; read-only.
    pub computed: bool,
    pub resolve_language: Option<String>,
    pub interpretation_hint: Option<String>,
    /// Dedup identity of the class (the "title-like" key).
    pub identity: bool,
}

impl PropertyInfo {
    /// Whether an MCP client may write this property at all.
    pub(crate) fn writable(&self) -> bool {
        !self.flag && !self.reverse && !self.computed
    }

    fn read_only_reason(&self) -> Option<&'static str> {
        if self.flag {
            Some("class marker set automatically on create")
        } else if self.reverse {
            Some("reverse relation — stored on the target instance")
        } else if self.computed {
            Some("computed by a getter expression")
        } else {
            None
        }
    }

    /// Cardinality as data: `{"min": 0|1, "max": 1|null}`.
    pub(crate) fn cardinality(&self) -> Value {
        json!({
            "min": if self.required { 1 } else { 0 },
            "max": if self.collection { Value::Null } else { json!(1) },
        })
    }

    /// Cardinality as words, for error messages.
    pub(crate) fn cardinality_text(&self) -> &'static str {
        match (self.required, self.collection) {
            (true, false) => "exactly one value (minCount 1, maxCount 1)",
            (false, false) => "at most one value (maxCount 1)",
            (true, true) => "one or more values (minCount 1, collection)",
            (false, true) => "any number of values (collection)",
        }
    }

    fn to_json(&self) -> Value {
        let mut v = json!({
            "name": self.name,
            "type": self.type_name,
            "required": self.required,
            "cardinality": self.cardinality(),
            "predicate": self.predicate,
        });
        if let Some((kind, target)) = &self.relation {
            v["relation_kind"] = json!(kind);
            v["target_class"] = json!(target);
        }
        if let Some(lang) = &self.resolve_language {
            v["resolve_language"] = json!(lang);
        }
        if let Some(hint) = &self.interpretation_hint {
            v["interpretation_hint"] = json!(hint);
        }
        if self.identity {
            v["identity"] = json!(true);
        }
        if let Some(reason) = self.read_only_reason() {
            v["read_only"] = json!(true);
            v["read_only_reason"] = json!(reason);
        }
        v
    }
}

/// Map an xsd datatype IRI (`xsd://string`, `xsd:dateTime`, …) to the
/// friendly type vocabulary used in `describe_perspective` output and
/// validation errors. Unknown datatypes pass through as their local name.
pub(crate) fn friendly_type(datatype: Option<&str>) -> String {
    let Some(dt) = datatype else {
        return "string".to_string();
    };
    let local = dt
        .rsplit(|c: char| c == '/' || c == '#' || c == ':')
        .next()
        .unwrap_or(dt)
        .to_ascii_lowercase();
    match local.as_str() {
        "string" | "normalizedstring" | "token" | "anyuri" => "string".to_string(),
        "boolean" => "boolean".to_string(),
        "integer" | "int" | "long" | "short" | "byte" | "nonnegativeinteger"
        | "positiveinteger" | "unsignedint" | "unsignedlong" | "unsignedshort" => {
            "integer".to_string()
        }
        "decimal" | "float" | "double" => "number".to_string(),
        "datetime" | "date" | "time" | "datetimestamp" => "datetime".to_string(),
        other => other.to_string(),
    }
}

/// Project a class shape into the per-property view. Flags are kept (so
/// validation can reject them by name) but marked; callers presenting the
/// schema drop them.
pub(crate) fn class_properties(shape: &ModelShape) -> Vec<PropertyInfo> {
    shape
        .properties
        .iter()
        .map(|p| {
            let relation = shape.include_relations.iter().find(|r| r.name == p.name);
            let reverse = p.direction.as_deref() == Some("reverse");
            let type_name = if relation.is_some() {
                "reference".to_string()
            } else {
                friendly_type(p.datatype.as_deref())
            };
            // Relations are all `is_collection` in the shape (the pipeline
            // hydrates them as arrays); the scalar-relation flag tells us
            // which ones are really single-valued.
            let collection = if relation.is_some() {
                !p.is_scalar_relation
            } else {
                p.is_collection
            };
            PropertyInfo {
                name: p.name.clone(),
                predicate: p.predicate.clone(),
                type_name,
                required: p.is_required,
                collection,
                relation: relation.map(|r| (r.kind.clone(), r.target_class_name.clone())),
                reverse,
                flag: p.is_flag,
                computed: relation.is_none() && p.getter.is_some(),
                resolve_language: p.resolve_language.clone(),
                interpretation_hint: p.interpretation_hint.clone(),
                identity: p.identity,
            }
        })
        .collect()
}

/// Describe one class as data: hint, single-valued `properties`, multi-valued
/// `collections`, and which property (if any) is the dedup identity.
pub(crate) fn describe_class(class_name: &str, shape: &ModelShape) -> Value {
    let infos = class_properties(shape);
    let properties: Vec<Value> = infos
        .iter()
        .filter(|i| !i.flag && !i.collection)
        .map(PropertyInfo::to_json)
        .collect();
    let collections: Vec<Value> = infos
        .iter()
        .filter(|i| !i.flag && i.collection)
        .map(PropertyInfo::to_json)
        .collect();
    let identity = infos.iter().find(|i| i.identity).map(|i| i.name.clone());
    let mut v = json!({
        "name": class_name,
        "class_uri": shape.target_class,
        "properties": properties,
        "collections": collections,
    });
    if let Some(hint) = &shape.interpretation_hint {
        v["interpretation_hint"] = json!(hint);
    }
    if let Some(id) = identity {
        v["identity_property"] = json!(id);
    }
    v
}

/// Describe the flows (state machines) registered on a perspective, compact
/// enough for an agent to read: states with their hints, transitions with
/// the action name that triggers them, and which classes a flow runs on.
pub(crate) async fn describe_flows(perspective: &PerspectiveInstance) -> Vec<Value> {
    let flows = match load_shacl_flows(perspective).await {
        Ok(f) => f,
        Err(e) => {
            log::warn!("describe_perspective: could not load flows: {e:#}");
            return vec![];
        }
    };
    let mut entries: Vec<(String, Value)> = flows
        .iter()
        .map(|(uri, flow)| {
            let states: Vec<Value> = flow
                .states
                .iter()
                .map(|s| {
                    let mut v = json!({ "name": s.name, "value": s.value });
                    if let Some(h) = &s.interpretation_hint {
                        v["interpretation_hint"] = json!(h);
                    }
                    v
                })
                .collect();
            let transitions: Vec<Value> = flow
                .transitions
                .iter()
                .map(|t| {
                    json!({
                        "action": t.action_name,
                        "from_state": t.from_state,
                        "to_state": t.to_state,
                    })
                })
                .collect();
            let mut v = json!({
                "name": flow.name,
                "uri": uri,
                "input_types": flow.input_types,
                "output_types": flow.output_types,
                "states": states,
                "transitions": transitions,
            });
            if let Some(h) = &flow.interpretation_hint {
                v["interpretation_hint"] = json!(h);
            }
            if let Some(h) = &flow.creation_hint {
                v["creation_hint"] = json!(h);
            }
            (uri.clone(), v)
        })
        .collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries.into_iter().map(|(_, v)| v).collect()
}

/// The full `describe_perspective` payload.
pub(crate) async fn describe_perspective_value(
    perspective: &PerspectiveInstance,
    handle: &PerspectiveHandle,
) -> Value {
    let mut class_names = perspective
        .get_subject_classes_from_shacl()
        .await
        .unwrap_or_default();
    class_names.sort();
    class_names.dedup();

    let mut classes = Vec::with_capacity(class_names.len());
    for name in &class_names {
        match perspective.get_shape(name) {
            Ok(shape) => classes.push(describe_class(name, &shape)),
            // Never drop a class silently — an agent that can't see a class
            // can't tell a missing class from a broken shape.
            Err(e) => classes.push(json!({ "name": name, "error": format!("{e:#}") })),
        }
    }

    let flows = describe_flows(perspective).await;

    json!({
        "perspective_id": handle.uuid,
        "name": handle.name,
        "shared_url": handle.shared_url,
        "is_neighbourhood": handle.neighbourhood.is_some(),
        "classes": classes,
        "flows": flows,
        "usage": "Pass a class `name` from `classes` as `class_name` to instance_create / \
                  instance_query / instance_get / instance_update / instance_add_to_collection / \
                  instance_remove. `properties` entries are single JSON values typed as shown; \
                  `collections` are written with instance_add_to_collection using item URIs. \
                  Properties are validated against this schema on every write.",
    })
}

// ============================================================================
// Validation — SHACL-derived, names property + expected type + cardinality
// ============================================================================

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
fn describe_value(v: &Value) -> String {
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
pub(crate) fn coerce_scalar(info: &PropertyInfo, value: &Value) -> Result<Value, ValidationError> {
    let err = |problem: String| ValidationError {
        property: info.name.clone(),
        problem,
        expected_type: Some(info.type_name.clone()),
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

    if let Some((_, target)) = &info.relation {
        return match value {
            Value::String(s) if is_safe_iri_target(s) => Ok(value.clone()),
            _ => Err(err(format!(
                "expects the URI of an existing {target} instance (as returned by instance_query)"
            ))),
        };
    }

    match info.type_name.as_str() {
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
            _ => Err(err(format!("expects a {}", info.type_name))),
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
            .map(|i| i.name.as_str())
            .collect();
        names.join(", ")
    };

    for (given_name, value) in props {
        let info = infos.iter().find(|i| i.name == *given_name).or_else(|| {
            infos
                .iter()
                .find(|i| i.name.eq_ignore_ascii_case(given_name))
        });
        let Some(info) = info else {
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
                property: info.name.clone(),
                problem: format!("read-only: {reason}"),
                expected_type: Some(info.type_name.clone()),
                cardinality: Some(info.cardinality_text().to_string()),
                received: Some(describe_value(value)),
            });
            continue;
        }

        if info.collection {
            match mode {
                WriteMode::Update => errors.push(ValidationError {
                    property: info.name.clone(),
                    problem: "is a collection — instance_update only sets single-valued \
                              properties; use instance_add_to_collection to add items"
                        .to_string(),
                    expected_type: Some(info.type_name.clone()),
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
                        out.collections.push((info.name.clone(), coerced));
                    }
                }
            }
            continue;
        }

        match coerce_scalar(info, value) {
            Ok(v) => {
                out.scalars.insert(info.name.clone(), v);
            }
            Err(e) => errors.push(e),
        }
    }

    if mode == WriteMode::Create {
        for info in infos.iter().filter(|i| i.required && i.writable()) {
            let supplied = if info.collection {
                out.collections.iter().any(|(n, _)| n == &info.name)
            } else {
                out.scalars.contains_key(&info.name)
            };
            let already_reported = errors.iter().any(|e| e.property == info.name);
            if !supplied && !already_reported {
                errors.push(ValidationError {
                    property: info.name.clone(),
                    problem: "missing required property".to_string(),
                    expected_type: Some(info.type_name.clone()),
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

/// Local name of a class URI (`flux://Channel` → `Channel`).
fn class_local_name(uri: &str) -> &str {
    uri.rsplit(|c: char| c == '/' || c == '#')
        .next()
        .unwrap_or(uri)
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
                let info = infos
                    .iter()
                    .find(|i| i.name == *key)
                    .or_else(|| infos.iter().find(|i| i.name.eq_ignore_ascii_case(key)));
                let Some(info) = info else {
                    let names: Vec<&str> = infos.iter().map(|i| i.name.as_str()).collect();
                    return Err(format!(
                        "unknown filter property '{}' on class '{}'. Available: id, {}",
                        key,
                        class_local_name(&shape.target_class),
                        names.join(", ")
                    ));
                };
                out.insert(info.name.clone(), value.clone());
            }
        }
    }
    Ok(out)
}

// ============================================================================
// Shared plumbing
// ============================================================================

fn pretty(v: &Value) -> String {
    serde_json::to_string_pretty(v).unwrap_or_else(|e| format!("Error: {e}"))
}

fn error_json(msg: impl Into<String>) -> String {
    json!({ "error": msg.into() }).to_string()
}

/// Random instance URI in the same form the per-class `{class}_create`
/// tool generates.
fn generate_instance_uri() -> String {
    let random_id: String = (0..24)
        .map(|_| {
            let idx = rand::random::<u8>() % 36;
            if idx < 10 {
                (b'0' + idx) as char
            } else {
                (b'a' + idx - 10) as char
            }
        })
        .collect();
    format!("literal://string:{random_id}")
}

/// Resolve a caller-supplied class name to its canonical spelling and shape.
///
/// Exact name first (waiting briefly for SHACL to sync on a neighbourhood),
/// then case-insensitive, then a JSON error listing the registered classes.
pub(crate) async fn resolve_class(
    perspective: &PerspectiveInstance,
    class_name: &str,
) -> Result<(String, Arc<ModelShape>), String> {
    if let Ok(shape) = perspective
        .get_shape_or_wait(class_name, CLASS_SYNC_WAIT)
        .await
    {
        return Ok((class_name.to_string(), shape));
    }
    if let Some(canonical) = shacl::find_class_name(perspective, &class_name.to_lowercase()).await {
        if let Ok(shape) = perspective.get_shape(&canonical) {
            return Ok((canonical, shape));
        }
    }
    let mut registered = perspective
        .get_subject_classes_from_shacl()
        .await
        .unwrap_or_default();
    registered.sort();
    Err(error_json(format!(
        "Unknown subject class '{}'. Registered classes: {}. Call describe_perspective to see \
         their schemas.",
        class_name,
        if registered.is_empty() {
            "(none)".to_string()
        } else {
            registered.join(", ")
        }
    )))
}

/// Run a model query for `class_name` and return `(instances, total_count)`.
async fn run_model_query(
    perspective: &PerspectiveInstance,
    class_name: &str,
    query: &Value,
) -> Result<(Vec<Value>, usize), String> {
    let raw = perspective
        .model_query(class_name, &query.to_string())
        .await
        .map_err(|e| format!("{e:#}"))?;
    let parsed: Value = serde_json::from_str(&raw).map_err(|e| e.to_string())?;
    let instances = parsed
        .get("instances")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    let total = parsed
        .get("totalCount")
        .and_then(Value::as_u64)
        .map(|n| n as usize)
        .unwrap_or(instances.len());
    Ok((instances, total))
}

/// Fetch one hydrated instance by URI, or `None` if no instance of that class
/// lives at `base_uri`.
pub(crate) async fn fetch_instance(
    perspective: &PerspectiveInstance,
    class_name: &str,
    base_uri: &str,
) -> Result<Option<Value>, String> {
    let query = json!({ "where": { "id": base_uri }, "limit": 1 });
    let (instances, _) = run_model_query(perspective, class_name, &query).await?;
    Ok(instances.into_iter().next())
}

fn not_found(class_name: &str, base_uri: &str) -> String {
    error_json(format!(
        "No {class_name} instance found at '{base_uri}'. Check base_uri and class_name; use \
         instance_query to list existing {class_name} instances."
    ))
}

/// Whether the SDNA declares a setter for `prop` on `class_uri`.
/// `create_subject` / `update_subject` silently skip properties without one
/// (they only log), so the check has to happen here for the write to be
/// honest about what it did.
async fn has_setter(perspective: &PerspectiveInstance, class_uri: &str, prop: &str) -> bool {
    let prop_shape_uri = format!("{class_uri}.{prop}");
    perspective
        .get_links(&LinkQuery {
            source: Some(prop_shape_uri),
            predicate: Some("ad4m://setter".to_string()),
            ..Default::default()
        })
        .await
        .map(|links| !links.is_empty())
        .unwrap_or(false)
}

/// Reject scalar writes to properties the SDNA can't actually set.
async fn check_setters(
    perspective: &PerspectiveInstance,
    shape: &ModelShape,
    scalars: &Map<String, Value>,
) -> Vec<ValidationError> {
    let infos = class_properties(shape);
    let mut errors = Vec::new();
    for name in scalars.keys() {
        if !has_setter(perspective, &shape.target_class, name).await {
            let info = infos.iter().find(|i| &i.name == name);
            errors.push(ValidationError {
                property: name.clone(),
                problem: "read-only: the class declares no setter for this property".to_string(),
                expected_type: info.map(|i| i.type_name.clone()),
                cardinality: info.map(|i| i.cardinality_text().to_string()),
                received: None,
            });
        }
    }
    errors
}

fn subject_class(class_name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(class_name.to_string()),
        query: None,
    }
}

/// URI-or-literal encoding for link targets, shared with the per-class tools.
fn link_target(value: &str) -> String {
    if value.contains("://") {
        value.to_string()
    } else {
        Ad4mMcpHandler::encode_literal(value)
    }
}

/// Remove every link touching `uri` (as source or target). Same cascade as
/// `delete_subject` / `{class}_delete`.
pub(crate) async fn remove_all_links_of(perspective: &mut PerspectiveInstance, uri: &str) -> usize {
    let mut removed = 0;
    for query in [
        LinkQuery {
            source: Some(uri.to_string()),
            ..Default::default()
        },
        LinkQuery {
            target: Some(uri.to_string()),
            ..Default::default()
        },
    ] {
        if let Ok(links) = perspective.get_links(&query).await {
            for link in links {
                if perspective.remove_link(link.into(), None).await.is_ok() {
                    removed += 1;
                }
            }
        }
    }
    removed
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Describe the data model of a perspective — the discovery step before any instance_* call.
    #[tool(
        description = "Describe the data model of a perspective: every registered subject class (model) with its properties (name, type, required, cardinality, hints), its collections, and any flows (state machines). Call this right after list_perspectives / neighbourhood_join_from_url — it returns the schema as data so you can then use the generic instance_create / instance_query / instance_get / instance_update / instance_add_to_collection / instance_remove tools with class_name set to one of the returned class names. Property values passed to those tools are validated against this schema."
    )]
    pub async fn describe_perspective(
        &self,
        params: Parameters<DescribePerspectiveParams>,
    ) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let handle = perspective.persisted.lock().await.clone();
        pretty(&describe_perspective_value(&perspective, &handle).await)
    }

    /// Create an instance of any subject class.
    #[tool(
        description = "Create a new instance of a subject class. class_name is one of the class names from describe_perspective; properties is a JSON object of property values (single JSON value per scalar property, array of item URIs per collection). Required properties must be present; every value is validated against the class schema (property, expected type, cardinality are named on rejection). Optionally pass parent to also link the instance as an ad4m://has_child child of another instance (e.g. a Message into a Channel). Returns the new instance's base_uri (its id). Example: instance_create(perspective_id, class_name='Message', properties={\"body\": \"Hello\"}, parent='<channel uri>')."
    )]
    pub async fn instance_create(&self, params: Parameters<InstanceCreateParams>) -> String {
        let p = &params.0;
        let (mut perspective, agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let (class_name, shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };

        let empty = Map::new();
        let props = p.properties.as_ref().unwrap_or(&empty);
        let validated = match validate_properties(&shape, props, WriteMode::Create) {
            Ok(v) => v,
            Err(errors) => return validation_failure(&class_name, &errors),
        };
        let setter_errors = check_setters(&perspective, &shape, &validated.scalars).await;
        if !setter_errors.is_empty() {
            return validation_failure(&class_name, &setter_errors);
        }

        let base_uri = match p.base_uri.as_deref() {
            Some(uri) if !uri.trim().is_empty() => uri.trim().to_string(),
            _ => generate_instance_uri(),
        };
        if let Ok(Some(_)) = fetch_instance(&perspective, &class_name, &base_uri).await {
            return error_json(format!(
                "A {class_name} instance already exists at '{base_uri}'. Use instance_update to \
                 change it, or omit base_uri to create a new instance with a generated id."
            ));
        }

        let initial_values = if validated.scalars.is_empty() {
            None
        } else {
            Some(Value::Object(validated.scalars.clone()))
        };
        if let Err(e) = perspective
            .create_subject(
                subject_class(&class_name),
                base_uri.clone(),
                initial_values,
                None,
                &agent_context,
            )
            .await
        {
            return error_json(format!("Error creating {class_name} instance: {e:#}"));
        }

        // Collections: one link per item, same path as instance_add_to_collection.
        let mut collections_set: Map<String, Value> = Map::new();
        for (collection, items) in &validated.collections {
            let predicate = match shacl::resolve_property_predicate(
                &perspective,
                &class_name,
                collection,
            )
            .await
            {
                Ok(pred) => pred,
                Err(e) => {
                    return error_json(format!(
                        "Created {class_name} at '{base_uri}' but could not resolve collection \
                         '{collection}': {e}"
                    ))
                }
            };
            let mut added = Vec::with_capacity(items.len());
            for item in items {
                let item_str = match item {
                    Value::String(s) => s.clone(),
                    other => other.to_string(),
                };
                let target = Self::create_property_expression(
                    &perspective,
                    &class_name,
                    collection,
                    &item_str,
                    &agent_context,
                )
                .await;
                let link = Link {
                    source: base_uri.clone(),
                    predicate: Some(predicate.clone()),
                    target,
                };
                if let Err(e) = perspective
                    .add_link(link, LinkStatus::Shared, None, &agent_context)
                    .await
                {
                    return error_json(format!(
                        "Created {class_name} at '{base_uri}' but failed to add '{item_str}' to \
                         collection '{collection}': {e:#}"
                    ));
                }
                added.push(Value::String(item_str));
            }
            collections_set.insert(collection.clone(), Value::Array(added));
        }

        let mut result = json!({
            "created": true,
            "perspective_id": p.perspective_id,
            "class_name": class_name,
            "base_uri": base_uri,
            "properties_set": validated.scalars.keys().cloned().collect::<Vec<_>>(),
        });
        if !collections_set.is_empty() {
            result["collections_set"] = Value::Object(collections_set);
        }

        if let Some(parent) = p.parent.as_deref().filter(|s| !s.trim().is_empty()) {
            let link = Link {
                source: link_target(parent.trim()),
                predicate: Some(HAS_CHILD.to_string()),
                target: link_target(&base_uri),
            };
            match perspective
                .add_link(link, LinkStatus::Shared, None, &agent_context)
                .await
            {
                Ok(_) => {
                    result["parent"] = json!(parent);
                    result["added_to_parent"] = json!(true);
                }
                Err(e) => {
                    result["parent"] = json!(parent);
                    result["added_to_parent"] = json!(false);
                    result["parent_error"] = json!(format!(
                        "Created instance but failed to link to parent: {e:#}"
                    ));
                }
            }
        }

        pretty(&result)
    }

    /// Query instances of any subject class, with typed property values.
    #[tool(
        description = "List instances of a subject class with their property values. class_name is one of the class names from describe_perspective. Optional filter is a where clause on property values: exact match {\"status\": \"open\"}, IN {\"status\": [\"open\", \"doing\"]}, operators {\"count\": {\"gt\": 5}} / {\"title\": {\"contains\": \"mcp\"}} / {\"owner\": {\"not\": \"…\"}}, combinators \"OR\" / \"AND\" / \"NOT\"; \"id\" filters on the instance URI. Optional parent restricts to ad4m://has_child children of one instance (e.g. messages of a channel). Paginate with limit (default 100) and offset; total_count reports the full match count. Each instance has id (its base_uri), author, timestamp, and one key per property."
    )]
    pub async fn instance_query(&self, params: Parameters<InstanceQueryParams>) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let (class_name, shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };

        let mut query = json!({ "limit": p.limit.unwrap_or(DEFAULT_QUERY_LIMIT) });
        if let Some(offset) = p.offset {
            query["offset"] = json!(offset);
        }
        if let Some(filter) = &p.filter {
            if !filter.is_empty() {
                match normalize_filter(&shape, filter) {
                    Ok(f) => query["where"] = Value::Object(f),
                    Err(e) => return error_json(e),
                }
            }
        }
        if let Some(parent) = p.parent.as_deref().filter(|s| !s.trim().is_empty()) {
            query["parent"] = json!({ "id": link_target(parent.trim()), "predicate": HAS_CHILD });
        }

        match run_model_query(&perspective, &class_name, &query).await {
            Ok((instances, total)) => pretty(&json!({
                "class_name": class_name,
                "count": instances.len(),
                "total_count": total,
                "instances": instances,
            })),
            Err(e) => error_json(format!("Error querying {class_name} instances: {e}")),
        }
    }

    /// Read one instance of any subject class.
    #[tool(
        description = "Get one instance of a subject class by its base_uri, with all property values and collections resolved. class_name is one of the class names from describe_perspective. Returns an error if no instance of that class exists at the URI."
    )]
    pub async fn instance_get(&self, params: Parameters<InstanceGetParams>) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let (class_name, _shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };
        match fetch_instance(&perspective, &class_name, &p.base_uri).await {
            Ok(Some(instance)) => pretty(&instance),
            Ok(None) => not_found(&class_name, &p.base_uri),
            Err(e) => error_json(format!("Error reading {class_name} instance: {e}")),
        }
    }

    /// Update single-valued properties of an instance.
    #[tool(
        description = "Set one or more single-valued properties on an existing instance. class_name is one of the class names from describe_perspective; properties is a JSON object of the values to change (unlisted properties are untouched). Values are validated against the class schema — the error names the property, expected type and cardinality. Collections cannot be set here: use instance_add_to_collection. Example: instance_update(perspective_id, class_name='Task', base_uri='<id>', properties={\"status\": \"done\"})."
    )]
    pub async fn instance_update(&self, params: Parameters<InstanceUpdateParams>) -> String {
        let p = &params.0;
        let (mut perspective, agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let (class_name, shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };
        if p.properties.is_empty() {
            return error_json("properties is empty — pass at least one property to set.");
        }

        let validated = match validate_properties(&shape, &p.properties, WriteMode::Update) {
            Ok(v) => v,
            Err(errors) => return validation_failure(&class_name, &errors),
        };
        let setter_errors = check_setters(&perspective, &shape, &validated.scalars).await;
        if !setter_errors.is_empty() {
            return validation_failure(&class_name, &setter_errors);
        }

        match fetch_instance(&perspective, &class_name, &p.base_uri).await {
            Ok(Some(_)) => {}
            Ok(None) => return not_found(&class_name, &p.base_uri),
            Err(e) => return error_json(format!("Error reading {class_name} instance: {e}")),
        }

        // One batch for the whole update so remove-old + add-new per property
        // lands atomically (same reasoning as the per-class setters: without
        // it a peer can observe the property as unset in between).
        let batch_id = perspective.create_batch().await;
        if let Err(e) = perspective
            .update_subject(
                subject_class(&class_name),
                p.base_uri.clone(),
                Value::Object(validated.scalars.clone()),
                Some(batch_id.clone()),
                &agent_context,
            )
            .await
        {
            return error_json(format!(
                "Error updating {class_name} instance (batch abandoned): {e:#}"
            ));
        }
        if let Err(e) = perspective.commit_batch(batch_id, &agent_context).await {
            return error_json(format!("Error committing update: {e:#}"));
        }

        pretty(&json!({
            "success": true,
            "class_name": class_name,
            "base_uri": p.base_uri,
            "updated_properties": validated.scalars.keys().cloned().collect::<Vec<_>>(),
        }))
    }

    /// Add an item to a collection property of an instance.
    #[tool(
        description = "Add an item to a collection property of an instance (e.g. add a Message to a Channel's messages). class_name is the owning instance's class, base_uri its id, collection one of the names listed under collections by describe_perspective, item_uri the URI of the item (usually another instance's id). Adding the same item twice is a no-op."
    )]
    pub async fn instance_add_to_collection(
        &self,
        params: Parameters<InstanceAddToCollectionParams>,
    ) -> String {
        let p = &params.0;
        let (mut perspective, agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let (class_name, shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };

        let infos = class_properties(&shape);
        let info = infos.iter().find(|i| i.name == p.collection).or_else(|| {
            infos
                .iter()
                .find(|i| i.name.eq_ignore_ascii_case(&p.collection))
        });
        let Some(info) = info else {
            let names: Vec<&str> = infos
                .iter()
                .filter(|i| i.collection && i.writable())
                .map(|i| i.name.as_str())
                .collect();
            return validation_failure(
                &class_name,
                &[ValidationError {
                    property: p.collection.clone(),
                    problem: format!(
                        "unknown collection on class '{}'. Collections: {}",
                        class_name,
                        if names.is_empty() {
                            "(none)".to_string()
                        } else {
                            names.join(", ")
                        }
                    ),
                    expected_type: None,
                    cardinality: None,
                    received: None,
                }],
            );
        };
        if !info.collection {
            return validation_failure(
                &class_name,
                &[ValidationError {
                    property: info.name.clone(),
                    problem: "is a single-valued property, not a collection — use \
                              instance_update to set it"
                        .to_string(),
                    expected_type: Some(info.type_name.clone()),
                    cardinality: Some(info.cardinality_text().to_string()),
                    received: Some(describe_value(&Value::String(p.item_uri.clone()))),
                }],
            );
        }
        if let Some(reason) = info.read_only_reason() {
            return validation_failure(
                &class_name,
                &[ValidationError {
                    property: info.name.clone(),
                    problem: format!("read-only: {reason}"),
                    expected_type: Some(info.type_name.clone()),
                    cardinality: Some(info.cardinality_text().to_string()),
                    received: None,
                }],
            );
        }
        if let Err(e) = coerce_scalar(info, &Value::String(p.item_uri.clone())) {
            return validation_failure(&class_name, &[e]);
        }

        match fetch_instance(&perspective, &class_name, &p.base_uri).await {
            Ok(Some(_)) => {}
            Ok(None) => return not_found(&class_name, &p.base_uri),
            Err(e) => return error_json(format!("Error reading {class_name} instance: {e}")),
        }

        let predicate =
            match shacl::resolve_property_predicate(&perspective, &class_name, &info.name).await {
                Ok(pred) => pred,
                Err(e) => {
                    return error_json(format!("Error resolving collection '{}': {}", info.name, e))
                }
            };
        let target = Self::create_property_expression(
            &perspective,
            &class_name,
            &info.name,
            &p.item_uri,
            &agent_context,
        )
        .await;
        let link = Link {
            source: p.base_uri.clone(),
            predicate: Some(predicate),
            target,
        };
        match perspective
            .add_link(link, LinkStatus::Shared, None, &agent_context)
            .await
        {
            Ok(_) => pretty(&json!({
                "success": true,
                "class_name": class_name,
                "base_uri": p.base_uri,
                "collection": info.name,
                "item_uri": p.item_uri,
            })),
            Err(e) => error_json(format!(
                "Error adding to collection '{}': {:#}",
                info.name, e
            )),
        }
    }

    /// Remove an instance and every link touching it.
    #[tool(
        description = "Delete an instance of a subject class: removes all its property links, its type markers, and every inbound link from other instances (e.g. collection membership). Refuses if no instance of class_name exists at base_uri, so a wrong class or URI cannot delete something else. ⚠️ Irreversible."
    )]
    pub async fn instance_remove(&self, params: Parameters<InstanceRemoveParams>) -> String {
        let p = &params.0;
        let (mut perspective, _agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let (class_name, _shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };
        match fetch_instance(&perspective, &class_name, &p.base_uri).await {
            Ok(Some(_)) => {}
            Ok(None) => return not_found(&class_name, &p.base_uri),
            Err(e) => return error_json(format!("Error reading {class_name} instance: {e}")),
        }

        let removed = remove_all_links_of(&mut perspective, &p.base_uri).await;
        pretty(&json!({
            "success": true,
            "class_name": class_name,
            "deleted": p.base_uri,
            "links_removed": removed,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mcp::server::McpContext;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::perspectives::register_perspective;
    use tokio::sync::RwLock;

    /// Flux-shaped Channel: string / boolean / integer scalars, one
    /// `has_child` collection, class + property interpretation hints.
    const CHANNEL_SDNA: &str = r#"{
      "target_class": "flux://Channel",
      "interpretation_hint": "A chat room that groups messages by topic.",
      "constructor_actions": [
        {"action":"addLink","source":"this","predicate":"flux://entry_type","target":"flux://has_channel"},
        {"action":"addLink","source":"this","predicate":"rdf://type","target":"flux://Channel"}
      ],
      "properties": [
        {"path":"flux://channel_name","name":"name","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
         "interpretation_hint":"Short room name.",
         "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://channel_name","target":"value"}]},
        {"path":"flux://channel_description","name":"description","datatype":"xsd:string","min_count":0,"max_count":1,"writable":true,
         "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://channel_description","target":"value"}]},
        {"path":"flux://channel_is_pinned","name":"isPinned","datatype":"xsd:boolean","min_count":0,"max_count":1,"writable":true,
         "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://channel_is_pinned","target":"value"}]},
        {"path":"flux://channel_rank","name":"rank","datatype":"xsd:integer","min_count":0,"max_count":1,"writable":true,
         "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://channel_rank","target":"value"}]},
        {"path":"ad4m://has_child","name":"messages","collection":true,"writable":true,
         "interpretation_hint":"The messages posted in this room.",
         "adder":[{"action":"addLink","source":"this","predicate":"ad4m://has_child","target":"value"}],
         "remover":[{"action":"removeLink","source":"this","predicate":"ad4m://has_child","target":"value"}]}
      ]
    }"#;

    /// Message whose body is stored as a signed literal envelope — the read
    /// path has to unwrap it to the plain text.
    const MESSAGE_SDNA: &str = r#"{
      "target_class": "flux://Message",
      "interpretation_hint": "One chat message.",
      "constructor_actions": [
        {"action":"addLink","source":"this","predicate":"flux://entry_type","target":"flux://has_message"},
        {"action":"addLink","source":"this","predicate":"rdf://type","target":"flux://Message"}
      ],
      "properties": [
        {"path":"flux://body","name":"body","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
         "resolve_language":"literal","interpretation_hint":"The message text.",
         "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://body","target":"value"}]}
      ]
    }"#;

    /// A perspective with both classes registered in the global registry, and
    /// an MCP handler authenticated as admin against it — the same path an
    /// external client takes minus the HTTP transport.
    /// Unregisters the fixture perspective when the test ends (also on
    /// panic), so tests that assert on empty global state stay honest.
    struct PerspectiveGuard(String);
    impl Drop for PerspectiveGuard {
        fn drop(&mut self) {
            crate::perspectives::unregister_perspective(&self.0);
        }
    }

    async fn setup(dynamic_class_tools: bool) -> (Ad4mMcpHandler, String, PerspectiveGuard) {
        let (perspective, _shapes, _ctx) =
            setup_perspective_no_llm(&[("Channel", CHANNEL_SDNA), ("Message", MESSAGE_SDNA)]).await;
        let uuid = perspective.persisted.lock().await.uuid.clone();
        register_perspective(uuid.clone(), perspective);
        let handler = Ad4mMcpHandler::new(McpContext {
            admin_credential: Some("test-admin".to_string()),
            auth_token: Arc::new(RwLock::new(Some("test-admin".to_string()))),
            dynamic_class_tools,
        });
        let guard = PerspectiveGuard(uuid.clone());
        (handler, uuid, guard)
    }

    fn parse(s: &str) -> Value {
        serde_json::from_str(s).unwrap_or_else(|e| panic!("tool returned non-JSON ({e}): {s}"))
    }

    fn find<'a>(list: &'a [Value], name: &str) -> &'a Value {
        list.iter()
            .find(|v| v["name"] == name)
            .unwrap_or_else(|| panic!("no entry named {name} in {list:?}"))
    }

    fn props(pairs: &[(&str, Value)]) -> Map<String, Value> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect()
    }

    #[test]
    fn friendly_type_maps_xsd_datatypes() {
        assert_eq!(friendly_type(None), "string");
        assert_eq!(friendly_type(Some("xsd://string")), "string");
        assert_eq!(friendly_type(Some("xsd:string")), "string");
        assert_eq!(friendly_type(Some("xsd:boolean")), "boolean");
        assert_eq!(friendly_type(Some("xsd://integer")), "integer");
        assert_eq!(friendly_type(Some("xsd:int")), "integer");
        assert_eq!(friendly_type(Some("xsd:decimal")), "number");
        assert_eq!(friendly_type(Some("xsd:double")), "number");
        assert_eq!(friendly_type(Some("xsd:dateTime")), "datetime");
        assert_eq!(
            friendly_type(Some("http://www.w3.org/2001/XMLSchema#date")),
            "datetime"
        );
        assert_eq!(friendly_type(Some("xsd:hexBinary")), "hexbinary");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn describe_perspective_reports_schema_as_data() {
        let (handler, uuid, _guard) = setup(false).await;
        let out = handler
            .describe_perspective(Parameters(DescribePerspectiveParams {
                perspective_id: uuid.clone(),
            }))
            .await;
        let desc = parse(&out);
        assert_eq!(desc["perspective_id"], uuid);

        let classes = desc["classes"].as_array().expect("classes array");
        let channel = find(classes, "Channel");
        assert_eq!(channel["class_uri"], "flux://Channel");
        assert_eq!(
            channel["interpretation_hint"],
            "A chat room that groups messages by topic."
        );

        let properties = channel["properties"].as_array().expect("properties");
        let name = find(properties, "name");
        assert_eq!(name["type"], "string");
        assert_eq!(name["required"], true);
        assert_eq!(name["cardinality"], json!({"min": 1, "max": 1}));
        assert_eq!(name["predicate"], "flux://channel_name");
        assert_eq!(name["interpretation_hint"], "Short room name.");
        assert_eq!(find(properties, "isPinned")["type"], "boolean");
        assert_eq!(find(properties, "rank")["type"], "integer");
        assert_eq!(find(properties, "description")["required"], false);
        assert!(
            properties.iter().all(|p| p["name"] != "messages"),
            "collections must not be listed under properties"
        );

        let collections = channel["collections"].as_array().expect("collections");
        let messages = find(collections, "messages");
        assert_eq!(messages["predicate"], "ad4m://has_child");
        assert_eq!(messages["cardinality"]["max"], Value::Null);
        assert_eq!(
            messages["interpretation_hint"],
            "The messages posted in this room."
        );

        let message = find(classes, "Message");
        let body = find(message["properties"].as_array().unwrap(), "body");
        assert_eq!(body["required"], true);
        assert_eq!(body["resolve_language"], "literal");

        assert!(desc["flows"].as_array().unwrap().is_empty());
        assert!(desc["usage"].as_str().unwrap().contains("instance_create"));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn validation_names_property_type_and_cardinality() {
        let (_handler, uuid, _guard) = setup(false).await;
        let perspective = crate::perspectives::get_perspective(&uuid).unwrap();
        let shape = perspective.get_shape("Channel").expect("Channel shape");

        // Missing required property.
        let errs = validate_properties(
            &shape,
            &props(&[("description", json!("no name"))]),
            WriteMode::Create,
        )
        .expect_err("missing name must fail");
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].property, "name");
        assert!(errs[0].problem.contains("missing required"));
        assert_eq!(errs[0].expected_type.as_deref(), Some("string"));
        assert!(errs[0]
            .cardinality
            .as_deref()
            .unwrap()
            .contains("minCount 1"));

        // Wrong type + array on a single-valued property: both reported.
        let errs = validate_properties(
            &shape,
            &props(&[("name", json!(["a", "b"])), ("isPinned", json!("maybe"))]),
            WriteMode::Create,
        )
        .expect_err("bad values must fail");
        assert_eq!(errs.len(), 2, "{errs:?}");
        let pinned = errs.iter().find(|e| e.property == "isPinned").unwrap();
        assert_eq!(pinned.expected_type.as_deref(), Some("boolean"));
        assert!(pinned.received.as_deref().unwrap().contains("maybe"));
        let name = errs.iter().find(|e| e.property == "name").unwrap();
        assert!(name.problem.contains("array of 2"));
        assert!(name.cardinality.as_deref().unwrap().contains("maxCount 1"));

        // Unknown property lists the available ones.
        let errs = validate_properties(
            &shape,
            &props(&[("name", json!("x")), ("nmae", json!("typo"))]),
            WriteMode::Create,
        )
        .expect_err("unknown property must fail");
        assert_eq!(errs[0].property, "nmae");
        assert!(errs[0].problem.contains("unknown property"));
        assert!(errs[0].problem.contains("description"));

        // Lenient coercion where intent is unambiguous.
        let ok = validate_properties(
            &shape,
            &props(&[
                ("name", json!("general")),
                ("rank", json!("7")),
                ("isPinned", json!("TRUE")),
                ("messages", json!(["flux://m1", "flux://m2"])),
            ]),
            WriteMode::Create,
        )
        .expect("valid create");
        assert_eq!(ok.scalars["rank"], json!(7));
        assert_eq!(ok.scalars["isPinned"], json!(true));
        assert_eq!(
            ok.collections,
            vec![(
                "messages".to_string(),
                vec![json!("flux://m1"), json!("flux://m2")]
            )]
        );

        // Case-insensitive property names resolve to the canonical spelling.
        let ok = validate_properties(
            &shape,
            &props(&[("Name", json!("x")), ("ispinned", json!(false))]),
            WriteMode::Create,
        )
        .expect("case-insensitive names");
        assert!(ok.scalars.contains_key("name"));
        assert!(ok.scalars.contains_key("isPinned"));

        // Update: no required check, collections refused.
        let ok = validate_properties(
            &shape,
            &props(&[("description", json!("only this"))]),
            WriteMode::Update,
        )
        .expect("partial update");
        assert_eq!(ok.scalars.len(), 1);
        let errs = validate_properties(
            &shape,
            &props(&[("messages", json!(["flux://m1"]))]),
            WriteMode::Update,
        )
        .expect_err("collection on update must fail");
        assert!(errs[0].problem.contains("instance_add_to_collection"));

        // The error payload carries the summary in `error` too.
        let payload = parse(&validation_failure("Channel", &errs));
        assert!(payload["error"].as_str().unwrap().contains("messages"));
        assert_eq!(payload["validation_errors"][0]["property"], "messages");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn instance_tools_round_trip_typed_data() {
        let (handler, uuid, _guard) = setup(false).await;

        // Create a channel with typed values.
        let out = handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                properties: Some(props(&[
                    ("name", json!("general")),
                    ("isPinned", json!(true)),
                    ("rank", json!(3)),
                ])),
                base_uri: None,
                parent: None,
            }))
            .await;
        let created = parse(&out);
        assert_eq!(created["created"], true, "{out}");
        let channel = created["base_uri"].as_str().unwrap().to_string();

        // Read it back: native JSON types, not strings.
        let got = parse(
            &handler
                .instance_get(Parameters(InstanceGetParams {
                    perspective_id: uuid.clone(),
                    class_name: "Channel".into(),
                    base_uri: channel.clone(),
                }))
                .await,
        );
        assert_eq!(got["id"], channel);
        assert_eq!(got["name"], "general");
        assert_eq!(got["isPinned"], true);
        assert_eq!(got["rank"], 3);

        // Validation happens before any write: nothing created on failure.
        let bad = parse(
            &handler
                .instance_create(Parameters(InstanceCreateParams {
                    perspective_id: uuid.clone(),
                    class_name: "Channel".into(),
                    properties: Some(props(&[("isPinned", json!("maybe"))])),
                    base_uri: Some("flux://never-created".into()),
                    parent: None,
                }))
                .await,
        );
        let err = bad["error"].as_str().unwrap();
        assert!(err.contains("name") && err.contains("isPinned"), "{err}");
        assert!(
            err.contains("boolean") && err.contains("minCount 1"),
            "{err}"
        );
        let missing = parse(
            &handler
                .instance_get(Parameters(InstanceGetParams {
                    perspective_id: uuid.clone(),
                    class_name: "Channel".into(),
                    base_uri: "flux://never-created".into(),
                }))
                .await,
        );
        assert!(missing["error"]
            .as_str()
            .unwrap()
            .contains("No Channel instance"));

        // Messages: one linked via `parent`, one via the collection tool.
        let m1 = parse(
            &handler
                .instance_create(Parameters(InstanceCreateParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    properties: Some(props(&[("body", json!("Hello from a static tool"))])),
                    base_uri: None,
                    parent: Some(channel.clone()),
                }))
                .await,
        );
        assert_eq!(m1["created"], true, "{m1}");
        assert_eq!(m1["added_to_parent"], true);
        let msg1 = m1["base_uri"].as_str().unwrap().to_string();

        let m2 = parse(
            &handler
                .instance_create(Parameters(InstanceCreateParams {
                    perspective_id: uuid.clone(),
                    class_name: "message".into(), // case-insensitive class name
                    properties: Some(props(&[("body", json!("Second message"))])),
                    base_uri: None,
                    parent: None,
                }))
                .await,
        );
        assert_eq!(m2["created"], true, "{m2}");
        assert_eq!(m2["class_name"], "Message");
        let msg2 = m2["base_uri"].as_str().unwrap().to_string();

        let add = parse(
            &handler
                .instance_add_to_collection(Parameters(InstanceAddToCollectionParams {
                    perspective_id: uuid.clone(),
                    class_name: "Channel".into(),
                    base_uri: channel.clone(),
                    collection: "messages".into(),
                    item_uri: msg2.clone(),
                }))
                .await,
        );
        assert_eq!(add["success"], true, "{add}");

        let got = parse(
            &handler
                .instance_get(Parameters(InstanceGetParams {
                    perspective_id: uuid.clone(),
                    class_name: "Channel".into(),
                    base_uri: channel.clone(),
                }))
                .await,
        );
        let members = got["messages"].as_array().expect("messages array");
        assert!(members.contains(&json!(msg1)) && members.contains(&json!(msg2)));

        // Query: envelope-stored bodies come back as plain text; parent
        // scope and filters narrow; pagination reports the full count.
        let all = parse(
            &handler
                .instance_query(Parameters(InstanceQueryParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    filter: None,
                    parent: None,
                    limit: None,
                    offset: None,
                }))
                .await,
        );
        assert_eq!(all["count"], 2, "{all}");
        let bodies: Vec<&str> = all["instances"]
            .as_array()
            .unwrap()
            .iter()
            .map(|i| i["body"].as_str().unwrap())
            .collect();
        assert!(bodies.contains(&"Hello from a static tool"));
        assert!(bodies.contains(&"Second message"));

        let in_channel = parse(
            &handler
                .instance_query(Parameters(InstanceQueryParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    filter: None,
                    parent: Some(channel.clone()),
                    limit: None,
                    offset: None,
                }))
                .await,
        );
        assert_eq!(in_channel["count"], 2, "{in_channel}");

        let filtered = parse(
            &handler
                .instance_query(Parameters(InstanceQueryParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    filter: Some(props(&[("body", json!({"contains": "Second"}))])),
                    parent: None,
                    limit: None,
                    offset: None,
                }))
                .await,
        );
        assert_eq!(filtered["count"], 1, "{filtered}");
        assert_eq!(filtered["instances"][0]["id"], msg2);

        let paged = parse(
            &handler
                .instance_query(Parameters(InstanceQueryParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    filter: None,
                    parent: None,
                    limit: Some(1),
                    offset: None,
                }))
                .await,
        );
        assert_eq!(paged["count"], 1);
        assert_eq!(paged["total_count"], 2);

        let bad_filter = parse(
            &handler
                .instance_query(Parameters(InstanceQueryParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    filter: Some(props(&[("subject", json!("x"))])),
                    parent: None,
                    limit: None,
                    offset: None,
                }))
                .await,
        );
        let err = bad_filter["error"].as_str().unwrap();
        assert!(err.contains("subject") && err.contains("body"), "{err}");

        // Update: partial, validated, atomic.
        let upd = parse(
            &handler
                .instance_update(Parameters(InstanceUpdateParams {
                    perspective_id: uuid.clone(),
                    class_name: "Channel".into(),
                    base_uri: channel.clone(),
                    properties: props(&[
                        ("name", json!("general-renamed")),
                        ("isPinned", json!(false)),
                    ]),
                }))
                .await,
        );
        assert_eq!(upd["success"], true, "{upd}");
        let got = parse(
            &handler
                .instance_get(Parameters(InstanceGetParams {
                    perspective_id: uuid.clone(),
                    class_name: "Channel".into(),
                    base_uri: channel.clone(),
                }))
                .await,
        );
        assert_eq!(got["name"], "general-renamed");
        assert_eq!(got["isPinned"], false);
        assert_eq!(got["rank"], 3, "untouched property must survive the update");

        let bad_upd = parse(
            &handler
                .instance_update(Parameters(InstanceUpdateParams {
                    perspective_id: uuid.clone(),
                    class_name: "Channel".into(),
                    base_uri: channel.clone(),
                    properties: props(&[("messages", json!([msg1.clone()]))]),
                }))
                .await,
        );
        assert!(bad_upd["error"]
            .as_str()
            .unwrap()
            .contains("instance_add_to_collection"));

        // Remove: refuses when the class doesn't match, cascades when it does.
        let wrong = parse(
            &handler
                .instance_remove(Parameters(InstanceRemoveParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    base_uri: channel.clone(),
                }))
                .await,
        );
        assert!(wrong["error"]
            .as_str()
            .unwrap()
            .contains("No Message instance"));

        let removed = parse(
            &handler
                .instance_remove(Parameters(InstanceRemoveParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    base_uri: msg2.clone(),
                }))
                .await,
        );
        assert_eq!(removed["success"], true, "{removed}");
        assert!(removed["links_removed"].as_u64().unwrap() > 0);

        let left = parse(
            &handler
                .instance_query(Parameters(InstanceQueryParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    filter: None,
                    parent: None,
                    limit: None,
                    offset: None,
                }))
                .await,
        );
        assert_eq!(left["count"], 1);
        assert_eq!(left["instances"][0]["id"], msg1);

        // Unknown class: lists what is registered.
        let unknown = parse(
            &handler
                .instance_query(Parameters(InstanceQueryParams {
                    perspective_id: uuid.clone(),
                    class_name: "Widget".into(),
                    filter: None,
                    parent: None,
                    limit: None,
                    offset: None,
                }))
                .await,
        );
        let err = unknown["error"].as_str().unwrap();
        assert!(err.contains("Widget") && err.contains("Channel") && err.contains("Message"));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn dynamic_tools_are_hidden_unless_flag_is_set() {
        let (hidden, uuid, _guard) = setup(false).await;
        let names: Vec<String> = hidden
            .exposed_tools()
            .await
            .iter()
            .map(|t| t.name.to_string())
            .collect();
        for expected in [
            "describe_perspective",
            "instance_create",
            "instance_query",
            "instance_get",
            "instance_update",
            "instance_add_to_collection",
            "instance_remove",
        ] {
            assert!(names.iter().any(|n| n == expected), "missing {expected}");
        }
        assert!(
            !names
                .iter()
                .any(|n| n.starts_with("channel_") || n.starts_with("message_")),
            "per-class tools leaked into the static surface: {names:?}"
        );

        let refused = hidden
            .dispatch_non_router_tool(
                "channel_create",
                Some(props(&[
                    ("perspective_id", json!(uuid.clone())),
                    ("name", json!("x")),
                ])),
            )
            .await
            .expect("dispatch returns a tool result, not a protocol error");
        assert_eq!(refused.is_error, Some(true));
        let text = refused
            .content
            .iter()
            .filter_map(|c| c.as_text().map(|t| t.text.clone()))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("channel_create") && text.contains("instance_create"));
        assert!(text.contains("dynamicClassTools"));

        // Same perspective, flag on: both surfaces are advertised.
        let shown = Ad4mMcpHandler::new(McpContext {
            admin_credential: Some("test-admin".to_string()),
            auth_token: Arc::new(RwLock::new(Some("test-admin".to_string()))),
            dynamic_class_tools: true,
        });
        let names: Vec<String> = shown
            .exposed_tools()
            .await
            .iter()
            .map(|t| t.name.to_string())
            .collect();
        assert!(names.iter().any(|n| n == "channel_create"), "{names:?}");
        assert!(names.iter().any(|n| n == "instance_create"));
    }
}

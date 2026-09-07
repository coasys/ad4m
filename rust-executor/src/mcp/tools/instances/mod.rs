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
//!
//! ## Module layout
//!
//! - [`describe`] — `describe_perspective` and the schema-as-data projection
//! - [`create`] — `instance_create`
//! - [`query`] — `instance_query` + `instance_get`
//! - [`update`] — `instance_update` + `instance_remove`
//! - [`collections`] — `instance_add_to_collection` + `instance_remove_from_collection`
//! - [`children`] — class-agnostic `add_child` / `get_children` on the
//!   `ad4m://has_child` tree
//! - [`transcript`] — `instance_transcript`, a compact chronological reading
//!   of one class's children under a node
//! - [`commands`] — `execute_commands`, raw SDNA actions on an instance
//! - [`validate`] — SHACL-derived property validation shared by every write
//! - this file — the borrowed [`PropView`] over a class shape, class
//!   resolution, instance URI minting and the other helpers the tools share

pub mod children;
pub mod collections;
pub mod commands;
pub mod create;
pub mod describe;
pub mod query;
pub mod transcript;
pub mod update;
pub(crate) mod validate;

#[cfg(test)]
mod tests;

pub use children::{AddChildParams, GetChildrenParams};
pub use collections::{InstanceAddToCollectionParams, InstanceRemoveFromCollectionParams};
pub use commands::ExecuteCommandsParams;
pub use create::InstanceCreateParams;
pub use describe::DescribePerspectiveParams;
pub use query::{InstanceGetParams, InstanceQueryParams};
pub use transcript::InstanceTranscriptParams;
pub use update::{InstanceRemoveParams, InstanceUpdateParams};
pub(crate) use validate::{
    coerce_scalar, describe_value, normalize_filter, validate_properties, validation_failure,
    ValidationError, WriteMode,
};

use super::Ad4mMcpHandler;
use crate::mcp::shacl;
use crate::perspectives::model_query::types::{ModelShape, ShapeProperty, ShapeRelation};
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::LinkQuery;
use serde_json::{json, Map, Value};
use std::sync::Arc;
use std::time::Duration;

/// Default page size for `instance_query` when the caller passes no `limit`.
/// Bounded so a class with thousands of instances doesn't flood the LLM
/// context; `total_count` in the response tells the caller there is more.
pub(crate) const DEFAULT_QUERY_LIMIT: usize = 100;
/// Hard ceiling on explicit `instance_query` limits — the LLM-context
/// protection holds even when a caller asks for more.
pub(crate) const MAX_QUERY_LIMIT: usize = 500;

/// How long class resolution waits for a class's SHACL to arrive over
/// p-diff-sync on a freshly joined neighbourhood before reporting it as
pub(super) const CLASS_SYNC_WAIT: Duration = Duration::from_secs(5);

/// Predicate used for the optional `parent` argument on `instance_create` /
/// `instance_query` — the same child link the per-class `{class}_create` /
/// `{class}_list` tools use.
pub(super) const HAS_CHILD: &str = "ad4m://has_child";

// ============================================================================
// Schema projection — SHACL shape → data an agent can read
// ============================================================================

/// One property of a class as it is presented to (and validated for) MCP
/// clients.
///
/// A *borrowed view* over the `ShapeProperty` (plus its `ShapeRelation`, when
/// the property is a link) that the query pipeline already loaded — not a
/// second copy of the schema. Everything the MCP layer needs is either a
/// field of those two or a one-line derivation from them, so this type owns
/// no state and cannot drift from what `model_query` validates against.
#[derive(Debug, Clone, Copy)]
pub(crate) struct PropView<'a> {
    prop: &'a ShapeProperty,
    /// `Some` when the property is a relation — matched by name in
    /// `ModelShape::include_relations`.
    relation: Option<&'a ShapeRelation>,
}

impl<'a> PropView<'a> {
    pub(crate) fn name(&self) -> &'a str {
        &self.prop.name
    }

    pub(crate) fn predicate(&self) -> &'a str {
        &self.prop.predicate
    }

    /// Friendly type name: `string` / `boolean` / `integer` / `number` /
    /// `datetime` / `reference` (link to another instance) / raw xsd local
    /// name for anything else.
    pub(crate) fn type_name(&self) -> String {
        if self.relation.is_some() {
            "reference".to_string()
        } else {
            friendly_type(self.prop.datatype.as_deref())
        }
    }

    pub(crate) fn required(&self) -> bool {
        self.prop.is_required
    }

    /// Multi-valued (collection or `*Many` relation).
    ///
    /// Relations are all `is_collection` in the shape (the pipeline hydrates
    /// them as arrays); `is_scalar_relation` is what tells us which ones are
    /// really single-valued.
    pub(crate) fn collection(&self) -> bool {
        if self.relation.is_some() {
            !self.prop.is_scalar_relation
        } else {
            self.prop.is_collection
        }
    }

    /// Relation kind (`hasOne`, `hasMany`, `belongsToOne`, `belongsToMany`)
    /// and the target class name, for link-typed properties.
    pub(crate) fn relation(&self) -> Option<(&'a str, &'a str)> {
        self.relation
            .map(|r| (r.kind.as_str(), r.target_class_name.as_str()))
    }

    /// `belongsTo*` — the link is stored on the *other* instance, so this
    /// side cannot write it.
    pub(crate) fn reverse(&self) -> bool {
        self.prop.direction.as_deref() == Some("reverse")
    }

    /// Class marker (`sh:hasValue` + `minCount 1`), set by the constructor.
    /// Never supplied by clients.
    pub(crate) fn flag(&self) -> bool {
        self.prop.is_flag
    }

    /// Derived via a getter expression; read-only. Relations carry a getter
    /// too (it encodes conformance filtering), so they are excluded.
    pub(crate) fn computed(&self) -> bool {
        self.relation.is_none() && self.prop.getter.is_some()
    }

    pub(crate) fn resolve_language(&self) -> Option<&'a str> {
        self.prop.resolve_language.as_deref()
    }

    pub(crate) fn interpretation_hint(&self) -> Option<&'a str> {
        self.prop.interpretation_hint.as_deref()
    }

    /// Dedup identity of the class (the "title-like" key).
    pub(crate) fn identity(&self) -> bool {
        self.prop.identity
    }

    /// Whether an MCP client may write this property at all.
    pub(crate) fn writable(&self) -> bool {
        !self.flag() && !self.reverse() && !self.computed()
    }

    pub(super) fn read_only_reason(&self) -> Option<&'static str> {
        if self.flag() {
            Some("class marker set automatically on create")
        } else if self.reverse() {
            Some("reverse relation — stored on the target instance")
        } else if self.computed() {
            Some("computed by a getter expression")
        } else {
            None
        }
    }

    /// Cardinality as data: `{"min": 0|1, "max": 1|null}`.
    pub(crate) fn cardinality(&self) -> Value {
        json!({
            "min": if self.required() { 1 } else { 0 },
            "max": if self.collection() { Value::Null } else { json!(1) },
        })
    }

    /// Cardinality as words, for error messages.
    pub(crate) fn cardinality_text(&self) -> &'static str {
        match (self.required(), self.collection()) {
            (true, false) => "exactly one value (minCount 1, maxCount 1)",
            (false, false) => "at most one value (maxCount 1)",
            (true, true) => "one or more values (minCount 1, collection)",
            (false, true) => "any number of values (collection)",
        }
    }

    pub(super) fn to_json(&self) -> Value {
        let mut v = json!({
            "name": self.name(),
            "type": self.type_name(),
            "required": self.required(),
            "cardinality": self.cardinality(),
            "predicate": self.predicate(),
        });
        if let Some((kind, target)) = self.relation() {
            v["relation_kind"] = json!(kind);
            v["target_class"] = json!(target);
        }
        if let Some(lang) = self.resolve_language() {
            v["resolve_language"] = json!(lang);
        }
        if let Some(hint) = self.interpretation_hint() {
            v["interpretation_hint"] = json!(hint);
        }
        if self.identity() {
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
pub(crate) fn class_properties(shape: &ModelShape) -> Vec<PropView<'_>> {
    shape
        .properties
        .iter()
        .map(|prop| PropView {
            prop,
            relation: shape.include_relations.iter().find(|r| r.name == prop.name),
        })
        .collect()
}

/// Look a property up by name, exact first, then case-insensitively — agents
/// get the casing wrong often enough that a hard failure is not worth it.
pub(crate) fn find_property<'a, 'p>(
    infos: &'a [PropView<'p>],
    name: &str,
) -> Option<&'a PropView<'p>> {
    infos
        .iter()
        .find(|i| i.name() == name)
        .or_else(|| infos.iter().find(|i| i.name().eq_ignore_ascii_case(name)))
}

// ============================================================================
// Shared plumbing
// ============================================================================

pub(super) fn pretty(v: &Value) -> String {
    serde_json::to_string_pretty(v).unwrap_or_else(|e| format!("Error: {e}"))
}

pub(super) fn error_json(msg: impl Into<String>) -> String {
    json!({ "error": msg.into() }).to_string()
}

/// Random instance URI, in the same form the TypeScript SDK mints for a
/// `Ad4mModel` without an explicit base expression (`Ad4mModel.ts:468`):
/// `ad4m://obj/` + 24 lowercase letters.
///
/// Deliberately *not* a `literal:` URI. Two reasons:
/// 1. `literal://string:x` is not a parseable IRI (`string:x` reads as
///    host:port with a non-numeric port), so it cannot be emitted inside
///    `<…>` in SPARQL — every query that inlines an id breaks on it.
/// 2. The single-colon form `literal:string:x` is not an option either: the
///    store decodes `literal:*:` *targets* into RDF literals
///    (`sparql_store::target_to_storage_term`), so an id in that form would
///    stop being a node the moment it is used as a link target — which is
///    exactly what a `parent` / collection link does.
pub(crate) fn generate_instance_uri() -> String {
    let random_id: String = (0..24)
        .map(|_| (b'a' + (rand::random::<u8>() % 26)) as char)
        .collect();
    format!("ad4m://obj/{random_id}")
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
pub(super) async fn run_model_query(
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

pub(super) fn not_found(class_name: &str, base_uri: &str) -> String {
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
pub(super) async fn check_setters(
    perspective: &PerspectiveInstance,
    shape: &ModelShape,
    scalars: &Map<String, Value>,
) -> Vec<ValidationError> {
    let infos = class_properties(shape);
    let mut errors = Vec::new();
    for name in scalars.keys() {
        if !has_setter(perspective, &shape.target_class, name).await {
            let info = infos.iter().find(|i| i.name() == name);
            errors.push(ValidationError {
                property: name.clone(),
                problem: "read-only: the class declares no setter for this property".to_string(),
                expected_type: info.map(|i| i.type_name()),
                cardinality: info.map(|i| i.cardinality_text().to_string()),
                received: None,
            });
        }
    }
    errors
}

pub(super) fn subject_class(class_name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(class_name.to_string()),
        query: None,
    }
}

/// The plain value behind a literal link target: a `literal:string:x` decodes
/// to `x`, a signed literal envelope (`literal:json:{…,"data":…}`) to its
/// `data` payload. Non-literal targets come back unchanged.
pub(super) fn decoded_literal(target: &str) -> String {
    let decoded = Ad4mMcpHandler::resolve_literal_value(target);
    if decoded == target {
        return decoded;
    }
    if let Ok(Value::Object(envelope)) = serde_json::from_str::<Value>(&decoded) {
        if let Some(data) = envelope.get("data") {
            return match data {
                Value::String(s) => s.clone(),
                other => other.to_string(),
            };
        }
    }
    decoded
}

/// URI-or-literal encoding for link targets, shared with the per-class tools.
///
/// Anything that already is a URI passes through: `scheme://…`, the
/// single-colon `literal:…` form the store hands back as link targets (so an
/// id read from `get_children` / `instance_get` can be passed straight back
/// in without being wrapped a second time), and `did:…`. Everything else is a
/// bare string and becomes a literal URI.
pub(super) fn link_target(value: &str) -> String {
    if value.contains("://") || value.starts_with("literal:") || value.starts_with("did:") {
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

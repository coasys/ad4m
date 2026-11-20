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
//! Correctness is unchanged. Every write through the typed tools is validated
//! against the class's SHACL shape *before* anything is written
//! ([`validate_properties`]); the one exception is [`commands`]'s
//! `execute_commands`, the deliberate raw-SDNA escape hatch documented in its
//! own module doc, which applies `{action, source, predicate, target}`
//! commands with no SHACL validation at all. A validation
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
//!   containment tree (the built-in `Container`)
//! - [`transcript`] — `instance_transcript`, a compact chronological reading
//!   of the children under a node that carry text, through the built-in
//!   TextContent
//! - [`commands`] — `execute_commands`, raw SDNA actions on an instance
//! - [`template_tools`] — `template_read` + `template_update`: a WE template, edited by WE's
//!   own rules (`tool_bundles::builtin`)
//! - [`validate`] — the text of a validation failure, and `filter` checks
//! - this file — the describe projection of a property, and the tools'
//!   text for the shared write core's errors
//!
//! The writes themselves (`instance_create` / `_update` / `_remove` /
//! `_add_to_collection` / `_remove_from_collection`) are
//! [`crate::perspectives::instances`], the same functions the
//! `perspective.instance*` RPC methods call; the tools here format their
//! results and errors as they always have.

pub mod children;
pub mod collections;
pub mod commands;
pub mod create;
pub mod describe;
pub mod query;
pub mod template_tools;
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
    normalize_filter, validate_properties, validation_failure, ValidationError, WriteMode,
};

pub(crate) use crate::perspectives::instances::{
    class_properties, fetch_instance, find_property, generate_instance_uri, link_target,
    subject_class, PropView,
};
// What the tests reach through `use super::*` from before the write core moved out.
#[cfg(test)]
use crate::{perspectives::instances::friendly_type, types::LinkQuery};

use super::Ad4mMcpHandler;
use crate::perspectives::instances::{self as shared, InstanceError, ReadOnlyReason};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use serde_json::{json, Map, Value};
use std::sync::Arc;

/// Default page size for `instance_query` when the caller passes no `limit`.
/// Bounded so a class with thousands of instances doesn't flood the LLM
/// context; `total_count` in the response tells the caller there is more.
pub(crate) const DEFAULT_QUERY_LIMIT: usize = 100;
/// Hard ceiling on explicit `instance_query` limits — the LLM-context
/// protection holds even when a caller asks for more.
pub(crate) const MAX_QUERY_LIMIT: usize = 500;

// ============================================================================
// Schema projection — a property as `describe_perspective` shows it
// ============================================================================

/// Why a property is read-only, in words.
pub(crate) fn read_only_text(reason: ReadOnlyReason) -> &'static str {
    match reason {
        ReadOnlyReason::ClassMarker => "class marker set automatically on create",
        ReadOnlyReason::ReverseRelation => "reverse relation — stored on the target instance",
        ReadOnlyReason::Computed => "computed by a getter expression",
        ReadOnlyReason::NoSetter => "the class declares no setter for this property",
    }
}

/// Cardinality as words, for error messages.
pub(crate) fn cardinality_text(cardinality: shared::Cardinality) -> &'static str {
    match (cardinality.min > 0, cardinality.max.is_none()) {
        (true, false) => "exactly one value (minCount 1, maxCount 1)",
        (false, false) => "at most one value (maxCount 1)",
        (true, true) => "one or more values (minCount 1, collection)",
        (false, true) => "any number of values (collection)",
    }
}

/// One property as `describe_perspective` presents it.
pub(super) fn prop_json(info: &PropView<'_>) -> Value {
    let cardinality = info.cardinality();
    let mut v = json!({
        "name": info.name(),
        "type": info.type_name(),
        "required": info.required(),
        "cardinality": {
            "min": cardinality.min,
            "max": cardinality.max,
        },
        "predicate": info.predicate(),
    });
    if let Some((kind, target)) = info.relation() {
        v["relation_kind"] = json!(kind);
        v["target_class"] = json!(target);
    }
    if let Some(lang) = info.resolve_language() {
        v["resolve_language"] = json!(lang);
    }
    if let Some(hint) = info.interpretation_hint() {
        v["interpretation_hint"] = json!(hint);
    }
    if let Some(allowed) = info.in_values() {
        v["allowed_values"] = Value::Array(
            allowed
                .iter()
                .map(|entry| {
                    let mut o = Map::new();
                    o.insert(
                        "value".to_string(),
                        shared::validate::allowed_value(entry).clone(),
                    );
                    if let Some(label) = entry.get("label") {
                        o.insert("label".to_string(), label.clone());
                    }
                    Value::Object(o)
                })
                .collect(),
        );
    }
    if info.identity() {
        v["identity"] = json!(true);
    }
    if info.local() {
        v["local"] = json!(true);
        v["local_note"] = json!(
            "stored locally in this perspective — links for this property are not shared \
             with the neighbourhood, so remote agents cannot read it"
        );
    }
    if let Some(reason) = info.read_only() {
        v["read_only"] = json!(true);
        v["read_only_reason"] = json!(read_only_text(reason));
    }
    v
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

/// The text of the errors every tool words the same way; `None` for the ones each tool words
/// itself (a store failure, a partial remove, …).
pub(super) fn common_error(error: &InstanceError) -> Option<String> {
    Some(match error {
        InstanceError::UnknownClass {
            class_name,
            registered,
        } => error_json(format!(
            "Unknown subject class '{}'. Registered classes: {}. Call describe_perspective to see \
             their schemas.",
            class_name,
            if registered.is_empty() {
                "(none)".to_string()
            } else {
                registered.join(", ")
            }
        )),
        InstanceError::AmbiguousClass {
            class_name,
            candidates,
        } => error_json(format!(
            "'{class_name}' names more than one registered class — pass its target class IRI \
             instead: {}. Call describe_perspective to see their schemas.",
            candidates.join(", ")
        )),
        InstanceError::AbstractClass { class_name } => error_json(format!(
            "'{class_name}' is an abstract class: collection membership is written through the \
             record's own class, the `__subjectClass` a query on '{class_name}' returns. \
             Through '{class_name}', instance_update sets its properties and instance_remove \
             deletes the record."
        )),
        InstanceError::InvalidUri { field, value } => error_json(format!(
            "'{value}' is not a usable {field}: instance ids must be absolute URIs, like the \
             'ad4m://obj/…' id that instance_create returns. Pass the id exactly as \
             instance_query / instance_get / get_children reported it."
        )),
        InstanceError::Validation { class_name, errors } => {
            let errors: Vec<ValidationError> = errors.iter().map(ValidationError::from).collect();
            validation_failure(class_name, &errors)
        }
        InstanceError::NotFound {
            class_name,
            base_uri,
        } => not_found(class_name, base_uri),
        InstanceError::BatchNotFound { batch_id } => {
            error_json(format!("No open batch '{batch_id}' — nothing was written."))
        }
        InstanceError::CallsInBatch {
            class_name,
            batch_id,
        } => error_json(format!(
            "Writing a {class_name} instance calls into its language, which batch \
             '{batch_id}' cannot roll back — nothing was written. Write it without batch_id."
        )),
        _ => return None,
    })
}

/// Resolve a caller-supplied class name to its canonical spelling and shape
/// ([`shared::resolve_class`]), or the tool's JSON error listing the registered classes.
pub(crate) async fn resolve_class(
    perspective: &PerspectiveInstance,
    class_name: &str,
) -> Result<(String, Arc<ModelShape>), String> {
    shared::resolve_class(perspective, class_name)
        .await
        .map_err(|e| common_error(&e).unwrap_or_else(|| error_json(format!("{e:?}"))))
}

pub(super) fn not_found(class_name: &str, base_uri: &str) -> String {
    error_json(format!(
        "No {class_name} instance found at '{base_uri}'. Check base_uri and class_name; use \
         instance_query to list existing {class_name} instances."
    ))
}

/// Reject scalar writes to properties the SDNA can't actually set ([`shared::check_setters`]).
pub(super) async fn check_setters(
    perspective: &PerspectiveInstance,
    shape: &ModelShape,
    scalars: &Map<String, Value>,
) -> Vec<ValidationError> {
    shared::check_setters(perspective, shape, scalars)
        .await
        .iter()
        .map(ValidationError::from)
        .collect()
}

// Shared with the flow engine's role/tombstone matching (which must accept
// the legacy spelling too, or fail open on tombstones — see
// `flow_evaluator::target_names_did`), so the implementations live in
// `crate::utils`; re-exported here for the tool layer's existing imports.
pub(super) use crate::utils::{normalize_legacy_literal, other_literal_spelling};

/// Gate a caller-supplied instance identifier before it is inlined into SPARQL
/// ([`shared::instance_uri`]), or the tool's JSON error.
pub(super) fn instance_uri(field: &str, value: &str) -> Result<String, String> {
    shared::instance_uri(field, value)
        .map_err(|e| common_error(&e).unwrap_or_else(|| error_json(format!("{e:?}"))))
}

//! `perspective.mutate`: validated model writes over the WS RPC. One call takes an ordered list
//! of typed ops (`create`, `update`, `delete`) and writes them in one batch with one commit.
//!
//! The ops run through [`crate::perspectives::instances`], the same functions as the MCP
//! `instance_*` tools: each op's write is checked against the class's SHACL shape before it is
//! written (each property's type, which are required, which take a single value, and `sh:in`; not
//! arbitrary `minCount`/`maxCount` bounds). Guard: `perspective:UPDATE` on the perspective, which
//! the caller's agent must own (`Guard::perspective`). The agent that writes is `ctx.acts_as()`.
//! A managed agent without credits is refused before anything is written; each op that lands
//! reserves one link write.
//!
//! A later op sees the earlier ops of the call: it finds an instance an earlier op created and
//! not one an earlier op removed, so create-then-update, create-then-add-to-collection and
//! create-then-delete of a new id work in one call.
//!
//! All or none. Without `batch` the ops stage into a batch of their own, committed at the end;
//! if any op is refused or fails, the batch is discarded and nothing is written. With `batch`
//! the ops stage into that open batch, which the caller commits (`perspective.commitBatch`); if
//! any op is refused or fails, the batch is put back as the call found it. Calls into one batch
//! run one at a time.
//!
//! A refusal is a `WsRpcError` whose `data` is the [`InstanceError`] (camelCase, tagged by
//! `kind`) plus `opIndex`, the index of the op that failed (absent when the call failed, not an
//! op: an unknown `batch`); see [`instance_error`] for the codes. A validation failure is 422
//! with `data = { kind: "validation", className, errors: ValidationError[], opIndex }`; each
//! error names the `property`, the `problem` (`{ kind, ... }`), `expectedType`, `cardinality`
//! (`{ min, max }`), `allowedValues` (`sh:in`), `received` and, for a collection item, its index
//! (`item`).
//!
//! `returns: { kind: "rows" }` also returns each created or updated instance as a read shows it
//! after the commit; it needs a call without `batch`, and, as a read of the instances, the
//! caller's `perspective:READ` on the perspective too (`Guard::perspective` with
//! `perspective_query_capability`, checked before anything is written).

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::{perspective_query_capability, perspective_update_capability};
use crate::perspectives::instances::{
    self, fetch_instance, InstanceError, MutateError, MutateOp, PerspectiveMutateResult, Returns,
    ValidationProblem,
};
use crate::types::RequestContext;

use super::guards::Guard;
use super::perspectives_ws::{
    check_credits, guarded_perspective, reserve_credits, DEFAULT_LINK_WRITE,
};
use super::ws_handler::{HandlerMap, WsRpcError};

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export, optional_fields)]
pub struct PerspectiveMutateParams {
    pub uuid: String,
    pub ops: Vec<MutateOp>,
    /// An open batch (`perspective.createBatch`) to stage the ops into; they land on its commit.
    /// Absent: the call commits its own batch.
    pub batch: Option<String>,
    /// Default `ids`.
    pub returns: Option<Returns>,
}

/// The RPC error for a refused or failed write: the code, a one-line message, and the
/// [`InstanceError`] as `data`.
///
/// - 422: `validation` (the write breaks the class's shape)
/// - 400: `unknownClass`, `ambiguousClass`, `abstractClass`, `invalidUri`,
///   `emptyProperties`, `storedAsExpressions`
/// - 404: `notFound` (no instance of the class at the URI), `batchNotFound`
/// - 409: `alreadyExists`
/// - 500: `failed`, `partiallyRemoved` (the store failed)
pub(crate) fn instance_error(error: InstanceError) -> WsRpcError {
    let (code, message) = match &error {
        InstanceError::Validation { class_name, errors } => {
            let problems: Vec<String> = errors
                .iter()
                .map(|e| format!("{} ({})", e.property, problem_kind(&e.problem)))
                .collect();
            (
                422,
                format!(
                    "Validation failed for class '{class_name}': {}",
                    problems.join(", ")
                ),
            )
        }
        InstanceError::UnknownClass { class_name, .. } => {
            (400, format!("Unknown subject class '{class_name}'"))
        }
        InstanceError::AmbiguousClass {
            class_name,
            candidates,
        } => (
            400,
            format!(
                "'{class_name}' names more than one class: pass its target class IRI ({})",
                candidates.join(", ")
            ),
        ),
        InstanceError::AbstractClass { class_name } => (
            400,
            format!(
                "'{class_name}' is an abstract class: write collection membership through the \
                 record's own class"
            ),
        ),
        InstanceError::InvalidUri { field, value } => (
            400,
            format!("'{value}' is not a usable {field}: instance ids must be absolute URIs"),
        ),
        InstanceError::EmptyProperties => (400, "properties is empty".to_string()),
        InstanceError::ParentClassUnresolved {
            parent,
            relation,
            candidates,
        } => (
            400,
            format!(
                "cannot tell which class of '{parent}' holds '{relation}' (found: {}): name it \
                 with `model`",
                if candidates.is_empty() {
                    "none".to_string()
                } else {
                    candidates.join(", ")
                }
            ),
        ),
        InstanceError::StoredAsExpressions {
            collection,
            item_uri,
            language,
            ..
        } => (
            400,
            format!(
                "'{item_uri}' is not how collection '{collection}' stores its members \
                 (resolveLanguage '{language}'): pass the member as a read lists it"
            ),
        ),
        InstanceError::NotFound {
            class_name,
            base_uri,
        } => (
            404,
            format!("No {class_name} instance found at '{base_uri}'"),
        ),
        InstanceError::BatchNotFound { batch_id } => (404, format!("No open batch '{batch_id}'")),
        InstanceError::BatchConflict { batch_id } => (
            409,
            format!(
                "Batch '{batch_id}' was written to while this call ran; nothing was written. \
                 Send the call again"
            ),
        ),
        InstanceError::ConcurrentWrite => (
            409,
            "Another write changed the links this call replaced while it ran; nothing was \
             written. Send the call again"
                .to_string(),
        ),
        InstanceError::AlreadyExists {
            class_name,
            base_uri,
        } => (
            409,
            format!("A {class_name} instance already exists at '{base_uri}'"),
        ),
        InstanceError::Failed {
            class_name, error, ..
        } => (
            500,
            format!("Writing the {class_name} instance failed: {error:#}"),
        ),
        InstanceError::PartiallyRemoved {
            class_name,
            base_uri,
            removed,
            error,
            ..
        } => (
            500,
            format!(
                "Removing the {class_name} instance at '{base_uri}' failed after {removed} \
                 link(s): {error:#}"
            ),
        ),
    };
    let data = serde_json::to_value(&error).unwrap_or(Value::Null);
    WsRpcError {
        code,
        message,
        data: Some(data),
    }
}

/// The problem's `kind`, as the wire names it.
fn problem_kind(problem: &ValidationProblem) -> String {
    serde_json::to_value(problem)
        .ok()
        .and_then(|v| v.get("kind").and_then(Value::as_str).map(str::to_string))
        .unwrap_or_default()
}

/// [`instance_error`] for the op that failed: its message names the op and its `data` carries
/// `opIndex`.
fn mutate_error(failure: MutateError) -> WsRpcError {
    let mut error = instance_error(failure.error);
    if let Some(index) = failure.op_index {
        error.message = format!("op {index}: {}", error.message);
        if let Some(Value::Object(data)) = error.data.as_mut() {
            data.insert("opIndex".to_string(), json!(index));
        }
    }
    error
}

async fn mutate(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_credits(ctx.user_did())?;
    let body: PerspectiveMutateParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let rows = match &body.returns {
        Some(Returns::Rows { properties }) => Some(properties.clone()),
        _ => None,
    };
    // The rows are instance data the call reads back: the guard of the methods that read.
    if rows.is_some() {
        Guard::perspective(perspective_query_capability)
            .admit("perspective.mutate", &params, &ctx)
            .await?;
    }
    if rows.is_some() && body.batch.is_some() {
        return Err(WsRpcError::bad_request(
            "returns rows needs a call without batch: a caller's batch is not committed yet",
        ));
    }
    let mut perspective = guarded_perspective(&params).await?;
    let done = instances::mutate(&mut perspective, &ctx.acts_as(), body.ops, body.batch)
        .await
        .map_err(mutate_error)?;

    if let Err(e) = reserve_credits(ctx.user_did(), done.len() as f64 * DEFAULT_LINK_WRITE) {
        // The ops are committed or staged already: a missed deduction is accounting drift to
        // see in the log, not a reason to fail the call.
        log::warn!("credit deduction failed (mutate already written): {}", e);
    }

    let ids = done.iter().map(|op| op.id.clone()).collect();
    let rows = match rows {
        None => None,
        Some(properties) => {
            let mut out = Vec::with_capacity(done.len());
            for op in &done {
                if op.deleted {
                    out.push(None);
                    continue;
                }
                let instance = fetch_instance(&perspective, &op.class_name, &op.id)
                    .await
                    .map_err(|e| {
                        WsRpcError::internal(format!(
                            "The write committed, but reading {} back failed: {e}",
                            op.id
                        ))
                    })?;
                out.push(instance.map(|instance| match (&properties, instance) {
                    (Some(wanted), Value::Object(mut fields)) => {
                        fields.retain(|key, _| key == "id" || wanted.contains(key));
                        Value::Object(fields)
                    }
                    (_, instance) => instance,
                }));
            }
            Some(out)
        }
    };
    Ok(serde_json::to_value(PerspectiveMutateResult { ids, rows })?)
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    // The guard of the other perspective writes.
    map.method::<PerspectiveMutateParams, PerspectiveMutateResult>("perspective.mutate", mutate)
        .guard(Guard::perspective(perspective_update_capability));
}

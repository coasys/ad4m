//! Validated instance writes over the WS RPC: `perspective.instanceCreate`, `.instanceUpdate`,
//! `.instanceRemove`, `.instanceAddToCollection`, `.instanceRemoveFromCollection`.
//!
//! Each calls the same function as the MCP tool of the same job
//! ([`crate::perspectives::instances`]): the write is checked against the class's SHACL shape
//! before anything is written — each property's type, which are required, which take a single
//! value, and `sh:in` (not arbitrary `minCount`/`maxCount` bounds). Same guard as
//! `perspective.createSubject`: `perspective:UPDATE` on the perspective, which the caller's
//! agent must own (`Guard::perspective`). The agent that writes is `ctx.acts_as()`.
//!
//! A refusal is a `WsRpcError` whose `data` is the [`InstanceError`] (camelCase, tagged by
//! `kind`); see [`instance_error`] for the codes. A validation failure is 422 with
//! `data = { kind: "validation", className, errors: ValidationError[] }`; each error names the
//! `property`, the `problem` (`{ kind, … }`), `expectedType`, `cardinality` (`{ min, max }`),
//! `allowedValues` (`sh:in`), `received` and, for a collection item, its index (`item`).
//!
//! Batches and partial failure: with `batchId` the write is staged into that open batch and
//! lands on its commit, and the link reads a write does of the state it manages (collection
//! membership, the remove cascade) see the batch's own staged work. Existence reads still see
//! committed state only: an instance created in the same batch is not found, so
//! create-then-update / -remove of a new instance in one batch fails `notFound`.
//! A *refused* call (a 422 or another 4xx) stages nothing — validation runs before any link
//! write. A failure that is not a refusal — a store error mid-write, `failed` or
//! `partiallyRemoved` — can leave part of the write staged in the caller's batch: discard it
//! (`perspective.discardBatch`) instead of committing it to keep the calls that succeeded.
//! Without a batch, a remove's cascade may have deleted some links already; `removed` counts
//! deleted links with no batch, staged ones with one.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::perspective_update_capability;
use crate::perspectives::instances::{
    self, CollectionItemAdded, CollectionItemRemoved, InstanceAddToCollection, InstanceCreate,
    InstanceCreated, InstanceError, InstanceRemove, InstanceRemoveFromCollection, InstanceRemoved,
    InstanceUpdate, InstanceUpdated, ValidationProblem,
};
use crate::types::RequestContext;

use super::guards::Guard;
use super::perspectives_ws::guarded_perspective;
use super::ws_handler::{HandlerMap, WsRpcError};

#[derive(Deserialize, TS)]
#[ts(export)]
pub struct PerspectiveInstanceCreateParams {
    pub uuid: String,
    #[serde(flatten)]
    pub body: InstanceCreate,
}

#[derive(Deserialize, TS)]
#[ts(export)]
pub struct PerspectiveInstanceUpdateParams {
    pub uuid: String,
    #[serde(flatten)]
    pub body: InstanceUpdate,
}

#[derive(Deserialize, TS)]
#[ts(export)]
pub struct PerspectiveInstanceRemoveParams {
    pub uuid: String,
    #[serde(flatten)]
    pub body: InstanceRemove,
}

#[derive(Deserialize, TS)]
#[ts(export)]
pub struct PerspectiveInstanceAddToCollectionParams {
    pub uuid: String,
    #[serde(flatten)]
    pub body: InstanceAddToCollection,
}

#[derive(Deserialize, TS)]
#[ts(export)]
pub struct PerspectiveInstanceRemoveFromCollectionParams {
    pub uuid: String,
    #[serde(flatten)]
    pub body: InstanceRemoveFromCollection,
}

/// The RPC error for a refused or failed write: the code, a one-line message, and the
/// [`InstanceError`] as `data`.
///
/// - 422: `validation` (the write breaks the class's shape)
/// - 400: `unknownClass`, `abstractClass`, `invalidUri`, `emptyProperties`,
///   `storedAsExpressions`
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

async fn instance_create(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: PerspectiveInstanceCreateParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let mut perspective = guarded_perspective(&params).await?;
    let created = instances::create(&mut perspective, &ctx.acts_as(), body.body)
        .await
        .map_err(instance_error)?;
    Ok(serde_json::to_value(created)?)
}

async fn instance_update(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: PerspectiveInstanceUpdateParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let mut perspective = guarded_perspective(&params).await?;
    let updated = instances::update(&mut perspective, &ctx.acts_as(), body.body)
        .await
        .map_err(instance_error)?;
    Ok(serde_json::to_value(updated)?)
}

async fn instance_remove(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: PerspectiveInstanceRemoveParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let mut perspective = guarded_perspective(&params).await?;
    let removed = instances::remove(&mut perspective, &ctx.acts_as(), body.body)
        .await
        .map_err(instance_error)?;
    Ok(serde_json::to_value(removed)?)
}

async fn instance_add_to_collection(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let body: PerspectiveInstanceAddToCollectionParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let mut perspective = guarded_perspective(&params).await?;
    let added = instances::add_to_collection(&mut perspective, &ctx.acts_as(), body.body)
        .await
        .map_err(instance_error)?;
    Ok(serde_json::to_value(added)?)
}

async fn instance_remove_from_collection(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let body: PerspectiveInstanceRemoveFromCollectionParams =
        serde_json::from_value(params.clone())
            .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let mut perspective = guarded_perspective(&params).await?;
    let removed = instances::remove_from_collection(&mut perspective, &ctx.acts_as(), body.body)
        .await
        .map_err(instance_error)?;
    Ok(serde_json::to_value(removed)?)
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    // The guard of `perspective.createSubject` and the other perspective writes.
    let update = || Guard::perspective(perspective_update_capability);

    map.method::<PerspectiveInstanceCreateParams, InstanceCreated>(
        "perspective.instanceCreate",
        instance_create,
    )
    .guard(update());
    map.method::<PerspectiveInstanceUpdateParams, InstanceUpdated>(
        "perspective.instanceUpdate",
        instance_update,
    )
    .guard(update());
    map.method::<PerspectiveInstanceRemoveParams, InstanceRemoved>(
        "perspective.instanceRemove",
        instance_remove,
    )
    .guard(update());
    map.method::<PerspectiveInstanceAddToCollectionParams, CollectionItemAdded>(
        "perspective.instanceAddToCollection",
        instance_add_to_collection,
    )
    .guard(update());
    map.method::<PerspectiveInstanceRemoveFromCollectionParams, CollectionItemRemoved>(
        "perspective.instanceRemoveFromCollection",
        instance_remove_from_collection,
    )
    .guard(update());
}

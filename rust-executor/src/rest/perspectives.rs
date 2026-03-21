//! Perspective REST endpoints: /api/v1/perspectives/*
//!
//! 10 harmonised endpoints including unified link mutations and query.

use axum::{
    extract::{Path, Query, State},
    Json,
};
use std::collections::HashMap;

use crate::agent::capabilities::*;
use crate::graphql::graphql_types::*;
use crate::graphql::query_resolvers::can_access_perspective;
use crate::perspectives::{
    self, add_perspective, get_perspective, remove_perspective, update_perspective,
};

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// GET /perspectives — list all
pub async fn list_perspectives(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<PerspectiveHandle>>, ApiError> {
    let context = auth.to_request_context();
    let user_email = user_email_from_token(context.auth_token.clone());

    let all = perspectives::all_perspectives()
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let filtered: Vec<PerspectiveHandle> = all
        .into_iter()
        .filter(|p| can_access_perspective(&user_email, p))
        .collect();

    Ok(Json(filtered))
}

/// GET /perspectives/:uuid — get one (with ?include=snapshot)
pub async fn get_perspective_handler(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let user_email = user_email_from_token(context.auth_token.clone());
    let handle = perspective.persisted
        .ok_or_else(|| ApiError::Internal("Perspective has no persisted handle".into()))?;

    if !can_access_perspective(&user_email, &handle) {
        return Err(ApiError::Forbidden("Access denied to perspective".into()));
    }

    let include = params.get("include").cloned().unwrap_or_default();
    if include.contains("snapshot") {
        let snapshot = perspective.snapshot()
            .await
            .map_err(|e| ApiError::Internal(e.to_string()))?;
        return Ok(Json(serde_json::to_value(snapshot).unwrap_or_default()));
    }

    Ok(Json(serde_json::to_value(handle).unwrap_or_default()))
}

/// POST /perspectives — create
pub async fn create_perspective(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<CreatePerspectiveRequest>,
) -> Result<Json<PerspectiveHandle>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &PERSPECTIVE_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let handle = add_perspective(body.name, None)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(handle))
}

/// PUT /perspectives/:uuid — update metadata
pub async fn update_perspective_handler(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<UpdatePerspectiveRequest>,
) -> Result<Json<PerspectiveHandle>, ApiError> {
    let context = auth.to_request_context();

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let handle = update_perspective(&uuid, body.name)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(handle))
}

/// DELETE /perspectives/:uuid — delete
pub async fn delete_perspective(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &PERSPECTIVE_DELETE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    remove_perspective(&uuid)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /perspectives/:uuid/links — unified link mutations (add/remove/update)
pub async fn mutate_links(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<LinkMutationRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    // Delegate to the perspective's link_mutations method
    // which handles additions, removals, and updates in one call.
    // The GraphQL `perspectiveLinkMutations` already does this.

    let additions = body.additions.unwrap_or_default();
    let removals = body.removals.unwrap_or_default();
    let updates = body.updates.unwrap_or_default();

    // Convert to internal types and call perspective methods
    let mut results = serde_json::Map::new();

    if !additions.is_empty() {
        let links: Vec<crate::types::Link> = additions
            .into_iter()
            .map(|l| crate::types::Link {
                source: l.source,
                target: l.target,
                predicate: l.predicate,
            })
            .collect();

        let added = perspective
            .add_links(links, &context.auth_token)
            .await
            .map_err(|e| ApiError::Internal(e.to_string()))?;
        results.insert(
            "additions".to_string(),
            serde_json::to_value(added).unwrap_or_default(),
        );
    }

    if !removals.is_empty() {
        let link_expressions: Vec<crate::types::DecoratedLinkExpression> = removals
            .into_iter()
            .map(|le| crate::types::DecoratedLinkExpression {
                author: le.author,
                timestamp: le.timestamp,
                data: crate::types::Link {
                    source: le.data.source,
                    target: le.data.target,
                    predicate: le.data.predicate,
                },
                proof: crate::types::DecoratedExpressionProof {
                    key: le.proof.as_ref().and_then(|p| p.key.clone()).unwrap_or_default(),
                    signature: le.proof.as_ref().and_then(|p| p.signature.clone()).unwrap_or_default(),
                    valid: le.proof.as_ref().and_then(|p| p.valid),
                    invalid: le.proof.as_ref().and_then(|p| p.invalid),
                },
                status: le.status.map(|s| match s.as_str() {
                    "shared" => crate::types::LinkStatus::Shared,
                    _ => crate::types::LinkStatus::Local,
                }),
            })
            .collect();

        for le in &link_expressions {
            perspective
                .remove_link(le.clone())
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
        }
        results.insert("removals".to_string(), serde_json::json!(true));
    }

    if !updates.is_empty() {
        for update in updates {
            let old = crate::types::DecoratedLinkExpression {
                author: update.old.author,
                timestamp: update.old.timestamp,
                data: crate::types::Link {
                    source: update.old.data.source,
                    target: update.old.data.target,
                    predicate: update.old.data.predicate,
                },
                proof: crate::types::DecoratedExpressionProof {
                    key: update.old.proof.as_ref().and_then(|p| p.key.clone()).unwrap_or_default(),
                    signature: update.old.proof.as_ref().and_then(|p| p.signature.clone()).unwrap_or_default(),
                    valid: update.old.proof.as_ref().and_then(|p| p.valid),
                    invalid: update.old.proof.as_ref().and_then(|p| p.invalid),
                },
                status: update.old.status.map(|s| match s.as_str() {
                    "shared" => crate::types::LinkStatus::Shared,
                    _ => crate::types::LinkStatus::Local,
                }),
            };
            let new_link = crate::types::Link {
                source: update.new.source,
                target: update.new.target,
                predicate: update.new.predicate,
            };
            perspective
                .update_link(old, new_link)
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
        }
        results.insert("updates".to_string(), serde_json::json!(true));
    }

    Ok(Json(serde_json::Value::Object(results)))
}

/// POST /perspectives/:uuid/query — unified query { engine, query }
pub async fn query_perspective(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<QueryRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let result = match body.engine.as_str() {
        "prolog" => {
            let res = perspective
                .prolog_query(&body.query)
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            serde_json::to_value(res).unwrap_or_default()
        }
        "surreal" => {
            let res = perspective
                .surreal_query(&body.query)
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            serde_json::to_value(res).unwrap_or_default()
        }
        other => {
            return Err(ApiError::BadRequest(format!(
                "Unknown query engine: {}. Use 'prolog' or 'surreal'.",
                other
            )));
        }
    };

    Ok(Json(result))
}

/// POST /perspectives/:uuid/sdna — add SDNA
pub async fn add_sdna(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<AddSdnaRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let sdna_type = match body.sdna_type.as_str() {
        "subject_class" => crate::perspectives::perspective_instance::SdnaType::SubjectClass,
        "flow" => crate::perspectives::perspective_instance::SdnaType::Flow,
        "custom" => crate::perspectives::perspective_instance::SdnaType::Custom,
        _ => return Err(ApiError::BadRequest("Invalid SDNA type".into())),
    };

    perspective
        .add_sdna(body.name, body.sdna_code, sdna_type)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /perspectives/:uuid/commands — execute commands
pub async fn execute_commands(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<ExecuteCommandsRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();

    let perspective = get_perspective(&uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))?;

    let result = perspective
        .execute_commands(
            serde_json::to_string(&body.commands).unwrap_or_default(),
            serde_json::to_string(&body.expression).unwrap_or_default(),
        )
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}

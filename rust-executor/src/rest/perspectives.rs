//! Perspective REST endpoints: /api/v1/perspectives/*
//!
//! 10 harmonised endpoints including unified link mutations and query.

use axum::{
    extract::{Path, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::agent::{AgentContext, AgentService};
use crate::db::Ad4mDb;
use crate::helpers::can_access_perspective;
use crate::perspectives::{
    add_perspective, get_perspective,
    perspective_instance::{PerspectiveInstance, SdnaType},
    remove_perspective, update_perspective,
    utils::prolog_resolution_to_string,
};
use crate::pubsub::mark_credits_dirty;
use crate::types::*;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

// ── Helpers ──

/// Get a perspective, returning ApiError::NotFound if missing
fn get_perspective_or_404(uuid: &str) -> Result<PerspectiveInstance, ApiError> {
    get_perspective(uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))
}

/// Get a perspective with access control check (multi-user aware)
async fn get_perspective_with_access_control(
    uuid: &str,
    auth_token: &str,
) -> Result<PerspectiveInstance, ApiError> {
    let perspective = get_perspective_or_404(uuid)?;
    let user_email = user_email_from_token(auth_token.to_string());

    let handle = perspective.persisted.lock().await.clone();
    if !can_access_perspective(&user_email, &handle) {
        return Err(ApiError::Forbidden(
            "Access denied: You don't have permission to access this perspective".into(),
        ));
    }

    Ok(perspective)
}

/// Read-only credit check. Returns Ok(()) if user can afford compute.
fn check_compute_credits(auth_token: &str) -> Result<(), ApiError> {
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| ApiError::Internal(e.to_string()))?;
        if !free {
            let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            if credits <= 0.0 {
                return Err(ApiError::Forbidden("Insufficient compute credits".into()));
            }
        }
    }
    Ok(())
}

/// Deduct compute credits after an operation.
fn reserve_compute_credits(auth_token: &str, amount: f64) -> Result<(), ApiError> {
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| ApiError::Internal(e.to_string()))?;
        if !free {
            Ad4mDb::with_global_instance(|db| db.deduct_user_credits_if_available(email, amount))
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            mark_credits_dirty(email);
        }
    }
    Ok(())
}

// Default pricing
const DEFAULT_LINK_WRITE: f64 = 0.25;

// ── Endpoints ──

/// GET /perspectives — list all perspectives
pub async fn list_perspectives(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<PerspectiveHandle>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![WILD_CARD.to_string()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let user_email = user_email_from_token(context.auth_token.clone());
    let all: Vec<PerspectiveInstance> = crate::perspectives::all_perspectives();

    // Filter perspectives based on access
    let mut filtered: Vec<PerspectiveHandle> = Vec::new();
    for p in all {
        let handle = p.persisted.lock().await.clone();
        if crate::helpers::can_access_perspective(&user_email, &handle) {
            filtered.push(handle);
        }
    }

    Ok(Json(filtered))
}

/// GET /perspectives/:uuid — get single perspective
pub async fn get_perspective_handler(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<PerspectiveHandle>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let handle = perspective.persisted.lock().await.clone();
    Ok(Json(handle))
}

/// GET /perspectives/:uuid/snapshot — get perspective snapshot (all links)
pub async fn get_snapshot(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<crate::types::domain::Perspective>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let links = perspective
        .get_links(&LinkQuery {
            source: None,
            target: None,
            predicate: None,
            from_date: None,
            until_date: None,
            limit: None,
        })
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(crate::types::domain::Perspective { links }))
}

/// GET /perspectives/:uuid/links — query links
pub async fn query_links(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(query): Json<LinkQuery>,
) -> Result<Json<Vec<DecoratedLinkExpression>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let links = perspective
        .get_links(&query)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(links))
}

/// POST /perspectives — create a new perspective
pub async fn create_perspective(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<CreatePerspectiveRequest>,
) -> Result<Json<PerspectiveHandle>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &PERSPECTIVE_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // Determine owner DID based on user context
    let user_email_opt = user_email_from_token(context.auth_token.clone());

    let owner_did = if let Some(user_email) = user_email_opt {
        Some(
            AgentService::get_user_did_by_email(&user_email)
                .map_err(|e| ApiError::Internal(format!("Failed to get user DID: {}", e)))?,
        )
    } else {
        None
    };

    let handle = if let Some(owner) = &owner_did {
        PerspectiveHandle::new_with_owner(body.name.clone(), owner.clone())
    } else {
        PerspectiveHandle::new_from_name(body.name.clone())
    };

    add_perspective(handle.clone(), None)
        .await
        .map_err(|e| ApiError::Internal(e))?;

    Ok(Json(handle))
}

/// PUT /perspectives/:uuid — update perspective name
pub async fn update_perspective_handler(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<UpdatePerspectiveRequest>,
) -> Result<Json<PerspectiveHandle>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let mut handle = perspective.persisted.lock().await.clone();
    handle.name = Some(body.name);
    update_perspective(&handle)
        .await
        .map_err(|e| ApiError::Internal(e))?;

    Ok(Json(handle))
}

/// DELETE /perspectives/:uuid — delete perspective
pub async fn delete_perspective(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_delete_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    remove_perspective(&uuid).await;
    Ok(Json(true))
}

/// POST /perspectives/:uuid/links — unified link mutations (add, remove, update, set status)
pub async fn mutate_links(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<LinkMutationRequest>,
) -> Result<Json<LinkMutationResponse>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    // Check compute credits (pre-check)
    check_compute_credits(&context.auth_token)?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let mut response = LinkMutationResponse {
        additions: vec![],
        removals: vec![],
        updates: vec![],
    };

    let status = body
        .status
        .as_deref()
        .map(|s| match s {
            "shared" => LinkStatus::Shared,
            _ => LinkStatus::Local,
        })
        .unwrap_or(LinkStatus::Local);

    // Handle additions and removals via link_mutations
    let has_additions = body.additions.as_ref().map_or(false, |a| !a.is_empty());
    let has_removals = body.removals.as_ref().map_or(false, |r| !r.is_empty());

    if has_additions || has_removals {
        let mutations = LinkMutations {
            additions: body.additions.unwrap_or_default(),
            removals: body
                .removals
                .unwrap_or_default()
                .into_iter()
                .map(|l| LinkExpressionInput {
                    author: String::new(),
                    data: l,
                    proof: ExpressionProofInput {
                        key: None,
                        signature: None,
                        valid: None,
                        invalid: None,
                    },
                    timestamp: String::new(),
                    status: None,
                })
                .collect(),
        };

        let diff = perspective
            .link_mutations(mutations, status.clone(), &agent_context)
            .await
            .map_err(|e| ApiError::Internal(e.to_string()))?;

        response.additions = diff.additions;
        response.removals = diff.removals;
    }

    // Handle updates separately
    if let Some(updates) = body.updates {
        for update in updates {
            let old = LinkExpressionInput {
                author: String::new(),
                data: update.old_link,
                proof: ExpressionProofInput {
                    key: None,
                    signature: None,
                    valid: None,
                    invalid: None,
                },
                timestamp: String::new(),
                status: None,
            };
            let new = LinkExpressionInput {
                author: String::new(),
                data: update.new_link,
                proof: ExpressionProofInput {
                    key: None,
                    signature: None,
                    valid: None,
                    invalid: None,
                },
                timestamp: String::new(),
                status: None,
            };
            let result = perspective
                .update_link(
                    LinkExpression::from_input_without_proof(old),
                    Link::from(new.data),
                    body.batch_id.clone(),
                    &agent_context,
                )
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            response.updates.push(result);
        }
    }

    // Deduct compute credits
    let total_ops = response.additions.len() + response.removals.len() + response.updates.len();
    if total_ops > 0 {
        let _ = reserve_compute_credits(&context.auth_token, total_ops as f64 * DEFAULT_LINK_WRITE);
    }

    Ok(Json(response))
}

/// POST /perspectives/:uuid/query — query perspective (prolog or surreal)
pub async fn query_perspective(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<QueryRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let result = match body.engine.as_str() {
        "prolog" => {
            let res = perspective
                .prolog_query_with_context(body.query, &agent_context)
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            serde_json::to_value(prolog_resolution_to_string(res)).unwrap_or_default()
        }
        "surreal" => {
            let res = perspective
                .surreal_query(body.query)
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
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let sdna_type = SdnaType::from_string(&body.sdna_type)
        .map_err(|e| ApiError::BadRequest(format!("Invalid SDNA type: {}", e)))?;

    let result = perspective
        .add_sdna(
            body.name,
            body.sdna_code.unwrap_or_default(),
            sdna_type,
            body.shacl_json,
            &agent_context,
        )
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// POST /perspectives/:uuid/commands — execute commands
pub async fn execute_commands(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<ExecuteCommandsRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;

    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
    let commands: Vec<crate::perspectives::perspective_instance::Command> =
        serde_json::from_value(serde_json::Value::Array(body.commands))
            .map_err(|e| ApiError::BadRequest(e.to_string()))?;
    let expression = serde_json::to_string(&body.expression).unwrap_or_default();

    let result = perspective
        .execute_commands(commands, expression, vec![], None, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(result).unwrap_or_default()))
}

//! Perspective REST endpoints: /api/v1/perspectives/*

use axum::{
    extract::{Path, Query, State},
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

fn get_perspective_or_404(uuid: &str) -> Result<PerspectiveInstance, ApiError> {
    get_perspective(uuid)
        .ok_or_else(|| ApiError::NotFound(format!("Perspective {} not found", uuid)))
}

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

const DEFAULT_LINK_WRITE: f64 = 0.25;

// ── Endpoints ──

/// GET /perspectives
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

    let mut filtered: Vec<PerspectiveHandle> = Vec::new();
    for p in all {
        let handle = p.persisted.lock().await.clone();
        if crate::helpers::can_access_perspective(&user_email, &handle) {
            filtered.push(handle);
        }
    }

    Ok(Json(filtered))
}

/// GET /perspectives/:uuid
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

/// GET /perspectives/:uuid/snapshot
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

/// POST /perspectives/:uuid/publish-snapshot
pub async fn publish_snapshot(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let snapshot_url = perspective
        .publish_snapshot()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(snapshot_url))
}

/// GET /perspectives/:uuid/links
pub async fn query_links(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Query(query): Query<LinkQuery>,
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

/// POST /perspectives — create
pub async fn create_perspective(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<CreatePerspectiveRequest>,
) -> Result<Json<PerspectiveHandle>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &PERSPECTIVE_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

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

/// PUT /perspectives/:uuid
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

/// DELETE /perspectives/:uuid
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

    let _perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    remove_perspective(&uuid).await;
    Ok(Json(true))
}

/// POST /perspectives/:uuid/links — add single link
pub async fn add_link(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<AddLinkRequest>,
) -> Result<Json<DecoratedLinkExpression>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let status = body
        .status
        .as_deref()
        .map(|s| match s {
            "shared" => LinkStatus::Shared,
            _ => LinkStatus::Local,
        })
        .unwrap_or(LinkStatus::Shared);

    let result = perspective
        .add_link(body.link, status, &agent_context, body.batch_id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let _ = reserve_compute_credits(&context.auth_token, DEFAULT_LINK_WRITE);
    Ok(Json(result))
}

/// POST /perspectives/:uuid/links/bulk — add multiple links
pub async fn add_links_bulk(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<AddLinksBulkRequest>,
) -> Result<Json<Vec<DecoratedLinkExpression>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let status = body
        .status
        .as_deref()
        .map(|s| match s {
            "shared" => LinkStatus::Shared,
            _ => LinkStatus::Local,
        })
        .unwrap_or(LinkStatus::Shared);

    let mutations = LinkMutations {
        additions: body.links,
        removals: vec![],
    };

    let diff = perspective
        .link_mutations(mutations, status, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let count = diff.additions.len();
    if count > 0 {
        let _ = reserve_compute_credits(&context.auth_token, count as f64 * DEFAULT_LINK_WRITE);
    }

    Ok(Json(diff.additions))
}

/// POST /perspectives/:uuid/links/remove-bulk — remove multiple links
pub async fn remove_links_bulk(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<RemoveLinksBulkRequest>,
) -> Result<Json<Vec<DecoratedLinkExpression>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let mutations = LinkMutations {
        additions: vec![],
        removals: body.links,
    };

    let diff = perspective
        .link_mutations(mutations, LinkStatus::Shared, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(diff.removals))
}

/// POST /perspectives/:uuid/links/mutations — combined add+remove mutations
pub async fn link_mutations(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<LinkMutationsRequest>,
) -> Result<Json<LinkMutationResponse>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let status = body
        .status
        .as_deref()
        .map(|s| match s {
            "shared" => LinkStatus::Shared,
            _ => LinkStatus::Local,
        })
        .unwrap_or(LinkStatus::Shared);

    let diff = perspective
        .link_mutations(body.mutations, status, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let total = diff.additions.len() + diff.removals.len();
    if total > 0 {
        let _ = reserve_compute_credits(&context.auth_token, total as f64 * DEFAULT_LINK_WRITE);
    }

    Ok(Json(LinkMutationResponse {
        additions: diff.additions,
        removals: diff.removals,
        updates: vec![],
    }))
}

/// POST /perspectives/:uuid/links/expression — add pre-signed link expression
pub async fn add_link_expression(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<AddLinkExpressionRequest>,
) -> Result<Json<DecoratedLinkExpression>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;
    check_compute_credits(&context.auth_token)?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let status = body
        .status
        .as_deref()
        .map(|s| match s {
            "shared" => LinkStatus::Shared,
            _ => LinkStatus::Local,
        })
        .unwrap_or(LinkStatus::Shared);

    let result = perspective
        .add_link_expression(body.link, status, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let _ = reserve_compute_credits(&context.auth_token, DEFAULT_LINK_WRITE);
    Ok(Json(result))
}

/// PUT /perspectives/:uuid/links — update link
pub async fn update_link(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<UpdateLinkRequest>,
) -> Result<Json<DecoratedLinkExpression>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let result = perspective
        .update_link(
            LinkExpression::from_input_without_proof(body.old_link),
            Link::from(body.new_link),
            body.batch_id,
            &agent_context,
        )
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(result))
}

/// DELETE /perspectives/:uuid/links — remove single link
pub async fn remove_link(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<RemoveLinkRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    perspective
        .remove_link(body.link, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /perspectives/:uuid/query — unified query (prolog, surreal, sparql)
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
        "sparql" => {
            let res = perspective
                .sparql_query(body.query)
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            serde_json::to_value(res).unwrap_or_default()
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
                "Unknown query engine: {}. Use 'prolog', 'surreal', or 'sparql'.",
                other
            )));
        }
    };

    Ok(Json(result))
}

/// POST /perspectives/:uuid/sdna
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

/// POST /perspectives/:uuid/commands
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

/// POST /perspectives/:uuid/batch — create batch
pub async fn create_batch(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let batch_id = perspective
        .create_batch()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(batch_id))
}

/// POST /perspectives/:uuid/batch/commit — commit batch
pub async fn commit_batch(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<CommitBatchRequest>,
) -> Result<Json<LinkMutationResponse>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let diff = perspective
        .commit_batch(&body.batch_id, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(LinkMutationResponse {
        additions: diff.additions,
        removals: diff.removals,
        updates: vec![],
    }))
}

/// POST /perspectives/:uuid/subscribe-query — subscribe to prolog query changes
pub async fn subscribe_query(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<SubscribeQueryRequest>,
) -> Result<Json<SubscribeQueryResponse>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let (subscription_id, result) = perspective
        .subscribe_prolog_query(body.query, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(SubscribeQueryResponse {
        subscription_id,
        result,
    }))
}

/// POST /perspectives/:uuid/subscribe-surreal-query
pub async fn subscribe_surreal_query(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<SubscribeQueryRequest>,
) -> Result<Json<SubscribeQueryResponse>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let (subscription_id, result) = perspective
        .subscribe_surreal_query(body.query, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(SubscribeQueryResponse {
        subscription_id,
        result,
    }))
}

/// POST /perspectives/:uuid/keep-alive-query
pub async fn keep_alive_query(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<KeepAliveQueryRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;

    perspective
        .keep_alive_query(&body.subscription_id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /perspectives/:uuid/keep-alive-surreal-query
pub async fn keep_alive_surreal_query(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<KeepAliveQueryRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;

    perspective
        .keep_alive_surreal_query(&body.subscription_id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /perspectives/:uuid/dispose-query-subscription
pub async fn dispose_query_subscription(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<DisposeQueryRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;

    perspective
        .dispose_query_subscription(&body.subscription_id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /perspectives/:uuid/dispose-surreal-query-subscription
pub async fn dispose_surreal_query_subscription(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<DisposeQueryRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;

    perspective
        .dispose_surreal_query_subscription(&body.subscription_id)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /perspectives/:uuid/create-subject
pub async fn create_subject(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<CreateSubjectRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let mut perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    perspective
        .create_subject(
            &body.subject_class,
            &body.expression_address,
            body.initial_values.as_deref(),
            body.batch_id.as_deref(),
            &agent_context,
        )
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /perspectives/:uuid/get-subject-data
pub async fn get_subject_data(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
    Json(body): Json<GetSubjectDataRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let perspective = get_perspective_with_access_control(&uuid, &context.auth_token).await?;
    let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

    let data = perspective
        .get_subject_data(&body.subject_class, &body.expression_address, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(data))
}

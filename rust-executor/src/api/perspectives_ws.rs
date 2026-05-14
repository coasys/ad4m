//! Perspective WS-native handlers.

use serde_json::Value;
use std::sync::Arc;
use std::time::Duration;

use crate::agent::capabilities::*;
use crate::agent::AgentContext;
use crate::db::Ad4mDb;
use crate::helpers::can_access_perspective_with_did;
use crate::perspectives::{
    add_perspective, get_perspective,
    perspective_instance::{PerspectiveInstance, SdnaType},
    remove_perspective, update_perspective,
    utils::prolog_resolution_to_string,
};
use crate::pubsub::mark_credits_dirty;
use crate::types::*;

use super::types::*;
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

// ── Helpers ──

fn get_perspective_or_404(uuid: &str) -> Result<PerspectiveInstance, WsRpcError> {
    get_perspective(uuid)
        .ok_or_else(|| WsRpcError::not_found(format!("Perspective {} not found", uuid)))
}

async fn get_perspective_with_access(
    uuid: &str,
    ctx: &RequestContext,
) -> Result<PerspectiveInstance, WsRpcError> {
    let perspective = get_perspective_or_404(uuid)?;

    if !ctx.is_admin_credential {
        let handle = perspective.persisted.lock().await.clone();
        if !can_access_perspective_with_did(&ctx.user_did, &handle) {
            return Err(WsRpcError::forbidden(
                "Access denied: You don't have permission to access this perspective",
            ));
        }
    }

    Ok(perspective)
}

fn check_credits(user_email: &Option<String>) -> Result<(), WsRpcError> {
    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    if global_free {
        return Ok(());
    }
    if let Some(ref email) = user_email {
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        if !free {
            let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            if credits <= 0.0 {
                return Err(WsRpcError::forbidden("Insufficient compute credits"));
            }
        }
    }
    Ok(())
}

fn reserve_credits(user_email: &Option<String>, amount: f64) -> Result<(), WsRpcError> {
    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    if global_free {
        return Ok(());
    }
    if let Some(ref email) = user_email {
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        if !free {
            Ad4mDb::with_global_instance(|db| db.deduct_user_credits_if_available(email, amount))
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            mark_credits_dirty(email);
        }
    }
    Ok(())
}

const DEFAULT_LINK_WRITE: f64 = 0.25;

fn parse_link_status(s: Option<&str>) -> LinkStatus {
    match s {
        Some("shared" | "SHARED") => LinkStatus::Shared,
        Some(_) => LinkStatus::Local,
        None => LinkStatus::Shared,
    }
}

// ── Handlers ──

async fn list_perspectives(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![WILD_CARD.to_string()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let all: Vec<PerspectiveInstance> = crate::perspectives::all_perspectives();

    // Admin sees everything; regular users see only their own perspectives
    if ctx.is_admin_credential {
        let mut handles: Vec<PerspectiveHandle> = Vec::new();
        for p in all {
            handles.push(p.persisted.lock().await.clone());
        }
        return Ok(serde_json::to_value(handles)?);
    }

    let mut filtered: Vec<PerspectiveHandle> = Vec::new();
    for p in all {
        let handle = p.persisted.lock().await.clone();
        if can_access_perspective_with_did(&ctx.user_did, &handle) {
            filtered.push(handle);
        }
    }

    Ok(serde_json::to_value(filtered)?)
}

async fn get_perspective_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    // Return null (via 404, caught by client) if perspective doesn't exist or user can't access it
    let perspective = match get_perspective_with_access(&uuid, &ctx).await {
        Ok(p) => p,
        Err(e) if e.code == 403 || e.code == 404 => return Ok(Value::Null),
        Err(e) => return Err(e),
    };
    let handle = perspective.persisted.lock().await.clone();
    Ok(serde_json::to_value(handle)?)
}

async fn get_snapshot(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    // Return null if perspective doesn't exist or user can't access it
    let perspective = match get_perspective_with_access(&uuid, &ctx).await {
        Ok(p) => p,
        Err(e) if e.code == 403 || e.code == 404 => return Ok(Value::Null),
        Err(e) => return Err(e),
    };
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
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(crate::types::domain::Perspective {
        links,
    })?)
}

async fn publish_snapshot(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let _perspective = get_perspective_with_access(&uuid, &ctx).await?;
    Err(WsRpcError::not_implemented(
        "publish_snapshot not yet implemented",
    ))
}

async fn query_links(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    let query = LinkQuery {
        source: params.opt_str("source"),
        predicate: params.opt_str("predicate"),
        target: params.opt_str("target"),
        from_date: params
            .opt_str("fromDate")
            .and_then(|s| serde_json::from_value(Value::String(s)).ok()),
        until_date: params
            .opt_str("untilDate")
            .and_then(|s| serde_json::from_value(Value::String(s)).ok()),
        limit: params
            .get("limit")
            .and_then(|v| v.as_i64())
            .map(|v| v as i32),
    };

    let links = perspective
        .get_links(&query)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(links)?)
}

async fn create_perspective(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &PERSPECTIVE_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: CreatePerspectiveRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let owner_did = ctx.user_did.clone();

    let handle = if let Some(owner) = &owner_did {
        PerspectiveHandle::new_with_owner(body.name.clone(), owner.clone())
    } else {
        PerspectiveHandle::new_from_name(body.name.clone())
    };

    add_perspective(handle.clone(), None)
        .await
        .map_err(|e| WsRpcError::internal(e))?;

    Ok(serde_json::to_value(handle)?)
}

async fn update_perspective_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: UpdatePerspectiveRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let mut handle = perspective.persisted.lock().await.clone();
    handle.name = Some(body.name);
    update_perspective(&handle)
        .await
        .map_err(|e| WsRpcError::internal(e))?;

    Ok(serde_json::to_value(handle)?)
}

async fn delete_perspective(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_delete_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let _perspective = get_perspective_with_access(&uuid, &ctx).await?;
    remove_perspective(&uuid).await;
    Ok(Value::Bool(true))
}

async fn add_link(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;
    check_credits(&ctx.user_email)?;

    let body: AddLinkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let status = parse_link_status(body.status.as_deref());

    let result = perspective
        .add_link(Link::from(body.link), status, body.batch_id, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let _ = reserve_credits(&ctx.user_email, DEFAULT_LINK_WRITE);
    Ok(serde_json::to_value(result)?)
}

async fn add_links_bulk(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;
    check_credits(&ctx.user_email)?;

    let body: AddLinksBulkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let status = parse_link_status(body.status.as_deref());

    let mutations = LinkMutations {
        additions: body.links,
        removals: vec![],
    };

    let diff = perspective
        .link_mutations(mutations, status, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let count = diff.additions.len();
    if count > 0 {
        let _ = reserve_credits(&ctx.user_email, count as f64 * DEFAULT_LINK_WRITE);
    }

    Ok(serde_json::to_value(diff.additions)?)
}

async fn remove_links_bulk(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: RemoveLinksBulkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    if let Some(batch_id) = body.batch_id {
        let mut removals = Vec::with_capacity(body.links.len());
        for link in body.links {
            let removed = perspective
                .remove_link(
                    LinkExpression::from_input_without_proof(link),
                    Some(batch_id.clone()),
                )
                .await
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            removals.push(removed);
        }
        return Ok(serde_json::to_value(removals)?);
    }

    let mutations = LinkMutations {
        additions: vec![],
        removals: body.links,
    };

    let diff = perspective
        .link_mutations(mutations, LinkStatus::Shared, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(diff.removals)?)
}

async fn link_mutations(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;
    check_credits(&ctx.user_email)?;

    let body: LinkMutationsRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let status = parse_link_status(body.status.as_deref());

    let diff = perspective
        .link_mutations(body.mutations, status, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let total = diff.additions.len() + diff.removals.len();
    if total > 0 {
        let _ = reserve_credits(&ctx.user_email, total as f64 * DEFAULT_LINK_WRITE);
    }

    Ok(serde_json::to_value(LinkMutationResponse {
        additions: diff.additions,
        removals: diff.removals,
        updates: vec![],
    })?)
}

async fn add_link_expression(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;
    check_credits(&ctx.user_email)?;

    let body: AddLinkExpressionRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;

    let status = parse_link_status(body.status.as_deref());

    let result = perspective
        .add_link_expression(body.link, status, body.batch_id)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let _ = reserve_credits(&ctx.user_email, DEFAULT_LINK_WRITE);
    Ok(serde_json::to_value(result)?)
}

async fn update_link(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: UpdateLinkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let result = perspective
        .update_link(
            LinkExpression::from_input_without_proof(body.old_link),
            Link::from(body.new_link),
            body.batch_id,
            &agent_context,
        )
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(result)?)
}

async fn remove_link(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: RemoveLinkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;

    let link_expr = LinkExpression::from_input_without_proof(body.link);
    perspective
        .remove_link(link_expr, body.batch_id)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn query_prolog(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let query = params.require_str("query")?;
    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let res = perspective
        .prolog_query_with_context(query, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(prolog_resolution_to_string(res))?)
}

/// Server-side timeout for SPARQL queries (seconds).
const SPARQL_QUERY_TIMEOUT_SECS: u64 = 30;

async fn query_sparql(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let query = params.require_str("query")?;
    let engine = params
        .opt_str("engine")
        .unwrap_or_else(|| "sparql".to_string());
    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    match engine.as_str() {
        "sparql" => {
            // Run the synchronous SPARQL query on a blocking thread with a timeout
            // so it doesn't block the async runtime or hang indefinitely.
            let result = tokio::time::timeout(
                Duration::from_secs(SPARQL_QUERY_TIMEOUT_SECS),
                tokio::task::spawn_blocking(move || perspective.sparql_query(query)),
            )
            .await;

            match result {
                Ok(Ok(Ok(json))) => Ok(serde_json::to_value(json)?),
                Ok(Ok(Err(e))) => Err(WsRpcError::internal(e.to_string())),
                Ok(Err(e)) => Err(WsRpcError::internal(format!("Task join error: {}", e))),
                Err(_) => {
                    log::warn!(
                        "SPARQL query timed out after {}s",
                        SPARQL_QUERY_TIMEOUT_SECS
                    );
                    Err(WsRpcError {
                        code: 408,
                        message: format!(
                            "SPARQL query timed out after {}s",
                            SPARQL_QUERY_TIMEOUT_SECS
                        ),
                    })
                }
            }
        }
        "surreal" => Err(WsRpcError::bad_request(
            "SurrealDB query engine not available. Use 'sparql'.",
        )),
        other => Err(WsRpcError::bad_request(format!(
            "Unknown query engine: {}. Use 'sparql' or 'surreal'.",
            other
        ))),
    }
}

async fn add_sdna(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    // Batch mode: entries array present
    if let Some(entries_val) = params.get("entries") {
        let entries: Vec<AddSdnaRequest> = serde_json::from_value(entries_val.clone())
            .map_err(|e| WsRpcError::bad_request(format!("Invalid entries: {}", e)))?;

        let batch: Vec<(String, String, SdnaType, Option<String>)> = entries
            .into_iter()
            .map(|entry| {
                let sdna_type = SdnaType::from_string(&entry.sdna_type)
                    .map_err(|e| WsRpcError::bad_request(format!("Invalid SDNA type: {}", e)))?;
                Ok((
                    entry.name,
                    entry.sdna_code.unwrap_or_default(),
                    sdna_type,
                    entry.shacl_json,
                ))
            })
            .collect::<Result<Vec<_>, WsRpcError>>()?;

        let results = perspective
            .add_sdna_batch(batch, &agent_context)
            .await
            .map_err(|e| WsRpcError::internal(e.to_string()))?;

        Ok(serde_json::to_value(results).unwrap())
    } else {
        // Single-entry mode (backward compatible)
        let body: AddSdnaRequest = serde_json::from_value(params.clone())
            .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

        let sdna_type = SdnaType::from_string(&body.sdna_type)
            .map_err(|e| WsRpcError::bad_request(format!("Invalid SDNA type: {}", e)))?;

        let result = perspective
            .add_sdna(
                body.name,
                body.sdna_code.unwrap_or_default(),
                sdna_type,
                body.shacl_json,
                &agent_context,
            )
            .await
            .map_err(|e| WsRpcError::internal(e.to_string()))?;

        Ok(Value::Bool(result))
    }
}

async fn execute_commands(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: ExecuteCommandsRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let commands: Vec<crate::perspectives::perspective_instance::Command> =
        serde_json::from_str(&body.commands)
            .map_err(|e| WsRpcError::bad_request(format!("Invalid commands JSON: {}", e)))?;
    let parameters: Vec<crate::perspectives::perspective_instance::Parameter> = body
        .parameters
        .as_ref()
        .map(|json| {
            serde_json::from_str(json)
                .map_err(|e| WsRpcError::bad_request(format!("Invalid parameters JSON: {}", e)))
        })
        .transpose()?
        .unwrap_or_default();

    let result = perspective
        .execute_commands(
            commands,
            body.expression.clone(),
            parameters,
            body.batch_id.clone(),
            &agent_context,
        )
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(result)?)
}

async fn create_batch(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let batch_id = perspective.create_batch().await;

    Ok(Value::String(batch_id))
}

async fn commit_batch(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: CommitBatchRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let diff = perspective
        .commit_batch(body.batch_id.clone(), &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(LinkMutationResponse {
        additions: diff.additions,
        removals: diff.removals,
        updates: vec![],
    })?)
}

async fn subscribe_query(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: SubscribeQueryRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    let (subscription_id, result) = perspective
        .subscribe_and_query(body.query, ctx.user_email.clone())
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(SubscribeQueryResponse {
        subscription_id,
        result,
    })?)
}

async fn subscribe_surreal_query(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let _ = get_perspective_with_access(&uuid, &ctx).await?;
    Err(WsRpcError::not_implemented(
        "subscribe_surreal_query not yet implemented",
    ))
}

async fn keep_alive_query(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    let body: KeepAliveQueryRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    perspective
        .keepalive_query(body.subscription_id)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn dispose_query(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    let body: DisposeQueryRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    perspective
        .dispose_query_subscription(body.subscription_id)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn create_subject(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: CreateSubjectRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let (resolved_class_name, parsed_initial_values) =
        match serde_json::from_str::<serde_json::Value>(&body.subject_class) {
            Ok(obj) if obj.is_object() && obj.get("className").is_some() => {
                let cn = obj["className"]
                    .as_str()
                    .unwrap_or(&body.subject_class)
                    .to_string();
                let iv = obj.get("initialValues").cloned();
                (cn, iv)
            }
            _ => (body.subject_class.clone(), None),
        };

    let subject_class = crate::perspectives::perspective_instance::SubjectClassOption {
        class_name: Some(resolved_class_name),
        query: None,
    };
    let initial_values: Option<serde_json::Value> = body
        .initial_values
        .as_ref()
        .and_then(|s| serde_json::from_str(s).ok())
        .or(parsed_initial_values);
    perspective
        .create_subject(
            subject_class,
            body.expression_address.clone(),
            initial_values,
            body.batch_id.clone(),
            &agent_context,
        )
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn get_subject_data(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: GetSubjectDataRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let subject_class = crate::perspectives::perspective_instance::SubjectClassOption {
        class_name: Some(body.subject_class.clone()),
        query: None,
    };
    let data = perspective
        .get_subject_data(
            subject_class,
            body.expression_address.clone(),
            &agent_context,
        )
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(data))
}

async fn model_query_handler(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let class_name = params.require_str("class_name")?;
    let query_json = params.require_str("query_json")?;
    let shape_json = params.opt_str("shape_json");

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    // Run synchronous SPARQL-backed model query on a blocking thread with timeout
    // to avoid blocking the async runtime (same pattern as query_sparql handler).
    let result = tokio::time::timeout(
        Duration::from_secs(SPARQL_QUERY_TIMEOUT_SECS),
        tokio::task::spawn_blocking(move || {
            perspective.model_query(&class_name, &query_json, shape_json.as_deref())
        }),
    )
    .await;

    match result {
        Ok(Ok(Ok(json))) => Ok(Value::String(json)),
        Ok(Ok(Err(e))) => Err(WsRpcError::internal(e.to_string())),
        Ok(Err(e)) => Err(WsRpcError::internal(format!("Task join error: {}", e))),
        Err(_) => {
            log::warn!(
                "Model query timed out after {}s",
                SPARQL_QUERY_TIMEOUT_SECS
            );
            Err(WsRpcError {
                code: 408,
                message: format!(
                    "Model query timed out after {}s",
                    SPARQL_QUERY_TIMEOUT_SECS
                ),
            })
        }
    }
}

async fn evaluate_getters_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let class_name = params.require_str("class_name")?;
    let shape_json = params.opt_str("shape_json");

    // Parse instance_ids array
    let instance_ids: Vec<String> = params
        .get("instance_ids")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default();

    // Parse optional property_names array
    let property_names: Option<Vec<String>> = params
        .get("property_names")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        });

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    // Run synchronous getter evaluation on a blocking thread with timeout
    // to avoid blocking the async runtime.
    let result = tokio::time::timeout(
        Duration::from_secs(SPARQL_QUERY_TIMEOUT_SECS),
        tokio::task::spawn_blocking(move || {
            perspective.evaluate_getters(
                &class_name,
                &instance_ids,
                property_names.as_deref(),
                shape_json.as_deref(),
            )
        }),
    )
    .await;

    match result {
        Ok(Ok(Ok(json))) => Ok(Value::String(json)),
        Ok(Ok(Err(e))) => Err(WsRpcError::internal(e.to_string())),
        Ok(Err(e)) => Err(WsRpcError::internal(format!("Task join error: {}", e))),
        Err(_) => {
            log::warn!(
                "Getter evaluation timed out after {}s",
                SPARQL_QUERY_TIMEOUT_SECS
            );
            Err(WsRpcError {
                code: 408,
                message: format!(
                    "Getter evaluation timed out after {}s",
                    SPARQL_QUERY_TIMEOUT_SECS
                ),
            })
        }
    }
}

async fn model_subscribe_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let class_name = params.require_str("class_name")?;
    let query_json = params.require_str("query_json")?;
    let shape_json = params.opt_str("shape_json");

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    let user_email = ctx.user_email.clone();
    let (subscription_id, result_string) = perspective
        .model_subscribe_and_query(class_name, query_json, shape_json, user_email)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(serde_json::json!({
        "subscription_id": subscription_id,
        "result": result_string,
    }))?)
}

// ── Registration ──

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("perspective.all", list_perspectives);
    map.register("perspective.get", get_perspective_handler);
    map.register("perspective.create", create_perspective);
    map.register("perspective.update", update_perspective_handler);
    map.register("perspective.remove", delete_perspective);
    map.register("perspective.snapshot", get_snapshot);
    map.register("perspective.publishSnapshot", publish_snapshot);
    map.register("perspective.queryLinks", query_links);
    map.register("perspective.addLink", add_link);
    map.register("perspective.addLinkExpression", add_link_expression);
    map.register("perspective.addLinks", add_links_bulk);
    map.register("perspective.updateLink", update_link);
    map.register("perspective.removeLink", remove_link);
    map.register("perspective.removeLinks", remove_links_bulk);
    map.register("perspective.linkMutations", link_mutations);
    map.register("perspective.queryProlog", query_prolog);
    map.register("perspective.querySparql", query_sparql);
    map.register("perspective.addSdna", add_sdna);
    map.register("perspective.executeCommands", execute_commands);
    map.register("perspective.createSubject", create_subject);
    map.register("perspective.getSubjectData", get_subject_data);
    map.register("perspective.createBatch", create_batch);
    map.register("perspective.commitBatch", commit_batch);
    map.register("perspective.subscribeQuery", subscribe_query);
    map.register("perspective.keepAliveQuery", keep_alive_query);
    map.register("perspective.disposeQuery", dispose_query);
    map.register("perspective.subscribeSparql", subscribe_surreal_query);
    map.register("perspective.keepAliveSparql", keep_alive_query);
    map.register("perspective.disposeSparql", dispose_query);
    map.register("perspective.modelQuery", model_query_handler);
    map.register("perspective.modelSubscribe", model_subscribe_handler);
    map.register("perspective.evaluateGetters", evaluate_getters_handler);
}

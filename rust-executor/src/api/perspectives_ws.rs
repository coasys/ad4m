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

    if let Err(e) = reserve_credits(&ctx.user_email, DEFAULT_LINK_WRITE) {
        log::warn!(
            "Credit deduction failed (operation already committed): {}",
            e
        );
    }
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
        if let Err(e) = reserve_credits(&ctx.user_email, count as f64 * DEFAULT_LINK_WRITE) {
            log::warn!(
                "Credit deduction failed for bulk add (operation already committed): {}",
                e
            );
        }
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
        if let Err(e) = reserve_credits(&ctx.user_email, total as f64 * DEFAULT_LINK_WRITE) {
            log::warn!(
                "Credit deduction failed for mutations (operation already committed): {}",
                e
            );
        }
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

    if let Err(e) = reserve_credits(&ctx.user_email, DEFAULT_LINK_WRITE) {
        log::warn!(
            "Credit deduction failed (operation already committed): {}",
            e
        );
    }
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

/// Timeout for `run_interpretation` — an LLM interpretation pass includes
/// prompt build + model call + planning + writes. Longer than the SPARQL
/// budget because the LLM leg alone can take tens of seconds on local
/// models; short enough that a stalled provider cannot pin a WS slot
/// forever (a slot held past this bound returns a 408 to the caller).
const RUN_INTERPRETATION_TIMEOUT_SECS: u64 = 300;

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
        other => Err(WsRpcError::bad_request(format!(
            "Unknown query engine: {}. Use 'sparql'.",
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

        Ok(serde_json::to_value(results)?)
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

async fn subscribe_sparql_query(
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
        "subscribe_sparql_query not yet implemented",
    ))
}

async fn keep_alive_query(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

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
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

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

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    // Run async model query with timeout
    let result = tokio::time::timeout(
        Duration::from_secs(SPARQL_QUERY_TIMEOUT_SECS),
        perspective.model_query(&class_name, &query_json),
    )
    .await;

    match result {
        Ok(Ok(json)) => Ok(Value::String(json)),
        Ok(Err(e)) => Err(WsRpcError::internal(e.to_string())),
        Err(_) => {
            log::warn!("Model query timed out after {}s", SPARQL_QUERY_TIMEOUT_SECS);
            Err(WsRpcError {
                code: 408,
                message: format!("Model query timed out after {}s", SPARQL_QUERY_TIMEOUT_SECS),
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
            perspective.evaluate_getters(&class_name, &instance_ids, property_names.as_deref())
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

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    let user_email = ctx.user_email.clone();
    let (subscription_id, result_string) = perspective
        .model_subscribe_and_query(class_name, query_json, user_email)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(serde_json::json!({
        "subscription_id": subscription_id,
        "result": result_string,
    }))?)
}

async fn run_interpretation_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;
    check_credits(&ctx.user_email)?;

    let body: RunInterpretationRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    // Interpretation writes via `create_subject`, so link status derives from each
    // class's SDNA `local` flags (same rule as app code) — there is no caller-facing
    // link-status knob on this path.

    // Target classes: the caller's explicit selection, or (default) all subject
    // classes registered in the perspective.
    let explicit_selection = matches!(&body.classes, Some(sel) if !sel.is_empty());
    let class_names = match &body.classes {
        Some(sel) if !sel.is_empty() => sel.clone(),
        _ => perspective
            .get_subject_classes_from_shacl()
            .await
            .map_err(|e| WsRpcError::internal(e.to_string()))?,
    };
    let mut shapes = Vec::with_capacity(class_names.len());
    let mut unresolved: Vec<String> = Vec::new();
    for name in &class_names {
        match perspective.get_shape(name) {
            Ok(shape) => shapes.push((*shape).clone()),
            Err(e) => {
                log::warn!("runInterpretation: skipping class '{}': {}", name, e);
                unresolved.push(name.clone());
            }
        }
    }
    // Explicit-selection failure surfaces the actual cause: the caller
    // passed class names and every one of them failed to resolve. Falling
    // back to the "no subject classes to extract into" default-path
    // message would misdirect them.
    if explicit_selection && !unresolved.is_empty() && shapes.is_empty() {
        return Err(WsRpcError::bad_request(format!(
            "runInterpretation: none of the requested classes could be resolved: [{}]",
            unresolved.join(", ")
        )));
    }
    if shapes.is_empty() {
        return Err(WsRpcError::bad_request(
            "perspective has no subject classes to extract into",
        ));
    }

    // The public WS turn carries `speaker`/`text` only. `timestamp` stays an
    // AutoProcessor concern: it is bound by the scope query's SPARQL gather so
    // repeated wording at different times hashes to distinct turns, which a
    // one-shot caller passing an explicit transcript has no cursor for.
    let transcript: Vec<crate::perspectives::interpretation::TranscriptTurn> = body
        .transcript
        .into_iter()
        .map(|t| {
            crate::perspectives::interpretation::TranscriptTurn::from_speaker_text(
                t.speaker, t.text,
            )
        })
        .collect();

    // Bound the LLM call: a stalled provider must not pin a WS request slot
    // indefinitely. Matches the pattern used by `query_sparql` /
    // `model_query_handler` / `evaluate_getters_handler`, but with a longer
    // budget appropriate for the interpretation pass (see
    // `RUN_INTERPRETATION_TIMEOUT_SECS`).
    let bases = match tokio::time::timeout(
        Duration::from_secs(RUN_INTERPRETATION_TIMEOUT_SECS),
        crate::perspectives::interpretation::run_interpretation(
            &mut perspective,
            &shapes,
            &transcript,
            &body.base_prefix,
            &agent_context,
            // Existing-instance scope: this direct WS entry point
            // interprets over the whole perspective (no per-channel
            // sub-scope). #883 added the `scope` plumbing; the
            // AutoProcessor is where a channel scope is supplied.
            None,
        ),
    )
    .await
    {
        Ok(Ok(bases)) => bases,
        Ok(Err(e)) => return Err(WsRpcError::internal(e.to_string())),
        Err(_) => {
            log::warn!(
                "run_interpretation timed out after {}s",
                RUN_INTERPRETATION_TIMEOUT_SECS
            );
            return Err(WsRpcError {
                code: 408,
                message: format!(
                    "runInterpretation timed out after {}s",
                    RUN_INTERPRETATION_TIMEOUT_SECS
                ),
            });
        }
    };

    if !bases.is_empty() {
        if let Err(e) = reserve_credits(&ctx.user_email, bases.len() as f64 * DEFAULT_LINK_WRITE) {
            log::warn!(
                "Credit deduction failed for runInterpretation (operation already committed): {}",
                e
            );
        }
    }

    Ok(serde_json::to_value(bases)?)
}

/// Register a neighbourhood auto-processor on a perspective. Writes the
/// `AutoProcessorConfig` into the shared graph; the executor watch loop reads it
/// back and starts running interpretation automatically over new source items,
/// emitting step signals on the events WebSocket (`auto-processor-event`).
async fn add_auto_processor_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    use crate::api::types::AddAutoProcessorRequest;
    use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};

    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: AddAutoProcessorRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    // Validate before persisting: `load_processors` silently skips configs
    // that violate these invariants, so if we accept a bad one the caller
    // sees a "success" response but the processor never runs. Bounce it
    // back at the boundary instead. Ranges mirror
    // `AutoProcessorConfig::config_from_instance` (rust-executor/src/
    // perspectives/auto_processor/config.rs).
    if body.interpretation_classes.is_empty() {
        return Err(WsRpcError::bad_request(
            "`interpretationClasses` must be non-empty",
        ));
    }
    if body.debounce_ms < 0 {
        return Err(WsRpcError::bad_request("`debounceMs` must be >= 0"));
    }
    if body.batch_max == 0 {
        return Err(WsRpcError::bad_request("`batchMax` must be >= 1"));
    }
    if body.claim_ttl_ms <= 0 {
        return Err(WsRpcError::bad_request("`claimTtlMs` must be > 0"));
    }
    if body.source_window_ms.is_some_and(|w| w <= 0) {
        return Err(WsRpcError::bad_request(
            "`sourceWindowMs` must be > 0 when set (omit for no window)",
        ));
    }
    if body.max_wait_ms.is_some_and(|w| w < 0) {
        return Err(WsRpcError::bad_request("`maxWaitMs` must be >= 0 when set"));
    }

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let cfg = AutoProcessorConfig {
        processor_id: body.processor_id.clone(),
        source_scope_query: body.source_scope_query,
        base_prefix: body.base_prefix,
        interpretation_classes: body.interpretation_classes,
        debounce_ms: body.debounce_ms,
        batch_min: body.batch_min.unwrap_or(1),
        batch_max: body.batch_max,
        max_wait_ms: body.max_wait_ms,
        claim_ttl_ms: body.claim_ttl_ms,
        dedup_strategy_json: body.dedup_strategy_json,
        source_window_ms: body.source_window_ms,
        existing_scope: body.existing_scope,
        mint_scope: body.mint_scope,
        debug_mode: body.debug_mode.unwrap_or(false),
    };
    write_processor(&mut perspective, &cfg, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(body.processor_id)?)
}

/// `perspective.acceptInterpretation` — materialize the overlay's staged
/// suggestion(s) as human-owned real value(s) and delete the (targeted) overlay.
async fn accept_interpretation_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    use crate::api::types::ResolveInterpretationRequest;
    use crate::perspectives::interpretation::overlay::accept_interpretation;

    let body: ResolveInterpretationRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![body.uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let mut perspective = get_perspective_with_access(&body.uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    accept_interpretation(
        &mut perspective,
        &body.base,
        body.property.as_deref(),
        &agent_context,
    )
    .await
    .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(true))
}

/// `perspective.rejectInterpretation` — drop the overlay's suggestion(s); a
/// whole-base reject of a `create` deletes the suggested instance.
async fn reject_interpretation_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    use crate::api::types::ResolveInterpretationRequest;
    use crate::perspectives::interpretation::overlay::reject_interpretation;

    let body: ResolveInterpretationRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![body.uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let mut perspective = get_perspective_with_access(&body.uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    reject_interpretation(
        &mut perspective,
        &body.base,
        body.property.as_deref(),
        &agent_context,
    )
    .await
    .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(true))
}

/// `perspective.interpretationOverlays` — pending overlay suggestions in the
/// perspective (read-only), for a UI to surface human accept/reject.
async fn interpretation_overlays_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    use crate::api::types::InterpretationOverlaysRequest;
    use crate::perspectives::interpretation::overlay::list_overlays;

    let body: InterpretationOverlaysRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![body.uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;
    let perspective = get_perspective_with_access(&body.uuid, &ctx).await?;
    let overlays = list_overlays(&perspective)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(overlays)?)
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
    map.register("perspective.subscribeSparql", subscribe_sparql_query);
    map.register("perspective.keepAliveSparql", keep_alive_query);
    map.register("perspective.disposeSparql", dispose_query);
    map.register("perspective.modelQuery", model_query_handler);
    map.register("perspective.modelSubscribe", model_subscribe_handler);
    map.register("perspective.evaluateGetters", evaluate_getters_handler);
    map.register("perspective.runInterpretation", run_interpretation_handler);
    map.register("perspective.addAutoProcessor", add_auto_processor_handler);
    map.register(
        "perspective.acceptInterpretation",
        accept_interpretation_handler,
    );
    map.register(
        "perspective.rejectInterpretation",
        reject_interpretation_handler,
    );
    map.register(
        "perspective.interpretationOverlays",
        interpretation_overlays_handler,
    );
}

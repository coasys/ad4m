//! Perspective WS-native handlers.

use serde::Serialize;
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

/// Adapter that implements the harness's `CreditGate` trait against the
/// per-user credit ledger. Each `check()` (a) refuses if the ledger says
/// insufficient, and (b) reserves `DEFAULT_LINK_WRITE` from the ledger
/// so the mid-loop opportunity cost is accounted for — not just the
/// bases the pass eventually lands.
///
/// Rationale (James's review 2026-08-25): the old shape was
/// `check_credits` once at entry, then `reserve_credits(bases.len() * DEFAULT_LINK_WRITE)`
/// at exit. Up-to-`max_tool_calls + 1` completions in between hit
/// neither, and `AIService::bill_prompt_if_authed` is a
/// fire-and-forget deduction that logs on `InsufficientCredits` but
/// doesn't halt the loop. A pass that ends with zero bases used to be
/// free regardless of how many completions it burned.
struct WsHarnessCreditGate {
    user_email: Option<String>,
}

impl WsHarnessCreditGate {
    fn new(user_email: Option<String>) -> Self {
        Self { user_email }
    }
}

#[async_trait::async_trait]
impl crate::ai_service::harness::CreditGate for WsHarnessCreditGate {
    async fn check(&self) -> anyhow::Result<()> {
        // Pre-check: cheap "credits > 0" gate. Free hosting / free-access
        // paths short-circuit inside `check_credits`.
        check_credits(&self.user_email).map_err(|e| anyhow::anyhow!("{e:?}"))?;
        // Reserve the per-completion opportunity cost. Fire-and-forget
        // wrt success — the ledger deduction happens atomically and any
        // downstream `InsufficientCredits` will fail the NEXT `check`.
        reserve_credits(&self.user_email, DEFAULT_LINK_WRITE)
            .map_err(|e| anyhow::anyhow!("{e:?}"))?;
        Ok(())
    }
}

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
        // KEEP AT warn (Nico + CodeRabbit, PR #942 round 2): money-losing
        // credit-ledger failures at debug hide ledger inconsistency. The
        // link write already committed, so this is not fatal to the
        // request — but the missed deduction is a real accounting drift
        // that ops needs to see. Do not downgrade in future cleanups.
        // See rust-executor/LOGGING.md.
        log::warn!(
            "⚠️ 💳 credit deduction failed (link write already committed): {}",
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

/// Longer server budget for the harness (tool-calling) path. A single
/// harness pass is N tool round-trips plus a final answer, so it
/// legitimately takes longer than a single-shot generation. Matches the
/// 20-minute RPC timeout the client (`PerspectiveClient
/// .runInterpretationWithHarness`) sets on this method; if the server
/// budget were shorter, slow local models would 408 on the server while
/// the client still waited.
const RUN_INTERPRETATION_HARNESS_TIMEOUT_SECS: u64 = 1200;

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
            // Run the synchronous SPARQL query on a blocking thread with a
            // hard timeout (so the executor doesn't hang indefinitely on
            // a pathological query) and a soft cancellation token (so the
            // client can abort with `request.cancel`).
            //
            // When `ctx.cancel_token` is present (always true under the
            // WS dispatcher), we use `sparql_query_cancellable` which
            // races the eval against `cancel.cancelled()`.  When it's
            // None (internal callers / tests), fall back to the
            // historical timeout + spawn_blocking shape.
            let timeout = Duration::from_secs(SPARQL_QUERY_TIMEOUT_SECS);
            let result = if let Some(cancel) = ctx.cancel_token.clone() {
                tokio::time::timeout(timeout, perspective.sparql_query_cancellable(query, cancel))
                    .await
            } else {
                let join = tokio::task::spawn_blocking(move || perspective.sparql_query(query));
                tokio::time::timeout(timeout, async move {
                    join.await
                        .map_err(|e| deno_core::anyhow::anyhow!("Task join error: {}", e))?
                })
                .await
            };

            match result {
                Ok(Ok(json)) => Ok(serde_json::to_value(json)?),
                Ok(Err(e)) => {
                    // Surface client cancellation as 499 so the dispatcher
                    // doesn't have to special-case it — same wire shape as
                    // the racing branch in `ws_rpc::handle_ws`.  Other
                    // errors (anyhow string, including "query cancelled")
                    // surface as 500 unless we recognise the cancel marker.
                    let msg = e.to_string();
                    if msg.contains("query cancelled") {
                        Err(WsRpcError {
                            code: 499,
                            message: "Request cancelled by client".to_string(),
                        })
                    } else {
                        Err(WsRpcError::internal(msg))
                    }
                }
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

async fn subject_classes_of_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    // A malformed element cannot be dropped here. The result map omits URIs that
    // matched no class, so a silently discarded input would be indistinguishable
    // from a URI that is simply not a subject instance — the caller would read a
    // shape error as a legitimate answer. Absent (or null, which is how an
    // undefined field arrives from JS) keeps the empty default; anything present
    // has to be an array of strings.
    let uris: Vec<String> = match params.get("uris") {
        None | Some(Value::Null) => Vec::new(),
        Some(v) => serde_json::from_value(v.clone()).map_err(|e| {
            WsRpcError::bad_request(format!(
                "Invalid parameter 'uris': expected string[]: {}",
                e
            ))
        })?,
    };

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    // Classification is synchronous SPARQL plus in-memory containment over every
    // class × every URI, so it runs on a blocking thread with the same timeout as
    // the other store-backed queries rather than holding a runtime worker.
    let result = tokio::time::timeout(
        Duration::from_secs(SPARQL_QUERY_TIMEOUT_SECS),
        tokio::task::spawn_blocking(move || perspective.subject_classes_of(&uris)),
    )
    .await;

    match result {
        Ok(Ok(Ok(map))) => {
            serde_json::to_value(map).map_err(|e| WsRpcError::internal(e.to_string()))
        }
        Ok(Ok(Err(e))) => Err(WsRpcError::internal(e.to_string())),
        Ok(Err(e)) => Err(WsRpcError::internal(format!("Task join error: {}", e))),
        Err(_) => {
            log::warn!(
                "Subject classification timed out after {}s",
                SPARQL_QUERY_TIMEOUT_SECS
            );
            Err(WsRpcError {
                code: 408,
                message: format!(
                    "Subject classification timed out after {}s",
                    SPARQL_QUERY_TIMEOUT_SECS
                ),
            })
        }
    }
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

    // Pre-pass snapshot of every instance URI the target classes already
    // know about — used below to filter mint-scope linking to freshly
    // created bases only. Skipped entirely when mint_scope is absent so
    // one-shot callers not using mint-scope pay nothing. Matches the
    // watcher's approach (`auto_processor::watcher::pass::run_pass`).
    let pre_existing_uris: std::collections::HashSet<String> = if body.mint_scope.is_some() {
        crate::perspectives::interpretation::existing_instance_context(
            &mut perspective,
            &shapes,
            None,
        )
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .into_values()
        .flat_map(|instances| instances.into_iter().map(|i| i.id))
        .collect()
    } else {
        std::collections::HashSet::new()
    };

    /*
       Live observability for a one-shot pass (#903 follow-up).

       A watch pass is fully observable; this path was not. The asymmetry is worth closing because
       `runInterpretation` is synchronous for the caller — it blocks for the whole pass, which is
       seconds to minutes on a local model — so it is the path where progress reporting is most
       useful and the only one that had none.

       `observation_id` is the caller's own identifier (see `RunInterpretationRequest`), used as
       both `processor_id` and `batch_key` so a consumer merging the two event streams handles a
       one-shot pass with the code it already has for a watch pass.

       Absent = every emit below is skipped and this handler behaves exactly as it did.
    */
    let observation = body.observation_id.clone();
    let observer_did = match observation.as_ref() {
        // Best-effort: telemetry must never be the reason a pass refuses to run, so an
        // unresolvable DID downgrades to no observation rather than to an error.
        Some(_) => crate::agent::did_for_context(&agent_context).ok(),
        None => None,
    };
    let observe = observation.as_ref().zip(observer_did.as_ref());
    let emit_ctx = observe
        .filter(|_| body.emit_debug_events.unwrap_or(false))
        .map(
            |(id, did)| crate::perspectives::auto_processor::events::InterpretationEmitContext {
                perspective_uuid: uuid.clone(),
                processor_id: id.clone(),
                agent_did: did.clone(),
                // No source item ids: the caller supplied the transcript directly rather than the
                // watcher gathering it, so there is nothing to list. `batch_key` carries the identity
                // instead — which is why it is a separate field rather than derived from these.
                item_ids: Vec::new(),
                batch_key: id.clone(),
            },
        );

    if let Some((id, did)) = observe {
        emit_one_shot_step(
            &uuid,
            id,
            did,
            crate::perspectives::auto_processor::events::AutoProcessorStep::RunningInterpretation,
        )
        .await;
        emit_one_shot_phase(
            &uuid,
            id,
            did,
            crate::perspectives::auto_processor::events::NeighbourhoodPhase::Claimed,
        )
        .await;
    }

    // Bound the LLM call: a stalled provider must not pin a WS request slot
    // indefinitely. Matches the pattern used by `query_sparql` /
    // `model_query_handler` / `evaluate_getters_handler`, but with a longer
    // budget appropriate for the interpretation pass (see
    // `RUN_INTERPRETATION_TIMEOUT_SECS`).
    let bases = match tokio::time::timeout(
        Duration::from_secs(RUN_INTERPRETATION_TIMEOUT_SECS),
        crate::perspectives::interpretation::run_interpretation_observed(
            &mut perspective,
            &shapes,
            &transcript,
            &body.base_prefix,
            &agent_context,
            // Existing-instance scope: when the caller supplies one,
            // dedup lookup is restricted to instances under that subtree
            // (same semantics as `AutoProcessorConfig.existingScope`).
            // `None` keeps the whole-perspective dedup set — the
            // pre-scope default that #883 originally added the plumbing for.
            body.existing_scope.as_ref(),
            emit_ctx.as_ref(),
        ),
    )
    .await
    {
        Ok(Ok(bases)) => bases,
        Ok(Err(e)) => {
            // Both failure arms close the row before returning. A consumer that opened one on
            // `Claimed` and never heard again would show a pass running forever — and the two
            // cases a person most wants to see reported (the model errored, the model hung) are
            // exactly the two that would hang the UI instead.
            if let Some((id, did)) = observe {
                emit_one_shot_abandoned(&uuid, id, did, &e.to_string()).await;
            }
            return Err(WsRpcError::internal(e.to_string()));
        }
        Err(_) => {
            log::warn!(
                "run_interpretation timed out after {}s",
                RUN_INTERPRETATION_TIMEOUT_SECS
            );
            if let Some((id, did)) = observe {
                emit_one_shot_abandoned(
                    &uuid,
                    id,
                    did,
                    &format!("timed out after {RUN_INTERPRETATION_TIMEOUT_SECS}s"),
                )
                .await;
            }
            return Err(WsRpcError {
                code: 408,
                message: format!(
                    "runInterpretation timed out after {}s",
                    RUN_INTERPRETATION_TIMEOUT_SECS
                ),
            });
        }
    };

    // Mint-scope child links: same rule as AutoProcessor — only freshly
    // created bases get an extra `mintScope.id --predicate--> new_base` edge
    // so upserts of pre-existing instances don't get re-parented into the
    // caller's scope. Written outside the interpretation batch (the watcher
    // takes the same approach): if this write fails the base is orphaned,
    // but a subsequent call will upsert by identity and re-attempt.
    if let Some(mint_scope) = body.mint_scope.as_ref() {
        let created = crate::perspectives::auto_processor::watcher::partition_created(
            &bases,
            &pre_existing_uris,
        );
        crate::perspectives::auto_processor::watcher::write_mint_scope_links(
            &mut perspective,
            mint_scope,
            &created,
            &agent_context,
            "runInterpretation",
        )
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    }

    if !bases.is_empty() {
        if let Err(e) = reserve_credits(&ctx.user_email, bases.len() as f64 * DEFAULT_LINK_WRITE) {
            log::warn!(
                "Credit deduction failed for runInterpretation (operation already committed): {}",
                e
            );
        }
    }

    // Emitted after the mint-scope links are written, not before: `Processed` carries the bases,
    // and a consumer that reads it as "these records exist and are attached" would be reading it
    // half a write early if it fired the moment interpretation returned.
    if let Some((id, did)) = observe {
        emit_one_shot_bases(&uuid, id, did, &bases).await;
        emit_one_shot_phase(
            &uuid,
            id,
            did,
            crate::perspectives::auto_processor::events::NeighbourhoodPhase::Finished,
        )
        .await;
    }

    Ok(serde_json::to_value(bases)?)
}

/// Harness-dispatched interpretation pass over WS-RPC — the tool-calling
/// counterpart to `perspective.runInterpretation`. Wraps
/// [`run_interpretation_with_harness_and_model`] with the same guardrails
/// (capability + credit checks, class resolution, timeout, credit
/// reservation). The LLM sees a live tool surface (`{Class}_query`,
/// `{Class}_propose_create`, `{Class}_propose_link_child`, …) and drives
/// the extraction by tool calls; buffered proposals drain through the
/// same overlay gate the single-shot path uses.
async fn run_interpretation_with_harness_handler(
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

    let body: RunInterpretationWithHarnessRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    // `max_tool_calls == 0` would collapse the harness loop to a no-op
    // final-answer step. Callers wanting the classic path should use
    // `perspective.runInterpretation` instead — bounce it here so the
    // mistake surfaces at the boundary.
    if body.max_tool_calls == 0 {
        return Err(WsRpcError::bad_request(
            "`maxToolCalls` must be > 0; use `perspective.runInterpretation` for the single-shot path",
        ));
    }

    let mut perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    // Class resolution mirrors `run_interpretation_handler`: explicit
    // selection if provided, otherwise every registered subject class in
    // the perspective. Explicit-selection failure surfaces the actual
    // cause rather than the "no subject classes" default-path message.
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
                log::warn!(
                    "runInterpretationWithHarness: skipping class '{}': {}",
                    name,
                    e
                );
                unresolved.push(name.clone());
            }
        }
    }
    if explicit_selection && !unresolved.is_empty() && shapes.is_empty() {
        return Err(WsRpcError::bad_request(format!(
            "runInterpretationWithHarness: none of the requested classes could be resolved: [{}]",
            unresolved.join(", ")
        )));
    }
    if shapes.is_empty() {
        return Err(WsRpcError::bad_request(
            "perspective has no subject classes to extract into",
        ));
    }

    // Same as the single-shot path: WS turns carry speaker + text only.
    let transcript: Vec<crate::perspectives::interpretation::TranscriptTurn> = body
        .transcript
        .into_iter()
        .map(|t| {
            crate::perspectives::interpretation::TranscriptTurn::from_speaker_text(
                t.speaker, t.text,
            )
        })
        .collect();

    // Thread the caller's auth token into the harness so `Ad4mMcpHandler`'s
    // tool dispatch executes with the caller's capabilities — same
    // principle as the `/v1` openai-compat path. Empty string means an
    // unauthenticated caller: don't propagate that as a phantom token
    // (Ad4mMcpHandler treats `None` as "no user session; fall back to
    // admin credential if configured").
    let auth_token = if ctx.auth_token.is_empty() {
        None
    } else {
        Some(ctx.auth_token.clone())
    };

    // Live-debug event surface: same shape as the classic single-shot
    // handler. `observation_id` names both `processor_id` and `batch_key`
    // on the emitted events so a subscribed UI can correlate this pass's
    // events to the caller-supplied id. `emit_debug_events` is a
    // dead-letter without an observation_id (nothing to key against), so
    // gate on both.
    let observer_did = match &body.observation_id {
        Some(_) => crate::agent::did_for_context(&agent_context).ok(),
        None => None,
    };
    let emit_ctx = body
        .observation_id
        .as_ref()
        .zip(observer_did.as_ref())
        .filter(|_| body.emit_debug_events.unwrap_or(false))
        .map(|(id, did)| {
            crate::perspectives::auto_processor::events::InterpretationEmitContext {
                perspective_uuid: uuid.clone(),
                processor_id: id.clone(),
                agent_did: did.clone(),
                // No source item ids — WS-RPC caller supplies the transcript
                // directly rather than the watcher gathering it. `batch_key`
                // carries the identity instead.
                item_ids: Vec::new(),
                batch_key: id.clone(),
            }
        });

    // Per-completion credit gate — bounds the pass to the caller's
    // available budget across the whole tool-calling loop. Replaces the
    // entry-only `check_credits` guard which used to leave up-to-
    // `max_tool_calls + 1` completions unmetered after credits ran out.
    // Reserves `DEFAULT_LINK_WRITE` per gate call (same rate the pass'
    // exit accounting uses for each landed base) so the accounting
    // stays proportional to the mid-loop opportunity cost.
    let credit_gate: Option<std::sync::Arc<dyn crate::ai_service::harness::CreditGate>> = Some(
        std::sync::Arc::new(WsHarnessCreditGate::new(ctx.user_email.clone())),
    );

    let bases = match tokio::time::timeout(
        Duration::from_secs(RUN_INTERPRETATION_HARNESS_TIMEOUT_SECS),
        crate::perspectives::interpretation::run_interpretation_with_harness_and_model(
            &mut perspective,
            &shapes,
            &transcript,
            &body.base_prefix,
            &agent_context,
            body.model_override.as_deref(),
            body.existing_scope.as_ref(),
            None,
            body.max_tool_calls,
            auth_token,
            emit_ctx.as_ref(),
            // WS-RPC is the one-shot path — dedup-on-drain stays off; the
            // caller reaches for the harness specifically to trust the LLM
            // to `_query` before proposing. The auto-processor watcher is
            // the caller that passes `true`.
            false,
            credit_gate,
        ),
    )
    .await
    {
        Ok(Ok(bases)) => bases,
        Ok(Err(e)) => return Err(WsRpcError::internal(e.to_string())),
        Err(_) => {
            log::warn!(
                "run_interpretation_with_harness timed out after {}s",
                RUN_INTERPRETATION_HARNESS_TIMEOUT_SECS
            );
            return Err(WsRpcError {
                code: 408,
                message: format!(
                    "runInterpretationWithHarness timed out after {}s",
                    RUN_INTERPRETATION_HARNESS_TIMEOUT_SECS
                ),
            });
        }
    };

    if !bases.is_empty() {
        if let Err(e) = reserve_credits(&ctx.user_email, bases.len() as f64 * DEFAULT_LINK_WRITE) {
            log::warn!(
                "Credit deduction failed for runInterpretationWithHarness (operation already committed): {}",
                e
            );
        }
    }

    Ok(serde_json::to_value(bases)?)
}

/*
   One-shot telemetry helpers.

   Separate functions rather than the `signal!` macro the watcher uses, because the two differ in
   what they can assume: the watcher has a `cfg`, a claim and a gathered batch in scope, and its
   macro reads all three. Here there is one caller-supplied id standing in for all of it.

   `processor_id` and `batch_key` are deliberately the same value. The pair means "which standing
   processor" and "which batch of its work" — a distinction a one-shot pass does not have, and
   collapsing them is more honest than minting a second id that would always be 1:1 with the first.
*/

/// A lifecycle step with no payload beyond identity.
async fn emit_one_shot_step(
    uuid: &str,
    observation_id: &str,
    did: &str,
    step: crate::perspectives::auto_processor::events::AutoProcessorStep,
) {
    use crate::perspectives::auto_processor::events::{emit, AutoProcessorEvent};
    emit(
        AutoProcessorEvent::new(uuid, observation_id, step)
            .with_agent_did(did)
            .with_batch_key(observation_id),
    )
    .await;
}

/// `Processed`, carrying what the pass wrote.
async fn emit_one_shot_bases(uuid: &str, observation_id: &str, did: &str, bases: &[String]) {
    use crate::perspectives::auto_processor::events::{
        emit, AutoProcessorEvent, AutoProcessorStep,
    };
    emit(
        AutoProcessorEvent::new(uuid, observation_id, AutoProcessorStep::Processed)
            .with_agent_did(did)
            .with_batch_key(observation_id)
            .with_bases(bases),
    )
    .await;
}

/// A perspective-scoped phase transition, so peers see the pass without seeing its payload.
async fn emit_one_shot_phase(
    uuid: &str,
    observation_id: &str,
    did: &str,
    phase: crate::perspectives::auto_processor::events::NeighbourhoodPhase,
) {
    use crate::perspectives::auto_processor::events::{
        emit_neighbourhood_state, AutoProcessorNeighbourhoodState,
    };
    emit_neighbourhood_state(AutoProcessorNeighbourhoodState::new(
        uuid,
        observation_id,
        did,
        observation_id,
        phase,
    ))
    .await;
}

/// Both halves of a failure: the reason on the owner's stream, the bare fact on everyone's.
///
/// Split that way because `detail` is where an LLM provider's error text ends up, and that can
/// name a model, an endpoint or an account. The neighbourhood stream carries no free-form field
/// at all, which is what keeps this honest — peers learn the pass ended without committing, and
/// the person who ran it learns why.
async fn emit_one_shot_abandoned(uuid: &str, observation_id: &str, did: &str, reason: &str) {
    use crate::perspectives::auto_processor::events::{
        emit, AutoProcessorEvent, AutoProcessorStep, NeighbourhoodPhase,
    };
    emit(
        AutoProcessorEvent::new(uuid, observation_id, AutoProcessorStep::Failed)
            .with_agent_did(did)
            .with_batch_key(observation_id)
            .with_detail(reason),
    )
    .await;
    emit_one_shot_phase(uuid, observation_id, did, NeighbourhoodPhase::Abandoned).await;
}

/// Register a neighbourhood auto-processor on a perspective. Writes the
/// `AutoProcessorConfig` into the shared graph; the executor watch loop reads it
/// back and starts running interpretation automatically over new source items,
/// emitting step signals on the events WebSocket (`auto-processor-event`).
///
/// **Registering is not idempotent in its class list.** Scalars overwrite through their
/// `setSingleTarget` setters, but `interpretationClasses` is a collection written with the shape's
/// `addLink` setter, so a second registration under the same `processorId` *unions* its classes
/// with what is already stored rather than replacing them. Registering `[A]` and then `[B]` leaves
/// a processor materializing both.
///
/// This endpoint is therefore for **creating** a processor. To change what an existing one
/// extracts, edit its `AutoProcessorConfig` through the model API, where a collection can be set to
/// exactly the values given; to stop it, use `perspective.removeAutoProcessor`. A client that
/// re-registers on reconnect should check whether the config already exists first — the config is
/// shared graph state that outlives any one agent's session, so "ensure it exists" is the shape
/// that wants, not "register it again".
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
    //
    // `scalar_string` treats an empty scalar as absent, so `config_from_instance` rejects a config
    // whose `processorId` is `""` — it writes, reports success, and never loads. It is also the
    // identity: the node URI, the claim's batch nodes and the processed-turn cursor are all derived
    // from it, so an empty one is not a processor that runs badly but a processor with no name.
    if body.processor_id.is_empty() {
        return Err(WsRpcError::bad_request("`processorId` must be non-empty"));
    }
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

    let emit_debug_events_write = body.emit_debug_events;
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
        max_tool_calls: body.max_tool_calls,
        emit_debug_events: emit_debug_events_write.unwrap_or(false),
    };
    write_processor(
        &mut perspective,
        &cfg,
        emit_debug_events_write,
        &agent_context,
    )
    .await
    .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(body.processor_id)?)
}

/// `perspective.removeAutoProcessor` — delete a processor's config instance, which is what stops
/// its watch: the loop reads the processor set back out of the perspective's graph on every tick.
///
/// Answers `true` when there was a processor to remove and `false` when there was not, rather than
/// erroring on the second case — a caller tidying up after a processor a peer has already removed
/// has done the right thing, and a teardown path is the worst place to raise an avoidable failure.
///
/// Deliberately validates less than its `add` counterpart, which rejects an empty `processorId`.
/// Strictness belongs on the way in: registration decides what may exist, and removal is how
/// anything already there is recovered from. A config written before that validation existed — or
/// by any writer that is not this handler — must stay removable, and refusing the id here would
/// leave junk on the graph with no API able to take it away. The id is used verbatim to derive one
/// node URI, so an empty one addresses exactly the node an empty-id write produced and nothing else.
///
/// Takes the same `update` capability as registering one. Deleting a processor is not a read: it
/// changes what the neighbourhood extracts, for everyone.
async fn remove_auto_processor_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    use crate::api::types::RemoveAutoProcessorRequest;
    use crate::perspectives::auto_processor::config::remove_processor;

    let body: RemoveAutoProcessorRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    check_capability(
        &ctx.capabilities,
        &perspective_update_capability(vec![body.uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let mut perspective = get_perspective_with_access(&body.uuid, &ctx).await?;
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    let removed = remove_processor(&mut perspective, &body.processor_id, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(removed))
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

// ── SHACL resolution endpoints ──
//
// These handlers move SHACL shape resolution from the TypeScript SDK (which paid
// N×M queryLinks round trips per shape) into the executor, where link reads hit
// the local SQLite store directly.  A perspective with 26 shapes that previously
// generated ~261 WS-RPC round trips now resolves in a single call to
// `perspective.getAllShacl`.

/// Hard cap on the number of shapes `perspective.getAllShacl` will serialise
/// in a single response. The endpoint returns the entire shape corpus in one
/// WS message with no pagination — fine for the 26-shape WE dataset that
/// motivates this PR, but a perspective with a few hundred shapes at ~10
/// properties each is a multi-MB response inside one WS frame that either
/// fails at the transport limit or (worse) silently truncates on some
/// clients. Fail loudly at this ceiling until a paginated variant lands.
/// See PR #935 review comment r3897752009.
pub(crate) const MAX_SHACL_SHAPES_PER_RESPONSE: usize = 500;

/// Helper: build a LinkQuery with only `source` and optionally `predicate` set.
pub(crate) fn shacl_link_query(source: &str, predicate: Option<&str>) -> LinkQuery {
    LinkQuery {
        source: Some(source.to_string()),
        predicate: predicate.map(|p| p.to_string()),
        ..Default::default()
    }
}

/// Simplified link triple returned by SHACL resolution endpoints.
/// Matches the `{source, predicate, target}` shape that
/// `SHACLShape.fromLinks()` in the TypeScript SDK expects.
#[derive(Debug, Serialize, Clone, PartialEq, Eq)]
pub(crate) struct ShaclLinkTriple {
    pub source: String,
    pub predicate: String,
    pub target: String,
}

/// Extract a SHACL shape name from a `has_shacl` link target.
///
/// The `has_shacl` target is a `literal:string:` URL. The TypeScript SDK
/// writes it as `literal:string:shacl://<Name>` verbatim (via
/// `Literal.fromUrl(...).toUrl()`), but the SPARQL store canonicalises
/// every `literal:string:` target through the typed-literal round-trip
/// — the payload lands as a URL-decoded `xsd:string` and is re-encoded
/// on read (`storage_term_to_target_string`). That flips the wire form
/// to `literal:string:shacl%3A%2F%2F<Name>`, so a plain
/// `strip_prefix("literal:string:shacl://")` misses every entry.
///
/// Accept both shapes so this helper survives store canonicalisation and
/// any future SDK write path that uses the un-encoded form directly.
pub(crate) fn shape_name_from_has_shacl_target(target: &str) -> Option<String> {
    let body = target.strip_prefix("literal:string:")?;
    // Fast path: raw form, no encoding.
    if let Some(name) = body.strip_prefix("shacl://") {
        return Some(name.to_string());
    }
    // Canonicalised form: percent-decode then re-check the scheme prefix.
    let decoded = percent_encoding::percent_decode_str(body)
        .decode_utf8()
        .ok()?;
    decoded
        .strip_prefix("shacl://")
        .map(|name| name.to_string())
}

/// Enumerate every SHACL shape name registered on `ad4m://self` via
/// `ad4m://has_shacl` links. Extracted so both the WS handler and the
/// in-crate integration tests exercise the exact same walk.
pub(crate) async fn resolve_shacl_names(
    perspective: &PerspectiveInstance,
) -> Result<Vec<String>, WsRpcError> {
    let query = shacl_link_query("ad4m://self", Some("ad4m://has_shacl"));
    let links = perspective
        .get_links(&query)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let mut seen = std::collections::HashSet::new();
    let names: Vec<String> = links
        .iter()
        .filter_map(|link| shape_name_from_has_shacl_target(&link.data.target))
        .filter(|name| seen.insert(name.clone()))
        .collect();
    Ok(names)
}

/// Resolve a shape name to its `sh://targetClass` URI (or `None` when the
/// name is unknown, or the shape has no target class). Returned as
/// `Option` so callers can pin the wire representation (JSON `null` vs
/// absent) at their own boundary.
pub(crate) async fn resolve_shacl_target_class(
    perspective: &PerspectiveInstance,
    name: &str,
) -> Result<Option<String>, WsRpcError> {
    let literal_url = format!("literal:string:shacl://{}", name);
    let uri_links = perspective
        .get_links(&shacl_link_query(
            &literal_url,
            Some("ad4m://shacl_shape_uri"),
        ))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let shape_uri = match uri_links.first() {
        Some(link) => link.data.target.clone(),
        None => return Ok(None),
    };

    let shape_links = perspective
        .get_links(&shacl_link_query(&shape_uri, None))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let target_class = shape_links
        .iter()
        .find(|l| l.data.predicate.as_deref() == Some("sh://targetClass"))
        .map(|l| l.data.target.clone());

    Ok(target_class)
}

/// List the names of every SHACL shape stored in a perspective.
/// Equivalent to the SDK's `PerspectiveProxy.getShaclNames()` but resolved
/// in-process — one handler call replaces one `queryLinks` round trip, plus
/// deduplication happens server-side.
async fn get_shacl_names(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let names = resolve_shacl_names(&perspective).await?;
    Ok(serde_json::to_value(names)?)
}

/// Resolve a shape's `sh:targetClass` by name without fetching its properties.
/// Equivalent to the SDK's `PerspectiveProxy.getShaclTargetClass(name)` — two
/// in-process link reads instead of two WS-RPC round trips.
async fn get_shacl_target_class(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    let name = params.require_str("name")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let target_class = resolve_shacl_target_class(&perspective, &name).await?;
    // `null` when the shape (or its `sh://targetClass`) is absent — pins
    // the wire contract the TS client already relies on
    // (`PerspectiveProxy.getShaclTargetClass` maps this null to
    // `undefined` at the proxy boundary).
    Ok(serde_json::to_value(target_class)?)
}

/// Collect all link triples that define a shape and its property sub-shapes.
/// Used by both `get_shacl` and `get_all_shacl` to avoid duplicating the
/// multi-step resolution logic.
pub(crate) async fn resolve_shacl_links(
    perspective: &PerspectiveInstance,
    name: &str,
) -> Result<Option<(String, Vec<ShaclLinkTriple>)>, WsRpcError> {
    // Step 1: name → shapeUri
    let literal_url = format!("literal:string:shacl://{}", name);
    let uri_links = perspective
        .get_links(&shacl_link_query(
            &literal_url,
            Some("ad4m://shacl_shape_uri"),
        ))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let shape_uri = match uri_links.first() {
        Some(link) => link.data.target.clone(),
        None => {
            // Diagnostic for the race Nico flagged in review r3897752007:
            // the caller enumerated this name from `has_shacl` but the
            // `shacl_shape_uri` edge is gone by the time we resolve it
            // (concurrent unlink between the two reads). Debug-level:
            // expected-in-race behaviour, not a bug — the caller's
            // returned list simply omits the vanished shape.
            log::debug!(
                "🔎 🔗 shacl: name={name} resolved to no shape uri, skipping (concurrent unlink?)"
            );
            return Ok(None);
        }
    };

    // Step 2: get all links from the shape node (targetClass, properties, etc.)
    let shape_links = perspective
        .get_links(&shacl_link_query(&shape_uri, None))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    // Step 3: find property shape URIs
    let prop_uris: Vec<String> = shape_links
        .iter()
        .filter(|l| l.data.predicate.as_deref() == Some("sh://property"))
        .map(|l| l.data.target.clone())
        .collect();

    // Step 4: fetch links from each property sub-shape. These reads are
    // independent, so we drive them concurrently with `try_join_all` (the
    // old TypeScript client used `Promise.all` for the same reason; the
    // first cut of this handler regressed to sequential `.await`s and
    // paid one round trip per property in serial). See PR #935 review
    // (r3897752002).
    //
    // The queries are materialised into an owned Vec first so each future
    // borrows a value that outlives the join; borrowing a temporary
    // built inside `.map(|_| ...)` would fail E0515.
    let prop_queries: Vec<LinkQuery> = prop_uris
        .iter()
        .map(|prop_uri| shacl_link_query(prop_uri, None))
        .collect();
    let all_prop_links: Vec<DecoratedLinkExpression> =
        futures::future::try_join_all(prop_queries.iter().map(|q| perspective.get_links(q)))
            .await
            .map_err(|e| WsRpcError::internal(e.to_string()))?
            .into_iter()
            .flatten()
            .collect();

    // Step 5: merge shape links + property links, deduplicate
    let mut seen = std::collections::HashSet::new();
    let mut triples = Vec::new();
    for link in shape_links.iter().chain(all_prop_links.iter()) {
        let source = link.data.source.clone();
        let predicate = link.data.predicate.clone().unwrap_or_default();
        let target = link.data.target.clone();
        let key = format!("{} {} {}", source, predicate, target);
        if seen.insert(key) {
            triples.push(ShaclLinkTriple {
                source,
                predicate,
                target,
            });
        }
    }

    Ok(Some((shape_uri, triples)))
}

/// Retrieve a single SHACL shape by name.  Returns the shape URI and all link
/// triples needed to reconstruct the shape via `SHACLShape.fromLinks()`.
/// Equivalent to the SDK's `PerspectiveProxy.getShacl(name)` — one handler
/// call replaces 3+N `queryLinks` round trips.
async fn get_shacl(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    let name = params.require_str("name")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    match resolve_shacl_links(&perspective, &name).await? {
        Some((shape_uri, links)) => {
            Ok(serde_json::json!({ "shapeUri": shape_uri, "links": links }))
        }
        None => Ok(Value::Null),
    }
}

/// Retrieve all SHACL shapes in one call.  Returns an array of
/// `{name, shapeUri, links}` objects — the client reconstructs each shape
/// with `SHACLShape.fromLinks(entry.links, entry.shapeUri)`.  Equivalent to
/// the SDK's `PerspectiveProxy.getAllShacl()` — one handler call replaces
/// 1 + N×(3+M) `queryLinks` round trips (N shapes, M properties each).
async fn get_all_shacl(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    check_capability(
        &ctx.capabilities,
        &perspective_query_capability(vec![uuid.clone()]),
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let perspective = get_perspective_with_access(&uuid, &ctx).await?;

    // Step 1: get all shape names
    let query = shacl_link_query("ad4m://self", Some("ad4m://has_shacl"));
    let name_links = perspective
        .get_links(&query)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let mut seen_names = std::collections::HashSet::new();
    let names: Vec<String> = name_links
        .iter()
        .filter_map(|link| shape_name_from_has_shacl_target(&link.data.target))
        .filter(|name| seen_names.insert(name.clone()))
        .collect();

    // Step 2: resolve each shape's full link set. Concurrent per-shape
    // walk — each `resolve_shacl_links` call is independent, so we let
    // them race and preserve original name order after joining. Recovers
    // the concurrency the old TS client had via `Promise.all` (review
    // r3897752002).
    // Cheap fail-loud gate before the fan-out: if the name enumeration
    // already exceeds the response cap there's no point resolving every
    // shape only to reject the serialisation. Post-walk check below
    // catches the race where extra names arrive between enumeration and
    // response (e.g. via subscription).
    if names.len() > MAX_SHACL_SHAPES_PER_RESPONSE {
        return Err(WsRpcError::bad_request(format!(
            "getAllShacl exceeds MAX_SHACL_SHAPES_PER_RESPONSE ({} > {}), use paginated variant (TODO: not yet implemented)",
            names.len(),
            MAX_SHACL_SHAPES_PER_RESPONSE,
        )));
    }

    let resolved = futures::future::try_join_all(
        names
            .iter()
            .map(|name| resolve_shacl_links(&perspective, name)),
    )
    .await?;

    let mut results: Vec<Value> = Vec::with_capacity(names.len());
    for (name, maybe) in names.iter().zip(resolved.into_iter()) {
        match maybe {
            Some((shape_uri, links)) => results.push(serde_json::json!({
                "name": name,
                "shapeUri": shape_uri,
                "links": links,
            })),
            None => {
                // `resolve_shacl_links` already logged the specific
                // per-name drop at debug level; nothing to add here —
                // this arm is just the filter that keeps the bulk
                // response array clean.
            }
        }
    }

    if results.len() > MAX_SHACL_SHAPES_PER_RESPONSE {
        return Err(WsRpcError::bad_request(format!(
            "getAllShacl exceeds MAX_SHACL_SHAPES_PER_RESPONSE ({} > {}), use paginated variant (TODO: not yet implemented)",
            results.len(),
            MAX_SHACL_SHAPES_PER_RESPONSE,
        )));
    }

    Ok(Value::Array(results))
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
    map.register("perspective.subjectClassesOf", subject_classes_of_handler);
    map.register("perspective.modelSubscribe", model_subscribe_handler);
    map.register("perspective.evaluateGetters", evaluate_getters_handler);
    map.register("perspective.runInterpretation", run_interpretation_handler);
    map.register(
        "perspective.runInterpretationWithHarness",
        run_interpretation_with_harness_handler,
    );
    map.register("perspective.addAutoProcessor", add_auto_processor_handler);
    map.register(
        "perspective.removeAutoProcessor",
        remove_auto_processor_handler,
    );
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
    map.register("perspective.getShaclNames", get_shacl_names);
    map.register("perspective.getShaclTargetClass", get_shacl_target_class);
    map.register("perspective.getShacl", get_shacl);
    map.register("perspective.getAllShacl", get_all_shacl);
}

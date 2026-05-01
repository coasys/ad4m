//! WebSocket RPC endpoint: GET /api/v1/ws
//!
//! Single WebSocket endpoint that replaces all REST HTTP calls with JSON-RPC
//! messages. Auth happens once on connection upgrade. Events (subscriptions)
//! flow on the same connection.
//!
//! **Request:**  `{ "id": "<correlation-id>", "type": "<operation>", ...params }`
//! **Response:** `{ "id": "<correlation-id>", "result": ... }` or
//!               `{ "id": "<correlation-id>", "error": { "code": N, "message": "..." } }`
//! **Events:**   `{ "type": "<event-type>", ...payload }` (no `id`)
//!
//! ## Implementation
//! Instead of duplicating handler logic, this endpoint maps WS message types
//! to internal HTTP requests and routes them through the existing axum handlers.
//! This means zero handler modifications — the WS layer is a pure transport proxy.

use axum::{
    body::Body,
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        Query, State,
    },
    http::{self, Request},
    response::IntoResponse,
    Router,
};
use futures::stream::StreamExt;
use serde::Deserialize;
use serde_json::{json, Value};
use std::sync::Arc;
use tokio::sync::mpsc;

use crate::agent::capabilities::*;

use super::auth::AppState;

// ── Auth query param ────────────────────────────────────────────────────────
#[derive(Deserialize, Default)]
pub struct WsAuthQuery {
    token: Option<String>,
}

// ── Message type → HTTP route mapping ───────────────────────────────────────

/// Map a WS message type to its HTTP route.
/// Returns (method, path, query_string_suffix, body_json).
fn map_message_to_route(msg_type: &str, params: &Value) -> Option<(&'static str, String, Value)> {
    // Helper to extract a string param
    let s = |key: &str| -> String {
        params
            .get(key)
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string()
    };

    // Helper to build query string from params for GET requests
    let query_string = |keys: &[&str]| -> String {
        let mut parts = Vec::new();
        for &key in keys {
            if let Some(val) = params.get(key) {
                if let Some(s) = val.as_str() {
                    if !s.is_empty() {
                        parts.push(format!("{}={}", key, urlencoding::encode(s)));
                    }
                } else if let Some(n) = val.as_i64() {
                    parts.push(format!("{}={}", key, n));
                } else if let Some(b) = val.as_bool() {
                    parts.push(format!("{}={}", key, b));
                }
            }
        }
        if parts.is_empty() {
            String::new()
        } else {
            format!("?{}", parts.join("&"))
        }
    };

    // Clone params for body, removing path/query params
    let body_without = |exclude: &[&str]| -> Value {
        if let Some(obj) = params.as_object() {
            let mut m = obj.clone();
            m.remove("id");
            m.remove("type");
            for &key in exclude {
                m.remove(key);
            }
            Value::Object(m)
        } else {
            json!({})
        }
    };

    let (method, path, body) = match msg_type {
        // ── Agent ──
        "agent.get" => ("GET", "/api/v1/agent".to_string(), json!(null)),
        "agent.status" => ("GET", "/api/v1/agent/status".to_string(), json!(null)),
        "agent.generate" => (
            "POST",
            "/api/v1/agent/generate".to_string(),
            body_without(&[]),
        ),
        "agent.lock" => ("POST", "/api/v1/agent/lock".to_string(), body_without(&[])),
        "agent.unlock" => (
            "POST",
            "/api/v1/agent/unlock".to_string(),
            body_without(&[]),
        ),
        "agent.import" => (
            "POST",
            "/api/v1/agent/import".to_string(),
            body_without(&[]),
        ),
        "agent.byDid" => (
            "GET",
            format!("/api/v1/agent/by-did/{}", urlencoding::encode(&s("did"))),
            json!(null),
        ),
        "agent.updateProfile" => (
            "PATCH",
            "/api/v1/agent/profile".to_string(),
            body_without(&[]),
        ),
        "agent.sign" => ("POST", "/api/v1/agent/sign".to_string(), body_without(&[])),
        "agent.isLocked" => ("GET", "/api/v1/agent/is-locked".to_string(), json!(null)),
        "agent.requestCapability" => (
            "POST",
            "/api/v1/agent/auth/request".to_string(),
            body_without(&[]),
        ),
        "agent.permitCapability" => (
            "POST",
            "/api/v1/agent/auth/permit".to_string(),
            body_without(&[]),
        ),
        "agent.generateJwt" => (
            "POST",
            "/api/v1/agent/auth/jwt".to_string(),
            body_without(&[]),
        ),
        "agent.getApps" => ("GET", "/api/v1/agent/apps".to_string(), json!(null)),
        "agent.removeApp" => (
            "DELETE",
            format!("/api/v1/agent/apps/{}", urlencoding::encode(&s("id"))),
            json!(null),
        ),
        "agent.revokeToken" => (
            "DELETE",
            format!(
                "/api/v1/agent/auth/token/{}",
                urlencoding::encode(&s("token"))
            ),
            json!(null),
        ),
        "agent.getTrustedAgents" => ("GET", "/api/v1/agent/trusted".to_string(), json!(null)),
        "agent.addTrustedAgents" => (
            "PUT",
            "/api/v1/agent/trusted".to_string(),
            body_without(&[]),
        ),
        "agent.deleteTrustedAgents" => (
            "DELETE",
            "/api/v1/agent/trusted".to_string(),
            body_without(&[]),
        ),
        "agent.getEntanglementProofs" => {
            ("GET", "/api/v1/agent/entanglement".to_string(), json!(null))
        }
        "agent.addEntanglementProofs" => (
            "POST",
            "/api/v1/agent/entanglement".to_string(),
            body_without(&[]),
        ),
        "agent.deleteEntanglementProofs" => (
            "DELETE",
            "/api/v1/agent/entanglement".to_string(),
            body_without(&[]),
        ),
        "agent.entanglementProofPreflight" => (
            "POST",
            "/api/v1/agent/entanglement-preflight".to_string(),
            body_without(&[]),
        ),

        // ── Perspectives ──
        "perspective.all" => ("GET", "/api/v1/perspectives".to_string(), json!(null)),
        "perspective.get" => (
            "GET",
            format!("/api/v1/perspectives/{}", s("uuid")),
            json!(null),
        ),
        "perspective.create" => (
            "POST",
            "/api/v1/perspectives".to_string(),
            body_without(&[]),
        ),
        "perspective.update" => (
            "PUT",
            format!("/api/v1/perspectives/{}", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.remove" => (
            "DELETE",
            format!("/api/v1/perspectives/{}", s("uuid")),
            json!(null),
        ),
        "perspective.snapshot" => (
            "GET",
            format!("/api/v1/perspectives/{}/snapshot", s("uuid")),
            json!(null),
        ),
        "perspective.publishSnapshot" => (
            "POST",
            format!("/api/v1/perspectives/{}/publish-snapshot", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.queryLinks" => {
            let uuid = s("uuid");
            let qs = query_string(&[
                "source",
                "predicate",
                "target",
                "fromDate",
                "untilDate",
                "limit",
            ]);
            (
                "GET",
                format!("/api/v1/perspectives/{}/links{}", uuid, qs),
                json!(null),
            )
        }
        "perspective.addLink" => (
            "POST",
            format!("/api/v1/perspectives/{}/links", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.addLinkExpression" => (
            "POST",
            format!("/api/v1/perspectives/{}/links/expression", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.addLinks" => (
            "POST",
            format!("/api/v1/perspectives/{}/links/bulk", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.updateLink" => (
            "PUT",
            format!("/api/v1/perspectives/{}/links", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.removeLink" => (
            "DELETE",
            format!("/api/v1/perspectives/{}/links", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.removeLinks" => (
            "POST",
            format!("/api/v1/perspectives/{}/links/remove-bulk", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.linkMutations" => (
            "POST",
            format!("/api/v1/perspectives/{}/links/mutations", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.queryProlog" => (
            "POST",
            format!("/api/v1/perspectives/{}/query", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.querySparql" => (
            "POST",
            format!("/api/v1/perspectives/{}/query/surreal", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.addSdna" => (
            "POST",
            format!("/api/v1/perspectives/{}/sdna", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.executeCommands" => (
            "POST",
            format!("/api/v1/perspectives/{}/execute-commands", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.createSubject" => (
            "POST",
            format!("/api/v1/perspectives/{}/create-subject", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.getSubjectData" => (
            "POST",
            format!("/api/v1/perspectives/{}/get-subject-data", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.createBatch" => (
            "POST",
            format!("/api/v1/perspectives/{}/batch", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.commitBatch" => (
            "POST",
            format!("/api/v1/perspectives/{}/batch/commit", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.subscribeQuery" => (
            "POST",
            format!("/api/v1/perspectives/{}/subscribe-query", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.keepAliveQuery" => (
            "POST",
            format!("/api/v1/perspectives/{}/keep-alive-query", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.disposeQuery" => (
            "POST",
            format!("/api/v1/perspectives/{}/dispose-query", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.subscribeSparql" => (
            "POST",
            format!("/api/v1/perspectives/{}/subscribe-surreal-query", s("uuid")),
            body_without(&["uuid"]),
        ),
        "perspective.keepAliveSparql" => (
            "POST",
            format!(
                "/api/v1/perspectives/{}/keep-alive-surreal-query",
                s("uuid")
            ),
            body_without(&["uuid"]),
        ),
        "perspective.disposeSparql" => (
            "POST",
            format!("/api/v1/perspectives/{}/dispose-surreal-query", s("uuid")),
            body_without(&["uuid"]),
        ),

        // ── Languages ──
        "language.all" => {
            let qs = query_string(&["filter"]);
            ("GET", format!("/api/v1/languages{}", qs), json!(null))
        }
        "language.get" => (
            "GET",
            format!("/api/v1/languages/{}", urlencoding::encode(&s("address"))),
            json!(null),
        ),
        "language.meta" => (
            "GET",
            format!(
                "/api/v1/languages/{}/meta",
                urlencoding::encode(&s("address"))
            ),
            json!(null),
        ),
        "language.source" => (
            "GET",
            format!(
                "/api/v1/languages/{}/source",
                urlencoding::encode(&s("address"))
            ),
            json!(null),
        ),
        "language.writeSettings" => (
            "PUT",
            format!(
                "/api/v1/languages/{}/settings",
                urlencoding::encode(&s("address"))
            ),
            body_without(&["address"]),
        ),
        "language.applyTemplate" => (
            "POST",
            "/api/v1/languages/apply-template".to_string(),
            body_without(&[]),
        ),
        "language.publish" => (
            "POST",
            "/api/v1/languages/publish".to_string(),
            body_without(&[]),
        ),
        "language.remove" => (
            "DELETE",
            format!("/api/v1/languages/{}", urlencoding::encode(&s("address"))),
            json!(null),
        ),

        // ── Neighbourhoods ──
        "neighbourhood.publish" => (
            "POST",
            "/api/v1/neighbourhoods/publish".to_string(),
            body_without(&[]),
        ),
        "neighbourhood.join" => (
            "POST",
            "/api/v1/neighbourhoods/join".to_string(),
            body_without(&[]),
        ),
        "neighbourhood.otherAgents" => (
            "GET",
            format!("/api/v1/neighbourhoods/{}/other-agents", s("uuid")),
            json!(null),
        ),
        "neighbourhood.hasTelepresence" => (
            "GET",
            format!("/api/v1/neighbourhoods/{}/has-telepresence", s("uuid")),
            json!(null),
        ),
        "neighbourhood.onlineAgents" => (
            "GET",
            format!("/api/v1/neighbourhoods/{}/online-agents", s("uuid")),
            json!(null),
        ),
        "neighbourhood.setOnlineStatus" => (
            "PUT",
            format!("/api/v1/neighbourhoods/{}/online-status", s("uuid")),
            body_without(&["uuid"]),
        ),
        "neighbourhood.sendSignal" => (
            "POST",
            format!("/api/v1/neighbourhoods/{}/signal", s("uuid")),
            body_without(&["uuid"]),
        ),
        "neighbourhood.sendBroadcast" => (
            "POST",
            format!("/api/v1/neighbourhoods/{}/broadcast", s("uuid")),
            body_without(&["uuid"]),
        ),

        // ── Expressions ──
        "expression.get" => {
            let url = s("url");
            let raw = params.get("raw").and_then(|v| v.as_bool()).unwrap_or(false);
            let qs = if raw { "?raw=true" } else { "" };
            (
                "GET",
                format!("/api/v1/expressions/{}{}", urlencoding::encode(&url), qs),
                json!(null),
            )
        }
        "expression.getMany" => (
            "POST",
            "/api/v1/expressions/many".to_string(),
            body_without(&[]),
        ),
        "expression.create" => ("POST", "/api/v1/expressions".to_string(), body_without(&[])),
        "expression.interactions" => (
            "GET",
            format!(
                "/api/v1/expressions/{}/interactions",
                urlencoding::encode(&s("url"))
            ),
            json!(null),
        ),
        "expression.interact" => (
            "POST",
            format!(
                "/api/v1/expressions/{}/interact",
                urlencoding::encode(&s("url"))
            ),
            body_without(&["url"]),
        ),

        // ── Runtime ──
        "runtime.info" => ("GET", "/api/v1/runtime/info".to_string(), json!(null)),
        "runtime.quit" => ("POST", "/api/v1/runtime/quit".to_string(), json!(null)),
        "runtime.openLink" => (
            "POST",
            "/api/v1/runtime/open-link".to_string(),
            body_without(&[]),
        ),
        "runtime.friends" => ("GET", "/api/v1/runtime/friends".to_string(), json!(null)),
        "runtime.addFriends" => (
            "PUT",
            "/api/v1/runtime/friends".to_string(),
            body_without(&[]),
        ),
        "runtime.removeFriends" => (
            "DELETE",
            "/api/v1/runtime/friends".to_string(),
            body_without(&[]),
        ),
        "runtime.friendStatus" => (
            "GET",
            format!("/api/v1/runtime/friends/{}", urlencoding::encode(&s("did"))),
            json!(null),
        ),
        "runtime.sendFriendMessage" => (
            "POST",
            format!(
                "/api/v1/runtime/friends/{}/message",
                urlencoding::encode(&s("did"))
            ),
            body_without(&["did"]),
        ),
        "runtime.inbox" => (
            "GET",
            "/api/v1/runtime/messages/inbox".to_string(),
            json!(null),
        ),
        "runtime.outbox" => (
            "GET",
            "/api/v1/runtime/messages/outbox".to_string(),
            json!(null),
        ),
        "runtime.notifications" => (
            "GET",
            "/api/v1/runtime/notifications".to_string(),
            json!(null),
        ),
        "runtime.createNotification" => (
            "POST",
            "/api/v1/runtime/notifications".to_string(),
            body_without(&[]),
        ),
        "runtime.updateNotification" => (
            "PATCH",
            format!("/api/v1/runtime/notifications/{}", s("id")),
            body_without(&["id"]),
        ),
        "runtime.grantNotification" => (
            "PATCH",
            format!("/api/v1/runtime/notifications/{}/grant", s("id")),
            body_without(&["id"]),
        ),
        "runtime.deleteNotification" => (
            "DELETE",
            format!("/api/v1/runtime/notifications/{}", s("id")),
            json!(null),
        ),
        "runtime.setStatus" => (
            "PUT",
            "/api/v1/runtime/status".to_string(),
            body_without(&[]),
        ),
        "runtime.linkLanguageTemplates" => (
            "GET",
            "/api/v1/runtime/link-language-templates".to_string(),
            json!(null),
        ),
        "runtime.addLinkLanguageTemplates" => (
            "PUT",
            "/api/v1/runtime/link-language-templates".to_string(),
            body_without(&[]),
        ),
        "runtime.removeLinkLanguageTemplates" => (
            "DELETE",
            "/api/v1/runtime/link-language-templates".to_string(),
            body_without(&[]),
        ),
        "runtime.hcAgentInfos" => (
            "GET",
            "/api/v1/runtime/hc/agent-infos".to_string(),
            json!(null),
        ),
        "runtime.addHcAgentInfos" => (
            "POST",
            "/api/v1/runtime/hc/agent-infos".to_string(),
            body_without(&[]),
        ),
        "runtime.networkMetrics" => (
            "GET",
            "/api/v1/runtime/network-metrics".to_string(),
            json!(null),
        ),
        "runtime.restartHolochain" => (
            "POST",
            "/api/v1/runtime/holochain/restart".to_string(),
            json!(null),
        ),
        "runtime.verifySignature" => (
            "POST",
            "/api/v1/runtime/verify-signature".to_string(),
            body_without(&[]),
        ),
        "runtime.tlsDomain" => ("GET", "/api/v1/runtime/tls-domain".to_string(), json!(null)),
        "runtime.exportData" => (
            "POST",
            "/api/v1/runtime/export".to_string(),
            body_without(&[]),
        ),
        "runtime.importData" => (
            "POST",
            "/api/v1/runtime/import".to_string(),
            body_without(&[]),
        ),
        "runtime.freeHostingEnabled" => (
            "GET",
            "/api/v1/runtime/free-hosting-enabled".to_string(),
            json!(null),
        ),
        "runtime.setFreeHostingEnabled" => (
            "PUT",
            "/api/v1/runtime/free-hosting-enabled".to_string(),
            body_without(&[]),
        ),
        "runtime.computeLog" => (
            "GET",
            "/api/v1/runtime/compute-log".to_string(),
            json!(null),
        ),
        "runtime.hostRates" => ("GET", "/api/v1/runtime/host-rates".to_string(), json!(null)),
        "runtime.setHostRates" => (
            "PUT",
            "/api/v1/runtime/host-rates".to_string(),
            body_without(&[]),
        ),

        // ── Runtime / Unyt ──
        "runtime.unytAgentKey" => (
            "GET",
            "/api/v1/runtime/unyt/agent-key".to_string(),
            json!(null),
        ),
        "runtime.unytSendHot" => (
            "POST",
            "/api/v1/runtime/unyt/send-hot".to_string(),
            body_without(&[]),
        ),
        "runtime.unytWalletBalance" => (
            "GET",
            "/api/v1/runtime/unyt/wallet-balance".to_string(),
            json!(null),
        ),
        "runtime.unytWalletHistory" => (
            "GET",
            "/api/v1/runtime/unyt/wallet-history".to_string(),
            json!(null),
        ),
        "runtime.unytVersionInfo" => (
            "GET",
            "/api/v1/runtime/unyt/version-info".to_string(),
            json!(null),
        ),
        "runtime.unytHotAgentPubkey" => (
            "GET",
            "/api/v1/runtime/unyt/hot-agent-pubkey".to_string(),
            json!(null),
        ),
        "runtime.unytMembraneProof" => (
            "POST",
            "/api/v1/runtime/unyt/membrane-proof".to_string(),
            body_without(&[]),
        ),
        "runtime.unytReinstallDna" => (
            "POST",
            "/api/v1/runtime/unyt/reinstall-dna".to_string(),
            body_without(&[]),
        ),

        // ── AI ──
        "ai.models" => ("GET", "/api/v1/ai/models".to_string(), json!(null)),
        "ai.addModel" => ("POST", "/api/v1/ai/models".to_string(), body_without(&[])),
        "ai.updateModel" => (
            "PUT",
            format!("/api/v1/ai/models/{}", s("id")),
            body_without(&["id"]),
        ),
        "ai.removeModel" => (
            "DELETE",
            format!("/api/v1/ai/models/{}", s("id")),
            json!(null),
        ),
        "ai.setDefaultModel" => (
            "PUT",
            format!("/api/v1/ai/models/{}/default", s("id")),
            body_without(&["id"]),
        ),
        "ai.getDefaultModel" => ("GET", "/api/v1/ai/models/default".to_string(), json!(null)),
        "ai.tasks" => ("GET", "/api/v1/ai/tasks".to_string(), json!(null)),
        "ai.addTask" => ("POST", "/api/v1/ai/tasks".to_string(), body_without(&[])),
        "ai.updateTask" => (
            "PUT",
            format!("/api/v1/ai/tasks/{}", s("id")),
            body_without(&["id"]),
        ),
        "ai.removeTask" => (
            "DELETE",
            format!("/api/v1/ai/tasks/{}", s("id")),
            json!(null),
        ),
        "ai.prompt" => ("POST", "/api/v1/ai/prompt".to_string(), body_without(&[])),
        "ai.embed" => ("POST", "/api/v1/ai/embed".to_string(), body_without(&[])),
        "ai.modelLoadingStatus" => {
            let qs = query_string(&["modelId"]);
            (
                "GET",
                format!("/api/v1/ai/model-loading-status{}", qs),
                json!(null),
            )
        }
        "ai.transcriptionOpen" => (
            "POST",
            "/api/v1/ai/transcription/open".to_string(),
            body_without(&[]),
        ),
        "ai.transcriptionClose" => (
            "POST",
            "/api/v1/ai/transcription/close".to_string(),
            body_without(&[]),
        ),

        // ── Users ──
        "user.create" => ("POST", "/api/v1/users".to_string(), body_without(&[])),
        "user.login" => ("POST", "/api/v1/users/login".to_string(), body_without(&[])),
        "user.verifyEmail" => (
            "POST",
            "/api/v1/users/verify-email".to_string(),
            body_without(&[]),
        ),
        "user.list" => ("GET", "/api/v1/users".to_string(), json!(null)),
        "user.multiUserEnabled" => (
            "GET",
            "/api/v1/users/multi-user-enabled".to_string(),
            json!(null),
        ),
        "user.setMultiUserEnabled" => (
            "PUT",
            "/api/v1/users/multi-user-enabled".to_string(),
            body_without(&[]),
        ),
        "user.freeAccess" => (
            "POST",
            "/api/v1/users/free-access".to_string(),
            body_without(&[]),
        ),
        "user.credits" => ("GET", "/api/v1/users/credits".to_string(), json!(null)),
        "user.wallet" => (
            "GET",
            format!("/api/v1/users/{}/wallet", urlencoding::encode(&s("email"))),
            json!(null),
        ),
        "user.emailTest" => (
            "POST",
            "/api/v1/dev/email-test".to_string(),
            body_without(&[]),
        ),

        // ── Hosting ──
        "hosting.info" => ("GET", "/api/v1/hosting".to_string(), json!(null)),
        "hosting.wallet" => ("GET", "/api/v1/hosting/wallet".to_string(), json!(null)),
        "hosting.walletHistory" => (
            "GET",
            "/api/v1/hosting/wallet-history".to_string(),
            json!(null),
        ),
        "hosting.requestPayment" => (
            "POST",
            "/api/v1/hosting/request-payment".to_string(),
            body_without(&[]),
        ),
        "hosting.setHotWallet" => (
            "PUT",
            "/api/v1/hosting/wallet".to_string(),
            body_without(&[]),
        ),

        _ => return None,
    };

    Some((method, path, body))
}

// ── Entry point ─────────────────────────────────────────────────────────────
pub async fn ws_rpc(
    ws: WebSocketUpgrade,
    State(state): State<AppState>,
    Query(query): Query<WsAuthQuery>,
) -> impl IntoResponse {
    let token = query.token.unwrap_or_default();

    ws.on_upgrade(move |socket| handle_ws_rpc(socket, state, token))
}

// ── Connection handler ──────────────────────────────────────────────────────
async fn handle_ws_rpc(socket: WebSocket, state: AppState, token: String) {
    // Track last_seen once
    track_last_seen_from_token(token.clone()).await;

    let (mut ws_sink, mut ws_stream) = socket.split();
    let (tx, mut rx) = mpsc::unbounded_channel::<String>();

    // Build the internal router for proxying RPC → HTTP handlers
    let router = super::rest_router(state.clone());

    // Event broadcast task (reuse events_ws functionality)
    let token_for_events = token.clone();
    let tx_events = tx.clone();
    tokio::spawn(async move {
        let user_email = user_email_from_token(token_for_events.clone());
        let event_stream = super::events_ws::build_event_stream(token_for_events, user_email).await;
        tokio::pin!(event_stream);
        while let Some(msg) = event_stream.next().await {
            if tx_events.send(msg).is_err() {
                break;
            }
        }
    });

    // Writer task
    let write_handle = tokio::spawn(async move {
        use futures::SinkExt;
        while let Some(msg) = rx.recv().await {
            if ws_sink.send(Message::Text(msg.into())).await.is_err() {
                break;
            }
        }
    });

    let router = Arc::new(router);

    // Reader loop — dispatch RPC messages
    while let Some(Ok(msg)) = ws_stream.next().await {
        let text = match &msg {
            Message::Text(t) => t.to_string(),
            Message::Close(_) => break,
            Message::Ping(_) | Message::Pong(_) => continue,
            _ => continue,
        };

        // Parse JSON
        let parsed: Value = match serde_json::from_str(&text) {
            Ok(v) => v,
            Err(_) => {
                let _ = tx.send(json!({"error":{"code":400,"message":"Invalid JSON"}}).to_string());
                continue;
            }
        };

        // Handle ping
        if parsed.get("type").and_then(|v| v.as_str()) == Some("ping") {
            let _ = tx.send(json!({"type":"pong"}).to_string());
            continue;
        }

        // Extract id and type
        let id = parsed
            .get("id")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let msg_type = match parsed.get("type").and_then(|v| v.as_str()) {
            Some(t) => t.to_string(),
            None => {
                let _ = tx.send(
                    json!({"id": id, "error":{"code":400,"message":"Missing 'type' field"}})
                        .to_string(),
                );
                continue;
            }
        };

        // Map to HTTP route
        // Client sends params under a "params" sub-key to avoid collision
        // with protocol fields "id" and "type".
        let rpc_params = parsed.get("params").cloned().unwrap_or(json!({}));
        let route_info = match map_message_to_route(&msg_type, &rpc_params) {
            Some(info) => info,
            None => {
                let _ = tx.send(
                    json!({"id": id, "error":{"code":404,"message":format!("Unknown type: {}", msg_type)}})
                        .to_string(),
                );
                continue;
            }
        };

        let token_clone = token.clone();
        let tx_clone = tx.clone();
        let router_clone = router.clone();
        let id_clone = id.clone();

        tokio::spawn(async move {
            let (method, path, body) = route_info;
            let result = proxy_to_http(&router_clone, method, &path, &body, &token_clone).await;
            let response = match result {
                Ok(val) => json!({"id": id_clone, "result": val}),
                Err((code, message)) => {
                    json!({"id": id_clone, "error": {"code": code, "message": message}})
                }
            };
            let _ = tx_clone.send(response.to_string());
        });
    }

    drop(tx);
    let _ = write_handle.await;
}

/// Proxy a WS RPC call through the internal axum router.
/// Constructs an HTTP request, routes it, and extracts the JSON response.
async fn proxy_to_http(
    router: &Router,
    method: &str,
    path: &str,
    body: &Value,
    token: &str,
) -> Result<Value, (u16, String)> {
    let http_method = match method {
        "GET" => http::Method::GET,
        "POST" => http::Method::POST,
        "PUT" => http::Method::PUT,
        "PATCH" => http::Method::PATCH,
        "DELETE" => http::Method::DELETE,
        _ => return Err((400, format!("Invalid method: {}", method))),
    };

    let body_bytes = if body.is_null() {
        Body::empty()
    } else {
        Body::from(serde_json::to_vec(body).unwrap_or_default())
    };

    let request = Request::builder()
        .method(http_method)
        .uri(path)
        .header("content-type", "application/json")
        .header("authorization", format!("Bearer {}", token))
        .body(body_bytes)
        .map_err(|e| (500u16, format!("Failed to build request: {}", e)))?;

    // Router<()> implements tower_service::Service<Request<Body>>
    let response = super::dispatch_request(router, request).await;

    let status = response.status();
    let body_bytes = axum::body::to_bytes(response.into_body(), 10 * 1024 * 1024)
        .await
        .map_err(|e| (500u16, format!("Failed to read response body: {}", e)))?;

    if status.is_success() {
        // Try to parse as JSON, fall back to string
        let body_str = String::from_utf8_lossy(&body_bytes);
        match serde_json::from_str::<Value>(&body_str) {
            Ok(val) => Ok(val),
            Err(_) => {
                // Return as raw string value
                Ok(Value::String(body_str.into_owned()))
            }
        }
    } else {
        let code = status.as_u16();
        let body_str = String::from_utf8_lossy(&body_bytes);
        // Try to extract error message from JSON body
        let message = serde_json::from_str::<Value>(&body_str)
            .ok()
            .and_then(|v| {
                v.get("message")
                    .and_then(|m| m.as_str())
                    .map(|s| s.to_string())
            })
            .unwrap_or_else(|| body_str.into_owned());
        Err((code, message))
    }
}

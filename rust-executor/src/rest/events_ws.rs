//! WebSocket event endpoint: GET /api/v1/ws/events
//!
//! Single WebSocket endpoint serving ALL event types as an alternative to SSE.
//! Each message is a JSON object `{ "type": "<event-type>", ...payload }`.
//! Events are filtered per-user in multi-user mode, identical to the SSE
//! endpoint.
//!
//! Client → Server messages:
//!   `{"type":"ping"}` → server responds with `{"type":"pong"}`
//!   Other messages are silently ignored (future extensibility).

use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        Query, State,
    },
    response::IntoResponse,
};
use futures::stream::StreamExt;
use serde::Deserialize;
use std::pin::Pin;

use crate::agent::capabilities::*;
use crate::agent::{did_for_context, AgentContext};
use crate::pubsub::{
    get_global_pubsub, AGENT_STATUS_CHANGED_TOPIC, AGENT_UPDATED_TOPIC, AI_MODEL_LOADING_STATUS,
    AI_TRANSCRIPTION_TEXT_TOPIC, APPS_CHANGED, EXCEPTION_OCCURRED_TOPIC,
    HOSTING_USER_INFO_CHANGED_TOPIC, NEIGHBOURHOOD_SIGNAL_TOPIC, PERSPECTIVE_ADDED_TOPIC,
    PERSPECTIVE_LINK_ADDED_TOPIC, PERSPECTIVE_LINK_REMOVED_TOPIC, PERSPECTIVE_LINK_UPDATED_TOPIC,
    PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC, PERSPECTIVE_REMOVED_TOPIC,
    PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC, PERSPECTIVE_UPDATED_TOPIC,
    RUNTIME_MESSAGED_RECEIVED_TOPIC, RUNTIME_NOTIFICATION_TRIGGERED_TOPIC,
};

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;

#[derive(Deserialize)]
pub struct EventsWsParams {
    #[allow(dead_code)]
    pub token: Option<String>,
}

/// GET /ws/events — WebSocket endpoint for all real-time events.
///
/// Authentication is via `token` query parameter or `Authorization` header.
/// All event types are multiplexed over a single WebSocket connection.
pub async fn events_ws(
    ws: WebSocketUpgrade,
    State(_state): State<AppState>,
    auth: AuthContext,
    Query(_params): Query<EventsWsParams>,
) -> Result<impl IntoResponse, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let auth_token = context.auth_token.clone();
    let user_email = user_email_from_token(auth_token.clone());

    Ok(ws.on_upgrade(move |socket| handle_events_ws(socket, auth_token, user_email)))
}

/// Build the merged event stream — identical logic to the SSE endpoint.
///
/// Returns a boxed stream of (event_type, data_string) tuples, already filtered
/// per-user.
pub(crate) async fn build_event_stream(
    auth_token: String,
    user_email: Option<String>,
) -> Pin<Box<dyn futures::stream::Stream<Item = String> + Send>> {
    use futures::stream;
    use tokio_stream::wrappers::BroadcastStream;

    // Resolve the DID once at subscription time — avoids repeated JWT decode +
    // DB / AgentService lookups on every single event.
    let resolved_did: Option<String> = {
        let ctx = AgentContext::from_auth_token(auth_token.clone());
        did_for_context(&ctx).ok()
    };

    // Clone the pre-resolved DID for each filter closure
    let d_persp_added = resolved_did.clone();
    let d_persp_removed = resolved_did.clone();
    let d_persp_updated = resolved_did.clone();
    let d_link_added = resolved_did.clone();
    let d_link_removed = resolved_did.clone();
    let d_link_updated = resolved_did.clone();
    let d_signal = resolved_did.clone();
    let d_agent_status = resolved_did.clone();
    let d_agent_updated = resolved_did.clone();
    let d_apps = resolved_did.clone();
    let d_trans = resolved_did.clone();
    let d_notif = resolved_did.clone();
    let d_query_sub = resolved_did;

    let pubsub = get_global_pubsub().await;

    // ── Helper macros ──

    macro_rules! owner_stream {
        ($rx:expr, $ty:expr, $did:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .filter_map(move |result| {
                    let current_did = $did.clone();
                    async move {
                        match result {
                            Ok(ref msg) if matches_owner(msg, current_did.as_deref()) => {
                                Some(wrap_event($ty, msg))
                            }
                            _ => None,
                        }
                    }
                })
        };
    }

    macro_rules! did_stream {
        ($rx:expr, $ty:expr, $did:expr, $filter_fn:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .filter_map(move |result| {
                    let current_did = $did.clone();
                    async move {
                        match result {
                            Ok(ref msg) if $filter_fn(msg, current_did.as_deref()) => {
                                Some(wrap_event($ty, msg))
                            }
                            _ => None,
                        }
                    }
                })
        };
    }

    macro_rules! did_stream_nested {
        ($rx:expr, $ty:expr, $key:expr, $did:expr, $filter_fn:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .filter_map(move |result| {
                    let current_did = $did.clone();
                    async move {
                        match result {
                            Ok(ref msg) if $filter_fn(msg, current_did.as_deref()) => {
                                Some(wrap_event_nested($ty, $key, msg))
                            }
                            _ => None,
                        }
                    }
                })
        };
    }

    macro_rules! broadcast_stream {
        ($rx:expr, $ty:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .filter_map(move |result| async move {
                    match result {
                        Ok(msg) => Some(wrap_event($ty, &msg)),
                        _ => None,
                    }
                })
        };
    }

    macro_rules! broadcast_stream_nested {
        ($rx:expr, $ty:expr, $key:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .filter_map(move |result| async move {
                    match result {
                        Ok(msg) => Some(wrap_event_nested($ty, $key, &msg)),
                        _ => None,
                    }
                })
        };
    }

    // ── Agent events ──
    let s_status = did_stream_nested!(
        pubsub.subscribe(&AGENT_STATUS_CHANGED_TOPIC).await,
        "agent-status-changed",
        "agent",
        d_agent_status,
        matches_agent_did
    );
    let s_agent_updated = did_stream_nested!(
        pubsub.subscribe(&AGENT_UPDATED_TOPIC).await,
        "agent-updated",
        "agent",
        d_agent_updated,
        matches_agent_did
    );
    let s_apps = did_stream!(
        pubsub.subscribe(&APPS_CHANGED).await,
        "apps-changed",
        d_apps,
        matches_apps_user
    );

    let s_hosting = {
        let hosting_rx = pubsub.subscribe(&HOSTING_USER_INFO_CHANGED_TOPIC).await;
        BroadcastStream::new(hosting_rx)
            .filter_map(|r| async { handle_broadcast_result(r) })
            .filter_map(move |result| {
                let email = user_email.clone();
                async move {
                    match result {
                        Ok(ref msg) if matches_hosting_user(msg, email.as_deref()) => {
                            Some(wrap_event("hosting-user-info-changed", msg))
                        }
                        _ => None,
                    }
                }
            })
    };

    // ── Perspective lifecycle ──
    let s_persp_added = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_ADDED_TOPIC).await,
        "perspective-added",
        d_persp_added
    );
    let s_persp_removed = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_REMOVED_TOPIC).await,
        "perspective-removed",
        d_persp_removed
    );
    let s_persp_updated = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_UPDATED_TOPIC).await,
        "perspective-updated",
        d_persp_updated
    );
    let s_sync = broadcast_stream!(
        pubsub.subscribe(&PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC).await,
        "sync-state-change"
    );

    // ── Link events ──
    let s_link_added = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_LINK_ADDED_TOPIC).await,
        "link-added",
        d_link_added
    );
    let s_link_removed = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_LINK_REMOVED_TOPIC).await,
        "link-removed",
        d_link_removed
    );
    let s_link_updated = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_LINK_UPDATED_TOPIC).await,
        "link-updated",
        d_link_updated
    );

    // ── Neighbourhood signals ──
    let s_signal = did_stream!(
        pubsub.subscribe(&NEIGHBOURHOOD_SIGNAL_TOPIC).await,
        "signal",
        d_signal,
        matches_signal_recipient
    );

    // ── Runtime events ──
    let s_msg = broadcast_stream_nested!(
        pubsub.subscribe(&RUNTIME_MESSAGED_RECEIVED_TOPIC).await,
        "message-received",
        "message"
    );
    let s_notif = did_stream_nested!(
        pubsub
            .subscribe(&RUNTIME_NOTIFICATION_TRIGGERED_TOPIC)
            .await,
        "notification-triggered",
        "notification",
        d_notif,
        matches_notification_owner
    );
    let s_exc = broadcast_stream_nested!(
        pubsub.subscribe(&EXCEPTION_OCCURRED_TOPIC).await,
        "exception-occurred",
        "exception"
    );

    // ── AI events ──
    let s_trans = did_stream!(
        pubsub.subscribe(&AI_TRANSCRIPTION_TEXT_TOPIC).await,
        "transcription-text",
        d_trans,
        matches_transcription_user
    );
    let s_loading = broadcast_stream!(
        pubsub.subscribe(&AI_MODEL_LOADING_STATUS).await,
        "model-loading-status"
    );

    // ── Query subscriptions ──
    let s_query_sub = did_stream!(
        pubsub
            .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
            .await,
        "query-subscription-update",
        d_query_sub,
        matches_query_subscription_owner
    );

    // ── Merge all streams ──
    let agent = stream::select(
        stream::select(s_status, s_apps),
        stream::select(s_agent_updated, s_hosting),
    );
    let persp = stream::select(
        stream::select(s_persp_added, s_persp_removed),
        stream::select(s_persp_updated, s_sync),
    );
    let links = stream::select(s_link_added, stream::select(s_link_removed, s_link_updated));
    let runtime = stream::select(s_msg, stream::select(s_notif, s_exc));
    let ai = stream::select(s_trans, stream::select(s_loading, s_query_sub));

    let top = stream::select(
        stream::select(agent, persp),
        stream::select(stream::select(links, s_signal), stream::select(runtime, ai)),
    );

    Box::pin(top)
}

async fn handle_events_ws(mut socket: WebSocket, auth_token: String, user_email: Option<String>) {
    log::info!("Events WebSocket connected");

    let mut event_stream = build_event_stream(auth_token, user_email).await;

    loop {
        tokio::select! {
            // Forward events from PubSub to WebSocket
            event = event_stream.next() => {
                match event {
                    Some(data) => {
                        if socket.send(Message::Text(data.into())).await.is_err() {
                            break;
                        }
                    }
                    None => break, // All streams ended (shouldn't happen with broadcast)
                }
            }
            // Handle incoming WebSocket messages
            msg = socket.recv() => {
                match msg {
                    Some(Ok(Message::Text(text))) => {
                        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&*text) {
                            if parsed.get("type").and_then(|v| v.as_str()) == Some("ping") {
                                let _ = socket.send(Message::Text(r#"{"type":"pong"}"#.into())).await;
                            }
                        }
                    }
                    Some(Ok(Message::Close(_))) | None => {
                        log::info!("Events WebSocket closed");
                        break;
                    }
                    Some(Ok(Message::Ping(data))) => {
                        let _ = socket.send(Message::Pong(data)).await;
                    }
                    _ => {} // Ignore binary, pong, etc.
                }
            }
        }
    }
}

// ── Event helper functions ──────────────────────────────────────────────────

use tokio_stream::wrappers::errors::BroadcastStreamRecvError;

pub(crate) fn handle_broadcast_result(
    r: Result<String, BroadcastStreamRecvError>,
) -> Option<Result<String, u64>> {
    match r {
        Ok(msg) => Some(Ok(msg)),
        Err(BroadcastStreamRecvError::Lagged(n)) => Some(Err(n)),
    }
}

pub(crate) fn wrap_event(event_type: &str, raw_json: &str) -> String {
    if let Ok(serde_json::Value::Object(mut map)) = serde_json::from_str(raw_json) {
        map.insert(
            "type".to_string(),
            serde_json::Value::String(event_type.to_string()),
        );
        serde_json::to_string(&map)
            .unwrap_or_else(|_| format!(r#"{{"type":"{}","data":{}}}"#, event_type, raw_json))
    } else {
        format!(r#"{{"type":"{}","data":{}}}"#, event_type, raw_json)
    }
}

pub(crate) fn wrap_event_nested(event_type: &str, payload_key: &str, raw_json: &str) -> String {
    format!(
        r#"{{"type":"{}","{}":{}}}"#,
        event_type, payload_key, raw_json
    )
}

pub(crate) fn matches_owner(msg: &str, current_did: Option<&str>) -> bool {
    match current_did {
        None => true,
        Some(did) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::String(owner)) = map.get("owner") {
                    return owner == did;
                }
            }
            true
        }
    }
}

pub(crate) fn matches_signal_recipient(msg: &str, current_did: Option<&str>) -> bool {
    match serde_json::from_str::<serde_json::Value>(msg) {
        Ok(serde_json::Value::Object(map)) => match map.get("recipient") {
            Some(serde_json::Value::String(recipient)) => {
                // If no DID resolved (e.g., connection before agent init), allow all signals
                current_did.map_or(true, |did| did == recipient)
            }
            Some(serde_json::Value::Null) | None => true,
            _ => false,
        },
        Err(_) => true,
        _ => false,
    }
}

pub(crate) fn matches_agent_did(msg: &str, current_did: Option<&str>) -> bool {
    match current_did {
        None => true,
        Some(did) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::String(agent_did)) = map.get("did") {
                    return agent_did == did;
                }
            }
            true
        }
    }
}

pub(crate) fn matches_apps_user(msg: &str, current_did: Option<&str>) -> bool {
    match current_did {
        None => true,
        Some(did) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::Object(auth)) = map.get("auth") {
                    if let Some(serde_json::Value::String(user_did)) = auth.get("user_did") {
                        return user_did == did;
                    }
                }
            }
            true
        }
    }
}

pub(crate) fn matches_hosting_user(msg: &str, user_email: Option<&str>) -> bool {
    match user_email {
        None => true,
        Some(email) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::String(msg_email)) = map.get("email") {
                    return msg_email == email;
                }
            }
            true
        }
    }
}

pub(crate) fn matches_transcription_user(msg: &str, current_did: Option<&str>) -> bool {
    match current_did {
        None => true,
        Some(did) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::String(user_did)) = map.get("userDid") {
                    return user_did == did;
                }
            }
            true
        }
    }
}

pub(crate) fn matches_notification_owner(msg: &str, current_did: Option<&str>) -> bool {
    match current_did {
        None => true,
        Some(did) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::String(uuid)) = map
                    .get("perspectiveId")
                    .or_else(|| map.get("perspective_id"))
                {
                    return perspective_is_owned_by(uuid, did);
                }
            }
            true
        }
    }
}

pub(crate) fn matches_query_subscription_owner(msg: &str, current_did: Option<&str>) -> bool {
    match current_did {
        None => true,
        Some(did) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::String(uuid)) = map.get("uuid") {
                    return perspective_is_owned_by(uuid, did);
                }
            }
            true
        }
    }
}

fn perspective_is_owned_by(uuid: &str, did: &str) -> bool {
    use crate::perspectives::get_perspective;
    match get_perspective(uuid) {
        Some(instance) => match instance.persisted.try_lock() {
            Ok(handle) => {
                if handle.is_unowned() {
                    true
                } else {
                    handle.is_owned_by(did)
                }
            }
            Err(_) => true,
        },
        None => true,
    }
}

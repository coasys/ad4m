//! SSE event endpoints: /api/v1/events/*
//!
//! 6 SSE endpoints tapping into the existing PubSub system.
//!
//! Events are sent as unnamed SSE events (no `.event()` call) so that the
//! browser/client `EventSource.onmessage` handler receives them.  Each message
//! is a JSON object `{ "type": "<event-type>", ...payload }` where `payload` is
//! the original pubsub message merged into the wrapper.

use std::convert::Infallible;

use axum::{
    extract::{Path, State},
    response::sse::{Event, KeepAlive, Sse},
};
use futures::stream::{self, Stream, StreamExt};
use tokio_stream::wrappers::errors::BroadcastStreamRecvError;
use tokio_stream::wrappers::BroadcastStream;

/// Convert a BroadcastStream result into an Option<String>, emitting a lagged event on overflow.
fn handle_broadcast_result(
    r: Result<String, BroadcastStreamRecvError>,
) -> Option<Result<String, u64>> {
    match r {
        Ok(msg) => Some(Ok(msg)),
        Err(BroadcastStreamRecvError::Lagged(n)) => Some(Err(n)),
    }
}

/// Create an SSE stream from a broadcast receiver, with lagged event reporting.
fn broadcast_to_sse_stream(
    rx: tokio::sync::broadcast::Receiver<String>,
    event_type: &'static str,
) -> impl Stream<Item = Result<Event, Infallible>> {
    BroadcastStream::new(rx)
        .filter_map(|r| async { handle_broadcast_result(r) })
        .map(move |result| match result {
            Ok(msg) => Ok(Event::default().data(wrap_event(event_type, &msg))),
            Err(n) => Ok(Event::default().data(format!(
                r#"{{"type":"lagged","missed":{},"stream":"{}"}}"#,
                n, event_type
            ))),
        })
}

fn broadcast_to_sse_stream_nested(
    rx: tokio::sync::broadcast::Receiver<String>,
    event_type: &'static str,
    payload_key: &'static str,
) -> impl Stream<Item = Result<Event, Infallible>> {
    BroadcastStream::new(rx)
        .filter_map(|r| async { handle_broadcast_result(r) })
        .map(move |result| match result {
            Ok(msg) => Ok(Event::default().data(wrap_event_nested(
                event_type,
                payload_key,
                &msg,
            ))),
            Err(n) => Ok(Event::default().data(format!(
                r#"{{"type":"lagged","missed":{},"stream":"{}"}}"#,
                n, event_type
            ))),
        })
}

use crate::agent::capabilities::*;
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
use ad4m_rest_macros::rest_handler;

/// Check if a pubsub JSON message belongs to the given perspective UUID.
/// Parses JSON and checks the `perspectiveUuid` field instead of string containment.
fn matches_perspective_uuid(msg: &str, uuid: &str) -> bool {
    if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
        if let Some(serde_json::Value::String(ref id)) = map.get("perspectiveUuid") {
            return id == uuid;
        }
    }
    // Fallback: string containment for backwards compatibility
    msg.contains(uuid)
}

/// Wrap a raw pubsub JSON string with a `"type"` field.
///
/// If the original message is a JSON object, the type is merged in:
///   `{"type": "foo", ...original}`
/// Otherwise (scalar / array / invalid JSON) it becomes:
///   `{"type": "foo", "data": <original>}`
fn wrap_event(event_type: &str, raw_json: &str) -> String {
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

fn wrap_event_nested(event_type: &str, payload_key: &str, raw_json: &str) -> String {
    format!(
        r#"{{"type":"{}","{}":{}}}"#,
        event_type, payload_key, raw_json
    )
}

/// GET /events/agent — SSE: agent-status-changed, apps-changed, agent-updated, hosting-user-info-changed, compute-log-updated
#[rest_handler(GET, "/events/agent", response = "void")]
pub async fn agent_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let pubsub = get_global_pubsub().await;

    let status_rx = pubsub.subscribe(&AGENT_STATUS_CHANGED_TOPIC).await;
    let apps_rx = pubsub.subscribe(&APPS_CHANGED).await;
    let updated_rx = pubsub.subscribe(&AGENT_UPDATED_TOPIC).await;
    let hosting_rx = pubsub.subscribe(&HOSTING_USER_INFO_CHANGED_TOPIC).await;

    let status_stream = broadcast_to_sse_stream(status_rx, "agent-status-changed");
    let apps_stream = broadcast_to_sse_stream(apps_rx, "apps-changed");
    let updated_stream = broadcast_to_sse_stream(updated_rx, "agent-updated");
    let hosting_stream = broadcast_to_sse_stream(hosting_rx, "hosting-user-info-changed");

    let merged = stream::select(
        stream::select(status_stream, apps_stream),
        stream::select(updated_stream, hosting_stream),
    );

    Ok(Sse::new(merged).keep_alive(KeepAlive::default()))
}

/// GET /events/perspectives — SSE: perspective-added, perspective-removed, perspective-updated, sync-state-change
#[rest_handler(GET, "/events/perspectives", response = "void")]
pub async fn perspective_lifecycle_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let pubsub = get_global_pubsub().await;

    let added_rx = pubsub.subscribe(&PERSPECTIVE_ADDED_TOPIC).await;
    let removed_rx = pubsub.subscribe(&PERSPECTIVE_REMOVED_TOPIC).await;
    let updated_rx = pubsub.subscribe(&PERSPECTIVE_UPDATED_TOPIC).await;
    let sync_rx = pubsub.subscribe(&PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC).await;

    let s1 = broadcast_to_sse_stream(added_rx, "perspective-added");
    let s2 = broadcast_to_sse_stream(removed_rx, "perspective-removed");
    let s3 = broadcast_to_sse_stream(updated_rx, "perspective-updated");
    let s4 = broadcast_to_sse_stream(sync_rx, "sync-state-change");

    let merged = stream::select(stream::select(s1, s2), stream::select(s3, s4));
    Ok(Sse::new(merged).keep_alive(KeepAlive::default()))
}

/// GET /events/perspectives/:uuid/links — SSE: link-added, link-removed, link-updated
#[rest_handler(GET, "/events/perspectives/:uuid/links", response = "void")]
pub async fn perspective_link_events(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let pubsub = get_global_pubsub().await;

    let added_rx = pubsub.subscribe(&PERSPECTIVE_LINK_ADDED_TOPIC).await;
    let removed_rx = pubsub.subscribe(&PERSPECTIVE_LINK_REMOVED_TOPIC).await;
    let updated_rx = pubsub.subscribe(&PERSPECTIVE_LINK_UPDATED_TOPIC).await;

    let uuid_clone = uuid.clone();
    let s1 = BroadcastStream::new(added_rx)
        .filter_map(|r| async { handle_broadcast_result(r) })
        .filter_map(move |result| {
            let uuid = uuid_clone.clone();
            async move {
                match result {
                    Ok(msg) if matches_perspective_uuid(&msg, &uuid) => {
                        Some(Ok(Event::default().data(wrap_event("link-added", &msg))))
                    }
                    Err(n) => Some(Ok(Event::default().data(format!(
                        r#"{{"type":"lagged","missed":{},"stream":"link-added"}}"#,
                        n
                    )))),
                    _ => None,
                }
            }
        });

    let uuid_clone = uuid.clone();
    let s2 = BroadcastStream::new(removed_rx)
        .filter_map(|r| async { handle_broadcast_result(r) })
        .filter_map(move |result| {
            let uuid = uuid_clone.clone();
            async move {
                match result {
                    Ok(msg) if matches_perspective_uuid(&msg, &uuid) => {
                        Some(Ok(Event::default().data(wrap_event("link-removed", &msg))))
                    }
                    Err(n) => Some(Ok(Event::default().data(format!(
                        r#"{{"type":"lagged","missed":{},"stream":"link-removed"}}"#,
                        n
                    )))),
                    _ => None,
                }
            }
        });

    let uuid_clone = uuid;
    let s3 = BroadcastStream::new(updated_rx)
        .filter_map(|r| async { handle_broadcast_result(r) })
        .filter_map(move |result| {
            let uuid = uuid_clone.clone();
            async move {
                match result {
                    Ok(msg) if matches_perspective_uuid(&msg, &uuid) => {
                        Some(Ok(Event::default().data(wrap_event("link-updated", &msg))))
                    }
                    Err(n) => Some(Ok(Event::default().data(format!(
                        r#"{{"type":"lagged","missed":{},"stream":"link-updated"}}"#,
                        n
                    )))),
                    _ => None,
                }
            }
        });

    let merged = stream::select(s1, stream::select(s2, s3));
    Ok(Sse::new(merged).keep_alive(KeepAlive::default()))
}

/// GET /events/neighbourhoods/:uuid/signals — SSE: signal
#[rest_handler(GET, "/events/neighbourhoods/:uuid/signals", response = "void")]
pub async fn neighbourhood_signal_events(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let pubsub = get_global_pubsub().await;
    let rx = pubsub.subscribe(&NEIGHBOURHOOD_SIGNAL_TOPIC).await;

    let stream = BroadcastStream::new(rx)
        .filter_map(|r| async { handle_broadcast_result(r) })
        .filter_map(move |result| {
            let uuid = uuid.clone();
            async move {
                match result {
                    Ok(msg) if matches_perspective_uuid(&msg, &uuid) => {
                        Some(Ok(Event::default().data(wrap_event("signal", &msg))))
                    }
                    Err(n) => Some(Ok(Event::default().data(format!(
                        r#"{{"type":"lagged","missed":{},"stream":"signal"}}"#,
                        n
                    )))),
                    _ => None,
                }
            }
        });

    Ok(Sse::new(stream).keep_alive(KeepAlive::default()))
}

/// GET /events/runtime — SSE: message-received, notification-triggered, exception-occurred
#[rest_handler(GET, "/events/runtime", response = "void")]
pub async fn runtime_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_MESSAGES_SUBSCRIBE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let pubsub = get_global_pubsub().await;

    let msg_rx = pubsub.subscribe(&RUNTIME_MESSAGED_RECEIVED_TOPIC).await;
    let notif_rx = pubsub
        .subscribe(&RUNTIME_NOTIFICATION_TRIGGERED_TOPIC)
        .await;
    let exc_rx = pubsub.subscribe(&EXCEPTION_OCCURRED_TOPIC).await;

    let s1 = broadcast_to_sse_stream_nested(msg_rx, "message-received", "message");
    let s2 = broadcast_to_sse_stream_nested(notif_rx, "notification-triggered", "notification");
    let s3 = broadcast_to_sse_stream_nested(exc_rx, "exception-occurred", "exception");

    let merged = stream::select(s1, stream::select(s2, s3));
    Ok(Sse::new(merged).keep_alive(KeepAlive::default()))
}

/// GET /events/ai — SSE: transcription-text, model-loading-status
#[rest_handler(GET, "/events/ai", response = "void")]
pub async fn ai_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_MESSAGES_SUBSCRIBE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let pubsub = get_global_pubsub().await;

    let trans_rx = pubsub.subscribe(&AI_TRANSCRIPTION_TEXT_TOPIC).await;
    let loading_rx = pubsub.subscribe(&AI_MODEL_LOADING_STATUS).await;

    let s1 = broadcast_to_sse_stream(trans_rx, "transcription-text");
    let s2 = broadcast_to_sse_stream(loading_rx, "model-loading-status");

    let merged = stream::select(s1, s2);
    Ok(Sse::new(merged).keep_alive(KeepAlive::default()))
}

/// GET /events/query-subscription/:subscription_id — SSE: query subscription updates
///
/// Filters the global `PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC` to only emit
/// events matching the given `subscription_id`.  The pubsub message is a JSON
/// object `{ uuid, subscription_id, result }` — we forward the full object
/// wrapped with `"type": "query-subscription-update"`.
#[rest_handler(GET, "/events/query-subscription/:subscription_id", response = "void")]
pub async fn query_subscription_events(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(subscription_id): Path<String>,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let pubsub = get_global_pubsub().await;
    let rx = pubsub
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    let stream = BroadcastStream::new(rx)
        .filter_map(|r| async { handle_broadcast_result(r) })
        .filter_map(move |result| {
            let sub_id = subscription_id.clone();
            async move {
                match result {
                    Ok(msg) => {
                        // Parse and check subscription_id field
                        if let Ok(serde_json::Value::Object(ref map)) =
                            serde_json::from_str::<serde_json::Value>(&msg)
                        {
                            if let Some(serde_json::Value::String(ref id)) = map
                                .get("subscription_id")
                                .or_else(|| map.get("subscriptionId"))
                            {
                                if id == &sub_id {
                                    return Some(Ok(Event::default()
                                        .data(wrap_event("query-subscription-update", &msg))));
                                }
                            }
                        }
                        None
                    }
                    Err(n) => Some(Ok(Event::default().data(format!(
                        r#"{{"type":"lagged","missed":{},"stream":"query-subscription"}}"#,
                        n
                    )))),
                }
            }
        });

    Ok(Sse::new(stream).keep_alive(KeepAlive::default()))
}

/// GET /events — Unified SSE endpoint that merges ALL event topics into a
/// single HTTP connection.  This avoids exhausting the browser's per-origin
/// connection limit (6 in Chrome) when multiple SSE streams are needed.
///
/// Each event is a JSON object with a `"type"` field identifying the topic.
/// Perspective-specific events include `"perspectiveUuid"` for client-side filtering.
#[rest_handler(GET, "/events/unified", response = "void")]
pub async fn unified_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let pubsub = get_global_pubsub().await;

    // Agent events
    let status_rx = pubsub.subscribe(&AGENT_STATUS_CHANGED_TOPIC).await;
    let apps_rx = pubsub.subscribe(&APPS_CHANGED).await;
    let agent_updated_rx = pubsub.subscribe(&AGENT_UPDATED_TOPIC).await;
    let hosting_rx = pubsub.subscribe(&HOSTING_USER_INFO_CHANGED_TOPIC).await;

    // Perspective lifecycle events
    let persp_added_rx = pubsub.subscribe(&PERSPECTIVE_ADDED_TOPIC).await;
    let persp_removed_rx = pubsub.subscribe(&PERSPECTIVE_REMOVED_TOPIC).await;
    let persp_updated_rx = pubsub.subscribe(&PERSPECTIVE_UPDATED_TOPIC).await;
    let sync_rx = pubsub.subscribe(&PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC).await;

    // Perspective link events (all perspectives, no filtering)
    let link_added_rx = pubsub.subscribe(&PERSPECTIVE_LINK_ADDED_TOPIC).await;
    let link_removed_rx = pubsub.subscribe(&PERSPECTIVE_LINK_REMOVED_TOPIC).await;
    let link_updated_rx = pubsub.subscribe(&PERSPECTIVE_LINK_UPDATED_TOPIC).await;

    // Neighbourhood signals (all neighbourhoods)
    let signal_rx = pubsub.subscribe(&NEIGHBOURHOOD_SIGNAL_TOPIC).await;

    // Runtime events
    let msg_rx = pubsub.subscribe(&RUNTIME_MESSAGED_RECEIVED_TOPIC).await;
    let notif_rx = pubsub
        .subscribe(&RUNTIME_NOTIFICATION_TRIGGERED_TOPIC)
        .await;
    let exc_rx = pubsub.subscribe(&EXCEPTION_OCCURRED_TOPIC).await;

    // AI events
    let trans_rx = pubsub.subscribe(&AI_TRANSCRIPTION_TEXT_TOPIC).await;
    let loading_rx = pubsub.subscribe(&AI_MODEL_LOADING_STATUS).await;

    // Convert each receiver into a typed stream
    macro_rules! typed_stream {
        ($rx:expr, $ty:expr) => {
            broadcast_to_sse_stream($rx, $ty)
        };
    }

    let s_status = typed_stream!(status_rx, "agent-status-changed");
    let s_apps = typed_stream!(apps_rx, "apps-changed");
    let s_agent_updated = typed_stream!(agent_updated_rx, "agent-updated");
    let s_hosting = typed_stream!(hosting_rx, "hosting-user-info-changed");

    let s_persp_added = typed_stream!(persp_added_rx, "perspective-added");
    let s_persp_removed = typed_stream!(persp_removed_rx, "perspective-removed");
    let s_persp_updated = typed_stream!(persp_updated_rx, "perspective-updated");
    let s_sync = typed_stream!(sync_rx, "sync-state-change");

    let s_link_added = typed_stream!(link_added_rx, "link-added");
    let s_link_removed = typed_stream!(link_removed_rx, "link-removed");
    let s_link_updated = typed_stream!(link_updated_rx, "link-updated");

    let s_signal = typed_stream!(signal_rx, "signal");

    let s_msg = broadcast_to_sse_stream_nested(msg_rx, "message-received", "message");
    let s_notif =
        broadcast_to_sse_stream_nested(notif_rx, "notification-triggered", "notification");
    let s_exc = broadcast_to_sse_stream_nested(exc_rx, "exception-occurred", "exception");

    let s_trans = typed_stream!(trans_rx, "transcription-text");
    let s_loading = typed_stream!(loading_rx, "model-loading-status");

    // Merge all streams using a balanced binary tree of stream::select
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
    let ai = stream::select(s_trans, s_loading);

    let top = stream::select(
        stream::select(agent, persp),
        stream::select(stream::select(links, s_signal), stream::select(runtime, ai)),
    );

    Ok(Sse::new(top).keep_alive(KeepAlive::default()))
}

#[cfg(test)]
mod tests {
    use super::{wrap_event, wrap_event_nested};

    #[test]
    fn wrap_event_preserves_flat_payloads_for_regular_events() {
        let wrapped = wrap_event("agent-status-changed", r#"{"isUnlocked":true}"#);
        assert_eq!(wrapped, r#"{"isUnlocked":true,"type":"agent-status-changed"}"#);
    }

    #[test]
    fn wrap_event_nested_preserves_inner_exception_type() {
        let wrapped = wrap_event_nested(
            "exception-occurred",
            "exception",
            r#"{"title":"Request","message":"Waiting","type":"CAPABILITY_REQUESTED","addon":"{}"}"#,
        );
        assert_eq!(
            wrapped,
            r#"{"type":"exception-occurred","exception":{"title":"Request","message":"Waiting","type":"CAPABILITY_REQUESTED","addon":"{}"}}"#
        );
    }
}

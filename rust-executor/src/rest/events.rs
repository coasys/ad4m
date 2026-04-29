//! SSE event endpoint: GET /api/v1/events
//!
//! Single SSE endpoint serving ALL event types.  Every event is a JSON object
//! `{ "type": "<event-type>", ...payload }`.  Events are filtered per-user in
//! multi-user mode so that each authenticated user only receives events that
//! belong to them.

use std::convert::Infallible;

use axum::{
    extract::State,
    response::sse::{Event, KeepAlive, Sse},
};
use futures::stream::{self, Stream, StreamExt};
use tokio_stream::wrappers::errors::BroadcastStreamRecvError;
use tokio_stream::wrappers::BroadcastStream;

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
use ad4m_rest_macros::rest_handler;

// ─── Helpers ──────────────────────────────────────────────────────────────────

/// Convert a BroadcastStream result, emitting a lagged marker on overflow.
fn handle_broadcast_result(
    r: Result<String, BroadcastStreamRecvError>,
) -> Option<Result<String, u64>> {
    match r {
        Ok(msg) => Some(Ok(msg)),
        Err(BroadcastStreamRecvError::Lagged(n)) => Some(Err(n)),
    }
}

/// Wrap a raw pubsub JSON string with a `"type"` field.
///
/// If the original message is a JSON object the type is merged in:
///   `{"type": "foo", ...original}`
/// Otherwise it becomes:
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

// ─── Per-user filtering predicates ───────────────────────────────────────────

/// Perspective lifecycle events and link events include an `owner` DID.
/// For managed users, only emit events whose owner matches.  For main agent /
/// admin (current_did is None), emit all events.
fn matches_owner(msg: &str, current_did: Option<&str>) -> bool {
    match current_did {
        None => true,
        Some(did) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::String(owner)) = map.get("owner") {
                    return owner == did;
                }
            }
            // No owner field → treat as broadcast (backwards compat)
            true
        }
    }
}

/// Neighbourhood signals: `recipient` field must match or be absent (broadcast).
fn matches_signal_recipient(msg: &str, current_did: Option<&str>) -> bool {
    match serde_json::from_str::<serde_json::Value>(msg) {
        Ok(serde_json::Value::Object(map)) => match map.get("recipient") {
            Some(serde_json::Value::String(recipient)) => {
                current_did.is_some_and(|did| did == recipient)
            }
            Some(serde_json::Value::Null) | None => true,
            _ => false,
        },
        Err(_) => true,
        _ => false,
    }
}

/// Agent events: the payload contains a `did` field.  For managed users, only
/// emit when it matches.  Main agent / admin sees all.
fn matches_agent_did(msg: &str, current_did: Option<&str>) -> bool {
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

/// Apps-changed events: the auth payload contains `user_did`.
fn matches_apps_user(msg: &str, current_did: Option<&str>) -> bool {
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

/// Hosting-user-info events: payload has `email`.  Match against the requesting
/// user's email (derived from the auth token).
fn matches_hosting_user(msg: &str, user_email: Option<&str>) -> bool {
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

/// Transcription events: payload has `userDid`.  Match against the requesting
/// user's DID.  Main agent / admin sees all.
fn matches_transcription_user(msg: &str, current_did: Option<&str>) -> bool {
    match current_did {
        None => true,
        Some(did) => {
            if let Ok(serde_json::Value::Object(map)) = serde_json::from_str(msg) {
                if let Some(serde_json::Value::String(user_did)) = map.get("userDid") {
                    return user_did == did;
                }
            }
            // No userDid field → legacy message, pass through
            true
        }
    }
}

/// Notification events: payload has a `perspective_id` (or `perspectiveId`).
/// Look up the perspective's owners and check if the current user is among them.
fn matches_notification_owner(msg: &str, current_did: Option<&str>) -> bool {
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

/// Query-subscription events: payload has `uuid` (perspective UUID).
/// Look up the perspective's owners to check if the current user owns it.
fn matches_query_subscription_owner(msg: &str, current_did: Option<&str>) -> bool {
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

/// Check if a perspective is owned by (or visible to) the given DID.
/// Falls back to `true` if the perspective doesn't exist or has no owners
/// (unowned perspectives are visible to everyone).
fn perspective_is_owned_by(uuid: &str, did: &str) -> bool {
    use crate::perspectives::get_perspective;
    match get_perspective(uuid) {
        Some(instance) => {
            // PerspectiveInstance.persisted is a tokio::sync::Mutex — use try_lock
            // to avoid blocking in the stream filter.
            match instance.persisted.try_lock() {
                Ok(handle) => {
                    if handle.is_unowned() {
                        true
                    } else {
                        handle.is_owned_by(did)
                    }
                }
                // Lock contended → allow through
                Err(_) => true,
            }
        }
        // Perspective not found → allow (might be stale event)
        None => true,
    }
}

// ─── The single SSE endpoint ─────────────────────────────────────────────────

/// GET /events — Single SSE endpoint for ALL event types.
///
/// Each event is a JSON object with a `"type"` field.
/// In multi-user mode, events are filtered per-user.
#[rest_handler(GET, "/events", response = "void")]
pub async fn events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Sse<impl Stream<Item = Result<Event, Infallible>>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let auth_token = context.auth_token.clone();

    // Pre-resolve user email for hosting-info filtering
    let user_email = user_email_from_token(auth_token.clone());

    // Clone auth tokens for each filter closure
    let t_persp_added = auth_token.clone();
    let t_persp_removed = auth_token.clone();
    let t_persp_updated = auth_token.clone();
    let t_link_added = auth_token.clone();
    let t_link_removed = auth_token.clone();
    let t_link_updated = auth_token.clone();
    let t_signal = auth_token.clone();
    let t_agent_status = auth_token.clone();
    let t_agent_updated = auth_token.clone();
    let t_apps = auth_token.clone();
    let t_trans = auth_token.clone();
    let t_notif = auth_token.clone();
    let t_query_sub = auth_token;

    let pubsub = get_global_pubsub().await;

    // ── Helper macros (local to this function) ──

    /// Owner-filtered stream (perspective lifecycle + link events)
    macro_rules! owner_stream {
        ($rx:expr, $ty:expr, $token:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .filter_map(move |result| {
                    let token = $token.clone();
                    async move {
                        let current_did =
                            did_for_context(&AgentContext::from_auth_token(token)).ok();
                        match result {
                            Ok(ref msg) if matches_owner(msg, current_did.as_deref()) => {
                                Some(Ok(Event::default().data(wrap_event($ty, msg))))
                            }
                            Err(n) => Some(Ok(Event::default().data(format!(
                                r#"{{"type":"lagged","missed":{},"stream":"{}"}}"#,
                                n, $ty
                            )))),
                            _ => None,
                        }
                    }
                })
        };
    }

    /// User-DID-filtered stream with custom predicate
    macro_rules! did_stream {
        ($rx:expr, $ty:expr, $token:expr, $filter_fn:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .filter_map(move |result| {
                    let token = $token.clone();
                    async move {
                        let current_did =
                            did_for_context(&AgentContext::from_auth_token(token)).ok();
                        match result {
                            Ok(ref msg) if $filter_fn(msg, current_did.as_deref()) => {
                                Some(Ok(Event::default().data(wrap_event($ty, msg))))
                            }
                            Err(n) => Some(Ok(Event::default().data(format!(
                                r#"{{"type":"lagged","missed":{},"stream":"{}"}}"#,
                                n, $ty
                            )))),
                            _ => None,
                        }
                    }
                })
        };
    }

    /// Nested-payload variant
    macro_rules! did_stream_nested {
        ($rx:expr, $ty:expr, $key:expr, $token:expr, $filter_fn:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .filter_map(move |result| {
                    let token = $token.clone();
                    async move {
                        let current_did =
                            did_for_context(&AgentContext::from_auth_token(token)).ok();
                        match result {
                            Ok(ref msg) if $filter_fn(msg, current_did.as_deref()) => {
                                Some(Ok(Event::default().data(wrap_event_nested($ty, $key, msg))))
                            }
                            Err(n) => Some(Ok(Event::default().data(format!(
                                r#"{{"type":"lagged","missed":{},"stream":"{}"}}"#,
                                n, $ty
                            )))),
                            _ => None,
                        }
                    }
                })
        };
    }

    /// Unfiltered broadcast stream (system-level events)
    macro_rules! broadcast_stream {
        ($rx:expr, $ty:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .map(move |result| match result {
                    Ok(msg) => Ok(Event::default().data(wrap_event($ty, &msg))),
                    Err(n) => Ok(Event::default().data(format!(
                        r#"{{"type":"lagged","missed":{},"stream":"{}"}}"#,
                        n, $ty
                    ))),
                })
        };
    }

    macro_rules! broadcast_stream_nested {
        ($rx:expr, $ty:expr, $key:expr) => {
            BroadcastStream::new($rx)
                .filter_map(|r| async { handle_broadcast_result(r) })
                .map(move |result| match result {
                    Ok(msg) => Ok(Event::default().data(wrap_event_nested($ty, $key, &msg))),
                    Err(n) => Ok(Event::default().data(format!(
                        r#"{{"type":"lagged","missed":{},"stream":"{}"}}"#,
                        n, $ty
                    ))),
                })
        };
    }

    // ── Agent events (filtered by DID) ──
    let s_status = did_stream_nested!(
        pubsub.subscribe(&AGENT_STATUS_CHANGED_TOPIC).await,
        "agent-status-changed",
        "agent",
        t_agent_status,
        matches_agent_did
    );
    let s_agent_updated = did_stream_nested!(
        pubsub.subscribe(&AGENT_UPDATED_TOPIC).await,
        "agent-updated",
        "agent",
        t_agent_updated,
        matches_agent_did
    );
    let s_apps = did_stream!(
        pubsub.subscribe(&APPS_CHANGED).await,
        "apps-changed",
        t_apps,
        matches_apps_user
    );

    // Hosting-user-info: filter by email (not DID)
    let s_hosting = {
        let hosting_rx = pubsub.subscribe(&HOSTING_USER_INFO_CHANGED_TOPIC).await;
        BroadcastStream::new(hosting_rx)
            .filter_map(|r| async { handle_broadcast_result(r) })
            .filter_map(move |result| {
                let email = user_email.clone();
                async move {
                    match result {
                        Ok(ref msg) if matches_hosting_user(msg, email.as_deref()) => {
                            Some(Ok(
                                Event::default()
                                    .data(wrap_event("hosting-user-info-changed", msg)),
                            ))
                        }
                        Err(n) => Some(Ok(Event::default().data(format!(
                            r#"{{"type":"lagged","missed":{},"stream":"hosting-user-info-changed"}}"#,
                            n
                        )))),
                        _ => None,
                    }
                }
            })
    };

    // ── Perspective lifecycle (filtered by owner DID) ──
    let s_persp_added = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_ADDED_TOPIC).await,
        "perspective-added",
        t_persp_added
    );
    let s_persp_removed = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_REMOVED_TOPIC).await,
        "perspective-removed",
        t_persp_removed
    );
    let s_persp_updated = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_UPDATED_TOPIC).await,
        "perspective-updated",
        t_persp_updated
    );
    let s_sync = broadcast_stream!(
        pubsub.subscribe(&PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC).await,
        "sync-state-change"
    );

    // ── Link events (filtered by owner DID) ──
    let s_link_added = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_LINK_ADDED_TOPIC).await,
        "link-added",
        t_link_added
    );
    let s_link_removed = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_LINK_REMOVED_TOPIC).await,
        "link-removed",
        t_link_removed
    );
    let s_link_updated = owner_stream!(
        pubsub.subscribe(&PERSPECTIVE_LINK_UPDATED_TOPIC).await,
        "link-updated",
        t_link_updated
    );

    // ── Neighbourhood signals (filtered by recipient DID) ──
    let s_signal = did_stream!(
        pubsub.subscribe(&NEIGHBOURHOOD_SIGNAL_TOPIC).await,
        "signal",
        t_signal,
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
        t_notif,
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
        t_trans,
        matches_transcription_user
    );
    let s_loading = broadcast_stream!(
        pubsub.subscribe(&AI_MODEL_LOADING_STATUS).await,
        "model-loading-status"
    );

    // ── Query subscriptions (filtered by perspective owner) ──
    let s_query_sub = did_stream!(
        pubsub
            .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
            .await,
        "query-subscription-update",
        t_query_sub,
        matches_query_subscription_owner
    );

    // ── Merge all streams (balanced binary tree for fairness) ──
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

    Ok(Sse::new(top).keep_alive(KeepAlive::default()))
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wrap_event_preserves_flat_payloads() {
        let wrapped = wrap_event("agent-status-changed", r#"{"isUnlocked":true}"#);
        assert_eq!(
            wrapped,
            r#"{"isUnlocked":true,"type":"agent-status-changed"}"#
        );
    }

    #[test]
    fn wrap_event_nested_preserves_inner_type() {
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

    #[test]
    fn matches_signal_recipient_allows_broadcasts() {
        assert!(matches_signal_recipient(
            r#"{"perspective":{"uuid":"p-1"},"signal":{"data":{"links":[]}}}"#,
            Some("did:key:z6Mktest"),
        ));
    }

    #[test]
    fn matches_signal_recipient_accepts_matching() {
        assert!(matches_signal_recipient(
            r#"{"perspective":{"uuid":"p-1"},"signal":{"data":{}},"recipient":"did:key:z6Mkmatch"}"#,
            Some("did:key:z6Mkmatch"),
        ));
    }

    #[test]
    fn matches_signal_recipient_rejects_other() {
        assert!(!matches_signal_recipient(
            r#"{"perspective":{"uuid":"p-1"},"signal":{"data":{}},"recipient":"did:key:z6Mkother"}"#,
            Some("did:key:z6Mkself"),
        ));
    }

    #[test]
    fn matches_owner_admin_sees_all() {
        assert!(matches_owner(
            r#"{"owner":"did:key:z6Mkuser","perspectiveUuid":"p-1"}"#,
            None,
        ));
    }

    #[test]
    fn matches_owner_user_sees_own() {
        assert!(matches_owner(
            r#"{"owner":"did:key:z6Mkuser","perspectiveUuid":"p-1"}"#,
            Some("did:key:z6Mkuser"),
        ));
    }

    #[test]
    fn matches_owner_user_blocked_from_other() {
        assert!(!matches_owner(
            r#"{"owner":"did:key:z6MkotherUser","perspectiveUuid":"p-1"}"#,
            Some("did:key:z6Mkuser"),
        ));
    }

    #[test]
    fn matches_agent_did_admin_sees_all() {
        assert!(matches_agent_did(
            r#"{"did":"did:key:z6Mkagent","isUnlocked":true}"#,
            None,
        ));
    }

    #[test]
    fn matches_agent_did_user_sees_own() {
        assert!(matches_agent_did(
            r#"{"did":"did:key:z6Mkuser","isUnlocked":true}"#,
            Some("did:key:z6Mkuser"),
        ));
    }

    #[test]
    fn matches_transcription_user_admin_sees_all() {
        assert!(matches_transcription_user(
            r#"{"streamId":"abc","text":"hello","userDid":"did:key:z6Mkuser"}"#,
            None,
        ));
    }

    #[test]
    fn matches_transcription_user_filters_correctly() {
        assert!(matches_transcription_user(
            r#"{"streamId":"abc","text":"hello","userDid":"did:key:z6Mkuser"}"#,
            Some("did:key:z6Mkuser"),
        ));
        assert!(!matches_transcription_user(
            r#"{"streamId":"abc","text":"hello","userDid":"did:key:z6Mkother"}"#,
            Some("did:key:z6Mkuser"),
        ));
    }

    #[test]
    fn matches_apps_user_admin_sees_all() {
        assert!(matches_apps_user(
            r#"{"auth":{"user_did":"did:key:z6Mkuser"},"request_id":"r1"}"#,
            None,
        ));
    }

    #[test]
    fn matches_apps_user_filters_correctly() {
        assert!(matches_apps_user(
            r#"{"auth":{"user_did":"did:key:z6Mkuser"},"request_id":"r1"}"#,
            Some("did:key:z6Mkuser"),
        ));
        assert!(!matches_apps_user(
            r#"{"auth":{"user_did":"did:key:z6Mkother"},"request_id":"r1"}"#,
            Some("did:key:z6Mkuser"),
        ));
    }
}

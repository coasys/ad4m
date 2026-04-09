//! SSE event endpoints: /api/v1/events/*
//!
//! 6 SSE endpoints tapping into the existing PubSub system.
<<<<<<< HEAD
=======
//!
//! Events are sent as unnamed SSE events (no `.event()` call) so that the
//! browser/client `EventSource.onmessage` handler receives them.  Each message
//! is a JSON object `{ "type": "<event-type>", ...payload }` where `payload` is
//! the original pubsub message merged into the wrapper.
>>>>>>> origin/feat/audio-transport-optimisation

use std::convert::Infallible;

use axum::{
    extract::{Path, Query, State},
    response::sse::{Event, KeepAlive, Sse},
};
use futures::stream::{self, Stream, StreamExt};
use std::collections::HashMap;
use tokio_stream::wrappers::BroadcastStream;

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

<<<<<<< HEAD
/// GET /events/agent — SSE: status-changed, apps-changed, updated
=======
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

/// GET /events/agent — SSE: agent-status-changed, apps-changed, agent-updated
/// TODO: SSE endpoints authenticate the caller but don't enforce per-resource access control.
/// Consider adding capability checks (e.g. AGENT_READ_CAPABILITY) before opening the stream.
>>>>>>> origin/feat/audio-transport-optimisation
pub async fn agent_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let pubsub = get_global_pubsub().await;

    let status_rx = pubsub.subscribe(&AGENT_STATUS_CHANGED_TOPIC).await;
    let apps_rx = pubsub.subscribe(&APPS_CHANGED).await;
    let updated_rx = pubsub.subscribe(&AGENT_UPDATED_TOPIC).await;

    let status_stream = BroadcastStream::new(status_rx)
        .filter_map(|r| async { r.ok() })
<<<<<<< HEAD
        .map(|msg| Ok(Event::default().event("status-changed").data(msg)));

    let apps_stream = BroadcastStream::new(apps_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("apps-changed").data(msg)));

    let updated_stream = BroadcastStream::new(updated_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("updated").data(msg)));
=======
        .map(|msg| Ok(Event::default().data(wrap_event("agent-status-changed", &msg))));

    let apps_stream = BroadcastStream::new(apps_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().data(wrap_event("apps-changed", &msg))));

    let updated_stream = BroadcastStream::new(updated_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().data(wrap_event("agent-updated", &msg))));
>>>>>>> origin/feat/audio-transport-optimisation

    let merged = stream::select(status_stream, stream::select(apps_stream, updated_stream));

    Sse::new(merged).keep_alive(KeepAlive::default())
}

<<<<<<< HEAD
/// GET /events/perspectives — SSE: added, removed, updated, sync-state
=======
/// GET /events/perspectives — SSE: perspective-added, perspective-removed, perspective-updated, sync-state-change
/// TODO: enforce per-perspective access control before opening SSE stream.
>>>>>>> origin/feat/audio-transport-optimisation
pub async fn perspective_lifecycle_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let pubsub = get_global_pubsub().await;

    let added_rx = pubsub.subscribe(&PERSPECTIVE_ADDED_TOPIC).await;
    let removed_rx = pubsub.subscribe(&PERSPECTIVE_REMOVED_TOPIC).await;
    let updated_rx = pubsub.subscribe(&PERSPECTIVE_UPDATED_TOPIC).await;
    let sync_rx = pubsub.subscribe(&PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC).await;

    let s1 = BroadcastStream::new(added_rx)
        .filter_map(|r| async { r.ok() })
<<<<<<< HEAD
        .map(|msg| Ok(Event::default().event("added").data(msg)));
    let s2 = BroadcastStream::new(removed_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("removed").data(msg)));
    let s3 = BroadcastStream::new(updated_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("updated").data(msg)));
    let s4 = BroadcastStream::new(sync_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("sync-state").data(msg)));
=======
        .map(|msg| Ok(Event::default().data(wrap_event("perspective-added", &msg))));
    let s2 = BroadcastStream::new(removed_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().data(wrap_event("perspective-removed", &msg))));
    let s3 = BroadcastStream::new(updated_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().data(wrap_event("perspective-updated", &msg))));
    let s4 = BroadcastStream::new(sync_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().data(wrap_event("sync-state-change", &msg))));
>>>>>>> origin/feat/audio-transport-optimisation

    let merged = stream::select(stream::select(s1, s2), stream::select(s3, s4));
    Sse::new(merged).keep_alive(KeepAlive::default())
}

/// GET /events/perspectives/:uuid/links — SSE: link-added, link-removed, link-updated
<<<<<<< HEAD
=======
/// TODO: verify caller has read access to this perspective before opening SSE stream.
>>>>>>> origin/feat/audio-transport-optimisation
pub async fn perspective_link_events(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let pubsub = get_global_pubsub().await;

    let added_rx = pubsub.subscribe(&PERSPECTIVE_LINK_ADDED_TOPIC).await;
    let removed_rx = pubsub.subscribe(&PERSPECTIVE_LINK_REMOVED_TOPIC).await;
    let updated_rx = pubsub.subscribe(&PERSPECTIVE_LINK_UPDATED_TOPIC).await;

    // Filter by perspective UUID
    let uuid_clone = uuid.clone();
    let s1 = BroadcastStream::new(added_rx)
        .filter_map(|r| async { r.ok() })
        .filter(move |msg| {
            let matches = msg.contains(&uuid_clone);
            futures::future::ready(matches)
        })
<<<<<<< HEAD
        .map(|msg| Ok(Event::default().event("link-added").data(msg)));
=======
        .map(|msg| Ok(Event::default().data(wrap_event("link-added", &msg))));
>>>>>>> origin/feat/audio-transport-optimisation

    let uuid_clone = uuid.clone();
    let s2 = BroadcastStream::new(removed_rx)
        .filter_map(|r| async { r.ok() })
        .filter(move |msg| {
            let matches = msg.contains(&uuid_clone);
            futures::future::ready(matches)
        })
<<<<<<< HEAD
        .map(|msg| Ok(Event::default().event("link-removed").data(msg)));
=======
        .map(|msg| Ok(Event::default().data(wrap_event("link-removed", &msg))));
>>>>>>> origin/feat/audio-transport-optimisation

    let uuid_clone = uuid;
    let s3 = BroadcastStream::new(updated_rx)
        .filter_map(|r| async { r.ok() })
        .filter(move |msg| {
            let matches = msg.contains(&uuid_clone);
            futures::future::ready(matches)
        })
<<<<<<< HEAD
        .map(|msg| Ok(Event::default().event("link-updated").data(msg)));
=======
        .map(|msg| Ok(Event::default().data(wrap_event("link-updated", &msg))));
>>>>>>> origin/feat/audio-transport-optimisation

    let merged = stream::select(s1, stream::select(s2, s3));
    Sse::new(merged).keep_alive(KeepAlive::default())
}

/// GET /events/neighbourhoods/:uuid/signals — SSE: signal
<<<<<<< HEAD
=======
/// TODO: verify caller has read access to this neighbourhood before opening SSE stream.
>>>>>>> origin/feat/audio-transport-optimisation
pub async fn neighbourhood_signal_events(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(uuid): Path<String>,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let pubsub = get_global_pubsub().await;
    let rx = pubsub.subscribe(&NEIGHBOURHOOD_SIGNAL_TOPIC).await;

    let stream = BroadcastStream::new(rx)
        .filter_map(|r| async { r.ok() })
        .filter(move |msg| {
            let matches = msg.contains(&uuid);
            futures::future::ready(matches)
        })
<<<<<<< HEAD
        .map(|msg| Ok(Event::default().event("signal").data(msg)));
=======
        .map(|msg| Ok(Event::default().data(wrap_event("signal", &msg))));
>>>>>>> origin/feat/audio-transport-optimisation

    Sse::new(stream).keep_alive(KeepAlive::default())
}

<<<<<<< HEAD
/// GET /events/runtime — SSE: message-received, notification-triggered, exception
=======
/// GET /events/runtime — SSE: message-received, notification-triggered, exception-occurred
/// TODO: enforce capability checks before opening SSE stream.
>>>>>>> origin/feat/audio-transport-optimisation
pub async fn runtime_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let pubsub = get_global_pubsub().await;

    let msg_rx = pubsub.subscribe(&RUNTIME_MESSAGED_RECEIVED_TOPIC).await;
    let notif_rx = pubsub
        .subscribe(&RUNTIME_NOTIFICATION_TRIGGERED_TOPIC)
        .await;
    let exc_rx = pubsub.subscribe(&EXCEPTION_OCCURRED_TOPIC).await;

    let s1 = BroadcastStream::new(msg_rx)
        .filter_map(|r| async { r.ok() })
<<<<<<< HEAD
        .map(|msg| Ok(Event::default().event("message-received").data(msg)));
    let s2 = BroadcastStream::new(notif_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("notification-triggered").data(msg)));
    let s3 = BroadcastStream::new(exc_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("exception").data(msg)));
=======
        .map(|msg| Ok(Event::default().data(wrap_event("message-received", &msg))));
    let s2 = BroadcastStream::new(notif_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().data(wrap_event("notification-triggered", &msg))));
    let s3 = BroadcastStream::new(exc_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().data(wrap_event("exception-occurred", &msg))));
>>>>>>> origin/feat/audio-transport-optimisation

    let merged = stream::select(s1, stream::select(s2, s3));
    Sse::new(merged).keep_alive(KeepAlive::default())
}

/// GET /events/ai — SSE: transcription-text, model-loading-status
<<<<<<< HEAD
=======
/// TODO: enforce capability checks before opening SSE stream.
>>>>>>> origin/feat/audio-transport-optimisation
pub async fn ai_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let pubsub = get_global_pubsub().await;

    let trans_rx = pubsub.subscribe(&AI_TRANSCRIPTION_TEXT_TOPIC).await;
    let loading_rx = pubsub.subscribe(&AI_MODEL_LOADING_STATUS).await;

    let s1 = BroadcastStream::new(trans_rx)
        .filter_map(|r| async { r.ok() })
<<<<<<< HEAD
        .map(|msg| Ok(Event::default().event("transcription-text").data(msg)));
    let s2 = BroadcastStream::new(loading_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("model-loading-status").data(msg)));
=======
        .map(|msg| Ok(Event::default().data(wrap_event("transcription-text", &msg))));
    let s2 = BroadcastStream::new(loading_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().data(wrap_event("model-loading-status", &msg))));
>>>>>>> origin/feat/audio-transport-optimisation

    let merged = stream::select(s1, s2);
    Sse::new(merged).keep_alive(KeepAlive::default())
}

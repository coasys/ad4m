//! SSE event endpoints: /api/v1/events/*
//!
//! 6 SSE endpoints tapping into the existing PubSub system.

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

/// GET /events/agent — SSE: status-changed, apps-changed, updated
/// TODO: SSE endpoints authenticate the caller but don't enforce per-resource access control.
/// Consider adding capability checks (e.g. AGENT_READ_CAPABILITY) before opening the stream.
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
        .map(|msg| Ok(Event::default().event("status-changed").data(msg)));

    let apps_stream = BroadcastStream::new(apps_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("apps-changed").data(msg)));

    let updated_stream = BroadcastStream::new(updated_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("updated").data(msg)));

    let merged = stream::select(status_stream, stream::select(apps_stream, updated_stream));

    Sse::new(merged).keep_alive(KeepAlive::default())
}

/// GET /events/perspectives — SSE: added, removed, updated, sync-state
/// TODO: enforce per-perspective access control before opening SSE stream.
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

    let merged = stream::select(stream::select(s1, s2), stream::select(s3, s4));
    Sse::new(merged).keep_alive(KeepAlive::default())
}

/// GET /events/perspectives/:uuid/links — SSE: link-added, link-removed, link-updated
/// TODO: verify caller has read access to this perspective before opening SSE stream.
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
        .map(|msg| Ok(Event::default().event("link-added").data(msg)));

    let uuid_clone = uuid.clone();
    let s2 = BroadcastStream::new(removed_rx)
        .filter_map(|r| async { r.ok() })
        .filter(move |msg| {
            let matches = msg.contains(&uuid_clone);
            futures::future::ready(matches)
        })
        .map(|msg| Ok(Event::default().event("link-removed").data(msg)));

    let uuid_clone = uuid;
    let s3 = BroadcastStream::new(updated_rx)
        .filter_map(|r| async { r.ok() })
        .filter(move |msg| {
            let matches = msg.contains(&uuid_clone);
            futures::future::ready(matches)
        })
        .map(|msg| Ok(Event::default().event("link-updated").data(msg)));

    let merged = stream::select(s1, stream::select(s2, s3));
    Sse::new(merged).keep_alive(KeepAlive::default())
}

/// GET /events/neighbourhoods/:uuid/signals — SSE: signal
/// TODO: verify caller has read access to this neighbourhood before opening SSE stream.
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
        .map(|msg| Ok(Event::default().event("signal").data(msg)));

    Sse::new(stream).keep_alive(KeepAlive::default())
}

/// GET /events/runtime — SSE: message-received, notification-triggered, exception
/// TODO: enforce capability checks before opening SSE stream.
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
        .map(|msg| Ok(Event::default().event("message-received").data(msg)));
    let s2 = BroadcastStream::new(notif_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("notification-triggered").data(msg)));
    let s3 = BroadcastStream::new(exc_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("exception-occurred").data(msg)));

    let merged = stream::select(s1, stream::select(s2, s3));
    Sse::new(merged).keep_alive(KeepAlive::default())
}

/// GET /events/ai — SSE: transcription-text, model-loading-status
/// TODO: enforce capability checks before opening SSE stream.
pub async fn ai_events(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let pubsub = get_global_pubsub().await;

    let trans_rx = pubsub.subscribe(&AI_TRANSCRIPTION_TEXT_TOPIC).await;
    let loading_rx = pubsub.subscribe(&AI_MODEL_LOADING_STATUS).await;

    let s1 = BroadcastStream::new(trans_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("transcription-text").data(msg)));
    let s2 = BroadcastStream::new(loading_rx)
        .filter_map(|r| async { r.ok() })
        .map(|msg| Ok(Event::default().event("model-loading-status").data(msg)));

    let merged = stream::select(s1, s2);
    Sse::new(merged).keep_alive(KeepAlive::default())
}

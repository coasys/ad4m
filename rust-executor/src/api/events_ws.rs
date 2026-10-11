//! WebSocket event endpoint: GET /api/v1/ws/events
//!
//! Single WebSocket endpoint serving ALL event types.
//! Each message is a JSON object `{ "type": "<event-type>", ...payload }`.
//! [`event_specs`] types every payload (exported to the SDK as `Events.ts`);
//! each perspective-scoped payload carries a flat `perspectiveUuid`.
//! Events are filtered per-user in multi-user mode, then to what the socket
//! asked for with `events.watch` (nothing until it does).
//!
//! Typed [`Event`](crate::events::Event)s from `PubSub::publish_event` reach a
//! socket when `Session::may_receive` (the audience and the topic's capability,
//! `Topic::capability`) and `event_interest::wants` hold, as [`wire_message`].
//! One string topic remains: live query updates, which need the capability to
//! query the perspective and wait for its owners (`matches_perspective_owner`)
//! until live queries belong to a connection (#1207).
//!
//! ## Event types
//!
//! "Owners" is `perspectives::event_recipients`: the owners, or the main agent
//! for an unowned perspective.
//!
//! | Type                          | Payload key   | Filtering              | Description                          |
//! |-------------------------------|---------------|------------------------|--------------------------------------|
//! | `agent-status-changed`        | `agent`       | typed: the agent's DID | Agent status changed                 |
//! | `agent-updated`               | `agent`       | typed: the agent's DID | Agent profile updated                |
//! | `apps-changed`                | (inline)      | typed: approvers       | Installed apps changed (no token)    |
//! | `hosting-user-info-changed`   | (inline)      | typed: the user + main agent | Hosting user info changed      |
//! | `perspective-added`           | (inline)      | typed: `owner` only    | New perspective created              |
//! | `perspective-removed`         | (inline)      | typed: `owner` only    | Perspective deleted                  |
//! | `perspective-updated`         | (inline)      | typed: `owner` only    | Perspective metadata updated         |
//! | `sync-state-change`           | (inline)      | typed: owners          | Neighbourhood sync state changed     |
//! | `link-added`                  | (inline)      | typed: `owner` only    | Link added to perspective            |
//! | `link-removed`                | (inline)      | typed: `owner` only    | Link removed from perspective        |
//! | `link-updated`                | (inline)      | typed: `owner` only    | Link updated in perspective          |
//! | `signal`                      | (inline)      | typed: `recipient`, else each owner | Neighbourhood signal received |
//! | `message-received`            | `message`     | typed: the agent it is for (no publisher yet) | Runtime message received |
//! | `notification-triggered`      | `notification` + `perspectiveUuid` | typed: notification owner DID | Notification triggered |
//! | `exception-occurred`          | `exception`   | typed: per type, see `Audience::for_exception` | Exception occurred |
//! | `transcription-text`          | (inline)      | typed: the stream owner's DID | AI transcription text         |
//! | `model-loading-status`        | (inline)      | typed: everyone        | AI model loading status              |
//! | `query-subscription-update`   | (inline)      | string: perspective owner | Live query subscription update    |
//! | `auto-processor-event`        | (inline)      | typed: operator + pass owner | Auto-processor pass step signal |
//! | `auto-processor-neighbourhood-state` | (inline) | typed: operator + readers | Coarse-grained neighbourhood view of "someone is auto-processing" |
//!
//! ## Client → Server messages
//!
//! | Type     | Description                          |
//! |----------|--------------------------------------|
//! | `ping`   | Server responds with `{"type":"pong"}`|
//! | `events.watch` / `events.unwatch` | Choose the events this socket gets (none until the first watch); see `event_interest.rs`. Same on `/api/v1/ws` |
//!
//! Other messages are silently ignored (future extensibility).

use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        State,
    },
    response::Response,
};
use futures::stream::StreamExt;
use std::pin::Pin;
use std::sync::Arc;
use tokio_stream::wrappers::{errors::BroadcastStreamRecvError, BroadcastStream};

use super::event_interest::{self, SharedInterest};
use crate::agent::capabilities::*;
use crate::agent::session::Session;
use crate::agent::session_registry::SessionRegistry;
use crate::agent::{did_for_context, AgentContext};
use crate::events::{Event, Topic};
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use crate::types::{
    AIModelLoadingStatus, Agent, AgentStatus, Apps, ExceptionInfo, HostingUserInfo,
    NeighbourhoodSignalFilter, NotificationTriggeredEvent, PerspectiveExpression,
    PerspectiveLinkUpdatedWithOwner, PerspectiveLinkWithOwner, PerspectiveQuerySubscriptionFilter,
    PerspectiveRemovedWithOwner, PerspectiveStateFilter, PerspectiveWithOwner,
    TranscriptionTextFilter,
};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// The name of every event this socket emits. `Topic::as_str` and
/// [`event_specs`] both use these, so an event cannot go out untyped.
pub mod events {
    pub const AGENT_STATUS_CHANGED: &str = "agent-status-changed";
    pub const AGENT_UPDATED: &str = "agent-updated";
    pub const APPS_CHANGED_EVENT: &str = "apps-changed";
    pub const HOSTING_USER_INFO_CHANGED: &str = "hosting-user-info-changed";
    pub const PERSPECTIVE_ADDED: &str = "perspective-added";
    pub const PERSPECTIVE_REMOVED: &str = "perspective-removed";
    pub const PERSPECTIVE_UPDATED: &str = "perspective-updated";
    pub const SYNC_STATE_CHANGE: &str = "sync-state-change";
    pub const LINK_ADDED: &str = "link-added";
    pub const LINK_REMOVED: &str = "link-removed";
    pub const LINK_UPDATED: &str = "link-updated";
    pub const SIGNAL: &str = "signal";
    pub const MESSAGE_RECEIVED: &str = "message-received";
    pub const NOTIFICATION_TRIGGERED: &str = "notification-triggered";
    pub const EXCEPTION_OCCURRED: &str = "exception-occurred";
    pub const TRANSCRIPTION_TEXT: &str = "transcription-text";
    pub const MODEL_LOADING_STATUS: &str = "model-loading-status";
    pub const QUERY_SUBSCRIPTION_UPDATE: &str = "query-subscription-update";
    pub const AUTO_PROCESSOR_EVENT: &str = "auto-processor-event";
    pub const AUTO_PROCESSOR_NEIGHBOURHOOD_STATE: &str = "auto-processor-neighbourhood-state";
}

/// One emitted event: its name, the TypeScript type of its payload (the
/// wire message without `type`) and whether it is about one perspective
/// (carries `perspectiveUuid`, so `events.watch` can narrow it).
pub struct EventSpec {
    pub name: &'static str,
    pub payload: super::ws_handler::TsType,
    pub scoped: bool,
}

impl EventSpec {
    fn of<T: ts_rs::TS + 'static>(name: &'static str, scoped: bool) -> Self {
        Self {
            name,
            payload: super::ws_handler::TsType::of::<T>(),
            scoped,
        }
    }
}

/// `agent-status-changed`: `{ agent }`.
#[derive(Debug, Serialize, Deserialize, TS)]
pub struct AgentStatusChangedEvent {
    pub agent: AgentStatus,
}

/// `agent-updated`: `{ agent }`.
#[derive(Debug, Serialize, Deserialize, TS)]
pub struct AgentUpdatedEvent {
    pub agent: Agent,
}

/// `message-received`: `{ message }`.
#[derive(Debug, Serialize, Deserialize, TS)]
pub struct MessageReceivedEvent {
    pub message: PerspectiveExpression,
}

/// `exception-occurred`: `{ exception }`.
#[derive(Debug, Serialize, Deserialize, TS)]
pub struct ExceptionOccurredEvent {
    pub exception: ExceptionInfo,
}

/// Every event the socket emits, with its payload type.
pub fn event_specs() -> Vec<EventSpec> {
    use crate::perspectives::auto_processor::events::{
        AutoProcessorEvent, AutoProcessorNeighbourhoodState,
    };
    use events::*;
    vec![
        EventSpec::of::<AgentStatusChangedEvent>(AGENT_STATUS_CHANGED, false),
        EventSpec::of::<AgentUpdatedEvent>(AGENT_UPDATED, false),
        EventSpec::of::<Apps>(APPS_CHANGED_EVENT, false),
        EventSpec::of::<HostingUserInfo>(HOSTING_USER_INFO_CHANGED, false),
        EventSpec::of::<PerspectiveWithOwner>(PERSPECTIVE_ADDED, true),
        EventSpec::of::<PerspectiveRemovedWithOwner>(PERSPECTIVE_REMOVED, true),
        EventSpec::of::<PerspectiveWithOwner>(PERSPECTIVE_UPDATED, true),
        EventSpec::of::<PerspectiveStateFilter>(SYNC_STATE_CHANGE, true),
        EventSpec::of::<PerspectiveLinkWithOwner>(LINK_ADDED, true),
        EventSpec::of::<PerspectiveLinkWithOwner>(LINK_REMOVED, true),
        EventSpec::of::<PerspectiveLinkUpdatedWithOwner>(LINK_UPDATED, true),
        EventSpec::of::<NeighbourhoodSignalFilter>(SIGNAL, true),
        EventSpec::of::<MessageReceivedEvent>(MESSAGE_RECEIVED, false),
        EventSpec::of::<NotificationTriggeredEvent>(NOTIFICATION_TRIGGERED, true),
        EventSpec::of::<ExceptionOccurredEvent>(EXCEPTION_OCCURRED, false),
        EventSpec::of::<TranscriptionTextFilter>(TRANSCRIPTION_TEXT, false),
        EventSpec::of::<AIModelLoadingStatus>(MODEL_LOADING_STATUS, false),
        EventSpec::of::<PerspectiveQuerySubscriptionFilter>(QUERY_SUBSCRIPTION_UPDATE, true),
        EventSpec::of::<AutoProcessorEvent>(AUTO_PROCESSOR_EVENT, true),
        EventSpec::of::<AutoProcessorNeighbourhoodState>(AUTO_PROCESSOR_NEIGHBOURHOOD_STATE, true),
    ]
}

/// GET /ws/events — WebSocket endpoint for all real-time events.
///
/// Authentication is via `token` query parameter or `Authorization` header.
/// All event types are multiplexed over a single WebSocket connection.
///
/// A token that opens no session gets the upgrade, then a close with 1008 at once: a browser
/// cannot read the HTTP status of a refused upgrade.
pub async fn events_ws(
    ws: WebSocketUpgrade,
    State(_state): State<AppState>,
    auth: Result<AuthContext, ApiError>,
) -> Result<Response, ApiError> {
    let auth = match auth {
        Ok(auth) => auth,
        Err(ApiError::Unauthorized(reason)) => {
            return Ok(ws.on_upgrade(move |socket| {
                super::ws_rpc::close_refused(socket, super::ws_rpc::CLOSE_TOKEN_ENDED, reason)
            }))
        }
        Err(ApiError::Unavailable(reason)) => {
            return Ok(ws.on_upgrade(move |socket| {
                super::ws_rpc::close_refused(socket, super::ws_rpc::CLOSE_TRY_AGAIN_LATER, reason)
            }))
        }
        Err(e) => return Err(e),
    };
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let user_email = context.user_email().map(str::to_string);
    let session = context.session.clone();

    Ok(ws.on_upgrade(move |socket| handle_events_ws(socket, session, user_email)))
}

/// Build the merged event stream for a given user.
///
/// Returns a boxed stream of JSON-stringified event messages, already filtered
/// per-user and to `interest` (what the socket asked for with `events.watch`).
/// The stream ends at the first event after `session.check()` fails.
pub(crate) async fn build_event_stream(
    session: Arc<Session>,
    interest: SharedInterest,
    user_email: Option<String>,
) -> Pin<Box<dyn futures::stream::Stream<Item = String> + Send>> {
    // The session's own agent, taken from the session: a token that stops decoding cannot
    // turn it into the main agent. Resolved once at subscription time, for the live-query
    // filter; `None` (a client that connected before `agent.generate()`) lets every update
    // through, as single-user mode.
    let resolved_did = did_for_context(&AgentContext::for_session(user_email)).ok();
    build_event_stream_for(session, interest, resolved_did).await
}

/// [`build_event_stream`] with the session DID already resolved (tests
/// inject one directly).
pub(crate) async fn build_event_stream_for(
    session: Arc<Session>,
    interest: SharedInterest,
    resolved_did: Option<String>,
) -> Pin<Box<dyn futures::stream::Stream<Item = String> + Send>> {
    use futures::future::ready;

    let pubsub = get_global_pubsub().await;
    let query_updates = query_updates(
        pubsub
            .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
            .await,
        resolved_did,
        session.clone(),
    );
    // The session decides who, the socket's interest what.
    let typed = {
        let session = session.clone();
        BroadcastStream::new(pubsub.subscribe_events())
            .filter_map(received)
            .filter_map(move |event| {
                ready(session.may_receive(&event).then(|| {
                    event_interest::wants(&interest, &event).then(|| wire_message(&event))
                }))
            })
    };

    // Events end with the token: expiry and revokeToken() stop them as they stop requests.
    Box::pin(
        futures::stream::select(query_updates, typed)
            .take_while(move |_| ready(session.check().is_ok()))
            .filter_map(ready),
    )
}

/// The live query updates of the perspectives `did` owns (all of them for `None`) that
/// `session` may query, as the stream builder's items: `Some(message)`, or `None` for an
/// update that is not JSON. They pass `events.watch` untouched.
fn query_updates(
    updates: tokio::sync::broadcast::Receiver<String>,
    did: Option<String>,
    session: Arc<Session>,
) -> impl futures::stream::Stream<Item = Option<String>> + Send {
    BroadcastStream::new(updates)
        .filter_map(received)
        .filter_map(move |msg| {
            let did = did.clone();
            let session = session.clone();
            async move {
                let topic = Topic::PerspectiveQuerySubscription;
                let readable =
                    session.may(&topic.capability(event_perspective_uuid(&msg).as_deref()));
                (readable && matches_perspective_owner(&msg, did.as_deref()).await).then(|| {
                    serde_json::from_str(&msg)
                        .ok()
                        .map(|payload| wire(topic, &payload))
                })
            }
        })
}

/// The socket message for `event`, built without parsing the payload again.
pub(crate) fn wire_message(event: &Event) -> String {
    wire(event.topic, &event.payload)
}

/// The socket message for a `topic` event with `payload`.
fn wire(topic: Topic, payload: &serde_json::Value) -> String {
    let event_type = topic.as_str();
    if let Some(key) = payload_key(topic) {
        return wrap_event_nested(event_type, key, &payload.to_string());
    }
    match payload {
        serde_json::Value::Object(map) => wrap_object(event_type, map.clone())
            .unwrap_or_else(|| wrap_data(event_type, &payload.to_string())),
        other => wrap_data(event_type, &other.to_string()),
    }
}

/// The key that holds the payload of a topic whose message nests it
/// (`{"type":"agent-updated","agent":{…}}`). The other topics put the payload's
/// fields beside `type`.
fn payload_key(topic: Topic) -> Option<&'static str> {
    match topic {
        Topic::AgentStatusChanged | Topic::AgentUpdated => Some("agent"),
        Topic::RuntimeMessageReceived => Some("message"),
        Topic::ExceptionOccurred => Some("exception"),
        // No `_` arm: a new topic does not compile until someone picks its shape.
        Topic::AppsChanged
        | Topic::HostingUserInfoChanged
        | Topic::PerspectiveAdded
        | Topic::PerspectiveRemoved
        | Topic::PerspectiveUpdated
        | Topic::PerspectiveSyncStateChange
        | Topic::LinkAdded
        | Topic::LinkRemoved
        | Topic::LinkUpdated
        | Topic::NeighbourhoodSignal
        | Topic::RuntimeNotificationTriggered
        | Topic::AiTranscriptionText
        | Topic::AiModelLoadingStatus
        | Topic::PerspectiveQuerySubscription
        | Topic::AutoProcessorEvent
        | Topic::AutoProcessorNeighbourhoodState => None,
    }
}

async fn handle_events_ws(
    mut socket: WebSocket,
    session: Arc<Session>,
    user_email: Option<String>,
) {
    log::info!("Events WebSocket connected");

    // Fires when the token is revoked: the socket closes at once, not at the next event.
    let revoked = tokio_util::sync::CancellationToken::new();
    let _registered = SessionRegistry::global().register(&session, revoked.clone());

    let interest = SharedInterest::default();
    let mut event_stream = build_event_stream(session, interest.clone(), user_email).await;

    loop {
        tokio::select! {
            _ = revoked.cancelled() => {
                super::ws_rpc::close_token_ended(&mut socket).await;
                break;
            }
            // Forward events from PubSub to WebSocket
            event = event_stream.next() => {
                match event {
                    Some(data) => {
                        if socket.send(Message::Text(data.into())).await.is_err() {
                            break;
                        }
                    }
                    // The stream ends when the token stops working.
                    None => {
                        super::ws_rpc::close_token_ended(&mut socket).await;
                        break;
                    }
                }
            }
            // Handle incoming WebSocket messages
            msg = socket.recv() => {
                match msg {
                    Some(Ok(Message::Text(text))) => {
                        if let Some(reply) = client_message_reply(&text, &interest) {
                            let _ = socket.send(Message::Text(reply.into())).await;
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

/// The reply to a client message on the events socket: `pong` for `ping`,
/// the `events.watch` / `events.unwatch` reply, or `None` (ignored).
fn client_message_reply(text: &str, interest: &SharedInterest) -> Option<String> {
    let parsed: serde_json::Value = serde_json::from_str(text).ok()?;
    let msg_type = parsed.get("type").and_then(|v| v.as_str()).unwrap_or("");
    if msg_type == "ping" {
        return Some(r#"{"type":"pong"}"#.to_string());
    }
    event_interest::handle_control(
        msg_type,
        parsed.get("id").unwrap_or(&serde_json::Value::Null),
        parsed.get("params").unwrap_or(&serde_json::json!({})),
        interest,
    )
}

// ── Event helper functions ──────────────────────────────────────────────────

/// The message a socket's receiver got, or `None` when it lagged.
fn received<T>(r: Result<T, BroadcastStreamRecvError>) -> futures::future::Ready<Option<T>> {
    futures::future::ready(match r {
        Ok(msg) => Some(msg),
        Err(BroadcastStreamRecvError::Lagged(n)) => {
            // The socket's receiver fell more than the channel's capacity behind (a slow
            // client, or an ownership filter waiting on a perspective lock), so these events
            // are gone for this socket. Say so: the client cannot know it missed them.
            log::warn!("events_ws: a socket's event stream lagged and dropped {n} event(s)");
            None
        }
    })
}

/// An object payload's fields with `type` beside them.
fn wrap_object(
    event_type: &str,
    mut map: serde_json::Map<String, serde_json::Value>,
) -> Option<String> {
    map.insert(
        "type".to_string(),
        serde_json::Value::String(event_type.to_string()),
    );
    serde_json::to_string(&map).ok()
}

/// A payload that is not an object, under `data`.
fn wrap_data(event_type: &str, raw_json: &str) -> String {
    format!(r#"{{"type":"{}","data":{}}}"#, event_type, raw_json)
}

/// A payload under `payload_key` (`{"type":"agent-updated","agent":{…}}`).
fn wrap_event_nested(event_type: &str, payload_key: &str, raw_json: &str) -> String {
    format!(
        r#"{{"type":"{}","{}":{}}}"#,
        event_type, payload_key, raw_json
    )
}

/// Multi-user: only live query updates about a perspective the session owns. An
/// update without `perspectiveUuid`, or one that does not parse, reaches nobody:
/// the publisher sets the field.
async fn matches_perspective_owner(msg: &str, current_did: Option<&str>) -> bool {
    let Some(did) = current_did else {
        return true;
    };
    match event_perspective_uuid(msg) {
        Some(uuid) => perspective_is_owned_by(&uuid, did).await,
        None => false,
    }
}

/// The event's `perspectiveUuid`, if `msg` is a JSON object holding one.
fn event_perspective_uuid(msg: &str) -> Option<String> {
    serde_json::from_str::<serde_json::Value>(msg)
        .ok()?
        .get("perspectiveUuid")?
        .as_str()
        .map(str::to_string)
}

/// Whether `did` owns (or nobody owns) perspective `uuid`. Fails closed for an
/// unknown perspective. Waits for the handle: a `try_lock` here dropped every
/// event published while a write held it, so live queries missed updates.
async fn perspective_is_owned_by(uuid: &str, did: &str) -> bool {
    let Some(instance) = crate::perspectives::get_perspective(uuid) else {
        return false;
    };
    let did = did.to_string();
    // Spawned so the runtime polls the lock, not the socket's event stream. The
    // tokio Mutex hands the permit to the first waiter, which keeps it until it
    // is polled; a stream parked behind a slow socket send would hold the
    // handle, and stall every writer of the perspective.
    tokio::spawn(async move {
        let handle = instance.persisted.lock().await;
        handle.is_unowned() || handle.is_owned_by(&did)
    })
    .await
    .unwrap_or(false)
}

#[cfg(test)]
mod perspective_is_owned_by_tests {
    //! `perspective_is_owned_by` fails closed for a perspective that is not
    //! registered (CodeRabbit #903), and waits for a perspective whose handle
    //! a write holds instead of failing: a failed `try_lock` here used to drop
    //! live-query updates published during writes.
    use super::{matches_perspective_owner, perspective_is_owned_by, query_updates};
    use crate::agent::capabilities::{perspective_query_capability, ListenerReach};
    use crate::agent::session::{Principal, Session, TokenId};
    use crate::perspectives::perspective_instance::PerspectiveInstance;
    use crate::perspectives::{register_perspective, unregister_perspective};
    use crate::types::PerspectiveHandle;
    use futures::StreamExt;
    use std::sync::Arc;
    use std::time::Duration;

    fn operator() -> Arc<Session> {
        Arc::new(Session::for_test(
            Principal::Operator,
            ListenerReach::Loopback,
        ))
    }

    // #1284: a live query update reaches a socket only if its session may query the
    // perspective, as `perspective.subscribeQuery` requires. An app acts as the main agent,
    // so the owner check alone let an app read the updates of every perspective.
    #[tokio::test]
    async fn a_live_query_update_needs_the_capability_to_query_its_perspective() {
        let app = Arc::new(Session::for_test(
            Principal::App {
                token_id: TokenId("live-query-app".into()),
                caps: vec![perspective_query_capability(vec!["readable".into()])],
            },
            ListenerReach::Network,
        ));
        let (tx, rx) = tokio::sync::broadcast::channel::<String>(16);
        let mut stream = Box::pin(query_updates(rx, None, app));
        for uuid in ["other", "readable"] {
            let update = serde_json::json!({ "perspectiveUuid": uuid, "subscriptionId": "s" });
            tx.send(update.to_string()).unwrap();
        }
        let sent = stream.next().await.unwrap().expect("the readable update");
        let sent: serde_json::Value = serde_json::from_str(&sent).unwrap();
        assert_eq!(sent["perspectiveUuid"], "readable");
    }

    #[tokio::test]
    async fn missing_perspective_returns_false_for_any_did() {
        for did in ["did:key:alice", "", "did:key:admin"] {
            assert!(!perspective_is_owned_by("not-a-registered-perspective", did).await);
        }
    }

    #[tokio::test]
    async fn waits_for_a_handle_a_write_holds() {
        let handle = PerspectiveHandle::new_with_owner("owned".into(), "did:key:alice".into());
        let uuid = handle.uuid.clone();
        let instance = PerspectiveInstance::new(handle, None);
        let persisted = instance.persisted.clone();
        register_perspective(uuid.clone(), instance);

        let write = persisted.lock().await;
        let update =
            serde_json::json!({ "perspectiveUuid": uuid, "subscriptionId": "s" }).to_string();
        let check =
            tokio::spawn(
                async move { matches_perspective_owner(&update, Some("did:key:alice")).await },
            );
        tokio::time::sleep(Duration::from_millis(50)).await;
        assert!(
            !check.is_finished(),
            "the check waits for the write, it does not fail"
        );
        drop(write);
        assert!(
            check.await.unwrap(),
            "the owner receives the update once the write ends"
        );

        assert!(!perspective_is_owned_by(&uuid, "did:key:bob").await);
        unregister_perspective(&uuid);
    }

    #[tokio::test]
    async fn a_parked_check_does_not_keep_the_handle_after_the_write() {
        let handle = PerspectiveHandle::new_with_owner("p".into(), "did:key:alice".into());
        let uuid = handle.uuid.clone();
        let instance = PerspectiveInstance::new(handle, None);
        let persisted = instance.persisted.clone();
        register_perspective(uuid.clone(), instance);
        let (tx, rx) = tokio::sync::broadcast::channel::<String>(16);
        let mut stream = Box::pin(query_updates(rx, Some("did:key:bob".into()), operator()));
        let write = persisted.lock().await;
        let update = serde_json::json!({ "perspectiveUuid": uuid, "subscriptionId": "s" });
        tx.send(update.to_string()).unwrap();
        assert!(futures::poll!(stream.next()).is_pending());
        drop(write);
        // The socket loop is busy in `socket.send` and does not poll the stream.
        let next_writer = tokio::time::timeout(Duration::from_millis(300), persisted.lock()).await;
        let parked = next_writer.is_err();
        drop(next_writer);
        unregister_perspective(&uuid);
        assert!(
            !parked,
            "a parked ownership check holds the handle after the write released it"
        );
    }

    // An update goes out as its fields beside `type`; one that is not JSON still reaches the
    // stream, which ends there if the token stopped working, but sends nothing.
    #[tokio::test]
    async fn a_live_query_update_goes_out_as_its_fields_beside_its_type() {
        let (tx, rx) = tokio::sync::broadcast::channel::<String>(16);
        let mut stream = Box::pin(query_updates(rx, None, operator()));
        let update = r#"{"perspectiveUuid":"p","uuid":"p","subscriptionId":"s","result":"[]"}"#;
        tx.send(update.into()).unwrap();
        tx.send("not json".into()).unwrap();
        let sent = stream.next().await.unwrap().expect("a JSON update is sent");
        assert_eq!(
            serde_json::from_str::<serde_json::Value>(&sent).unwrap(),
            serde_json::json!({
                "type": "query-subscription-update",
                "perspectiveUuid": "p",
                "uuid": "p",
                "subscriptionId": "s",
                "result": "[]",
            })
        );
        assert_eq!(stream.next().await, Some(None));
    }
}

#[cfg(test)]
mod events_socket_message_tests {
    use crate::api::event_interest::SharedInterest;
    use serde_json::json;

    #[test]
    fn events_socket_watch_round_trip_carries_the_request_id() {
        let interest: SharedInterest = Default::default();
        let reply = |text: &str| -> serde_json::Value {
            serde_json::from_str(&super::client_message_reply(text, &interest).unwrap()).unwrap()
        };
        assert_eq!(
            reply(r#"{"id":"w1","type":"events.watch","params":{"agent-updated":null}}"#),
            json!({ "id": "w1", "result": true })
        );
        assert_eq!(
            reply(r#"{"id":"u1","type":"events.unwatch"}"#),
            json!({ "id": "u1", "result": true })
        );
        assert_eq!(reply(r#"{"type":"ping"}"#), json!({ "type": "pong" }));
        assert!(super::client_message_reply(r#"{"type":"other"}"#, &interest).is_none());
        assert!(super::client_message_reply("not json", &interest).is_none());
    }
}

#[cfg(test)]
mod app_approval_event_tests {
    use super::*;
    use crate::agent::session::{Principal, TokenId};
    use crate::events::Audience;
    use std::collections::BTreeSet;
    use std::time::Duration;

    fn app(name: &str, caps: Vec<Capability>) -> Arc<Session> {
        let token_id = TokenId(format!("approval-{name}"));
        let app = Principal::App { token_id, caps };
        Arc::new(Session::for_test(app, ListenerReach::Network))
    }

    /// The event types `stream` delivers that mention `request_id`, until the fence (a
    /// message with `fence` and the id).
    async fn types_for(
        stream: &mut Pin<Box<dyn futures::stream::Stream<Item = String> + Send>>,
        request_id: &str,
    ) -> BTreeSet<String> {
        let mut types = BTreeSet::new();
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let msg = stream.next().await.expect("the stream stays open");
                if !msg.contains(request_id) {
                    continue; // another test's event
                }
                if msg.contains("\"fence\"") {
                    return;
                }
                let v: serde_json::Value = serde_json::from_str(&msg).unwrap();
                types.insert(v["type"].as_str().unwrap().to_string());
            }
        })
        .await
        .unwrap_or_else(|_| panic!("the fence never arrived; got {types:?}"));
        types
    }

    // An app token used to reach every connected socket through `apps-changed`, including
    // sockets that never authenticated. The real publishers of an app approval: the
    // capability request (`exception-occurred`) and the approval (`apps-changed`) reach the
    // sessions that may approve apps (`AGENT_PERMIT`, the rule of `can_approve_apps`), and no
    // other session.
    #[tokio::test]
    async fn app_approval_events_reach_only_sessions_that_can_approve_apps() {
        use crate::agent::capabilities::{
            generate_capability_token, permit_capability, request_capability, AuthInfo,
            AuthInfoExtended,
        };
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let operator = Arc::new(Session::for_test(
            Principal::Operator,
            ListenerReach::Loopback,
        ));
        let user = Principal::User {
            token_id: TokenId("approval-user".into()),
            email: "approval.user@example.org".into(),
            did: "did:key:approval-user".into(),
            caps: crate::agent::capabilities::get_user_default_capabilities(),
        };
        // A socket that never authenticated gets no stream and is no approver.
        let anonymous = Session::for_test(
            Principal::Anonymous { caps: vec![] },
            ListenerReach::Network,
        );
        assert!(!anonymous.is_authenticated());
        assert!(!anonymous.in_audience(&Audience::Approvers));
        let sessions = [
            ("operator", operator, true),
            (
                "permit app",
                app("permit", vec![ALL_CAPABILITY.clone()]),
                true,
            ),
            (
                "reader app",
                app("reader", vec![AGENT_READ_CAPABILITY.clone()]),
                false,
            ),
            (
                "user",
                Arc::new(Session::for_test(user, ListenerReach::Network)),
                false,
            ),
        ];
        let mut streams = Vec::new();
        for (name, session, approver) in sessions {
            let all = event_interest::watching_all();
            streams.push((name, approver, build_event_stream(session, all, None).await));
        }

        let auth = AuthInfo {
            app_name: "Approval test app".into(),
            app_desc: String::new(),
            app_domain: None,
            app_url: None,
            app_icon_path: None,
            capabilities: Some(vec![AGENT_READ_CAPABILITY.clone()]),
            user_email: None,
        };
        let request_id = request_capability(auth.clone()).await;
        let code = permit_capability(AuthInfoExtended {
            request_id: request_id.clone(),
            auth,
        })
        .unwrap();
        generate_capability_token(request_id.clone(), code)
            .await
            .unwrap();
        // Every session here may read agents (AGENT_READ), so each gets the fence.
        get_global_pubsub().await.publish_event(Event::new(
            Topic::AgentUpdated,
            Audience::Everyone,
            serde_json::json!({ "fence": request_id }),
        ));

        let approval: BTreeSet<String> = [events::EXCEPTION_OCCURRED, events::APPS_CHANGED_EVENT]
            .map(String::from)
            .into();
        for (name, approver, stream) in &mut streams {
            let expected = if *approver {
                approval.clone()
            } else {
                BTreeSet::new()
            };
            assert_eq!(types_for(stream, &request_id).await, expected, "{name}");
        }
    }

    // Goes through `build_event_stream`, so a sync-state stream that keeps the DID it saw at
    // connect time fails here. The operator's socket opens before the main agent has a DID,
    // as the launcher's does on first run.
    #[tokio::test]
    async fn sync_state_reaches_a_socket_opened_before_the_did_resolved() {
        use crate::types::{PerspectiveHandle, PerspectiveState, PerspectiveStateFilter};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main_did = crate::agent::AgentService::with_global_instance(|a| a.did.clone())
            .expect("the test agent has a DID");
        crate::agent::AgentService::with_mutable_global_instance(|a| a.did = None);
        let all = event_interest::watching_all();
        let operator = Arc::new(Session::for_test(
            Principal::Operator,
            ListenerReach::Loopback,
        ));
        let mut stream = build_event_stream(operator, all, None).await;
        crate::agent::AgentService::with_mutable_global_instance(|a| {
            a.did = Some(main_did.clone())
        });

        let event = |uuid: &str, owners: Option<Vec<String>>| {
            let perspective = PerspectiveHandle {
                uuid: uuid.to_string(),
                owners,
                ..Default::default()
            };
            Event::new(
                Topic::PerspectiveSyncStateChange,
                Audience::Dids(crate::perspectives::event_recipients(&perspective)),
                PerspectiveStateFilter {
                    perspective_uuid: uuid.to_string(),
                    state: PerspectiveState::Synced,
                    perspective,
                },
            )
        };
        let pubsub = get_global_pubsub().await;
        let someone_else = Some(vec!["did:key:someone-else".to_string()]);
        pubsub.publish_event(event("sync-of-another-agent", someone_else));
        pubsub.publish_event(event("sync-of-the-main-agent", None));

        let received = tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let event = stream.next().await.expect("the stream stays open");
                if event.contains("sync-state-change") {
                    break event;
                }
            }
        })
        .await
        .expect("the socket gets sync-state once the DID exists");
        assert!(received.contains("sync-of-the-main-agent"), "{}", received);
    }
}

#[cfg(test)]
mod typed_wire_tests {
    use super::*;
    use crate::events::Audience;
    use serde_json::{json, Value};

    /// The message the string topics sent for `topic`, before every topic was typed: the
    /// clients' wire format.
    fn string_path(topic: Topic, raw_json: &str) -> String {
        let nested = [
            (events::AGENT_STATUS_CHANGED, "agent"),
            (events::AGENT_UPDATED, "agent"),
            (events::MESSAGE_RECEIVED, "message"),
            (events::EXCEPTION_OCCURRED, "exception"),
        ];
        let name = topic.as_str();
        if let Some((_, key)) = nested.iter().find(|(n, _)| *n == name) {
            return format!(r#"{{"type":"{name}","{key}":{raw_json}}}"#);
        }
        match serde_json::from_str(raw_json) {
            Ok(Value::Object(mut map)) => {
                map.insert("type".into(), Value::String(name.into()));
                serde_json::to_string(&map).unwrap()
            }
            _ => format!(r#"{{"type":"{name}","data":{raw_json}}}"#),
        }
    }

    // A typed event goes out as the message its string topic sent: the same JSON for every
    // topic, and the same bytes where the payload's fields sit beside `type`.
    #[test]
    fn a_typed_event_sends_the_message_of_its_string_topic() {
        let payloads = [
            json!({ "perspectiveUuid": "p", "owner": "did:x", "link": { "data": { "source": "s" } } }),
            json!({ "did": "did:x", "perspective": null }),
            json!("a string"),
            json!([1, 2]),
        ];
        for topic in Topic::ALL {
            for payload in &payloads {
                let raw = serde_json::to_string(payload).unwrap();
                let typed = wire_message(&Event::new(topic, Audience::Everyone, payload));
                let string = string_path(topic, &raw);
                assert_eq!(
                    serde_json::from_str::<Value>(&typed).unwrap(),
                    serde_json::from_str::<Value>(&string).unwrap(),
                    "{topic}: {raw}"
                );
                if payload_key(topic).is_none() {
                    assert_eq!(typed, string, "{topic}: {raw}");
                }
            }
        }
    }
}

#[cfg(test)]
mod audience_matrix_tests {
    //! Plan 20 §8.2: for each (topic, audience) case, which sessions' event streams deliver
    //! the event. Each case goes through `Session::may_receive` and through a real stream
    //! from `build_event_stream_for`. A new topic or audience adds its cases here.
    use super::*;
    use crate::agent::session::{Principal, TokenId};
    use crate::events::Audience;
    use std::collections::BTreeSet;
    use std::time::Duration;

    const A: &str = "did:key:matrix-user-a";
    const B: &str = "did:key:matrix-user-b";
    /// The perspective the "P reader" app may query.
    const P: &str = "matrix-perspective-p";
    /// A perspective the "P reader" app may not query.
    const Q: &str = "matrix-perspective-q";

    fn user(name: &str, did: &str) -> Principal {
        Principal::User {
            token_id: TokenId(format!("matrix-{name}")),
            email: format!("matrix.{name}@example.org"),
            did: did.to_string(),
            caps: get_user_default_capabilities(),
        }
    }

    fn app(name: &str, caps: Vec<Capability>) -> Principal {
        let token_id = TokenId(format!("matrix-{name}"));
        Principal::App { token_id, caps }
    }

    /// The sessions of the matrix, by name. Users hold what a user token holds; "app" holds
    /// the same set, so the audience alone decides what it gets. The AI app and the P reader
    /// act as the main agent too, but their capabilities narrow what they receive (#1284).
    fn sessions() -> Vec<(&'static str, Arc<Session>)> {
        let ai = vec![
            AI_READ_CAPABILITY.clone(),
            AI_PROMPT_CAPABILITY.clone(),
            AI_TRANSCRIBE_CAPABILITY.clone(),
        ];
        let p_reader = vec![perspective_query_capability(vec![P.to_string()])];
        [
            ("operator", Principal::Operator, ListenerReach::Loopback),
            (
                "app",
                app("app", get_user_default_capabilities()),
                ListenerReach::Network,
            ),
            // An app whose token may approve apps (`AGENT_PERMIT`, here through `ALL`).
            (
                "permit app",
                app("permit-app", vec![ALL_CAPABILITY.clone()]),
                ListenerReach::Network,
            ),
            ("user A", user("a", A), ListenerReach::Network),
            ("user B", user("b", B), ListenerReach::Network),
            ("AI app", app("ai-app", ai), ListenerReach::Network),
            (
                "P reader",
                app("p-reader", p_reader),
                ListenerReach::Network,
            ),
            (
                "anonymous",
                Principal::Anonymous { caps: vec![] },
                ListenerReach::Network,
            ),
        ]
        .into_iter()
        .map(|(name, principal, reach)| (name, Arc::new(Session::for_test(principal, reach))))
        .collect()
    }

    type Stream = Pin<Box<dyn futures::stream::Stream<Item = String> + Send>>;

    /// A matrix case: its name, topic, audience, the perspective it is about and the sessions
    /// that receive it.
    type Case<'a> = (String, Topic, Audience, Option<&'a str>, Vec<&'a str>);

    /// A stream that watches every event, as a socket that asked for all of them.
    async fn open(session: Arc<Session>) -> Stream {
        build_event_stream_for(session, event_interest::watching_all(), None).await
    }

    /// The streams of `sessions`, by name. An anonymous session gets no stream on a real
    /// socket and holds no capability, so no event reaches it and no fence could end a read:
    /// `may_receive` pins it in [`each_audience_reaches_exactly_its_sessions`].
    async fn open_all(sessions: &[(&'static str, Arc<Session>)]) -> Vec<(&'static str, Stream)> {
        let mut streams = Vec::new();
        for (name, session) in sessions {
            if session.is_authenticated() {
                streams.push((*name, open(session.clone()).await));
            }
        }
        streams
    }

    /// Publishes `payload` as a fence that reaches every stream of [`open_all`]: as
    /// `model-loading-status` (AI_READ) and as `link-added` on P (the P reader's only
    /// capability). A stream stops reading at the first one it gets.
    async fn publish_fence(payload: serde_json::Value) {
        let pubsub = get_global_pubsub().await;
        let model = Event::new(Topic::AiModelLoadingStatus, Audience::Everyone, &payload);
        pubsub.publish_event(model);
        let link = Event::new(Topic::LinkAdded, Audience::Everyone, &payload).for_perspective(P);
        pubsub.publish_event(link);
    }

    /// The tags of this run's events that `stream` delivers before `fence`. Events reach a
    /// stream in publish order, so everything published before the fence has arrived.
    async fn delivered(stream: &mut Stream, run: &str, fence: &str) -> BTreeSet<String> {
        let mut tags = BTreeSet::new();
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let msg = stream.next().await.expect("the stream stays open");
                let Some(at) = msg.find(&format!("\"{run}:")) else {
                    continue; // another test's event
                };
                let tag = msg[at + 1..].split('"').next().unwrap().to_string();
                if tag == fence {
                    return;
                }
                tags.insert(tag);
            }
        })
        .await
        .unwrap_or_else(|_| panic!("the fence never arrived; got {tags:?}"));
        tags
    }

    #[tokio::test]
    async fn each_audience_reaches_exactly_its_sessions() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let sessions = sessions();
        let main = sessions[0]
            .1
            .acts_as_did()
            .expect("the main agent has a DID");
        let main = main.to_string();
        assert_eq!(sessions[1].1.acts_as_did(), Some(main.as_str()), "app");

        // The sessions that hold a user token's capabilities, or every capability.
        let readers = ["operator", "app", "permit app", "user A", "user B"];
        // The operator and apps act as the main agent; these hold the topics' capabilities.
        let main_agent = ["operator", "app", "permit app"];
        let approvers = ["operator", "permit app"];
        let mut cases: Vec<(String, Topic, Audience, Vec<&str>)> = vec![
            // The AI app holds AI_READ; the P reader and anonymous callers do not.
            (
                "everyone".into(),
                Topic::AiModelLoadingStatus,
                Audience::Everyone,
                vec![
                    "operator",
                    "app",
                    "permit app",
                    "user A",
                    "user B",
                    "AI app",
                ],
            ),
            // A message names the agent it is for; the AI app may not read messages.
            (
                "message-to-main".into(),
                Topic::RuntimeMessageReceived,
                Audience::Dids(vec![main.clone()]),
                main_agent.to_vec(),
            ),
            (
                "operator".into(),
                Topic::AppsChanged,
                Audience::Operator,
                vec!["operator"],
            ),
            (
                "approvers".into(),
                Topic::AppsChanged,
                Audience::Approvers,
                approvers.to_vec(),
            ),
            (
                "user-a".into(),
                Topic::LinkAdded,
                Audience::Dids(vec![A.into()]),
                vec!["user A"],
            ),
            (
                "users-a-b".into(),
                Topic::NeighbourhoodSignal,
                Audience::Dids(vec![A.into(), B.into()]),
                vec!["user A", "user B"],
            ),
            (
                "main-agent".into(),
                Topic::PerspectiveUpdated,
                Audience::Dids(vec![main.clone()]),
                main_agent.to_vec(),
            ),
            (
                "no-did".into(),
                Topic::AgentUpdated,
                Audience::Dids(vec![]),
                vec![],
            ),
        ];
        // Agent topics go to the agent the payload names (`matches_agent_did` before), or to
        // every session that may read agents while the status names no agent (before
        // `agent.generate()`).
        for topic in [Topic::AgentStatusChanged, Topic::AgentUpdated] {
            for (name, did, to) in [
                ("user-a", Some(A), vec!["user A"]),
                ("main-agent", Some(main.as_str()), main_agent.to_vec()),
                ("no-agent-yet", None, readers.to_vec()),
            ] {
                cases.push((format!("{topic}-{name}"), topic, Audience::agent(did), to));
            }
        }
        // A triggered notification goes to its owner (`matches_notification_owner` before),
        // never to the perspective's co-owners.
        for (name, owner, to) in [
            ("a", A, vec!["user A"]),
            ("main", &main, main_agent.to_vec()),
        ] {
            let audience = Audience::Dids(vec![owner.to_string()]);
            let topic = Topic::RuntimeNotificationTriggered;
            cases.push((format!("notification-of-{name}"), topic, audience, to));
        }
        // A signal goes to the recipient it names. One without a recipient goes to each
        // owner (`telepresence_signals_reach_their_recipients`), never to every session.
        for (name, recipient, to) in [
            ("to-a", A.to_string(), vec!["user A"]),
            ("to-main", main.clone(), main_agent.to_vec()),
        ] {
            let signal = crate::perspectives::signal_event(
                Default::default(),
                Default::default(),
                recipient,
            );
            let topic = Topic::NeighbourhoodSignal;
            cases.push((format!("signal-{name}"), topic, signal.audience, to));
        }
        // Transcription text goes to the stream's owner (`matches_transcription_user` before).
        // The AI app acts as the main agent and may transcribe.
        let mut main_and_ai = main_agent.to_vec();
        main_and_ai.push("AI app");
        for (name, did, to) in [("a", A, vec!["user A"]), ("main", &main, main_and_ai)] {
            let audience = Audience::agent(Some(did));
            cases.push((
                format!("transcription-of-{name}"),
                Topic::AiTranscriptionText,
                audience,
                to,
            ));
        }
        // The auto-processor topics: the operator always, and the pass owner or the
        // perspective's readers (`matches_auto_processor_*` before; their `is_admin` case is
        // the operator).
        {
            use crate::perspectives::auto_processor::events::{
                neighbourhood_state_audience, pass_audience, Readers,
            };
            let owners =
                |dids: &[&str]| Readers::Owners(dids.iter().map(|d| d.to_string()).collect());
            let pass = Topic::AutoProcessorEvent;
            let state = Topic::AutoProcessorNeighbourhoodState;
            for (name, topic, audience, to) in [
                (
                    "pass-of-a",
                    pass,
                    pass_audience(owners(&[A]), Some(A)),
                    vec!["operator", "user A"],
                ),
                (
                    "pass-of-a-on-b",
                    pass,
                    pass_audience(owners(&[B]), Some(A)),
                    vec!["operator"],
                ),
                (
                    "pass-of-main",
                    pass,
                    pass_audience(owners(&[&main]), Some(&main)),
                    main_agent.to_vec(),
                ),
                (
                    "pass-of-a-unowned",
                    pass,
                    pass_audience(owners(&[&main]), Some(A)),
                    vec!["operator"],
                ),
                (
                    "pass-unknown",
                    pass,
                    pass_audience(Readers::Unknown, Some(A)),
                    vec!["operator"],
                ),
                (
                    "pass-no-owner",
                    pass,
                    pass_audience(owners(&[&main]), None),
                    vec!["operator"],
                ),
                (
                    "state-of-a-b",
                    state,
                    neighbourhood_state_audience(owners(&[A, B])),
                    vec!["operator", "user A", "user B"],
                ),
                // An unowned perspective's reader is the main agent (it reached every
                // authenticated session, users included, before one helper chose them).
                (
                    "state-unowned",
                    state,
                    neighbourhood_state_audience(owners(&[&main])),
                    main_agent.to_vec(),
                ),
                (
                    "state-unknown",
                    state,
                    neighbourhood_state_audience(Readers::Unknown),
                    vec!["operator"],
                ),
            ] {
                cases.push((name.to_string(), topic, audience, to));
            }
        }
        // Exceptions go per type (`exception_visible` before): approval requests to the
        // sessions that may approve apps, the others to every session that may subscribe to
        // exceptions.
        {
            use crate::types::{ExceptionInfo, ExceptionType::*};
            for kind in [
                LanguageIsNotLoaded,
                ExpressionIsNotVerified,
                AgentIsUntrusted,
                CapabilityRequested,
                InstallNotificationRequest,
            ] {
                let to = match kind {
                    CapabilityRequested | InstallNotificationRequest => approvers.to_vec(),
                    _ => readers.to_vec(),
                };
                let name = format!("exception-{kind:?}");
                let info = ExceptionInfo {
                    r#type: kind,
                    ..Default::default()
                };
                let audience = Event::exception(&info).audience;
                cases.push((name, Topic::ExceptionOccurred, audience, to));
            }
        }
        // Perspective topics name the perspective's owners, or the main agent for an
        // unowned perspective: the rule the string filters applied before (`owner` equal to
        // the session's DID, which is the main agent's for the operator and apps).
        let handle = |owners: Option<Vec<&str>>| crate::types::PerspectiveHandle {
            owners: owners.map(|o| o.into_iter().map(String::from).collect()),
            ..Default::default()
        };
        // Sync-state goes out once, to all owners. The other perspective and link topics go
        // out once per owner, each to that owner (`publish_to_each_owner`).
        let recipients = crate::perspectives::event_recipients;
        let owned_by_a = handle(Some(vec![A]));
        let owned_by_a_b = handle(Some(vec![A, B]));
        for topic in [
            Topic::PerspectiveAdded,
            Topic::PerspectiveRemoved,
            Topic::PerspectiveUpdated,
            Topic::PerspectiveSyncStateChange,
            Topic::LinkAdded,
            Topic::LinkRemoved,
            Topic::LinkUpdated,
        ] {
            let mut perspectives = vec![
                ("owned-by-a", owned_by_a.clone(), vec!["user A"]),
                ("unowned", handle(None), main_agent.to_vec()),
                ("no-owners", handle(Some(vec![])), main_agent.to_vec()),
            ];
            if topic == Topic::PerspectiveSyncStateChange {
                perspectives.push((
                    "owned-by-a-b",
                    owned_by_a_b.clone(),
                    vec!["user A", "user B"],
                ));
            }
            for (name, perspective, to) in perspectives {
                let audience = Audience::Dids(recipients(&perspective));
                cases.push((format!("{topic}-{name}"), topic, audience, to));
            }
            if topic != Topic::PerspectiveSyncStateChange {
                // A's and B's copies of one event on a perspective they share.
                for (owner, to, copy) in [(A, "user A", "a"), (B, "user B", "b")] {
                    let audience = Audience::Dids(vec![owner.into()]);
                    cases.push((format!("{topic}-{copy}-of-a-b"), topic, audience, vec![to]));
                }
            }
        }
        // #1284: an app acts as the main agent, so the audience alone gave it every event of
        // every perspective the main agent owns. A per-perspective topic also needs the
        // capability that reads it on that perspective: the P reader gets the main agent's
        // events on P only, the AI app none. The rows above carry no perspective, so they
        // need the capability on every perspective (`*`), which the P reader lacks.
        let mut cases: Vec<Case> = cases
            .into_iter()
            .map(|(case, topic, audience, to)| (case, topic, audience, None, to))
            .collect();
        let mut main_and_p_reader = main_agent.to_vec();
        main_and_p_reader.push("P reader");
        let to_main = || Audience::Dids(vec![main.clone()]);
        for topic in [
            Topic::PerspectiveAdded,
            Topic::PerspectiveRemoved,
            Topic::PerspectiveUpdated,
            Topic::PerspectiveSyncStateChange,
            Topic::LinkAdded,
            Topic::LinkRemoved,
            Topic::LinkUpdated,
            Topic::PerspectiveQuerySubscription,
            Topic::AutoProcessorEvent,
            Topic::AutoProcessorNeighbourhoodState,
        ] {
            for (name, audience, on, to) in [
                ("main-on-p", to_main(), P, main_and_p_reader.clone()),
                ("main-on-q", to_main(), Q, main_agent.to_vec()),
                ("a-on-p", Audience::Dids(vec![A.into()]), P, vec!["user A"]),
            ] {
                cases.push((format!("{topic}-{name}"), topic, audience, Some(on), to));
            }
        }
        // A signal needs the neighbourhood capability: the P reader may query P, not read
        // its neighbourhood.
        cases.push((
            "signal-main-on-p".into(),
            Topic::NeighbourhoodSignal,
            to_main(),
            Some(P),
            main_agent.to_vec(),
        ));

        let mut streams = open_all(&sessions).await;

        let run = uuid::Uuid::new_v4().to_string();
        let tag = |case: &str| format!("{run}:{case}");
        let pubsub = get_global_pubsub().await;
        for (case, topic, audience, perspective, to) in &cases {
            let mut event = Event::new(*topic, audience.clone(), json_case(&tag(case)));
            if let Some(perspective) = perspective {
                event = event.for_perspective(*perspective);
            }
            for (name, session) in &sessions {
                let expected = to.contains(name);
                assert_eq!(session.may_receive(&event), expected, "{case} → {name}");
            }
            pubsub.publish_event(event);
        }
        let fence = tag("fence");
        publish_fence(json_case(&fence)).await;

        for (name, stream) in &mut streams {
            let got = delivered(stream, &run, &fence).await;
            let expected: BTreeSet<String> = cases
                .iter()
                .filter(|(.., to)| to.contains(name))
                .map(|(case, ..)| tag(case))
                .collect();
            assert_eq!(got, expected, "{name}");
        }
    }

    fn json_case(tag: &str) -> serde_json::Value {
        serde_json::json!({ "case": tag })
    }

    /// `(type, perspectiveUuid, owner)` of each message `stream` delivers for a perspective of
    /// this run, sorted, until the `fence` perspective's message. `owner` is empty when the
    /// message has none.
    async fn perspective_events(
        stream: &mut Stream,
        run: &str,
        fence: &str,
    ) -> Vec<(String, String, String)> {
        let mut got = Vec::new();
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let msg = stream.next().await.expect("the stream stays open");
                let v: serde_json::Value = serde_json::from_str(&msg).unwrap();
                let Some(uuid) = v["perspectiveUuid"].as_str().filter(|u| u.starts_with(run))
                else {
                    continue; // another test's event
                };
                if uuid == fence {
                    return;
                }
                let text = |key: &str| v[key].as_str().unwrap_or_default().to_string();
                got.push((text("type"), uuid.to_string(), text("owner")));
            }
        })
        .await
        .unwrap_or_else(|_| panic!("the fence never arrived; got {got:?}"));
        got.sort();
        got
    }

    /// The sorted `(type, perspectiveUuid, owner)` triples of `types` for one perspective.
    fn expect(types: &[&str], uuid: &str, owner: &str) -> Vec<(String, String, String)> {
        let mut list: Vec<_> = types
            .iter()
            .map(|t| (t.to_string(), uuid.to_string(), owner.to_string()))
            .collect();
        list.sort();
        list
    }

    /// An open database, for the perspective publishers. Keeps one that is already open.
    fn database() {
        if crate::db::Ad4mDb::global_instance()
            .lock()
            .unwrap()
            .is_none()
        {
            crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        }
    }

    // The real publishers: creating, renaming, syncing and removing a user's perspective
    // reaches that user's stream only. An unowned perspective's events, its removal too,
    // reach the operator and the apps that may query it (its removal used to reach nobody).
    #[tokio::test]
    async fn perspective_lifecycle_events_reach_exactly_the_owners_streams() {
        use crate::perspectives::{
            add_perspective, get_perspective, remove_perspective, update_perspective,
        };
        use crate::types::{PerspectiveHandle, PerspectiveState};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        database();
        let sessions = sessions();
        let mut streams = open_all(&sessions).await;

        let run = uuid::Uuid::new_v4().to_string();
        let owned = PerspectiveHandle {
            uuid: format!("{run}-owned-by-a"),
            owners: Some(vec![A.to_string()]),
            ..Default::default()
        };
        let unowned = PerspectiveHandle {
            uuid: format!("{run}-unowned"),
            ..Default::default()
        };
        for handle in [&owned, &unowned] {
            add_perspective(handle.clone(), None).await.unwrap();
            let renamed = PerspectiveHandle {
                name: Some("renamed".into()),
                ..handle.clone()
            };
            update_perspective(&renamed).await.unwrap();
            // Publishes `perspective-updated` too: the state change updates the handle.
            get_perspective(&handle.uuid)
                .unwrap()
                .update_perspective_state(PerspectiveState::Synced)
                .await
                .unwrap();
            assert!(remove_perspective(&handle.uuid).await.is_some());
        }
        let fence = format!("{run}-fence");
        publish_fence(serde_json::json!({ "perspectiveUuid": fence })).await;

        // Sync-state carries no `owner`; the others name the receiving owner.
        let lifecycle = |uuid: &str, owner: &str| {
            let owned = [
                events::PERSPECTIVE_ADDED,
                events::PERSPECTIVE_UPDATED,
                events::PERSPECTIVE_UPDATED,
                events::PERSPECTIVE_REMOVED,
            ];
            let mut list = expect(&owned, uuid, owner);
            list.extend(expect(&[events::SYNC_STATE_CHANGE], uuid, ""));
            list.sort();
            list
        };
        let main = crate::agent::did();
        for (name, stream) in &mut streams {
            let got = perspective_events(stream, &run, &fence).await;
            let expected = match *name {
                "user A" => lifecycle(&owned.uuid, A),
                "operator" | "app" | "permit app" => lifecycle(&unowned.uuid, &main),
                _ => vec![],
            };
            assert_eq!(got, expected, "{name}");
        }
    }

    // Link events go out once per owner, each to that owner with the owner's DID in `owner`.
    // Each owner's stream gets its own copy of each change exactly once, never a co-owner's
    // copy; nobody else gets any.
    #[tokio::test]
    async fn each_link_event_reaches_each_owner_exactly_once() {
        use crate::agent::AgentContext;
        use crate::perspectives::perspective_instance::PerspectiveInstance;
        use crate::types::{Link, LinkExpression, LinkStatus, PerspectiveHandle};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        database();
        crate::prolog_service::init_prolog_service().await;
        let sessions = sessions();
        let mut streams = open_all(&sessions).await;

        let run = uuid::Uuid::new_v4().to_string();
        let perspective = |name: &str, owners: Option<Vec<String>>| {
            let handle = PerspectiveHandle {
                uuid: format!("{run}-{name}"),
                owners,
                ..Default::default()
            };
            PerspectiveInstance::new(handle, None)
        };
        let mut shared = perspective("owned-by-a-b", Some(vec![A.into(), B.into()]));
        let mut unowned = perspective("unowned", None);
        let main = AgentContext::main_agent();
        for p in [&mut shared, &mut unowned] {
            let link = |target: &str| Link {
                source: format!("test://{run}"),
                predicate: None,
                target: target.to_string(),
            };
            let added = p
                .add_link(link("first"), LinkStatus::Local, None, &main)
                .await
                .unwrap();
            let updated = p
                .update_link(LinkExpression::from(added), link("second"), None, &main)
                .await
                .unwrap();
            p.remove_link(LinkExpression::from(updated), None)
                .await
                .unwrap();
        }
        let fence = format!("{run}-fence");
        publish_fence(serde_json::json!({ "perspectiveUuid": fence })).await;

        let changes = [
            events::LINK_ADDED,
            events::LINK_UPDATED,
            events::LINK_REMOVED,
        ];
        let shared_uuid = format!("{run}-owned-by-a-b");
        let unowned_uuid = format!("{run}-unowned");
        let main_did = crate::agent::did();
        for (name, stream) in &mut streams {
            let got = perspective_events(stream, &run, &fence).await;
            let expected = match *name {
                "user A" => expect(&changes, &shared_uuid, A),
                "user B" => expect(&changes, &shared_uuid, B),
                "operator" | "app" | "permit app" => expect(&changes, &unowned_uuid, &main_did),
                _ => vec![],
            };
            assert_eq!(got, expected, "{name}");
        }
    }

    // The link language's signal publisher: a signal without a recipient goes once to each
    // of the perspective's `event_recipients`, each copy naming that DID as `recipient`. An
    // unowned perspective's recipient is the main agent. A signal without a recipient on a
    // perspective without owners used to reach every session (every user in multi-user mode)
    // through the link language's slow path, and nobody through its cached-handle path.
    #[tokio::test]
    async fn telepresence_signals_reach_their_recipients() {
        use crate::perspectives::perspective_instance::PerspectiveInstance;
        use crate::perspectives::{
            handle_telepresence_signal_from_link_language,
            handle_telepresence_signal_from_link_language_impl, register_link_language_perspective,
            register_perspective, unregister_perspective,
        };
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main = crate::agent::did();
        let sessions = sessions();
        let mut streams = open_all(&sessions).await;

        let run = uuid::Uuid::new_v4().to_string();
        let handle = |name: &str, owners: Option<Vec<String>>| crate::types::PerspectiveHandle {
            uuid: format!("{run}-{name}"),
            owners,
            ..Default::default()
        };
        let shared = handle("owned-by-a-b", Some(vec![A.into(), B.into()]));
        let unowned = handle("unowned", None);
        let publish = crate::perspectives::publish_telepresence_signal;
        publish(shared.clone(), Default::default(), None);
        publish(shared.clone(), Default::default(), Some(B.into()));
        publish(unowned.clone(), Default::default(), None);

        // Both link-language paths, for a perspective whose owner list is empty: the cached
        // handle (published at once) and the lookup of the registered perspective.
        let in_neighbourhood = |name: &str| {
            let mut handle = handle(name, Some(vec![]));
            let mut neighbourhood = crate::types::DecoratedNeighbourhoodExpression::default();
            neighbourhood.data.link_language = format!("{run}-{name}-language");
            handle.neighbourhood = Some(neighbourhood);
            handle
        };
        let cached = in_neighbourhood("no-owners-cached");
        let cached_language = format!("{run}-no-owners-cached-language");
        register_link_language_perspective(cached_language.clone(), cached.clone());
        handle_telepresence_signal_from_link_language(Default::default(), cached_language, None);
        let looked_up = in_neighbourhood("no-owners-looked-up");
        register_perspective(
            looked_up.uuid.clone(),
            PerspectiveInstance::new(looked_up.clone(), None),
        );
        let looked_up_language = format!("{run}-no-owners-looked-up-language");
        handle_telepresence_signal_from_link_language_impl(
            Default::default(),
            looked_up_language,
            None,
        )
        .await;
        unregister_perspective(&looked_up.uuid);
        let fence = format!("{run}-fence");
        publish_fence(serde_json::json!({ "perspectiveUuid": fence })).await;

        // `(type, perspectiveUuid, recipient)` triples.
        let signal = |p: &crate::types::PerspectiveHandle, to: &str| {
            (events::SIGNAL.to_string(), p.uuid.clone(), to.to_string())
        };
        for (name, stream) in &mut streams {
            let mut got = Vec::new();
            tokio::time::timeout(Duration::from_secs(5), async {
                loop {
                    let msg = stream.next().await.expect("the stream stays open");
                    let v: serde_json::Value = serde_json::from_str(&msg).unwrap();
                    let Some(uuid) = v["perspectiveUuid"]
                        .as_str()
                        .filter(|u| u.starts_with(&run))
                    else {
                        continue;
                    };
                    if uuid == fence {
                        return;
                    }
                    let to = v["recipient"].as_str().unwrap_or_default().to_string();
                    got.push((
                        v["type"].as_str().unwrap().to_string(),
                        uuid.to_string(),
                        to,
                    ));
                }
            })
            .await
            .unwrap_or_else(|_| panic!("{name}: the fence never arrived; got {got:?}"));
            got.sort();
            let mut expected = match *name {
                "user A" => vec![signal(&shared, A)],
                "user B" => vec![signal(&shared, B), signal(&shared, B)],
                // The AI app and the P reader may not read neighbourhoods.
                "operator" | "app" | "permit app" => vec![
                    signal(&unowned, &main),
                    signal(&cached, &main),
                    signal(&looked_up, &main),
                ],
                _ => vec![],
            };
            expected.sort();
            assert_eq!(got, expected, "{name}");
        }
    }

    // A user's hosting info goes to that user and to the sessions that act as the main agent:
    // `matches_hosting_user` let every session without a user email through, and a user's
    // session only for its own email. Other users get nothing.
    #[tokio::test]
    async fn hosting_user_info_reaches_the_user_and_the_main_agent() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        database();
        let email = format!("hosting-{}@example.org", uuid::Uuid::new_v4());
        crate::agent::AgentService::ensure_user_key_exists(&email).unwrap();
        let did = crate::agent::AgentService::get_user_did_by_email(&email).unwrap();
        let hosted = Principal::User {
            token_id: TokenId("matrix-hosted".into()),
            email: email.clone(),
            did,
            caps: get_user_default_capabilities(),
        };
        let mut sessions = sessions();
        sessions.push((
            "hosted user",
            Arc::new(Session::for_test(hosted, ListenerReach::Network)),
        ));

        let info = crate::types::HostingUserInfo {
            email: email.clone(),
            remaining_credits: "7".into(),
            hot_wallet_address: None,
            free_access: false,
        };
        let event = Event::hosting_user_info(&info);
        assert_eq!(event.topic, Topic::HostingUserInfoChanged);
        assert_eq!(event.payload, serde_json::to_value(&info).unwrap());
        for (name, session) in &sessions {
            let expected = matches!(*name, "hosted user" | "operator" | "app" | "permit app");
            assert_eq!(session.may_receive(&event), expected, "{name}");
        }
    }

    // The auto-processor's publishers: a pass's step signal reaches the operator and the pass
    // owner where the owner may read the perspective; a neighbourhood-state event reaches the
    // operator and every reader. The readers come from the registered handle at publish.
    #[tokio::test]
    async fn auto_processor_events_reach_the_operator_and_the_readers() {
        use crate::perspectives::auto_processor::events::{
            emit, emit_neighbourhood_state, AutoProcessorEvent, AutoProcessorNeighbourhoodState,
            AutoProcessorStep, NeighbourhoodPhase,
        };
        use crate::perspectives::perspective_instance::PerspectiveInstance;
        use crate::perspectives::{register_perspective, unregister_perspective};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main = crate::agent::did();
        let sessions = sessions();
        let mut streams = open_all(&sessions).await;

        let run = uuid::Uuid::new_v4().to_string();
        let uuid = |name: &str| format!("{run}-{name}");
        for (name, owners) in [
            ("owned-by-a", Some(vec![A.to_string()])),
            ("owned-by-a-b", Some(vec![A.to_string(), B.to_string()])),
            ("unowned", None),
        ] {
            let handle = crate::types::PerspectiveHandle {
                uuid: uuid(name),
                owners,
                ..Default::default()
            };
            register_perspective(uuid(name), PerspectiveInstance::new(handle, None));
        }
        // The processor id names the case.
        let pass = |perspective: &str, case: &str, did: Option<&str>| {
            let event =
                AutoProcessorEvent::new(&uuid(perspective), case, AutoProcessorStep::Claimed);
            match did {
                Some(did) => event.with_agent_did(did),
                None => event,
            }
        };
        emit(pass("owned-by-a", "pass-of-a", Some(A))).await;
        emit(pass("owned-by-a", "pass-of-b", Some(B))).await;
        emit(pass("unowned", "pass-of-main", Some(&main))).await;
        emit(pass("unowned", "pass-of-a-on-unowned", Some(A))).await;
        emit(pass("not-registered", "pass-unknown", Some(A))).await;
        emit(pass("owned-by-a", "pass-no-owner", None)).await;
        let state = |perspective: &str, case: &str| {
            let phase = NeighbourhoodPhase::Claimed;
            AutoProcessorNeighbourhoodState::new(&uuid(perspective), case, A, "batch", phase)
        };
        emit_neighbourhood_state(state("owned-by-a-b", "state-of-a-b")).await;
        emit_neighbourhood_state(state("unowned", "state-unowned")).await;
        emit_neighbourhood_state(state("not-registered", "state-unknown")).await;
        let fence = uuid("fence");
        publish_fence(serde_json::json!({ "perspectiveUuid": fence })).await;
        for name in ["owned-by-a", "owned-by-a-b", "unowned"] {
            unregister_perspective(&uuid(name));
        }

        for (name, stream) in &mut streams {
            let mut got = BTreeSet::new();
            tokio::time::timeout(Duration::from_secs(5), async {
                loop {
                    let msg = stream.next().await.expect("the stream stays open");
                    let v: serde_json::Value = serde_json::from_str(&msg).unwrap();
                    let Some(p) = v["perspectiveUuid"]
                        .as_str()
                        .filter(|u| u.starts_with(&run))
                    else {
                        continue;
                    };
                    if p == fence {
                        return;
                    }
                    got.insert(v["processorId"].as_str().unwrap().to_string());
                }
            })
            .await
            .unwrap_or_else(|_| panic!("{name}: the fence never arrived; got {got:?}"));
            let expected: &[&str] = match *name {
                "operator" => &[
                    "pass-of-a",
                    "pass-of-b",
                    "pass-of-main",
                    "pass-of-a-on-unowned",
                    "pass-unknown",
                    "pass-no-owner",
                    "state-of-a-b",
                    "state-unowned",
                    "state-unknown",
                ],
                "app" | "permit app" => &["pass-of-main", "state-unowned"],
                // Users may not read an unowned perspective, so they get none of its events.
                "user A" => &["pass-of-a", "state-of-a-b"],
                "user B" => &["state-of-a-b"],
                _ => &[],
            };
            let expected: BTreeSet<String> = expected.iter().map(|s| s.to_string()).collect();
            assert_eq!(got, expected, "{name}");
        }
    }

    // The socket's `events.watch` narrows typed events by topic and perspective.
    #[tokio::test]
    async fn a_socket_gets_only_the_typed_events_it_watches() {
        let session = Arc::new(Session::for_test(
            Principal::Operator,
            ListenerReach::Loopback,
        ));
        let interest = SharedInterest::default();
        event_interest::handle_control(
            event_interest::WATCH,
            &serde_json::Value::Null,
            &serde_json::json!({ "link-added": ["wanted"] }),
            &interest,
        );
        let mut stream: Stream = build_event_stream_for(session, interest, None).await;

        let run = uuid::Uuid::new_v4().to_string();
        let tag = |case: &str| format!("{run}:{case}");
        let pubsub = get_global_pubsub().await;
        let link = |case: &str, perspective: &str| {
            Event::new(Topic::LinkAdded, Audience::Everyone, json_case(&tag(case)))
                .for_perspective(perspective)
        };
        pubsub.publish_event(link("wanted", "wanted"));
        pubsub.publish_event(link("other-perspective", "other"));
        pubsub.publish_event(Event::new(
            Topic::AgentUpdated,
            Audience::Everyone,
            json_case(&tag("unwatched")),
        ));
        // Live query updates pass without a watch, so they fence this stream.
        let fence = tag("fence");
        pubsub.publish_event(Event::new(
            Topic::PerspectiveQuerySubscription,
            Audience::Everyone,
            json_case(&fence),
        ));

        let got = delivered(&mut stream, &run, &fence).await;
        assert_eq!(got, BTreeSet::from([tag("wanted")]));
    }

    // The stream ends at the first event after the token stopped working, typed ones too.
    #[tokio::test]
    async fn a_typed_event_after_the_token_expired_ends_the_stream() {
        let an_hour_ago = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
            - 3600;
        // It may read the event: one it may not read never reaches the stream's end check.
        let app = Principal::App {
            token_id: TokenId("matrix-expired".into()),
            caps: vec![AI_READ_CAPABILITY.clone()],
        };
        let expired = Arc::new(Session::for_test_expiring(app, an_hour_ago));
        let mut stream = open(expired).await;
        get_global_pubsub().await.publish_event(Event::new(
            Topic::AiModelLoadingStatus,
            Audience::Everyone,
            json_case("expired"),
        ));
        let next = tokio::time::timeout(Duration::from_secs(5), stream.next())
            .await
            .expect("the stream must answer, not hang");
        assert!(next.is_none(), "an expired session got {next:?}");
    }
}

#[cfg(test)]
mod token_expiry_tests {
    use super::*;
    use crate::agent::session::{Principal, TokenId};
    use crate::test_utils::{setup_agent, setup_wallet, MultiUserMode};
    use std::time::Duration;

    const USER: &str = "events.user@example.org";

    /// Builds a user's event stream, publishes one event and returns what the stream yields.
    async fn next_event(session: Session) -> Option<String> {
        let all = event_interest::watching_all();
        let mut stream = build_event_stream(Arc::new(session), all, Some(USER.to_string())).await;
        get_global_pubsub().await.publish_event(Event::new(
            Topic::AiModelLoadingStatus,
            crate::events::Audience::Everyone,
            serde_json::json!({ "model": "m", "status": "loading" }),
        ));
        tokio::time::timeout(Duration::from_secs(5), stream.next())
            .await
            .expect("the stream must answer, not hang")
    }

    // A connection kept its event stream after its token expired.
    #[tokio::test]
    async fn a_token_that_expired_after_connect_ends_the_event_stream() {
        let an_hour_ago = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
            - 3600;
        let user = Principal::User {
            token_id: TokenId("user-jwt".into()),
            email: USER.into(),
            did: "did:key:events-user".into(),
            caps: get_user_default_capabilities(),
        };
        let expired = Session::for_test_expiring(user, an_hour_ago);
        assert!(next_event(expired).await.is_none());
    }

    #[tokio::test]
    async fn a_valid_token_keeps_its_event_stream() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        crate::agent::AgentService::ensure_user_key_exists(USER).unwrap();
        let token = crate::user_management::generate_user_jwt(USER, "test").unwrap();
        let session = Session::open(&token, &None, ListenerReach::Network).unwrap();
        assert!(next_event(session).await.is_some());
    }
}

#[cfg(test)]
mod event_spec_tests {
    use super::{event_specs, events};
    use crate::events::Topic;
    use crate::types::*;
    use std::collections::BTreeSet;

    #[test]
    fn every_emitted_event_has_exactly_one_spec() {
        let specs: Vec<&str> = event_specs().iter().map(|s| s.name).collect();
        let unique: BTreeSet<&str> = specs.iter().copied().collect();
        assert_eq!(unique.len(), specs.len(), "duplicate spec: {specs:?}");
        let topics = Topic::ALL.map(Topic::as_str);
        assert_eq!(unique, topics.into_iter().collect::<BTreeSet<_>>());
    }

    #[test]
    fn nested_payloads_keep_their_wire_key() {
        let wire = super::wrap_event_nested(events::AGENT_UPDATED, "agent", r#"{"did":"d"}"#);
        let v: serde_json::Value = serde_json::from_str(&wire).unwrap();
        assert_eq!(v["agent"]["did"], "d");
        let _: super::AgentUpdatedEvent = serde_json::from_value(v).unwrap();
    }

    /// Serializes a scoped payload's publish struct and checks the flat
    /// `perspectiveUuid` that `events.watch` narrows by.
    fn uuid_of(payload: impl serde::Serialize) -> String {
        serde_json::to_value(payload).unwrap()["perspectiveUuid"]
            .as_str()
            .expect("scoped payload carries perspectiveUuid")
            .to_string()
    }

    #[test]
    fn scoped_payloads_serialize_perspective_uuid() {
        use crate::perspectives::auto_processor::events::{
            AutoProcessorNeighbourhoodState, NeighbourhoodPhase,
        };
        let p = || "p".to_string();
        let handle = PerspectiveHandle {
            uuid: p(),
            ..Default::default()
        };
        let scoped = [
            uuid_of(PerspectiveWithOwner {
                perspective_uuid: p(),
                perspective: handle.clone(),
                owner: "o".into(),
            }),
            uuid_of(PerspectiveRemovedWithOwner {
                perspective_uuid: p(),
                uuid: p(),
                owner: "o".into(),
            }),
            uuid_of(PerspectiveStateFilter {
                perspective_uuid: p(),
                state: PerspectiveState::Private,
                perspective: handle.clone(),
            }),
            uuid_of(PerspectiveLinkWithOwner {
                perspective_uuid: p(),
                ..Default::default()
            }),
            uuid_of(PerspectiveLinkUpdatedWithOwner {
                perspective_uuid: p(),
                ..Default::default()
            }),
            uuid_of(NeighbourhoodSignalFilter {
                perspective_uuid: p(),
                perspective: handle,
                ..Default::default()
            }),
            uuid_of(NotificationTriggeredEvent {
                perspective_uuid: p(),
                notification: TriggeredNotification {
                    notification: serde_json::from_value(serde_json::json!({
                        "id": "n", "granted": true, "description": "", "appName": "",
                        "appUrl": "", "appIconPath": "", "trigger": "", "perspectiveIds": [],
                        "webhookUrl": "", "webhookAuth": "", "ownerDid": "did:x",
                    }))
                    .unwrap(),
                    perspective_id: p(),
                    trigger_match: "[]".into(),
                },
            }),
            uuid_of(PerspectiveQuerySubscriptionFilter {
                perspective_uuid: p(),
                uuid: p(),
                subscription_id: "s".into(),
                result: "[]".into(),
            }),
            uuid_of(AutoProcessorNeighbourhoodState::new(
                "p",
                "proc",
                "did:x",
                "k",
                NeighbourhoodPhase::Claimed,
            )),
        ];
        assert!(scoped.iter().all(|u| u == "p"));
    }
}

#[cfg(test)]
mod perspective_owner_filter_tests {
    use super::matches_perspective_owner;

    #[tokio::test]
    async fn filters_by_the_flat_perspective_uuid() {
        let unknown = r#"{"perspectiveUuid":"no-such-perspective","notification":{}}"#;
        assert!(
            !matches_perspective_owner(unknown, Some("did:x")).await,
            "an unverifiable owner fails closed"
        );
        assert!(
            matches_perspective_owner(unknown, None).await,
            "single-user"
        );
        assert!(
            !matches_perspective_owner(r#"{"notification":{}}"#, Some("did:x")).await,
            "an event without perspectiveUuid fails closed"
        );
        assert!(
            !matches_perspective_owner("not json", Some("did:x")).await,
            "an event that does not parse fails closed"
        );
    }
}

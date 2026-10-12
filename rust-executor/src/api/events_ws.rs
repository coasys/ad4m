//! Event delivery on the RPC socket (`GET /api/v1/ws`).
//!
//! Each event message is a JSON object `{ "type": "<event-type>", ...payload }` (no `id`).
//! [`event_specs`] types every payload (exported to the SDK as `Events.ts`);
//! each perspective-scoped payload carries a flat `perspectiveUuid`.
//!
//! Typed [`Event`](crate::events::Event)s from `PubSub::publish_event` reach a
//! socket when `Session::may_receive` (the audience and the event's capability) and
//! `event_interest::wants` (what the socket asked for with `events.watch`; nothing until it
//! does, live query updates aside) hold, as [`wire_message`]. Nothing here takes a lock or
//! parses a payload.
//!
//! ## Event types
//!
//! "Owners" is `perspectives::event_recipients`: the owners, or the main agent for an
//! unowned perspective. Each event goes out once, to every DID of its audience; only a
//! signal goes out once per recipient, each copy naming its `recipient`.
//!
//! | Type                          | Payload key   | Audience               | Description                          |
//! |-------------------------------|---------------|------------------------|--------------------------------------|
//! | `agent-status-changed`        | `agent`       | the agent's DID        | Agent status changed                 |
//! | `agent-updated`               | `agent`       | the agent's DID        | Agent profile updated                |
//! | `apps-changed`                | (inline)      | the approved agent's DID, AGENT_PERMIT | Installed apps changed (no token) |
//! | `hosting-user-info-changed`   | (inline)      | the user + main agent  | Hosting user info changed            |
//! | `perspective-added`           | (inline)      | owners                 | New perspective created              |
//! | `perspective-removed`         | (inline)      | owners                 | Perspective deleted                  |
//! | `perspective-updated`         | (inline)      | owners                 | Perspective metadata updated         |
//! | `sync-state-change`           | (inline)      | owners                 | Neighbourhood sync state changed     |
//! | `link-added`                  | (inline)      | owners                 | Link added to perspective            |
//! | `link-removed`                | (inline)      | owners                 | Link removed from perspective        |
//! | `link-updated`                | (inline)      | owners                 | Link updated in perspective          |
//! | `signal`                      | (inline)      | `recipient`, else a copy per owner | Neighbourhood signal received |
//! | `notification-triggered`      | `notification` + `perspectiveUuid` | the notification owner's DID | Notification triggered |
//! | `exception-occurred`          | `exception`   | approval requests: the agent asked, AGENT_PERMIT; else everyone | Exception occurred |
//! | `transcription-text`          | (inline)      | the stream owner's DID | AI transcription text                |
//! | `model-loading-status`        | (inline)      | everyone               | AI model loading status              |
//! | `query-subscription-update`   | (inline)      | owners (passes `events.watch`) | Live query subscription update |
//! | `auto-processor-event`        | (inline)      | the pass owner, if it reads the perspective | Auto-processor pass step signal |
//! | `auto-processor-neighbourhood-state` | (inline) | owners                | Coarse-grained neighbourhood view of "someone is auto-processing" |
//!
//! Every session also needs the event's capability (`Event::capability`).

use futures::stream::StreamExt;
use std::pin::Pin;
use std::sync::Arc;
use tokio_stream::wrappers::{errors::BroadcastStreamRecvError, BroadcastStream};

use super::event_interest::{self, SharedInterest};
use crate::agent::session::Session;
use crate::events::{Event, Topic};
use crate::pubsub::get_global_pubsub;

use crate::types::{
    AIModelLoadingStatus, Agent, AgentStatus, Apps, ExceptionInfo, HostingUserInfo,
    NeighbourhoodSignalFilter, NotificationTriggeredEvent, PerspectiveEvent, PerspectiveLinkEvent,
    PerspectiveLinkUpdatedEvent, PerspectiveQuerySubscriptionFilter, PerspectiveRemovedEvent,
    PerspectiveStateFilter, TranscriptionTextFilter,
};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// One emitted event: its topic, the TypeScript type of its payload (the
/// wire message without `type`) and whether it is about one perspective
/// (carries `perspectiveUuid`, so `events.watch` can narrow it).
pub struct EventSpec {
    pub topic: Topic,
    pub payload: super::ws_handler::TsType,
    pub scoped: bool,
}

impl EventSpec {
    fn of<T: ts_rs::TS + 'static>(topic: Topic, scoped: bool) -> Self {
        Self {
            topic,
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

/// `exception-occurred`: `{ exception }`.
#[derive(Debug, Serialize, Deserialize, TS)]
pub struct ExceptionOccurredEvent {
    pub exception: ExceptionInfo,
}

/// Every event the executor emits, with its payload type, in the order of `Topic::ALL`.
pub fn event_specs() -> Vec<EventSpec> {
    use crate::perspectives::auto_processor::events::{
        AutoProcessorEvent, AutoProcessorNeighbourhoodState,
    };
    use Topic::*;
    vec![
        EventSpec::of::<AgentStatusChangedEvent>(AgentStatusChanged, false),
        EventSpec::of::<AgentUpdatedEvent>(AgentUpdated, false),
        EventSpec::of::<Apps>(AppsChanged, false),
        EventSpec::of::<HostingUserInfo>(HostingUserInfoChanged, false),
        EventSpec::of::<PerspectiveEvent>(PerspectiveAdded, true),
        EventSpec::of::<PerspectiveRemovedEvent>(PerspectiveRemoved, true),
        EventSpec::of::<PerspectiveEvent>(PerspectiveUpdated, true),
        EventSpec::of::<PerspectiveStateFilter>(PerspectiveSyncStateChange, true),
        EventSpec::of::<PerspectiveLinkEvent>(LinkAdded, true),
        EventSpec::of::<PerspectiveLinkEvent>(LinkRemoved, true),
        EventSpec::of::<PerspectiveLinkUpdatedEvent>(LinkUpdated, true),
        EventSpec::of::<NeighbourhoodSignalFilter>(NeighbourhoodSignal, true),
        EventSpec::of::<NotificationTriggeredEvent>(RuntimeNotificationTriggered, true),
        EventSpec::of::<ExceptionOccurredEvent>(ExceptionOccurred, false),
        EventSpec::of::<TranscriptionTextFilter>(AiTranscriptionText, false),
        EventSpec::of::<AIModelLoadingStatus>(AiModelLoadingStatus, false),
        EventSpec::of::<PerspectiveQuerySubscriptionFilter>(PerspectiveQuerySubscription, true),
        EventSpec::of::<AutoProcessorEvent>(AutoProcessorEvent, true),
        EventSpec::of::<AutoProcessorNeighbourhoodState>(AutoProcessorNeighbourhoodState, true),
    ]
}

/// The event stream of a session's socket: the JSON message of each event the session may
/// receive and `interest` (what the socket asked for with `events.watch`) wants. The
/// session's DID is read per event, so a socket opened before the main agent had a DID gets
/// its events once it has one. The stream ends when the session's grant ends (revoked or
/// removed, without waiting for another event), and at the first event after
/// `session.check()` fails: expiry and revocation stop events as they stop requests.
pub(crate) async fn build_event_stream(
    session: Arc<Session>,
    interest: SharedInterest,
) -> Pin<Box<dyn futures::stream::Stream<Item = String> + Send>> {
    use futures::future::ready;

    let events = get_global_pubsub().await.subscribe_events();
    let check = session.clone();
    let ended = session.ended();
    // The session decides who, the socket's interest what. An event the session may receive
    // checks the token even when the socket does not want it.
    Box::pin(
        BroadcastStream::new(events)
            .filter_map(received)
            .filter_map(move |event| {
                ready(session.may_receive(&event).then(|| {
                    event_interest::wants(&interest, &event).then(|| wire_message(&event))
                }))
            })
            .take_while(move |_| ready(check.check().is_ok()))
            .filter_map(ready)
            .take_until(ended.cancelled_owned()),
    )
}

/// The socket message for `event`, built without parsing the payload again.
pub(crate) fn wire_message(event: &Event) -> String {
    let event_type = event.topic.as_str();
    let payload = &event.payload;
    if let Some(key) = payload_key(event.topic) {
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

// ── Event helper functions ──────────────────────────────────────────────────

/// The message a socket's receiver got, or `None` when it lagged.
fn received<T>(r: Result<T, BroadcastStreamRecvError>) -> futures::future::Ready<Option<T>> {
    futures::future::ready(match r {
        Ok(msg) => Some(msg),
        Err(BroadcastStreamRecvError::Lagged(n)) => {
            // The socket's receiver fell more than the channel's capacity behind (a slow
            // client), so these events are gone for this socket. Say so: the client cannot
            // know it missed them.
            log::warn!("events: a socket's event stream lagged and dropped {n} event(s)");
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

#[cfg(test)]
mod app_approval_event_tests {
    use super::*;
    use crate::agent::capabilities::*;
    use crate::agent::session::Principal;
    use crate::agent::AgentContext;
    use crate::events::Audience;
    use std::collections::BTreeSet;
    use std::time::Duration;

    /// An app's session: a grant named `name` on the main agent.
    fn app(name: &str, caps: Vec<Capability>) -> Arc<Session> {
        let main = AgentContext::main_agent();
        let grant = crate::agent::grants::LiveGrant::for_test(name, main.clone(), u64::MAX);
        Arc::new(
            Session::for_test(Principal::Agent(main))
                .with_caps(caps)
                .with_grant(Arc::new(grant)),
        )
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
    // sessions of the agent the app asks for (the main agent here) that hold `AGENT_PERMIT`,
    // and no other session: not one of the main agent without it, nor another agent's that
    // holds it. An app that may approve but not subscribe to exceptions still gets the
    // request (it went to every approver before, through the exception subscription only).
    #[tokio::test]
    async fn app_approval_events_reach_exactly_the_main_agents_sessions_holding_agent_permit() {
        use crate::agent::capabilities::{
            generate_capability_token, permit_capability, request_capability, AuthInfo,
            AuthInfoExtended,
        };
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
        let operator = Arc::new(Session::for_test(Principal::Operator));
        let user = |name: &str, caps: Vec<Capability>| {
            Session::for_test(Principal::Agent(AgentContext::for_user_email(format!(
                "approval.{name}@example.org"
            ))))
            .with_caps(caps)
            .with_did(&format!("did:key:approval-{name}"))
        };
        let mut user_caps = get_user_default_capabilities();
        let user_session = user("user", user_caps.clone());
        user_caps.push(AGENT_PERMIT_CAPABILITY.clone());
        let permitting_user = user("permitting-user", user_caps);
        // A socket that never authenticated gets no stream and acts as no agent.
        let anonymous = Session::for_test(Principal::Anonymous);
        assert!(!anonymous.is_authenticated());
        assert_eq!(anonymous.acts_as_did(), None);
        let permit_only = vec![
            AGENT_PERMIT_CAPABILITY.clone(),
            AGENT_READ_CAPABILITY.clone(),
        ];
        assert!(!app("x", permit_only.clone()).may(&RUNTIME_EXCEPTION_SUBSCRIBE_CAPABILITY));
        let sessions = [
            ("operator", operator, true),
            (
                "permit app",
                app("permit", vec![ALL_CAPABILITY.clone()]),
                true,
            ),
            ("permit-only app", app("permit-only", permit_only), true),
            (
                "reader app",
                app("reader", vec![AGENT_READ_CAPABILITY.clone()]),
                false,
            ),
            ("user", Arc::new(user_session), false),
            ("permitting user", Arc::new(permitting_user), false),
        ];
        let mut streams = Vec::new();
        for (name, session, approver) in sessions {
            let all = event_interest::watching_all();
            streams.push((name, approver, build_event_stream(session, all).await));
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

        let approval: BTreeSet<String> = [
            Topic::ExceptionOccurred.as_str(),
            Topic::AppsChanged.as_str(),
        ]
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
        let operator = Arc::new(Session::for_test(Principal::Operator));
        let mut stream = build_event_stream(operator, all).await;
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
            ("agent-status-changed", "agent"),
            ("agent-updated", "agent"),
            ("exception-occurred", "exception"),
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

    // A live query update was the last string topic: its message was the update's fields
    // beside `type`. The typed event sends the same message.
    #[test]
    fn a_live_query_update_goes_out_as_its_fields_beside_its_type() {
        let update = crate::types::PerspectiveQuerySubscriptionFilter {
            perspective_uuid: "p".into(),
            uuid: "p".into(),
            subscription_id: "s".into(),
            result: "[]".into(),
        };
        let event = Event::new(
            Topic::PerspectiveQuerySubscription,
            Audience::Dids(vec![]),
            &update,
        );
        assert_eq!(
            serde_json::from_str::<Value>(&wire_message(&event)).unwrap(),
            json!({
                "type": "query-subscription-update",
                "perspectiveUuid": "p",
                "uuid": "p",
                "subscriptionId": "s",
                "result": "[]",
            })
        );
    }
}

#[cfg(test)]
mod audience_matrix_tests {
    //! Plan 20 §8.2: for each (topic, audience) case, which sessions' event streams deliver
    //! the event. Each case goes through `Session::may_receive` and through a real stream
    //! from `build_event_stream`. A new topic or audience adds its cases here.
    use super::*;
    use crate::agent::capabilities::*;
    use crate::agent::session::Principal;
    use crate::agent::AgentContext;
    use crate::events::Audience;
    use std::collections::BTreeSet;
    use std::time::Duration;

    const A: &str = "did:key:matrix-user-a";
    const B: &str = "did:key:matrix-user-b";
    /// The perspective the "P reader" app may query.
    const P: &str = "matrix-perspective-p";
    /// A perspective the "P reader" app may not query.
    const Q: &str = "matrix-perspective-q";

    fn user(name: &str, did: &str) -> Session {
        let agent = AgentContext::for_user_email(format!("matrix.{name}@example.org"));
        Session::for_test(Principal::Agent(agent))
            .with_caps(get_user_default_capabilities())
            .with_did(did)
    }

    /// A grant named `name` on the main agent.
    fn app(name: &str, caps: Vec<Capability>) -> Session {
        let main = AgentContext::main_agent();
        let grant = crate::agent::grants::LiveGrant::for_test(name, main.clone(), u64::MAX);
        Session::for_test(Principal::Agent(main))
            .with_caps(caps)
            .with_grant(Arc::new(grant))
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
            ("operator", Session::for_test(Principal::Operator)),
            ("app", app("app", get_user_default_capabilities())),
            // An app whose token may approve apps (`AGENT_PERMIT`, here through `ALL`).
            (
                "permit app",
                app("permit-app", vec![ALL_CAPABILITY.clone()]),
            ),
            ("user A", user("a", A)),
            ("user B", user("b", B)),
            ("AI app", app("ai-app", ai)),
            ("P reader", app("p-reader", p_reader)),
            ("anonymous", Session::for_test(Principal::Anonymous)),
        ]
        .into_iter()
        .map(|(name, session)| (name, Arc::new(session)))
        .collect()
    }

    type Stream = Pin<Box<dyn futures::stream::Stream<Item = String> + Send>>;

    /// A matrix case: its name, the event as its publisher builds it (its payload is replaced
    /// by the case's tag) and the sessions that receive it.
    type Case<'a> = (String, Event, Vec<&'a str>);

    /// An event of `topic` for `audience`, about no perspective.
    fn event(topic: Topic, audience: Audience) -> Event {
        Event::new(topic, audience, serde_json::Value::Null)
    }

    /// A stream that watches every event, as a socket that asked for all of them.
    async fn open(session: Arc<Session>) -> Stream {
        build_event_stream(session, event_interest::watching_all()).await
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
        // The main agent's sessions that hold AGENT_PERMIT.
        let approvers = ["operator", "permit app"];
        let mut cases: Vec<Case> = vec![
            // The AI app holds AI_READ; the P reader and anonymous callers do not.
            (
                "everyone".into(),
                event(Topic::AiModelLoadingStatus, Audience::Everyone),
                vec![
                    "operator",
                    "app",
                    "permit app",
                    "user A",
                    "user B",
                    "AI app",
                ],
            ),
            // An app approval for the main agent: its sessions that may approve apps.
            (
                "apps-changed-of-main".into(),
                Event::apps_changed(&Default::default(), &AgentContext::main_agent()),
                approvers.to_vec(),
            ),
            // One for user A: A's sessions holding AGENT_PERMIT, which a user token lacks yet.
            (
                "apps-changed-of-a".into(),
                event(Topic::AppsChanged, Audience::Dids(vec![A.into()])),
                vec![],
            ),
            (
                "user-a".into(),
                event(Topic::LinkAdded, Audience::Dids(vec![A.into()])),
                vec!["user A"],
            ),
            (
                "users-a-b".into(),
                event(
                    Topic::NeighbourhoodSignal,
                    Audience::Dids(vec![A.into(), B.into()]),
                ),
                vec!["user A", "user B"],
            ),
            (
                "main-agent".into(),
                event(
                    Topic::PerspectiveUpdated,
                    Audience::Dids(vec![main.clone()]),
                ),
                main_agent.to_vec(),
            ),
            (
                "no-did".into(),
                event(Topic::AgentUpdated, Audience::Dids(vec![])),
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
                let e = event(topic, Audience::agent(did));
                cases.push((format!("{topic}-{name}"), e, to));
            }
        }
        // A triggered notification goes to its owner (`matches_notification_owner` before),
        // never to the perspective's co-owners.
        for (name, owner, to) in [
            ("a", A, vec!["user A"]),
            ("main", &main, main_agent.to_vec()),
        ] {
            let audience = Audience::Dids(vec![owner.to_string()]);
            let e = event(Topic::RuntimeNotificationTriggered, audience);
            cases.push((format!("notification-of-{name}"), e, to));
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
            let e = event(Topic::NeighbourhoodSignal, signal.audience);
            cases.push((format!("signal-{name}"), e, to));
        }
        // Transcription text goes to the stream's owner (`matches_transcription_user` before).
        // The AI app acts as the main agent and may transcribe.
        let mut main_and_ai = main_agent.to_vec();
        main_and_ai.push("AI app");
        for (name, did, to) in [("a", A, vec!["user A"]), ("main", &main, main_and_ai)] {
            let e = event(Topic::AiTranscriptionText, Audience::agent(Some(did)));
            cases.push((format!("transcription-of-{name}"), e, to));
        }
        // The auto-processor topics: a pass to its owner when the owner reads the perspective,
        // a neighbourhood state to the perspective's readers. The operator acts as the main
        // agent: it no longer sees another agent's pass (`OperatorAndDids` before), whose
        // debug events carry that agent's LLM prompts.
        {
            use crate::perspectives::auto_processor::events::{
                neighbourhood_state_audience, pass_audience,
            };
            let owners = |dids: &[&str]| dids.iter().map(|d| d.to_string()).collect::<Vec<_>>();
            let pass = Topic::AutoProcessorEvent;
            let state = Topic::AutoProcessorNeighbourhoodState;
            for (name, topic, audience, to) in [
                (
                    "pass-of-a",
                    pass,
                    pass_audience(&owners(&[A]), Some(A)),
                    vec!["user A"],
                ),
                (
                    "pass-of-a-on-b",
                    pass,
                    pass_audience(&owners(&[B]), Some(A)),
                    vec![],
                ),
                (
                    "pass-of-main",
                    pass,
                    pass_audience(&owners(&[&main]), Some(&main)),
                    main_agent.to_vec(),
                ),
                (
                    "pass-of-a-unowned",
                    pass,
                    pass_audience(&owners(&[&main]), Some(A)),
                    vec![],
                ),
                ("pass-unknown", pass, pass_audience(&[], Some(A)), vec![]),
                (
                    "pass-no-owner",
                    pass,
                    pass_audience(&owners(&[&main]), None),
                    vec![],
                ),
                (
                    "state-of-a-b",
                    state,
                    neighbourhood_state_audience(owners(&[A, B])),
                    vec!["user A", "user B"],
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
                    neighbourhood_state_audience(vec![]),
                    vec![],
                ),
            ] {
                cases.push((name.to_string(), event(topic, audience), to));
            }
        }
        // Exceptions go per type (`exception_visible` before): approval requests to the main
        // agent's sessions that may approve apps (whatever their exception subscription), the
        // others to every session that may subscribe to exceptions.
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
                cases.push((name, Event::exception(&info), to));
            }
        }
        // Perspective topics name the perspective's owners, or the main agent for an
        // unowned perspective: the rule the string filters applied before (`owner` equal to
        // the session's DID, which is the main agent's for the operator and apps). Each goes
        // out once, to all of them.
        let handle = |owners: Option<Vec<&str>>| crate::types::PerspectiveHandle {
            owners: owners.map(|o| o.into_iter().map(String::from).collect()),
            ..Default::default()
        };
        let recipients = crate::perspectives::event_recipients;
        for topic in [
            Topic::PerspectiveAdded,
            Topic::PerspectiveRemoved,
            Topic::PerspectiveUpdated,
            Topic::PerspectiveSyncStateChange,
            Topic::LinkAdded,
            Topic::LinkRemoved,
            Topic::LinkUpdated,
            Topic::PerspectiveQuerySubscription,
        ] {
            for (name, perspective, to) in [
                ("owned-by-a", handle(Some(vec![A])), vec!["user A"]),
                (
                    "owned-by-a-b",
                    handle(Some(vec![A, B])),
                    vec!["user A", "user B"],
                ),
                ("unowned", handle(None), main_agent.to_vec()),
                ("no-owners", handle(Some(vec![])), main_agent.to_vec()),
            ] {
                let e = event(topic, Audience::Dids(recipients(&perspective)));
                cases.push((format!("{topic}-{name}"), e, to));
            }
        }
        // #1284: an app acts as the main agent, so the audience alone gave it every event of
        // every perspective the main agent owns. A per-perspective topic also needs the
        // capability that reads it on that perspective: the P reader gets the main agent's
        // events on P only, the AI app none. The rows above carry no perspective, so they
        // need the capability on every perspective (`*`), which the P reader lacks.
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
                let e = event(topic, audience).for_perspective(on);
                cases.push((format!("{topic}-{name}"), e, to));
            }
        }
        // A signal needs the neighbourhood capability: the P reader may query P, not read
        // its neighbourhood.
        cases.push((
            "signal-main-on-p".into(),
            event(Topic::NeighbourhoodSignal, to_main()).for_perspective(P),
            main_agent.to_vec(),
        ));

        let mut streams = open_all(&sessions).await;

        let run = uuid::Uuid::new_v4().to_string();
        let tag = |case: &str| format!("{run}:{case}");
        let pubsub = get_global_pubsub().await;
        for (case, event, to) in &cases {
            let event = Event {
                payload: json_case(&tag(case)),
                ..event.clone()
            };
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

    /// `(type, perspectiveUuid)` of each message `stream` delivers for a perspective of this
    /// run, sorted, until the `fence` perspective's message. No message names an `owner`: an
    /// event goes out once to all of its recipients (D9).
    async fn perspective_events(
        stream: &mut Stream,
        run: &str,
        fence: &str,
    ) -> Vec<(String, String)> {
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
                assert!(v.get("owner").is_none(), "a message names its owner: {msg}");
                got.push((v["type"].as_str().unwrap().to_string(), uuid.to_string()));
            }
        })
        .await
        .unwrap_or_else(|_| panic!("the fence never arrived; got {got:?}"));
        got.sort();
        got
    }

    /// The sorted `(type, perspectiveUuid)` pairs of `topics` for one perspective.
    fn expect(topics: &[Topic], uuid: &str) -> Vec<(String, String)> {
        let mut list: Vec<_> = topics
            .iter()
            .map(|t| (t.as_str().to_string(), uuid.to_string()))
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

        let lifecycle = |uuid: &str| {
            use Topic::*;
            let topics = [
                PerspectiveAdded,
                PerspectiveUpdated,
                PerspectiveUpdated,
                PerspectiveSyncStateChange,
                PerspectiveRemoved,
            ];
            expect(&topics, uuid)
        };
        for (name, stream) in &mut streams {
            let got = perspective_events(stream, &run, &fence).await;
            let expected = match *name {
                "user A" => lifecycle(&owned.uuid),
                "operator" | "app" | "permit app" => lifecycle(&unowned.uuid),
                _ => vec![],
            };
            assert_eq!(got, expected, "{name}");
        }
    }

    // D9: a link change is one event to all of the perspective's owners, not one per owner
    // naming that owner in `owner`. Each owner's stream gets each change exactly once, with
    // no `owner` field; nobody else gets any.
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
        let mut published = get_global_pubsub().await.subscribe_events();

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

        let changes = [Topic::LinkAdded, Topic::LinkUpdated, Topic::LinkRemoved];
        let shared_uuid = format!("{run}-owned-by-a-b");
        let unowned_uuid = format!("{run}-unowned");
        // One event per change on the shared perspective, to both owners.
        let mut shared_events = Vec::new();
        while let Ok(event) = published.try_recv() {
            if event.perspective.as_deref() == Some(shared_uuid.as_str()) {
                shared_events.push((event.topic, event.audience.clone()));
            }
        }
        let both = Audience::Dids(vec![A.into(), B.into()]);
        assert_eq!(
            shared_events,
            changes.map(|t| (t, both.clone())).to_vec(),
            "one event per change"
        );
        for (name, stream) in &mut streams {
            let got = perspective_events(stream, &run, &fence).await;
            let expected = match *name {
                "user A" | "user B" => expect(&changes, &shared_uuid),
                "operator" | "app" | "permit app" => expect(&changes, &unowned_uuid),
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
            (
                Topic::NeighbourhoodSignal.to_string(),
                p.uuid.clone(),
                to.to_string(),
            )
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

    // `send_signal` and `send_broadcast` deliver to this executor's own agents in a
    // neighbourhood through `event_recipients`: a signal to an owner is published here (once,
    // naming it), a signal to anyone else goes to the link language; a broadcast reaches every
    // owner but the main agent when the main agent sent it. Without a link language the
    // remote part fails after the local delivery.
    #[tokio::test]
    async fn signals_to_this_executors_agents_are_delivered_here() {
        use crate::perspectives::perspective_instance::PerspectiveInstance;
        use crate::types::{PerspectiveExpression, PerspectiveHandle};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main = crate::agent::did();
        let run = uuid::Uuid::new_v4().to_string();
        let neighbourhood = |name: &str, owners: Vec<String>| {
            PerspectiveInstance::new(
                PerspectiveHandle {
                    uuid: format!("{run}-{name}"),
                    shared_url: Some(format!("neighbourhood://{run}-{name}")),
                    owners: Some(owners),
                    ..Default::default()
                },
                None,
            )
        };
        let shared = neighbourhood("a-b", vec![A.into(), B.into()]);
        let with_main = neighbourhood("a-main", vec![A.into(), main.clone()]);
        let mut published = get_global_pubsub().await.subscribe_events();
        let mut signals = || {
            let mut got = Vec::new();
            while let Ok(event) = published.try_recv() {
                let ours = event
                    .perspective
                    .as_deref()
                    .is_some_and(|p| p.starts_with(&run));
                if event.topic == Topic::NeighbourhoodSignal && ours {
                    let recipient = event.payload["recipient"].as_str().unwrap().to_string();
                    got.push((event.audience.clone(), recipient));
                }
            }
            got.sort_by(|x: &(Audience, String), y| x.1.cmp(&y.1));
            got
        };
        let to = |did: &str| (Audience::Dids(vec![did.to_string()]), did.to_string());

        shared
            .send_signal(B.into(), PerspectiveExpression::default())
            .await
            .expect("an owner's signal is delivered here");
        assert_eq!(signals(), vec![to(B)]);
        assert!(shared
            .send_signal("did:key:remote".into(), PerspectiveExpression::default())
            .await
            .is_err());
        assert_eq!(
            signals(),
            vec![],
            "a remote agent's signal goes to the link language"
        );

        let from_main = PerspectiveExpression {
            author: main.clone(),
            ..Default::default()
        };
        let _ = with_main.send_broadcast(from_main, false).await;
        assert_eq!(signals(), vec![to(A)], "the main agent sent it");
        let from_a = PerspectiveExpression {
            author: A.into(),
            ..Default::default()
        };
        let _ = with_main.send_broadcast(from_a, false).await;
        let mut expected = vec![to(A), to(&main)];
        expected.sort_by(|x, y| x.1.cmp(&y.1));
        assert_eq!(signals(), expected);
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
        let hosted = Session::for_test(Principal::Agent(AgentContext::for_user_email(
            email.clone(),
        )))
        .with_caps(get_user_default_capabilities())
        .with_did(&did);
        let mut sessions = sessions();
        sessions.push(("hosted user", Arc::new(hosted)));

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

    // The auto-processor's publishers: a pass's step signal reaches the pass owner where the
    // owner may read the perspective; a neighbourhood-state event reaches every reader. The
    // readers come from the registered handle at publish. The operator acts as the main
    // agent: it gets the main agent's passes, not a managed agent's (it got every pass
    // before, LLM prompts in debug events included).
    #[tokio::test]
    async fn auto_processor_events_reach_the_pass_owner_and_the_readers() {
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
                "operator" | "app" | "permit app" => &["pass-of-main", "state-unowned"],
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
        let session = Arc::new(Session::for_test(Principal::Operator));
        let interest = SharedInterest::default();
        event_interest::handle_control(
            event_interest::WATCH,
            &serde_json::Value::Null,
            &serde_json::json!({ "link-added": ["wanted"] }),
            &interest,
        );
        let mut stream: Stream = build_event_stream(session, interest).await;

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
        let expired =
            Arc::new(app("expired", vec![AI_READ_CAPABILITY.clone()]).expiring_at(an_hour_ago));
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

    // A live query update is a typed event to the perspective's owners (`event_recipients`,
    // read when it is published) that may query it. Only they get it, and delivering it takes
    // no lock: an owner gets it while a write holds the perspective's handle. Each socket
    // used to wait for the handle to check ownership (`perspective_is_owned_by`), and to let
    // every update of an unowned perspective through to every session, users included.
    #[tokio::test]
    async fn a_live_query_update_reaches_the_owners_that_may_query_it_without_a_lock() {
        use crate::perspectives::perspective_instance::PerspectiveInstance;
        use crate::perspectives::{register_perspective, unregister_perspective};
        use crate::types::PerspectiveHandle;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let sessions = sessions();
        let mut streams = open_all(&sessions).await;

        let run = uuid::Uuid::new_v4().to_string();
        let tag = |case: &str| format!("{run}:{case}");
        let mut perspectives = Vec::new();
        for (name, owners) in [("owned-by-a", Some(vec![A.to_string()])), ("unowned", None)] {
            let handle = PerspectiveHandle {
                uuid: format!("{run}-{name}"),
                owners,
                ..Default::default()
            };
            let instance = PerspectiveInstance::new(handle.clone(), None);
            register_perspective(handle.uuid, instance.clone());
            perspectives.push((name, instance));
        }
        let mut published = get_global_pubsub().await.subscribe_events();
        let mut writes = Vec::new();
        for (name, instance) in &perspectives {
            instance
                .send_subscription_update(tag(name), "[]".into(), None)
                .await;
            // A write holds the handle from here until every stream has its updates.
            writes.push(instance.persisted.lock().await);
        }
        // The updates go out from a task; the fence goes after them.
        let mut updates = 0;
        tokio::time::timeout(Duration::from_secs(5), async {
            while updates < perspectives.len() {
                let event = published.recv().await.unwrap();
                let ours = event
                    .perspective
                    .as_deref()
                    .is_some_and(|p| p.starts_with(&run));
                if event.topic == Topic::PerspectiveQuerySubscription && ours {
                    updates += 1;
                }
            }
        })
        .await
        .expect("the updates go out while a write holds the handle");
        let fence = tag("fence");
        publish_fence(json_case(&fence)).await;

        for (name, stream) in &mut streams {
            let got = delivered(stream, &run, &fence).await;
            let expected: BTreeSet<String> = match *name {
                "user A" => [tag("owned-by-a")].into(),
                // The AI app and the P reader may not query these perspectives.
                "operator" | "app" | "permit app" => [tag("unowned")].into(),
                _ => BTreeSet::new(),
            };
            assert_eq!(got, expected, "{name}");
        }
        drop(writes);
        for (_, instance) in &perspectives {
            unregister_perspective(&instance.uuid);
        }
    }
}

#[cfg(test)]
mod token_expiry_tests {
    use super::*;
    use crate::agent::capabilities::*;
    use crate::agent::session::Principal;
    use crate::agent::AgentContext;
    use crate::test_utils::{setup_agent, setup_wallet, MultiUserMode};
    use std::time::Duration;

    const USER: &str = "events.user@example.org";

    /// Builds a user's event stream, publishes one event and returns what the stream yields.
    async fn next_event(session: Session) -> Option<String> {
        let all = event_interest::watching_all();
        let mut stream = build_event_stream(Arc::new(session), all).await;
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
        let user = Principal::Agent(AgentContext::for_user_email(USER.into()));
        let expired = Session::for_test(user)
            .with_caps(get_user_default_capabilities())
            .with_did("did:key:events-user")
            .expiring_at(an_hour_ago);
        assert!(next_event(expired).await.is_none());
    }

    // A revoked grant ends its session's event stream at once, not at the next event: the
    // stream must not wait on an event that may never come.
    #[tokio::test]
    async fn a_revoked_grant_ends_the_event_stream_without_another_event() {
        use crate::agent::grants::LiveGrant;
        let agent = AgentContext::for_user_email(USER.into());
        let grant = Arc::new(LiveGrant::for_test(
            "revoked-events",
            agent.clone(),
            u64::MAX,
        ));
        let session = Session::for_test(Principal::Agent(agent))
            .with_caps(get_user_default_capabilities())
            .with_did("did:key:events-user")
            .with_grant(grant.clone());
        let mut stream =
            build_event_stream(Arc::new(session), event_interest::watching_all()).await;
        grant.ended.cancel();
        let next = tokio::time::timeout(Duration::from_secs(5), stream.next())
            .await
            .expect("the stream ends when the grant does, without another event");
        assert!(next.is_none(), "a revoked session got {next:?}");
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
    use super::event_specs;
    use crate::events::{Audience, Event, Topic};
    use crate::types::*;
    use std::collections::BTreeSet;

    #[test]
    fn every_emitted_event_has_exactly_one_spec() {
        let specs: Vec<Topic> = event_specs().iter().map(|s| s.topic).collect();
        let unique: BTreeSet<&str> = specs.iter().map(|t| t.as_str()).collect();
        assert_eq!(unique.len(), specs.len(), "duplicate spec: {specs:?}");
        let topics = Topic::ALL.map(Topic::as_str);
        assert_eq!(unique, topics.into_iter().collect::<BTreeSet<_>>());
    }

    #[test]
    fn nested_payloads_keep_their_wire_key() {
        let wire =
            super::wrap_event_nested(Topic::AgentUpdated.as_str(), "agent", r#"{"did":"d"}"#);
        let v: serde_json::Value = serde_json::from_str(&wire).unwrap();
        assert_eq!(v["agent"]["did"], "d");
        let _: super::AgentUpdatedEvent = serde_json::from_value(v).unwrap();
    }

    // `events.watch` narrows by `Event::perspective`, which comes from the payload's
    // `perspectiveUuid`. A payload of a topic that is about no perspective carries none, so a
    // watch that lists perspectives never takes it (`EventSpec::scoped` is false).
    #[test]
    fn unscoped_payloads_name_no_perspective() {
        let payloads = [
            (
                Topic::AgentStatusChanged,
                serde_json::to_value(AgentStatus::default()),
            ),
            (Topic::AgentUpdated, serde_json::to_value(Agent::default())),
            (Topic::AppsChanged, serde_json::to_value(Apps::default())),
            (
                Topic::HostingUserInfoChanged,
                serde_json::to_value(HostingUserInfo::default()),
            ),
            (
                Topic::ExceptionOccurred,
                serde_json::to_value(ExceptionInfo::default()),
            ),
            (
                Topic::AiTranscriptionText,
                serde_json::to_value(TranscriptionTextFilter::default()),
            ),
            (
                Topic::AiModelLoadingStatus,
                serde_json::to_value(AIModelLoadingStatus::default()),
            ),
        ];
        let unscoped: BTreeSet<&str> = event_specs()
            .iter()
            .filter(|s| !s.scoped)
            .map(|s| s.topic.as_str())
            .collect();
        assert_eq!(
            unscoped,
            payloads.iter().map(|(t, _)| t.as_str()).collect(),
            "every unscoped topic has a payload here"
        );
        for (topic, payload) in payloads {
            let event = Event::new(topic, Audience::Everyone, payload.unwrap());
            assert_eq!(event.perspective, None, "{topic}");
        }
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
            uuid_of(PerspectiveEvent {
                perspective_uuid: p(),
                perspective: handle.clone(),
            }),
            uuid_of(PerspectiveRemovedEvent {
                perspective_uuid: p(),
                uuid: p(),
            }),
            uuid_of(PerspectiveStateFilter {
                perspective_uuid: p(),
                state: PerspectiveState::Private,
                perspective: handle.clone(),
            }),
            uuid_of(PerspectiveLinkEvent {
                perspective_uuid: p(),
                ..Default::default()
            }),
            uuid_of(PerspectiveLinkUpdatedEvent {
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

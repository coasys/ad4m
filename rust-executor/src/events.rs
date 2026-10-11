//! Typed events: every event names its topic and who may receive it.
//!
//! A publisher builds an [`Event`] with [`Event::new`] and hands it to
//! `PubSub::publish_event`. Each socket's event stream delivers it when
//! `Session::may_receive` (who) and `event_interest::wants` (what the socket asked for)
//! both hold. The payload is serialized once, here, not once per socket.

use crate::agent::{did_for_context, AgentContext, AgentService};
use crate::api::events_ws::events as names;
use crate::types::{Agent, AgentStatus, ExceptionInfo, ExceptionType, HostingUserInfo};
use serde::Serialize;
use serde_json::Value;

/// One event topic. Each string topic in `pubsub` has one variant
/// (`AGENT_UPDATED_TOPIC` → [`Topic::AgentUpdated`]); [`Topic::as_str`] gives the
/// event type the socket sends (`agent-updated`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Topic {
    AgentStatusChanged,
    AgentUpdated,
    AppsChanged,
    HostingUserInfoChanged,
    PerspectiveAdded,
    PerspectiveRemoved,
    PerspectiveUpdated,
    PerspectiveSyncStateChange,
    LinkAdded,
    LinkRemoved,
    LinkUpdated,
    NeighbourhoodSignal,
    RuntimeMessageReceived,
    RuntimeNotificationTriggered,
    ExceptionOccurred,
    AiTranscriptionText,
    AiModelLoadingStatus,
    PerspectiveQuerySubscription,
    AutoProcessorEvent,
    AutoProcessorNeighbourhoodState,
}

impl Topic {
    /// Every topic, in the order of `events_ws::events::ALL`.
    pub const ALL: [Topic; 20] = [
        Topic::AgentStatusChanged,
        Topic::AgentUpdated,
        Topic::AppsChanged,
        Topic::HostingUserInfoChanged,
        Topic::PerspectiveAdded,
        Topic::PerspectiveRemoved,
        Topic::PerspectiveUpdated,
        Topic::PerspectiveSyncStateChange,
        Topic::LinkAdded,
        Topic::LinkRemoved,
        Topic::LinkUpdated,
        Topic::NeighbourhoodSignal,
        Topic::RuntimeMessageReceived,
        Topic::RuntimeNotificationTriggered,
        Topic::ExceptionOccurred,
        Topic::AiTranscriptionText,
        Topic::AiModelLoadingStatus,
        Topic::PerspectiveQuerySubscription,
        Topic::AutoProcessorEvent,
        Topic::AutoProcessorNeighbourhoodState,
    ];

    /// The event type on the wire: the `type` field of the socket message and the key
    /// of `events.watch` params.
    pub const fn as_str(self) -> &'static str {
        match self {
            Topic::AgentStatusChanged => names::AGENT_STATUS_CHANGED,
            Topic::AgentUpdated => names::AGENT_UPDATED,
            Topic::AppsChanged => names::APPS_CHANGED_EVENT,
            Topic::HostingUserInfoChanged => names::HOSTING_USER_INFO_CHANGED,
            Topic::PerspectiveAdded => names::PERSPECTIVE_ADDED,
            Topic::PerspectiveRemoved => names::PERSPECTIVE_REMOVED,
            Topic::PerspectiveUpdated => names::PERSPECTIVE_UPDATED,
            Topic::PerspectiveSyncStateChange => names::SYNC_STATE_CHANGE,
            Topic::LinkAdded => names::LINK_ADDED,
            Topic::LinkRemoved => names::LINK_REMOVED,
            Topic::LinkUpdated => names::LINK_UPDATED,
            Topic::NeighbourhoodSignal => names::SIGNAL,
            Topic::RuntimeMessageReceived => names::MESSAGE_RECEIVED,
            Topic::RuntimeNotificationTriggered => names::NOTIFICATION_TRIGGERED,
            Topic::ExceptionOccurred => names::EXCEPTION_OCCURRED,
            Topic::AiTranscriptionText => names::TRANSCRIPTION_TEXT,
            Topic::AiModelLoadingStatus => names::MODEL_LOADING_STATUS,
            Topic::PerspectiveQuerySubscription => names::QUERY_SUBSCRIPTION_UPDATE,
            Topic::AutoProcessorEvent => names::AUTO_PROCESSOR_EVENT,
            Topic::AutoProcessorNeighbourhoodState => names::AUTO_PROCESSOR_NEIGHBOURHOOD_STATE,
        }
    }

    /// The topic whose wire name is `name`.
    #[allow(dead_code, reason = "event_interest keys interest by Topic in 20-3h")]
    pub fn from_wire(name: &str) -> Option<Topic> {
        Topic::ALL.into_iter().find(|t| t.as_str() == name)
    }
}

impl std::fmt::Display for Topic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Who may receive an event. The publisher names it: it holds the data, so it needs no
/// lookup at delivery.
///
/// When live queries belong to a connection (#1207), `Connection(id)` joins these for
/// `perspective-query-subscription`.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(
    dead_code,
    reason = "publishers move to typed events from 20-3b; tests build every variant"
)]
pub enum Audience {
    /// Every session that has an event stream: nothing in the payload is private.
    Everyone,
    /// Every session that is the operator or holds a token: the sessions that received
    /// every broadcast topic before topics named their audience.
    Authenticated,
    /// The node's operator only.
    Operator,
    /// The sessions that may approve apps: the operator, and any app or user whose token
    /// holds `AGENT_PERMIT`. App approvals and notification install requests go here: these
    /// sessions answer them.
    Approvers,
    /// The sessions that act as one of these DIDs. A user acts as their own DID; the
    /// operator and apps act as the node's main agent.
    Dids(Vec<String>),
    /// The operator, and the sessions that act as one of these DIDs: for the auto-processor
    /// topics, which the operator sees on every perspective, other sessions only on theirs.
    OperatorAndDids(Vec<String>),
}

impl Audience {
    /// The sessions of the agent `did`. An agent status without a DID (before
    /// `agent.generate()`) names no agent, so it goes to every session.
    pub fn agent(did: Option<&str>) -> Audience {
        match did {
            Some(did) => Audience::Dids(vec![did.to_string()]),
            None => Audience::Everyone,
        }
    }

    /// Who sees an exception of type `kind`. A capability request carries the request id
    /// that `agent.generateJwt` redeems, and a notification install request carries the
    /// webhook's credentials, so they go to approvers. The other types go to every session.
    pub fn for_exception(kind: &ExceptionType) -> Audience {
        // No `_` arm: a new exception type does not compile until someone decides who sees it.
        match kind {
            ExceptionType::LanguageIsNotLoaded
            | ExceptionType::ExpressionIsNotVerified
            | ExceptionType::AgentIsUntrusted => Audience::Everyone,
            ExceptionType::CapabilityRequested | ExceptionType::InstallNotificationRequest => {
                Audience::Approvers
            }
        }
    }
}

/// An event and the sessions that may receive it.
#[derive(Clone, Debug)]
pub struct Event {
    pub topic: Topic,
    pub audience: Audience,
    /// The perspective the event is about: what `events.watch` narrows by.
    pub perspective: Option<String>,
    /// The socket message without `type`.
    pub payload: Value,
}

#[allow(dead_code, reason = "publishers move to typed events from 20-3b")]
impl Event {
    /// An event of `topic` for `audience`. There is no default audience.
    ///
    /// `perspective` comes from the payload's `perspectiveUuid`, the field the wire
    /// message carries for `events.watch`. A payload that does not serialize is logged
    /// and sent as `null`.
    pub fn new(topic: Topic, audience: Audience, payload: impl Serialize) -> Event {
        let payload = serde_json::to_value(payload).unwrap_or_else(|e| {
            log::error!("event {topic}: the payload does not serialize: {e}");
            Value::Null
        });
        let perspective = payload
            .get("perspectiveUuid")
            .and_then(Value::as_str)
            .map(str::to_string);
        Event {
            topic,
            audience,
            perspective,
            payload,
        }
    }

    /// `agent-status-changed` for `status`, to the agent it names.
    pub fn agent_status(status: &AgentStatus) -> Event {
        let audience = Audience::agent(status.did.as_deref());
        Event::new(Topic::AgentStatusChanged, audience, status)
    }

    /// `agent-updated` for `agent`'s profile, to that agent.
    pub fn agent_updated(agent: &Agent) -> Event {
        let audience = Audience::agent(Some(&agent.did));
        Event::new(Topic::AgentUpdated, audience, agent)
    }

    /// `hosting-user-info-changed` for `info`: to the user it is about, and to the operator
    /// and apps (they act as the main agent). `matches_hosting_user` let every session
    /// without a user email through, and a user's session only for its own email.
    pub fn hosting_user_info(info: &HostingUserInfo) -> Event {
        let user = AgentService::get_user_did_by_email(&info.email);
        if let Err(e) = &user {
            log::warn!(
                "hosting-user-info-changed: no DID for the user, so no user session gets it: {e}"
            );
        }
        let main = did_for_context(&AgentContext::main_agent()).ok();
        let dids = user.ok().into_iter().chain(main).collect();
        Event::new(Topic::HostingUserInfoChanged, Audience::Dids(dids), info)
    }

    /// `exception-occurred` for `exception`, to the audience of its type.
    pub fn exception(exception: &ExceptionInfo) -> Event {
        let audience = Audience::for_exception(&exception.r#type);
        Event::new(Topic::ExceptionOccurred, audience, exception)
    }

    /// The event is about perspective `uuid`, whatever the payload says.
    pub fn for_perspective(mut self, uuid: impl Into<String>) -> Event {
        self.perspective = Some(uuid.into());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn each_topic_has_the_wire_name_of_one_emitted_event() {
        let wire: Vec<&str> = Topic::ALL.iter().map(|t| t.as_str()).collect();
        assert_eq!(wire, names::ALL);
        for topic in Topic::ALL {
            assert_eq!(Topic::from_wire(topic.as_str()), Some(topic));
            assert_eq!(topic.to_string(), topic.as_str());
        }
        assert_eq!(Topic::from_wire("link-added-topic"), None);
    }

    // The rule `exception_visible` applied per socket before exceptions named their audience.
    #[test]
    fn approval_requests_go_to_approvers_and_other_exceptions_to_everyone() {
        use ExceptionType::*;
        for (kind, audience) in [
            (LanguageIsNotLoaded, Audience::Everyone),
            (ExpressionIsNotVerified, Audience::Everyone),
            (AgentIsUntrusted, Audience::Everyone),
            (CapabilityRequested, Audience::Approvers),
            (InstallNotificationRequest, Audience::Approvers),
        ] {
            let info = ExceptionInfo {
                title: "t".into(),
                message: "m".into(),
                r#type: kind,
                addon: Some("a".into()),
            };
            let e = Event::exception(&info);
            assert_eq!(e.topic, Topic::ExceptionOccurred);
            assert_eq!(e.audience, audience, "{:?}", info.r#type);
            assert_eq!(e.payload, serde_json::to_value(&info).unwrap());
        }
    }

    #[test]
    fn an_agent_event_without_a_did_goes_to_everyone() {
        assert_eq!(
            Audience::agent(Some("did:x")),
            Audience::Dids(vec!["did:x".into()])
        );
        assert_eq!(Audience::agent(None), Audience::Everyone);
    }

    #[test]
    fn the_perspective_comes_from_the_payload_unless_named() {
        let payload = json!({ "perspectiveUuid": "p", "link": {} });
        let e = Event::new(Topic::LinkAdded, Audience::Operator, &payload);
        assert_eq!(e.perspective.as_deref(), Some("p"));
        assert_eq!(e.payload, payload);

        let e = Event::new(
            Topic::AgentUpdated,
            Audience::Operator,
            json!({ "did": "d" }),
        );
        assert_eq!(e.perspective, None);
        assert_eq!(e.for_perspective("q").perspective.as_deref(), Some("q"));
    }
}

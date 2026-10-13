//! Typed events: every event names its topic, who may receive it and the capability they need.
//!
//! A publisher builds an [`Event`] with [`Event::new`] and hands it to
//! `PubSub::publish_event`. Each socket's event stream delivers it when
//! `Session::may_receive` (who: the audience and the event's capability) and
//! `event_interest::wants` (what the socket asked for) both hold. The payload is serialized
//! once, here, not once per socket.

use crate::agent::capabilities::types::{Capability, Resource};
use crate::agent::capabilities::{self as caps};
use crate::agent::{did_for_context, AgentContext, AgentService};
use crate::types::{Agent, AgentStatus, Apps, ExceptionInfo, ExceptionType, HostingUserInfo};
use serde::Serialize;
use serde_json::Value;

/// One event topic. [`Topic::as_str`] gives the event type the socket sends
/// (`agent-updated`): the one list of wire names.
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
    RuntimeNotificationTriggered,
    ExceptionOccurred,
    AiTranscriptionText,
    AiModelLoadingStatus,
    PerspectiveQuerySubscription,
    AutoProcessorEvent,
    AutoProcessorNeighbourhoodState,
}

impl Topic {
    /// Every topic, in the order of `events_ws::event_specs`.
    pub const ALL: [Topic; 19] = [
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
            Topic::AgentStatusChanged => "agent-status-changed",
            Topic::AgentUpdated => "agent-updated",
            Topic::AppsChanged => "apps-changed",
            Topic::HostingUserInfoChanged => "hosting-user-info-changed",
            Topic::PerspectiveAdded => "perspective-added",
            Topic::PerspectiveRemoved => "perspective-removed",
            Topic::PerspectiveUpdated => "perspective-updated",
            Topic::PerspectiveSyncStateChange => "sync-state-change",
            Topic::LinkAdded => "link-added",
            Topic::LinkRemoved => "link-removed",
            Topic::LinkUpdated => "link-updated",
            Topic::NeighbourhoodSignal => "signal",
            Topic::RuntimeNotificationTriggered => "notification-triggered",
            Topic::ExceptionOccurred => "exception-occurred",
            Topic::AiTranscriptionText => "transcription-text",
            Topic::AiModelLoadingStatus => "model-loading-status",
            Topic::PerspectiveQuerySubscription => "query-subscription-update",
            Topic::AutoProcessorEvent => "auto-processor-event",
            Topic::AutoProcessorNeighbourhoodState => "auto-processor-neighbourhood-state",
        }
    }

    /// The topic whose wire name is `name`.
    pub fn from_wire(name: &str) -> Option<Topic> {
        Topic::ALL.into_iter().find(|t| t.as_str() == name)
    }

    /// The capability a session needs to receive this topic's events: the one the RPC method
    /// that reads the same data checks. Per-perspective topics name `perspective`; an event
    /// without one needs the capability on every perspective (`*`). [`Event::new`] puts it
    /// on the event; approval exceptions replace it ([`Event::approval_request`]).
    pub fn capability(self, perspective: Option<&str>) -> Capability {
        let on_perspective = || vec![perspective.unwrap_or(caps::WILD_CARD).to_string()];
        // No `_` arm: a new topic does not compile until someone chooses its capability.
        match self {
            // `agent.status`, `agent.get`.
            Topic::AgentStatusChanged | Topic::AgentUpdated => caps::AGENT_READ_CAPABILITY.clone(),
            // An app approval: the sessions that approve apps for the agent (`agent.permitCapability`).
            Topic::AppsChanged => caps::AGENT_PERMIT_CAPABILITY.clone(),
            // `hosting.info`; the main agent's copy needs RUNTIME_USER_MANAGEMENT_READ instead
            // (`Event::hosting_user_info`).
            Topic::HostingUserInfoChanged => caps::RUNTIME_HOSTING_READ_CAPABILITY.clone(),
            // `perspective.get`, `perspective.queryLinks`, `perspective.subscribeQuery`.
            Topic::PerspectiveAdded
            | Topic::PerspectiveRemoved
            | Topic::PerspectiveUpdated
            | Topic::PerspectiveSyncStateChange
            | Topic::LinkAdded
            | Topic::LinkRemoved
            | Topic::LinkUpdated
            | Topic::PerspectiveQuerySubscription
            | Topic::AutoProcessorEvent
            | Topic::AutoProcessorNeighbourhoodState
            // `runtime.notifications` checks AGENT_UPDATE, but a triggered notification carries
            // `triggerMatch`: query results from its perspective.
            | Topic::RuntimeNotificationTriggered => {
                caps::perspective_query_capability(on_perspective())
            }
            // `neighbourhood.onlineAgents` / `otherAgents` check NEIGHBOURHOOD_READ; a signal
            // names it for its perspective, which a `*` grant covers.
            Topic::NeighbourhoodSignal => Capability {
                with: Resource {
                    domain: caps::NEIGHBOURHOOD.to_string(),
                    pointers: on_perspective(),
                },
                can: vec![caps::READ.to_string()],
            },
            // The exception subscription's capability; approval exceptions need AGENT_PERMIT
            // instead (`Event::approval_request`).
            Topic::ExceptionOccurred => caps::RUNTIME_EXCEPTION_SUBSCRIBE_CAPABILITY.clone(),
            // `ai.transcriptionOpen`.
            Topic::AiTranscriptionText => caps::AI_TRANSCRIBE_CAPABILITY.clone(),
            // `ai.modelLoadingStatus`.
            Topic::AiModelLoadingStatus => caps::AI_READ_CAPABILITY.clone(),
        }
    }
}

impl std::fmt::Display for Topic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Who may receive an event. The publisher names it: it holds the data, so it needs no
/// lookup at delivery. Each session in the audience also needs the event's capability
/// ([`Event::capability`]).
///
/// A live query update also names the RPC connection that opened the query
/// ([`Event::connection`]): the audience says who may get it, the connection which socket.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Audience {
    /// Every session: the payload names no agent.
    Everyone,
    /// The sessions that act as one of these DIDs (`Session::acts_as_did`). A grant acts as
    /// its agent; the operator acts as the main agent, which it owns. Empty: nobody.
    Dids(Vec<String>),
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

    /// The sessions of `agent`. An agent without a DID yet (the main agent before
    /// `agent.generate()`) has no session that acts as it, so nobody.
    pub fn of(agent: &AgentContext) -> Audience {
        match did_for_context(agent) {
            Ok(did) => Audience::Dids(vec![did]),
            Err(e) => {
                log::warn!("event audience: the agent has no DID, so nobody gets it: {e}");
                Audience::Dids(vec![])
            }
        }
    }
}

/// An event and the sessions that may receive it.
#[derive(Clone, Debug)]
pub struct Event {
    pub topic: Topic,
    pub audience: Audience,
    /// What a session in the audience must hold to receive the event: the topic's capability
    /// on the event's perspective ([`Topic::capability`]), or AGENT_PERMIT for an approval
    /// request.
    pub capability: Capability,
    /// The perspective the event is about: what `events.watch` narrows by.
    pub perspective: Option<String>,
    /// The socket message without `type`.
    pub payload: Value,
    /// The RPC connection (`RequestContext::connection_id`) the event is for, or `None`
    /// for every socket. Only a live query update sets it ([`Event::for_connection`]): it
    /// goes to the socket of the connection that opened the query, and to no other, even a
    /// socket of the same agent. The key is not part of the wire message.
    pub connection: Option<String>,
}

impl Event {
    /// An event of `topic` for `audience`. There is no default audience.
    ///
    /// `perspective` comes from the payload's `perspectiveUuid`, the field the wire
    /// message carries for `events.watch`, and the capability from the topic on that
    /// perspective. A payload that does not serialize is logged and sent as `null`.
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
            capability: topic.capability(perspective.as_deref()),
            perspective,
            payload,
            connection: None,
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

    /// `hosting-user-info-changed` for `info`, as two events. The user's copy goes to the user
    /// it is about with RUNTIME_HOSTING_READ (`hosting.info`, which shows a session its own
    /// account only). The main agent's copy goes to its sessions that may list the users
    /// (`user.list`, `user.wallet`: RUNTIME_USER_MANAGEMENT_READ), not to every app that
    /// reads hosting: the info names the user's email, credits and wallet. The info names its
    /// user by email; the DID comes from this executor's users table, which a publish never
    /// imports into.
    pub fn hosting_user_info(info: &HostingUserInfo) -> [Event; 2] {
        let user = AgentService::local_did_for_email(&info.email);
        if user.is_none() {
            log::warn!("hosting-user-info-changed: no local user has an agent for the email");
        }
        let main = did_for_context(&AgentContext::main_agent()).ok();
        let topic = Topic::HostingUserInfoChanged;
        [
            Event::new(topic, Audience::Dids(user.into_iter().collect()), info),
            Event {
                capability: caps::RUNTIME_USER_MANAGEMENT_READ_CAPABILITY.clone(),
                ..Event::new(topic, Audience::Dids(main.into_iter().collect()), info)
            },
        ]
    }

    /// `apps-changed` for `apps`, an app approved for agent `target`: to `target`'s
    /// sessions that may approve apps (the topic's capability, AGENT_PERMIT). The payload
    /// carries no token.
    pub fn apps_changed(apps: &Apps, target: &AgentContext) -> Event {
        Event::new(Topic::AppsChanged, Audience::of(target), apps)
    }

    /// `exception-occurred` for `exception`. An approval request goes to the main agent's
    /// approvers ([`Event::approval_request`]); a publisher whose request names another
    /// agent calls that directly. The other types go to every session that may subscribe to
    /// exceptions.
    pub fn exception(exception: &ExceptionInfo) -> Event {
        // No `_` arm: a new exception type does not compile until someone decides who sees it.
        match exception.r#type {
            ExceptionType::LanguageIsNotLoaded
            | ExceptionType::ExpressionIsNotVerified
            | ExceptionType::AgentIsUntrusted => {
                Event::new(Topic::ExceptionOccurred, Audience::Everyone, exception)
            }
            ExceptionType::CapabilityRequested | ExceptionType::InstallNotificationRequest => {
                Event::approval_request(exception, &AgentContext::main_agent())
            }
        }
    }

    /// `exception-occurred` for an approval request (`CapabilityRequested`,
    /// `InstallNotificationRequest`) for agent `target`: to `target`'s sessions that hold
    /// AGENT_PERMIT, which answer it, whether or not they subscribe to other exceptions.
    /// The payload carries the request id that `agent.generateJwt` redeems, or a webhook's
    /// credentials, so no other session gets it.
    pub fn approval_request(exception: &ExceptionInfo, target: &AgentContext) -> Event {
        Event {
            capability: caps::AGENT_PERMIT_CAPABILITY.clone(),
            ..Event::new(Topic::ExceptionOccurred, Audience::of(target), exception)
        }
    }

    /// The event goes only to the socket of the RPC connection `connection_id`, if its
    /// session also may receive it.
    pub fn for_connection(mut self, connection_id: impl Into<String>) -> Event {
        self.connection = Some(connection_id.into());
        self
    }

    /// The event is about perspective `uuid`, whatever the payload says, and needs the
    /// topic's capability on it.
    #[cfg(test)]
    pub fn for_perspective(mut self, uuid: impl Into<String>) -> Event {
        let uuid = uuid.into();
        self.capability = self.topic.capability(Some(&uuid));
        self.perspective = Some(uuid);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn each_topic_has_the_wire_name_of_one_emitted_event() {
        let specs = crate::api::events_ws::event_specs();
        assert_eq!(
            Topic::ALL.to_vec(),
            specs.iter().map(|s| s.topic).collect::<Vec<_>>()
        );
        for topic in Topic::ALL {
            assert_eq!(Topic::from_wire(topic.as_str()), Some(topic));
            assert_eq!(topic.to_string(), topic.as_str());
        }
        assert_eq!(Topic::from_wire("link-added-topic"), None);
        assert_eq!(Topic::from_wire("message-received"), None);
    }

    // The rule `exception_visible` applied per socket before exceptions named their audience.
    // An approval request goes to the main agent's sessions and needs AGENT_PERMIT, not the
    // exception subscription; the other types go to everyone that may subscribe.
    #[test]
    fn approval_requests_go_to_the_main_agents_approvers_and_other_exceptions_to_everyone() {
        use ExceptionType::*;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main = Audience::Dids(vec![crate::agent::did()]);
        let subscribe = caps::RUNTIME_EXCEPTION_SUBSCRIBE_CAPABILITY.clone();
        let permit = caps::AGENT_PERMIT_CAPABILITY.clone();
        for (kind, audience, capability) in [
            (LanguageIsNotLoaded, Audience::Everyone, &subscribe),
            (ExpressionIsNotVerified, Audience::Everyone, &subscribe),
            (AgentIsUntrusted, Audience::Everyone, &subscribe),
            (CapabilityRequested, main.clone(), &permit),
            (InstallNotificationRequest, main.clone(), &permit),
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
            assert_eq!(&e.capability, capability, "{:?}", info.r#type);
            assert_eq!(e.payload, serde_json::to_value(&info).unwrap());
        }
    }

    // #1399: the publisher names the agent an approval is for; its DID is the audience.
    #[test]
    fn an_approval_goes_to_the_agent_it_is_for() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let email = format!("approval-target-{}@example.org", uuid::Uuid::new_v4());
        let did = crate::test_utils::create_test_user(&email);
        let user = AgentContext::managed(did.clone());
        let info = ExceptionInfo {
            r#type: ExceptionType::CapabilityRequested,
            ..Default::default()
        };
        let request = Event::approval_request(&info, &user);
        assert_eq!(request.audience, Audience::Dids(vec![did.clone()]));
        assert_eq!(request.capability, *caps::AGENT_PERMIT_CAPABILITY);
        let approved = Event::apps_changed(&Apps::default(), &user);
        assert_eq!(approved.topic, Topic::AppsChanged);
        assert_eq!(approved.audience, Audience::Dids(vec![did]));
        assert_eq!(approved.capability, *caps::AGENT_PERMIT_CAPABILITY);
    }

    #[test]
    fn an_agent_event_without_a_did_goes_to_everyone() {
        assert_eq!(
            Audience::agent(Some("did:x")),
            Audience::Dids(vec!["did:x".into()])
        );
        assert_eq!(Audience::agent(None), Audience::Everyone);
    }

    // A user token holds every topic's capability on every perspective, so the capability
    // check takes no event from a user session. The operator may do anything. `apps-changed`
    // included: users approve apps for their own agent (AGENT_PERMIT, #1399); the audience
    // keeps another agent's approvals from them.
    #[test]
    fn a_user_token_holds_the_capability_of_every_topic() {
        let user = caps::get_user_default_capabilities();
        for topic in Topic::ALL {
            for perspective in [None, Some("p")] {
                let cap = topic.capability(perspective);
                let held = caps::capability_matches(&user, &cap);
                assert!(held, "{topic}: {cap:?}");
            }
        }
    }

    // A per-perspective topic needs its capability on the event's perspective: a grant on that
    // perspective covers it, a grant on another does not, and an event without a perspective
    // needs a grant on every perspective.
    #[test]
    fn a_per_perspective_topic_needs_its_capability_on_that_perspective() {
        let grant_on = |cap: &Capability, uuid: &str| Capability {
            with: Resource {
                domain: cap.with.domain.clone(),
                pointers: vec![uuid.to_string()],
            },
            can: cap.can.clone(),
        };
        for spec in crate::api::events_ws::event_specs() {
            let topic = spec.topic;
            let on_p = topic.capability(Some("p"));
            if !spec.scoped {
                assert_eq!(on_p.with.pointers, vec![caps::WILD_CARD], "{topic}");
                continue;
            }
            assert!(
                caps::capability_matches(&[grant_on(&on_p, "p")], &on_p),
                "{topic}"
            );
            assert!(
                !caps::capability_matches(&[grant_on(&on_p, "q")], &on_p),
                "{topic}"
            );
            let anywhere = topic.capability(None);
            assert!(
                !caps::capability_matches(&[grant_on(&on_p, "p")], &anywhere),
                "{topic}"
            );
        }
    }

    #[test]
    fn the_perspective_and_capability_come_from_the_payload_unless_named() {
        let payload = json!({ "perspectiveUuid": "p", "link": {} });
        let e = Event::new(Topic::LinkAdded, Audience::Everyone, &payload);
        assert_eq!(e.perspective.as_deref(), Some("p"));
        assert_eq!(e.capability, Topic::LinkAdded.capability(Some("p")));
        assert_eq!(e.payload, payload);

        let e = Event::new(
            Topic::AgentUpdated,
            Audience::Everyone,
            json!({ "did": "d" }),
        );
        assert_eq!(e.perspective, None);
        let e = Event::new(Topic::LinkAdded, Audience::Everyone, json!({})).for_perspective("q");
        assert_eq!(e.perspective.as_deref(), Some("q"));
        assert_eq!(e.capability, Topic::LinkAdded.capability(Some("q")));
    }
}

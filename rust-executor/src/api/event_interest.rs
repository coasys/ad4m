//! Event interest: `events.watch` / `events.unwatch` on the RPC socket. A
//! socket gets no events until it calls `events.watch`.
//!
//! Request: `{ "id": "...", "type": "events.watch", "params": { "<event type>": null | ["<perspective uuid>", ...] } }`
//! Reply:   `{ "id": "...", "result": true }`
//!
//! - Each `events.watch` replaces the previous interest; `events.unwatch`
//!   clears it.
//! - `null` takes every event of that type; a list takes only events about
//!   those perspectives.
//! - Live query updates (`query-subscription-update`) are not filtered: a
//!   connection gets the updates of the live queries it opened without a
//!   watch, and no other connection gets them (`Event::connection`).
//! - A list matches an event about a listed perspective (`Event::perspective`, the
//!   payload's `perspectiveUuid`). Events about no perspective carry none, so a list
//!   never takes them.

use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use super::ws_handler::ParamExt;
use crate::events::{Event, Topic};

pub const WATCH: &str = "events.watch";
pub const UNWATCH: &str = "events.unwatch";

/// Event topic → the perspectives wanted (`None`: all).
pub type EventInterest = HashMap<Topic, Option<HashSet<String>>>;

/// Per-connection interest; empty = no events.
pub type SharedInterest = Arc<RwLock<EventInterest>>;

/// The TypeScript type of `events.watch` params: event name → the
/// perspectives wanted (`null`: all). `EventName` comes from `Events.ts`.
pub struct WatchParams;

impl ts_rs::TS for WatchParams {
    type WithoutGenerics = Self;
    type OptionInnerType = Self;
    fn name(_: &ts_rs::Config) -> String {
        "Partial<Record<EventName, Array<string> | null>>".to_string()
    }
    fn inline(cfg: &ts_rs::Config) -> String {
        Self::name(cfg)
    }
}

fn parse(params: &Value) -> Result<EventInterest, String> {
    let types = params
        .as_object()
        .ok_or("`events.watch` params must map event types to perspectives or null")?;
    let mut interest = EventInterest::new();
    for name in types.keys() {
        let perspectives = params.opt_str_set(name).map_err(|e| e.message)?;
        // A name that is no event type is accepted and matches nothing.
        if let Some(topic) = Topic::from_wire(name) {
            interest.insert(topic, perspectives);
        }
    }
    Ok(interest)
}

/// Should this socket forward `event`? Reads its topic and perspective; parses nothing.
pub(crate) fn wants(interest: &SharedInterest, event: &Event) -> bool {
    if event.topic == Topic::PerspectiveQuerySubscription {
        return true;
    }
    match interest
        .read()
        .unwrap_or_else(|e| e.into_inner())
        .get(&event.topic)
    {
        None => false,
        Some(None) => true,
        Some(Some(wanted)) => event
            .perspective
            .as_deref()
            .is_some_and(|p| wanted.contains(p)),
    }
}

/// An interest in every event, every perspective.
#[cfg(test)]
pub fn watching_all() -> SharedInterest {
    let all = Topic::ALL.map(|t| (t, None));
    Arc::new(RwLock::new(all.into_iter().collect()))
}

/// Handle `events.watch` / `events.unwatch`. Returns `None` for any other
/// message type, otherwise the reply to send. A malformed watch keeps the
/// previous interest.
pub fn handle_control(
    msg_type: &str,
    id: &Value,
    params: &Value,
    interest: &SharedInterest,
) -> Option<String> {
    let new_interest = match msg_type {
        WATCH => parse(params),
        UNWATCH => Ok(EventInterest::new()),
        _ => return None,
    };
    Some(
        match new_interest {
            Ok(i) => {
                *interest.write().unwrap_or_else(|e| e.into_inner()) = i;
                json!({ "id": id, "result": true })
            }
            Err(e) => json!({ "id": id, "error": { "code": 400, "message": e } }),
        }
        .to_string(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::events::Audience;

    fn event(topic: Topic, payload: Value) -> Event {
        Event::new(topic, Audience::Everyone, payload)
    }

    fn link(perspective: &str) -> Event {
        let payload = json!({ "perspectiveUuid": perspective, "link": {} });
        event(Topic::LinkAdded, payload)
    }

    fn updated_b() -> Event {
        let payload = json!({ "perspectiveUuid": "B", "perspective": { "uuid": "B" } });
        event(Topic::PerspectiveUpdated, payload)
    }

    fn agent() -> Event {
        event(Topic::AgentUpdated, json!({ "did": "did:x" }))
    }

    fn watch(i: &SharedInterest, params: Value) -> Value {
        serde_json::from_str(&handle_control(WATCH, &json!("1"), &params, i).unwrap()).unwrap()
    }

    #[test]
    fn without_watch_no_event_passes_but_query_updates_do() {
        let i = SharedInterest::default();
        for e in [link("A"), link("B"), updated_b(), agent()] {
            assert!(!wants(&i, &e), "{e:?}");
        }
        let query = json!({ "perspectiveUuid": "A", "subscriptionId": "s", "revision": 1 });
        assert!(wants(
            &i,
            &event(Topic::PerspectiveQuerySubscription, query)
        ));
    }

    #[test]
    fn watch_narrows_by_type_and_perspective() {
        let i = SharedInterest::default();
        let reply = watch(
            &i,
            json!({ "link-added": ["A"], "perspective-updated": null, "agent-updated": ["A"] }),
        );
        assert_eq!(reply, json!({ "id": "1", "result": true }));
        assert!(wants(&i, &link("A")));
        assert!(!wants(&i, &link("B")), "other perspective");
        assert!(wants(&i, &updated_b()), "null: every perspective");
        assert!(
            !wants(&i, &agent()),
            "a list excludes events without a perspective"
        );
    }

    // A name that is no event type was always accepted and matched nothing.
    #[test]
    fn a_watch_of_an_unknown_event_type_is_accepted_and_matches_nothing() {
        let i = SharedInterest::default();
        let reply = watch(
            &i,
            json!({ "link-added-topic": null, "agent-updated": null }),
        );
        assert_eq!(reply, json!({ "id": "1", "result": true }));
        assert!(wants(&i, &agent()));
        assert!(!wants(&i, &link("A")));
        assert_eq!(
            watch(&i, json!({ "no-such-event": 1 }))["error"]["code"],
            400
        );
    }

    #[test]
    fn each_watch_replaces_the_last_and_unwatch_clears_it() {
        let i = SharedInterest::default();
        watch(&i, json!({ "agent-updated": null }));
        watch(&i, json!({ "link-added": null }));
        assert!(!wants(&i, &agent()));
        assert!(wants(&i, &link("B")));
        let reply = handle_control(UNWATCH, &json!("2"), &json!({}), &i).unwrap();
        assert_eq!(
            serde_json::from_str::<Value>(&reply).unwrap(),
            json!({ "id": "2", "result": true })
        );
        assert!(!wants(&i, &link("B")));
    }

    #[test]
    fn malformed_watch_is_rejected_and_keeps_the_old_interest() {
        let i = SharedInterest::default();
        watch(&i, json!({ "agent-updated": null }));
        for bad in [
            json!({ "link-added": "A" }),
            json!({ "link-added": [1] }),
            json!([]),
        ] {
            assert_eq!(watch(&i, bad)["error"]["code"], 400);
        }
        assert!(wants(&i, &agent()));
    }

    // Every topic, with and without a perspective, under every kind of watch: `null` takes
    // the topic whole; a list takes the topic's events about a listed perspective only (an
    // unscoped topic's payload names none: `events_ws::event_spec_tests`).
    #[test]
    fn each_kind_of_watch_narrows_every_topic() {
        for topic in Topic::ALL {
            let t = topic.as_str();
            let query = topic == Topic::PerspectiveQuerySubscription;
            for (payload, about_a) in [
                (json!({ "perspectiveUuid": "A" }), true),
                (json!({}), false),
            ] {
                let e = event(topic, payload);
                for (params, expected) in [
                    (json!({}), false),
                    (json!({ t: null }), true),
                    (json!({ t: ["A"] }), about_a),
                    (json!({ t: ["B", "A"] }), about_a),
                    (json!({ t: ["B"] }), false),
                ] {
                    let i = SharedInterest::default();
                    watch(&i, params.clone());
                    assert_eq!(wants(&i, &e), expected || query, "{t} {e:?} under {params}");
                }
            }
        }
        let i = SharedInterest::default();
        watch(&i, json!({ "link-added": ["A"] }));
        let unnamed = event(Topic::LinkAdded, json!({}));
        assert!(
            !wants(&i, &unnamed),
            "a list excludes an event without a perspective"
        );
        assert!(wants(&i, &unnamed.for_perspective("A")));
    }

    #[test]
    fn other_messages_are_not_handled() {
        assert!(handle_control("ping", &Value::Null, &json!({}), &Default::default()).is_none());
    }

    // The perspective comes from the flat `perspectiveUuid`, not from a nested one.
    #[test]
    fn perspective_extraction_per_event_type() {
        use Topic::*;
        let cases = [
            (NeighbourhoodSignal, json!({ "perspectiveUuid": "P" }), true),
            (PerspectiveAdded, json!({ "perspectiveUuid": "P" }), true),
            (
                PerspectiveSyncStateChange,
                json!({ "perspectiveUuid": "P", "state": "x" }),
                true,
            ),
            (
                PerspectiveRemoved,
                json!({ "perspectiveUuid": "P", "uuid": "P" }),
                true,
            ),
            (
                RuntimeNotificationTriggered,
                json!({ "perspectiveUuid": "P", "notification": { "perspectiveId": "P" } }),
                true,
            ),
            (
                NeighbourhoodSignal,
                json!({ "perspective": { "uuid": "P" } }),
                false,
            ),
            (AutoProcessorEvent, json!({ "perspectiveUuid": "P" }), true),
            (
                ExceptionOccurred,
                json!({ "exception": { "uuid": "P" } }),
                false,
            ),
            (AgentUpdated, json!({ "did": "P" }), false),
        ];
        for (topic, payload, scoped) in cases {
            let i = SharedInterest::default();
            watch(&i, json!({ topic.as_str(): ["P"] }));
            assert_eq!(
                wants(&i, &event(topic, payload.clone())),
                scoped,
                "{topic} {payload}"
            );
        }
    }
}

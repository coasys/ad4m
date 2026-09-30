//! Event interest: `events.watch` / `events.unwatch`, on both the RPC socket
//! and the events socket. A socket gets no events until it calls
//! `events.watch`.
//!
//! Request: `{ "id": "...", "type": "events.watch", "params": { "<event type>": null | ["<perspective uuid>", ...] } }`
//! Reply:   `{ "id": "...", "result": true }`
//!
//! - Each `events.watch` replaces the previous interest; `events.unwatch`
//!   clears it.
//! - `null` takes every event of that type; a list takes only events about
//!   those perspectives.
//! - Live query updates (`query-subscription-update`) are not filtered: they
//!   only ever reach the connection that opened the subscription.

use futures::stream::{Stream, StreamExt};
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use super::ws_handler::ParamExt;

pub const WATCH: &str = "events.watch";
pub const UNWATCH: &str = "events.unwatch";

/// Event type → the perspectives wanted (`None`: all).
pub type EventInterest = HashMap<String, Option<HashSet<String>>>;

/// Per-connection interest; empty = no events.
pub type SharedInterest = Arc<RwLock<EventInterest>>;

/// The perspective an event is about, by event type. `None` for events that
/// are not perspective-scoped.
fn event_perspective<'a>(event_type: &str, event: &'a Value) -> Option<&'a str> {
    let v = match event_type {
        "link-added"
        | "link-removed"
        | "link-updated"
        | "auto-processor-event"
        | "auto-processor-neighbourhood-state" => event.get("perspectiveUuid"),
        "perspective-added" | "perspective-updated" | "sync-state-change" | "signal" => {
            event.get("perspective").and_then(|p| p.get("uuid"))
        }
        "perspective-removed" => event.get("uuid"),
        "notification-triggered" => event
            .get("notification")
            .and_then(|n| n.get("perspectiveId").or_else(|| n.get("perspective_id"))),
        _ => None,
    };
    v.and_then(Value::as_str)
}

fn parse(params: &Value) -> Result<EventInterest, String> {
    let types = params
        .as_object()
        .ok_or("`events.watch` params must map event types to perspectives or null")?;
    types
        .keys()
        .map(|t| Ok((t.clone(), params.opt_str_set(t).map_err(|e| e.message)?)))
        .collect()
}

/// Should this socket forward `event_json`?
pub fn wants(interest: &SharedInterest, event_json: &str) -> bool {
    let Ok(event) = serde_json::from_str::<Value>(event_json) else {
        return false;
    };
    let event_type = event.get("type").and_then(Value::as_str).unwrap_or("");
    if event_type == "query-subscription-update" {
        return true;
    }
    match interest
        .read()
        .unwrap_or_else(|e| e.into_inner())
        .get(event_type)
    {
        None => false,
        Some(None) => true,
        Some(Some(wanted)) => {
            event_perspective(event_type, &event).is_some_and(|p| wanted.contains(p))
        }
    }
}

/// `stream` minus the events this socket did not ask for.
pub fn filter_stream<S>(stream: S, interest: SharedInterest) -> impl Stream<Item = String>
where
    S: Stream<Item = String>,
{
    stream.filter(move |event| futures::future::ready(wants(&interest, event)))
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

    const LINK_A: &str = r#"{"type":"link-added","perspectiveUuid":"A","owner":"did:x","link":{}}"#;
    const LINK_B: &str = r#"{"type":"link-added","perspectiveUuid":"B","owner":"did:x","link":{}}"#;
    const UPDATED_B: &str =
        r#"{"type":"perspective-updated","perspective":{"uuid":"B"},"owner":"did:x"}"#;
    const QUERY_A: &str =
        r#"{"type":"query-subscription-update","uuid":"A","subscriptionId":"s","revision":1}"#;
    const AGENT: &str = r#"{"type":"agent-updated","agent":{"did":"did:x"}}"#;

    fn watch(i: &SharedInterest, params: Value) -> Value {
        serde_json::from_str(&handle_control(WATCH, &json!("1"), &params, i).unwrap()).unwrap()
    }

    #[test]
    fn without_watch_no_event_passes_but_query_updates_do() {
        let i = SharedInterest::default();
        for e in [LINK_A, LINK_B, UPDATED_B, AGENT] {
            assert!(!wants(&i, e), "{e}");
        }
        assert!(wants(&i, QUERY_A));
    }

    #[test]
    fn watch_narrows_by_type_and_perspective() {
        let i = SharedInterest::default();
        let reply = watch(
            &i,
            json!({ "link-added": ["A"], "perspective-updated": null, "agent-updated": ["A"] }),
        );
        assert_eq!(reply, json!({ "id": "1", "result": true }));
        assert!(wants(&i, LINK_A));
        assert!(!wants(&i, LINK_B), "other perspective");
        assert!(wants(&i, UPDATED_B), "null: every perspective");
        assert!(
            !wants(&i, AGENT),
            "a list excludes events without a perspective"
        );
    }

    #[test]
    fn each_watch_replaces_the_last_and_unwatch_clears_it() {
        let i = SharedInterest::default();
        watch(&i, json!({ "agent-updated": null }));
        watch(&i, json!({ "link-added": null }));
        assert!(!wants(&i, AGENT));
        assert!(wants(&i, LINK_B));
        let reply = handle_control(UNWATCH, &json!("2"), &json!({}), &i).unwrap();
        assert_eq!(
            serde_json::from_str::<Value>(&reply).unwrap(),
            json!({ "id": "2", "result": true })
        );
        assert!(!wants(&i, LINK_B));
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
        assert!(wants(&i, AGENT));
    }

    #[test]
    fn other_messages_are_not_handled() {
        assert!(handle_control("ping", &Value::Null, &json!({}), &Default::default()).is_none());
    }

    #[test]
    fn perspective_extraction_per_event_type() {
        let cases = [
            (r#"{"type":"signal","perspective":{"uuid":"P"}}"#, true),
            (
                r#"{"type":"sync-state-change","perspective":{"uuid":"P"},"state":"x"}"#,
                true,
            ),
            (
                r#"{"type":"perspective-removed","uuid":"P","owner":"o"}"#,
                true,
            ),
            (
                r#"{"type":"notification-triggered","notification":{"perspectiveId":"P"}}"#,
                true,
            ),
            (
                r#"{"type":"auto-processor-event","perspectiveUuid":"P"}"#,
                true,
            ),
            (
                r#"{"type":"exception-occurred","exception":{"uuid":"P"}}"#,
                false,
            ),
        ];
        for (event, scoped) in cases {
            let t = serde_json::from_str::<Value>(event).unwrap()["type"]
                .as_str()
                .unwrap()
                .to_string();
            let i = SharedInterest::default();
            watch(&i, json!({ t: ["P"] }));
            assert_eq!(wants(&i, event), scoped, "{event}");
        }
    }
}

//! Event interest registration: `events.watch` / `events.unwatch`.
//!
//! Per-socket filter over the (already per-user) event stream, on both the
//! events socket and the RPC socket. A socket that never sends `events.watch`
//! receives every event, as before.
//!
//! Request: `{ "id"?: "...", "type": "events.watch", "params": { "types"?: [..], "perspectives"?: [..] } }`
//! Reply:   `{ "id": "...", "result": { "watching": { "types": [..] | null, "perspectives": [..] | null } } }`
//! (`events.unwatch` replies `{ "watching": null }`). A reply has no `type` key,
//! so clients never mistake it for an event.
//!
//! Rules:
//! - Each `events.watch` replaces the previous interest; `events.unwatch` clears it.
//! - An omitted (or `null`) dimension does not restrict; an empty array matches nothing.
//! - `perspectives` restricts only events that name a perspective. Agent,
//!   runtime and AI events carry none and pass on `types` alone.

use futures::stream::{Stream, StreamExt};
use serde_json::{json, Value};
use std::collections::HashSet;
use std::sync::{Arc, RwLock};

pub const WATCH: &str = "events.watch";
pub const UNWATCH: &str = "events.unwatch";

#[derive(Debug, Clone, PartialEq)]
pub struct EventInterest {
    types: Option<HashSet<String>>,
    perspectives: Option<HashSet<String>>,
}

/// Per-connection interest; `None` = every event.
pub type SharedInterest = Arc<RwLock<Option<EventInterest>>>;

fn string_set(params: &Value, key: &str) -> Result<Option<HashSet<String>>, String> {
    match params.get(key) {
        None | Some(Value::Null) => Ok(None),
        Some(Value::Array(items)) => items
            .iter()
            .map(|v| {
                v.as_str()
                    .map(str::to_string)
                    .ok_or_else(|| format!("`{}` must be an array of strings", key))
            })
            .collect::<Result<_, _>>()
            .map(Some),
        Some(_) => Err(format!("`{}` must be an array of strings", key)),
    }
}

fn sorted(set: &Option<HashSet<String>>) -> Value {
    match set {
        None => Value::Null,
        Some(s) => {
            let mut v: Vec<&String> = s.iter().collect();
            v.sort();
            json!(v)
        }
    }
}

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
        "perspective-removed" | "query-subscription-update" => event.get("uuid"),
        "notification-triggered" => event
            .get("notification")
            .and_then(|n| n.get("perspectiveId").or_else(|| n.get("perspective_id"))),
        _ => None,
    };
    v.and_then(Value::as_str)
}

impl EventInterest {
    pub fn from_params(params: &Value) -> Result<Self, String> {
        Ok(Self {
            types: string_set(params, "types")?,
            perspectives: string_set(params, "perspectives")?,
        })
    }

    pub fn to_json(&self) -> Value {
        json!({ "types": sorted(&self.types), "perspectives": sorted(&self.perspectives) })
    }

    /// Does this serialized event (`{ "type": ..., ...payload }`) match?
    pub fn matches(&self, event_json: &str) -> bool {
        let Ok(event) = serde_json::from_str::<Value>(event_json) else {
            return true;
        };
        let event_type = event.get("type").and_then(Value::as_str).unwrap_or("");
        if let Some(types) = &self.types {
            if !types.contains(event_type) {
                return false;
            }
        }
        match (&self.perspectives, event_perspective(event_type, &event)) {
            (Some(wanted), Some(uuid)) => wanted.contains(uuid),
            _ => true,
        }
    }
}

/// Should this socket forward `event_json`?
pub fn wants(interest: &SharedInterest, event_json: &str) -> bool {
    match &*interest.read().unwrap_or_else(|e| e.into_inner()) {
        None => true,
        Some(i) => i.matches(event_json),
    }
}

/// `stream` minus the events this socket's interest excludes.
pub fn filter_stream<S>(stream: S, interest: SharedInterest) -> impl Stream<Item = String>
where
    S: Stream<Item = String>,
{
    stream.filter(move |event| futures::future::ready(wants(&interest, event)))
}

/// Handle `events.watch` / `events.unwatch`. Returns `None` for any other
/// message type, otherwise the reply to send.
pub fn handle_control(
    msg_type: &str,
    id: &Value,
    params: &Value,
    interest: &SharedInterest,
) -> Option<String> {
    let result = match msg_type {
        WATCH => EventInterest::from_params(params).map(|i| {
            let watching = i.to_json();
            *interest.write().unwrap_or_else(|e| e.into_inner()) = Some(i);
            json!({ "watching": watching })
        }),
        UNWATCH => {
            *interest.write().unwrap_or_else(|e| e.into_inner()) = None;
            Ok(json!({ "watching": null }))
        }
        _ => return None,
    };
    Some(
        match result {
            Ok(r) => json!({ "id": id, "result": r }),
            Err(e) => json!({ "id": id, "error": { "code": 400, "message": e } }),
        }
        .to_string(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn shared() -> SharedInterest {
        Arc::new(RwLock::new(None))
    }

    const LINK_A: &str = r#"{"type":"link-added","perspectiveUuid":"A","owner":"did:x","link":{}}"#;
    const LINK_B: &str = r#"{"type":"link-added","perspectiveUuid":"B","owner":"did:x","link":{}}"#;
    const UPDATED_B: &str =
        r#"{"type":"perspective-updated","perspective":{"uuid":"B"},"owner":"did:x"}"#;
    const QUERY_A: &str =
        r#"{"type":"query-subscription-update","uuid":"A","subscriptionId":"s","result":"[]"}"#;
    const AGENT: &str = r#"{"type":"agent-updated","agent":{"did":"did:x"}}"#;

    #[test]
    fn without_watch_every_event_passes() {
        let i = shared();
        for e in [LINK_A, LINK_B, UPDATED_B, QUERY_A, AGENT] {
            assert!(wants(&i, e));
        }
    }

    #[test]
    fn perspectives_filter_scoped_events_only() {
        let i = shared();
        handle_control(WATCH, &json!("1"), &json!({ "perspectives": ["A"] }), &i).unwrap();
        assert!(wants(&i, LINK_A));
        assert!(!wants(&i, LINK_B));
        assert!(!wants(&i, UPDATED_B));
        assert!(wants(&i, QUERY_A));
        assert!(wants(&i, AGENT), "not perspective-scoped: passes");
    }

    #[test]
    fn types_and_perspectives_combine() {
        let i = shared();
        handle_control(
            WATCH,
            &json!("1"),
            &json!({ "types": ["link-added"], "perspectives": ["B"] }),
            &i,
        )
        .unwrap();
        assert!(!wants(&i, LINK_A));
        assert!(wants(&i, LINK_B));
        assert!(!wants(&i, UPDATED_B));
        assert!(!wants(&i, AGENT));
    }

    #[test]
    fn empty_types_match_nothing_and_unwatch_restores_everything() {
        let i = shared();
        handle_control(WATCH, &json!("1"), &json!({ "types": [] }), &i).unwrap();
        assert!(!wants(&i, AGENT));
        let reply = handle_control(UNWATCH, &json!("2"), &json!({}), &i).unwrap();
        assert_eq!(
            serde_json::from_str::<Value>(&reply).unwrap(),
            json!({ "id": "2", "result": { "watching": null } })
        );
        assert!(wants(&i, AGENT));
    }

    #[test]
    fn watch_replaces_and_echoes_the_interest() {
        let i = shared();
        handle_control(
            WATCH,
            &json!("1"),
            &json!({ "types": ["agent-updated"] }),
            &i,
        )
        .unwrap();
        let reply = handle_control(
            WATCH,
            &json!("2"),
            &json!({ "perspectives": ["B", "A"] }),
            &i,
        )
        .unwrap();
        assert_eq!(
            serde_json::from_str::<Value>(&reply).unwrap(),
            json!({ "id": "2", "result": { "watching": { "types": null, "perspectives": ["A", "B"] } } })
        );
        assert!(wants(&i, AGENT), "the earlier types filter was replaced");
    }

    #[test]
    fn malformed_watch_is_rejected_and_keeps_the_old_interest() {
        let i = shared();
        let reply =
            handle_control(WATCH, &json!("1"), &json!({ "types": "link-added" }), &i).unwrap();
        let reply: Value = serde_json::from_str(&reply).unwrap();
        assert_eq!(reply["error"]["code"], 400);
        assert!(wants(&i, AGENT));
    }

    #[test]
    fn other_messages_are_not_handled() {
        assert!(handle_control("ping", &Value::Null, &json!({}), &shared()).is_none());
    }

    #[test]
    fn perspective_extraction_per_event_type() {
        let cases = [
            (r#"{"type":"signal","perspective":{"uuid":"P"}}"#, Some("P")),
            (
                r#"{"type":"sync-state-change","perspective":{"uuid":"P"},"state":"x"}"#,
                Some("P"),
            ),
            (
                r#"{"type":"perspective-removed","uuid":"P","owner":"o"}"#,
                Some("P"),
            ),
            (
                r#"{"type":"notification-triggered","notification":{"perspectiveId":"P"}}"#,
                Some("P"),
            ),
            (
                r#"{"type":"auto-processor-event","perspectiveUuid":"P"}"#,
                Some("P"),
            ),
            (
                r#"{"type":"exception-occurred","exception":{"uuid":"P"}}"#,
                None,
            ),
        ];
        for (event, want) in cases {
            let v: Value = serde_json::from_str(event).unwrap();
            let t = v["type"].as_str().unwrap();
            assert_eq!(event_perspective(t, &v), want, "{event}");
        }
    }
}

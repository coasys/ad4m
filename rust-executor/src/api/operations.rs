//! Operation handles for long calls (protocol feature `operations.async`).
//!
//! A call to one of [`ASYNC_METHODS`] with `params.async === true` is answered
//! at once with `{ operationId }`. The handler keeps running; when it ends the
//! same socket receives one event:
//!
//! `{ "type": "operation-completed", "operationId": "...", "result": ... }` or
//! `{ "type": "operation-completed", "operationId": "...", "error": { "code", "message" } }`
//!
//! `request.cancel { targetId: operationId }` cancels it (the event then
//! carries error 499). Without `async: true` these methods reply as before.
//! Only the RPC socket (`/api/v1/ws`) supports this: the event goes to the
//! calling socket only, never through the shared event bus. An operation
//! keeps running if its socket closes; its result is then lost.

use serde_json::{json, Value};

/// Methods that accept `{ async: true }`.
pub const ASYNC_METHODS: &[&str] = &[
    "neighbourhood.join",
    "neighbourhood.publish",
    "agent.unlock",
    "ai.prompt",
];

pub const COMPLETED_EVENT: &str = "operation-completed";

/// Does this call ask to run as an operation?
pub fn is_async_call(msg_type: &str, params: &Value) -> bool {
    ASYNC_METHODS.contains(&msg_type) && params.get("async").and_then(Value::as_bool) == Some(true)
}

/// Where a call's outcome goes: the reply to its request id (every call
/// today), or an `operation-completed` event for an async call.
#[derive(Debug, Clone, PartialEq)]
pub enum ReplyTarget {
    Request(String),
    Operation(String),
}

impl ReplyTarget {
    /// Key of the call's cancel token in the connection's in-flight registry.
    pub fn key(&self) -> &str {
        match self {
            Self::Request(id) | Self::Operation(id) => id,
        }
    }

    pub fn result(&self, val: Value) -> String {
        match self {
            Self::Request(id) => json!({"id": id, "result": val}),
            Self::Operation(op) => {
                json!({"type": COMPLETED_EVENT, "operationId": op, "result": val})
            }
        }
        .to_string()
    }

    pub fn error(&self, code: u16, message: impl Into<Value>) -> String {
        let message = message.into();
        match self {
            Self::Request(id) => json!({"id": id, "error": {"code": code, "message": message}}),
            Self::Operation(op) => json!({
                "type": COMPLETED_EVENT,
                "operationId": op,
                "error": {"code": code, "message": message},
            }),
        }
        .to_string()
    }
}

/// The immediate reply to an async call.
pub fn accepted(request_id: &str, operation_id: &str) -> String {
    json!({"id": request_id, "result": {"operationId": operation_id}}).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_listed_methods_with_async_true_run_as_operations() {
        assert!(is_async_call("ai.prompt", &json!({ "async": true })));
        assert!(!is_async_call("ai.prompt", &json!({})));
        assert!(!is_async_call("ai.prompt", &json!({ "async": "true" })));
        assert!(!is_async_call("ai.prompt", &json!({ "async": false })));
        assert!(!is_async_call("perspective.all", &json!({ "async": true })));
    }

    /// The request path must produce exactly the envelopes `ws_rpc` sent
    /// before operation handles existed.
    #[test]
    fn request_envelopes_are_unchanged() {
        let r = ReplyTarget::Request("7".into());
        assert_eq!(
            r.result(json!([1])),
            json!({"id": "7", "result": [1]}).to_string()
        );
        assert_eq!(
            r.error(499, "Request cancelled by client"),
            json!({"id": "7", "error": {"code": 499, "message": "Request cancelled by client"}})
                .to_string()
        );
        assert_eq!(r.result(json!([1])), r#"{"id":"7","result":[1]}"#);
    }

    #[test]
    fn operation_envelopes_are_events() {
        let op = ReplyTarget::Operation("op-1".into());
        let done: Value = serde_json::from_str(&op.result(json!(true))).unwrap();
        assert_eq!(
            done,
            json!({"type": "operation-completed", "operationId": "op-1", "result": true})
        );
        let failed: Value = serde_json::from_str(&op.error(500, "boom".to_string())).unwrap();
        assert_eq!(failed["error"], json!({"code": 500, "message": "boom"}));
        assert!(failed.get("id").is_none(), "an event, not a reply");
        assert_eq!(
            serde_json::from_str::<Value>(&accepted("9", "op-1")).unwrap(),
            json!({"id": "9", "result": {"operationId": "op-1"}})
        );
    }
}

//! Runtime handlers. No case here reaches the global DB, which other tests share.

use serde_json::json;
use std::sync::Arc;

use crate::api::runtime_ws::register_ws_handlers;
use crate::api::ws_handler::HandlerMap;
use crate::types::RequestContext;

// Nothing delivers a friend message and the outbox has no owner, so storing one would let
// every user read it through `runtime.outbox`.
#[tokio::test]
async fn send_friend_message_is_not_implemented_and_stores_nothing() {
    use crate::agent::capabilities::RUNTIME_MESSAGES_CREATE_CAPABILITY;
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let ctx = Arc::new(RequestContext {
        capabilities: Ok(vec![RUNTIME_MESSAGES_CREATE_CAPABILITY.clone()]),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential: false,
        user_email: None,
        user_did: None,
        cancel_token: None,
    });
    let message = json!({ "did": "did:key:friend", "message": { "links": [] } });
    let err = map
        .dispatch("runtime.sendFriendMessage", message, ctx)
        .await
        .unwrap_err();
    assert_eq!(err.code, 501, "{}", err.message);
}

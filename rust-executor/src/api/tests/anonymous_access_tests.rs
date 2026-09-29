//! A caller with no token, on a node with no admin credential (#1059).
//!
//! Such a caller used to be the operator on every listener, including one bound to the
//! network: anonymous `agent.getApps` returned every app token on the node. It is still the
//! operator on a loopback listener, the single-user local mode, and anonymous on any other.
//!
//! The socket tests serve the real router, marked by the same `listener_router` the executor
//! uses, and connect over a real socket. A listener's reach comes from the address it bound,
//! so a test reaches a `0.0.0.0` listener through `127.0.0.1` and it still counts as network.

use crate::api::auth::AppState;
use crate::api::{api_router, bind_api_listeners, listener_router};
use axum::body::Body;
use axum::http::{Request, StatusCode};
use futures::{SinkExt, StreamExt};
use serde_json::{json, Value};
use std::net::SocketAddr;
use std::time::Duration;
use tokio_tungstenite::tungstenite::Message;
use tower::ServiceExt;

fn no_credential() -> AppState {
    AppState {
        admin_credential: None,
        auto_permit_cap_requests: false,
    }
}

/// Serves the API with no admin credential on `bind` and returns the loopback address to
/// connect to.
async fn serve_without_credential(bind: &str) -> SocketAddr {
    // The socket's event stream reads the agent.
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
    let listener = tokio::net::TcpListener::bind(bind).await.unwrap();
    let bound = listener.local_addr().unwrap();
    let app = listener_router(no_credential(), &listener).unwrap();
    tokio::spawn(async move { axum::serve(listener, app).await.unwrap() });
    SocketAddr::from(([127, 0, 0, 1], bound.port()))
}

/// The API router with no admin credential, marked from a listener bound on `bind`.
async fn router_bound_on(bind: &str) -> axum::Router {
    let listener = tokio::net::TcpListener::bind(bind).await.unwrap();
    listener_router(no_credential(), &listener).unwrap()
}

/// Opens a socket with no token, sends one call and returns its reply.
async fn call_without_token(addr: SocketAddr, op: &str, params: Value) -> Value {
    let (mut ws, _) = tokio_tungstenite::connect_async(format!("ws://{addr}/api/v1/ws"))
        .await
        .expect("the socket opens");
    let call = json!({ "id": "call-1", "type": op, "params": params });
    ws.send(Message::Text(call.to_string().into()))
        .await
        .unwrap();
    loop {
        let msg = tokio::time::timeout(Duration::from_secs(10), ws.next())
            .await
            .expect("a reply within 10 s")
            .expect("the socket stays open")
            .unwrap();
        if let Message::Text(text) = msg {
            let reply: Value = serde_json::from_str(&text).unwrap();
            // Events share the socket; only the reply carries the call's id.
            if reply["id"] == "call-1" {
                return reply;
            }
        }
    }
}

#[tokio::test]
async fn an_anonymous_caller_on_a_network_listener_cannot_list_app_tokens() {
    let addr = serve_without_credential("0.0.0.0:0").await;
    let reply = call_without_token(addr, "agent.getApps", json!({})).await;
    assert_eq!(reply["error"]["code"], 403, "{reply}");
    // The capability check refused it, not something later in the handler.
    let message = reply["error"]["message"].as_str().unwrap_or_default();
    assert!(message.contains("Capability is not matched"), "{reply}");
}

// `is_admin_credential` gates user management and runtime calls by itself, without a
// capability check, so it has to follow the listener too.
#[tokio::test]
async fn an_anonymous_caller_on_a_network_listener_is_not_the_admin() {
    let addr = serve_without_credential("0.0.0.0:0").await;
    // `false` is the default, so the broken behaviour changes nothing when it lets this through.
    let reply = call_without_token(
        addr,
        "user.setMultiUserEnabled",
        json!({ "enabled": false }),
    )
    .await;
    assert_eq!(reply["error"]["code"], 403, "{reply}");
    assert_eq!(
        reply["error"]["message"], "Admin credential required",
        "{reply}"
    );
}

// The single-user local mode is unchanged. This is also the control for the two tests above:
// the same calls on the same router succeed when only the bind address differs.
#[tokio::test]
async fn an_anonymous_caller_on_a_loopback_listener_is_still_the_operator() {
    let addr = serve_without_credential("127.0.0.1:0").await;
    let apps = call_without_token(addr, "agent.getApps", json!({})).await;
    assert!(apps["result"].is_array(), "{apps}");
    let admin_only = call_without_token(
        addr,
        "user.setMultiUserEnabled",
        json!({ "enabled": false }),
    )
    .await;
    assert!(admin_only.get("error").is_none(), "{admin_only}");
}

/// Status of `GET /v1/models` with no token. It goes through the HTTP auth extractor
/// (`AuthContext`), not the socket, and checks AI_READ.
async fn list_models_status(app: axum::Router) -> StatusCode {
    let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
    let request = Request::builder()
        .uri("/v1/models")
        .body(Body::empty())
        .unwrap();
    app.oneshot(request).await.unwrap().status()
}

#[tokio::test]
async fn the_http_extractor_follows_the_listener_too() {
    assert_eq!(
        list_models_status(router_bound_on("0.0.0.0:0").await).await,
        StatusCode::FORBIDDEN
    );
    // On loopback the capability check passes; what the handler answers after it is not
    // this test's concern.
    assert_ne!(
        list_models_status(router_bound_on("127.0.0.1:0").await).await,
        StatusCode::FORBIDDEN
    );
}

// A router nobody marked has not shown that its callers are on this machine.
#[tokio::test]
async fn an_unmarked_router_treats_a_caller_without_a_token_as_anonymous() {
    assert_eq!(
        list_models_status(api_router(no_credential())).await,
        StatusCode::FORBIDDEN
    );
}

/// Whether a caller with no token is the operator on this router: `/v1/models` checks
/// AI_READ, which only the operator has without a token.
async fn is_operator_without_token(app: axum::Router) -> bool {
    list_models_status(app).await != StatusCode::FORBIDDEN
}

// The tests below go through `bind_api_listeners`, which `start_server` uses: each router
// must be marked from the socket it is served on, not from an address a call site supplies.

#[tokio::test]
async fn the_default_api_listener_is_loopback() {
    let listeners = bind_api_listeners(no_credential(), 0, true, None)
        .await
        .unwrap();
    let (listener, app) = listeners.http;
    assert!(listener.local_addr().unwrap().ip().is_loopback());
    assert!(is_operator_without_token(app).await);
    assert!(listeners.https.is_none());
}

#[tokio::test]
async fn with_localhost_false_the_api_listener_is_on_the_network() {
    let listeners = bind_api_listeners(no_credential(), 0, false, None)
        .await
        .unwrap();
    let (listener, app) = listeners.http;
    assert!(listener.local_addr().unwrap().ip().is_unspecified());
    assert!(!is_operator_without_token(app).await);
}

// With TLS, `localhost` does not matter: cleartext stays on loopback, TLS is on the network.
#[tokio::test]
async fn with_tls_the_https_listener_is_on_the_network() {
    for localhost in [true, false] {
        let listeners = bind_api_listeners(no_credential(), 0, localhost, Some(0))
            .await
            .unwrap();
        let (listener, app) = listeners.http;
        assert!(
            listener.local_addr().unwrap().ip().is_loopback(),
            "{localhost}"
        );
        assert!(is_operator_without_token(app).await, "{localhost}");

        let (tls_listener, tls_app) = listeners.https.expect("the TLS port binds");
        assert!(
            tls_listener.local_addr().unwrap().ip().is_unspecified(),
            "{localhost}"
        );
        assert!(!is_operator_without_token(tls_app).await, "{localhost}");
    }
}

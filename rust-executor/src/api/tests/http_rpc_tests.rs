//! `POST /api/v1/rpc`: only open methods, through the real router.

use crate::api::auth::AppState;
use crate::api::listener_router;
use axum::body::Body;
use axum::http::{Request, StatusCode};
use serde_json::{json, Value};
use tower::ServiceExt;

/// A loopback router with no admin credential: a caller with no token would be the operator
/// on a socket, which is what the HTTP route must not give it.
async fn router() -> axum::Router {
    let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    listener_router(
        AppState {
            admin_credential: None,
        },
        &listener,
    )
    .unwrap()
}

async fn post(raw: Vec<u8>) -> (StatusCode, Value) {
    let response = router()
        .await
        .oneshot(
            Request::post("/api/v1/rpc")
                .header("content-type", "application/json")
                .body(Body::from(raw))
                .unwrap(),
        )
        .await
        .unwrap();
    let status = response.status();
    let bytes = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    (
        status,
        serde_json::from_slice(&bytes).unwrap_or(Value::Null),
    )
}

async fn call(body: Value) -> (StatusCode, Value) {
    post(body.to_string().into_bytes()).await
}

#[tokio::test]
async fn host_info_answers_over_http_with_its_own_error_code() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
    let backend = crate::db_backend::db_backend();

    // Multi-user mode off: the method's own refusal, `not-a-host`, with its status.
    backend.set_multi_user_enabled(false).unwrap();
    let (status, body) = call(json!({ "method": "host.info", "params": {} })).await;
    assert_eq!(status, StatusCode::UNPROCESSABLE_ENTITY, "{body}");
    assert_eq!(body["error"]["code"], "not-a-host", "{body}");
    assert!(body.get("result").is_none());

    // A missing `params` is `{}`.
    let (status, body) = call(json!({ "method": "host.info" })).await;
    assert_eq!(status, StatusCode::UNPROCESSABLE_ENTITY, "{body}");
    assert_eq!(body["error"]["code"], "not-a-host", "{body}");
}

#[tokio::test]
async fn an_operator_method_is_refused_even_on_loopback_without_a_credential() {
    let (status, body) = call(json!({ "method": "agent.getApps", "params": {} })).await;
    assert_eq!(status, StatusCode::FORBIDDEN, "{body}");
    assert_eq!(body["error"]["code"], "forbidden", "{body}");

    let (status, body) = call(json!({ "method": "index.status", "params": {} })).await;
    assert_eq!(status, StatusCode::FORBIDDEN, "{body}");
    assert_eq!(body["error"]["code"], "forbidden", "{body}");
}

#[tokio::test]
async fn an_unknown_method_is_not_found() {
    let (status, body) = call(json!({ "method": "no.such.method", "params": {} })).await;
    assert_eq!(status, StatusCode::NOT_FOUND, "{body}");
    assert_eq!(body["error"]["code"], "not-found", "{body}");
}

#[tokio::test]
async fn a_malformed_body_is_400() {
    let (status, body) = post(b"{not json".to_vec()).await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert_eq!(body["error"]["code"], "bad-request", "{body}");

    // JSON, but not a call.
    let (status, body) = call(json!({ "params": {} })).await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    let (status, _) = call(json!([1, 2])).await;
    assert_eq!(status, StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn a_body_over_the_limit_is_refused() {
    let big = json!({ "method": "host.info", "params": { "pad": "x".repeat(crate::api::http_rpc::MAX_BODY_BYTES) } });
    let (status, body) = call(big).await;
    assert_eq!(status, StatusCode::PAYLOAD_TOO_LARGE, "{body}");
    assert_eq!(body["error"]["code"], "too-large", "{body}");
}

/// No method whose guard is not `Open` runs over the route (the open ones are not called: some
/// have effects).
#[tokio::test]
async fn every_method_that_is_not_open_is_refused() {
    use crate::api::guards::Guard;
    let map = crate::api::ws_handler::shared_handler_map();
    let mut open = 0;
    let mut refused = 0;
    for (name, guard) in map.guards() {
        if matches!(guard, Some(Guard::Open)) {
            open += 1;
            continue;
        }
        let (status, body) = call(json!({ "method": name, "params": {} })).await;
        assert_eq!(status, StatusCode::FORBIDDEN, "{name}: {body}");
        assert_eq!(body["error"]["code"], "forbidden", "{name}: {body}");
        refused += 1;
    }
    assert!(open > 0 && refused > open);
}

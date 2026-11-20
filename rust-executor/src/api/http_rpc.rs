//! `POST /api/v1/rpc`: the executor's open methods over plain HTTP.
//!
//! The executor's RPC is a WebSocket, which a server-side fetch (the hosting index's health
//! check reading `host.info`) cannot use. This route takes `{"method": "...", "params": {...}}`
//! and runs the method through the same handler map the socket uses, so no method has code
//! of its own here. Only a method whose guard is [`Guard::Open`] runs: those check nothing of
//! the caller, so what they answer is what an anonymous socket gets. Every other method
//! answers 403 `forbidden`, an unknown one 404 `not-found`.
//!
//! No token and no session are read: the caller is anonymous, on any listener, including a
//! loopback one (the "no token on loopback is the operator" rule does not apply here). The
//! global CORS layer covers the route (any origin, no credentials are read, so a page that
//! calls it learns only what any anonymous caller can already fetch or read over the socket).
//!
//! Success is `200 {"result": ...}`. A failure is `{"error": {"code": "...", "message": "..."}}`
//! with the method's own code when it names one (`error.data.code`, e.g. `not-a-host`), else the
//! code of its status (`bad-request`, `forbidden`, `not-found`, ...), and the same HTTP status as
//! the socket's error code.

use std::net::SocketAddr;
use std::sync::Arc;

use axum::{
    body::Bytes,
    extract::{rejection::BytesRejection, ConnectInfo, Extension},
    http::StatusCode,
    response::{IntoResponse, Json, Response},
};
use serde_json::{json, Value};

use crate::agent::capabilities::ListenerReach;
use crate::types::RequestContext;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, WsRpcError};

/// The largest request body: a method name and its params, never data.
pub const MAX_BODY_BYTES: usize = 64 * 1024;

/// The error code a client reads: the method's own (`data.code`), else its status's name.
fn error_code(e: &WsRpcError) -> String {
    if let Some(code) = e
        .data
        .as_ref()
        .and_then(|d| d.get("code"))
        .and_then(Value::as_str)
    {
        return code.to_string();
    }
    match e.code {
        400 => "bad-request",
        401 => "unauthorized",
        403 => "forbidden",
        404 => "not-found",
        409 => "conflict",
        413 => "too-large",
        422 => "unprocessable",
        429 => "too-many-requests",
        501 => "not-implemented",
        503 => "unavailable",
        _ => "internal",
    }
    .to_string()
}

fn failure(e: &WsRpcError) -> Response {
    let status = StatusCode::from_u16(e.code).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR);
    let mut error = json!({ "code": error_code(e), "message": e.message });
    if let Some(data) = &e.data {
        error["data"] = data.clone();
    }
    (status, Json(json!({ "error": error }))).into_response()
}

/// Runs the open method the body names; see the module comment.
pub async fn http_rpc(
    Extension(handler_map): Extension<Arc<HandlerMap>>,
    reach: ListenerReach,
    connect_info: Option<Extension<ConnectInfo<SocketAddr>>>,
    body: Result<Bytes, BytesRejection>,
) -> Response {
    let body = match body {
        Ok(body) => body,
        Err(rejection) => {
            let status = rejection.status();
            return failure(&WsRpcError {
                code: status.as_u16(),
                message: format!(
                    "the request body could not be read: {}",
                    rejection.body_text()
                ),
                data: None,
            });
        }
    };
    let request: Value = match serde_json::from_slice(&body) {
        Ok(request) => request,
        Err(e) => {
            return failure(&WsRpcError::bad_request(format!(
                "the body is not JSON: {e}"
            )))
        }
    };
    let Some(method) = request.get("method").and_then(Value::as_str) else {
        return failure(&WsRpcError::bad_request(
            "the body needs a string \"method\": {\"method\": \"host.info\", \"params\": {}}",
        ));
    };
    let params = match request.get("params") {
        None | Some(Value::Null) => json!({}),
        Some(params) => params.clone(),
    };
    match handler_map.guard_of(method) {
        None => {
            return failure(&WsRpcError::not_found(format!("Unknown type: {method}")));
        }
        Some(Guard::Open) => {}
        Some(_) => {
            return failure(&WsRpcError::forbidden(format!(
                "{method} is not open to callers without a token; use the WebSocket"
            )));
        }
    }
    let ctx = Arc::new(RequestContext::anonymous(
        reach,
        connect_info.map(|info| info.0 .0.ip()),
    ));
    match handler_map.dispatch(method, params, ctx).await {
        Ok(result) => Json(json!({ "result": result })).into_response(),
        Err(e) => failure(&e),
    }
}

//! API module — WebSocket RPC + minimal HTTP endpoints.
//!
//! All SDK operations go through the WebSocket at `/api/v1/ws`.
//! The only HTTP routes are health, info, and binary audio feed.

pub mod auth;
pub mod errors;
pub mod events_ws;
pub mod internal;
pub mod openai_compat;
pub mod types;
pub mod ws_rpc;

#[cfg(test)]
mod tests;

// ── WS-native handler modules ──
pub mod agent_ws;
pub mod ai_ws;
pub mod expressions_ws;
pub mod hosting_ws;
pub mod languages_ws;
pub mod neighbourhoods_ws;
pub mod perspectives_ws;
pub mod runtime_ws;
pub mod users_ws;
pub mod ws_handler;

use crate::agent::capabilities::ListenerReach;
use crate::Ad4mConfig;
use auth::AppState;
use axum::{
    extract::DefaultBodyLimit,
    http::Method,
    response::Json,
    routing::{get, post},
    Extension, Router,
};
use deno_core::error::AnyError;
use serde_json::json;
use std::net::SocketAddr;
use std::sync::Arc;
use tower_http::catch_panic::CatchPanicLayer;
use tower_http::cors::{AllowOrigin, CorsLayer};

/// Build the API router.
pub fn api_router(state: AppState) -> Router {
    let cors = CorsLayer::new()
        .allow_origin(AllowOrigin::any())
        .allow_methods([
            Method::GET,
            Method::POST,
            Method::PUT,
            Method::PATCH,
            Method::DELETE,
            Method::OPTIONS,
        ])
        .allow_headers(tower_http::cors::Any)
        .expose_headers([
            "Cross-Origin-Embedder-Policy".parse().unwrap(),
            "Cross-Origin-Resource-Policy".parse().unwrap(),
            "Cross-Origin-Opener-Policy".parse().unwrap(),
            // Chrome Private Network Access — required for cross-port localhost requests
            "Access-Control-Allow-Private-Network".parse().unwrap(),
        ])
        .allow_private_network(true);

    // Chrome Private Network Access (PNA) requires this header for
    // preflight responses when a page fetches from a different local port.
    // Without it, Chrome blocks requests from localhost:3030 → localhost:12000.
    // We use axum middleware (runs after CORS layer) to ensure it appears on all responses.

    // Root info endpoint
    let root = Router::new()
        .route(
            "/",
            get(|| async {
                Json(json!({
                    "name": "AD4M Executor",
                    "version": *crate::globals::AD4M_VERSION,
                    "api": "/api/v1",
                    "transport": "websocket",
                    "ws": "/api/v1/ws"
                }))
            }),
        )
        .route("/health", get(|| async { Json(json!({"status": "ok"})) }))
        // Internal platform → executor endpoint (INTERNAL_API_TOKEN auth)
        .route("/internal/shutdown", post(internal::internal_shutdown));

    // Build the WS handler map once at startup
    let handler_map = Arc::new(ws_handler::build_handler_map());

    root.nest(
        "/api/v1",
        Router::new()
            // ── WebSocket RPC (primary client transport) ──
            .route("/ws", get(ws_rpc::ws_rpc))
            // ── WebSocket Events (event stream) ──
            .route("/ws/events", get(events_ws::events_ws))
            // ── HTTP-only: binary transcription feed (can't go through WS JSON) ──
            .route(
                "/ai/transcription/feed",
                post(ai_ws::feed_transcription_stream),
            ),
    )
    // ── OpenAI-compatible /v1 surface ──
    //
    // Mounted at both `/v1` (the canonical OpenAI path) and
    // `/api/v1/openai/v1` (for proxies that hard-code the `/api/v1`
    // prefix from the native AD4M surface).  Both share the same
    // handlers + AppState.
    .nest("/v1", openai_compat::router())
    .nest("/api/v1/openai/v1", openai_compat::router())
    // ── State + Middleware ──
    .with_state(state)
    .layer(Extension(handler_map))
    .layer(DefaultBodyLimit::max(10 * 1024 * 1024)) // 10MB default
    .layer(CatchPanicLayer::new())
    .layer(cors)
}

/// The API router for one listener, marked with who can connect to it.
///
/// Every listener the executor binds goes through here, so the auth extractors know whether
/// a caller without a token is on this machine. Without an admin credential that caller is
/// the operator on a loopback listener, and anonymous on any other (#1059). The reach comes
/// from the bound socket itself, so no call site can mark a listener with an address it did
/// not bind.
pub fn listener_router(
    state: AppState,
    listener: &tokio::net::TcpListener,
) -> std::io::Result<Router> {
    let addr = listener.local_addr()?;
    let reach = ListenerReach::of(&addr);
    if reach == ListenerReach::Network && state.admin_credential.is_none() {
        log::warn!(
            "API on {addr} is reachable from the network and no --admin-credential is set. \
             Callers there without a token can only request a capability, check whether \
             multi-user mode is on and sign up and log in when it is; they are not the operator. Set an admin credential to \
             administer this node remotely."
        );
    }
    Ok(api_router(state).layer(Extension(reach)))
}

/// The API's listeners, bound, each with the router marked from its own socket.
pub(crate) struct ApiListeners {
    /// Cleartext HTTP and WebSocket.
    pub http: (tokio::net::TcpListener, Router),
    /// HTTPS and WSS, when TLS is configured and its port could be bound.
    pub https: Option<(std::net::TcpListener, Router)>,
}

/// Binds the API's listeners. With TLS the cleartext listener is on 127.0.0.1 and the TLS
/// one on 0.0.0.0; without, the cleartext listener is on 127.0.0.1 unless `localhost` is
/// false.
pub(crate) async fn bind_api_listeners(
    state: AppState,
    port: u16,
    localhost: bool,
    tls_port: Option<u16>,
) -> std::io::Result<ApiListeners> {
    let address: [u8; 4] = if localhost || tls_port.is_some() {
        [127, 0, 0, 1]
    } else {
        [0, 0, 0, 0]
    };
    let listener = tokio::net::TcpListener::bind(SocketAddr::from((address, port))).await?;
    let app = listener_router(state.clone(), &listener)?;

    let https = match tls_port {
        None => None,
        Some(tls_port) => {
            match tokio::net::TcpListener::bind(SocketAddr::from(([0, 0, 0, 0], tls_port))).await {
                Ok(tls_listener) => {
                    let tls_app = listener_router(state, &tls_listener)?;
                    // axum-server serves a std socket; `into_std` leaves it non-blocking.
                    Some((tls_listener.into_std()?, tls_app))
                }
                // As before, a TLS listener that cannot start does not stop the cleartext one.
                Err(e) => {
                    log::error!(
                        "API HTTPS listener on port {tls_port} could not start: {e}. The remote \
                         API is unavailable: the cleartext listener is on 127.0.0.1:{port} \
                         because TLS is configured. Free port {tls_port} and restart the \
                         executor to restore remote access."
                    );
                    None
                }
            }
        }
    };

    Ok(ApiListeners {
        http: (listener, app),
        https,
    })
}

/// Start the API server (HTTP + WebSocket).
pub async fn start_server(config: Ad4mConfig) -> Result<(), AnyError> {
    // Set global SMTP config for email verification
    crate::config::set_smtp_config(config.smtp_config.clone())?;

    let port = config
        .port
        .ok_or_else(|| deno_core::anyhow::anyhow!("API port not configured"))?;
    let admin_credential = config.admin_credential.clone();
    let auto_permit = config.auto_permit_cap_requests.unwrap_or(false);

    let state = AppState {
        admin_credential,
        auto_permit_cap_requests: auto_permit,
    };

    let rustls_config = match &config.tls {
        Some(tls_config) => Some(
            axum_server::tls_rustls::RustlsConfig::from_pem_file(
                &tls_config.cert_file_path,
                &tls_config.key_file_path,
            )
            .await
            .map_err(|e| deno_core::anyhow::anyhow!("TLS config error: {}", e))?,
        ),
        None => None,
    };

    let ApiListeners {
        http: (listener, app),
        https,
    } = bind_api_listeners(
        state,
        port,
        config.localhost.unwrap_or(true),
        config.tls.as_ref().map(|tls| tls.tls_port),
    )
    .await?;

    // The listeners are bound before these lines: the integration tests take the "starting"
    // line as the server being ready.
    if let (Some((tls_listener, tls_app)), Some(rustls_config)) = (https, rustls_config) {
        let tls_port = tls_listener.local_addr()?.port();
        log::info!("Starting API server (HTTP) on {}", listener.local_addr()?);
        log::info!(
            "Starting API server (HTTPS) on {}",
            tls_listener.local_addr()?
        );

        tokio::spawn(async move {
            axum_server::from_tcp_rustls(tls_listener, rustls_config)
                .serve(tls_app.into_make_service())
                .await
                // Same reasoning as the MCP HTTPS listener: the cleartext API
                // below is bound to 127.0.0.1 because TLS is *configured*, not
                // because this task bound. If it fails, the API has no remote
                // surface, and "TLS server error" is not a sentence an operator
                // maps to that outage.
                .unwrap_or_else(|e| {
                    log::error!(
                        "API HTTPS listener on port {tls_port} stopped: {e}. The remote API is \
                         now unavailable: the cleartext listener is on 127.0.0.1:{port} because \
                         TLS is configured. Free port {tls_port} and restart the executor to \
                         restore remote access."
                    )
                });
        });
    } else {
        log::info!(
            "API server starting on http://{}/api/v1",
            listener.local_addr()?
        );
    }

    axum::serve(listener, app.into_make_service()).await?;

    Ok(())
}

//! API module — WebSocket RPC + minimal HTTP endpoints.
//!
//! All SDK operations go through the WebSocket at `/api/v1/ws`.
//! The only HTTP routes are health, info, and binary audio feed.

pub mod auth;
pub mod errors;
pub mod events_ws;
pub mod types;
pub mod ws_rpc;

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

#[cfg(test)]
mod tests;

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
pub fn rest_router(state: AppState) -> Router {
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
        .route("/health", get(|| async { Json(json!({"status": "ok"})) }));

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
    // ── State + Middleware ──
    .with_state(state)
    .layer(Extension(handler_map))
    .layer(DefaultBodyLimit::max(10 * 1024 * 1024)) // 10MB default
    .layer(CatchPanicLayer::new())
    .layer(cors)
}

/// Start the REST API server.
pub async fn start_server(config: Ad4mConfig) -> Result<(), AnyError> {
    // Set global SMTP config for email verification
    crate::config::set_smtp_config(config.smtp_config.clone())?;

    let port = config.gql_port.expect("Did not get port");
    let admin_credential = config.admin_credential.clone();
    let auto_permit = config.auto_permit_cap_requests.unwrap_or(false);

    let state = AppState {
        admin_credential: admin_credential.clone(),
        auto_permit_cap_requests: auto_permit,
    };

    let app = rest_router(state);

    if let Some(tls_config) = &config.tls {
        let tls_port = tls_config.tls_port;
        let cert_path = tls_config.cert_file_path.clone();
        let key_path = tls_config.key_file_path.clone();

        log::info!("Starting REST API (HTTP) on 127.0.0.1:{}", port);
        log::info!("Starting REST API (HTTPS) on 0.0.0.0:{}", tls_port);

        let tls_state = AppState {
            admin_credential: admin_credential.clone(),
            auto_permit_cap_requests: auto_permit,
        };
        let tls_app = rest_router(tls_state);

        let rustls_config =
            axum_server::tls_rustls::RustlsConfig::from_pem_file(&cert_path, &key_path)
                .await
                .map_err(|e| deno_core::anyhow::anyhow!("TLS config error: {}", e))?;

        tokio::spawn(async move {
            axum_server::bind_rustls(SocketAddr::from(([0, 0, 0, 0], tls_port)), rustls_config)
                .serve(tls_app.into_make_service())
                .await
                .unwrap_or_else(|e| log::error!("TLS server error: {}", e));
        });

        let listener =
            tokio::net::TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], port))).await?;
        axum::serve(listener, app.into_make_service()).await?;
    } else {
        let address: [u8; 4] = if config.localhost.unwrap_or(true) {
            [127, 0, 0, 1]
        } else {
            [0, 0, 0, 0]
        };

        let addr = SocketAddr::from((address, port));
        log::info!("REST API server starting on http://{}/api/v1", addr);

        let listener = tokio::net::TcpListener::bind(addr).await?;
        axum::serve(listener, app.into_make_service()).await?;
    }

    Ok(())
}

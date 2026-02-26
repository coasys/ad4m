//! MCP Server setup and transport handling
//!
//! The MCP server runs as an HTTP service alongside AD4M's GraphQL server.
//! AI agents connect via HTTP to interact with AD4M perspectives, models,
//! and neighbourhoods using the Model Context Protocol.
//!
//! Authentication: clients can pass a JWT token via the `Authorization: Bearer <token>`
//! HTTP header. This token is extracted by middleware and injected into the MCP session
//! context, so the agent doesn't need to call auth tools to authenticate.

use super::tools::Ad4mMcpHandler;
use crate::js_core::JsCoreHandle;
use anyhow::Result;
use axum::{extract::Request, http, middleware, response::Response};
use log::info;
use rmcp::transport::streamable_http_server::{
    session::local::LocalSessionManager, StreamableHttpServerConfig, StreamableHttpService,
};
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::RwLock;

/// MCP Server context, similar to GraphQL's RequestContext
#[derive(Clone)]
pub struct McpContext {
    pub js_handle: JsCoreHandle,
    pub admin_credential: Option<String>,
    pub auth_token: Arc<RwLock<Option<String>>>,
}

/// Configuration for the MCP HTTP server
#[derive(Clone, Debug)]
pub struct McpServerConfig {
    /// Port to listen on (default: 3001)
    pub port: u16,
    /// Host to bind to (default: 127.0.0.1)
    pub host: String,
}

impl Default for McpServerConfig {
    fn default() -> Self {
        Self {
            port: 3001,
            host: "127.0.0.1".to_string(),
        }
    }
}

/// Start the MCP server with HTTP transport
///
/// This runs an HTTP server that accepts MCP protocol requests.
/// AI agents can connect via HTTP to interact with AD4M.
pub async fn start_mcp_server(
    js_core_handle: JsCoreHandle,
    admin_credential: Option<String>,
    auth_token: Option<String>,
    config: McpServerConfig,
) -> Result<()> {
    let addr: SocketAddr = format!("{}:{}", config.host, config.port).parse()?;
    info!("Starting AD4M MCP server on http://{}", addr);

    let context = McpContext {
        js_handle: js_core_handle,
        admin_credential,
        auth_token: Arc::new(RwLock::new(auth_token)),
    };

    // Create the session manager for HTTP transport
    let session_manager = Arc::new(LocalSessionManager::default());

    // Create config for the HTTP server
    let http_config = StreamableHttpServerConfig::default();

    // Create the HTTP service with a factory that creates handlers
    let context_clone = context.clone();
    let service = StreamableHttpService::new(
        move || Ok(Ad4mMcpHandler::new(context_clone.clone())),
        session_manager,
        http_config,
    );

    // Create the TCP listener and serve using axum
    let listener = tokio::net::TcpListener::bind(addr).await?;
    info!("MCP HTTP server listening on {}", addr);

    // Middleware to extract Authorization: Bearer <token> from HTTP headers
    // and inject it into the shared MCP session context
    let auth_token_ref = context.auth_token.clone();
    let auth_layer = middleware::from_fn(move |req: Request, next: middleware::Next| {
        let token_ref = auth_token_ref.clone();
        async move {
            if let Some(auth_header) = req.headers().get(http::header::AUTHORIZATION) {
                if let Ok(auth_str) = auth_header.to_str() {
                    if let Some(token) = auth_str.strip_prefix("Bearer ") {
                        let mut guard = token_ref.write().await;
                        *guard = Some(token.to_string());
                        info!("MCP auth: Bearer token extracted from HTTP header");
                    }
                }
            }
            let response: Response = next.run(req).await;
            Ok::<_, std::convert::Infallible>(response)
        }
    });

    let app = axum::Router::new()
        .fallback_service(service)
        .layer(auth_layer);

    axum::serve(listener, app).await?;

    Ok(())
}

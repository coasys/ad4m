//! MCP Server setup and transport handling
//!
//! The MCP server runs as an HTTP service alongside AD4M's REST server.
//! AI agents connect via HTTP to interact with AD4M perspectives, models,
//! and neighbourhoods using the Model Context Protocol.
//!
//! Authentication: clients authenticate via MCP tools (`request_capability` + `login_email`).
//! Bearer header auth is not yet supported for multi-session setups — each session gets
//! its own isolated token state, so a shared HTTP middleware cannot route tokens correctly.

use super::tools::Ad4mMcpHandler;
use anyhow::Result;
use log::info;
use rmcp::transport::streamable_http_server::{
    session::local::LocalSessionManager, StreamableHttpServerConfig, StreamableHttpService,
};
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::RwLock;

/// MCP Server context, similar to REST's RequestContext
#[derive(Clone)]
pub struct McpContext {
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
    admin_credential: Option<String>,
    auth_token: Option<String>,
    config: McpServerConfig,
) -> Result<()> {
    let addr: SocketAddr = format!("{}:{}", config.host, config.port).parse()?;
    info!("Starting AD4M MCP server on http://{}", addr);

    let initial_token = auth_token;

    let context = McpContext {
        admin_credential,
        auth_token: Arc::new(RwLock::new(initial_token.clone())),
    };

    // Create the session manager for HTTP transport
    let session_manager = Arc::new(LocalSessionManager::default());

    // Create config for the HTTP server
    let http_config = StreamableHttpServerConfig::default();

    // Create the HTTP service with a factory that creates handlers
    // Each session gets its own auth_token Arc to prevent cross-session token leaking.
    let context_clone = context.clone();
    let initial_token_clone = initial_token.clone();
    let service = StreamableHttpService::new(
        move || {
            let mut ctx = context_clone.clone();
            // Per-session auth token — prevents cross-session token leaking
            ctx.auth_token = Arc::new(RwLock::new(initial_token_clone.clone()));
            Ok(Ad4mMcpHandler::new(ctx))
        },
        session_manager,
        http_config,
    );

    // Create the TCP listener and serve using axum
    let listener = tokio::net::TcpListener::bind(addr).await?;
    info!("MCP HTTP server listening on {}", addr);

    // NOTE: Bearer header auth removed — the per-session token isolation means an HTTP
    // middleware can't route tokens to the correct session without a session ID registry.
    // Clients authenticate via MCP tools (request_capability + login_email) instead.
    let app = axum::Router::new().fallback_service(service);

    axum::serve(listener, app).await?;

    Ok(())
}

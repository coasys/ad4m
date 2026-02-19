//! MCP Server setup and transport handling

use super::tools::Ad4mMcpHandler;
use crate::js_core::JsCoreHandle;
use anyhow::Result;
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

    // The StreamableHttpService implements tower::Service, so we can use it directly with axum
    let app = axum::Router::new().fallback_service(service);

    axum::serve(listener, app).await?;

    Ok(())
}

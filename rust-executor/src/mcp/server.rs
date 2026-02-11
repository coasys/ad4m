//! MCP Server setup and transport handling

use super::tools::Ad4mMcpHandler;
use crate::js_core::JsCoreHandle;
use anyhow::Result;
use log::{info, warn};
use rmcp::{ServiceExt, transport::stdio};
use std::sync::Arc;
use tokio::sync::RwLock;

/// MCP Server context, similar to GraphQL's RequestContext
#[derive(Clone)]
pub struct McpContext {
    pub js_handle: JsCoreHandle,
    pub admin_credential: Option<String>,
    pub auth_token: Arc<RwLock<Option<String>>>,
}

/// Start the MCP server on stdio
/// 
/// This is designed to be run alongside the GraphQL server, sharing the same
/// JsCoreHandle for executing AD4M operations.
pub async fn start_mcp_server(
    js_core_handle: JsCoreHandle,
    admin_credential: Option<String>,
    auth_token: Option<String>,
) -> Result<()> {
    info!("Starting AD4M MCP server on stdio");

    let context = McpContext {
        js_handle: js_core_handle,
        admin_credential,
        auth_token: Arc::new(RwLock::new(auth_token)),
    };

    let handler = Ad4mMcpHandler::new(context);
    let transport = stdio();
    let running = handler.serve(transport).await?;
    
    info!("MCP server running");
    running.waiting().await?;
    
    Ok(())
}

/// Start MCP server on HTTP transport (for remote access)
#[allow(dead_code)]
pub async fn start_mcp_http_server(
    _js_core_handle: JsCoreHandle,
    _admin_credential: Option<String>,
    port: u16,
) -> Result<()> {
    info!("Starting AD4M MCP HTTP server on port {}", port);
    
    // TODO: Implement HTTP transport with auth header extraction
    warn!("MCP HTTP transport not yet implemented");
    
    Ok(())
}

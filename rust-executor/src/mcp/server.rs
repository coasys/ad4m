//! MCP Server setup and transport handling

use super::tools::Ad4mMcpHandler;
use crate::js_core::JsCoreHandle;
use anyhow::Result;
use log::info;
use rmcp::{transport::stdio, ServiceExt};
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

// TODO: HTTP transport implementation
// The rmcp crate supports StreamableHttpService for HTTP transport,
// but it requires hyper 1.x which conflicts with the project's hyper 0.14 (via reqwest).
// Options to add HTTP support:
// 1. Update reqwest to use hyper 1.x when available
// 2. Use a warp-based wrapper that translates HTTP -> MCP protocol
// 3. Run MCP HTTP on a separate process that communicates via IPC
//
// For now, Claude Desktop and other MCP clients can use the stdio transport
// by running the executor as a subprocess.

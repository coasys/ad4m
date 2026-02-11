//! AD4M MCP Server
//! 
//! Exposes AD4M functionality via the Model Context Protocol (MCP).
//! Supports stdio transport for Claude Desktop integration.

mod server;

use clap::Parser;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Parser, Debug)]
#[command(name = "ad4m-mcp-server")]
#[command(about = "MCP server for AD4M - enables AI agents to interact with AD4M")]
struct Args {
    /// AD4M executor URL
    #[arg(short, long, default_value = "http://localhost:4000/graphql")]
    executor_url: String,

    /// Capability token for AD4M authentication
    #[arg(short, long)]
    token: Option<String>,

    /// Admin credential for capability token generation (optional)
    #[arg(long)]
    admin_credential: Option<String>,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize logging to stderr (stdout is for MCP protocol)
    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::new(
            std::env::var("RUST_LOG").unwrap_or_else(|_| "info".into()),
        ))
        .with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
        .init();

    let args = Args::parse();

    tracing::info!("Starting AD4M MCP Server");
    tracing::info!("Executor URL: {}", args.executor_url);

    // Create and run the MCP server
    let server = server::Ad4mMcpServer::new(
        args.executor_url,
        args.token,
        args.admin_credential,
    ).await?;

    server.serve_stdio().await?;

    Ok(())
}

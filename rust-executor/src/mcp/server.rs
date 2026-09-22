//! MCP Server setup and transport handling
//!
//! The MCP server runs as an HTTP service alongside AD4M's REST server.
//! AI agents connect via HTTP to interact with AD4M perspectives, models,
//! and neighbourhoods using the Model Context Protocol.
//!
//! Authentication: clients authenticate via MCP tools (`request_capability` + `login_email`).
//! Bearer header auth is not yet supported for multi-session setups — each session gets
//! its own isolated token state, so a shared HTTP middleware cannot route tokens correctly.
//!
//! What the server binds follows from that: a node with no admin credential
//! authenticates every caller, so it is reachable from the node itself and
//! nowhere else. See [`resolve_host`].

use super::tools::Ad4mMcpHandler;
use anyhow::Result;
use log::{info, warn};
use rmcp::transport::streamable_http_server::{
    session::local::LocalSessionManager, StreamableHttpServerConfig, StreamableHttpService,
};
use std::net::{IpAddr, SocketAddr};
use std::sync::Arc;
use tokio::sync::RwLock;

/// MCP Server context, similar to REST's RequestContext
#[derive(Clone)]
pub struct McpContext {
    pub admin_credential: Option<String>,
    pub auth_token: Arc<RwLock<Option<String>>>,
    /// Whether the dynamic per-class SHACL tools are exposed over this MCP
    /// transport (listed by `tools/list`, callable by `tools/call`). Off by
    /// default so external clients see a stable, DNA-independent tool set;
    /// the generic `instance_*` tools cover the same operations. Only the
    /// MCP transport reads this — the in-process harness bridge
    /// (`list_tool_schemas` / `call_tool_by_name`) always includes the
    /// dynamic tools.
    pub dynamic_class_tools: bool,
}

/// Configuration for the MCP HTTP server
#[derive(Clone, Debug)]
pub struct McpServerConfig {
    /// Port to listen on (default: 3001)
    pub port: u16,
    /// The address an operator asked for, or `None` to let
    /// [`resolve_host`] choose from whether access is gated.
    ///
    /// An IP literal, not a hostname: the address is parsed as a
    /// [`SocketAddr`], which does not resolve names, so `localhost` fails to
    /// start where `127.0.0.1` binds.
    pub host: Option<String>,
    /// Expose dynamic per-class SHACL tools over MCP (see
    /// [`McpContext::dynamic_class_tools`]). Default `false`.
    pub dynamic_class_tools: bool,
}

impl Default for McpServerConfig {
    fn default() -> Self {
        Self {
            port: 3001,
            host: None,
            dynamic_class_tools: false,
        }
    }
}

/// Where to bind, given whether anything gates access.
///
/// Without an admin credential every caller is authenticated —
/// `check_auth`'s last step is the single-user local trust model the REST
/// server uses, and it grants `request_capability`, which mints
/// `ALL_CAPABILITY`. That premise is true on loopback and false anywhere
/// else, so an ungated server binds loopback and nothing else. Configure a
/// credential and the default widens back to `0.0.0.0`, where a sibling
/// container on the same Docker network can reach it.
///
/// `has_credential` therefore has to mean exactly what that last step tests:
/// `admin_cred.is_none() && session_token.is_empty() && !header_present`
/// (`mcp::tools::check_auth`, step 4). The two are the same question asked
/// from opposite ends — *is anything gating this port?* — and only the
/// credential half of it is knowable at bind time, before any request has a
/// token or a header to present. If they ever drift, the failure is silent in
/// one of two directions: a wide bind on a node where every caller is
/// authenticated, or a loopback-only bind on a node that does gate access and
/// wanted to be reachable. Change one, change the other.
///
/// A multi-user node is **not** gated by having user accounts. A login JWT
/// authenticates a user to the API; it is not an admin credential, and step 4
/// does not consult the user table. `--enable-multi-user` without
/// `--admin-credential` is an ungated node and binds loopback here.
///
/// `MCP_HOST` is the operator saying it outright and is honoured either way.
/// Saying it on an ungated node hands every tool to whoever can route to the
/// address, so that combination warns rather than passing quietly.
fn resolve_host(requested: Option<&str>, has_credential: bool) -> String {
    match requested {
        Some(host) => {
            if !has_credential && !is_loopback(host) {
                warn!(
                    "MCP: MCP_HOST={host} binds beyond loopback with no --admin-credential set. \
                     Every caller that can reach this address is authenticated and may mint \
                     ALL_CAPABILITY. Set an admin credential, or bind 127.0.0.1."
                );
            }
            host.to_string()
        }
        None if has_credential => "0.0.0.0".to_string(),
        None => "127.0.0.1".to_string(),
    }
}

/// Whether this address reaches only the node itself.
///
/// Unparseable reads as *not* loopback: the warning is the fail-safe
/// direction, and an address that does not parse fails to bind a line later
/// anyway.
fn is_loopback(host: &str) -> bool {
    host.parse::<IpAddr>()
        .map(|ip| ip.is_loopback())
        .unwrap_or(false)
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
    let requested = config
        .host
        .clone()
        .or_else(|| std::env::var("MCP_HOST").ok());
    let host = resolve_host(requested.as_deref(), admin_credential.is_some());
    let addr: SocketAddr = format!("{}:{}", host, config.port).parse()?;
    info!("Starting AD4M MCP server on http://{}", addr);

    let initial_token = auth_token;

    if config.dynamic_class_tools {
        info!("MCP: dynamic per-class SHACL tools are exposed (dynamicClassTools=true)");
    } else {
        info!("MCP: static tool surface only (dynamicClassTools=false); per-class tools hidden");
    }

    let context = McpContext {
        admin_credential,
        auth_token: Arc::new(RwLock::new(initial_token.clone())),
        dynamic_class_tools: config.dynamic_class_tools,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_ungated_server_binds_loopback() {
        // check_auth's last step authenticates any caller when no admin
        // credential is configured. Loopback is the only place that is true.
        assert_eq!(resolve_host(None, false), "127.0.0.1");
    }

    #[test]
    fn a_credential_widens_the_default_to_every_interface() {
        // A sibling container on the same Docker network reaches it here, and
        // the credential is what stands between it and the tools.
        assert_eq!(resolve_host(None, true), "0.0.0.0");
    }

    #[test]
    fn an_explicit_host_is_honoured_either_way() {
        // MCP_HOST is the operator's own decision, including the ungated case:
        // refusing it would break a deployment that fronts MCP with its own
        // gateway. It warns there; the binding still happens.
        assert_eq!(resolve_host(Some("10.0.0.5"), false), "10.0.0.5");
        assert_eq!(resolve_host(Some("10.0.0.5"), true), "10.0.0.5");
        assert_eq!(resolve_host(Some("127.0.0.1"), true), "127.0.0.1");
    }

    #[test]
    fn loopback_is_every_spelling_of_it() {
        // 127.0.0.0/8 is loopback in full, not just .1, and v6 has its own.
        assert!(is_loopback("127.0.0.1"));
        assert!(is_loopback("127.1.2.3"));
        assert!(is_loopback("::1"));
        assert!(!is_loopback("0.0.0.0"));
        assert!(!is_loopback("10.0.0.5"));
        // Not an address at all. Reads as non-loopback so the warning fires;
        // the bind that follows fails on it regardless.
        assert!(!is_loopback("localhost"));
    }
}

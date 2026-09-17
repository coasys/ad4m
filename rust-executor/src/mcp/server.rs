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
//!
//! When the executor is configured for TLS, MCP is also served over HTTPS on
//! `mcp_port + 1`, using the same certificate as the RPC port — see
//! [`start_tls_listener`]. The plain listener stays up beside it for same-host
//! clients such as `mcporter`, and narrows to loopback so that it serves only
//! them: wide TLS, local plain — see [`resolve_plain_host`].

use super::tools::Ad4mMcpHandler;
use crate::config::TlsConfig;
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
    /// The executor's TLS material, shared with the RPC port. `Some` starts a
    /// second, HTTPS listener beside the plain one — see [`start_mcp_server`].
    /// There is no separate MCP certificate to issue or renew.
    pub tls: Option<TlsConfig>,
}

impl Default for McpServerConfig {
    fn default() -> Self {
        Self {
            port: 3001,
            host: None,
            dynamic_class_tools: false,
            tls: None,
        }
    }
}

/// Why the HTTPS listener has no port, when it has none.
///
/// One variant per reason rather than an `Option`, because the two refusals
/// need different warnings and different operator actions: one says lower
/// `--mcp-port`, the other says move it away from the RPC HTTPS port. Folding
/// them into `None` would make the log say "choose a lower MCP port" to an
/// operator whose port is fine and whose collision is elsewhere.
#[derive(Debug, PartialEq, Eq)]
enum TlsPort {
    /// `mcp_port + 1`, free of the RPC port's HTTPS port as far as config says.
    Use(u16),
    /// `--mcp-port` is `u16::MAX`; there is no port above it.
    NoPortAbove,
    /// `mcp_port + 1` is the RPC server's own HTTPS port, which binds first.
    ClashesWithRpcTls(u16),
}

/// The port the HTTPS listener takes: one above the plain MCP port.
///
/// Derived rather than configured because an operator who has already chosen
/// `--mcp-port` has said where MCP lives; a second flag to choose a port one
/// higher is a knob with no decision behind it. A deployment that needs the
/// two ports apart is the reason to add the flag, and it does not exist yet.
///
/// Derivation is cheap to get wrong, so the one collision this executor can
/// see from config — its own RPC HTTPS port — is checked here rather than left
/// to the bind. Every other collision (a neighbouring executor's plain MCP
/// port, say: two executors per host is this fleet's normal shape, and `+1`
/// walks straight into the neighbour) is invisible to config and is caught by
/// binding the socket before the caller is told HTTPS is up.
fn resolve_tls_port(plain_port: u16, rpc_tls_port: u16) -> TlsPort {
    match plain_port.checked_add(1) {
        None => TlsPort::NoPortAbove,
        Some(port) if port == rpc_tls_port => TlsPort::ClashesWithRpcTls(port),
        Some(port) => TlsPort::Use(port),
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

/// Where the *plain* listener binds once HTTPS is up beside it.
///
/// [`resolve_host`] decides who may reach MCP at all. This decides something
/// narrower: given that an encrypted port now exists, does the cleartext one
/// still need to be reachable from off-box? It does not. Both listeners accept
/// the same admin credential and the same JWTs, so leaving the plain one on
/// `0.0.0.0` means the credential TLS was added to protect can still cross the
/// network in the clear on the port next door — and the reason to add TLS on a
/// LAN is that the LAN is not trusted. TLS that is merely *additive* changes
/// what an attacker must do by nothing at all; they use the other port.
///
/// So HTTPS listening narrows plain to loopback, which is the audience the
/// module doc already claims for it: same-host clients such as `mcporter`,
/// which speak plain HTTP to `127.0.0.1` and would need `allowInsecureHttp`
/// or a cert-trust dance to use the HTTPS port. Wide TLS, local plain.
///
/// Two cases deliberately keep the old binding:
///
/// - **An explicit `--mcp-host` / `MCP_HOST`.** Same rule as [`resolve_host`]:
///   the operator said where MCP lives, and a deployment fronting the plain
///   port with its own TLS gateway is exactly why that escape hatch exists.
/// - **TLS configured but not listening** — no admin credential, or no port
///   above `--mcp-port`. There is then no encrypted port to move to, and
///   narrowing would take remote access away while offering nothing in its
///   place. `tls_listening` is the *outcome* of starting the listener, not the
///   config, so this cannot drift from the decisions
///   [`start_tls_listener`] makes.
///
/// `tls_listening` does prove the HTTPS socket bound: [`start_tls_listener`]
/// creates the listener synchronously before returning, so a port conflict
/// reaches this function as `false` and the plain bind is left alone. That is
/// the one place this diverges from the RPC port, which narrows on
/// `config.tls.is_some()` — on configuration, which cannot fail. Narrowing on
/// an outcome is the stronger rule, and it is worth the divergence precisely
/// because the port here is *derived* (`--mcp-port + 1`) rather than typed by
/// an operator: nobody chose it, so nobody checked what else is on it.
///
/// What no boolean here can prove is that the HTTPS listener ever carried an
/// MCP message — that needs an end-to-end test with a certificate fixture,
/// which #986 names and this PR does not add.
fn resolve_plain_host(
    requested: Option<&str>,
    has_credential: bool,
    tls_listening: bool,
) -> String {
    match requested {
        None if tls_listening => "127.0.0.1".to_string(),
        _ => resolve_host(requested, has_credential),
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

    let initial_token = auth_token;

    if config.dynamic_class_tools {
        info!("MCP: dynamic per-class SHACL tools are exposed (dynamicClassTools=true)");
    } else {
        info!("MCP: static tool surface only (dynamicClassTools=false); per-class tools hidden");
    }

    let has_credential = admin_credential.is_some();
    let context = McpContext {
        admin_credential,
        auth_token: Arc::new(RwLock::new(initial_token.clone())),
        dynamic_class_tools: config.dynamic_class_tools,
    };

    let tls_listening = match config.tls.clone() {
        Some(tls) => {
            start_tls_listener(
                &context,
                initial_token.clone(),
                config.port,
                has_credential,
                tls,
            )
            .await?
        }
        None => false,
    };

    let host = resolve_plain_host(requested.as_deref(), has_credential, tls_listening);
    let addr: SocketAddr = format!("{}:{}", host, config.port).parse()?;
    info!("Starting AD4M MCP server on http://{}", addr);

    // Create the TCP listener and serve using axum
    let listener = tokio::net::TcpListener::bind(addr).await?;
    info!("MCP HTTP server listening on {}", addr);

    // NOTE: Bearer header auth removed — the per-session token isolation means an HTTP
    // middleware can't route tokens to the correct session without a session ID registry.
    // Clients authenticate via MCP tools (request_capability + login_email) instead.
    axum::serve(listener, mcp_router(&context, initial_token)).await?;

    Ok(())
}

/// One MCP transport: a router with its own session manager.
///
/// Built per listener rather than shared, because a session id is a bearer
/// credential — a session opened over HTTPS must not be resumable by anyone who
/// guesses or intercepts its id on the plaintext port.
fn mcp_router(context: &McpContext, initial_token: Option<String>) -> axum::Router {
    let context = context.clone();
    let service = StreamableHttpService::new(
        move || {
            let mut ctx = context.clone();
            // Per-session auth token — prevents cross-session token leaking
            ctx.auth_token = Arc::new(RwLock::new(initial_token.clone()));
            Ok(Ad4mMcpHandler::new(ctx))
        },
        Arc::new(LocalSessionManager::default()),
        StreamableHttpServerConfig::default(),
    );
    axum::Router::new().fallback_service(service)
}

/// Serve MCP over HTTPS as well, on `mcp_port + 1`, using the RPC port's cert.
///
/// This is what makes remote agent login safe without an SSH tunnel or a
/// reverse proxy: the credential crosses an encrypted connection instead of
/// riding on the client's `allowInsecureHttp` escape hatch.
///
/// It binds `0.0.0.0`, because a TLS endpoint nobody outside can reach answers
/// no question. That is only sound while something authenticates the caller, so
/// **without an admin credential the HTTPS listener does not start at all** —
/// the same rule `resolve_host` applies to the plain listener, in the one place
/// where the consequence is worse: TLS proves the server's identity to a caller
/// whom the server would then not check at all.
///
/// A bad certificate path fails startup, matching the RPC server.
///
/// Returns whether the HTTPS listener actually bound. The plain listener reads
/// that to decide whether it can narrow to loopback — see
/// [`resolve_plain_host`] — so every warn-and-continue path below must report
/// `false`, not merely log.
///
/// "Actually bound" is why the socket is created here, synchronously, and
/// handed to `from_tcp_rustls`, rather than letting `bind_rustls` bind inside
/// the spawned task. Spawning first would make the returned `true` a
/// prediction: on a port conflict the cleartext listener would already have
/// narrowed to loopback, and the failure would arrive later, from a detached
/// task, with no remote MCP surface left at all. Binding first turns that
/// outcome into `false` — no HTTPS, cleartext stays reachable, loud warning.
async fn start_tls_listener(
    context: &McpContext,
    initial_token: Option<String>,
    plain_port: u16,
    has_credential: bool,
    tls: TlsConfig,
) -> Result<bool> {
    if !has_credential {
        warn!(
            "MCP: TLS is configured but no --admin-credential is set, so the HTTPS MCP listener \
             is not started. An unauthenticated HTTPS endpoint would publish every AD4M tool to \
             anyone who can reach it. Set an admin credential to enable it."
        );
        return Ok(false);
    }
    let tls_port = match resolve_tls_port(plain_port, tls.tls_port) {
        TlsPort::Use(port) => port,
        TlsPort::NoPortAbove => {
            warn!(
                "MCP: no port above --mcp-port={plain_port}, so the HTTPS MCP listener is not \
                 started. Choose a lower MCP port."
            );
            return Ok(false);
        }
        TlsPort::ClashesWithRpcTls(port) => {
            warn!(
                "MCP: the HTTPS MCP port is --mcp-port + 1 = {port}, which is already the \
                 executor's RPC HTTPS port, so the HTTPS MCP listener is not started. Move \
                 --mcp-port so that --mcp-port + 1 is free."
            );
            return Ok(false);
        }
    };

    let addr = SocketAddr::from(([0, 0, 0, 0], tls_port));

    // Bind before cert I/O and before reporting success: check port
    // availability first so that a conflict returns Ok(false) rather than
    // propagating an Err after the cert has already loaded. See the doc comment
    // above for why the bind must happen synchronously here.
    let std_listener = match std::net::TcpListener::bind(addr) {
        Ok(listener) => listener,
        Err(e) => {
            warn!(
                "MCP: could not bind the HTTPS MCP listener on {addr}: {e}. HTTPS MCP is not \
                 started; the cleartext listener keeps its usual address. Free port {tls_port} \
                 and restart the executor to serve MCP over TLS."
            );
            return Ok(false);
        }
    };
    // axum-server drives this socket from tokio; a blocking accept would stall
    // the runtime thread it lands on.
    std_listener.set_nonblocking(true)?;

    let rustls_config = axum_server::tls_rustls::RustlsConfig::from_pem_file(
        &tls.cert_file_path,
        &tls.key_file_path,
    )
    .await
    .map_err(|e| anyhow::anyhow!("MCP TLS config error: {}", e))?;

    let router = mcp_router(context, initial_token);
    info!("Starting AD4M MCP server (HTTPS) on https://{}", addr);
    tokio::spawn(async move {
        axum_server::from_tcp_rustls(std_listener, rustls_config)
            .serve(router.into_make_service())
            .await
            // Reached only if serving stops after the socket was ours, so this
            // is a running listener dying rather than a failure to start.
            // Name the outage, not just the listener that caused it: by now
            // `resolve_plain_host` has narrowed the cleartext listener to
            // loopback, so an operator reading "TLS server error" reasonably
            // concludes HTTPS is missing and the rest still works; what
            // actually happened is that MCP has no remote surface at all.
            .unwrap_or_else(|e| {
                log::error!(
                    "MCP HTTPS listener on port {tls_port} stopped: {e}. Remote MCP is now \
                     unavailable: the cleartext listener was narrowed to 127.0.0.1:{plain_port} \
                     because this listener was expected to serve remote clients (unless \
                     --mcp-host was set explicitly, which overrides that). Free port \
                     {tls_port} and restart the executor to restore remote access."
                )
            });
    });
    Ok(true)
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
    fn the_https_port_sits_one_above_the_plain_one() {
        assert_eq!(resolve_tls_port(3001, 12000), TlsPort::Use(3002));
        assert_eq!(resolve_tls_port(0, 12000), TlsPort::Use(1));
    }

    #[test]
    fn there_is_no_https_port_above_the_last_one() {
        // The plain listener still starts; only HTTPS is refused, loudly.
        assert_eq!(resolve_tls_port(u16::MAX, 12000), TlsPort::NoPortAbove);
    }

    #[test]
    fn the_rpc_https_port_is_not_taken_from_the_rpc_server() {
        // --mcp-port one below the RPC HTTPS port would send MCP to bind a
        // port the executor itself is about to take. Refused by name, so the
        // warning tells the operator which of the two ports to move.
        assert_eq!(
            resolve_tls_port(12000, 12001),
            TlsPort::ClashesWithRpcTls(12001)
        );
        // One away in the other direction is fine: only the derived port matters.
        assert_eq!(resolve_tls_port(12001, 12001), TlsPort::Use(12002));
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

    #[test]
    fn https_listening_narrows_the_cleartext_port_to_loopback() {
        // The case the pairing exists for: a credentialed node with TLS up.
        // Both listeners accept the same credential, so leaving plain wide
        // would let it cross the LAN in the clear on the port next door.
        assert_eq!(resolve_plain_host(None, true, true), "127.0.0.1");
    }

    #[test]
    fn tls_configured_but_not_listening_leaves_the_plain_bind_alone() {
        // No credential, or no port above --mcp-port: start_tls_listener warns
        // and returns false. There is no encrypted port to move to, so
        // narrowing would remove remote access and offer nothing back.
        // Ungated stays loopback for its own reason; gated stays wide.
        assert_eq!(resolve_plain_host(None, false, false), "127.0.0.1");
        assert_eq!(resolve_plain_host(None, true, false), "0.0.0.0");
    }

    #[test]
    fn an_explicit_host_outranks_the_tls_pairing() {
        // Same escape hatch resolve_host honours: a deployment fronting the
        // plain port with its own TLS gateway needs it to stay reachable.
        assert_eq!(resolve_plain_host(Some("10.0.0.5"), true, true), "10.0.0.5");
        // Including the ungated case, which still only warns.
        assert_eq!(
            resolve_plain_host(Some("10.0.0.5"), false, true),
            "10.0.0.5"
        );
    }

    /// The bind happens synchronously before `start_tls_listener` returns, so a
    /// port conflict degrades to `Ok(false)` — not to `Ok(true)` (which the
    /// pre-fix code returned immediately after `tokio::spawn`, before touching
    /// the socket) and not to `Err` (which cert-I/O-before-bind would produce
    /// because the nonexistent path would fail first).
    #[tokio::test]
    async fn start_tls_listener_returns_false_when_port_is_already_held() {
        // Hold a port so the TLS bind fails.
        let holder = std::net::TcpListener::bind("0.0.0.0:0").unwrap();
        let held_port = holder.local_addr().unwrap().port();
        // plain_port + 1 == held_port so resolve_tls_port yields TlsPort::Use,
        // reaching the actual bind.  rpc_tls_port differs so we don't hit the
        // ClashesWithRpcTls early-return instead.
        let plain_port = held_port.wrapping_sub(1);
        let rpc_tls_port = held_port.wrapping_add(1);

        let context = McpContext {
            admin_credential: Some("cred".to_string()),
            auth_token: Arc::new(RwLock::new(None)),
            dynamic_class_tools: false,
        };
        let tls = TlsConfig {
            cert_file_path: "/nonexistent/cert.pem".to_string(),
            key_file_path: "/nonexistent/key.pem".to_string(),
            tls_port: rpc_tls_port,
        };

        let result = start_tls_listener(&context, None, plain_port, true, tls).await;
        assert_eq!(
            result.unwrap(),
            false,
            "a held port must degrade to Ok(false); Ok(true) means the bind \
             is still inside the spawned task (pre-fix), Err means cert I/O \
             happened before the bind (wrong ordering)"
        );
    }
}

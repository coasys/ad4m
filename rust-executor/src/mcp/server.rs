//! MCP Server setup and transport handling
//!
//! The MCP server runs as an HTTP service alongside AD4M's REST server.
//! AI agents connect via HTTP to interact with AD4M perspectives, models,
//! and neighbourhoods using the Model Context Protocol.
//!
//! Authentication: each MCP session opens one `Session` ([`super::caller`]) from the
//! `Authorization` header or from the token a login tool (`login_email`,
//! `verify_email_code`, `generate_jwt`) sets, the same way RPC does. Each MCP session has
//! its own [`Caller`], so no token crosses from one MCP session to another.
//!
//! What the server binds follows from that: on a node with no admin credential a
//! caller without a token on a loopback listener is the operator, so the server is
//! reachable from the node itself and nowhere else. See [`resolve_host`].
//!
//! When the executor is configured for TLS, MCP is also served over HTTPS on
//! `mcp_port + 1`, using the same certificate as the RPC port — see
//! [`start_tls_listener`]. The plain listener stays up beside it for same-host
//! clients such as `mcporter`, and narrows to loopback so that it serves only
//! them: wide TLS, local plain — see [`resolve_plain_host`].

use super::caller::Caller;
use super::tools::Ad4mMcpHandler;
use crate::agent::capabilities::ListenerReach;
use crate::config::TlsConfig;
use crate::secret::Secret;
use anyhow::Result;
use log::{info, warn};
use rmcp::transport::streamable_http_server::{
    session::local::LocalSessionManager, StreamableHttpServerConfig, StreamableHttpService,
};
use std::net::{IpAddr, SocketAddr};
use std::sync::Arc;

/// One MCP session's context: the node's admin credential, the listener's reach and the
/// session's caller.
#[derive(Clone)]
pub struct McpContext {
    pub admin_credential: Option<Secret>,
    /// Who can connect to the listener: decides what a caller without a token is (#1059).
    pub reach: ListenerReach,
    /// The session this MCP session's token opened.
    pub caller: Caller,
    /// Whether the dynamic per-class SHACL tools are exposed over this MCP
    /// transport (listed by `tools/list`, callable by `tools/call`). Off by
    /// default so external clients see a stable, DNA-independent tool set;
    /// the generic `instance_*` tools cover the same operations. Only the
    /// MCP transport reads this — the in-process harness bridge
    /// (`list_tool_schemas` / `call_tool_by_name`) always includes the
    /// dynamic tools.
    pub dynamic_class_tools: bool,
}

impl McpContext {
    /// A context whose caller has no token yet.
    pub fn new(
        admin_credential: Option<Secret>,
        reach: ListenerReach,
        dynamic_class_tools: bool,
    ) -> Self {
        Self {
            admin_credential,
            reach,
            caller: Caller::default(),
            dynamic_class_tools,
        }
    }

    /// The same context with a caller that starts from `token`: for a session whose token
    /// is known before any call, as in the in-process interpretation harness.
    pub fn with_token(mut self, token: &str) -> Self {
        self.caller = Caller::from_token(token, &self.admin_credential, self.reach);
        self
    }
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
/// Without an admin credential a caller with no token on a loopback listener
/// is the operator (`Session::open`, the single-user local trust model RPC
/// uses), and the operator may run `request_capability`, which mints
/// `ALL_CAPABILITY`. So an ungated server binds loopback and nothing else.
/// Configure a credential and the default widens back to `0.0.0.0`, where a
/// sibling container on the same Docker network can reach it.
///
/// A multi-user node is **not** gated by having user accounts. A login JWT
/// authenticates a user; it is not an admin credential. `--enable-multi-user`
/// without `--admin-credential` is an ungated node and binds loopback here.
///
/// `MCP_HOST` is the operator saying it outright and is honoured either way.
/// On an ungated node a non-loopback bind makes the listener a network one,
/// so a caller there without a token is anonymous, not the operator (#1059),
/// and every caller on it, local ones included, must present a token. That
/// combination warns rather than passing quietly.
fn resolve_host(requested: Option<&str>, has_credential: bool) -> String {
    match requested {
        Some(host) => {
            if !has_credential && !is_loopback(host) {
                warn!(
                    "MCP: MCP_HOST={host} binds beyond loopback with no --admin-credential set. \
                     Callers without a token are anonymous on it, local ones included, so \
                     they must log in or present a capability token. Set an admin \
                     credential, or bind 127.0.0.1."
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
/// MCP message. That is proven end-to-end — generated certificate fixture,
/// real [`start_mcp_server`], an MCP initialize + tools/list over HTTPS — by
/// `https_listener_carries_mcp_and_narrows_plain_to_loopback` in this
/// module's tests: the fixture test #986 named, added in #1050.
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
    admin_credential: Option<Secret>,
    config: McpServerConfig,
) -> Result<()> {
    let requested = config
        .host
        .clone()
        .or_else(|| std::env::var("MCP_HOST").ok());

    if config.dynamic_class_tools {
        info!("MCP: dynamic per-class SHACL tools are exposed (dynamicClassTools=true)");
    } else {
        info!("MCP: static tool surface only (dynamicClassTools=false); per-class tools hidden");
    }

    let has_credential = admin_credential.is_some();

    let tls_listening = match config.tls.clone() {
        Some(tls) => {
            start_tls_listener(
                &admin_credential,
                config.dynamic_class_tools,
                config.port,
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

    let reach = ListenerReach::of(&listener.local_addr()?);
    let context = McpContext::new(admin_credential, reach, config.dynamic_class_tools);
    axum::serve(listener, mcp_router(context)).await?;

    Ok(())
}

/// One MCP transport: a router with its own session manager.
///
/// Built per listener rather than shared, because a session id is a bearer
/// credential — a session opened over HTTPS must not be resumable by anyone who
/// guesses or intercepts its id on the plaintext port. The listener's reach rides
/// each request as an extension, as on the API (`api::listener_router`), so
/// `api::auth::listener_reach` also reads a proxied request as a network one.
fn mcp_router(context: McpContext) -> axum::Router {
    let reach = context.reach;
    let service = StreamableHttpService::new(
        move || {
            // Each MCP session gets its own caller: no token crosses between sessions.
            let mut ctx = context.clone();
            ctx.caller = Caller::default();
            Ok(Ad4mMcpHandler::new(ctx))
        },
        Arc::new(LocalSessionManager::default()),
        StreamableHttpServerConfig::default(),
    );
    axum::Router::new()
        .fallback_service(service)
        .layer(axum::Extension(reach))
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
/// A bad certificate path fails this listener (fail-closed) — unlike the RPC
/// API, where TLS being configured keeps the cleartext listener on loopback
/// even when the certificate fails to load. A broken cert therefore leaves
/// the RPC API reachable locally with no remote surface, while it leaves MCP
/// down entirely.
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
    admin_credential: &Option<Secret>,
    dynamic_class_tools: bool,
    plain_port: u16,
    tls: TlsConfig,
) -> Result<bool> {
    if admin_credential.is_none() {
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

    let context = McpContext::new(
        admin_credential.clone(),
        ListenerReach::of(&addr),
        dynamic_class_tools,
    );
    let router = mcp_router(context);
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
        // With no admin credential, a caller without a token on a loopback
        // listener is the operator. Loopback is the only place that is safe.
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

        let tls = TlsConfig {
            cert_file_path: "/nonexistent/cert.pem".to_string(),
            key_file_path: "/nonexistent/key.pem".to_string(),
            tls_port: rpc_tls_port,
        };

        let result = start_tls_listener(&Some("cred".into()), false, plain_port, tls).await;
        assert_eq!(
            result.unwrap(),
            false,
            "a held port must degrade to Ok(false); Ok(true) means the bind \
             is still inside the spawned task (pre-fix), Err means cert I/O \
             happened before the bind (wrong ordering)"
        );
    }

    /// Two adjacent free ports `(plain, plain + 1)`, so `resolve_tls_port`
    /// derives an HTTPS port the test server can actually bind. Both probe
    /// sockets drop before returning; the window before the server rebinds
    /// them is acceptable in a `--test-threads=1` suite.
    fn free_adjacent_ports() -> (u16, u16) {
        for _ in 0..64 {
            let probe = std::net::TcpListener::bind("0.0.0.0:0").unwrap();
            let plain = probe.local_addr().unwrap().port();
            if plain == u16::MAX {
                continue;
            }
            if std::net::TcpListener::bind(("0.0.0.0", plain + 1)).is_ok() {
                return (plain, plain + 1);
            }
        }
        panic!("could not find two adjacent free ports");
    }

    async fn wait_until_listening(port: u16) {
        for _ in 0..200 {
            if tokio::net::TcpStream::connect(("127.0.0.1", port))
                .await
                .is_ok()
            {
                return;
            }
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        }
        panic!("port {port} never started listening");
    }

    /// Read the SSE response body until the JSON-RPC response for `id`
    /// arrives, and return it decoded. The stream can stay open for
    /// keep-alives after the message, so this reads chunks against a deadline
    /// instead of collecting the whole body.
    async fn read_jsonrpc_response(mut resp: reqwest::Response, id: u64) -> serde_json::Value {
        let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(15);
        let mut buf = String::new();
        loop {
            let chunk = tokio::time::timeout_at(deadline, resp.chunk())
                .await
                .unwrap_or_else(|_| panic!("timed out waiting for response {id}; got: {buf}"))
                .expect("reading the SSE body failed");
            let Some(chunk) = chunk else {
                panic!("SSE stream ended without a response to request {id}; got: {buf}");
            };
            buf.push_str(&String::from_utf8_lossy(&chunk));
            for line in buf.lines() {
                if let Some(data) = line.strip_prefix("data:") {
                    if let Ok(v) = serde_json::from_str::<serde_json::Value>(data.trim()) {
                        if v.get("id").and_then(|i| i.as_u64()) == Some(id) {
                            return v;
                        }
                    }
                }
            }
        }
    }

    /// The end-to-end proof the [`resolve_plain_host`] doc comment points at:
    /// a certificate fixture generated at test time, a real
    /// [`start_mcp_server`], and an MCP `initialize` + `tools/list` exchange
    /// carried over the HTTPS listener — the test #986 named and #1050 adds.
    /// The client trusts exactly the fixture certificate (no
    /// `danger_accept_invalid_certs`), so a passing handshake means the
    /// server terminated TLS with that certificate, not merely that a socket
    /// was open.
    ///
    /// Alongside it, the narrowing [`resolve_plain_host`] only *decides* is
    /// observed on real sockets: while HTTPS is up, the plain listener
    /// answers on 127.0.0.1 and refuses the rest of 127/8.
    #[tokio::test]
    async fn https_listener_carries_mcp_and_narrows_plain_to_loopback() {
        // An inherited MCP_HOST would override the binding decisions this
        // test observes.
        std::env::remove_var("MCP_HOST");
        // In production `run()` installs this before any listener starts
        // (lib.rs); the test binary has to do it itself or rustls refuses to
        // pick between the ring and aws-lc-rs features in the dependency tree.
        let _ = rustls::crypto::aws_lc_rs::default_provider().install_default();

        let cert = rcgen::generate_simple_self_signed(vec!["localhost".to_string()])
            .expect("generate self-signed certificate");
        let dir = tempfile::tempdir().expect("tempdir");
        let cert_path = dir.path().join("cert.pem");
        let key_path = dir.path().join("key.pem");
        std::fs::write(&cert_path, cert.cert.pem()).unwrap();
        std::fs::write(&key_path, cert.signing_key.serialize_pem()).unwrap();

        let (plain_port, tls_port) = free_adjacent_ports();
        let config = McpServerConfig {
            port: plain_port,
            host: None,
            dynamic_class_tools: false,
            tls: Some(TlsConfig {
                cert_file_path: cert_path.to_string_lossy().into_owned(),
                key_file_path: key_path.to_string_lossy().into_owned(),
                // Only compared against plain_port + 1 in the clash check,
                // never bound; any other value works.
                tls_port: plain_port - 1,
            }),
        };
        tokio::spawn(start_mcp_server(Some("test-admin-cred".into()), config));
        wait_until_listening(tls_port).await;
        wait_until_listening(plain_port).await;

        let client = reqwest::Client::builder()
            .add_root_certificate(
                reqwest::Certificate::from_pem(cert.cert.pem().as_bytes()).unwrap(),
            )
            .build()
            .unwrap();
        let url = format!("https://localhost:{tls_port}/mcp");

        let resp = client
            .post(&url)
            .header("accept", "application/json, text/event-stream")
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": { "name": "tls-e2e-test", "version": "0" }
                }
            }))
            .send()
            .await
            .expect("initialize over HTTPS");
        assert!(
            resp.status().is_success(),
            "initialize returned {}",
            resp.status()
        );
        let session_id = resp
            .headers()
            .get("mcp-session-id")
            .expect("initialize response carries a session id")
            .to_str()
            .unwrap()
            .to_string();
        let init = read_jsonrpc_response(resp, 1).await;
        assert_eq!(
            init["result"]["serverInfo"]["name"], "ad4m-executor",
            "the HTTPS listener answered initialize with something other than \
             this server: {init}"
        );

        // The spec-required notification, then a second request on the same
        // session: more MCP messages over the same TLS listener.
        let resp = client
            .post(&url)
            .header("accept", "application/json, text/event-stream")
            .header("mcp-session-id", &session_id)
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "method": "notifications/initialized"
            }))
            .send()
            .await
            .expect("initialized notification over HTTPS");
        assert!(
            resp.status().is_success(),
            "initialized notification returned {}",
            resp.status()
        );

        let resp = client
            .post(&url)
            .header("accept", "application/json, text/event-stream")
            .header("mcp-session-id", &session_id)
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 2,
                "method": "tools/list"
            }))
            .send()
            .await
            .expect("tools/list over HTTPS");
        let tools = read_jsonrpc_response(resp, 2).await;
        let names: Vec<&str> = tools["result"]["tools"]
            .as_array()
            .expect("tools/list returns a tool array")
            .iter()
            .filter_map(|t| t["name"].as_str())
            .collect();
        assert!(
            names.contains(&"get_documentation"),
            "tools/list over HTTPS is missing the static surface: {names:?}"
        );

        // The narrowing itself, on real sockets. 127.0.0.2 is loopback that
        // the *address* 127.0.0.1 does not cover, so a 0.0.0.0 bind accepts
        // it and a 127.0.0.1 bind refuses it. All of 127/8 sits on `lo` on
        // Linux only; elsewhere the resolve_plain_host unit tests carry this.
        tokio::net::TcpStream::connect(("127.0.0.1", plain_port))
            .await
            .expect("plain listener answers on loopback");
        #[cfg(target_os = "linux")]
        assert!(
            tokio::net::TcpStream::connect(("127.0.0.2", plain_port))
                .await
                .is_err(),
            "the plain listener is reachable beyond 127.0.0.1 while HTTPS is up"
        );
    }

    /// The other half of the pairing on real sockets: TLS configured but not
    /// listening — its derived port already held — leaves the credentialed
    /// plain listener wide. Narrowing here would remove remote access while
    /// offering no encrypted port in exchange (#1050).
    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn plain_listener_stays_wide_when_tls_cannot_bind() {
        std::env::remove_var("MCP_HOST");

        let (plain_port, tls_port) = free_adjacent_ports();
        let _holder = std::net::TcpListener::bind(("0.0.0.0", tls_port)).unwrap();
        let config = McpServerConfig {
            port: plain_port,
            host: None,
            dynamic_class_tools: false,
            tls: Some(TlsConfig {
                // Never read: the bind on the held port fails first.
                cert_file_path: "/nonexistent/cert.pem".to_string(),
                key_file_path: "/nonexistent/key.pem".to_string(),
                tls_port: plain_port - 1,
            }),
        };
        tokio::spawn(start_mcp_server(Some("test-admin-cred".into()), config));
        wait_until_listening(plain_port).await;
        tokio::net::TcpStream::connect(("127.0.0.2", plain_port))
            .await
            .expect("with no HTTPS listener, the credentialed plain listener stays on 0.0.0.0");
    }

    /// A plain MCP server on 127.0.0.1 and an initialized MCP session on it.
    async fn local_server_session(admin_credential: Option<Secret>) -> (String, String) {
        crate::test_utils::ensure_db();
        let (port, _) = free_adjacent_ports();
        let config = McpServerConfig {
            port,
            host: Some("127.0.0.1".to_string()),
            dynamic_class_tools: false,
            tls: None,
        };
        tokio::spawn(start_mcp_server(admin_credential, config));
        wait_until_listening(port).await;

        let url = format!("http://127.0.0.1:{port}/mcp");
        let client = reqwest::Client::new();
        let resp = client
            .post(&url)
            .header("accept", "application/json, text/event-stream")
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": { "name": "mcp-session-test", "version": "0" }
                }
            }))
            .send()
            .await
            .expect("initialize");
        let session_id = resp.headers()["mcp-session-id"]
            .to_str()
            .unwrap()
            .to_string();
        read_jsonrpc_response(resp, 1).await;
        client
            .post(&url)
            .header("accept", "application/json, text/event-stream")
            .header("mcp-session-id", &session_id)
            .json(&serde_json::json!({ "jsonrpc": "2.0", "method": "notifications/initialized" }))
            .send()
            .await
            .expect("initialized notification");
        (url, session_id)
    }

    /// Calls `tool` over HTTP with `headers`, and returns the text of its result.
    async fn call(
        url: &str,
        session: &str,
        id: u64,
        tool: &str,
        headers: &[(&str, &str)],
    ) -> String {
        let mut request = reqwest::Client::new()
            .post(url)
            .header("accept", "application/json, text/event-stream")
            .header("mcp-session-id", session);
        for (name, value) in headers {
            request = request.header(*name, *value);
        }
        let resp = request
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": id,
                "method": "tools/call",
                "params": { "name": tool, "arguments": {} }
            }))
            .send()
            .await
            .expect("tools/call");
        let answer = read_jsonrpc_response(resp, id).await;
        answer["result"]["content"][0]["text"]
            .as_str()
            .unwrap_or_else(|| panic!("no text in {answer}"))
            .to_string()
    }

    fn json(text: &str) -> serde_json::Value {
        serde_json::from_str(text).unwrap_or_else(|_| panic!("not JSON: {text}"))
    }

    // #851 1 + 2 over the real transport: the admin credential in the header is the
    // operator, auth_status says so, and the MCP session remembers it.
    #[tokio::test]
    async fn the_admin_credential_header_is_the_operator_over_http() {
        let (url, session) = local_server_session(Some("http-admin".into())).await;

        let refused = call(&url, &session, 2, "get_my_did", &[]).await;
        assert!(refused.contains("Authentication required."), "{refused}");
        let anonymous = json(&call(&url, &session, 3, "auth_status", &[]).await);
        assert_eq!(anonymous["authenticated"], false, "{anonymous}");

        let wrong = call(
            &url,
            &session,
            4,
            "get_my_did",
            &[("authorization", "wrong")],
        )
        .await;
        assert!(wrong.contains("Authentication required:"), "{wrong}");

        let bearer = [("authorization", "Bearer http-admin")];
        let status = json(&call(&url, &session, 5, "auth_status", &bearer).await);
        assert_eq!(status["authenticated"], true, "{status}");
        assert_eq!(status["principal"], "operator");
        let remembered = json(&call(&url, &session, 6, "auth_status", &[]).await);
        assert_eq!(remembered["principal"], "operator", "{remembered}");
    }

    // Without a credential, a caller with no token on a loopback listener is the operator,
    // unless the request came through a proxy (#1059), as on RPC.
    #[tokio::test]
    async fn no_token_on_a_local_listener_is_the_operator_unless_proxied() {
        let (url, session) = local_server_session(None).await;
        let local = json(&call(&url, &session, 2, "auth_status", &[]).await);
        assert_eq!(local["principal"], "operator", "{local}");

        let forwarded = [("x-forwarded-for", "203.0.113.9")];
        let proxied = json(&call(&url, &session, 3, "auth_status", &forwarded).await);
        assert_eq!(proxied["authenticated"], false, "{proxied}");
        let refused = call(&url, &session, 4, "get_my_did", &forwarded).await;
        assert!(refused.contains("Authentication required."), "{refused}");
    }
}

//! The `ad4m-connect` flow for Rust apps (`connect/src/core.ts`): find the
//! executor, authenticate — the capability handshake on a single-user
//! executor, email sign-in on a multi-user one — and keep the token for the
//! next run.
//!
//! ```no_run
//! # async fn demo() -> anyhow::Result<()> {
//! use ad4m_client::connect::{Ad4mConnect, AppInfo, ConnectOptions};
//! # struct NoUi;
//! # impl ad4m_client::connect::ConnectPrompts for NoUi {}
//! # let mut ui = NoUi;
//! let mut session = Ad4mConnect::new(ConnectOptions::new(AppInfo::new(
//!     "My egui app",
//!     "does things",
//!     "https://example.com",
//! )))?;
//! session.connect(&mut ui).await?;
//! # Ok(())
//! # }
//! ```

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::future::Future;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::watch;

use crate::types::{CapabilityInput, ResourceInput, UserCreationResult, VerificationRequestResult};
use crate::ws_rpc::{RpcCallError, WsRpcClient};
use crate::Ad4mClient;

/// The port `ad4m-executor run` listens on when nothing overrides it.
pub const DEFAULT_PORT: u16 = 12000;
/// `checkConnection`'s budget per candidate (TS `utils.ts` `checkConnection`).
const HEALTH_TIMEOUT: Duration = Duration::from_secs(10);

/// What the executor says, in words, when a call needs keys a locked wallet
/// cannot open (TS `LOCKED_WALLET`).
const LOCKED_WALLET: &str = "Cannot extractByTags from a ciphered wallet. You must unlock first.";
/// How the executor's close reason (code 1013) and 503 begin when its wallet
/// is locked (TS `EXECUTOR_LOCKED`).
const EXECUTOR_LOCKED: &str = "Executor is locked";
/// The executor's words when a token itself does not hold: a bad signature,
/// past its expiry, revoked (TS `DEAD_TOKEN`).
const DEAD_TOKEN: [&str; 3] = [
    "InvalidSignature",
    "ExpiredSignature",
    "Unauthorized access",
];

// ── Options ──

/// The app as the capability request shows it (TS `AuthInfo`).
#[derive(Debug, Clone)]
pub struct AppInfo {
    pub name: String,
    pub description: String,
    /// Sent as both `appDomain` and `appUrl` (TS `buildAppInfo`).
    pub url: String,
    pub icon_path: Option<String>,
    /// What the request asks for. The executor requires a non-empty list; the
    /// default is the wildcard the CLI asks for (`*:*` / `*`).
    pub capabilities: Vec<CapabilityInput>,
}

impl AppInfo {
    pub fn new(
        name: impl Into<String>,
        description: impl Into<String>,
        url: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            url: url.into(),
            icon_path: None,
            capabilities: vec![CapabilityInput {
                can: vec!["*".to_string()],
                with: ResourceInput {
                    domain: "*".to_string(),
                    pointers: vec!["*".to_string()],
                },
            }],
        }
    }

    /// Ask for narrower capabilities than the wildcard default.
    pub fn capabilities(mut self, capabilities: Vec<CapabilityInput>) -> Self {
        self.capabilities = capabilities;
        self
    }
}

/// `Ad4mConnect` inputs. Only `app` is required.
#[derive(Debug, Clone)]
pub struct ConnectOptions {
    pub app: AppInfo,
    /// An explicit executor URL (`http(s)://` or `ws(s)://` — the latter is
    /// normalized like TS `wsUrlToHttpBase`); wins over every other source.
    pub url: Option<String>,
    /// An explicit port → `http://127.0.0.1:{port}`.
    pub port: Option<u16>,
    /// A multi-user sign-in email; `None` asks through `ConnectPrompts::email`.
    pub email: Option<String>,
    /// The directory holding `tokens.json`; `None` = `{config}/ad4m-connect`.
    pub token_dir: Option<PathBuf>,
    /// `~/.ad4m/executor-port` (tests point it elsewhere).
    pub executor_port_file: Option<PathBuf>,
    /// Whether a stored `last_url` may propose the executor (TS `ad4m-url`).
    pub use_stored_url: bool,
}

impl ConnectOptions {
    pub fn new(app: AppInfo) -> Self {
        Self {
            app,
            url: None,
            port: None,
            email: None,
            token_dir: None,
            executor_port_file: None,
            use_stored_url: true,
        }
    }
}

// ── The UI seam ──

/// What a frontend must show while `connect` runs (TS's views):
/// the pending capability request, the emailed-code and password prompts, and
/// a "waiting" status line. An `Err` answer cancels the connect.
pub trait ConnectPrompts {
    /// A capability request is pending approval (`request_id`): show it and
    /// return the 6-digit code the user typed.
    fn capability_code<'a>(
        &'a mut self,
        request_id: &'a str,
        app: &'a AppInfo,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        let _ = (request_id, app);
        async { Err(anyhow!("this frontend cannot ask for a capability code")) }
    }

    /// Ask the user's email for a multi-user sign-in (never called when
    /// `ConnectOptions::email` is set).
    fn email<'a>(&'a mut self) -> impl Future<Output = Result<String>> + Send + 'a {
        async { Err(anyhow!("this frontend cannot ask for an email")) }
    }

    /// `email` was sent a sign-in code: return what the user typed.
    fn email_code<'a>(
        &'a mut self,
        email: &'a str,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        let _ = email;
        async { Err(anyhow!("this frontend cannot ask for an email code")) }
    }

    /// `email` needs a password: the new account's (`new_account = true`, the
    /// executor emails a sign-up code too) or an existing user's login
    /// password.
    fn password<'a>(
        &'a mut self,
        email: &'a str,
        new_account: bool,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        let _ = (email, new_account);
        async { Err(anyhow!("this frontend cannot ask for a password")) }
    }

    /// A status line the frontend may display ("waiting for approval"…).
    fn status(&mut self, _status: ConnectStatus) {}
}

/// A "waiting" state for `ConnectPrompts::status`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConnectStatus {
    /// Checking candidates and opening the socket.
    Connecting,
    /// A capability request waits for approval in the user's AD4M UI.
    WaitingForApproval { request_id: String },
    /// `email` was sent a sign-in code.
    WaitingForEmailCode { email: String },
    /// The last code was refused — the prompt shows again.
    CodeRejected,
    /// The pending request died (expired, or five wrong codes); a fresh one
    /// is requested next.
    RequestExpired,
}

/// The session's auth state (TS `AuthStates`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AuthState {
    /// No working session. Re-run `connect` (the stored token, if any, was
    /// dropped when the executor refused it).
    Unauthenticated,
    /// The executor's wallet is locked: the client stays — calls that need
    /// keys fail until the operator unlocks. Re-run `connect` after.
    Locked,
    /// A working session (a token the executor accepts, or operator/anonymous
    /// access that needs none).
    Authenticated,
}

/// The failure `connect` returns inside its `anyhow::Error`
/// (`err.downcast_ref::<ConnectError>()`).
#[derive(Debug)]
pub enum ConnectError {
    /// No candidate URL answered `GET /health` as an AD4M executor.
    ExecutorNotFound { tried: Vec<String> },
    /// `user.requestVerification` or `user.create` answered `success: false`.
    Refused { message: String },
}

impl std::fmt::Display for ConnectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConnectError::ExecutorNotFound { tried } => {
                write!(f, "no AD4M executor found (tried {})", tried.join(", "))
            }
            ConnectError::Refused { message } => write!(f, "{message}"),
        }
    }
}

impl std::error::Error for ConnectError {}

// ── The wire seam (production + tests) ──

/// The RPC surface `Ad4mConnect` drives — `WsRpcClient` in production,
/// scripted fakes in tests.
pub trait Rpc {
    fn call<'a>(
        &'a self,
        method: &'a str,
        params: Value,
    ) -> impl Future<Output = Result<Value>> + Send + 'a;
    /// True once the executor has closed the socket refusing the token.
    fn auth_failed(&self) -> bool {
        false
    }
    /// `cb(reason)` runs if the executor closes the socket refusing the token
    /// mid-session (close 1008). Returns the registration's id.
    fn on_auth_failed<'a>(
        &'a self,
        cb: Arc<dyn Fn(&str) + Send + Sync>,
    ) -> impl Future<Output = u64> + Send + 'a {
        let _ = cb;
        async { 0 }
    }
    fn close<'a>(&'a self) -> impl Future<Output = ()> + Send + 'a {
        async {}
    }
}

/// Where the flow gets health checks and sockets from.
pub trait ExecutorIo {
    type Rpc: Rpc;
    /// `Ok(())` only when `base_url` answers `GET /health` `{"status":"ok"}`.
    fn healthy<'a>(&'a self, base_url: &'a str) -> impl Future<Output = Result<()>> + Send + 'a;
    /// Open an RPC socket to `base_url`; `token` may be empty.
    fn rpc<'a>(
        &'a self,
        base_url: &'a str,
        token: &'a str,
    ) -> impl Future<Output = Result<Self::Rpc>> + Send + 'a;
}

impl Rpc for WsRpcClient {
    fn call<'a>(
        &'a self,
        method: &'a str,
        params: Value,
    ) -> impl Future<Output = Result<Value>> + Send + 'a {
        self.call_value(method, params)
    }
    fn auth_failed(&self) -> bool {
        WsRpcClient::auth_failed(self)
    }
    fn on_auth_failed<'a>(
        &'a self,
        cb: Arc<dyn Fn(&str) + Send + Sync>,
    ) -> impl Future<Output = u64> + Send + 'a {
        WsRpcClient::on_auth_failed(self, move |reason| cb(reason))
    }
    fn close<'a>(&'a self) -> impl Future<Output = ()> + Send + 'a {
        WsRpcClient::close(self)
    }
}

impl<T: Rpc + ?Sized> Rpc for Arc<T> {
    fn call<'a>(
        &'a self,
        method: &'a str,
        params: Value,
    ) -> impl Future<Output = Result<Value>> + Send + 'a {
        (**self).call(method, params)
    }
    fn auth_failed(&self) -> bool {
        (**self).auth_failed()
    }
    fn on_auth_failed<'a>(
        &'a self,
        cb: Arc<dyn Fn(&str) + Send + Sync>,
    ) -> impl Future<Output = u64> + Send + 'a {
        (**self).on_auth_failed(cb)
    }
    fn close<'a>(&'a self) -> impl Future<Output = ()> + Send + 'a {
        (**self).close()
    }
}

/// The production `ExecutorIo`: `reqwest` for `/health`, `WsRpcClient` for RPC.
pub struct WsIo {
    http: reqwest::Client,
}

impl WsIo {
    pub fn new() -> Self {
        Self {
            http: reqwest::Client::builder()
                .timeout(HEALTH_TIMEOUT)
                .build()
                .expect("reqwest client"),
        }
    }
}

impl Default for WsIo {
    fn default() -> Self {
        Self::new()
    }
}

impl ExecutorIo for WsIo {
    type Rpc = Arc<WsRpcClient>;

    async fn healthy<'a>(&'a self, base_url: &'a str) -> Result<()> {
        // TS `checkConnection`: 200 + `{status:"ok"}`.
        let url = format!("{}/health", base_url.trim_end_matches('/'));
        let res = self.http.get(&url).send().await?;
        if !res.status().is_success() {
            return Err(anyhow!("Health check returned {}", res.status()));
        }
        let body: Value = res.json().await?;
        if body.get("status").and_then(Value::as_str) != Some("ok") {
            return Err(anyhow!("Not an AD4M executor"));
        }
        Ok(())
    }

    async fn rpc<'a>(&'a self, base_url: &'a str, token: &'a str) -> Result<Arc<WsRpcClient>> {
        WsRpcClient::connect(base_url, token).await.map(Arc::new)
    }
}

// ── Token store ──

/// The default token file's directory: `{config_dir}/ad4m-connect`.
pub fn default_token_dir() -> PathBuf {
    dirs::config_dir()
        .or_else(|| dirs::home_dir().map(|h| h.join(".config")))
        .unwrap_or_else(|| PathBuf::from("."))
        .join("ad4m-connect")
}

/// `~/.ad4m/executor-port` — where the launcher-installed executor publishes
/// its port (`cli/src/startup.rs`).
pub fn default_executor_port_file() -> PathBuf {
    dirs::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".ad4m")
        .join("executor-port")
}

/// Tokens in `tokens.json`, one per executor-URL + app pair, written with
/// file mode 0600. Mirrors TS's `ad4m-token`/`ad4m-url` localStorage keys.
#[derive(Clone, Debug)]
pub struct TokenStore {
    path: PathBuf,
}

#[derive(Default, Serialize, Deserialize)]
struct StoreFile {
    #[serde(default)]
    last_url: Option<String>,
    #[serde(default)]
    tokens: BTreeMap<String, String>,
}

impl TokenStore {
    /// A store at `{dir}/tokens.json`.
    pub fn new(dir: impl Into<PathBuf>) -> Self {
        Self {
            path: dir.into().join("tokens.json"),
        }
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    fn key(url: &str, app: &str) -> String {
        format!("{url}|{app}")
    }

    fn load(&self) -> StoreFile {
        std::fs::read_to_string(&self.path)
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok())
            .unwrap_or_default()
    }

    fn save(&self, store: &StoreFile) -> Result<()> {
        if let Some(dir) = self.path.parent() {
            std::fs::create_dir_all(dir)?;
        }
        // Create with 0600 from the start so the token file is never born
        // readable; `set_permissions` covers a pre-existing loose file too.
        let mut opts = std::fs::OpenOptions::new();
        opts.write(true).create(true).truncate(true);
        #[cfg(unix)]
        {
            use std::os::unix::fs::OpenOptionsExt;
            opts.mode(0o600);
        }
        let file = opts.open(&self.path)?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let _ = std::fs::set_permissions(&self.path, std::fs::Permissions::from_mode(0o600));
        }
        serde_json::to_writer_pretty(file, store)?;
        Ok(())
    }

    /// The stored token for this executor + app, if any.
    pub fn token(&self, url: &str, app: &str) -> Option<String> {
        self.load().tokens.get(&Self::key(url, app)).cloned()
    }

    pub fn set_token(&self, url: &str, app: &str, token: &str) -> Result<()> {
        let mut store = self.load();
        store.tokens.insert(Self::key(url, app), token.to_string());
        self.save(&store)
    }

    pub fn remove_token(&self, url: &str, app: &str) -> Result<()> {
        let mut store = self.load();
        store.tokens.remove(&Self::key(url, app));
        self.save(&store)
    }

    /// The URL last connected — TS `ad4m-url`.
    pub fn last_url(&self) -> Option<String> {
        self.load().last_url
    }

    pub fn set_last_url(&self, url: &str) -> Result<()> {
        let mut store = self.load();
        store.last_url = Some(url.to_string());
        self.save(&store)
    }
}

// ── URL resolution ──

/// `ws(s)://` → `http(s)://`, trailing `/` off (TS `wsUrlToHttpBase`).
pub fn normalize_base_url(url: &str) -> String {
    let u = url.trim().trim_end_matches('/');
    u.strip_prefix("ws://")
        .map(|rest| format!("http://{rest}"))
        .or_else(|| {
            u.strip_prefix("wss://")
                .map(|rest| format!("https://{rest}"))
        })
        .unwrap_or_else(|| u.to_string())
}

// ── The session ──

/// The auth read — `readAuth` in TS.
enum Reading {
    Authenticated,
    Locked,
    /// `refused`: the error named a dead token (401, or 403 matching
    /// `DEAD_TOKEN`), or the socket itself closed 1008 — drop the token.
    Unauthenticated {
        refused: bool,
    },
}

/// A connection's whole state, like TS `Ad4mConnect`: the resolved URL, the
/// adopted socket, the auth state, and the stored token — re-runnable
/// (`connect` again after `Unauthenticated`).
pub struct Ad4mConnect<E: ExecutorIo = WsIo> {
    opts: ConnectOptions,
    io: E,
    store: TokenStore,
    url: Option<String>,
    rpc: Option<E::Rpc>,
    state: watch::Sender<AuthState>,
}

impl Ad4mConnect<WsIo> {
    /// A session over the real transport (`reqwest` + `WsRpcClient`).
    pub fn new(opts: ConnectOptions) -> Result<Self> {
        Self::with_io(opts, WsIo::new())
    }

    /// The `Ad4mClient` of the session's socket, once `connect` ran.
    pub fn client(&self) -> Option<Ad4mClient> {
        self.rpc
            .as_ref()
            .map(|rpc| Ad4mClient::from_ws(rpc.clone()))
    }
}

impl<E: ExecutorIo> Ad4mConnect<E> {
    /// A session over `io` — `WsIo` in production, fakes in tests.
    pub fn with_io(opts: ConnectOptions, io: E) -> Result<Self> {
        let dir = opts.token_dir.clone().unwrap_or_else(default_token_dir);
        let (state, _) = watch::channel(AuthState::Unauthenticated);
        Ok(Self {
            opts,
            io,
            store: TokenStore::new(dir),
            url: None,
            rpc: None,
            state,
        })
    }

    /// The resolved URL, once `connect` ran.
    pub fn url(&self) -> Option<&str> {
        self.url.as_deref()
    }

    /// The adopted socket.
    pub fn rpc(&self) -> Option<&E::Rpc> {
        self.rpc.as_ref()
    }

    /// The current auth state.
    pub fn state(&self) -> AuthState {
        *self.state.borrow()
    }

    /// A receiver the app can `changed().await`: a mid-session token refusal
    /// (grant revoked, token expired) flips it to `Unauthenticated` and drops
    /// the stored token, so the app re-runs `connect` (TS `watchAuth` +
    /// `discardToken`).
    pub fn watch_state(&self) -> watch::Receiver<AuthState> {
        self.state.subscribe()
    }

    /// The token store (e.g. to drop credentials on sign-out).
    pub fn store(&self) -> &TokenStore {
        &self.store
    }

    fn set_state(&self, state: AuthState) -> AuthState {
        // `send_replace`: `send` stores nothing while no receiver is alive,
        // and `state()` would keep reporting the stale value (F2).
        self.state.send_replace(state);
        state
    }

    /// Run the flow: resolve the URL, try the stored token, and when nothing
    /// stored works go through the capability handshake (single-user) or the
    /// email sign-in (multi-user). Returns the end state — `Locked` is not an
    /// error: the wallet must be unlocked, then run again.
    pub async fn connect(&mut self, prompts: &mut impl ConnectPrompts) -> Result<AuthState> {
        prompts.status(ConnectStatus::Connecting);
        let url = self.resolve_url().await?;
        self.url = Some(url.clone());

        // A stored token answers `agent.status` in one call on a good session.
        if let Some(token) = self.store.token(&url, &self.opts.app.name) {
            match self.open_and_read(&url, &token).await {
                Ok((Reading::Authenticated, rpc)) => {
                    let _ = self.store.set_last_url(&url);
                    self.adopt(rpc).await;
                    return Ok(self.set_state(AuthState::Authenticated));
                }
                Ok((Reading::Locked, rpc)) => {
                    self.adopt(rpc).await;
                    return Ok(self.set_state(AuthState::Locked));
                }
                Ok((Reading::Unauthenticated { refused }, rpc)) => {
                    rpc.close().await;
                    if refused {
                        self.store.remove_token(&url, &self.opts.app.name)?;
                    }
                }
                Err(e) => {
                    // The socket never opened: connectivity, not auth — the
                    // token may still be good next time, so keep it.
                    eprintln!("[ad4m-connect] stored-token connect failed: {e}");
                }
            }
        }

        // The anonymous socket: reads the mode and serves the auth calls.
        // On an executor without an admin credential a loopback caller is the
        // operator, so `agent.status` answers and there is nothing to mint.
        let anon = self.io.rpc(&url, "").await?;
        match self.read_auth(&anon).await {
            Reading::Authenticated => {
                let _ = self.store.set_last_url(&url);
                self.adopt(anon).await;
                return Ok(self.set_state(AuthState::Authenticated));
            }
            Reading::Locked => {
                self.adopt(anon).await;
                return Ok(self.set_state(AuthState::Locked));
            }
            Reading::Unauthenticated { .. } => {}
        }

        // TS `isMultiUser`: a failure means single-user.
        let multi_user = anon
            .call("user.multiUserEnabled", json!({}))
            .await
            .ok()
            .and_then(|v| v.as_bool())
            .unwrap_or(false);

        let jwt = if multi_user {
            self.email_login(&anon, prompts).await
        } else {
            self.capability_handshake(&anon, prompts).await
        };
        // The anonymous socket is always closed — a dropped WsRpcClient keeps
        // its reader (and reconnect task) alive.
        anon.close().await;
        let jwt = jwt?;

        // The minted token opens the real session; it must read authenticated.
        let rpc = self.io.rpc(&url, &jwt).await?;
        match self.read_auth(&rpc).await {
            r @ (Reading::Authenticated | Reading::Locked) => {
                // A locked wallet is a state, not a bad token: keep it (TS
                // stores the JWT before `connect()` re-checks the wallet).
                self.store.set_token(&url, &self.opts.app.name, &jwt)?;
                let _ = self.store.set_last_url(&url);
                let authed = matches!(r, Reading::Authenticated);
                self.adopt(rpc).await;
                Ok(self.set_state(if authed {
                    AuthState::Authenticated
                } else {
                    AuthState::Locked
                }))
            }
            Reading::Unauthenticated { .. } => {
                rpc.close().await;
                Err(anyhow!(
                    "the executor still refuses the token it just minted"
                ))
            }
        }
    }

    /// Drop the session and the stored token (TS `disconnect`).
    pub async fn disconnect(&mut self) -> Result<()> {
        if let Some(rpc) = self.rpc.take() {
            rpc.close().await;
        }
        if let Some(url) = &self.url {
            self.store.remove_token(url, &self.opts.app.name)?;
            self.store.set_last_url(url)?;
        }
        self.set_state(AuthState::Unauthenticated);
        Ok(())
    }

    /// Install `rpc` as the session's socket, closing the replaced one, and
    /// wire the mid-session refusal: drop the stored token and go
    /// `Unauthenticated`.
    async fn adopt(&mut self, rpc: E::Rpc) {
        let store = self.store.clone();
        let url = self.url.clone();
        let app = self.opts.app.name.clone();
        let state = self.state.clone();
        rpc.on_auth_failed(Arc::new(move |reason| {
            eprintln!("[ad4m-connect] the executor refused the token: {reason}");
            if let Some(url) = &url {
                let _ = store.remove_token(url, &app);
            }
            state.send_replace(AuthState::Unauthenticated);
        }))
        .await;
        if let Some(old) = self.rpc.replace(rpc) {
            old.close().await;
        }
    }

    /// Ordered candidates: explicit url → explicit port → stored last_url →
    /// `~/.ad4m/executor-port` → the default port. First `/health` win.
    fn candidates(&self) -> Vec<String> {
        let mut out: Vec<String> = Vec::new();
        let mut push = |u: String| {
            if !out.contains(&u) {
                out.push(u);
            }
        };
        if let Some(url) = &self.opts.url {
            push(normalize_base_url(url));
        }
        if let Some(port) = self.opts.port {
            push(format!("http://127.0.0.1:{port}"));
        }
        if self.opts.use_stored_url {
            if let Some(url) = self.store.last_url() {
                push(normalize_base_url(&url));
            }
        }
        let port_file = self
            .opts
            .executor_port_file
            .clone()
            .unwrap_or_else(default_executor_port_file);
        if let Ok(port) = std::fs::read_to_string(&port_file) {
            if let Ok(port) = port.trim().parse::<u16>() {
                push(format!("http://127.0.0.1:{port}"));
            }
        }
        push(format!("http://127.0.0.1:{DEFAULT_PORT}"));
        out
    }

    async fn resolve_url(&self) -> Result<String> {
        let candidates = self.candidates();
        let mut tried = Vec::new();
        for url in candidates {
            tried.push(url.clone());
            match self.io.healthy(&url).await {
                Ok(()) => return Ok(url),
                Err(e) => eprintln!("[ad4m-connect] {url} is not an AD4M executor: {e}"),
            }
        }
        Err(ConnectError::ExecutorNotFound { tried }.into())
    }

    /// Open a socket with `token` and read its auth — a helper so a socket
    /// failure is not confused with an auth one.
    async fn open_and_read(&self, url: &str, token: &str) -> Result<(Reading, E::Rpc)> {
        let rpc = self.io.rpc(url, token).await?;
        let reading = self.read_auth(&rpc).await;
        Ok((reading, rpc))
    }

    /// TS `readAuth`: `agent.status` first; `agent.isLocked` (open to
    /// anonymous callers) tells a locked wallet from a refused token.
    async fn read_auth(&self, rpc: &E::Rpc) -> Reading {
        let status_err = match rpc.call("agent.status", json!({})).await {
            Ok(v) => {
                let initialized = v
                    .get("isInitialized")
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                let unlocked = v
                    .get("isUnlocked")
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                return if initialized && unlocked {
                    Reading::Authenticated
                } else {
                    Reading::Locked
                };
            }
            Err(e) => e,
        };
        if says_locked(&status_err) {
            return Reading::Locked;
        }
        // The socket may have closed 1008 before the reply: that is a refusal
        // too, whatever the call's error says.
        let refused = refuses_token(&status_err) || rpc.auth_failed();
        match rpc.call("agent.isLocked", json!({})).await {
            Ok(v) if v.as_bool() == Some(true) => return Reading::Locked,
            Err(e) if says_locked(&e) => return Reading::Locked,
            _ => {}
        }
        Reading::Unauthenticated {
            refused: refused || rpc.auth_failed(),
        }
    }

    /// `agent.requestCapability` → the code prompt → `agent.generateJwt`,
    /// re-requesting when the request itself dies (TS `requestCapability` +
    /// `verifyLocalAd4mCode`).
    async fn capability_handshake(
        &self,
        rpc: &E::Rpc,
        prompts: &mut impl ConnectPrompts,
    ) -> Result<String> {
        let app = &self.opts.app;
        loop {
            let result = rpc
                .call(
                    "agent.requestCapability",
                    json!({
                        "authInfo": {
                            "appName": app.name,
                            "appDesc": app.description,
                            "appDomain": app.url,
                            "appUrl": app.url,
                            "appIconPath": app.icon_path,
                            "capabilities": app.capabilities,
                        }
                    }),
                )
                .await?;
            let request_id = result
                .as_str()
                .ok_or_else(|| anyhow!("agent.requestCapability did not answer a string"))?
                .to_string();
            prompts.status(ConnectStatus::WaitingForApproval {
                request_id: request_id.clone(),
            });
            loop {
                let code = prompts.capability_code(&request_id, app).await?;
                match rpc
                    .call(
                        "agent.generateJwt",
                        json!({ "requestId": request_id, "rand": code }),
                    )
                    .await
                {
                    Ok(v) => {
                        return Ok(v
                            .as_str()
                            .ok_or_else(|| anyhow!("agent.generateJwt did not answer a string"))?
                            .to_string())
                    }
                    // "…request access again" — expired or five wrong codes:
                    // the request is gone, so ask for a fresh one.
                    Err(e) if request_gone(&e) => {
                        prompts.status(ConnectStatus::RequestExpired);
                        break;
                    }
                    // "Can't find permitted request" — not approved yet or a
                    // wrong code: show the prompt again.
                    Err(_) => prompts.status(ConnectStatus::CodeRejected),
                }
            }
        }
    }

    /// `user.requestVerification` → email code (`user.verifyEmail`) or the
    /// password path (`user.create` + `user.login`) — TS `submitEmail`,
    /// `verifyEmailCode`, `loginWithPassword`, `createAccount`.
    async fn email_login(&self, rpc: &E::Rpc, prompts: &mut impl ConnectPrompts) -> Result<String> {
        let app = &self.opts.app;
        let email = match &self.opts.email {
            Some(email) => email.clone(),
            None => prompts.email().await?,
        };
        let result = rpc
            .call(
                "user.requestVerification",
                json!({ "email": email, "appInfo": { "appName": app.name } }),
            )
            .await?;
        let res: VerificationRequestResult = serde_json::from_value(result)
            .map_err(|e| anyhow!("bad user.requestVerification reply: {e}"))?;
        if !res.success {
            return Err(ConnectError::Refused {
                message: res.message,
            }
            .into());
        }

        if res.requires_password {
            // New accounts (`createAccount` → `user.create` then `user.login`)
            // and existing users when no SMTP is configured both sign in by
            // password.
            let password = prompts.password(&email, !res.is_existing_user).await?;
            if !res.is_existing_user {
                let result = rpc
                    .call(
                        "user.create",
                        json!({ "email": email, "password": password }),
                    )
                    .await?;
                let created: UserCreationResult = serde_json::from_value(result)
                    .map_err(|e| anyhow!("bad user.create reply: {e}"))?;
                if !created.success {
                    return Err(ConnectError::Refused {
                        message: created
                            .error
                            .unwrap_or_else(|| "account creation failed".to_string()),
                    }
                    .into());
                }
            }
            let result = rpc
                .call(
                    "user.login",
                    json!({
                        "email": email,
                        "password": password,
                        "appName": app.name,
                    }),
                )
                .await?;
            return Ok(result
                .as_str()
                .ok_or_else(|| anyhow!("user.login did not answer a string"))?
                .to_string());
        }

        prompts.status(ConnectStatus::WaitingForEmailCode {
            email: email.clone(),
        });
        loop {
            let code = prompts.email_code(&email).await?;
            match rpc
                .call(
                    "user.verifyEmail",
                    json!({
                        "email": email,
                        "code": code,
                        "verificationType": "login",
                        "appName": app.name,
                    }),
                )
                .await
            {
                Ok(v) => {
                    return Ok(v
                        .as_str()
                        .ok_or_else(|| anyhow!("user.verifyEmail did not answer a string"))?
                        .to_string())
                }
                Err(_) => prompts.status(ConnectStatus::CodeRejected),
            }
        }
    }
}

// ── Error classification (TS `saysLocked` / `refusesToken`) ──

fn error_text(err: &anyhow::Error) -> String {
    err.downcast_ref::<RpcCallError>()
        .map(|e| e.message.clone())
        .unwrap_or_else(|| err.to_string())
}

/// Whether `err` says the executor's wallet is locked: a state of the
/// executor, so the token stays.
fn says_locked(err: &anyhow::Error) -> bool {
    let text = error_text(err);
    text == LOCKED_WALLET || text.starts_with(EXECUTOR_LOCKED)
}

/// Whether the executor refused the token itself (TS `refusesToken`): 401
/// when it opens no session, 403 only when the reason names a dead token.
fn refuses_token(err: &anyhow::Error) -> bool {
    match err.downcast_ref::<RpcCallError>() {
        Some(e) => {
            e.code == 401 || (e.code == 403 && DEAD_TOKEN.iter().any(|s| e.message.contains(s)))
        }
        None => false,
    }
}

/// Whether `agent.generateJwt` says the request is gone: expired, or five
/// wrong codes. "Can't find permitted request" is not here — that is a wrong
/// code (or an unapproved request), which re-asks.
fn request_gone(err: &anyhow::Error) -> bool {
    error_text(err).contains("request access again")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::VecDeque;
    use std::path::Path;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Mutex;

    // ── Scripted socket + io ──

    /// (token, method, params) → reply — the executor, scripted.
    type Handler = Arc<dyn Fn(&str, &str, &Value) -> Result<Value> + Send + Sync>;
    /// base_url → health verdict.
    type HealthFn = Arc<dyn Fn(&str) -> Result<()> + Send + Sync>;
    type AuthCb = Arc<dyn Fn(&str) + Send + Sync>;

    struct FakeShared {
        /// The token this socket was opened with — the handler dispatches on it.
        token: String,
        calls: Mutex<Vec<(String, Value)>>,
        handler: Handler,
        auth_failed: AtomicBool,
        auth_cbs: Mutex<Vec<AuthCb>>,
        closed: AtomicBool,
    }

    #[derive(Clone)]
    struct FakeRpc(Arc<FakeShared>);

    impl FakeRpc {
        /// Simulate the executor closing the socket with 1008.
        fn fail_auth(&self, reason: &str) {
            self.0.auth_failed.store(true, Ordering::SeqCst);
            for cb in self.0.auth_cbs.lock().unwrap().iter() {
                cb(reason);
            }
        }
    }

    impl Rpc for FakeRpc {
        fn call<'a>(
            &'a self,
            method: &'a str,
            params: Value,
        ) -> impl Future<Output = Result<Value>> + Send + 'a {
            self.0
                .calls
                .lock()
                .unwrap()
                .push((method.to_string(), params.clone()));
            let res = (self.0.handler)(&self.0.token, method, &params);
            async move { res }
        }
        fn auth_failed(&self) -> bool {
            self.0.auth_failed.load(Ordering::SeqCst)
        }
        fn on_auth_failed<'a>(&'a self, cb: AuthCb) -> impl Future<Output = u64> + Send + 'a {
            self.0.auth_cbs.lock().unwrap().push(cb);
            async { 0 }
        }
        fn close<'a>(&'a self) -> impl Future<Output = ()> + Send + 'a {
            self.0.closed.store(true, Ordering::SeqCst);
            async {}
        }
    }

    struct FakeIo {
        healthy: HealthFn,
        handler: Handler,
        /// (base_url, token) of every socket the flow opened, in order.
        opened: Arc<Mutex<Vec<(String, String)>>>,
        rpcs: Arc<Mutex<Vec<FakeRpc>>>,
    }

    impl FakeIo {
        fn new(
            handler: impl Fn(&str, &str, &Value) -> Result<Value> + Send + Sync + 'static,
        ) -> Self {
            Self {
                healthy: Arc::new(|_| Ok(())),
                handler: Arc::new(handler),
                opened: Arc::new(Mutex::new(vec![])),
                rpcs: Arc::new(Mutex::new(vec![])),
            }
        }
        fn unhealthy() -> Self {
            let mut io = Self::new(|_, _, _| Ok(Value::Null));
            io.healthy = Arc::new(|_| Err(anyhow!("connection refused")));
            io
        }
        fn opened(&self) -> Vec<(String, String)> {
            self.opened.lock().unwrap().clone()
        }
        fn last_rpc(&self) -> FakeRpc {
            self.rpcs.lock().unwrap().last().unwrap().clone()
        }
    }

    impl ExecutorIo for FakeIo {
        type Rpc = FakeRpc;
        fn healthy<'a>(
            &'a self,
            base_url: &'a str,
        ) -> impl Future<Output = Result<()>> + Send + 'a {
            let res = (self.healthy)(base_url);
            async move { res }
        }
        fn rpc<'a>(
            &'a self,
            base_url: &'a str,
            token: &'a str,
        ) -> impl Future<Output = Result<FakeRpc>> + Send + 'a {
            self.opened
                .lock()
                .unwrap()
                .push((base_url.to_string(), token.to_string()));
            let rpc = FakeRpc(Arc::new(FakeShared {
                token: token.to_string(),
                calls: Mutex::new(vec![]),
                handler: self.handler.clone(),
                auth_failed: AtomicBool::new(false),
                auth_cbs: Mutex::new(vec![]),
                closed: AtomicBool::new(false),
            }));
            self.rpcs.lock().unwrap().push(rpc.clone());
            async move { Ok(rpc) }
        }
    }

    // ── Scripted prompts ──

    #[derive(Default)]
    struct ScriptedPrompts {
        cap_codes: VecDeque<String>,
        emails: VecDeque<String>,
        email_codes: VecDeque<String>,
        passwords: VecDeque<String>,
        asked: Vec<String>,
        statuses: Vec<ConnectStatus>,
    }

    impl ConnectPrompts for ScriptedPrompts {
        fn capability_code<'a>(
            &'a mut self,
            request_id: &'a str,
            _app: &'a AppInfo,
        ) -> impl Future<Output = Result<String>> + Send + 'a {
            self.asked.push(format!("capability_code:{request_id}"));
            let next = self.cap_codes.pop_front();
            async move { next.ok_or_else(|| anyhow!("no scripted capability code")) }
        }
        fn email<'a>(&'a mut self) -> impl Future<Output = Result<String>> + Send + 'a {
            self.asked.push("email".to_string());
            let next = self.emails.pop_front();
            async move { next.ok_or_else(|| anyhow!("no scripted email")) }
        }
        fn email_code<'a>(
            &'a mut self,
            email: &'a str,
        ) -> impl Future<Output = Result<String>> + Send + 'a {
            self.asked.push(format!("email_code:{email}"));
            let next = self.email_codes.pop_front();
            async move { next.ok_or_else(|| anyhow!("no scripted email code")) }
        }
        fn password<'a>(
            &'a mut self,
            email: &'a str,
            new_account: bool,
        ) -> impl Future<Output = Result<String>> + Send + 'a {
            self.asked.push(format!("password:{email}:{new_account}"));
            let next = self.passwords.pop_front();
            async move { next.ok_or_else(|| anyhow!("no scripted password")) }
        }
        fn status(&mut self, status: ConnectStatus) {
            self.statuses.push(status);
        }
    }

    /// A prompt that must never be asked (stored-token and operator paths).
    struct NoPrompts;
    impl ConnectPrompts for NoPrompts {
        async fn capability_code<'a>(
            &'a mut self,
            _request_id: &'a str,
            _app: &'a AppInfo,
        ) -> Result<String> {
            panic!("no prompt expected")
        }
        async fn email(&mut self) -> Result<String> {
            panic!("no prompt expected")
        }
        async fn email_code<'a>(&'a mut self, _email: &'a str) -> Result<String> {
            panic!("no prompt expected")
        }
        async fn password<'a>(&'a mut self, _email: &'a str, _new_account: bool) -> Result<String> {
            panic!("no prompt expected")
        }
    }

    // ── Helpers ──

    const URL: &str = "http://127.0.0.1:14200";

    fn opts() -> ConnectOptions {
        let mut opts = ConnectOptions::new(AppInfo::new("test-app", "tests", "https://app"));
        opts.url = Some(URL.to_string());
        opts.use_stored_url = false;
        opts
    }

    fn session(dir: &Path, io: FakeIo, mut opts: ConnectOptions) -> Ad4mConnect<FakeIo> {
        opts.token_dir = Some(dir.to_path_buf());
        Ad4mConnect::with_io(opts, io).unwrap()
    }

    fn authed_status() -> Value {
        json!({
            "did": "did:key:main",
            "didDocument": null,
            "error": null,
            "isInitialized": true,
            "isUnlocked": true,
        })
    }

    fn rpc_err(code: i32, message: &str) -> anyhow::Error {
        anyhow::Error::new(RpcCallError {
            code,
            message: message.to_string(),
            data: None,
        })
    }

    /// Anonymous calls fail `agent.status` 403 (executor has an admin
    /// credential); the minted `jwt` succeeds and any other token is refused
    /// — the single-user script.
    fn single_user_handler(jwt: &'static str) -> impl Fn(&str, &str, &Value) -> Result<Value> {
        move |token, method, _params| match (token, method) {
            ("", "agent.status") => Err(rpc_err(403, "missing capability for method")),
            (t, "agent.status") if t == jwt => Ok(authed_status()),
            (_, "agent.status") => Err(rpc_err(401, "Unauthorized access")),
            (_, "agent.isLocked") => Ok(Value::Bool(false)),
            (_, "user.multiUserEnabled") => Ok(Value::Bool(false)),
            (_, "agent.requestCapability") => Ok(Value::String("req-1".to_string())),
            (_, "agent.generateJwt") => Ok(Value::String(jwt.to_string())),
            (_, other) => Err(rpc_err(404, &format!("unknown method {other}"))),
        }
    }

    // ── Tests ──

    #[test]
    fn candidates_run_in_order() {
        let dir = tempfile::tempdir().unwrap();
        let port_file = dir.path().join("executor-port");
        std::fs::write(&port_file, "14444").unwrap();
        let store_dir = dir.path().join("tokens");
        let store = TokenStore::new(&store_dir);
        store.set_last_url("ws://stored.example:9000/").unwrap();

        let mut o = opts();
        o.url = Some("http://explicit:1/".to_string());
        o.port = Some(2000);
        o.use_stored_url = true;
        o.executor_port_file = Some(port_file);
        o.token_dir = Some(store_dir);
        let s: Ad4mConnect<FakeIo> = Ad4mConnect::with_io(o, FakeIo::unhealthy()).unwrap();
        assert_eq!(
            s.candidates(),
            vec![
                "http://explicit:1".to_string(),          // normalized (trailing /)
                "http://127.0.0.1:2000".to_string(),      // explicit port
                "http://stored.example:9000".to_string(), // stored, ws→http
                "http://127.0.0.1:14444".to_string(),     // executor-port file
                "http://127.0.0.1:12000".to_string(),     // default
            ]
        );
    }

    #[tokio::test]
    async fn executor_not_found_reports_every_candidate() {
        let dir = tempfile::tempdir().unwrap();
        let mut s = session(dir.path(), FakeIo::unhealthy(), opts());
        let err = s.connect(&mut NoPrompts).await.unwrap_err();
        let not_found = err.downcast_ref::<ConnectError>().expect("ConnectError");
        match not_found {
            ConnectError::ExecutorNotFound { tried } => {
                assert!(tried.contains(&URL.to_string()), "{tried:?}");
            }
            other => panic!("expected ExecutorNotFound, got {other:?}"),
        }
        assert_eq!(s.state(), AuthState::Unauthenticated);
    }

    #[tokio::test]
    async fn a_stored_token_authenticates_without_prompts() {
        let dir = tempfile::tempdir().unwrap();
        TokenStore::new(dir.path())
            .set_token(URL, "test-app", "jwt-stored")
            .unwrap();
        let io = FakeIo::new(|_token, method, _| match method {
            "agent.status" => Ok(authed_status()),
            other => Err(rpc_err(404, &format!("unexpected {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        let state = s.connect(&mut NoPrompts).await.unwrap();
        assert_eq!(state, AuthState::Authenticated);
        assert_eq!(
            s.io.opened(),
            vec![(URL.to_string(), "jwt-stored".to_string())]
        );
    }

    #[tokio::test]
    async fn a_refused_stored_token_falls_back_to_the_handshake() {
        let dir = tempfile::tempdir().unwrap();
        TokenStore::new(dir.path())
            .set_token(URL, "test-app", "dead-jwt")
            .unwrap();
        let io = FakeIo::new(single_user_handler("jwt-fresh"));
        let mut s = session(dir.path(), io, opts());
        let mut prompts = ScriptedPrompts {
            cap_codes: ["123456".into()].into(),
            ..Default::default()
        };
        let state = s.connect(&mut prompts).await.unwrap();
        assert_eq!(state, AuthState::Authenticated);

        // The stored token is gone; the minted one is stored in its place.
        let store = TokenStore::new(dir.path());
        assert_eq!(store.token(URL, "test-app").as_deref(), Some("jwt-fresh"));
        assert_eq!(
            s.io.opened(),
            vec![
                (URL.to_string(), "dead-jwt".to_string()),
                (URL.to_string(), "".to_string()),
                (URL.to_string(), "jwt-fresh".to_string()),
            ]
        );
        assert_eq!(prompts.asked, vec!["capability_code:req-1".to_string()]);
    }

    #[tokio::test]
    async fn a_403_naming_a_dead_token_also_falls_back() {
        let dir = tempfile::tempdir().unwrap();
        TokenStore::new(dir.path())
            .set_token(URL, "test-app", "expired-jwt")
            .unwrap();
        let io = FakeIo::new(|token, method, _| match (token, method) {
            ("expired-jwt", "agent.status") => {
                Err(rpc_err(403, "ExpiredSignature: the token has expired"))
            }
            (_, "agent.status") if token.is_empty() => {
                Err(rpc_err(403, "missing capability for method"))
            }
            (_, "agent.status") => Ok(authed_status()),
            (_, "agent.isLocked") => Ok(Value::Bool(false)),
            (_, "user.multiUserEnabled") => Ok(Value::Bool(false)),
            (_, "agent.requestCapability") => Ok(Value::String("req-9".to_string())),
            (_, "agent.generateJwt") => Ok(Value::String("jwt-2".to_string())),
            (_, other) => Err(rpc_err(404, &format!("unknown {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        let mut prompts = ScriptedPrompts {
            cap_codes: ["111111".into()].into(),
            ..Default::default()
        };
        assert_eq!(
            s.connect(&mut prompts).await.unwrap(),
            AuthState::Authenticated
        );
        assert_eq!(
            TokenStore::new(dir.path())
                .token(URL, "test-app")
                .as_deref(),
            Some("jwt-2")
        );
    }

    #[tokio::test]
    async fn an_anonymous_operator_needs_no_token() {
        // `--insecure-no-admin-credential`: the loopback anonymous session is
        // the operator, `agent.status` just answers.
        let dir = tempfile::tempdir().unwrap();
        let io = FakeIo::new(|token, method, _| match (token, method) {
            ("", "agent.status") => Ok(authed_status()),
            (_, other) => Err(rpc_err(404, &format!("unexpected {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        assert_eq!(
            s.connect(&mut NoPrompts).await.unwrap(),
            AuthState::Authenticated
        );
        assert_eq!(s.io.opened(), vec![(URL.to_string(), "".to_string())]);
    }

    #[tokio::test]
    async fn a_locked_executor_reports_locked() {
        let dir = tempfile::tempdir().unwrap();
        let io = FakeIo::new(|token, method, _| match (token, method) {
            ("", "agent.status") => Ok(json!({
                "did": null, "didDocument": null, "error": null,
                "isInitialized": false, "isUnlocked": false,
            })),
            (_, other) => Err(rpc_err(404, &format!("unexpected {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        assert_eq!(s.connect(&mut NoPrompts).await.unwrap(), AuthState::Locked);
    }

    #[tokio::test]
    async fn a_wrong_code_rejects_and_reasks() {
        let dir = tempfile::tempdir().unwrap();
        let attempts = Arc::new(Mutex::new(0));
        let attempts2 = attempts.clone();
        let io = FakeIo::new(move |token, method, params| match (token, method) {
            (_, "agent.status") if token.is_empty() => Err(rpc_err(403, "missing capability")),
            (_, "agent.status") => Ok(authed_status()),
            (_, "agent.isLocked") => Ok(Value::Bool(false)),
            (_, "user.multiUserEnabled") => Ok(Value::Bool(false)),
            (_, "agent.requestCapability") => Ok(Value::String("req-1".to_string())),
            (_, "agent.generateJwt") => {
                *attempts2.lock().unwrap() += 1;
                if params["rand"] == "123456" {
                    Ok(Value::String("jwt-ok".to_string()))
                } else {
                    Err(rpc_err(500, "Can't find permitted request"))
                }
            }
            (_, other) => Err(rpc_err(404, &format!("unknown {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        let mut prompts = ScriptedPrompts {
            cap_codes: ["000000".into(), "123456".into()].into(),
            ..Default::default()
        };
        assert_eq!(
            s.connect(&mut prompts).await.unwrap(),
            AuthState::Authenticated
        );
        assert_eq!(*attempts.lock().unwrap(), 2);
        assert!(prompts.statuses.contains(&ConnectStatus::CodeRejected));
    }

    #[tokio::test]
    async fn a_dead_request_is_requested_again() {
        let dir = tempfile::tempdir().unwrap();
        let requests = Arc::new(Mutex::new(0));
        let requests2 = requests.clone();
        let io = FakeIo::new(move |token, method, _params| match (token, method) {
            (_, "agent.status") if token.is_empty() => Err(rpc_err(403, "missing capability")),
            (_, "agent.status") => Ok(authed_status()),
            (_, "agent.isLocked") => Ok(Value::Bool(false)),
            (_, "user.multiUserEnabled") => Ok(Value::Bool(false)),
            (_, "agent.requestCapability") => {
                let mut n = requests2.lock().unwrap();
                *n += 1;
                Ok(Value::String(format!("req-{}", *n)))
            }
            (_, "agent.generateJwt") => {
                if *requests2.lock().unwrap() == 1 {
                    Err(rpc_err(500, "This request expired; request access again"))
                } else {
                    Ok(Value::String("jwt-3".to_string()))
                }
            }
            (_, other) => Err(rpc_err(404, &format!("unknown {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        let mut prompts = ScriptedPrompts {
            cap_codes: ["111111".into(), "222222".into()].into(),
            ..Default::default()
        };
        assert_eq!(
            s.connect(&mut prompts).await.unwrap(),
            AuthState::Authenticated
        );
        assert_eq!(*requests.lock().unwrap(), 2);
        assert_eq!(
            prompts.asked,
            vec![
                "capability_code:req-1".to_string(),
                "capability_code:req-2".to_string()
            ]
        );
        assert!(prompts.statuses.contains(&ConnectStatus::RequestExpired));
    }

    #[tokio::test]
    async fn multi_user_email_code_logs_in() {
        let dir = tempfile::tempdir().unwrap();
        let mut o = opts();
        o.email = Some("ada@example.com".to_string());
        let io = FakeIo::new(move |token, method, params| match (token, method) {
            (_, "agent.status") if token.is_empty() => Err(rpc_err(403, "missing capability")),
            (_, "agent.status") => Ok(authed_status()),
            (_, "agent.isLocked") => Ok(Value::Bool(false)),
            (_, "user.multiUserEnabled") => Ok(Value::Bool(true)),
            (_, "user.requestVerification") => {
                assert_eq!(params["email"], "ada@example.com");
                Ok(json!({
                    "success": true,
                    "message": "Verification code sent.",
                    "requiresPassword": false,
                    "isExistingUser": true,
                }))
            }
            (_, "user.verifyEmail") => {
                assert_eq!(params["code"], "654321");
                assert_eq!(params["verificationType"], "login");
                Ok(Value::String("jwt-mu".to_string()))
            }
            (_, other) => Err(rpc_err(404, &format!("unknown {other}"))),
        });
        let mut s = session(dir.path(), io, o);
        let mut prompts = ScriptedPrompts {
            email_codes: ["654321".into()].into(),
            ..Default::default()
        };
        assert_eq!(
            s.connect(&mut prompts).await.unwrap(),
            AuthState::Authenticated
        );
        assert_eq!(
            TokenStore::new(dir.path())
                .token(URL, "test-app")
                .as_deref(),
            Some("jwt-mu")
        );
        assert!(prompts
            .statuses
            .iter()
            .any(|st| matches!(st, ConnectStatus::WaitingForEmailCode { email } if email == "ada@example.com")));
    }

    #[tokio::test]
    async fn multi_user_existing_user_logs_in_by_password() {
        let dir = tempfile::tempdir().unwrap();
        let io = FakeIo::new(move |token, method, params| match (token, method) {
            (_, "agent.status") if token.is_empty() => Err(rpc_err(403, "missing capability")),
            (_, "agent.status") => Ok(authed_status()),
            (_, "agent.isLocked") => Ok(Value::Bool(false)),
            (_, "user.multiUserEnabled") => Ok(Value::Bool(true)),
            (_, "user.requestVerification") => Ok(json!({
                "success": true,
                "message": "Email not configured. Please log in with your password.",
                "requiresPassword": true,
                "isExistingUser": true,
            })),
            (_, "user.login") => {
                assert_eq!(params["email"], "bob@example.com");
                assert_eq!(params["password"], "hunter2");
                Ok(Value::String("jwt-pw".to_string()))
            }
            (_, other) => Err(rpc_err(404, &format!("unknown {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        let mut prompts = ScriptedPrompts {
            emails: ["bob@example.com".into()].into(),
            passwords: ["hunter2".into()].into(),
            ..Default::default()
        };
        assert_eq!(
            s.connect(&mut prompts).await.unwrap(),
            AuthState::Authenticated
        );
        assert_eq!(
            prompts.asked,
            vec![
                "email".to_string(),
                "password:bob@example.com:false".to_string()
            ]
        );
    }

    #[tokio::test]
    async fn multi_user_new_account_creates_then_logs_in() {
        let dir = tempfile::tempdir().unwrap();
        let io = FakeIo::new(move |token, method, params| match (token, method) {
            (_, "agent.status") if token.is_empty() => Err(rpc_err(403, "missing capability")),
            (_, "agent.status") => Ok(authed_status()),
            (_, "agent.isLocked") => Ok(Value::Bool(false)),
            (_, "user.multiUserEnabled") => Ok(Value::Bool(true)),
            (_, "user.requestVerification") => Ok(json!({
                "success": true,
                "message": "No account found yet. Provide a password to create one.",
                "requiresPassword": true,
                "isExistingUser": false,
            })),
            (_, "user.create") => Ok(json!({
                "did": "did:key:new-user", "success": true, "error": null,
            })),
            (_, "user.login") => {
                assert_eq!(params["password"], "newpass");
                Ok(Value::String("jwt-new".to_string()))
            }
            (_, other) => Err(rpc_err(404, &format!("unknown {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        let mut prompts = ScriptedPrompts {
            emails: ["new@example.com".into()].into(),
            passwords: ["newpass".into()].into(),
            ..Default::default()
        };
        assert_eq!(
            s.connect(&mut prompts).await.unwrap(),
            AuthState::Authenticated
        );
        assert!(prompts
            .asked
            .contains(&"password:new@example.com:true".to_string()));
    }

    #[tokio::test]
    async fn a_mid_session_refusal_drops_the_token() {
        let dir = tempfile::tempdir().unwrap();
        TokenStore::new(dir.path())
            .set_token(URL, "test-app", "jwt-stored")
            .unwrap();
        let io = FakeIo::new(|_token, method, _| match method {
            "agent.status" => Ok(authed_status()),
            other => Err(rpc_err(404, &format!("unexpected {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        assert_eq!(
            s.connect(&mut NoPrompts).await.unwrap(),
            AuthState::Authenticated
        );
        let mut watcher = s.watch_state();

        s.io.last_rpc().fail_auth("Unauthorized access");
        watcher.changed().await.unwrap();
        assert_eq!(s.state(), AuthState::Unauthenticated);
        assert_eq!(TokenStore::new(dir.path()).token(URL, "test-app"), None);
    }

    /// F2: the auth state is kept with no receiver alive. `connect` with
    /// nobody watching, then `state()` and a fresh `watch_state()` must say
    /// Authenticated; a refusal after the last receiver dropped must land too.
    #[tokio::test]
    async fn the_auth_state_is_retained_without_a_watcher() {
        let dir = tempfile::tempdir().unwrap();
        TokenStore::new(dir.path())
            .set_token(URL, "test-app", "jwt-stored")
            .unwrap();
        let io = FakeIo::new(|_token, method, _| match method {
            "agent.status" => Ok(authed_status()),
            other => Err(rpc_err(404, &format!("unexpected {other}"))),
        });
        let mut s = session(dir.path(), io, opts());
        s.connect(&mut NoPrompts).await.unwrap();
        assert_eq!(s.state(), AuthState::Authenticated);
        assert_eq!(*s.watch_state().borrow(), AuthState::Authenticated);

        // Every receiver dropped (the one above is a temporary), then the
        // executor refuses the token.
        s.io.last_rpc().fail_auth("Unauthorized access");
        assert_eq!(s.state(), AuthState::Unauthenticated);
    }

    #[test]
    fn token_store_keeps_tokens_per_url_and_app_with_0600() {
        let dir = tempfile::tempdir().unwrap();
        let store = TokenStore::new(dir.path());
        store.set_token("http://a:1", "app1", "tok-a1").unwrap();
        store.set_token("http://a:1", "app2", "tok-a2").unwrap();
        store.set_token("http://b:2", "app1", "tok-b1").unwrap();
        store.set_last_url("http://b:2").unwrap();

        // A second store on the same dir reads the same tokens.
        let store2 = TokenStore::new(dir.path());
        assert_eq!(
            store2.token("http://a:1", "app1").as_deref(),
            Some("tok-a1")
        );
        assert_eq!(
            store2.token("http://a:1", "app2").as_deref(),
            Some("tok-a2")
        );
        assert_eq!(
            store2.token("http://b:2", "app1").as_deref(),
            Some("tok-b1")
        );
        assert_eq!(store2.last_url().as_deref(), Some("http://b:2"));

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(store2.path())
                .unwrap()
                .permissions()
                .mode();
            assert_eq!(mode & 0o777, 0o600, "token file mode {mode:o}");
        }

        store2.remove_token("http://a:1", "app1").unwrap();
        assert_eq!(store2.token("http://a:1", "app1"), None);
        assert_eq!(
            store2.token("http://a:1", "app2").as_deref(),
            Some("tok-a2")
        );
    }

    #[test]
    fn normalize_base_url_converts_ws_and_trims() {
        assert_eq!(
            normalize_base_url("ws://localhost:12000/"),
            "http://localhost:12000"
        );
        assert_eq!(
            normalize_base_url("wss://host.example/"),
            "https://host.example"
        );
        assert_eq!(normalize_base_url("http://a/"), "http://a");
    }
}

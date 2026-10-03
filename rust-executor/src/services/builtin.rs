//! The contract every service implementation satisfies.
//!
//! Only `builtin` implementations exist so far: Rust types compiled into
//! the executor. The wasm / js / native runtimes (Phases 4–5) will adapt to
//! the same trait.

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use async_trait::async_trait;
use serde::Serialize;
use serde_json::Value;
use ts_rs::TS;

use crate::agent::capabilities::Capability;

/// Who made a call.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Caller {
    /// An app over WS RPC or MCP.
    App,
    /// Executor code acting on its own (boot, background tasks), by module.
    Executor { module: String },
    /// A language, by address.
    Language { address: String },
    /// A service, by implementation hash.
    Service { implementation: String },
    /// An AI harness thread.
    Harness { thread: String },
}

/// The context of one dispatch.
#[derive(Debug, Clone)]
pub struct CallContext {
    pub caller: Caller,
    /// The callers before this one, outermost first. Empty for a direct call.
    pub origin: Vec<Caller>,
    /// The agent the call acts for.
    pub agent_did: Option<String>,
    /// Multi-user account id: the session's user.
    pub user: Option<String>,
    /// The caller's auth token, for implementations that scope per session
    /// (billing, transcription streams). `None` for calls the host makes itself.
    pub auth_token: Option<String>,
    /// The call came with the executor's admin credential. Some operations
    /// (host rates, Holochain restart) need it beyond any grant.
    pub is_admin: bool,
    /// Grant layers. A call needs an action allowed by **every** layer: the
    /// original caller's grants, then each intermediate service's own grants.
    /// This is the intersection rule that prevents a confused deputy.
    pub grants: Vec<Vec<Capability>>,
    pub deadline: Option<Instant>,
}

impl CallContext {
    /// A call the executor makes on its own behalf (a background task, a
    /// language, boot), for `user` / `agent_did` when it acts for one. It
    /// holds the executor's grants; calls made while serving a request
    /// should use that request's context instead, so its grants apply.
    pub fn system(caller: Caller, user: Option<String>, agent_did: Option<String>) -> Self {
        CallContext {
            caller,
            origin: Vec::new(),
            agent_did,
            user,
            auth_token: None,
            is_admin: true,
            grants: vec![vec![
                crate::agent::capabilities::defs::ALL_CAPABILITY.clone()
            ]],
            deadline: None,
        }
    }
}

/// A failure an implementation reports.
#[derive(Debug, Clone, PartialEq)]
pub enum ServiceError {
    /// A method error the interface declares (`errors`), by name.
    Method {
        name: String,
        data: Option<Value>,
        message: String,
    },
    /// The service cannot serve right now (→ 503).
    Unavailable(String),
    /// The caller may not do this, whatever its grants say (→ 403).
    Forbidden(String),
    /// Anything else (→ 500).
    Internal(String),
}

impl ServiceError {
    pub fn method(name: impl Into<String>, message: impl Into<String>) -> Self {
        ServiceError::Method {
            name: name.into(),
            data: None,
            message: message.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, serde::Deserialize, TS)]
#[serde(tag = "state", content = "reason", rename_all = "camelCase")]
pub enum ServiceHealth {
    Starting,
    Running,
    Degraded(String),
    Stopped,
    Failed(String),
}

/// Sends a service's events into the executor event stream.
#[derive(Clone)]
pub struct EventEmitter {
    pub(crate) host: Arc<super::host::ServiceHost>,
    pub(crate) implementation: String,
}

/// Whose sockets a service event goes to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EventOwner {
    /// The sockets of one agent DID.
    Agent(String),
    /// The sockets of one multi-user account (by email).
    User(String),
    /// Every socket.
    All,
    /// No user's sockets: admin sockets and the executor's own consumers.
    Executor,
}

impl EventEmitter {
    /// Emit `event` (as the interface names it) to `owner`. The host checks
    /// the payload against the interface and delivers it only to the owner's
    /// sockets that hold the event's action (admins see every owner's).
    pub async fn emit(&self, event: &str, owner: EventOwner, payload: Value) -> Result<(), String> {
        self.host
            .emit_event(&self.implementation, event, owner, payload)
            .await
    }
}

/// Lets a service call other services through the host with the context
/// grant-layer rule of [`CallContext::grants`].
#[derive(Clone)]
pub struct ServiceCaller {
    pub(crate) host: Arc<super::host::ServiceHost>,
    pub(crate) implementation: String,
    pub(crate) grants: Arc<Vec<Capability>>,
}

impl ServiceCaller {
    /// Call `<hash>.<method>` while handling `outer`. The nested call keeps
    /// the user, records this service in `origin`, and adds this service's
    /// own grants as a layer.
    pub async fn call(
        &self,
        outer: &CallContext,
        method: &str,
        params: Value,
    ) -> Result<Value, crate::api::ws_handler::WsRpcError> {
        let mut ctx = outer.clone();
        ctx.origin.push(ctx.caller.clone());
        ctx.caller = Caller::Service {
            implementation: self.implementation.clone(),
        };
        ctx.grants.push(self.grants.as_ref().clone());
        self.host.dispatch(method, params, ctx).await
    }
}

/// What `start` receives.
pub struct StartContext {
    pub config: Value,
    pub data_dir: PathBuf,
    pub events: EventEmitter,
    pub services: ServiceCaller,
}

#[async_trait]
pub trait ServiceImplementation: Send + Sync {
    async fn start(&self, ctx: StartContext) -> Result<(), String>;
    async fn stop(&self) -> Result<(), String>;
    async fn health(&self) -> ServiceHealth;
    async fn migrate(&self, _from_version: &str) -> Result<(), String> {
        Ok(())
    }
    /// Handle one method of any implemented interface. The host has already
    /// resolved, authorised and validated the call.
    async fn call(
        &self,
        method: &str,
        params: Value,
        ctx: CallContext,
    ) -> Result<Value, ServiceError>;
}

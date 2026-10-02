//! The contract every service implementation satisfies (SPEC §7, §9.3).
//!
//! Phase 0 runs `builtin` implementations only: Rust types compiled into
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
    /// A language, by address.
    Language { address: String },
    /// A service, by implementation hash.
    Service { implementation: String },
    /// An AI harness thread.
    Harness { thread: String },
}

/// The context of one dispatch (SPEC §9.3).
#[derive(Debug, Clone)]
pub struct CallContext {
    pub caller: Caller,
    /// The callers before this one, outermost first. Empty for a direct call.
    pub origin: Vec<Caller>,
    /// The agent the call acts for.
    pub agent_did: Option<String>,
    /// Multi-user account id: the session's user.
    pub user: Option<String>,
    /// Grant layers. A call needs an action allowed by **every** layer: the
    /// original caller's grants, then each intermediate service's own grants.
    /// This is the intersection rule that prevents a confused deputy.
    pub grants: Vec<Vec<Capability>>,
    pub deadline: Option<Instant>,
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

impl EventEmitter {
    /// Emit `event` (as the interface names it) for the agent `owner`. The
    /// host checks the payload against the interface and delivers it only to
    /// that agent's sockets that hold the event's action.
    pub async fn emit(&self, event: &str, owner: &str, payload: Value) -> Result<(), String> {
        self.host
            .emit_event(&self.implementation, event, owner, payload)
            .await
    }
}

/// Lets a service call other services through the host with the context
/// rules of SPEC §9.3.
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

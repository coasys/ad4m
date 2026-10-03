//! WebSocket RPC handler infrastructure.
//!
//! Provides the types and registration mechanism for WS-native handlers.
//! Each domain module (agent, perspectives, runtime, etc.) registers its
//! handlers via `register_ws_handlers(&mut HandlerMap)` — no central route map.

use serde::de::DeserializeOwned;
use serde_json::Value;
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use crate::types::RequestContext;

// ── Error type ──────────────────────────────────────────────────────────────

/// Error returned by WS RPC handlers.
/// Maps directly to the `{ "error": { "code": N, "message": "..." } }` wire format.
#[derive(Debug)]
pub struct WsRpcError {
    pub code: u16,
    pub message: String,
    /// Typed error detail (service method errors carry `{ name, … }`).
    pub data: Option<Value>,
}

impl WsRpcError {
    pub fn new(code: u16, msg: impl Into<String>) -> Self {
        Self {
            code,
            message: msg.into(),
            data: None,
        }
    }
    pub fn with_data(mut self, data: Value) -> Self {
        self.data = Some(data);
        self
    }
    /// The wire form: `{ "code", "message", "data"? }`.
    pub fn to_json(&self) -> Value {
        let mut e = serde_json::json!({ "code": self.code, "message": self.message });
        if let Some(d) = &self.data {
            e["data"] = d.clone();
        }
        e
    }
    pub fn bad_request(msg: impl Into<String>) -> Self {
        Self {
            code: 400,
            message: msg.into(),
            data: None,
        }
    }
    pub fn unauthorized(msg: impl Into<String>) -> Self {
        Self {
            code: 401,
            message: msg.into(),
            data: None,
        }
    }
    pub fn forbidden(msg: impl Into<String>) -> Self {
        Self {
            code: 403,
            message: msg.into(),
            data: None,
        }
    }
    pub fn not_found(msg: impl Into<String>) -> Self {
        Self {
            code: 404,
            message: msg.into(),
            data: None,
        }
    }
    pub fn internal(msg: impl Into<String>) -> Self {
        Self {
            code: 500,
            message: msg.into(),
            data: None,
        }
    }
    pub fn not_implemented(msg: impl Into<String>) -> Self {
        Self {
            code: 501,
            message: msg.into(),
            data: None,
        }
    }
}

impl std::fmt::Display for WsRpcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WsRpcError({}): {}", self.code, self.message)
    }
}

impl std::error::Error for WsRpcError {}

impl From<serde_json::Error> for WsRpcError {
    fn from(e: serde_json::Error) -> Self {
        Self::bad_request(format!("JSON error: {}", e))
    }
}

// ── Handler type ────────────────────────────────────────────────────────────

/// A boxed async handler function.
///
/// Takes `(params: Value, ctx: Arc<RequestContext>)` and returns `Result<Value, WsRpcError>`.
pub type WsHandler = Box<
    dyn Fn(
            Value,
            Arc<RequestContext>,
        ) -> Pin<Box<dyn Future<Output = Result<Value, WsRpcError>> + Send>>
        + Send
        + Sync,
>;

/// An async handler body: `(params, ctx) -> Result<Value, WsRpcError>`.
pub trait WsFn: Send + Sync + 'static {
    fn call(
        &self,
        params: Value,
        ctx: Arc<RequestContext>,
    ) -> Pin<Box<dyn Future<Output = Result<Value, WsRpcError>> + Send>>;
}

impl<F, Fut> WsFn for F
where
    F: Fn(Value, Arc<RequestContext>) -> Fut + Send + Sync + 'static,
    Fut: Future<Output = Result<Value, WsRpcError>> + Send + 'static,
{
    fn call(
        &self,
        params: Value,
        ctx: Arc<RequestContext>,
    ) -> Pin<Box<dyn Future<Output = Result<Value, WsRpcError>> + Send>> {
        Box::pin(self(params, ctx))
    }
}

// ── Method contract ─────────────────────────────────────────────────────────

/// The params type of a method that takes none. Accepts any params (`{}`,
/// `null` or absent) and is `Record<string, never>` in TypeScript.
pub struct NoParams;

impl<'de> serde::Deserialize<'de> for NoParams {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        serde::de::IgnoredAny::deserialize(d).map(|_| NoParams)
    }
}

impl ts_rs::TS for NoParams {
    type WithoutGenerics = Self;
    type OptionInnerType = Self;
    fn name(_: &ts_rs::Config) -> String {
        "Record<string, never>".into()
    }
    fn inline(cfg: &ts_rs::Config) -> String {
        Self::name(cfg)
    }
}

/// A TypeScript type of a method contract: its expression and the exported
/// types it reaches.
pub struct TsType {
    /// The type expression, e.g. `Array<LinkExpression>` or `Agent | null`.
    pub name: String,
    /// `(ts name, path under the export dir)` of every exported type it
    /// reaches, directly or through fields and generic arguments.
    pub deps: Vec<(String, std::path::PathBuf)>,
    /// Writes those types to the export dir.
    pub export: fn(&ts_rs::Config) -> Result<(), ts_rs::ExportError>,
    /// Those types whose file under `dir` differs from what ts-rs renders.
    pub stale: fn(&ts_rs::Config, &std::path::Path) -> Vec<String>,
}

impl TsType {
    pub(crate) fn of<T: ts_rs::TS + 'static + ?Sized>() -> Self {
        let cfg = ts_rs::Config::from_env();
        let mut deps: Vec<(String, std::path::PathBuf)> = Reached::of::<T>(&cfg)
            .0
            .into_iter()
            .map(|t| (t.name, t.path))
            .collect();
        deps.sort();
        Self {
            name: T::name(&cfg),
            deps,
            export: |cfg| {
                for t in Reached::of::<T>(cfg).0 {
                    (t.export)(cfg)?;
                }
                Ok(())
            },
            stale: |cfg, dir| {
                Reached::of::<T>(cfg)
                    .0
                    .into_iter()
                    .filter(|t| {
                        let have = std::fs::read_to_string(dir.join(&t.path)).ok();
                        (t.render)(cfg).ok() != have
                    })
                    .map(|t| t.path.display().to_string())
                    .collect()
            },
        }
    }
}

/// An exported type reached from a contract type.
struct ReachedType {
    name: String,
    path: std::path::PathBuf,
    export: fn(&ts_rs::Config) -> Result<(), ts_rs::ExportError>,
    render: fn(&ts_rs::Config) -> Result<String, ts_rs::ExportError>,
}

/// Every exported type reachable from `T`, through fields and generic
/// arguments (`Option<Agent>` reaches `Agent`).
struct Reached<'a>(
    Vec<ReachedType>,
    std::collections::HashSet<std::any::TypeId>,
    &'a ts_rs::Config,
);

impl<'a> Reached<'a> {
    fn of<T: ts_rs::TS + 'static + ?Sized>(cfg: &'a ts_rs::Config) -> Self {
        let mut r = Reached(Vec::new(), std::collections::HashSet::new(), cfg);
        ts_rs::TypeVisitor::visit::<T>(&mut r);
        r
    }
}

impl ts_rs::TypeVisitor for Reached<'_> {
    fn visit<T: ts_rs::TS + 'static + ?Sized>(&mut self) {
        if !self.1.insert(std::any::TypeId::of::<T>()) {
            return;
        }
        if let Some(path) = T::output_path() {
            self.0.push(ReachedType {
                name: T::ident(self.2),
                path,
                export: |cfg| T::export(cfg),
                render: |cfg| T::export_to_string(cfg),
            });
        }
        T::visit_dependencies(self);
        T::visit_generics(self);
    }
}

/// What the SDK needs to know about a method: its name, the TypeScript
/// types of its params and result, and how the client should call it.
pub struct MethodSpec {
    pub name: String,
    pub params: TsType,
    pub result: TsType,
    /// An idempotent read: the client may resend it once after a reconnect.
    pub read: bool,
    /// Can run for minutes (LLM work, Holochain, publishing): the client's
    /// default timeout is its long one.
    pub long: bool,
}

type Check = fn(&Value) -> Result<(), String>;

fn check<T: DeserializeOwned>(v: &Value) -> Result<(), String> {
    T::deserialize(v).map(|_| ()).map_err(|e| e.to_string())
}

struct Entry {
    handler: WsHandler,
    params: Check,
    #[cfg_attr(not(debug_assertions), allow(dead_code))]
    result: Check,
}

/// Returned by [`HandlerMap::method`] to mark a method `read` or `long`.
pub struct MethodFlags<'a>(&'a mut MethodSpec);

impl MethodFlags<'_> {
    pub fn read(self) -> Self {
        self.0.read = true;
        self
    }
    pub fn long(self) -> Self {
        self.0.long = true;
        self
    }
}

// ── Handler map ─────────────────────────────────────────────────────────────

/// Registry of message-type → handler mappings.
///
/// Built at startup by calling each module's `register_ws_handlers()`.
/// Used by the WS dispatcher to route incoming RPC messages.
pub struct HandlerMap {
    handlers: HashMap<String, Entry>,
    specs: Vec<MethodSpec>,
}

impl HandlerMap {
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
            specs: Vec::new(),
        }
    }

    /// Register `handler` for `name` with its contract: dispatch rejects
    /// params that do not deserialize into `P` (400), and debug builds log a
    /// result that does not deserialize into `R`. The contract is exported
    /// to the SDK (`core/src/generated/api/RpcMethods.ts`).
    pub fn method<P, R>(&mut self, name: &str, handler: impl WsFn) -> MethodFlags<'_>
    where
        P: DeserializeOwned + ts_rs::TS + 'static,
        R: DeserializeOwned + ts_rs::TS + 'static,
    {
        if self.handlers.contains_key(name) {
            panic!("Duplicate WS handler registration for '{}'", name);
        }
        self.handlers.insert(
            name.to_string(),
            Entry {
                handler: Box::new(move |params, ctx| handler.call(params, ctx)),
                params: check::<P>,
                result: check::<R>,
            },
        );
        self.inline::<P, R>(name)
    }

    /// Record the contract of a method the socket reader handles itself
    /// (`events.watch`, `events.unwatch`), so the SDK gets its types. It is
    /// not dispatched through the map.
    pub fn inline<P, R>(&mut self, name: &str) -> MethodFlags<'_>
    where
        P: ts_rs::TS + 'static,
        R: ts_rs::TS + 'static,
    {
        self.specs.push(MethodSpec {
            name: name.to_string(),
            params: TsType::of::<P>(),
            result: TsType::of::<R>(),
            read: false,
            long: false,
        });
        MethodFlags(self.specs.last_mut().expect("just pushed"))
    }

    /// Every method contract, sorted by name.
    pub fn specs(&self) -> Vec<&MethodSpec> {
        let mut specs: Vec<&MethodSpec> = self.specs.iter().collect();
        specs.sort_by(|a, b| a.name.cmp(&b.name));
        specs
    }

    /// Dispatch an RPC message to the appropriate handler.
    pub async fn dispatch(
        &self,
        msg_type: &str,
        params: Value,
        ctx: Arc<RequestContext>,
    ) -> Result<Value, WsRpcError> {
        let Some(entry) = self.handlers.get(msg_type) else {
            // `<hash>.<method>` addresses a service method, not a core one.
            if crate::services::is_service_method(msg_type) {
                let call = crate::services::ServiceHost::context_for_request(&ctx);
                return crate::services::host()
                    .dispatch(msg_type, params, call)
                    .await;
            }
            return Err(WsRpcError::not_found(format!("Unknown type: {}", msg_type)));
        };
        (entry.params)(&params).map_err(|e| {
            WsRpcError::bad_request(format!("Invalid params for {}: {}", msg_type, e))
        })?;
        let result = (entry.handler)(params, ctx).await?;
        #[cfg(debug_assertions)]
        if let Err(e) = (entry.result)(&result) {
            log::error!("{} returned a result outside its contract: {}", msg_type, e);
        }
        Ok(result)
    }

    /// Number of registered handlers (useful for logging at startup).
    pub fn len(&self) -> usize {
        self.handlers.len()
    }
}

// ── Build the handler map ───────────────────────────────────────────────────

/// Build the complete handler map by calling each module's registration function.
///
/// This is the ONLY place that lists modules — each module owns its own handlers.
pub fn build_handler_map() -> HandlerMap {
    let mut map = HandlerMap::new();
    super::agent_ws::register_ws_handlers(&mut map);
    super::perspectives_ws::register_ws_handlers(&mut map);
    super::runtime_ws::register_ws_handlers(&mut map);
    super::languages_ws::register_ws_handlers(&mut map);
    super::expressions_ws::register_ws_handlers(&mut map);
    super::neighbourhoods_ws::register_ws_handlers(&mut map);
    super::users_ws::register_ws_handlers(&mut map);
    crate::services::ws::register_ws_handlers(&mut map);
    // Event type → the perspectives wanted (`null`: all); replaces the socket's interest.
    map.inline::<super::event_interest::WatchParams, bool>(super::event_interest::WATCH);
    map.inline::<NoParams, bool>(super::event_interest::UNWATCH);
    log::info!("WS RPC: registered {} handlers", map.len());
    map
}

// ── Param extraction helpers ────────────────────────────────────────────────

/// Helper trait for extracting typed values from JSON params.
pub trait ParamExt {
    /// Get a required string parameter.
    fn require_str(&self, key: &str) -> Result<String, WsRpcError>;
    /// Get an optional string parameter.
    fn opt_str(&self, key: &str) -> Option<String>;
    /// Get a required nested object/value parameter.
    fn require(&self, key: &str) -> Result<Value, WsRpcError>;
    /// Get an optional array of strings as a set: `None` when absent or
    /// `null`, 400 when present but not an array of strings.
    fn opt_str_set(&self, key: &str) -> Result<Option<HashSet<String>>, WsRpcError>;
}

impl ParamExt for Value {
    fn require_str(&self, key: &str) -> Result<String, WsRpcError> {
        self.get(key)
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .ok_or_else(|| {
                WsRpcError::bad_request(format!("Missing required parameter: '{}'", key))
            })
    }

    fn opt_str(&self, key: &str) -> Option<String> {
        self.get(key)
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
    }

    fn require(&self, key: &str) -> Result<Value, WsRpcError> {
        self.get(key).cloned().ok_or_else(|| {
            WsRpcError::bad_request(format!("Missing required parameter: '{}'", key))
        })
    }

    fn opt_str_set(&self, key: &str) -> Result<Option<HashSet<String>>, WsRpcError> {
        let invalid = || WsRpcError::bad_request(format!("`{}` must be an array of strings", key));
        match self.get(key) {
            None | Some(Value::Null) => Ok(None),
            Some(Value::Array(items)) => items
                .iter()
                .map(|v| v.as_str().map(str::to_string).ok_or_else(invalid))
                .collect::<Result<_, _>>()
                .map(Some),
            Some(_) => Err(invalid()),
        }
    }
}

//! The service host: resolve → authorise → validate → dispatch
//! → check result, and service events.

use std::collections::HashMap;
use std::sync::{Arc, LazyLock, Mutex, RwLock};
use std::time::{Duration, Instant};

use serde_json::{Map, Value};
use tokio::sync::broadcast;

use super::builtin::{
    CallContext, Caller, EventEmitter, EventOwner, ServiceCaller, ServiceError, ServiceHealth,
    ServiceImplementation, StartContext,
};
use super::capability::{allowed, service_capability, service_domain};
use super::interface::{genesis_of, is_hash, InterfaceDocument};
use super::registry::{BuiltinManifest, Registry, ResolveError};
use crate::agent::capabilities::Capability;
use crate::api::events_ws::events::SERVICE_STREAM_END;
use crate::api::ws_handler::WsRpcError;
use crate::types::RequestContext;

/// Default deadline of a method call, and of a method marked `long`.
const CALL_TIMEOUT: Duration = Duration::from_secs(60);
const LONG_CALL_TIMEOUT: Duration = Duration::from_secs(15 * 60);

/// One service event on its way to the event sockets.
#[derive(Debug, Clone)]
pub struct ServiceEvent {
    /// `<interface hash>.<event>`
    pub event_type: String,
    /// Whose sockets get it.
    pub owner: EventOwner,
    /// The capability a socket needs to receive it.
    pub needs: Capability,
    /// The wire message: the payload plus `type`.
    pub wire: String,
}

pub struct ServiceHost {
    registry: RwLock<Registry>,
    validators: Mutex<HashMap<String, Arc<jsonschema::Validator>>>,
    events: broadcast::Sender<ServiceEvent>,
    data_root: RwLock<Option<std::path::PathBuf>>,
    meter: RwLock<Option<String>>,
}

static HOST: LazyLock<Arc<ServiceHost>> = LazyLock::new(ServiceHost::new);

/// The executor's service host.
pub fn host() -> Arc<ServiceHost> {
    HOST.clone()
}

/// Dispatch `<hash>.<method>` on the executor's host.
pub async fn dispatch(method: &str, params: Value, ctx: CallContext) -> Result<Value, WsRpcError> {
    host().dispatch(method, params, ctx).await
}

/// `true` for a WS method name that addresses a service (`<hash>.<method>`).
pub fn is_service_method(name: &str) -> bool {
    name.split_once('.')
        .is_some_and(|(h, m)| is_hash(h) && !m.is_empty())
}

/// `<app data>/ad4m/services`, next to the languages directory.
fn default_data_root() -> std::path::PathBuf {
    let languages = crate::utils::languages_directory();
    match languages.parent() {
        Some(ad4m) => ad4m.join("services"),
        None => languages.join("services"),
    }
}

fn not_found(msg: String) -> WsRpcError {
    WsRpcError::not_found(msg)
}

fn unavailable(msg: String) -> WsRpcError {
    WsRpcError::new(503, msg)
}

impl ServiceHost {
    pub fn new() -> Arc<Self> {
        let (events, _) = broadcast::channel(1024);
        Arc::new(Self {
            registry: RwLock::new(Registry::default()),
            validators: Mutex::new(HashMap::new()),
            events,
            data_root: RwLock::new(None),
            meter: RwLock::new(None),
        })
    }

    pub fn registry(&self) -> std::sync::RwLockReadGuard<'_, Registry> {
        self.registry.read().unwrap_or_else(|e| e.into_inner())
    }

    fn registry_mut(&self) -> std::sync::RwLockWriteGuard<'_, Registry> {
        self.registry.write().unwrap_or_else(|e| e.into_inner())
    }

    /// Where service data directories live (`<root>/<genesis hash>/`).
    pub fn set_data_root(&self, root: std::path::PathBuf) {
        *self.data_root.write().unwrap_or_else(|e| e.into_inner()) = Some(root);
    }

    /// Meter `meter`-declared methods against this ledger interface: before
    /// such a call the host asks `<ledger>.check { operation }` for the
    /// caller, and answers 402 when the ledger refuses.
    pub fn set_meter(&self, ledger_interface: Option<String>) {
        *self.meter.write().unwrap_or_else(|e| e.into_inner()) = ledger_interface;
    }

    async fn meter_check(&self, operation: &str, ctx: &CallContext) -> Result<(), WsRpcError> {
        let Some(ledger) = self.meter.read().unwrap_or_else(|e| e.into_inner()).clone() else {
            return Ok(());
        };
        // The host asks on the caller's behalf; the caller needs no ledger grant.
        let host_ctx = CallContext {
            origin: vec![ctx.caller.clone()],
            grants: vec![vec![
                crate::agent::capabilities::defs::ALL_CAPABILITY.clone()
            ]],
            ..ctx.clone()
        };
        let allowed = Box::pin(self.dispatch(
            &format!("{}.check", ledger),
            serde_json::json!({ "operation": operation }),
            host_ctx,
        ))
        .await?;
        if allowed == Value::Bool(true) {
            Ok(())
        } else {
            Err(
                WsRpcError::new(402, "Insufficient compute credits").with_data(
                    serde_json::json!({ "name": "InsufficientCredit", "operation": operation }),
                ),
            )
        }
    }

    pub fn subscribe_events(&self) -> broadcast::Receiver<ServiceEvent> {
        self.events.subscribe()
    }

    /// Live subscribers to the event bus (watches included).
    #[cfg(test)]
    pub fn event_subscribers(&self) -> usize {
        self.events.receiver_count()
    }

    /// In-process subscription to one stream: the payloads of `chunk_type`
    /// events of `stream_id`, in order, ending (channel closed) after the
    /// stream's `service-stream-end`. One subscriber serves both, so no
    /// chunk can arrive after the end.
    pub fn watch_stream(
        &self,
        chunk_type: String,
        stream_id: String,
    ) -> tokio::sync::mpsc::UnboundedReceiver<Value> {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let mut events = self.events.subscribe();
        tokio::spawn(async move {
            loop {
                let next = tokio::select! {
                    e = events.recv() => e,
                    _ = tx.closed() => break,
                };
                match next {
                    Ok(e) if e.event_type == chunk_type || e.event_type == SERVICE_STREAM_END => {
                        let Ok(payload) = serde_json::from_str::<Value>(&e.wire) else {
                            continue;
                        };
                        if payload.get("streamId").and_then(Value::as_str)
                            != Some(stream_id.as_str())
                        {
                            continue;
                        }
                        if e.event_type == SERVICE_STREAM_END || tx.send(payload).is_err() {
                            break;
                        }
                    }
                    Ok(_) => {}
                    // A lost chunk or end marker would leave the stream
                    // incomplete or open forever: end it here.
                    Err(broadcast::error::RecvError::Lagged(n)) => {
                        log::warn!("stream watch {} lagged by {}; ending it", stream_id, n);
                        break;
                    }
                    Err(broadcast::error::RecvError::Closed) => break,
                }
            }
        });
        rx
    }

    /// In-process subscription to one service event type (`<hash>.<event>`),
    /// optionally narrowed to one scope value. Yields the payloads; the
    /// executor's own consumers hold no grants, so nothing is filtered by
    /// owner.
    pub fn watch(
        &self,
        event_type: String,
        scope: Option<String>,
    ) -> tokio::sync::mpsc::UnboundedReceiver<Value> {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        let mut events = self.events.subscribe();
        let field = self.registry().event_scope(&event_type);
        tokio::spawn(async move {
            loop {
                let next = tokio::select! {
                    e = events.recv() => e,
                    _ = tx.closed() => break,
                };
                match next {
                    Ok(e) if e.event_type == event_type => {
                        let Ok(payload) = serde_json::from_str::<Value>(&e.wire) else {
                            continue;
                        };
                        let in_scope = match (&scope, &field) {
                            (Some(want), Some(f)) => {
                                payload.get(f).and_then(Value::as_str) == Some(want.as_str())
                            }
                            _ => true,
                        };
                        if in_scope && tx.send(payload).is_err() {
                            break;
                        }
                    }
                    Ok(_) => {}
                    Err(broadcast::error::RecvError::Lagged(n)) => {
                        log::warn!("service event watch on {} lagged by {}", event_type, n)
                    }
                    Err(broadcast::error::RecvError::Closed) => break,
                }
            }
        });
        rx
    }

    /// Register an interface version (see [`Registry::register_interface`]).
    pub fn register_interface(
        &self,
        raw: Value,
        signature: Option<&str>,
        trusted: bool,
    ) -> Result<Arc<InterfaceDocument>, String> {
        self.registry_mut()
            .register_interface(raw, signature, trusted)
    }

    /// Register a built-in implementation. It stays stopped until [`Self::start`].
    pub fn register_builtin(
        &self,
        manifest: BuiltinManifest,
        service: Arc<dyn ServiceImplementation>,
    ) -> Result<String, String> {
        self.registry_mut()
            .register_implementation(manifest, service)
    }

    /// Prefer the Service Language `module` for `interface`'s line, for
    /// `user` or (None) as the executor default.
    pub fn set_preference(
        &self,
        user: Option<String>,
        interface: &str,
        module: &str,
    ) -> Result<(), String> {
        self.registry_mut().set_preference(user, interface, module)
    }

    /// Start an implementation with `config`.
    pub async fn start(
        self: &Arc<Self>,
        implementation: &str,
        config: Value,
    ) -> Result<(), String> {
        let (service, grants, data_dir) = {
            let mut reg = self.registry_mut();
            let i = reg
                .implementation(implementation)
                .ok_or_else(|| format!("unknown implementation {}", implementation))?;
            if matches!(
                i.health,
                ServiceHealth::Starting | ServiceHealth::Running | ServiceHealth::Degraded(_)
            ) {
                return Err(format!(
                    "implementation {} is already started",
                    implementation
                ));
            }
            let (service, grants) = (i.service.clone(), i.grants.clone());
            let module = genesis_of(&i.module_id).unwrap_or_default().to_string();
            let root = self
                .data_root
                .read()
                .unwrap_or_else(|e| e.into_inner())
                .clone()
                .unwrap_or_else(default_data_root);
            reg.set_health(implementation, ServiceHealth::Starting);
            (service, grants, root.join(module))
        };
        std::fs::create_dir_all(&data_dir)
            .map_err(|e| format!("cannot create {}: {}", data_dir.display(), e))?;
        let ctx = StartContext {
            config,
            data_dir,
            events: EventEmitter {
                host: self.clone(),
                implementation: implementation.to_string(),
            },
            services: ServiceCaller {
                host: self.clone(),
                implementation: implementation.to_string(),
                grants,
            },
        };
        match service.start(ctx).await {
            Ok(()) => {
                let health = service.health().await;
                self.registry_mut().set_health(implementation, health);
                Ok(())
            }
            Err(e) => {
                self.registry_mut()
                    .set_health(implementation, ServiceHealth::Failed(e.clone()));
                Err(e)
            }
        }
    }

    /// Stop an implementation. Callers then get 503.
    pub async fn stop(&self, implementation: &str) -> Result<(), String> {
        let service = self
            .registry()
            .implementation(implementation)
            .map(|i| i.service.clone())
            .ok_or_else(|| format!("unknown implementation {}", implementation))?;
        let result = service.stop().await;
        self.registry_mut()
            .set_health(implementation, ServiceHealth::Stopped);
        result
    }

    /// Ask every running implementation for its health.
    pub async fn refresh_health(&self) {
        let services: Vec<(String, Arc<dyn ServiceImplementation>)> = self
            .registry()
            .implementations()
            .filter(|i| i.is_running())
            .map(|i| (i.hash.clone(), i.service.clone()))
            .collect();
        for (hash, service) in services {
            let health = service.health().await;
            self.registry_mut().set_health(&hash, health);
        }
    }

    fn validator(
        &self,
        key: String,
        doc: &InterfaceDocument,
        schema: &Value,
    ) -> Result<Arc<jsonschema::Validator>, WsRpcError> {
        let mut cache = self.validators.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(v) = cache.get(&key) {
            return Ok(v.clone());
        }
        let v = jsonschema::draft202012::new(&doc.standalone_schema(schema))
            .map(Arc::new)
            .map_err(|e| {
                WsRpcError::internal(format!("schema of {} does not compile: {}", key, e))
            })?;
        cache.insert(key, v.clone());
        Ok(v)
    }

    /// Build the context of a WS RPC call.
    pub fn context_for_request(req: &RequestContext) -> CallContext {
        let agent_did = req.user_did.clone().or_else(|| {
            crate::agent::did_for_context(&crate::agent::AgentContext::from_auth_token(
                req.auth_token.clone(),
            ))
            .ok()
        });
        CallContext {
            caller: Caller::App,
            origin: Vec::new(),
            agent_did,
            user: req.user_email.clone(),
            auth_token: Some(req.auth_token.clone()),
            is_admin: req.is_admin_credential,
            grants: vec![req.capabilities.clone().unwrap_or_default()],
            deadline: None,
        }
    }

    /// Dispatch `<target>.<method>`: an interface version hash, or an
    /// implementation hash to pin one build.
    pub async fn dispatch(
        &self,
        full: &str,
        params: Value,
        mut ctx: CallContext,
    ) -> Result<Value, WsRpcError> {
        let (target, method) = full
            .split_once('.')
            .ok_or_else(|| not_found(format!("`{}` is not `<hash>.<method>`", full)))?;
        let (doc, implementation, service, builtin) = {
            let reg = self.registry();
            let (doc, implementation) =
                reg.resolve(target, method, ctx.user.as_deref())
                    .map_err(|e| match e {
                        ResolveError::NotFound(m) => not_found(m),
                        ResolveError::Unavailable(m) => unavailable(m),
                    })?;
            let i = reg
                .implementation(&implementation)
                .expect("resolved implementation exists");
            let builtin = i.manifest.runtime.get("kind").and_then(Value::as_str) == Some("builtin");
            (doc, implementation, i.service.clone(), builtin)
        };
        let def = doc
            .doc
            .methods
            .get(method)
            .expect("resolved method exists")
            .clone();

        let needs = service_capability(&doc.module_id(), &doc.compat(), &def.action);
        if !allowed(&ctx.grants, &needs) {
            return Err(WsRpcError::forbidden(format!(
                "missing grant {}#{}",
                service_domain(&doc.module_id(), &doc.compat()),
                def.action
            )));
        }

        let params_validator =
            self.validator(format!("{}.{}#params", doc.hash, method), &doc, &def.params)?;
        if let Some(e) = params_validator.iter_errors(&params).next() {
            return Err(WsRpcError::bad_request(format!(
                "Invalid params for {}: {} at `{}`",
                full,
                e,
                e.instance_path()
            )));
        }

        if let Some(meter) = &def.meter {
            self.meter_check(&meter.operation, &ctx).await?;
        }

        let timeout = if def.long {
            LONG_CALL_TIMEOUT
        } else {
            CALL_TIMEOUT
        };
        let deadline = Instant::now() + timeout;
        let deadline = ctx.deadline.map_or(deadline, |d| d.min(deadline));
        ctx.deadline = Some(deadline);
        let stream_end = def.stream.as_ref().and_then(|_| {
            let id = params.get("streamId").and_then(Value::as_str)?.to_string();
            let owner = ctx
                .agent_did
                .clone()
                .map(EventOwner::Agent)
                .unwrap_or(EventOwner::Executor);
            Some((id, owner))
        });
        // The admin credential is the executor's to honour: only its own
        // (builtin) services see it.
        if !builtin {
            ctx.is_admin = false;
        }
        let outcome =
            tokio::time::timeout_at(deadline.into(), service.call(method, params, ctx)).await;
        // Sent after every chunk the service emitted, on the same path, so it
        // reaches the socket after them (the reply may not).
        if let Some((stream_id, owner)) = stream_end {
            let _ = self.events.send(ServiceEvent {
                event_type: SERVICE_STREAM_END.to_string(),
                owner,
                needs: needs.clone(),
                wire: serde_json::json!({
                    "type": SERVICE_STREAM_END,
                    "streamId": stream_id,
                    "method": full,
                    "ok": matches!(outcome, Ok(Ok(_))),
                })
                .to_string(),
            });
        }
        let result = match outcome {
            Err(_) => {
                return Err(WsRpcError::new(
                    504,
                    format!("{} passed its deadline", full),
                ))
            }
            Ok(Err(ServiceError::Unavailable(m))) => return Err(unavailable(m)),
            Ok(Err(ServiceError::Forbidden(m))) => return Err(WsRpcError::forbidden(m)),
            Ok(Err(ServiceError::Internal(m))) => return Err(WsRpcError::internal(m)),
            Ok(Err(ServiceError::Method {
                name,
                data,
                message,
            })) => {
                return Err(match def.errors.get(&name) {
                    Some(e) => {
                        if let (Some(schema), Some(value)) = (&e.data, &data) {
                            let validator = self.validator(
                                format!("{}.{}#error.{}", doc.hash, method, name),
                                &doc,
                                schema,
                            )?;
                            let violation = validator.iter_errors(value).next().map(|v| {
                                format!(
                                    "{} error `{}` data outside its contract: {}",
                                    full, name, v
                                )
                            });
                            if let Some(msg) = violation {
                                if !builtin {
                                    return Err(WsRpcError::new(502, msg));
                                }
                                log::error!("{}", msg);
                            }
                        }
                        let mut d = match data {
                            Some(Value::Object(m)) => m,
                            Some(other) => Map::from_iter([("value".to_string(), other)]),
                            None => Map::new(),
                        };
                        d.insert("name".into(), Value::String(name));
                        WsRpcError::new(e.code, message).with_data(Value::Object(d))
                    }
                    None => {
                        log::error!(
                            "{} ({}) returned undeclared error `{}`",
                            full,
                            implementation,
                            name
                        );
                        WsRpcError::internal(message)
                    }
                })
            }
            Ok(Ok(v)) => v,
        };
        if cfg!(debug_assertions) || !builtin {
            let result_validator =
                self.validator(format!("{}.{}#result", doc.hash, method), &doc, &def.result)?;
            let violation = result_validator.iter_errors(&result).next().map(|e| {
                format!(
                    "{} returned a result outside its contract: {} at `{}`",
                    full,
                    e,
                    e.instance_path()
                )
            });
            if let Some(msg) = violation {
                if builtin {
                    log::error!("{}", msg);
                } else {
                    return Err(WsRpcError::new(502, msg));
                }
            }
        }
        Ok(result)
    }

    /// Emit an implementation's event. The payload is checked
    /// against the newest implemented version that declares the event, then
    /// goes out once per compatible version's event type.
    pub async fn emit_event(
        &self,
        implementation: &str,
        event: &str,
        owner: EventOwner,
        payload: Value,
    ) -> Result<(), String> {
        let targets = self.registry().event_targets(implementation, event);
        let newest = targets
            .iter()
            .max_by(|a, b| a.version.cmp(&b.version))
            .ok_or_else(|| format!("{} declares no event `{}`", implementation, event))?
            .clone();
        let def = &newest.doc.events[event];
        let validator = self
            .validator(
                format!("{}.{}#event", newest.hash, event),
                &newest,
                &def.payload,
            )
            .map_err(|e| e.message)?;
        if let Some(e) = validator.iter_errors(&payload).next() {
            return Err(format!(
                "event `{}` payload outside its contract: {} at `{}`",
                event,
                e,
                e.instance_path()
            ));
        }
        let Value::Object(fields) = payload else {
            return Err("event payload must be an object".into());
        };
        for doc in targets {
            let def = &doc.doc.events[event];
            let event_type = format!("{}.{}", doc.hash, event);
            let mut wire = fields.clone();
            wire.insert("type".into(), Value::String(event_type.clone()));
            let _ = self.events.send(ServiceEvent {
                event_type,
                owner: owner.clone(),
                needs: service_capability(&doc.module_id(), &doc.compat(), &def.action),
                wire: Value::Object(wire).to_string(),
            });
        }
        Ok(())
    }

    /// Should a socket of agent `did` / account `user` holding `capabilities`
    /// (or an admin) get `event`?
    pub fn delivers(
        event: &ServiceEvent,
        did: Option<&str>,
        user: Option<&str>,
        is_admin: bool,
        capabilities: &[Capability],
    ) -> bool {
        let owns = match &event.owner {
            EventOwner::Agent(d) => did == Some(d.as_str()),
            EventOwner::User(u) => user == Some(u.as_str()),
            EventOwner::All => true,
            EventOwner::Executor => false,
        };
        (is_admin || owns) && allowed(&[capabilities.to_vec()], &event.needs)
    }
}

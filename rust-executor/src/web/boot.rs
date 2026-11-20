//! The web executor's start and its one way in: [`boot`], [`dispatch`], [`on_event`].
//!
//! `boot` does, in memory, the part of native `run()` (`lib_native.rs`) that ring 2 needs:
//! the config, the database, the wallet, the agent service and the handler map. Nothing is
//! written to a disk and nothing survives a reload: SQLite is `:memory:`, every perspective's
//! store is an in-memory Oxigraph (no `app_data_path`), and the agent file and profiles are in
//! `rt::fs`'s map. There is no HTTP server, no language runtime and no Holochain.
//!
//! The caller is the page that started the Worker. It is the operator, with no token
//! (decision 6): the context is native's own for an empty token on a loopback listener of an
//! executor without an admin credential ([`RequestContext::open`]), so the web executor adds
//! no new way to become the operator. Any script on the page is the operator.
//!
//! No JS type is named here: the `web-executor` crate turns these into wasm-bindgen exports.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use serde::Deserialize;
use serde_json::{json, Value};

use crate::agent::capabilities::ListenerReach;
use crate::api::ws_handler::{build_handler_map, HandlerMap};
use crate::types::RequestContext;

/// The app data path the agent service is given. Only a key prefix in `rt::fs`'s map: the
/// global config keeps `app_data_path` empty, so perspectives open in-memory stores.
const VIRTUAL_APP_PATH: &str = "/ad4m-web";

/// What `boot` was given: a JSON object, every key optional. Unknown keys are refused.
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct BootConfig {
    /// `true` for an executor that loads only the language language (no effect yet: there are
    /// no system languages in the web executor).
    pub language_language_only: Option<bool>,
}

struct Core {
    handlers: HandlerMap,
    operator: Arc<RequestContext>,
}

thread_local! {
    static CORE: RefCell<Option<Rc<Core>>> = const { RefCell::new(None) };
}

/// Starts the executor in memory. `config_json` is a [`BootConfig`] (`"{}"` for the
/// defaults). Fails on a config it cannot read, a second boot, or a database that does not
/// open; the error says which.
pub fn boot(config_json: &str) -> Result<(), String> {
    if CORE.with(|core| core.borrow().is_some()) {
        return Err("the web executor is already booted".to_string());
    }
    let boot_config: BootConfig = if config_json.trim().is_empty() {
        BootConfig::default()
    } else {
        serde_json::from_str(config_json).map_err(|e| format!("boot config: {e}"))?
    };

    // `unprepared`, not `Default`: `prepare` reads the environment and names the data folder
    // on disk. No data path, so every perspective's store is in memory; no Holochain.
    let mut config = crate::config::Ad4mConfig::unprepared();
    config.run_holochain = Some(false);
    config.connect_holochain = Some(false);
    config.language_language_only = Some(boot_config.language_language_only.unwrap_or(false));
    config.run_dapp_server = Some(false);
    crate::config::set_global_config(config);

    crate::db::Ad4mDb::init_global_instance(":memory:").map_err(|e| format!("database: {e}"))?;
    // The database backend is the default one (`LocalDb` over `Ad4mDb`), as native without
    // `db_backend = "shared"`.
    let wallet: Arc<dyn crate::wallet::WalletBackend> = Arc::new(crate::wallet::LocalWallet::new());
    crate::wallet::try_init_wallet_backend(wallet);
    crate::agent::AgentService::init_global_instance(VIRTUAL_APP_PATH.to_string());

    let operator = RequestContext::open("", &None, ListenerReach::Loopback)
        .map_err(|e| format!("operator session: {e}"))?;
    debug_assert!(operator.is_operator());
    let core = Core {
        handlers: build_handler_map(),
        operator: Arc::new(operator),
    };
    CORE.with(|slot| *slot.borrow_mut() = Some(Rc::new(core)));
    Ok(())
}

/// One request frame in, its reply frame out, as on the RPC socket (`api/ws_rpc.rs`):
/// `{"id", "type", "params"}` gives `{"id", "result"}` or `{"id", "error": {"code",
/// "message", "data"?}}`. Never fails: a frame it cannot read, or a call before `boot`,
/// answers with an error frame.
pub async fn dispatch(frame_json: String) -> String {
    let frame: Value = match serde_json::from_str(&frame_json) {
        Ok(frame) => frame,
        Err(_) => return error_frame(&Value::Null, 400, "Invalid JSON"),
    };
    let id = frame.get("id").cloned().unwrap_or(Value::Null);
    let Some(method) = frame.get("type").and_then(Value::as_str) else {
        return error_frame(&id, 400, "Missing 'type' field");
    };
    let Some(core) = CORE.with(|core| core.borrow().clone()) else {
        return error_frame(&id, 503, "the web executor is not booted");
    };
    let params = frame.get("params").cloned().unwrap_or_else(|| json!({}));
    let reply = match core
        .handlers
        .dispatch(method, params, core.operator.clone())
        .await
    {
        Ok(result) => json!({ "id": id, "result": result }),
        Err(e) => json!({ "id": id, "error": e.to_wire() }),
    };
    reply.to_string()
}

fn error_frame(id: &Value, code: u16, message: &str) -> String {
    json!({ "id": id, "error": { "code": code, "message": message } }).to_string()
}

/// Calls `callback` with every event the executor publishes, as the socket's event frame
/// (`{"type": <event>, ...payload}`), from now until the Worker ends. Every event: the
/// per-session filter (`events.watch`, audiences, live queries of one connection) is the
/// transport's (S10).
pub fn on_event(callback: impl Fn(String) + 'static) {
    let mut events = crate::pubsub::get_global_pubsub_sync().subscribe_events();
    wasm_bindgen_futures::spawn_local(async move {
        use tokio::sync::broadcast::error::RecvError;
        loop {
            match events.recv().await {
                Ok(event) => callback(event_frame(&event)),
                Err(RecvError::Lagged(missed)) => {
                    log::warn!("on_event: {missed} events dropped, the listener was too slow")
                }
                Err(RecvError::Closed) => break,
            }
        }
    });
}

fn event_frame(event: &crate::events::Event) -> String {
    let mut frame = match &event.payload {
        Value::Object(fields) => fields.clone(),
        other => {
            let mut fields = serde_json::Map::new();
            fields.insert("payload".to_string(), other.clone());
            fields
        }
    };
    frame.insert("type".to_string(), json!(event.topic.as_str()));
    Value::Object(frame).to_string()
}

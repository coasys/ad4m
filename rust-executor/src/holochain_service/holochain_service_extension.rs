use deno_core::error::AnyError;
use deno_core::{anyhow::anyhow, op2};
use holochain::{
    conductor::api::AppInfo,
    prelude::{hash_type::Agent, ExternIO, HoloHash, InstallAppPayload, ZomeCallResponse},
};
use log::error;
use serde::{Deserialize, Serialize};
use std::future::Future;
use std::io::Cursor;
use std::time::{Duration, Instant};
use tokio::time::timeout;

use super::{holochain_service_once_started, HolochainServiceInterface};
use crate::holochain_service::{HolochainService, LocalConductorConfig};
use crate::js_core::error::AnyhowWrapperError;
use crate::js_core::languages_extension::try_with_isolate_state;
use crate::{
    agent::AgentContext,
    holochain_service::user_cells::{ConductorBackend, UserCells},
    js_core::agent_extension::CallAgent,
};

/// The agent a `HOLOCHAIN_SERVICE` op acts as: the agent of the call the JS runs for
/// (`__ad4mCallAgent()`, #1861's call token). With no token the op refuses, as the `AGENT` ops
/// do: code that clears its async context during a managed user's call must not reach the main
/// agent's cell, or another user's app id. Every host entry into a language (bootstrap, `init`
/// and its `registerDNAs`, callback registration, a Holochain signal, a sync) runs with a
/// main-agent token, so no host path needs a default.
fn agent_of(call: Option<&CallAgent>) -> Result<AgentContext, AnyhowWrapperError> {
    call.map(|call| call.agent().clone()).ok_or_else(|| {
        AnyhowWrapperError::from(anyhow!(
            "No agent: this code runs outside any call into the language, so it may not use \
             Holochain"
        ))
    })
}

/// The language this isolate runs, if it runs one.
fn own_language() -> Option<String> {
    try_with_isolate_state(|state| state.language_address.clone()).flatten()
}

/// The app id a call of `agent` to `app_id` goes to: `app_id` for the main agent, the user's
/// own copy for a managed user (installed on first use; see `user_cells`).
async fn app_for_call(
    interface: &HolochainServiceInterface,
    app_id: &str,
    language: Option<&str>,
    agent: &AgentContext,
) -> Result<String, AnyError> {
    if agent.is_main_agent() {
        return Ok(app_id.to_string());
    }
    UserCells::global()
        .app_for_call(
            &ConductorBackend(interface.clone()),
            app_id,
            language,
            agent,
        )
        .await
}

/// Convert an rmpv::Value (full-fidelity msgpack) to serde_json::Value.
/// Binary data is represented as a JSON object `{"__binary": [byte, byte, ...]}` so
/// the JS layer can recognize it and convert to Uint8Array.
pub fn msgpack_value_to_json(val: rmpv::Value) -> serde_json::Value {
    match val {
        rmpv::Value::Nil => serde_json::Value::Null,
        rmpv::Value::Boolean(b) => serde_json::Value::Bool(b),
        rmpv::Value::Integer(i) => {
            if let Some(n) = i.as_i64() {
                serde_json::Value::Number(n.into())
            } else if let Some(n) = i.as_u64() {
                serde_json::Value::Number(n.into())
            } else {
                serde_json::Value::Null
            }
        }
        rmpv::Value::F32(f) => serde_json::Number::from_f64(f as f64)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        rmpv::Value::F64(f) => serde_json::Number::from_f64(f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        rmpv::Value::String(s) => serde_json::Value::String(s.into_str().unwrap_or_default()),
        rmpv::Value::Binary(bytes) => {
            // Mark binary data so JS can convert it to Uint8Array
            let arr: Vec<serde_json::Value> = bytes
                .iter()
                .map(|b| serde_json::Value::Number((*b).into()))
                .collect();
            let mut map = serde_json::Map::new();
            map.insert("__binary".to_string(), serde_json::Value::Array(arr));
            serde_json::Value::Object(map)
        }
        rmpv::Value::Array(arr) => {
            serde_json::Value::Array(arr.into_iter().map(msgpack_value_to_json).collect())
        }
        rmpv::Value::Map(entries) => {
            let mut map = serde_json::Map::new();
            for (k, v) in entries {
                let key = match k {
                    rmpv::Value::String(s) => s.into_str().unwrap_or_default(),
                    other => format!("{}", other),
                };
                map.insert(key, msgpack_value_to_json(v));
            }
            serde_json::Value::Object(map)
        }
        rmpv::Value::Ext(_, data) => {
            // Treat extension types like binary
            let arr: Vec<serde_json::Value> = data
                .iter()
                .map(|b| serde_json::Value::Number((*b).into()))
                .collect();
            let mut map = serde_json::Map::new();
            map.insert("__binary".to_string(), serde_json::Value::Array(arr));
            serde_json::Value::Object(map)
        }
    }
}

/// Convert a serde_json::Value back to rmpv::Value for encoding to msgpack.
/// Recognizes `{"__binary": [...]}` objects and converts them back to Binary.
/// Also heuristically converts top-level or nested arrays of u8-range numbers to Binary.
fn json_to_msgpack_value(val: &serde_json::Value) -> rmpv::Value {
    match val {
        serde_json::Value::Null => rmpv::Value::Nil,
        serde_json::Value::Bool(b) => rmpv::Value::Boolean(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                rmpv::Value::Integer(rmpv::Integer::from(i))
            } else if let Some(u) = n.as_u64() {
                rmpv::Value::Integer(rmpv::Integer::from(u))
            } else if let Some(f) = n.as_f64() {
                integral_f64_to_msgpack(f)
            } else {
                rmpv::Value::Nil
            }
        }
        serde_json::Value::String(s) => rmpv::Value::String(s.clone().into()),
        serde_json::Value::Array(arr) => {
            rmpv::Value::Array(arr.iter().map(json_to_msgpack_value).collect())
        }
        serde_json::Value::Object(map) => {
            // Check for our __binary marker
            if map.len() == 1 {
                if let Some(serde_json::Value::Array(bytes)) = map.get("__binary") {
                    let byte_vec: Vec<u8> = bytes
                        .iter()
                        .filter_map(|v| v.as_u64().map(|n| n as u8))
                        .collect();
                    return rmpv::Value::Binary(byte_vec);
                }
            }
            let entries: Vec<(rmpv::Value, rmpv::Value)> = map
                .iter()
                .map(|(k, v)| {
                    (
                        rmpv::Value::String(k.clone().into()),
                        json_to_msgpack_value(v),
                    )
                })
                .collect();
            rmpv::Value::Map(entries)
        }
    }
}

/// JS has no integer type, so a whole number such as `5` or `Date.now()` can
/// reach us as f64. Encode a finite whole-number f64 that fits in i64 as a
/// msgpack integer, so zome fields typed `u64`/`i64` decode it. Zome fields
/// typed `f64` still accept a msgpack integer. Fractions, NaN, infinities and
/// values outside i64 range stay F64. Minus zero becomes Integer 0, so its sign
/// is lost; a whole number has no use for it.
fn integral_f64_to_msgpack(f: f64) -> rmpv::Value {
    // 2^63 is exact in f64; i64 covers [-2^63, 2^63).
    const I64_BOUND: f64 = 9_223_372_036_854_775_808.0;
    if f.is_finite() && f.fract() == 0.0 && (-I64_BOUND..I64_BOUND).contains(&f) {
        rmpv::Value::Integer(rmpv::Integer::from(f as i64))
    } else {
        rmpv::Value::F64(f)
    }
}

/// A JS-friendly version of ZomeCallResponse that decodes ExternIO bytes to JSON.
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
enum DecodedZomeCallResponse {
    Ok { value: serde_json::Value },
    NetworkError { value: String },
    CountersigningSession { value: String },
}

impl DecodedZomeCallResponse {
    fn from_zome_call_response(response: ZomeCallResponse) -> Result<Self, AnyhowWrapperError> {
        match response {
            ZomeCallResponse::Ok(extern_io) => {
                // Decode msgpack bytes using rmpv for full-fidelity type preservation.
                // This properly handles Binary types (hashes, etc.) that serde_json::Value
                // cannot represent directly.
                let bytes = extern_io.as_bytes();
                let value = if bytes.is_empty() {
                    serde_json::Value::Null
                } else {
                    let mut cursor = Cursor::new(bytes);
                    match rmpv::decode::read_value(&mut cursor) {
                        Ok(msgpack_val) => msgpack_value_to_json(msgpack_val),
                        Err(e) => {
                            log::warn!(
                                "rmpv decode failed ({}), falling back to raw bytes array",
                                e
                            );
                            // Last resort: return raw bytes as an array of numbers
                            serde_json::Value::Array(
                                bytes
                                    .iter()
                                    .map(|b| serde_json::Value::Number((*b).into()))
                                    .collect(),
                            )
                        }
                    }
                };
                Ok(DecodedZomeCallResponse::Ok { value })
            }
            ZomeCallResponse::NetworkError(msg) => {
                Ok(DecodedZomeCallResponse::NetworkError { value: msg })
            }
            ZomeCallResponse::CountersigningSession(msg) => {
                Ok(DecodedZomeCallResponse::CountersigningSession { value: msg })
            }
            other => Err(AnyhowWrapperError::from(anyhow!(
                "Unexpected ZomeCallResponse variant: {:?}",
                other
            ))),
        }
    }
}

// The duration to use for timeouts
const TIMEOUT_DURATION: Duration = Duration::from_secs(90);

// Bumped 20s → 45s to accommodate the two-stage network-readiness gate
// added in install_app (see holochain_service/mod.rs): per-cell
// await_cell_network_join_complete (up to 10s) + per-DNA
// await_initial_peer_discovery (up to 11s). A single-cell / single-DNA
// language install on a cold, isolated node routinely sits at ~21s
// before install_app_bundle + enable_app even execute; the old 20s
// budget was already tight on dev and would fire the dispatcher
// timeout under the new gate. 45s gives comfortable headroom while
// still catching genuinely stuck installs. (Data's PR #907 review item MED-1.)
pub(crate) const APP_INSTALL_TIMEOUT_DURATION: Duration = Duration::from_secs(45);

#[op2(async(lazy))] // op2 v2.9: not fast-compatible (complex serde arg)
async fn start_holochain_conductor(
    #[serde] config: LocalConductorConfig,
) -> Result<(), AnyhowWrapperError> {
    HolochainService::init(config).await?;
    Ok(())
}

#[op2(async(lazy), fast)]
async fn log_dht_status() -> Result<(), AnyhowWrapperError> {
    let Some(interface) = holochain_service_once_started().await else {
        return Ok(());
    };
    let res = timeout(TIMEOUT_DURATION, interface.log_network_metrics()).await;
    match res {
        Ok(_) => Ok(()),
        Err(_) => {
            error!("Timeout error logging dht status");
            Ok(())
        }
    }
}

/// Installs an app. As the main agent (a language's `registerDNAs` during its `init`), as
/// before, and the payload is remembered for the language's users (#1775): a managed user's
/// first call to the app installs the same payload under the user's own key. As a managed
/// user, the language's payload never reaches the conductor: the answer is the user's own app,
/// installed by Rust's per-user installer under the user's key. User app ids (`<app>@<did>`)
/// are reserved to that installer.
#[op2(async(lazy))] // op2 v2.9: not fast-compatible (complex serde arg)
#[serde]
async fn install_app(
    #[serde] install_app_payload: InstallAppPayload,
    #[cppgc] call: Option<&CallAgent>,
) -> Result<AppInfo, AnyhowWrapperError> {
    let agent = agent_of(call)?;
    let language = own_language();
    let app_id = install_app_payload.installed_app_id.clone();
    if let Some(app_id) = app_id.as_deref().filter(|id| id.contains('@')) {
        return Err(AnyhowWrapperError::from(anyhow!(
            "installApp: app id {app_id} contains '@', which is reserved to the users' own apps"
        )));
    }
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    if !agent.is_main_agent() {
        let app_id = app_id.ok_or_else(|| {
            AnyhowWrapperError::from(anyhow!("installApp needs an installed_app_id"))
        })?;
        let user_app = app_for_call(&interface, &app_id, language.as_deref(), &agent).await?;
        return interface
            .get_app_info(user_app.clone())
            .await?
            .ok_or_else(|| AnyhowWrapperError::from(anyhow!("app {user_app} is not installed")));
    }
    // Remembered before the install: after a restart the app is installed already and the
    // install below fails, but the language's users still need the payload. Only an app id
    // of the language's own (`<address>-<nick>`), so one language cannot take over the
    // routing of another's app.
    if let (Some(language), Some(app_id)) = (&language, &app_id) {
        if app_id.starts_with(&format!("{language}-")) {
            UserCells::global().remember_main_app(app_id, language, install_app_payload.clone());
        }
    }
    let installed = timeout(
        APP_INSTALL_TIMEOUT_DURATION,
        interface.install_app(install_app_payload),
    )
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")));
    if let Some(app_id) = &app_id {
        UserCells::global()
            .restore_user_routes(&ConductorBackend(interface.clone()), app_id)
            .await;
    }
    installed?.map_err(AnyhowWrapperError::from)
}

/// The app info of `app_id` as the calling agent sees it: the main agent's app for the main
/// agent, the user's own copy for a managed user (installed on first use, #1775).
// nofast: a fast op cannot keep the call token borrowed across its awaits.
#[op2(async(lazy), nofast)]
#[serde]
async fn get_app_info(
    #[string] app_id: String,
    #[cppgc] call: Option<&CallAgent>,
) -> Result<Option<AppInfo>, AnyhowWrapperError> {
    let agent = agent_of(call)?;
    let language = own_language();
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    // A user asking about an app no language registered (`registerDNAs` probing for an
    // install, when it runs during a user's call) sees no app: never the main agent's.
    let cells = UserCells::global();
    let system = language
        .as_deref()
        .is_some_and(|language| cells.is_system_language(language));
    if !agent.is_main_agent() && !system && !cells.is_registered(&app_id) {
        return Ok(None);
    }
    let app_id = app_for_call(&interface, &app_id, language.as_deref(), &agent).await?;
    timeout(TIMEOUT_DURATION, interface.get_app_info(app_id))
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

/// The body of the `call_zome_function` op: waits for the conductor (`started`), then runs
/// the call with `budget` as its timeout.
///
/// The dispatch loop refuses the call if it is still queued when that timeout fires (#1133):
/// past that instant nobody is waiting for the result, so running it would only fire a
/// stale zome call after the caller has already reported a timeout.
pub(crate) async fn call_zome_within(
    started: impl Future<Output = Option<HolochainServiceInterface>>,
    budget: Duration,
    app_id: String,
    cell_name: String,
    zome_name: String,
    fn_name: String,
    payload: Option<ExternIO>,
) -> Result<ZomeCallResponse, AnyError> {
    let interface = started
        .await
        .ok_or_else(|| anyhow!("Holochain conductor not available"))?;
    // Taken after the conductor wait (up to `SERVICE_WAIT`), so it is the instant the
    // `timeout` below fires, however long the conductor took to start.
    let deadline = Some(Instant::now() + budget);
    timeout(
        budget,
        interface.call_zome_function(app_id, cell_name, zome_name, fn_name, payload, deadline),
    )
    .await
    .map_err(|_| anyhow!("Timeout error"))?
}

//TODO
//Have install app use lair to generate the membrane proof
#[op2(async(lazy))] // op2 v2.9: not fast-compatible (complex serde arg)
#[serde]
async fn call_zome_function(
    #[string] app_id: String,
    #[string] cell_name: String,
    #[string] zome_name: String,
    #[string] fn_name: String,
    #[serde] payload: Option<serde_json::Value>,
    #[cppgc] call: Option<&CallAgent>,
) -> Result<DecodedZomeCallResponse, AnyhowWrapperError> {
    // The caller's cell (#1775): taken before any await, the token is borrowed from JS.
    let agent = agent_of(call)?;
    let language = own_language();
    // Convert the JSON value to ExternIO (msgpack-encoded bytes) that Holochain expects.
    // We use json_to_msgpack_value to properly reconvert __binary markers and byte arrays
    // back to msgpack Binary type, preserving round-trip fidelity for Holochain hashes etc.
    let extern_payload = match payload {
        Some(val) => {
            let msgpack_val = json_to_msgpack_value(&val);
            let mut buf = Vec::new();
            rmpv::encode::write_value(&mut buf, &msgpack_val).map_err(|e| {
                AnyhowWrapperError::from(anyhow!("Failed to encode payload to msgpack: {}", e))
            })?;
            Some(ExternIO::from(buf))
        }
        None => None,
    };
    // One lookup for routing and the call: a second lookup could find a conductor that
    // started in between and dispatch an app id `app_for_call` never resolved.
    let interface = holochain_service_once_started().await;
    let app_id = match &interface {
        Some(interface) => app_for_call(interface, &app_id, language.as_deref(), &agent).await?,
        None => app_id,
    };
    let response = call_zome_within(
        std::future::ready(interface),
        TIMEOUT_DURATION,
        app_id,
        cell_name,
        zome_name,
        fn_name,
        extern_payload,
    )
    .await
    .map_err(AnyhowWrapperError::from)?;

    // Decode ExternIO bytes to JSON before returning to JS
    DecodedZomeCallResponse::from_zome_call_response(response)
}

#[op2(async(lazy), fast)]
#[serde]
async fn agent_infos() -> Result<Vec<String>, AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(TIMEOUT_DURATION, interface.agent_infos())
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

// op2 v2.9 lint: not fast-compatible (Vec<String> serde arg).
#[op2(async(lazy))]
async fn add_agent_infos(
    #[serde] agent_infos_payload: Vec<String>,
) -> Result<(), AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(
        TIMEOUT_DURATION,
        interface.add_agent_infos(agent_infos_payload),
    )
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async(lazy), fast)]
async fn remove_app(#[string] app_id: String) -> Result<(), AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(TIMEOUT_DURATION, interface.remove_app(app_id))
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

#[op2(async(lazy), fast)]
async fn shutdown() -> Result<(), AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(TIMEOUT_DURATION, interface.shutdown())
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

/// The node's first lair key, not the calling language's own agent key — see
/// `HolochainServiceInterface::get_agent_key`. Since #1099 gave every language its own
/// agent key, use `get_agent_key_for_language` instead unless a third-party language
/// specifically needs this one.
#[op2(async(lazy), fast)]
#[serde]
async fn get_agent_key() -> Result<HoloHash<Agent>, AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(TIMEOUT_DURATION, interface.get_agent_key())
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

/// Per-language agent key (issue #1099): distinct languages get distinct
/// cells even when they bundle the same DNA + network seed. See
/// `HolochainServiceInterface::agent_key_for_language` for the resolution
/// rules (stored mapping → adoption of an existing install → fresh key).
///
/// `language_address` is read from the isolate's own thread-local state, not
/// a JS argument: every extension global is reachable from any language's
/// code (see `languages::signing_reach_tests`), so a caller-supplied address
/// would let one language pre-bind — and mint a lair key and a DB row for —
/// another language's mapping before that language ever loads. `app_id` is
/// still caller-supplied (the DNA nick is a per-call choice), but is checked
/// against the real address below.
#[op2(async(lazy), fast)]
#[serde]
async fn get_agent_key_for_language(
    #[string] app_id: String,
) -> Result<HoloHash<Agent>, AnyhowWrapperError> {
    let language_address = try_with_isolate_state(|state| state.language_address.clone())
        .flatten()
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("languageAddress not set yet")))?;

    if !app_id.starts_with(&format!("{}-", language_address)) {
        return Err(AnyhowWrapperError::from(anyhow!(
            "app id {} does not belong to language {}",
            app_id,
            language_address
        )));
    }

    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(
        TIMEOUT_DURATION,
        interface.agent_key_for_language(&language_address, &app_id),
    )
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async(lazy), fast)]
#[string]
async fn pack_dna(#[string] path: String) -> Result<String, AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(TIMEOUT_DURATION, interface.pack_dna(path))
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

#[op2(async(lazy), fast)]
#[string]
async fn unpack_dna(#[string] path: String) -> Result<String, AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(TIMEOUT_DURATION, interface.unpack_dna(path))
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

#[op2(async(lazy), fast)]
#[string]
async fn pack_happ(#[string] path: String) -> Result<String, AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(TIMEOUT_DURATION, interface.pack_happ(path))
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

#[op2(async(lazy), fast)]
#[string]
async fn unpack_happ(#[string] path: String) -> Result<String, AnyhowWrapperError> {
    let interface = holochain_service_once_started()
        .await
        .ok_or_else(|| AnyhowWrapperError::from(anyhow!("Holochain conductor not available")))?;
    timeout(TIMEOUT_DURATION, interface.unpack_happ(path))
        .await
        .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
        .map_err(AnyhowWrapperError::from)
}

//Implement signal callbacks from dna/holochain to js
deno_core::extension!(
    holochain_service,
    ops = [start_holochain_conductor, log_dht_status, install_app, get_app_info, call_zome_function, agent_infos, add_agent_infos, remove_app, shutdown, get_agent_key, get_agent_key_for_language, pack_dna, unpack_dna, pack_happ, unpack_happ],
    esm_entry_point = "ext:holochain_service/holochain_service_extension.js",
    esm = [dir "src/holochain_service", "holochain_service_extension.js"]
);

#[cfg(test)]
mod tests {
    use super::json_to_msgpack_value;
    use holochain::prelude::ExternIO;
    use serde::Deserialize;
    use serde_json::json;

    fn encode(val: &serde_json::Value) -> ExternIO {
        let mut buf = Vec::new();
        rmpv::encode::write_value(&mut buf, &json_to_msgpack_value(val)).unwrap();
        ExternIO::from(buf)
    }

    #[test]
    fn integral_floats_become_msgpack_integers() {
        assert_eq!(
            json_to_msgpack_value(&json!(5.0)),
            rmpv::Value::Integer(5.into())
        );
        assert_eq!(
            json_to_msgpack_value(&json!(-5.0)),
            rmpv::Value::Integer((-5i64).into())
        );
        assert_eq!(
            json_to_msgpack_value(&json!(0.0)),
            rmpv::Value::Integer(0.into())
        );
        // A Date.now() timestamp.
        assert_eq!(
            json_to_msgpack_value(&json!(1794912243597.0)),
            rmpv::Value::Integer(1794912243597i64.into())
        );
        assert_eq!(
            json_to_msgpack_value(&json!(-9_223_372_036_854_775_808.0)),
            rmpv::Value::Integer(i64::MIN.into())
        );
    }

    #[test]
    fn non_integral_or_out_of_range_floats_stay_floats() {
        assert_eq!(json_to_msgpack_value(&json!(5.5)), rmpv::Value::F64(5.5));
        assert_eq!(
            json_to_msgpack_value(&json!(-0.25)),
            rmpv::Value::F64(-0.25)
        );
        // 2^63 and above do not fit in i64.
        assert_eq!(
            json_to_msgpack_value(&json!(9_223_372_036_854_775_808.0)),
            rmpv::Value::F64(9_223_372_036_854_775_808.0)
        );
        assert_eq!(json_to_msgpack_value(&json!(1e30)), rmpv::Value::F64(1e30));
        assert_eq!(
            json_to_msgpack_value(&json!(-1e30)),
            rmpv::Value::F64(-1e30)
        );
        // serde_json cannot hold NaN or infinities, so test the helper directly.
        assert!(matches!(
            super::integral_f64_to_msgpack(f64::NAN),
            rmpv::Value::F64(f) if f.is_nan()
        ));
        assert_eq!(
            super::integral_f64_to_msgpack(f64::INFINITY),
            rmpv::Value::F64(f64::INFINITY)
        );
        assert_eq!(
            super::integral_f64_to_msgpack(f64::NEG_INFINITY),
            rmpv::Value::F64(f64::NEG_INFINITY)
        );
    }

    #[test]
    fn json_integers_are_unchanged() {
        assert_eq!(
            json_to_msgpack_value(&json!(u64::MAX)),
            rmpv::Value::Integer(u64::MAX.into())
        );
        assert_eq!(
            json_to_msgpack_value(&json!(i64::MIN)),
            rmpv::Value::Integer(i64::MIN.into())
        );
    }

    #[test]
    fn nested_maps_and_arrays_are_converted() {
        let v = json_to_msgpack_value(&json!({
            "outer": { "count": 3.0, "ratio": 0.5 },
            "list": [1.0, 2.5, [7.0]]
        }));
        let rmpv::Value::Map(entries) = v else {
            panic!("expected map, got {v:?}");
        };
        let get = |entries: &[(rmpv::Value, rmpv::Value)], key: &str| {
            entries
                .iter()
                .find(|(k, _)| k.as_str() == Some(key))
                .map(|(_, v)| v.clone())
                .unwrap()
        };
        let rmpv::Value::Map(outer) = get(&entries, "outer") else {
            panic!("expected nested map");
        };
        assert_eq!(get(&outer, "count"), rmpv::Value::Integer(3.into()));
        assert_eq!(get(&outer, "ratio"), rmpv::Value::F64(0.5));
        assert_eq!(
            get(&entries, "list"),
            rmpv::Value::Array(vec![
                rmpv::Value::Integer(1.into()),
                rmpv::Value::F64(2.5),
                rmpv::Value::Array(vec![rmpv::Value::Integer(7.into())]),
            ])
        );
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct WithU64 {
        randomizer: u64,
        offset: i64,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct WithF64 {
        ratio: f64,
        whole: f64,
    }

    #[test]
    fn integral_float_decodes_into_u64_and_i64_fields() {
        let decoded: WithU64 = encode(&json!({ "randomizer": 1794912243597.0, "offset": -5.0 }))
            .decode()
            .unwrap();
        assert_eq!(
            decoded,
            WithU64 {
                randomizer: 1794912243597,
                offset: -5
            }
        );
    }

    #[test]
    fn typed_f64_field_still_decodes_an_integer() {
        let decoded: WithF64 = encode(&json!({ "ratio": 0.5, "whole": 5.0 }))
            .decode()
            .unwrap();
        assert_eq!(
            decoded,
            WithF64 {
                ratio: 0.5,
                whole: 5.0
            }
        );
    }
}

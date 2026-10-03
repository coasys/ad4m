//! Holochain: `holochain.conductor`, the embedded conductor. Operators read
//! and tune it; languages and other services install apps, call zomes and
//! receive signals through it.

use async_trait::async_trait;
use base64::Engine;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::OnceCell;

use super::{internal, params, require_admin, to_value, NoParams};
use crate::holochain_service::holochain_service_extension::direct;
use crate::holochain_service::{get_holochain_service, maybe_get_holochain_service};
use crate::services::builtin::{
    CallContext, EventEmitter, EventOwner, ServiceError, ServiceHealth, ServiceImplementation,
    StartContext,
};
use crate::services::interface::{Risk, Selection};
use crate::services::schema_export::{InterfaceBuilder, MethodOptions};

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct AgentInfosParams {
    pub agent_infos: Vec<String>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct InstallAppParams {
    /// Holochain's `InstallAppPayload`, as serde serialises it.
    pub payload: Value,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct AppIdParams {
    pub app_id: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct PathParams {
    pub path: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct CallZomeParams {
    pub app_id: String,
    pub cell_name: String,
    pub zome_name: String,
    pub fn_name: String,
    /// JSON; `Uint8Array`s as `{ "__binary": [...] }`.
    #[serde(default)]
    pub payload: Option<Value>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct CallZomeRawParams {
    pub app_id: String,
    pub cell_name: String,
    pub zome_name: String,
    pub fn_name: String,
    /// Msgpack, base64.
    #[serde(default)]
    #[schemars(extend("contentEncoding" = "base64"))]
    pub payload: Option<String>,
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", tag = "kind", content = "value")]
pub enum RawZomeResponse {
    /// The zome's msgpack answer, base64.
    Ok(String),
    NetworkError(String),
    CountersigningSession(String),
    /// The zome refused the call's capability.
    Unauthorized(String),
    AuthenticationFailed(String),
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SignParams {
    /// The agent key (39 bytes) that signs.
    pub key: Vec<u8>,
    /// Base64.
    #[schemars(extend("contentEncoding" = "base64"))]
    pub data: String,
}

/// `signal`: an app signal a cell emitted.
#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct Signal {
    /// `<dna hash hex>:<agent key hex>`.
    pub cell_id: String,
    pub dna_hash: Vec<u8>,
    pub agent_pub_key: Vec<u8>,
    pub zome_name: String,
    /// The signal decoded from msgpack; `Uint8Array`s as `__binary` markers.
    pub payload: Value,
}

fn opts(read: bool, long: bool) -> MethodOptions {
    MethodOptions {
        read,
        long,
        ..Default::default()
    }
}

fn invalid_payload(o: MethodOptions) -> MethodOptions {
    MethodOptions {
        errors: vec![("InvalidPayload", 422)],
        ..o
    }
}

pub fn conductor_interface() -> Value {
    InterfaceBuilder::new(
        "holochain.conductor",
        super::AUTHOR,
        "1.0.0",
        Selection::Executor,
        "The executor's embedded Holochain conductor.",
    )
    .action("READ", "See Holochain peers", "Read the conductor's agent infos and network metrics.", Risk::Safe)
    .action("PEERS", "Add Holochain peers", "Tell the conductor about other agents.", Risk::Write)
    .action("CALL", "Call Holochain zomes", "Call zome functions of installed apps.", Risk::Admin)
    .action("APPS", "Manage Holochain apps", "Install, enable, remove and pack Holochain apps and DNAs.", Risk::Admin)
    .action("KEYS", "Use Holochain keys", "Create agent keys and sign with them.", Risk::Admin)
    .action("SIGNALS", "Receive Holochain signals", "Receive every app signal of every cell.", Risk::Admin)
    .action("ADMIN", "Operate Holochain", "Restart and stop the conductor.", Risk::Admin)
    .method::<NoParams, Vec<String>>("agentInfos", "READ", "The conductor's agent infos (encoded).", opts(true, false))
    .method::<AgentInfosParams, bool>("addAgentInfos", "PEERS", "Add other agents' infos to the conductor.", opts(false, false))
    .method::<NoParams, String>("networkMetrics", "READ", "The conductor's network metrics (JSON text).", opts(true, false))
    .method::<NoParams, bool>("logNetworkStatus", "READ", "Write the conductor's network metrics to the executor log.", opts(false, false))
    .method::<InstallAppParams, Value>("installApp", "APPS", "Install an app; answers its `AppInfo`.", invalid_payload(opts(false, true)))
    .method::<AppIdParams, Option<Value>>("appInfo", "APPS", "An installed app's `AppInfo`.", opts(true, false))
    .method::<AppIdParams, bool>("enableApp", "APPS", "Enable an installed app.", opts(false, false))
    .method::<AppIdParams, bool>("removeApp", "APPS", "Uninstall an app.", opts(false, false))
    .method::<PathParams, String>("packDna", "APPS", "Pack a DNA directory; answers the bundle path.", opts(false, true))
    .method::<PathParams, String>("unpackDna", "APPS", "Unpack a DNA bundle; answers the directory.", opts(false, true))
    .method::<PathParams, String>("packHapp", "APPS", "Pack an hApp directory; answers the bundle path.", opts(false, true))
    .method::<PathParams, String>("unpackHapp", "APPS", "Unpack an hApp bundle; answers the directory.", opts(false, true))
    .method::<CallZomeParams, Value>(
        "callZome",
        "CALL",
        "Call a zome function with JSON; answers `{ type: \"Ok\" | \"NetworkError\" | \"CountersigningSession\", value }`.",
        opts(false, true),
    )
    .method::<CallZomeRawParams, RawZomeResponse>("callZomeRaw", "CALL", "Call a zome function with msgpack in and out.", invalid_payload(opts(false, true)))
    .method::<NoParams, Vec<u8>>("agentKey", "KEYS", "The conductor's agent key.", opts(true, false))
    .method::<NoParams, Vec<u8>>("newSignKeypair", "KEYS", "Create a random signing key pair; answers its agent key.", opts(false, false))
    .method::<SignParams, Vec<u8>>("signWithKey", "KEYS", "Sign data with a conductor-held key; answers the 64-byte signature.", invalid_payload(opts(false, false)))
    .method::<NoParams, bool>("restart", "ADMIN", "Wait until the conductor is up. Needs the admin credential.", opts(false, true))
    .method::<NoParams, bool>("restartService", "ADMIN", "Restart the conductor with its stored configuration.", opts(false, true))
    .method::<NoParams, bool>("shutdown", "ADMIN", "Stop the conductor.", opts(false, true))
    .event::<Signal>("signal", "SIGNALS", "An app signal a cell emitted.", Some("cellId"))
    .build()
}

#[derive(Default)]
pub struct Holochain {
    started: OnceCell<()>,
}

fn enabled() -> Result<(), ServiceError> {
    let enabled = crate::config::try_get_global_config()
        .and_then(|c| c.run_holochain)
        .unwrap_or(true);
    if enabled {
        Ok(())
    } else {
        Err(ServiceError::Unavailable(
            "Holochain is disabled on this executor (run_holochain=false)".into(),
        ))
    }
}

/// Conductor errors: not running → 503, everything else → 500.
fn conductor_error(e: impl std::fmt::Display) -> ServiceError {
    let message = e.to_string();
    if message.contains("not available") {
        ServiceError::Unavailable(message)
    } else {
        ServiceError::Internal(message)
    }
}

fn from_json<T: serde::de::DeserializeOwned>(v: Value, what: &str) -> Result<T, ServiceError> {
    serde_json::from_value(v)
        .map_err(|e| ServiceError::method("InvalidPayload", format!("{}: {}", what, e)))
}

fn decode_base64(s: &str) -> Result<Vec<u8>, ServiceError> {
    base64::prelude::BASE64_STANDARD
        .decode(s)
        .map_err(|e| ServiceError::method("InvalidPayload", format!("not base64: {}", e)))
}

/// Forward the conductor's app signals as `signal` events, for as long as
/// the executor runs (the conductor may start, stop and restart under it).
async fn forward_signals(events: EventEmitter) {
    use holochain::prelude::Signal as HcSignal;
    loop {
        let Some(conductor) = maybe_get_holochain_service().await else {
            tokio::time::sleep(std::time::Duration::from_millis(500)).await;
            continue;
        };
        let signal = conductor.stream_receiver.lock().await.recv().await;
        let Some(HcSignal::App {
            cell_id,
            zome_name,
            signal,
        }) = signal
        else {
            if signal.is_none() {
                tokio::time::sleep(std::time::Duration::from_millis(500)).await;
            }
            continue;
        };
        let dna_hash = cell_id.dna_hash().get_raw_39().to_vec();
        let agent_pub_key = cell_id.agent_pubkey().get_raw_39().to_vec();
        let hex = |b: &[u8]| b.iter().map(|x| format!("{:02x}", x)).collect::<String>();
        let bytes = signal.into_inner().as_bytes().to_vec();
        let payload = match rmpv::decode::read_value(&mut std::io::Cursor::new(&bytes)) {
            Ok(v) => {
                crate::holochain_service::holochain_service_extension::msgpack_value_to_json(v)
            }
            Err(e) => {
                log::warn!("Failed to decode signal payload from msgpack: {}", e);
                Value::Null
            }
        };
        let event = Signal {
            cell_id: format!("{}:{}", hex(&dna_hash), hex(&agent_pub_key)),
            dna_hash,
            agent_pub_key,
            zome_name: zome_name.to_string(),
            payload,
        };
        // Signals carry other agents' data: only admin sockets and the
        // executor's own consumers (`ServiceHost::watch`) receive them.
        if let Err(e) = events
            .emit(
                "signal",
                EventOwner::Executor,
                serde_json::to_value(event).unwrap_or_default(),
            )
            .await
        {
            log::error!("holochain signal dropped: {}", e);
        }
    }
}

#[async_trait]
impl ServiceImplementation for Holochain {
    async fn start(&self, ctx: StartContext) -> Result<(), String> {
        if enabled().is_ok() && self.started.set(()).is_ok() {
            tokio::spawn(forward_signals(ctx.events));
        }
        Ok(())
    }

    async fn stop(&self) -> Result<(), String> {
        Ok(())
    }

    async fn health(&self) -> ServiceHealth {
        if enabled().is_err() {
            return ServiceHealth::Degraded("Holochain is disabled on this executor".into());
        }
        match maybe_get_holochain_service().await {
            Some(_) => ServiceHealth::Running,
            None => ServiceHealth::Degraded("conductor not started yet".into()),
        }
    }

    async fn call(&self, method: &str, p: Value, ctx: CallContext) -> Result<Value, ServiceError> {
        enabled()?;
        // Operator actions need the admin credential beyond any grant;
        // reads and peer exchange do not.
        if !matches!(
            method,
            "agentInfos" | "addAgentInfos" | "networkMetrics" | "logNetworkStatus"
        ) {
            require_admin(&ctx)?;
        }
        match method {
            "agentInfos" => {
                let hc = maybe_get_holochain_service()
                    .await
                    .ok_or_else(|| conductor_error("Holochain conductor not available"))?;
                to_value(hc.agent_infos().await.map_err(internal)?)
            }
            "addAgentInfos" => {
                let p: AgentInfosParams = params(p)?;
                let hc = maybe_get_holochain_service()
                    .await
                    .ok_or_else(|| conductor_error("Holochain conductor not available"))?;
                hc.add_agent_infos(p.agent_infos).await.map_err(internal)?;
                to_value(true)
            }
            "networkMetrics" => {
                let hc = maybe_get_holochain_service()
                    .await
                    .ok_or_else(|| conductor_error("Holochain conductor not available"))?;
                to_value(hc.get_network_metrics().await.map_err(internal)?)
            }
            "logNetworkStatus" => {
                direct::log_network_status()
                    .await
                    .map_err(conductor_error)?;
                to_value(true)
            }
            "installApp" => {
                let p: InstallAppParams = params(p)?;
                let payload = from_json(p.payload, "InstallAppPayload")?;
                to_value(
                    direct::install_app(payload)
                        .await
                        .map_err(conductor_error)?,
                )
            }
            "appInfo" => {
                let p: AppIdParams = params(p)?;
                to_value(direct::app_info(p.app_id).await.map_err(conductor_error)?)
            }
            "enableApp" => {
                let p: AppIdParams = params(p)?;
                direct::enable_app(p.app_id)
                    .await
                    .map_err(conductor_error)?;
                to_value(true)
            }
            "removeApp" => {
                let p: AppIdParams = params(p)?;
                direct::remove_app(p.app_id)
                    .await
                    .map_err(conductor_error)?;
                to_value(true)
            }
            "packDna" | "unpackDna" | "packHapp" | "unpackHapp" => {
                let p: PathParams = params(p)?;
                let out = match method {
                    "packDna" => direct::pack_dna(p.path).await,
                    "unpackDna" => direct::unpack_dna(p.path).await,
                    "packHapp" => direct::pack_happ(p.path).await,
                    _ => direct::unpack_happ(p.path).await,
                };
                to_value(out.map_err(conductor_error)?)
            }
            "callZome" => {
                let p: CallZomeParams = params(p)?;
                let r = direct::call_zome_json(
                    p.app_id,
                    p.cell_name,
                    p.zome_name,
                    p.fn_name,
                    p.payload,
                )
                .await
                .map_err(conductor_error)?;
                to_value(r)
            }
            "callZomeRaw" => {
                use holochain::prelude::{ExternIO, ZomeCallResponse};
                let p: CallZomeRawParams = params(p)?;
                let p_fn_name = p.fn_name.clone();
                let payload = p
                    .payload
                    .as_deref()
                    .map(decode_base64)
                    .transpose()?
                    .map(ExternIO::from);
                let r =
                    direct::call_zome_raw(p.app_id, p.cell_name, p.zome_name, p.fn_name, payload)
                        .await
                        .map_err(conductor_error)?;
                to_value(match r {
                    ZomeCallResponse::Ok(io) => {
                        RawZomeResponse::Ok(base64::prelude::BASE64_STANDARD.encode(io.as_bytes()))
                    }
                    ZomeCallResponse::NetworkError(m) => RawZomeResponse::NetworkError(m),
                    ZomeCallResponse::CountersigningSession(m) => {
                        RawZomeResponse::CountersigningSession(m)
                    }
                    ZomeCallResponse::Unauthorized(..) => RawZomeResponse::Unauthorized(p_fn_name),
                    ZomeCallResponse::AuthenticationFailed(..) => {
                        RawZomeResponse::AuthenticationFailed(p_fn_name)
                    }
                })
            }
            "agentKey" => to_value(direct::agent_key().await.map_err(conductor_error)?),
            "newSignKeypair" => {
                to_value(direct::new_sign_keypair().await.map_err(conductor_error)?)
            }
            "signWithKey" => {
                let p: SignParams = params(p)?;
                let key = from_json(serde_json::to_value(p.key).map_err(internal)?, "agent key")?;
                let sig = direct::sign_with_key(key, decode_base64(&p.data)?)
                    .await
                    .map_err(conductor_error)?;
                to_value(sig)
            }
            "restart" => {
                let _ = get_holochain_service().await;
                to_value(true)
            }
            "restartService" => {
                crate::holochain_service::HolochainService::restart_service()
                    .await
                    .map_err(conductor_error)?;
                to_value(true)
            }
            "shutdown" => {
                direct::shutdown().await.map_err(conductor_error)?;
                to_value(true)
            }
            other => Err(internal(format!("holochain has no method {}", other))),
        }
    }
}

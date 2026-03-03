use deno_core::{anyhow::anyhow, op2};
use holochain::{
    conductor::api::AppInfo,
    prelude::{
        hash_type::Agent, ExternIO, HoloHash, InstallAppPayload, Signature, ZomeCallResponse,
    },
};
use log::error;
use serde::{Deserialize, Serialize};
use std::io::Cursor;
use std::time::Duration;
use tokio::time::timeout;

use super::get_holochain_service;
use crate::holochain_service::{HolochainService, LocalConductorConfig};
use crate::js_core::error::AnyhowWrapperError;

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
                rmpv::Value::F64(f)
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

const APP_INSTALL_TIMEOUT_DURATION: Duration = Duration::from_secs(20);

#[op2(async)]
async fn start_holochain_conductor(
    #[serde] config: LocalConductorConfig,
) -> Result<(), AnyhowWrapperError> {
    HolochainService::init(config).await?;
    Ok(())
}

#[op2(async)]
async fn log_dht_status() -> Result<(), AnyhowWrapperError> {
    let res = timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.log_network_metrics().await
    })
    .await;
    match res {
        Ok(_) => Ok(()),
        Err(_) => {
            error!("Timeout error logging dht status");
            Ok(())
        }
    }
}

#[op2(async)]
#[serde]
async fn install_app(
    #[serde] install_app_payload: InstallAppPayload,
) -> Result<AppInfo, AnyhowWrapperError> {
    timeout(APP_INSTALL_TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.install_app(install_app_payload).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
#[serde]
async fn get_app_info(#[string] app_id: String) -> Result<Option<AppInfo>, AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.get_app_info(app_id).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

//TODO
//Have install app use lair to generate the membrane proof
#[op2(async)]
#[serde]
async fn call_zome_function(
    #[string] app_id: String,
    #[string] cell_name: String,
    #[string] zome_name: String,
    #[string] fn_name: String,
    #[serde] payload: Option<serde_json::Value>,
) -> Result<DecodedZomeCallResponse, AnyhowWrapperError> {
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
    let response = timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface
            .call_zome_function(app_id, cell_name, zome_name, fn_name, extern_payload)
            .await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)?;

    // Decode ExternIO bytes to JSON before returning to JS
    DecodedZomeCallResponse::from_zome_call_response(response)
}

#[op2(async)]
#[serde]
async fn agent_infos() -> Result<Vec<String>, AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.agent_infos().await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
async fn add_agent_infos(
    #[serde] agent_infos_payload: Vec<String>,
) -> Result<(), AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.add_agent_infos(agent_infos_payload).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
async fn remove_app(#[string] app_id: String) -> Result<(), AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.remove_app(app_id).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
#[serde]
async fn sign_string(#[string] data: String) -> Result<Signature, AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.sign(data).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
async fn shutdown() -> Result<(), AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.shutdown().await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
#[serde]
async fn get_agent_key() -> Result<HoloHash<Agent>, AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.get_agent_key().await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
#[string]
async fn pack_dna(#[string] path: String) -> Result<String, AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.pack_dna(path).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
#[string]
async fn unpack_dna(#[string] path: String) -> Result<String, AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.unpack_dna(path).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
#[string]
async fn pack_happ(#[string] path: String) -> Result<String, AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.pack_happ(path).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
#[string]
async fn unpack_happ(#[string] path: String) -> Result<String, AnyhowWrapperError> {
    timeout(TIMEOUT_DURATION, async {
        let interface = get_holochain_service().await;
        interface.unpack_happ(path).await
    })
    .await
    .map_err(|_| AnyhowWrapperError::from(anyhow!("Timeout error")))?
    .map_err(AnyhowWrapperError::from)
}

//Implement signal callbacks from dna/holochain to js
deno_core::extension!(
    holochain_service,
    ops = [start_holochain_conductor, log_dht_status, install_app, get_app_info, call_zome_function, agent_infos, add_agent_infos, remove_app, sign_string, shutdown, get_agent_key, pack_dna, unpack_dna, pack_happ, unpack_happ],
    esm_entry_point = "ext:holochain_service/holochain_service_extension.js",
    esm = [dir "src/holochain_service", "holochain_service_extension.js"]
);

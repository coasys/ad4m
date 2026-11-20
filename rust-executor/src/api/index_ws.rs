//! Hosting index and `host.info` WS handlers (AR-07 4.6, AR-05 4.13).
//!
//! `index.*` are the operator's calls: the executor makes the HTTP calls to the index
//! (`crate::index_client`) with its main device key; the caller only presses the buttons.
//! `host.info` is what the index reads at each health check.

use std::sync::Arc;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::Mutex;
use ts_rs::TS;

use crate::agent::AgentContext;
use crate::identity::device::WalletSigner;
use crate::index_client::{
    IndexClient, IndexConfig, IndexError, IndexSession, ListedModel, ListingDraft, ListingText,
    OwnListing, PricingItem,
};
use crate::types::{ModelType, RequestContext};

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};

/// The `settings` row of [`IndexConfig`].
const CONFIG_KEY: &str = "hosting_index.config";

/// The open index session. In memory only (AR-07 4.3.3): a restart signs in again.
static SESSION: Mutex<Option<IndexSession>> = Mutex::const_new(None);

fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

pub fn load_config() -> Result<IndexConfig, WsRpcError> {
    let raw = crate::db_backend::db_backend()
        .get_setting(CONFIG_KEY)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    match raw {
        None => Ok(IndexConfig::default()),
        Some(raw) => serde_json::from_str(&raw)
            .map_err(|e| WsRpcError::internal(format!("the stored index config: {e}"))),
    }
}

/// The client of the configured index; refuses while none is set or it is off.
fn configured_client() -> Result<IndexClient, WsRpcError> {
    let config = load_config()?;
    if !config.enabled {
        return Err(WsRpcError::bad_request(
            "the hosting index is off; call index.configure with a url and enabled: true",
        ));
    }
    if config.url.is_empty() {
        return Err(WsRpcError::bad_request("no hosting index URL is set"));
    }
    IndexClient::new(&config.url).map_err(index_failure)
}

/// The identity this executor lists for: the operator record's `did:scid`.
fn operator_scid() -> Result<String, WsRpcError> {
    crate::db_backend::db_backend()
        .operator_record()
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .map(|r| r.scid)
        .ok_or_else(|| {
            WsRpcError::bad_request(
                "this executor has no operator; start it with `ad4m-executor init --operator <did:scid>`",
            )
        })
}

/// Maps an index failure to a status: the index's refusals keep their meaning, a down index is
/// 502.
fn index_failure(e: IndexError) -> WsRpcError {
    let (code, data) = match &e {
        IndexError::Refused { status, code, .. } => (
            match *status {
                401 | 403 => 403,
                400 | 422 => 422,
                429 => 429,
                _ => 502,
            },
            Some(serde_json::json!({ "indexStatus": status, "indexCode": code })),
        ),
        IndexError::Signing(_) => (500, None),
        _ => (502, None),
    };
    WsRpcError {
        code,
        message: e.to_string(),
        data,
    }
}

/// A live session, signing in when there is none.
async fn session(client: &IndexClient, fresh: bool) -> Result<IndexSession, WsRpcError> {
    let mut slot = SESSION.lock().await;
    if !fresh {
        if let Some(open) = slot.as_ref().filter(|s| s.is_live(now())) {
            return Ok(open.clone());
        }
    }
    let scid = operator_scid()?;
    let signer = WalletSigner::new(&AgentContext::Main).map_err(WsRpcError::internal)?;
    let opened = client.sign_in(&signer, &scid, now()).await;
    signer
        .check()
        .map_err(|e| WsRpcError::internal(format!("the wallet could not sign: {e}")))?;
    let opened = opened.map_err(index_failure)?;
    *slot = Some(opened.clone());
    Ok(opened)
}

/// Runs `call` with a session; when the index says the session is not valid, signs in once more
/// and runs it again.
async fn with_session<T, F, Fut>(client: &IndexClient, call: F) -> Result<T, WsRpcError>
where
    F: Fn(IndexSession) -> Fut,
    Fut: std::future::Future<Output = Result<T, IndexError>>,
{
    match call(session(client, false).await?).await {
        Err(e) if e.is_unauthorized() => call(session(client, true).await?)
            .await
            .map_err(index_failure),
        other => other.map_err(index_failure),
    }
}

/// The AI models and per-operation rates this executor shows in a listing (AR-07 4.6: the
/// operator does not type them).
fn models_and_rates() -> Result<(Vec<ListedModel>, Vec<PricingItem>), WsRpcError> {
    let backend = crate::db_backend::db_backend();
    let models = backend
        .get_models()
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .into_iter()
        .filter_map(|m| {
            let kind = match m.model_type {
                ModelType::Llm => "llm",
                ModelType::Embedding => "embedding",
                ModelType::Transcription => "transcription",
                // The index lists no speech models.
                ModelType::Speech => return None,
            };
            Some(ListedModel {
                name: m.name,
                kind: kind.into(),
            })
        })
        .collect();
    let rates = backend
        .get_host_rates()
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .into_iter()
        .map(|(description, price_in_hot)| PricingItem {
            description,
            price_in_hot,
            unit: "credits".into(),
        })
        .collect();
    Ok((models, rates))
}

// ── index.* ────────────────────────────────────────────────────────────────

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct IndexStatus {
    pub config: IndexConfig,
    /// The operator identity this executor lists for; `null` without an operator record.
    pub scid: Option<String>,
    /// The executor's main device key (`did:key`), the listing's `executorDid`.
    pub executor_did: Option<String>,
    /// `null` while the index is off. Else whether the index admits the operator.
    pub allowed: Option<bool>,
    /// The listing as the index shows it; `null` when there is none (or the index is off).
    pub listing: Option<OwnListing>,
}

async fn index_configure(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let config: IndexConfig = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let config = IndexConfig {
        url: config.url.trim().to_string(),
        enabled: config.enabled,
    };
    if config.enabled || !config.url.is_empty() {
        // A bad URL stops here, not at the first publish.
        IndexClient::new(&config.url).map_err(index_failure)?;
    }
    crate::db_backend::db_backend()
        .set_setting(
            CONFIG_KEY,
            &serde_json::to_string(&config).map_err(|e| WsRpcError::internal(e.to_string()))?,
        )
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    // Another index is another audience: the session is not carried over.
    *SESSION.lock().await = None;
    Ok(serde_json::to_value(config)?)
}

async fn index_status(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let config = load_config()?;
    let scid = crate::db_backend::db_backend()
        .operator_record()
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .map(|r| r.scid);
    let executor_did = crate::agent::did_for_context(&AgentContext::Main).ok();
    let mut status = IndexStatus {
        config: config.clone(),
        scid,
        executor_did,
        allowed: None,
        listing: None,
    };
    if config.enabled {
        let client = configured_client()?;
        match with_session(&client, |s| {
            let client = &client;
            async move { client.me(&s).await }
        })
        .await
        {
            Ok(me) => {
                status.allowed = Some(me.allowed);
                status.listing = me.listing;
            }
            // The operator's identity is not on the allow list: a state, not a failure.
            Err(e)
                if e.data
                    .as_ref()
                    .is_some_and(|d| d["indexCode"] == "not-allowed") =>
            {
                status.allowed = Some(false);
            }
            Err(e) => return Err(e),
        }
    }
    Ok(serde_json::to_value(status)?)
}

async fn index_publish(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let text: ListingText = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let client = configured_client()?;
    let (ai_models, rates) = models_and_rates()?;
    let draft = ListingDraft {
        text,
        ai_models,
        rates,
    };
    let listing = with_session(&client, |s| {
        let (client, draft) = (&client, &draft);
        async move { client.publish(&s, draft).await }
    })
    .await?;
    Ok(serde_json::to_value(listing)?)
}

async fn index_withdraw(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let client = configured_client()?;
    with_session(&client, |s| {
        let client = &client;
        async move { client.withdraw(&s).await }
    })
    .await?;
    Ok(Value::Bool(true))
}

// ── host.info ──────────────────────────────────────────────────────────────

/// What a host tells anyone (AR-05 4.13.1, the fields the hosting index reads). Tenants add the
/// rest (name, `runByCoasys`, `emailCheckRequired`) and take `signup` and `full` from their
/// settings.
#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostInfo {
    /// The operator's full `did:scid:coasys:1:...`.
    pub operator_did: String,
    /// This executor's main device key as `did:key` (AR-05-R54).
    pub executor_did: String,
    /// `anyone`, `by_invite` or `no_one`.
    pub signup: String,
    /// `signup == anyone` (AR-07-R50).
    pub admits_guests: bool,
    /// No charge for tenants.
    pub free_access: bool,
    /// Whether the host takes no more tenants.
    pub full: bool,
    pub version: String,
}

async fn host_info(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let backend = crate::db_backend::db_backend();
    // A host takes tenants only in multi-user mode (AR-05 4.13.3).
    if !backend.get_multi_user_enabled().unwrap_or(false) {
        return Err(WsRpcError {
            code: 422,
            message: "not-a-host: multi-user mode is off".into(),
            data: Some(serde_json::json!({ "code": "not-a-host" })),
        });
    }
    let executor_did = crate::agent::did_for_context(&AgentContext::Main)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(host_info_of(
        operator_scid()?,
        executor_did,
        backend.get_free_hosting_enabled().unwrap_or(true),
    ))?)
}

/// The host's description. Sign-up is open in multi-user mode until the tenant policy (slice
/// 3.D) can close it; no tenant limit exists yet, so the host is never full.
fn host_info_of(operator_did: String, executor_did: String, free_access: bool) -> HostInfo {
    let signup = "anyone".to_string();
    HostInfo {
        operator_did,
        executor_did,
        admits_guests: signup == "anyone",
        signup,
        free_access,
        full: false,
        version: crate::globals::AD4M_VERSION.clone(),
    }
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    // The operator sets which index to use and whether to call it.
    map.method::<IndexConfig, IndexConfig>("index.configure", index_configure)
        .guard(Guard::Operator);
    // Reads the listing from the index (signing in when needed).
    map.method::<NoParams, IndexStatus>("index.status", index_status)
        .read()
        .guard(Guard::Operator);
    // Publishes the listing, or updates it: models and rates come from this executor.
    map.method::<ListingText, OwnListing>("index.publish", index_publish)
        .guard(Guard::Operator);
    map.method::<NoParams, bool>("index.withdraw", index_withdraw)
        .guard(Guard::Operator);
    // The host's description. Public: the index reads it with no credentials.
    map.method::<NoParams, HostInfo>("host.info", host_info)
        .read()
        .guard(Guard::Open);
}

#[cfg(test)]
mod tests;

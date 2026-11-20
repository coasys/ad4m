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
    rates_hash, republish_due, IndexClient, IndexConfig, IndexError, IndexSession, ListedModel,
    ListingDraft, ListingText, LocalListing, OwnListing, PendingChange, PricingItem,
};
use crate::types::{ModelType, RequestContext};

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};

/// The `settings` row of [`IndexConfig`].
const CONFIG_KEY: &str = "hosting_index.config";

/// The `settings` row of [`LocalListing`].
const LISTING_KEY: &str = "index_listing";

/// The republisher looks at the models and rates this often.
const REPUBLISH_TICK: std::time::Duration = std::time::Duration::from_secs(20);

/// While listed, the republisher reads the index's view of the listing this often, so a change of
/// state (unreachable, delisted) reaches the operator as `index-listing-changed` without the
/// operator asking.
const REFRESH_S: u64 = 60 * 60;

/// After a failed index call the republisher waits this long before the next one (the index
/// limits a listing to 10 publishes an hour).
const RETRY_HOLD_S: u64 = 10 * 60;

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

/// Stores the config file's `hosting_index` as the executor's index config (AR-07 4.13: the
/// file's value is the runtime copy at each start). A URL the client would not use is refused.
pub fn seed_config(config: &IndexConfig) -> Result<(), String> {
    if config.enabled || !config.url.is_empty() {
        IndexClient::new(&config.url).map_err(|e| e.to_string())?;
    }
    let config = IndexConfig {
        url: config.url.trim().to_string(),
        enabled: config.enabled,
    };
    crate::db_backend::db_backend()
        .set_setting(
            CONFIG_KEY,
            &serde_json::to_string(&config).map_err(|e| e.to_string())?,
        )
        .map_err(|e| e.to_string())
}

/// What this executor remembers of its listing; `None` before the first publish.
pub fn load_local() -> Result<Option<LocalListing>, WsRpcError> {
    let raw = crate::db_backend::db_backend()
        .get_setting(LISTING_KEY)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    raw.map(|raw| {
        serde_json::from_str(&raw)
            .map_err(|e| WsRpcError::internal(format!("the stored listing: {e}")))
    })
    .transpose()
}

fn store_local(local: &LocalListing) -> Result<(), WsRpcError> {
    crate::db_backend::db_backend()
        .set_setting(
            LISTING_KEY,
            &serde_json::to_string(local).map_err(|e| WsRpcError::internal(e.to_string()))?,
        )
        .map_err(|e| WsRpcError::internal(e.to_string()))
}

/// Tells the operator the listing changed (`index-listing-changed`).
fn emit_listing(listing: &OwnListing) {
    crate::pubsub::get_global_pubsub_sync().publish_event(crate::events::Event::new(
        crate::events::Topic::IndexListingChanged,
        crate::events::Audience::Operator,
        listing,
    ));
}

/// Keeps the listing the index answered: the stored record follows its state and reason, and
/// the operator hears of a change (`force`: whatever the stored record says, as after a
/// publish or a withdraw). Returns whether the listing changed.
fn observe(listing: &OwnListing, force: bool) -> Result<bool, WsRpcError> {
    let known = load_local()?;
    let changed = force
        || known
            .as_ref()
            .is_none_or(|k| k.state != listing.state || k.reason != listing.reason);
    if !changed {
        return Ok(false);
    }
    let local = match known {
        Some(known) => LocalListing {
            state: listing.state.clone(),
            reason: listing.reason.clone(),
            ..known
        },
        // A listing the index has and this executor has no record of (the settings were lost):
        // what the index shows is what was last sent.
        None => LocalListing {
            listing_id: listing.listing_id.clone(),
            draft: ListingText {
                name: listing.name.clone(),
                description: listing.description.clone(),
                location: listing.location.clone(),
                url: listing.url.clone(),
            },
            state: listing.state.clone(),
            reason: listing.reason.clone(),
            published_at: now(),
            rates_hash: rates_hash(&listing.ai_models, &listing.rates),
        },
    };
    store_local(&local)?;
    emit_listing(listing);
    Ok(true)
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
    /// What this executor remembers of its listing: the operator's text and the last state the
    /// index answered. Shown while the index is down; `null` before the first publish.
    pub local: Option<LocalListing>,
    /// Why the index could not be asked (down, a refused sign-in, no operator); `null` when it
    /// answered or is off. `allowed` and `listing` are `null` then; `local` is what is known.
    pub index_error: Option<String>,
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
        local: load_local()?,
        index_error: None,
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
                if let Some(listing) = &me.listing {
                    observe(listing, false)?;
                    status.local = load_local()?;
                }
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
            // The index cannot be asked: the status still shows what is known.
            Err(e) => status.index_error = Some(e.message),
        }
    }
    Ok(serde_json::to_value(status)?)
}

/// Publishes (or updates) the listing with `text` and this executor's models and rates, and
/// keeps what the index answered.
async fn publish_listing(text: ListingText) -> Result<OwnListing, WsRpcError> {
    let client = configured_client()?;
    let (ai_models, rates) = models_and_rates()?;
    let sent_hash = rates_hash(&ai_models, &rates);
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
    store_local(&LocalListing {
        listing_id: listing.listing_id.clone(),
        draft: draft.text,
        state: listing.state.clone(),
        reason: listing.reason.clone(),
        published_at: now(),
        rates_hash: sent_hash,
    })?;
    emit_listing(&listing);
    Ok(listing)
}

async fn index_publish(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let text: ListingText = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    Ok(serde_json::to_value(publish_listing(text).await?)?)
}

/// The index's view of the listing, kept: `Some` when there is one.
async fn refresh_listing(
    client: &IndexClient,
    force: bool,
) -> Result<Option<OwnListing>, WsRpcError> {
    let me = with_session(client, |s| async move { client.me(&s).await }).await?;
    if let Some(listing) = &me.listing {
        observe(listing, force)?;
    }
    Ok(me.listing)
}

async fn index_withdraw(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let client = configured_client()?;
    with_session(&client, |s| {
        let client = &client;
        async move { client.withdraw(&s).await }
    })
    .await?;
    // The record says withdrawn whatever the next call answers; the operator hears of it from
    // the index's own view of the listing when it can be read.
    if let Some(known) = load_local()? {
        store_local(&LocalListing {
            state: "withdrawn".into(),
            reason: None,
            ..known
        })?;
    }
    if let Err(e) = refresh_listing(&client, true).await {
        log::warn!("index.withdraw: the listing could not be read back: {e}");
    }
    Ok(Value::Bool(true))
}

// ── Republishing (AR-07 4.6) ───────────────────────────────────────────────

/// What the republisher carries from one look to the next.
#[derive(Default)]
struct RepublishState {
    pending: Option<PendingChange>,
    /// No index call before this (unix seconds), after a failed one.
    hold_until: u64,
    /// When the index's view of the listing was last read.
    refreshed_at: u64,
}

/// One look: publishes again when the models or rates changed and stayed so for 60 s, or a day
/// has passed since the last publish; else reads the index's view once an hour. Does nothing
/// while the index is off or nothing was ever published.
async fn republish_tick(state: &mut RepublishState, now: u64) {
    let Ok(config) = load_config() else { return };
    if !config.enabled || config.url.is_empty() {
        state.pending = None;
        return;
    }
    let Ok(Some(local)) = load_local() else {
        return;
    };
    if now < state.hold_until {
        return;
    }
    let Ok((models, rates)) = models_and_rates() else {
        return;
    };
    let current = rates_hash(&models, &rates);
    let outcome = if republish_due(&local, &current, &mut state.pending, now) {
        let published = publish_listing(local.draft.clone()).await.map(|_| ());
        if published.is_ok() {
            state.pending = None;
            state.refreshed_at = now;
        }
        published
    } else if local.state != "withdrawn" && now.saturating_sub(state.refreshed_at) >= REFRESH_S {
        state.refreshed_at = now;
        match configured_client() {
            Ok(client) => refresh_listing(&client, false).await.map(|_| ()),
            Err(e) => Err(e),
        }
    } else {
        Ok(())
    };
    if let Err(e) = outcome {
        log::warn!("hosting index: {e}");
        state.hold_until = now + RETRY_HOLD_S;
    }
}

/// Starts the republisher: it keeps the listing current with this executor's models and rates
/// for as long as the executor runs.
pub fn spawn_republisher() {
    tokio::spawn(async {
        let mut state = RepublishState::default();
        loop {
            tokio::time::sleep(REPUBLISH_TICK).await;
            republish_tick(&mut state, now()).await;
        }
    });
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

//! The executor's side of `did:scid:coasys:1`: which
//! identity each local agent's device key speaks for, the key logs it checked, and the
//! `identity.*` calls. See `AGENTS.md` in this directory.
//!
//! The executor holds device keys only (the wallet key `main`, one per managed user); master
//! keys and the 12 words stay with the apps (`ad4m-identity-client`). Logs arrive only through
//! the agent language ([`backend`]) and are kept, after `fold`, in the executor database
//! ([`cache`]).

pub mod backend;
pub mod cache;
pub mod claim;
pub mod device;
pub mod head;
pub mod operator;
pub mod service;
pub mod types;

use std::path::Path;
use std::sync::{Arc, OnceLock};

use crate::api::ws_handler::WsRpcError;
use crate::events::{Audience, Event, Topic};
use backend::AgentLanguageBackend;
use cache::{CacheError, KelCache};
use service::IdentityService;
use types::IdentityForkEvent;

static SERVICE: OnceLock<Arc<IdentityService>> = OnceLock::new();

/// The identity settings of the executor's config (`identity.storeUrl`, `identity.storeKeys`).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct IdentitySettings {
    /// The store's base URL; the agent language's `storeUrl` setting. Unset: the language's
    /// default (the production store).
    pub store_url: Option<String>,
    /// The store keys (multikeys) a head must be signed by. Empty: no head is accepted.
    pub store_keys: Vec<String>,
}

/// Opens the key-log cache in the executor database at `db_path` and starts the module. Called
/// once at startup, after the database opens.
pub fn init(db_path: &Path, settings: &IdentitySettings) -> Result<(), CacheError> {
    let mut cache = KelCache::open(db_path, settings.store_keys.clone())?;
    cache.set_fork_listener(Box::new(publish_fork));
    if settings.store_keys.is_empty() {
        log::warn!(
            "identity: identity.storeKeys is not set, so no head from the identity store is accepted"
        );
    }
    let service = IdentityService {
        backend: Arc::new(AgentLanguageBackend),
        cache: Arc::new(cache),
        store_url: settings.store_url.clone(),
    };
    if SERVICE.set(Arc::new(service)).is_err() {
        log::warn!("identity: the module was started twice; keeping the first");
    }
    Ok(())
}

/// The module, once [`init`] ran.
pub fn service() -> Result<Arc<IdentityService>, WsRpcError> {
    SERVICE.get().cloned().ok_or_else(|| WsRpcError {
        code: 503,
        message: "unavailable: the identity module has not started".into(),
    })
}

/// `identity-fork`, to the sessions of the identity's local agents, when the cache finds two
/// logs for one identity.
fn publish_fork(did: &str, fork: &ad4m_identity_core::ForkEvidence) {
    let scid = cache::scid_from(did).to_string();
    let agents = crate::db::Ad4mDb::with_global_instance(|db| db.agents_of_identity(&scid))
        .unwrap_or_else(|e| {
            log::error!("identity-fork: could not read the identity's agents: {e}");
            vec![]
        });
    let payload = IdentityForkEvent {
        did: did.to_string(),
        seq: fork.seq,
        ours: fork.ours.clone(),
        theirs: fork.theirs.clone(),
    };
    let event = Event::new(Topic::IdentityFork, Audience::Dids(agents), payload);
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => {
            handle.spawn(async move {
                crate::pubsub::get_global_pubsub()
                    .await
                    .publish_event(event);
            });
        }
        Err(_) => log::error!("identity-fork: no runtime to publish on"),
    }
}

/// Fills the agent language's `storeUrl` setting from `identity.storeUrl` before the language
/// loads (it reads `languages/<address>/settings.json` at load). Other settings stay.
pub fn write_agent_language_settings(
    languages_dir: &Path,
    agent_language: &str,
    settings: &IdentitySettings,
) -> std::io::Result<()> {
    let Some(url) = &settings.store_url else {
        return Ok(());
    };
    let dir = languages_dir.join(agent_language);
    std::fs::create_dir_all(&dir)?;
    let path = dir.join("settings.json");
    let mut value = match std::fs::read_to_string(&path) {
        Ok(text) => serde_json::from_str::<serde_json::Value>(&text)
            .ok()
            .filter(|v| v.is_object())
            .unwrap_or_else(|| serde_json::json!({})),
        Err(_) => serde_json::json!({}),
    };
    value["storeUrl"] = serde_json::Value::String(url.clone());
    std::fs::write(&path, serde_json::to_string(&value)?)
}

/// The identity settings from the executor's global config.
pub fn settings_from_config() -> IdentitySettings {
    crate::config::GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .as_ref()
        .map(|c| IdentitySettings {
            store_url: c.identity_store_url.clone(),
            store_keys: c.identity_store_keys.clone().unwrap_or_default(),
        })
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_store_url_reaches_the_agent_languages_settings_and_keeps_the_others() {
        let dir = tempfile::tempdir().unwrap();
        let settings = IdentitySettings {
            store_url: Some("http://127.0.0.1:9".into()),
            store_keys: vec![],
        };
        write_agent_language_settings(dir.path(), "QmAgent", &settings).unwrap();
        let path = dir.path().join("QmAgent").join("settings.json");
        let read = || -> serde_json::Value {
            serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap()
        };
        assert_eq!(read()["storeUrl"], "http://127.0.0.1:9");

        std::fs::write(&path, r#"{"storagePath": "/x", "storeUrl": "old"}"#).unwrap();
        write_agent_language_settings(dir.path(), "QmAgent", &settings).unwrap();
        assert_eq!(read()["storeUrl"], "http://127.0.0.1:9");
        assert_eq!(read()["storagePath"], "/x");

        // No storeUrl configured: the language keeps its own.
        write_agent_language_settings(dir.path(), "QmAgent", &IdentitySettings::default()).unwrap();
        assert_eq!(read()["storeUrl"], "http://127.0.0.1:9");
    }
}

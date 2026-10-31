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
pub mod person;
pub mod resolver;
pub mod service;
pub mod src_fetch;
pub mod types;
pub mod verdict;

use std::collections::BTreeSet;
use std::path::Path;
use std::sync::{Arc, OnceLock};
use std::time::Duration;

use crate::api::ws_handler::WsRpcError;
use crate::events::{Audience, Event, Topic};
use crate::types::{Expression, LinkExpression};
use backend::AgentLanguageBackend;
use cache::{CacheError, KelCache};
pub use resolver::verify_expression;
use resolver::{CheckedLink, Resolver};
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
    /// `identity.allowPrivateSrc`: let a DID URL's `src` reach loopback and private addresses.
    /// Off by default; for tests and local development only.
    pub allow_private_src: bool,
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
    if settings.allow_private_src {
        log::warn!(
            "identity: identity.allowPrivateSrc is set, so a DID URL's src may reach loopback and private addresses"
        );
    }
    let backend: Arc<dyn backend::IdentityBackend> = Arc::new(AgentLanguageBackend);
    let cache = Arc::new(cache);
    let resolver = Resolver::new(
        cache.clone(),
        backend.clone(),
        settings.store_url.as_deref(),
        settings.allow_private_src,
    );
    let service = IdentityService {
        backend,
        cache,
        store_url: settings.store_url.clone(),
        resolver: Arc::new(resolver),
    };
    let service = Arc::new(service);
    if SERVICE.set(service.clone()).is_err() {
        log::warn!("identity: the module was started twice; keeping the first");
        return Ok(());
    }
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => {
            handle.spawn(run_recheck(service));
        }
        Err(_) => log::error!("identity: no runtime for the re-check of links"),
    }
    Ok(())
}

/// How often links whose signer's log could not be read are checked again.
pub const RECHECK_EVERY: Duration = Duration::from_secs(60);

/// The re-check of stored links: the links of each key whose verdict the cache changed (a
/// revocation, a key a log lists for the first time), and every [`RECHECK_EVERY`] the links
/// that could not be checked. Each re-check judges a link again from the key map and rewrites
/// it only when its verdict or person changed, so it is idempotent.
async fn run_recheck(service: Arc<IdentityService>) {
    use tokio::sync::broadcast::error::{RecvError, TryRecvError};
    let mut changes = service.cache.subscribe();
    let mut tick = tokio::time::interval(RECHECK_EVERY);
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    loop {
        tokio::select! {
            change = changes.recv() => {
                let mut signers = BTreeSet::new();
                match change {
                    Ok(change) => {
                        signers.insert(format!("did:key:{}", change.key));
                        loop {
                            match changes.try_recv() {
                                Ok(change) => {
                                    signers.insert(format!("did:key:{}", change.key));
                                }
                                Err(TryRecvError::Lagged(_)) => {
                                    signers.extend(listed_signers(&service));
                                }
                                Err(TryRecvError::Empty) | Err(TryRecvError::Closed) => break,
                            }
                        }
                    }
                    // Changes were missed: every key the map lists.
                    Err(RecvError::Lagged(_)) => signers.extend(listed_signers(&service)),
                    Err(RecvError::Closed) => return,
                }
                crate::perspectives::recheck_links(&crate::perspectives::Recheck::Signers(signers))
                    .await;
            }
            _ = tick.tick() => {
                crate::perspectives::recheck_links(&crate::perspectives::Recheck::CantCheck).await;
            }
        }
    }
}

fn listed_signers(service: &IdentityService) -> Vec<String> {
    service
        .cache
        .key_map()
        .listed_keys()
        .into_iter()
        .map(|key| format!("did:key:{key}"))
        .collect()
}

/// The verdict of a signed expression (a link) and its person, from the executor's key map,
/// with no I/O ([`resolver::check_signed_with`]). No space order is known here: a link signed
/// before its key's revocation counts.
pub fn check_signed<T: serde::Serialize>(expr: &Expression<T>) -> CheckedLink {
    resolver::check_signed_with(expr, &cache::global_key_map(), None, service::now())
}

/// Fills the key map for the signers of `links` before they are checked and stored: the key
/// index and the logs it names, through the agent language. Does nothing before [`init`].
pub async fn prepare_link_signers(links: &[LinkExpression]) {
    let Some(service) = SERVICE.get() else {
        return;
    };
    let signers: Vec<(String, Option<u64>)> = links
        .iter()
        .map(|l| (l.author.clone(), l.proof.anchor))
        .collect();
    service.resolver.prepare(&signers).await;
}

/// The anchor of a link that the local agent `agent_did` signs now: the trusted seq of its
/// identity's log, when the agent is bound to an identity (`identity_agents`) and that log
/// lists its key as a device key up to that seq. `None` otherwise: the link carries no anchor.
///
/// Read from the key map, with no I/O: the logs of local identities are loaded at start
/// ([`load_local_identities`]) and never leave the map, and each accepted head replaces its
/// log there, so a link signed after a newer head is accepted carries the new anchor.
pub fn anchor_for(agent_did: &str) -> Option<u64> {
    let service = SERVICE.get()?;
    let scid = {
        let db = crate::db::Ad4mDb::global_instance();
        let db = db.lock().ok()?;
        db.as_ref()?.agent_identity(agent_did).ok().flatten()?
    };
    anchor_in(service.cache.key_map(), agent_did, &scid)
}

/// [`anchor_for`] of `agent_did` bound to `scid`, from `key_map`.
fn anchor_in(key_map: &cache::KeyMap, agent_did: &str, scid: &str) -> Option<u64> {
    let log = key_map.log(scid)?;
    let key = resolver::multikey_of(agent_did)?;
    let id = log.state.device_key_id_of(&key.to_string())?;
    log.state
        .keys
        .get(id)
        .is_some_and(|r| r.added <= log.trusted)
        .then_some(log.trusted)
}

/// Loads the trusted log of every identity a local agent speaks for into the key map and pins
/// it there ([`KelCache::keep_local`]), so [`anchor_for`] knows it from the first link. Called
/// once at startup, after [`init`] and before the API and the perspectives start. A log that
/// cannot be read is logged and skipped: links then carry no anchor until its next accepted
/// head.
pub async fn load_local_identities() {
    let Some(service) = SERVICE.get() else {
        return;
    };
    let scids = match crate::db::Ad4mDb::with_global_instance(|db| db.local_identities()) {
        Ok(scids) => scids,
        Err(e) => {
            log::warn!("identity: could not read the local identities: {e}");
            return;
        }
    };
    keep_local_all(&service.cache, &scids).await;
}

async fn keep_local_all(cache: &KelCache, scids: &[String]) {
    for scid in scids {
        match cache.keep_local(scid).await {
            Ok(true) => {}
            Ok(false) => log::warn!(
                "identity: no trusted log of the local identity {scid} yet; its links carry no anchor until its next accepted head"
            ),
            Err(e) => log::warn!(
                "identity: the log of the local identity {scid} is unreadable ({e}); its links carry no anchor until its next accepted head"
            ),
        }
    }
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
///
/// The file is replaced atomically (a temporary file in the same directory, then a rename), so
/// a failure leaves the old file as it was. An error means the setting may not be in force: the
/// caller must not load the agent language then (see
/// `LanguageController::install_agent_language`).
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
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => serde_json::json!({}),
        Err(e) => return Err(e),
    };
    value["storeUrl"] = serde_json::Value::String(url.clone());
    replace_file(&path, serde_json::to_string(&value)?.as_bytes())
}

/// Writes `bytes` to a new file beside `path`, syncs it and renames it over `path`; on any
/// failure the new file is removed and `path` is untouched.
fn replace_file(path: &Path, bytes: &[u8]) -> std::io::Result<()> {
    use std::io::Write;
    let dir = path
        .parent()
        .ok_or_else(|| std::io::Error::other("the settings file has no directory"))?;
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let temp = dir.join(format!(".settings.json.{}.{nanos}.tmp", std::process::id()));
    let written = (|| {
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temp)?;
        file.write_all(bytes)?;
        file.sync_all()?;
        std::fs::rename(&temp, path)
    })();
    if written.is_err() {
        let _ = std::fs::remove_file(&temp);
    }
    written
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
            allow_private_src: c.identity_allow_private_src.unwrap_or(false),
        })
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ad4m_identity_core::build::Signer;
    use cache::tests::{accept_signed, on_file, store_key, trusting, TestLog};

    fn agent_of(log: &TestLog) -> String {
        format!("did:key:{}", log.device.key())
    }

    /// Adds a second device key to `log` at the next seq; the agent's d1 stays.
    fn add_device(log: &mut TestLog, seed: u8) {
        use ad4m_identity_core::build::{add_unsigned, cosign};
        use ad4m_identity_core::{BotScope, DeviceKey, KeyLife};
        let bot = ed25519_dalek::SigningKey::from_bytes(&[seed; 32]);
        let chain = log.chain();
        let device = DeviceKey {
            id: chain.state().next_device_id(),
            sign: bot.key(),
            enc: ad4m_identity_core::encode_x25519_multikey(&[seed; 32]),
            life: KeyLife::Bot(BotScope {
                can: vec!["read".into()],
                expires: 4_000_000_000,
            }),
        };
        let mut event = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
        cosign(&mut event, "m0", &log.master).unwrap();
        log.events.push(event);
    }

    /// The anchor of a bound agent is its log's trusted seq, read from the key map with no
    /// `await`; after a newer head is accepted, a link signed then carries the new seq.
    #[tokio::test]
    async fn the_anchor_is_the_trusted_seq_and_follows_a_newer_head() {
        let mut log = TestLog::new(61);
        let cache = trusting();
        let agent = agent_of(&log);
        assert_eq!(anchor_in(cache.key_map(), &agent, &log.scid()), None);
        accept_signed(&cache, &log.events).await;
        assert_eq!(anchor_in(cache.key_map(), &agent, &log.scid()), Some(0));

        add_device(&mut log, 62);
        // Events with no store head move nothing.
        cache.ingest(&log.scid(), &log.events[1..]).await.unwrap();
        assert_eq!(anchor_in(cache.key_map(), &agent, &log.scid()), Some(0));
        accept_signed(&cache, &log.events).await;
        assert_eq!(anchor_in(cache.key_map(), &agent, &log.scid()), Some(1));
    }

    /// A key the log lists only as its master, or not at all, signs with no anchor.
    #[tokio::test]
    async fn only_a_listed_device_key_gets_an_anchor() {
        let log = TestLog::new(63);
        let cache = trusting();
        accept_signed(&cache, &log.events).await;
        let master = format!("did:key:{}", log.master.key());
        assert_eq!(anchor_in(cache.key_map(), &master, &log.scid()), None);
        let stranger = format!("did:key:{}", TestLog::new(64).device.key());
        assert_eq!(anchor_in(cache.key_map(), &stranger, &log.scid()), None);
    }

    /// After a restart the key map is empty; the startup load puts every local identity's
    /// trusted log there, so the anchor is known with no read by the signer. A log that cannot
    /// be loaded is skipped.
    #[tokio::test]
    async fn the_startup_load_makes_the_anchor_known() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        let log = TestLog::new(65);
        {
            let cache = on_file(&path, vec![store_key().multikey()]);
            accept_signed(&cache, &log.events).await;
        }
        let cache = on_file(&path, vec![store_key().multikey()]);
        let agent = agent_of(&log);
        assert_eq!(anchor_in(cache.key_map(), &agent, &log.scid()), None);
        let unknown = TestLog::new(66).scid();
        keep_local_all(&cache, &[unknown, log.scid()]).await;
        assert_eq!(anchor_in(cache.key_map(), &agent, &log.scid()), Some(0));
    }

    #[test]
    fn the_store_url_reaches_the_agent_languages_settings_and_keeps_the_others() {
        let dir = tempfile::tempdir().unwrap();
        let settings = IdentitySettings {
            store_url: Some("http://127.0.0.1:9".into()),
            ..Default::default()
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

    fn store_url(url: &str) -> IdentitySettings {
        IdentitySettings {
            store_url: Some(url.into()),
            ..Default::default()
        }
    }

    fn temp_files_in(dir: &Path) -> Vec<String> {
        std::fs::read_dir(dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .filter(|name| name.ends_with(".tmp"))
            .collect()
    }

    #[test]
    fn a_settings_file_that_cannot_be_read_is_reported_not_replaced() {
        // `settings.json` is a directory: an error, not a silent fallback.
        let dir = tempfile::tempdir().unwrap();
        let lang = dir.path().join("QmAgent");
        std::fs::create_dir_all(lang.join("settings.json")).unwrap();
        assert!(
            write_agent_language_settings(dir.path(), "QmAgent", &store_url("http://x")).is_err()
        );
        assert!(lang.join("settings.json").is_dir());
        assert!(temp_files_in(&lang).is_empty());
    }

    #[test]
    fn a_failed_replace_leaves_the_old_file_whole_and_no_temporary_file() {
        let dir = tempfile::tempdir().unwrap();
        // The rename cannot replace a non-empty directory.
        let target = dir.path().join("settings.json");
        std::fs::create_dir_all(target.join("inner")).unwrap();
        assert!(replace_file(&target, b"{}").is_err());
        assert!(target.join("inner").is_dir());
        assert!(temp_files_in(dir.path()).is_empty());

        // A good replace swaps the whole file.
        let file = dir.path().join("other.json");
        std::fs::write(&file, "old").unwrap();
        replace_file(&file, b"new").unwrap();
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "new");
        assert!(temp_files_in(dir.path()).is_empty());
    }

    #[cfg(unix)]
    #[test]
    fn a_read_only_settings_directory_keeps_the_old_store_url_and_reports_it() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        let lang = dir.path().join("QmAgent");
        std::fs::create_dir_all(&lang).unwrap();
        let path = lang.join("settings.json");
        let old = r#"{"storeUrl":"http://old.store","storagePath":"/x"}"#;
        std::fs::write(&path, old).unwrap();
        std::fs::set_permissions(&lang, std::fs::Permissions::from_mode(0o555)).unwrap();
        // Root ignores the mode: the check holds only where the mode does.
        let enforced = std::fs::File::create(lang.join("probe")).is_err();
        let written =
            write_agent_language_settings(dir.path(), "QmAgent", &store_url("http://127.0.0.1:9"));
        std::fs::set_permissions(&lang, std::fs::Permissions::from_mode(0o755)).unwrap();
        if enforced {
            assert!(written.is_err());
            assert_eq!(std::fs::read_to_string(&path).unwrap(), old);
            assert!(temp_files_in(&lang).is_empty());
        }
    }
}

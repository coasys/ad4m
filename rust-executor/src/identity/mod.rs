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
pub mod custody;
pub mod device;
pub mod head;
pub mod operator;
pub mod person;
pub mod rekey;
pub mod resolver;
pub mod service;
pub mod src_fetch;
pub mod types;

use std::path::Path;
use std::sync::{Arc, OnceLock};

use crate::api::ws_handler::WsRpcError;
use crate::events::{Audience, Event, Topic};
use backend::AgentLanguageBackend;
use cache::{CacheError, KelCache};
pub use resolver::verify_expression;
use resolver::Resolver;
use service::IdentityService;
use types::IdentityForkEvent;

static SERVICE: OnceLock<Arc<IdentityService>> = OnceLock::new();

/// `identity.storeKeys` as the executor can use them: each a bare Ed25519 multikey (`z6Mk…`),
/// which a signed head's `store` names as `did:key:<multikey>` and `verify_head` compares
/// without the prefix. Any other form (a `did:key:` prefix, a space after the comma, an empty
/// string) never matches a head, while `KelCache::trusts_a_store` would say the executor trusts
/// one: `identity.publish` and `identity.claimOffer` would then commit at the store, the cache
/// would accept no head, and the agent's key would be taken for a log it can never bind to. So
/// the executor refuses to start with such an entry.
pub fn check_store_keys(keys: &[String]) -> Result<(), String> {
    match keys
        .iter()
        .find(|key| ad4m_hash_chain::decode_ed25519_multikey(key).is_none())
    {
        None => Ok(()),
        Some(bad) => Err(format!(
            "identity.storeKeys: {bad:?} is not an Ed25519 multikey; give the store's key as the \
             bare multikey (z6Mk…), without did:key: and without spaces"
        )),
    }
}

/// `identity.storeUrl` as the executor can use it: unset (the network's store, or none) or a
/// URL. The agent language reads an empty `storeUrl` as unset, so `""` would leave the language
/// without a store while the executor's resolver names one: the executor refuses to start with
/// it.
pub fn check_store_url(url: Option<&str>) -> Result<(), String> {
    match url {
        Some(url) if url.trim().is_empty() => Err(
            "identity.storeUrl is empty; give the store's base URL, or leave it out for the \
             network's store"
                .into(),
        ),
        _ => Ok(()),
    }
}

/// The identity store this executor uses and the keys it trusts for it, built by
/// [`IdentitySettings::from_config`] from the config and the network's seed (`Default` is the
/// empty value, no store and no key: for tests).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct IdentitySettings {
    /// The store's base URL; the agent language's `storeUrl` setting. `None`: no store (the
    /// network has none), so the language answers every identity call `invalid` (`store-url`).
    pub store_url: Option<String>,
    /// The store keys (multikeys) a head must be signed by. Empty: no head is accepted.
    pub store_keys: Vec<String>,
    /// `identity.allowPrivateSrc`: let a DID URL's `src` reach loopback and private addresses.
    /// Off by default; for tests and local development only.
    pub allow_private_src: bool,
}

impl IdentitySettings {
    /// The settings of `config` on the network whose seed names `seed_store` (its
    /// `identityStore`). The one place that decides the store and its keys; the config
    /// (`identity.storeUrl`, `identity.storeKeys`: config file, CLI or env) wins over the seed:
    /// - the URL: `identity.storeUrl`, else the seed's store, else none (no store);
    /// - the keys: `identity.storeKeys` set: exactly those (`[]` too, and for the seed's store
    ///   too); else `identity.storeUrl` set: none (the seed's keys never apply to a configured
    ///   URL); else the seed's keys, else none.
    ///
    /// Without keys [`missing_store_keys_warning`] warns at start and `identity.publish` /
    /// `identity.claimOffer` refuse `no-store-keys`.
    pub fn from_config(
        config: &crate::config::Ad4mConfig,
        seed_store: Option<&crate::runtime_service::IdentityStoreSeed>,
    ) -> Self {
        let store_url = config
            .identity_store_url
            .clone()
            .or_else(|| seed_store.map(|seed| seed.url.clone()));
        let store_keys = match (&config.identity_store_keys, &config.identity_store_url) {
            (Some(keys), _) => keys.clone(),
            (None, Some(_)) => vec![],
            (None, None) => seed_store.map(|seed| seed.keys.clone()).unwrap_or_default(),
        };
        IdentitySettings {
            store_url,
            store_keys,
            allow_private_src: config.identity_allow_private_src.unwrap_or(false),
        }
    }
}

/// The warning logged at start when the executor trusts no store key, naming why and what that
/// costs; `None` with keys. Without a store (the network's seed has no `identityStore` and the
/// config names none) it says the network has no identity store; with a store URL but no keys
/// it names the store and how to give its key.
pub fn missing_store_keys_warning(settings: &IdentitySettings) -> Option<String> {
    if !settings.store_keys.is_empty() {
        return None;
    }
    let consequence = "so this executor verifies no did:scid record: expressions by a did:scid \
         author are invalid, no device key speaks for a person, and identity.publish and \
         identity.claimOffer refuse with no-store-keys";
    Some(match &settings.store_url {
        None => format!(
            "identity: this network has no identity store (its bootstrap seed names no \
             identityStore and identity.storeUrl is unset), {consequence}."
        ),
        Some(url) => format!(
            "identity: no key of the identity store at {url} is trusted (identity.storeKeys is \
             empty), {consequence}. Set identity.storeKeys (--identity-store-keys) to the store's \
             Ed25519 multikey."
        ),
    })
}

/// Opens the key-log cache in the executor database at `db_path` and starts the module. Called
/// once at startup, after the database opens.
pub fn init(db_path: &Path, settings: &IdentitySettings) -> Result<(), CacheError> {
    let mut cache = KelCache::open(db_path, settings.store_keys.clone())?;
    cache.set_fork_listener(Box::new(publish_fork));
    if let Some(warning) = missing_store_keys_warning(settings) {
        log::warn!("{warning}");
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
    let agents = crate::db_backend::db_backend()
        .agents_of_identity(&scid)
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

/// Writes the agent language's `storeUrl` and `storeKeys` settings from [`IdentitySettings`]
/// (the config's store, else the network seed's) before the language loads (it reads
/// `languages/<address>/settings.json` at load). Called on every start: no store removes
/// `storeUrl` (the language has no default, so every identity call answers `invalid`
/// `store-url`), and empty `IdentitySettings::store_keys` removes `storeKeys` (the language
/// then trusts no head), so nothing an earlier run wrote stays in force. Other settings stay.
///
/// The file is replaced atomically (a temporary file in the same directory, then a rename), so
/// a failure leaves the old file as it was. An error means the settings may not be in force:
/// the caller must not load the agent language then (see
/// `LanguageController::install_agent_language`).
pub fn write_agent_language_settings(
    languages_dir: &Path,
    agent_language: &str,
    settings: &IdentitySettings,
) -> std::io::Result<()> {
    let dir = languages_dir.join(agent_language);
    std::fs::create_dir_all(&dir)?;
    let path = dir.join("settings.json");
    let mut fields = match std::fs::read_to_string(&path) {
        Ok(text) => serde_json::from_str::<serde_json::Map<String, serde_json::Value>>(&text)
            .unwrap_or_default(),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => serde_json::Map::new(),
        Err(e) => return Err(e),
    };
    match &settings.store_url {
        Some(url) => {
            fields.insert("storeUrl".into(), url.clone().into());
        }
        None => {
            fields.remove("storeUrl");
        }
    }
    if settings.store_keys.is_empty() {
        fields.remove("storeKeys");
    } else {
        fields.insert("storeKeys".into(), settings.store_keys.clone().into());
    }
    replace_file(&path, serde_json::to_string(&fields)?.as_bytes())
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

/// The identity settings from the executor's global config and the loaded network seed
/// ([`IdentitySettings::from_config`]).
pub fn settings_from_config() -> IdentitySettings {
    let seed_store = crate::runtime_service::RuntimeService::seed_identity_store();
    crate::config::GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .as_ref()
        .map(|config| IdentitySettings::from_config(config, seed_store.as_ref()))
        .unwrap_or_else(|| {
            IdentitySettings::from_config(
                &crate::config::Ad4mConfig::unprepared(),
                seed_store.as_ref(),
            )
        })
}

#[cfg(test)]
mod tests {
    use super::*;

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

        // No store: the earlier run's URL goes, so the language has none; other settings stay.
        write_agent_language_settings(dir.path(), "QmAgent", &IdentitySettings::default()).unwrap();
        assert!(read().get("storeUrl").is_none(), "{}", read());
        assert_eq!(read()["storagePath"], "/x");
    }

    /// `identity.storeKeys` becomes the language's `storeKeys`, and goes when unset.
    #[test]
    fn the_store_keys_reach_the_agent_languages_settings_and_go_when_unset() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("QmAgent").join("settings.json");
        let read = || -> serde_json::Value {
            serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap()
        };
        let settings = IdentitySettings {
            store_url: Some("http://127.0.0.1:9".into()),
            store_keys: vec!["z6MkA".into(), "z6MkB".into()],
            ..Default::default()
        };
        write_agent_language_settings(dir.path(), "QmAgent", &settings).unwrap();
        assert_eq!(read()["storeKeys"], serde_json::json!(["z6MkA", "z6MkB"]));

        let without_keys = IdentitySettings {
            store_keys: vec![],
            ..settings
        };
        write_agent_language_settings(dir.path(), "QmAgent", &without_keys).unwrap();
        assert!(read().get("storeKeys").is_none(), "{}", read());
        assert_eq!(read()["storeUrl"], "http://127.0.0.1:9");
    }

    /// An empty `identity.storeUrl` is refused (the language would read it as the default
    /// store); unset and a URL pass.
    #[test]
    fn an_empty_store_url_is_refused() {
        assert!(check_store_url(Some("")).is_err());
        assert!(check_store_url(Some("  ")).is_err());
        assert!(check_store_url(None).is_ok());
        assert!(check_store_url(Some("https://store.example")).is_ok());
    }

    /// Without store keys the executor warns at start: with a store URL, naming the store and
    /// `identity.storeKeys`; without a store, saying the network has none. With keys it says
    /// nothing.
    #[test]
    fn empty_store_keys_warn_naming_the_cause_and_the_consequence() {
        let configured =
            missing_store_keys_warning(&store_url("https://store.example")).expect("a warning");
        assert!(configured.contains("https://store.example"), "{configured}");
        assert!(
            configured.contains("verifies no did:scid record"),
            "{configured}"
        );
        assert!(configured.contains("identity.storeKeys"), "{configured}");

        let none = missing_store_keys_warning(&IdentitySettings::default()).expect("a warning");
        assert!(
            none.contains("this network has no identity store"),
            "{none}"
        );
        assert!(none.contains("verifies no did:scid record"), "{none}");
        assert!(!none.contains("Set identity.storeKeys"), "{none}");

        let keyed = IdentitySettings {
            store_keys: vec!["z6MkA".into()],
            ..store_url("https://store.example")
        };
        assert_eq!(missing_store_keys_warning(&keyed), None);
    }

    fn config(url: Option<&str>, keys: Option<&[&str]>) -> crate::config::Ad4mConfig {
        let mut config = crate::config::Ad4mConfig::unprepared();
        config.identity_store_url = url.map(String::from);
        config.identity_store_keys = keys.map(|keys| keys.iter().map(|k| k.to_string()).collect());
        config
    }

    /// A store as a seed names it.
    fn seed_store() -> crate::runtime_service::IdentityStoreSeed {
        crate::runtime_service::IdentityStoreSeed {
            url: "https://seed.store".into(),
            keys: vec!["z6MkSeed".into()],
        }
    }

    fn settings_file(dir: &Path) -> serde_json::Value {
        serde_json::from_str(
            &std::fs::read_to_string(dir.join("QmAgent").join("settings.json")).unwrap(),
        )
        .unwrap()
    }

    /// Neither `identity.storeUrl` nor `identity.storeKeys`: the seed's store and its keys.
    /// Nothing to warn of, and the agent language gets both.
    #[test]
    fn without_a_configured_store_the_seeds_store_and_keys_count() {
        let settings = IdentitySettings::from_config(&config(None, None), Some(&seed_store()));
        assert_eq!(settings.store_url.as_deref(), Some("https://seed.store"));
        assert_eq!(settings.store_keys, vec!["z6MkSeed".to_string()]);
        assert_eq!(missing_store_keys_warning(&settings), None);

        let dir = tempfile::tempdir().unwrap();
        write_agent_language_settings(dir.path(), "QmAgent", &settings).unwrap();
        let written = settings_file(dir.path());
        assert_eq!(written["storeUrl"], "https://seed.store");
        assert_eq!(written["storeKeys"], serde_json::json!(["z6MkSeed"]));
    }

    /// Neither configured nor in the seed (a network without a store): no store. The warning says
    /// the network has none, and the agent language gets no `storeUrl` and no `storeKeys`, even
    /// where an earlier run wrote them.
    #[test]
    fn without_a_configured_store_or_one_in_the_seed_there_is_no_store() {
        let settings = IdentitySettings::from_config(&config(None, None), None);
        assert_eq!(settings.store_url, None);
        assert!(settings.store_keys.is_empty());
        let warning = missing_store_keys_warning(&settings).expect("a warning");
        assert!(warning.contains("no identity store"), "{warning}");

        let dir = tempfile::tempdir().unwrap();
        let seeded = IdentitySettings::from_config(&config(None, None), Some(&seed_store()));
        write_agent_language_settings(dir.path(), "QmAgent", &seeded).unwrap();
        write_agent_language_settings(dir.path(), "QmAgent", &settings).unwrap();
        let written = settings_file(dir.path());
        assert!(written.get("storeUrl").is_none(), "{written}");
        assert!(written.get("storeKeys").is_none(), "{written}");
    }

    /// A configured `identity.storeUrl` without `identity.storeKeys` wins over the seed's store
    /// and gets no key (the seed's keys never apply to a configured URL, not even the seed's
    /// own URL written out), so it warns, naming the store.
    #[test]
    fn a_configured_store_url_without_keys_trusts_no_key() {
        for seed in [None, Some(seed_store())] {
            for url in ["http://127.0.0.1:8080", "https://seed.store"] {
                let settings =
                    IdentitySettings::from_config(&config(Some(url), None), seed.as_ref());
                assert_eq!(settings.store_url.as_deref(), Some(url));
                assert!(settings.store_keys.is_empty(), "{url} {seed:?}");
                let warning = missing_store_keys_warning(&settings).expect("a warning");
                assert!(warning.contains(url), "{warning}");
            }
        }
    }

    /// `identity.storeKeys` set: exactly those, over the seed's keys, an explicit empty list
    /// too. With `identity.storeUrl` they are that store's; without it, the seed's store's
    /// (no store without one).
    #[test]
    fn configured_store_keys_are_taken_as_given() {
        let keys = ["z6MkA", "z6MkB"];
        for seed in [None, Some(seed_store())] {
            let configured = IdentitySettings::from_config(
                &config(Some("http://127.0.0.1:8080"), Some(&keys)),
                seed.as_ref(),
            );
            assert_eq!(configured.store_keys, vec!["z6MkA", "z6MkB"]);
            assert_eq!(
                configured.store_url.as_deref(),
                Some("http://127.0.0.1:8080")
            );

            let keys_only =
                IdentitySettings::from_config(&config(None, Some(&keys)), seed.as_ref());
            assert_eq!(keys_only.store_keys, vec!["z6MkA", "z6MkB"]);
            assert_eq!(
                keys_only.store_url,
                seed.as_ref().map(|seed| seed.url.clone()),
            );

            let empty = IdentitySettings::from_config(&config(None, Some(&[])), seed.as_ref());
            assert!(empty.store_keys.is_empty(), "{seed:?}");
            assert!(missing_store_keys_warning(&empty).is_some());
        }
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

    /// A store key the executor cannot match against a head is refused before the executor
    /// starts, named in the error: the cache must never say it trusts a store it cannot check.
    #[test]
    fn store_keys_must_be_bare_ed25519_multikeys() {
        let key = crate::identity::head::test_store_key::TestStoreKey::new(1).multikey();
        assert_eq!(check_store_keys(&[]), Ok(()));
        assert_eq!(check_store_keys(&[key.clone()]), Ok(()));
        for bad in [
            format!(" {key}"),
            format!("did:key:{key}"),
            String::new(),
            "z6MkA".to_string(),
        ] {
            let err = check_store_keys(&[key.clone(), bad.clone()]).unwrap_err();
            assert!(err.contains(&format!("{bad:?}")), "{err}");
        }
    }
}

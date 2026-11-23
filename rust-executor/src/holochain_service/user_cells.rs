//! One Holochain cell per (language, managed user) (issue #1775).
//!
//! A language is loaded once and installs its hApps once, as the main agent, during its
//! `init` (`registerDNAs` -> `install_app`), under the main agent's key for that language
//! (#1099). That install stays exactly as it is: the main agent keeps its key, its app ids and
//! its cells, so existing data and neighbourhoods are untouched.
//!
//! A managed user's Holochain calls go to a cell of their own. On the user's first call in a
//! language, [`UserCells::app_for_call`] resolves the user's agent key for that language
//! ([`HolochainServiceInterface::agent_key_for`]: the stored mapping, else the key of the
//! user's app if it is installed already, else a new lair key, persisted) and installs the
//! main app's payload again under that key as app `<main app id>@<user DID>`. Same bundle,
//! same network seed, so the same DNA hash: the user's cell joins the same network and reads
//! the same DHT, but writes and signs as its own agent. A per-(app, user) lock makes two
//! concurrent first calls install once.
//!
//! The cells of user apps are routed for signals ([`UserCells::route_for_cell`]): the signal
//! loop hands a user cell's signal to the language as that user.
//!
//! System languages (agent, language, perspective, neighbourhood languages and every link
//! language the executor loads as a system language) never get user cells: their calls stay
//! on the main agent's app whoever makes them.
//!
//! The conductor side sits behind [`CellBackend`], so the routing is tested without one.
//!
//! [`HolochainServiceInterface::agent_key_for`]: super::HolochainServiceInterface::agent_key_for

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex as StdMutex, RwLock};

use async_trait::async_trait;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use holochain::prelude::{AgentPubKey, CellId, InstallAppPayload};
use tokio::sync::Mutex;

use crate::agent::AgentContext;

/// What the routing needs to know about an installed app.
#[derive(Clone, Debug)]
pub(crate) struct InstalledApp {
    pub app_id: String,
    pub agent: AgentPubKey,
    pub cells: Vec<CellId>,
}

impl From<&holochain::conductor::api::AppInfo> for InstalledApp {
    fn from(info: &holochain::conductor::api::AppInfo) -> Self {
        use holochain::conductor::api::CellInfo;
        let cells = info
            .cell_info
            .values()
            .flatten()
            .filter_map(|cell| match cell {
                CellInfo::Provisioned(c) => Some(c.cell_id.clone()),
                CellInfo::Cloned(c) => Some(c.cell_id.clone()),
                CellInfo::Stem(_) => None,
            })
            .collect();
        InstalledApp {
            app_id: info.installed_app_id.clone(),
            agent: info.agent_pub_key.clone(),
            cells,
        }
    }
}

/// The conductor and the settings store, as the routing uses them.
#[async_trait]
pub(crate) trait CellBackend: Send + Sync {
    async fn app(&self, app_id: &str) -> Result<Option<InstalledApp>, AnyError>;
    async fn install(&self, payload: InstallAppPayload) -> Result<InstalledApp, AnyError>;
    /// The agent key of `agent` in `language`, resolved and persisted
    /// ([`super::HolochainServiceInterface::agent_key_for`]).
    async fn agent_key(
        &self,
        language: &str,
        agent: &AgentContext,
        adopt_app_id: &str,
    ) -> Result<AgentPubKey, AnyError>;
    /// The agent key stored for `agent` in `language`, if one is; never resolves a new one.
    fn stored_agent_key(
        &self,
        language: &str,
        agent: &AgentContext,
    ) -> Result<Option<AgentPubKey>, AnyError>;
    fn setting(&self, key: &str) -> Result<Option<String>, AnyError>;
    fn set_setting(&self, key: &str, value: &str) -> Result<(), AnyError>;
}

/// The key a cell's signals are routed by: `hex(dna hash):hex(agent key)`, the format of
/// `HOLOCHAIN_SIGNAL_HANDLERS` and of the JS signal dispatcher.
pub(crate) fn cell_key(cell: &CellId) -> String {
    let hex = |bytes: &[u8]| bytes.iter().map(|b| format!("{b:02x}")).collect::<String>();
    format!(
        "{}:{}",
        hex(cell.dna_hash().get_raw_39()),
        hex(cell.agent_pubkey().get_raw_39())
    )
}

/// The app id of `did`'s copy of the main app `app_id`.
pub(crate) fn user_app_id(app_id: &str, did: &str) -> String {
    format!("{app_id}@{did}")
}

/// The setting that lists the users with an agent key in `language`, so their apps' signal
/// routes come back after a restart.
fn users_setting(language: &str) -> String {
    format!("language_agent_users:{language}")
}

/// A main app a language registered: which language, and how it was installed.
#[derive(Clone)]
struct MainApp {
    language: String,
    payload: InstallAppPayload,
}

/// Where a user cell's signals go.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct UserRoute {
    pub language: String,
    pub did: String,
}

#[derive(Default)]
pub(crate) struct UserCells {
    /// Main apps by the app id the language calls them by.
    main_apps: StdMutex<HashMap<String, MainApp>>,
    /// (main app id, DID) -> the user's installed app id, once known.
    user_apps: StdMutex<HashMap<(String, String), String>>,
    /// Serialises the install of a user's app per (main app, DID).
    locks: StdMutex<HashMap<String, Arc<Mutex<()>>>>,
    /// User cells' signal routes, by [`cell_key`].
    routes: RwLock<HashMap<String, UserRoute>>,
    /// Languages loaded as system languages: their calls always use the main app.
    system_languages: RwLock<HashSet<String>>,
}

lazy_static::lazy_static! {
    static ref USER_CELLS: UserCells = UserCells::default();
}

impl UserCells {
    pub(crate) fn global() -> &'static UserCells {
        &USER_CELLS
    }

    fn lock_for(&self, name: String) -> Arc<Mutex<()>> {
        self.locks
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .entry(name)
            .or_default()
            .clone()
    }

    /// Marks `language` as a system language: no user cells, every call on the main app.
    pub(crate) fn mark_system_language(&self, language: &str) {
        self.system_languages
            .write()
            .unwrap_or_else(|p| p.into_inner())
            .insert(language.to_string());
    }

    pub(crate) fn is_system_language(&self, language: &str) -> bool {
        self.system_languages
            .read()
            .unwrap_or_else(|p| p.into_inner())
            .contains(language)
    }

    /// Records the main agent's install of `app_id` (the id the language calls it by) for
    /// `language`, so a user's call can install the same app under the user's key.
    pub(crate) fn remember_main_app(
        &self,
        app_id: &str,
        language: &str,
        payload: InstallAppPayload,
    ) {
        self.main_apps
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .insert(
                app_id.to_string(),
                MainApp {
                    language: language.to_string(),
                    payload,
                },
            );
    }

    /// The users that have an agent key in `language`, as stored.
    fn users_of(&self, backend: &dyn CellBackend, language: &str) -> Vec<String> {
        backend
            .setting(&users_setting(language))
            .ok()
            .flatten()
            .and_then(|json| serde_json::from_str::<Vec<String>>(&json).ok())
            .unwrap_or_default()
    }

    fn add_user(
        &self,
        backend: &dyn CellBackend,
        language: &str,
        did: &str,
    ) -> Result<(), AnyError> {
        let mut users = self.users_of(backend, language);
        if !users.iter().any(|u| u == did) {
            users.push(did.to_string());
            backend.set_setting(&users_setting(language), &serde_json::to_string(&users)?)?;
        }
        Ok(())
    }

    /// After a restart: routes the signals of the user apps of main app `app_id` that are
    /// installed already, so they reach their users before each user's next call.
    pub(crate) async fn restore_user_routes(&self, backend: &dyn CellBackend, app_id: &str) {
        let Some(language) = self.main_app(app_id).map(|m| m.language) else {
            return;
        };
        for did in self.users_of(backend, &language) {
            let id = user_app_id(app_id, &did);
            if let Ok(Some(app)) = backend.app(&id).await {
                let agent = AgentContext::managed(did.clone());
                if let Err(e) = Self::check_user_app(backend, &app, &language, &agent) {
                    log::warn!("Not restoring {id}: {e}");
                    continue;
                }
                self.add_routes(&app, &language, &did);
                self.user_apps
                    .lock()
                    .unwrap_or_else(|p| p.into_inner())
                    .insert((app_id.to_string(), did.clone()), app.app_id.clone());
            }
        }
    }

    /// Whether a language registered `app_id` as one of its main apps.
    pub(crate) fn is_registered(&self, app_id: &str) -> bool {
        self.main_app(app_id).is_some()
    }

    fn main_app(&self, app_id: &str) -> Option<MainApp> {
        self.main_apps
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .get(app_id)
            .cloned()
    }

    fn cached_user_app(&self, app_id: &str, did: &str) -> Option<String> {
        self.user_apps
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .get(&(app_id.to_string(), did.to_string()))
            .cloned()
    }

    /// The app a Holochain call of `agent` to app `app_id`, made by code of `language`, goes
    /// to. The main agent, and every call of a system language, use `app_id` itself, as
    /// before #1775. A managed user uses their own copy of the app, installed on first use.
    /// A user's call to an app that `language` did not register is refused: it would
    /// otherwise reach the main agent's cell (or another language's).
    pub(crate) async fn app_for_call(
        &self,
        backend: &dyn CellBackend,
        app_id: &str,
        language: Option<&str>,
        agent: &AgentContext,
    ) -> Result<String, AnyError> {
        let Some(did) = agent.managed_did() else {
            return Ok(app_id.to_string());
        };
        if let Some(language) = language {
            if self.is_system_language(language) {
                return Ok(app_id.to_string());
            }
        }
        let main = self.main_app(app_id).ok_or_else(|| {
            anyhow!(
                "Holochain app {app_id} was not registered by a language, so it has no cell for \
                 user {did}"
            )
        })?;
        match language {
            Some(language) if language == main.language => {}
            Some(language) => {
                return Err(anyhow!(
                    "language {language} may not call Holochain app {app_id} of language {}",
                    main.language
                ))
            }
            None => {
                return Err(anyhow!(
                    "a call for user {did} outside any language may not use Holochain app {app_id}"
                ))
            }
        }
        if let Some(id) = self.cached_user_app(app_id, did) {
            return Ok(id);
        }
        let lock = self.lock_for(format!("{app_id}|{did}"));
        let _guard = lock.lock().await;
        // A call that waited on the lock finds the install the first one made.
        if let Some(id) = self.cached_user_app(app_id, did) {
            return Ok(id);
        }
        let wanted = user_app_id(app_id, did);
        let app = match backend.app(&wanted).await? {
            Some(app) => {
                Self::check_user_app(backend, &app, &main.language, agent)?;
                app
            }
            None => {
                let key = backend.agent_key(&main.language, agent, &wanted).await?;
                self.add_user(backend, &main.language, did)?;
                let mut payload = main.payload.clone();
                payload.agent_key = Some(key);
                payload.installed_app_id = Some(wanted.clone());
                let started = std::time::Instant::now();
                let app = backend.install(payload).await?;
                log::info!(
                    "Installed Holochain app {} for {did} (agent {}) in language {} in {} ms",
                    app.app_id,
                    app.agent,
                    main.language,
                    started.elapsed().as_millis()
                );
                app
            }
        };
        if app.app_id != wanted {
            // `install_app` recovers a `CellAlreadyExists` by answering the app that owns the
            // cell. For a user's own fresh key that is a cell of another app: refuse it.
            return Err(anyhow!(
                "the cell for {did} in Holochain app {app_id} belongs to app {}",
                app.app_id
            ));
        }
        self.add_routes(&app, &main.language, did);
        self.user_apps
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .insert((app_id.to_string(), did.to_string()), app.app_id.clone());
        Ok(app.app_id)
    }

    /// A user app is used only if it carries the key stored for its (language, user): the one
    /// the per-user installer resolved and installed it under. Anything else (an app installed
    /// by other means, under another key) would make the user's calls sign as another agent.
    fn check_user_app(
        backend: &dyn CellBackend,
        app: &InstalledApp,
        language: &str,
        agent: &AgentContext,
    ) -> Result<(), AnyError> {
        match backend.stored_agent_key(language, agent)? {
            Some(key) if key == app.agent => Ok(()),
            Some(key) => Err(anyhow!(
                "Holochain app {} has agent {}, not the key {key} of its user",
                app.app_id,
                app.agent
            )),
            None => Err(anyhow!(
                "Holochain app {} was not installed by its user's installer: no key is stored \
                 for its user",
                app.app_id
            )),
        }
    }

    fn add_routes(&self, app: &InstalledApp, language: &str, did: &str) {
        let mut routes = self.routes.write().unwrap_or_else(|p| p.into_inner());
        for cell in &app.cells {
            routes.insert(
                cell_key(cell),
                UserRoute {
                    language: language.to_string(),
                    did: did.to_string(),
                },
            );
        }
    }

    /// The language and user a user cell's signal goes to; `None` for any other cell.
    pub(crate) fn route_for_cell(&self, cell_key: &str) -> Option<UserRoute> {
        self.routes
            .read()
            .unwrap_or_else(|p| p.into_inner())
            .get(cell_key)
            .cloned()
    }

    /// Forgets the routes and cached apps of `language` (it was removed). Its keys and
    /// installed apps stay: a reinstall under the same address reuses them.
    pub(crate) fn forget_language(&self, language: &str) {
        let mut main_apps = self.main_apps.lock().unwrap_or_else(|p| p.into_inner());
        let gone: Vec<String> = main_apps
            .iter()
            .filter(|(_, m)| m.language == language)
            .map(|(id, _)| id.clone())
            .collect();
        main_apps.retain(|_, m| m.language != language);
        drop(main_apps);
        self.user_apps
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .retain(|(app_id, _), _| !gone.contains(app_id));
        self.routes
            .write()
            .unwrap_or_else(|p| p.into_inner())
            .retain(|_, r| r.language != language);
    }
}

/// The real backend: the running conductor and Ad4mDb's settings.
pub(crate) struct ConductorBackend(pub super::HolochainServiceInterface);

#[async_trait]
impl CellBackend for ConductorBackend {
    async fn app(&self, app_id: &str) -> Result<Option<InstalledApp>, AnyError> {
        Ok(self
            .0
            .get_app_info(app_id.to_string())
            .await?
            .as_ref()
            .map(InstalledApp::from))
    }

    async fn install(&self, payload: InstallAppPayload) -> Result<InstalledApp, AnyError> {
        let info = tokio::time::timeout(
            super::holochain_service_extension::APP_INSTALL_TIMEOUT_DURATION,
            self.0.install_app(payload),
        )
        .await
        .map_err(|_| anyhow!("Timeout installing a user's Holochain app"))??;
        Ok(InstalledApp::from(&info))
    }

    async fn agent_key(
        &self,
        language: &str,
        agent: &AgentContext,
        adopt_app_id: &str,
    ) -> Result<AgentPubKey, AnyError> {
        self.0.agent_key_for(language, agent, adopt_app_id).await
    }

    fn stored_agent_key(
        &self,
        language: &str,
        agent: &AgentContext,
    ) -> Result<Option<AgentPubKey>, AnyError> {
        super::HolochainServiceInterface::stored_agent_key_for(language, agent)
    }

    fn setting(&self, key: &str) -> Result<Option<String>, AnyError> {
        crate::db_backend::db_backend()
            .get_setting(key)
            .map_err(|e| anyhow!("{e}"))
    }

    fn set_setting(&self, key: &str, value: &str) -> Result<(), AnyError> {
        crate::db_backend::db_backend()
            .set_setting(key, value)
            .map_err(|e| anyhow!("{e}"))
    }
}

#[cfg(test)]
#[path = "user_cells_tests.rs"]
mod user_cells_tests;

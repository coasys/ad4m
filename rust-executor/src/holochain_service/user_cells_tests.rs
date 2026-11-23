use super::*;
use holochain::prelude::{AppBundleSource, DnaHash};

/// A conductor and settings store in memory. Every installed app has one cell, of `DNA`.
#[derive(Default)]
struct Backend {
    apps: StdMutex<HashMap<String, InstalledApp>>,
    settings: StdMutex<HashMap<String, String>>,
    installs: StdMutex<Vec<InstallAppPayload>>,
    keys: std::sync::atomic::AtomicU8,
}

fn dna() -> DnaHash {
    DnaHash::from_raw_32(vec![0xD0; 32])
}

#[async_trait]
impl CellBackend for Backend {
    async fn app(&self, app_id: &str) -> Result<Option<InstalledApp>, AnyError> {
        Ok(self.apps.lock().unwrap().get(app_id).cloned())
    }

    async fn install(&self, payload: InstallAppPayload) -> Result<InstalledApp, AnyError> {
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        let agent = payload
            .agent_key
            .clone()
            .expect("a user install names its key");
        let app = InstalledApp {
            app_id: payload.installed_app_id.clone().unwrap(),
            agent: agent.clone(),
            cells: vec![CellId::new(dna(), agent)],
        };
        self.installs.lock().unwrap().push(payload);
        self.apps
            .lock()
            .unwrap()
            .insert(app.app_id.clone(), app.clone());
        Ok(app)
    }

    async fn agent_key(
        &self,
        language: &str,
        agent: &AgentContext,
        _adopt_app_id: &str,
    ) -> Result<AgentPubKey, AnyError> {
        let setting = format!("key:{language}:{:?}", agent.managed_did());
        if let Some(key) = self.settings.lock().unwrap().get(&setting) {
            return Ok(AgentPubKey::try_from(key.as_str()).unwrap());
        }
        let n = self.keys.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1;
        let key = AgentPubKey::from_raw_32(vec![n; 32]);
        self.set_setting(&setting, &key.to_string())?;
        Ok(key)
    }

    fn setting(&self, key: &str) -> Result<Option<String>, AnyError> {
        Ok(self.settings.lock().unwrap().get(key).cloned())
    }

    fn set_setting(&self, key: &str, value: &str) -> Result<(), AnyError> {
        self.settings
            .lock()
            .unwrap()
            .insert(key.to_string(), value.to_string());
        Ok(())
    }
}

fn payload(app_id: &str) -> InstallAppPayload {
    InstallAppPayload {
        source: AppBundleSource::Path("/nonexistent/acorn.happ".into()),
        agent_key: Some(AgentPubKey::from_raw_32(vec![0xAA; 32])),
        installed_app_id: Some(app_id.to_string()),
        network_seed: Some("project-1".into()),
        roles_settings: None,
        ignore_genesis_failure: false,
        restore_from_dht: false,
    }
}

const LANG: &str = "QmAcorn";
const APP: &str = "QmAcorn-acorn";
const ALICE: &str = "did:key:alice";
const BOB: &str = "did:key:bob";

fn alice() -> AgentContext {
    AgentContext::managed(ALICE.into())
}

fn bob() -> AgentContext {
    AgentContext::managed(BOB.into())
}

#[tokio::test]
async fn the_main_agent_keeps_the_main_app() {
    let cells = UserCells::default();
    let backend = Backend::default();
    cells.remember_main_app(APP, LANG, payload(APP));
    let app = cells
        .app_for_call(&backend, APP, Some(LANG), &AgentContext::main_agent())
        .await
        .unwrap();
    assert_eq!(app, APP);
    assert!(backend.installs.lock().unwrap().is_empty());
}

#[tokio::test]
async fn a_user_gets_a_copy_of_the_main_app_under_a_key_of_its_own() {
    let cells = UserCells::default();
    let backend = Backend::default();
    cells.remember_main_app(APP, LANG, payload(APP));

    let a = cells
        .app_for_call(&backend, APP, Some(LANG), &alice())
        .await
        .unwrap();
    let b = cells
        .app_for_call(&backend, APP, Some(LANG), &bob())
        .await
        .unwrap();
    assert_eq!(a, user_app_id(APP, ALICE));
    assert_eq!(b, user_app_id(APP, BOB));

    let installs = backend.installs.lock().unwrap().clone();
    assert_eq!(installs.len(), 2);
    let main_key = payload(APP).agent_key.unwrap();
    let (ka, kb) = (
        installs[0].agent_key.clone().unwrap(),
        installs[1].agent_key.clone().unwrap(),
    );
    assert_ne!(ka, main_key);
    assert_ne!(kb, main_key);
    assert_ne!(ka, kb);
    // Same bundle and network seed: the same DNA, so the same network and DHT.
    for install in &installs {
        assert_eq!(install.network_seed.as_deref(), Some("project-1"));
        assert!(matches!(&install.source, AppBundleSource::Path(p) if p.ends_with("acorn.happ")));
    }

    // Later calls reuse the app; the users are recorded for a restart.
    let again = cells
        .app_for_call(&backend, APP, Some(LANG), &alice())
        .await
        .unwrap();
    assert_eq!(again, a);
    assert_eq!(backend.installs.lock().unwrap().len(), 2);
    assert_eq!(
        cells.users_of(&backend, LANG),
        vec![ALICE.to_string(), BOB.to_string()]
    );
}

#[tokio::test]
async fn two_first_calls_of_one_user_install_once() {
    let cells = UserCells::default();
    let backend = Backend::default();
    cells.remember_main_app(APP, LANG, payload(APP));
    let alice = alice();
    let (x, y) = tokio::join!(
        cells.app_for_call(&backend, APP, Some(LANG), &alice),
        cells.app_for_call(&backend, APP, Some(LANG), &alice),
    );
    assert_eq!(x.unwrap(), y.unwrap());
    assert_eq!(backend.installs.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn a_user_cannot_reach_another_users_app_or_another_languages_app() {
    let cells = UserCells::default();
    let backend = Backend::default();
    cells.remember_main_app(APP, LANG, payload(APP));
    cells
        .app_for_call(&backend, APP, Some(LANG), &bob())
        .await
        .unwrap();

    // Bob's app is no main app: Alice naming it is refused, not routed to Bob's cell.
    let bobs = user_app_id(APP, BOB);
    assert!(cells
        .app_for_call(&backend, &bobs, Some(LANG), &alice())
        .await
        .is_err());
    // Another language may not use this language's app for its users.
    assert!(cells
        .app_for_call(&backend, APP, Some("QmOther"), &alice())
        .await
        .is_err());
    // Nor may code outside any language.
    assert!(cells
        .app_for_call(&backend, APP, None, &alice())
        .await
        .is_err());
    // An app no language registered has no user cell.
    assert!(cells
        .app_for_call(&backend, "QmAcorn-unknown", Some(LANG), &alice())
        .await
        .is_err());
}

#[tokio::test]
async fn a_system_language_stays_on_the_main_app() {
    let cells = UserCells::default();
    let backend = Backend::default();
    cells.mark_system_language("QmPDiff");
    cells.remember_main_app("QmPDiff-pdiff", "QmPDiff", payload("QmPDiff-pdiff"));
    let app = cells
        .app_for_call(&backend, "QmPDiff-pdiff", Some("QmPDiff"), &alice())
        .await
        .unwrap();
    assert_eq!(app, "QmPDiff-pdiff");
    assert!(backend.installs.lock().unwrap().is_empty());
}

#[tokio::test]
async fn a_user_cells_signals_route_to_that_user_also_after_a_restart() {
    let backend = Backend::default();
    let cells = UserCells::default();
    cells.remember_main_app(APP, LANG, payload(APP));
    cells
        .app_for_call(&backend, APP, Some(LANG), &alice())
        .await
        .unwrap();
    let alice_cell = backend.apps.lock().unwrap()[&user_app_id(APP, ALICE)].cells[0].clone();
    let route = UserRoute {
        language: LANG.into(),
        did: ALICE.into(),
    };
    assert_eq!(
        cells.route_for_cell(&cell_key(&alice_cell)),
        Some(route.clone())
    );
    // The main agent's cell is no user cell: it keeps its route in HOLOCHAIN_SIGNAL_HANDLERS.
    let main_cell = CellId::new(dna(), payload(APP).agent_key.unwrap());
    assert_eq!(cells.route_for_cell(&cell_key(&main_cell)), None);

    // A restart: the language registers its app again, and Alice's route comes back before
    // her next call.
    let restarted = UserCells::default();
    restarted.remember_main_app(APP, LANG, payload(APP));
    restarted.restore_user_routes(&backend, APP).await;
    assert_eq!(
        restarted.route_for_cell(&cell_key(&alice_cell)),
        Some(route)
    );
    let again = restarted
        .app_for_call(&backend, APP, Some(LANG), &alice())
        .await
        .unwrap();
    assert_eq!(again, user_app_id(APP, ALICE));
    assert_eq!(
        backend.installs.lock().unwrap().len(),
        1,
        "reinstalled after a restart"
    );

    // Removing the language drops its routes.
    restarted.forget_language(LANG);
    assert_eq!(restarted.route_for_cell(&cell_key(&alice_cell)), None);
}

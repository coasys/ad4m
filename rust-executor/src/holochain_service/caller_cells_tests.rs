//! A language's Holochain calls act as the caller (issue #1775), checked end to end through a
//! language runtime's real `HOLOCHAIN_SERVICE` ops against a mock conductor: the main agent
//! keeps its app and key, each managed user gets an app and a key of its own in the language,
//! a user cannot reach another user's app, and concurrent calls of two users each reach their
//! own cell.
//!
//! The mock conductor answers a zome call with the agent key of the app it reached, so a
//! script sees which Holochain agent its call acted as.

use super::dispatch::{run_dispatch_loop, RequestDispatch};
use super::interface::{
    set_holochain_service, take_holochain_service, HolochainServiceInterface,
    HolochainServiceRequest, HolochainServiceResponse,
};
use crate::agent::AgentContext;
use crate::languages::language_context::LanguageContext;
use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
use async_trait::async_trait;
use deno_core::anyhow::anyhow;
use holochain::conductor::api::AppInfo;
use holochain::prelude::{
    AgentPubKey, AppManifest, AppManifestV0Builder, AppStatus, ExternIO, ZomeCallResponse,
};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex as StdMutex};
use tokio::sync::{mpsc, Mutex};

const LANGUAGE: &str = "QmCallerCellsTest";

/// A conductor that keeps installed apps in a map.
#[derive(Default)]
struct MockConductor {
    apps: StdMutex<HashMap<String, AgentPubKey>>,
    installs: AtomicUsize,
    keys: AtomicUsize,
    /// Every request the conductor got.
    requests: AtomicUsize,
    /// The app id of every install it got.
    installed_ids: StdMutex<Vec<String>>,
}

fn app_info(app_id: &str, agent_pub_key: AgentPubKey) -> AppInfo {
    AppInfo {
        installed_app_id: app_id.to_string(),
        cell_info: Default::default(),
        status: AppStatus::Enabled,
        agent_pub_key,
        manifest: AppManifest::V0(
            AppManifestV0Builder::default()
                .name(app_id.to_string())
                .description(None)
                .roles(vec![])
                .build()
                .unwrap(),
        ),
        installed_at: holochain_timestamp::Timestamp::ZERO,
    }
}

#[async_trait]
impl RequestDispatch for MockConductor {
    async fn handle(&self, request: HolochainServiceRequest) {
        self.requests.fetch_add(1, Ordering::SeqCst);
        match request {
            HolochainServiceRequest::InstallApp(payload, response) => {
                self.installs.fetch_add(1, Ordering::SeqCst);
                self.installed_ids
                    .lock()
                    .unwrap()
                    .push(payload.installed_app_id.clone().unwrap_or_default());
                // A first install takes a while, so two first calls overlap.
                tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                let id = payload.installed_app_id.clone().unwrap_or_default();
                let key = payload
                    .agent_key
                    .clone()
                    .unwrap_or_else(|| AgentPubKey::from_raw_32(vec![0xEE; 32]));
                let mut apps = self.apps.lock().unwrap();
                let result = if apps.contains_key(&id) {
                    Err(anyhow!("AppAlreadyInstalled {id}"))
                } else {
                    apps.insert(id.clone(), key.clone());
                    Ok(app_info(&id, key))
                };
                let _ = response.send(HolochainServiceResponse::InstallApp(result));
            }
            HolochainServiceRequest::GetAppInfo(app_id, response) => {
                let info = self
                    .apps
                    .lock()
                    .unwrap()
                    .get(&app_id)
                    .map(|key| app_info(&app_id, key.clone()));
                let _ = response.send(HolochainServiceResponse::GetAppInfo(Ok(info)));
            }
            HolochainServiceRequest::NewSignKeypair(response) => {
                let n = self.keys.fetch_add(1, Ordering::SeqCst) + 1;
                let key = AgentPubKey::from_raw_32(vec![n as u8; 32]);
                let _ = response.send(HolochainServiceResponse::NewSignKeypair(Ok(key)));
            }
            HolochainServiceRequest::CallZomeFunction {
                app_id, response, ..
            } => {
                let agent = self.apps.lock().unwrap().get(&app_id).cloned();
                let result = match agent {
                    Some(agent) => Ok(ZomeCallResponse::Ok(
                        ExternIO::encode(agent.to_string()).unwrap(),
                    )),
                    None => Err(anyhow!("no app {app_id}")),
                };
                let _ = response.send(HolochainServiceResponse::CallZomeFunction(result));
            }
            other => {
                let name = other.name();
                other.refuse(anyhow!("mock does not handle {name}"));
            }
        }
    }
}

/// Runs `script` (a JS expression that returns a JSON string) as `context`.
async fn run_as(handle: &LanguageRuntimeHandle, context: &AgentContext, script: &str) -> Value {
    let raw = handle
        .execute_with_context(script.to_string(), context.clone())
        .await
        .unwrap_or_else(|e| panic!("the script runs: {e}"));
    serde_json::from_str(&raw).expect("the script returns JSON")
}

/// The Holochain agent a zome call of `context` to `app_id` acts as, or `refused: <error>`.
async fn whoami(handle: &LanguageRuntimeHandle, context: &AgentContext, app_id: &str) -> String {
    let script = format!(
        r#"(async () => {{
  try {{
    return JSON.stringify(await HOLOCHAIN_SERVICE.callZomeFunction({app_id:?}, "acorn", "profiles", "whoami", null));
  }} catch (e) {{ return JSON.stringify("refused: " + (e && e.message || e)); }}
}})()"#
    );
    run_as(handle, context, &script)
        .await
        .as_str()
        .unwrap_or_default()
        .to_string()
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn every_agent_calls_through_its_own_cell() {
    crate::test_utils::init_v8_platform();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let alice =
        crate::test_utils::create_test_user(&format!("alice.{}@example.org", uuid::Uuid::new_v4()));
    let bob =
        crate::test_utils::create_test_user(&format!("bob.{}@example.org", uuid::Uuid::new_v4()));
    let main = AgentContext::main_agent();
    let as_alice = AgentContext::managed(alice.clone());
    let as_bob = AgentContext::managed(bob.clone());

    let running = take_holochain_service().await;
    let (sender, receiver) = mpsc::unbounded_channel();
    let (_signals, signal_rx) = mpsc::unbounded_channel();
    let conductor = Arc::new(MockConductor::default());
    let dispatch = tokio::spawn(run_dispatch_loop(receiver, conductor.clone()));
    set_holochain_service(HolochainServiceInterface {
        sender,
        stream_receiver: Arc::new(Mutex::new(signal_rx)),
    })
    .await;

    let dir = tempfile::tempdir().unwrap();
    let handle =
        LanguageRuntimeHandle::spawn(LANGUAGE.into(), dir.path().to_path_buf(), false).unwrap();
    handle
        .load_module(
            "export function init() {}".to_string(),
            LanguageContext {
                agent_did: String::new(),
                agent_signing_key_id: String::new(),
                custom_settings: None,
                storage_directory: dir.path().to_path_buf(),
                language_address: LANGUAGE.to_string(),
            },
        )
        .await
        .expect("loading a trivial module");

    // The language registers its DNA as the main agent, the way `registerDNAs` does in init.
    let app_id = format!("{LANGUAGE}-acorn");
    let main_key = AgentPubKey::from_raw_32(vec![0xAA; 32]);
    let installed = run_as(
        &handle,
        &main,
        &format!(
            r#"(async () => {{
  const info = await HOLOCHAIN_SERVICE.installApp({{
    installed_app_id: {app_id:?},
    agent_key: new Uint8Array({key}),
    source: {{ type: "path", value: "/nonexistent/acorn.happ" }},
    network_seed: "project-1",
    roles_settings: null,
    ignore_genesis_failure: false,
    restore_from_dht: false,
  }});
  return JSON.stringify(info.installed_app_id);
}})()"#,
            key = serde_json::to_string(&main_key.get_raw_39().to_vec()).unwrap(),
        ),
    )
    .await;
    assert_eq!(installed, json!(app_id));

    // The main agent keeps its app and key.
    assert_eq!(whoami(&handle, &main, &app_id).await, main_key.to_string());

    // Concurrent first calls of two users: each acts as a Holochain agent of its own.
    let (a1, b1, a2) = tokio::join!(
        whoami(&handle, &as_alice, &app_id),
        whoami(&handle, &as_bob, &app_id),
        whoami(&handle, &as_alice, &app_id),
    );
    assert!(!a1.starts_with("refused"), "{a1}");
    assert!(!b1.starts_with("refused"), "{b1}");
    assert_ne!(
        a1,
        main_key.to_string(),
        "Alice's call acted as the main agent"
    );
    assert_ne!(
        b1,
        main_key.to_string(),
        "Bob's call acted as the main agent"
    );
    assert_ne!(a1, b1, "Alice and Bob acted as one Holochain agent");
    assert_eq!(a1, a2, "Alice's two first calls acted as two agents");
    // One install each, though Alice's two first calls overlapped.
    assert_eq!(conductor.installs.load(Ordering::SeqCst), 3);

    // Stable: later calls act as the same agents.
    assert_eq!(whoami(&handle, &as_alice, &app_id).await, a1);
    assert_eq!(whoami(&handle, &as_bob, &app_id).await, b1);
    assert_eq!(whoami(&handle, &main, &app_id).await, main_key.to_string());

    // The caller's app info is the caller's own app: its key is the caller's agent.
    let info_key = |context: AgentContext| {
        let handle = &handle;
        let app_id = app_id.clone();
        async move {
            run_as(
                handle,
                &context,
                &format!(
                    r#"(async () => {{
  const info = await HOLOCHAIN_SERVICE.getAppInfo({app_id:?});
  return JSON.stringify(info ? Array.from(info.agent_pub_key) : null);
}})()"#
                ),
            )
            .await
        }
    };
    let key_of = |info: Value| {
        let bytes: Vec<u8> = serde_json::from_value(info).expect("an app info with a key");
        AgentPubKey::from_raw_39(bytes)
    };
    let alice_key = key_of(info_key(as_alice.clone()).await);
    assert_eq!(alice_key.to_string(), a1);
    assert_eq!(key_of(info_key(main.clone()).await), main_key);

    // Alice cannot reach Bob's cell by naming his app, nor an app no language registered.
    let bobs_app = format!("{app_id}@{bob}");
    let reached = whoami(&handle, &as_alice, &bobs_app).await;
    assert!(
        reached.starts_with("refused"),
        "Alice reached Bob's app {bobs_app}: acted as {reached}"
    );
    assert_ne!(reached, b1);

    let _ = handle.teardown().await;
    take_holochain_service().await;
    dispatch.abort();
    if let Some(service) = running {
        set_holochain_service(service).await;
    }
}

/// A language runtime of `LANGUAGE` on a mock conductor, its DNA registered by the main agent.
struct Rig {
    handle: LanguageRuntimeHandle,
    conductor: Arc<MockConductor>,
    dispatch: tokio::task::JoinHandle<()>,
    running: Option<HolochainServiceInterface>,
    _dir: tempfile::TempDir,
    app_id: String,
    main_key: AgentPubKey,
}

/// The JS of an `installApp` of `app_id` under `key`.
fn install_script(app_id: &str, key: &AgentPubKey) -> String {
    format!(
        r#"HOLOCHAIN_SERVICE.installApp({{
    installed_app_id: {app_id:?},
    agent_key: new Uint8Array({key}),
    source: {{ type: "path", value: "/nonexistent/acorn.happ" }},
    network_seed: "project-1",
    roles_settings: null,
    ignore_genesis_failure: false,
    restore_from_dht: false,
  }})"#,
        key = serde_json::to_string(&key.get_raw_39().to_vec()).unwrap(),
    )
}

/// What `context`'s `installApp` of `app_id` under `key` did: the installed app id and its
/// agent key, or `refused: <error>`.
async fn install_as(
    handle: &LanguageRuntimeHandle,
    context: &AgentContext,
    app_id: &str,
    key: &AgentPubKey,
) -> String {
    let script = format!(
        r#"(async () => {{
  try {{
    const info = await {install};
    return JSON.stringify(info.installed_app_id);
  }} catch (e) {{ return JSON.stringify("refused: " + (e && e.message || e)); }}
}})()"#,
        install = install_script(app_id, key),
    );
    run_as(handle, context, &script)
        .await
        .as_str()
        .unwrap_or_default()
        .to_string()
}

impl Rig {
    async fn start() -> Rig {
        crate::test_utils::init_v8_platform();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let running = take_holochain_service().await;
        let (sender, receiver) = mpsc::unbounded_channel();
        let (_signals, signal_rx) = mpsc::unbounded_channel();
        let conductor = Arc::new(MockConductor::default());
        let dispatch = tokio::spawn(run_dispatch_loop(receiver, conductor.clone()));
        set_holochain_service(HolochainServiceInterface {
            sender,
            stream_receiver: Arc::new(Mutex::new(signal_rx)),
        })
        .await;

        let dir = tempfile::tempdir().unwrap();
        let handle =
            LanguageRuntimeHandle::spawn(LANGUAGE.into(), dir.path().to_path_buf(), false).unwrap();
        handle
            .load_module(
                "export function init() {}".to_string(),
                LanguageContext {
                    agent_did: String::new(),
                    agent_signing_key_id: String::new(),
                    custom_settings: None,
                    storage_directory: dir.path().to_path_buf(),
                    language_address: LANGUAGE.to_string(),
                },
            )
            .await
            .expect("loading a trivial module");
        let app_id = format!("{LANGUAGE}-acorn");
        let main_key = AgentPubKey::from_raw_32(vec![0xAA; 32]);
        let installed = install_as(&handle, &AgentContext::main_agent(), &app_id, &main_key).await;
        assert_eq!(installed, app_id);
        Rig {
            handle,
            conductor,
            dispatch,
            running,
            _dir: dir,
            app_id,
            main_key,
        }
    }

    fn requests(&self) -> usize {
        self.conductor.requests.load(Ordering::SeqCst)
    }

    async fn stop(self) {
        let _ = self.handle.teardown().await;
        take_holochain_service().await;
        self.dispatch.abort();
        if let Some(service) = self.running {
            set_holochain_service(service).await;
        }
    }
}

fn new_user(name: &str) -> String {
    crate::test_utils::create_test_user(&format!("{name}.{}@example.org", uuid::Uuid::new_v4()))
}

/// Code in Alice's call that clears its async context holds no call token. Its Holochain
/// calls must not fall back to the main agent: a call to the main app or to Bob's app, an app
/// info read and an install are all refused before they reach the conductor.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn holochain_ops_without_a_call_token_are_refused() {
    let rig = Rig::start().await;
    let alice = new_user("alice");
    let bob = new_user("bob");
    let as_alice = AgentContext::managed(alice.clone());
    let as_bob = AgentContext::managed(bob.clone());
    let app_id = rig.app_id.clone();
    // Bob's app exists, so a call that reaches it would succeed.
    let bob_key = whoami(&rig.handle, &as_bob, &app_id).await;
    assert!(!bob_key.starts_with("refused"), "{bob_key}");
    let bobs_app = format!("{app_id}@{bob}");

    let before = rig.requests();
    let seen = run_as(
        &rig.handle,
        &as_alice,
        &format!(
            r#"(async () => {{
  const core = Deno[Deno.internal].core;
  // Starts `f` with the async context cleared, so the op gets no call token.
  const cleared = async (f) => {{
    const saved = core.getAsyncContext();
    let promise;
    core.setAsyncContext(undefined);
    try {{ promise = f(); }} catch (e) {{ promise = Promise.reject(e); }}
    finally {{ core.setAsyncContext(saved); }}
    try {{ return "reached: " + JSON.stringify(await promise); }}
    catch (e) {{ return "refused: " + (e && e.message || e); }}
  }};
  return JSON.stringify({{
    mainCall: await cleared(() => HOLOCHAIN_SERVICE.callZomeFunction({app_id:?}, "acorn", "profiles", "whoami", null)),
    bobCall: await cleared(() => HOLOCHAIN_SERVICE.callZomeFunction({bobs_app:?}, "acorn", "profiles", "whoami", null)),
    mainInfo: await cleared(() => HOLOCHAIN_SERVICE.getAppInfo({app_id:?})),
    install: await cleared(() => {install}),
  }});
}})()"#,
            install = install_script(&format!("{LANGUAGE}-other"), &rig.main_key),
        ),
    )
    .await;
    for op in ["mainCall", "bobCall", "mainInfo", "install"] {
        let outcome = seen[op].as_str().unwrap_or_default();
        assert!(
            outcome.starts_with("refused"),
            "{op} with no call token: {outcome}"
        );
    }
    assert_eq!(
        rig.requests(),
        before,
        "an op with no call token reached the conductor"
    );
    rig.stop().await;
}

/// `<app>@<did>` ids belong to Rust's per-user installer. A language cannot install a user's
/// app itself (as the main agent, under the main key, so the user's calls would sign as the
/// main agent), and a managed user's `installApp` never forwards the language's payload: it
/// answers the user's own app, installed by Rust under the user's key.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn user_app_ids_are_reserved_to_the_per_user_installer() {
    let rig = Rig::start().await;
    let alice = new_user("alice");
    let as_alice = AgentContext::managed(alice.clone());
    let main = AgentContext::main_agent();
    let app_id = rig.app_id.clone();
    let alices_app = format!("{app_id}@{alice}");

    // The main agent plants Alice's app under the main key: refused, nothing installed.
    let before = rig.requests();
    let planted = install_as(&rig.handle, &main, &alices_app, &rig.main_key).await;
    assert!(
        planted.starts_with("refused"),
        "the main agent installed Alice's app: {planted}"
    );
    // Alice herself cannot pick her app's key either.
    let own = install_as(&rig.handle, &as_alice, &alices_app, &rig.main_key).await;
    assert!(
        own.starts_with("refused"),
        "Alice's installApp installed {alices_app}: {own}"
    );
    assert_eq!(
        rig.requests(),
        before,
        "a refused install reached the conductor"
    );

    // Alice's calls act as a key of her own, not the main agent's.
    let a = whoami(&rig.handle, &as_alice, &app_id).await;
    assert!(!a.starts_with("refused"), "{a}");
    assert_ne!(
        a,
        rig.main_key.to_string(),
        "Alice's call acted as the main agent"
    );

    // Alice's installApp of the language's app answers her own app, and the conductor never
    // gets the language's payload (its main key) from her call.
    let installs_before = rig.conductor.installed_ids.lock().unwrap().len();
    let answered = install_as(&rig.handle, &as_alice, &app_id, &rig.main_key).await;
    assert_eq!(answered, alices_app);
    assert_eq!(
        rig.conductor.installed_ids.lock().unwrap()[installs_before..].to_vec(),
        Vec::<String>::new(),
        "Alice's installApp forwarded the language's payload to the conductor"
    );
    rig.stop().await;
}

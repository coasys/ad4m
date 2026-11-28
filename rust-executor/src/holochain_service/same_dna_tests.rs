//! Issue #1099 (review #1101, point 1): two languages that bundle the same DNA with the same
//! network seed (a language published or templated without a new seed) must each get a cell of
//! their own. With one node-wide agent key the second language's install asks for the cell the
//! first one already has (cell id = DNA hash + agent key): the conductor recovers by answering
//! the first language's app, so both languages share one cell, and the per-cell signal route is
//! last-writer-wins.
//!
//! Runs two real language runtimes whose `init` calls `registerDNAs` through `ad4m:host`, as
//! a language does, against a mock conductor that keys cells by (network seed, agent key) and
//! recovers a duplicate the way `HolochainService::install_app` does.

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
use holochain::prelude::{AgentPubKey, AppManifest, AppManifestV0Builder, AppStatus};
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex as StdMutex};
use tokio::sync::{mpsc, Mutex};

/// Installed apps by id, and which app owns each (network seed, agent key) cell.
#[derive(Default)]
struct MockConductor {
    apps: StdMutex<HashMap<String, AgentPubKey>>,
    cells: StdMutex<HashMap<(String, AgentPubKey), String>>,
    keys: AtomicUsize,
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

/// The node-wide key `getAgentKey` answers: lair's first key.
fn node_key() -> AgentPubKey {
    AgentPubKey::from_raw_32(vec![0xEE; 32])
}

#[async_trait]
impl RequestDispatch for MockConductor {
    async fn handle(&self, request: HolochainServiceRequest) {
        match request {
            HolochainServiceRequest::InstallApp(payload, response) => {
                let id = payload.installed_app_id.clone().unwrap_or_default();
                let key = payload.agent_key.clone().unwrap_or_else(node_key);
                let cell = (
                    payload.network_seed.clone().unwrap_or_default(),
                    key.clone(),
                );
                let mut cells = self.cells.lock().unwrap();
                // `HolochainService::install_app` answers the owning app on CellAlreadyExists.
                let owner = cells.entry(cell).or_insert_with(|| id.clone()).clone();
                self.apps.lock().unwrap().insert(owner.clone(), key.clone());
                let _ = response.send(HolochainServiceResponse::InstallApp(Ok(app_info(
                    &owner, key,
                ))));
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
            HolochainServiceRequest::GetAgentKey(response) => {
                let _ = response.send(HolochainServiceResponse::GetAgentKey(Ok(node_key())));
            }
            HolochainServiceRequest::NewSignKeypair(response) => {
                let n = self.keys.fetch_add(1, Ordering::SeqCst) + 1;
                let key = AgentPubKey::from_raw_32(vec![n as u8; 32]);
                let _ = response.send(HolochainServiceResponse::NewSignKeypair(Ok(key)));
            }
            other => {
                let name = other.name();
                other.refuse(anyhow!("mock does not handle {name}"));
            }
        }
    }
}

const LANGUAGE_SOURCE: &str = r#"
import { holochainRegisterDnas } from "ad4m:host";
export async function init() {
  globalThis.__infos = await holochainRegisterDnas([
    { nick: "pdiff", path: "/nonexistent/p-diff-sync.happ", network_seed: "one-seed" },
  ]);
}
"#;

/// A language runtime for `address` whose `init` registered the shared DNA.
async fn registered(address: &str, dir: &std::path::Path) -> LanguageRuntimeHandle {
    let handle = LanguageRuntimeHandle::spawn(address.into(), dir.to_path_buf(), false).unwrap();
    let context = LanguageContext {
        agent_did: String::new(),
        agent_signing_key_id: String::new(),
        custom_settings: None,
        storage_directory: dir.to_path_buf(),
        language_address: address.to_string(),
    };
    let json = context.to_json();
    handle
        .load_module(LANGUAGE_SOURCE.to_string(), context)
        .await
        .expect("the module loads");
    handle
        .load_language(json)
        .await
        .unwrap_or_else(|e| panic!("{address}'s init registers its DNA: {e}"));
    handle
}

/// The app id and agent key `registerDNAs` answered the language.
async fn registered_app(handle: &LanguageRuntimeHandle) -> (String, Vec<u8>) {
    let raw = handle
        .execute_with_context(
            "JSON.stringify([globalThis.__infos[0].installed_app_id, Array.from(globalThis.__infos[0].agent_pub_key)])".to_string(),
            AgentContext::main_agent(),
        )
        .await
        .expect("the script runs");
    serde_json::from_str(&raw).expect("JSON")
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn two_languages_with_one_dna_and_seed_get_a_cell_each() {
    crate::test_utils::init_v8_platform();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    crate::db::Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");

    let running = take_holochain_service().await;
    let (sender, receiver) = mpsc::unbounded_channel();
    let (_signals, signal_rx) = mpsc::unbounded_channel();
    let dispatch = tokio::spawn(run_dispatch_loop(
        receiver,
        Arc::new(MockConductor::default()),
    ));
    set_holochain_service(HolochainServiceInterface {
        sender,
        stream_receiver: Arc::new(Mutex::new(signal_rx)),
    })
    .await;

    let (d1, d2) = (tempfile::tempdir().unwrap(), tempfile::tempdir().unwrap());
    let one = registered("QmSameDnaOne", d1.path()).await;
    let two = registered("QmSameDnaTwo", d2.path()).await;
    let (app_one, key_one) = registered_app(&one).await;
    let (app_two, key_two) = registered_app(&two).await;

    let _ = one.teardown().await;
    let _ = two.teardown().await;
    take_holochain_service().await;
    dispatch.abort();
    if let Some(service) = running {
        set_holochain_service(service).await;
    }

    assert_eq!(app_one, "QmSameDnaOne-pdiff");
    assert_eq!(
        app_two, "QmSameDnaTwo-pdiff",
        "the second language got the first one's app, so its cell and signals"
    );
    assert_ne!(key_one, key_two, "two languages share one Holochain agent");
}

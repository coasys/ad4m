//! One-click local AI: the executor detects this device, picks models that fit it, downloads
//! a pinned model runtime and the models into a per-user store, runs the runtimes as its
//! own supervised children on loopback, and registers them as managed endpoints, models and
//! defaults. The launcher and WE drive it through the same RPCs (`ai.local*`).
//!
//! - `data.rs`: the embedded manifest, recommendations and hardware classes; the classifier.
//! - `hardware.rs`: detection (`ai.localHardware`).
//! - `plan.rs`: the pure plan (`ai.localPlan`).
//! - `install.rs`: the store, resumable verified downloads, unpacking.
//! - `supervisor.rs`: child processes, ports, the restart budget.
//! - `onnx_runtime.rs`: ONNX Runtime for the speech gate where `ort` loads it at run time
//!   (macOS), fetched into the store on first use.
//! - here: install, start, stop, remove, status, registration, the health of managed
//!   endpoints, autostart.
//!
//! Two runtimes: llama.cpp's `llama-server` in router mode serves the chat and embedding
//! models on one port (a preset file names them; `LLAMA_API_KEY` holds a random key per start),
//! NVIDIA NeMo-Speech.cpp's `nemo-speech serve` serves transcription (Parakeet) on
//! `/v1/audio/transcriptions` and lists its model on `/v1/models` (its key in a config file
//! only its user reads). Both listen on loopback only. The executor's per-agent state
//! (selection, ports, pids, logs) lives in `<app data>/local-ai/`; the files in the shared
//! store.

pub mod data;
pub mod hardware;
pub mod install;
pub mod onnx_runtime;
pub mod plan;
pub mod supervisor;

use anyhow::{anyhow, bail, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};
use ts_rs::TS;
use url::Url;

use self::data::{Provider, Service};
use self::hardware::{Backend, LocalHardware};
use self::install::Store;
use self::plan::LocalPlan;
use self::supervisor::{
    ChildSpec, Phase, ProcessStatus, Readiness, RestartBudget, Supervised, Timing,
};
use super::AIService;
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub;
use crate::secret::Secret;
use crate::types::{
    AiCapability, Endpoint, EndpointHealth, EndpointKind, EndpointState, ModelInput,
};

/// The names of the managed endpoints.
const LLAMA_ENDPOINT: &str = "This device (llama.cpp)";
const NEMO_SPEECH_ENDPOINT: &str = "This device (NeMo Speech)";

/// At most this often per item an install publishes its progress.
const PROGRESS_INTERVAL: Duration = Duration::from_millis(250);

/// The shared store: `local_ai_dir` from the config, else `AD4M_LOCAL_AI_DIR`, else the
/// per-user default.
pub fn store_root(local_ai_dir: Option<String>) -> PathBuf {
    local_ai_dir
        .or_else(|| {
            std::env::var("AD4M_LOCAL_AI_DIR")
                .ok()
                .filter(|d| !d.is_empty())
        })
        .map(PathBuf::from)
        .unwrap_or_else(install::default_store_root)
}

/// Where local AI keeps its files: the shared store and this executor's state.
#[derive(Debug, Clone)]
pub struct LocalPaths {
    pub store: PathBuf,
    pub state: PathBuf,
}

impl LocalPaths {
    /// The store from the config (`local_ai_dir`, also `--local-ai-dir` and
    /// `AD4M_LOCAL_AI_DIR`), else the per-user default; the state in the app data path.
    pub fn from_config() -> LocalPaths {
        let config = crate::config::get_global_config();
        let store = store_root(config.local_ai_dir.clone());
        let state = config
            .app_data_path
            .clone()
            .map(|p| PathBuf::from(p).join("local-ai"))
            .unwrap_or_else(|| store.join("state"));
        LocalPaths { store, state }
    }
}

/// What `ai.localInstall` asks for.
#[derive(Serialize, Deserialize, Debug, Clone, Default, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "LocalAiInstallParams")]
pub struct InstallParams {
    /// The services to set up, of those the plan offers. Left out: all of them.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub services: Option<Vec<Service>>,
    /// Start the runtimes and register them when the files are in. Left out: yes.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub start: Option<bool>,
}

/// What `ai.localRemove` asks for.
#[derive(Serialize, Deserialize, Debug, Clone, Default, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "LocalAiRemoveParams")]
pub struct RemoveParams {
    /// Also delete the runtimes and models from the per-user store. Other agents of this
    /// user share it. Left out: no, only this executor's setup goes.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub delete_files: Option<bool>,
}

/// An install's progress, as `ai-local-progress` events.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "LocalAiProgress")]
pub struct ProgressEvent {
    /// `plan`, `download`, `start`, `register`, `done` or `failed`.
    pub phase: String,
    /// The runtime id or model id being downloaded.
    pub item: Option<String>,
    #[ts(type = "number")]
    pub item_done_bytes: u64,
    #[ts(type = "number")]
    pub item_total_bytes: u64,
    #[ts(type = "number")]
    pub done_bytes: u64,
    #[ts(type = "number")]
    pub total_bytes: u64,
    pub detail: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiRuntimeStatus")]
pub struct RuntimeStatus {
    pub provider: Provider,
    pub runtime_id: String,
    pub version: String,
    pub build: String,
    pub services: Vec<Service>,
    /// `null`: not started by this executor since it started.
    pub phase: Option<Phase>,
    pub port: Option<u16>,
    pub pid: Option<u32>,
    pub restarts: u32,
    pub detail: Option<String>,
    pub endpoint_id: Option<String>,
    /// After a start on a GPU build: whether the chat model's process holds GPU memory (a
    /// GPU build that finds no device runs on the CPU without saying so). `null`: not
    /// checked (a CPU build, not Linux, or an NVIDIA GPU, whose memory DRM does not show).
    pub on_gpu: Option<bool>,
    /// The GPU memory the runtime's processes held at the check.
    #[ts(type = "number | null")]
    pub gpu_bytes: Option<u64>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiModelStatus")]
pub struct ModelStatus {
    pub service: Service,
    pub model_id: String,
    /// The executor's model row (`ai.models`), once registered.
    pub model: Option<String>,
}

/// `ai.localStatus`: what is set up here and how the runtimes run.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "LocalAiStatus")]
pub struct LocalStatus {
    /// An install finished for this executor.
    pub installed: bool,
    pub installing: bool,
    pub storage_dir: String,
    /// Starts with the executor (`local_ai_autostart`, on unless set off).
    pub autostart: bool,
    pub runtimes: Vec<RuntimeStatus>,
    pub models: Vec<ModelStatus>,
}

/// What an install chose, kept in the state directory: the next start reads it.
#[derive(Serialize, Deserialize, Debug, Clone, Default, PartialEq)]
#[serde(rename_all = "camelCase")]
struct Selection {
    runtimes: Vec<SelectedRuntime>,
    models: Vec<SelectedModel>,
    /// The port each provider had, kept so endpoint URLs rarely change.
    #[serde(default)]
    ports: BTreeMap<String, u16>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
struct SelectedRuntime {
    provider: Provider,
    runtime_id: String,
    version: String,
    build: String,
    binary: PathBuf,
    lib_dirs: Vec<PathBuf>,
    services: Vec<Service>,
    /// What the build runs on; the GPU check after a start reads it.
    #[serde(default)]
    backend: Option<Backend>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
struct SelectedModel {
    service: Service,
    model_id: String,
    provider: Provider,
    path: PathBuf,
    /// The server flags the plan gave the model (the measurement's, else the defaults).
    #[serde(default)]
    server_flags: Vec<String>,
}

impl Selection {
    fn path(paths: &LocalPaths) -> PathBuf {
        paths.state.join("selection.json")
    }

    fn load(paths: &LocalPaths) -> Option<Selection> {
        let text = std::fs::read_to_string(Self::path(paths)).ok()?;
        serde_json::from_str(&text)
            .map_err(|e| log::warn!("Local AI: the selection file is unreadable: {e}"))
            .ok()
    }

    fn save(&self, paths: &LocalPaths) -> Result<()> {
        std::fs::create_dir_all(&paths.state)?;
        let tmp = Self::path(paths).with_extension("json.tmp");
        std::fs::write(&tmp, serde_json::to_string_pretty(self)?)?;
        std::fs::rename(tmp, Self::path(paths))?;
        Ok(())
    }

    fn models_of(&self, provider: Provider) -> Vec<&SelectedModel> {
        self.models
            .iter()
            .filter(|m| m.provider == provider)
            .collect()
    }
}

/// A running runtime: its supervision and what it serves.
struct Running {
    supervised: Supervised,
    endpoint_id: Option<String>,
}

/// What `managed_health` reads without the service lock: per port, the status and the
/// model ids the runtime serves.
struct ManagedPort {
    status: tokio::sync::watch::Receiver<ProcessStatus>,
    readiness: Readiness,
    served: Vec<String>,
    /// The runtime lists what it serves on `/v1/models` (nemo-speech): a running one's
    /// health is that listing, as for any endpoint. Else (llama-server) the liveness probe
    /// and `served`.
    lists_models: bool,
}

lazy_static! {
    static ref RUNNING: tokio::sync::Mutex<BTreeMap<Provider, Running>> =
        tokio::sync::Mutex::new(BTreeMap::new());
    static ref MANAGED_PORTS: std::sync::RwLock<BTreeMap<u16, ManagedPort>> =
        std::sync::RwLock::new(BTreeMap::new());
    /// Per runtime, the GPU check of its last start: on the GPU, and the bytes it held.
    static ref GPU_CHECKS: std::sync::Mutex<BTreeMap<Provider, GpuCheck>> =
        std::sync::Mutex::new(BTreeMap::new());
}

/// A GPU check's outcome: on the GPU, and the GPU memory held. `None`s: not checked (yet).
type GpuCheck = (Option<bool>, Option<u64>);

static INSTALLING: AtomicBool = AtomicBool::new(false);

/// An install while another runs, or a plan that does not fit: the caller's to fix.
#[derive(Debug)]
pub struct LocalAiRefused(pub String);

impl std::fmt::Display for LocalAiRefused {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for LocalAiRefused {}

/// This device, detected now (GPU probes once per process).
pub async fn hardware() -> Result<LocalHardware> {
    hardware_at(LocalPaths::from_config()).await
}

async fn hardware_at(paths: LocalPaths) -> Result<LocalHardware> {
    let data = data::embedded()?;
    Ok(tokio::task::spawn_blocking(move || hardware::detect(&paths.store, &data.classes)).await?)
}

/// The plan for this device against the store's files.
pub async fn current_plan() -> Result<LocalPlan> {
    plan_at(&LocalPaths::from_config()).await
}

async fn plan_at(paths: &LocalPaths) -> Result<LocalPlan> {
    plan_for(paths, crate::config::get_global_config().local_ai_device).await
}

/// The plan for this device held to `device` (`None`: the best one detected), against the
/// store's files.
async fn plan_for(paths: &LocalPaths, device: Option<Backend>) -> Result<LocalPlan> {
    let data = data::embedded()?;
    let hardware = hardware_at(paths.clone()).await?;
    let store = Store::new(&paths.store);
    let installed = {
        let manifest = data.manifest.clone();
        tokio::task::spawn_blocking(move || store.installed_digests(&manifest)).await?
    };
    Ok(plan::plan_with_device(data, &hardware, device, &|sha| {
        installed.contains(sha)
    }))
}

fn autostart_enabled() -> bool {
    crate::config::get_global_config()
        .local_ai_autostart
        .unwrap_or(true)
}

/// The GPU check of `provider`'s last start: (on the GPU, bytes held), `None`s when none ran.
fn gpu_check(provider: Provider) -> GpuCheck {
    GPU_CHECKS
        .lock()
        .unwrap_or_else(|p| p.into_inner())
        .get(&provider)
        .copied()
        .unwrap_or((None, None))
}

/// `ai.localStatus`.
pub async fn status() -> LocalStatus {
    let paths = LocalPaths::from_config();
    let selection = Selection::load(&paths);
    let running = RUNNING.lock().await;
    let models = crate::db_backend::db_backend()
        .get_models()
        .unwrap_or_default();
    let endpoints = crate::db_backend::db_backend()
        .get_endpoints()
        .unwrap_or_default();
    let managed: Vec<&Endpoint> = endpoints.iter().filter(|e| e.managed).collect();
    let selection_ref = selection.as_ref();
    LocalStatus {
        installed: selection.is_some(),
        installing: INSTALLING.load(Ordering::SeqCst),
        storage_dir: paths.store.to_string_lossy().into_owned(),
        autostart: autostart_enabled(),
        runtimes: selection_ref
            .map(|s| {
                s.runtimes
                    .iter()
                    .map(|r| {
                        let live = running.get(&r.provider);
                        let process = live.map(|l| l.supervised.status());
                        RuntimeStatus {
                            provider: r.provider,
                            runtime_id: r.runtime_id.clone(),
                            version: r.version.clone(),
                            build: r.build.clone(),
                            services: r.services.clone(),
                            phase: process.as_ref().map(|p| p.phase),
                            port: process
                                .as_ref()
                                .map(|p| p.port)
                                .or_else(|| s.ports.get(r.provider.as_str()).copied()),
                            pid: process.as_ref().and_then(|p| p.pid),
                            restarts: process.as_ref().map(|p| p.restarts).unwrap_or(0),
                            detail: process.and_then(|p| p.detail),
                            on_gpu: gpu_check(r.provider).0,
                            gpu_bytes: gpu_check(r.provider).1,
                            endpoint_id: live.and_then(|l| l.endpoint_id.clone()).or_else(|| {
                                managed
                                    .iter()
                                    .find(|e| e.name == endpoint_name(r.provider))
                                    .map(|e| e.id.clone())
                            }),
                        }
                    })
                    .collect()
            })
            .unwrap_or_default(),
        models: selection_ref
            .map(|s| {
                s.models
                    .iter()
                    .map(|m| ModelStatus {
                        service: m.service,
                        model_id: m.model_id.clone(),
                        model: models
                            .iter()
                            .find(|row| {
                                row.name == m.model_id
                                    && managed.iter().any(|e| e.id == row.endpoint_id)
                            })
                            .map(|row| row.id.clone()),
                    })
                    .collect()
            })
            .unwrap_or_default(),
    }
}

fn endpoint_name(provider: Provider) -> &'static str {
    match provider {
        Provider::LlamaServer => LLAMA_ENDPOINT,
        Provider::NemoSpeech => NEMO_SPEECH_ENDPOINT,
    }
}

async fn publish_progress(event: ProgressEvent) {
    get_global_pubsub().await.publish_event(Event::new(
        Topic::AiLocalProgress,
        Audience::Operator,
        &event,
    ));
}

/// Clears the install flag when the install ends, also on an error or a panic.
struct InstallGuard;

impl Drop for InstallGuard {
    fn drop(&mut self) {
        INSTALLING.store(false, Ordering::SeqCst);
    }
}

/// `ai.localInstall`: download what the plan names (resuming, verified), keep the
/// selection, then start and register, unless `start` is false.
pub async fn install(params: InstallParams) -> Result<LocalStatus> {
    let paths = LocalPaths::from_config();
    run_install(install_at(&paths, &params)).await
}

/// One install at a time: runs `work` under the install flag, publishes how it ended and
/// answers the status after it.
async fn run_install(work: impl std::future::Future<Output = Result<()>>) -> Result<LocalStatus> {
    if INSTALLING.swap(true, Ordering::SeqCst) {
        return Err(LocalAiRefused("A local AI install is running already".into()).into());
    }
    let guard = InstallGuard;
    let outcome = work.await;
    // Done before the answer: the status it carries says no install runs.
    drop(guard);
    match &outcome {
        Ok(()) => {
            publish_progress(ProgressEvent {
                phase: "done".into(),
                item: None,
                item_done_bytes: 0,
                item_total_bytes: 0,
                done_bytes: 0,
                total_bytes: 0,
                detail: None,
            })
            .await
        }
        Err(e) => {
            publish_progress(ProgressEvent {
                phase: "failed".into(),
                item: None,
                item_done_bytes: 0,
                item_total_bytes: 0,
                done_bytes: 0,
                total_bytes: 0,
                detail: Some(format!("{e:#}")),
            })
            .await
        }
    }
    outcome?;
    Ok(status().await)
}

async fn install_at(paths: &LocalPaths, params: &InstallParams) -> Result<()> {
    install_for(
        paths,
        params,
        crate::config::get_global_config().local_ai_device,
    )
    .await
}

/// Set up local AI's `service` for an executor with no model of its type, on its first use:
/// what dev's default models were (Bert, Whisper), downloaded when something first needed
/// them, not at start. The selection keeps what it had and gains `service`; the runtimes
/// restart with it, and its model becomes the default of a type that has none (an
/// operator's defaults stay). Off with `local_ai_autostart: false`. An install already
/// running (the launcher's) is waited for first.
pub async fn ensure_service(service: Service) -> Result<()> {
    if !autostart_enabled() {
        return Err(LocalAiRefused(format!(
            "No {} model on this executor, and local_ai_autostart is off: set up local AI \
             (ai.localInstall) or register a model on an endpoint",
            service.as_str()
        ))
        .into());
    }
    while INSTALLING.load(Ordering::SeqCst) {
        tokio::time::sleep(Duration::from_millis(500)).await;
    }
    let paths = LocalPaths::from_config();
    let mut services: Vec<Service> = Selection::load(&paths)
        .map(|s| s.models.iter().map(|m| m.service).collect())
        .unwrap_or_default();
    if !services.contains(&service) {
        services.push(service);
    }
    log::info!(
        "No {} model on this executor: setting up local AI for it ({})",
        service.as_str(),
        services
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    );
    let params = InstallParams {
        services: Some(services),
        start: Some(false),
    };
    run_install(async {
        install_at(&paths, &params).await?;
        stop_all().await;
        start_at(&paths, false).await
    })
    .await?;
    Ok(())
}

/// `ai.localInstall` without an executor (`ad4m-executor local-ai install`, the Docker
/// image's `INCLUDE_MODELS`): download what the plan for this device, held to `device`,
/// names into `store` and keep the selection in `state`; start nothing. `progress` gets
/// each `ai-local-progress` event. An executor whose state is `state` (or a copy of it)
/// and whose store is `store` starts them with no download (`local_ai_autostart`).
pub async fn install_into(
    store: PathBuf,
    state: PathBuf,
    device: Option<Backend>,
    services: Option<Vec<Service>>,
    progress: impl Fn(ProgressEvent) + Send + 'static,
) -> Result<()> {
    let mut events = get_global_pubsub().await.subscribe_events();
    let reporter = tokio::spawn(async move {
        while let Ok(event) = events.recv().await {
            if event.topic == Topic::AiLocalProgress {
                if let Ok(step) = serde_json::from_value(event.payload.clone()) {
                    progress(step);
                }
            }
        }
    });
    let paths = LocalPaths { store, state };
    let params = InstallParams {
        services,
        start: Some(false),
    };
    // Not `run_install`: its answer is the status, which reads this executor's database,
    // and there is none here.
    let outcome = install_for(&paths, &params, device).await;
    reporter.abort();
    outcome
}

async fn install_for(
    paths: &LocalPaths,
    params: &InstallParams,
    device: Option<Backend>,
) -> Result<()> {
    let data = data::embedded()?;
    let mut plan = plan_for(paths, device).await?;
    if let Some(wanted) = &params.services {
        plan.models.retain(|m| wanted.contains(&m.service));
        plan.runtimes
            .retain(|r| r.services.iter().any(|s| wanted.contains(s)));
        for runtime in &mut plan.runtimes {
            runtime.services.retain(|s| wanted.contains(s));
        }
    }
    if plan.models.is_empty() {
        return Err(LocalAiRefused(format!(
            "Nothing to install on this device: {}",
            plan.unavailable
                .iter()
                .map(|u| u.reason.clone())
                .chain(plan.reasons.clone())
                .collect::<Vec<_>>()
                .join("; ")
        ))
        .into());
    }
    if !plan.fits {
        return Err(LocalAiRefused(format!(
            "The plan does not fit this device: {}",
            plan.reasons.join("; ")
        ))
        .into());
    }

    let store = Store::new(&paths.store);
    let client = install::http_client()?;
    let total: u64 = plan.runtimes.iter().map(|r| r.size_bytes).sum::<u64>()
        + plan.models.iter().map(|m| m.size_bytes).sum::<u64>();
    let mut done = 0u64;

    let mut selection = Selection {
        ports: Selection::load(paths).map(|s| s.ports).unwrap_or_default(),
        ..Default::default()
    };
    for runtime in &plan.runtimes {
        let build = &data.manifest.runtimes[&runtime.runtime_id].builds[&runtime.build];
        let reporter = Reporter::new(runtime.runtime_id.clone(), runtime.size_bytes, done, total);
        let installed = store
            .fetch_build(
                &client,
                &runtime.runtime_id,
                &runtime.version,
                &runtime.build,
                build,
                &|bytes| reporter.report(bytes),
            )
            .await?;
        reporter.flush().await;
        done += runtime.size_bytes;
        let binary = installed.dir.join(&build.binary);
        if !binary.exists() {
            bail!(
                "{} has no {} after unpacking",
                runtime.runtime_id,
                build.binary
            );
        }
        selection.runtimes.push(SelectedRuntime {
            provider: runtime.provider,
            runtime_id: runtime.runtime_id.clone(),
            version: runtime.version.clone(),
            build: runtime.build.clone(),
            lib_dirs: installed.lib_dirs,
            binary,
            services: runtime.services.clone(),
            backend: Some(runtime.backend),
        });
    }
    for model in &plan.models {
        let file = &data.manifest.models[&model.model_id];
        let reporter = Reporter::new(model.model_id.clone(), model.size_bytes, done, total);
        let path = store
            .fetch_model(&client, file, &|bytes| reporter.report(bytes))
            .await?;
        reporter.flush().await;
        done += model.size_bytes;
        selection.models.push(SelectedModel {
            service: model.service,
            model_id: model.model_id.clone(),
            provider: model.provider,
            path,
            server_flags: model.server_flags.clone(),
        });
    }
    selection.save(paths)?;

    if params.start.unwrap_or(true) {
        // A new set of models: stop what runs, start the new one, make it the default.
        stop_all().await;
        start_at(paths, true).await?;
    }
    Ok(())
}

/// Publishes one item's download progress at most every [`PROGRESS_INTERVAL`].
struct Reporter {
    item: String,
    item_total: u64,
    before: u64,
    total: u64,
    state: std::sync::Mutex<(Option<Instant>, u64)>,
}

impl Reporter {
    fn new(item: String, item_total: u64, before: u64, total: u64) -> Reporter {
        Reporter {
            item,
            item_total,
            before,
            total,
            state: std::sync::Mutex::new((None, 0)),
        }
    }

    fn event(&self, bytes: u64) -> ProgressEvent {
        ProgressEvent {
            phase: "download".into(),
            item: Some(self.item.clone()),
            item_done_bytes: bytes,
            item_total_bytes: self.item_total,
            done_bytes: self.before + bytes,
            total_bytes: self.total,
            detail: None,
        }
    }

    fn report(&self, bytes: u64) {
        let due = {
            let mut state = self.state.lock().unwrap_or_else(|p| p.into_inner());
            state.1 = bytes;
            let due = state.0.is_none_or(|at| at.elapsed() >= PROGRESS_INTERVAL);
            if due {
                state.0 = Some(Instant::now());
            }
            due
        };
        if due {
            let event = self.event(bytes);
            tokio::spawn(publish_progress(event));
        }
    }

    async fn flush(&self) {
        let bytes = self.state.lock().unwrap_or_else(|p| p.into_inner()).1;
        publish_progress(self.event(bytes.max(self.item_total))).await;
    }
}

/// `ai.localStart`: start the installed runtimes and register them. Defaults are set
/// only for types that have none.
pub async fn start() -> Result<LocalStatus> {
    let paths = LocalPaths::from_config();
    start_at(&paths, false).await?;
    Ok(status().await)
}

/// A random key for a runtime (llama-server's `LLAMA_API_KEY`, nemo-speech's config file):
/// 32 bytes, hex.
fn random_key() -> String {
    use rand::RngCore;
    let mut bytes = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut bytes);
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

/// A server's flags as preset keys: `-c 8192` is `c = 8192`, a flag without a value
/// (`--embeddings`) is `embeddings = true`. Keys are the long or short flag names, which
/// presets take as they are.
fn preset_keys(flags: &[String]) -> Vec<(String, String)> {
    let mut keys = Vec::new();
    let mut i = 0;
    while i < flags.len() {
        let key = flags[i].trim_start_matches('-').to_string();
        let value = flags
            .get(i + 1)
            .filter(|next| !next.starts_with('-') || next.parse::<f64>().is_ok());
        match value {
            Some(value) => {
                keys.push((key, value.clone()));
                i += 2;
            }
            None => {
                keys.push((key, "true".to_string()));
                i += 1;
            }
        }
    }
    keys
}

/// The preset file of llama-server's router: one section per model, loaded at startup,
/// with the model's server flags (`-c 8192 -ngl 999 --reasoning off`, `--embeddings`).
fn llama_presets(models: &[&SelectedModel]) -> String {
    let mut ini = String::from("version = 1\n\n[*]\nfit = on\n");
    for model in models {
        ini.push_str(&format!(
            "\n[{}]\nmodel = {}\nload-on-startup = true\n",
            model.model_id,
            model.path.display()
        ));
        let flags = if model.server_flags.is_empty() {
            plan::default_flags(model.service)
        } else {
            model.server_flags.clone()
        };
        let mut keys = preset_keys(&flags);
        // Tool calls go through the chat template: on by default in b11669, set so that a
        // build with another default still parses them.
        if model.service == Service::Llm && !keys.iter().any(|(k, _)| k.contains("jinja")) {
            keys.push(("jinja".into(), "true".into()));
        }
        // An embedding model is served as one, whatever its flags say.
        if model.service == Service::Embeddings
            && !keys
                .iter()
                .any(|(k, _)| k == "embeddings" || k == "embedding")
        {
            keys.push(("embeddings".into(), "true".into()));
        }
        for (key, value) in keys {
            ini.push_str(&format!("{key} = {value}\n"));
        }
    }
    ini
}

fn child_spec(
    paths: &LocalPaths,
    runtime: &SelectedRuntime,
    selection: &Selection,
    port: u16,
    key: &str,
) -> Result<ChildSpec> {
    let models = selection.models_of(runtime.provider);
    let (args, readiness) = match runtime.provider {
        Provider::LlamaServer => {
            let presets = paths.state.join("llama-models.ini");
            std::fs::write(&presets, llama_presets(&models))?;
            (
                vec![
                    "--host".into(),
                    "127.0.0.1".into(),
                    "--port".into(),
                    port.to_string(),
                    "--models-preset".into(),
                    presets.to_string_lossy().into_owned(),
                    "--models-max".into(),
                    models.len().max(1).to_string(),
                    "--no-webui".into(),
                    "--offline".into(),
                ],
                Readiness::Http("/health".into()),
            )
        }
        Provider::NemoSpeech => {
            let model = models
                .first()
                .ok_or_else(|| anyhow!("nemo-speech has no model selected"))?;
            // The key in a config file only this user reads: `ps` shows arguments to every
            // user. `/ready` needs no key.
            let config = paths.state.join("nemo-speech.yaml");
            write_private(&config, &format!("http:\n  api-key: \"{key}\"\n"))?;
            let mut args: Vec<String> = vec![
                "serve".into(),
                "--config".into(),
                config.to_string_lossy().into_owned(),
                "--asr-model".into(),
                model.path.to_string_lossy().into_owned(),
                "--host".into(),
                "127.0.0.1".into(),
                "--port".into(),
                port.to_string(),
                // The build's backend: `auto` would take a GPU a CPU build was chosen over,
                // and a measured `vulkan` must not reach a CPU build.
                "--device".into(),
                nemo_device(runtime.backend).into(),
            ];
            args.extend(nemo_flags(&model.server_flags));
            (args, Readiness::Http("/ready".into()))
        }
    };
    let mut env = Vec::new();
    if runtime.provider == Provider::LlamaServer {
        // In the environment, not the arguments: `ps` shows arguments to every user, a
        // process's environment only to its own user.
        env.push(("LLAMA_API_KEY".to_string(), key.to_string()));
    }
    if !runtime.lib_dirs.is_empty() {
        let joined = std::env::join_paths(
            runtime.lib_dirs.iter().chain(
                std::env::var_os(if cfg!(windows) {
                    "PATH"
                } else {
                    "LD_LIBRARY_PATH"
                })
                .map(|p| std::env::split_paths(&p).collect::<Vec<_>>())
                .unwrap_or_default()
                .iter(),
            ),
        )?;
        env.push((
            if cfg!(windows) {
                "PATH"
            } else {
                "LD_LIBRARY_PATH"
            }
            .to_string(),
            joined.to_string_lossy().into_owned(),
        ));
    }
    Ok(ChildSpec {
        name: runtime.provider.as_str().to_string(),
        program: runtime.binary.clone(),
        args,
        env,
        cwd: runtime
            .binary
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| paths.state.clone()),
        port,
        readiness,
        log_file: paths
            .state
            .join(format!("{}.log", runtime.provider.as_str())),
    })
}

/// nemo-speech's `--device` for a build's backend.
fn nemo_device(backend: Option<Backend>) -> &'static str {
    match backend {
        Some(Backend::Cpu) => "cpu",
        Some(Backend::Vulkan) => "vulkan",
        Some(Backend::Metal) => "metal",
        Some(Backend::Cuda) => "cuda",
        _ => "auto",
    }
}

/// The measured flags of a nemo-speech model, without those the executor sets (where it
/// listens, its key, its model, its device, its config) and without the playground
/// (`--no-ui` always).
fn nemo_flags(measured: &[String]) -> Vec<String> {
    const WITH_VALUE: [&str; 11] = [
        "--host",
        "--port",
        "--api-key",
        "--asr-model",
        "--device",
        "--backend",
        "--config",
        "--cors-origin",
        "--tls-cert",
        "--tls-key",
        "--http.api-key",
    ];
    let mut flags = Vec::new();
    let mut i = 0;
    while i < measured.len() {
        let flag = measured[i].as_str();
        if WITH_VALUE.contains(&flag) {
            i += 2;
            continue;
        }
        if !matches!(flag, "--open" | "--no-ui") {
            flags.push(measured[i].clone());
        }
        i += 1;
    }
    flags.push("--no-ui".into());
    flags
}

/// Write `text` to `path`, readable by this user only.
fn write_private(path: &Path, text: &str) -> Result<()> {
    use std::io::Write;
    let mut options = std::fs::OpenOptions::new();
    options.write(true).create(true).truncate(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
        options.mode(0o600);
        // An older file keeps its mode through `open`.
        if path.exists() {
            std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o600))?;
        }
    }
    options.open(path)?.write_all(text.as_bytes())?;
    Ok(())
}

/// The pids of this executor's runtimes, for the next start to stop if a crash left them.
fn pid_file(paths: &LocalPaths) -> PathBuf {
    paths.state.join("pids.json")
}

async fn write_pids(paths: &LocalPaths) {
    let running = RUNNING.lock().await;
    let pids: Vec<(u32, PathBuf)> = running
        .values()
        .filter_map(|r| {
            Some((
                r.supervised.status().pid?,
                r.supervised.spec.program.clone(),
            ))
        })
        .collect();
    if let Ok(text) = serde_json::to_string(&pids) {
        let _ = std::fs::write(pid_file(paths), text);
    }
}

fn reap_from_pid_file(paths: &LocalPaths) {
    let Ok(text) = std::fs::read_to_string(pid_file(paths)) else {
        return;
    };
    let pids: Vec<(u32, PathBuf)> = serde_json::from_str(&text).unwrap_or_default();
    supervisor::reap_stale(&pids);
    let _ = std::fs::remove_file(pid_file(paths));
}

/// The managed port of a runtime a start has not handed to `RUNNING` yet: dropped before
/// [`StartingPort::keep`], it leaves `MANAGED_PORTS`.
struct StartingPort {
    port: u16,
    kept: bool,
}

impl StartingPort {
    fn keep(mut self) -> u16 {
        self.kept = true;
        self.port
    }
}

impl Drop for StartingPort {
    fn drop(&mut self) {
        if !self.kept {
            MANAGED_PORTS
                .write()
                .unwrap_or_else(|p| p.into_inner())
                .remove(&self.port);
        }
    }
}

async fn start_at(paths: &LocalPaths, make_default: bool) -> Result<()> {
    let mut selection = Selection::load(paths)
        .ok_or_else(|| LocalAiRefused("Local AI is not installed: call ai.localInstall".into()))?;
    std::fs::create_dir_all(&paths.state)?;
    if RUNNING.lock().await.is_empty() {
        reap_from_pid_file(paths);
    }
    let avoid = supervisor::avoided_ports();
    for runtime in selection.runtimes.clone() {
        if RUNNING.lock().await.contains_key(&runtime.provider) {
            continue;
        }
        let preferred = selection.ports.get(runtime.provider.as_str()).copied();
        let port = supervisor::choose_port(preferred, &avoid)?;
        selection
            .ports
            .insert(runtime.provider.as_str().to_string(), port);
        let key = random_key();
        let spec = child_spec(paths, &runtime, &selection, port, &key)?;
        let readiness = spec.readiness.clone();
        let supervised = Supervised::start(spec, Timing::default(), RestartBudget::standard());
        let served: Vec<String> = selection
            .models_of(runtime.provider)
            .iter()
            .map(|m| m.model_id.clone())
            .collect();
        MANAGED_PORTS
            .write()
            .unwrap_or_else(|p| p.into_inner())
            .insert(
                port,
                ManagedPort {
                    status: supervised.watch(),
                    readiness,
                    served,
                    lists_models: runtime.provider == Provider::NemoSpeech,
                },
            );
        // Until `RUNNING` has it, this start owns the runtime: a caller that goes away
        // during the wait (a cancelled RPC) drops `supervised`, which stops the child, and
        // `port`, which forgets the port.
        let port = StartingPort { port, kept: false };
        let settled = supervised
            .wait_until_settled(Timing::default().start_timeout + Duration::from_secs(5))
            .await;
        RUNNING.lock().await.insert(
            runtime.provider,
            Running {
                supervised,
                endpoint_id: None,
            },
        );
        let port = port.keep();
        if settled.phase != Phase::Running {
            log::warn!(
                "Local AI: {} is {:?}: {}",
                runtime.provider.as_str(),
                settled.phase,
                settled.detail.clone().unwrap_or_default()
            );
        }
        let endpoint_id = register(&selection, &runtime, port, &key, make_default).await?;
        if let Some(running) = RUNNING.lock().await.get_mut(&runtime.provider) {
            running.endpoint_id = Some(endpoint_id.clone());
            spawn_health_follower(endpoint_id, running.supervised.watch());
        }
        GPU_CHECKS
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .insert(runtime.provider, (None, None));
        if let Some(model_bytes) = gpu_check_for(&runtime, &selection).await {
            if let Some(running) = RUNNING.lock().await.get(&runtime.provider) {
                spawn_gpu_check(runtime.provider, running.supervised.watch(), model_bytes);
            }
        }
    }
    selection.save(paths)?;
    write_pids(paths).await;
    Ok(())
}

/// Check the managed endpoint's health at every phase change of its runtime, so
/// `ai-endpoint-status` follows a crash and a restart without waiting for the 60 s loop.
fn spawn_health_follower(
    endpoint_id: String,
    mut status: tokio::sync::watch::Receiver<ProcessStatus>,
) {
    tokio::spawn(async move {
        let mut last = status.borrow().phase;
        while status.changed().await.is_ok() {
            let phase = status.borrow().phase;
            if phase == last {
                continue;
            }
            last = phase;
            if let Ok(Some(endpoint)) = crate::db_backend::db_backend().get_endpoint(&endpoint_id) {
                let _ = super::endpoints::check_health(&endpoint).await;
            }
            if matches!(phase, Phase::Stopped | Phase::Failed) {
                return;
            }
        }
    });
}

/// The memory one DRM client of an fdinfo holds, with the client's id: the
/// `drm-memory-*` / `drm-resident-*` lines of vram, gtt, system and local memory (amdgpu,
/// i915, xe). `None` for a file that is no DRM client.
fn drm_client_bytes(fdinfo: &str) -> Option<(String, u64)> {
    let id = fdinfo.lines().find_map(|l| {
        l.strip_prefix("drm-client-id:")
            .map(|v| v.trim().to_string())
    })?;
    let mut bytes = 0u64;
    for line in fdinfo.lines() {
        let Some(rest) = line
            .strip_prefix("drm-memory-")
            .or_else(|| line.strip_prefix("drm-resident-"))
        else {
            continue;
        };
        let Some((region, value)) = rest.split_once(':') else {
            continue;
        };
        if !matches!(region, "vram" | "gtt" | "system" | "local") {
            continue;
        }
        let mut parts = value.split_whitespace();
        let Some(number) = parts.next().and_then(|n| n.parse::<u64>().ok()) else {
            continue;
        };
        bytes += number
            * match parts.next() {
                Some("MiB") => 1024 * 1024,
                Some("KiB") | Some("kB") => 1024,
                _ => 1,
            };
    }
    Some((id, bytes))
}

/// `root` and every process under it (llama-server's router runs a process per model).
fn process_tree(root: u32) -> Vec<u32> {
    let mut parents: Vec<(u32, u32)> = Vec::new();
    if let Ok(entries) = std::fs::read_dir("/proc") {
        for entry in entries.flatten() {
            let Ok(pid) = entry.file_name().to_string_lossy().parse::<u32>() else {
                continue;
            };
            // `/proc/<pid>/stat`: pid (comm) state ppid ...; comm may hold spaces.
            let Ok(stat) = std::fs::read_to_string(entry.path().join("stat")) else {
                continue;
            };
            let ppid = stat
                .rsplit_once(')')
                .and_then(|(_, rest)| rest.split_whitespace().nth(1))
                .and_then(|p| p.parse::<u32>().ok());
            if let Some(ppid) = ppid {
                parents.push((pid, ppid));
            }
        }
    }
    let mut tree = vec![root];
    let mut at = 0;
    while at < tree.len() {
        let parent = tree[at];
        tree.extend(
            parents
                .iter()
                .filter(|(_, p)| *p == parent)
                .map(|(c, _)| *c),
        );
        at += 1;
    }
    tree
}

/// The GPU memory `pids` hold through DRM (Linux), each client once.
fn drm_bytes(pids: &[u32]) -> u64 {
    let mut clients: BTreeMap<String, u64> = BTreeMap::new();
    for pid in pids {
        let Ok(fds) = std::fs::read_dir(format!("/proc/{pid}/fdinfo")) else {
            continue;
        };
        for fd in fds.flatten() {
            let Ok(info) = std::fs::read_to_string(fd.path()) else {
                continue;
            };
            if let Some((id, bytes)) = drm_client_bytes(&info) {
                let held = clients.entry(format!("{pid}:{id}")).or_default();
                *held = (*held).max(bytes);
            }
        }
    }
    clients.values().sum()
}

/// After a start on a GPU build: wait (up to 2 minutes, while the models load) for the
/// runtime's processes to hold at least half the chat model's size in GPU memory, as the
/// wind tunnel checks. A GPU build that finds no device runs on the CPU and says nothing.
/// Linux with an AMD or Intel GPU only: NVIDIA's memory does not show in DRM fdinfo.
fn spawn_gpu_check(
    provider: Provider,
    status: tokio::sync::watch::Receiver<ProcessStatus>,
    model_bytes: u64,
) {
    tokio::spawn(async move {
        let started = Instant::now();
        let mut held = 0;
        while started.elapsed() < Duration::from_secs(120) {
            let pid = {
                let current = status.borrow();
                (current.phase == Phase::Running)
                    .then_some(current.pid)
                    .flatten()
            };
            if let Some(pid) = pid {
                held = tokio::task::spawn_blocking(move || drm_bytes(&process_tree(pid)))
                    .await
                    .unwrap_or(0);
                if held * 2 >= model_bytes {
                    break;
                }
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
        let on_gpu = held * 2 >= model_bytes;
        if on_gpu {
            log::info!(
                "Local AI: {} holds {:.2} GB of GPU memory for a {:.2} GB model",
                provider.as_str(),
                held as f64 / 1e9,
                model_bytes as f64 / 1e9
            );
        } else {
            log::warn!(
                "Local AI: {} holds {:.2} GB of GPU memory for a {:.2} GB model: the GPU build runs on the CPU",
                provider.as_str(),
                held as f64 / 1e9,
                model_bytes as f64 / 1e9
            );
        }
        GPU_CHECKS
            .lock()
            .unwrap_or_else(|p| p.into_inner())
            .insert(provider, (Some(on_gpu), Some(held)));
    });
}

/// Whether a start of `runtime` gets the GPU check, and against how many bytes: a GPU
/// build, Linux, an AMD or Intel GPU; the chat model's file size (else the largest model's).
async fn gpu_check_for(runtime: &SelectedRuntime, selection: &Selection) -> Option<u64> {
    let backend = runtime.backend?;
    if backend == Backend::Cpu || !cfg!(target_os = "linux") {
        return None;
    }
    let hardware = hardware().await.ok()?;
    let vendor = hardware.primary_gpu()?.vendor;
    if !matches!(
        vendor,
        hardware::GpuVendor::Amd | hardware::GpuVendor::Intel
    ) {
        return None;
    }
    let models = selection.models_of(runtime.provider);
    let size = |m: &&SelectedModel| m.path.metadata().map(|md| md.len()).unwrap_or(0);
    models
        .iter()
        .find(|m| m.service == Service::Llm)
        .map(size)
        .or_else(|| models.iter().map(size).max())
        .filter(|bytes| *bytes > 0)
}

/// The id requests name `model` by. llama-server's router serves each model under its preset
/// name, the manifest id. nemo-speech serves its model under the id it reads from the file
/// (Parakeet v3 Q8_0: `parakeet-tdt-0.6b-v3.oss-align.q8_0`) and refuses any other: the id its
/// `/v1/models` lists, else the id an earlier start registered, else the manifest id.
fn remote_model(
    provider: Provider,
    model: &SelectedModel,
    health: Option<&EndpointHealth>,
    registered: Option<&str>,
) -> String {
    match provider {
        Provider::LlamaServer => model.model_id.clone(),
        Provider::NemoSpeech => health
            .filter(|h| h.state == EndpointState::Reachable)
            .and_then(|h| h.served_models.first().cloned())
            .or_else(|| registered.map(str::to_string))
            .unwrap_or_else(|| model.model_id.clone()),
    }
}

/// Whether the executor sends a model tools as definitions. The managed chat model: yes.
/// llama-server renders them with the model's chat template (`--jinja`), parses the calls
/// and honours `parallel_tool_calls: false`; a call `tool_choice` forces (`"required"`, a
/// named function), which it does not enforce, `/v1` makes by a schema it does enforce
/// (`api::openai_compat::chat::constrained_tool_calls`).
fn native_tools(model: &SelectedModel) -> Option<bool> {
    (model.provider == Provider::LlamaServer && model.service == Service::Llm).then_some(true)
}

/// Register `runtime` as a managed endpoint with its models; make each the default of its
/// type when `make_default`, or when the type has none. Answers the endpoint id.
async fn register(
    selection: &Selection,
    runtime: &SelectedRuntime,
    port: u16,
    key: &str,
    make_default: bool,
) -> Result<String> {
    let models = selection.models_of(runtime.provider);
    let (kind, capabilities) = match runtime.provider {
        Provider::LlamaServer => (
            EndpointKind::LlamaServer,
            models
                .iter()
                .map(|m| m.service.capability())
                .collect::<Vec<AiCapability>>(),
        ),
        Provider::NemoSpeech => (EndpointKind::OpenAiAudio, vec![AiCapability::Transcription]),
    };
    let mut capabilities = capabilities;
    capabilities.sort();
    capabilities.dedup();
    let name = endpoint_name(runtime.provider);
    let existing = crate::db_backend::db_backend().get_endpoint_by_name(name)?;
    if existing.as_ref().is_some_and(|e| !e.managed) {
        bail!("An endpoint named {name:?} exists already and is not local AI's: rename it");
    }
    let endpoint = Endpoint {
        id: existing
            .as_ref()
            .map(|e| e.id.clone())
            .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
        name: name.to_string(),
        kind,
        base_url: Url::parse(&format!("http://127.0.0.1:{port}"))?,
        api_key: Secret::new(key),
        capabilities,
        max_num_ctx: None,
        request_timeout_secs: None,
        max_concurrent: None,
        managed: true,
        // The stored health, so the check below publishes a change.
        health: existing
            .as_ref()
            .map(|e| e.health.clone())
            .unwrap_or_default(),
    };
    let service = AIService::global_instance().await?;
    match &existing {
        None => crate::db_backend::db_backend().insert_endpoint(&endpoint)?,
        Some(_) => {
            for model_id in crate::db_backend::db_backend().update_endpoint(&endpoint)? {
                if let Err(e) = service.reload_model(model_id.clone()).await {
                    log::warn!("Local AI: model {model_id} did not reload: {e:#}");
                }
            }
        }
    }
    let health = super::endpoints::check_health(&endpoint)
        .await
        .map_err(|e| log::warn!("Local AI: health of {name}: {e}"))
        .ok();

    let rows = crate::db_backend::db_backend().get_models()?;
    for model in models {
        let model_type = model.service.model_type();
        let row = rows.iter().find(|row| row.name == model.model_id);
        let input = ModelInput {
            name: model.model_id.clone(),
            model_type: model_type.clone(),
            endpoint_id: endpoint.id.clone(),
            remote_model: remote_model(
                runtime.provider,
                model,
                health.as_ref(),
                row.filter(|r| r.endpoint_id == endpoint.id)
                    .map(|r| r.remote_model.as_str()),
            ),
            native_tools: native_tools(model),
        };
        let id = match row {
            Some(row) if row.endpoint_id == endpoint.id => {
                if row.model_type != model_type
                    || row.remote_model != input.remote_model
                    || row.native_tools != input.native_tools
                {
                    service.update_model(row.id.clone(), input).await?;
                }
                row.id.clone()
            }
            Some(_) => {
                log::warn!(
                    "Local AI: a model named {:?} exists on another endpoint; left as it is",
                    model.model_id
                );
                continue;
            }
            None => service.add_model(input).await?,
        };
        let has_default = crate::db_backend::db_backend()
            .get_default_model(model_type.clone())?
            .is_some_and(|default| rows.iter().any(|r| r.id == default) || default == id);
        if make_default || !has_default {
            service.set_default_model(model_type, id).await?;
        }
    }
    Ok(endpoint.id)
}

/// Stop every runtime this executor runs. Endpoints and models stay registered (down).
async fn stop_all() {
    let running: Vec<Running> = std::mem::take(&mut *RUNNING.lock().await)
        .into_values()
        .collect();
    let mut endpoint_ids = Vec::new();
    for running in running {
        let port = running.supervised.spec.port;
        running.supervised.stop().await;
        MANAGED_PORTS
            .write()
            .unwrap_or_else(|p| p.into_inner())
            .remove(&port);
        endpoint_ids.extend(running.endpoint_id);
    }
    for id in endpoint_ids {
        if let Ok(Some(endpoint)) = crate::db_backend::db_backend().get_endpoint(&id) {
            let _ = super::endpoints::check_health(&endpoint).await;
        }
    }
}

/// `ai.localStop`.
pub async fn stop() -> LocalStatus {
    stop_all().await;
    let _ = std::fs::remove_file(pid_file(&LocalPaths::from_config()));
    status().await
}

/// `ai.localRemove`: stop, unregister the managed endpoints and their models, forget the
/// selection; with `delete_files`, delete the store's runtimes and models too.
pub async fn remove(params: RemoveParams) -> Result<bool> {
    if INSTALLING.load(Ordering::SeqCst) {
        return Err(LocalAiRefused("A local AI install is running".into()).into());
    }
    stop_all().await;
    let paths = LocalPaths::from_config();
    let service = AIService::global_instance().await?;
    for endpoint in crate::db_backend::db_backend().get_endpoints()? {
        if !endpoint.managed {
            continue;
        }
        for model_id in crate::db_backend::db_backend().endpoint_model_ids(&endpoint.id)? {
            service.remove_model(model_id).await?;
        }
        crate::db_backend::db_backend().remove_endpoint(&endpoint.id)?;
    }
    let _ = std::fs::remove_file(Selection::path(&paths));
    let _ = std::fs::remove_file(pid_file(&paths));
    if params.delete_files.unwrap_or(false) {
        let store = Store::new(&paths.store);
        for dir in [
            store.root.join("runtime"),
            store.models_dir(),
            store.downloads_dir(),
        ] {
            if dir.exists() {
                std::fs::remove_dir_all(&dir)?;
            }
        }
    }
    Ok(true)
}

/// The health of a managed endpoint: whether the supervisor's runtime on its port runs, and
/// then for nemo-speech its `/v1/models` listing (with the key), for llama-server its
/// liveness probe. A runtime that is starting, restarting, stopped or failed says so.
pub async fn managed_health(endpoint: &Endpoint) -> EndpointHealth {
    let checked_at = Some(chrono::Utc::now().timestamp_millis());
    let entry = endpoint.base_url.port().and_then(|port| {
        MANAGED_PORTS
            .read()
            .unwrap_or_else(|p| p.into_inner())
            .get(&port)
            .map(|m| {
                (
                    port,
                    m.status.borrow().clone(),
                    m.readiness.clone(),
                    m.served.clone(),
                    m.lists_models,
                )
            })
    });
    let down = |detail: String| EndpointHealth {
        state: EndpointState::Unreachable,
        checked_at,
        detail: Some(detail),
        served_models: Vec::new(),
    };
    match entry {
        None => down("Local AI is not running: start it with ai.localStart".into()),
        Some((_, status, _, _, true)) if status.phase == Phase::Running => {
            super::providers::discovery::discover_for(
                endpoint.kind,
                endpoint.base_url.clone(),
                endpoint.api_key.expose(),
                super::providers::discovery::PROBE_TIMEOUT,
            )
            .health()
            .await
        }
        Some((port, status, readiness, served, _)) => match status.phase {
            Phase::Running if supervisor::probe(port, &readiness).await => EndpointHealth {
                state: EndpointState::Reachable,
                checked_at,
                detail: None,
                served_models: served,
            },
            Phase::Running => down("Local AI runs but does not answer".into()),
            Phase::Starting | Phase::Restarting => down(format!(
                "Local AI is {}",
                if status.phase == Phase::Starting {
                    "starting"
                } else {
                    "restarting"
                }
            )),
            Phase::Stopped => down("Local AI is stopped".into()),
            Phase::Failed => down(status.detail.unwrap_or_else(|| "Local AI failed".into())),
        },
    }
}

/// At executor start: stop what a crash left, then start the installed runtimes unless
/// `local_ai_autostart` is off. In the background: the API serves meanwhile.
pub fn autostart() {
    tokio::spawn(async {
        let paths = LocalPaths::from_config();
        if Selection::load(&paths).is_none() {
            return;
        }
        reap_from_pid_file(&paths);
        if !autostart_enabled() {
            log::info!(
                "Local AI is installed; local_ai_autostart is off, so it waits for ai.localStart"
            );
            return;
        }
        log::info!("Local AI: starting the installed runtimes");
        if let Err(e) = start_at(&paths, false).await {
            log::error!("Local AI did not start: {e:#}");
        }
    });
}

/// How long a runtime has after SIGTERM when the executor exits. With the Holochain
/// conductor's stop beside it, under the 4 s a signal leaves the executor's shutdown.
const EXIT_GRACE: Duration = Duration::from_secs(2);

/// At executor shutdown: every runtime gets SIGTERM at once and SIGKILL after
/// [`EXIT_GRACE`], so the stop fits in the deadline the executor's shutdown has (a runtime
/// that ignores SIGTERM would otherwise hold it for the supervisor's 5 s, per runtime).
pub async fn stop_on_exit() {
    let _ = tokio::task::spawn_blocking(|| stop_children_blocking(EXIT_GRACE)).await;
    // The children are gone: this ends their supervisors and marks the endpoints down.
    stop_all().await;
    let _ = std::fs::remove_file(pid_file(&LocalPaths::from_config()));
}

/// Stop the runtimes from a thread that cannot await, right before the process exits:
/// the executor's shutdown ([`stop_on_exit`]) and the launcher's Quit, which calls
/// `std::process::exit` without that shutdown. SIGTERM, up to `grace` for them to go, then
/// SIGKILL. After a crash Linux kills them anyway (`PR_SET_PDEATHSIG`); macOS and Windows
/// would not.
pub fn stop_children_blocking(grace: Duration) {
    use sysinfo::{Pid, ProcessesToUpdate, Signal, System};
    let pids: Vec<Pid> = MANAGED_PORTS
        .read()
        .unwrap_or_else(|p| p.into_inner())
        .values()
        .filter_map(|m| m.status.borrow().pid)
        .map(Pid::from_u32)
        .collect();
    if pids.is_empty() {
        return;
    }
    // No restart of a child that exits now.
    if let Ok(running) = RUNNING.try_lock() {
        for r in running.values() {
            r.supervised.request_stop();
        }
    }
    let mut system = System::new();
    system.refresh_processes(ProcessesToUpdate::Some(&pids), true);
    for pid in &pids {
        if let Some(process) = system.process(*pid) {
            if process.kill_with(Signal::Term).is_none() {
                process.kill();
            }
        }
    }
    let deadline = Instant::now() + grace;
    loop {
        system.refresh_processes(ProcessesToUpdate::Some(&pids), true);
        let alive: Vec<&Pid> = pids
            .iter()
            .filter(|p| {
                system
                    .process(**p)
                    .is_some_and(|proc| proc.status() != sysinfo::ProcessStatus::Zombie)
            })
            .collect();
        if alive.is_empty() {
            break;
        }
        if Instant::now() >= deadline {
            for pid in alive {
                if let Some(process) = system.process(*pid) {
                    process.kill();
                }
            }
            break;
        }
        std::thread::sleep(Duration::from_millis(100));
    }
    // The pid file stays: the next start checks each pid against its binary first.
}

#[cfg(test)]
mod tests {
    use super::*;

    fn selected(service: Service, id: &str, provider: Provider) -> SelectedModel {
        SelectedModel {
            service,
            model_id: id.into(),
            provider,
            path: PathBuf::from(format!("/store/models/{id}.gguf")),
            server_flags: vec![],
        }
    }

    /// Without measured flags: chat without thinking (Qwen3.5 otherwise thinks until the
    /// context is full), the embedding model as one.
    #[test]
    fn the_router_presets_load_each_model_at_start_and_mark_the_embedding_one() {
        let chat = selected(Service::Llm, "qwen3.5-2b-q4_k_m", Provider::LlamaServer);
        let embed = selected(
            Service::Embeddings,
            "nomic-embed-text-v1.5-q8_0",
            Provider::LlamaServer,
        );
        let ini = llama_presets(&[&chat, &embed]);
        assert!(ini.starts_with("version = 1\n"));
        assert!(ini.contains("[qwen3.5-2b-q4_k_m]\nmodel = /store/models/qwen3.5-2b-q4_k_m.gguf\nload-on-startup = true\nreasoning = off\n"), "{ini}");
        assert!(ini.contains("[nomic-embed-text-v1.5-q8_0]\nmodel = /store/models/nomic-embed-text-v1.5-q8_0.gguf\nload-on-startup = true\nembeddings = true\n"), "{ini}");
        assert_eq!(ini.matches("embeddings = true").count(), 1);
        assert_eq!(ini.matches("reasoning = off").count(), 1);
    }

    /// The wind tunnel's measured flags become the model's preset keys.
    #[test]
    fn measured_server_flags_become_preset_keys() {
        let flags = |s: &str| s.split(' ').map(String::from).collect::<Vec<_>>();
        assert_eq!(
            preset_keys(&flags("-c 8192 -ngl 999 --reasoning off")),
            vec![
                ("c".to_string(), "8192".to_string()),
                ("ngl".to_string(), "999".to_string()),
                ("reasoning".to_string(), "off".to_string())
            ]
        );
        assert_eq!(
            preset_keys(&flags("-c 8192 --embeddings --temp -1")),
            vec![
                ("c".to_string(), "8192".to_string()),
                ("embeddings".to_string(), "true".to_string()),
                ("temp".to_string(), "-1".to_string())
            ]
        );
        let mut chat = selected(Service::Llm, "qwen3.5-4b-q4_k_m", Provider::LlamaServer);
        chat.server_flags = flags("-c 8192 -ngl 999 --reasoning off");
        let mut embed = selected(Service::Embeddings, "nomic", Provider::LlamaServer);
        embed.server_flags = flags("-c 8192 -ngl 999");
        let ini = llama_presets(&[&chat, &embed]);
        assert!(
            ini.contains("load-on-startup = true\nc = 8192\nngl = 999\nreasoning = off\n"),
            "{ini}"
        );
        // The embedding model is one even when its flags forget to say so.
        assert!(ini.ends_with("ngl = 999\nembeddings = true\n"), "{ini}");
    }

    /// amdgpu's fdinfo of a DRM client: the memory it holds, by region.
    #[test]
    fn a_drm_clients_memory_is_read_from_its_fdinfo() {
        let fdinfo = "pos:\t0\nflags:\t02100002\ndrm-driver:\tamdgpu\ndrm-client-id:\t42\n\
                      drm-memory-vram:\t1024 KiB\ndrm-memory-gtt:\t2 MiB\ndrm-memory-cpu:\t0 KiB\n\
                      drm-engine-gfx:\t12345 ns\n";
        assert_eq!(
            drm_client_bytes(fdinfo),
            Some(("42".to_string(), 1024 * 1024 + 2 * 1024 * 1024))
        );
        assert_eq!(drm_client_bytes("pos:\t0\nflags:\t0\n"), None);
        // This process is in its own tree.
        assert!(process_tree(std::process::id()).contains(&std::process::id()));
    }

    #[test]
    fn llama_server_and_nemo_speech_listen_on_loopback_with_their_keys_out_of_the_arguments() {
        let dir = tempfile::tempdir().unwrap();
        let paths = LocalPaths {
            store: dir.path().join("store"),
            state: dir.path().join("state"),
        };
        std::fs::create_dir_all(&paths.state).unwrap();
        let selection = Selection {
            runtimes: vec![],
            models: vec![
                selected(Service::Llm, "chat", Provider::LlamaServer),
                selected(Service::Stt, "parakeet-v3", Provider::NemoSpeech),
            ],
            ports: BTreeMap::new(),
        };
        let llama = SelectedRuntime {
            provider: Provider::LlamaServer,
            runtime_id: "llama.cpp".into(),
            version: "b1".into(),
            build: "linux-x64-cuda".into(),
            binary: dir.path().join("rt/llama-server"),
            lib_dirs: vec![dir.path().join("rt/cudart")],
            services: vec![Service::Llm],
            backend: Some(Backend::Cuda),
        };
        let spec = child_spec(&paths, &llama, &selection, 40001, "k3y").unwrap();
        let args = spec.args.join(" ");
        assert!(args.contains("--host 127.0.0.1 --port 40001"), "{args}");
        assert!(
            !args.contains("k3y"),
            "the key stays out of the arguments: {args}"
        );
        assert!(spec
            .env
            .contains(&("LLAMA_API_KEY".to_string(), "k3y".to_string())));
        assert!(args.contains("--models-preset"));
        assert_eq!(spec.readiness, Readiness::Http("/health".into()));
        let (var, value) = &spec.env[1];
        assert_eq!(
            var,
            if cfg!(windows) {
                "PATH"
            } else {
                "LD_LIBRARY_PATH"
            }
        );
        assert!(value.starts_with(&dir.path().join("rt/cudart").to_string_lossy().into_owned()));
        assert!(
            std::fs::read_to_string(paths.state.join("llama-models.ini"))
                .unwrap()
                .contains("[chat]")
        );

        let nemo = SelectedRuntime {
            provider: Provider::NemoSpeech,
            runtime_id: "nemo-speech".into(),
            version: "0.3.0".into(),
            build: "linux-x64-cpu".into(),
            binary: dir.path().join("rt/bin/nemo-speech"),
            lib_dirs: vec![],
            services: vec![Service::Stt],
            backend: Some(Backend::Cpu),
        };
        // Measured on Vulkan, with a host and a key of its own: this CPU build gets none of it.
        let mut measured = selection.clone();
        measured.models[1].server_flags = [
            "--device",
            "vulkan",
            "--no-ui",
            "--host",
            "0.0.0.0",
            "--api-key",
            "x",
            "--open",
            "--access-log",
        ]
        .map(String::from)
        .to_vec();
        let spec = child_spec(&paths, &nemo, &measured, 40002, "n3mo").unwrap();
        let args = spec.args.join(" ");
        let config = paths.state.join("nemo-speech.yaml");
        assert_eq!(
            args,
            format!(
                "serve --config {} --asr-model {} --host 127.0.0.1 --port 40002 --device cpu --access-log --no-ui",
                config.display(),
                measured.models[1].path.display()
            )
        );
        assert!(!args.contains("n3mo"), "the key stays out of the arguments");
        assert_eq!(
            std::fs::read_to_string(&config).unwrap(),
            "http:\n  api-key: \"n3mo\"\n"
        );
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(&config).unwrap().permissions().mode();
            assert_eq!(mode & 0o777, 0o600);
        }
        assert_eq!(spec.readiness, Readiness::Http("/ready".into()));
        assert!(spec.env.is_empty());

        let vulkan = SelectedRuntime {
            backend: Some(Backend::Vulkan),
            ..nemo
        };
        let args = child_spec(&paths, &vulkan, &selection, 40002, "k")
            .unwrap()
            .args
            .join(" ");
        assert!(args.ends_with("--device vulkan --no-ui"), "{args}");
    }

    /// nemo-speech refuses a model name it does not list: requests name the listed id.
    #[test]
    fn nemo_speech_models_are_registered_under_the_id_the_server_lists() {
        let model = selected(
            Service::Stt,
            "parakeet-tdt-0.6b-v3-q8_0",
            Provider::NemoSpeech,
        );
        let listed = EndpointHealth {
            state: EndpointState::Reachable,
            checked_at: None,
            detail: None,
            served_models: vec!["parakeet-tdt-0.6b-v3.oss-align.q8_0".into()],
        };
        assert_eq!(
            remote_model(Provider::NemoSpeech, &model, Some(&listed), Some("old")),
            "parakeet-tdt-0.6b-v3.oss-align.q8_0"
        );
        let down = EndpointHealth {
            state: EndpointState::Unreachable,
            served_models: vec![],
            ..listed.clone()
        };
        assert_eq!(
            remote_model(Provider::NemoSpeech, &model, Some(&down), Some("old")),
            "old"
        );
        assert_eq!(
            remote_model(Provider::NemoSpeech, &model, None, None),
            "parakeet-tdt-0.6b-v3-q8_0"
        );
        let chat = selected(Service::Llm, "qwen", Provider::LlamaServer);
        assert_eq!(
            remote_model(Provider::LlamaServer, &chat, Some(&listed), None),
            "qwen"
        );
    }

    /// The managed chat model takes tools as definitions, rendered by its chat template.
    #[test]
    fn the_managed_chat_model_takes_tools_as_definitions_through_its_template() {
        let chat = selected(Service::Llm, "qwen", Provider::LlamaServer);
        let embed = selected(Service::Embeddings, "nomic", Provider::LlamaServer);
        let stt = selected(Service::Stt, "parakeet", Provider::NemoSpeech);
        assert_eq!(native_tools(&chat), Some(true));
        assert_eq!(native_tools(&embed), None);
        assert_eq!(native_tools(&stt), None);
        let ini = llama_presets(&[&chat, &embed]);
        let section = |name: &str| {
            ini.split(&format!("[{name}]"))
                .nth(1)
                .unwrap()
                .split("\n[")
                .next()
                .unwrap()
                .to_string()
        };
        assert!(section("qwen").contains("jinja = true"), "{ini}");
        assert!(!section("nomic").contains("jinja"), "{ini}");
        let mut off = chat.clone();
        off.server_flags = vec!["--no-jinja".into()];
        let ini = llama_presets(&[&off]);
        assert!(
            ini.contains("no-jinja = true") && !ini.contains("\njinja"),
            "{ini}"
        );
    }

    /// A running nemo-speech is as healthy as its `/v1/models` listing with the key says: the
    /// served ids are the listed ones, a refused key is `Unauthorized`. One that is starting
    /// says so without a request.
    #[tokio::test]
    async fn a_running_nemo_speech_is_checked_by_its_listing() {
        let mut server = mockito::Server::new_async().await;
        let listing = server
            .mock("GET", "/v1/models")
            .match_header("authorization", "Bearer good")
            .with_body(r#"{"object":"list","data":[{"id":"parakeet-tdt-0.6b-v3.oss-align.q8_0","object":"model","capability":"transcription"}]}"#)
            .expect(1)
            .create_async()
            .await;
        let refused = server
            .mock("GET", "/v1/models")
            .match_header("authorization", "Bearer bad")
            .with_status(401)
            .with_body(r#"{"error":{"message":"missing or invalid bearer token"}}"#)
            .create_async()
            .await;
        let base_url = Url::parse(&server.url()).unwrap();
        let port = base_url.port().unwrap();
        let (tx, rx) = tokio::sync::watch::channel(ProcessStatus {
            phase: Phase::Running,
            pid: None,
            port,
            restarts: 0,
            detail: None,
        });
        MANAGED_PORTS.write().unwrap().insert(
            port,
            ManagedPort {
                status: rx,
                readiness: Readiness::Http("/ready".into()),
                served: vec!["parakeet-tdt-0.6b-v3-q8_0".into()],
                lists_models: true,
            },
        );
        let endpoint = |key: &str| Endpoint {
            id: "e".into(),
            name: NEMO_SPEECH_ENDPOINT.into(),
            kind: EndpointKind::OpenAiAudio,
            base_url: base_url.clone(),
            api_key: Secret::new(key),
            capabilities: vec![AiCapability::Transcription],
            max_num_ctx: None,
            request_timeout_secs: None,
            max_concurrent: None,
            managed: true,
            health: EndpointHealth::default(),
        };
        let health = managed_health(&endpoint("good")).await;
        assert_eq!(
            health.state,
            EndpointState::Reachable,
            "{:?}",
            health.detail
        );
        assert_eq!(
            health.served_models,
            vec!["parakeet-tdt-0.6b-v3.oss-align.q8_0".to_string()]
        );
        assert_eq!(
            managed_health(&endpoint("bad")).await.state,
            EndpointState::Unauthorized
        );
        tx.send_modify(|s| s.phase = Phase::Starting);
        let starting = managed_health(&endpoint("good")).await;
        MANAGED_PORTS.write().unwrap().remove(&port);
        assert_eq!(starting.detail.as_deref(), Some("Local AI is starting"));
        listing.assert_async().await;
        refused.assert_async().await;
    }

    /// The exit path of the CLI and the launcher, which cannot await: every managed child
    /// gets SIGTERM, and SIGKILL when it is still there after the grace.
    #[cfg(unix)]
    #[test]
    fn the_blocking_stop_ends_every_managed_child() {
        let mut polite = std::process::Command::new("sleep")
            .arg("60")
            .spawn()
            .unwrap();
        // Ignores SIGTERM, as a runtime stuck in a model load may.
        let mut stubborn = std::process::Command::new("/bin/sh")
            .args(["-c", "trap '' TERM; while true; do sleep 0.1; done"])
            .spawn()
            .unwrap();
        std::thread::sleep(Duration::from_millis(200));
        let mut senders = Vec::new();
        for (port, child) in [(1u16, &polite), (2u16, &stubborn)] {
            let (tx, rx) = tokio::sync::watch::channel(ProcessStatus {
                phase: Phase::Running,
                pid: Some(child.id()),
                port,
                restarts: 0,
                detail: None,
            });
            senders.push(tx);
            MANAGED_PORTS.write().unwrap().insert(
                port,
                ManagedPort {
                    status: rx,
                    readiness: Readiness::Tcp,
                    served: vec![],
                    lists_models: false,
                },
            );
        }
        let started = Instant::now();
        stop_children_blocking(Duration::from_millis(500));
        MANAGED_PORTS.write().unwrap().clear();
        use std::os::unix::process::ExitStatusExt;
        assert_eq!(polite.wait().unwrap().signal(), Some(libc::SIGTERM));
        assert_eq!(stubborn.wait().unwrap().signal(), Some(libc::SIGKILL));
        assert!(started.elapsed() < Duration::from_secs(3));
    }

    /// The executor's shutdown (what a signal runs before the process exits) stops the
    /// runtimes it supervises, one that ignores SIGTERM too, within the deadline a signal
    /// leaves it, and leaves none of them supervised.
    #[cfg(unix)]
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn the_executor_shutdown_stops_the_supervised_runtimes() {
        use crate::config::{Ad4mConfig, GLOBAL_AD4M_CONFIG};
        let store = tempfile::tempdir().unwrap();
        let saved_config = GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        crate::config::set_global_config(Ad4mConfig {
            local_ai_dir: Some(store.path().to_string_lossy().into_owned()),
            ..Default::default()
        });

        // The fake runtimes do not listen themselves: the test holds their ports.
        let mut listeners = Vec::new();
        let mut pids = Vec::new();
        for (provider, script) in [
            (Provider::LlamaServer, "exec sleep 60"),
            // Ignores SIGTERM (inherited through exec), as a runtime stuck in a load may.
            (Provider::NemoSpeech, "trap '' TERM; exec sleep 60"),
        ] {
            let listener = std::net::TcpListener::bind(("127.0.0.1", 0)).unwrap();
            let port = listener.local_addr().unwrap().port();
            listeners.push(listener);
            let supervised = Supervised::start(
                ChildSpec {
                    name: provider.as_str().into(),
                    program: PathBuf::from("/bin/sh"),
                    args: vec!["-c".into(), script.into()],
                    env: vec![],
                    cwd: store.path().to_path_buf(),
                    port,
                    readiness: Readiness::Tcp,
                    log_file: store.path().join(format!("{}.log", provider.as_str())),
                },
                Timing {
                    start_timeout: Duration::from_secs(5),
                    probe_interval: Duration::from_millis(100),
                    backoff: Duration::from_millis(20),
                },
                RestartBudget::standard(),
            );
            let status = supervised.wait_until_settled(Duration::from_secs(10)).await;
            assert_eq!(status.phase, Phase::Running, "{provider:?}");
            pids.push(status.pid.unwrap());
            MANAGED_PORTS.write().unwrap().insert(
                port,
                ManagedPort {
                    status: supervised.watch(),
                    readiness: Readiness::Tcp,
                    served: vec![],
                    lists_models: false,
                },
            );
            RUNNING.lock().await.insert(
                provider,
                Running {
                    supervised,
                    endpoint_id: None,
                },
            );
        }

        let started = Instant::now();
        crate::clean_up_before_exit().await;
        let took = started.elapsed();

        let alive = |pid: u32| {
            // SAFETY: signal 0 only checks that the process exists.
            unsafe { libc::kill(pid as libc::pid_t, 0) == 0 }
        };
        let still_alive: Vec<u32> = pids.iter().copied().filter(|p| alive(*p)).collect();
        let running_left = RUNNING.lock().await.len();
        let ports_left = MANAGED_PORTS.read().unwrap().len();
        *GLOBAL_AD4M_CONFIG.lock().unwrap_or_else(|e| e.into_inner()) = saved_config;
        drop(listeners);

        assert!(still_alive.is_empty(), "still running: {still_alive:?}");
        assert_eq!(running_left, 0, "no runtime is left supervised");
        assert_eq!(ports_left, 0, "no managed port is left");
        assert!(
            took < Duration::from_secs(4),
            "the stop took {took:?}, past the deadline a signal leaves the shutdown"
        );
    }

    /// `ai.localInstall`'s answer is the status after the install: it says no install runs
    /// (the launcher shows "installing" until it does). While one runs, another is refused;
    /// a failed one clears the flag too.
    #[tokio::test]
    async fn a_finished_install_answers_that_no_install_runs() {
        use crate::config::{Ad4mConfig, GLOBAL_AD4M_CONFIG};
        crate::test_utils::ensure_test_db();
        let store = tempfile::tempdir().unwrap();
        let saved_config = GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        crate::config::set_global_config(Ad4mConfig {
            local_ai_dir: Some(store.path().to_string_lossy().into_owned()),
            ..Default::default()
        });

        let finished = run_install(async {
            assert!(
                INSTALLING.load(Ordering::SeqCst),
                "the flag is up meanwhile"
            );
            let second = run_install(async { Ok(()) }).await;
            assert!(
                second.is_err_and(|e| e.downcast_ref::<LocalAiRefused>().is_some()),
                "a second install is refused"
            );
            Ok(())
        })
        .await;
        let failed = run_install(async { Err(anyhow!("the download broke off")) }).await;
        let flag_after_failure = INSTALLING.load(Ordering::SeqCst);
        *GLOBAL_AD4M_CONFIG.lock().unwrap_or_else(|e| e.into_inner()) = saved_config;

        let finished = finished.unwrap();
        assert!(!finished.installing, "{finished:?}");
        assert!(failed.unwrap_err().to_string().contains("broke off"));
        assert!(!flag_after_failure);
    }

    // The first use of a missing default sets up local AI only where local AI may start on
    // its own; with local_ai_autostart off it says what to do instead, and downloads nothing.
    #[tokio::test]
    async fn without_autostart_a_first_use_sets_nothing_up_and_says_how() {
        use crate::config::{Ad4mConfig, GLOBAL_AD4M_CONFIG};
        let store = tempfile::tempdir().unwrap();
        let saved_config = GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        crate::config::set_global_config(Ad4mConfig {
            local_ai_dir: Some(store.path().to_string_lossy().into_owned()),
            app_data_path: Some(store.path().join("app").to_string_lossy().into_owned()),
            local_ai_autostart: Some(false),
            ..Default::default()
        });
        let outcome = ensure_service(Service::Embeddings).await;
        *GLOBAL_AD4M_CONFIG.lock().unwrap_or_else(|e| e.into_inner()) = saved_config;

        let error = outcome.unwrap_err();
        assert!(error.downcast_ref::<LocalAiRefused>().is_some(), "{error}");
        assert!(
            error.to_string().contains("local_ai_autostart is off"),
            "{error}"
        );
        assert_eq!(
            std::fs::read_dir(store.path()).unwrap().count(),
            0,
            "nothing was downloaded"
        );
    }

    /// A start whose caller goes away while the runtime loads (a cancelled RPC) leaves no
    /// runtime: the child stops, and nothing says its port is managed. Else it would serve
    /// on, out of `ai.localStop`'s reach, and the next start would run a second copy.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_start_dropped_while_the_runtime_loads_stops_the_runtime() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        let paths = LocalPaths {
            store: dir.path().join("store"),
            state: dir.path().join("state"),
        };
        // A llama-server that says where it runs and never answers, as one loading models.
        let binary = dir.path().join("llama-server");
        let started = dir.path().join("started");
        std::fs::write(
            &binary,
            format!(
                "#!/bin/sh\necho $$ \"$@\" > '{0}.tmp'\nmv '{0}.tmp' '{0}'\nexec sleep 60\n",
                started.display()
            ),
        )
        .unwrap();
        std::fs::set_permissions(&binary, std::fs::Permissions::from_mode(0o755)).unwrap();
        Selection {
            runtimes: vec![SelectedRuntime {
                provider: Provider::LlamaServer,
                runtime_id: "fake".into(),
                version: "0".into(),
                build: "cpu".into(),
                binary,
                lib_dirs: vec![],
                services: vec![],
                backend: None,
            }],
            models: vec![],
            ports: BTreeMap::new(),
        }
        .save(&paths)
        .unwrap();

        tokio::select! {
            result = start_at(&paths, false) => panic!("the start ended: {result:?}"),
            _ = async {
                while !started.exists() {
                    tokio::time::sleep(Duration::from_millis(20)).await;
                }
            } => {}
        }
        let line = std::fs::read_to_string(&started).unwrap();
        let words: Vec<&str> = line.split_whitespace().collect();
        let pid: u32 = words[0].parse().unwrap();
        let port: u16 = words[words.iter().position(|w| *w == "--port").unwrap() + 1]
            .parse()
            .unwrap();

        assert!(!RUNNING.lock().await.contains_key(&Provider::LlamaServer));
        assert!(
            !MANAGED_PORTS.read().unwrap().contains_key(&port),
            "the port of a start that did not finish is not managed"
        );
        let alive = |pid: u32| {
            // SAFETY: signal 0 only checks that the process exists.
            unsafe { libc::kill(pid as libc::pid_t, 0) == 0 }
        };
        let deadline = Instant::now() + Duration::from_secs(10);
        while alive(pid) && Instant::now() < deadline {
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
        let left = alive(pid);
        if left {
            // SAFETY: kill(2) on the fake runtime this test started.
            unsafe {
                libc::kill(pid as libc::pid_t, libc::SIGKILL);
            }
        }
        assert!(!left, "the runtime of the dropped start still runs");
    }

    #[test]
    fn a_random_key_is_long_and_new_each_time() {
        let a = random_key();
        assert_eq!(a.len(), 64);
        assert_ne!(a, random_key());
    }
}

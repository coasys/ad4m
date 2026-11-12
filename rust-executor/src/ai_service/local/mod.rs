//! One-click local AI: the executor detects this device, picks models that fit it, downloads
//! a pinned model runtime and the models into a per-user store, runs the runtimes as its
//! own supervised children on loopback, and registers them as managed endpoints, models and
//! defaults. The launcher and WE drive it through the same RPCs (`ai.local*`).
//!
//! It downloads only when asked: `ai.localInstall` (an app's call, such as the launcher's
//! setup screen), `ad4m-executor local-ai install`, or the Docker image's `INCLUDE_MODELS`.
//! No request and no executor start installs anything; a request that needs a model of a
//! type the executor has none of gets an error that says how to get one. At start the
//! executor only starts what is installed (`local_ai_autostart`).
//!
//! - `data.rs`: the embedded manifest, recommendations and hardware classes; the classifier.
//! - `hardware.rs`: detection (`ai.localHardware`).
//! - `plan.rs`: the pure plan (`ai.localPlan`).
//! - `install.rs`: the store, resumable verified downloads, unpacking.
//! - `supervisor.rs`: child processes, ports, the restart budget.
//! - `onnx_runtime.rs`: ONNX Runtime for the speech gate where `ort` loads it at run time
//!   (macOS), fetched into the store by an install that sets up transcription.
//! - `setup.rs`: install and remove.
//! - `autostart.rs`: the start of the installed runtimes at executor start.
//! - `runtimes.rs`: start, stop and status of the runtimes.
//! - `health.rs`: the health of managed endpoints, and the GPU check after a start.
//! - `registration.rs`: model registration and defaults.
//! - here: the paths, the API types, the selection, the shared state, hardware and plan.
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

mod autostart;
mod health;
mod registration;
mod runtimes;
mod setup;

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::atomic::AtomicBool;
use ts_rs::TS;

use self::data::{Provider, Service};
use self::hardware::{Backend, LocalHardware};
use self::install::Store;
use self::plan::LocalPlan;
use self::supervisor::{Phase, ProcessStatus, Readiness, StopHandle, Supervised};
#[cfg(test)]
use super::AI_SERVICE;
use super::{endpoints, providers, AIService};
use crate::secret::Secret;

pub use self::autostart::autostart;
pub use self::health::managed_health;
pub(crate) use self::registration::is_managed_endpoint_name;
pub use self::runtimes::{start, status, stop, stop_children_blocking, stop_on_exit};
pub use self::setup::{install, install_into, remove};

/// The shared store: `local_ai_dir` from the config, else `AD4M_LOCAL_AI_DIR`, else the
/// per-user default; made absolute (a runtime starts in its own directory, where a relative
/// path names something else). An error when none is set and there is no per-user default.
pub fn store_root(local_ai_dir: Option<String>) -> Result<PathBuf> {
    store_root_from(
        local_ai_dir,
        std::env::var("AD4M_LOCAL_AI_DIR").ok(),
        install::default_store_root(),
    )
}

fn store_root_from(
    configured: Option<String>,
    env: Option<String>,
    user_default: Option<PathBuf>,
) -> Result<PathBuf> {
    let root = configured
        .filter(|d| !d.is_empty())
        .or_else(|| env.filter(|d| !d.is_empty()))
        .map(PathBuf::from)
        .or(user_default)
        .ok_or_else(|| {
            LocalAiRefused(
                "This user has no data directory for local AI's files: set local_ai_dir \
                 (--local-ai-dir, AD4M_LOCAL_AI_DIR) to a private directory"
                    .into(),
            )
        })?;
    Ok(std::path::absolute(&root)?)
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
    pub fn from_config() -> Result<LocalPaths> {
        let config = crate::config::get_global_config();
        let store = store_root(config.local_ai_dir.clone())?;
        let state = config
            .app_data_path
            .clone()
            .map(|p| PathBuf::from(p).join("local-ai"))
            .unwrap_or_else(|| store.join("state"));
        Ok(LocalPaths { store, state })
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
    /// The installed runtimes start with the executor (`local_ai_autostart`, on unless set
    /// off). Nothing is ever installed at start or on a request: only `ai.localInstall`.
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
    /// The key this start gave the runtime, for a registration done again.
    key: Secret,
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
    /// Asks the supervision to stop, also for a start that has not reached `RUNNING`.
    stop: StopHandle,
}

lazy_static! {
    static ref RUNNING: tokio::sync::Mutex<BTreeMap<Provider, Running>> =
        tokio::sync::Mutex::new(BTreeMap::new());
    /// Held by a start, a stop and a remove for all of their work: they run one at a time.
    /// A second start waits for the first and finds its runtimes in `RUNNING`; a stop
    /// during a start stops what the start started. Not by the exit stop, which must not
    /// wait for a start (the runtime's stop handle reaches a start in progress).
    static ref LIFECYCLE: tokio::sync::Mutex<()> = tokio::sync::Mutex::new(());
    /// Held around each write of `selection.json`, and around a start's read of it and
    /// write back: an install saves without `LIFECYCLE`, so it must not land between them.
    static ref SELECTION_FILE: std::sync::Mutex<()> = std::sync::Mutex::new(());
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
    hardware_at(LocalPaths::from_config()?).await
}

async fn hardware_at(paths: LocalPaths) -> Result<LocalHardware> {
    let data = data::embedded()?;
    Ok(tokio::task::spawn_blocking(move || hardware::detect(&paths.store, &data.classes)).await?)
}

/// The plan for this device against the store's files.
pub async fn current_plan() -> Result<LocalPlan> {
    plan_at(&LocalPaths::from_config()?).await
}

async fn plan_at(paths: &LocalPaths) -> Result<LocalPlan> {
    plan_for(
        paths,
        crate::config::get_global_config().local_ai_device,
        None,
    )
    .await
}

/// The plan for this device held to `device` (`None`: the best one detected), against the
/// store's files; of `services` only when given (an install of some services fits by
/// them, not by the whole set). Its sizes count ONNX Runtime where an install with
/// transcription fetches it (macOS), so what the setup screen shows is all it downloads.
async fn plan_for(
    paths: &LocalPaths,
    device: Option<Backend>,
    services: Option<&[Service]>,
) -> Result<LocalPlan> {
    let data = data::embedded()?;
    let hardware = hardware_at(paths.clone()).await?;
    let store = Store::new(&paths.store);
    let installed = {
        let manifest = data.manifest.clone();
        tokio::task::spawn_blocking(move || store.installed_digests(&manifest)).await?
    };
    let mut plan = plan::plan_services_with_device(
        data,
        &hardware,
        device,
        &|sha| installed.contains(sha),
        services.unwrap_or(&plan::PLANNED_SERVICES),
    );
    if plan.models.iter().any(|m| m.service == Service::Stt) {
        if let Some((size, have)) = onnx_runtime::planned(&Store::new(&paths.store)) {
            plan.total_bytes += size;
            if !have {
                plan.download_bytes += size;
            }
        }
    }
    Ok(plan)
}

/// What the tests of the modules here share: the lock that runs them one at a time and
/// the builders of a selected model and a process status.
#[cfg(test)]
mod test_support {
    use super::data::{Provider, Service};
    use super::supervisor::{Phase, ProcessStatus};
    use super::SelectedModel;
    use std::path::PathBuf;

    /// One test at a time of those that use the process-wide state (`RUNNING`,
    /// `MANAGED_PORTS`, `GPU_CHECKS`, the install flag, the global service and database):
    /// a plain `cargo test` runs tests in parallel.
    pub(super) static TEST_LOCK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

    pub(super) fn selected(service: Service, id: &str, provider: Provider) -> SelectedModel {
        SelectedModel {
            service,
            model_id: id.into(),
            provider,
            path: PathBuf::from(format!("/store/models/{id}.gguf")),
            server_flags: vec![],
        }
    }

    pub(super) fn status_in(phase: Phase) -> ProcessStatus {
        ProcessStatus {
            phase,
            pid: None,
            port: 1,
            restarts: 0,
            detail: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Without a configured store and without a per-user data directory there is no store:
    /// never the shared temporary directory. A relative store is made absolute.
    #[test]
    fn the_store_is_never_the_temporary_directory_and_always_absolute() {
        let none = store_root_from(None, None, None).unwrap_err();
        assert!(none.downcast_ref::<LocalAiRefused>().is_some(), "{none}");
        assert!(store_root_from(Some(String::new()), Some(String::new()), None).is_err());

        let relative = store_root_from(Some("models".into()), None, None).unwrap();
        assert!(relative.is_absolute());
        assert_eq!(relative, std::env::current_dir().unwrap().join("models"));
        let user = PathBuf::from("/home/u/.local/share/ad4m/local-ai");
        assert_eq!(
            store_root_from(None, None, Some(user.clone())).unwrap(),
            user
        );
        assert_eq!(
            store_root_from(None, Some("/env".into()), Some(user)).unwrap(),
            PathBuf::from("/env")
        );
    }
}

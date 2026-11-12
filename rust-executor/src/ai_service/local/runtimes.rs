//! Start, stop and status of the runtimes: `ai.localStatus`, `ai.localStart` and
//! `ai.localStop`, the child spec of each runtime (llama-server's router presets, nemo-speech's
//! flags and key file), `pids.json` and the reaping of what a crash left, and the stop at
//! executor exit.

use anyhow::{anyhow, Result};
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};

use super::data::{Provider, Service};
use super::first_use::autostart_enabled;
use super::hardware::Backend;
use super::health::{
    gpu_check, gpu_check_for, spawn_gpu_check, spawn_health_follower, spawn_pid_follower,
};
use super::plan;
use super::registration::{endpoint_name, register, registration_missing, unregister};
use super::supervisor::{self, ChildSpec, Phase, Readiness, RestartBudget, Supervised, Timing};
use super::{
    LocalAiRefused, LocalPaths, LocalStatus, ManagedPort, ModelStatus, Running, RuntimeStatus,
    SelectedModel, SelectedRuntime, Selection, GPU_CHECKS, INSTALLING, LIFECYCLE, MANAGED_PORTS,
    RUNNING, SELECTION_FILE,
};
use crate::secret::Secret;
use crate::types::Endpoint;

/// `ai.localStatus`.
pub async fn status() -> LocalStatus {
    let paths = LocalPaths::from_config();
    let selection = paths.as_ref().ok().and_then(Selection::load);
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
        storage_dir: paths
            .map(|p| p.store.to_string_lossy().into_owned())
            .unwrap_or_default(),
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

/// `ai.localStart`: start the installed runtimes and register them. Defaults are set
/// only for types that have none.
pub async fn start() -> Result<LocalStatus> {
    let paths = LocalPaths::from_config()?;
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
pub(super) fn llama_presets(models: &[&SelectedModel]) -> String {
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
pub(super) fn pid_file(paths: &LocalPaths) -> PathBuf {
    paths.state.join("pids.json")
}

pub(super) async fn write_pids(paths: &LocalPaths) {
    write_pids_with(paths, None).await
}

/// `pids.json`: the runtimes in `RUNNING`, and `starting` (a runtime a start has spawned
/// and not handed to `RUNNING` yet, while its models load).
pub(super) async fn write_pids_with(paths: &LocalPaths, starting: Option<(u32, PathBuf)>) {
    let running = RUNNING.lock().await;
    let mut pids: Vec<(u32, PathBuf)> = running
        .values()
        .filter_map(|r| {
            Some((
                r.supervised.status().pid?,
                r.supervised.spec.program.clone(),
            ))
        })
        .collect();
    drop(running);
    if let Some(starting) = starting {
        if !pids.iter().any(|(pid, _)| *pid == starting.0) {
            pids.push(starting);
        }
    }
    if let Ok(text) = serde_json::to_string(&pids) {
        let _ = std::fs::write(pid_file(paths), text);
    }
}

pub(super) fn reap_from_pid_file(paths: &LocalPaths) {
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

/// Whether the runtime on `port` was started with other models than `selection` has for
/// `runtime`. A port this executor does not manage: no.
fn serves_other_models(port: u16, selection: &Selection, runtime: &SelectedRuntime) -> bool {
    let mut wanted: Vec<&str> = selection
        .models_of(runtime.provider)
        .iter()
        .map(|m| m.model_id.as_str())
        .collect();
    wanted.sort_unstable();
    MANAGED_PORTS
        .read()
        .unwrap_or_else(|p| p.into_inner())
        .get(&port)
        .is_some_and(|managed| {
            let mut served: Vec<&str> = managed.served.iter().map(String::as_str).collect();
            served.sort_unstable();
            served != wanted
        })
}

pub(super) async fn start_at(paths: &LocalPaths, make_default: bool) -> Result<()> {
    let _lifecycle = LIFECYCLE.lock().await;
    let mut selection = Selection::load(paths)
        .ok_or_else(|| LocalAiRefused("Local AI is not installed: call ai.localInstall".into()))?;
    // The store may have lost them (a Docker container recreated with the store outside
    // its volume, another executor's `ai.localRemove` with `deleteFiles`).
    let missing: Vec<String> = selection
        .runtimes
        .iter()
        .map(|r| &r.binary)
        .chain(selection.models.iter().map(|m| &m.path))
        .filter(|p| !p.exists())
        .map(|p| p.to_string_lossy().into_owned())
        .collect();
    if !missing.is_empty() {
        return Err(LocalAiRefused(format!(
            "Local AI's files are missing from the store ({}): run ai.localInstall again",
            missing.join(", ")
        ))
        .into());
    }
    std::fs::create_dir_all(&paths.state)?;
    if RUNNING.lock().await.is_empty() {
        reap_from_pid_file(paths);
    }
    // A runtime the selection no longer has (`ai.localInstall` with fewer services) is not
    // started again: its endpoint and models go.
    let kept: Vec<&str> = selection
        .runtimes
        .iter()
        .map(|r| endpoint_name(r.provider))
        .collect();
    if let Err(e) = unregister(|endpoint| !kept.contains(&endpoint.name.as_str())).await {
        log::warn!("Local AI: the endpoints of dropped runtimes stay: {e:#}");
    }
    let avoid = supervisor::avoided_ports();
    for runtime in selection.runtimes.clone() {
        // Failed (restarts spent, a binary that does not start) or stopped: start it again,
        // as a new runtime. Running or starting: its registration, unless it is there (one
        // that failed, or whose start was cancelled before it, nothing else repairs).
        // A runtime that serves other models than the selection has for it
        // (`ai.localInstall` with `start: false` while it ran): stopped and started again.
        let mut stale: Option<Running> = None;
        let registered_as = {
            let mut running = RUNNING.lock().await;
            let live = running.get(&runtime.provider).map(|r| {
                (
                    r.supervised.status().phase,
                    r.endpoint_id.clone(),
                    r.key.expose().to_string(),
                    r.supervised.spec.port,
                )
            });
            match live {
                Some((Phase::Failed | Phase::Stopped, ..)) => {
                    if let Some(old) = running.remove(&runtime.provider) {
                        MANAGED_PORTS
                            .write()
                            .unwrap_or_else(|p| p.into_inner())
                            .remove(&old.supervised.spec.port);
                    }
                    None
                }
                Some((.., port)) if serves_other_models(port, &selection, &runtime) => {
                    stale = running.remove(&runtime.provider);
                    None
                }
                Some((_, endpoint_id, key, port)) => Some((endpoint_id, key, port)),
                None => None,
            }
        };
        if let Some(old) = stale {
            log::info!(
                "Local AI: {} serves other models than the selection: starting it again",
                runtime.provider.as_str()
            );
            stop_runtime(old).await;
        }
        if let Some((endpoint_id, key, port)) = registered_as {
            if registration_missing(&selection, &runtime, endpoint_id.as_deref())? {
                log::info!(
                    "Local AI: {} runs without its registration: registering it",
                    runtime.provider.as_str()
                );
                let id = register(&selection, &runtime, port, &key, make_default).await?;
                if let Some(running) = RUNNING.lock().await.get_mut(&runtime.provider) {
                    if running.endpoint_id.as_deref() != Some(id.as_str()) {
                        running.endpoint_id = Some(id.clone());
                        spawn_health_follower(id, running.supervised.watch());
                    }
                }
            }
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
        let program = spec.program.clone();
        let supervised = Supervised::start(spec, Timing::default(), RestartBudget::standard());
        spawn_pid_follower(paths.clone(), supervised.watch(), program);
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
                    stop: supervised.stop_handle(),
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
                key: Secret::new(key.clone()),
            },
        );
        let port = port.keep();
        // Before the registration, which may fail: the child runs and must be found.
        write_pids(paths).await;
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
                drop(spawn_gpu_check(
                    runtime.provider,
                    running.supervised.watch(),
                    model_bytes,
                ));
            }
        }
    }
    // `ai.localInstall` saves its selection without the lifecycle lock, so it may have
    // done so while the runtimes above loaded: that selection stays, with the ports of
    // this start. The install's own start, or the next one, runs its models.
    let _file = SELECTION_FILE.lock().unwrap();
    match Selection::load(paths) {
        Some(mut saved) => {
            saved.ports = selection.ports;
            saved.save(paths)?;
        }
        None => selection.save(paths)?,
    }
    Ok(())
}

/// Stop every runtime this executor runs. Endpoints and models stay registered (down).
pub(super) async fn stop_all() {
    let _lifecycle = LIFECYCLE.lock().await;
    stop_running().await;
}

/// [`stop_all`] for a caller that holds [`LIFECYCLE`] or must not wait for it.
pub(super) async fn stop_running() {
    let running: Vec<Running> = std::mem::take(&mut *RUNNING.lock().await)
        .into_values()
        .collect();
    let mut endpoint_ids = Vec::new();
    for running in running {
        endpoint_ids.extend(stop_runtime(running).await);
    }
    for id in endpoint_ids {
        if let Ok(Some(endpoint)) = crate::db_backend::db_backend().get_endpoint(&id) {
            let _ = super::endpoints::check_health(&endpoint).await;
        }
    }
}

/// Stop `running` and forget its port. Answers its endpoint id, whose models stay
/// registered (down).
async fn stop_runtime(running: Running) -> Option<String> {
    let port = running.supervised.spec.port;
    running.supervised.stop().await;
    MANAGED_PORTS
        .write()
        .unwrap_or_else(|p| p.into_inner())
        .remove(&port);
    running.endpoint_id
}

/// `ai.localStop`.
pub async fn stop() -> LocalStatus {
    stop_all().await;
    if let Ok(paths) = LocalPaths::from_config() {
        let _ = std::fs::remove_file(pid_file(&paths));
    }
    status().await
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
    stop_running().await;
    if let Ok(paths) = LocalPaths::from_config() {
        let _ = std::fs::remove_file(pid_file(&paths));
    }
}

/// Stop the runtimes from a thread that cannot await, right before the process exits:
/// the executor's shutdown ([`stop_on_exit`]) and the launcher's Quit, which calls
/// `std::process::exit` without that shutdown. SIGTERM, up to `grace` for them to go, then
/// SIGKILL. After a crash Linux kills them anyway (`PR_SET_PDEATHSIG`); macOS and Windows
/// would not.
pub fn stop_children_blocking(grace: Duration) {
    use sysinfo::{Pid, ProcessesToUpdate, Signal, System};
    let pids: Vec<Pid> = {
        let ports = MANAGED_PORTS.read().unwrap_or_else(|p| p.into_inner());
        // No restart of a child that exits now: also one a start has not handed to
        // `RUNNING` yet.
        for port in ports.values() {
            port.stop.request_stop();
        }
        ports
            .values()
            .filter_map(|m| m.status.borrow().pid)
            .map(Pid::from_u32)
            .collect()
    };
    if pids.is_empty() {
        return;
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
    use crate::ai_service::local::registration::LLAMA_ENDPOINT;
    use crate::ai_service::local::supervisor::{ProcessStatus, StopHandle};
    use crate::ai_service::local::test_support::{selected, TEST_LOCK};
    use crate::ai_service::AIService;
    use std::collections::BTreeMap;

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

    /// The exit path of the CLI and the launcher, which cannot await: every managed child
    /// gets SIGTERM, and SIGKILL when it is still there after the grace.
    #[cfg(unix)]
    #[test]
    fn the_blocking_stop_ends_every_managed_child() {
        let _serial = TEST_LOCK.blocking_lock();
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
                    stop: StopHandle::default(),
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
        let _serial = TEST_LOCK.lock().await;
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
                    stop: supervised.stop_handle(),
                },
            );
            RUNNING.lock().await.insert(
                provider,
                Running {
                    supervised,
                    endpoint_id: None,
                    key: Secret::new("k"),
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

    /// A start whose caller goes away while the runtime loads (a cancelled RPC) leaves no
    /// runtime: the child stops, and nothing says its port is managed. Else it would serve
    /// on, out of `ai.localStop`'s reach, and the next start would run a second copy.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_start_dropped_while_the_runtime_loads_stops_the_runtime() {
        let _serial = TEST_LOCK.lock().await;
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

    /// A llama-server that adds its pid to `spawned` and never answers, as one loading
    /// models, installed as the selection of `dir`'s state.
    #[cfg(unix)]
    fn never_ready_llama(dir: &Path) -> (LocalPaths, PathBuf) {
        use std::os::unix::fs::PermissionsExt;
        let paths = LocalPaths {
            store: dir.join("store"),
            state: dir.join("state"),
        };
        let binary = dir.join("llama-server");
        let spawned = dir.join("spawned");
        std::fs::write(
            &binary,
            format!(
                "#!/bin/sh\necho $$ >> '{}'\nexec sleep 60\n",
                spawned.display()
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
        (paths, spawned)
    }

    #[cfg(unix)]
    fn spawned_pids(spawned: &Path) -> Vec<u32> {
        std::fs::read_to_string(spawned)
            .unwrap_or_default()
            .lines()
            .filter_map(|l| l.trim().parse().ok())
            .collect()
    }

    /// Wait up to 10 s for `pids` to end; SIGKILL what is left. Answers the pids left.
    #[cfg(unix)]
    async fn wait_gone(pids: &[u32]) -> Vec<u32> {
        let alive = |pid: u32| {
            // SAFETY: signal 0 only checks that the process exists.
            unsafe { libc::kill(pid as libc::pid_t, 0) == 0 }
        };
        let deadline = Instant::now() + Duration::from_secs(10);
        while pids.iter().any(|p| alive(*p)) && Instant::now() < deadline {
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
        let left: Vec<u32> = pids.iter().copied().filter(|p| alive(*p)).collect();
        for pid in &left {
            // SAFETY: kill(2) on a fake runtime this test started.
            unsafe {
                libc::kill(*pid as libc::pid_t, libc::SIGKILL);
            }
        }
        left
    }

    /// Two starts at once (autostart, and a click on "Start local AI" while it loads): one
    /// runtime. The second waits for the first, and does not spawn a second copy.
    #[cfg(unix)]
    #[tokio::test]
    async fn two_starts_at_once_spawn_one_runtime() {
        let _serial = TEST_LOCK.lock().await;
        let dir = tempfile::tempdir().unwrap();
        let (paths, spawned) = never_ready_llama(dir.path());
        tokio::select! {
            result = async { tokio::join!(start_at(&paths, false), start_at(&paths, false)) } => {
                panic!("a start ended: {result:?}")
            }
            _ = async {
                while spawned_pids(&spawned).is_empty() {
                    tokio::time::sleep(Duration::from_millis(20)).await;
                }
                tokio::time::sleep(Duration::from_millis(1500)).await;
            } => {}
        }
        // Both starts are dropped: their runtimes stop.
        let pids = spawned_pids(&spawned);
        let left = wait_gone(&pids).await;
        assert_eq!(pids.len(), 1, "runtimes spawned: {pids:?}");
        assert!(left.is_empty(), "still running: {left:?}");
    }

    /// A runtime whose restarts are spent stays in `RUNNING`, failed: the next start
    /// replaces it, and forgets its port.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_failed_runtime_starts_again() {
        let _serial = TEST_LOCK.lock().await;
        let dir = tempfile::tempdir().unwrap();
        let (paths, spawned) = never_ready_llama(dir.path());
        let failed = Supervised::start(
            ChildSpec {
                name: "fake".into(),
                program: PathBuf::from("/bin/sh"),
                args: vec!["-c".into(), "exit 3".into()],
                env: vec![],
                cwd: dir.path().to_path_buf(),
                port: supervisor::choose_port(None, &[]).unwrap(),
                readiness: Readiness::Tcp,
                log_file: dir.path().join("failed.log"),
            },
            Timing {
                start_timeout: Duration::from_secs(5),
                probe_interval: Duration::from_millis(100),
                backoff: Duration::from_millis(20),
            },
            RestartBudget::new(0, Duration::from_secs(300)),
        );
        assert_eq!(
            failed
                .wait_until_settled(Duration::from_secs(10))
                .await
                .phase,
            Phase::Failed
        );
        let old_port = failed.spec.port;
        MANAGED_PORTS.write().unwrap().insert(
            old_port,
            ManagedPort {
                status: failed.watch(),
                readiness: Readiness::Tcp,
                served: vec![],
                lists_models: false,
                stop: failed.stop_handle(),
            },
        );
        RUNNING.lock().await.insert(
            Provider::LlamaServer,
            Running {
                supervised: failed,
                endpoint_id: None,
                key: Secret::new("k"),
            },
        );

        let started = tokio::select! {
            result = start_at(&paths, false) => Err(format!("the start ended: {result:?}")),
            _ = async {
                while spawned_pids(&spawned).is_empty() {
                    tokio::time::sleep(Duration::from_millis(20)).await;
                }
            } => Ok(()),
        };
        let old_port_kept = MANAGED_PORTS.write().unwrap().remove(&old_port).is_some();
        if let Some(left) = RUNNING.lock().await.remove(&Provider::LlamaServer) {
            left.supervised.stop().await;
        }
        let left = wait_gone(&spawned_pids(&spawned)).await;

        started.unwrap();
        assert!(!old_port_kept, "the failed runtime's port is forgotten");
        assert!(left.is_empty(), "still running: {left:?}");
    }

    /// A selection whose files are gone (a container recreated without its store, another
    /// executor's `ai.localRemove` with `deleteFiles`) starts nothing and says what to do.
    #[tokio::test]
    async fn a_selection_whose_files_are_gone_says_to_install_again() {
        let _serial = TEST_LOCK.lock().await;
        let dir = tempfile::tempdir().unwrap();
        let paths = LocalPaths {
            store: dir.path().join("store"),
            state: dir.path().join("state"),
        };
        Selection {
            runtimes: vec![SelectedRuntime {
                provider: Provider::LlamaServer,
                runtime_id: "llama.cpp".into(),
                version: "b1".into(),
                build: "linux-x64-cpu".into(),
                binary: dir.path().join("store/runtime/llama-server"),
                lib_dirs: vec![],
                services: vec![Service::Llm],
                backend: Some(Backend::Cpu),
            }],
            models: vec![selected(Service::Llm, "chat", Provider::LlamaServer)],
            ports: BTreeMap::new(),
        }
        .save(&paths)
        .unwrap();

        let outcome = start_at(&paths, false).await;
        let supervised = RUNNING.lock().await.remove(&Provider::LlamaServer);
        if let Some(left) = supervised {
            MANAGED_PORTS
                .write()
                .unwrap()
                .remove(&left.supervised.spec.port);
            left.supervised.stop().await;
            panic!("a runtime was started: {outcome:?}");
        }
        let error = outcome.unwrap_err();
        assert!(error.downcast_ref::<LocalAiRefused>().is_some(), "{error}");
        let text = error.to_string();
        assert!(
            text.contains("ai.localInstall") && text.contains("llama-server"),
            "{text}"
        );
    }

    /// The executor's exit during a start: the runtime that is loading gets its stop too,
    /// so its supervisor does not restart it after the SIGTERM.
    #[cfg(unix)]
    #[tokio::test]
    async fn the_exit_stop_also_stops_a_runtime_that_is_starting() {
        let _serial = TEST_LOCK.lock().await;
        let dir = tempfile::tempdir().unwrap();
        let port = supervisor::choose_port(None, &[]).unwrap();
        let supervised = Supervised::start(
            ChildSpec {
                name: "fake".into(),
                program: PathBuf::from("/bin/sh"),
                args: vec!["-c".into(), "exec sleep 60".into()],
                env: vec![],
                cwd: dir.path().to_path_buf(),
                port,
                readiness: Readiness::Tcp,
                log_file: dir.path().join("fake.log"),
            },
            Timing {
                start_timeout: Duration::from_secs(30),
                probe_interval: Duration::from_millis(100),
                backoff: Duration::from_millis(20),
            },
            RestartBudget::standard(),
        );
        let mut status = supervised.watch();
        let first = tokio::time::timeout(
            Duration::from_secs(10),
            status.wait_for(|s| s.pid.is_some()),
        )
        .await
        .unwrap()
        .unwrap()
        .pid
        .unwrap();
        // As a start in progress has it: in `MANAGED_PORTS`, not yet in `RUNNING`.
        MANAGED_PORTS.write().unwrap().insert(
            port,
            ManagedPort {
                status: supervised.watch(),
                readiness: Readiness::Tcp,
                served: vec![],
                lists_models: false,
                stop: supervised.stop_handle(),
            },
        );
        stop_children_blocking(Duration::from_millis(500));
        MANAGED_PORTS.write().unwrap().remove(&port);
        tokio::time::sleep(Duration::from_secs(1)).await;
        let after = supervised.status();
        let pids: Vec<u32> = [Some(first), after.pid].into_iter().flatten().collect();
        supervised.stop().await;
        let left = wait_gone(&pids).await;

        assert_eq!(after.phase, Phase::Stopped, "{after:?}");
        assert!(left.is_empty(), "still running: {left:?}");
    }

    /// A runtime that runs but whose registration failed (or was cancelled): the next
    /// `ai.localStart` registers it, and does not start a second copy.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_start_registers_a_running_runtime_that_has_no_endpoint() {
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let saved = super::super::AI_SERVICE
            .lock()
            .await
            .replace(AIService::empty());
        let db = crate::db_backend::db_backend();
        let dir = tempfile::tempdir().unwrap();
        let (paths, spawned) = never_ready_llama(dir.path());
        // A runtime that answers: the port of a listener this test holds.
        let listener = std::net::TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        let supervised = Supervised::start(
            ChildSpec {
                name: "fake".into(),
                program: PathBuf::from("/bin/sh"),
                args: vec!["-c".into(), "exec sleep 60".into()],
                env: vec![],
                cwd: dir.path().to_path_buf(),
                port,
                readiness: Readiness::Tcp,
                log_file: dir.path().join("fake.log"),
            },
            Timing {
                start_timeout: Duration::from_secs(5),
                probe_interval: Duration::from_millis(100),
                backoff: Duration::from_millis(20),
            },
            RestartBudget::standard(),
        );
        assert_eq!(
            supervised
                .wait_until_settled(Duration::from_secs(10))
                .await
                .phase,
            Phase::Running
        );
        MANAGED_PORTS.write().unwrap().insert(
            port,
            ManagedPort {
                status: supervised.watch(),
                readiness: Readiness::Tcp,
                served: vec![],
                lists_models: false,
                stop: supervised.stop_handle(),
            },
        );
        RUNNING.lock().await.insert(
            Provider::LlamaServer,
            Running {
                supervised,
                endpoint_id: None,
                key: Secret::new("k3y"),
            },
        );

        let outcome = tokio::time::timeout(Duration::from_secs(20), start_at(&paths, false)).await;
        let endpoint = db.get_endpoint_by_name(LLAMA_ENDPOINT).unwrap();
        let recorded = RUNNING
            .lock()
            .await
            .get(&Provider::LlamaServer)
            .and_then(|r| r.endpoint_id.clone());
        if let Some(left) = RUNNING.lock().await.remove(&Provider::LlamaServer) {
            left.supervised.stop().await;
        }
        MANAGED_PORTS.write().unwrap().remove(&port);
        if let Some(endpoint) = &endpoint {
            db.remove_endpoint(&endpoint.id).unwrap();
        }
        *super::super::AI_SERVICE.lock().await = saved;
        let left = wait_gone(&spawned_pids(&spawned)).await;

        outcome.expect("the start ends").unwrap();
        let endpoint = endpoint.expect("the running runtime is registered");
        assert!(endpoint.managed);
        assert_eq!(endpoint.base_url.port(), Some(port));
        assert!(endpoint.api_key.expose_for_compare("k3y"));
        assert_eq!(recorded, Some(endpoint.id));
        assert!(
            spawned_pids(&spawned).is_empty(),
            "no second copy: {left:?}"
        );
    }

    /// `ai.localInstall {start: false}` changed the models while the runtime ran. The next
    /// `ai.localStart` registered the new models against the process that serves the old
    /// ones. It starts the runtime again with the new models.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_start_restarts_a_running_runtime_whose_models_the_selection_changed() {
        use std::os::unix::fs::PermissionsExt;
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let saved = super::super::AI_SERVICE
            .lock()
            .await
            .replace(AIService::empty());
        let db = crate::db_backend::db_backend();
        let dir = tempfile::tempdir().unwrap();
        let paths = LocalPaths {
            store: dir.path().join("store"),
            state: dir.path().join("state"),
        };
        // The new runtime answers `/health`: a file server on the port it is given
        // (`--host 127.0.0.1 --port N ...`).
        let www = dir.path().join("www");
        std::fs::create_dir_all(&www).unwrap();
        std::fs::write(www.join("health"), "ok").unwrap();
        let spawned = dir.path().join("spawned");
        let binary = dir.path().join("llama-server");
        std::fs::write(
            &binary,
            format!(
                "#!/bin/sh\necho $$ >> '{}'\ncd '{}' && exec python3 -m http.server \"$4\" --bind 127.0.0.1\n",
                spawned.display(),
                www.display()
            ),
        )
        .unwrap();
        std::fs::set_permissions(&binary, std::fs::Permissions::from_mode(0o755)).unwrap();
        let mut new_chat = selected(Service::Llm, "new-chat", Provider::LlamaServer);
        new_chat.path = dir.path().join("new-chat.gguf");
        std::fs::write(&new_chat.path, "gguf").unwrap();
        Selection {
            runtimes: vec![SelectedRuntime {
                provider: Provider::LlamaServer,
                runtime_id: "fake".into(),
                version: "0".into(),
                build: "cpu".into(),
                binary,
                lib_dirs: vec![],
                services: vec![Service::Llm],
                backend: None,
            }],
            models: vec![new_chat],
            ports: BTreeMap::new(),
        }
        .save(&paths)
        .unwrap();

        // The runtime as the old selection started it: it serves `old-chat`.
        let listener = std::net::TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        let old = Supervised::start(
            ChildSpec {
                name: "fake".into(),
                program: PathBuf::from("/bin/sh"),
                args: vec!["-c".into(), "exec sleep 60".into()],
                env: vec![],
                cwd: dir.path().to_path_buf(),
                port,
                readiness: Readiness::Tcp,
                log_file: dir.path().join("fake.log"),
            },
            Timing {
                start_timeout: Duration::from_secs(5),
                probe_interval: Duration::from_millis(100),
                backoff: Duration::from_millis(20),
            },
            RestartBudget::standard(),
        );
        let settled = old.wait_until_settled(Duration::from_secs(10)).await;
        assert_eq!(settled.phase, Phase::Running);
        let old_pid = settled.pid.expect("the old runtime has a pid");
        MANAGED_PORTS.write().unwrap().insert(
            port,
            ManagedPort {
                status: old.watch(),
                readiness: Readiness::Tcp,
                served: vec!["old-chat".into()],
                lists_models: false,
                stop: old.stop_handle(),
            },
        );
        RUNNING.lock().await.insert(
            Provider::LlamaServer,
            Running {
                supervised: old,
                endpoint_id: None,
                key: Secret::new("k3y"),
            },
        );

        let outcome = tokio::time::timeout(Duration::from_secs(30), start_at(&paths, false)).await;
        let endpoint = db.get_endpoint_by_name(LLAMA_ENDPOINT).unwrap();
        let mut names: Vec<String> = endpoint
            .iter()
            .flat_map(|e| db.endpoint_model_ids(&e.id).unwrap())
            .filter_map(|id| db.get_model(id).unwrap().map(|m| m.name))
            .collect();
        names.sort();
        let running_pid = RUNNING
            .lock()
            .await
            .get(&Provider::LlamaServer)
            .and_then(|r| r.supervised.status().pid);
        if let Some(left) = RUNNING.lock().await.remove(&Provider::LlamaServer) {
            let left_port = left.supervised.spec.port;
            left.supervised.stop().await;
            MANAGED_PORTS.write().unwrap().remove(&left_port);
        }
        MANAGED_PORTS.write().unwrap().remove(&port);
        drop(listener);
        if let Some(endpoint) = &endpoint {
            let service = AIService::global_instance().await.unwrap();
            for id in db.endpoint_model_ids(&endpoint.id).unwrap() {
                service.remove_model(id).await.unwrap();
            }
            db.remove_endpoint(&endpoint.id).unwrap();
        }
        *super::super::AI_SERVICE.lock().await = saved;
        let old_left = wait_gone(&[old_pid]).await;
        let new_left = wait_gone(&spawned_pids(&spawned)).await;

        outcome.expect("the start ends").unwrap();
        assert_eq!(
            spawned_pids(&spawned).len(),
            1,
            "the runtime started again with the new models"
        );
        assert!(running_pid.is_some_and(|pid| pid != old_pid));
        assert!(old_left.is_empty(), "the old runtime still runs");
        assert!(new_left.is_empty(), "still running: {new_left:?}");
        assert_eq!(names, vec!["new-chat"], "the endpoint lists the new models");
    }

    /// `ai.localInstall` saved a new selection while a start loaded the models of the old
    /// one (autostart after a boot, or `ai.localStart`). The start wrote the selection it
    /// had loaded back when it ended, so the install's own start ran the old models again.
    /// A start keeps the selection it finds at its end, with the ports it chose.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_start_keeps_the_selection_an_install_saved_while_it_loaded() {
        use std::os::unix::fs::PermissionsExt;
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let saved = super::super::AI_SERVICE
            .lock()
            .await
            .replace(AIService::empty());
        let db = crate::db_backend::db_backend();
        let dir = tempfile::tempdir().unwrap();
        let paths = LocalPaths {
            store: dir.path().join("store"),
            state: dir.path().join("state"),
        };
        // A runtime that loads for a second, then answers `/health`: a file server on the
        // port it is given (`--host 127.0.0.1 --port N ...`).
        let www = dir.path().join("www");
        std::fs::create_dir_all(&www).unwrap();
        std::fs::write(www.join("health"), "ok").unwrap();
        let spawned = dir.path().join("spawned");
        let binary = dir.path().join("llama-server");
        std::fs::write(
            &binary,
            format!(
                "#!/bin/sh\necho $$ >> '{}'\nsleep 1\ncd '{}' && exec python3 -m http.server \"$4\" --bind 127.0.0.1\n",
                spawned.display(),
                www.display()
            ),
        )
        .unwrap();
        std::fs::set_permissions(&binary, std::fs::Permissions::from_mode(0o755)).unwrap();
        let selection_of = |id: &str| {
            let mut model = selected(Service::Llm, id, Provider::LlamaServer);
            model.path = dir.path().join(format!("{id}.gguf"));
            std::fs::write(&model.path, "gguf").unwrap();
            Selection {
                runtimes: vec![SelectedRuntime {
                    provider: Provider::LlamaServer,
                    runtime_id: "fake".into(),
                    version: "0".into(),
                    build: "cpu".into(),
                    binary: binary.clone(),
                    lib_dirs: vec![],
                    services: vec![Service::Llm],
                    backend: None,
                }],
                models: vec![model],
                ports: BTreeMap::new(),
            }
        };
        selection_of("old-chat").save(&paths).unwrap();

        // The install saves its selection while the start waits for the runtime.
        let (outcome, _) = tokio::join!(
            tokio::time::timeout(Duration::from_secs(30), start_at(&paths, false)),
            async {
                let deadline = Instant::now() + Duration::from_secs(10);
                while spawned_pids(&spawned).is_empty() && Instant::now() < deadline {
                    tokio::time::sleep(Duration::from_millis(20)).await;
                }
                selection_of("new-chat").save(&paths).unwrap();
            }
        );
        let kept = Selection::load(&paths);
        let port = RUNNING
            .lock()
            .await
            .get(&Provider::LlamaServer)
            .map(|r| r.supervised.spec.port);
        if let Some(left) = RUNNING.lock().await.remove(&Provider::LlamaServer) {
            let left_port = left.supervised.spec.port;
            left.supervised.stop().await;
            MANAGED_PORTS.write().unwrap().remove(&left_port);
        }
        if let Some(endpoint) = db.get_endpoint_by_name(LLAMA_ENDPOINT).unwrap() {
            let service = AIService::global_instance().await.unwrap();
            for id in db.endpoint_model_ids(&endpoint.id).unwrap() {
                service.remove_model(id).await.unwrap();
            }
            db.remove_endpoint(&endpoint.id).unwrap();
        }
        *super::super::AI_SERVICE.lock().await = saved;
        let left = wait_gone(&spawned_pids(&spawned)).await;

        outcome.expect("the start ends").unwrap();
        assert!(left.is_empty(), "still running: {left:?}");
        let kept = kept.expect("a selection is saved");
        let models: Vec<&str> = kept.models.iter().map(|m| m.model_id.as_str()).collect();
        assert_eq!(models, vec!["new-chat"], "the install's selection stays");
        assert!(port.is_some(), "the start left its runtime running");
        assert_eq!(
            kept.ports.get(Provider::LlamaServer.as_str()).copied(),
            port,
            "with the port this start chose"
        );
    }

    /// `pids.json` names a runtime from its spawn, not from the end of its model load: an
    /// executor that crashes while the models load leaves no runtime the next start misses.
    #[cfg(unix)]
    #[tokio::test]
    async fn the_pid_file_names_a_runtime_while_it_loads() {
        let _serial = TEST_LOCK.lock().await;
        let dir = tempfile::tempdir().unwrap();
        let (paths, spawned) = never_ready_llama(dir.path());
        let named = tokio::select! {
            result = start_at(&paths, false) => Err(format!("the start ended: {result:?}")),
            named = async {
                while spawned_pids(&spawned).is_empty() {
                    tokio::time::sleep(Duration::from_millis(20)).await;
                }
                let pid = spawned_pids(&spawned)[0];
                let deadline = Instant::now() + Duration::from_secs(5);
                while Instant::now() < deadline {
                    let text = std::fs::read_to_string(pid_file(&paths)).unwrap_or_default();
                    let pids: Vec<(u32, PathBuf)> = serde_json::from_str(&text).unwrap_or_default();
                    if pids.iter().any(|(p, _)| *p == pid) {
                        return true;
                    }
                    tokio::time::sleep(Duration::from_millis(50)).await;
                }
                false
            } => Ok(named),
        };
        let left = wait_gone(&spawned_pids(&spawned)).await;
        assert!(
            named.unwrap(),
            "pids.json does not name the loading runtime"
        );
        assert!(left.is_empty(), "still running: {left:?}");
    }

    #[test]
    fn a_random_key_is_long_and_new_each_time() {
        let a = random_key();
        assert_eq!(a.len(), 64);
        assert_ne!(a, random_key());
    }
}

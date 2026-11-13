//! Health and the follow-up of a start: the health of a managed endpoint, the followers that
//! check it and keep `pids.json` current at each phase change of its runtime, and the GPU
//! check after a start on a GPU build (the GPU memory a runtime's processes hold through
//! DRM).

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::{Duration, Instant};

use super::data::{Provider, Service};
use super::hardware;
use super::hardware::Backend;
use super::runtimes::write_pids_with;
use super::supervisor::{self, Phase, ProcessStatus};
use super::{
    GpuCheck, LocalPaths, SelectedModel, SelectedRuntime, Selection, GPU_CHECKS, MANAGED_PORTS,
};
use crate::types::{Endpoint, EndpointHealth, EndpointState};

/// The GPU check of `provider`'s last start: (on the GPU, bytes held), `None`s when none ran.
pub(super) fn gpu_check(provider: Provider) -> GpuCheck {
    GPU_CHECKS
        .lock()
        .unwrap_or_else(|p| p.into_inner())
        .get(&provider)
        .copied()
        .unwrap_or((None, None))
}

/// Check the managed endpoint's health when the follower starts and at every phase change
/// of its runtime, so `ai-endpoint-status` follows a crash and a restart without waiting for
/// the 60 s loop. The first check covers a change between the registration's check and
/// the follower's start. Probes, not debounced checks: a phase change right after another
/// check must not answer the stored health.
pub(super) fn spawn_health_follower(
    endpoint_id: String,
    mut status: tokio::sync::watch::Receiver<ProcessStatus>,
) {
    tokio::spawn(async move {
        let probe = |endpoint_id: &str| {
            let endpoint = crate::db_backend::db_backend().get_endpoint(endpoint_id);
            async move {
                if let Ok(Some(endpoint)) = endpoint {
                    let _ = super::endpoints::probe_health(&endpoint).await;
                }
            }
        };
        let mut last = status.borrow_and_update().phase;
        probe(&endpoint_id).await;
        while status.changed().await.is_ok() {
            let phase = status.borrow_and_update().phase;
            if phase == last {
                continue;
            }
            last = phase;
            probe(&endpoint_id).await;
            if matches!(phase, Phase::Stopped | Phase::Failed) {
                return;
            }
        }
    });
}

/// Write `pids.json` when the runtime of `status` gets a pid: at its spawn, before its
/// models load (an executor that crashes meanwhile leaves no runtime the next start
/// misses), and when a supervised restart gives it a new one.
pub(super) fn spawn_pid_follower(
    paths: LocalPaths,
    mut status: tokio::sync::watch::Receiver<ProcessStatus>,
    program: PathBuf,
) {
    tokio::spawn(async move {
        let mut last = None;
        loop {
            let (phase, pid) = {
                let current = status.borrow_and_update();
                (current.phase, current.pid)
            };
            if matches!(phase, Phase::Stopped | Phase::Failed) {
                return;
            }
            if let Some(pid) = pid.filter(|pid| last != Some(*pid)) {
                last = Some(pid);
                write_pids_with(&paths, Some((pid, program.clone()))).await;
            }
            if status.changed().await.is_err() {
                return;
            }
        }
    });
}

/// The memory one DRM client of an fdinfo holds, with the client's id: its vram, gtt,
/// system and local memory (amdgpu, i915, xe). From kernel 6.13 amdgpu prints
/// `drm-resident-<region>` and then the legacy `drm-memory-<region>` again as its alias, so
/// the legacy keys count only where no `drm-resident-` key is printed (amdgpu up to 6.12),
/// as the wind tunnel's `procs.ts` reads them. `None` for a file that is no DRM client.
fn drm_client_bytes(fdinfo: &str) -> Option<(String, u64)> {
    let id = fdinfo.lines().find_map(|l| {
        l.strip_prefix("drm-client-id:")
            .map(|v| v.trim().to_string())
    })?;
    let sum = |prefix: &str| -> Option<u64> {
        let mut bytes: Option<u64> = None;
        for line in fdinfo.lines() {
            let Some(rest) = line.strip_prefix(prefix) else {
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
            let unit = match parts.next() {
                Some("MiB") => 1024 * 1024,
                Some("KiB") | Some("kB") => 1024,
                _ => 1,
            };
            bytes = Some(bytes.unwrap_or(0) + number * unit);
        }
        bytes
    };
    Some((
        id,
        sum("drm-resident-")
            .or_else(|| sum("drm-memory-"))
            .unwrap_or(0),
    ))
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
pub(super) fn spawn_gpu_check(
    provider: Provider,
    status: tokio::sync::watch::Receiver<ProcessStatus>,
    model_bytes: u64,
) -> tokio::task::JoinHandle<()> {
    tokio::spawn(async move {
        let started = Instant::now();
        let mut held = 0;
        // A runtime that is gone (stopped, or replaced by a new start: its supervisor and
        // the status sender end) gets no result: it would overwrite the next one's.
        let gone =
            |status: &tokio::sync::watch::Receiver<ProcessStatus>| status.has_changed().is_err();
        while started.elapsed() < Duration::from_secs(120) {
            if gone(&status) {
                return;
            }
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
        if gone(&status) {
            return;
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
    })
}

/// Whether a start of `runtime` gets the GPU check, and against how many bytes: a GPU
/// build, Linux, an AMD or Intel GPU; the chat model's file size (else the largest model's).
pub(super) async fn gpu_check_for(runtime: &SelectedRuntime, selection: &Selection) -> Option<u64> {
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
            super::providers::discovery::health_of(
                super::providers::discovery::discover_for(
                    endpoint.kind,
                    endpoint.base_url.clone(),
                    endpoint.api_key.expose(),
                    super::providers::discovery::PROBE_TIMEOUT,
                )
                .list_models()
                .await,
            )
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai_service::local::registration::NEMO_SPEECH_ENDPOINT;
    use crate::ai_service::local::runtimes::{pid_file, write_pids};
    use crate::ai_service::local::supervisor::{
        ChildSpec, Readiness, RestartBudget, StopHandle, Supervised, Timing,
    };
    use crate::ai_service::local::test_support::{status_in, TEST_LOCK};
    use crate::ai_service::local::{ManagedPort, Running, RUNNING};
    use crate::secret::Secret;
    use crate::types::{AiCapability, EndpointKind};
    use url::Url;

    /// The GPU check of a runtime that is gone (stopped, or replaced by a new start) ends
    /// and publishes nothing: it would overwrite the check of the runtime after it.
    #[tokio::test]
    async fn a_gpu_check_of_a_runtime_that_is_gone_publishes_nothing() {
        let _serial = TEST_LOCK.lock().await;
        let (supervisor, status) = tokio::sync::watch::channel(status_in(Phase::Running));
        drop(supervisor);
        GPU_CHECKS
            .lock()
            .unwrap()
            .insert(Provider::NemoSpeech, (None, None));
        tokio::time::timeout(
            Duration::from_secs(5),
            spawn_gpu_check(Provider::NemoSpeech, status, 1),
        )
        .await
        .expect("the check ends with its runtime")
        .unwrap();
        assert_eq!(gpu_check(Provider::NemoSpeech), (None, None));
    }

    /// The health follower checks the endpoint when it starts, not only at a phase change:
    /// a runtime that became ready between the registration's check and the follower's
    /// start would stay down until the 60 s loop. The registration's check just before
    /// does not hold it back.
    #[tokio::test]
    async fn the_health_follower_checks_the_endpoint_when_it_starts() {
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let endpoint = Endpoint {
            managed: true,
            ..Endpoint::for_test(
                "health-follower-test",
                EndpointKind::LlamaServer,
                "http://127.0.0.1:1",
                "",
            )
        };
        crate::db_backend::db_backend()
            .insert_endpoint(&endpoint)
            .unwrap();
        let stored_check = || {
            crate::db_backend::db_backend()
                .get_endpoint(&endpoint.id)
                .unwrap()
                .unwrap()
                .health
                .checked_at
        };
        super::super::endpoints::check_health(&endpoint)
            .await
            .unwrap();
        let registration = stored_check().expect("the registration's check is stored");
        tokio::time::sleep(Duration::from_millis(5)).await;

        let (_supervisor, status) = tokio::sync::watch::channel(status_in(Phase::Running));
        spawn_health_follower(endpoint.id.clone(), status);
        let checked = tokio::time::timeout(Duration::from_secs(5), async {
            while stored_check() == Some(registration) {
                tokio::time::sleep(Duration::from_millis(20)).await;
            }
        })
        .await;
        crate::db_backend::db_backend()
            .remove_endpoint(&endpoint.id)
            .unwrap();
        assert!(checked.is_ok(), "the follower did not check at its start");
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
        // From kernel 6.13 amdgpu prints `drm-resident-*` and then `drm-memory-*` again as
        // an alias: counted once (the wind tunnel's fixture).
        let fdinfo = "drm-driver:\tamdgpu\ndrm-client-id:\t43\ndrm-total-cpu:\t256580 KiB\n\
                      drm-resident-cpu:\t0\ndrm-total-gtt:\t1455376 KiB\ndrm-shared-gtt:\t0\n\
                      drm-resident-gtt:\t1455364 KiB\ndrm-purgeable-gtt:\t0\n\
                      drm-total-vram:\t952 KiB\ndrm-resident-vram:\t952 KiB\n\
                      drm-memory-vram:\t952 KiB\ndrm-memory-gtt: \t1455364 KiB\n\
                      drm-memory-cpu: \t0 KiB\namd-requested-gtt:\t1455376 KiB\n";
        assert_eq!(
            drm_client_bytes(fdinfo),
            Some(("43".to_string(), (1455364 + 952) * 1024))
        );
        // This process is in its own tree.
        assert!(process_tree(std::process::id()).contains(&std::process::id()));
    }

    /// From kernel 6.13 amdgpu prints `drm-resident-<region>` and then the legacy
    /// `drm-memory-<region>` again as its alias: the memory counts once, by the resident keys
    /// (as the wind tunnel's `procs.ts` reads it).
    #[test]
    fn amdgpu_memory_printed_twice_counts_once() {
        let fdinfo = "drm-driver:\tamdgpu\ndrm-client-id:\t7\n\
                      drm-resident-vram:\t1024 KiB\ndrm-resident-gtt:\t2 MiB\n\
                      drm-memory-vram:\t1024 KiB\ndrm-memory-gtt:\t2 MiB\n";
        assert_eq!(
            drm_client_bytes(fdinfo),
            Some(("7".to_string(), 1024 * 1024 + 2 * 1024 * 1024))
        );
        // i915 and xe print only `drm-resident-`.
        let xe = "drm-client-id:\t8\ndrm-resident-system:\t4 MiB\n";
        assert_eq!(
            drm_client_bytes(xe),
            Some(("8".to_string(), 4 * 1024 * 1024))
        );
    }

    /// A running nemo-speech is as healthy as its `/v1/models` listing with the key says: the
    /// served ids are the listed ones, a refused key is `Unauthorized`. One that is starting
    /// says so without a request.
    #[tokio::test]
    async fn a_running_nemo_speech_is_checked_by_its_listing() {
        let _serial = TEST_LOCK.lock().await;
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
                stop: StopHandle::default(),
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

    /// A supervised restart spawns a new pid: `pids.json` names it, so a start after a
    /// crash of the executor finds it.
    #[cfg(unix)]
    #[tokio::test]
    async fn the_pid_file_follows_a_restart() {
        let _serial = TEST_LOCK.lock().await;
        let dir = tempfile::tempdir().unwrap();
        let paths = LocalPaths {
            store: dir.path().join("store"),
            state: dir.path().join("state"),
        };
        std::fs::create_dir_all(&paths.state).unwrap();
        // The fake runtime does not listen itself: the test holds the port, so it answers.
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
        let first = supervised
            .wait_until_settled(Duration::from_secs(10))
            .await
            .pid
            .unwrap();
        let status = supervised.watch();
        RUNNING.lock().await.insert(
            Provider::LlamaServer,
            Running {
                supervised,
                endpoint_id: None,
                key: Secret::new("k"),
            },
        );
        write_pids(&paths).await;
        spawn_pid_follower(paths.clone(), status, PathBuf::from("/bin/sh"));
        // SAFETY: kill(2) on the fake runtime this test started.
        unsafe {
            libc::kill(first as libc::pid_t, libc::SIGKILL);
        }
        let read = || -> Vec<u32> {
            std::fs::read_to_string(pid_file(&paths))
                .ok()
                .and_then(|t| serde_json::from_str::<Vec<(u32, PathBuf)>>(&t).ok())
                .unwrap_or_default()
                .into_iter()
                .map(|(pid, _)| pid)
                .collect()
        };
        let deadline = Instant::now() + Duration::from_secs(10);
        while read().iter().all(|p| *p == first) && Instant::now() < deadline {
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
        let written = read();
        let running = RUNNING.lock().await.remove(&Provider::LlamaServer).unwrap();
        let now = running.supervised.status().pid;
        running.supervised.stop().await;
        drop(listener);

        assert!(now.is_some_and(|p| p != first), "restarted: {now:?}");
        assert_eq!(
            written,
            vec![now.unwrap()],
            "the pid file names the restarted runtime"
        );
    }
}

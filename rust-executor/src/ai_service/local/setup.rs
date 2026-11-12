//! The install and remove flow: `ai.localInstall` (one install at a time, under the install
//! flag), `install_into` for the CLI and the Docker image, the download of the plan's
//! runtimes and models with its progress events, and `ai.localRemove`.

use anyhow::{bail, Result};
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};

use super::data::{self, Service};
use super::hardware::Backend;
use super::install::{self, Store};
use super::registration::unregister;
use super::runtimes::{pid_file, start_at, status, stop_all, stop_running};
use super::{
    plan_for, InstallParams, LocalAiRefused, LocalPaths, LocalStatus, ProgressEvent, RemoveParams,
    SelectedModel, SelectedRuntime, Selection, INSTALLING, LIFECYCLE, SELECTION_FILE,
};
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub;

/// At most this often per item an install publishes its progress.
const PROGRESS_INTERVAL: Duration = Duration::from_millis(250);

async fn publish_progress(event: ProgressEvent) {
    publish_progress_now(event);
}

fn publish_progress_now(event: ProgressEvent) {
    crate::pubsub::get_global_pubsub_sync().publish_event(Event::new(
        Topic::AiLocalProgress,
        Audience::Operator,
        &event,
    ));
}

/// Clears the install flag when the install ends, also on an error or a panic.
pub(super) struct InstallGuard;

impl Drop for InstallGuard {
    fn drop(&mut self) {
        INSTALLING.store(false, Ordering::SeqCst);
    }
}

/// `ai.localInstall`: download what the plan names (resuming, verified), keep the
/// selection, then start and register, unless `start` is false.
pub async fn install(params: InstallParams) -> Result<LocalStatus> {
    let paths = LocalPaths::from_config()?;
    run_install(install_at(&paths, &params)).await
}

/// One install at a time: runs `work` under the install flag, publishes how it ended and
/// answers the status after it.
async fn run_install(work: impl std::future::Future<Output = Result<()>>) -> Result<LocalStatus> {
    if INSTALLING.swap(true, Ordering::SeqCst) {
        return Err(LocalAiRefused("A local AI install is running already".into()).into());
    }
    run_installing(InstallGuard, work).await
}

/// The install flag, once no install runs: waits for one that does.
pub(super) async fn take_install_flag() -> InstallGuard {
    while INSTALLING.swap(true, Ordering::SeqCst) {
        tokio::time::sleep(Duration::from_millis(500)).await;
    }
    InstallGuard
}

/// [`run_install`] with the flag taken.
pub(super) async fn run_installing(
    guard: InstallGuard,
    work: impl std::future::Future<Output = Result<()>>,
) -> Result<LocalStatus> {
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

pub(super) async fn install_at(paths: &LocalPaths, params: &InstallParams) -> Result<()> {
    install_for(
        paths,
        params,
        crate::config::get_global_config().local_ai_device,
    )
    .await
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
    let mut plan = plan_for(paths, device, params.services.as_deref()).await?;
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
    {
        let _file = SELECTION_FILE.lock().unwrap();
        selection.save(paths)?;
    }

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
            // Now, not on a task: a report published after the flush would put an older
            // byte count after the item's last one.
            publish_progress_now(self.event(bytes));
        }
    }

    async fn flush(&self) {
        let bytes = self.state.lock().unwrap_or_else(|p| p.into_inner()).1;
        publish_progress(self.event(bytes.max(self.item_total))).await;
    }
}

/// `ai.localRemove`: stop, unregister the managed endpoints and their models, forget the
/// selection; with `delete_files`, delete the store's runtimes and models too.
pub async fn remove(params: RemoveParams) -> Result<bool> {
    if INSTALLING.load(Ordering::SeqCst) {
        return Err(LocalAiRefused("A local AI install is running".into()).into());
    }
    // Held to the end: a start meanwhile would register what this removes.
    let _lifecycle = LIFECYCLE.lock().await;
    stop_running().await;
    let paths = LocalPaths::from_config()?;
    unregister(|_| true).await?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai_service::local::test_support::TEST_LOCK;
    use anyhow::anyhow;

    /// A download's progress reaches subscribers in the order it was reported: a report
    /// published later, on a spawned task, would put an older byte count after the flush.
    #[tokio::test]
    async fn download_progress_is_published_in_order() {
        let mut events = get_global_pubsub().await.subscribe_events();
        let reporter = Reporter::new("progress-order-test".into(), 100, 0, 100);
        reporter.report(10);
        reporter.flush().await;
        let mut seen = Vec::new();
        while let Ok(event) = events.try_recv() {
            if event.topic != Topic::AiLocalProgress {
                continue;
            }
            let step: ProgressEvent = serde_json::from_value(event.payload.clone()).unwrap();
            if step.item.as_deref() == Some("progress-order-test") {
                seen.push(step.item_done_bytes);
            }
        }
        assert_eq!(seen, vec![10, 100]);
    }

    /// `ai.localInstall`'s answer is the status after the install: it says no install runs
    /// (the launcher shows "installing" until it does). While one runs, another is refused;
    /// a failed one clears the flag too.
    #[tokio::test]
    async fn a_finished_install_answers_that_no_install_runs() {
        let _serial = TEST_LOCK.lock().await;
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

    /// A first use while an install runs waits for it and then takes the flag itself: an
    /// install that begins in between makes it wait again, not fail.
    #[tokio::test]
    async fn a_first_use_waits_for_a_running_install_and_then_takes_the_flag() {
        let _serial = TEST_LOCK.lock().await;
        INSTALLING.store(true, Ordering::SeqCst);
        let waiting = tokio::spawn(take_install_flag());
        tokio::time::sleep(Duration::from_millis(700)).await;
        let waited = !waiting.is_finished();
        INSTALLING.store(false, Ordering::SeqCst);
        let guard = tokio::time::timeout(Duration::from_secs(5), waiting).await;
        let taken = INSTALLING.load(Ordering::SeqCst);
        drop(guard);
        let released = !INSTALLING.load(Ordering::SeqCst);

        assert!(waited, "it waits while the install runs");
        assert!(taken, "then it holds the flag");
        assert!(released, "and gives it back when done");
    }
}

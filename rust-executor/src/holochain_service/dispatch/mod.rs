//! The Holochain service's dispatch loop.
//!
//! `HolochainService::init` runs one `run_dispatch_loop` per conductor on the service's own
//! tokio runtime. The loop takes `Envelope`s off the channel every
//! `HolochainServiceInterface` method writes to and runs them against a `RequestDispatch`:
//! `ConductorDispatch` in production (the per-variant `match` that used to live inline in
//! `init`), a mock in `tests.rs`.
//!
//! Extracted from `init` for #1133 so the loop's concurrency contract can be unit-tested
//! without a conductor, and so #970 item 10's macro can generate on top of this shape.

use async_trait::async_trait;
use deno_core::anyhow::anyhow;
use holochain::conductor::api::AppInfo;
use log::{debug, error, warn};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::mpsc::error::SendError;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use tokio::sync::{OwnedRwLockReadGuard, RwLock, Semaphore};
use tokio::task::{JoinError, JoinHandle, JoinSet};
use tokio::time::timeout;

use super::interface::{Envelope, HolochainServiceRequest, HolochainServiceResponse};
use super::HolochainService;

/// Upper bound on zome calls and the other conductor/network requests running at once. 32
/// is a guess sized for a desktop node with a few dozen joined perspectives: high enough
/// that a burst of `sync()` zome calls across spaces never queues a presence broadcast, low
/// enough that a runaway caller cannot flood the conductor. Not a config knob until a
/// measurement says the guess is wrong.
pub(crate) const ZOME_CALL_CONCURRENCY: usize = 32;

/// Upper bound on `HolochainServiceRequest::is_local` requests (keystore, app info) running
/// at once. They have their own permits so a full zome call pool never delays them; the
/// keystore answers one at a time anyway, so more would only queue inside it.
pub(crate) const LOCAL_CONCURRENCY: usize = 4;

/// Runs one request to completion and answers it on the request's oneshot.
///
/// Every implementation must answer every request it is handed (`HolochainServiceRequest::refuse`
/// exists for the ones it will not run); a dropped oneshot surfaces to the caller as a
/// channel error with no reason attached.
#[async_trait]
pub(crate) trait RequestDispatch: Send + Sync + 'static {
    async fn handle(&self, request: HolochainServiceRequest);
}

/// Takes requests off `receiver` and runs them against `dispatcher` until `Shutdown` is
/// dispatched or every sender is gone.
///
/// Two classes of request (#1133):
///
/// - **Lifecycle** (`HolochainServiceRequest::is_lifecycle`: install, remove, enable,
///   shutdown) runs inline, holding the `lifecycle` write lock. tokio's `RwLock` is
///   write-preferring, so a lifecycle request first waits for every request the loop handed
///   to a lane before it, queued or running, to finish, and the loop reads nothing new until
///   it is done. `Shutdown` instead closes both lanes, waits for them to drain, is
///   dispatched, and ends the loop; only the lanes spawn, so after the drain nothing is
///   running.
/// - **Everything else** is handed, with a `lifecycle` read guard, to one of two lanes
///   (`run_lane`): the **local** lane (`HolochainServiceRequest::is_local`: keystore and app
///   info, `LOCAL_CONCURRENCY` permits) or the **conductor** lane (zome calls, agent infos,
///   metrics, pack/unpack, `ZOME_CALL_CONCURRENCY` permits). A lane starts its requests in
///   the order they arrived, each once it has a permit, and spawns it holding the permit and
///   the read guard. The loop itself never waits for a permit, so it always reads the next
///   request: a `Sign` reaches the local lane even while every conductor permit is held and
///   more zome calls are queued behind them. Those queued calls stay entries in the
///   conductor lane's channel, not tasks, exactly as they used to stay in this loop's.
///
/// Once a request has what it waited for (the drain, the write lock, or a permit), `admit`
/// logs its time in queue (`debug`; `warn` above 1 s) and, if its `deadline` has passed,
/// answers it with an error so it never reaches the dispatcher.
pub(crate) async fn run_dispatch_loop<D: RequestDispatch>(
    mut receiver: UnboundedReceiver<Envelope>,
    dispatcher: Arc<D>,
) {
    let lifecycle = Arc::new(RwLock::new(()));
    let (conductor, conductor_lane) = spawn_lane(ZOME_CALL_CONCURRENCY, dispatcher.clone());
    let (local, local_lane) = spawn_lane(LOCAL_CONCURRENCY, dispatcher.clone());

    while let Some(envelope) = receiver.recv().await {
        if matches!(envelope.request, HolochainServiceRequest::Shutdown(_)) {
            drop((conductor, local));
            for lane in [conductor_lane, local_lane] {
                log_task_failure(lane.await);
            }
            let Envelope {
                request,
                queued_at,
                deadline,
            } = envelope;
            if let Some(request) = admit(request, queued_at, deadline, 0) {
                dispatcher.handle(request).await;
            }
            break;
        }

        if envelope.request.is_lifecycle() {
            let _exclusive = lifecycle.write().await;
            let Envelope {
                request,
                queued_at,
                deadline,
            } = envelope;
            if let Some(request) = admit(request, queued_at, deadline, 0) {
                dispatcher.handle(request).await;
            }
            continue;
        }

        // Never waits: the loop is the only writer, and it holds the write lock only inline
        // above.
        let shared = lifecycle.clone().read_owned().await;
        let lane = if envelope.request.is_local() {
            &local
        } else {
            &conductor
        };
        // A lane ends only after its sender is dropped at `Shutdown`, or if it panicked.
        if let Err(SendError((envelope, _))) = lane.send((envelope, shared)) {
            let name = envelope.request.name();
            envelope
                .request
                .refuse(anyhow!("{name}: the Holochain dispatch lane is gone"));
        }
    }
    error!("Holochain service receiver closed");
}

/// A request handed to a lane, with the `lifecycle` read guard the loop took for it.
type Admitted = (Envelope, OwnedRwLockReadGuard<()>);

fn spawn_lane<D: RequestDispatch>(
    permits: usize,
    dispatcher: Arc<D>,
) -> (UnboundedSender<Admitted>, JoinHandle<()>) {
    let (sender, receiver) = unbounded_channel();
    (
        sender,
        tokio::spawn(run_lane(receiver, permits, dispatcher)),
    )
}

/// One lane of `run_dispatch_loop`: runs at most `capacity` requests at once, starting them
/// in the order they arrived. Waiting for a permit here, not in the loop, is what keeps one
/// full lane from holding up the other. Ends once the loop drops its sender and every
/// request it spawned has finished.
async fn run_lane<D: RequestDispatch>(
    mut receiver: UnboundedReceiver<Admitted>,
    capacity: usize,
    dispatcher: Arc<D>,
) {
    let permits = Arc::new(Semaphore::new(capacity));
    let mut in_flight: JoinSet<()> = JoinSet::new();

    while let Some((envelope, shared)) = receiver.recv().await {
        reap_finished(&mut in_flight);

        let permit = permits
            .clone()
            .acquire_owned()
            .await
            .expect("the semaphore is never closed");
        // Not counting the permit just taken for this request.
        let others = capacity - permits.available_permits() - 1;
        let Envelope {
            request,
            queued_at,
            deadline,
        } = envelope;
        let Some(request) = admit(request, queued_at, deadline, others) else {
            continue;
        };
        let dispatcher = dispatcher.clone();
        in_flight.spawn(async move {
            dispatcher.handle(request).await;
            drop(shared);
            drop(permit);
        });
    }

    while let Some(finished) = in_flight.join_next().await {
        log_task_failure(finished);
    }
}

/// Called once the request is about to run (after the drain, the write lock or the permit it
/// waited for): logs the time in queue and refuses the request if its deadline has passed.
/// Checking here and not on dequeue means a request that waited for a permit behind
/// `ZOME_CALL_CONCURRENCY` slow calls is caught too. `in_flight` is the number of other
/// requests running in the same lane.
fn admit(
    request: HolochainServiceRequest,
    queued_at: Instant,
    deadline: Option<Instant>,
    in_flight: usize,
) -> Option<HolochainServiceRequest> {
    let name = request.name();
    let waited = queued_at.elapsed();
    log_queue_wait(name, waited, in_flight);
    if deadline.is_some_and(|deadline| Instant::now() > deadline) {
        request.refuse(anyhow!(
            "{name} expired after {} ms in queue",
            waited.as_millis()
        ));
        return None;
    }
    Some(request)
}

/// Drops finished tasks from the set so it does not grow with every request served.
fn reap_finished(in_flight: &mut JoinSet<()>) {
    while let Some(finished) = in_flight.try_join_next() {
        log_task_failure(finished);
    }
}

fn log_task_failure(finished: Result<(), JoinError>) {
    if let Err(e) = finished {
        // The request's oneshot was dropped with the task, so its caller already sees a
        // channel error; this is the only place the reason is visible.
        error!("❌ 🐝 [hc-actor] request task did not complete: {e}");
    }
}

fn log_queue_wait(name: &str, waited: Duration, in_flight: usize) {
    let ms = waited.as_millis();
    if waited > Duration::from_secs(1) {
        warn!("⚠️ 🐝 [hc-actor] {name} waited {ms} ms in queue, {in_flight} in flight");
    } else {
        debug!("🐝 [hc-actor] {name} waited {ms} ms in queue, {in_flight} in flight");
    }
}

/// The production `RequestDispatch`: each request variant, its timeout, and the call into
/// `HolochainService` or the conductor.
pub(crate) struct ConductorDispatch {
    pub service: HolochainService,
    /// Each newly installed app is announced here so `init`'s signal fan-in subscribes to
    /// its signals.
    pub new_app_ids: UnboundedSender<AppInfo>,
}

#[async_trait]
impl RequestDispatch for ConductorDispatch {
    async fn handle(&self, message: HolochainServiceRequest) {
        let service = &self.service;
        let new_app_ids_sender = &self.new_app_ids;
        match message {
            HolochainServiceRequest::InstallApp(payload, response) => {
                match timeout(
                    std::time::Duration::from_secs(10),
                    service.install_app(payload),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; InstallApp call"))
                {
                    Ok(result) => {
                        if let Ok(app_info) = &result {
                            let _ = new_app_ids_sender.send(app_info.clone());
                        }
                        let _ = response.send(HolochainServiceResponse::InstallApp(result));
                    }
                    Err(err) => {
                        let _ = response.send(HolochainServiceResponse::InstallApp(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::CallZomeFunction {
                app_id,
                cell_name,
                zome_name,
                fn_name,
                payload,
                response,
            } => {
                match timeout(
                    std::time::Duration::from_secs(90),
                    service.call_zome_function(app_id, cell_name, zome_name, fn_name, payload),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; Call Zome Function"))
                {
                    Ok(result) => {
                        let _ = response.send(HolochainServiceResponse::CallZomeFunction(result));
                    }
                    Err(err) => {
                        let _ = response.send(HolochainServiceResponse::CallZomeFunction(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::RemoveApp(app_id, response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(10),
                    service.remove_app(app_id),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; Remove App"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::RemoveApp(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::RemoveApp(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::EnableApp(app_id, response_tx) => {
                match timeout(std::time::Duration::from_secs(10), async {
                    service
                        .conductor
                        .clone()
                        .enable_app(app_id)
                        .await
                        .map(|_| ())
                        .map_err(|e| anyhow!("Could not enable app: {:?}", e))
                })
                .await
                .map_err(|_| anyhow!("Timeout error; Enable App"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::EnableApp(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::EnableApp(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::AgentInfos(response_tx) => {
                match timeout(std::time::Duration::from_secs(30), service.agent_infos())
                    .await
                    .map_err(|_| anyhow!("Timeout error; AgentInfos"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::AgentInfos(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::AgentInfos(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::AddAgentInfos(agent_infos, response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(30),
                    service.add_agent_infos(agent_infos),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; AddAgentInfos"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::AddAgentInfos(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::AddAgentInfos(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::Sign(data, response_tx) => {
                match timeout(std::time::Duration::from_secs(3), service.sign(data))
                    .await
                    .map_err(|_| anyhow!("Timeout error; Sign"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::Sign(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::Sign(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::Shutdown(response_tx) => {
                match timeout(std::time::Duration::from_secs(3), service.shutdown())
                    .await
                    .map_err(|_| anyhow!("Timeout error Shutdown"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::Shutdown(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::Shutdown(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::GetAgentKey(response_tx) => {
                match timeout(std::time::Duration::from_secs(3), service.get_agent_key())
                    .await
                    .map_err(|_| anyhow!("Timeout error; GetAgentKey"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::GetAgentKey(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::GetAgentKey(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::GetAppInfo(app_id, response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(3),
                    service.get_app_info(app_id),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; GetAppInfo"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::GetAppInfo(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::GetAppInfo(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::LogNetworkMetrics(response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(3),
                    service.log_network_metrics(),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; LogNetworkMetrics"))
                {
                    Ok(result) => {
                        let _ =
                            response_tx.send(HolochainServiceResponse::LogNetworkMetrics(result));
                    }
                    Err(err) => {
                        let _ =
                            response_tx.send(HolochainServiceResponse::LogNetworkMetrics(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::GetNetworkMetrics(response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(30),
                    service.get_network_metrics(),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; GetNetworkMetrics"))
                {
                    Ok(result) => {
                        let _ =
                            response_tx.send(HolochainServiceResponse::GetNetworkMetrics(result));
                    }
                    Err(err) => {
                        // KEEP AT warn (Nico + CodeRabbit, PR #942 round 2):
                        // this path is reachable from `runtime.networkMetrics`
                        // via WS-RPC, so a real user request just failed. Do not
                        // downgrade in future cleanups; if there's a caller-
                        // specific periodic path that wants debug, gate the
                        // downgrade behind that path only.
                        // See rust-executor/LOGGING.md.
                        log::warn!("⚠️ 🐝 GetNetworkMetrics timed out after 30s");
                        let _ =
                            response_tx.send(HolochainServiceResponse::GetNetworkMetrics(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::PackDna(path, response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(3),
                    HolochainService::pack_dna(path),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; PackDna"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::PackDna(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::PackDna(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::UnPackDna(path, response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(3),
                    HolochainService::unpack_dna(path),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; UnpackDna"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::UnPackDna(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::UnPackDna(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::PackHapp(path, response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(3),
                    HolochainService::pack_happ(path),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; PackHapp"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::PackHapp(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::PackHapp(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::UnPackHapp(path, response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(3),
                    HolochainService::unpack_happ(path),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; UnPackHapp"))
                {
                    Ok(result) => {
                        let _ = response_tx.send(HolochainServiceResponse::UnPackHapp(result));
                    }
                    Err(err) => {
                        let _ = response_tx.send(HolochainServiceResponse::UnPackHapp(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::NewSignKeypair(response_tx) => {
                match timeout(
                    std::time::Duration::from_secs(10),
                    service.conductor.keystore().new_sign_keypair_random(),
                )
                .await
                .map_err(|_| anyhow!("Timeout error; NewSignKeypair"))
                {
                    Ok(result) => {
                        let result = result
                            .map_err(|e| anyhow!("Failed to generate new signing keypair: {}", e));
                        let _ = response_tx.send(HolochainServiceResponse::NewSignKeypair(result));
                    }
                    Err(err) => {
                        let _ =
                            response_tx.send(HolochainServiceResponse::NewSignKeypair(Err(err)));
                    }
                }
            }
            HolochainServiceRequest::SignWithKey(agent_key, data, response_tx) => {
                let keystore = service.conductor.keystore();
                let data_arc = Arc::from(data.into_boxed_slice());
                let result = keystore
                    .sign(agent_key, data_arc)
                    .await
                    .map_err(|e| anyhow!("Failed to sign with key: {}", e));
                let _ = response_tx.send(HolochainServiceResponse::SignWithKey(result));
            }
        };
    }
}

#[cfg(test)]
mod tests;

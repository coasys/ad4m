//! The Holochain service's dispatch loop.
//!
//! `HolochainService::init` runs one `run_dispatch_loop` per conductor on the service's own
//! tokio runtime. The loop takes `Envelope`s off the channel every
//! `HolochainServiceInterface` method writes to and runs them against a `ZomeDispatch`:
//! `ConductorDispatch` in production (the per-variant `match` that used to live inline in
//! `init`), a mock in `tests.rs`.
//!
//! Extracted from `init` for #1133 so the loop's concurrency contract can be unit-tested
//! without a conductor, and so #970 item 10's macro can generate on top of this shape.

use async_trait::async_trait;
use deno_core::anyhow::anyhow;
use holochain::conductor::api::AppInfo;
use log::error;
use std::sync::Arc;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::time::timeout;

use super::interface::{Envelope, HolochainServiceRequest, HolochainServiceResponse};
use super::HolochainService;

/// Upper bound on non-lifecycle requests (zome calls, signing, agent infos, …) running at
/// once. 32 is a guess sized for a desktop node with a few dozen joined perspectives: high
/// enough that a burst of `sync()` zome calls across spaces never queues a presence
/// broadcast, low enough that a runaway caller cannot flood the conductor. Not a config
/// knob until a measurement says the guess is wrong.
pub(crate) const ZOME_CALL_CONCURRENCY: usize = 32;

/// Runs one request to completion and answers it on the request's oneshot.
///
/// Every implementation must answer every request it is handed (`HolochainServiceRequest::refuse`
/// exists for the ones it will not run); a dropped oneshot surfaces to the caller as a
/// channel error with no reason attached.
#[async_trait]
pub(crate) trait ZomeDispatch: Send + Sync + 'static {
    async fn handle(&self, request: HolochainServiceRequest);
}

/// Takes requests off `receiver` and runs them against `dispatcher` until `Shutdown` is
/// dispatched or every sender is gone.
pub(crate) async fn run_dispatch_loop<D: ZomeDispatch>(
    mut receiver: UnboundedReceiver<Envelope>,
    dispatcher: Arc<D>,
) {
    while let Some(envelope) = receiver.recv().await {
        let request = envelope.request;
        let is_shutdown = matches!(request, HolochainServiceRequest::Shutdown(_));
        dispatcher.handle(request).await;
        if is_shutdown {
            break;
        }
    }
    error!("Holochain service receiver closed");
}

/// The production `ZomeDispatch`: each request variant, its timeout, and the call into
/// `HolochainService` or the conductor.
pub(crate) struct ConductorDispatch {
    pub service: HolochainService,
    /// Each newly installed app is announced here so `init`'s signal fan-in subscribes to
    /// its signals.
    pub new_app_ids: UnboundedSender<AppInfo>,
}

#[async_trait]
impl ZomeDispatch for ConductorDispatch {
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

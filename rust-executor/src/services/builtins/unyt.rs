//! Unyt: `unyt.wallet`, the executor's HoT wallet on the Unyt alliance DNA.

use async_trait::async_trait;
use base64::Engine;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::{internal, params, require_admin, to_value, NoParams};
use crate::api::types::UnytVersionInfo;
use crate::services::builtin::{
    CallContext, ServiceError, ServiceHealth, ServiceImplementation, StartContext,
};
use crate::services::interface::{Risk, Selection};
use crate::services::schema_export::{InterfaceBuilder, MethodOptions};

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct MembraneProofParams {
    /// Base64-encoded membrane proof from the hosting joining service.
    pub proof: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct HistoryParams {
    /// Page boundary from a previous page; omit for the newest page.
    #[serde(default)]
    pub page: Option<u64>,
    /// Defaults to 50.
    #[serde(default)]
    pub per_page: Option<u64>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SendParams {
    /// The recipient's Unyt agent key.
    pub recipient: String,
    pub amount: String,
    #[serde(default)]
    pub note: Option<String>,
}

#[derive(Serialize, Deserialize, JsonSchema)]
pub struct Outcome {
    pub success: bool,
    /// The proposal hash after a send; the reason after a failure.
    pub message: String,
}

pub fn wallet_interface() -> Value {
    InterfaceBuilder::new(
        "unyt.wallet",
        super::AUTHOR,
        "1.0.0",
        Selection::Executor,
        "The executor's HoT wallet on the Unyt alliance DNA.",
    )
    .action("READ", "See the HoT wallet", "Read the wallet's balance, history, keys and DNA version.", Risk::Safe)
    .action("SEND", "Send HoT", "Send HoT from the executor's wallet.", Risk::Spend)
    .action("ADMIN", "Administer the Unyt DNA", "Install, reinstall and authorise the Unyt DNA.", Risk::Admin)
    .method::<NoParams, UnytVersionInfo>("versionInfo", "READ", "Installed and bundled DNA versions, and why the last install failed.", MethodOptions { read: true, ..Default::default() })
    .method::<MembraneProofParams, bool>(
        "setMembraneProof",
        "ADMIN",
        "Store the membrane proof, then install the DNA in the background; `versionInfo` shows the outcome. Needs the admin credential.",
        MethodOptions { errors: vec![("InvalidProof", 422)], ..Default::default() },
    )
    .method::<NoParams, String>("agentKey", "READ", "The executor's Unyt agent key, created on first use.", MethodOptions { long: true, ..Default::default() })
    .method::<NoParams, String>("hotAgentPubkey", "READ", "The agent key the alliance cell runs as.", MethodOptions { long: true, ..Default::default() })
    .method::<NoParams, Value>("balance", "READ", "The ledger (`get_ledger` of the transactor zome).", MethodOptions { long: true, ..Default::default() })
    .method::<HistoryParams, Value>("history", "READ", "One page of transactions (`get_history`).", MethodOptions { long: true, ..Default::default() })
    .method::<SendParams, Outcome>("sendHot", "SEND", "Propose a payment to `recipient`; it settles when they commit.", MethodOptions { long: true, ..Default::default() })
    .method::<NoParams, Outcome>("reinstallDna", "ADMIN", "Uninstall and install the DNA again. Needs the admin credential.", MethodOptions { long: true, ..Default::default() })
    .build()
}

#[derive(Default)]
pub struct Unyt {
    started: tokio::sync::OnceCell<()>,
}

/// Unyt's background work, when the executor runs Holochain: install the
/// alliance DNA (if a membrane proof is stored), settle payments every 30 s,
/// and handle the alliance cell's signals.
fn start_background() {
    tokio::spawn(async {
        if crate::unyt_service::get_membrane_proof().is_none() {
            log::info!("No Unyt membrane proof stored — skipping eager DNA install");
            return;
        }
        match crate::unyt_service::ensure_installed().await {
            Ok(()) => log::info!("Unyt alliance DNA ready"),
            Err(e) => log::error!("Failed to install Unyt alliance DNA: {}", e),
        }
    });
    tokio::spawn(async {
        loop {
            tokio::time::sleep(std::time::Duration::from_secs(30)).await;
            crate::unyt_service::check_pending_payments().await;
            crate::unyt_service::check_pending_sends().await;
        }
    });
    let mut signals = crate::services::host().watch(
        super::event_type(super::Builtin::HolochainConductor, "signal"),
        None,
    );
    tokio::spawn(async move {
        while let Some(signal) = signals.recv().await {
            let Some(cell_id) = signal.get("cellId").and_then(Value::as_str) else {
                continue;
            };
            if crate::unyt_service::is_alliance_cell(cell_id).await {
                let payload = signal.get("payload").cloned().unwrap_or(Value::Null);
                tokio::spawn(async move { crate::unyt_service::handle_signal(&payload).await });
            }
        }
    });
}

#[async_trait]
impl ServiceImplementation for Unyt {
    async fn start(&self, _ctx: StartContext) -> Result<(), String> {
        let holochain = crate::config::try_get_global_config()
            .and_then(|c| c.run_holochain)
            .unwrap_or(true);
        if holochain && self.started.set(()).is_ok() {
            start_background();
        }
        Ok(())
    }

    async fn stop(&self) -> Result<(), String> {
        Ok(())
    }

    async fn health(&self) -> ServiceHealth {
        ServiceHealth::Running
    }

    async fn call(&self, method: &str, p: Value, ctx: CallContext) -> Result<Value, ServiceError> {
        match method {
            "versionInfo" => {
                let (installed, bundled) = crate::unyt_service::version_info();
                to_value(UnytVersionInfo {
                    installed,
                    bundled,
                    install_error: crate::unyt_service::install_error(),
                })
            }
            "setMembraneProof" => {
                require_admin(&ctx)?;
                let p: MembraneProofParams = params(p)?;
                if p.proof.is_empty() {
                    return Err(ServiceError::method(
                        "InvalidProof",
                        "'proof' must not be empty",
                    ));
                }
                // The install decodes it later and, if that fails, installs without a proof.
                if let Err(e) = base64::engine::general_purpose::STANDARD.decode(&p.proof) {
                    return Err(ServiceError::method(
                        "InvalidProof",
                        format!("'proof' is not valid base64: {}", e),
                    ));
                }
                crate::unyt_service::set_membrane_proof(&p.proof).map_err(internal)?;
                tokio::spawn(async {
                    match crate::unyt_service::ensure_installed().await {
                        Ok(()) => {
                            log::info!("Unyt alliance DNA installed after membrane proof was set")
                        }
                        Err(e) => log::error!("Failed to install Unyt alliance DNA: {}", e),
                    }
                });
                to_value(true)
            }
            "agentKey" => to_value(
                crate::unyt_service::get_or_create_agent_key()
                    .await
                    .map_err(unavailable)?,
            ),
            "hotAgentPubkey" => to_value(crate::unyt_service::whoami().await.map_err(unavailable)?),
            "balance" => crate::unyt_service::get_ledger().await.map_err(unavailable),
            "history" => {
                let p: HistoryParams = params(p)?;
                crate::unyt_service::get_history(p.page, p.per_page.unwrap_or(50))
                    .await
                    .map_err(unavailable)
            }
            "sendHot" => {
                let p: SendParams = params(p)?;
                to_value(
                    match crate::unyt_service::send_hot(&p.recipient, &p.amount, p.note.as_deref())
                        .await
                    {
                        Ok(proposal) => Outcome {
                            success: true,
                            message: proposal,
                        },
                        Err(e) => Outcome {
                            success: false,
                            message: e.to_string(),
                        },
                    },
                )
            }
            "reinstallDna" => {
                require_admin(&ctx)?;
                to_value(match crate::unyt_service::reinstall().await {
                    Ok(()) => Outcome {
                        success: true,
                        message: "Unyt DNA reinstalled".into(),
                    },
                    Err(e) => Outcome {
                        success: false,
                        message: e.to_string(),
                    },
                })
            }
            other => Err(internal(format!("unyt has no method {}", other))),
        }
    }
}

/// Zome calls fail while the DNA is not installed or the conductor is down.
fn unavailable(e: impl std::fmt::Display) -> ServiceError {
    ServiceError::Unavailable(e.to_string())
}

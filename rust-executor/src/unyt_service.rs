//! Unyt/mHOT currency integration service.
//!
//! Manages the alliance DNA on the Holochain conductor, providing payment
//! request/acceptance flows and balance queries for the hosting credit system.

use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

use deno_core::error::AnyError;
use holochain::prelude::{ExternIO, ZomeCallResponse};
use log::{error, info, warn};
use base64::Engine;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use tokio::sync::RwLock;

use crate::db::Ad4mDb;
use crate::holochain_service::holochain_service_extension::msgpack_value_to_json;
use crate::holochain_service::interface::{get_holochain_service, maybe_get_holochain_service};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

pub const UNYT_APP_ID: &str = "unyt-mhot";
const UNYT_CELL_NAME: &str = "alliance";
const UNYT_ZOME: &str = "transactor";
const ALLIANCE_DNA_VERSION: &str = "0.61.0";

/// Embedded alliance DNA bytes.
const ALLIANCE_DNA_BYTES: &[u8] = include_bytes!("resources/alliance_0.61.0.dna");

// ---------------------------------------------------------------------------
// Global state: the DNA hash of the installed alliance cell, used for signal routing
// ---------------------------------------------------------------------------

lazy_static! {
    static ref ALLIANCE_DNA_HASH: Arc<RwLock<Option<String>>> = Arc::new(RwLock::new(None));
    static ref INSTALL_ONCE: Arc<tokio::sync::Mutex<bool>> = Arc::new(tokio::sync::Mutex::new(false));
}

/// Check if a cell_id_key (hex dna_hash:hex agent_key) belongs to the alliance DNA.
pub async fn is_alliance_cell(cell_id_key: &str) -> bool {
    let lock = ALLIANCE_DNA_HASH.read().await;
    if let Some(ref dna_hash_hex) = *lock {
        cell_id_key.starts_with(dna_hash_hex.as_str())
    } else {
        false
    }
}

// ---------------------------------------------------------------------------
// Serde types matching the Unyt zome interface
// ---------------------------------------------------------------------------

/// Input for `create_proposal` zome call.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProposalInput {
    /// Amount map: unit_symbol -> amount_string, e.g. {"HOT": "100"}
    pub amount: BTreeMap<String, String>,
    /// Counterparty agent public keys (base64)
    pub counterparty: Vec<String>,
    /// Optional note
    pub note: Option<ProposalPayload>,
    /// Lane definitions (action hashes) — can be empty for simple transfers
    pub lane_definitions: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "type", content = "value")]
pub enum ProposalPayload {
    SimpleNote(String),
    None,
}

/// Input for `create_commitment` zome call.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CommitmentInput {
    pub counterparty: Vec<String>,
    pub amount: BTreeMap<String, String>,
    pub note: Option<ProposalPayload>,
    pub lane_definitions: Vec<String>,
}

/// Input for `create_accept` zome call.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct AcceptInput {
    pub commitment: String,
    pub note: Option<ProposalPayload>,
}

/// Pagination for `get_history`.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Pagination {
    pub high_boundary: Option<u64>,
    pub per_page: u64,
}

/// Result of a payment request operation.
#[derive(Debug, Clone)]
pub struct PaymentResult {
    pub success: bool,
    pub proposal_hash: Option<String>,
    pub message: String,
}

// ---------------------------------------------------------------------------
// DNA installation
// ---------------------------------------------------------------------------

/// Ensure the alliance DNA is installed. Safe to call multiple times —
/// only the first call actually installs; subsequent calls are no-ops.
/// Blocks until holochain service is available.
/// Returns an error if no membrane proof has been stored yet.
pub async fn ensure_installed() -> Result<(), AnyError> {
    // Fast path: already installed
    {
        let installed = INSTALL_ONCE.lock().await;
        if *installed {
            return Ok(());
        }
    }

    // Don't attempt install without a membrane proof
    if get_membrane_proof().is_none() {
        return Err(deno_core::anyhow::anyhow!(
            "No membrane proof stored — call setUnytMembraneProof first"
        ));
    }

    // Wait for holochain service to become available (no timeout — agent unlock may take a while)
    while maybe_get_holochain_service().await.is_none() {
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
    }

    // Resolve data path from Ad4mConfig
    let data_path = {
        let config = crate::config::get_global_config();
        std::path::PathBuf::from(
            config
                .app_data_path
                .as_ref()
                .ok_or_else(|| deno_core::anyhow::anyhow!("App data path not configured"))?,
        )
    };

    // Retry installation — the holochain service has a 10s timeout per install call,
    // but DNA installation with network join can take longer.
    let mut last_err = None;
    for attempt in 1..=5 {
        match install_alliance_dna(&data_path).await {
            Ok(()) => {
                let mut installed = INSTALL_ONCE.lock().await;
                *installed = true;
                return Ok(());
            }
            Err(e) => {
                warn!(
                    "Unyt DNA install attempt {}/5 failed: {}. Retrying in 5s...",
                    attempt, e
                );
                last_err = Some(e);
                tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
            }
        }
    }
    Err(last_err.unwrap_or_else(|| deno_core::anyhow::anyhow!("Install failed after 5 attempts")))
}

/// Install the alliance DNA on the Holochain conductor.
///
/// Writes the embedded DNA to a temp directory, creates a happ manifest,
/// packs it, and installs via `HolochainService::install_app()`.
pub async fn install_alliance_dna(data_path: &Path) -> Result<(), AnyError> {
    let hc = match maybe_get_holochain_service().await {
        Some(hc) => hc,
        None => return Err(deno_core::anyhow::anyhow!("Holochain service not available")),
    };

    // Check if already installed with correct version
    if let Ok(Some(_)) = hc.get_app_info(UNYT_APP_ID.to_string()).await {
        let installed_version = Ad4mDb::with_global_instance(|db| {
            db.get_setting("unyt_dna_version")
        }).unwrap_or(None);

        if installed_version.as_deref() == Some(ALLIANCE_DNA_VERSION) {
            info!("Unyt alliance DNA v{} already installed", ALLIANCE_DNA_VERSION);
            capture_dna_hash().await;
            return Ok(());
        }
        // Version mismatch — will be handled by explicit reinstall
        info!(
            "Unyt alliance DNA installed but version mismatch (installed={}, bundled={})",
            installed_version.as_deref().unwrap_or("unknown"),
            ALLIANCE_DNA_VERSION
        );
        capture_dna_hash().await;
        return Ok(());
    }

    do_install(data_path, &hc).await
}

/// Actually perform the DNA installation (shared by install and reinstall).
async fn do_install(
    data_path: &Path,
    hc: &crate::holochain_service::interface::HolochainServiceInterface,
) -> Result<(), AnyError> {
    info!("Installing Unyt alliance DNA v{}...", ALLIANCE_DNA_VERSION);

    // Write DNA to data directory
    let unyt_dir = data_path.join("unyt");
    std::fs::create_dir_all(&unyt_dir)?;

    let dna_path = unyt_dir.join("alliance.dna");
    std::fs::write(&dna_path, ALLIANCE_DNA_BYTES)?;

    // Create happ manifest directory
    let happ_dir = unyt_dir.join("happ");
    std::fs::create_dir_all(&happ_dir)?;

    // Write happ.yaml manifest with network seed and properties for the test network
    let happ_yaml = format!(
        r#"---
manifest_version: "0"
name: unyt-mhot
roles:
  - name: alliance
    provisioning:
      strategy: create
      deferred: false
    dna:
      path: {}
      modifiers:
        network_seed: "Ga-FM2jL7uq3NDI9QX1Zl"
        properties:
          progenitor_pubkey: "uhCAkdeTV-5BNlhK4pC9tVpVwlUhzcOA8zqn3lEhtkN41qWGo0PWr"
          joining_server_signer: "uhCAk_Jbtn_3RR-VCLPtJdhcQvVrpM7Vw5vHGog8_CwW5tO0_Cf37"
"#,
        dna_path.to_string_lossy()
    );
    let happ_yaml_path = happ_dir.join("happ.yaml");
    std::fs::write(&happ_yaml_path, &happ_yaml)?;

    // Pack happ
    let happ_file = hc
        .pack_happ(happ_dir.to_string_lossy().to_string())
        .await?;
    info!("Packed alliance hApp at: {}", happ_file);

    // Install
    use holochain::prelude::{InstallAppPayload, MembraneProof, SerializedBytes};
    use holochain_types::app::{AppBundleSource, RoleSettings};
    use std::collections::HashMap;
    use std::path::PathBuf;

    // Build roles_settings with membrane proof if available
    let roles_settings = match get_membrane_proof() {
        Some(proof_b64) => {
            match base64::engine::general_purpose::STANDARD.decode(&proof_b64) {
                Ok(proof_bytes) => {
                    info!("Using stored membrane proof ({} bytes) for DNA installation", proof_bytes.len());
                    let membrane_proof = MembraneProof::from(SerializedBytes::from(
                        holochain::prelude::UnsafeBytes::from(proof_bytes),
                    ));
                    let mut settings = HashMap::new();
                    settings.insert(
                        UNYT_CELL_NAME.into(),
                        RoleSettings::Provisioned {
                            membrane_proof: Some(membrane_proof),
                            modifiers: None,
                        },
                    );
                    Some(settings)
                }
                Err(e) => {
                    warn!("Failed to decode membrane proof base64: {}. Installing without proof.", e);
                    None
                }
            }
        }
        None => {
            warn!("No membrane proof stored — DNA installation may fail if membrane proof is required");
            None
        }
    };

    // Use the pre-generated agent key if stored (in Holochain "uhCAk..." format)
    let agent_key = match Ad4mDb::with_global_instance(|db| {
        db.get_setting("unyt_agent_key")
    }).unwrap_or(None) {
        Some(key_str) => {
            match holochain::prelude::AgentPubKey::try_from(key_str.as_str()) {
                Ok(key) => {
                    info!("Using pre-generated Unyt agent key: {}", key_str);
                    Some(key)
                }
                Err(e) => {
                    warn!("Failed to parse stored Unyt agent key '{}': {}. Will generate new.", key_str, e);
                    None
                }
            }
        }
        None => {
            info!("No pre-generated Unyt agent key — Holochain will create one");
            None
        }
    };

    let payload = InstallAppPayload {
        source: AppBundleSource::Path(PathBuf::from(&happ_file)),
        agent_key,
        installed_app_id: Some(UNYT_APP_ID.to_string()),
        network_seed: None,
        roles_settings,
        ignore_genesis_failure: false,
    };

    match hc.install_app(payload).await {
        Ok(app_info) => {
            info!(
                "Unyt alliance DNA installed successfully: {:?}",
                app_info.installed_app_id
            );
        }
        Err(e) => {
            error!("Failed to install Unyt alliance DNA: {}", e);
            return Err(e);
        }
    }

    // Store installed version
    if let Err(e) = Ad4mDb::with_global_instance(|db| {
        db.set_setting("unyt_dna_version", ALLIANCE_DNA_VERSION)
    }) {
        warn!("Failed to store Unyt DNA version in DB: {}", e);
    }

    // Capture DNA hash for signal routing
    capture_dna_hash().await;

    Ok(())
}

/// Reinstall the alliance DNA (uninstall old, install new).
pub async fn reinstall() -> Result<(), AnyError> {
    let hc = match maybe_get_holochain_service().await {
        Some(hc) => hc,
        None => return Err(deno_core::anyhow::anyhow!("Holochain service not available")),
    };

    // Uninstall existing
    info!("Uninstalling old Unyt alliance DNA...");
    if let Err(e) = hc.remove_app(UNYT_APP_ID.to_string()).await {
        warn!("Failed to uninstall old Unyt DNA (may not exist): {}", e);
    }

    // Reset install flag
    {
        let mut installed = INSTALL_ONCE.lock().await;
        *installed = false;
    }

    let data_path = {
        let config = crate::config::get_global_config();
        std::path::PathBuf::from(
            config
                .app_data_path
                .as_ref()
                .ok_or_else(|| deno_core::anyhow::anyhow!("App data path not configured"))?,
        )
    };

    do_install(&data_path, &hc).await?;

    let mut installed = INSTALL_ONCE.lock().await;
    *installed = true;
    Ok(())
}

/// Get installed vs bundled version info.
pub fn version_info() -> (Option<String>, String) {
    let installed = Ad4mDb::with_global_instance(|db| {
        db.get_setting("unyt_dna_version")
    }).unwrap_or(None);
    (installed, ALLIANCE_DNA_VERSION.to_string())
}

// ---------------------------------------------------------------------------
// Membrane proof management
// ---------------------------------------------------------------------------

/// Store a membrane proof (base64-encoded bytes) for use during DNA installation.
/// This should be called before `ensure_installed()` with auth material obtained
/// from the hosting API / joining server.
pub fn set_membrane_proof(proof_base64: &str) -> Result<(), AnyError> {
    Ad4mDb::with_global_instance(|db| {
        db.set_setting("unyt_membrane_proof", proof_base64)
    })?;
    info!("Stored Unyt membrane proof ({} bytes encoded)", proof_base64.len());
    Ok(())
}

/// Retrieve the stored membrane proof, if any.
pub fn get_membrane_proof() -> Option<String> {
    Ad4mDb::with_global_instance(|db| {
        db.get_setting("unyt_membrane_proof")
    }).unwrap_or(None)
}

/// Pre-generate a Holochain agent key for the Unyt DNA and store it.
/// Returns the agent pubkey in Holochain's native string format (e.g. "uhCAk...").
/// If a key was already generated, returns the stored one.
pub async fn get_or_create_agent_key() -> Result<String, AnyError> {
    // Check if we already have one stored (and it's valid)
    if let Some(existing) = Ad4mDb::with_global_instance(|db| {
        db.get_setting("unyt_agent_key")
    }).unwrap_or(None) {
        // Validate it's in the correct format
        if holochain::prelude::AgentPubKey::try_from(existing.as_str()).is_ok() {
            return Ok(existing);
        }
        warn!("Stored Unyt agent key is in invalid format, regenerating...");
    }

    let hc = match maybe_get_holochain_service().await {
        Some(hc) => hc,
        None => return Err(deno_core::anyhow::anyhow!("Holochain service not available")),
    };

    let agent_key = hc.new_sign_keypair_random().await?;
    // Use Holochain's native Display format: "u" + base64url_no_pad(39 bytes)
    let key_str = agent_key.to_string();

    Ad4mDb::with_global_instance(|db| {
        db.set_setting("unyt_agent_key", &key_str)
    })?;

    info!("Generated and stored Unyt agent key: {}", key_str);
    Ok(key_str)
}

/// Capture the DNA hash from the installed app for signal routing.
async fn capture_dna_hash() {
    let hc = match maybe_get_holochain_service().await {
        Some(hc) => hc,
        None => return,
    };

    if let Ok(Some(app_info)) = hc.get_app_info(UNYT_APP_ID.to_string()).await {
        for (_role, cells) in &app_info.cell_info {
            for cell_info in cells {
                if let holochain::conductor::api::CellInfo::Provisioned(cell) = cell_info {
                    let dna_hash_hex = cell
                        .cell_id
                        .dna_hash()
                        .get_raw_39()
                        .iter()
                        .map(|b| format!("{:02x}", b))
                        .collect::<String>();
                    let mut lock = ALLIANCE_DNA_HASH.write().await;
                    *lock = Some(dna_hash_hex.clone());
                    info!("Captured alliance DNA hash for signal routing: {}", dna_hash_hex);
                    return;
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Zome call helpers
// ---------------------------------------------------------------------------

/// Call a zome function on the alliance DNA and return decoded JSON.
/// Automatically ensures the DNA is installed first.
async fn call_zome(fn_name: &str, payload: Option<ExternIO>) -> Result<JsonValue, AnyError> {
    ensure_installed().await?;
    let hc = get_holochain_service().await;

    let response = hc
        .call_zome_function(
            UNYT_APP_ID.to_string(),
            UNYT_CELL_NAME.to_string(),
            UNYT_ZOME.to_string(),
            fn_name.to_string(),
            payload,
        )
        .await?;

    match response {
        ZomeCallResponse::Ok(extern_io) => {
            let bytes = extern_io.as_bytes().to_vec();
            let mut cursor = std::io::Cursor::new(&bytes);
            match rmpv::decode::read_value(&mut cursor) {
                Ok(msgpack_val) => Ok(msgpack_value_to_json(msgpack_val)),
                Err(e) => Err(deno_core::anyhow::anyhow!(
                    "Failed to decode zome response: {}",
                    e
                )),
            }
        }
        ZomeCallResponse::Unauthorized(_, _, _, _) => Err(deno_core::anyhow::anyhow!(
            "Unauthorized zome call: {}",
            fn_name
        )),
        ZomeCallResponse::NetworkError(msg) => Err(deno_core::anyhow::anyhow!(
            "Network error in zome call {}: {}",
            fn_name,
            msg
        )),
        ZomeCallResponse::CountersigningSession(msg) => Err(deno_core::anyhow::anyhow!(
            "Countersigning error in zome call {}: {}",
            fn_name,
            msg
        )),
        ZomeCallResponse::AuthenticationFailed(_, _) => Err(deno_core::anyhow::anyhow!(
            "Authentication failed for zome call: {}",
            fn_name
        )),
    }
}

fn encode_payload<T: Serialize + std::fmt::Debug>(val: &T) -> Result<ExternIO, AnyError> {
    ExternIO::encode(val).map_err(|e| deno_core::anyhow::anyhow!("Failed to encode payload: {}", e))
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Convert a `{"__binary": [byte, ...]}` JSON value to a base64 HoloHash string (e.g. "uhCAk...").
fn binary_json_to_holohash(val: &JsonValue) -> Option<String> {
    let bytes = val
        .as_object()
        .and_then(|obj| obj.get("__binary"))
        .and_then(|arr| arr.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_u64().map(|n| n as u8))
                .collect::<Vec<u8>>()
        })?;
    if bytes.is_empty() {
        return None;
    }
    Some(base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(&bytes))
}

/// Get the host's agent public key on the mHOT DHT.
pub async fn whoami() -> Result<String, AnyError> {
    let result = call_zome("whoami", None).await?;
    // Result comes back as {"__binary": [132, 32, 36, ...]} — convert to base64 HoloHash
    if let Some(hash) = binary_json_to_holohash(&result) {
        return Ok(hash);
    }
    match result {
        JsonValue::String(s) => Ok(s),
        other => Ok(serde_json::to_string(&other)?),
    }
}

/// Get the current ledger (balance).
pub async fn get_ledger() -> Result<JsonValue, AnyError> {
    call_zome("get_ledger", None).await
}

/// Create a payment proposal targeting a user's mHOT agent key.
/// Returns the proposal action hash.
pub async fn create_proposal(
    amount_hot: &str,
    counterparty_agent_key: &str,
    note: Option<&str>,
) -> Result<String, AnyError> {
    let mut amount = BTreeMap::new();
    amount.insert("HOT".to_string(), amount_hot.to_string());

    let input = ProposalInput {
        amount,
        counterparty: vec![counterparty_agent_key.to_string()],
        note: note.map(|n| ProposalPayload::SimpleNote(n.to_string())),
        lane_definitions: vec![],
    };

    let result = call_zome("create_proposal", Some(encode_payload(&input)?)).await?;

    match result {
        JsonValue::String(hash) => Ok(hash),
        other => Ok(serde_json::to_string(&other)?),
    }
}

/// Get transaction status by action hash.
pub async fn get_status(action_hash: &str) -> Result<JsonValue, AnyError> {
    call_zome("get_status", Some(encode_payload(&action_hash)?)).await
}

/// Get a specific transaction by action hash.
pub async fn get_transaction(action_hash: &str) -> Result<JsonValue, AnyError> {
    call_zome("get_transaction", Some(encode_payload(&action_hash)?)).await
}

/// Get transaction history.
pub async fn get_history(page: Option<u64>, per_page: u64) -> Result<JsonValue, AnyError> {
    let pagination = Pagination {
        high_boundary: page,
        per_page,
    };
    call_zome("get_history", Some(encode_payload(&pagination)?)).await
}

/// Get all notification links (incoming transaction notifications).
pub async fn get_all_notification_links() -> Result<JsonValue, AnyError> {
    call_zome("get_all_notification_links", None).await
}

/// Get actionable transactions from notification links.
pub async fn get_actionable_transactions(links: JsonValue) -> Result<JsonValue, AnyError> {
    call_zome(
        "get_actionable_transactions",
        Some(encode_payload(&links)?),
    )
    .await
}

/// Send mHOT to an external address (host withdrawal).
/// Creates a commitment (direct transfer) to the recipient.
pub async fn send_hot(
    recipient_agent_key: &str,
    amount_hot: &str,
    note: Option<&str>,
) -> Result<String, AnyError> {
    let mut amount = BTreeMap::new();
    amount.insert("HOT".to_string(), amount_hot.to_string());

    let input = CommitmentInput {
        counterparty: vec![recipient_agent_key.to_string()],
        amount,
        note: note.map(|n| ProposalPayload::SimpleNote(n.to_string())),
        lane_definitions: vec![],
    };

    let result = call_zome("create_commitment", Some(encode_payload(&input)?)).await?;

    match result {
        JsonValue::String(hash) => Ok(hash),
        other => Ok(serde_json::to_string(&other)?),
    }
}

// ---------------------------------------------------------------------------
// Signal handling
// ---------------------------------------------------------------------------

/// Handle an incoming signal from the alliance DNA.
/// If it's a completed payment (Accept/Receipt), credit the user's account.
pub async fn handle_signal(payload_json: &JsonValue) {
    // The signal payload is SignalPayload::Tx(Transaction)
    // We need to check if it's an Accept or Receipt where we are the payee
    let _tx_type = payload_json
        .get("tx_type")
        .or_else(|| payload_json.get("Tx").and_then(|tx| tx.get("tx_type")));

    // Try to extract transaction from either direct or wrapped format
    let tx = if payload_json.get("Tx").is_some() {
        payload_json.get("Tx")
    } else {
        Some(payload_json)
    };

    let tx = match tx {
        Some(tx) => tx,
        None => {
            warn!("Unyt signal: could not extract transaction from payload");
            return;
        }
    };

    let tx_type_str = tx
        .get("tx_type")
        .and_then(|v| v.as_str())
        .unwrap_or("");

    // We're interested in Accept and Receipt types — these indicate completed payments
    if tx_type_str != "Accept" && tx_type_str != "Receipt" {
        info!("Unyt signal: ignoring tx_type={}", tx_type_str);
        return;
    }

    info!("Unyt signal: received {} transaction", tx_type_str);

    // Extract amount from the transaction
    let amount = tx.get("amount");
    let hot_amount = amount
        .and_then(|a| a.get("HOT"))
        .and_then(|v| v.as_str())
        .or_else(|| {
            // Try other unit names
            amount.and_then(|a| {
                a.as_object().and_then(|obj| {
                    obj.values().next().and_then(|v| v.as_str())
                })
            })
        });

    let hot_amount = match hot_amount {
        Some(a) => a,
        None => {
            warn!("Unyt signal: could not extract HOT amount from transaction");
            return;
        }
    };

    // Extract counterparty (the payer)
    let counterparty = tx
        .get("counterparty")
        .and_then(|c| c.as_array())
        .and_then(|arr| arr.first())
        .and_then(|v| v.as_str());

    let counterparty = match counterparty {
        Some(c) => c,
        None => {
            warn!("Unyt signal: could not extract counterparty from transaction");
            return;
        }
    };

    // Look up user by their mHOT wallet address (= agent pubkey)
    let user_email = Ad4mDb::with_global_instance(|db| {
        db.get_user_by_hot_wallet_address(counterparty)
    });

    match user_email {
        Ok(Some(email)) => {
            // Parse amount and credit user
            match hot_amount.parse::<f64>() {
                Ok(amount_f64) => {
                    if let Err(e) = Ad4mDb::with_global_instance(|db| {
                        db.add_user_credits(&email, amount_f64)
                    }) {
                        error!("Failed to credit user {} with {} HOT: {}", email, amount_f64, e);
                    } else {
                        info!(
                            "Credited user {} with {} HOT from mHOT payment",
                            email, amount_f64
                        );
                    }

                    // Update payment request status if we have one
                    let tx_id = tx.get("id").and_then(|v| v.as_str());
                    if let Some(id) = tx_id {
                        let _ = Ad4mDb::with_global_instance(|db| {
                            db.complete_payment_request(id)
                        });
                    }
                }
                Err(e) => {
                    error!("Failed to parse HOT amount '{}': {}", hot_amount, e);
                }
            }
        }
        Ok(None) => {
            info!(
                "Unyt signal: received payment from unknown wallet {}",
                counterparty
            );
        }
        Err(e) => {
            error!("Failed to look up user by wallet address: {}", e);
        }
    }
}

// ---------------------------------------------------------------------------
// Background payment polling
// ---------------------------------------------------------------------------

/// Check pending payment requests and credit users for completed ones.
/// Called periodically as a fallback for missed signals.
pub async fn check_pending_payments() {
    let pending = match Ad4mDb::with_global_instance(|db| db.get_pending_payment_requests()) {
        Ok(requests) => requests,
        Err(e) => {
            warn!("Failed to get pending payment requests: {}", e);
            return;
        }
    };

    if pending.is_empty() {
        return;
    }

    for request in pending {
        if let Some(ref action_hash) = request.proposal_action_hash {
            match get_status(action_hash).await {
                Ok(status) => {
                    // Check if the transaction has been completed
                    let status_str = status
                        .as_str()
                        .unwrap_or_else(|| {
                            status.get("status").and_then(|s| s.as_str()).unwrap_or("")
                        });

                    let is_completed = status_str == "Completed"
                        || status_str == "completed"
                        || status.get("Completed").is_some();

                    if is_completed {
                        info!(
                            "Payment request {} completed for user {}",
                            action_hash, request.user_email
                        );

                        // Credit the user
                        match request.amount_hot.parse::<f64>() {
                            Ok(amount) => {
                                if let Err(e) = Ad4mDb::with_global_instance(|db| {
                                    db.add_user_credits(&request.user_email, amount)
                                }) {
                                    error!("Failed to credit user {}: {}", request.user_email, e);
                                }
                            }
                            Err(e) => {
                                error!(
                                    "Failed to parse amount '{}': {}",
                                    request.amount_hot, e
                                );
                            }
                        }

                        // Mark as completed
                        let _ = Ad4mDb::with_global_instance(|db| {
                            db.complete_payment_request(action_hash)
                        });
                    }
                }
                Err(e) => {
                    warn!(
                        "Failed to check status of payment request {}: {}",
                        action_hash, e
                    );
                }
            }
        }
    }
}

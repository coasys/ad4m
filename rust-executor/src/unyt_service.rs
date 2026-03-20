//! Unyt/mHOT currency integration service.
//!
//! Manages the alliance DNA on the Holochain conductor, providing payment
//! request/acceptance flows and balance queries for the hosting credit system.

use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

use base64::Engine;
use deno_core::error::AnyError;
use holochain::prelude::{ExternIO, ZomeCallResponse};
use log::{error, info, warn};
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
const ALLIANCE_DNA_VERSION: &str = "0.62.0";

/// Embedded alliance DNA bytes (downloaded by build.rs).
const ALLIANCE_DNA_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/alliance.dna"));

// ---------------------------------------------------------------------------
// Global state: the DNA hash of the installed alliance cell, used for signal routing
// ---------------------------------------------------------------------------

lazy_static! {
    static ref ALLIANCE_DNA_HASH: Arc<RwLock<Option<String>>> = Arc::new(RwLock::new(None));
    static ref INSTALL_ONCE: Arc<tokio::sync::Mutex<bool>> =
        Arc::new(tokio::sync::Mutex::new(false));
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
/// Must match the Unyt alliance integrity zome's `ProposalInput` exactly.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProposalInput {
    /// Counterparty agent public key (AgentPubKeyB64)
    pub counterparty: String,
    /// Amount map: unit_symbol -> amount_string, e.g. {"HOT": "100"}
    pub amount: BTreeMap<String, String>,
    /// Optional note (plain string)
    pub note: Option<String>,
    /// Lane definitions (action hashes) — can be empty for simple transfers
    #[serde(default)]
    pub lane_definitions: Vec<serde_json::Value>,
}

/// Input for `create_commitment` zome call.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CommitmentInput {
    pub counterparty: String,
    pub amount: BTreeMap<String, String>,
    pub note: Option<String>,
    #[serde(default)]
    pub lane_definitions: Vec<serde_json::Value>,
}

/// Input for `create_accept` zome call.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct AcceptInput {
    pub commitment: String,
    pub note: Option<String>,
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
    // Acquire the lock for the entire install sequence to prevent races.
    let mut installed = INSTALL_ONCE.lock().await;
    if *installed {
        return Ok(());
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
                *installed = true;

                // After install: obtain hc-auth bootstrap credentials and
                // restart the conductor with the space override so the unyt
                // DNA can reach the network.
                setup_bootstrap_auth().await;

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
        None => {
            return Err(deno_core::anyhow::anyhow!(
                "Holochain service not available"
            ))
        }
    };

    // Check if already installed with correct version
    if let Ok(Some(_)) = hc.get_app_info(UNYT_APP_ID.to_string()).await {
        let installed_version =
            Ad4mDb::with_global_instance(|db| db.get_setting("unyt_dna_version")).unwrap_or(None);

        if installed_version.as_deref() == Some(ALLIANCE_DNA_VERSION) {
            info!(
                "Unyt alliance DNA v{} already installed",
                ALLIANCE_DNA_VERSION
            );
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

    // App not found via get_app_info. Check if we previously installed it
    // (DB has version marker) — if so, the app may be in a disabled/broken state.
    // Try to enable it rather than reinstalling, to preserve the source chain
    // (which holds transaction history and balances).
    let previously_installed =
        Ad4mDb::with_global_instance(|db| db.get_setting("unyt_dna_version"))
            .unwrap_or(None)
            .is_some();

    if previously_installed {
        info!("Unyt DNA was previously installed but not found by conductor — attempting to enable existing app");
        match hc.enable_app(UNYT_APP_ID.to_string()).await {
            Ok(_) => {
                info!("Successfully re-enabled Unyt alliance DNA");
                capture_dna_hash().await;
                return Ok(());
            }
            Err(e) => {
                warn!(
                    "Failed to enable existing Unyt app: {}. Will attempt fresh install.",
                    e
                );
                // Fall through to do_install as last resort — this may fail with
                // GenesisFailed if the source chain exists, but we don't want to
                // remove_app and lose the user's transaction history.
            }
        }
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
        network_seed: "7RysrsJstXhPQcYb088dR",
        properties:
          progenitor_pubkey: "uhCAkzTjgUwMRiiKO5Gu8XHISzDKckzZ0b7QiUcf5rxg2LsVMNbQP"
          joining_server_signer: "uhCAk_Jbtn_3RR-VCLPtJdhcQvVrpM7Vw5vHGog8_CwW5tO0_Cf37"
"#,
        dna_path.to_string_lossy()
    );
    let happ_yaml_path = happ_dir.join("happ.yaml");
    std::fs::write(&happ_yaml_path, &happ_yaml)?;

    // Pack happ
    let happ_file = hc.pack_happ(happ_dir.to_string_lossy().to_string()).await?;
    info!("Packed alliance hApp at: {}", happ_file);

    // Install
    use holochain::prelude::{InstallAppPayload, MembraneProof, SerializedBytes};
    use holochain_types::app::{AppBundleSource, RoleSettings};
    use std::collections::HashMap;
    use std::path::PathBuf;

    // Build roles_settings with membrane proof if available
    let roles_settings = match get_membrane_proof() {
        Some(proof_b64) => match base64::engine::general_purpose::STANDARD.decode(&proof_b64) {
            Ok(proof_bytes) => {
                info!(
                    "Using stored membrane proof ({} bytes) for DNA installation",
                    proof_bytes.len()
                );
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
                warn!(
                    "Failed to decode membrane proof base64: {}. Installing without proof.",
                    e
                );
                None
            }
        },
        None => {
            warn!("No membrane proof stored — DNA installation may fail if membrane proof is required");
            None
        }
    };

    // Use the pre-generated agent key if stored (in Holochain "uhCAk..." format)
    let agent_key =
        match Ad4mDb::with_global_instance(|db| db.get_setting("unyt_agent_key")).unwrap_or(None) {
            Some(key_str) => match holochain::prelude::AgentPubKey::try_from(key_str.as_str()) {
                Ok(key) => {
                    info!("Using pre-generated Unyt agent key: {}", key_str);
                    Some(key)
                }
                Err(e) => {
                    warn!(
                        "Failed to parse stored Unyt agent key '{}': {}. Will generate new.",
                        key_str, e
                    );
                    None
                }
            },
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
            // Store the actual installed agent key so hc-auth uses the correct key
            let installed_key_str = app_info.agent_pub_key.to_string();
            if let Err(e) = Ad4mDb::with_global_instance(|db| {
                db.set_setting("unyt_agent_key", &installed_key_str)
            }) {
                warn!("Failed to store installed agent key: {}", e);
            } else {
                info!("Stored installed Unyt agent key: {}", installed_key_str);
            }
        }
        Err(e) => {
            error!("Failed to install Unyt alliance DNA: {}", e);
            return Err(e);
        }
    }

    // Store installed version
    if let Err(e) =
        Ad4mDb::with_global_instance(|db| db.set_setting("unyt_dna_version", ALLIANCE_DNA_VERSION))
    {
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
        None => {
            return Err(deno_core::anyhow::anyhow!(
                "Holochain service not available"
            ))
        }
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

    // After reinstall: set up bootstrap auth (same as fresh install)
    setup_bootstrap_auth().await;

    let mut installed = INSTALL_ONCE.lock().await;
    *installed = true;
    Ok(())
}

/// Get installed vs bundled version info.
pub fn version_info() -> (Option<String>, String) {
    let installed =
        Ad4mDb::with_global_instance(|db| db.get_setting("unyt_dna_version")).unwrap_or(None);
    (installed, ALLIANCE_DNA_VERSION.to_string())
}

// ---------------------------------------------------------------------------
// Membrane proof management
// ---------------------------------------------------------------------------

/// Store a membrane proof (base64-encoded bytes) for use during DNA installation.
/// This should be called before `ensure_installed()` with auth material obtained
/// from the hosting API / joining server.
pub fn set_membrane_proof(proof_base64: &str) -> Result<(), AnyError> {
    Ad4mDb::with_global_instance(|db| db.set_setting("unyt_membrane_proof", proof_base64))?;
    info!(
        "Stored Unyt membrane proof ({} bytes encoded)",
        proof_base64.len()
    );
    Ok(())
}

/// Retrieve the stored membrane proof, if any.
pub fn get_membrane_proof() -> Option<String> {
    Ad4mDb::with_global_instance(|db| db.get_setting("unyt_membrane_proof")).unwrap_or(None)
}

/// Pre-generate a Holochain agent key for the Unyt DNA and store it.
/// Returns the agent pubkey in Holochain's native string format (e.g. "uhCAk...").
/// If a key was already generated, returns the stored one.
pub async fn get_or_create_agent_key() -> Result<String, AnyError> {
    // Check if we already have one stored (and it's valid)
    if let Some(existing) =
        Ad4mDb::with_global_instance(|db| db.get_setting("unyt_agent_key")).unwrap_or(None)
    {
        // Validate it's in the correct format
        if holochain::prelude::AgentPubKey::try_from(existing.as_str()).is_ok() {
            return Ok(existing);
        }
        warn!("Stored Unyt agent key is in invalid format, regenerating...");
    }

    let hc = match maybe_get_holochain_service().await {
        Some(hc) => hc,
        None => {
            return Err(deno_core::anyhow::anyhow!(
                "Holochain service not available"
            ))
        }
    };

    let agent_key = hc.new_sign_keypair_random().await?;
    // Use Holochain's native Display format: "u" + base64url_no_pad(39 bytes)
    let key_str = agent_key.to_string();

    Ad4mDb::with_global_instance(|db| db.set_setting("unyt_agent_key", &key_str))?;

    info!("Generated and stored Unyt agent key: {}", key_str);
    Ok(key_str)
}

// ---------------------------------------------------------------------------
// HC Auth — bootstrap authentication for the unyt network
// ---------------------------------------------------------------------------

const HC_AUTH_URL: &str = "https://hc-auth-iroh-unyt.holochain.org";
pub const UNYT_BOOTSTRAP_URL: &str = "https://hc-auth-iroh-unyt.holochain.org";
pub const UNYT_SIGNAL_URL: &str = "wss://hc-auth-iroh-unyt.holochain.org";
pub const UNYT_RELAY_URL: &str = "https://hc-auth-iroh-unyt.holochain.org";

/// Fetch a challenge from the hc-auth `/now` endpoint, sign it with the unyt
/// agent key, and return the base64-encoded auth material JSON for the
/// bootstrap server.
///
/// The resulting string is suitable for `base64_auth_material` in the
/// conductor config's space override.
pub async fn get_or_create_auth_material() -> Result<String, AnyError> {
    if let Some(existing) =
        Ad4mDb::with_global_instance(|db| db.get_setting("unyt_auth_material")).unwrap_or(None)
    {
        return Ok(existing);
    }

    let auth_material = create_auth_material().await?;

    Ad4mDb::with_global_instance(|db| db.set_setting("unyt_auth_material", &auth_material))?;
    info!("Stored unyt bootstrap auth material");

    Ok(auth_material)
}

/// Create fresh auth material by calling the hc-auth `/now` endpoint, signing
/// the challenge, and packing everything into base64-encoded JSON.
async fn create_auth_material() -> Result<String, AnyError> {
    use base64::engine::general_purpose::{STANDARD, URL_SAFE_NO_PAD};

    let agent_key_str = get_or_create_agent_key().await?;
    let agent_key = holochain::prelude::AgentPubKey::try_from(agent_key_str.as_str())
        .map_err(|e| deno_core::anyhow::anyhow!("Invalid agent key: {}", e))?;

    // HoloHash layout: bytes 0-2 = type prefix, 3-34 = 32-byte Ed25519 pubkey, 35-38 = location
    let raw_39 = agent_key.get_raw_39();
    assert!(
        raw_39.len() == 39,
        "Expected 39-byte HoloHash, got {} bytes",
        raw_39.len()
    );
    let raw_pubkey: Vec<u8> = raw_39[3..35].to_vec();

    // GET /now — returns base64url-encoded 32-byte challenge
    let now_url = format!("{}/now", HC_AUTH_URL);
    let client = reqwest::Client::builder()
        .timeout(std::time::Duration::from_secs(10))
        .build()
        .unwrap_or_else(|_| reqwest::Client::new());
    let payload_b64url = client
        .get(&now_url)
        .send()
        .await
        .map_err(|e| deno_core::anyhow::anyhow!("Failed to GET {}: {}", now_url, e))?
        .text()
        .await
        .map_err(|e| deno_core::anyhow::anyhow!("Failed to read /now response: {}", e))?;

    let payload_b64url = payload_b64url.trim().to_string();
    info!("Got hc-auth /now challenge: {}", payload_b64url);

    // Decode the base64url payload to get the 32 raw bytes to sign
    let payload_bytes = URL_SAFE_NO_PAD
        .decode(&payload_b64url)
        .map_err(|e| deno_core::anyhow::anyhow!("Failed to decode /now payload: {}", e))?;

    if payload_bytes.len() != 32 {
        return Err(deno_core::anyhow::anyhow!(
            "Expected 32-byte /now payload, got {} bytes",
            payload_bytes.len()
        ));
    }

    // Sign the raw 32-byte payload with the unyt agent key
    let hc = get_holochain_service().await;
    let signature = hc
        .sign_with_key(agent_key, payload_bytes)
        .await
        .map_err(|e| deno_core::anyhow::anyhow!("Failed to sign /now payload: {}", e))?;

    // Build the JSON structure with all fields as base64url-no-pad
    let pubkey_b64url = URL_SAFE_NO_PAD.encode(&raw_pubkey);
    let sig_b64url = URL_SAFE_NO_PAD.encode(signature.0.as_ref());

    let auth_json = serde_json::json!({
        "pubKey": pubkey_b64url,
        "payload": payload_b64url,
        "signature": sig_b64url,
    });

    let auth_json_str = serde_json::to_string(&auth_json)?;
    let auth_material = STANDARD.encode(auth_json_str.as_bytes());

    info!("Created hc-auth bootstrap auth material");
    Ok(auth_material)
}

/// Obtain hc-auth bootstrap credentials and restart the conductor so the
/// unyt DNA space has its own bootstrap/signal with auth material.
///
/// This is a best-effort operation — if any step fails the DNA is still
/// installed and will simply not have network access until the next restart
/// (at which point the auth material will be retried).
async fn setup_bootstrap_auth() {
    // Skip if auth material is already stored (previous run already set it up)
    let has_auth = Ad4mDb::with_global_instance(|db| db.get_setting("unyt_auth_material"))
        .unwrap_or(None)
        .is_some();
    let has_dna_hash = Ad4mDb::with_global_instance(|db| db.get_setting("unyt_dna_hash"))
        .unwrap_or(None)
        .is_some();

    if has_auth && has_dna_hash {
        info!("Bootstrap auth material and DNA hash already exist, skipping setup");
        return;
    }

    info!("Setting up hc-auth bootstrap credentials for unyt DNA...");

    if !has_auth {
        // Retry auth material creation up to 3 times (hc-auth server may be briefly unavailable)
        let mut auth_ok = false;
        for attempt in 1..=3 {
            match create_auth_material().await {
                Ok(auth_material) => {
                    if let Err(e) = Ad4mDb::with_global_instance(|db| {
                        db.set_setting("unyt_auth_material", &auth_material)
                    }) {
                        error!("Failed to store auth material: {}", e);
                        return;
                    }
                    info!("Stored unyt bootstrap auth material");
                    auth_ok = true;
                    break;
                }
                Err(e) => {
                    warn!(
                        "Failed to create hc-auth material (attempt {}/3): {}",
                        attempt, e
                    );
                    if attempt < 3 {
                        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;
                    }
                }
            }
        }
        if !auth_ok {
            error!("Failed to create hc-auth material after 3 attempts. The unyt DNA will not have network access until this is resolved.");
            return;
        }
    }

    if !has_dna_hash {
        // Capture the DNA hash now (it should be available since we just installed)
        capture_dna_hash().await;
        // Verify it was stored
        if Ad4mDb::with_global_instance(|db| db.get_setting("unyt_dna_hash"))
            .unwrap_or(None)
            .is_none()
        {
            error!("Failed to capture DNA hash after install — cannot set up space override");
            return;
        }
    }

    // Restart the conductor so the space override takes effect
    info!("Restarting Holochain conductor to apply unyt space override...");
    match crate::holochain_service::HolochainService::restart_service().await {
        Ok(()) => {
            info!("Holochain conductor restarted with unyt space override");
            // Wait for holochain service to come back up
            let mut waited = 0;
            while maybe_get_holochain_service().await.is_none() {
                tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
                waited += 2;
                if waited > 60 {
                    error!("Holochain service did not come back up within 60s after restart");
                    return;
                }
            }
            info!("Holochain service is back up after restart");
        }
        Err(e) => error!("Failed to restart conductor after auth setup: {}", e),
    }
}

/// Get the base64-encoded DNA hash string for the installed alliance cell.
/// Returns the DnaHash in Holochain's native string representation (e.g. "uhC0k..."),
/// which is the key format used for `space_overrides` in the conductor config.
pub async fn get_alliance_dna_hash_b64() -> Option<String> {
    let hc = maybe_get_holochain_service().await?;
    let app_info = hc.get_app_info(UNYT_APP_ID.to_string()).await.ok()??;
    for (_role, cells) in &app_info.cell_info {
        for cell_info in cells {
            if let holochain::conductor::api::CellInfo::Provisioned(cell) = cell_info {
                return Some(cell.cell_id.dna_hash().to_string());
            }
        }
    }
    None
}

/// Capture the DNA hash from the installed app for signal routing and persist
/// the base64 DNA hash to the DB for conductor space-override configuration.
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
                    info!(
                        "Captured alliance DNA hash for signal routing: {}",
                        dna_hash_hex
                    );

                    // Persist the DNA hash in Holochain string format for the
                    // conductor config space_overrides key.
                    let dna_hash_str = cell.cell_id.dna_hash().to_string();
                    if let Err(e) = Ad4mDb::with_global_instance(|db| {
                        db.set_setting("unyt_dna_hash", &dna_hash_str)
                    }) {
                        warn!("Failed to persist unyt DNA hash: {}", e);
                    } else {
                        info!(
                            "Persisted unyt DNA hash for space override: {}",
                            dna_hash_str
                        );
                    }

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
    let encoded = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(&bytes);
    Some(format!("u{}", encoded))
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
    // "0" is the base unit index in the UnitMap (not the symbol "HOT")
    amount.insert("0".to_string(), amount_hot.to_string());

    let input = ProposalInput {
        counterparty: counterparty_agent_key.to_string(),
        amount,
        note: note.map(|n| n.to_string()),
        lane_definitions: vec![],
    };

    let result = call_zome("create_proposal", Some(encode_payload(&input)?)).await?;
    info!("create_proposal raw result: {:?}", result);

    // ActionHashB64 comes back as a string like "uhCkk..."
    match &result {
        JsonValue::String(hash) => Ok(hash.clone()),
        // If it comes as binary, convert to HoloHash string
        other => {
            if let Some(hash_str) = binary_json_to_holohash(other) {
                Ok(hash_str)
            } else {
                Ok(serde_json::to_string(other)?)
            }
        }
    }
}

/// Get transaction status by action hash (accepts ActionHashB64 string like "uhCkk...").
pub async fn get_status(action_hash_b64: &str) -> Result<JsonValue, AnyError> {
    // The zome expects ActionHash (raw bytes), so convert from base64 representation
    let action_hash = holochain::prelude::ActionHash::try_from(action_hash_b64).map_err(|e| {
        deno_core::anyhow::anyhow!("Invalid action hash '{}': {}", action_hash_b64, e)
    })?;
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
    call_zome("get_actionable_transactions", Some(encode_payload(&links)?)).await
}

/// Send mHOT to an external address (host withdrawal).
/// Creates a proposal with negative amount (offer to send) to the recipient.
/// The recipient's AD4M will auto-commit, then we auto-accept.
pub async fn send_hot(
    recipient_agent_key: &str,
    amount_hot: &str,
    note: Option<&str>,
) -> Result<String, AnyError> {
    // Negate the amount: negative = "I send to counterparty"
    let neg_amount = format!("-{}", amount_hot.trim_start_matches('-'));
    let proposal_hash = create_proposal(&neg_amount, recipient_agent_key, note).await?;

    // Track in DB so the poller can auto-accept when commitment arrives
    Ad4mDb::with_global_instance(|db| {
        db.create_pending_send(recipient_agent_key, amount_hot, &proposal_hash)
    })
    .map_err(|e| anyhow::anyhow!("Failed to track pending send in DB: {}", e))?;

    info!(
        "Created outgoing proposal {} to send {} HOT to {}",
        proposal_hash, amount_hot, recipient_agent_key
    );
    Ok(proposal_hash)
}

/// Accept a commitment (step 3 in the negotiated flow: Proposal → Commitment → Accept).
/// The commitment_hash is an ActionHashB64 string.
pub async fn accept_commitment(
    commitment_hash_b64: &str,
    note: Option<&str>,
) -> Result<String, AnyError> {
    let input = AcceptInput {
        commitment: commitment_hash_b64.to_string(),
        note: note.map(|n| n.to_string()),
    };

    let result = call_zome("create_accept", Some(encode_payload(&input)?)).await?;
    info!("create_accept raw result: {:?}", result);

    match &result {
        JsonValue::String(hash) => Ok(hash.clone()),
        other => {
            if let Some(hash_str) = binary_json_to_holohash(other) {
                Ok(hash_str)
            } else {
                Ok(serde_json::to_string(other)?)
            }
        }
    }
}

/// Helper: credit user and mark payment request as completed.
fn credit_and_complete(request: &crate::db::PaymentRequest) {
    let amount = match request.amount_hot.parse::<f64>() {
        Ok(amount) => amount,
        Err(e) => {
            error!("Failed to parse amount '{}': {}", request.amount_hot, e);
            return;
        }
    };

    if let Err(e) =
        Ad4mDb::with_global_instance(|db| db.add_user_credits(&request.user_email, amount))
    {
        error!("Failed to credit user {}: {}", request.user_email, e);
        return;
    }

    info!("Credited user {} with {} HOT", request.user_email, amount);

    if let Some(ref action_hash) = request.proposal_action_hash {
        let _ = Ad4mDb::with_global_instance(|db| db.complete_payment_request(action_hash));
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

    let tx_type_str = tx.get("tx_type").and_then(|v| v.as_str()).unwrap_or("");
    info!(
        "Unyt signal: received tx_type={}, payload={}",
        tx_type_str, tx
    );

    let tx_id = tx.get("id").and_then(|v| v.as_str()).map(|s| s.to_string());

    // Handle incoming Proposal: auto-commit only if it matches a tracked pending request
    if tx_type_str == "Proposal" {
        if let Some(ref proposal_hash) = tx_id {
            // Verify this proposal is tracked (either as a payment_request or pending_send)
            let is_tracked = matches!(
                Ad4mDb::with_global_instance(|db| db.get_payment_request_by_hash(proposal_hash)),
                Ok(Some(_))
            ) || matches!(
                Ad4mDb::with_global_instance(|db| db.get_pending_send_by_hash(proposal_hash)),
                Ok(Some(_))
            );
            if !is_tracked {
                info!(
                    "Unyt signal: ignoring untracked proposal {}, not auto-committing",
                    proposal_hash
                );
                return;
            }
            info!(
                "Unyt signal: incoming proposal {}, auto-committing...",
                proposal_hash
            );
            // Build commitment input from the proposal
            let counterparty = tx
                .get("counterparty")
                .and_then(|c| c.as_array())
                .and_then(|arr| arr.first())
                .and_then(|v| v.as_str());
            let amount_obj = tx.get("amount").and_then(|a| a.as_object());

            if let (Some(cp), Some(amt)) = (counterparty, amount_obj) {
                let mut amount = BTreeMap::new();
                for (k, v) in amt {
                    if let Some(val) = v.as_str() {
                        amount.insert(k.clone(), val.to_string());
                    }
                }
                let input = CommitmentInput {
                    counterparty: cp.to_string(),
                    amount,
                    note: Some("Auto-committed".to_string()),
                    lane_definitions: vec![],
                };
                match encode_payload(&input) {
                    Ok(payload) => match call_zome("create_commitment", Some(payload)).await {
                        Ok(result) => {
                            info!("Auto-committed to proposal {}: {:?}", proposal_hash, result)
                        }
                        Err(e) => {
                            warn!("Failed to auto-commit to proposal {}: {}", proposal_hash, e)
                        }
                    },
                    Err(e) => warn!(
                        "Failed to encode commitment payload for proposal {}: {}",
                        proposal_hash, e
                    ),
                }
            }
        }
        return;
    }

    // Handle incoming Commitment on our proposal: auto-accept only if tracked
    if tx_type_str == "Commitment" {
        if let Some(ref commitment_hash) = tx_id {
            // Check that the related proposal is one we initiated
            let related_proposal = tx
                .get("proposal")
                .and_then(|v| v.as_str())
                .or_else(|| tx.get("related_proposal").and_then(|v| v.as_str()));
            let is_tracked = related_proposal.map_or(false, |hash| {
                matches!(
                    Ad4mDb::with_global_instance(|db| db.get_pending_send_by_hash(hash)),
                    Ok(Some(_))
                ) || matches!(
                    Ad4mDb::with_global_instance(|db| db.get_payment_request_by_hash(hash)),
                    Ok(Some(_))
                )
            });
            if !is_tracked {
                info!(
                    "Unyt signal: ignoring untracked commitment {}, not auto-accepting",
                    commitment_hash
                );
                return;
            }
            info!(
                "Unyt signal: incoming commitment {}, auto-accepting...",
                commitment_hash
            );
            match accept_commitment(commitment_hash, None).await {
                Ok(accept_hash) => info!(
                    "Auto-accepted commitment {} -> {}",
                    commitment_hash, accept_hash
                ),
                Err(e) => warn!(
                    "Failed to auto-accept commitment {}: {}",
                    commitment_hash, e
                ),
            }
        }
        return;
    }

    // Handle rejection: clean up pending sends/payment requests
    if tx_type_str == "Rejection" || tx_type_str == "Declined" || tx_type_str == "Cancelled" {
        // Try to find the related proposal hash and mark as rejected
        let related_hash = tx
            .get("proposal")
            .and_then(|v| v.as_str())
            .or_else(|| tx.get("related_proposal").and_then(|v| v.as_str()))
            .or_else(|| tx_id.as_deref());
        if let Some(hash) = related_hash {
            info!(
                "Unyt signal: transaction {} rejected/declined, cleaning up",
                hash
            );
            let _ = Ad4mDb::with_global_instance(|db| db.reject_pending_send(hash));
            let _ = Ad4mDb::with_global_instance(|db| db.reject_payment_request(hash));
        }
        return;
    }

    // Handle Accept and Receipt — these indicate completed payments (hosting payment flow)
    if tx_type_str != "Accept" && tx_type_str != "Receipt" {
        info!("Unyt signal: ignoring tx_type={}", tx_type_str);
        return;
    }

    info!("Unyt signal: received {} transaction", tx_type_str);

    // Extract the proposal hash so we can check idempotency against payment_requests
    let proposal_hash = tx
        .get("proposal")
        .and_then(|v| v.as_str())
        .or_else(|| tx.get("related_proposal").and_then(|v| v.as_str()))
        .or_else(|| tx_id.as_deref());

    // Idempotency: if we have a proposal hash, check whether this payment was already completed
    if let Some(hash) = proposal_hash {
        match Ad4mDb::with_global_instance(|db| db.get_payment_request_by_hash(hash)) {
            Ok(Some(ref req)) if req.status == "completed" => {
                info!(
                    "Unyt signal: payment for proposal {} already completed, ignoring duplicate",
                    hash
                );
                return;
            }
            _ => {}
        }
    }

    // Extract amount from the transaction
    let amount = tx.get("amount");
    let hot_amount = amount
        .and_then(|a| a.get("HOT"))
        .and_then(|v| v.as_str())
        .or_else(|| {
            amount.and_then(|a| {
                a.as_object()
                    .and_then(|obj| obj.values().next().and_then(|v| v.as_str()))
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
    let user_email =
        Ad4mDb::with_global_instance(|db| db.get_user_by_hot_wallet_address(counterparty));

    match user_email {
        Ok(Some(email)) => match hot_amount.parse::<f64>() {
            Ok(amount_f64) => {
                // Verify pending request matches if we have a proposal hash
                if let Some(hash) = proposal_hash {
                    match Ad4mDb::with_global_instance(|db| db.get_payment_request_by_hash(hash)) {
                        Ok(Some(ref req)) => {
                            if req.user_email != email {
                                warn!(
                                    "Unyt signal: payment request payer mismatch for {}: expected {}, got {}",
                                    hash, req.user_email, email
                                );
                                return;
                            }
                        }
                        Ok(None) => {
                            // No tracked request — could be an ad-hoc payment, proceed
                        }
                        Err(e) => {
                            error!("DB error checking payment request {}: {}", hash, e);
                            return;
                        }
                    }
                }

                // Credit user first, then mark completed only on success
                if let Err(e) =
                    Ad4mDb::with_global_instance(|db| db.add_user_credits(&email, amount_f64))
                {
                    error!(
                        "Failed to credit user {} with {} HOT: {}",
                        email, amount_f64, e
                    );
                } else {
                    info!(
                        "Credited user {} with {} HOT from mHOT payment",
                        email, amount_f64
                    );
                    if let Some(hash) = proposal_hash {
                        let _ =
                            Ad4mDb::with_global_instance(|db| db.complete_payment_request(hash));
                    }
                }
            }
            Err(e) => {
                error!("Failed to parse HOT amount '{}': {}", hot_amount, e);
            }
        },
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
                    info!("Payment request {} status: {:?}", action_hash, status);

                    // State is { status: WatchStatus, initial_amount, action_message, related_transaction }
                    // WatchStatus::Completed serializes as "Completed" string
                    let watch_status = status.get("status");
                    let is_completed = match watch_status {
                        Some(serde_json::Value::String(s)) => s == "Completed",
                        // Enum variant with data: {"Completed": ...}
                        Some(serde_json::Value::Object(obj)) => obj.contains_key("Completed"),
                        _ => false,
                    };

                    if is_completed {
                        info!(
                            "Payment request {} completed for user {}",
                            action_hash, request.user_email
                        );
                        credit_and_complete(&request);
                        continue;
                    }

                    // Check if there's a Commitment in related_transaction that we need to Accept
                    let needs_accept = status
                        .get("related_transaction")
                        .and_then(|rt| rt.as_array())
                        .and_then(|txs| {
                            txs.iter().find(|tx| {
                                tx.get("tx_type").and_then(|t| t.as_str()) == Some("Commitment")
                            })
                        })
                        .and_then(|commitment_tx| {
                            commitment_tx
                                .get("id")
                                .and_then(|id| id.as_str())
                                .map(|s| s.to_string())
                        });

                    if let Some(commitment_hash) = needs_accept {
                        info!(
                            "Payment request {}: found commitment {}, auto-accepting...",
                            action_hash, commitment_hash
                        );
                        match accept_commitment(&commitment_hash, None).await {
                            Ok(accept_hash) => {
                                info!(
                                    "Auto-accepted commitment {} -> accept hash: {}",
                                    commitment_hash, accept_hash
                                );
                                // After accept, the payment may still need a receipt step.
                                // We'll detect completion on the next poll cycle.
                            }
                            Err(e) => {
                                warn!(
                                    "Failed to auto-accept commitment {}: {}",
                                    commitment_hash, e
                                );
                            }
                        }
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

/// Check pending outgoing sends and auto-accept commitments.
/// Called periodically as a fallback for missed signals.
pub async fn check_pending_sends() {
    let pending = match Ad4mDb::with_global_instance(|db| db.get_pending_sends()) {
        Ok(sends) => sends,
        Err(e) => {
            warn!("Failed to get pending sends: {}", e);
            return;
        }
    };

    if pending.is_empty() {
        return;
    }

    info!("Checking {} pending outgoing sends...", pending.len());

    for (_recipient, _amount, proposal_hash) in pending {
        match get_status(&proposal_hash).await {
            Ok(status) => {
                log::info!("Outgoing send {} status: {:?}", proposal_hash, status);
                let watch_status = status.get("status");
                let is_completed = match watch_status {
                    Some(serde_json::Value::String(s)) => s == "Completed",
                    Some(serde_json::Value::Object(obj)) => obj.contains_key("Completed"),
                    _ => false,
                };

                if is_completed {
                    info!("Outgoing send {} completed", proposal_hash);
                    let _ =
                        Ad4mDb::with_global_instance(|db| db.complete_pending_send(&proposal_hash));
                    continue;
                }

                // Check for rejection
                let is_rejected = match watch_status {
                    Some(serde_json::Value::String(s)) => {
                        s == "Rejected" || s == "Declined" || s == "Cancelled"
                    }
                    Some(serde_json::Value::Object(obj)) => {
                        obj.contains_key("Rejected")
                            || obj.contains_key("Declined")
                            || obj.contains_key("Cancelled")
                    }
                    _ => false,
                };

                if is_rejected {
                    info!("Outgoing send {} was rejected", proposal_hash);
                    let _ =
                        Ad4mDb::with_global_instance(|db| db.reject_pending_send(&proposal_hash));
                    continue;
                }

                // Check for commitment to auto-accept
                let needs_accept = status
                    .get("related_transaction")
                    .and_then(|rt| rt.as_array())
                    .and_then(|txs| {
                        txs.iter().find(|tx| {
                            tx.get("tx_type").and_then(|t| t.as_str()) == Some("Commitment")
                        })
                    })
                    .and_then(|commitment_tx| {
                        commitment_tx
                            .get("id")
                            .and_then(|id| id.as_str())
                            .map(|s| s.to_string())
                    });

                if let Some(commitment_hash) = needs_accept {
                    info!(
                        "Outgoing send {}: found commitment {}, auto-accepting...",
                        proposal_hash, commitment_hash
                    );
                    match accept_commitment(&commitment_hash, None).await {
                        Ok(accept_hash) => {
                            info!(
                                "Auto-accepted commitment {} -> {}",
                                commitment_hash, accept_hash
                            );
                        }
                        Err(e) => {
                            let err_str = format!("{}", e);
                            if err_str.contains("Link does not exist")
                                || err_str.contains("not found")
                            {
                                info!("Outgoing send {} was rejected (link removed), marking as rejected", proposal_hash);
                                let _ = Ad4mDb::with_global_instance(|db| {
                                    db.reject_pending_send(&proposal_hash)
                                });
                            } else {
                                warn!(
                                    "Failed to auto-accept commitment {}: {}",
                                    commitment_hash, e
                                );
                            }
                        }
                    }
                }
            }
            Err(e) => {
                let err_str = format!("{}", e);
                if err_str.contains("Link does not exist") || err_str.contains("not found") {
                    info!(
                        "Outgoing send {} was rejected (get_status failed), marking as rejected",
                        proposal_hash
                    );
                    match Ad4mDb::with_global_instance(|db| db.reject_pending_send(&proposal_hash))
                    {
                        Ok(_) => info!(
                            "Successfully marked send {} as rejected in DB",
                            proposal_hash
                        ),
                        Err(e) => warn!("Failed to mark send {} as rejected: {}", proposal_hash, e),
                    }
                } else {
                    warn!(
                        "Failed to check status of outgoing send {}: {}",
                        proposal_hash, e
                    );
                }
            }
        }
    }
}

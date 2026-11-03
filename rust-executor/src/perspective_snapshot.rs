//! Per-perspective snapshots of the OxiGraph/RocksDB perspective stores.
//!
//! Periodically archives each perspective's store as its own tar.gz, lists the
//! archives in a JSON manifest (`snapshots/{did}/manifest.json` on the backend),
//! and uploads both via HMAC-presigned URLs. An open store is archived from a
//! RocksDB checkpoint, never from its live directory.
//!
//! On start-up an executor that opens its perspectives eagerly downloads every
//! archive before they open ([`restore_perspectives`]). One that defers them
//! (shared DB backend) downloads the manifest only ([`restore_perspectives_lazy`]);
//! each archive downloads when its perspective hydrates
//! ([`restore_perspective_archive`]). Both fall back to the monolithic
//! `perspectives.tar.gz` of earlier executors when no manifest exists.
//!
//! The presign endpoint lives at `{platform_base}/internal/snapshots/presign`.
//! Platform base URL derives from `wallet_backend_url` config.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use std::path::{Path, PathBuf};
use tar::{Archive, Builder};

use crate::config::Ad4mConfig;
use crate::perspectives::sparql_store::SparqlStore;
use std::sync::Mutex;

// ── Manifest ────────────────────────────────────────────────────────────────────

/// The perspective archives one agent's snapshot holds, stored as
/// `snapshots/{did}/manifest.json` on the backend.
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq)]
pub struct SnapshotManifest {
    /// When the manifest was written (RFC 3339).
    pub created_at: String,
    pub perspectives: Vec<ManifestEntry>,
}

/// One perspective's archive in a [`SnapshotManifest`].
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq)]
pub struct ManifestEntry {
    pub uuid: String,
    /// Size of the tar.gz archive in bytes.
    pub size_bytes: u64,
    /// When the archive was uploaded (RFC 3339).
    pub last_modified: String,
}

impl SnapshotManifest {
    fn entry(&self, uuid: &str) -> Option<&ManifestEntry> {
        self.perspectives.iter().find(|e| e.uuid == uuid)
    }
}

/// The manifest the remote snapshot holds, as this executor last read or wrote it.
/// `None` until the start-up restore read it: a backup then cannot tell which
/// archives a deferred perspective has, so it refuses rather than drop them.
static CURRENT_MANIFEST: Mutex<Option<SnapshotManifest>> = Mutex::new(None);

fn current_manifest() -> Option<SnapshotManifest> {
    CURRENT_MANIFEST
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .clone()
}

fn set_current_manifest(manifest: Option<SnapshotManifest>) {
    *CURRENT_MANIFEST
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = manifest;
}

/// Whether this executor backs its perspectives up to (and restores them from) the
/// snapshot backend: in shared wallet mode.
pub fn snapshots_enabled(config: &Ad4mConfig) -> bool {
    config.wallet_backend.as_deref() == Some("shared")
}

// ── Path helpers ───────────────────────────────────────────────────────────────

/// Resolve the `perspectives/` directory under the executor data path.
pub fn perspectives_dir(config: &Ad4mConfig) -> Result<PathBuf, AnyError> {
    let data_path = config
        .app_data_path
        .as_ref()
        .ok_or_else(|| anyhow!("app_data_path not configured"))?;
    Ok(PathBuf::from(data_path).join("perspectives"))
}

/// Derive the platform Worker base URL from `wallet_backend_url`.
///
/// `wallet_backend_url` looks like `http://host:port/internal/wallet`.
/// Strips the path from `/internal/` onward to get `http://host:port`.
fn platform_base_url(config: &Ad4mConfig) -> Result<String, AnyError> {
    let url = config
        .wallet_backend_url
        .as_ref()
        .or(config.db_backend_url.as_ref())
        .ok_or_else(|| anyhow!("wallet_backend_url or db_backend_url required for snapshots"))?;

    if let Some(idx) = url.find("/internal/") {
        Ok(url[..idx].to_string())
    } else {
        Ok(url.clone())
    }
}

fn presign_url(config: &Ad4mConfig) -> Result<String, AnyError> {
    let base = platform_base_url(config)?;
    Ok(format!("{}/internal/snapshots/presign", base))
}

fn internal_token(config: &Ad4mConfig) -> Result<String, AnyError> {
    // The Worker's bearer token, sent with the snapshot requests.
    config
        .internal_api_token
        .as_ref()
        .map(|token| token.expose().to_string())
        .ok_or_else(|| anyhow!("INTERNAL_API_TOKEN required for snapshots"))
}

// ── Tar/Gz operations ──────────────────────────────────────────────────────────

/// Create a tar.gz archive of the `perspectives/` directory.
///
/// Returns the compressed bytes. Each perspective UUID subdirectory
/// (containing `sparql_store/` RocksDB files) gets included recursively.
pub fn create_snapshot(perspectives_path: &Path) -> Result<Vec<u8>, AnyError> {
    if !perspectives_path.exists() {
        return Err(anyhow!(
            "Perspectives directory does not exist: {:?}",
            perspectives_path
        ));
    }

    let mut compressed = Vec::new();
    {
        let encoder = GzEncoder::new(&mut compressed, Compression::fast());
        let mut archive = Builder::new(encoder);

        // Append the entire perspectives directory, preserving the tree structure.
        // Use "perspectives" as the archive prefix so restore extracts cleanly.
        archive
            .append_dir_all("perspectives", perspectives_path)
            .map_err(|e| anyhow!("Failed to build snapshot archive: {}", e))?;

        let encoder = archive
            .into_inner()
            .map_err(|e| anyhow!("Failed to finalise archive: {}", e))?;
        encoder
            .finish()
            .map_err(|e| anyhow!("Failed to finish gzip: {}", e))?;
    }

    log::info!(
        "Created snapshot: {} bytes from {:?}",
        compressed.len(),
        perspectives_path,
    );
    Ok(compressed)
}

/// Extract a tar.gz snapshot into the data directory.
///
/// The archive contains a `perspectives/` top-level directory.
/// Extracts into `data_path` so `{data_path}/perspectives/{uuid}/sparql_store/`
/// gets restored.
pub fn restore_snapshot(data_path: &Path, snapshot: &[u8]) -> Result<(), AnyError> {
    let decoder = GzDecoder::new(snapshot);
    let mut archive = Archive::new(decoder);

    archive
        .unpack(data_path)
        .map_err(|e| anyhow!("Failed to extract snapshot: {}", e))?;

    log::info!("Restored snapshot to {:?}", data_path);
    Ok(())
}

/// Create a tar.gz archive of one perspective: `{perspectives_path}/{uuid}/`, under the
/// prefix `perspectives/{uuid}/`, the layout [`restore_perspective_snapshot`] restores.
pub fn create_perspective_snapshot(
    perspectives_path: &Path,
    uuid: &str,
) -> Result<Vec<u8>, AnyError> {
    let persp_subdir = perspectives_path.join(uuid);
    if !persp_subdir.exists() {
        return Err(anyhow!(
            "Perspective directory does not exist: {:?}",
            persp_subdir
        ));
    }

    let mut compressed = Vec::new();
    {
        let encoder = GzEncoder::new(&mut compressed, Compression::fast());
        let mut archive = Builder::new(encoder);
        archive
            .append_dir_all(format!("perspectives/{}", uuid), &persp_subdir)
            .map_err(|e| anyhow!("Failed to build perspective archive: {}", e))?;
        let encoder = archive
            .into_inner()
            .map_err(|e| anyhow!("Failed to finalise archive: {}", e))?;
        encoder
            .finish()
            .map_err(|e| anyhow!("Failed to finish gzip: {}", e))?;
    }

    log::info!(
        "Created perspective snapshot for {}: {} bytes",
        uuid,
        compressed.len(),
    );
    Ok(compressed)
}

/// Restore one perspective's archive to `{perspectives_path}/{uuid}/`, replacing what
/// is there. It extracts next to `perspectives_path` first, so a failed extraction
/// leaves the old directory, and files of an older store never mix with the new one.
/// Only the archive's `perspectives/{uuid}/` tree is kept.
pub fn restore_perspective_snapshot(
    perspectives_path: &Path,
    uuid: &str,
    snapshot: &[u8],
) -> Result<(), AnyError> {
    let parent = perspectives_path.parent().unwrap_or(perspectives_path);
    let staging = parent.join(format!(".restore-{}", uuid::Uuid::new_v4()));
    let result = (|| {
        restore_snapshot(&staging, snapshot)?;
        let extracted = staging.join("perspectives").join(uuid);
        if !extracted.is_dir() {
            return Err(anyhow!(
                "Archive for {} holds no perspectives/{}/",
                uuid,
                uuid
            ));
        }
        std::fs::create_dir_all(perspectives_path)
            .map_err(|e| anyhow!("Failed to create {:?}: {}", perspectives_path, e))?;
        let target = perspectives_path.join(uuid);
        if target.exists() {
            std::fs::remove_dir_all(&target)
                .map_err(|e| anyhow!("Failed to replace {:?}: {}", target, e))?;
        }
        std::fs::rename(&extracted, &target)
            .map_err(|e| anyhow!("Failed to move {:?} to {:?}: {}", extracted, target, e))
    })();
    if staging.exists() {
        if let Err(e) = std::fs::remove_dir_all(&staging) {
            log::warn!("Failed to remove restore staging {:?}: {}", staging, e);
        }
    }
    result
}

// ── HTTP operations ────────────────────────────────────────────────────────────

/// Presigned-URL response from the platform Worker.
#[derive(serde::Deserialize, Debug)]
struct PresignResponse {
    url: String,
    #[allow(dead_code)]
    token: String,
    #[allow(dead_code)]
    key: String,
}

/// What a presigned URL reaches in an agent's snapshot.
#[derive(Clone, Copy, Debug)]
enum SnapshotObject<'a> {
    /// The monolithic `perspectives.tar.gz` of earlier executors (no `objectType`).
    Legacy,
    Manifest,
    Perspective(&'a str),
}

/// Request a presigned URL from the platform Worker.
fn presign(
    config: &Ad4mConfig,
    operation: &str,
    did: &str,
    object: SnapshotObject,
) -> Result<PresignResponse, AnyError> {
    let url = presign_url(config)?;
    let token = internal_token(config)?;

    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(30))
        .build()
        .map_err(|e| anyhow!("HTTP client build: {}", e))?;

    let mut body = serde_json::json!({
        "operation": operation,
        "did": did,
    });
    match object {
        SnapshotObject::Legacy => {}
        SnapshotObject::Manifest => body["objectType"] = "manifest".into(),
        SnapshotObject::Perspective(uuid) => {
            body["objectType"] = "perspective".into();
            body["perspectiveUuid"] = uuid.into();
        }
    }

    let resp = client
        .post(&url)
        .header("Authorization", format!("Bearer {}", token))
        .json(&body)
        .send()
        .map_err(|e| anyhow!("Presign request failed: {}", e))?;

    if !resp.status().is_success() {
        return Err(anyhow!("Presign returned {}", resp.status()));
    }

    resp.json::<PresignResponse>()
        .map_err(|e| anyhow!("Presign response parse: {}", e))
}

/// Upload a snapshot object via a presigned PUT URL.
fn upload(url: &str, content_type: &str, data: Vec<u8>) -> Result<(), AnyError> {
    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(120))
        .build()
        .map_err(|e| anyhow!("HTTP client build: {}", e))?;

    let resp = client
        .put(url)
        .header("Content-Type", content_type)
        .body(data)
        .send()
        .map_err(|e| anyhow!("Snapshot upload failed: {}", e))?;

    if !resp.status().is_success() {
        return Err(anyhow!("Snapshot upload returned {}", resp.status()));
    }

    Ok(())
}

/// Download a snapshot via a presigned GET URL.
fn download(url: &str) -> Result<Vec<u8>, AnyError> {
    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(120))
        .build()
        .map_err(|e| anyhow!("HTTP client build: {}", e))?;

    let resp = client
        .get(url)
        .send()
        .map_err(|e| anyhow!("Snapshot download failed: {}", e))?;

    if resp.status().as_u16() == 404 {
        log::info!("No remote snapshot found (first run)");
        return Ok(Vec::new());
    }

    if !resp.status().is_success() {
        return Err(anyhow!("Snapshot download returned {}", resp.status()));
    }

    let bytes = resp
        .bytes()
        .map_err(|e| anyhow!("Snapshot download body: {}", e))?;
    Ok(bytes.to_vec())
}

// ── Public API ─────────────────────────────────────────────────────────────────

/// Checkpoint each store into `staging/perspectives/{uuid}/sparql_store`, the
/// layout `create_snapshot` archives and `restore_snapshot` restores. Returns
/// the `staging/perspectives` directory.
pub fn checkpoint_stores(
    stores: &[(String, std::sync::Arc<SparqlStore>)],
    staging: &Path,
) -> Result<PathBuf, AnyError> {
    let persp_dir = staging.join("perspectives");
    std::fs::create_dir_all(&persp_dir)
        .map_err(|e| anyhow!("Failed to create {:?}: {}", persp_dir, e))?;
    for (uuid, store) in stores {
        let uuid_dir = persp_dir.join(uuid);
        std::fs::create_dir_all(&uuid_dir)
            .map_err(|e| anyhow!("Failed to create {:?}: {}", uuid_dir, e))?;
        store.backup(&uuid_dir.join("sparql_store"))?;
    }
    Ok(persp_dir)
}

/// The stored perspectives not registered yet (`registered` lists the uuids that are).
fn missing_stores(
    stored: &[crate::types::PerspectiveHandle],
    registered: &[String],
) -> Vec<String> {
    stored
        .iter()
        .filter(|h| !registered.contains(&h.uuid))
        .map(|h| h.uuid.clone())
        .collect()
}

/// Backup all perspective data to the platform backend.
///
/// 1. Checkpoint every open SPARQL store into a staging directory next to
///    `perspectives/` (same filesystem, so the checkpoints hard-link), archive each
///    checkpoint and upload it.
/// 2. A deferred perspective keeps the archive the manifest lists for it: nothing
///    changed it since. One the manifest does not list (restored from a legacy
///    snapshot) is archived from its closed store directory, with its hydration held
///    off meanwhile.
/// 3. Upload the manifest of every archive, then remove the staging directory.
pub fn backup_perspectives(config: &Ad4mConfig) -> Result<(), AnyError> {
    let did = crate::agent::did();
    let persp_dir = perspectives_dir(config)?;

    if !persp_dir.exists() {
        log::info!("No perspectives directory — skipping backup");
        return Ok(());
    }

    let registered = crate::perspectives::all_perspectives();
    // Start-up opens each stored perspective in its own task. An archive taken
    // before all are open would replace the last full snapshot with a partial
    // one, so refuse until every stored perspective is registered.
    let stored = crate::db::Ad4mDb::with_global_instance(|db| db.get_all_perspectives())
        .map_err(|e| anyhow!("Failed to list perspectives: {}", e))?;
    let uuids: Vec<String> = registered.iter().map(|p| p.uuid.clone()).collect();
    let loading = missing_stores(&stored, &uuids);
    if !loading.is_empty() {
        return Err(anyhow!(
            "Skipping snapshot: {} perspective(s) still loading: {}",
            loading.len(),
            loading.join(", ")
        ));
    }
    // A unique name, so a shutdown backup and a periodic one cannot collide.
    let staging = persp_dir
        .parent()
        .unwrap_or(&persp_dir)
        .join(format!(".snapshot-{}", uuid::Uuid::new_v4()));
    let manifest = archive_perspectives(&registered, &persp_dir, &staging, &mut |uuid, data| {
        let presign_resp = presign(config, "put", &did, SnapshotObject::Perspective(uuid))?;
        upload(&presign_resp.url, "application/gzip", data)
    });
    if staging.exists() {
        if let Err(e) = std::fs::remove_dir_all(&staging) {
            log::warn!("Failed to remove snapshot staging {:?}: {}", staging, e);
        }
    }
    let manifest = manifest?;

    let manifest_json = serde_json::to_vec_pretty(&manifest)
        .map_err(|e| anyhow!("Failed to serialize manifest: {}", e))?;
    let presign_resp = presign(config, "put", &did, SnapshotObject::Manifest)?;
    upload(&presign_resp.url, "application/json", manifest_json)?;

    log::info!(
        "Snapshot uploaded for DID {} ({} perspectives)",
        did,
        manifest.perspectives.len()
    );
    set_current_manifest(Some(manifest));
    Ok(())
}

/// Archive every perspective in `registered` and pass each archive to `put`; return
/// the manifest of the archives. See [`backup_perspectives`].
fn archive_perspectives(
    registered: &[crate::perspectives::perspective_instance::PerspectiveInstance],
    persp_dir: &Path,
    staging: &Path,
    put: &mut dyn FnMut(&str, Vec<u8>) -> Result<(), AnyError>,
) -> Result<SnapshotManifest, AnyError> {
    let known = current_manifest();
    let mut entries = Vec::new();
    let mut archive_and_put = |dir: &Path, uuid: &str| -> Result<ManifestEntry, AnyError> {
        let data = create_perspective_snapshot(dir, uuid)?;
        let size_bytes = data.len() as u64;
        put(uuid, data)?;
        Ok(ManifestEntry {
            uuid: uuid.to_string(),
            size_bytes,
            last_modified: chrono::Utc::now().to_rfc3339(),
        })
    };

    for p in registered {
        let deferred = p.while_deferred(|| -> Result<Option<ManifestEntry>, AnyError> {
            let known = known.as_ref().ok_or_else(|| {
                anyhow!(
                    "Skipping snapshot: perspective {} is deferred and the remote manifest was never read",
                    p.uuid
                )
            })?;
            if let Some(entry) = known.entry(&p.uuid) {
                return Ok(Some(entry.clone()));
            }
            if persp_dir.join(&p.uuid).join("sparql_store").exists() {
                return archive_and_put(persp_dir, &p.uuid).map(Some);
            }
            // Never stored anything: no archive to keep.
            Ok(None)
        });
        match deferred {
            Some(entry) => entries.extend(entry?),
            None => {
                let checkpoints = checkpoint_stores(&[(p.uuid.clone(), p.store())], staging)?;
                entries.push(archive_and_put(&checkpoints, &p.uuid)?);
            }
        }
    }

    Ok(SnapshotManifest {
        created_at: chrono::Utc::now().to_rfc3339(),
        perspectives: entries,
    })
}

/// Restore every perspective archive from the platform backend, if available.
///
/// Called before perspective initialisation when perspectives open eagerly.
/// Reads the manifest and restores each archive it lists; without a manifest,
/// restores the legacy monolithic snapshot.
/// Returns Ok(true) if data was restored, Ok(false) if no snapshot existed.
pub fn restore_perspectives(config: &Ad4mConfig) -> Result<bool, AnyError> {
    let did = crate::agent::did();
    let persp_dir = perspectives_dir(config)?;

    let Some(manifest) = fetch_manifest(config, &did)? else {
        return restore_legacy(config, &did);
    };
    for entry in &manifest.perspectives {
        if !fetch_perspective(config, &did, &persp_dir, &entry.uuid)? {
            log::warn!(
                "Perspective archive for {} not found — skipping",
                entry.uuid
            );
        }
    }
    set_current_manifest(Some(manifest));
    Ok(true)
}

/// Restore for an executor that defers its perspectives: read the manifest only;
/// each perspective's archive downloads when it hydrates
/// ([`restore_perspective_archive`]). Without a manifest, restores the legacy
/// monolithic snapshot in full, so its perspectives hydrate from their local stores.
/// Returns Ok(true) if a snapshot existed, Ok(false) if not.
pub fn restore_perspectives_lazy(config: &Ad4mConfig) -> Result<bool, AnyError> {
    let did = crate::agent::did();

    let Some(manifest) = fetch_manifest(config, &did)? else {
        return restore_legacy(config, &did);
    };
    log::info!(
        "📋 Manifest lists {} perspectives for DID {} (archives download on first access)",
        manifest.perspectives.len(),
        did
    );
    set_current_manifest(Some(manifest));
    Ok(true)
}

/// Download and restore perspective `uuid`'s archive, for its hydration.
/// Returns Ok(false) when the snapshot holds no archive for it (a perspective
/// created since, or one restored from a legacy snapshot).
pub fn restore_perspective_archive(config: &Ad4mConfig, uuid: &str) -> Result<bool, AnyError> {
    if let Some(manifest) = current_manifest() {
        if manifest.entry(uuid).is_none() {
            return Ok(false);
        }
    }
    let did = crate::agent::did();
    fetch_perspective(config, &did, &perspectives_dir(config)?, uuid)
}

/// The remote manifest, or `None` when the snapshot has none.
fn fetch_manifest(config: &Ad4mConfig, did: &str) -> Result<Option<SnapshotManifest>, AnyError> {
    let presign_resp = presign(config, "get", did, SnapshotObject::Manifest)?;
    let data = download(&presign_resp.url)?;
    if data.is_empty() {
        return Ok(None);
    }
    serde_json::from_slice(&data)
        .map(Some)
        .map_err(|e| anyhow!("Failed to parse snapshot manifest: {}", e))
}

/// Download and restore one perspective's archive; Ok(false) if it has none.
fn fetch_perspective(
    config: &Ad4mConfig,
    did: &str,
    persp_dir: &Path,
    uuid: &str,
) -> Result<bool, AnyError> {
    let presign_resp = presign(config, "get", did, SnapshotObject::Perspective(uuid))?;
    let data = download(&presign_resp.url)?;
    if data.is_empty() {
        return Ok(false);
    }
    restore_perspective_snapshot(persp_dir, uuid, &data)?;
    log::info!("📥 Restored perspective {} ({} bytes)", uuid, data.len());
    Ok(true)
}

/// Restore the monolithic snapshot of earlier executors, if there is one. Either
/// way the snapshot now has no per-perspective archives: the next backup writes
/// them and the manifest.
fn restore_legacy(config: &Ad4mConfig, did: &str) -> Result<bool, AnyError> {
    let data_path = config
        .app_data_path
        .as_ref()
        .ok_or_else(|| anyhow!("app_data_path not configured"))?;
    let presign_resp = presign(config, "get", did, SnapshotObject::Legacy)?;
    let data = download(&presign_resp.url)?;
    let restored = if data.is_empty() {
        log::info!("No remote snapshot for DID {} — starting fresh", did);
        false
    } else {
        log::info!(
            "Downloaded {} byte legacy snapshot for DID {} — restoring",
            data.len(),
            did
        );
        restore_snapshot(Path::new(data_path), &data)?;
        true
    };
    set_current_manifest(Some(SnapshotManifest {
        created_at: chrono::Utc::now().to_rfc3339(),
        perspectives: Vec::new(),
    }));
    Ok(restored)
}

/// Spawn a background task that periodically backs up perspectives to the platform backend.
///
/// Runs every `snapshot_interval_secs` (default 300 = 5 minutes).
/// Logs errors but does not propagate — backup failure must not crash the executor.
pub fn spawn_periodic_backup(config: Ad4mConfig) {
    let interval_secs = config.snapshot_interval_secs.unwrap_or(300);
    if interval_secs == 0 {
        log::info!("Periodic perspective snapshots disabled (interval = 0)");
        return;
    }

    log::info!(
        "Starting periodic perspective snapshot every {}s",
        interval_secs
    );

    tokio::task::spawn(async move {
        let mut interval = tokio::time::interval(std::time::Duration::from_secs(interval_secs));
        // Skip the first tick (fires immediately on creation)
        interval.tick().await;

        loop {
            interval.tick().await;
            let config_clone = config.clone();
            let result =
                tokio::task::spawn_blocking(move || backup_perspectives(&config_clone)).await;

            match result {
                Ok(Ok(())) => log::debug!("Periodic snapshot completed"),
                Ok(Err(e)) => log::warn!("Periodic snapshot failed: {}", e),
                Err(e) => log::warn!("Periodic snapshot task panic: {}", e),
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_create_and_restore_snapshot() {
        // Set up a fake perspectives directory
        let src_dir = TempDir::new().unwrap();
        let persp_dir = src_dir.path().join("perspectives");
        let uuid_dir = persp_dir.join("test-uuid-1234").join("sparql_store");
        fs::create_dir_all(&uuid_dir).unwrap();
        fs::write(uuid_dir.join("data.db"), b"fake-rocksdb-data").unwrap();
        fs::write(uuid_dir.join("wal.log"), b"write-ahead-log").unwrap();

        // Create snapshot
        let data = create_snapshot(&persp_dir).unwrap();
        assert!(!data.is_empty());

        // Restore to a different directory
        let dst_dir = TempDir::new().unwrap();
        restore_snapshot(dst_dir.path(), &data).unwrap();

        // Verify files exist
        let restored = dst_dir
            .path()
            .join("perspectives/test-uuid-1234/sparql_store/data.db");
        assert!(restored.exists());
        assert_eq!(fs::read(&restored).unwrap(), b"fake-rocksdb-data");

        let restored_wal = dst_dir
            .path()
            .join("perspectives/test-uuid-1234/sparql_store/wal.log");
        assert!(restored_wal.exists());
        assert_eq!(fs::read(&restored_wal).unwrap(), b"write-ahead-log");
    }

    /// Checkpointed stores round-trip through the archive: each restored store
    /// opens with its links.
    #[test]
    fn checkpointed_stores_restore_with_their_links() {
        use crate::agent::signatures::TestSigner;
        use crate::types::{Link, LinkExpression, LinkStatus};
        use std::sync::Arc;

        let signer = TestSigner::generate();
        let link = |i: usize| {
            let signed = signer.sign(
                Link {
                    source: format!("ad4m://s/{i}"),
                    predicate: Some("ad4m://p".to_string()),
                    target: format!("ad4m://t/{i}"),
                }
                .normalize(),
            );
            LinkExpression {
                author: signed.author,
                timestamp: signed.timestamp,
                data: signed.data,
                proof: signed.proof,
                status: Some(LinkStatus::Shared),
            }
        };

        let live = TempDir::new().unwrap();
        let mut stores = Vec::new();
        for (uuid, count) in [("uuid-a", 3), ("uuid-b", 5)] {
            let dir = live.path().join("perspectives").join(uuid);
            let store = Arc::new(SparqlStore::new(Some(dir.to_str().unwrap())).unwrap());
            for i in 0..count {
                store.add_link(&link(i)).unwrap();
            }
            stores.push((uuid.to_string(), store));
        }

        let staging = live.path().join(".snapshot-test");
        let persp_dir = checkpoint_stores(&stores, &staging).unwrap();
        let data = create_snapshot(&persp_dir).unwrap();

        let restored = TempDir::new().unwrap();
        restore_snapshot(restored.path(), &data).unwrap();
        for (uuid, count) in [("uuid-a", 3), ("uuid-b", 5)] {
            let dir = restored.path().join("perspectives").join(uuid);
            let store = SparqlStore::new(Some(dir.to_str().unwrap())).unwrap();
            assert_eq!(store.get_all_links().unwrap().len(), count, "{uuid}");
        }
    }

    /// A stored perspective not registered yet blocks the backup.
    #[test]
    fn a_stored_perspective_not_registered_is_missing() {
        use crate::types::PerspectiveHandle;

        let handle = |uuid: &str| PerspectiveHandle {
            uuid: uuid.to_string(),
            ..Default::default()
        };
        let registered = vec!["uuid-a".to_string()];

        assert!(missing_stores(&[handle("uuid-a")], &registered).is_empty());
        assert_eq!(
            missing_stores(&[handle("uuid-a"), handle("uuid-b")], &registered),
            vec!["uuid-b".to_string()]
        );
    }

    fn signed_link(
        signer: &crate::agent::signatures::TestSigner,
        i: usize,
    ) -> crate::types::LinkExpression {
        use crate::types::{Link, LinkExpression, LinkStatus};
        let signed = signer.sign(
            Link {
                source: format!("ad4m://s/{i}"),
                predicate: Some("ad4m://p".to_string()),
                target: format!("ad4m://t/{i}"),
            }
            .normalize(),
        );
        LinkExpression {
            author: signed.author,
            timestamp: signed.timestamp,
            data: signed.data,
            proof: signed.proof,
            status: Some(LinkStatus::Shared),
        }
    }

    fn store_at(dir: &Path, links: usize) -> SparqlStore {
        let signer = crate::agent::signatures::TestSigner::generate();
        let store = SparqlStore::new(Some(dir.to_str().unwrap())).unwrap();
        for i in 0..links {
            store.add_link(&signed_link(&signer, i)).unwrap();
        }
        store
    }

    fn perspective(uuid: &str) -> crate::perspectives::perspective_instance::PerspectiveInstance {
        crate::perspectives::perspective_instance::PerspectiveInstance::new_deferred(
            crate::types::PerspectiveHandle {
                uuid: uuid.to_string(),
                ..Default::default()
            },
        )
    }

    fn entry(uuid: &str) -> ManifestEntry {
        ManifestEntry {
            uuid: uuid.to_string(),
            size_bytes: 42,
            last_modified: "2026-01-01T00:00:00Z".to_string(),
        }
    }

    /// Sets the current manifest for one test and clears it after.
    struct KnownManifest;
    impl KnownManifest {
        fn set(manifest: Option<SnapshotManifest>) -> Self {
            set_current_manifest(manifest);
            KnownManifest
        }
    }
    impl Drop for KnownManifest {
        fn drop(&mut self) {
            set_current_manifest(None);
        }
    }

    #[test]
    fn manifest_round_trips_through_json() {
        let manifest = SnapshotManifest {
            created_at: "2026-01-01T00:00:00Z".to_string(),
            perspectives: vec![entry("uuid-a"), entry("uuid-b")],
        };
        let json = serde_json::to_vec(&manifest).unwrap();
        assert_eq!(
            serde_json::from_slice::<SnapshotManifest>(&json).unwrap(),
            manifest
        );
    }

    /// A restored archive replaces the perspective's directory: no file of the store it
    /// replaces stays, and other perspectives are untouched.
    #[test]
    fn a_perspective_archive_replaces_its_directory() {
        let src = TempDir::new().unwrap();
        let src_persp = src.path().join("perspectives");
        fs::create_dir_all(src_persp.join("uuid-a/sparql_store")).unwrap();
        fs::write(src_persp.join("uuid-a/sparql_store/CURRENT"), b"new").unwrap();
        let archive = create_perspective_snapshot(&src_persp, "uuid-a").unwrap();

        let dst = TempDir::new().unwrap();
        let dst_persp = dst.path().join("perspectives");
        fs::create_dir_all(dst_persp.join("uuid-a/sparql_store")).unwrap();
        fs::write(dst_persp.join("uuid-a/sparql_store/OLD-000001.sst"), b"old").unwrap();
        fs::create_dir_all(dst_persp.join("uuid-b")).unwrap();
        fs::write(dst_persp.join("uuid-b/keep"), b"b").unwrap();

        restore_perspective_snapshot(&dst_persp, "uuid-a", &archive).unwrap();
        assert_eq!(
            fs::read(dst_persp.join("uuid-a/sparql_store/CURRENT")).unwrap(),
            b"new"
        );
        assert!(!dst_persp
            .join("uuid-a/sparql_store/OLD-000001.sst")
            .exists());
        assert_eq!(fs::read(dst_persp.join("uuid-b/keep")).unwrap(), b"b");
        let leftovers: Vec<_> = fs::read_dir(dst.path())
            .unwrap()
            .map(|e| e.unwrap().file_name())
            .filter(|n| n.to_string_lossy().starts_with(".restore-"))
            .collect();
        assert!(leftovers.is_empty(), "staging removed: {leftovers:?}");
    }

    /// An archive for another perspective does not restore over this one.
    #[test]
    fn an_archive_for_another_perspective_is_refused() {
        let src = TempDir::new().unwrap();
        let src_persp = src.path().join("perspectives");
        fs::create_dir_all(src_persp.join("uuid-b")).unwrap();
        let archive = create_perspective_snapshot(&src_persp, "uuid-b").unwrap();

        let dst = TempDir::new().unwrap();
        let dst_persp = dst.path().join("perspectives");
        assert!(restore_perspective_snapshot(&dst_persp, "uuid-a", &archive).is_err());
        assert!(!dst_persp.join("uuid-a").exists());
    }

    /// A backup archives each open store from a checkpoint, keeps a deferred
    /// perspective's listed archive without re-uploading it, archives a deferred
    /// perspective the manifest does not list from its closed store, and lists nothing
    /// for a deferred perspective with no store.
    #[test]
    fn a_backup_archives_open_stores_and_keeps_deferred_archives() {
        let data = TempDir::new().unwrap();
        let persp_dir = data.path().join("perspectives");

        let open = perspective("uuid-open");
        open.swap_store(store_at(&persp_dir.join("uuid-open"), 3));
        let listed = perspective("uuid-listed");
        let legacy = perspective("uuid-legacy");
        drop(store_at(&persp_dir.join("uuid-legacy"), 2));
        let empty = perspective("uuid-empty");

        let _known = KnownManifest::set(Some(SnapshotManifest {
            created_at: "2026-01-01T00:00:00Z".to_string(),
            perspectives: vec![entry("uuid-listed")],
        }));
        let mut uploaded = std::collections::BTreeMap::new();
        let manifest = archive_perspectives(
            &[open, listed, legacy, empty],
            &persp_dir,
            &data.path().join(".snapshot-test"),
            &mut |uuid, data| {
                uploaded.insert(uuid.to_string(), data);
                Ok(())
            },
        )
        .unwrap();

        let uuids: Vec<_> = manifest
            .perspectives
            .iter()
            .map(|e| e.uuid.as_str())
            .collect();
        assert_eq!(uuids, vec!["uuid-open", "uuid-listed", "uuid-legacy"]);
        assert_eq!(
            manifest.perspectives[1],
            entry("uuid-listed"),
            "kept as listed"
        );
        assert_eq!(
            uploaded.keys().collect::<Vec<_>>(),
            vec!["uuid-legacy", "uuid-open"]
        );

        for (uuid, links) in [("uuid-open", 3), ("uuid-legacy", 2)] {
            let restored = TempDir::new().unwrap();
            let restored_persp = restored.path().join("perspectives");
            restore_perspective_snapshot(&restored_persp, uuid, &uploaded[uuid]).unwrap();
            let store =
                SparqlStore::new(Some(restored_persp.join(uuid).to_str().unwrap())).unwrap();
            assert_eq!(store.get_all_links().unwrap().len(), links, "{uuid}");
        }
    }

    /// Without the remote manifest a backup cannot tell what a deferred perspective's
    /// archive is, so it refuses instead of writing a manifest without it.
    #[test]
    fn a_backup_refuses_a_deferred_perspective_without_the_manifest() {
        let data = TempDir::new().unwrap();
        let _known = KnownManifest::set(None);
        let result = archive_perspectives(
            &[perspective("uuid-deferred")],
            &data.path().join("perspectives"),
            &data.path().join(".snapshot-test"),
            &mut |_, _| panic!("nothing to upload"),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_create_snapshot_missing_dir() {
        let tmp = TempDir::new().unwrap();
        let missing = tmp.path().join("nonexistent");
        let result = create_snapshot(&missing);
        assert!(result.is_err());
    }

    #[test]
    fn test_create_snapshot_empty_dir() {
        let tmp = TempDir::new().unwrap();
        let persp = tmp.path().join("perspectives");
        fs::create_dir_all(&persp).unwrap();
        // Empty directory should still produce a valid (small) archive
        let data = create_snapshot(&persp).unwrap();
        assert!(!data.is_empty());
    }

    #[test]
    fn test_platform_base_url_derivation() {
        let mut config = Ad4mConfig::default();
        config.wallet_backend_url =
            Some("http://host.docker.internal:8787/internal/wallet".to_string());

        let base = platform_base_url(&config).unwrap();
        assert_eq!(base, "http://host.docker.internal:8787");

        let url = presign_url(&config).unwrap();
        assert_eq!(
            url,
            "http://host.docker.internal:8787/internal/snapshots/presign"
        );
    }

    #[test]
    fn test_platform_base_url_no_internal_path() {
        let mut config = Ad4mConfig::default();
        config.wallet_backend_url = Some("http://localhost:8787".to_string());

        let base = platform_base_url(&config).unwrap();
        assert_eq!(base, "http://localhost:8787");
    }

    #[test]
    fn test_platform_base_url_missing() {
        let mut config = Ad4mConfig::default();
        config.wallet_backend_url = None;
        config.db_backend_url = None;

        let result = platform_base_url(&config);
        assert!(result.is_err());
    }
}

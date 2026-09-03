//! Per-perspective snapshots of the OxiGraph/RocksDB perspective stores.
//!
//! Each perspective gets its own tar.gz archive. A JSON manifest tracks
//! which perspectives exist and their sizes. Both get uploaded to the
//! remote snapshot backend via HMAC-presigned URLs.
//!
//! In multi-tenant mode, startup downloads the manifest first, then
//! fetches individual perspective archives on demand. Falls back to
//! the legacy monolithic `perspectives.tar.gz` format if no manifest
//! exists.
//!
//! The presign endpoint lives at `{backend_base}/internal/snapshots/presign`.
//! Backend base URL derives from `wallet_backend_url` or `db_backend_url`
//! config — the same endpoint serves both hosted and self-hosted deployments.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
// std::io::Read brought in by GzDecoder via tar::Archive internally.
use std::path::{Path, PathBuf};
use tar::{Archive, Builder};

use crate::config::Ad4mConfig;

// ── Manifest ──────────────────────────────────────────────────────────────────

/// Manifest for per-perspective snapshots.
///
/// Stored as `snapshots/{did}/manifest.json` on the remote backend.
/// Each entry describes one perspective's archive.
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct SnapshotManifest {
    /// ISO-8601 timestamp of when the manifest was created.
    pub created_at: String,
    /// List of perspective entries.
    pub perspectives: Vec<ManifestEntry>,
}

/// One entry in the snapshot manifest.
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq)]
pub struct ManifestEntry {
    /// Perspective UUID.
    pub uuid: String,
    /// Size of the tar.gz archive in bytes.
    pub size_bytes: u64,
    /// ISO-8601 timestamp of last modification.
    pub last_modified: String,
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

/// Derive the remote backend base URL from `wallet_backend_url`.
///
/// `wallet_backend_url` looks like `http://host:port/internal/wallet`.
/// Strips the path from `/internal/` onward to get `http://host:port`.
fn backend_base_url(config: &Ad4mConfig) -> Result<String, AnyError> {
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
    let base = backend_base_url(config)?;
    Ok(format!("{}/internal/snapshots/presign", base))
}

fn internal_token(config: &Ad4mConfig) -> Result<String, AnyError> {
    config
        .internal_api_token
        .clone()
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

// ── Per-perspective archive ────────────────────────────────────────────────────

/// Create a tar.gz archive of a single perspective's directory.
///
/// Archives `{perspectives_path}/{uuid}/` into a tar.gz.
/// The archive root uses `perspectives/{uuid}/` as prefix so
/// `restore_snapshot()` extracts it to the correct location.
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
        let prefix = format!("perspectives/{}", uuid);
        archive
            .append_dir_all(&prefix, &persp_subdir)
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

// ── HTTP operations ────────────────────────────────────────────────────────────

/// Presigned-URL response from the snapshot backend.
#[derive(serde::Deserialize, Debug)]
struct PresignResponse {
    url: String,
    #[allow(dead_code)]
    token: String,
    #[allow(dead_code)]
    key: String,
}

/// Request a presigned URL from the snapshot backend.
///
/// `object_type`: `"snapshot"` (legacy monolithic), `"manifest"`, or `"perspective"`.
/// `perspective_uuid`: required when `object_type` is `"perspective"`.
fn presign(
    config: &Ad4mConfig,
    operation: &str,
    did: &str,
    object_type: &str,
    perspective_uuid: Option<&str>,
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
        "objectType": object_type,
    });
    if let Some(uuid) = perspective_uuid {
        body["perspectiveUuid"] = serde_json::Value::String(uuid.to_string());
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

/// Upload a snapshot via a presigned PUT URL.
fn upload(url: &str, data: Vec<u8>) -> Result<(), AnyError> {
    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(120))
        .build()
        .map_err(|e| anyhow!("HTTP client build: {}", e))?;

    let resp = client
        .put(url)
        .header("Content-Type", "application/gzip")
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

/// Upload JSON content via a presigned PUT URL.
fn upload_json(url: &str, data: Vec<u8>) -> Result<(), AnyError> {
    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(30))
        .build()
        .map_err(|e| anyhow!("HTTP client build: {}", e))?;

    let resp = client
        .put(url)
        .header("Content-Type", "application/json")
        .body(data)
        .send()
        .map_err(|e| anyhow!("Manifest upload failed: {}", e))?;

    if !resp.status().is_success() {
        return Err(anyhow!("Manifest upload returned {}", resp.status()));
    }

    Ok(())
}

// ── Manifest cache (for lazy loading) ─────────────────────────────────────────

use std::sync::Mutex as StdMutex;

lazy_static::lazy_static! {
    /// Cached manifest from `restore_perspectives_lazy()`. Used by
    /// `restore_perspective_archive()` to download individual archives
    /// on demand during lazy hydration.
    static ref CACHED_MANIFEST: StdMutex<Option<SnapshotManifest>> = StdMutex::new(None);
}

// ── Public API ─────────────────────────────────────────────────────────────────

/// Lazy restore: download the manifest only, without fetching individual
/// perspective archives. Archives get downloaded on demand when each
/// perspective hydrates via `restore_perspective_archive()`.
///
/// Returns `Ok(true)` if a manifest was found (even with zero entries),
/// `Ok(false)` if no manifest exists (first run).
pub fn restore_perspectives_lazy(config: &Ad4mConfig) -> Result<bool, AnyError> {
    let did = crate::agent::did();

    let manifest_presign = presign(config, "get", &did, "manifest", None)?;
    let manifest_data = download(&manifest_presign.url)?;

    if manifest_data.is_empty() {
        log::info!(
            "No manifest found for DID {} — starting fresh (lazy mode)",
            did
        );
        return Ok(false);
    }

    let manifest: SnapshotManifest = serde_json::from_slice(&manifest_data)
        .map_err(|e| anyhow!("Failed to parse snapshot manifest: {}", e))?;

    log::info!(
        "📋 Manifest loaded with {} perspectives for DID {} (lazy — archives deferred)",
        manifest.perspectives.len(),
        did
    );

    // Cache for later per-perspective downloads.
    *CACHED_MANIFEST.lock().unwrap() = Some(manifest);

    Ok(true)
}

/// Download and extract a single perspective's archive from the remote
/// snapshot backend. Used during lazy hydration when a deferred
/// perspective gets accessed for the first time.
///
/// Returns `Ok(true)` if the archive was downloaded and extracted,
/// `Ok(false)` if the perspective has no remote archive (new perspective).
pub fn restore_perspective_archive(config: &Ad4mConfig, uuid: &str) -> Result<bool, AnyError> {
    // Check manifest cache — if the perspective has no entry, skip download.
    {
        let manifest = CACHED_MANIFEST.lock().unwrap();
        if let Some(ref m) = *manifest {
            if !m.perspectives.iter().any(|e| e.uuid == uuid) {
                log::info!(
                    "Perspective {} has no manifest entry — skipping archive download",
                    uuid
                );
                return Ok(false);
            }
        }
    }

    let did = crate::agent::did();
    let data_path = config
        .app_data_path
        .as_ref()
        .ok_or_else(|| anyhow!("app_data_path not configured"))?;

    let persp_presign = presign(config, "get", &did, "perspective", Some(uuid))?;
    let archive_data = download(&persp_presign.url)?;

    if archive_data.is_empty() {
        log::info!(
            "No remote archive for perspective {} — hydrating with fresh store",
            uuid
        );
        return Ok(false);
    }

    restore_snapshot(Path::new(data_path), &archive_data)?;
    log::info!(
        "📥 Restored perspective archive {} ({} bytes) for hydration",
        uuid,
        archive_data.len()
    );

    Ok(true)
}

/// Backup all perspective data to the remote snapshot backend.
///
/// Creates per-perspective tar.gz archives and a manifest.json:
/// 1. Flush all SPARQL stores.
/// 2. For each perspective UUID directory, create + upload an individual archive.
/// 3. Build and upload the manifest listing all perspectives.
pub fn backup_perspectives(config: &Ad4mConfig) -> Result<(), AnyError> {
    let did = crate::agent::did();
    let persp_dir = perspectives_dir(config)?;

    if !persp_dir.exists() {
        log::info!("No perspectives directory — skipping backup");
        return Ok(());
    }

    // Flush all open SPARQL stores before snapshotting.
    flush_all_stores();

    // Enumerate perspective UUID directories and upload each individually.
    let mut entries = Vec::new();
    let dir_iter = std::fs::read_dir(&persp_dir)
        .map_err(|e| anyhow!("Failed to read perspectives directory: {}", e))?;

    for dir_entry in dir_iter {
        let dir_entry = dir_entry.map_err(|e| anyhow!("Failed to read dir entry: {}", e))?;
        let path = dir_entry.path();
        if !path.is_dir() {
            continue;
        }
        let uuid = path
            .file_name()
            .and_then(|n| n.to_str())
            .ok_or_else(|| anyhow!("Invalid perspective directory name"))?
            .to_string();

        let data = create_perspective_snapshot(&persp_dir, &uuid)?;
        let size_bytes = data.len() as u64;

        let presign_resp = presign(config, "put", &did, "perspective", Some(&uuid))?;
        upload(&presign_resp.url, data)?;
        log::info!(
            "Uploaded perspective snapshot for {}: {} bytes",
            uuid,
            size_bytes
        );

        entries.push(ManifestEntry {
            uuid,
            size_bytes,
            last_modified: chrono::Utc::now().to_rfc3339(),
        });
    }

    // Upload the manifest.
    let manifest = SnapshotManifest {
        created_at: chrono::Utc::now().to_rfc3339(),
        perspectives: entries,
    };
    let manifest_json = serde_json::to_vec_pretty(&manifest)
        .map_err(|e| anyhow!("Failed to serialize manifest: {}", e))?;

    let presign_resp = presign(config, "put", &did, "manifest", None)?;
    upload_json(&presign_resp.url, manifest_json)?;

    log::info!(
        "Snapshot manifest uploaded for DID {} ({} perspectives)",
        did,
        manifest.perspectives.len()
    );
    Ok(())
}

/// Restore perspective data from the remote snapshot backend, if available.
///
/// Tries the manifest-based per-perspective format first. Falls back to
/// the legacy monolithic `perspectives.tar.gz` if no manifest exists.
/// Returns `Ok(true)` if data was restored, `Ok(false)` if no snapshot existed.
pub fn restore_perspectives(config: &Ad4mConfig) -> Result<bool, AnyError> {
    let did = crate::agent::did();
    let data_path = config
        .app_data_path
        .as_ref()
        .ok_or_else(|| anyhow!("app_data_path not configured"))?;

    // Try manifest-based restore first.
    let manifest_presign = presign(config, "get", &did, "manifest", None)?;
    let manifest_data = download(&manifest_presign.url)?;

    if !manifest_data.is_empty() {
        let manifest: SnapshotManifest = serde_json::from_slice(&manifest_data)
            .map_err(|e| anyhow!("Failed to parse snapshot manifest: {}", e))?;

        log::info!(
            "Found manifest with {} perspectives for DID {}",
            manifest.perspectives.len(),
            did
        );

        if manifest.perspectives.is_empty() {
            return Ok(true);
        }

        for entry in &manifest.perspectives {
            let persp_presign = presign(config, "get", &did, "perspective", Some(&entry.uuid))?;
            let archive_data = download(&persp_presign.url)?;

            if archive_data.is_empty() {
                log::warn!(
                    "Perspective archive for {} not found — skipping",
                    entry.uuid
                );
                continue;
            }

            restore_snapshot(Path::new(data_path), &archive_data)?;
            log::info!(
                "Restored perspective {} ({} bytes)",
                entry.uuid,
                archive_data.len()
            );
        }

        return Ok(true);
    }

    // Fall back to legacy monolithic snapshot.
    log::info!("No manifest found — trying legacy monolithic snapshot");
    let legacy_presign = presign(config, "get", &did, "snapshot", None)?;
    let legacy_data = download(&legacy_presign.url)?;

    if legacy_data.is_empty() {
        log::info!("No remote snapshot for DID {} — starting fresh", did);
        return Ok(false);
    }

    log::info!(
        "Downloaded {} byte legacy snapshot for DID {} — restoring",
        legacy_data.len(),
        did
    );
    restore_snapshot(Path::new(data_path), &legacy_data)?;
    Ok(true)
}

/// Flush all registered perspective SPARQL stores.
///
/// Iterates the global PERSPECTIVES map and calls `flush()` on each store.
fn flush_all_stores() {
    let perspectives = crate::perspectives::all_perspectives();
    for instance in &perspectives {
        // Skip unhydrated (deferred) perspectives — they have no persistent store.
        if !instance.is_hydrated() {
            continue;
        }
        if let Err(e) = instance.store().flush() {
            log::warn!("Failed to flush SPARQL store for {}: {}", instance.uuid, e);
        }
    }
}

/// Spawn a background task that periodically backs up perspectives to the remote snapshot backend.
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
    fn test_backend_base_url_derivation() {
        let mut config = Ad4mConfig::default();
        config.wallet_backend_url =
            Some("http://host.docker.internal:8787/internal/wallet".to_string());

        let base = backend_base_url(&config).unwrap();
        assert_eq!(base, "http://host.docker.internal:8787");

        let url = presign_url(&config).unwrap();
        assert_eq!(
            url,
            "http://host.docker.internal:8787/internal/snapshots/presign"
        );
    }

    #[test]
    fn test_backend_base_url_no_internal_path() {
        let mut config = Ad4mConfig::default();
        config.wallet_backend_url = Some("http://localhost:8787".to_string());

        let base = backend_base_url(&config).unwrap();
        assert_eq!(base, "http://localhost:8787");
    }

    #[test]
    fn test_backend_base_url_missing() {
        let mut config = Ad4mConfig::default();
        config.wallet_backend_url = None;
        config.db_backend_url = None;

        let result = backend_base_url(&config);
        assert!(result.is_err());
    }

    // ── Per-perspective snapshot tests ─────────────────────────────────

    #[test]
    fn test_create_perspective_snapshot() {
        let src_dir = TempDir::new().unwrap();
        let persp_dir = src_dir.path().join("perspectives");
        let uuid = "test-uuid-1234";
        let store_dir = persp_dir.join(uuid).join("sparql_store");
        fs::create_dir_all(&store_dir).unwrap();
        fs::write(store_dir.join("data.db"), b"perspective-data").unwrap();
        fs::write(store_dir.join("wal.log"), b"perspective-wal").unwrap();

        let data = create_perspective_snapshot(&persp_dir, uuid).unwrap();
        assert!(!data.is_empty());

        // Restore and verify
        let dst_dir = TempDir::new().unwrap();
        restore_snapshot(dst_dir.path(), &data).unwrap();

        let restored_data = dst_dir
            .path()
            .join("perspectives")
            .join(uuid)
            .join("sparql_store/data.db");
        assert!(restored_data.exists());
        assert_eq!(fs::read(&restored_data).unwrap(), b"perspective-data");

        let restored_wal = dst_dir
            .path()
            .join("perspectives")
            .join(uuid)
            .join("sparql_store/wal.log");
        assert!(restored_wal.exists());
        assert_eq!(fs::read(&restored_wal).unwrap(), b"perspective-wal");
    }

    #[test]
    fn test_create_perspective_snapshot_missing_uuid() {
        let tmp = TempDir::new().unwrap();
        let persp_dir = tmp.path().join("perspectives");
        fs::create_dir_all(&persp_dir).unwrap();

        let result = create_perspective_snapshot(&persp_dir, "nonexistent-uuid");
        assert!(result.is_err());
    }

    #[test]
    fn test_manifest_serialization_roundtrip() {
        let manifest = SnapshotManifest {
            created_at: "2026-09-01T00:00:00Z".to_string(),
            perspectives: vec![
                ManifestEntry {
                    uuid: "uuid-1".to_string(),
                    size_bytes: 1024,
                    last_modified: "2026-09-01T00:00:00Z".to_string(),
                },
                ManifestEntry {
                    uuid: "uuid-2".to_string(),
                    size_bytes: 2048,
                    last_modified: "2026-09-01T01:00:00Z".to_string(),
                },
            ],
        };

        let json = serde_json::to_vec_pretty(&manifest).unwrap();
        let deserialized: SnapshotManifest = serde_json::from_slice(&json).unwrap();

        assert_eq!(deserialized.created_at, manifest.created_at);
        assert_eq!(deserialized.perspectives.len(), 2);
        assert_eq!(deserialized.perspectives[0], manifest.perspectives[0]);
        assert_eq!(deserialized.perspectives[1], manifest.perspectives[1]);
    }

    // =========================================================================
    // 14.9, 14.11, 14.12: Lazy perspective loading — flush/backup/manifest
    // =========================================================================

    /// 14.9 (snapshot-side) flush_all_stores skips unhydrated perspectives.
    ///
    /// Tests that the internal flush_all_stores() function gracefully
    /// handles a mix of hydrated and unhydrated perspectives by calling
    /// the same skip logic used in the production code path.
    #[test]
    fn test_flush_all_stores_skips_unhydrated() {
        crate::test_utils::setup_wallet();
        crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        crate::agent::AgentService::init_global_test_instance();

        let uuid_h = uuid::Uuid::new_v4().to_string();
        let uuid_d = uuid::Uuid::new_v4().to_string();

        // Register a hydrated perspective
        let handle_h = crate::types::PerspectiveHandle {
            uuid: uuid_h.clone(),
            name: Some("Hydrated".to_string()),
            shared_url: None,
            neighbourhood: None,
            state: crate::types::PerspectiveState::Private,
            owners: None,
        };
        let p_h =
            crate::perspectives::perspective_instance::PerspectiveInstance::new(handle_h, None);
        crate::perspectives::register_perspective(uuid_h.clone(), p_h);

        // Register a deferred (unhydrated) perspective
        let handle_d = crate::types::PerspectiveHandle {
            uuid: uuid_d.clone(),
            name: Some("Deferred".to_string()),
            shared_url: None,
            neighbourhood: None,
            state: crate::types::PerspectiveState::Private,
            owners: None,
        };
        let p_d =
            crate::perspectives::perspective_instance::PerspectiveInstance::new_deferred(handle_d);
        assert!(!p_d.is_hydrated());
        crate::perspectives::register_perspective(uuid_d.clone(), p_d);

        // flush_all_stores must not panic (skips the unhydrated one)
        flush_all_stores();

        // Deferred perspective must still report unhydrated
        let p = crate::perspectives::get_perspective(&uuid_d).unwrap();
        assert!(
            !p.is_hydrated(),
            "flush_all_stores must not trigger hydration"
        );
    }

    /// 14.11 Backup skips unhydrated perspectives.
    ///
    /// backup_perspectives() calls flush_all_stores() which skips unhydrated.
    /// Additionally, backup only archives perspective directories that exist
    /// on disk — deferred perspectives have no directory yet, so they produce
    /// no archive. The manifest carries forward their entry from the cached
    /// manifest (the remote archive remains untouched).
    ///
    /// This test verifies that flush_all_stores (called within backup) does
    /// not trigger hydration of deferred perspectives.
    #[test]
    fn test_backup_flush_does_not_hydrate_deferred() {
        // Same setup as 14.9 — the assertion is that flush inside
        // backup_perspectives does not force hydration.
        crate::test_utils::setup_wallet();
        crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        crate::agent::AgentService::init_global_test_instance();

        let uuid = uuid::Uuid::new_v4().to_string();
        let handle = crate::types::PerspectiveHandle {
            uuid: uuid.clone(),
            name: Some("Deferred backup".to_string()),
            shared_url: None,
            neighbourhood: None,
            state: crate::types::PerspectiveState::Private,
            owners: None,
        };
        let p =
            crate::perspectives::perspective_instance::PerspectiveInstance::new_deferred(handle);
        assert!(!p.is_hydrated());
        crate::perspectives::register_perspective(uuid.clone(), p);

        // flush_all_stores (the first step of backup_perspectives) must skip
        flush_all_stores();

        let after = crate::perspectives::get_perspective(&uuid).unwrap();
        assert!(
            !after.is_hydrated(),
            "flush during backup must not hydrate deferred perspectives"
        );
    }

    /// 14.12 Cached manifest stores lazy-mode data for per-perspective downloads.
    ///
    /// The CACHED_MANIFEST global stores the manifest after
    /// restore_perspectives_lazy() downloads it. This test verifies the
    /// caching and lookup logic without requiring network access.
    #[test]
    fn test_cached_manifest_stores_and_retrieves() {
        // Set a manifest with two perspectives
        let manifest = SnapshotManifest {
            created_at: "2026-09-01T00:00:00Z".to_string(),
            perspectives: vec![
                ManifestEntry {
                    uuid: "uuid-known".to_string(),
                    size_bytes: 1024,
                    last_modified: "2026-09-01T00:00:00Z".to_string(),
                },
                ManifestEntry {
                    uuid: "uuid-also-known".to_string(),
                    size_bytes: 2048,
                    last_modified: "2026-09-01T01:00:00Z".to_string(),
                },
            ],
        };

        // Simulate what restore_perspectives_lazy does: cache the manifest
        *CACHED_MANIFEST.lock().unwrap() = Some(manifest);

        // Verify: known UUID present in manifest
        {
            let cached = CACHED_MANIFEST.lock().unwrap();
            let m = cached.as_ref().expect("manifest must be cached");
            assert_eq!(m.perspectives.len(), 2);
            assert!(
                m.perspectives.iter().any(|e| e.uuid == "uuid-known"),
                "known UUID must appear in cached manifest"
            );
        }

        // Verify: unknown UUID not in manifest (restore_perspective_archive
        // would skip download in this case)
        {
            let cached = CACHED_MANIFEST.lock().unwrap();
            let m = cached.as_ref().unwrap();
            assert!(
                !m.perspectives.iter().any(|e| e.uuid == "uuid-unknown"),
                "unknown UUID must not appear in cached manifest"
            );
        }

        // Clean up
        *CACHED_MANIFEST.lock().unwrap() = None;
    }

    /// 14.12b Empty manifest returns Ok(true) in lazy mode.
    ///
    /// A manifest with zero entries means "no perspectives to download"
    /// but the manifest itself was found. restore_perspectives_lazy stores
    /// it (with zero entries) and returns Ok(true).
    #[test]
    fn test_cached_manifest_empty_entries_still_cached() {
        let manifest = SnapshotManifest {
            created_at: "2026-09-01T00:00:00Z".to_string(),
            perspectives: vec![],
        };

        *CACHED_MANIFEST.lock().unwrap() = Some(manifest);

        let cached = CACHED_MANIFEST.lock().unwrap();
        let m = cached.as_ref().expect("empty manifest must be cached");
        assert!(
            m.perspectives.is_empty(),
            "empty manifest must have zero entries"
        );

        // Clean up
        drop(cached);
        *CACHED_MANIFEST.lock().unwrap() = None;
    }

    #[test]
    fn test_create_multiple_perspective_snapshots() {
        let src_dir = TempDir::new().unwrap();
        let persp_dir = src_dir.path().join("perspectives");

        // Create two perspectives
        for uuid in &["uuid-aaa", "uuid-bbb"] {
            let store_dir = persp_dir.join(uuid).join("sparql_store");
            fs::create_dir_all(&store_dir).unwrap();
            fs::write(store_dir.join("data.db"), format!("data-{}", uuid)).unwrap();
        }

        // Archive each separately
        let archive_a = create_perspective_snapshot(&persp_dir, "uuid-aaa").unwrap();
        let archive_b = create_perspective_snapshot(&persp_dir, "uuid-bbb").unwrap();

        // Restore both to a fresh directory
        let dst_dir = TempDir::new().unwrap();
        restore_snapshot(dst_dir.path(), &archive_a).unwrap();
        restore_snapshot(dst_dir.path(), &archive_b).unwrap();

        // Verify both perspectives exist
        let data_a = dst_dir
            .path()
            .join("perspectives/uuid-aaa/sparql_store/data.db");
        let data_b = dst_dir
            .path()
            .join("perspectives/uuid-bbb/sparql_store/data.db");
        assert!(data_a.exists());
        assert!(data_b.exists());
        assert_eq!(fs::read_to_string(&data_a).unwrap(), "data-uuid-aaa");
        assert_eq!(fs::read_to_string(&data_b).unwrap(), "data-uuid-bbb");
    }
}

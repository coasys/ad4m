//! Directory-level snapshots of the OxiGraph/RocksDB perspective store.
//!
//! Periodically archives `{data}/perspectives/` as a tar.gz and uploads it
//! to the platform backend via HMAC-presigned URLs. On startup in shared
//! mode, downloads and extracts the latest snapshot so OxiGraph opens with
//! the restored data.
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

/// Request a presigned URL from the platform Worker.
fn presign(config: &Ad4mConfig, operation: &str, did: &str) -> Result<PresignResponse, AnyError> {
    let url = presign_url(config)?;
    let token = internal_token(config)?;

    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(30))
        .build()
        .map_err(|e| anyhow!("HTTP client build: {}", e))?;

    let body = serde_json::json!({
        "operation": operation,
        "did": did,
    });

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

// ── Public API ─────────────────────────────────────────────────────────────────

/// Backup all perspective data to the platform backend.
///
/// 1. Flush all SPARQL stores (caller must ensure perspectives exist).
/// 2. Create tar.gz of the `perspectives/` directory.
/// 3. Presign a PUT URL from the platform backend.
/// 4. Upload the archive.
pub fn backup_perspectives(config: &Ad4mConfig) -> Result<(), AnyError> {
    let did = crate::agent::did();
    let persp_dir = perspectives_dir(config)?;

    if !persp_dir.exists() {
        log::info!("No perspectives directory — skipping backup");
        return Ok(());
    }

    // Flush all open SPARQL stores before snapshotting.
    flush_all_stores();

    let data = create_snapshot(&persp_dir)?;
    log::info!("Snapshot created: {} bytes", data.len());

    let presign_resp = presign(config, "put", &did)?;
    upload(&presign_resp.url, data)?;

    log::info!("Snapshot uploaded for DID {}", did);
    Ok(())
}

/// Restore perspective data from the platform backend, if available.
///
/// Called before perspective initialisation in shared mode.
/// Downloads the tar.gz and extracts to the data directory.
/// Returns Ok(true) if data was restored, Ok(false) if no snapshot existed.
pub fn restore_perspectives(config: &Ad4mConfig) -> Result<bool, AnyError> {
    let did = crate::agent::did();
    let data_path = config
        .app_data_path
        .as_ref()
        .ok_or_else(|| anyhow!("app_data_path not configured"))?;

    let presign_resp = presign(config, "get", &did)?;
    let data = download(&presign_resp.url)?;

    if data.is_empty() {
        log::info!("No remote snapshot for DID {} — starting fresh", did);
        return Ok(false);
    }

    log::info!(
        "Downloaded {} byte snapshot for DID {} — restoring",
        data.len(),
        did
    );
    restore_snapshot(Path::new(data_path), &data)?;
    Ok(true)
}

/// Flush all registered perspective SPARQL stores.
///
/// Iterates the global PERSPECTIVES map and calls `flush()` on each store.
fn flush_all_stores() {
    let perspectives = crate::perspectives::all_perspectives();
    for instance in &perspectives {
        if let Err(e) = instance.sparql_store.flush() {
            log::warn!("Failed to flush SPARQL store for {}: {}", instance.uuid, e);
        }
    }
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

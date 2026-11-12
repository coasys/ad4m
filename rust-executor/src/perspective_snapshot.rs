//! Directory-level snapshots of the OxiGraph/RocksDB perspective store.
//!
//! Periodically checkpoints every perspective's SPARQL store, archives the
//! checkpoints as a tar.gz, seals the archive under this executor's keyring
//! (`at_rest::archive`: the checkpoints are plain, so the archive never leaves
//! unsealed) and uploads it to the platform backend via HMAC-presigned URLs.
//! On startup in shared mode, downloads, opens and extracts the latest
//! snapshot so OxiGraph opens with the restored data. A snapshot uploaded
//! before snapshots were sealed (a plain tar.gz) is restored once and replaced
//! by a sealed one; after this executor has uploaded a sealed snapshot, a
//! plain one is refused.
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

use crate::at_rest::archive;
use crate::at_rest::data_keys::DataKeys;
use crate::config::Ad4mConfig;
use crate::perspectives::sparql_store::SparqlStore;

/// The first bytes of a gzip stream: a plain snapshot from before snapshots were sealed.
const GZIP_MAGIC: [u8; 2] = [0x1f, 0x8b];

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
        .header("Content-Type", "application/octet-stream")
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

/// The stored perspectives that have no open store in `stores`.
fn missing_stores(
    stored: &[crate::types::PerspectiveHandle],
    stores: &[(String, std::sync::Arc<SparqlStore>)],
) -> Vec<String> {
    stored
        .iter()
        .filter(|h| !stores.iter().any(|(uuid, _)| *uuid == h.uuid))
        .map(|h| h.uuid.clone())
        .collect()
}

/// Backup all perspective data to the platform backend.
///
/// 1. Checkpoint every open SPARQL store into a staging directory next to
///    `perspectives/` (same filesystem, so the checkpoints hard-link).
/// 2. Create tar.gz of the checkpoints, then remove the staging directory.
/// 3. Seal the archive under the keyring; refuse while the keyring is sealed.
/// 4. Presign a PUT URL from the platform backend and upload the sealed archive.
pub fn backup_perspectives(config: &Ad4mConfig) -> Result<(), AnyError> {
    let did = crate::agent::did();
    let persp_dir = perspectives_dir(config)?;

    if !persp_dir.exists() {
        log::info!("No perspectives directory — skipping backup");
        return Ok(());
    }
    let keys = crate::at_rest::agent_passphrase::keys()
        .ok_or_else(|| anyhow!("Skipping snapshot: the storage is sealed"))?;

    let stores: Vec<_> = crate::perspectives::all_perspectives()
        .into_iter()
        .map(|p| (p.uuid.clone(), p.sparql_store.clone()))
        .collect();
    // Start-up opens each stored perspective in its own task. An archive taken
    // before all are open would replace the last full snapshot with a partial
    // one, so refuse until every stored perspective has its store.
    let stored = crate::db_backend::db_backend()
        .get_all_perspectives()
        .map_err(|e| anyhow!("Failed to list perspectives: {}", e))?;
    let loading = missing_stores(&stored, &stores);
    if !loading.is_empty() {
        return Err(anyhow!(
            "Skipping snapshot: {} perspective(s) still loading: {}",
            loading.len(),
            loading.join(", ")
        ));
    }
    upload_sealed_snapshot(config, &did, &keys, &stores, &persp_dir)?;
    log::info!("Snapshot uploaded for DID {}", did);
    Ok(())
}

/// What a restore found.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Restored {
    /// No remote snapshot.
    Nothing,
    /// A sealed snapshot, opened and extracted.
    Sealed,
    /// A plain snapshot uploaded before snapshots were sealed, extracted: the next
    /// snapshot must be uploaded sealed soon, so the plain one does not stay.
    Plain,
}

/// Restore perspective data from the platform backend, if available.
///
/// Called before perspective initialisation in shared mode, once the keyring
/// is open. Downloads the snapshot, opens it under the keyring and extracts it
/// to the data directory.
pub fn restore_perspectives(config: &Ad4mConfig) -> Result<Restored, AnyError> {
    let did = crate::agent::did();
    let data_path = config
        .app_data_path
        .as_ref()
        .ok_or_else(|| anyhow!("app_data_path not configured"))?;
    let keys = crate::at_rest::agent_passphrase::keys()
        .ok_or_else(|| anyhow!("the storage is sealed: a snapshot cannot be opened"))?;
    download_and_restore(config, &did, &keys, Path::new(data_path))
}

/// Checkpoints `stores`, archives the checkpoints, seals the archive under `keys` and uploads
/// it for `did`. The staging directory is made next to `persp_dir` and removed again. After
/// the upload, a plain snapshot is no longer restored on this executor.
fn upload_sealed_snapshot(
    config: &Ad4mConfig,
    did: &str,
    keys: &DataKeys,
    stores: &[(String, std::sync::Arc<SparqlStore>)],
    persp_dir: &Path,
) -> Result<(), AnyError> {
    // A unique name, so a shutdown backup and a periodic one cannot collide.
    let staging = persp_dir
        .parent()
        .unwrap_or(persp_dir)
        .join(format!(".snapshot-{}", uuid::Uuid::new_v4()));
    let data = checkpoint_stores(stores, &staging).and_then(|dir| create_snapshot(&dir));
    if let Err(e) = std::fs::remove_dir_all(&staging) {
        log::warn!("Failed to remove snapshot staging {:?}: {}", staging, e);
    }
    let sealed = archive::seal_archive(keys, &data?)
        .map_err(|e| anyhow!("Failed to seal the snapshot: {}", e))?;
    log::info!("Snapshot sealed: {} bytes", sealed.len());

    let presign_resp = presign(config, "put", did)?;
    upload(&presign_resp.url, sealed)?;
    if let Err(e) = archive::mark_sealed_only(keys) {
        log::warn!("Failed to record that snapshots are sealed: {}", e);
    }
    Ok(())
}

/// Downloads the snapshot of `did` and extracts it into `data_path`: a sealed one opened
/// under `keys`; a plain one only while this executor has not uploaded a sealed one. Nothing
/// is extracted from a snapshot that does not open.
fn download_and_restore(
    config: &Ad4mConfig,
    did: &str,
    keys: &DataKeys,
    data_path: &Path,
) -> Result<Restored, AnyError> {
    let presign_resp = presign(config, "get", did)?;
    let data = download(&presign_resp.url)?;

    if data.is_empty() {
        log::info!("No remote snapshot for DID {} — starting fresh", did);
        return Ok(Restored::Nothing);
    }
    log::info!(
        "Downloaded {} byte snapshot for DID {} — restoring",
        data.len(),
        did
    );

    let (plain, restored) = if archive::is_sealed_archive(&data) {
        let plain = archive::open_archive(keys, &data)
            .map_err(|e| anyhow!("The snapshot does not open: {}", e))?;
        (plain, Restored::Sealed)
    } else if data.starts_with(&GZIP_MAGIC) {
        if archive::is_sealed_only(keys) {
            return Err(anyhow!(
                "Refusing a plain snapshot: this executor has uploaded sealed snapshots"
            ));
        }
        log::warn!(
            "The remote snapshot is a plain archive from before snapshots were sealed: \
             restoring it once; a sealed snapshot replaces it"
        );
        (data, Restored::Plain)
    } else {
        return Err(anyhow!("The remote snapshot is not a snapshot archive"));
    };
    restore_snapshot(data_path, &plain)?;
    Ok(restored)
}

/// Spawn a background task that periodically backs up perspectives to the platform backend.
///
/// Runs every `snapshot_interval_secs` (default 300 = 5 minutes). With `reseal_first` (the
/// start restored a plain snapshot) it first uploads a sealed snapshot as soon as every
/// perspective is open, even with the periodic snapshots off, so the plain one is replaced.
/// Logs errors but does not propagate — backup failure must not crash the executor.
pub fn spawn_periodic_backup(config: Ad4mConfig, reseal_first: bool) {
    let interval_secs = config.snapshot_interval_secs.unwrap_or(300);
    if interval_secs == 0 {
        log::info!("Periodic perspective snapshots disabled (interval = 0)");
        if !reseal_first {
            return;
        }
    } else {
        log::info!(
            "Starting periodic perspective snapshot every {}s",
            interval_secs
        );
    }

    tokio::task::spawn(async move {
        if reseal_first {
            reseal(&config).await;
        }
        if interval_secs == 0 {
            return;
        }
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

/// How often, and how many times, the start tries to replace a restored plain snapshot.
const RESEAL_RETRY: std::time::Duration = std::time::Duration::from_secs(5);
const RESEAL_ATTEMPTS: u32 = 120;

/// Uploads a sealed snapshot in place of the plain one the start restored: retried while
/// perspectives are still loading or the upload fails, for up to ten minutes.
async fn reseal(config: &Ad4mConfig) {
    for attempt in 1..=RESEAL_ATTEMPTS {
        tokio::time::sleep(RESEAL_RETRY).await;
        let config_clone = config.clone();
        match tokio::task::spawn_blocking(move || backup_perspectives(&config_clone)).await {
            Ok(Ok(())) => {
                log::info!("The plain snapshot is replaced by a sealed one");
                return;
            }
            Ok(Err(e)) if attempt == RESEAL_ATTEMPTS => {
                log::error!("The plain snapshot is still the remote one: {}", e)
            }
            Ok(Err(e)) => log::debug!("Sealed snapshot not uploaded yet: {}", e),
            Err(e) => log::warn!("Sealed snapshot task panic: {}", e),
        }
    }
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

    /// A stored perspective whose store is not open yet blocks the backup.
    #[test]
    fn a_stored_perspective_without_an_open_store_is_missing() {
        use crate::types::PerspectiveHandle;
        use std::sync::Arc;

        let handle = |uuid: &str| PerspectiveHandle {
            uuid: uuid.to_string(),
            ..Default::default()
        };
        let open = vec![(
            "uuid-a".to_string(),
            Arc::new(SparqlStore::new(None).unwrap()),
        )];

        assert!(missing_stores(&[handle("uuid-a")], &open).is_empty());
        assert_eq!(
            missing_stores(&[handle("uuid-a"), handle("uuid-b")], &open),
            vec!["uuid-b".to_string()]
        );
    }

    /// A local stand-in for the platform's presign endpoint and object store: presigns one
    /// object URL, keeps what is PUT there and serves it to GET (404 before any PUT).
    struct PresignStub {
        base: String,
        object: std::sync::Arc<std::sync::Mutex<Option<Vec<u8>>>>,
    }

    impl PresignStub {
        fn start() -> Self {
            use std::io::{BufRead, BufReader, Read, Write};
            let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
            let base = format!("http://{}", listener.local_addr().unwrap());
            let object = std::sync::Arc::new(std::sync::Mutex::new(None::<Vec<u8>>));
            let (stored, object_url) = (object.clone(), format!("{base}/object"));
            std::thread::spawn(move || {
                for stream in listener.incoming() {
                    let Ok(mut stream) = stream else { continue };
                    let mut reader = BufReader::new(stream.try_clone().unwrap());
                    let mut request_line = String::new();
                    reader.read_line(&mut request_line).unwrap();
                    let mut length = 0usize;
                    let mut authorised = false;
                    loop {
                        let mut line = String::new();
                        reader.read_line(&mut line).unwrap();
                        let line = line.trim_end();
                        if line.is_empty() {
                            break;
                        }
                        let lower = line.to_ascii_lowercase();
                        if let Some(v) = lower.strip_prefix("content-length:") {
                            length = v.trim().parse().unwrap();
                        }
                        if lower == "authorization: bearer internal-token" {
                            authorised = true;
                        }
                    }
                    let mut body = vec![0u8; length];
                    reader.read_exact(&mut body).unwrap();
                    let mut parts = request_line.split_whitespace();
                    let (method, path) = (parts.next().unwrap(), parts.next().unwrap());
                    let (status, reply): (&str, Vec<u8>) = match (method, path) {
                        ("POST", "/internal/snapshots/presign") if authorised => (
                            "200 OK",
                            serde_json::json!({ "url": object_url, "token": "t", "key": "k" })
                                .to_string()
                                .into_bytes(),
                        ),
                        ("POST", _) => ("401 Unauthorized", Vec::new()),
                        ("PUT", "/object") => {
                            *stored.lock().unwrap() = Some(body);
                            ("200 OK", Vec::new())
                        }
                        ("GET", "/object") => match stored.lock().unwrap().clone() {
                            Some(bytes) => ("200 OK", bytes),
                            None => ("404 Not Found", Vec::new()),
                        },
                        _ => ("404 Not Found", Vec::new()),
                    };
                    let head = format!(
                        "HTTP/1.1 {status}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                        reply.len()
                    );
                    let _ = stream.write_all(head.as_bytes());
                    let _ = stream.write_all(&reply);
                }
            });
            PresignStub { base, object }
        }

        fn config(&self) -> Ad4mConfig {
            Ad4mConfig {
                wallet_backend_url: Some(format!("{}/internal/wallet", self.base)),
                internal_api_token: Some(crate::secret::Secret::from("internal-token")),
                ..Default::default()
            }
        }

        fn uploaded(&self) -> Vec<u8> {
            self.object
                .lock()
                .unwrap()
                .clone()
                .expect("an uploaded object")
        }

        fn put(&self, bytes: Vec<u8>) {
            *self.object.lock().unwrap() = Some(bytes);
        }
    }

    const DID: &str = "did:key:z6MkSnapshotTest";
    const MARKERS: [&str; 2] = ["marker-link-source-4b1d", "marker-link-target-9e7c"];

    fn open_keys(data: &Path) -> std::sync::Arc<DataKeys> {
        let keys = DataKeys::load(crate::at_rest::keyring_file::path_in(data)).unwrap();
        keys.unlock(zeroize::Zeroizing::new([9u8; 32])).unwrap();
        std::sync::Arc::new(keys)
    }

    /// Two perspectives under `data/perspectives`, their links carrying [`MARKERS`].
    fn stores_with_markers(data: &Path) -> Vec<(String, std::sync::Arc<SparqlStore>)> {
        use crate::agent::signatures::TestSigner;
        use crate::types::{Link, LinkExpression, LinkStatus};

        let signer = TestSigner::generate();
        let mut stores = Vec::new();
        for (uuid, count) in [("uuid-a", 3), ("uuid-b", 4)] {
            let dir = data.join("perspectives").join(uuid);
            let store = std::sync::Arc::new(SparqlStore::new(Some(dir.to_str().unwrap())).unwrap());
            for i in 0..count {
                let signed = signer.sign(
                    Link {
                        source: format!("literal://string:{}-{i}", MARKERS[0]),
                        predicate: Some("ad4m://p".to_string()),
                        target: format!("literal://string:{}-{i}", MARKERS[1]),
                    }
                    .normalize(),
                );
                store
                    .add_link(&LinkExpression {
                        author: signed.author,
                        timestamp: signed.timestamp,
                        data: signed.data,
                        proof: signed.proof,
                        status: Some(LinkStatus::Shared),
                    })
                    .unwrap();
            }
            stores.push((uuid.to_string(), store));
        }
        stores
    }

    fn contains(haystack: &[u8], needle: &[u8]) -> bool {
        haystack.windows(needle.len()).any(|w| w == needle)
    }

    fn tar_names(tar_gz: &[u8]) -> Vec<String> {
        Archive::new(GzDecoder::new(tar_gz))
            .entries()
            .unwrap()
            .map(|e| e.unwrap().path().unwrap().to_string_lossy().into_owned())
            .collect()
    }

    /// The uploaded snapshot is sealed as a whole: it starts with the archive magic, no tar
    /// header, file name or gzip stream in it is readable, and it holds none of the link
    /// markers the (plain) checkpoints hold.
    #[test]
    fn the_uploaded_snapshot_is_sealed_and_holds_no_link() {
        let stub = PresignStub::start();
        let data = TempDir::new().unwrap();
        let keys = open_keys(data.path());
        let stores = stores_with_markers(data.path());
        let persp_dir = data.path().join("perspectives");

        upload_sealed_snapshot(&stub.config(), DID, &keys, &stores, &persp_dir).unwrap();
        let object = stub.uploaded();

        assert!(object.starts_with(b"AD4MARCH"));
        assert!(!object.starts_with(&GZIP_MAGIC));
        for readable in [&b"ustar"[..], b"perspectives", b"sparql_store", b"uuid-a"] {
            assert!(!contains(&object, readable), "{:?}", readable);
        }
        for marker in MARKERS {
            assert!(!contains(&object, marker.as_bytes()), "{marker}");
        }
        assert!(Archive::new(GzDecoder::new(&object[..]))
            .entries()
            .and_then(|mut e| e.next().transpose())
            .is_err());
        // The staging directory is gone.
        let left: Vec<_> = fs::read_dir(data.path())
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .filter(|n| n.starts_with(".snapshot-"))
            .collect();
        assert!(left.is_empty(), "{left:?}");

        // Opened, it is the archive of the checkpoints: the seal is what kept them unread.
        let plain = archive::open_archive(&keys, &object).unwrap();
        assert!(tar_names(&plain)
            .iter()
            .any(|n| n.starts_with("perspectives/uuid-a/sparql_store")));
    }

    /// A sealed snapshot restores under the keyring that sealed it, with every link.
    #[test]
    fn a_sealed_snapshot_restores_its_stores() {
        let stub = PresignStub::start();
        let live = TempDir::new().unwrap();
        let keys = open_keys(live.path());
        let stores = stores_with_markers(live.path());
        let config = stub.config();
        upload_sealed_snapshot(
            &config,
            DID,
            &keys,
            &stores,
            &live.path().join("perspectives"),
        )
        .unwrap();

        let restored = TempDir::new().unwrap();
        assert_eq!(
            download_and_restore(&config, DID, &keys, restored.path()).unwrap(),
            Restored::Sealed
        );
        for (uuid, count) in [("uuid-a", 3), ("uuid-b", 4)] {
            let dir = restored.path().join("perspectives").join(uuid);
            let store = SparqlStore::new(Some(dir.to_str().unwrap())).unwrap();
            assert_eq!(store.get_all_links().unwrap().len(), count, "{uuid}");
        }
    }

    /// Another keyring cannot open the snapshot, and nothing is extracted.
    #[test]
    fn a_snapshot_of_another_keyring_restores_nothing() {
        let stub = PresignStub::start();
        let live = TempDir::new().unwrap();
        let keys = open_keys(live.path());
        let stores = stores_with_markers(live.path());
        let config = stub.config();
        upload_sealed_snapshot(
            &config,
            DID,
            &keys,
            &stores,
            &live.path().join("perspectives"),
        )
        .unwrap();

        let elsewhere = TempDir::new().unwrap();
        let other = open_keys(elsewhere.path());
        let err = download_and_restore(&config, DID, &other, elsewhere.path()).unwrap_err();
        assert!(err.to_string().contains("does not open"), "{err}");
        assert!(!elsewhere.path().join("perspectives").exists());
    }

    /// A plain snapshot from before snapshots were sealed restores once: after this executor
    /// uploaded a sealed one, a plain one is refused.
    #[test]
    fn a_plain_snapshot_restores_until_a_sealed_one_was_uploaded() {
        let stub = PresignStub::start();
        let config = stub.config();
        let old = TempDir::new().unwrap();
        let stores = stores_with_markers(old.path());
        let staging = old.path().join(".snapshot-old");
        let plain = create_snapshot(&checkpoint_stores(&stores, &staging).unwrap()).unwrap();
        stub.put(plain.clone());

        let data = TempDir::new().unwrap();
        let keys = open_keys(data.path());
        assert_eq!(
            download_and_restore(&config, DID, &keys, data.path()).unwrap(),
            Restored::Plain
        );
        let dir = data.path().join("perspectives/uuid-b");
        let store = std::sync::Arc::new(SparqlStore::new(Some(dir.to_str().unwrap())).unwrap());
        assert_eq!(store.get_all_links().unwrap().len(), 4);

        // The reseal: the sealed snapshot replaces the plain one.
        upload_sealed_snapshot(
            &config,
            DID,
            &keys,
            &[("uuid-b".to_string(), store)],
            &data.path().join("perspectives"),
        )
        .unwrap();
        assert!(stub.uploaded().starts_with(b"AD4MARCH"));

        // A plain snapshot put back in its place is refused, and nothing is extracted.
        stub.put(plain);
        let again = TempDir::new().unwrap();
        let err = download_and_restore(&config, DID, &keys, again.path()).unwrap_err();
        assert!(
            err.to_string().contains("Refusing a plain snapshot"),
            "{err}"
        );
        assert!(!again.path().join("perspectives").exists());
    }

    #[test]
    fn no_remote_snapshot_restores_nothing_and_junk_is_refused() {
        let stub = PresignStub::start();
        let config = stub.config();
        let data = TempDir::new().unwrap();
        let keys = open_keys(data.path());
        assert_eq!(
            download_and_restore(&config, DID, &keys, data.path()).unwrap(),
            Restored::Nothing
        );
        stub.put(b"not a snapshot".to_vec());
        assert!(download_and_restore(&config, DID, &keys, data.path()).is_err());
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

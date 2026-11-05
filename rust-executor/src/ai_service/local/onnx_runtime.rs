//! ONNX Runtime for the speech gate (Silero VAD, `transcription::SpeechGate`) where `ort`
//! loads it at run time instead of linking it in: macOS, whose static ONNX Runtime is out
//! of ARM64 branch range (`Cargo.toml`, `load-dynamic`). Elsewhere it is linked in and
//! [`ready`] answers yes at once.
//!
//! On macOS the pinned build (`libraries.onnxruntime` in `manifest.json`, Microsoft's
//! release for the ONNX Runtime minor `ort` binds, 1.18) is fetched into the local AI store
//! like a runtime build, verified by size and sha256, and `ort` is pointed at its library
//! before the first model is built. `ort` keeps the first library path it resolves for the
//! life of the process, so nothing touches `ort` before the file is there: a stream that
//! opens while the library downloads runs without the gate (with a warning) and starts the
//! download in the background; the next stream has the gate. `ORT_DYLIB_PATH`, or a
//! `libonnxruntime.dylib` beside the executable, still win: `ort` resolves those itself.

use anyhow::{anyhow, bail, Result};
use std::path::PathBuf;
use std::sync::{Mutex, OnceLock};
use std::thread::JoinHandle;

use super::data::Library;
use super::install::{http_client, Store};

/// The manifest's and the store's name for it.
const LIBRARY: &str = "onnxruntime";

/// This platform's build key where `ort` loads ONNX Runtime at run time; `None` where it is
/// linked in.
pub fn build_key() -> Option<&'static str> {
    if cfg!(all(target_os = "macos", target_arch = "aarch64")) {
        Some("macos-arm64")
    } else if cfg!(all(target_os = "macos", target_arch = "x86_64")) {
        Some("macos-x64")
    } else {
        None
    }
}

/// The library file of build `key` when the store holds it, verified at its unpack.
pub fn installed(store: &Store, library: &Library, key: &str) -> Option<PathBuf> {
    let build = library.builds.get(key)?;
    if !store.build_installed(LIBRARY, &library.version, key, build) {
        return None;
    }
    let path = store
        .runtime_dir(LIBRARY, &library.version, key)
        .join(&build.binary);
    path.is_file().then_some(path)
}

/// Download and unpack build `key` into the store (resuming), unless it is there; the
/// library file.
pub async fn fetch(
    store: &Store,
    client: &reqwest::Client,
    library: &Library,
    key: &str,
) -> Result<PathBuf> {
    let build = library
        .builds
        .get(key)
        .ok_or_else(|| anyhow!("The manifest has no {LIBRARY} build for {key}"))?;
    let unpacked = store
        .fetch_build(client, LIBRARY, &library.version, key, build, &|_| {})
        .await?;
    let path = unpacked.dir.join(&build.binary);
    if !path.is_file() {
        bail!("{} has no {}", build.url, build.binary);
    }
    Ok(path)
}

/// The background download of the library: at most one at a time.
#[derive(Default)]
pub struct Fetcher {
    running: Mutex<Option<JoinHandle<()>>>,
}

impl Fetcher {
    pub const fn new() -> Fetcher {
        Fetcher {
            running: Mutex::new(None),
        }
    }

    /// The library file when the store holds it. Else its download starts on a thread of
    /// its own (unless one is running), which outlives the stream that asked, and the
    /// answer is `None`; a download that failed is tried again by the next call.
    pub fn installed_or_fetch(
        &self,
        store: &Store,
        library: &Library,
        key: &str,
    ) -> Option<PathBuf> {
        if let Some(path) = installed(store, library, key) {
            return Some(path);
        }
        let mut running = self.running.lock().unwrap_or_else(|e| e.into_inner());
        if running.as_ref().is_some_and(|h| !h.is_finished()) {
            return None;
        }
        let (store, library, key) = (store.clone(), library.clone(), key.to_string());
        log::info!(
            "Downloading ONNX Runtime {} for the speech gate into {}",
            library.version,
            store.root.display()
        );
        *running = Some(std::thread::spawn(move || {
            let fetched = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .map_err(anyhow::Error::from)
                .and_then(|rt| {
                    rt.block_on(async { fetch(&store, &http_client()?, &library, &key).await })
                });
            match fetched {
                Ok(path) => log::info!(
                    "ONNX Runtime is at {}: streams opened from now on have the speech gate",
                    path.display()
                ),
                Err(e) => log::warn!(
                    "ONNX Runtime did not download ({e:#}): streams run without the speech \
                     gate; the next one tries again"
                ),
            }
        }));
        None
    }

    /// Wait for the running download, if there is one.
    pub fn wait(&self) {
        let handle = self
            .running
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .take();
        if let Some(handle) = handle {
            let _ = handle.join();
        }
    }
}

static FETCHER: Fetcher = Fetcher::new();

/// Whether `ort` was pointed at the store's library, and it loaded.
static POINTED: OnceLock<bool> = OnceLock::new();

/// Whether ONNX Runtime can be loaded now. Where it is linked in, or where `ORT_DYLIB_PATH`
/// or a library beside the executable provides it: yes. Else (macOS) yes once the store
/// holds the pinned build and `ort` loaded it; without the build, its download starts in
/// the background and the answer is no.
pub fn ready() -> bool {
    let Some(key) = build_key() else {
        return true;
    };
    if let Some(loaded) = POINTED.get() {
        return *loaded;
    }
    if provided_outside_the_store() {
        return true;
    }
    let library = match super::data::embedded() {
        Ok(data) => match data.manifest.libraries.get(LIBRARY) {
            Some(library) => library,
            None => {
                log::warn!("The manifest pins no ONNX Runtime: no speech gate");
                return false;
            }
        },
        Err(e) => {
            log::warn!("{e}: no speech gate");
            return false;
        }
    };
    let configured = crate::config::GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .as_ref()
        .and_then(|c| c.local_ai_dir.clone());
    let store = Store::new(super::store_root(configured));
    match FETCHER.installed_or_fetch(&store, library, key) {
        Some(path) => *POINTED.get_or_init(|| point_ort_at(path)),
        None => false,
    }
}

/// Wait for the library's background download, if one is running.
pub fn wait_for_download() {
    FETCHER.wait();
}

/// `ORT_DYLIB_PATH` names a library, or one lies beside the executable: `ort` loads that.
fn provided_outside_the_store() -> bool {
    std::env::var_os("ORT_DYLIB_PATH").is_some_and(|p| !p.is_empty())
        || std::env::current_exe()
            .ok()
            .and_then(|exe| exe.parent().map(|dir| dir.join("libonnxruntime.dylib")))
            .is_some_and(|lib| lib.exists())
}

/// Point `ort` at `path` and load it. `ort` panics, rather than returning an error, on a
/// library it cannot load.
#[cfg(target_os = "macos")]
fn point_ort_at(path: PathBuf) -> bool {
    let shown = path.display().to_string();
    match std::panic::catch_unwind(move || {
        ort::init_from(path.to_string_lossy().into_owned()).commit()
    }) {
        Ok(Ok(())) => {
            log::info!("ONNX Runtime loaded from {shown}");
            true
        }
        Ok(Err(e)) => {
            log::warn!("ONNX Runtime at {shown} did not start ({e}): no speech gate");
            false
        }
        Err(_) => {
            log::warn!("ONNX Runtime at {shown} did not load: no speech gate");
            false
        }
    }
}

/// Only macOS loads it at run time ([`build_key`]).
#[cfg(not(target_os = "macos"))]
fn point_ort_at(_path: PathBuf) -> bool {
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use sha2::{Digest, Sha256};

    fn sha(bytes: &[u8]) -> String {
        Sha256::digest(bytes)
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect()
    }

    fn tar_gz(path: &str, data: &[u8]) -> Vec<u8> {
        let mut builder = tar::Builder::new(flate2::write::GzEncoder::new(
            Vec::new(),
            flate2::Compression::fast(),
        ));
        let mut header = tar::Header::new_gnu();
        header.set_size(data.len() as u64);
        header.set_mode(0o755);
        header.set_cksum();
        builder.append_data(&mut header, path, data).unwrap();
        builder.into_inner().unwrap().finish().unwrap()
    }

    fn library(url: String, archive: &[u8], sha256: &str) -> Library {
        serde_json::from_value(serde_json::json!({
            "version": "1.18.1",
            "builds": { "macos-arm64": {
                "url": url, "sha256": sha256, "sizeBytes": archive.len(),
                "binary": "ort/lib/libonnxruntime.1.18.1.dylib"
            } }
        }))
        .unwrap()
    }

    /// The manifest pins a build for each macOS architecture, of the ONNX Runtime minor
    /// `ort` binds (rc.4: API 18; an older minor panics at load), each a library file.
    #[test]
    fn the_manifest_pins_onnx_runtime_1_18_for_both_macs() {
        let manifest = &super::super::data::embedded().unwrap().manifest;
        let library = &manifest.libraries[LIBRARY];
        assert!(library.version.starts_with("1.18."), "{}", library.version);
        #[cfg(target_os = "macos")]
        assert_eq!(
            library.version.split('.').nth(1).unwrap(),
            ort::MINOR_VERSION.to_string()
        );
        let keys: Vec<&str> = library.builds.keys().map(String::as_str).collect();
        assert_eq!(keys, vec!["macos-arm64", "macos-x64"]);
        for build in library.builds.values() {
            assert!(build.url.as_str().starts_with(
                "https://github.com/microsoft/onnxruntime/releases/download/v1.18.1/"
            ));
            assert!(build.binary.ends_with("/lib/libonnxruntime.1.18.1.dylib"));
            assert_eq!(build.sha256.len(), 64);
        }
        // The planner reads runtimes, not libraries.
        assert!(!manifest.runtimes.contains_key(LIBRARY));
    }

    #[test]
    fn only_macos_loads_it_at_run_time() {
        assert_eq!(build_key().is_some(), cfg!(target_os = "macos"));
        if build_key().is_none() {
            assert!(ready(), "linked in: always ready");
        }
    }

    /// The first ask starts one download and answers no; an ask while it runs starts no
    /// second; once it is done the store holds the verified library and the ask answers it.
    #[test]
    fn the_first_ask_downloads_in_the_background_and_a_later_one_finds_the_library() {
        let archive = tar_gz("ort/lib/libonnxruntime.1.18.1.dylib", b"a dylib");
        let mut server = mockito::Server::new();
        let mock = server
            .mock("GET", "/ort.tgz")
            .with_body(&archive)
            .expect(1)
            .create();
        let library = library(
            format!("{}/ort.tgz", server.url()),
            &archive,
            &sha(&archive),
        );
        let dir = tempfile::tempdir().unwrap();
        let store = Store::new(dir.path());
        let fetcher = Fetcher::new();

        assert_eq!(installed(&store, &library, "macos-arm64"), None);
        assert_eq!(
            fetcher.installed_or_fetch(&store, &library, "macos-arm64"),
            None
        );
        // Still downloading (no), or done (the library): either way no second request.
        let meanwhile = fetcher.installed_or_fetch(&store, &library, "macos-arm64");
        fetcher.wait();
        if let Some(path) = meanwhile {
            assert_eq!(Some(path), installed(&store, &library, "macos-arm64"));
        }
        let path = fetcher
            .installed_or_fetch(&store, &library, "macos-arm64")
            .expect("installed after the download");
        assert_eq!(
            path,
            store
                .runtime_dir(LIBRARY, "1.18.1", "macos-arm64")
                .join("ort/lib/libonnxruntime.1.18.1.dylib")
        );
        assert_eq!(std::fs::read(&path).unwrap(), b"a dylib");
        mock.assert();
    }

    /// A download whose digest is not the manifest's installs nothing; the next ask tries
    /// again.
    #[test]
    fn a_wrong_digest_installs_nothing_and_the_next_ask_tries_again() {
        let archive = tar_gz("ort/lib/libonnxruntime.1.18.1.dylib", b"a dylib");
        let mut server = mockito::Server::new();
        let mock = server
            .mock("GET", "/ort.tgz")
            .with_body(&archive)
            .expect(2)
            .create();
        let library = library(
            format!("{}/ort.tgz", server.url()),
            &archive,
            &"0".repeat(64),
        );
        let dir = tempfile::tempdir().unwrap();
        let store = Store::new(dir.path());
        let fetcher = Fetcher::new();
        for _ in 0..2 {
            assert_eq!(
                fetcher.installed_or_fetch(&store, &library, "macos-arm64"),
                None
            );
            fetcher.wait();
        }
        assert_eq!(installed(&store, &library, "macos-arm64"), None);
        assert!(!store.runtime_dir(LIBRARY, "1.18.1", "macos-arm64").exists());
        mock.assert();
    }

    /// An archive without the library the manifest names is an error, not a path.
    #[tokio::test]
    async fn an_archive_without_the_library_is_refused() {
        let archive = tar_gz("ort/LICENSE", b"MIT");
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/ort.tgz")
            .with_body(&archive)
            .create_async()
            .await;
        let library = library(
            format!("{}/ort.tgz", server.url()),
            &archive,
            &sha(&archive),
        );
        let dir = tempfile::tempdir().unwrap();
        let store = Store::new(dir.path());
        let error = fetch(&store, &http_client().unwrap(), &library, "macos-arm64")
            .await
            .unwrap_err();
        assert!(
            error.to_string().contains("libonnxruntime.1.18.1.dylib"),
            "{error}"
        );
        assert_eq!(installed(&store, &library, "macos-arm64"), None);
    }
}

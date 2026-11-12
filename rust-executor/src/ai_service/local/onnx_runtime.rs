//! ONNX Runtime for the speech gate (Silero VAD, `transcription::SpeechGate`) where `ort`
//! loads it at run time instead of linking it in: macOS, whose static ONNX Runtime is out
//! of ARM64 branch range (`Cargo.toml`, `load-dynamic`). Elsewhere it is linked in and
//! [`ready`] answers yes at once.
//!
//! On macOS the pinned build (`libraries.onnxruntime` in `manifest.json`, Microsoft's
//! release for the ONNX Runtime minor `ort` binds, 1.18) is fetched into the local AI store
//! like a runtime build, verified by size and sha256, by an explicit install that sets up
//! transcription (`ai.localInstall`, `ad4m-executor local-ai install`, `INCLUDE_MODELS`;
//! [`fetch_pinned`]). A stream never downloads it: without the library a stream runs
//! without the gate, with a warning that says to set up local AI. `ort` is pointed at the
//! library before the first model is built; it keeps the first library path it resolves for
//! the life of the process, so nothing touches `ort` before the file is there.
//! `ORT_DYLIB_PATH`, or a `libonnxruntime.dylib` beside the executable, still win: `ort`
//! resolves those itself.

use anyhow::{anyhow, bail, Result};
use std::path::PathBuf;
use std::sync::OnceLock;

use super::data::Library;
use super::install::Store;

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
/// library file. `progress` gets the bytes done.
pub async fn fetch(
    store: &Store,
    client: &reqwest::Client,
    library: &Library,
    key: &str,
    progress: &(dyn Fn(u64) + Send + Sync),
) -> Result<PathBuf> {
    let build = library
        .builds
        .get(key)
        .ok_or_else(|| anyhow!("The manifest has no {LIBRARY} build for {key}"))?;
    let unpacked = store
        .fetch_build(client, LIBRARY, &library.version, key, build, progress)
        .await?;
    let path = unpacked.dir.join(&build.binary);
    if !path.is_file() {
        bail!("{} has no {}", build.url, build.binary);
    }
    Ok(path)
}

/// The pinned library and this platform's build key, where `ort` loads ONNX Runtime at run
/// time (macOS) and the manifest pins it; `None` where it is linked in.
pub(crate) fn pinned() -> Option<(&'static Library, &'static str)> {
    let key = build_key()?;
    let library = super::data::embedded()
        .ok()?
        .manifest
        .libraries
        .get(LIBRARY)?;
    library.builds.contains_key(key).then_some((library, key))
}

/// The download size of the pinned build where it is needed, and whether `store` holds it.
pub(crate) fn planned(store: &Store) -> Option<(u64, bool)> {
    planned_with(store, pinned())
}

/// [`planned`] for `pinned` (the library and build key; `None` where ONNX Runtime is linked
/// in): a test passes a pinned build on any platform.
pub(super) fn planned_with(store: &Store, pinned: Option<(&Library, &str)>) -> Option<(u64, bool)> {
    let (library, key) = pinned?;
    let build = library.builds.get(key)?;
    Some((build.size_bytes, installed(store, library, key).is_some()))
}

/// For an explicit install: fetch `pinned` (the library and build key, [`pinned`]) into
/// `store`, unless it is there. `None` where ONNX Runtime is linked in (`pinned` is `None`).
/// A test passes a pinned build on any platform.
pub(crate) async fn fetch_pinned(
    store: &Store,
    client: &reqwest::Client,
    pinned: Option<(&Library, &str)>,
    progress: &(dyn Fn(u64) + Send + Sync),
) -> Result<Option<PathBuf>> {
    let Some((library, key)) = pinned else {
        return Ok(None);
    };
    fetch(store, client, library, key, progress).await.map(Some)
}

/// Whether `ort` was pointed at the store's library, and it loaded.
static POINTED: OnceLock<bool> = OnceLock::new();

/// Whether ONNX Runtime can be loaded now. Where it is linked in, or where `ORT_DYLIB_PATH`
/// or a library beside the executable provides it: yes. Else (macOS) yes once the store
/// holds the pinned build (an explicit install put it there) and `ort` loaded it; without
/// the build, no. It never downloads.
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
    let Ok(root) = super::store_root(configured) else {
        return false;
    };
    match installed(&Store::new(root), library, key) {
        Some(path) => *POINTED.get_or_init(|| point_ort_at(path)),
        None => false,
    }
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
    use super::super::install::http_client;
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
        assert_eq!(pinned().is_some(), cfg!(target_os = "macos"));
        let store = Store::new(tempfile::tempdir().unwrap().path());
        assert_eq!(planned(&store).is_some(), cfg!(target_os = "macos"));
        if build_key().is_none() {
            assert!(ready(), "linked in: always ready");
        }
    }

    /// An explicit install's fetch downloads the build once, verified, and the store then
    /// holds the library; a second fetch finds it and downloads nothing.
    #[tokio::test]
    async fn a_fetch_installs_the_verified_library_once() {
        let archive = tar_gz("ort/lib/libonnxruntime.1.18.1.dylib", b"a dylib");
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("GET", "/ort.tgz")
            .with_body(&archive)
            .expect(1)
            .create_async()
            .await;
        let library = library(
            format!("{}/ort.tgz", server.url()),
            &archive,
            &sha(&archive),
        );
        let dir = tempfile::tempdir().unwrap();
        let store = Store::new(dir.path());
        let client = http_client().unwrap();

        assert_eq!(installed(&store, &library, "macos-arm64"), None);
        let path = fetch(&store, &client, &library, "macos-arm64", &|_| {})
            .await
            .unwrap();
        assert_eq!(
            path,
            store
                .runtime_dir(LIBRARY, "1.18.1", "macos-arm64")
                .join("ort/lib/libonnxruntime.1.18.1.dylib")
        );
        assert_eq!(std::fs::read(&path).unwrap(), b"a dylib");
        assert_eq!(
            installed(&store, &library, "macos-arm64"),
            Some(path.clone())
        );
        let again = fetch(&store, &client, &library, "macos-arm64", &|_| {})
            .await
            .unwrap();
        assert_eq!(again, path);
        mock.assert_async().await;
    }

    /// A download whose digest is not the manifest's installs nothing.
    #[tokio::test]
    async fn a_wrong_digest_installs_nothing() {
        let archive = tar_gz("ort/lib/libonnxruntime.1.18.1.dylib", b"a dylib");
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/ort.tgz")
            .with_body(&archive)
            .create_async()
            .await;
        let library = library(
            format!("{}/ort.tgz", server.url()),
            &archive,
            &"0".repeat(64),
        );
        let dir = tempfile::tempdir().unwrap();
        let store = Store::new(dir.path());
        assert!(fetch(
            &store,
            &http_client().unwrap(),
            &library,
            "macos-arm64",
            &|_| {}
        )
        .await
        .is_err());
        assert_eq!(installed(&store, &library, "macos-arm64"), None);
        assert!(!store.runtime_dir(LIBRARY, "1.18.1", "macos-arm64").exists());
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
        let error = fetch(
            &store,
            &http_client().unwrap(),
            &library,
            "macos-arm64",
            &|_| {},
        )
        .await
        .unwrap_err();
        assert!(
            error.to_string().contains("libonnxruntime.1.18.1.dylib"),
            "{error}"
        );
        assert_eq!(installed(&store, &library, "macos-arm64"), None);
    }

    /// The plan's size of a pinned build, and whether the store holds it: on every platform
    /// when a build is pinned, none where it is linked in.
    #[tokio::test]
    async fn a_pinned_build_is_planned_by_its_size_until_the_store_holds_it() {
        let archive = tar_gz("ort/lib/libonnxruntime.1.18.1.dylib", b"a dylib");
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
        let pinned = Some((&library, "macos-arm64"));

        assert_eq!(planned_with(&store, None), None);
        assert_eq!(planned_with(&store, Some((&library, "linux-x64"))), None);
        assert_eq!(
            planned_with(&store, pinned),
            Some((archive.len() as u64, false))
        );

        let client = http_client().unwrap();
        assert_eq!(
            fetch_pinned(&store, &client, None, &|_| {}).await.unwrap(),
            None,
            "linked in: nothing to fetch"
        );
        let path = fetch_pinned(&store, &client, pinned, &|_| {})
            .await
            .unwrap()
            .unwrap();
        assert_eq!(std::fs::read(path).unwrap(), b"a dylib");
        assert_eq!(
            planned_with(&store, pinned),
            Some((archive.len() as u64, true))
        );
    }
}

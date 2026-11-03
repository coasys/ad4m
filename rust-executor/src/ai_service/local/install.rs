//! The per-user store of runtimes and models, and how files get into it: a streaming
//! download that resumes a `.part` file with a `Range` request and checks the size and the
//! sha256 before the file takes its name, then an unpack of runtime archives.
//!
//! Layout under the store root (`dirs::data_local_dir()/ad4m/local-ai` by default, shared
//! by every agent and app of the user):
//!
//! ```text
//! downloads/<file>.part         an unfinished download, resumed next time
//! runtime/<runtime>/<version>-<build>/   an unpacked build, with `.ad4m-installed`
//! models/<file>                 a model, with `<file>.sha256` once verified
//! ```

use anyhow::{anyhow, bail, Context, Result};
use futures::StreamExt;
use sha2::{Digest, Sha256};
use std::collections::HashSet;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::time::Duration;
use url::Url;

use super::data::{Build, Manifest, ModelFile};

/// A chunk that takes longer than this to arrive ends the download (it resumes next time).
const IDLE_TIMEOUT: Duration = Duration::from_secs(60);

/// The marker a finished runtime directory holds: the sha256 of each archive in it.
const INSTALLED_MARKER: &str = ".ad4m-installed";

/// The store root when neither the config (`local_ai_dir`) nor `AD4M_LOCAL_AI_DIR` names
/// one: `~/.local/share/ad4m/local-ai`, `~/Library/Application Support/ad4m/local-ai`,
/// `%LOCALAPPDATA%\ad4m\local-ai`.
pub fn default_store_root() -> PathBuf {
    dirs::data_local_dir()
        .unwrap_or_else(std::env::temp_dir)
        .join("ad4m")
        .join("local-ai")
}

/// The download's checksum or size is not the manifest's: the file is deleted.
#[derive(Debug)]
pub struct ChecksumMismatch {
    pub url: String,
    pub expected: String,
    pub actual: String,
}

impl std::fmt::Display for ChecksumMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} does not match the manifest: sha256 {} expected, {} downloaded",
            self.url, self.expected, self.actual
        )
    }
}

impl std::error::Error for ChecksumMismatch {}

/// An unpacked runtime build: its directory and the library directories of its extras,
/// which the supervisor puts on the library path.
#[derive(Debug, Clone, PartialEq)]
pub struct InstalledBuild {
    pub dir: PathBuf,
    pub lib_dirs: Vec<PathBuf>,
}

/// Beside the marker: each extra's library dir, relative, one per line.
const LIB_DIRS: &str = ".ad4m-lib-dirs";

/// The library dirs a build recorded at its unpack; the manifest's names for one unpacked
/// before the record existed.
fn read_lib_dirs(dir: &Path, build: &Build) -> Vec<PathBuf> {
    match std::fs::read_to_string(dir.join(LIB_DIRS)) {
        Ok(text) if !build.extras.is_empty() => text.lines().map(|r| dir.join(r)).collect(),
        _ => build.extras.iter().map(|e| dir.join(&e.lib_dir)).collect(),
    }
}

#[derive(Debug, Clone)]
pub struct Store {
    pub root: PathBuf,
}

impl Store {
    pub fn new(root: impl Into<PathBuf>) -> Store {
        Store { root: root.into() }
    }

    pub fn downloads_dir(&self) -> PathBuf {
        self.root.join("downloads")
    }

    pub fn models_dir(&self) -> PathBuf {
        self.root.join("models")
    }

    pub fn runtime_dir(&self, runtime_id: &str, version: &str, build: &str) -> PathBuf {
        self.root
            .join("runtime")
            .join(runtime_id)
            .join(format!("{version}-{build}"))
    }

    pub fn model_path(&self, model: &ModelFile) -> PathBuf {
        self.models_dir().join(model.file_name())
    }

    fn sidecar(path: &Path) -> PathBuf {
        let mut name = path.file_name().unwrap_or_default().to_os_string();
        name.push(".sha256");
        path.with_file_name(name)
    }

    /// Whether `model` is in the store, verified: the file and its checksum sidecar.
    pub fn model_installed(&self, model: &ModelFile) -> bool {
        let path = self.model_path(model);
        path.metadata().is_ok_and(|m| m.len() == model.size_bytes)
            && std::fs::read_to_string(Self::sidecar(&path))
                .is_ok_and(|sha| sha.trim().eq_ignore_ascii_case(&model.sha256))
    }

    /// Whether `build` (and its extras) is unpacked at its directory.
    pub fn build_installed(
        &self,
        runtime_id: &str,
        version: &str,
        key: &str,
        build: &Build,
    ) -> bool {
        let marker = self
            .runtime_dir(runtime_id, version, key)
            .join(INSTALLED_MARKER);
        let Ok(text) = std::fs::read_to_string(marker) else {
            return false;
        };
        let shas: HashSet<&str> = text.lines().map(str::trim).collect();
        shas.contains(build.sha256.as_str())
            && build
                .extras
                .iter()
                .all(|e| shas.contains(e.sha256.as_str()))
    }

    /// The sha256 of every manifest file the store holds: the plan's "installed".
    pub fn installed_digests(&self, manifest: &Manifest) -> HashSet<String> {
        let mut digests = HashSet::new();
        for (id, runtime) in &manifest.runtimes {
            for (key, build) in &runtime.builds {
                if self.build_installed(id, &runtime.version, key, build) {
                    digests.insert(build.sha256.clone());
                    digests.extend(build.extras.iter().map(|e| e.sha256.clone()));
                }
            }
        }
        for model in manifest.models.values() {
            if self.model_installed(model) {
                digests.insert(model.sha256.clone());
            }
        }
        digests
    }

    /// Download `model` into the store (resuming), unless it is there.
    pub async fn fetch_model(
        &self,
        client: &reqwest::Client,
        model: &ModelFile,
        progress: &(dyn Fn(u64) + Send + Sync),
    ) -> Result<PathBuf> {
        let path = self.model_path(model);
        if self.model_installed(model) {
            progress(model.size_bytes);
            return Ok(path);
        }
        download(
            client,
            &model.url,
            &model.sha256,
            model.size_bytes,
            &path,
            progress,
        )
        .await?;
        std::fs::write(Self::sidecar(&path), &model.sha256)?;
        Ok(path)
    }

    /// Download and unpack `build` and its extras into its runtime directory, unless they
    /// are there. `progress` gets the bytes of all archives together.
    pub async fn fetch_build(
        &self,
        client: &reqwest::Client,
        runtime_id: &str,
        version: &str,
        key: &str,
        build: &Build,
        progress: &(dyn Fn(u64) + Send + Sync),
    ) -> Result<InstalledBuild> {
        let dir = self.runtime_dir(runtime_id, version, key);
        let archives: Vec<(&Url, &str, u64)> =
            std::iter::once((&build.url, build.sha256.as_str(), build.size_bytes))
                .chain(
                    build
                        .extras
                        .iter()
                        .map(|e| (&e.url, e.sha256.as_str(), e.size_bytes)),
                )
                .collect();
        if self.build_installed(runtime_id, version, key, build) {
            progress(archives.iter().map(|a| a.2).sum());
            let lib_dirs = read_lib_dirs(&dir, build);
            return Ok(InstalledBuild { dir, lib_dirs });
        }
        let mut done = 0u64;
        let mut files = Vec::new();
        for (url, sha256, size) in &archives {
            let name = url
                .path_segments()
                .and_then(|mut s| s.next_back())
                .unwrap_or("archive");
            let path = self.downloads_dir().join(name);
            let before = done;
            download(client, url, sha256, *size, &path, &|bytes| {
                progress(before + bytes)
            })
            .await?;
            done += size;
            files.push(path);
        }

        let staging = dir.with_extension("partial");
        let target = dir.clone();
        let shas: Vec<String> = archives.iter().map(|a| a.1.to_string()).collect();
        let named: Vec<String> = build.extras.iter().map(|e| e.lib_dir.clone()).collect();
        let relative = tokio::task::spawn_blocking(move || -> Result<Vec<String>> {
            if staging.exists() {
                std::fs::remove_dir_all(&staging)?;
            }
            std::fs::create_dir_all(&staging)?;
            let mut relative = Vec::new();
            for (i, file) in files.iter().enumerate() {
                let top = unpack(file, &staging)
                    .with_context(|| format!("Unpacking {}", file.display()))?;
                // An extra's libraries: the dir the manifest names, else the archive's one
                // top-level dir (cudart unpacks into `cudart-llama-...-x64/`), else its root.
                if i > 0 {
                    let named = &named[i - 1];
                    relative.push(if !named.is_empty() {
                        named.clone()
                    } else if top.len() == 1 {
                        top.into_iter().next().unwrap_or_default()
                    } else {
                        String::new()
                    });
                }
            }
            std::fs::write(staging.join(LIB_DIRS), relative.join("\n"))?;
            std::fs::write(staging.join(INSTALLED_MARKER), shas.join("\n"))?;
            if target.exists() {
                std::fs::remove_dir_all(&target)?;
            }
            std::fs::rename(&staging, &target)?;
            clear_quarantine(&target);
            for file in &files {
                let _ = std::fs::remove_file(file);
            }
            Ok(relative)
        })
        .await??;
        let lib_dirs = relative.iter().map(|r| dir.join(r)).collect();
        Ok(InstalledBuild { dir, lib_dirs })
    }
}

/// The client local AI downloads with.
pub fn http_client() -> Result<reqwest::Client> {
    Ok(reqwest::Client::builder()
        .connect_timeout(Duration::from_secs(30))
        .user_agent(concat!(
            "ad4m-executor/",
            env!("CARGO_PKG_VERSION"),
            " local-ai"
        ))
        .build()?)
}

fn hex(digest: impl AsRef<[u8]>) -> String {
    digest.as_ref().iter().map(|b| format!("{b:02x}")).collect()
}

/// The sha256 of the first bytes of `file`, all of them.
fn hash_file(path: &Path) -> Result<(Sha256, u64)> {
    let mut hasher = Sha256::new();
    let mut file = std::fs::File::open(path)?;
    let mut buffer = vec![0u8; 1 << 20];
    let mut total = 0u64;
    loop {
        let n = file.read(&mut buffer)?;
        if n == 0 {
            break;
        }
        hasher.update(&buffer[..n]);
        total += n as u64;
    }
    Ok((hasher, total))
}

/// Download `url` to `dest`, `size` bytes with digest `sha256`. Bytes go to `dest.part`;
/// a `.part` left by an earlier try is resumed with `Range: bytes=<len>-` (a server that
/// answers 200 instead of 206 sends it all again). Only a file of the right size and
/// digest takes the name `dest`; anything else is deleted and is the error.
/// `progress` gets the bytes this file has so far.
pub async fn download(
    client: &reqwest::Client,
    url: &Url,
    sha256: &str,
    size: u64,
    dest: &Path,
    progress: &(dyn Fn(u64) + Send + Sync),
) -> Result<()> {
    if let Some(dir) = dest.parent() {
        tokio::fs::create_dir_all(dir).await?;
    }
    let mut part_name = dest.file_name().unwrap_or_default().to_os_string();
    part_name.push(".part");
    let part = dest.with_file_name(part_name);

    let existing = tokio::fs::metadata(&part)
        .await
        .map(|m| m.len())
        .unwrap_or(0);
    let (mut hasher, mut have) = if existing > 0 && existing <= size {
        let path = part.clone();
        tokio::task::spawn_blocking(move || hash_file(&path)).await??
    } else {
        if existing > size {
            tokio::fs::remove_file(&part).await?;
        }
        (Sha256::new(), 0)
    };

    if have < size {
        let mut request = client.get(url.clone());
        if have > 0 {
            request = request.header(reqwest::header::RANGE, format!("bytes={have}-"));
        }
        let response = request.send().await.with_context(|| format!("GET {url}"))?;
        let status = response.status();
        let resumed = status == reqwest::StatusCode::PARTIAL_CONTENT
            && response
                .headers()
                .get(reqwest::header::CONTENT_RANGE)
                .and_then(|v| v.to_str().ok())
                .is_some_and(|range| range.starts_with(&format!("bytes {have}-")));
        if have > 0 && !resumed {
            if status == reqwest::StatusCode::OK {
                // The server ignores the range and sends it all: start over.
                hasher = Sha256::new();
                have = 0;
            } else if status == reqwest::StatusCode::RANGE_NOT_SATISFIABLE
                || status == reqwest::StatusCode::PARTIAL_CONTENT
            {
                // It cannot resume from here (or would from elsewhere): a fresh try, which
                // sends no range.
                drop(response);
                tokio::fs::remove_file(&part).await.ok();
                return Box::pin(download(client, url, sha256, size, dest, progress)).await;
            } else {
                bail!("GET {url} answered {status}");
            }
        } else if !status.is_success() {
            bail!("GET {url} answered {status}");
        }
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .append(have > 0)
            .truncate(have == 0)
            .open(&part)?;
        progress(have);
        let mut stream = response.bytes_stream();
        loop {
            let chunk = match tokio::time::timeout(IDLE_TIMEOUT, stream.next()).await {
                Err(_) => bail!(
                    "GET {url}: no data for {}s (it resumes next time)",
                    IDLE_TIMEOUT.as_secs()
                ),
                Ok(None) => break,
                Ok(Some(chunk)) => {
                    chunk.with_context(|| format!("GET {url} (it resumes next time)"))?
                }
            };
            if have + chunk.len() as u64 > size {
                drop(file);
                tokio::fs::remove_file(&part).await.ok();
                bail!("{url} is larger than the manifest's {size} bytes");
            }
            file.write_all(&chunk)?;
            hasher.update(&chunk);
            have += chunk.len() as u64;
            progress(have);
        }
        file.flush()?;
    }

    if have < size {
        bail!("{url} ended after {have} of {size} bytes (it resumes next time)");
    }
    let actual = hex(hasher.finalize());
    if !actual.eq_ignore_ascii_case(sha256) {
        tokio::fs::remove_file(&part).await.ok();
        return Err(ChecksumMismatch {
            url: url.to_string(),
            expected: sha256.to_string(),
            actual,
        }
        .into());
    }
    tokio::fs::rename(&part, dest).await?;
    Ok(())
}

/// Unpack a `.tar.gz` or `.zip` into `into`, refusing entries that would land outside it.
/// Answers the archive's top-level names.
pub fn unpack(archive: &Path, into: &Path) -> Result<std::collections::BTreeSet<String>> {
    let mut top = std::collections::BTreeSet::new();
    let mut note_top = |path: &Path| {
        if let Some(std::path::Component::Normal(first)) = path.components().next() {
            top.insert(first.to_string_lossy().into_owned());
        }
    };
    let name = archive.to_string_lossy().to_lowercase();
    if name.ends_with(".tar.gz") || name.ends_with(".tgz") {
        let file = std::fs::File::open(archive)?;
        let mut tar = tar::Archive::new(flate2::read::GzDecoder::new(file));
        tar.set_preserve_permissions(true);
        tar.set_overwrite(true);
        for entry in tar.entries()? {
            let mut entry = entry?;
            note_top(&entry.path()?);
            // `unpack_in` refuses paths that leave `into`.
            if !entry.unpack_in(into)? {
                bail!("{} holds a path outside the archive", archive.display());
            }
        }
        Ok(top)
    } else if name.ends_with(".zip") {
        let file = std::fs::File::open(archive)?;
        let mut zip = zip::ZipArchive::new(file)?;
        for i in 0..zip.len() {
            let mut entry = zip.by_index(i)?;
            let relative = entry
                .enclosed_name()
                .ok_or_else(|| anyhow!("{} holds a path outside the archive", archive.display()))?
                .to_path_buf();
            note_top(&relative);
            let path = into.join(relative);
            if entry.is_dir() {
                std::fs::create_dir_all(&path)?;
                continue;
            }
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            let mut out = std::fs::File::create(&path)?;
            std::io::copy(&mut entry, &mut out)?;
            #[cfg(unix)]
            if let Some(mode) = entry.unix_mode() {
                use std::os::unix::fs::PermissionsExt;
                std::fs::set_permissions(&path, std::fs::Permissions::from_mode(mode))?;
            }
        }
        Ok(top)
    } else {
        bail!("{} is neither .tar.gz nor .zip", archive.display())
    }
}

/// macOS: drop the quarantine attribute, so Gatekeeper does not hold the unsigned binaries.
/// A file the executor downloads carries none, but an archive copied in by hand may.
fn clear_quarantine(dir: &Path) {
    if cfg!(target_os = "macos") {
        let _ = std::process::Command::new("xattr")
            .args(["-dr", "com.apple.quarantine"])
            .arg(dir)
            .status();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    fn body(n: usize) -> Vec<u8> {
        (0..n).map(|i| (i * 7 % 251) as u8).collect()
    }

    fn sha(bytes: &[u8]) -> String {
        hex(Sha256::digest(bytes))
    }

    fn url(server: &mockito::ServerGuard, path: &str) -> Url {
        Url::parse(&format!("{}{path}", server.url())).unwrap()
    }

    #[tokio::test]
    async fn a_download_is_verified_and_named_only_on_a_match() {
        let data = body(100_000);
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/f.bin")
            .with_body(&data)
            .create_async()
            .await;
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("f.bin");
        let seen = AtomicU64::new(0);
        download(
            &http_client().unwrap(),
            &url(&server, "/f.bin"),
            &sha(&data),
            data.len() as u64,
            &dest,
            &|n| seen.store(n, Ordering::SeqCst),
        )
        .await
        .unwrap();
        assert_eq!(std::fs::read(&dest).unwrap(), data);
        assert_eq!(seen.load(Ordering::SeqCst), data.len() as u64);
        assert!(!dir.path().join("f.bin.part").exists());
    }

    #[tokio::test]
    async fn a_wrong_digest_deletes_the_file_and_names_both_digests() {
        let data = body(1000);
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/f.bin")
            .with_body(&data)
            .create_async()
            .await;
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("f.bin");
        let wrong = "0".repeat(64);
        let error = download(
            &http_client().unwrap(),
            &url(&server, "/f.bin"),
            &wrong,
            1000,
            &dest,
            &|_| {},
        )
        .await
        .unwrap_err();
        let mismatch = error
            .downcast_ref::<ChecksumMismatch>()
            .expect("a checksum mismatch");
        assert_eq!(mismatch.actual, sha(&data));
        assert!(!dest.exists());
        assert!(!dir.path().join("f.bin.part").exists());
    }

    /// A `.part` from an interrupted try: only the rest is asked for, with a range request,
    /// and the digest covers the bytes from both tries.
    #[tokio::test]
    async fn an_interrupted_download_resumes_with_a_range_request() {
        let data = body(200_000);
        let half = 75_000;
        let mut server = mockito::Server::new_async().await;
        let rest = server
            .mock("GET", "/f.bin")
            .match_header("range", format!("bytes={half}-").as_str())
            .with_status(206)
            .with_header(
                "content-range",
                &format!("bytes {half}-{}/{}", data.len() - 1, data.len()),
            )
            .with_body(&data[half..])
            .expect(1)
            .create_async()
            .await;
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("f.bin");
        std::fs::write(dir.path().join("f.bin.part"), &data[..half]).unwrap();
        download(
            &http_client().unwrap(),
            &url(&server, "/f.bin"),
            &sha(&data),
            data.len() as u64,
            &dest,
            &|_| {},
        )
        .await
        .unwrap();
        rest.assert_async().await;
        assert_eq!(std::fs::read(&dest).unwrap(), data);
    }

    /// A server that ignores `Range` sends everything with 200: the `.part` starts over.
    #[tokio::test]
    async fn a_server_without_ranges_sends_it_all_and_the_part_starts_over() {
        let data = body(50_000);
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/f.bin")
            .with_status(200)
            .with_body(&data)
            .create_async()
            .await;
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("f.bin");
        // Not even the right bytes: they are not kept.
        std::fs::write(dir.path().join("f.bin.part"), vec![9u8; 10_000]).unwrap();
        download(
            &http_client().unwrap(),
            &url(&server, "/f.bin"),
            &sha(&data),
            data.len() as u64,
            &dest,
            &|_| {},
        )
        .await
        .unwrap();
        assert_eq!(std::fs::read(&dest).unwrap(), data);
    }

    #[tokio::test]
    async fn a_complete_part_is_verified_without_a_request_and_a_larger_one_is_dropped() {
        let data = body(10_000);
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join("f.bin");
        std::fs::write(dir.path().join("f.bin.part"), &data).unwrap();
        // No server: a request would fail.
        let nowhere = Url::parse("http://127.0.0.1:1/f.bin").unwrap();
        download(
            &http_client().unwrap(),
            &nowhere,
            &sha(&data),
            data.len() as u64,
            &dest,
            &|_| {},
        )
        .await
        .unwrap();
        assert_eq!(std::fs::read(&dest).unwrap(), data);

        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/g.bin")
            .with_body(&data)
            .create_async()
            .await;
        let other = dir.path().join("g.bin");
        std::fs::write(dir.path().join("g.bin.part"), body(20_000)).unwrap();
        download(
            &http_client().unwrap(),
            &url(&server, "/g.bin"),
            &sha(&data),
            data.len() as u64,
            &other,
            &|_| {},
        )
        .await
        .unwrap();
        assert_eq!(std::fs::read(&other).unwrap(), data);
    }

    #[tokio::test]
    async fn a_body_longer_than_the_manifest_says_is_refused() {
        let data = body(5000);
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/f.bin")
            .with_body(&data)
            .create_async()
            .await;
        let dir = tempfile::tempdir().unwrap();
        let error = download(
            &http_client().unwrap(),
            &url(&server, "/f.bin"),
            &sha(&data[..4000]),
            4000,
            &dir.path().join("f.bin"),
            &|_| {},
        )
        .await
        .unwrap_err();
        assert!(error.to_string().contains("larger"), "{error}");
    }

    fn tar_gz(entries: &[(&str, &[u8], u32)]) -> Vec<u8> {
        let mut builder = tar::Builder::new(flate2::write::GzEncoder::new(
            Vec::new(),
            flate2::Compression::fast(),
        ));
        for (path, data, mode) in entries {
            let mut header = tar::Header::new_gnu();
            header.set_size(data.len() as u64);
            header.set_mode(*mode);
            header.set_cksum();
            builder.append_data(&mut header, path, *data).unwrap();
        }
        builder.into_inner().unwrap().finish().unwrap()
    }

    /// A build and its extra download, unpack into one directory with the marker; the
    /// archives go, and the store then counts the build as installed.
    #[tokio::test]
    async fn a_build_unpacks_with_its_extras_and_counts_as_installed() {
        let main = tar_gz(&[("rt/server", b"#!/bin/sh\n", 0o755)]);
        let extra = tar_gz(&[("libs/libx.so", b"x", 0o644)]);
        let mut server = mockito::Server::new_async().await;
        server
            .mock("GET", "/main.tar.gz")
            .with_body(&main)
            .create_async()
            .await;
        server
            .mock("GET", "/extra.tar.gz")
            .with_body(&extra)
            .create_async()
            .await;
        let build: Build = serde_json::from_value(serde_json::json!({
            "url": url(&server, "/main.tar.gz"), "sha256": sha(&main), "sizeBytes": main.len(), "binary": "rt/server",
            "extras": [{ "url": url(&server, "/extra.tar.gz"), "sha256": sha(&extra), "sizeBytes": extra.len() }]
        }))
        .unwrap();
        let dir = tempfile::tempdir().unwrap();
        let store = Store::new(dir.path());
        assert!(!store.build_installed("rt", "v1", "linux-x86_64-cpu", &build));
        let total = AtomicU64::new(0);
        let installed = store
            .fetch_build(
                &http_client().unwrap(),
                "rt",
                "v1",
                "linux-x86_64-cpu",
                &build,
                &|n| total.store(n, Ordering::SeqCst),
            )
            .await
            .unwrap();
        assert_eq!(
            total.load(Ordering::SeqCst),
            (main.len() + extra.len()) as u64
        );
        let at = installed.dir.clone();
        assert!(at.join("rt/server").exists());
        assert!(at.join("libs/libx.so").exists());
        // No `libDir` in the manifest: the extra's one top-level dir holds its libraries.
        assert_eq!(installed.lib_dirs, vec![at.join("libs")]);
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            assert_eq!(
                std::fs::metadata(at.join("rt/server"))
                    .unwrap()
                    .permissions()
                    .mode()
                    & 0o111,
                0o111
            );
        }
        assert!(store.build_installed("rt", "v1", "linux-x86_64-cpu", &build));
        assert!(!store.downloads_dir().join("main.tar.gz").exists());
        // Installed already: no download, the same library dirs.
        drop(server);
        let again = store
            .fetch_build(
                &http_client().unwrap(),
                "rt",
                "v1",
                "linux-x86_64-cpu",
                &build,
                &|_| {},
            )
            .await
            .unwrap();
        assert_eq!(again, installed);
    }

    #[test]
    fn an_archive_entry_outside_the_target_is_refused() {
        let dir = tempfile::tempdir().unwrap();
        let mut builder = tar::Builder::new(flate2::write::GzEncoder::new(
            Vec::new(),
            flate2::Compression::fast(),
        ));
        let mut header = tar::Header::new_gnu();
        let data = b"evil";
        header.set_size(data.len() as u64);
        header.set_mode(0o644);
        // `append_data` refuses `..`; write the raw name as a hostile archive would.
        header.as_gnu_mut().unwrap().name[..12].copy_from_slice(b"../escape.sh");
        header.set_cksum();
        builder.append(&header, &data[..]).unwrap();
        let archive = dir.path().join("evil.tar.gz");
        std::fs::write(&archive, builder.into_inner().unwrap().finish().unwrap()).unwrap();
        let into = dir.path().join("into");
        std::fs::create_dir_all(&into).unwrap();
        let _ = unpack(&archive, &into);
        assert!(!dir.path().join("escape.sh").exists());
    }
}

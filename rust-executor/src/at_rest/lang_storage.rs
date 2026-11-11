//! The languages' storage folders under their keys.
//!
//! **Where.** Each language has a storage folder, `<languages folder>/<address>`. The executor
//! keeps the language's own files there too (`bundle.js`, `meta.json`, `settings.json`): those
//! stay plain and a language cannot read or write them through its storage calls. Every other
//! file in the folder is the language's: host.js's KV file and whatever else the language
//! writes through `readStorageFile` / `writeStorageFile`.
//!
//! **The file.** A sealed file is
//!
//! ```text
//! "AD4MSEAL" | format (u8, 1) | store id length (u16) | store id (UTF-8)
//!            | key version (u32) | nonce (24) | XChaCha20-Poly1305 ciphertext and tag
//! ```
//!
//! integers big endian, under the LanguageStorage subkey of the store named in the header
//! (`executor`, or `perspective:<uuid>` for the link language of that perspective's
//! neighbourhood). The associated data names the language's address, the file's path inside
//! the folder and the store, so a file moved into another language's folder, or to another
//! name, does not open.
//!
//! **Which store.** A language serves the perspective whose neighbourhood names it as its link
//! language (the executor's link-language routing cache), else the executor. A file sealed
//! under another store than the current one (a link language that wrote before its
//! neighbourhood was known) opens under the store its header names and is sealed again under
//! the current one.
//!
//! **From plaintext.** [`prepare`] seals every plaintext file of every language folder in
//! place at start, each one written beside it and renamed over it. Until every file is sealed
//! the languages' storage state is `plain` (or `migrating` meanwhile) and a plaintext file a
//! language reads is accepted and sealed on the spot; once it is `encrypted` a plaintext file
//! is refused. A failed migration leaves the file it failed on in the clear and the state
//! `plain`.
//!
//! Without installed keys (unit tests, an embedder that never called `run`) the files are
//! read and written plain.

use std::collections::HashSet;
use std::io;
use std::path::{Component, Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use ad4m_keyring::KeyVersion;
use chacha20poly1305::aead::{Aead, KeyInit, Payload};
use chacha20poly1305::{XChaCha20Poly1305, XNonce};
use chrono::Utc;
use rand::RngCore;

use super::data_keys::{DataKeys, KeyringError, SubkeyPurpose};
use super::files;
use super::keyring_file::{StoreId, StoreState};
use super::{StorageErrorKind, StorageGate};

/// What every sealed language file starts with.
pub const MAGIC: &[u8; 8] = b"AD4MSEAL";
/// The format byte after [`MAGIC`].
pub const FORMAT: u8 = 1;
/// The executor's own files in a language's folder: plain, and not the language's to touch.
pub const RESERVED: [&str; 3] = ["bundle.js", "meta.json", "settings.json"];
/// The languages folder's entry that is not a language.
const NOT_A_LANGUAGE: &str = "temp";

const NONCE_LEN: usize = 24;
const TAG_LEN: usize = 16;
const AD_LABEL: &[u8] = b"ad4m:at-rest:language-file:v1";

/// Why a storage call failed. The messages of a missing file start with `NotFound: No such
/// file`: host.js reads that as an empty store.
#[derive(Debug, thiserror::Error)]
pub enum LangStorageError {
    #[error("NotFound: No such file or directory: {0}")]
    NotFound(String),
    #[error("{0} is not a path a language may use for its storage")]
    BadPath(String),
    #[error("{0} is the executor's file in the language's folder, not the language's")]
    Reserved(String),
    #[error("{0} is outside the language's storage folder")]
    Outside(String),
    #[error("{0} is a plaintext file in a language's storage that is under its key")]
    NotSealed(String),
    #[error("{path} does not open under the key of {store}: it was written for another language, file or store, or changed")]
    DoesNotOpen { path: String, store: String },
    #[error("{0} is sealed and this executor holds no keys to open it")]
    NoKeys(String),
    #[error("{0} is not UTF-8 text")]
    NotText(String),
    #[error("keyring: {0}")]
    Keyring(#[from] KeyringError),
    #[error("{path}: {error}")]
    Io { path: String, error: io::Error },
}

fn io_error(path: &Path, error: io::Error) -> LangStorageError {
    if error.kind() == io::ErrorKind::NotFound {
        LangStorageError::NotFound(path.display().to_string())
    } else {
        LangStorageError::Io {
            path: path.display().to_string(),
            error,
        }
    }
}

static INSTALLED: OnceLock<Arc<DataKeys>> = OnceLock::new();

/// Installs the keys the languages' storage calls seal under. `run` calls it once the keyring
/// is open; a second call (a second `run` in one process) keeps the first.
pub fn install(keys: Arc<DataKeys>) {
    let _ = INSTALLED.set(keys);
}

/// The installed keys, once `run` installed them.
pub fn installed() -> Option<Arc<DataKeys>> {
    INSTALLED.get().cloned()
}

/// Whether `bytes` is a sealed language file.
pub fn is_sealed(bytes: &[u8]) -> bool {
    bytes.starts_with(MAGIC)
}

fn associated_data(address: &str, file: &str, store: &str) -> Vec<u8> {
    let mut ad = Vec::with_capacity(AD_LABEL.len() + address.len() + file.len() + 64);
    for part in [
        AD_LABEL,
        &[FORMAT][..],
        address.as_bytes(),
        file.as_bytes(),
        store.as_bytes(),
    ] {
        ad.extend_from_slice(&(part.len() as u32).to_be_bytes());
        ad.extend_from_slice(part);
    }
    ad
}

/// Seals `plain` as the file `file` (its path inside the folder, `/`-separated) of the
/// language at `address`, under the current LanguageStorage subkey of `store`.
pub fn seal(
    keys: &DataKeys,
    store: &StoreId,
    address: &str,
    file: &str,
    plain: &[u8],
) -> Result<Vec<u8>, KeyringError> {
    let (version, key) = keys.subkey(store, None, SubkeyPurpose::LanguageStorage)?;
    let cipher = XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key");
    let mut nonce = [0u8; NONCE_LEN];
    rand::thread_rng().fill_bytes(&mut nonce);
    let store = store.to_string();
    let ad = associated_data(address, file, &store);
    let sealed = cipher
        .encrypt(
            XNonce::from_slice(&nonce),
            Payload {
                msg: plain,
                aad: &ad,
            },
        )
        .expect("XChaCha20-Poly1305 seals any length a file holds");
    let mut bytes = Vec::with_capacity(MAGIC.len() + 7 + store.len() + NONCE_LEN + sealed.len());
    bytes.extend_from_slice(MAGIC);
    bytes.push(FORMAT);
    bytes.extend_from_slice(&(store.len() as u16).to_be_bytes());
    bytes.extend_from_slice(store.as_bytes());
    bytes.extend_from_slice(&version.0.to_be_bytes());
    bytes.extend_from_slice(&nonce);
    bytes.extend_from_slice(&sealed);
    Ok(bytes)
}

/// Opens a sealed file `file` of the language at `address`: the store it was sealed under
/// and its content. `shown` names the file in errors.
pub fn open(
    keys: &DataKeys,
    address: &str,
    file: &str,
    sealed: &[u8],
    shown: &str,
) -> Result<(StoreId, Vec<u8>), LangStorageError> {
    let does_not_open = |store: &str| LangStorageError::DoesNotOpen {
        path: shown.to_string(),
        store: store.to_string(),
    };
    let rest = sealed
        .strip_prefix(&MAGIC[..])
        .ok_or_else(|| LangStorageError::NotSealed(shown.to_string()))?;
    let (&format, rest) = rest.split_first().ok_or_else(|| does_not_open("?"))?;
    if format != FORMAT || rest.len() < 2 {
        return Err(does_not_open("?"));
    }
    let store_len = u16::from_be_bytes([rest[0], rest[1]]) as usize;
    let rest = &rest[2..];
    if rest.len() < store_len + 4 + NONCE_LEN + TAG_LEN {
        return Err(does_not_open("?"));
    }
    let store_text = std::str::from_utf8(&rest[..store_len]).map_err(|_| does_not_open("?"))?;
    let store: StoreId = store_text.parse().map_err(|_| does_not_open(store_text))?;
    let rest = &rest[store_len..];
    let version = KeyVersion(u32::from_be_bytes([rest[0], rest[1], rest[2], rest[3]]));
    let nonce = &rest[4..4 + NONCE_LEN];
    let ciphertext = &rest[4 + NONCE_LEN..];
    let (_, key) = keys.subkey(&store, Some(version), SubkeyPurpose::LanguageStorage)?;
    let cipher = XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key");
    let ad = associated_data(address, file, store_text);
    let plain = cipher
        .decrypt(
            XNonce::from_slice(nonce),
            Payload {
                msg: ciphertext,
                aad: &ad,
            },
        )
        .map_err(|_| does_not_open(store_text))?;
    Ok((store, plain))
}

/// Where a path a language named is.
#[derive(Debug, PartialEq, Eq)]
pub enum Resolved {
    /// In the language's folder: the full path and the path inside it, `/`-separated.
    Inside { path: PathBuf, file: String },
    /// Anywhere else (an absolute path).
    Outside(PathBuf),
}

/// Resolves `path` against the language's `folder`. A relative path is inside the folder. A
/// path with `..`, an empty path, the folder itself and the executor's own files are refused.
pub fn resolve(folder: &Path, path: &str) -> Result<Resolved, LangStorageError> {
    let bad = || LangStorageError::BadPath(path.to_string());
    if path.is_empty() || path.contains('\0') {
        return Err(bad());
    }
    let named = Path::new(path);
    if named
        .components()
        .any(|c| matches!(c, Component::ParentDir))
    {
        return Err(bad());
    }
    let normal = |p: &Path| -> PathBuf {
        p.components()
            .filter(|c| !matches!(c, Component::CurDir))
            .collect()
    };
    // A path that starts with the folder (as `languageStorageDirectory()` gives it, absolute or
    // not) is inside it; any other absolute path is outside; any other relative path is
    // relative to the folder.
    let inside = match normal(named).strip_prefix(normal(folder)) {
        Ok(rest) => rest.to_path_buf(),
        Err(_) if named.is_absolute() => return Ok(Resolved::Outside(normal(named))),
        Err(_) => normal(named),
    };
    let mut parts = Vec::new();
    for component in inside.components() {
        match component {
            Component::Normal(part) => parts.push(part.to_str().ok_or_else(bad)?.to_string()),
            _ => return Err(bad()),
        }
    }
    if parts.is_empty() {
        return Err(bad());
    }
    if parts.len() == 1 && RESERVED.contains(&parts[0].as_str()) {
        return Err(LangStorageError::Reserved(path.to_string()));
    }
    let file = parts.join("/");
    Ok(Resolved::Inside {
        path: folder.join(&inside),
        file,
    })
}

/// The language a storage call came from.
#[derive(Clone, Copy, Debug)]
pub struct Caller<'a> {
    /// The language's storage folder.
    pub folder: &'a Path,
    pub address: &'a str,
    /// A system language may also use plain files outside its folder, under the executor's
    /// working directory.
    pub system: bool,
}

/// The store the language at `address` serves: the perspective whose neighbourhood names it
/// as its link language, else the executor.
pub fn scope_for(keys: &DataKeys, address: &str) -> StoreId {
    let perspective = crate::perspectives::cached_perspective_handle(address)
        .and_then(|handle| uuid::Uuid::parse_str(&handle.uuid).ok())
        .map(StoreId::Perspective);
    match perspective {
        Some(store) if keys.has_store(&store).unwrap_or(false) => store,
        _ => StoreId::Executor,
    }
}

static WARNED_OUTSIDE: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();

fn outside(caller: Caller, path: &Path, shown: &str) -> Result<(), LangStorageError> {
    let under_cwd = std::env::current_dir()
        .map(|cwd| path.starts_with(cwd))
        .unwrap_or(false);
    if !caller.system || !under_cwd {
        return Err(LangStorageError::Outside(shown.to_string()));
    }
    let warned = WARNED_OUTSIDE.get_or_init(|| Mutex::new(HashSet::new()));
    if warned.lock().unwrap().insert(caller.address.to_string()) {
        log::warn!(
            "The system language {} keeps files outside its storage folder ({}): those are written in the clear",
            caller.address,
            path.display()
        );
    }
    Ok(())
}

/// Reads the file `path` names for `caller`: opened when sealed. A plaintext file in the
/// folder is accepted while the languages' storage is not yet `encrypted`, and sealed then.
pub fn read_file(
    keys: Option<&DataKeys>,
    caller: Caller,
    path: &str,
) -> Result<String, LangStorageError> {
    let (full, file) = match resolve(caller.folder, path)? {
        Resolved::Outside(full) => {
            outside(caller, &full, path)?;
            return std::fs::read_to_string(&full).map_err(|e| io_error(&full, e));
        }
        Resolved::Inside { path, file } => (path, file),
    };
    let bytes = std::fs::read(&full).map_err(|e| io_error(&full, e))?;
    let text = |bytes: Vec<u8>| {
        String::from_utf8(bytes).map_err(|_| LangStorageError::NotText(path.to_string()))
    };
    let Some(keys) = keys else {
        if is_sealed(&bytes) {
            return Err(LangStorageError::NoKeys(path.to_string()));
        }
        return text(bytes);
    };
    let scope = scope_for(keys, caller.address);
    if is_sealed(&bytes) {
        let (store, plain) = open(keys, caller.address, &file, &bytes, path)?;
        if store != scope {
            reseal(keys, &scope, caller.address, &file, &full, &plain);
        }
        return text(plain);
    }
    if keys.language_storage_state()? == StoreState::Encrypted {
        return Err(LangStorageError::NotSealed(path.to_string()));
    }
    reseal(keys, &scope, caller.address, &file, &full, &bytes);
    text(bytes)
}

/// Seals `plain` again under `scope` in place. A failure only logs: the reader has the
/// content, and the file still opens as it is.
fn reseal(keys: &DataKeys, scope: &StoreId, address: &str, file: &str, full: &Path, plain: &[u8]) {
    let written = seal(keys, scope, address, file, plain)
        .map_err(|e| e.to_string())
        .and_then(|sealed| files::write_atomic(full, &sealed).map_err(|e| e.to_string()));
    if let Err(e) = written {
        log::warn!("Could not seal {} under {scope}: {e}", full.display());
    }
}

/// Writes `content` to the file `path` names for `caller`, sealed under the key of the store
/// the language serves, atomically.
pub fn write_file(
    keys: Option<&DataKeys>,
    caller: Caller,
    path: &str,
    content: &str,
) -> Result<(), LangStorageError> {
    let (full, file) = match resolve(caller.folder, path)? {
        Resolved::Outside(full) => {
            outside(caller, &full, path)?;
            return std::fs::write(&full, content).map_err(|e| io_error(&full, e));
        }
        Resolved::Inside { path, file } => (path, file),
    };
    if let Some(parent) = full.parent() {
        files::create_private_dir_all(parent).map_err(|e| io_error(parent, e))?;
    }
    let bytes = match keys {
        Some(keys) => {
            let scope = scope_for(keys, caller.address);
            seal(keys, &scope, caller.address, &file, content.as_bytes())?
        }
        None => content.as_bytes().to_vec(),
    };
    files::write_atomic(&full, &bytes).map_err(|e| io_error(&full, e))
}

/// Deletes every file the language at `address` keeps in its storage folder under
/// `languages` (the executor's own files of the language stay), and the folders left empty
/// inside it. Answers how many files it deleted; none for a language without a folder.
pub fn delete_storage(languages: &Path, address: &str) -> io::Result<usize> {
    if address.is_empty() || address == NOT_A_LANGUAGE || address.contains(['/', '\\']) {
        return Ok(0);
    }
    let folder = languages.join(address);
    if !folder.is_dir() {
        return Ok(0);
    }
    delete_files_in(&folder, true)
}

fn delete_files_in(dir: &Path, top: bool) -> io::Result<usize> {
    let mut deleted = 0;
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let kind = entry.file_type()?;
        let path = entry.path();
        if kind.is_dir() {
            deleted += delete_files_in(&path, false)?;
            if std::fs::read_dir(&path)?.next().is_none() {
                std::fs::remove_dir(&path)?;
            }
            continue;
        }
        let reserved = top
            && entry
                .file_name()
                .to_str()
                .is_some_and(|name| RESERVED.contains(&name));
        if !reserved {
            std::fs::remove_file(&path)?;
            deleted += 1;
        }
    }
    Ok(deleted)
}

/// What the start step did.
#[derive(Debug, PartialEq, Eq)]
pub enum LanguageStorageStep {
    /// Every language file is sealed; `sealed` of them were sealed now.
    Sealed { sealed: usize },
    /// A file did not seal: it and the files after it stay in the clear.
    KeptPlain { sealed: usize, error: String },
}

/// The plaintext files of every language folder under `languages`: (address, path inside the
/// folder, full path).
fn plaintext_files(languages: &Path) -> io::Result<Vec<(String, String, PathBuf)>> {
    let mut found = Vec::new();
    let entries = match std::fs::read_dir(languages) {
        Ok(entries) => entries,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(found),
        Err(e) => return Err(e),
    };
    for entry in entries {
        let entry = entry?;
        if !entry.file_type()?.is_dir() {
            continue;
        }
        let Some(address) = entry.file_name().to_str().map(str::to_string) else {
            continue;
        };
        if address == NOT_A_LANGUAGE {
            continue;
        }
        collect(&entry.path(), "", &address, &mut found)?;
    }
    found.sort_by(|a, b| a.2.cmp(&b.2));
    Ok(found)
}

fn collect(
    dir: &Path,
    prefix: &str,
    address: &str,
    found: &mut Vec<(String, String, PathBuf)>,
) -> io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let Some(name) = entry.file_name().to_str().map(str::to_string) else {
            continue;
        };
        let file = if prefix.is_empty() {
            name.clone()
        } else {
            format!("{prefix}/{name}")
        };
        let kind = entry.file_type()?;
        if kind.is_dir() {
            collect(&entry.path(), &file, address, found)?;
            continue;
        }
        if !kind.is_file()
            || (prefix.is_empty() && RESERVED.contains(&name.as_str()))
            || (name.starts_with('.') && name.ends_with(".tmp"))
        {
            continue;
        }
        let mut head = [0u8; 8];
        let read = {
            use std::io::Read;
            let mut opened = std::fs::File::open(entry.path())?;
            let mut read = 0;
            while read < head.len() {
                match opened.read(&mut head[read..])? {
                    0 => break,
                    n => read += n,
                }
            }
            read
        };
        if !is_sealed(&head[..read]) {
            found.push((address.to_string(), file, entry.path()));
        }
    }
    Ok(())
}

fn set_state(keys: &DataKeys, gate: Option<&StorageGate>, state: StoreState) -> Result<(), String> {
    keys.set_language_storage_state(state).map_err(|e| {
        format!("Could not record the languages' storage state in the keyring: {e}")
    })?;
    if let Some(gate) = gate {
        gate.publish();
    }
    Ok(())
}

/// The start step: seals every plaintext file of every language folder under `languages`
/// ([`migrate`]), then installs `keys` for the languages' storage calls. Runs once the keyring
/// is open and the link-language routing cache knows every neighbourhood, before any language
/// loads. `Err` stops the start: the keyring cannot be written.
pub fn prepare(
    keys: &Arc<DataKeys>,
    languages: &Path,
    gate: Option<&StorageGate>,
) -> Result<LanguageStorageStep, String> {
    let step = migrate(keys, languages, gate)?;
    install(keys.clone());
    Ok(step)
}

/// Seals every plaintext file of every language folder under `languages` in place, each one
/// written sealed beside it and renamed over it, then checked to open to what it held. The
/// state is `migrating` meanwhile, `encrypted` after, and back to `plain` (the gate reports
/// `migration-failed`) when a file does not seal: that file keeps its plaintext.
pub fn migrate(
    keys: &DataKeys,
    languages: &Path,
    gate: Option<&StorageGate>,
) -> Result<LanguageStorageStep, String> {
    migrate_with(keys, languages, gate, |from, to| std::fs::rename(from, to))
}

/// [`migrate`] with the step that puts each sealed file in place given: tests make it fail.
pub(crate) fn migrate_with(
    keys: &DataKeys,
    languages: &Path,
    gate: Option<&StorageGate>,
    rename: impl Fn(&Path, &Path) -> io::Result<()>,
) -> Result<LanguageStorageStep, String> {
    let fail = |sealed: usize, error: String| -> Result<LanguageStorageStep, String> {
        log::error!("The languages' storage did not move under its keys: {error}");
        set_state(keys, gate, StoreState::Plain)?;
        if let Some(gate) = gate {
            gate.report_error(StorageErrorKind::MigrationFailed);
        }
        Ok(LanguageStorageStep::KeptPlain { sealed, error })
    };
    let pending = match plaintext_files(languages) {
        Ok(pending) => pending,
        Err(e) => return fail(0, format!("Could not list {}: {e}", languages.display())),
    };
    if pending.is_empty() {
        if keys.language_storage_state().map_err(|e| e.to_string())? != StoreState::Encrypted {
            set_state(keys, gate, StoreState::Encrypted)?;
        }
        return Ok(LanguageStorageStep::Sealed { sealed: 0 });
    }
    log::info!(
        "Sealing {} files of the languages' storage under their keys",
        pending.len()
    );
    set_state(
        keys,
        gate,
        StoreState::Migrating {
            started_at: Utc::now(),
        },
    )?;
    let mut sealed = 0;
    for (address, file, full) in &pending {
        let one = || -> Result<(), String> {
            let plain = std::fs::read(full).map_err(|e| format!("{}: {e}", full.display()))?;
            let scope = scope_for(keys, address);
            let bytes = seal(keys, &scope, address, file, &plain).map_err(|e| format!("{e}"))?;
            files::write_atomic_with(full, &bytes, |from, to| rename(from, to))
                .map_err(|e| format!("{}: {e}", full.display()))?;
            let back = std::fs::read(full).map_err(|e| format!("{}: {e}", full.display()))?;
            let shown = full.display().to_string();
            let (_, opened) =
                open(keys, address, file, &back, &shown).map_err(|e| e.to_string())?;
            if opened != plain {
                return Err(format!("{shown} does not open to what it held"));
            }
            Ok(())
        };
        if let Err(error) = one() {
            return fail(sealed, error);
        }
        sealed += 1;
    }
    set_state(keys, gate, StoreState::Encrypted)?;
    log::info!("The languages' storage is under its keys: {sealed} files sealed");
    Ok(LanguageStorageStep::Sealed { sealed })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn open_keys(folder: &Path) -> Arc<DataKeys> {
        let keys = DataKeys::load(crate::at_rest::keyring_file::path_in(folder)).unwrap();
        keys.unlock(zeroize::Zeroizing::new([9u8; 32])).unwrap();
        Arc::new(keys)
    }

    fn caller<'a>(folder: &'a Path, address: &'a str) -> Caller<'a> {
        Caller {
            folder,
            address,
            system: false,
        }
    }

    /// A language's storage goes, down every subfolder, and its bundle, meta and settings stay;
    /// an address that is not one folder deletes nothing.
    #[test]
    fn a_languages_storage_goes_and_its_bundle_stays() {
        let data = tempfile::tempdir().unwrap();
        let languages = data.path().join("languages");
        let folder = languages.join("QmLanguageA");
        std::fs::create_dir_all(folder.join("sub").join("deeper")).unwrap();
        for name in RESERVED {
            std::fs::write(folder.join(name), b"kept").unwrap();
        }
        std::fs::write(folder.join("links.db"), b"stored").unwrap();
        std::fs::write(folder.join("sub").join("bundle.js"), b"stored").unwrap();
        std::fs::write(folder.join("sub").join("deeper").join("x"), b"stored").unwrap();
        let other = languages.join("QmLanguageB");
        std::fs::create_dir_all(&other).unwrap();
        std::fs::write(other.join("links.db"), b"other").unwrap();

        for refused in [
            "",
            NOT_A_LANGUAGE,
            "../QmLanguageB",
            "QmLanguageB/..",
            "missing",
        ] {
            assert_eq!(delete_storage(&languages, refused).unwrap(), 0, "{refused}");
        }
        assert_eq!(delete_storage(&languages, "QmLanguageA").unwrap(), 3);
        let mut left: Vec<_> = std::fs::read_dir(&folder)
            .unwrap()
            .map(|e| e.unwrap().file_name().into_string().unwrap())
            .collect();
        left.sort();
        let mut reserved: Vec<_> = RESERVED.iter().map(|s| s.to_string()).collect();
        reserved.sort();
        assert_eq!(left, reserved);
        assert!(other.join("links.db").exists());
    }

    #[test]
    fn a_written_file_is_sealed_on_disk_and_reads_back() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let folder = data.path().join("languages").join("QmLanguageA");
        std::fs::create_dir_all(&folder).unwrap();
        let me = caller(&folder, "QmLanguageA");

        write_file(
            Some(&keys),
            me,
            "ad4m-language-kv.json",
            r#"{"secret":"v"}"#,
        )
        .unwrap();
        let raw = std::fs::read(folder.join("ad4m-language-kv.json")).unwrap();
        assert!(raw.starts_with(MAGIC), "the file starts with the magic");
        assert_eq!(raw[8], FORMAT);
        assert!(
            !raw.windows(6).any(|w| w == b"secret"),
            "the content is not on disk in the clear"
        );
        assert_eq!(
            read_file(Some(&keys), me, "ad4m-language-kv.json").unwrap(),
            r#"{"secret":"v"}"#
        );
        // The absolute path inside the folder is the same file.
        let absolute = folder.join("ad4m-language-kv.json");
        assert_eq!(
            read_file(Some(&keys), me, absolute.to_str().unwrap()).unwrap(),
            r#"{"secret":"v"}"#
        );
    }

    #[test]
    fn a_file_moved_into_another_language_or_name_does_not_open() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let a = data.path().join("QmLanguageA");
        let b = data.path().join("QmLanguageB");
        std::fs::create_dir_all(&a).unwrap();
        std::fs::create_dir_all(&b).unwrap();
        write_file(
            Some(&keys),
            caller(&a, "QmLanguageA"),
            "kv.json",
            "a's data",
        )
        .unwrap();

        std::fs::rename(a.join("kv.json"), b.join("kv.json")).unwrap();
        let err = read_file(Some(&keys), caller(&b, "QmLanguageB"), "kv.json").unwrap_err();
        assert!(
            matches!(err, LangStorageError::DoesNotOpen { .. }),
            "another language's folder: {err}"
        );

        std::fs::rename(b.join("kv.json"), a.join("other.json")).unwrap();
        let err = read_file(Some(&keys), caller(&a, "QmLanguageA"), "other.json").unwrap_err();
        assert!(
            matches!(err, LangStorageError::DoesNotOpen { .. }),
            "another name: {err}"
        );
    }

    #[test]
    fn a_language_cannot_name_another_languages_files() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let a = data.path().join("QmLanguageA");
        let b = data.path().join("QmLanguageB");
        std::fs::create_dir_all(&a).unwrap();
        std::fs::create_dir_all(&b).unwrap();
        write_file(
            Some(&keys),
            caller(&b, "QmLanguageB"),
            "kv.json",
            "b's data",
        )
        .unwrap();
        let me = caller(&a, "QmLanguageA");

        let theirs = b.join("kv.json");
        for path in [
            theirs.to_str().unwrap().to_string(),
            "../QmLanguageB/kv.json".to_string(),
            format!("{}/../QmLanguageB/kv.json", a.display()),
        ] {
            let err = read_file(Some(&keys), me, &path).unwrap_err();
            assert!(
                matches!(
                    err,
                    LangStorageError::Outside(_) | LangStorageError::BadPath(_)
                ),
                "{path}: {err}"
            );
            let err = write_file(Some(&keys), me, &path, "overwritten").unwrap_err();
            assert!(
                matches!(
                    err,
                    LangStorageError::Outside(_) | LangStorageError::BadPath(_)
                ),
                "{path}: {err}"
            );
        }
        assert_eq!(
            read_file(Some(&keys), caller(&b, "QmLanguageB"), "kv.json").unwrap(),
            "b's data"
        );
    }

    #[test]
    fn the_executors_own_files_are_not_the_languages() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let a = data.path().join("QmLanguageA");
        std::fs::create_dir_all(&a).unwrap();
        std::fs::write(a.join("bundle.js"), "export default 1").unwrap();
        for name in RESERVED {
            let err = write_file(Some(&keys), caller(&a, "QmLanguageA"), name, "x").unwrap_err();
            assert!(
                matches!(err, LangStorageError::Reserved(_)),
                "{name}: {err}"
            );
            let err = read_file(Some(&keys), caller(&a, "QmLanguageA"), name).unwrap_err();
            assert!(
                matches!(err, LangStorageError::Reserved(_)),
                "{name}: {err}"
            );
        }
        assert_eq!(
            std::fs::read_to_string(a.join("bundle.js")).unwrap(),
            "export default 1"
        );
        // A nested file of the same name is the language's.
        write_file(
            Some(&keys),
            caller(&a, "QmLanguageA"),
            "sub/meta.json",
            "{}",
        )
        .unwrap();
        assert!(std::fs::read(a.join("sub/meta.json"))
            .unwrap()
            .starts_with(MAGIC));
    }

    #[test]
    fn paths_resolve_inside_the_folder_however_the_folder_is_named() {
        let inside = |folder: &str, path: &str| match resolve(Path::new(folder), path).unwrap() {
            Resolved::Inside { path, file } => (path, file),
            Resolved::Outside(p) => panic!("{path} in {folder} is outside: {}", p.display()),
        };
        let kv = "ad4m-language-kv.json";
        for (folder, path) in [
            (
                "/data/languages/Qm1",
                "/data/languages/Qm1/ad4m-language-kv.json",
            ),
            ("/data/languages/Qm1", "./ad4m-language-kv.json"),
            ("/data/languages/Qm1", "ad4m-language-kv.json"),
            (
                "data/languages/Qm1",
                "data/languages/Qm1/ad4m-language-kv.json",
            ),
            ("data/languages/Qm1", "ad4m-language-kv.json"),
        ] {
            let (full, file) = inside(folder, path);
            assert_eq!(file, kv, "{folder} {path}");
            assert_eq!(full, Path::new(folder).join(kv), "{folder} {path}");
        }
        assert_eq!(inside("/data/languages/Qm1", "a/b/c.json").1, "a/b/c.json");
        assert_eq!(
            resolve(
                Path::new("/data/languages/Qm1"),
                "/data/languages/Qm2/kv.json"
            )
            .unwrap(),
            Resolved::Outside(PathBuf::from("/data/languages/Qm2/kv.json"))
        );
        for bad in [
            "",
            "..",
            "a/../../b",
            "/data/languages/Qm1",
            "/data/languages/Qm1/",
        ] {
            assert!(
                matches!(
                    resolve(Path::new("/data/languages/Qm1"), bad),
                    Err(LangStorageError::BadPath(_))
                ),
                "{bad:?}"
            );
        }
    }

    #[test]
    fn a_missing_file_reads_as_not_found() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let a = data.path().join("QmLanguageA");
        std::fs::create_dir_all(&a).unwrap();
        let err = read_file(Some(&keys), caller(&a, "QmLanguageA"), "nothing.json").unwrap_err();
        assert!(
            err.to_string().starts_with("NotFound: No such file"),
            "{err}"
        );
    }

    #[test]
    fn a_plaintext_file_is_sealed_when_read_and_refused_once_encrypted() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let a = data.path().join("QmLanguageA");
        std::fs::create_dir_all(&a).unwrap();
        let me = caller(&a, "QmLanguageA");

        std::fs::write(a.join("old.json"), "from before").unwrap();
        assert_eq!(
            read_file(Some(&keys), me, "old.json").unwrap(),
            "from before"
        );
        assert!(std::fs::read(a.join("old.json"))
            .unwrap()
            .starts_with(MAGIC));
        assert_eq!(
            read_file(Some(&keys), me, "old.json").unwrap(),
            "from before"
        );

        keys.set_language_storage_state(StoreState::Encrypted)
            .unwrap();
        std::fs::write(a.join("planted.json"), "planted").unwrap();
        let err = read_file(Some(&keys), me, "planted.json").unwrap_err();
        assert!(matches!(err, LangStorageError::NotSealed(_)), "{err}");
    }

    #[test]
    fn a_file_sealed_under_another_store_opens_and_moves_to_the_current_one() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let a = data.path().join("QmLanguageA");
        std::fs::create_dir_all(&a).unwrap();
        let perspective = StoreId::Perspective(uuid::Uuid::new_v4());
        keys.create_store(perspective.clone(), &[]).unwrap();
        let sealed = seal(&keys, &perspective, "QmLanguageA", "kv.json", b"under p").unwrap();
        std::fs::write(a.join("kv.json"), &sealed).unwrap();

        // No neighbourhood names this language now: the executor's store.
        assert_eq!(
            read_file(Some(&keys), caller(&a, "QmLanguageA"), "kv.json").unwrap(),
            "under p"
        );
        let raw = std::fs::read(a.join("kv.json")).unwrap();
        let (store, plain) = open(&keys, "QmLanguageA", "kv.json", &raw, "kv.json").unwrap();
        assert_eq!(store, StoreId::Executor);
        assert_eq!(plain, b"under p");
    }

    #[test]
    fn a_system_language_keeps_plain_files_under_the_working_directory_only() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let a = data.path().join("QmLanguageA");
        std::fs::create_dir_all(&a).unwrap();
        let system = Caller {
            folder: &a,
            address: "QmSystem",
            system: true,
        };
        let cwd = std::env::current_dir().unwrap();
        let shared = tempfile::tempdir_in(&cwd).unwrap();
        let shared_file = shared.path().join("expression.json");
        let shared_path = shared_file.to_str().unwrap();
        write_file(Some(&keys), system, shared_path, "{\"a\":1}").unwrap();
        assert_eq!(std::fs::read_to_string(&shared_file).unwrap(), "{\"a\":1}");
        assert_eq!(
            read_file(Some(&keys), system, shared_path).unwrap(),
            "{\"a\":1}"
        );

        let elsewhere = data.path().join("elsewhere.json");
        if !elsewhere.starts_with(&cwd) {
            let err =
                write_file(Some(&keys), system, elsewhere.to_str().unwrap(), "x").unwrap_err();
            assert!(matches!(err, LangStorageError::Outside(_)), "{err}");
        }
        let err = write_file(Some(&keys), caller(&a, "QmOther"), shared_path, "x").unwrap_err();
        assert!(
            matches!(err, LangStorageError::Outside(_)),
            "not a system language: {err}"
        );
    }

    #[test]
    fn without_keys_the_files_are_plain() {
        let data = tempfile::tempdir().unwrap();
        let a = data.path().join("QmLanguageA");
        std::fs::create_dir_all(&a).unwrap();
        let me = caller(&a, "QmLanguageA");
        write_file(None, me, "kv.json", "plain").unwrap();
        assert_eq!(std::fs::read_to_string(a.join("kv.json")).unwrap(), "plain");
        assert_eq!(read_file(None, me, "kv.json").unwrap(), "plain");
    }

    fn language_folders(languages: &Path) -> (PathBuf, PathBuf) {
        let a = languages.join("QmLanguageA");
        let b = languages.join("QmLanguageB");
        std::fs::create_dir_all(a.join("nested")).unwrap();
        std::fs::create_dir_all(&b).unwrap();
        std::fs::create_dir_all(languages.join("temp")).unwrap();
        for dir in [&a, &b] {
            std::fs::write(dir.join("bundle.js"), "bundle").unwrap();
            std::fs::write(dir.join("meta.json"), "{\"meta\":1}").unwrap();
        }
        std::fs::write(a.join("ad4m-language-kv.json"), "{\"a\":\"kv\"}").unwrap();
        std::fs::write(a.join("nested").join("deep.txt"), "deep").unwrap();
        std::fs::write(b.join("ad4m-language-kv.json"), "{\"b\":\"kv\"}").unwrap();
        std::fs::write(languages.join("temp").join("download.js"), "temp").unwrap();
        (a, b)
    }

    #[test]
    fn the_migration_seals_every_language_file_in_place_and_keeps_its_content() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let languages = data.path().join("languages");
        let (a, b) = language_folders(&languages);

        let step = migrate(&keys, &languages, None).unwrap();
        assert_eq!(step, LanguageStorageStep::Sealed { sealed: 3 });
        assert_eq!(
            keys.language_storage_state().unwrap(),
            StoreState::Encrypted
        );
        for (dir, address, file, content) in [
            (&a, "QmLanguageA", "ad4m-language-kv.json", "{\"a\":\"kv\"}"),
            (&a, "QmLanguageA", "nested/deep.txt", "deep"),
            (&b, "QmLanguageB", "ad4m-language-kv.json", "{\"b\":\"kv\"}"),
        ] {
            let raw = std::fs::read(dir.join(file)).unwrap();
            assert!(raw.starts_with(MAGIC), "{file} is sealed");
            assert_eq!(
                read_file(Some(&keys), caller(dir, address), file).unwrap(),
                content
            );
        }
        for dir in [&a, &b] {
            assert_eq!(
                std::fs::read_to_string(dir.join("bundle.js")).unwrap(),
                "bundle"
            );
            assert_eq!(
                std::fs::read_to_string(dir.join("meta.json")).unwrap(),
                "{\"meta\":1}"
            );
            assert!(
                std::fs::read_dir(dir).unwrap().all(|e| !e
                    .unwrap()
                    .file_name()
                    .to_string_lossy()
                    .ends_with(".tmp")),
                "no temporary file is left"
            );
        }
        assert_eq!(
            std::fs::read_to_string(languages.join("temp").join("download.js")).unwrap(),
            "temp",
            "the downloads folder is not a language"
        );

        // A second start finds nothing to seal.
        let again = migrate(&keys, &languages, None).unwrap();
        assert_eq!(again, LanguageStorageStep::Sealed { sealed: 0 });
    }

    #[test]
    fn a_failed_migration_keeps_the_plaintext() {
        let data = tempfile::tempdir().unwrap();
        let keys = open_keys(data.path());
        let languages = data.path().join("languages");
        let (a, b) = language_folders(&languages);

        let calls = std::sync::atomic::AtomicUsize::new(0);
        let step = migrate_with(&keys, &languages, None, |from, to| {
            if calls.fetch_add(1, std::sync::atomic::Ordering::SeqCst) == 1 {
                Err(io::Error::other("the disk is full"))
            } else {
                std::fs::rename(from, to)
            }
        })
        .unwrap();
        let LanguageStorageStep::KeptPlain { sealed, error } = step else {
            panic!("the migration did not fail: {step:?}");
        };
        assert_eq!(sealed, 1);
        assert!(error.contains("the disk is full"), "{error}");
        assert_eq!(keys.language_storage_state().unwrap(), StoreState::Plain);

        // Files are sealed in path order: a's KV first, then a's nested file (failed), then b's.
        assert!(std::fs::read(a.join("ad4m-language-kv.json"))
            .unwrap()
            .starts_with(MAGIC));
        assert_eq!(
            std::fs::read_to_string(a.join("nested").join("deep.txt")).unwrap(),
            "deep",
            "the file that failed keeps its plaintext"
        );
        assert_eq!(
            std::fs::read_to_string(b.join("ad4m-language-kv.json")).unwrap(),
            "{\"b\":\"kv\"}"
        );
        assert!(
            std::fs::read_dir(a.join("nested")).unwrap().count() == 1,
            "no temporary file is left"
        );
        // Every file still reads while the state is plain.
        assert_eq!(
            read_file(
                Some(&keys),
                caller(&a, "QmLanguageA"),
                "ad4m-language-kv.json"
            )
            .unwrap(),
            "{\"a\":\"kv\"}"
        );
        assert_eq!(
            read_file(
                Some(&keys),
                caller(&b, "QmLanguageB"),
                "ad4m-language-kv.json"
            )
            .unwrap(),
            "{\"b\":\"kv\"}"
        );

        // The next start finishes it.
        let step = migrate(&keys, &languages, None).unwrap();
        assert!(
            matches!(step, LanguageStorageStep::Sealed { .. }),
            "{step:?}"
        );
        assert_eq!(
            keys.language_storage_state().unwrap(),
            StoreState::Encrypted
        );
    }

    #[test]
    fn the_state_survives_a_reload_of_the_keyring() {
        let data = tempfile::tempdir().unwrap();
        {
            let keys = open_keys(data.path());
            assert_eq!(keys.language_storage_state().unwrap(), StoreState::Plain);
            keys.set_language_storage_state(StoreState::Encrypted)
                .unwrap();
        }
        let keys = open_keys(data.path());
        assert_eq!(
            keys.language_storage_state().unwrap(),
            StoreState::Encrypted
        );
    }
}

//! The keyring file, `{data}/ad4m/keyring.json`: every data key of this executor, each
//! wrapped under the key above it, and nothing that opens without the operator secret.
//!
//! ```text
//! operator secret ── HKDF(salt = keyring id) ──> root key (generation g)
//!   root key ──wraps──> executor store key, each person's data key
//!   person's data key ──wraps──> the keys of the stores that person owns
//! ```
//!
//! A wrap's associated data names the keyring, the key it holds and the key it is wrapped
//! under ([`ad4m_keyring::WrapContext`]), so a wrap moved to another entry, version or keyring
//! does not open. `root_check` is a wrap under the root key alone: it tells a wrong secret
//! from a damaged file before any other wrap is tried.
//!
//! The file is written whole and atomically ([`super::files::write_atomic`]), mode 0600, with
//! `seq` one higher on every write. No copy of it is kept beside it.

use std::collections::BTreeMap;
use std::fmt;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use ad4m_keyring::{
    hkdf_subkey, unwrap, wrap, KeyRef, KeyVersion, SecretKey, SymWrap, VersionedKey, WrapContext,
};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// The file's name in `{data}/ad4m`.
pub const FILE_NAME: &str = "keyring.json";
/// The file format this executor writes and reads.
pub const FORMAT: u32 = 1;
/// HKDF `info` of the root key; the salt is the keyring id.
pub const ROOT_INFO: &str = "ad4m:at-rest:root:v1";
/// HKDF `info` of the value `root_check` wraps.
const ROOT_CHECK_INFO: &str = "ad4m:at-rest:root-check:v1";

/// The keyring file of the data folder `data_dir` (the executor's `app_data_path`).
pub fn path_in(data_dir: &Path) -> PathBuf {
    data_dir.join("ad4m").join(FILE_NAME)
}

/// The id of one keyring: 16 random bytes, made once, hex in the file and the status.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct KeyringId(pub [u8; 16]);

impl KeyringId {
    pub fn random() -> Self {
        let mut bytes = [0u8; 16];
        rand::RngCore::fill_bytes(&mut rand::rngs::OsRng, &mut bytes);
        KeyringId(bytes)
    }

    pub fn to_hex(&self) -> String {
        hex::encode(self.0)
    }
}

impl fmt::Debug for KeyringId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "KeyringId({})", self.to_hex())
    }
}

impl Serialize for KeyringId {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&self.to_hex())
    }
}

impl<'de> Deserialize<'de> for KeyringId {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let text = String::deserialize(d)?;
        let bytes = hex::decode(&text).map_err(serde::de::Error::custom)?;
        let bytes: [u8; 16] = bytes
            .try_into()
            .map_err(|_| serde::de::Error::custom("a keyring id has 16 bytes"))?;
        Ok(KeyringId(bytes))
    }
}

/// Whose data key: the main agent's, or a managed user's (by username).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum KeyOwner {
    Main,
    User(String),
}

impl fmt::Display for KeyOwner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KeyOwner::Main => f.write_str("main"),
            KeyOwner::User(name) => write!(f, "user:{name}"),
        }
    }
}

impl FromStr for KeyOwner {
    type Err = String;

    fn from_str(text: &str) -> Result<Self, String> {
        match text {
            "main" => Ok(KeyOwner::Main),
            _ => match text.strip_prefix("user:") {
                Some(name) if !name.is_empty() => Ok(KeyOwner::User(name.to_string())),
                _ => Err(format!("`{text}` names no key owner")),
            },
        }
    }
}

/// One store of data, each with its own key.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StoreId {
    /// The executor's own database.
    Executor,
    /// A person's private store.
    Person(KeyOwner),
    /// One perspective's store.
    Perspective(uuid::Uuid),
}

impl fmt::Display for StoreId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StoreId::Executor => f.write_str("executor"),
            StoreId::Person(owner) => write!(f, "person:{owner}"),
            StoreId::Perspective(uuid) => write!(f, "perspective:{uuid}"),
        }
    }
}

impl FromStr for StoreId {
    type Err = String;

    fn from_str(text: &str) -> Result<Self, String> {
        if text == "executor" {
            return Ok(StoreId::Executor);
        }
        if let Some(owner) = text.strip_prefix("person:") {
            return owner.parse().map(StoreId::Person);
        }
        if let Some(uuid) = text.strip_prefix("perspective:") {
            return uuid
                .parse()
                .map(StoreId::Perspective)
                .map_err(|e| format!("`{text}`: {e}"));
        }
        Err(format!("`{text}` names no store"))
    }
}

macro_rules! string_serde {
    ($type:ty) => {
        impl Serialize for $type {
            fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                s.collect_str(self)
            }
        }
        impl<'de> Deserialize<'de> for $type {
            fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
                String::deserialize(d)?
                    .parse()
                    .map_err(serde::de::Error::custom)
            }
        }
    };
}
string_serde!(KeyOwner);
string_serde!(StoreId);

/// A key wrapped under the root key of one generation.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RootWrap {
    pub generation: u32,
    pub wrap: SymWrap,
}

/// A person's data key, every version wrapped under the root key.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersonEntry {
    pub owner: KeyOwner,
    pub created_at: DateTime<Utc>,
    pub key: VersionedKey<RootWrap>,
}

/// Whether a store's data is under its key yet.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum StoreState {
    /// Written in the clear: the key exists, the data does not use it yet.
    Plain,
    /// Being moved under its key.
    Migrating { started_at: DateTime<Utc> },
    /// Under its key.
    Encrypted,
}

/// Whether a version of a store key is the one written with or only still read.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VersionState {
    Active,
    Retiring,
}

/// The key a store key is wrapped under.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Wrapper {
    Root {
        generation: u32,
    },
    Person {
        owner: KeyOwner,
        version: KeyVersion,
    },
}

/// One wrap of a store key.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StoreWrap {
    pub wrapper: Wrapper,
    pub wrap: SymWrap,
}

/// One version of a store key: a wrap for each owner (or one under the root key).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StoreKeyWraps {
    pub state: VersionState,
    pub wraps: Vec<StoreWrap>,
}

/// A store and its key.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StoreEntry {
    pub id: StoreId,
    pub state: StoreState,
    pub key: VersionedKey<StoreKeyWraps>,
}

impl StoreEntry {
    /// The persons whose data key wraps the current version.
    pub fn owners(&self) -> Vec<KeyOwner> {
        self.key
            .current_value()
            .wraps
            .iter()
            .filter_map(|w| match &w.wrapper {
                Wrapper::Person { owner, .. } => Some(owner.clone()),
                Wrapper::Root { .. } => None,
            })
            .collect()
    }
}

/// What was destroyed: a person's key or a store's key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TombstoneKind {
    Person,
    Store,
}

/// A destroyed key: its id only as a hash, when, and why.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Tombstone {
    pub kind: TombstoneKind,
    /// SHA-256 of the id, hex.
    pub id_hash: String,
    pub destroyed_at: DateTime<Utc>,
    pub reason: String,
}

impl Tombstone {
    pub fn new(kind: TombstoneKind, id: &str, reason: &str) -> Self {
        Tombstone {
            kind,
            id_hash: hex::encode(Sha256::digest(id.as_bytes())),
            destroyed_at: Utc::now(),
            reason: reason.to_string(),
        }
    }
}

/// The keyring file.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct KeyringFile {
    pub format: u32,
    pub keyring_id: KeyringId,
    pub created_at: DateTime<Utc>,
    pub root_generation: u32,
    /// One more on every write.
    pub seq: u64,
    /// A wrap under the root key alone: it opens only under the right secret.
    pub root_check: SymWrap,
    pub executor_store: StoreEntry,
    pub persons: BTreeMap<KeyOwner, PersonEntry>,
    pub stores: BTreeMap<StoreId, StoreEntry>,
    pub tombstones: Vec<Tombstone>,
    /// Reserved for the agent passphrase held for the unlock; never set by this executor.
    pub held_agent_passphrase: Option<SymWrap>,
}

/// Why the keyring file did not read.
#[derive(Debug, thiserror::Error)]
pub enum FileError {
    #[error("cannot read the keyring file {path}: {error}")]
    Io {
        path: PathBuf,
        error: std::io::Error,
    },
    #[error("the keyring file {path} is damaged: {reason}")]
    Corrupt { path: PathBuf, reason: String },
}

const ROOT: &str = "root";
const ROOT_CHECK: &str = "root-check";
const PERSON: &str = "person";
const STORE: &str = "store";

/// The root key: HKDF-SHA256 of the operator secret, salted with the keyring id.
pub fn derive_root(secret: &SecretKey, keyring_id: &KeyringId) -> SecretKey {
    hkdf_subkey(secret, &keyring_id.0, ROOT_INFO)
}

/// The context of a wrap: what it holds and what it is wrapped under.
fn context<'a>(id: &'a KeyringId, wrapped: KeyRef<'a>, wrapper: KeyRef<'a>) -> WrapContext<'a> {
    WrapContext {
        keyring_id: &id.0,
        wrapped,
        wrapper,
    }
}

impl KeyringFile {
    /// A new keyring: `root` (of generation 1) checked by `root_check`, a fresh executor store
    /// key and a fresh data key for the main agent, all wrapped under `root`. Every store is
    /// [`StoreState::Plain`]. Returns the keys made, to hold in memory.
    pub fn create(keyring_id: KeyringId, root: &SecretKey) -> (KeyringFile, SecretKey, SecretKey) {
        let generation = 1;
        let now = Utc::now();
        let executor_key = SecretKey::random();
        let main_key = SecretKey::random();
        let mut file = KeyringFile {
            format: FORMAT,
            keyring_id,
            created_at: now,
            root_generation: generation,
            seq: 0,
            root_check: root_check(&keyring_id, root, generation),
            executor_store: StoreEntry {
                id: StoreId::Executor,
                state: StoreState::Plain,
                key: VersionedKey::new(StoreKeyWraps {
                    state: VersionState::Active,
                    wraps: vec![],
                }),
            },
            persons: BTreeMap::new(),
            stores: BTreeMap::new(),
            tombstones: vec![],
            held_agent_passphrase: None,
        };
        let executor_wrap = file.wrap_store_key(
            &StoreId::Executor,
            KeyVersion::FIRST,
            &executor_key,
            &Wrapper::Root { generation },
            root,
        );
        file.executor_store.key = VersionedKey::new(StoreKeyWraps {
            state: VersionState::Active,
            wraps: vec![executor_wrap],
        });
        let main_wrap = file.wrap_person_key(&KeyOwner::Main, KeyVersion::FIRST, &main_key, root);
        file.persons.insert(
            KeyOwner::Main,
            PersonEntry {
                owner: KeyOwner::Main,
                created_at: now,
                key: VersionedKey::new(main_wrap),
            },
        );
        (file, executor_key, main_key)
    }

    /// Whether `root` is this keyring's root key: `root_check` opens under it.
    pub fn root_matches(&self, root: &SecretKey) -> bool {
        let generation = self.root_generation.to_string();
        let ctx = context(
            &self.keyring_id,
            KeyRef::new(ROOT_CHECK, &generation, self.root_generation),
            KeyRef::new(ROOT, "", self.root_generation),
        );
        unwrap(&self.root_check, root, &ctx).is_ok()
    }

    /// `key`, a version of `owner`'s data key, wrapped under the current root key.
    pub fn wrap_person_key(
        &self,
        owner: &KeyOwner,
        version: KeyVersion,
        key: &SecretKey,
        root: &SecretKey,
    ) -> RootWrap {
        let owner = owner.to_string();
        let ctx = context(
            &self.keyring_id,
            KeyRef::new(PERSON, &owner, version.0),
            KeyRef::new(ROOT, "", self.root_generation),
        );
        RootWrap {
            generation: self.root_generation,
            wrap: wrap(key, root, &ctx),
        }
    }

    /// Opens a version of `owner`'s data key.
    pub fn unwrap_person_key(
        &self,
        owner: &KeyOwner,
        version: KeyVersion,
        wrapped: &RootWrap,
        root: &SecretKey,
    ) -> Result<SecretKey, ad4m_keyring::KeyringError> {
        let owner = owner.to_string();
        let ctx = context(
            &self.keyring_id,
            KeyRef::new(PERSON, &owner, version.0),
            KeyRef::new(ROOT, "", wrapped.generation),
        );
        unwrap(&wrapped.wrap, root, &ctx)
    }

    /// `key`, a version of store `id`'s key, wrapped under `under` (the key `wrapper` names).
    pub fn wrap_store_key(
        &self,
        id: &StoreId,
        version: KeyVersion,
        key: &SecretKey,
        wrapper: &Wrapper,
        under: &SecretKey,
    ) -> StoreWrap {
        let store = id.to_string();
        let (kind, wrapper_id, wrapper_version) = wrapper_ref(wrapper);
        let ctx = context(
            &self.keyring_id,
            KeyRef::new(STORE, &store, version.0),
            KeyRef::new(kind, &wrapper_id, wrapper_version),
        );
        StoreWrap {
            wrapper: wrapper.clone(),
            wrap: wrap(key, under, &ctx),
        }
    }

    /// Opens one wrap of a version of store `id`'s key under `under`.
    pub fn unwrap_store_key(
        &self,
        id: &StoreId,
        version: KeyVersion,
        wrapped: &StoreWrap,
        under: &SecretKey,
    ) -> Result<SecretKey, ad4m_keyring::KeyringError> {
        let store = id.to_string();
        let (kind, wrapper_id, wrapper_version) = wrapper_ref(&wrapped.wrapper);
        let ctx = context(
            &self.keyring_id,
            KeyRef::new(STORE, &store, version.0),
            KeyRef::new(kind, &wrapper_id, wrapper_version),
        );
        unwrap(&wrapped.wrap, under, &ctx)
    }

    /// The store `id`: the executor's or one in `stores`.
    pub fn store(&self, id: &StoreId) -> Option<&StoreEntry> {
        match id {
            StoreId::Executor => Some(&self.executor_store),
            other => self.stores.get(other),
        }
    }

    /// Every store, the executor's first.
    pub fn all_stores(&self) -> impl Iterator<Item = &StoreEntry> {
        std::iter::once(&self.executor_store).chain(self.stores.values())
    }

    /// Reads the keyring file at `path`: `None` when there is none. A file that does not parse,
    /// or is of another format, is [`FileError::Corrupt`].
    pub fn read(path: &Path) -> Result<Option<KeyringFile>, FileError> {
        let bytes = match std::fs::read(path) {
            Ok(bytes) => bytes,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(error) => {
                return Err(FileError::Io {
                    path: path.to_path_buf(),
                    error,
                })
            }
        };
        let corrupt = |reason: String| FileError::Corrupt {
            path: path.to_path_buf(),
            reason,
        };
        let file: KeyringFile =
            serde_json::from_slice(&bytes).map_err(|e| corrupt(e.to_string()))?;
        if file.format != FORMAT {
            return Err(corrupt(format!(
                "format {} (this executor reads format {FORMAT})",
                file.format
            )));
        }
        Ok(Some(file))
    }

    /// Writes the file at `path` atomically, mode 0600, making `{data}/ad4m` (0700) if needed.
    pub fn write(&self, path: &Path) -> std::io::Result<()> {
        if let Some(folder) = path.parent() {
            super::files::create_private_dir_all(folder)?;
        }
        let bytes = serde_json::to_vec_pretty(self).map_err(std::io::Error::other)?;
        super::files::write_atomic(path, &bytes)
    }
}

/// The root check of `root` of `generation`: a wrap of a value derived from `root`, under
/// `root` itself, for a place no other wrap names.
fn root_check(keyring_id: &KeyringId, root: &SecretKey, generation: u32) -> SymWrap {
    let generation_text = generation.to_string();
    let ctx = context(
        keyring_id,
        KeyRef::new(ROOT_CHECK, &generation_text, generation),
        KeyRef::new(ROOT, "", generation),
    );
    wrap(&hkdf_subkey(root, &[], ROOT_CHECK_INFO), root, &ctx)
}

fn wrapper_ref(wrapper: &Wrapper) -> (&'static str, String, u32) {
    match wrapper {
        Wrapper::Root { generation } => (ROOT, String::new(), *generation),
        Wrapper::Person { owner, version } => (PERSON, owner.to_string(), version.0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn secret(byte: u8) -> SecretKey {
        SecretKey::from_slice(&[byte; 32]).unwrap()
    }

    #[test]
    fn ids_and_owners_read_back_from_their_text() {
        for owner in [KeyOwner::Main, KeyOwner::User("alice".into())] {
            assert_eq!(owner.to_string().parse::<KeyOwner>().unwrap(), owner);
        }
        let uuid = uuid::Uuid::new_v4();
        for id in [
            StoreId::Executor,
            StoreId::Person(KeyOwner::Main),
            StoreId::Person(KeyOwner::User("bob:x".into())),
            StoreId::Perspective(uuid),
        ] {
            assert_eq!(id.to_string().parse::<StoreId>().unwrap(), id);
        }
        assert!("user:".parse::<KeyOwner>().is_err());
        assert!("perspective:nope".parse::<StoreId>().is_err());
    }

    #[test]
    fn a_new_keyring_writes_and_reads_back_whole() {
        let folder = tempfile::tempdir().unwrap();
        let path = path_in(folder.path());
        let id = KeyringId::random();
        let root = derive_root(&secret(7), &id);
        let (file, executor_key, main_key) = KeyringFile::create(id, &root);
        file.write(&path).unwrap();

        let read = KeyringFile::read(&path)
            .unwrap()
            .expect("the file is there");
        assert_eq!(read, file);
        assert!(read.root_matches(&root));
        let store = read.store(&StoreId::Executor).unwrap();
        assert_eq!(store.state, StoreState::Plain);
        let opened = read
            .unwrap_store_key(
                &StoreId::Executor,
                KeyVersion::FIRST,
                &store.key.current_value().wraps[0],
                &root,
            )
            .unwrap();
        assert_eq!(opened.expose_secret(), executor_key.expose_secret());
        let main = &read.persons[&KeyOwner::Main];
        let opened = read
            .unwrap_person_key(
                &KeyOwner::Main,
                KeyVersion::FIRST,
                main.key.current_value(),
                &root,
            )
            .unwrap();
        assert_eq!(opened.expose_secret(), main_key.expose_secret());
        assert_eq!(read.held_agent_passphrase, None);
    }

    #[test]
    fn another_secret_does_not_match_the_root_check() {
        let id = KeyringId::random();
        let (file, _, _) = KeyringFile::create(id, &derive_root(&secret(1), &id));
        assert!(!file.root_matches(&derive_root(&secret(2), &id)));
        // The same secret under another keyring id is another root.
        assert!(!file.root_matches(&derive_root(&secret(1), &KeyringId::random())));
    }

    #[test]
    fn a_wrap_moved_to_another_entry_does_not_open() {
        let id = KeyringId::random();
        let root = derive_root(&secret(3), &id);
        let (file, _, _) = KeyringFile::create(id, &root);
        let main_wrap = file.persons[&KeyOwner::Main].key.current_value().clone();
        assert!(file
            .unwrap_person_key(
                &KeyOwner::User("eve".into()),
                KeyVersion::FIRST,
                &main_wrap,
                &root
            )
            .is_err());
        assert!(file
            .unwrap_person_key(&KeyOwner::Main, KeyVersion(2), &main_wrap, &root)
            .is_err());
    }

    #[test]
    fn a_damaged_or_foreign_file_is_corrupt_and_a_missing_one_is_none() {
        let folder = tempfile::tempdir().unwrap();
        let path = path_in(folder.path());
        assert!(KeyringFile::read(&path).unwrap().is_none());
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, b"{ not json").unwrap();
        assert!(matches!(
            KeyringFile::read(&path),
            Err(FileError::Corrupt { .. })
        ));
        let id = KeyringId::random();
        let (mut file, _, _) = KeyringFile::create(id, &derive_root(&secret(4), &id));
        file.format = 2;
        std::fs::write(&path, serde_json::to_vec(&file).unwrap()).unwrap();
        assert!(matches!(
            KeyringFile::read(&path),
            Err(FileError::Corrupt { .. })
        ));
    }

    #[cfg(unix)]
    #[test]
    fn the_file_is_0600_in_a_0700_folder() {
        use std::os::unix::fs::PermissionsExt;
        let folder = tempfile::tempdir().unwrap();
        let path = path_in(folder.path());
        let id = KeyringId::random();
        let (file, _, _) = KeyringFile::create(id, &derive_root(&secret(5), &id));
        file.write(&path).unwrap();
        let mode = |p: &Path| std::fs::metadata(p).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode(&path), 0o600);
        assert_eq!(mode(path.parent().unwrap()), 0o700);
    }
}

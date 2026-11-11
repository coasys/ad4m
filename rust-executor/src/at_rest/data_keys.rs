//! The data keys service: the keyring file opened under the operator secret, its keys held in
//! memory (each zeroed when dropped), and every change to it written to disk before the call
//! returns.
//!
//! Before [`DataKeys::unlock`] only the keyring id and whether a keyring exists are known;
//! every other call answers [`KeyringError::Sealed`]. The first unlock of a data folder makes
//! the keyring: the root key from the secret, a fresh executor store key and a data key for
//! the main agent. A wrong secret changes nothing, in memory or on disk.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use ad4m_keyring::{hkdf_subkey, KeyVersion, SecretKey, VersionedKey};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use zeroize::Zeroizing;

use super::keyring_file::{
    derive_root, FileError, KeyOwner, KeyringFile, KeyringId, PersonEntry, StoreEntry, StoreId,
    StoreKeyWraps, StoreState, Tombstone, TombstoneKind, VersionState, Wrapper,
};

/// Why a data keys call failed. No variant carries key material.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum KeyringError {
    #[error("storage-sealed: the keyring is not unlocked")]
    Sealed,
    #[error("wrong-secret: the secret does not open this keyring")]
    WrongSecret,
    #[error("corrupt-keyring: {0}")]
    CorruptKeyring(String),
    #[error("no such key owner")]
    UnknownOwner,
    #[error("no such store")]
    UnknownStore,
    #[error("the owner exists already")]
    OwnerExists,
    #[error("the store exists already")]
    StoreExists,
    #[error("the person still owns a store")]
    StillOwned,
    #[error("the store would have no owner left; destroy it instead")]
    LastOwner,
    #[error("the main agent's data key is not one that can be destroyed")]
    MainPerson,
    #[error("cannot write the keyring file: {0}")]
    Io(String),
}

impl From<FileError> for KeyringError {
    fn from(e: FileError) -> Self {
        match e {
            FileError::Io { .. } => KeyringError::Io(e.to_string()),
            FileError::Corrupt { .. } => KeyringError::CorruptKeyring(e.to_string()),
        }
    }
}

/// What a store subkey is for: each purpose its own key, so no key serves two ciphers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SubkeyPurpose {
    Sqlite,
    LanguageStorage,
    Rows,
    Archive,
}

/// Why a key was destroyed: the reason its tombstone keeps.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DestroyReason {
    /// A tenant left with its data deleted, or the operator removed it.
    TenantDeleted,
    /// A tenant's join did not complete.
    PendingExpired,
    /// A managed user (a legacy user or a guest) was removed.
    UserRemoved,
    /// The perspective was removed.
    PerspectiveRemoved,
    /// The store's last owner left it.
    OwnerRemoved,
}

impl DestroyReason {
    /// The reason as the tombstone writes it.
    pub fn as_str(self) -> &'static str {
        match self {
            DestroyReason::TenantDeleted => "tenant-deleted",
            DestroyReason::PendingExpired => "pending-expired",
            DestroyReason::UserRemoved => "user-removed",
            DestroyReason::PerspectiveRemoved => "perspective-removed",
            DestroyReason::OwnerRemoved => "owner-removed",
        }
    }
}

/// What [`DataKeys::destroy_person`] did.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DestroyReport {
    /// When the keyring file without the person's keys was on disk.
    pub key_destroyed_at: DateTime<Utc>,
    /// When no copy of the person's keys is left anywhere the executor made one. With no
    /// backups of the keyring, the same time as `key_destroyed_at`.
    pub shredded_everywhere_at: DateTime<Utc>,
}

impl SubkeyPurpose {
    /// The HKDF `info` of the purpose.
    pub fn info(self) -> &'static str {
        match self {
            SubkeyPurpose::Sqlite => "ad4m:at-rest:sqlite:v1",
            SubkeyPurpose::LanguageStorage => "ad4m:at-rest:lang:v1",
            SubkeyPurpose::Rows => "ad4m:at-rest:row:v1",
            SubkeyPurpose::Archive => "ad4m:at-rest:archive:v1",
        }
    }
}

/// What an unlock did.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UnlockOutcome {
    /// The unlock made the keyring: the data folder had none.
    pub created: bool,
}

/// `storage.keys`: the keyring's entries, without a key or a wrap.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct KeyringSummary {
    pub persons: Vec<PersonSummary>,
    pub stores: Vec<StoreSummary>,
    pub tombstones: Vec<TombstoneSummary>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PersonSummary {
    /// `main` or `user:<username>`.
    pub owner: String,
    pub versions: Vec<u32>,
    pub current: u32,
    pub created_at: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StoreSummary {
    /// `executor`, `person:<owner>` or `perspective:<uuid>`.
    pub id: String,
    pub state: StoreStateName,
    pub current: u32,
    pub retiring: Vec<u32>,
    pub owners: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "lowercase")]
#[ts(export)]
pub enum StoreStateName {
    Plain,
    Migrating,
    Encrypted,
}

impl From<&StoreState> for StoreStateName {
    fn from(state: &StoreState) -> Self {
        match state {
            StoreState::Plain => StoreStateName::Plain,
            StoreState::Migrating { .. } => StoreStateName::Migrating,
            StoreState::Encrypted => StoreStateName::Encrypted,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TombstoneSummary {
    pub kind: TombstoneKindName,
    pub destroyed_at: String,
    pub reason: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "lowercase")]
#[ts(export)]
pub enum TombstoneKindName {
    Person,
    Store,
}

/// The keys of an open keyring.
struct OpenKeys {
    file: KeyringFile,
    root: SecretKey,
    persons: HashMap<(KeyOwner, KeyVersion), SecretKey>,
    stores: HashMap<(StoreId, KeyVersion), SecretKey>,
}

enum Inner {
    /// Not unlocked: the id of the keyring on disk, or the one the first unlock will make.
    Sealed {
        keyring_id: KeyringId,
        file: Option<Box<KeyringFile>>,
    },
    Open(Box<OpenKeys>),
}

/// The data keys of one data folder.
pub struct DataKeys {
    path: PathBuf,
    inner: Mutex<Inner>,
}

impl DataKeys {
    /// The data keys of the keyring file at `path`, sealed. Without a file, a keyring id is made
    /// now, so the status shows the id the first unlock will write. A damaged file is
    /// [`KeyringError::CorruptKeyring`].
    pub fn load(path: PathBuf) -> Result<Self, KeyringError> {
        let file = KeyringFile::read(&path)?;
        let keyring_id = file
            .as_ref()
            .map_or_else(KeyringId::random, |f| f.keyring_id);
        Ok(DataKeys {
            path,
            inner: Mutex::new(Inner::Sealed {
                keyring_id,
                file: file.map(Box::new),
            }),
        })
    }

    /// Data keys for a keyring file that did not read: sealed, without reading `path` again.
    /// The caller refuses every unlock (the gate answers `corrupt-keyring`), so the file is
    /// never written over.
    pub(crate) fn unreadable(path: PathBuf) -> Self {
        DataKeys {
            path,
            inner: Mutex::new(Inner::Sealed {
                keyring_id: KeyringId::random(),
                file: None,
            }),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, Inner> {
        self.inner.lock().unwrap_or_else(|p| p.into_inner())
    }

    pub fn keyring_id(&self) -> KeyringId {
        match &*self.lock() {
            Inner::Sealed { keyring_id, .. } => *keyring_id,
            Inner::Open(open) => open.file.keyring_id,
        }
    }

    pub fn is_open(&self) -> bool {
        matches!(&*self.lock(), Inner::Open(_))
    }

    /// The root key's generation, once open.
    pub fn root_generation(&self) -> Option<u32> {
        match &*self.lock() {
            Inner::Sealed { .. } => None,
            Inner::Open(open) => Some(open.file.root_generation),
        }
    }

    /// Opens the keyring under `secret`, or makes it on the first unlock. Unlocking an open
    /// keyring with its own secret changes nothing; with another it is `WrongSecret`. The
    /// secret is dropped (zeroed) before this returns.
    pub fn unlock(&self, secret: Zeroizing<[u8; 32]>) -> Result<UnlockOutcome, KeyringError> {
        let secret = SecretKey::from_slice(&*secret).expect("32 bytes");
        let mut inner = self.lock();
        match &*inner {
            Inner::Open(open) => {
                let root = derive_root(&secret, &open.file.keyring_id);
                if open.file.root_matches(&root) {
                    Ok(UnlockOutcome { created: false })
                } else {
                    Err(KeyringError::WrongSecret)
                }
            }
            Inner::Sealed {
                keyring_id,
                file: None,
            } => {
                let root = derive_root(&secret, keyring_id);
                let (file, executor_key, main_key) = KeyringFile::create(*keyring_id, &root);
                file.write(&self.path)
                    .map_err(|e| KeyringError::Io(e.to_string()))?;
                let mut open = OpenKeys {
                    file,
                    root,
                    persons: HashMap::new(),
                    stores: HashMap::new(),
                };
                open.persons
                    .insert((KeyOwner::Main, KeyVersion::FIRST), main_key);
                open.stores
                    .insert((StoreId::Executor, KeyVersion::FIRST), executor_key);
                *inner = Inner::Open(Box::new(open));
                Ok(UnlockOutcome { created: true })
            }
            Inner::Sealed {
                keyring_id,
                file: Some(file),
            } => {
                let root = derive_root(&secret, keyring_id);
                if !file.root_matches(&root) {
                    return Err(KeyringError::WrongSecret);
                }
                let open = open_all((**file).clone(), root)?;
                *inner = Inner::Open(Box::new(open));
                Ok(UnlockOutcome { created: false })
            }
        }
    }

    fn with_open<T>(
        &self,
        f: impl FnOnce(&mut OpenKeys) -> Result<T, KeyringError>,
    ) -> Result<T, KeyringError> {
        match &mut *self.lock() {
            Inner::Sealed { .. } => Err(KeyringError::Sealed),
            Inner::Open(open) => f(open),
        }
    }

    /// Writes `next` (its `seq` one higher) and makes it the keyring in memory. A failed write
    /// leaves the old file and the old keyring.
    fn commit(path: &Path, open: &mut OpenKeys, mut next: KeyringFile) -> Result<(), KeyringError> {
        next.seq = open.file.seq + 1;
        next.write(path)
            .map_err(|e| KeyringError::Io(e.to_string()))?;
        open.file = next;
        Ok(())
    }

    /// The state of every store, the executor's first.
    pub fn store_states(&self) -> Result<Vec<(StoreId, StoreState)>, KeyringError> {
        self.with_open(|open| {
            Ok(open
                .file
                .all_stores()
                .map(|s| (s.id.clone(), s.state.clone()))
                .collect())
        })
    }

    /// `storage.keys`.
    pub fn summary(&self) -> Result<KeyringSummary, KeyringError> {
        self.with_open(|open| Ok(summarise(&open.file)))
    }

    /// Makes sure `owner` is a person: a data key wrapped under the root key, and the person's
    /// own store (`person:<owner>`) with its key wrapped under that data key, both in one write
    /// of the keyring file. Answers the current version of the person's data key. A person
    /// who exists keeps its keys; one made before persons had their own store gets it now.
    pub fn create_person(&self, owner: KeyOwner) -> Result<KeyVersion, KeyringError> {
        let path = self.path.clone();
        self.with_open(|open| {
            let store = StoreId::Person(owner.clone());
            let existing = open.file.persons.get(&owner).map(|p| p.key.current());
            if let Some(version) = existing {
                if open.file.store(&store).is_some() {
                    return Ok(version);
                }
            }
            let mut next = open.file.clone();
            let new_person = match existing {
                Some(_) => None,
                None => {
                    let key = SecretKey::random();
                    let wrapped = next.wrap_person_key(&owner, KeyVersion::FIRST, &key, &open.root);
                    next.persons.insert(
                        owner.clone(),
                        PersonEntry {
                            owner: owner.clone(),
                            created_at: Utc::now(),
                            key: VersionedKey::new(wrapped),
                        },
                    );
                    Some(key)
                }
            };
            let version = existing.unwrap_or(KeyVersion::FIRST);
            let store_key = SecretKey::random();
            let wrap = {
                let person_key = match &new_person {
                    Some(key) => key,
                    None => open
                        .persons
                        .get(&(owner.clone(), version))
                        .ok_or(KeyringError::UnknownOwner)?,
                };
                next.wrap_store_key(
                    &store,
                    KeyVersion::FIRST,
                    &store_key,
                    &Wrapper::Person {
                        owner: owner.clone(),
                        version,
                    },
                    person_key,
                )
            };
            next.stores.insert(
                store.clone(),
                StoreEntry {
                    id: store.clone(),
                    state: StoreState::Plain,
                    key: VersionedKey::new(StoreKeyWraps {
                        state: VersionState::Active,
                        wraps: vec![wrap],
                    }),
                },
            );
            Self::commit(&path, open, next)?;
            if let Some(key) = new_person {
                open.persons.insert((owner, KeyVersion::FIRST), key);
            }
            open.stores.insert((store, KeyVersion::FIRST), store_key);
            Ok(version)
        })
    }

    /// Whether `owner` is a person of this keyring.
    pub fn has_person(&self, owner: &KeyOwner) -> Result<bool, KeyringError> {
        self.with_open(|open| Ok(open.file.persons.contains_key(owner)))
    }

    /// The stores `owner` holds a wrap of, its own person store left out.
    pub fn stores_of(&self, owner: &KeyOwner) -> Result<Vec<StoreId>, KeyringError> {
        self.with_open(|open| {
            Ok(open
                .file
                .all_stores()
                .filter(|s| s.id != StoreId::Person(owner.clone()) && wrapped_by(s, owner))
                .map(|s| s.id.clone())
                .collect())
        })
    }

    /// Destroys a person: every version of its data key and its own person store, leaving a
    /// tombstone for each, in one write of the keyring file. Refused while any other store
    /// still has the person as an owner (`StillOwned`): the caller takes the person out of
    /// every store first, and deletes the rows of its person store, so the keys go after the
    /// data they protect. The main agent's person is not one that can be destroyed.
    pub fn destroy_person(
        &self,
        owner: &KeyOwner,
        reason: DestroyReason,
    ) -> Result<DestroyReport, KeyringError> {
        if *owner == KeyOwner::Main {
            return Err(KeyringError::MainPerson);
        }
        let path = self.path.clone();
        self.with_open(|open| {
            if !open.file.persons.contains_key(owner) {
                return Err(KeyringError::UnknownOwner);
            }
            let own = StoreId::Person(owner.clone());
            if open
                .file
                .all_stores()
                .any(|s| s.id != own && wrapped_by(s, owner))
            {
                return Err(KeyringError::StillOwned);
            }
            let now = Utc::now();
            let tombstone = |kind, id: &str| Tombstone {
                destroyed_at: now,
                ..Tombstone::new(kind, id, reason.as_str())
            };
            let mut next = open.file.clone();
            if next.stores.remove(&own).is_some() {
                next.tombstones
                    .push(tombstone(TombstoneKind::Store, &own.to_string()));
            }
            next.persons.remove(owner);
            next.tombstones
                .push(tombstone(TombstoneKind::Person, &owner.to_string()));
            Self::commit(&path, open, next)?;
            open.stores.retain(|(s, _), _| *s != own);
            open.persons.retain(|(o, _), _| o != owner);
            Ok(DestroyReport {
                key_destroyed_at: now,
                shredded_everywhere_at: now,
            })
        })
    }

    /// A key for a new store, wrapped under each owner's current data key (under the root key
    /// when `owners` is empty). The store starts [`StoreState::Plain`].
    pub fn create_store(&self, id: StoreId, owners: &[KeyOwner]) -> Result<(), KeyringError> {
        let path = self.path.clone();
        self.with_open(|open| {
            if open.file.store(&id).is_some() {
                return Err(KeyringError::StoreExists);
            }
            let key = SecretKey::random();
            let mut next = open.file.clone();
            let wraps = if owners.is_empty() {
                vec![next.wrap_store_key(
                    &id,
                    KeyVersion::FIRST,
                    &key,
                    &Wrapper::Root {
                        generation: next.root_generation,
                    },
                    &open.root,
                )]
            } else {
                owners
                    .iter()
                    .map(|owner| wrap_for_owner(open, &next, &id, KeyVersion::FIRST, &key, owner))
                    .collect::<Result<Vec<_>, _>>()?
            };
            next.stores.insert(
                id.clone(),
                StoreEntry {
                    id: id.clone(),
                    state: StoreState::Plain,
                    key: VersionedKey::new(StoreKeyWraps {
                        state: VersionState::Active,
                        wraps,
                    }),
                },
            );
            Self::commit(&path, open, next)?;
            open.stores.insert((id, KeyVersion::FIRST), key);
            Ok(())
        })
    }

    /// Wraps every held version of store `id`'s key under `owner`'s current data key too.
    pub fn add_owner(&self, id: &StoreId, owner: &KeyOwner) -> Result<(), KeyringError> {
        let path = self.path.clone();
        self.with_open(|open| {
            let store = open.file.store(id).ok_or(KeyringError::UnknownStore)?;
            if wrapped_by(store, owner) {
                return Err(KeyringError::OwnerExists);
            }
            let mut next = open.file.clone();
            let versions: Vec<KeyVersion> = store.key.iter().map(|(v, _)| v).collect();
            for version in versions {
                let key = open.stores.get(&(id.clone(), version)).ok_or_else(|| {
                    KeyringError::CorruptKeyring(format!("{id} {version} not held"))
                })?;
                let wrap = wrap_for_owner(open, &next, id, version, key, owner)?;
                store_mut(&mut next, id)?
                    .key
                    .get_mut(version)
                    .map_err(|e| KeyringError::CorruptKeyring(e.to_string()))?
                    .wraps
                    .push(wrap);
            }
            Self::commit(&path, open, next)
        })
    }

    /// Removes `owner`'s wraps of store `id`'s key and answers the owners left. When `owner` is
    /// the last one the keyring is left as it is and the answer is empty: the caller deletes
    /// the store's files, then calls [`DataKeys::destroy_store`], so the key goes after the
    /// data it protects and never before.
    pub fn remove_owner(
        &self,
        id: &StoreId,
        owner: &KeyOwner,
    ) -> Result<Vec<KeyOwner>, KeyringError> {
        let path = self.path.clone();
        self.with_open(|open| {
            let store = open.file.store(id).ok_or(KeyringError::UnknownStore)?;
            if !wrapped_by(store, owner) {
                return Err(KeyringError::UnknownOwner);
            }
            let left: Vec<KeyOwner> = owners_of(store)
                .into_iter()
                .filter(|o| o != owner)
                .collect();
            if left.is_empty() {
                return Ok(left);
            }
            let mut next = open.file.clone();
            let entry = store_mut(&mut next, id)?;
            let versions: Vec<KeyVersion> = entry.key.iter().map(|(v, _)| v).collect();
            for version in versions {
                let wraps = &mut entry
                    .key
                    .get_mut(version)
                    .map_err(|e| KeyringError::CorruptKeyring(e.to_string()))?
                    .wraps;
                wraps.retain(
                    |w| !matches!(&w.wrapper, Wrapper::Person { owner: o, .. } if o == owner),
                );
                if wraps.is_empty() {
                    return Err(KeyringError::LastOwner);
                }
            }
            Self::commit(&path, open, next)?;
            Ok(left)
        })
    }

    /// The persons that hold store `id`'s key, in the order they were added.
    pub fn store_owners(&self, id: &StoreId) -> Result<Vec<KeyOwner>, KeyringError> {
        self.with_open(|open| {
            let store = open.file.store(id).ok_or(KeyringError::UnknownStore)?;
            Ok(owners_of(store))
        })
    }

    /// Makes the owners of store `id` exactly `owners` (each made a person when it is not one
    /// yet): the store is made when it does not exist, each new owner gets a wrap, then each
    /// owner not listed loses its own. Wraps are added before any is removed, so the key always
    /// has one. `owners` must not be empty: a store with no owner is destroyed, not emptied.
    pub fn set_store_owners(&self, id: &StoreId, owners: &[KeyOwner]) -> Result<(), KeyringError> {
        if owners.is_empty() {
            return Err(KeyringError::LastOwner);
        }
        self.ensure_store(id, owners)?;
        for owner in self.store_owners(id)? {
            if !owners.contains(&owner) {
                self.remove_owner(id, &owner)?;
            }
        }
        Ok(())
    }

    /// Destroys store `id`'s key, leaving a tombstone. The executor's own store is not one
    /// that can be destroyed: `UnknownStore`.
    pub fn destroy_store(&self, id: &StoreId, reason: DestroyReason) -> Result<(), KeyringError> {
        let path = self.path.clone();
        self.with_open(|open| {
            if !open.file.stores.contains_key(id) {
                return Err(KeyringError::UnknownStore);
            }
            let mut next = open.file.clone();
            next.stores.remove(id);
            next.tombstones.push(Tombstone::new(
                TombstoneKind::Store,
                &id.to_string(),
                reason.as_str(),
            ));
            Self::commit(&path, open, next)?;
            open.stores.retain(|(s, _), _| s != id);
            Ok(())
        })
    }

    /// A subkey of store `id`'s key for `purpose`: of `version`, or of the current version.
    /// Returns the version it is of.
    pub fn subkey(
        &self,
        id: &StoreId,
        version: Option<KeyVersion>,
        purpose: SubkeyPurpose,
    ) -> Result<(KeyVersion, SecretKey), KeyringError> {
        self.with_open(|open| {
            let store = open.file.store(id).ok_or(KeyringError::UnknownStore)?;
            let version = version.unwrap_or_else(|| store.key.current());
            let key = open
                .stores
                .get(&(id.clone(), version))
                .ok_or(KeyringError::UnknownStore)?;
            Ok((version, hkdf_subkey(key, &[], purpose.info())))
        })
    }

    /// Sets store `id`'s state, written to the keyring file before this returns.
    pub fn set_store_state(&self, id: &StoreId, state: StoreState) -> Result<(), KeyringError> {
        let path = self.path.clone();
        self.with_open(|open| {
            open.file.store(id).ok_or(KeyringError::UnknownStore)?;
            let mut next = open.file.clone();
            store_mut(&mut next, id)?.state = state;
            Self::commit(&path, open, next)
        })
    }

    /// Makes sure store `id` exists and each of `owners` (each made a person when it is not
    /// one yet) holds its key. A store made here starts [`StoreState::Plain`].
    pub fn ensure_store(&self, id: &StoreId, owners: &[KeyOwner]) -> Result<(), KeyringError> {
        for owner in owners {
            if !self.has_person(owner)? {
                self.create_person(owner.clone())?;
            }
        }
        match self.create_store(id.clone(), owners) {
            Ok(()) => return Ok(()),
            Err(KeyringError::StoreExists) => {}
            Err(e) => return Err(e),
        }
        for owner in owners {
            match self.add_owner(id, owner) {
                Ok(()) | Err(KeyringError::OwnerExists) => {}
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    /// Store `id`'s state, when it exists.
    pub fn store_state(&self, id: &StoreId) -> Result<Option<StoreState>, KeyringError> {
        self.with_open(|open| Ok(open.file.store(id).map(|s| s.state.clone())))
    }

    /// Whether the languages' storage folders hold only sealed files.
    pub fn language_storage_state(&self) -> Result<StoreState, KeyringError> {
        self.with_open(|open| Ok(open.file.language_storage.clone()))
    }

    /// Sets the languages' storage state, written to the keyring file before this returns.
    pub fn set_language_storage_state(&self, state: StoreState) -> Result<(), KeyringError> {
        let path = self.path.clone();
        self.with_open(|open| {
            let mut next = open.file.clone();
            next.language_storage = state;
            Self::commit(&path, open, next)
        })
    }

    /// Whether store `id` exists.
    pub fn has_store(&self, id: &StoreId) -> Result<bool, KeyringError> {
        self.with_open(|open| Ok(open.file.store(id).is_some()))
    }
}

fn store_mut<'a>(
    file: &'a mut KeyringFile,
    id: &StoreId,
) -> Result<&'a mut StoreEntry, KeyringError> {
    match id {
        StoreId::Executor => Ok(&mut file.executor_store),
        other => file.stores.get_mut(other).ok_or(KeyringError::UnknownStore),
    }
}

fn wrapped_by(store: &StoreEntry, owner: &KeyOwner) -> bool {
    store.key.iter().any(|(_, v)| {
        v.wraps
            .iter()
            .any(|w| matches!(&w.wrapper, Wrapper::Person { owner: o, .. } if o == owner))
    })
}

/// Every person with a wrap of any version of `store`'s key, each once, in wrap order.
fn owners_of(store: &StoreEntry) -> Vec<KeyOwner> {
    let mut owners = Vec::new();
    for (_, version) in store.key.iter() {
        for wrap in &version.wraps {
            if let Wrapper::Person { owner, .. } = &wrap.wrapper {
                if !owners.contains(owner) {
                    owners.push(owner.clone());
                }
            }
        }
    }
    owners
}

/// A wrap of `key` (version `version` of store `id`) under `owner`'s current data key.
fn wrap_for_owner(
    open: &OpenKeys,
    file: &KeyringFile,
    id: &StoreId,
    version: KeyVersion,
    key: &SecretKey,
    owner: &KeyOwner,
) -> Result<super::keyring_file::StoreWrap, KeyringError> {
    let person = file.persons.get(owner).ok_or(KeyringError::UnknownOwner)?;
    let person_version = person.key.current();
    let person_key = open
        .persons
        .get(&(owner.clone(), person_version))
        .ok_or(KeyringError::UnknownOwner)?;
    Ok(file.wrap_store_key(
        id,
        version,
        key,
        &Wrapper::Person {
            owner: owner.clone(),
            version: person_version,
        },
        person_key,
    ))
}

/// Opens every key of `file` under `root`. A wrap that does not open is a damaged keyring:
/// the root check passed, so the secret is right.
fn open_all(file: KeyringFile, root: SecretKey) -> Result<OpenKeys, KeyringError> {
    let corrupt = |what: String| KeyringError::CorruptKeyring(format!("{what} does not open"));
    let mut persons = HashMap::new();
    for (owner, entry) in &file.persons {
        for (version, wrapped) in entry.key.iter() {
            let key = file
                .unwrap_person_key(owner, version, wrapped, &root)
                .map_err(|_| corrupt(format!("the data key of {owner} {version}")))?;
            persons.insert((owner.clone(), version), key);
        }
    }
    let mut stores = HashMap::new();
    for store in file.all_stores() {
        for (version, wraps) in store.key.iter() {
            let key = wraps
                .wraps
                .iter()
                .find_map(|w| {
                    let under = match &w.wrapper {
                        Wrapper::Root { .. } => Some(&root),
                        Wrapper::Person { owner, version } => {
                            persons.get(&(owner.clone(), *version))
                        }
                    }?;
                    file.unwrap_store_key(&store.id, version, w, under).ok()
                })
                .ok_or_else(|| corrupt(format!("the key of store {} {version}", store.id)))?;
            stores.insert((store.id.clone(), version), key);
        }
    }
    Ok(OpenKeys {
        file,
        root,
        persons,
        stores,
    })
}

fn summarise(file: &KeyringFile) -> KeyringSummary {
    KeyringSummary {
        persons: file
            .persons
            .values()
            .map(|p| PersonSummary {
                owner: p.owner.to_string(),
                versions: p.key.iter().map(|(v, _)| v.0).collect(),
                current: p.key.current().0,
                created_at: p.created_at.to_rfc3339(),
            })
            .collect(),
        stores: file
            .all_stores()
            .map(|s| StoreSummary {
                id: s.id.to_string(),
                state: (&s.state).into(),
                current: s.key.current().0,
                retiring: s
                    .key
                    .iter()
                    .filter(|(_, w)| w.state == VersionState::Retiring)
                    .map(|(v, _)| v.0)
                    .collect(),
                owners: s.owners().iter().map(ToString::to_string).collect(),
            })
            .collect(),
        tombstones: file
            .tombstones
            .iter()
            .map(|t| TombstoneSummary {
                kind: match t.kind {
                    TombstoneKind::Person => TombstoneKindName::Person,
                    TombstoneKind::Store => TombstoneKindName::Store,
                },
                destroyed_at: t.destroyed_at.to_rfc3339(),
                reason: t.reason.clone(),
            })
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn secret(byte: u8) -> Zeroizing<[u8; 32]> {
        Zeroizing::new([byte; 32])
    }

    fn sealed() -> (tempfile::TempDir, DataKeys) {
        let folder = tempfile::tempdir().unwrap();
        let keys = DataKeys::load(super::super::keyring_file::path_in(folder.path())).unwrap();
        (folder, keys)
    }

    #[test]
    fn every_call_but_unlock_is_sealed_before_the_unlock() {
        let (_folder, keys) = sealed();
        assert!(!keys.is_open());
        assert_eq!(keys.summary(), Err(KeyringError::Sealed));
        assert_eq!(keys.store_states(), Err(KeyringError::Sealed));
        assert_eq!(
            keys.create_person(KeyOwner::User("a".into())),
            Err(KeyringError::Sealed)
        );
        assert_eq!(
            keys.create_store(StoreId::Perspective(uuid::Uuid::new_v4()), &[]),
            Err(KeyringError::Sealed)
        );
        assert!(matches!(
            keys.subkey(&StoreId::Executor, None, SubkeyPurpose::Sqlite),
            Err(KeyringError::Sealed)
        ));
        assert!(!keys.path().exists(), "nothing is written while sealed");
    }

    #[test]
    fn the_first_unlock_makes_the_keyring_with_the_id_shown_before_it() {
        let (_folder, keys) = sealed();
        let before = keys.keyring_id();
        assert_eq!(
            keys.unlock(secret(1)).unwrap(),
            UnlockOutcome { created: true }
        );
        assert_eq!(keys.keyring_id(), before);
        let file = KeyringFile::read(keys.path()).unwrap().unwrap();
        assert_eq!(file.keyring_id, before);
        assert_eq!(keys.root_generation(), Some(1));
        let summary = keys.summary().unwrap();
        assert_eq!(summary.persons.len(), 1);
        assert_eq!(summary.persons[0].owner, "main");
        assert_eq!(summary.stores[0].id, "executor");
        assert_eq!(summary.stores[0].state, StoreStateName::Plain);
    }

    #[test]
    fn a_restart_opens_the_same_keys_with_the_same_secret() {
        let (folder, keys) = sealed();
        keys.unlock(secret(9)).unwrap();
        keys.create_person(KeyOwner::User("alice".into())).unwrap();
        let perspective = StoreId::Perspective(uuid::Uuid::new_v4());
        keys.create_store(perspective.clone(), &[KeyOwner::User("alice".into())])
            .unwrap();
        let (_, before) = keys
            .subkey(&perspective, None, SubkeyPurpose::Sqlite)
            .unwrap();
        let id = keys.keyring_id();
        drop(keys);

        let again = DataKeys::load(super::super::keyring_file::path_in(folder.path())).unwrap();
        assert_eq!(again.keyring_id(), id);
        assert!(!again.is_open());
        assert_eq!(
            again.unlock(secret(9)).unwrap(),
            UnlockOutcome { created: false }
        );
        let (version, after) = again
            .subkey(&perspective, None, SubkeyPurpose::Sqlite)
            .unwrap();
        assert_eq!(version, KeyVersion::FIRST);
        assert_eq!(after.expose_secret(), before.expose_secret());
        let (_, other) = again
            .subkey(&perspective, None, SubkeyPurpose::LanguageStorage)
            .unwrap();
        assert_ne!(other.expose_secret(), after.expose_secret());
    }

    #[test]
    fn a_wrong_secret_is_refused_and_the_file_keeps_every_byte() {
        let (folder, keys) = sealed();
        keys.unlock(secret(1)).unwrap();
        let path = keys.path().to_path_buf();
        drop(keys);
        let bytes = std::fs::read(&path).unwrap();
        let modified = std::fs::metadata(&path).unwrap().modified().unwrap();

        let keys = DataKeys::load(super::super::keyring_file::path_in(folder.path())).unwrap();
        assert_eq!(keys.unlock(secret(2)), Err(KeyringError::WrongSecret));
        assert!(!keys.is_open());
        assert_eq!(keys.summary(), Err(KeyringError::Sealed));
        assert_eq!(std::fs::read(&path).unwrap(), bytes);
        assert_eq!(
            std::fs::metadata(&path).unwrap().modified().unwrap(),
            modified
        );
        // The right one still opens it, and an open keyring refuses another secret too.
        keys.unlock(secret(1)).unwrap();
        assert_eq!(keys.unlock(secret(3)), Err(KeyringError::WrongSecret));
        assert!(keys.unlock(secret(1)).is_ok());
    }

    #[test]
    fn a_damaged_keyring_does_not_load() {
        let (folder, keys) = sealed();
        keys.unlock(secret(1)).unwrap();
        std::fs::write(keys.path(), b"{}").unwrap();
        assert!(matches!(
            DataKeys::load(super::super::keyring_file::path_in(folder.path())),
            Err(KeyringError::CorruptKeyring(_))
        ));
    }

    #[test]
    fn owners_and_stores_follow_their_rules_and_every_change_is_on_disk() {
        let (_folder, keys) = sealed();
        keys.unlock(secret(5)).unwrap();
        let alice = KeyOwner::User("alice".into());
        let bob = KeyOwner::User("bob".into());
        assert_eq!(keys.create_person(alice.clone()), Ok(KeyVersion::FIRST));
        let written = std::fs::read(keys.path()).unwrap();
        assert_eq!(
            keys.create_person(alice.clone()),
            Ok(KeyVersion::FIRST),
            "a person made twice keeps its keys"
        );
        assert_eq!(
            std::fs::read(keys.path()).unwrap(),
            written,
            "and writes nothing"
        );
        let store = StoreId::Perspective(uuid::Uuid::new_v4());
        assert_eq!(
            keys.create_store(store.clone(), std::slice::from_ref(&bob)),
            Err(KeyringError::UnknownOwner)
        );
        keys.create_store(store.clone(), std::slice::from_ref(&alice))
            .unwrap();
        assert_eq!(
            keys.create_store(store.clone(), &[]),
            Err(KeyringError::StoreExists)
        );
        keys.create_person(bob.clone()).unwrap();
        keys.add_owner(&store, &bob).unwrap();
        assert_eq!(keys.add_owner(&store, &bob), Err(KeyringError::OwnerExists));
        assert_eq!(
            keys.destroy_person(&alice, DestroyReason::TenantDeleted),
            Err(KeyringError::StillOwned)
        );
        assert_eq!(
            keys.remove_owner(&store, &alice).unwrap(),
            vec![bob.clone()]
        );
        assert_eq!(
            keys.remove_owner(&store, &bob).unwrap(),
            Vec::<KeyOwner>::new(),
            "the last owner keeps its wrap until the store is destroyed"
        );
        assert_eq!(keys.store_owners(&store).unwrap(), vec![bob.clone()]);
        keys.destroy_person(&alice, DestroyReason::TenantDeleted)
            .unwrap();

        let on_disk = KeyringFile::read(keys.path()).unwrap().unwrap();
        assert_eq!(on_disk.seq, 6);
        assert_eq!(summarise(&on_disk), keys.summary().unwrap());
        let summary = keys.summary().unwrap();
        let entry = summary
            .stores
            .iter()
            .find(|s| s.id == store.to_string())
            .unwrap();
        assert_eq!(entry.owners, vec!["user:bob".to_string()]);
        // Alice's own store and her data key.
        assert_eq!(
            summary
                .tombstones
                .iter()
                .map(|t| t.kind)
                .collect::<Vec<_>>(),
            vec![TombstoneKindName::Store, TombstoneKindName::Person]
        );
        let raw = std::fs::read_to_string(keys.path()).unwrap();
        assert!(
            !raw.contains("user:alice"),
            "a tombstone keeps only the hash of the id"
        );

        keys.destroy_store(&store, DestroyReason::PerspectiveRemoved)
            .unwrap();
        assert!(matches!(
            keys.subkey(&store, None, SubkeyPurpose::Rows),
            Err(KeyringError::UnknownStore)
        ));
        assert_eq!(
            keys.destroy_store(&StoreId::Executor, DestroyReason::OwnerRemoved),
            Err(KeyringError::UnknownStore)
        );
    }

    /// The main agent's person and an unknown person are not destroyed, and a refusal writes
    /// nothing. A destroyed person reports one time for its key and for the last copy of it,
    /// lists no store, and can be made again as a new person.
    #[test]
    fn a_destroyed_person_reports_one_time_and_main_is_never_destroyed() {
        let (_folder, keys) = sealed();
        keys.unlock(secret(12)).unwrap();
        let before = std::fs::read(keys.path()).unwrap();
        assert_eq!(
            keys.destroy_person(&KeyOwner::Main, DestroyReason::UserRemoved),
            Err(KeyringError::MainPerson)
        );
        let dave = KeyOwner::User("dave".into());
        assert_eq!(
            keys.destroy_person(&dave, DestroyReason::UserRemoved),
            Err(KeyringError::UnknownOwner)
        );
        assert_eq!(std::fs::read(keys.path()).unwrap(), before);

        let first = keys.create_person(dave.clone()).unwrap();
        assert!(keys.has_store(&StoreId::Person(dave.clone())).unwrap());
        assert!(
            keys.stores_of(&dave).unwrap().is_empty(),
            "its own store is not listed"
        );
        let report = keys
            .destroy_person(&dave, DestroyReason::UserRemoved)
            .unwrap();
        assert_eq!(report.key_destroyed_at, report.shredded_everywhere_at);
        assert!(!keys.has_person(&dave).unwrap());
        assert!(!keys.has_store(&StoreId::Person(dave.clone())).unwrap());
        let tombstones = keys.summary().unwrap().tombstones;
        assert_eq!(tombstones.len(), 2, "its store and its key");
        assert!(tombstones
            .iter()
            .all(|t| t.destroyed_at == report.key_destroyed_at.to_rfc3339()));
        assert!(tombstones.iter().all(|t| t.reason == "user-removed"));
        assert_eq!(keys.create_person(dave.clone()).unwrap(), first);
    }

    #[cfg(unix)]
    #[test]
    fn a_failed_write_leaves_the_old_keyring_on_disk_and_in_memory() {
        use std::os::unix::fs::PermissionsExt;
        let (_folder, keys) = sealed();
        keys.unlock(secret(6)).unwrap();
        let before = std::fs::read(keys.path()).unwrap();
        let folder = keys.path().parent().unwrap().to_path_buf();
        // A folder the executor cannot write to: the temporary file cannot be made.
        std::fs::set_permissions(&folder, std::fs::Permissions::from_mode(0o500)).unwrap();
        let refused = keys.create_person(KeyOwner::User("carol".into()));
        std::fs::set_permissions(&folder, std::fs::Permissions::from_mode(0o700)).unwrap();
        if nix_is_root() {
            // Root writes to any folder: the failure cannot be made this way.
            return;
        }
        assert!(matches!(refused, Err(KeyringError::Io(_))), "{refused:?}");
        assert_eq!(std::fs::read(keys.path()).unwrap(), before);
        assert_eq!(keys.summary().unwrap().persons.len(), 1);
        keys.create_person(KeyOwner::User("carol".into())).unwrap();
    }

    /// A perspective with two owners: each removal drops only that owner's wrap, the key still
    /// opens through the other after a restart, and the last removal leaves the store to
    /// `destroy_store`, after which no entry and no wrap of it is left, on disk or after a
    /// restart.
    #[test]
    fn a_store_lives_while_an_owner_holds_it_and_its_key_goes_with_the_last() {
        let (folder, keys) = sealed();
        keys.unlock(secret(11)).unwrap();
        let alice = KeyOwner::User("alice".into());
        let bob = KeyOwner::User("bob".into());
        let store = StoreId::Perspective(uuid::Uuid::new_v4());
        keys.set_store_owners(&store, &[alice.clone(), bob.clone()])
            .unwrap();
        assert_eq!(
            keys.store_owners(&store).unwrap(),
            vec![alice.clone(), bob.clone()]
        );
        let (_, rows) = keys.subkey(&store, None, SubkeyPurpose::Rows).unwrap();

        assert_eq!(
            keys.remove_owner(&store, &alice).unwrap(),
            vec![bob.clone()]
        );
        drop(keys);
        let keys = DataKeys::load(super::super::keyring_file::path_in(folder.path())).unwrap();
        keys.unlock(secret(11)).unwrap();
        let (_, again) = keys.subkey(&store, None, SubkeyPurpose::Rows).unwrap();
        assert_eq!(again.expose_secret(), rows.expose_secret());
        assert_eq!(keys.store_owners(&store).unwrap(), vec![bob.clone()]);

        assert!(keys.remove_owner(&store, &bob).unwrap().is_empty());
        keys.destroy_store(&store, DestroyReason::PerspectiveRemoved)
            .unwrap();
        let raw = std::fs::read_to_string(keys.path()).unwrap();
        assert!(
            !raw.contains(&store.to_string()),
            "no entry names the store"
        );
        assert!(matches!(
            keys.subkey(&store, None, SubkeyPurpose::Rows),
            Err(KeyringError::UnknownStore)
        ));
        drop(keys);
        let keys = DataKeys::load(super::super::keyring_file::path_in(folder.path())).unwrap();
        keys.unlock(secret(11)).unwrap();
        assert!(matches!(
            keys.subkey(&store, None, SubkeyPurpose::Rows),
            Err(KeyringError::UnknownStore)
        ));
        let summary = keys.summary().unwrap();
        assert!(summary.stores.iter().all(|s| s.id != store.to_string()));
        assert_eq!(summary.tombstones.len(), 1);
        assert_eq!(summary.tombstones[0].kind, TombstoneKindName::Store);
    }

    /// `set_store_owners` adds before it removes, so a store handed from one person to another
    /// never has no owner, and an empty owner list is refused.
    #[test]
    fn setting_a_stores_owners_hands_it_over_without_a_gap() {
        let (_folder, keys) = sealed();
        keys.unlock(secret(12)).unwrap();
        let alice = KeyOwner::User("alice".into());
        let bob = KeyOwner::User("bob".into());
        let store = StoreId::Perspective(uuid::Uuid::new_v4());
        keys.set_store_owners(&store, std::slice::from_ref(&alice))
            .unwrap();
        keys.set_store_owners(&store, std::slice::from_ref(&bob))
            .unwrap();
        assert_eq!(keys.store_owners(&store).unwrap(), vec![bob.clone()]);
        assert_eq!(
            keys.set_store_owners(&store, &[]),
            Err(KeyringError::LastOwner)
        );
        assert_eq!(keys.store_owners(&store).unwrap(), vec![bob]);
        // Alice owns nothing now: her key can go.
        keys.destroy_person(&alice, DestroyReason::UserRemoved)
            .unwrap();
    }

    /// Each person's perspective has its own key, wrapped only under its owner's data key: the
    /// other person's data key opens no wrap of it.
    #[test]
    fn one_persons_data_key_does_not_open_anothers_perspective() {
        let (_folder, keys) = sealed();
        keys.unlock(secret(13)).unwrap();
        let alice = KeyOwner::User("alice".into());
        let bob = KeyOwner::User("bob".into());
        let alices = StoreId::Perspective(uuid::Uuid::new_v4());
        let bobs = StoreId::Perspective(uuid::Uuid::new_v4());
        keys.set_store_owners(&alices, std::slice::from_ref(&alice))
            .unwrap();
        keys.set_store_owners(&bobs, std::slice::from_ref(&bob))
            .unwrap();
        let (_, alices_rows) = keys.subkey(&alices, None, SubkeyPurpose::Rows).unwrap();
        let (_, bobs_rows) = keys.subkey(&bobs, None, SubkeyPurpose::Rows).unwrap();
        assert_ne!(alices_rows.expose_secret(), bobs_rows.expose_secret());
        keys.with_open(|open| {
            let alices_key = &open.persons[&(alice.clone(), KeyVersion::FIRST)];
            let bobs_entry = open.file.store(&bobs).unwrap();
            let wraps = &bobs_entry.key.current_value().wraps;
            assert_eq!(wraps.len(), 1);
            assert!(
                open.file
                    .unwrap_store_key(&bobs, KeyVersion::FIRST, &wraps[0], alices_key)
                    .is_err(),
                "alice's data key opens no wrap of bob's perspective"
            );
            let bobs_key = &open.persons[&(bob.clone(), KeyVersion::FIRST)];
            assert!(open
                .file
                .unwrap_store_key(&bobs, KeyVersion::FIRST, &wraps[0], bobs_key)
                .is_ok());
            Ok(())
        })
        .unwrap();
    }

    #[cfg(unix)]
    fn nix_is_root() -> bool {
        // SAFETY: geteuid reads the effective user id and has no failure case.
        unsafe { libc::geteuid() == 0 }
    }
}

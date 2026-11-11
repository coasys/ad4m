//! Moving a store to a new key: `storage.reencrypt`.
//!
//! A store's key is versioned. Re-encrypting a store makes a new version current (new data is
//! sealed under it from then on) and marks the older ones retiring, then seals again under the
//! new version everything still sealed under an older one: the store's sealed database
//! columns and the language storage files that name it. Once nothing under an older version
//! is left, the older versions are dropped from the keyring. Until then data under them still
//! opens, so a job that stops (a crash, a value that does not open) loses nothing; running it
//! again resumes it, without making yet another version.
//!
//! The executor's own store is not re-encrypted here: its key also keys the SQLCipher
//! database file, which the executor opens once at start.

use std::collections::HashSet;
use std::path::Path;
use std::sync::Mutex;

use ad4m_keyring::KeyVersion;

use super::data_keys::{DataKeys, KeyringError};
use super::keyring_file::StoreId;
use super::lang_storage;
use crate::db::sealed::ResealReport;

/// How many times the job seals again before it gives up on values that keep turning up
/// under an older version (written by a writer that read the old version just before the
/// new one became current).
const PASSES: usize = 3;

/// The sealed database columns of the stores, as the job needs them.
pub trait SealedRows {
    /// Seals again under the store key's current version every value of `store` sealed under
    /// an older one.
    fn reseal(&self, store: &StoreId) -> Result<ResealReport, String>;
    /// How many values of `store` are sealed under a version other than `version`.
    fn count_not_under(&self, store: &StoreId, version: KeyVersion) -> Result<usize, String>;
}

impl SealedRows for crate::db::Ad4mDb {
    fn reseal(&self, store: &StoreId) -> Result<ResealReport, String> {
        self.reseal_store_values(store).map_err(|e| e.to_string())
    }

    fn count_not_under(&self, store: &StoreId, version: KeyVersion) -> Result<usize, String> {
        self.count_store_values_not_under(store, version)
            .map_err(|e| e.to_string())
    }
}

/// The executor's database, through its backend.
pub struct ExecutorRows;

impl SealedRows for ExecutorRows {
    fn reseal(&self, store: &StoreId) -> Result<ResealReport, String> {
        crate::db_backend::db_backend()
            .reseal_store_values(store)
            .map_err(|e| e.to_string())
    }

    fn count_not_under(&self, store: &StoreId, version: KeyVersion) -> Result<usize, String> {
        crate::db_backend::db_backend()
            .count_store_values_not_under(store, version)
            .map_err(|e| e.to_string())
    }
}

/// Why a re-encryption did not start or did not finish.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ReencryptError {
    #[error("the executor's own store is not re-encrypted while the executor runs")]
    ExecutorStore,
    #[error("no such store")]
    UnknownStore,
    #[error("a re-encryption of {0} is running")]
    Running(String),
    #[error("keyring: {0}")]
    Keyring(KeyringError),
    #[error("the store's database values: {0}")]
    Rows(String),
    #[error("the store's language files: {0}")]
    Files(String),
}

impl From<KeyringError> for ReencryptError {
    fn from(e: KeyringError) -> Self {
        match e {
            KeyringError::UnknownStore => ReencryptError::UnknownStore,
            other => ReencryptError::Keyring(other),
        }
    }
}

/// What a re-encryption did.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReencryptReport {
    /// The store key's current version.
    pub version: KeyVersion,
    /// The job resumed one that had stopped: no new version was made.
    pub resumed: bool,
    pub resealed_values: usize,
    pub resealed_files: usize,
    /// Values and files under an older version that did not open.
    pub unopenable: usize,
    /// Values and files still under an older version at the end.
    pub left: usize,
    /// The versions dropped from the keyring: none while anything is left.
    pub retired: Vec<KeyVersion>,
}

static RUNNING: Mutex<Option<HashSet<StoreId>>> = Mutex::new(None);

/// A store's claim on the running jobs: one job per store at a time.
pub struct Claim(StoreId);

impl Claim {
    /// Claims `store`, or `Running` while another job has it.
    pub fn take(store: &StoreId) -> Result<Claim, ReencryptError> {
        let mut running = RUNNING.lock().unwrap_or_else(|p| p.into_inner());
        if !running
            .get_or_insert_with(HashSet::new)
            .insert(store.clone())
        {
            return Err(ReencryptError::Running(store.to_string()));
        }
        Ok(Claim(store.clone()))
    }
}

impl Drop for Claim {
    fn drop(&mut self) {
        let mut running = RUNNING.lock().unwrap_or_else(|p| p.into_inner());
        if let Some(running) = running.as_mut() {
            running.remove(&self.0);
        }
    }
}

/// Checks that `store` can be re-encrypted and makes its new key version current (or finds
/// the move that stopped). Answers the current version and whether it resumed.
pub fn begin(keys: &DataKeys, store: &StoreId) -> Result<(KeyVersion, bool), ReencryptError> {
    if *store == StoreId::Executor {
        return Err(ReencryptError::ExecutorStore);
    }
    if !keys.has_store(store)? {
        return Err(ReencryptError::UnknownStore);
    }
    Ok(keys.begin_store_rotation(store)?)
}

/// Seals again everything of `store` still under an older key version, then drops the older
/// versions once nothing is left under them. [`begin`] runs first.
pub fn finish(
    keys: &DataKeys,
    rows: &dyn SealedRows,
    languages: &Path,
    store: &StoreId,
    version: KeyVersion,
    resumed: bool,
) -> Result<ReencryptReport, ReencryptError> {
    let mut report = ReencryptReport {
        version,
        resumed,
        resealed_values: 0,
        resealed_files: 0,
        unopenable: 0,
        left: 0,
        retired: Vec::new(),
    };
    for _ in 0..PASSES {
        let values = rows.reseal(store).map_err(ReencryptError::Rows)?;
        let files = lang_storage::reseal_store(keys, languages, store)
            .map_err(|e| ReencryptError::Files(e.to_string()))?;
        report.resealed_values += values.resealed;
        report.resealed_files += files.resealed;
        report.unopenable = values.unopenable + files.unopenable;
        report.left = rows
            .count_not_under(store, version)
            .map_err(ReencryptError::Rows)?
            + lang_storage::count_store_files_not_under(languages, store, version)
                .map_err(|e| ReencryptError::Files(e.to_string()))?;
        if report.left == 0 || report.left == report.unopenable {
            break;
        }
    }
    if report.left == 0 {
        report.retired = keys.retire_store_versions(store)?;
    } else {
        log::warn!(
            "🔑 {store}: {} values or files are still under an older key ({} do not open); \
             the older key versions stay until a re-encryption moves them",
            report.left,
            report.unopenable
        );
    }
    Ok(report)
}

/// [`begin`] then [`finish`], holding the store's claim.
pub fn reencrypt(
    keys: &DataKeys,
    rows: &dyn SealedRows,
    languages: &Path,
    store: &StoreId,
) -> Result<ReencryptReport, ReencryptError> {
    let _claim = Claim::take(store)?;
    let (version, resumed) = begin(keys, store)?;
    finish(keys, rows, languages, store, version, resumed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::at_rest::data_keys::SubkeyPurpose;
    use crate::at_rest::keyring_file::{path_in, KeyOwner};
    use crate::at_rest::sqlite::{database_key, open_keyed, sealed_version};
    use crate::db::Ad4mDb;
    use crate::types::{PerspectiveHandle, PerspectiveState};
    use std::path::PathBuf;
    use std::sync::Arc;

    fn keyring(folder: &Path) -> Arc<DataKeys> {
        let keys = DataKeys::load(path_in(folder)).unwrap();
        keys.unlock(zeroize::Zeroizing::new([40; 32])).unwrap();
        Arc::new(keys)
    }

    fn database(folder: &Path, keys: &Arc<DataKeys>) -> Ad4mDb {
        let key = database_key(keys).unwrap();
        Ad4mDb::open(&folder.join("db.sqlite"), Some((&key, keys.clone()))).unwrap()
    }

    fn perspective(db: &Ad4mDb, name: &str) -> StoreId {
        let uuid = uuid::Uuid::new_v4();
        db.add_perspective(&PerspectiveHandle {
            uuid: uuid.to_string(),
            name: Some(name.into()),
            neighbourhood: None,
            shared_url: Some(format!("neighbourhood://{name}")),
            state: PerspectiveState::Private,
            owners: None,
        })
        .unwrap();
        StoreId::Perspective(uuid)
    }

    fn stored_versions(folder: &Path, keys: &DataKeys, uuid: &str) -> Vec<KeyVersion> {
        raw(folder, keys)
            .query_row(
                "SELECT name, shared_url FROM perspective_handle WHERE uuid = ?1",
                [uuid],
                |row| Ok(vec![row.get::<_, String>(0)?, row.get::<_, String>(1)?]),
            )
            .unwrap()
            .iter()
            .map(|v| sealed_version(v).unwrap())
            .collect()
    }

    /// A language file of `store` sealed under its current version.
    fn language_file(keys: &DataKeys, languages: &Path, store: &StoreId, text: &str) -> PathBuf {
        let folder = languages.join("QmLanguage");
        std::fs::create_dir_all(&folder).unwrap();
        let path = folder.join("data.json");
        let sealed =
            lang_storage::seal(keys, store, "QmLanguage", "data.json", text.as_bytes()).unwrap();
        std::fs::write(&path, sealed).unwrap();
        path
    }

    /// A second connection to the keyed database, to read and change stored values as they
    /// are on disk.
    fn raw(folder: &Path, keys: &DataKeys) -> rusqlite::Connection {
        open_keyed(&folder.join("db.sqlite"), &database_key(keys).unwrap()).unwrap()
    }

    /// Re-encrypting one perspective moves its values and language files to a new key
    /// version and retires the old one; another perspective's values stay as they are.
    #[test]
    fn a_reencrypted_store_carries_only_the_new_version_and_others_are_untouched() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path());
        let db = database(folder.path(), &keys);
        let moved = perspective(&db, "moved");
        let kept = perspective(&db, "kept");
        let StoreId::Perspective(moved_uuid) = &moved else {
            unreachable!()
        };
        let StoreId::Perspective(kept_uuid) = &kept else {
            unreachable!()
        };
        let languages = folder.path().join("languages");
        let file = language_file(&keys, &languages, &moved, "the language's data");
        assert_eq!(
            stored_versions(folder.path(), &keys, &moved_uuid.to_string()),
            vec![KeyVersion(1); 2]
        );

        let report = reencrypt(&keys, &db, &languages, &moved).unwrap();
        assert_eq!(report.version, KeyVersion(2));
        assert!(!report.resumed);
        assert_eq!(report.resealed_values, 2);
        assert_eq!(report.resealed_files, 1);
        assert_eq!((report.left, report.unopenable), (0, 0));
        assert_eq!(report.retired, vec![KeyVersion(1)]);

        assert_eq!(
            stored_versions(folder.path(), &keys, &moved_uuid.to_string()),
            vec![KeyVersion(2); 2]
        );
        assert_eq!(
            stored_versions(folder.path(), &keys, &kept_uuid.to_string()),
            vec![KeyVersion(1); 2]
        );
        let bytes = std::fs::read(&file).unwrap();
        assert_eq!(
            lang_storage::sealed_header(&bytes),
            Some((moved.clone(), KeyVersion(2)))
        );
        let (_, plain) = lang_storage::open(&keys, "QmLanguage", "data.json", &bytes, "f").unwrap();
        assert_eq!(plain, b"the language's data");
        let handles = db.get_all_perspectives().unwrap();
        let names: HashSet<String> = handles.iter().filter_map(|h| h.name.clone()).collect();
        assert_eq!(
            names,
            HashSet::from(["moved".to_string(), "kept".to_string()])
        );
        let summary = keys.summary().unwrap();
        let entry = summary
            .stores
            .iter()
            .find(|s| s.id == moved.to_string())
            .unwrap();
        assert_eq!((entry.current, entry.retiring.clone()), (2, vec![]));
        assert!(keys
            .subkey(&moved, Some(KeyVersion(1)), SubkeyPurpose::Rows)
            .is_err());
    }

    /// A job that stopped after the new version became current resumes: the next run makes
    /// no further version and finishes the move.
    #[test]
    fn a_stopped_reencryption_resumes_without_a_new_version() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path());
        let db = database(folder.path(), &keys);
        let store = perspective(&db, "resumed");
        let StoreId::Perspective(uuid) = &store else {
            unreachable!()
        };
        // The first run stops right after making the new version current.
        assert_eq!(begin(&keys, &store).unwrap(), (KeyVersion(2), false));
        assert_eq!(keys.retiring_versions(&store).unwrap(), vec![KeyVersion(1)]);
        // Data written meanwhile is sealed under the new version; the old data still opens.
        assert_eq!(
            db.get_all_perspectives()
                .unwrap()
                .iter()
                .filter_map(|h| h.name.clone())
                .collect::<Vec<_>>(),
            vec!["resumed".to_string()]
        );

        let report = reencrypt(&keys, &db, folder.path(), &store).unwrap();
        assert_eq!((report.version, report.resumed), (KeyVersion(2), true));
        assert_eq!(report.retired, vec![KeyVersion(1)]);
        assert_eq!(
            stored_versions(folder.path(), &keys, &uuid.to_string()),
            vec![KeyVersion(2); 2]
        );
        // A second re-encryption makes version 3.
        let again = reencrypt(&keys, &db, folder.path(), &store).unwrap();
        assert_eq!(again.version, KeyVersion(3));
        assert_eq!(again.retired, vec![KeyVersion(2)]);
    }

    /// A value under the old version that does not open keeps the old version: nothing is
    /// lost, and the store stays retiring.
    #[test]
    fn a_value_that_does_not_open_keeps_the_old_version() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path());
        let db = database(folder.path(), &keys);
        let store = perspective(&db, "damaged");
        let StoreId::Perspective(uuid) = &store else {
            unreachable!()
        };
        // Move a sealed value into another column: it no longer opens there.
        raw(folder.path(), &keys)
            .execute(
                "UPDATE perspective_handle SET name = shared_url WHERE uuid = ?1",
                [uuid.to_string()],
            )
            .unwrap();
        let report = reencrypt(&keys, &db, folder.path(), &store).unwrap();
        assert_eq!(report.unopenable, 1);
        assert_eq!(report.left, 1);
        assert!(report.retired.is_empty());
        assert_eq!(keys.retiring_versions(&store).unwrap(), vec![KeyVersion(1)]);
    }

    #[test]
    fn the_executors_store_an_unknown_store_and_a_running_job_are_refused() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path());
        let db = database(folder.path(), &keys);
        assert_eq!(
            reencrypt(&keys, &db, folder.path(), &StoreId::Executor),
            Err(ReencryptError::ExecutorStore)
        );
        assert_eq!(
            reencrypt(
                &keys,
                &db,
                folder.path(),
                &StoreId::Perspective(uuid::Uuid::new_v4())
            ),
            Err(ReencryptError::UnknownStore)
        );
        let store = StoreId::Person(KeyOwner::Main);
        keys.create_person(KeyOwner::Main).unwrap();
        let claim = Claim::take(&store).unwrap();
        assert_eq!(
            reencrypt(&keys, &db, folder.path(), &store),
            Err(ReencryptError::Running(store.to_string()))
        );
        drop(claim);
        let report = reencrypt(&keys, &db, folder.path(), &store).unwrap();
        assert_eq!(report.version, KeyVersion(2));
    }
}

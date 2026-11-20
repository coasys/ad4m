//! Moving a store to a new key: `storage.reencrypt`.
//!
//! A store's key is versioned. Re-encrypting a store makes a new version current (new data is
//! sealed under it from then on) and marks the older ones retiring, then seals again under the
//! new version everything still sealed under an older one: the store's sealed database
//! columns and the language storage files that name it. Once nothing under an older version
//! is left, the older versions are dropped from the keyring. Until then data under them still
//! opens, so a job that stops (a crash, a value that does not open) loses nothing; running it
//! again resumes it, without making yet another version. A language file's write, and this
//! job's seal-again of a file, hold one lock (`lang_storage::exclusive`) from taking the key to
//! renaming the file in place, and the job counts and retires under it: a write that took an
//! older version just before the new one became current is in place before the older version
//! goes, and is counted.
//!
//! **The snapshot rule.** A platform snapshot holds each perspective's part sealed under the
//! perspective store's key at the version that was current when the snapshot was taken, and
//! only the newest snapshot exists (an upload replaces it). Retiring a version therefore
//! strands the part of every snapshot taken under it, and a restore could not open the
//! perspective (it is alive: its key is there, only that version is gone). So before
//! [`finish`] retires versions of a perspective's store it has the executor take a new
//! snapshot ([`ReplaceSnapshot`]), which seals the part under the new, current version and
//! replaces the old upload. When that fails, the versions are not retired and the job
//! stops with [`ReencryptError::Snapshot`]: running it again resumes and tries again, so the
//! newest snapshot is never left unreadable by a rotation. A snapshot another task sealed
//! under the old version in the moment before the new one became current and uploaded after
//! ours is the one remaining way to a stranded part; a restore then fails naming the
//! perspective and the version (`DataKeys::subkey`: `VersionRetired`) rather than leaving the
//! perspective out, and the next periodic snapshot repairs the upload.
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

/// Uploads a platform snapshot taken now, replacing the one before it (an executor that
/// uploads none answers `Ok`): `perspective_snapshot::replace_snapshot` in the executor.
pub type ReplaceSnapshot<'a> = &'a dyn Fn() -> Result<(), String>;

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
    #[error("the snapshot could not be replaced, so the old key versions stay until it can: {0}")]
    Snapshot(String),
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
    replace_snapshot: ReplaceSnapshot,
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
    if report.left == 0 && matches!(store, StoreId::Perspective(_)) && has_retiring(keys, store)? {
        // The snapshot rule (module docs): the part of the newest snapshot is under a version
        // about to go, so a snapshot under the current one replaces it first.
        replace_snapshot().map_err(ReencryptError::Snapshot)?;
    }
    if report.left == 0 {
        // A language write holds the files lock from taking the key to renaming its file in
        // place. Counting again and retiring under that lock finds every file a writer that
        // took an older version published, and no writer takes one after (the current version
        // is the new one): nothing is stranded under a version that is gone.
        report.retired = lang_storage::exclusive(|| -> Result<Vec<KeyVersion>, ReencryptError> {
            let files_left = lang_storage::count_store_files_not_under(languages, store, version)
                .map_err(|e| ReencryptError::Files(e.to_string()))?;
            if files_left > 0 {
                report.left = files_left;
                return Ok(Vec::new());
            }
            Ok(keys.retire_store_versions(store)?)
        })?;
    }
    if report.left != 0 {
        log::warn!(
            "🔑 {store}: {} values or files are still under an older key ({} do not open); \
             the older key versions stay until a re-encryption moves them",
            report.left,
            report.unopenable
        );
    }
    Ok(report)
}

/// Whether `store`'s key holds a version that retiring would drop.
fn has_retiring(keys: &DataKeys, store: &StoreId) -> Result<bool, ReencryptError> {
    let id = store.to_string();
    Ok(keys
        .summary()?
        .stores
        .iter()
        .any(|s| s.id == id && !s.retiring.is_empty()))
}

/// [`begin`] then [`finish`], holding the store's claim.
pub fn reencrypt(
    keys: &DataKeys,
    rows: &dyn SealedRows,
    replace_snapshot: ReplaceSnapshot,
    languages: &Path,
    store: &StoreId,
) -> Result<ReencryptReport, ReencryptError> {
    let _claim = Claim::take(store)?;
    let (version, resumed) = begin(keys, store)?;
    finish(
        keys,
        rows,
        replace_snapshot,
        languages,
        store,
        version,
        resumed,
    )
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

        let report = reencrypt(&keys, &db, &|| Ok(()), &languages, &moved).unwrap();
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

    /// A language's write that took the old key version before the re-encryption switched to
    /// the new one, and publishes after the job has counted and resealed, is not stranded: the
    /// job waits for it before it retires the old version, counts its file and seals it again.
    #[test]
    fn a_write_holding_the_old_version_is_not_stranded_by_the_retirement() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path());
        let db = database(folder.path(), &keys);
        let store = perspective(&db, "moved");
        let StoreId::Perspective(uuid) = &store else {
            unreachable!()
        };
        let languages = folder.path().join("languages");
        let language_folder = languages.join("QmWriter");
        std::fs::create_dir_all(&language_folder).unwrap();
        crate::perspectives::route_link_language_for_test(
            "QmWriter",
            Some(PerspectiveHandle {
                uuid: uuid.to_string(),
                name: None,
                neighbourhood: None,
                shared_url: None,
                state: PerspectiveState::Private,
                owners: None,
            }),
        );

        // A writer holds the files lock with a file sealed under version 1 and not yet in
        // place; the job starts once it holds the lock, and the writer publishes once the job
        // is counted among the lock's waiters. A job that retires without taking the lock
        // never waits, and the writer's wait fails.
        let held =
            lang_storage::seal(&keys, &store, "QmWriter", "kv.json", b"acknowledged").unwrap();
        let (locked_tx, locked_rx) = std::sync::mpsc::channel::<()>();
        let file = language_folder.join("kv.json");
        let before = lang_storage::waiting();
        let writer = std::thread::spawn(move || {
            lang_storage::exclusive(|| {
                locked_tx.send(()).unwrap();
                lang_storage::tests::wait_for_waiter(before);
                crate::at_rest::files::write_atomic(&file, &held).unwrap();
            })
        });
        locked_rx.recv().unwrap();
        let report = reencrypt(&keys, &db, &|| Ok(()), &languages, &store);
        writer.join().unwrap();
        let report = report.unwrap();
        assert_eq!(
            report.left, 1,
            "the job counted the write that landed after it began"
        );
        let me = lang_storage::Caller {
            folder: &language_folder,
            address: "QmWriter",
            system: false,
        };
        assert_eq!(
            lang_storage::read_file(Some(&keys), me, "kv.json").unwrap(),
            "acknowledged",
            "the write that held version 1 still opens"
        );
        // The write is in place and still under version 1, which stays: nothing is stranded,
        // and the job run again moves the file and then retires version 1.
        assert!(report.retired.is_empty());
        let again = reencrypt(&keys, &db, &|| Ok(()), &languages, &store).unwrap();
        assert!(again.resumed);
        assert_eq!((again.left, again.resealed_files), (0, 1));
        assert_eq!(again.retired, vec![KeyVersion(1)]);
        assert_eq!(
            lang_storage::read_file(Some(&keys), me, "kv.json").unwrap(),
            "acknowledged",
            "and after the retirement"
        );
        crate::perspectives::route_link_language_for_test("QmWriter", None);
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

        let report = reencrypt(&keys, &db, &|| Ok(()), folder.path(), &store).unwrap();
        assert_eq!((report.version, report.resumed), (KeyVersion(2), true));
        assert_eq!(report.retired, vec![KeyVersion(1)]);
        assert_eq!(
            stored_versions(folder.path(), &keys, &uuid.to_string()),
            vec![KeyVersion(2); 2]
        );
        // A second re-encryption makes version 3.
        let again = reencrypt(&keys, &db, &|| Ok(()), folder.path(), &store).unwrap();
        assert_eq!(again.version, KeyVersion(3));
        assert_eq!(again.retired, vec![KeyVersion(2)]);
    }

    /// A snapshot that cannot be replaced keeps the old version: the newest snapshot's part
    /// is under it. The job stops with the error, and run again it retires once a snapshot
    /// is taken. A store that is not a perspective's takes no snapshot.
    #[test]
    fn the_old_version_stays_until_the_snapshot_is_replaced() {
        let tries = std::sync::atomic::AtomicUsize::new(0);
        let down = || {
            tries.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            Err("the platform is down".to_string())
        };
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path());
        let db = database(folder.path(), &keys);
        let store = perspective(&db, "moved");
        assert_eq!(
            reencrypt(&keys, &db, &down, folder.path(), &store),
            Err(ReencryptError::Snapshot("the platform is down".to_string()))
        );
        assert_eq!(keys.retiring_versions(&store).unwrap(), vec![KeyVersion(1)]);
        assert_eq!(tries.load(std::sync::atomic::Ordering::SeqCst), 1);

        let report = reencrypt(&keys, &db, &|| Ok(()), folder.path(), &store).unwrap();
        assert!(report.resumed);
        assert_eq!(report.retired, vec![KeyVersion(1)]);
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
        let report = reencrypt(&keys, &db, &|| Ok(()), folder.path(), &store).unwrap();
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
            reencrypt(&keys, &db, &|| Ok(()), folder.path(), &StoreId::Executor),
            Err(ReencryptError::ExecutorStore)
        );
        assert_eq!(
            reencrypt(
                &keys,
                &db,
                &|| Ok(()),
                folder.path(),
                &StoreId::Perspective(uuid::Uuid::new_v4())
            ),
            Err(ReencryptError::UnknownStore)
        );
        let store = StoreId::Person(KeyOwner::Main);
        keys.create_person(KeyOwner::Main).unwrap();
        let claim = Claim::take(&store).unwrap();
        assert_eq!(
            reencrypt(&keys, &db, &|| Ok(()), folder.path(), &store),
            Err(ReencryptError::Running(store.to_string()))
        );
        drop(claim);
        let report = reencrypt(&keys, &db, &|| Ok(()), folder.path(), &store).unwrap();
        assert_eq!(report.version, KeyVersion(2));
    }
}

//! The executor database's step at start, once the keyring is open: the file is put under
//! the database key (the executor store's SQLite subkey) before anything opens it.
//!
//! - No file yet: it is made keyed.
//! - A keyed file: it is checked under the key; a key that does not open it stops the start.
//! - A plaintext file (an executor from before SQLCipher): it moves under the key
//!   ([`sqlite::migrate_to_sqlcipher`]) and its sealed columns are sealed on the way. The
//!   executor store is `migrating` meanwhile. If the move fails the plaintext stays as it
//!   was, the executor runs on it in the clear, the store goes back to `plain`, and the gate
//!   reports `migration-failed`.
//!
//! On success the database keys are installed ([`sqlite::install`]): every connection to the
//! file opens under them and the sealed columns are sealed.

use std::path::Path;
use std::sync::Arc;

use chrono::Utc;

use super::keyring_file::{StoreId, StoreState};
use super::sqlite::{self, DatabaseFile, MigrationError, MigrationReport};
use super::{DataKeys, StorageErrorKind, StorageGate};
use crate::db::Ad4mDb;

/// What the step did.
#[derive(Debug)]
pub enum DatabaseStep {
    /// The file is under its key: new, already keyed, or migrated now (`Some`).
    Keyed(Option<MigrationReport>),
    /// The plaintext did not move: the executor runs on it in the clear.
    KeptPlain(MigrationError),
}

/// Puts the database at `path` under the database key of `keys`, and installs the keys when
/// it is. `Err` stops the start: the keyed file does not open under this keyring's key, or
/// the keyring cannot be written. `gate`, when given, publishes each store state and reports
/// a failed migration.
pub fn prepare(
    keys: &Arc<DataKeys>,
    path: &Path,
    gate: Option<&StorageGate>,
) -> Result<DatabaseStep, String> {
    let step = key_file(keys, path, gate)?;
    if let DatabaseStep::Keyed(_) = step {
        let sqlite = sqlite::database_key(keys).map_err(|e| e.to_string())?;
        // A second `run` in one process keeps the keys the first installed: the same keyring.
        let _ = sqlite::install(sqlite::DatabaseKeys {
            sqlite,
            rows: keys.clone(),
        });
    }
    Ok(step)
}

fn set_state(keys: &DataKeys, gate: Option<&StorageGate>, state: StoreState) -> Result<(), String> {
    keys.set_store_state(&StoreId::Executor, state)
        .map_err(|e| format!("Could not record the database's state in the keyring: {e}"))?;
    if let Some(gate) = gate {
        gate.publish();
    }
    Ok(())
}

/// Puts the database file at `path` under the database key, without installing the keys: the
/// database step of [`super::migration`], and the first half of [`prepare`].
pub(crate) fn key_file(
    keys: &Arc<DataKeys>,
    path: &Path,
    gate: Option<&StorageGate>,
) -> Result<DatabaseStep, String> {
    let key = sqlite::database_key(keys).map_err(|e| e.to_string())?;
    let file = sqlite::database_file(path)
        .map_err(|e| format!("Could not read {}: {e}", path.display()))?;
    match file {
        DatabaseFile::New | DatabaseFile::Encrypted => {
            sqlite::open_keyed(path, &key).map_err(|e| {
                format!(
                    "The database {} does not open under this keyring's key: {e}",
                    path.display()
                )
            })?;
            set_state(keys, gate, StoreState::Encrypted)?;
            Ok(DatabaseStep::Keyed(None))
        }
        DatabaseFile::Plaintext => {
            log::info!("Moving the database {} under its key", path.display());
            set_state(
                keys,
                gate,
                StoreState::Migrating {
                    started_at: Utc::now(),
                },
            )?;
            let migrated = sqlite::migrate_to_sqlcipher(path, &key, |new_file| {
                // The schema's own migrations run first (the owner columns), then every value
                // of a sealed column is sealed for its row's store.
                let db = Ad4mDb::open(new_file, Some((&key, keys.clone())))
                    .map_err(|e| e.to_string())?;
                let sealed = db.seal_plain_values().map_err(|e| e.to_string())?;
                log::info!("Sealed {sealed} values of the database");
                Ok(())
            });
            match migrated {
                Ok(report) => {
                    set_state(keys, gate, StoreState::Encrypted)?;
                    log::info!(
                        "The database is under its key: {} tables, user_version {}",
                        report.tables.len(),
                        report.user_version
                    );
                    Ok(DatabaseStep::Keyed(Some(report)))
                }
                Err(e) => {
                    log::error!("{e}");
                    set_state(keys, gate, StoreState::Plain)?;
                    if let Some(gate) = gate {
                        gate.report_error(StorageErrorKind::MigrationFailed);
                    }
                    Ok(DatabaseStep::KeptPlain(e))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::at_rest::keyring_file::KeyOwner;
    use crate::at_rest::sqlite::{open_keyed, SEALED_PREFIX};
    use crate::types::NotificationInput;
    use rusqlite::Connection;

    fn open_keys(folder: &Path) -> Arc<DataKeys> {
        let keys = DataKeys::load(crate::at_rest::keyring_file::path_in(folder)).unwrap();
        keys.unlock(zeroize::Zeroizing::new([9u8; 32])).unwrap();
        Arc::new(keys)
    }

    fn notification() -> NotificationInput {
        NotificationInput {
            description: "a private description".into(),
            app_name: "app".into(),
            app_url: "https://app.test".into(),
            app_icon_path: String::new(),
            trigger: "SELECT 1".into(),
            perspective_ids: vec![],
            webhook_url: "https://webhook.test".into(),
            webhook_auth: "auth".into(),
        }
    }

    /// A database an executor from before SQLCipher left: plaintext, its values in the clear.
    fn plaintext_database(path: &Path) -> (String, String) {
        let db = Ad4mDb::new(path.to_str().unwrap()).unwrap();
        let id = db
            .add_notification(notification(), "did:key:someone", true)
            .unwrap();
        let perspective = uuid::Uuid::new_v4().to_string();
        db.add_perspective(&crate::types::PerspectiveHandle {
            uuid: perspective.clone(),
            name: Some("a private name".into()),
            neighbourhood: None,
            shared_url: Some("neighbourhood://private".into()),
            state: crate::types::PerspectiveState::Private,
            owners: Some(vec!["did:key:someone".into()]),
        })
        .unwrap();
        drop(db);
        Connection::open(path)
            .unwrap()
            .execute_batch("PRAGMA user_version = 3;")
            .unwrap();
        (id, perspective)
    }

    /// Every table with its row count.
    fn counts(conn: &Connection) -> Vec<(String, i64)> {
        let names: Vec<String> = conn
            .prepare(
                "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'",
            )
            .unwrap()
            .query_map([], |r| r.get(0))
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap();
        names
            .into_iter()
            .map(|name| {
                let n = conn
                    .query_row(&format!("SELECT count(*) FROM \"{name}\""), [], |r| {
                        r.get(0)
                    })
                    .unwrap();
                (name, n)
            })
            .collect()
    }

    #[test]
    fn a_plaintext_database_moves_under_its_key_with_every_row_and_its_values_sealed() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path());
        let path = folder.path().join("ad4m_db.sqlite");
        let (id, perspective) = plaintext_database(&path);
        let before = {
            let conn = Connection::open(&path).unwrap();
            counts(&conn)
        };

        let step = key_file(&keys, &path, None).unwrap();
        assert!(matches!(step, DatabaseStep::Keyed(Some(_))), "{step:?}");
        assert_eq!(
            keys.store_state(&StoreId::Executor).unwrap(),
            Some(StoreState::Encrypted)
        );
        assert_eq!(
            sqlite::database_file(&path).unwrap(),
            DatabaseFile::Encrypted
        );
        assert!(!sqlite::migration_file(&path).exists());

        let key = sqlite::database_key(&keys).unwrap();
        let conn = open_keyed(&path, &key).unwrap();
        let after = counts(&conn);
        for (table, count) in &before {
            assert!(
                after.contains(&(table.clone(), *count)),
                "{table}: {after:?}"
            );
        }
        let version: i64 = conn
            .query_row("PRAGMA user_version", [], |r| r.get(0))
            .unwrap();
        assert_eq!(version, 3);
        let description: String = conn
            .query_row(
                "SELECT description FROM notifications WHERE id = ?1",
                [&id],
                |r| r.get(0),
            )
            .unwrap();
        assert!(description.starts_with(SEALED_PREFIX), "{description}");
        let name: String = conn
            .query_row(
                "SELECT name FROM perspective_handle WHERE uuid = ?1",
                [&perspective],
                |r| r.get(0),
            )
            .unwrap();
        assert!(name.starts_with(SEALED_PREFIX), "{name}");
        drop(conn);

        let db = Ad4mDb::open(&path, Some((&key, keys.clone()))).unwrap();
        let read = db.get_notification(id).unwrap().unwrap();
        assert_eq!(read.description, "a private description");
        let handle = db._get_perspective(&perspective).unwrap().unwrap();
        assert_eq!(handle.name.as_deref(), Some("a private name"));
        assert_eq!(
            db.get_neighbourhood_owners("neighbourhood://private")
                .unwrap(),
            vec!["did:key:someone".to_string()]
        );
    }

    #[test]
    fn a_failed_migration_runs_on_the_plaintext_and_says_so() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path());
        let path = folder.path().join("ad4m_db.sqlite");
        let (id, _) = plaintext_database(&path);
        // A perspective id that is not a uuid has no store to seal for: the seal fails.
        Connection::open(&path)
            .unwrap()
            .execute(
                "INSERT INTO perspective_diff (perspective, additions, removals, is_pending) VALUES ('not-a-uuid', '[]', '[]', 1)",
                [],
            )
            .unwrap();
        let plaintext = std::fs::read(&path).unwrap();

        let step = key_file(&keys, &path, None).unwrap();
        let DatabaseStep::KeptPlain(e) = step else {
            panic!("{step:?}")
        };
        assert_eq!(e.stage, "seal", "{e}");
        assert!(
            e.to_string().contains("the plaintext database is kept"),
            "{e}"
        );
        assert_eq!(
            std::fs::read(&path).unwrap(),
            plaintext,
            "the plaintext is as it was"
        );
        assert!(!sqlite::migration_file(&path).exists());
        assert_eq!(
            keys.store_state(&StoreId::Executor).unwrap(),
            Some(StoreState::Plain)
        );
        let db = Ad4mDb::new(path.to_str().unwrap()).unwrap();
        assert!(db.get_notification(id).unwrap().is_some());
    }

    #[test]
    fn a_keyed_database_of_another_keyring_stops_the_start() {
        let folder = tempfile::tempdir().unwrap();
        let path = folder.path().join("ad4m_db.sqlite");
        let other = tempfile::tempdir().unwrap();
        let other_keys = open_keys(other.path());
        open_keyed(&path, &sqlite::database_key(&other_keys).unwrap())
            .unwrap()
            .execute_batch("CREATE TABLE t (x);")
            .unwrap();

        let keys = open_keys(folder.path());
        let e = key_file(&keys, &path, None).unwrap_err();
        assert!(e.contains("does not open under this keyring's key"), "{e}");
    }

    #[test]
    fn a_new_database_starts_keyed() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path());
        let path = folder.path().join("ad4m_db.sqlite");
        let step = key_file(&keys, &path, None).unwrap();
        assert!(matches!(step, DatabaseStep::Keyed(None)), "{step:?}");
        // Its rows are sealed for their owners' persons.
        let key = sqlite::database_key(&keys).unwrap();
        let db = Ad4mDb::open(&path, Some((&key, keys.clone()))).unwrap();
        db.add_notification(notification(), "did:key:someone", true)
            .unwrap();
        assert!(keys
            .subkey(
                &StoreId::Person(KeyOwner::Main),
                None,
                crate::at_rest::data_keys::SubkeyPurpose::Rows
            )
            .is_ok());
        drop(db);
        // Once the schema is written, plain SQLite cannot read the file, and the next start
        // takes it as keyed.
        assert!(Connection::open(&path)
            .unwrap()
            .query_row("SELECT count(*) FROM sqlite_master", [], |r| r
                .get::<_, i64>(0))
            .is_err());
        assert_eq!(
            sqlite::database_file(&path).unwrap(),
            DatabaseFile::Encrypted
        );
        assert!(matches!(
            key_file(&keys, &path, None).unwrap(),
            DatabaseStep::Keyed(None)
        ));
    }
}

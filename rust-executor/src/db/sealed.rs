//! The sealed columns of the executor database.
//!
//! The columns below hold a person's or a perspective's data: under SQLCipher the whole file
//! is under the database key, and these values are sealed again, each under the Rows subkey
//! of the store that owns its row (`at_rest::sqlite::seal_value`). The database key alone
//! (the executor's) does not read them; a person's or a perspective's store key does.
//!
//! | Table | Sealed columns | Store |
//! |---|---|---|
//! | `perspective_handle` | `name`, `neighbourhood`, `shared_url` | the perspective's (`uuid`) |
//! | `perspective_diff` | `additions`, `removals` | the perspective's (`perspective`) |
//! | `flow_replica_state` | `current_state` | the perspective's (`perspective`) |
//! | `notifications` | `description` … `webhookAuth` | the owner's (`owner_did`) |
//! | `tasks` | `name`, `system_prompt`, `prompt_examples`, `metadata` | the owner's (`agent_did`) |
//! | `expression` | `data` | the owner's (`owner`), the executor's for `''` |
//! | `outbox` | `message`, `recipient` | the owner's (`owner_did`) |
//!
//! An owner DID names a person: a managed user's DID (`users.did`) names that user, any other
//! DID (and `NULL`) the main agent. A perspective's store is wrapped for each of its owners.
//!
//! A sealed column is never in a `WHERE`, an `ORDER BY` or an index: a lookup by one reads
//! the rows and compares the opened values. A database opened without keys (unit tests, a
//! plaintext database whose migration failed) keeps every value in the clear.

use rusqlite::{params, OptionalExtension};

use super::{Ad4mDb, Ad4mDbResult};
use crate::at_rest::data_keys::DestroyReason;
use crate::at_rest::keyring_file::{KeyOwner, StoreId};
use crate::at_rest::sqlite::{self, Cell, SealError};

/// A row as `seal_plain_values` reads it: rowid, owner, key values, column values.
type StoredRow = (i64, Option<String>, Vec<String>, Vec<Option<String>>);

/// What names the store of a sealed table's row.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum OwnerColumn {
    /// A perspective's uuid: the perspective's store.
    Perspective(&'static str),
    /// An owner DID: the person's store.
    Person(&'static str),
    /// An owner DID, or `''` for the executor's own rows (the executor's store).
    PersonOrExecutor(&'static str),
}

impl OwnerColumn {
    pub(crate) fn column(self) -> &'static str {
        match self {
            OwnerColumn::Perspective(c)
            | OwnerColumn::Person(c)
            | OwnerColumn::PersonOrExecutor(c) => c,
        }
    }
}

/// A table with sealed columns.
#[derive(Debug)]
pub(crate) struct SealedTable {
    pub table: &'static str,
    /// The columns of the row's key, bound into each sealed value.
    pub key: &'static [&'static str],
    pub owner: OwnerColumn,
    pub columns: &'static [&'static str],
}

pub(crate) const PERSPECTIVE_HANDLE: SealedTable = SealedTable {
    table: "perspective_handle",
    key: &["uuid"],
    owner: OwnerColumn::Perspective("uuid"),
    columns: &["name", "neighbourhood", "shared_url"],
};

pub(crate) const PERSPECTIVE_DIFF: SealedTable = SealedTable {
    table: "perspective_diff",
    key: &["id"],
    owner: OwnerColumn::Perspective("perspective"),
    columns: &["additions", "removals"],
};

pub(crate) const FLOW_REPLICA_STATE: SealedTable = SealedTable {
    table: "flow_replica_state",
    key: &["perspective", "agent_did", "instance"],
    owner: OwnerColumn::Perspective("perspective"),
    columns: &["current_state"],
};

pub(crate) const NOTIFICATIONS: SealedTable = SealedTable {
    table: "notifications",
    key: &["id"],
    owner: OwnerColumn::Person("owner_did"),
    columns: &[
        "description",
        "appName",
        "appUrl",
        "appIconPath",
        "trigger",
        "perspective_ids",
        "webhookUrl",
        "webhookAuth",
    ],
};

pub(crate) const TASKS: SealedTable = SealedTable {
    table: "tasks",
    key: &["id"],
    owner: OwnerColumn::Person("agent_did"),
    columns: &["name", "system_prompt", "prompt_examples", "metadata"],
};

pub(crate) const EXPRESSION: SealedTable = SealedTable {
    table: "expression",
    key: &["url", "owner"],
    owner: OwnerColumn::PersonOrExecutor("owner"),
    columns: &["data"],
};

pub(crate) const OUTBOX: SealedTable = SealedTable {
    table: "outbox",
    key: &["id"],
    owner: OwnerColumn::Person("owner_did"),
    columns: &["message", "recipient"],
};

/// Every table with sealed columns.
pub(crate) const SEALED_TABLES: [&SealedTable; 7] = [
    &PERSPECTIVE_HANDLE,
    &PERSPECTIVE_DIFF,
    &FLOW_REPLICA_STATE,
    &NOTIFICATIONS,
    &TASKS,
    &EXPRESSION,
    &OUTBOX,
];

/// The parts of a row's key, as bound into its sealed values.
fn row_key(parts: &[&str]) -> String {
    parts.join("\u{1f}")
}

fn seal_failure(e: SealError) -> rusqlite::Error {
    rusqlite::Error::ToSqlConversionFailure(Box::new(e))
}

fn open_failure(e: SealError) -> rusqlite::Error {
    rusqlite::Error::FromSqlConversionFailure(0, rusqlite::types::Type::Text, Box::new(e))
}

/// Whether `e` is a sealed value that did not open (a row of a destroyed key, a value moved
/// into another row): a listing skips such a row.
pub(crate) fn is_unreadable(e: &rusqlite::Error) -> bool {
    matches!(e, rusqlite::Error::FromSqlConversionFailure(_, _, inner) if inner.is::<SealError>())
}

/// `Ok(None)` (with a warning) for a row whose sealed values do not open, so one such row
/// does not hide the others of a listing.
pub(crate) fn readable<T>(table: &str, row: rusqlite::Result<T>) -> rusqlite::Result<Option<T>> {
    match row {
        Ok(row) => Ok(Some(row)),
        Err(e) if is_unreadable(&e) => {
            log::warn!("{table}: skipping a row that does not open: {e}");
            Ok(None)
        }
        Err(e) => Err(e),
    }
}

/// Seals and opens the sealed values of one row.
pub(crate) struct RowSealer<'a> {
    db: &'a Ad4mDb,
    table: &'static str,
    row: String,
    /// `None`: the database has no keys, and values stay as they are.
    store: Option<StoreId>,
}

impl RowSealer<'_> {
    pub(crate) fn seal(&self, column: &str, plain: &str) -> rusqlite::Result<String> {
        match (&self.db.rows, &self.store) {
            (Some(keys), Some(store)) => sqlite::seal_value(
                keys,
                store,
                &Cell {
                    table: self.table,
                    column,
                    row: &self.row,
                },
                plain,
            )
            .map_err(seal_failure),
            _ => Ok(plain.to_string()),
        }
    }

    pub(crate) fn seal_opt(
        &self,
        column: &str,
        plain: Option<&str>,
    ) -> rusqlite::Result<Option<String>> {
        plain.map(|plain| self.seal(column, plain)).transpose()
    }

    pub(crate) fn open(&self, column: &str, stored: String) -> rusqlite::Result<String> {
        match (&self.db.rows, &self.store) {
            (Some(keys), Some(store)) => sqlite::open_value(
                keys,
                store,
                &Cell {
                    table: self.table,
                    column,
                    row: &self.row,
                },
                &stored,
            )
            .map_err(open_failure),
            _ => Ok(stored),
        }
    }

    pub(crate) fn open_opt(
        &self,
        column: &str,
        stored: Option<String>,
    ) -> rusqlite::Result<Option<String>> {
        stored.map(|stored| self.open(column, stored)).transpose()
    }
}

impl Ad4mDb {
    /// Whether this database seals its sealed columns.
    pub(crate) fn seals(&self) -> bool {
        self.rows.is_some()
    }

    /// The person an owner DID names: the managed user whose DID it is, else the main agent.
    pub(crate) fn key_owner(&self, did: Option<&str>) -> rusqlite::Result<KeyOwner> {
        let Some(did) = did.filter(|d| !d.is_empty()) else {
            return Ok(KeyOwner::Main);
        };
        let user: Option<String> = self
            .conn
            .query_row(
                "SELECT username FROM users WHERE did = ?1 AND did != ''",
                params![did],
                |row| row.get(0),
            )
            .optional()?;
        Ok(user.map_or(KeyOwner::Main, KeyOwner::User))
    }

    fn store_of(&self, owner: OwnerColumn, value: Option<&str>) -> rusqlite::Result<StoreId> {
        match owner {
            OwnerColumn::Perspective(_) => {
                let value = value.unwrap_or_default();
                uuid::Uuid::parse_str(value)
                    .map(StoreId::Perspective)
                    .map_err(|e| {
                        rusqlite::Error::ToSqlConversionFailure(
                            format!("perspective {value} is not a uuid: {e}").into(),
                        )
                    })
            }
            OwnerColumn::PersonOrExecutor(_) if value.unwrap_or_default().is_empty() => {
                Ok(StoreId::Executor)
            }
            OwnerColumn::Person(_) | OwnerColumn::PersonOrExecutor(_) => {
                Ok(StoreId::Person(self.key_owner(value)?))
            }
        }
    }

    /// The persons a perspective's owner DIDs name; the main agent for none.
    pub(crate) fn perspective_key_owners(
        &self,
        owner_dids: &[String],
    ) -> rusqlite::Result<Vec<KeyOwner>> {
        let mut owners = Vec::new();
        for did in owner_dids {
            let owner = self.key_owner(Some(did))?;
            if !owners.contains(&owner) {
                owners.push(owner);
            }
        }
        if owners.is_empty() {
            owners.push(KeyOwner::Main);
        }
        Ok(owners)
    }

    /// The owner DIDs of perspective `uuid`'s handle; none without a handle.
    fn perspective_owner_dids(&self, uuid: &str) -> rusqlite::Result<Vec<String>> {
        let owners: Option<Option<String>> = self
            .conn
            .query_row(
                "SELECT owners FROM perspective_handle WHERE uuid = ?1",
                params![uuid],
                |row| row.get(0),
            )
            .optional()?;
        Ok(owners
            .flatten()
            .and_then(|json| serde_json::from_str(&json).ok())
            .unwrap_or_default())
    }

    /// The sealer of a row to read: `owner` is the row's owner column value, `key` its key
    /// columns' values.
    pub(crate) fn reader(
        &self,
        table: &SealedTable,
        owner: Option<&str>,
        key: &[&str],
    ) -> rusqlite::Result<RowSealer<'_>> {
        let store = if self.seals() {
            Some(self.store_of(table.owner, owner)?)
        } else {
            None
        };
        Ok(RowSealer {
            db: self,
            table: table.table,
            row: row_key(key),
            store,
        })
    }

    /// The sealer of a row to write. Makes the store (and its owners' persons) when it does
    /// not exist yet. A perspective's handle being written (`perspective_owners` given) sets
    /// the store's owners to the persons its owner DIDs name: an owner added to the handle
    /// gets a wrap of the key, one dropped from it loses its wrap. Any other row of a
    /// perspective only makes sure its handle's owners hold the key.
    pub(crate) fn writer(
        &self,
        table: &SealedTable,
        owner: Option<&str>,
        key: &[&str],
        perspective_owners: Option<&[String]>,
    ) -> rusqlite::Result<RowSealer<'_>> {
        let sealer = self.reader(table, owner, key)?;
        if let (Some(keys), Some(store)) = (&self.rows, &sealer.store) {
            let keyed = match store {
                StoreId::Executor => keys.ensure_store(store, &[]),
                StoreId::Person(person) => keys.ensure_store(store, std::slice::from_ref(person)),
                StoreId::Perspective(uuid) => match perspective_owners {
                    Some(dids) => keys.set_store_owners(store, &self.perspective_key_owners(dids)?),
                    None => {
                        let dids = self.perspective_owner_dids(&uuid.to_string())?;
                        keys.ensure_store(store, &self.perspective_key_owners(&dids)?)
                    }
                },
            };
            keyed.map_err(|e| seal_failure(SealError::Keyring(e)))?;
        }
        Ok(sealer)
    }

    /// Destroys perspective `uuid`'s store key, once its rows and its files are gone: its
    /// sealed values anywhere (freed pages, a copy of the file) no longer open. A store that
    /// does not exist (never made, or destroyed already) is not an error; neither is a
    /// database without keys.
    pub(crate) fn destroy_perspective_store(&self, uuid: &str) -> Ad4mDbResult<()> {
        let Some(keys) = &self.rows else {
            return Ok(());
        };
        let store = StoreId::Perspective(uuid::Uuid::parse_str(uuid)?);
        match keys.destroy_store(&store, DestroyReason::PerspectiveRemoved) {
            Ok(()) | Err(crate::at_rest::KeyringError::UnknownStore) => Ok(()),
            Err(e) => Err(e.into()),
        }
    }

    /// Deletes every row sealed under the person store of the managed user whose DID is
    /// `did`: the rows of the tables whose owner column names a person (notifications, tasks,
    /// expressions, the outbox) that name `did`. Answers how many it deleted. Runs before the
    /// person's key is destroyed, so the key goes after the rows it protects.
    pub(crate) fn delete_person_rows(&self, did: &str) -> Ad4mDbResult<usize> {
        if did.is_empty() {
            return Ok(0);
        }
        let tx = self.conn.unchecked_transaction()?;
        let mut deleted = 0;
        for table in SEALED_TABLES {
            if let OwnerColumn::Person(column) | OwnerColumn::PersonOrExecutor(column) = table.owner
            {
                deleted += tx.execute(
                    &format!("DELETE FROM {} WHERE {column} = ?1", table.table),
                    params![did],
                )?;
            }
        }
        tx.commit()?;
        Ok(deleted)
    }

    /// The perspectives the keyring holds a store key for that have no handle in this
    /// database: left by a removal that stopped between deleting the rows and destroying the
    /// key. Read from the handles' plain `uuid` column, so a handle whose sealed values do not
    /// open still counts as present.
    pub(crate) fn perspective_stores_without_handle(&self) -> Ad4mDbResult<Vec<String>> {
        let Some(keys) = &self.rows else {
            return Ok(Vec::new());
        };
        let handles: std::collections::HashSet<String> = self
            .conn
            .prepare("SELECT uuid FROM perspective_handle")?
            .query_map([], |row| row.get(0))?
            .collect::<Result<_, _>>()?;
        Ok(keys
            .store_states()?
            .into_iter()
            .filter_map(|(id, _)| match id {
                StoreId::Perspective(uuid) => Some(uuid.to_string()),
                _ => None,
            })
            .filter(|uuid| !handles.contains(uuid))
            .collect())
    }

    /// Seals every value of a sealed column still in the clear: after the export into a
    /// keyed file, and after an import. Answers how many values it sealed.
    pub(crate) fn seal_plain_values(&self) -> Ad4mDbResult<usize> {
        if !self.seals() {
            return Ok(0);
        }
        let tx = self.conn.unchecked_transaction()?;
        let mut sealed = 0;
        for table in SEALED_TABLES {
            // A notification of an older executor names its owner by `user_email` until
            // `migrate_notification_owners` gives it the user's DID: seal it for that user.
            let legacy_email = table.table == "notifications"
                && self.conn.query_row(
                    "SELECT COUNT(*) > 0 FROM pragma_table_info('notifications') WHERE name = 'user_email'",
                    [],
                    |row| row.get::<_, bool>(0),
                )?;
            let owner = if legacy_email {
                "COALESCE(owner_did, (SELECT NULLIF(did, '') FROM users WHERE username = notifications.user_email))".to_string()
            } else {
                table.owner.column().to_string()
            };
            let select = format!(
                "SELECT rowid, {owner}, {}, {} FROM {}",
                table
                    .key
                    .iter()
                    .map(|k| format!("CAST({k} AS TEXT)"))
                    .collect::<Vec<_>>()
                    .join(", "),
                table.columns.join(", "),
                table.table
            );
            let rows: Vec<StoredRow> = self
                .conn
                .prepare(&select)?
                .query_map([], |row| {
                    let keys = (0..table.key.len())
                        .map(|i| {
                            row.get::<_, Option<String>>(2 + i)
                                .map(Option::unwrap_or_default)
                        })
                        .collect::<rusqlite::Result<Vec<_>>>()?;
                    let values = (0..table.columns.len())
                        .map(|i| row.get::<_, Option<String>>(2 + table.key.len() + i))
                        .collect::<rusqlite::Result<Vec<_>>>()?;
                    Ok((row.get(0)?, row.get(1)?, keys, values))
                })?
                .collect::<Result<_, _>>()?;
            for (rowid, owner, keys, values) in rows {
                if values.iter().flatten().all(|v| sqlite::is_sealed(v)) {
                    continue;
                }
                let key: Vec<&str> = keys.iter().map(String::as_str).collect();
                let writer = self.writer(table, owner.as_deref(), &key, None)?;
                for (column, value) in table.columns.iter().zip(values) {
                    let Some(value) = value.filter(|v| !sqlite::is_sealed(v)) else {
                        continue;
                    };
                    let value = writer.seal(column, &value)?;
                    tx.execute(
                        &format!("UPDATE {} SET {column} = ?1 WHERE rowid = ?2", table.table),
                        params![value, rowid],
                    )?;
                    sealed += 1;
                }
            }
        }
        tx.commit()?;
        Ok(sealed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::at_rest::keyring_file::path_in;
    use crate::at_rest::sqlite::{database_key, open_keyed, SEALED_PREFIX};
    use crate::at_rest::DataKeys;
    use crate::types::{NotificationInput, PerspectiveHandle, PerspectiveState};
    use rusqlite::Connection;
    use std::path::Path;
    use std::sync::Arc;

    const SECRET_WORDS: [&str; 6] = [
        "secret-perspective-name",
        "neighbourhood://secret-url",
        "secret-description",
        "secret-system-prompt",
        "secret-flow-state",
        "secret-recipient",
    ];

    fn keyring(folder: &Path, byte: u8) -> Arc<DataKeys> {
        let keys = DataKeys::load(path_in(folder)).unwrap();
        keys.unlock(zeroize::Zeroizing::new([byte; 32])).unwrap();
        Arc::new(keys)
    }

    /// A keyed database with a row in every sealed table. Answers the perspective's uuid.
    fn filled(db: &Ad4mDb) -> String {
        db.add_user("alice", "did:key:alice", "pw").unwrap();
        let perspective = uuid::Uuid::new_v4().to_string();
        db.add_perspective(&PerspectiveHandle {
            uuid: perspective.clone(),
            name: Some(SECRET_WORDS[0].into()),
            neighbourhood: None,
            shared_url: Some(SECRET_WORDS[1].into()),
            state: PerspectiveState::Private,
            owners: Some(vec!["did:key:alice".into()]),
        })
        .unwrap();
        db.add_pending_diff(
            &perspective,
            &crate::types::PerspectiveDiff {
                additions: vec![],
                removals: vec![],
            },
        )
        .unwrap();
        db.record_flow_replica_pass(
            &perspective,
            "did:key:alice",
            "instance",
            Some(SECRET_WORDS[4]),
            &[],
            "t",
        )
        .unwrap();
        let notification = |description: &str| NotificationInput {
            description: description.into(),
            app_name: "app".into(),
            app_url: "https://app.test".into(),
            app_icon_path: "icon".into(),
            trigger: "SELECT 1".into(),
            perspective_ids: vec![perspective.clone()],
            webhook_url: "https://webhook.test".into(),
            webhook_auth: "auth".into(),
        };
        db.add_notification(notification(SECRET_WORDS[2]), "did:key:alice", true)
            .unwrap();
        db.add_notification(notification("another"), "did:key:main", true)
            .unwrap();
        db.add_task(
            "task".into(),
            "model".into(),
            SECRET_WORDS[3].into(),
            vec![],
            Some("{}".into()),
            Some("did:key:alice"),
        )
        .unwrap();
        db._add_expression(
            "literal://expression",
            &crate::types::Expression {
                author: "did:key:main".into(),
                timestamp: "t".into(),
                data: serde_json::json!("expression-data"),
                proof: Default::default(),
            },
        )
        .unwrap();
        db.add_to_outbox(&Default::default(), SECRET_WORDS[5].into())
            .unwrap();
        perspective
    }

    /// Every stored value of every sealed column, by table and column.
    fn stored_values(conn: &Connection) -> Vec<(&'static str, &'static str, String)> {
        let mut values = Vec::new();
        for table in SEALED_TABLES {
            for column in table.columns {
                let column_values: Vec<Option<String>> = conn
                    .prepare(&format!("SELECT {column} FROM {}", table.table))
                    .unwrap()
                    .query_map([], |row| row.get(0))
                    .unwrap()
                    .collect::<Result<_, _>>()
                    .unwrap();
                values.extend(
                    column_values
                        .into_iter()
                        .flatten()
                        .map(|v| (table.table, *column, v)),
                );
            }
        }
        values
    }

    #[test]
    fn every_sealed_column_is_sealed_and_reads_back_through_the_db() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path(), 1);
        let key = database_key(&keys).unwrap();
        let path = folder.path().join("ad4m_db.sqlite");
        let db = Ad4mDb::open(&path, Some((&key, keys.clone()))).unwrap();
        let perspective = filled(&db);

        let values = stored_values(&db.conn);
        for table in SEALED_TABLES {
            assert!(
                values.iter().any(|(t, _, _)| *t == table.table),
                "{} has no row",
                table.table
            );
        }
        for (table, column, value) in &values {
            assert!(
                value.starts_with(SEALED_PREFIX),
                "{table}.{column} is not sealed: {value}"
            );
        }

        let handle = db._get_perspective(&perspective).unwrap().unwrap();
        assert_eq!(handle.name.as_deref(), Some(SECRET_WORDS[0]));
        assert_eq!(
            db.get_neighbourhood_owners(SECRET_WORDS[1]).unwrap(),
            vec!["did:key:alice".to_string()]
        );
        let mine = db.get_notifications_for_owner("did:key:alice").unwrap();
        assert_eq!(mine[0].description, SECRET_WORDS[2]);
        assert_eq!(mine[0].perspective_ids, vec![perspective.clone()]);
        let tasks = db.get_tasks().unwrap();
        assert_eq!(tasks[0].system_prompt, SECRET_WORDS[3]);
        let state = db
            .get_flow_replica_state(&perspective, "did:key:alice", "instance")
            .unwrap()
            .unwrap();
        assert_eq!(state.current_state.as_deref(), Some(SECRET_WORDS[4]));
        assert_eq!(
            db.get_all_from_outbox().unwrap()[0].recipient,
            SECRET_WORDS[5]
        );
        assert_eq!(
            db._get_expression("literal://expression")
                .unwrap()
                .unwrap()
                .data,
            serde_json::json!("expression-data")
        );
        assert_eq!(db.get_pending_diffs(&perspective, None).unwrap().1.len(), 1);

        // A re-key reseals the flow state for its new row.
        db.move_flow_rows("did:key:alice", "did:key:alice2")
            .unwrap();
        let moved = db
            .get_flow_replica_state(&perspective, "did:key:alice2", "instance")
            .unwrap()
            .unwrap();
        assert_eq!(moved.current_state.as_deref(), Some(SECRET_WORDS[4]));
    }

    /// A perspective's store key: made when the perspective is added, wrapped for a person who
    /// joins its neighbourhood, unwrapped for one dropped from its owners, and destroyed after
    /// the perspective is removed, so its sealed values no longer open anywhere.
    #[test]
    fn a_perspectives_store_key_follows_its_owners_and_goes_with_the_perspective() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path(), 1);
        let key = database_key(&keys).unwrap();
        let path = folder.path().join("ad4m_db.sqlite");
        let db = Ad4mDb::open(&path, Some((&key, keys.clone()))).unwrap();
        let alice = KeyOwner::User("alice".into());
        let bob = KeyOwner::User("bob".into());
        db.add_user("bob", "did:key:bob", "pw").unwrap();

        // Added: the store exists, wrapped for its one owner.
        let uuid = filled(&db);
        let store = StoreId::Perspective(uuid::Uuid::parse_str(&uuid).unwrap());
        assert_eq!(keys.store_owners(&store).unwrap(), vec![alice.clone()]);

        // Bob joins its neighbourhood: he gets a wrap too.
        db.add_owner_to_neighbourhood(SECRET_WORDS[1], "did:key:bob")
            .unwrap();
        assert_eq!(
            keys.store_owners(&store).unwrap(),
            vec![alice.clone(), bob.clone()]
        );

        // The handle drops alice: her wrap goes, bob still reads the perspective.
        let mut handle = db._get_perspective(&uuid).unwrap().unwrap();
        handle.owners = Some(vec!["did:key:bob".into()]);
        db.update_perspective(&handle).unwrap();
        assert_eq!(keys.store_owners(&store).unwrap(), vec![bob]);
        assert_eq!(
            db._get_perspective(&uuid).unwrap().unwrap().name.as_deref(),
            Some(SECRET_WORDS[0])
        );

        // Removed: first the rows, then (the files and) the key.
        let stored_name: String = db
            .conn
            .query_row(
                "SELECT name FROM perspective_handle WHERE uuid = ?1",
                [&uuid],
                |r| r.get(0),
            )
            .unwrap();
        let row = row_key(&[&uuid]);
        let cell = Cell {
            table: "perspective_handle",
            column: "name",
            row: &row,
        };
        assert_eq!(
            sqlite::open_value(&keys, &store, &cell, &stored_name).unwrap(),
            SECRET_WORDS[0]
        );
        assert!(db.perspective_stores_without_handle().unwrap().is_empty());
        db.remove_perspective(&uuid).unwrap();
        // A removal that stopped here is found at the next start.
        assert_eq!(
            db.perspective_stores_without_handle().unwrap(),
            vec![uuid.clone()]
        );
        db.destroy_perspective_store(&uuid).unwrap();
        assert_eq!(
            keys.store_owners(&store),
            Err(crate::at_rest::KeyringError::UnknownStore)
        );
        // A copy of the sealed value (a freed page, a backup) no longer opens.
        assert!(sqlite::open_value(&keys, &store, &cell, &stored_name).is_err());
        assert!(db.perspective_stores_without_handle().unwrap().is_empty());
        // Destroying it again is not an error.
        db.destroy_perspective_store(&uuid).unwrap();

        // Also after a restart: the key is gone from the keyring file.
        drop(db);
        let again = keyring(folder.path(), 1);
        assert!(sqlite::open_value(&again, &store, &cell, &stored_name).is_err());
        assert_eq!(
            again.store_owners(&store),
            Err(crate::at_rest::KeyringError::UnknownStore)
        );
    }

    /// The rows of a person's own store go with one call; another person's rows, the
    /// executor's rows and every perspective's rows stay. An empty owner names the executor,
    /// so it deletes nothing.
    #[test]
    fn a_persons_rows_go_and_no_one_elses() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path(), 1);
        let key = database_key(&keys).unwrap();
        let db =
            Ad4mDb::open(&folder.path().join("db.sqlite"), Some((&key, keys.clone()))).unwrap();
        let perspective = filled(&db);
        assert_eq!(db.delete_person_rows("").unwrap(), 0);
        let expressions = |db: &Ad4mDb| {
            db.conn
                .query_row("SELECT count(*) FROM expression", [], |r| {
                    r.get::<_, i64>(0)
                })
                .unwrap()
        };
        let before = expressions(&db);

        // Alice's notification and task.
        assert_eq!(db.delete_person_rows("did:key:alice").unwrap(), 2);
        assert!(db
            .get_notifications_for_owner("did:key:alice")
            .unwrap()
            .is_empty());
        assert!(db.get_tasks().unwrap().is_empty());
        assert_eq!(
            db.get_notifications_for_owner("did:key:main")
                .unwrap()
                .len(),
            1
        );
        assert_eq!(expressions(&db), before);
        assert!(db._get_perspective(&perspective).unwrap().is_some());
        assert_eq!(db.delete_person_rows("did:key:alice").unwrap(), 0);
    }

    #[test]
    fn a_sealed_value_opens_only_under_its_store_and_not_in_another_row() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path(), 1);
        let key = database_key(&keys).unwrap();
        let db =
            Ad4mDb::open(&folder.path().join("db.sqlite"), Some((&key, keys.clone()))).unwrap();
        filled(&db);
        let (alices, mains): (Vec<_>, Vec<_>) = db
            .get_notifications()
            .unwrap()
            .into_iter()
            .partition(|n| n.owner_did == "did:key:alice");
        let (alices, mains) = (&alices[0], &mains[0]);
        let stored: String = db
            .conn
            .query_row(
                "SELECT description FROM notifications WHERE id = ?1",
                [&alices.id],
                |r| r.get(0),
            )
            .unwrap();

        // Under another person's store it does not open.
        let cell = Cell {
            table: "notifications",
            column: "description",
            row: &alices.id,
        };
        let alice = StoreId::Person(KeyOwner::User("alice".into()));
        assert_eq!(
            sqlite::open_value(&keys, &alice, &cell, &stored).unwrap(),
            SECRET_WORDS[2]
        );
        assert!(matches!(
            sqlite::open_value(&keys, &StoreId::Person(KeyOwner::Main), &cell, &stored),
            Err(SealError::DoesNotOpen { .. })
        ));

        // Copied into the main agent's row it does not open there: the row is left out.
        db.conn
            .execute(
                "UPDATE notifications SET description = ?1 WHERE id = ?2",
                params![stored, mains.id],
            )
            .unwrap();
        let error = db.get_notification(mains.id.clone()).unwrap();
        assert!(error.is_none(), "{error:?}");
        let left = db.get_notifications().unwrap();
        assert_eq!(left.len(), 1);
        assert_eq!(left[0].id, alices.id);

        // A value in the clear in a keyed database is refused too.
        db.conn
            .execute(
                "UPDATE notifications SET description = 'clear' WHERE id = ?1",
                params![alices.id],
            )
            .unwrap();
        assert!(db.get_notifications().unwrap().is_empty());
    }

    #[test]
    fn the_file_is_unreadable_in_plain_sqlite_and_its_values_with_the_database_key_alone() {
        let folder = tempfile::tempdir().unwrap();
        let keys = keyring(folder.path(), 1);
        let key = database_key(&keys).unwrap();
        let path = folder.path().join("ad4m_db.sqlite");
        filled(&Ad4mDb::open(&path, Some((&key, keys.clone()))).unwrap());

        // Plain SQLite: not a database.
        let plain = Connection::open(&path).unwrap();
        let e = plain
            .query_row("SELECT count(*) FROM sqlite_master", [], |r| {
                r.get::<_, i64>(0)
            })
            .unwrap_err();
        assert_eq!(
            e.sqlite_error_code(),
            Some(rusqlite::ErrorCode::NotADatabase),
            "{e}"
        );
        let bytes = std::fs::read(&path).unwrap();
        for word in SECRET_WORDS {
            assert!(
                !bytes.windows(word.len()).any(|w| w == word.as_bytes()),
                "{word} is in the file"
            );
        }

        // The database key alone (another keyring holding it, without the persons' and the
        // perspectives' store keys): the file opens, the sealed values do not.
        let conn = open_keyed(&path, &key).unwrap();
        let other = tempfile::tempdir().unwrap();
        let without_stores = keyring(other.path(), 2);
        let values = stored_values(&conn);
        for (table, column, value) in &values {
            for word in SECRET_WORDS {
                assert!(!value.contains(word), "{table}.{column} shows {word}");
            }
        }
        let description: (String, String) = conn
            .query_row(
                "SELECT id, description FROM notifications WHERE owner_did = 'did:key:alice'",
                [],
                |r| Ok((r.get(0)?, r.get(1)?)),
            )
            .unwrap();
        let cell = Cell {
            table: "notifications",
            column: "description",
            row: &description.0,
        };
        let alice = StoreId::Person(KeyOwner::User("alice".into()));
        assert!(sqlite::open_value(&without_stores, &alice, &cell, &description.1).is_err());
        assert_eq!(
            sqlite::open_value(&keys, &alice, &cell, &description.1).unwrap(),
            SECRET_WORDS[2]
        );
    }

    #[test]
    fn no_index_where_or_order_by_names_a_sealed_column() {
        // The schema: no index of a sealed table covers a sealed column.
        let db = Ad4mDb::new(":memory:").unwrap();
        for table in SEALED_TABLES {
            let indexes: Vec<String> = db
                .conn
                .prepare(&format!(
                    "SELECT name FROM pragma_index_list('{}')",
                    table.table
                ))
                .unwrap()
                .query_map([], |r| r.get(0))
                .unwrap()
                .collect::<Result<_, _>>()
                .unwrap();
            for index in indexes {
                let columns: Vec<Option<String>> = db
                    .conn
                    .prepare(&format!("SELECT name FROM pragma_index_info('{index}')"))
                    .unwrap()
                    .query_map([], |r| r.get(0))
                    .unwrap()
                    .collect::<Result<_, _>>()
                    .unwrap();
                for column in columns.into_iter().flatten() {
                    assert!(
                        !table.columns.contains(&column.as_str()),
                        "index {index} covers {}.{column}",
                        table.table
                    );
                }
            }
        }

        // The statements: every SQL literal of the database code that reads or writes a sealed
        // table names none of its sealed columns after WHERE or ORDER BY.
        let src = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
        let mut files = vec![src.join("db.rs")];
        for entry in std::fs::read_dir(src.join("db")).unwrap() {
            files.push(entry.unwrap().path());
        }
        let literal = regex::Regex::new(r#""((?:[^"\\]|\\.)*)""#).unwrap();
        let clause = regex::Regex::new(r"(?is)\b(WHERE|ORDER\s+BY)\b(.*)").unwrap();
        let mut checked = 0;
        for file in files {
            let text = std::fs::read_to_string(&file).unwrap();
            // The code, not its tests: a test may read a sealed column to show it is sealed.
            let code = text.split("#[cfg(test)]\nmod tests").next().unwrap();
            for sql in literal.captures_iter(code).map(|c| c[1].to_string()) {
                for table in SEALED_TABLES {
                    let names = regex::Regex::new(&format!(
                        r"(?i)\b(FROM|UPDATE|JOIN|INTO)\s+{}\b",
                        table.table
                    ))
                    .unwrap();
                    if !names.is_match(&sql) {
                        continue;
                    }
                    checked += 1;
                    let Some(tail) = clause.captures(&sql).map(|c| c[2].to_string()) else {
                        continue;
                    };
                    for column in table.columns {
                        let named = regex::Regex::new(&format!(r"\b{column}\b")).unwrap();
                        assert!(
                            !named.is_match(&tail),
                            "{}: {sql:?} names {}.{column} in its WHERE or ORDER BY",
                            file.display(),
                            table.table
                        );
                    }
                }
            }
        }
        assert!(checked > 20, "only {checked} statements found");
    }
}

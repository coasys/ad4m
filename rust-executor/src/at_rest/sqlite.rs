//! The executor database (`ad4m_db.sqlite`) under its key.
//!
//! **The file.** SQLCipher encrypts every page under the database key: the executor store's
//! SQLite subkey. [`key_connection`] gives a connection the key before anything else touches
//! the file, turns on SQLCipher's memory wiping, keeps temporary tables in memory, silences
//! SQLCipher's own log and reads once, so a wrong key fails at open and not at the first
//! query. Every connection to the file opens through [`open_database`], which keys it with
//! the key `run` installed ([`install`]) once the keyring is open.
//!
//! **Sealed values.** A column that holds a person's or a perspective's data (see
//! `crate::db`'s sealed columns) holds [`SEALED_PREFIX`] + base64 of
//! `key version (u32, big endian) | nonce (24) | XChaCha20-Poly1305 ciphertext and tag`, under
//! the Rows subkey of the store that owns the row. The associated data names the table, the
//! column, the row's key and the store, so a value copied into another row, column or store
//! does not open. The database key alone does not open a sealed value: that takes the
//! owner's store key.
//!
//! **From plaintext.** A data folder from before keeps a plaintext database.
//! [`migrate_to_sqlcipher`] exports it into a new keyed file beside it, lets the caller seal
//! the sealed columns there, checks the new file, and only then renames it over the
//! plaintext. A failure leaves the plaintext as it was and removes the new file.

use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};

use ad4m_keyring::{KeyVersion, SecretKey};
use base64::Engine;
use chacha20poly1305::aead::{Aead, KeyInit, Payload};
use chacha20poly1305::{XChaCha20Poly1305, XNonce};
use rand::RngCore;
use rusqlite::Connection;
use zeroize::Zeroizing;

use super::data_keys::{DataKeys, KeyringError, SubkeyPurpose};
use super::files;
use super::keyring_file::StoreId;

/// What a sealed value starts with.
pub const SEALED_PREFIX: &str = "s1:";

const NONCE_LEN: usize = 24;
const VERSION_LEN: usize = 4;
/// The first 16 bytes of every plaintext SQLite file.
const PLAINTEXT_HEADER: &[u8; 16] = b"SQLite format 3\0";
/// The associated data's label: the format of a sealed value.
const SEALED_AD_LABEL: &[u8] = b"ad4m:at-rest:sealed-value:v1";

/// Why the database did not open under its key.
#[derive(Debug, thiserror::Error)]
pub enum DbKeyError {
    #[error("the database {0} does not open under this executor's database key (a wrong key, or a file that is not an executor database)")]
    WrongKey(String),
    #[error("database: {0}")]
    Sqlite(#[from] rusqlite::Error),
    #[error("database file: {0}")]
    Io(#[from] io::Error),
}

/// The executor database's key: the executor store's SQLite subkey (of its current version).
pub fn database_key(keys: &DataKeys) -> Result<SecretKey, KeyringError> {
    keys.subkey(&StoreId::Executor, None, SubkeyPurpose::Sqlite)
        .map(|(_, key)| key)
}

/// `x'<hex>'`: SQLCipher's raw key form, so it uses the key as it is and derives nothing.
fn raw_key_literal(key: &SecretKey) -> Zeroizing<String> {
    let mut literal = Zeroizing::new(String::with_capacity(3 + 64));
    literal.push_str("x'");
    literal.push_str(&Zeroizing::new(hex::encode(key.expose_secret())));
    literal.push('\'');
    literal
}

/// Keys `conn` with `key` and reads once. Must be the first statement on the connection. A
/// key that does not open the file is [`DbKeyError::WrongKey`].
pub fn key_connection(conn: &Connection, key: &SecretKey) -> Result<(), DbKeyError> {
    let literal = raw_key_literal(key);
    let mut pragma = Zeroizing::new(String::with_capacity(literal.len() + 16));
    pragma.push_str("PRAGMA key = \"");
    pragma.push_str(&literal);
    pragma.push_str("\";");
    conn.execute_batch(&pragma)?;
    conn.execute_batch(
        "PRAGMA cipher_memory_security = ON;
         PRAGMA temp_store = MEMORY;
         PRAGMA cipher_log_level = NONE;",
    )?;
    conn.query_row("SELECT count(*) FROM sqlite_master", [], |row| {
        row.get::<_, i64>(0)
    })
    .map_err(|e| match e.sqlite_error_code() {
        Some(rusqlite::ErrorCode::NotADatabase) => {
            DbKeyError::WrongKey(conn.path().unwrap_or("(memory)").to_string())
        }
        _ => DbKeyError::Sqlite(e),
    })?;
    Ok(())
}

/// Opens the database at `path` under `key`. A file that does not exist yet is made 0600
/// first, so the journal and WAL files SQLite makes beside it are 0600 too (SQLite gives them
/// the database file's mode).
pub fn open_keyed(path: &Path, key: &SecretKey) -> Result<Connection, DbKeyError> {
    make_private(path)?;
    let conn = Connection::open(path)?;
    key_connection(&conn, key)?;
    Ok(conn)
}

fn make_private(path: &Path) -> io::Result<()> {
    match files::open_private_file(path, true) {
        Ok(_) => Ok(()),
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => Ok(()),
        Err(e) => Err(e),
    }
}

/// What the file at a database path is.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DatabaseFile {
    /// No file, or an empty one: a new database.
    New,
    /// A plaintext SQLite database.
    Plaintext,
    /// Anything else: an encrypted database (or not a database at all).
    Encrypted,
}

/// Reads the header of the file at `path`.
pub fn database_file(path: &Path) -> io::Result<DatabaseFile> {
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(DatabaseFile::New),
        Err(e) => return Err(e),
    };
    let mut header = [0u8; 16];
    let mut read = 0;
    while read < header.len() {
        match file.read(&mut header[read..])? {
            0 => break,
            n => read += n,
        }
    }
    Ok(match read {
        0 => DatabaseFile::New,
        16 if &header == PLAINTEXT_HEADER => DatabaseFile::Plaintext,
        _ => DatabaseFile::Encrypted,
    })
}

/// The keys this process opens the executor database with.
pub struct DatabaseKeys {
    /// The SQLCipher key of the file.
    pub sqlite: SecretKey,
    /// The keyring whose store keys seal the sealed columns.
    pub rows: Arc<DataKeys>,
}

static INSTALLED: OnceLock<DatabaseKeys> = OnceLock::new();

/// Installs the database keys of this process: every later [`open_database`] keys its
/// connection with them, and the executor database seals its sealed columns. `run` calls it
/// once the database is under its key; a second call is refused.
pub fn install(keys: DatabaseKeys) -> Result<(), DatabaseKeys> {
    INSTALLED.set(keys)
}

/// The installed database keys, if any.
pub fn installed() -> Option<&'static DatabaseKeys> {
    INSTALLED.get()
}

/// Opens a connection to the executor database at `path`: under the installed key, or in the
/// clear when none is installed (a plaintext database, unit tests).
pub fn open_database(path: &Path) -> Result<Connection, DbKeyError> {
    match installed() {
        Some(keys) => open_keyed(path, &keys.sqlite),
        None => Ok(Connection::open(path)?),
    }
}

/// Where a sealed value sits: what its associated data binds it to.
#[derive(Clone, Copy, Debug)]
pub struct Cell<'a> {
    pub table: &'a str,
    pub column: &'a str,
    /// The row's primary key, as text (the parts of a composite key joined by the caller).
    pub row: &'a str,
}

/// Why a value was not sealed or opened.
#[derive(Debug, thiserror::Error)]
pub enum SealError {
    #[error("{0}")]
    Keyring(#[from] KeyringError),
    #[error("{table}.{column} of row {row} is not sealed")]
    NotSealed {
        table: String,
        column: String,
        row: String,
    },
    #[error("{table}.{column} of row {row} does not open under the key of {store}")]
    DoesNotOpen {
        table: String,
        column: String,
        row: String,
        store: String,
    },
}

/// Whether `value` is a sealed value.
pub fn is_sealed(value: &str) -> bool {
    value.starts_with(SEALED_PREFIX)
}

/// The version of the store key a sealed value is sealed under; `None` for a value that is
/// not a sealed value.
pub fn sealed_version(value: &str) -> Option<KeyVersion> {
    let encoded = value.strip_prefix(SEALED_PREFIX)?;
    // The version is the first 4 bytes: the first 8 base64 characters hold them.
    let head = encoded.get(..8)?;
    let bytes = base64::engine::general_purpose::STANDARD
        .decode(head)
        .ok()?;
    let version: [u8; VERSION_LEN] = bytes.get(..VERSION_LEN)?.try_into().ok()?;
    Some(KeyVersion(u32::from_be_bytes(version)))
}

fn associated_data(store: &StoreId, cell: &Cell) -> Vec<u8> {
    let store = store.to_string();
    let mut ad = Vec::with_capacity(
        SEALED_AD_LABEL.len() + cell.table.len() + cell.column.len() + cell.row.len() + 64,
    );
    for part in [
        SEALED_AD_LABEL,
        cell.table.as_bytes(),
        cell.column.as_bytes(),
        cell.row.as_bytes(),
        store.as_bytes(),
    ] {
        ad.extend_from_slice(&(part.len() as u32).to_be_bytes());
        ad.extend_from_slice(part);
    }
    ad
}

/// Seals `plain` for `cell` under the current Rows subkey of `store`.
pub fn seal_value(
    keys: &DataKeys,
    store: &StoreId,
    cell: &Cell,
    plain: &str,
) -> Result<String, SealError> {
    let (version, key) = keys.subkey(store, None, SubkeyPurpose::Rows)?;
    let cipher = XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key");
    let mut nonce = [0u8; NONCE_LEN];
    rand::thread_rng().fill_bytes(&mut nonce);
    let ad = associated_data(store, cell);
    let sealed = cipher
        .encrypt(
            XNonce::from_slice(&nonce),
            Payload {
                msg: plain.as_bytes(),
                aad: &ad,
            },
        )
        .expect("XChaCha20-Poly1305 seals any length this database holds");
    let mut bytes = Vec::with_capacity(VERSION_LEN + NONCE_LEN + sealed.len());
    bytes.extend_from_slice(&version.0.to_be_bytes());
    bytes.extend_from_slice(&nonce);
    bytes.extend_from_slice(&sealed);
    let mut text = String::from(SEALED_PREFIX);
    base64::engine::general_purpose::STANDARD.encode_string(bytes, &mut text);
    Ok(text)
}

/// Opens the sealed value `sealed` of `cell`, sealed under a Rows subkey of `store`.
pub fn open_value(
    keys: &DataKeys,
    store: &StoreId,
    cell: &Cell,
    sealed: &str,
) -> Result<String, SealError> {
    let does_not_open = || SealError::DoesNotOpen {
        table: cell.table.to_string(),
        column: cell.column.to_string(),
        row: cell.row.to_string(),
        store: store.to_string(),
    };
    let encoded = sealed
        .strip_prefix(SEALED_PREFIX)
        .ok_or_else(|| SealError::NotSealed {
            table: cell.table.to_string(),
            column: cell.column.to_string(),
            row: cell.row.to_string(),
        })?;
    let bytes = base64::engine::general_purpose::STANDARD
        .decode(encoded)
        .map_err(|_| does_not_open())?;
    if bytes.len() < VERSION_LEN + NONCE_LEN {
        return Err(does_not_open());
    }
    let (version, rest) = bytes.split_at(VERSION_LEN);
    let (nonce, ciphertext) = rest.split_at(NONCE_LEN);
    let version = KeyVersion(u32::from_be_bytes(version.try_into().expect("4 bytes")));
    let (_, key) = keys
        .subkey(store, Some(version), SubkeyPurpose::Rows)
        .map_err(|e| match e {
            KeyringError::UnknownStore => does_not_open(),
            other => SealError::Keyring(other),
        })?;
    let cipher = XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key");
    let ad = associated_data(store, cell);
    let plain = cipher
        .decrypt(
            XNonce::from_slice(nonce),
            Payload {
                msg: ciphertext,
                aad: &ad,
            },
        )
        .map_err(|_| does_not_open())?;
    String::from_utf8(plain).map_err(|_| does_not_open())
}

/// What a migration to SQLCipher moved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MigrationReport {
    /// Every table of the plaintext database with its row count, the same in the new file.
    pub tables: Vec<(String, i64)>,
    pub user_version: i64,
}

/// Why a migration to SQLCipher failed. The plaintext database is as it was.
#[derive(Debug, thiserror::Error)]
#[error(
    "the database did not move under its key ({stage}): {cause}; the plaintext database is kept"
)]
pub struct MigrationError {
    pub stage: &'static str,
    pub cause: String,
}

fn failed(stage: &'static str) -> impl Fn(String) -> MigrationError {
    move |cause| MigrationError { stage, cause }
}

/// The new file a migration of `path` writes before it replaces `path`.
pub fn migration_file(path: &Path) -> PathBuf {
    let mut name = path.as_os_str().to_owned();
    name.push(".enc");
    PathBuf::from(name)
}

fn sidecars(path: &Path) -> [PathBuf; 3] {
    ["-wal", "-shm", "-journal"].map(|suffix| {
        let mut name = path.as_os_str().to_owned();
        name.push(suffix);
        PathBuf::from(name)
    })
}

fn remove_if_present(path: &Path) -> io::Result<()> {
    match std::fs::remove_file(path) {
        Err(e) if e.kind() != io::ErrorKind::NotFound => Err(e),
        _ => Ok(()),
    }
}

fn table_counts(conn: &Connection, schema: &str) -> rusqlite::Result<Vec<(String, i64)>> {
    let names: Vec<String> = conn
        .prepare(&format!(
            "SELECT name FROM {schema}.sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name"
        ))?
        .query_map([], |row| row.get(0))?
        .collect::<Result<_, _>>()?;
    names
        .into_iter()
        .map(|name| {
            let count = conn.query_row(
                &format!(
                    "SELECT count(*) FROM {schema}.\"{}\"",
                    name.replace('"', "\"\"")
                ),
                [],
                |row| row.get(0),
            )?;
            Ok((name, count))
        })
        .collect()
}

/// Moves the plaintext database at `path` under `key`:
///
/// 1. reads the plaintext's tables, row counts and `user_version`;
/// 2. exports it into a new file beside it ([`migration_file`], 0600) keyed with `key`, and
///    copies `user_version` (the export does not);
/// 3. runs `seal` on the new file (the caller opens it under `key` and seals its sealed
///    columns there);
/// 4. opens the new file again under `key` and checks it: integrity, every table of the
///    plaintext with the same row count, the same `user_version`;
/// 5. syncs the new file, renames it over the plaintext, syncs the folder, and removes the
///    plaintext's journal files.
///
/// Until the rename the plaintext is untouched; a failure before it removes the new file and
/// leaves the plaintext.
pub fn migrate_to_sqlcipher(
    path: &Path,
    key: &SecretKey,
    seal: impl FnOnce(&Path) -> Result<(), String>,
) -> Result<MigrationReport, MigrationError> {
    let new_file = migration_file(path);
    let result = export_and_check(path, &new_file, key, seal);
    if result.is_err() {
        for leftover in std::iter::once(new_file.clone()).chain(sidecars(&new_file)) {
            if let Err(e) = remove_if_present(&leftover) {
                log::warn!(
                    "Could not remove {} of a failed migration: {e}",
                    leftover.display()
                );
            }
        }
        return result;
    }
    let report = result?;
    replace(path, &new_file).map_err(|e| failed("replace")(e.to_string()))?;
    Ok(report)
}

fn export_and_check(
    path: &Path,
    new_file: &Path,
    key: &SecretKey,
    seal: impl FnOnce(&Path) -> Result<(), String>,
) -> Result<MigrationReport, MigrationError> {
    // A new file a crashed migration left is not a database anybody uses.
    for leftover in std::iter::once(new_file.to_path_buf()).chain(sidecars(new_file)) {
        remove_if_present(&leftover).map_err(|e| failed("prepare")(e.to_string()))?;
    }
    let plain = Connection::open(path).map_err(|e| failed("read")(e.to_string()))?;
    let tables = table_counts(&plain, "main").map_err(|e| failed("read")(e.to_string()))?;
    let user_version: i64 = plain
        .query_row("PRAGMA user_version", [], |row| row.get(0))
        .map_err(|e| failed("read")(e.to_string()))?;

    files::open_private_file(new_file, true).map_err(|e| failed("export")(e.to_string()))?;
    let literal = raw_key_literal(key);
    let target = new_file
        .to_str()
        .ok_or_else(|| failed("export")("the path is not UTF-8".to_string()))?;
    plain
        .execute(
            "ATTACH DATABASE ?1 AS migrated KEY ?2",
            rusqlite::params![target, literal.as_str()],
        )
        .map_err(|e| failed("export")(e.to_string()))?;
    plain
        .query_row("SELECT sqlcipher_export('migrated')", [], |_| Ok(()))
        .map_err(|e| failed("export")(e.to_string()))?;
    plain
        .execute_batch(&format!("PRAGMA migrated.user_version = {user_version};"))
        .map_err(|e| failed("export")(e.to_string()))?;
    plain
        .execute_batch("DETACH DATABASE migrated;")
        .map_err(|e| failed("export")(e.to_string()))?;
    drop(plain);

    seal(new_file).map_err(failed("seal"))?;

    let check = open_keyed(new_file, key).map_err(|e| failed("check")(e.to_string()))?;
    let integrity: String = check
        .query_row("PRAGMA quick_check", [], |row| row.get(0))
        .map_err(|e| failed("check")(e.to_string()))?;
    if integrity != "ok" {
        return Err(failed("check")(format!("integrity: {integrity}")));
    }
    let moved = table_counts(&check, "main").map_err(|e| failed("check")(e.to_string()))?;
    for (table, count) in &tables {
        match moved.iter().find(|(name, _)| name == table) {
            Some((_, n)) if n == count => {}
            Some((_, n)) => {
                return Err(failed("check")(format!(
                    "table {table} has {n} rows, the plaintext {count}"
                )))
            }
            None => return Err(failed("check")(format!("table {table} is missing"))),
        }
    }
    let moved_version: i64 = check
        .query_row("PRAGMA user_version", [], |row| row.get(0))
        .map_err(|e| failed("check")(e.to_string()))?;
    if moved_version != user_version {
        return Err(failed("check")(format!(
            "user_version {moved_version}, the plaintext {user_version}"
        )));
    }
    drop(check);
    File::open(new_file)
        .and_then(|f| f.sync_all())
        .map_err(|e| failed("sync")(e.to_string()))?;
    Ok(MigrationReport {
        tables,
        user_version,
    })
}

/// Renames `new_file` over `path`, syncs the folder, and removes `path`'s old journal files.
fn replace(path: &Path, new_file: &Path) -> io::Result<()> {
    std::fs::rename(new_file, path)?;
    if let Some(dir) = path.parent() {
        let dir = if dir.as_os_str().is_empty() {
            Path::new(".")
        } else {
            dir
        };
        File::open(dir)?.sync_all()?;
    }
    for sidecar in sidecars(path) {
        remove_if_present(&sidecar)?;
    }
    files::restrict(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::at_rest::keyring_file::KeyOwner;

    fn keyring() -> (tempfile::TempDir, DataKeys) {
        let folder = tempfile::tempdir().unwrap();
        let keys = DataKeys::load(crate::at_rest::keyring_file::path_in(folder.path())).unwrap();
        keys.unlock(Zeroizing::new([7u8; 32])).unwrap();
        (folder, keys)
    }

    fn not_a_database(e: rusqlite::Error) -> bool {
        e.sqlite_error_code() == Some(rusqlite::ErrorCode::NotADatabase)
    }

    #[test]
    fn a_keyed_database_does_not_open_without_its_key_or_with_another() {
        let folder = tempfile::tempdir().unwrap();
        let path = folder.path().join("db.sqlite");
        let key = SecretKey::random();
        let conn = open_keyed(&path, &key).unwrap();
        conn.execute_batch("CREATE TABLE t (v TEXT); INSERT INTO t VALUES ('secret');")
            .unwrap();
        drop(conn);
        assert_eq!(database_file(&path).unwrap(), DatabaseFile::Encrypted);
        assert!(!std::fs::read(&path)
            .unwrap()
            .windows(6)
            .any(|w| w == b"secret"));

        let unkeyed = Connection::open(&path).unwrap();
        let err = unkeyed
            .query_row("SELECT count(*) FROM sqlite_master", [], |r| {
                r.get::<_, i64>(0)
            })
            .unwrap_err();
        assert!(not_a_database(err), "no key: not a database");

        match open_keyed(&path, &SecretKey::random()) {
            Err(DbKeyError::WrongKey(_)) => {}
            other => panic!("another key opened the database: {:?}", other.map(|_| ())),
        }
        let reopened = open_keyed(&path, &key).unwrap();
        let v: String = reopened
            .query_row("SELECT v FROM t", [], |r| r.get(0))
            .unwrap();
        assert_eq!(v, "secret");
    }

    #[cfg(unix)]
    #[test]
    fn the_database_and_its_wal_and_shm_files_are_0600() {
        use std::os::unix::fs::PermissionsExt;
        let folder = tempfile::tempdir().unwrap();
        let path = folder.path().join("db.sqlite");
        let conn = open_keyed(&path, &SecretKey::random()).unwrap();
        let mode: String = conn
            .query_row("PRAGMA journal_mode = WAL", [], |r| r.get(0))
            .unwrap();
        assert_eq!(mode, "wal");
        conn.execute_batch("CREATE TABLE t (v TEXT); INSERT INTO t VALUES ('x');")
            .unwrap();
        let mode = |p: &Path| std::fs::metadata(p).unwrap().permissions().mode() & 0o777;
        let [wal, shm, _] = sidecars(&path);
        for file in [&path, &wal, &shm] {
            assert!(file.exists(), "{}", file.display());
            assert_eq!(mode(file), 0o600, "{}", file.display());
        }
    }

    #[test]
    fn a_sealed_value_opens_only_in_its_cell_and_under_its_store() {
        let (_folder, keys) = keyring();
        let alice = KeyOwner::User("alice".into());
        let alices = StoreId::Person(alice.clone());
        let mains = StoreId::Person(KeyOwner::Main);
        keys.ensure_store(&alices, &[alice]).unwrap();
        keys.ensure_store(&mains, &[KeyOwner::Main]).unwrap();
        let cell = Cell {
            table: "tasks",
            column: "name",
            row: "t1",
        };
        let sealed = seal_value(&keys, &alices, &cell, "alice's task").unwrap();
        assert!(sealed.starts_with(SEALED_PREFIX));
        assert!(!sealed.contains("alice"));
        assert_eq!(
            open_value(&keys, &alices, &cell, &sealed).unwrap(),
            "alice's task"
        );
        for (store, other) in [
            (&alices, Cell { row: "t2", ..cell }),
            (
                &alices,
                Cell {
                    column: "system_prompt",
                    ..cell
                },
            ),
            (
                &alices,
                Cell {
                    table: "notifications",
                    ..cell
                },
            ),
            (&mains, cell),
        ] {
            assert!(
                matches!(
                    open_value(&keys, store, &other, &sealed),
                    Err(SealError::DoesNotOpen { .. })
                ),
                "{other:?} under {store}"
            );
        }
        assert!(matches!(
            open_value(&keys, &alices, &cell, "alice's task"),
            Err(SealError::NotSealed { .. })
        ));
    }

    fn plaintext_db(path: &Path) {
        let conn = Connection::open(path).unwrap();
        conn.execute_batch(
            "CREATE TABLE a (id INTEGER PRIMARY KEY, v TEXT);
             CREATE TABLE b (k TEXT PRIMARY KEY);
             INSERT INTO a (v) VALUES ('one'), ('two'), ('three');
             INSERT INTO b VALUES ('x'), ('y');
             PRAGMA user_version = 7;",
        )
        .unwrap();
    }

    #[test]
    fn a_migration_keeps_every_row_and_the_user_version() {
        let folder = tempfile::tempdir().unwrap();
        let path = folder.path().join("db.sqlite");
        plaintext_db(&path);
        assert_eq!(database_file(&path).unwrap(), DatabaseFile::Plaintext);
        let key = SecretKey::random();
        let mut sealed_in = None;
        let report = migrate_to_sqlcipher(&path, &key, |file| {
            sealed_in = Some(file.to_path_buf());
            let conn = open_keyed(file, &key).map_err(|e| e.to_string())?;
            conn.execute("UPDATE a SET v = 's1:' || v", [])
                .map(|_| ())
                .map_err(|e| e.to_string())
        })
        .unwrap();
        assert_eq!(sealed_in, Some(migration_file(&path)));
        assert_eq!(
            report,
            MigrationReport {
                tables: vec![("a".into(), 3), ("b".into(), 2)],
                user_version: 7
            }
        );
        assert_eq!(database_file(&path).unwrap(), DatabaseFile::Encrypted);
        assert!(!migration_file(&path).exists());
        let conn = open_keyed(&path, &key).unwrap();
        let values: Vec<String> = conn
            .prepare("SELECT v FROM a ORDER BY id")
            .unwrap()
            .query_map([], |r| r.get(0))
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap();
        assert_eq!(values, ["s1:one", "s1:two", "s1:three"]);
        let version: i64 = conn
            .query_row("PRAGMA user_version", [], |r| r.get(0))
            .unwrap();
        assert_eq!(version, 7);
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(&path).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode, 0o600);
        }
    }

    #[test]
    fn a_failed_migration_keeps_the_plaintext_and_says_why() {
        let folder = tempfile::tempdir().unwrap();
        let path = folder.path().join("db.sqlite");
        plaintext_db(&path);
        let before = std::fs::read(&path).unwrap();
        let key = SecretKey::random();

        let err = migrate_to_sqlcipher(&path, &key, |_| Err("no seal today".into())).unwrap_err();
        assert_eq!(err.stage, "seal");
        assert!(
            err.to_string().contains("plaintext database is kept"),
            "{err}"
        );
        assert_eq!(std::fs::read(&path).unwrap(), before);
        assert!(!migration_file(&path).exists());

        // A seal that loses a row fails the check.
        let err = migrate_to_sqlcipher(&path, &key, |file| {
            let conn = open_keyed(file, &key).map_err(|e| e.to_string())?;
            conn.execute("DELETE FROM b WHERE k = 'y'", [])
                .map(|_| ())
                .map_err(|e| e.to_string())
        })
        .unwrap_err();
        assert_eq!(err.stage, "check");
        assert!(err.cause.contains("table b"), "{}", err.cause);
        assert_eq!(std::fs::read(&path).unwrap(), before);
        assert!(!migration_file(&path).exists());
        assert_eq!(database_file(&path).unwrap(), DatabaseFile::Plaintext);

        // A leftover new file of a crash does not stop the next migration.
        std::fs::write(migration_file(&path), b"half written").unwrap();
        migrate_to_sqlcipher(&path, &key, |_| Ok(())).unwrap();
        assert_eq!(database_file(&path).unwrap(), DatabaseFile::Encrypted);
    }
}

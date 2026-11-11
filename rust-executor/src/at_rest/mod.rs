//! Data at rest: the operator secret, the keyring it opens, and the gate that keeps every store
//! closed until it is open.
//!
//! `run` installs the [`StorageGate`] before it opens any store. The gate starts
//! [`StorageState::Sealed`]: the RPC dispatcher answers `storage-sealed` to every method but
//! [`ANSWERED_WHILE_GATED`], and a socket whose token needs the wallet is closed with 1013. With
//! the secret (from the embedding shell, the secret file, or `storage.unlock` on a loopback
//! listener) the keyring opens, the gate goes to [`StorageState::Unlocking`] while `run` opens
//! the stores, then to [`StorageState::Open`]. Each change is published as
//! `storage-state-changed` to the operator.
//!
//! Without an installed gate (unit tests, an embedder that never called `run`) nothing is
//! gated.

pub mod data_keys;
pub mod database;
pub mod files;
pub mod keyring_file;
pub mod secret_source;
pub mod sqlite;

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use serde::{Deserialize, Serialize};
use tokio::sync::watch;
use ts_rs::TS;

pub use data_keys::{DataKeys, KeyringError, KeyringSummary, StoreStateName, UnlockOutcome};
use keyring_file::StoreId;
pub use secret_source::{SecretSourceKind, StorageSecret};

/// The methods a gated executor still answers. `/health` is an HTTP route and always answers.
pub const ANSWERED_WHILE_GATED: [&str; 2] = ["runtime.storageStatus", "storage.unlock"];

/// The error message prefix of a method refused while gated.
pub const SEALED: &str = "storage-sealed";

/// Where the executor is on the way to its stores.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum StorageState {
    /// Waiting for the storage secret: no store is open.
    Sealed,
    /// The keyring is open; the executor is opening its stores.
    Unlocking,
    /// Every store is open.
    Open,
}

/// Why the last unlock failed, or which store did not move under its key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum StorageErrorKind {
    WrongSecret,
    CorruptKeyring,
    SecretOnDataDisk,
    /// The plaintext database did not move under its key: it stays as it was, in the clear.
    MigrationFailed,
}

/// Whether each kind of store is written under its key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StoreStates {
    /// The executor's database: the executor store's state, `encrypted` once the database file
    /// is under the database key.
    pub database: StoreStateName,
    /// The languages' storage: `plain` until it is written under a key.
    pub language_storage: StoreStateName,
    /// The perspectives' links: the least advanced of the perspectives' stores, `plain` while
    /// there is none.
    pub links: StoreStateName,
}

/// `runtime.storageStatus` and the payload of `storage-state-changed`. The operator's view
/// carries the fields after `keyring_id`; every other caller gets them `null`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageStatus {
    pub state: StorageState,
    /// Every store is under its key and the secret is off the data disk.
    pub encrypted_at_rest: bool,
    /// The stores' states, once the keyring is open; `null` while sealed.
    pub stores: Option<StoreStates>,
    /// The keyring's id, hex: the same before and after the unlock.
    pub keyring_id: String,
    /// Operator only: where this start's secret came from; `null` while sealed.
    pub secret_source: Option<SecretSourceKind>,
    /// Operator only: the secret file is on the data folder's disk (allowed by
    /// `allow_secret_on_data_disk`).
    pub secret_on_data_disk: Option<bool>,
    /// Operator only: the root key's generation, once open.
    pub root_generation: Option<u32>,
    /// Operator only: why the last unlock failed, or which store did not move under its key.
    pub last_error: Option<StorageErrorKind>,
}

/// What a refused unlock tells the caller.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum UnlockError {
    #[error("wrong-secret: the secret does not open this executor's keyring")]
    WrongSecret,
    #[error("corrupt-keyring: {0}")]
    CorruptKeyring(String),
    #[error("{0}")]
    SecretOnDataDisk(String),
    #[error("cannot write the keyring: {0}")]
    Io(String),
}

impl UnlockError {
    fn kind(&self) -> Option<StorageErrorKind> {
        match self {
            UnlockError::WrongSecret => Some(StorageErrorKind::WrongSecret),
            UnlockError::CorruptKeyring(_) => Some(StorageErrorKind::CorruptKeyring),
            UnlockError::SecretOnDataDisk(_) => Some(StorageErrorKind::SecretOnDataDisk),
            UnlockError::Io(_) => None,
        }
    }
}

#[derive(Default)]
struct GateInfo {
    source: Option<SecretSourceKind>,
    on_data_disk: bool,
    last_error: Option<StorageErrorKind>,
}

/// The storage gate of one executor process.
pub struct StorageGate {
    keys: Arc<DataKeys>,
    data_dir: PathBuf,
    allow_secret_on_data_disk: bool,
    state: watch::Sender<StorageState>,
    info: Mutex<GateInfo>,
    /// A keyring file that did not read at start: every unlock fails with it.
    load_error: Option<String>,
}

impl StorageGate {
    /// The gate of the data folder `data_dir`, sealed. A keyring file that does not read
    /// leaves the gate sealed with `corrupt-keyring` as its last error.
    pub fn new(data_dir: &Path, allow_secret_on_data_disk: bool) -> Self {
        let path = keyring_file::path_in(data_dir);
        let (keys, load_error) = match DataKeys::load(path.clone()) {
            Ok(keys) => (keys, None),
            Err(e) => {
                log::error!("{e}");
                // Every unlock answers the load error, so the damaged file stays as it is.
                (DataKeys::unreadable(path), Some(e.to_string()))
            }
        };
        let info = GateInfo {
            last_error: load_error
                .as_ref()
                .map(|_| StorageErrorKind::CorruptKeyring),
            ..GateInfo::default()
        };
        StorageGate {
            keys: Arc::new(keys),
            data_dir: data_dir.to_path_buf(),
            allow_secret_on_data_disk,
            state: watch::Sender::new(StorageState::Sealed),
            info: Mutex::new(info),
            load_error,
        }
    }

    fn info(&self) -> std::sync::MutexGuard<'_, GateInfo> {
        self.info.lock().unwrap_or_else(|p| p.into_inner())
    }

    pub fn keys(&self) -> &Arc<DataKeys> {
        &self.keys
    }

    pub fn state(&self) -> StorageState {
        *self.state.borrow()
    }

    /// Whether `method` is refused now: the gate is not open and the method is not one of
    /// [`ANSWERED_WHILE_GATED`].
    pub fn refuses(&self, method: &str) -> bool {
        self.state() != StorageState::Open && !ANSWERED_WHILE_GATED.contains(&method)
    }

    /// Opens the keyring with `secret`. A secret file on the data folder's disk is refused
    /// unless allowed. On success a sealed gate goes to [`StorageState::Unlocking`]; an
    /// unlocking or open one only checks the secret. A failure keeps the state and becomes the
    /// status's `lastError`. Either way the new status is published.
    pub fn unlock(&self, secret: &StorageSecret) -> Result<UnlockOutcome, UnlockError> {
        let result = self.try_unlock(secret);
        {
            let mut info = self.info();
            match &result {
                Ok(_) => {
                    if self.state() == StorageState::Sealed {
                        info.source = Some(secret.source);
                    }
                    info.last_error = None;
                }
                Err(e) => info.last_error = e.kind().or(info.last_error),
            }
        }
        if result.is_ok() {
            self.state.send_if_modified(|state| {
                let sealed = *state == StorageState::Sealed;
                if sealed {
                    *state = StorageState::Unlocking;
                }
                sealed
            });
        }
        self.publish();
        result
    }

    fn try_unlock(&self, secret: &StorageSecret) -> Result<UnlockOutcome, UnlockError> {
        if let Some(e) = &self.load_error {
            return Err(UnlockError::CorruptKeyring(e.clone()));
        }
        let on_disk =
            secret_source::check_placement(secret, &self.data_dir, self.allow_secret_on_data_disk)
                .map_err(|e| UnlockError::SecretOnDataDisk(e.to_string()))?;
        let outcome = self
            .keys
            .unlock(secret.to_zeroizing())
            .map_err(|e| match e {
                KeyringError::WrongSecret => UnlockError::WrongSecret,
                KeyringError::CorruptKeyring(why) => UnlockError::CorruptKeyring(why),
                other => UnlockError::Io(other.to_string()),
            })?;
        if self.state() == StorageState::Sealed {
            self.info().on_data_disk = on_disk;
        }
        Ok(outcome)
    }

    /// Every store is open: the gate opens and says so.
    pub fn mark_open(&self) {
        self.state.send_replace(StorageState::Open);
        self.publish();
    }

    /// A store did not move under its key (`MigrationFailed`): it becomes the status's
    /// `lastError`, and the new status is published.
    pub fn report_error(&self, kind: StorageErrorKind) {
        self.info().last_error = Some(kind);
        self.publish();
    }

    /// Waits until the gate is no longer sealed.
    pub async fn unsealed(&self) {
        let mut rx = self.state.subscribe();
        let _ = rx.wait_for(|s| *s != StorageState::Sealed).await;
    }

    /// The status; `operator` adds the operator's fields.
    pub fn status(&self, operator: bool) -> StorageStatus {
        let state = self.state();
        let stores = self
            .keys
            .store_states()
            .ok()
            .map(|states| store_states(&states));
        let info = self.info();
        let encrypted_at_rest = !info.on_data_disk
            && stores.is_some_and(|s| {
                [s.database, s.language_storage, s.links]
                    .iter()
                    .all(|s| *s == StoreStateName::Encrypted)
            });
        StorageStatus {
            state,
            encrypted_at_rest,
            stores,
            keyring_id: self.keys.keyring_id().to_hex(),
            secret_source: operator.then_some(info.source).flatten(),
            secret_on_data_disk: (operator && self.keys.is_open()).then_some(info.on_data_disk),
            root_generation: operator.then(|| self.keys.root_generation()).flatten(),
            last_error: operator.then_some(info.last_error).flatten(),
        }
    }

    /// Publishes the operator's status as `storage-state-changed`.
    pub fn publish(&self) {
        crate::pubsub::get_global_pubsub_sync().publish_event(crate::events::Event::new(
            crate::events::Topic::StorageStateChanged,
            crate::events::Audience::Operator,
            self.status(true),
        ));
    }
}

/// The three kinds of store from the keyring's stores.
fn store_states(states: &[(StoreId, keyring_file::StoreState)]) -> StoreStates {
    let database = states
        .iter()
        .find(|(id, _)| *id == StoreId::Executor)
        .map_or(StoreStateName::Plain, |(_, s)| s.into());
    let rank = |s: StoreStateName| match s {
        StoreStateName::Plain => 0,
        StoreStateName::Migrating => 1,
        StoreStateName::Encrypted => 2,
    };
    let links = states
        .iter()
        .filter(|(id, _)| matches!(id, StoreId::Perspective(_)))
        .map(|(_, s)| StoreStateName::from(s))
        .min_by_key(|s| rank(*s))
        .unwrap_or(StoreStateName::Plain);
    StoreStates {
        database,
        // No step writes the languages' storage under a key yet.
        language_storage: StoreStateName::Plain,
        links,
    }
}

static GATE: OnceLock<Arc<StorageGate>> = OnceLock::new();

/// Installs the process's gate. `run` calls it once, before it opens any store; a second
/// call (a second `run` in one process) is refused.
pub fn install(gate: Arc<StorageGate>) -> Result<(), Arc<StorageGate>> {
    GATE.set(gate)
}

/// The process's gate, once `run` installed it.
pub fn gate() -> Option<&'static Arc<StorageGate>> {
    GATE.get()
}

/// Whether the executor is gated: a gate is installed and not open.
pub fn is_gated() -> bool {
    gate().is_some_and(|g| g.state() != StorageState::Open)
}

/// The refusal a gated executor answers `method` with, if any. `gate`: the process's gate, or
/// one a test made.
pub fn refusal(gate: Option<&StorageGate>, method: &str) -> Option<String> {
    let gate = gate?;
    gate.refuses(method).then(|| {
        format!(
            "{SEALED}: the executor is {} and answers only {} until its storage is open",
            match gate.state() {
                StorageState::Sealed => "sealed",
                StorageState::Unlocking => "opening its stores",
                StorageState::Open => "open",
            },
            ANSWERED_WHILE_GATED.join(" and ")
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn secret(byte: u8) -> StorageSecret {
        StorageSecret::in_process([byte; 32])
    }

    #[test]
    fn a_sealed_gate_answers_only_the_status_and_the_unlock() {
        let folder = tempfile::tempdir().unwrap();
        let gate = StorageGate::new(folder.path(), false);
        assert_eq!(gate.state(), StorageState::Sealed);
        for method in ANSWERED_WHILE_GATED {
            assert_eq!(refusal(Some(&gate), method), None);
        }
        for method in [
            "agent.status",
            "perspective.all",
            "storage.keys",
            "runtime.info",
        ] {
            let refused = refusal(Some(&gate), method).expect(method);
            assert!(refused.starts_with("storage-sealed"), "{refused}");
        }
        assert_eq!(refusal(None, "agent.status"), None, "no gate, no refusal");
        gate.unlock(&secret(1)).unwrap();
        assert_eq!(gate.state(), StorageState::Unlocking);
        assert!(refusal(Some(&gate), "agent.status").is_some());
        gate.mark_open();
        assert_eq!(refusal(Some(&gate), "agent.status"), None);
    }

    #[test]
    fn nothing_is_written_to_the_data_folder_while_sealed() {
        let folder = tempfile::tempdir().unwrap();
        let gate = StorageGate::new(folder.path(), false);
        let _ = gate.status(true);
        assert!(std::fs::read_dir(folder.path()).unwrap().next().is_none());
    }

    #[test]
    fn the_status_shows_the_keyring_id_while_sealed_and_the_stores_once_open() {
        let folder = tempfile::tempdir().unwrap();
        let gate = StorageGate::new(folder.path(), false);
        let sealed = gate.status(true);
        assert_eq!(sealed.state, StorageState::Sealed);
        assert_eq!(sealed.keyring_id.len(), 32);
        assert_eq!(sealed.stores, None);
        assert_eq!(sealed.secret_source, None);
        assert_eq!(sealed.secret_on_data_disk, None);
        assert_eq!(sealed.root_generation, None);
        assert!(!sealed.encrypted_at_rest);

        gate.unlock(&secret(2)).unwrap();
        gate.mark_open();
        let open = gate.status(true);
        assert_eq!(open.state, StorageState::Open);
        assert_eq!(open.keyring_id, sealed.keyring_id, "the id does not change");
        assert_eq!(
            open.stores,
            Some(StoreStates {
                database: StoreStateName::Plain,
                language_storage: StoreStateName::Plain,
                links: StoreStateName::Plain,
            })
        );
        assert_eq!(open.secret_source, Some(SecretSourceKind::InProcess));
        assert_eq!(open.secret_on_data_disk, Some(false));
        assert_eq!(open.root_generation, Some(1));
        assert!(!open.encrypted_at_rest, "no store is under its key yet");

        let app = gate.status(false);
        assert_eq!(app.keyring_id, open.keyring_id);
        assert_eq!(app.stores, open.stores);
        assert_eq!(
            (
                app.secret_source,
                app.secret_on_data_disk,
                app.root_generation,
                app.last_error
            ),
            (None, None, None, None),
            "the operator's fields are the operator's"
        );
        let json = serde_json::to_value(&app).unwrap();
        assert!(json.get("keyringId").is_some() && json.get("encryptedAtRest").is_some());
    }

    #[test]
    fn a_wrong_secret_keeps_the_gate_sealed_and_says_why() {
        let folder = tempfile::tempdir().unwrap();
        StorageGate::new(folder.path(), false)
            .unlock(&secret(3))
            .unwrap();
        let gate = StorageGate::new(folder.path(), false);
        assert_eq!(gate.unlock(&secret(4)), Err(UnlockError::WrongSecret));
        assert_eq!(gate.state(), StorageState::Sealed);
        assert_eq!(
            gate.status(true).last_error,
            Some(StorageErrorKind::WrongSecret)
        );
        gate.unlock(&secret(3)).unwrap();
        assert_eq!(gate.status(true).last_error, None);
        assert_eq!(gate.unlock(&secret(4)), Err(UnlockError::WrongSecret));
        assert_eq!(gate.state(), StorageState::Unlocking, "still unlocked");
    }

    #[test]
    fn a_damaged_keyring_keeps_the_gate_sealed() {
        let folder = tempfile::tempdir().unwrap();
        files::create_private_dir_all(&folder.path().join("ad4m")).unwrap();
        std::fs::write(keyring_file::path_in(folder.path()), b"not json").unwrap();
        let gate = StorageGate::new(folder.path(), false);
        assert_eq!(
            gate.status(true).last_error,
            Some(StorageErrorKind::CorruptKeyring)
        );
        assert!(matches!(
            gate.unlock(&secret(1)),
            Err(UnlockError::CorruptKeyring(_))
        ));
        assert_eq!(gate.state(), StorageState::Sealed);
        assert_eq!(
            std::fs::read(keyring_file::path_in(folder.path())).unwrap(),
            b"not json"
        );
    }

    #[cfg(unix)]
    #[test]
    fn a_secret_file_on_the_data_disk_is_refused_by_the_gate() {
        use std::os::unix::fs::PermissionsExt;
        let folder = tempfile::tempdir().unwrap();
        let file = folder.path().join("secret");
        std::fs::write(&file, "ab".repeat(32)).unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o600)).unwrap();
        let secret = StorageSecret::read_file(&file).unwrap();

        let gate = StorageGate::new(folder.path(), false);
        let refused = gate.unlock(&secret).unwrap_err();
        assert!(refused.to_string().starts_with("secret-on-data-disk"));
        assert_eq!(
            gate.status(true).last_error,
            Some(StorageErrorKind::SecretOnDataDisk)
        );
        assert!(!keyring_file::path_in(folder.path()).exists());

        let allowed = StorageGate::new(folder.path(), true);
        allowed.unlock(&secret).unwrap();
        let status = allowed.status(true);
        assert_eq!(status.secret_on_data_disk, Some(true));
        assert_eq!(status.secret_source, Some(SecretSourceKind::File));
        assert!(!status.encrypted_at_rest);
    }

    #[tokio::test]
    async fn unsealed_waits_for_the_unlock() {
        let folder = tempfile::tempdir().unwrap();
        let gate = Arc::new(StorageGate::new(folder.path(), false));
        let waiting = tokio::spawn({
            let gate = gate.clone();
            async move { gate.unsealed().await }
        });
        tokio::task::yield_now().await;
        assert!(!waiting.is_finished());
        gate.unlock(&secret(7)).unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(5), waiting)
            .await
            .expect("the unlock ends the wait")
            .unwrap();
    }
}

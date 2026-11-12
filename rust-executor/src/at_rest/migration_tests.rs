use std::cell::RefCell;

use super::*;
use crate::at_rest::keyring_file::{self, KeyOwner, StoreId, StoreState};
use crate::at_rest::{StorageErrorKind, StorageSecret, StorageState};
use crate::types::domain::Neighbourhood;
use crate::types::{
    DecoratedNeighbourhoodExpression, NotificationInput, PerspectiveHandle, PerspectiveState,
};

const USER: &str = "alice@example.test";

/// The gate of `data`, unlocked: a new process's, the keyring read again from disk.
fn unlocked_gate(data: &Path) -> StorageGate {
    let gate = StorageGate::new(data, false);
    gate.unlock(&StorageSecret::in_process([7u8; 32])).unwrap();
    gate
}

/// What a run sent: (step, stores done, stores total, store), and the gate's state and status
/// as each was sent.
type Sent = RefCell<Vec<(StorageMigrationProgress, StorageState, bool)>>;

fn recording<'a>(gate: &'a StorageGate, sent: &'a Sent, every: Duration) -> Progress<'a> {
    Progress::with(gate, every, move |progress| {
        let status = gate.status(true);
        sent.borrow_mut().push((
            progress.clone(),
            gate.state(),
            status.migration.as_ref() == Some(progress),
        ))
    })
}

fn quiet(gate: &StorageGate) -> Progress<'_> {
    Progress::with(gate, Duration::ZERO, |_| {})
}

/// An executor from before at-rest encryption: a plaintext database with a notification, a
/// perspective in a neighbourhood and a managed user; a link language and another language
/// with plaintext storage files beside their bundles.
struct Fixture {
    data: tempfile::TempDir,
    paths: Paths,
    notification: String,
    perspective: String,
    /// (address, file, content), sorted by address.
    files: Vec<(String, String, Vec<u8>)>,
}

impl Fixture {
    fn new() -> Self {
        let data = tempfile::tempdir().unwrap();
        let paths = Paths::in_data_dir(data.path());
        std::fs::create_dir_all(&paths.languages).unwrap();
        let tag = uuid::Uuid::new_v4().simple().to_string();
        let link_language = format!("QmALink{tag}");
        let other_language = format!("QmBOther{tag}");

        let db = Ad4mDb::new(paths.database.to_str().unwrap()).unwrap();
        let notification = db
            .add_notification(
                NotificationInput {
                    description: "a private description".into(),
                    app_name: "app".into(),
                    app_url: "https://app.test".into(),
                    app_icon_path: String::new(),
                    trigger: "SELECT 1".into(),
                    perspective_ids: vec![],
                    webhook_url: "https://webhook.test".into(),
                    webhook_auth: "auth".into(),
                },
                "did:key:someone",
                true,
            )
            .unwrap();
        let perspective = uuid::Uuid::new_v4().to_string();
        db.add_perspective(&PerspectiveHandle {
            uuid: perspective.clone(),
            name: Some("a private name".into()),
            neighbourhood: Some(DecoratedNeighbourhoodExpression {
                data: Neighbourhood {
                    link_language: link_language.clone(),
                    ..Default::default()
                },
                ..Default::default()
            }),
            shared_url: Some("neighbourhood://private".into()),
            state: PerspectiveState::Synced,
            owners: Some(vec!["did:key:someone".into()]),
        })
        .unwrap();
        db.add_user(USER, "did:key:alice", "a password").unwrap();
        drop(db);

        let files = vec![
            (
                link_language.clone(),
                "links.json".to_string(),
                b"[\"a private link\"]".to_vec(),
            ),
            (
                link_language.clone(),
                "sub/state.bin".to_string(),
                vec![0u8, 1, 2, 255],
            ),
            (
                other_language.clone(),
                "store.json".to_string(),
                b"{\"private\":true}".to_vec(),
            ),
        ];
        for (address, file, content) in &files {
            let path = paths.languages.join(address).join(file);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, content).unwrap();
        }
        for address in [&link_language, &other_language] {
            std::fs::write(paths.languages.join(address).join("bundle.js"), b"bundle").unwrap();
        }
        Fixture {
            data,
            paths,
            notification,
            perspective,
            files,
        }
    }

    fn languages(&self) -> Vec<String> {
        let mut addresses: Vec<String> = self.files.iter().map(|f| f.0.clone()).collect();
        addresses.dedup();
        addresses
    }

    /// Every store is under its key and every value reads back: the database (keyed, no
    /// copy left beside it), the managed user's person, every language file (sealed once,
    /// under its store's key) and the bundles in the clear.
    fn assert_fully_sealed(&self, keys: &Arc<DataKeys>) {
        assert!(matches!(
            sqlite::database_file(&self.paths.database).unwrap(),
            DatabaseFile::Encrypted
        ));
        assert!(!sqlite::migration_file(&self.paths.database).exists());
        let key = sqlite::database_key(keys).unwrap();
        let db = Ad4mDb::open(&self.paths.database, Some((&key, keys.clone()))).unwrap();
        let notification = db
            .get_notification(self.notification.clone())
            .unwrap()
            .expect("the notification");
        assert_eq!(notification.description, "a private description");
        let handle = db
            .get_all_perspectives()
            .unwrap()
            .into_iter()
            .find(|h| h.uuid == self.perspective)
            .expect("the perspective");
        assert_eq!(handle.name.as_deref(), Some("a private name"));
        assert!(db.list_users().unwrap().iter().any(|u| u.username == USER));
        drop(db);

        let owner = KeyOwner::User(USER.into());
        assert!(keys.has_person(&owner).unwrap());
        assert!(keys.has_store(&StoreId::Person(owner)).unwrap());
        assert_eq!(
            keys.store_state(&StoreId::Executor).unwrap(),
            Some(StoreState::Encrypted)
        );
        assert_eq!(
            keys.language_storage_state().unwrap(),
            StoreState::Encrypted
        );

        let perspective = StoreId::Perspective(uuid::Uuid::parse_str(&self.perspective).unwrap());
        let link_language = &self.files[0].0;
        for (address, file, content) in &self.files {
            let path = self.paths.languages.join(address).join(file);
            let bytes = std::fs::read(&path).unwrap();
            let (store, _) = lang_storage::sealed_header(&bytes).expect("sealed");
            let expected = if address == link_language {
                perspective.clone()
            } else {
                StoreId::Executor
            };
            assert_eq!(store, expected, "{address}/{file}");
            let (_, opened) =
                lang_storage::open(keys, address, file, &bytes, &path.display().to_string())
                    .unwrap();
            assert_eq!(
                &opened, content,
                "{address}/{file} sealed once, to its content"
            );
        }
        for address in self.languages() {
            assert_eq!(
                std::fs::read(self.paths.languages.join(address).join("bundle.js")).unwrap(),
                b"bundle"
            );
        }
        assert!(lang_storage::plaintext_folders(&self.paths.languages)
            .unwrap()
            .is_empty());
        let record = keys.migration().unwrap().expect("the record");
        assert!(record.finished(), "{record:?}");
        assert_eq!(record.done, MigrationStep::ALL.to_vec());
    }
}

/// A plaintext executor ends fully sealed with every value read back; the gate is migrating
/// meanwhile, with the progress in its status, then back to unlocking; the progress arrives in
/// order, one per store, the last with every store done.
#[test]
fn a_plaintext_executor_ends_fully_sealed_and_the_progress_arrives_in_order() {
    let fixture = Fixture::new();
    let gate = unlocked_gate(fixture.data.path());
    assert_eq!(
        plan(gate.keys(), &fixture.paths).unwrap(),
        Plan {
            pending: MigrationStep::ALL.to_vec(),
            language_folders: fixture.languages(),
        }
    );
    let sent = Sent::default();
    let outcome = run_with(
        &gate,
        &fixture.paths,
        &mut recording(&gate, &sent, Duration::ZERO),
        &Hooks::default(),
    )
    .unwrap();
    assert_eq!(
        outcome,
        Outcome::Migrated {
            steps: MigrationStep::ALL.to_vec()
        }
    );
    fixture.assert_fully_sealed(gate.keys());
    assert_eq!(gate.state(), StorageState::Unlocking);
    let status = gate.status(true);
    assert_eq!(status.migration, None);
    assert_eq!(status.last_error, None);

    let languages = fixture.languages();
    let expected: Vec<(MigrationStep, u32, Option<String>)> = vec![
        (MigrationStep::Database, 0, Some("executor".into())),
        (MigrationStep::Persons, 1, Some("persons".into())),
        (
            MigrationStep::LanguageStorage,
            2,
            Some(format!("language:{}", languages[0])),
        ),
        (
            MigrationStep::LanguageStorage,
            3,
            Some(format!("language:{}", languages[1])),
        ),
        (MigrationStep::LanguageStorage, 4, None),
    ];
    let sent = sent.into_inner();
    assert_eq!(
        sent.iter()
            .map(|(p, _, _)| (p.step, p.stores_done, p.store.clone()))
            .collect::<Vec<_>>(),
        expected
    );
    for (progress, state, in_status) in &sent {
        assert_eq!(progress.stores_total, 4);
        assert_eq!(*state, StorageState::Migrating, "{progress:?}");
        assert!(in_status, "the status carries {progress:?}");
    }
}

/// A second run, in a new process, finds nothing to do: it writes nothing, sends nothing and
/// leaves the gate as it was.
#[test]
fn a_second_run_is_a_no_op() {
    let fixture = Fixture::new();
    let gate = unlocked_gate(fixture.data.path());
    run_with(&gate, &fixture.paths, &mut quiet(&gate), &Hooks::default()).unwrap();
    drop(gate);

    let keyring = keyring_file::path_in(fixture.data.path());
    let before = std::fs::read(&keyring).unwrap();
    let database = std::fs::read(&fixture.paths.database).unwrap();
    let gate = unlocked_gate(fixture.data.path());
    let sent = Sent::default();
    let outcome = run_with(
        &gate,
        &fixture.paths,
        &mut recording(&gate, &sent, Duration::ZERO),
        &Hooks::default(),
    )
    .unwrap();
    assert_eq!(outcome, Outcome::NothingToDo);
    assert!(sent.borrow().is_empty());
    assert_eq!(gate.state(), StorageState::Unlocking);
    assert_eq!(std::fs::read(&keyring).unwrap(), before);
    assert_eq!(std::fs::read(&fixture.paths.database).unwrap(), database);
    fixture.assert_fully_sealed(gate.keys());
}

/// An executor with no plaintext data and no record (a new one) runs nothing and writes no
/// record.
#[test]
fn a_new_executor_has_nothing_to_migrate() {
    let data = tempfile::tempdir().unwrap();
    let paths = Paths::in_data_dir(data.path());
    let gate = unlocked_gate(data.path());
    assert_eq!(run(&gate, &paths).unwrap(), Outcome::NothingToDo);
    assert_eq!(gate.keys().migration().unwrap(), None);
    assert_eq!(gate.state(), StorageState::Unlocking);
}

/// A kill once each step's work is done, before the record names it: the gate goes back to
/// sealed with `migration-failed`, the record names the steps before; the next process runs the
/// rest and ends fully sealed.
#[test]
fn a_kill_at_each_step_resumes_and_finishes() {
    for (n, killed) in MigrationStep::ALL.into_iter().enumerate() {
        let fixture = Fixture::new();
        let gate = unlocked_gate(fixture.data.path());
        let failure = run_with(
            &gate,
            &fixture.paths,
            &mut quiet(&gate),
            &Hooks {
                after_step: &|step| {
                    if step == killed {
                        Err("killed".into())
                    } else {
                        Ok(())
                    }
                },
                ..Hooks::default()
            },
        )
        .unwrap_err();
        assert_eq!(failure.step, Some(killed));
        assert_eq!(gate.state(), StorageState::Sealed, "{killed:?}");
        let status = gate.status(true);
        assert_eq!(status.last_error, Some(StorageErrorKind::MigrationFailed));
        assert_eq!(status.migration, None);
        let record = gate.keys().migration().unwrap().unwrap();
        assert!(!record.finished());
        assert_eq!(record.done, MigrationStep::ALL[..n].to_vec(), "{killed:?}");
        drop(gate);

        let gate = unlocked_gate(fixture.data.path());
        assert_eq!(
            plan(gate.keys(), &fixture.paths).unwrap().pending,
            MigrationStep::ALL[n..].to_vec(),
            "{killed:?}"
        );
        let sent = Sent::default();
        let outcome = run_with(
            &gate,
            &fixture.paths,
            &mut recording(&gate, &sent, Duration::ZERO),
            &Hooks::default(),
        )
        .unwrap();
        assert_eq!(
            outcome,
            Outcome::Migrated {
                steps: MigrationStep::ALL[n..].to_vec()
            }
        );
        fixture.assert_fully_sealed(gate.keys());
        let sent = sent.into_inner();
        // The steps done before the kill count as done from the start.
        let first = &sent.first().unwrap().0;
        assert_eq!(first.step, killed);
        assert_eq!(first.stores_done, n as u32);
        let last = &sent.last().unwrap().0;
        assert_eq!(last.stores_done, last.stores_total);
    }
}

/// A kill in the middle of the database's move, before the keyed copy replaced the plaintext:
/// the half-made copy is dropped and the move starts again from the plaintext.
#[test]
fn a_kill_inside_the_database_move_resumes_from_the_plaintext() {
    let fixture = Fixture::new();
    let gate = unlocked_gate(fixture.data.path());
    gate.keys()
        .set_migration(MigrationRecord {
            started_at: Utc::now(),
            done: vec![],
            finished_at: None,
        })
        .unwrap();
    gate.keys()
        .set_store_state(
            &StoreId::Executor,
            StoreState::Migrating {
                started_at: Utc::now(),
            },
        )
        .unwrap();
    std::fs::write(
        sqlite::migration_file(&fixture.paths.database),
        b"half of a keyed copy",
    )
    .unwrap();
    drop(gate);

    let gate = unlocked_gate(fixture.data.path());
    run_with(&gate, &fixture.paths, &mut quiet(&gate), &Hooks::default()).unwrap();
    fixture.assert_fully_sealed(gate.keys());
}

/// A kill in the middle of the languages' storage: the files sealed stay sealed (never sealed
/// twice), the one that failed keeps its plaintext, and the next run seals the rest.
#[test]
fn a_kill_inside_the_language_storage_resumes_without_sealing_twice() {
    let fixture = Fixture::new();
    let gate = unlocked_gate(fixture.data.path());
    let renames = std::sync::atomic::AtomicUsize::new(0);
    let failure = run_with(
        &gate,
        &fixture.paths,
        &mut quiet(&gate),
        &Hooks {
            rename: &|from, to| {
                if renames.fetch_add(1, std::sync::atomic::Ordering::SeqCst) == 1 {
                    Err(io::Error::other("killed"))
                } else {
                    std::fs::rename(from, to)
                }
            },
            ..Hooks::default()
        },
    )
    .unwrap_err();
    assert_eq!(failure.step, Some(MigrationStep::LanguageStorage));
    assert_eq!(gate.state(), StorageState::Sealed);
    let (address, file, content) = &fixture.files[1];
    assert_eq!(
        &std::fs::read(fixture.paths.languages.join(address).join(file)).unwrap(),
        content,
        "the file that failed keeps its plaintext"
    );
    let first = &fixture.files[0];
    assert!(lang_storage::sealed_header(
        &std::fs::read(fixture.paths.languages.join(&first.0).join(&first.1)).unwrap()
    )
    .is_some());
    drop(gate);

    let gate = unlocked_gate(fixture.data.path());
    assert_eq!(
        plan(gate.keys(), &fixture.paths).unwrap().pending,
        vec![MigrationStep::LanguageStorage]
    );
    run_with(&gate, &fixture.paths, &mut quiet(&gate), &Hooks::default()).unwrap();
    fixture.assert_fully_sealed(gate.keys());
}

/// The progress goes out at most once an interval, the first and the last always.
#[test]
fn the_progress_is_spaced_out_but_the_first_and_last_always_go() {
    let fixture = Fixture::new();
    let gate = unlocked_gate(fixture.data.path());
    let sent = Sent::default();
    run_with(
        &gate,
        &fixture.paths,
        &mut recording(&gate, &sent, Duration::from_secs(3600)),
        &Hooks::default(),
    )
    .unwrap();
    let sent = sent.into_inner();
    assert_eq!(
        sent.iter()
            .map(|(p, _, _)| (p.stores_done, p.store.is_some()))
            .collect::<Vec<_>>(),
        vec![(0, true), (4, false)]
    );
}

/// After a failed run the gate is sealed: only the status and the unlock are answered, and
/// the unlock moves it on so the next run can start.
#[test]
fn a_failed_run_seals_the_gate_until_the_next_unlock() {
    let fixture = Fixture::new();
    let gate = unlocked_gate(fixture.data.path());
    run_with(
        &gate,
        &fixture.paths,
        &mut quiet(&gate),
        &Hooks {
            after_step: &|_| Err("killed".into()),
            ..Hooks::default()
        },
    )
    .unwrap_err();
    assert_eq!(gate.state(), StorageState::Sealed);
    assert!(gate.refuses("agent.status"));
    assert!(!gate.refuses("storage.unlock"));
    gate.unlock(&StorageSecret::in_process([7u8; 32])).unwrap();
    assert_eq!(gate.state(), StorageState::Unlocking);
    run_with(&gate, &fixture.paths, &mut quiet(&gate), &Hooks::default()).unwrap();
    fixture.assert_fully_sealed(gate.keys());
}

//! Persons through their life: a managed user gets a person (a data key and its own person
//! store) when its `users` row is made, keeps it while it is away with its data kept, and
//! loses it, with every store it alone owns, when its data is deleted.
//!
//! **Made.** [`create_for_user`] runs when `user.create` makes a user (and when a user from
//! the shared database is adopted here), before the user's first write. [`prepare`] runs at
//! start, once the database is open: a user made before persons existed gets its person then.
//!
//! **Kept.** [`leave`] with `keep_data` changes nothing: the person's data key stays wrapped
//! under the root key, so the executor still opens its perspectives. Nothing removes kept
//! data later: no job of the executor reads how long a person has been away.
//!
//! **Deleted** ([`leave`] without `keep_data`), in this order, so a key goes only after the
//! data it protects:
//!
//! 1. Each perspective whose owners name the user: with other owners, the user leaves its
//!    owners (its wrap of the perspective's key goes, the data stays for the others); with
//!    none, the perspective is removed: its rows, its SPARQL store folder, then its key; and
//!    the storage files of its link language when no other perspective uses that language.
//! 2. Each store the keyring still holds a wrap of for the person: the wrap goes, and a store
//!    left with no owner is removed the same way.
//! 3. The rows of the person's own store (notifications, tasks, expressions, outbox).
//! 4. The person's data key and its person store, leaving tombstones with no personal data.
//!
//! When step 4 has written the keyring file, no copy of the person's keys is left: the
//! executor keeps no backup of the keyring. So `shredded_everywhere_at` equals
//! `key_destroyed_at`.

use std::path::Path;
use std::sync::Arc;

use ad4m_keyring::KeyVersion;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::data_keys::{DataKeys, DestroyReason, KeyringError};
use super::keyring_file::{KeyOwner, StoreId};

/// The person of the managed user `username`.
pub fn owner_of(username: &str) -> KeyOwner {
    KeyOwner::User(username.to_string())
}

/// What a person's leave did.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PersonLeave {
    /// The data was kept.
    pub kept: bool,
    /// Perspectives the person owned alone, removed with their files and keys.
    pub removed_perspectives: u32,
    /// Perspectives with other owners that the person left.
    pub left_shared_perspectives: u32,
    /// Delete only: when the person's data key was destroyed.
    pub key_destroyed_at: Option<DateTime<Utc>>,
    /// Delete only: when no copy of the person's keys is left anywhere the executor made one.
    /// The executor keeps no backup of its keyring, so this equals `key_destroyed_at`.
    pub shredded_everywhere_at: Option<DateTime<Utc>>,
}

/// The keys of this executor, when its storage gate is installed and the keyring is open.
fn open_keys() -> Option<Arc<DataKeys>> {
    super::gate()
        .map(|gate| gate.keys().clone())
        .filter(|keys| keys.is_open())
}

/// Makes the person of the managed user `username` with `keys`: its data key and its person
/// store. A user that is a person already keeps its keys.
pub fn create_person(keys: &DataKeys, username: &str) -> Result<KeyVersion, KeyringError> {
    keys.create_person(owner_of(username))
}

/// Makes the person of the managed user `username` in this executor's keyring. `Ok(None)`
/// without a storage gate (unit tests, an embedder that never called `run`). A sealed
/// keyring is an error: no user may be made before the keyring is open.
pub fn create_for_user(username: &str) -> Result<Option<KeyVersion>, String> {
    let Some(gate) = super::gate() else {
        return Ok(None);
    };
    create_person(gate.keys(), username)
        .map(Some)
        .map_err(|e| format!("Could not make the data key of user {username}: {e}"))
}

/// The start step: every managed user in `usernames` that is not a person yet becomes one.
/// Answers how many persons it made. `Err` stops the start: the keyring cannot be written.
pub fn prepare(keys: &DataKeys, usernames: &[String]) -> Result<usize, String> {
    let mut made = 0;
    for username in usernames {
        let owner = owner_of(username);
        let had = keys.has_person(&owner).map_err(|e| e.to_string())?
            && keys
                .has_store(&StoreId::Person(owner.clone()))
                .map_err(|e| e.to_string())?;
        if !had {
            keys.create_person(owner)
                .map_err(|e| format!("Could not make the data key of user {username}: {e}"))?;
            made += 1;
        }
    }
    Ok(made)
}

/// The leave of the managed user `username` on this executor: keep or delete its data (see
/// the module's text for the order). The caller has ended the user's sessions first, so no
/// write of the user runs meanwhile. Refused without an open keyring.
pub async fn leave(
    username: &str,
    keep_data: bool,
    reason: DestroyReason,
) -> Result<PersonLeave, String> {
    let keys = open_keys().ok_or_else(|| {
        format!(
            "{}: the keyring is not open, so no person's data can be deleted",
            super::SEALED
        )
    })?;
    leave_with(
        &keys,
        &crate::utils::languages_directory(),
        username,
        keep_data,
        reason,
    )
    .await
}

/// [`leave`] with the keyring and the languages folder given.
pub async fn leave_with(
    keys: &DataKeys,
    languages: &Path,
    username: &str,
    keep_data: bool,
    reason: DestroyReason,
) -> Result<PersonLeave, String> {
    let owner = owner_of(username);
    if !keys.has_person(&owner).map_err(|e| e.to_string())? {
        return Err(format!("User {username} has no data key on this executor"));
    }
    if keep_data {
        return Ok(PersonLeave {
            kept: true,
            removed_perspectives: 0,
            left_shared_perspectives: 0,
            key_destroyed_at: None,
            shredded_everywhere_at: None,
        });
    }
    let db = crate::db_backend::db_backend();
    let did = db
        .get_user(username)
        .ok()
        .map(|user| user.did)
        .filter(|did| !did.is_empty());

    let mut removed = 0u32;
    let mut left_shared = 0u32;
    // 1. The perspectives whose owners name the user.
    if let Some(did) = &did {
        let handles = db.get_all_perspectives().map_err(|e| e.to_string())?;
        for handle in handles {
            let owners = handle.owners.clone().unwrap_or_default();
            if !owners.contains(did) {
                continue;
            }
            let others: Vec<String> = owners.into_iter().filter(|o| o != did).collect();
            if others.is_empty() {
                remove_perspective(languages, &handle.uuid).await;
                removed += 1;
            } else {
                leave_shared(&handle, others).await?;
                left_shared += 1;
            }
        }
    }
    // 2. The stores the keyring still holds a wrap of for the person.
    for store in keys.stores_of(&owner).map_err(|e| e.to_string())? {
        let left = keys
            .remove_owner(&store, &owner)
            .map_err(|e| format!("Could not take user {username} out of {store}: {e}"))?;
        if !left.is_empty() {
            continue;
        }
        match &store {
            StoreId::Perspective(uuid) => {
                remove_perspective(languages, &uuid.to_string()).await;
                // A store a removal left behind (its key not destroyed) goes now.
                match keys.destroy_store(&store, DestroyReason::OwnerRemoved) {
                    Ok(()) | Err(KeyringError::UnknownStore) => {}
                    Err(e) => return Err(format!("Could not destroy the key of {store}: {e}")),
                }
                removed += 1;
            }
            StoreId::Person(_) | StoreId::Executor => {
                keys.destroy_store(&store, DestroyReason::OwnerRemoved)
                    .map_err(|e| format!("Could not destroy the key of {store}: {e}"))?;
            }
        }
    }
    // 3. The rows of the person's own store.
    if let Some(did) = &did {
        let deleted = db
            .delete_person_rows(did)
            .map_err(|e| format!("Could not delete the rows of user {username}: {e}"))?;
        log::info!("Deleted {deleted} rows of a managed user before its data key");
    }
    // 4. The person's keys.
    let report = keys
        .destroy_person(&owner, reason)
        .map_err(|e| format!("Could not destroy the data key of user {username}: {e}"))?;
    Ok(PersonLeave {
        kept: false,
        removed_perspectives: removed,
        left_shared_perspectives: left_shared,
        key_destroyed_at: Some(report.key_destroyed_at),
        shredded_everywhere_at: Some(report.shredded_everywhere_at),
    })
}

/// Removes perspective `uuid` (its rows, its SPARQL store folder, then its key), then the
/// storage files of its link language when no perspective left uses that language.
async fn remove_perspective(languages: &Path, uuid: &str) {
    let link_language = crate::db_backend::db_backend()
        .get_all_perspectives()
        .ok()
        .and_then(|handles| handles.into_iter().find(|h| h.uuid == uuid))
        .and_then(|h| h.neighbourhood.map(|nh| nh.data.link_language));
    crate::perspectives::remove_perspective(uuid).await;
    let Some(language) = link_language else {
        return;
    };
    let still_used = crate::db_backend::db_backend()
        .get_all_perspectives()
        .map(|handles| {
            handles.iter().any(|h| {
                h.neighbourhood
                    .as_ref()
                    .is_some_and(|nh| nh.data.link_language == language)
            })
        })
        .unwrap_or(true);
    if still_used {
        return;
    }
    match super::lang_storage::delete_storage(languages, &language) {
        Ok(n) => log::debug!("Deleted {n} storage files of a removed perspective's link language"),
        Err(e) => log::warn!("Could not delete the storage of link language {language}: {e}"),
    }
}

/// Takes the user out of the owners of a perspective that keeps `others`: its wrap of the
/// perspective's key goes when the handle is written.
async fn leave_shared(
    handle: &crate::types::PerspectiveHandle,
    others: Vec<String>,
) -> Result<(), String> {
    if crate::perspectives::get_perspective(&handle.uuid).is_some() {
        crate::perspectives::update_perspective_with(&handle.uuid, |h| {
            h.owners = Some(others);
        })
        .await
        .map(|_| ())
    } else {
        let mut handle = handle.clone();
        handle.owners = Some(others);
        crate::db_backend::db_backend()
            .update_perspective(&handle)
            .map_err(|e| e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::at_rest::keyring_file::path_in;
    use crate::at_rest::sqlite::database_key;
    use crate::db::Ad4mDb;
    use crate::types::domain::Neighbourhood;
    use crate::types::{
        DecoratedNeighbourhoodExpression, NotificationInput, PerspectiveHandle, PerspectiveState,
    };
    use std::path::PathBuf;

    const LINK_LANGUAGE: &str = "QmLinkLanguageOfAlicesNeighbourhood";

    /// An executor's data folder with an open keyring and a keyed database put in place of the
    /// global one, with the perspectives' data folder pointed at it. Both come back when
    /// dropped, even after a failed assertion.
    struct Executor {
        folder: tempfile::TempDir,
        keys: Arc<DataKeys>,
        languages: PathBuf,
        replaced_db: Option<Ad4mDb>,
        replaced_path: Option<String>,
    }

    impl Executor {
        fn start() -> Self {
            let folder = tempfile::tempdir().unwrap();
            let keys = DataKeys::load(path_in(folder.path())).unwrap();
            keys.unlock(zeroize::Zeroizing::new([21u8; 32])).unwrap();
            let keys = Arc::new(keys);
            let key = database_key(&keys).unwrap();
            let db = Ad4mDb::open(
                &folder.path().join("ad4m_db.sqlite"),
                Some((&key, keys.clone())),
            )
            .unwrap();
            let replaced_db = Ad4mDb::global_instance()
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .replace(db);
            let replaced_path = crate::perspectives::replace_app_data_path(Some(
                folder.path().to_string_lossy().into_owned(),
            ));
            let languages = folder.path().join("languages");
            std::fs::create_dir_all(&languages).unwrap();
            Executor {
                folder,
                keys,
                languages,
                replaced_db,
                replaced_path,
            }
        }

        fn db<R>(&self, f: impl FnOnce(&Ad4mDb) -> R) -> R {
            Ad4mDb::with_global_instance(f)
        }

        fn perspective(&self, owners: &[&str], link_language: Option<&str>) -> String {
            let uuid = uuid::Uuid::new_v4().to_string();
            self.db(|db| {
                db.add_perspective(&PerspectiveHandle {
                    uuid: uuid.clone(),
                    name: Some("a perspective of alice".into()),
                    neighbourhood: link_language.map(|address| DecoratedNeighbourhoodExpression {
                        data: Neighbourhood {
                            link_language: address.into(),
                            ..Default::default()
                        },
                        ..Default::default()
                    }),
                    shared_url: None,
                    state: PerspectiveState::Private,
                    owners: Some(owners.iter().map(|o| o.to_string()).collect()),
                })
            })
            .unwrap();
            // Its SPARQL store.
            let sparql = self.folder.path().join("perspectives").join(&uuid);
            std::fs::create_dir_all(&sparql).unwrap();
            std::fs::write(sparql.join("store.db"), b"triples").unwrap();
            uuid
        }

        fn handle(&self, uuid: &str) -> Option<PerspectiveHandle> {
            self.db(|db| db.get_all_perspectives())
                .unwrap()
                .into_iter()
                .find(|h| h.uuid == uuid)
        }

        fn store(uuid: &str) -> StoreId {
            StoreId::Perspective(uuid.parse().unwrap())
        }

        /// Every file under the data folder, with its bytes.
        fn files(&self) -> Vec<(PathBuf, Vec<u8>)> {
            fn walk(dir: &Path, found: &mut Vec<(PathBuf, Vec<u8>)>) {
                for entry in std::fs::read_dir(dir).unwrap() {
                    let path = entry.unwrap().path();
                    if path.is_dir() {
                        walk(&path, found);
                    } else {
                        let bytes = std::fs::read(&path).unwrap();
                        found.push((path, bytes));
                    }
                }
            }
            let mut found = Vec::new();
            walk(self.folder.path(), &mut found);
            found
        }
    }

    impl Drop for Executor {
        fn drop(&mut self) {
            *Ad4mDb::global_instance()
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner) = self.replaced_db.take();
            crate::perspectives::replace_app_data_path(self.replaced_path.take());
        }
    }

    fn notification(description: &str) -> NotificationInput {
        NotificationInput {
            description: description.into(),
            app_name: "app".into(),
            app_url: "https://app.test".into(),
            app_icon_path: "icon".into(),
            trigger: "SELECT 1".into(),
            perspective_ids: vec![],
            webhook_url: "https://webhook.test".into(),
            webhook_auth: "auth".into(),
        }
    }

    /// Alice owns one perspective alone (on a link language with stored files) and shares one
    /// with Bob; she has a notification and a task. Bob has a notification of his own.
    fn alice_and_bob(executor: &Executor) -> (String, String) {
        executor.db(|db| {
            db.add_user("alice", "did:key:alice", "pw").unwrap();
            db.add_user("bob", "did:key:bob", "pw").unwrap();
        });
        assert_eq!(
            prepare(&executor.keys, &["alice".into(), "bob".into()]).unwrap(),
            2
        );
        let own = executor.perspective(&["did:key:alice"], Some(LINK_LANGUAGE));
        let shared = executor.perspective(&["did:key:alice", "did:key:bob"], None);
        let language = executor.languages.join(LINK_LANGUAGE);
        std::fs::create_dir_all(&language).unwrap();
        std::fs::write(language.join("bundle.js"), b"bundle").unwrap();
        std::fs::write(language.join("links.db"), b"alice's links").unwrap();
        executor.db(|db| {
            db.add_notification(notification("alice's"), "did:key:alice", true)
                .unwrap();
            db.add_notification(notification("bob's"), "did:key:bob", true)
                .unwrap();
            db.add_task(
                "task".into(),
                "model".into(),
                "alice's prompt".into(),
                vec![],
                None,
                Some("did:key:alice"),
            )
            .unwrap();
        });
        (own, shared)
    }

    /// A person's delete: her own perspective goes with its rows, SPARQL store, key and link
    /// language storage; the shared one stays for Bob with her wrap gone; the store no handle
    /// names goes; her rows, data key and person store go. The tombstones hold no personal
    /// data, the delete reports one time for the key and for its last copy, and no file but
    /// the keyring holds a keyring.
    #[tokio::test]
    async fn a_deleted_person_leaves_no_key_no_row_and_no_store_it_owned_alone() {
        let executor = Executor::start();
        let (own, shared) = alice_and_bob(&executor);
        // A store of hers whose perspective's removal stopped before its key went.
        let orphan = StoreId::Perspective(uuid::Uuid::new_v4());
        executor
            .keys
            .set_store_owners(&orphan, &[owner_of("alice")])
            .unwrap();
        let alice = owner_of("alice");
        let bob = owner_of("bob");
        assert_eq!(
            executor.keys.stores_of(&alice).unwrap().len(),
            3,
            "her perspective, the shared one and the orphan"
        );

        let left = leave_with(
            &executor.keys,
            &executor.languages,
            "alice",
            false,
            DestroyReason::UserRemoved,
        )
        .await
        .unwrap();
        assert!(!left.kept);
        assert_eq!(left.removed_perspectives, 2);
        assert_eq!(left.left_shared_perspectives, 1);
        assert!(left.key_destroyed_at.is_some());
        assert_eq!(left.shredded_everywhere_at, left.key_destroyed_at);

        // Her own perspective.
        assert!(executor.handle(&own).is_none());
        assert!(!executor.keys.has_store(&Executor::store(&own)).unwrap());
        assert!(!executor
            .folder
            .path()
            .join("perspectives")
            .join(&own)
            .exists());
        let language = executor.languages.join(LINK_LANGUAGE);
        assert!(!language.join("links.db").exists());
        assert!(
            language.join("bundle.js").exists(),
            "the bundle is not hers"
        );
        // The shared one.
        let kept = executor.handle(&shared).unwrap();
        assert_eq!(kept.owners, Some(vec!["did:key:bob".to_string()]));
        assert_eq!(
            executor
                .keys
                .store_owners(&Executor::store(&shared))
                .unwrap(),
            vec![bob.clone()]
        );
        assert!(executor
            .folder
            .path()
            .join("perspectives")
            .join(&shared)
            .exists());
        // The orphan.
        assert!(!executor.keys.has_store(&orphan).unwrap());
        // Her rows, not Bob's.
        executor.db(|db| {
            assert!(db
                .get_notifications_for_owner("did:key:alice")
                .unwrap()
                .is_empty());
            assert_eq!(
                db.get_notifications_for_owner("did:key:bob").unwrap().len(),
                1
            );
            assert!(db.get_tasks().unwrap().is_empty());
        });
        // Her keys.
        assert!(!executor.keys.has_person(&alice).unwrap());
        assert!(!executor
            .keys
            .has_store(&StoreId::Person(alice.clone()))
            .unwrap());
        assert!(executor.keys.stores_of(&alice).unwrap().is_empty());
        assert!(executor.keys.has_person(&bob).unwrap());

        // The tombstones: hashes, a time, a reason.
        let raw = std::fs::read_to_string(executor.keys.path()).unwrap();
        for personal in ["alice", own.as_str(), &orphan.to_string()] {
            assert!(!raw.contains(personal), "the keyring names {personal}");
        }
        let tombstones = executor.keys.summary().unwrap().tombstones;
        let at = left.key_destroyed_at.unwrap().to_rfc3339();
        let person_tombstones: Vec<_> = tombstones
            .iter()
            .filter(|t| t.reason == "user-removed")
            .collect();
        assert_eq!(person_tombstones.len(), 2, "her store and her key");
        assert!(person_tombstones.iter().all(|t| t.destroyed_at == at));

        // No copy of a keyring outside its file.
        let parsed: serde_json::Value = serde_json::from_str(&raw).unwrap();
        let keyring_id = parsed["keyring_id"].to_string();
        let keyring_id = keyring_id.trim_matches('"');
        assert!(!keyring_id.is_empty());
        let file_name = super::super::keyring_file::FILE_NAME;
        for (path, bytes) in executor.files() {
            if path.file_name().and_then(|n| n.to_str()) == Some(file_name) {
                assert_eq!(path, executor.keys.path());
                continue;
            }
            let text = String::from_utf8_lossy(&bytes);
            assert!(
                !text.contains("root_check") && !text.contains(keyring_id),
                "{} holds a keyring",
                path.display()
            );
        }
    }

    /// Kept data stays as it is: the leave writes nothing, and every job the executor runs
    /// at start and on its timer afterwards leaves the keys, the rows and the stores alone.
    #[tokio::test]
    async fn kept_data_stays_through_every_scheduled_job() {
        let executor = Executor::start();
        let (own, shared) = alice_and_bob(&executor);
        let keyring_before = std::fs::read(executor.keys.path()).unwrap();
        let summary_before = executor.keys.summary().unwrap();
        let files_before: Vec<_> = executor
            .files()
            .into_iter()
            .filter(|(path, _)| path.starts_with(&executor.languages))
            .collect();

        let left = leave_with(
            &executor.keys,
            &executor.languages,
            "alice",
            true,
            DestroyReason::UserRemoved,
        )
        .await
        .unwrap();
        assert!(left.kept);
        assert_eq!(left.removed_perspectives + left.left_shared_perspectives, 0);
        assert_eq!(left.key_destroyed_at, None);
        assert_eq!(left.shredded_everywhere_at, None);

        // The start steps and the timer's job.
        assert_eq!(
            prepare(&executor.keys, &["alice".into(), "bob".into()]).unwrap(),
            0
        );
        let backend = crate::db_backend::db_backend();
        assert!(backend
            .perspective_stores_without_handle()
            .unwrap()
            .is_empty());
        backend.cleanup_expired_codes().unwrap();

        assert_eq!(std::fs::read(executor.keys.path()).unwrap(), keyring_before);
        assert_eq!(executor.keys.summary().unwrap(), summary_before);
        assert!(executor.handle(&own).is_some());
        assert!(executor.handle(&shared).is_some());
        assert!(executor.keys.has_store(&Executor::store(&own)).unwrap());
        assert!(executor.keys.has_store(&Executor::store(&shared)).unwrap());
        assert!(executor.keys.has_person(&owner_of("alice")).unwrap());
        executor.db(|db| {
            assert_eq!(
                db.get_notifications_for_owner("did:key:alice")
                    .unwrap()
                    .len(),
                1
            );
            assert_eq!(db.get_tasks().unwrap().len(), 1);
        });
        let files_after: Vec<_> = executor
            .files()
            .into_iter()
            .filter(|(path, _)| path.starts_with(&executor.languages))
            .collect();
        assert_eq!(files_after, files_before);
    }

    /// The start step makes a person for each user made before persons existed, and only
    /// once; the main agent's person is there from the keyring's start.
    #[test]
    fn users_from_before_persons_get_theirs_at_start() {
        let folder = tempfile::tempdir().unwrap();
        let keys = DataKeys::load(path_in(folder.path())).unwrap();
        keys.unlock(zeroize::Zeroizing::new([22u8; 32])).unwrap();
        let users = vec!["carol".to_string(), "dave".to_string()];
        assert!(!keys.has_person(&owner_of("carol")).unwrap());
        assert_eq!(prepare(&keys, &users).unwrap(), 2);
        for user in &users {
            assert!(keys.has_person(&owner_of(user)).unwrap());
            assert!(keys.has_store(&StoreId::Person(owner_of(user))).unwrap());
        }
        let written = std::fs::read(keys.path()).unwrap();
        assert_eq!(prepare(&keys, &users).unwrap(), 0);
        assert_eq!(std::fs::read(keys.path()).unwrap(), written);
        // A user an owner change made a person of has its person store already.
        keys.set_store_owners(
            &StoreId::Perspective(uuid::Uuid::new_v4()),
            &[owner_of("erin")],
        )
        .unwrap();
        assert!(keys.has_store(&StoreId::Person(owner_of("erin"))).unwrap());
        assert_eq!(prepare(&keys, &["erin".to_string()]).unwrap(), 0);
        assert!(keys.has_person(&KeyOwner::Main).unwrap());
    }

    /// Without a storage gate there is no keyring to make a person in: user creation goes on.
    /// A sealed keyring refuses the leave.
    #[tokio::test]
    async fn no_gate_makes_no_person_and_a_sealed_keyring_refuses_the_leave() {
        if crate::at_rest::gate().is_none() {
            assert_eq!(create_for_user("frank").unwrap(), None);
            assert!(leave("frank", false, DestroyReason::UserRemoved)
                .await
                .unwrap_err()
                .contains(crate::at_rest::SEALED));
        }
        let folder = tempfile::tempdir().unwrap();
        let keys = DataKeys::load(path_in(folder.path())).unwrap();
        let refused = leave_with(
            &keys,
            folder.path(),
            "frank",
            false,
            DestroyReason::UserRemoved,
        )
        .await;
        assert!(refused.is_err());
    }
}

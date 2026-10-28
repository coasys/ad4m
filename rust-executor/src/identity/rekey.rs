//! `identity.rekey`: a new main wallet key for an executor whose key was revoked, so it can
//! link again as a fresh executor. Nothing goes into a log. The old key stays in the wallet
//! for history only; what it signed keeps its verdicts, which come from the log.
//!
//! Every place that holds the main agent's DID moves to the new key:
//!
//! | Place | How it moves |
//! |---|---|
//! | the wallet's `main` key | a new key; the old one under `retired-main:<did>` (`agent::rekey`) |
//! | `AgentService`: DID, DID document, signing key id, profile; `agent.json`, `agentProfile.json` | `AgentService::rekey_main_key` |
//! | each language's agent context (`agent.did()`, signing) | reads `AgentService` on each call |
//! | open sessions of the main agent (event audiences) | `Session::acts_as_did` reads the DID again after a re-key |
//! | apps on the main agent | their grants are revoked: their tokens were signed with the old key |
//! | perspective owners | each retired main DID is replaced by the new one (`update_perspective_with`) |
//! | notification owners | from each retired main DID (`Ad4mDb::move_notification_owner`) |
//! | the claims the old key made (`identity.joinStatus`) | forgotten (`Ad4mDb::clear_joins`) |
//! | the published agent profile | published again under the new DID |
//!
//! Stays: the operator record (it names the operator's identity, not this key), the old key's
//! binding in `identity_agents` (history), links the old key signed.
//!
//! One re-key runs at a time (`REKEY`), in its own task: a caller that goes away does not stop
//! it half way. Owners move from every retired main key, not only the last one, so the next
//! re-key repairs a place an earlier one failed to move.

use crate::agent::grants::Grants;
use crate::agent::rekey::retired_key_name;
use crate::agent::{AgentContext, AgentService};
use crate::api::ws_handler::WsRpcError;
use crate::db::Ad4mDb;
use crate::events::Event;
use crate::pubsub::get_global_pubsub_sync;
use crate::wallet::wallet_backend;

use super::types::DeviceKeys;

/// Held across a whole re-key: the key change and every move that follows it.
static REKEY: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

/// Re-keys the main agent and moves every place that holds its DID (the table above). Waits
/// for a re-key already running. The work runs in a spawned task, so it completes even when
/// the caller's future is dropped.
pub async fn rekey_main() -> Result<DeviceKeys, WsRpcError> {
    tokio::spawn(async {
        let _one_at_a_time = REKEY.lock().await;
        rekey_and_move().await
    })
    .await
    .map_err(|e| WsRpcError::internal(format!("the re-key stopped: {e}")))?
}

/// The main agent's DIDs of earlier keys, from their retired names in the wallet.
fn retired_main_dids() -> Vec<String> {
    let prefix = retired_key_name("");
    wallet_backend()
        .list_key_names()
        .into_iter()
        .filter_map(|name| name.strip_prefix(&prefix).map(str::to_string))
        .collect()
}

/// The key changes first; a place that then fails to move is logged and named in a 500, the
/// others still move.
async fn rekey_and_move() -> Result<DeviceKeys, WsRpcError> {
    let rekeyed = AgentService::with_mutable_global_instance(|svc| svc.rekey_main_key())
        .map_err(|e| WsRpcError::conflict(format!("cannot re-key: {e}")))?;
    let (old, new) = (rekeyed.old_did.as_str(), rekeyed.new_did.as_str());
    log::warn!("identity.rekey: the main agent's key is now {new}; {old} is kept for history");
    let retired = retired_main_dids();
    let mut failed: Vec<String> = Vec::new();

    if let Err(e) = Grants::global().revoke_agent(&AgentContext::main_agent()) {
        failed.push(format!("the apps on the main agent: {e:?}"));
    }

    for instance in crate::perspectives::all_perspectives() {
        let handle = instance.persisted.lock().await.clone();
        if !retired.iter().any(|did| handle.is_owned_by(did)) {
            continue;
        }
        let moved = crate::perspectives::update_perspective_with(&handle.uuid, |h| {
            if let Some(owners) = h.owners.as_mut() {
                let mut moved: Vec<String> = Vec::with_capacity(owners.len());
                for owner in owners.drain(..) {
                    let owner = if retired.contains(&owner) {
                        new.to_string()
                    } else {
                        owner
                    };
                    if !moved.contains(&owner) {
                        moved.push(owner);
                    }
                }
                *owners = moved;
            }
        })
        .await;
        if let Err(e) = moved {
            failed.push(format!("the owners of perspective {}: {e}", handle.uuid));
        }
    }

    let rows = Ad4mDb::with_global_instance(|db| {
        for did in &retired {
            db.move_notification_owner(did, new)?;
        }
        db.clear_joins()
    });
    if let Err(e) = rows {
        failed.push(format!("the notifications and claims: {e}"));
    }

    let (status, agent) = AgentService::with_global_instance(|svc| (svc.dump(), svc.agent.clone()));
    let pubsub = get_global_pubsub_sync();
    pubsub.publish_event(Event::agent_status(&status));
    if let Some(agent) = agent {
        pubsub.publish_event(Event::agent_updated(&agent));
    }
    tokio::spawn(async {
        if let Err(e) = AgentService::publish_agent_to_language(&AgentContext::main_agent()).await {
            log::warn!("identity.rekey: could not publish the profile under the new key: {e}");
        }
    });

    if !failed.is_empty() {
        log::error!(
            "identity.rekey: the key changed, but these did not move to it: {}",
            failed.join("; ")
        );
        return Err(WsRpcError::internal(format!(
            "the main key is now {new}, but these did not move to it: {}",
            failed.join("; ")
        )));
    }
    super::custody::main_device_keys()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::{AuthInfo, ListenerReach};
    use crate::agent::session::Session;
    use crate::db::JoinRow;
    use crate::types::{NotificationInput, PerspectiveHandle};

    /// After a re-key, every place of the table above holds the new DID (or let go of the
    /// old key's state), and the old key still signs nothing.
    #[tokio::test]
    async fn a_rekey_moves_every_place_that_holds_the_main_agents_did() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        let old = AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("rekey".into());
            svc.store_agent_profile();
            svc.save("rekey".into());
            svc.did.clone().unwrap()
        });

        // An operator session that read the DID, and an app on the main agent.
        let operator =
            Session::open("admin", &Some("admin".into()), ListenerReach::Loopback).unwrap();
        assert_eq!(operator.acts_as_did().as_deref(), Some(old.as_str()));
        let app_token = crate::test_utils::granted_token(AuthInfo {
            app_name: "an app on the main agent".to_string(),
            ..AuthInfo::default()
        });
        let app = Session::open(&app_token, &Some("admin".into()), ListenerReach::Network)
            .expect("the app's token opens a session before the re-key");

        // A perspective the main agent owns, beside a user's.
        let owned = PerspectiveHandle::new_with_owner("mine".into(), old.clone());
        let users = PerspectiveHandle::new_with_owner("theirs".into(), "did:key:rekey-user".into());
        crate::perspectives::add_perspective(owned.clone(), None)
            .await
            .unwrap();
        crate::perspectives::add_perspective(users.clone(), None)
            .await
            .unwrap();
        // A notification it owns, and a claim its old key made.
        Ad4mDb::with_global_instance(|db| {
            db.add_notification(
                NotificationInput {
                    description: "mine".into(),
                    app_name: "app".into(),
                    app_url: "https://app.test".into(),
                    app_icon_path: String::new(),
                    trigger: "SELECT 1".into(),
                    perspective_ids: vec![],
                    webhook_url: "https://webhook.test".into(),
                    webhook_auth: "auth".into(),
                },
                &old,
                true,
            )?;
            db.add_join(&JoinRow {
                offer_id: "rekey-offer".into(),
                scid: "rekey-scid".into(),
                claimed_at: 1,
            })
        })
        .unwrap();

        let keys = rekey_main().await.expect("the re-key moves every place");
        let new = keys.sign.clone();

        assert_ne!(new, old);
        assert!(keys.enc.starts_with("z6LS"), "{}", keys.enc);
        // The main agent's DID everywhere the executor reads it.
        assert_eq!(crate::agent::did(), new);
        assert_eq!(operator.acts_as_did().as_deref(), Some(new.as_str()));
        // The app's grant is revoked: its token was signed with the old key.
        assert!(app.check().is_err(), "the app's grant ends at the re-key");
        assert!(Session::open(&app_token, &Some("admin".into()), ListenerReach::Network).is_err());
        // Perspective owners: the main agent's perspective moved, the user's did not.
        let mine = crate::perspectives::get_perspective(&owned.uuid).unwrap();
        let mine = mine.persisted.lock().await.clone();
        assert_eq!(mine.owners, Some(vec![new.clone()]));
        let theirs = crate::perspectives::get_perspective(&users.uuid).unwrap();
        let theirs = theirs.persisted.lock().await.clone();
        assert_eq!(theirs.owners, Some(vec!["did:key:rekey-user".to_string()]));
        // Notifications and claims.
        let (owners, join) = Ad4mDb::with_global_instance(|db| {
            let owners: Vec<String> = db
                .get_notifications()
                .unwrap()
                .into_iter()
                .map(|n| n.owner_did)
                .collect();
            (owners, db.join("rekey-offer").unwrap())
        });
        assert!(owners.contains(&new), "{owners:?}");
        assert!(!owners.contains(&old), "{owners:?}");
        assert_eq!(join, None);

        crate::perspectives::remove_perspective(&owned.uuid).await;
        crate::perspectives::remove_perspective(&users.uuid).await;
        crate::test_utils::setup_agent();
    }

    /// The operator record names the operator's identity, not this key: a re-key leaves it.
    #[tokio::test]
    async fn a_rekey_leaves_the_operator_record() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("rekey".into());
            svc.store_agent_profile();
            svc.save("rekey".into());
        });
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(Some("did:scid:op")))
            .unwrap();
        rekey_main().await.unwrap();
        let record = Ad4mDb::with_global_instance(|db| db.operator_record()).unwrap();
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        crate::test_utils::setup_agent();
        assert_eq!(record.map(|r| r.scid).as_deref(), Some("did:scid:op"));
    }

    /// A main agent with a key, a passphrase in memory and its files in `dir`; returns its DID.
    fn main_agent_in(dir: &tempfile::TempDir) -> String {
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("rekey".into());
            svc.store_agent_profile();
            svc.save("rekey".into());
            svc.did.clone().unwrap()
        })
    }

    /// Waits until the main agent's DID is no longer `did`, and returns the new one.
    async fn key_after(did: &str) -> String {
        for _ in 0..1000 {
            let now = crate::agent::did();
            if now != did {
                return now;
            }
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
        panic!("the main key did not change from {did}");
    }

    async fn owners_of(uuid: &str) -> Option<Vec<String>> {
        let instance = crate::perspectives::get_perspective(uuid).unwrap();
        let owners = instance.persisted.lock().await.owners.clone();
        owners
    }

    /// A second re-key waits until the first has moved every place, so the owners end on the
    /// last key. A test that holds the perspective stops the first re-key after its key change.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn two_rekeys_run_one_after_the_other() {
        let dir = tempfile::tempdir().unwrap();
        let old = main_agent_in(&dir);
        let owned = PerspectiveHandle::new_with_owner("serial".into(), old.clone());
        crate::perspectives::add_perspective(owned.clone(), None)
            .await
            .unwrap();
        let instance = crate::perspectives::get_perspective(&owned.uuid).unwrap();

        let held = instance.persisted.lock().await;
        let first = tokio::spawn(rekey_main());
        let first_key = key_after(&old).await;
        let second = tokio::spawn(rekey_main());
        tokio::time::sleep(std::time::Duration::from_millis(300)).await;
        let during = crate::agent::did();
        drop(held);
        let first = first.await.unwrap().unwrap();
        let second = second.await.unwrap().unwrap();
        let owners = owners_of(&owned.uuid).await;
        crate::perspectives::remove_perspective(&owned.uuid).await;
        crate::test_utils::setup_agent();

        assert_eq!(during, first_key, "the second re-key waits for the first");
        assert_eq!(first.sign, first_key);
        assert_ne!(second.sign, first_key);
        assert_eq!(owners, Some(vec![second.sign]));
    }

    /// A caller that goes away after the key changed does not stop the moves.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn a_rekey_whose_caller_goes_away_still_moves_every_place() {
        let dir = tempfile::tempdir().unwrap();
        let old = main_agent_in(&dir);
        let owned = PerspectiveHandle::new_with_owner("dropped".into(), old.clone());
        crate::perspectives::add_perspective(owned.clone(), None)
            .await
            .unwrap();
        let instance = crate::perspectives::get_perspective(&owned.uuid).unwrap();

        let held = instance.persisted.lock().await;
        let call = tokio::spawn(rekey_main());
        let new = key_after(&old).await;
        call.abort();
        assert!(call.await.unwrap_err().is_cancelled());
        drop(held);
        let mut owners = None;
        for _ in 0..500 {
            owners = owners_of(&owned.uuid).await;
            if owners == Some(vec![new.clone()]) {
                break;
            }
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
        // The re-key holds `REKEY` until it is done.
        drop(REKEY.lock().await);
        crate::perspectives::remove_perspective(&owned.uuid).await;
        crate::test_utils::setup_agent();

        assert_eq!(owners, Some(vec![new]));
    }

    /// Places an earlier re-key left on its old key move on the next one.
    #[tokio::test]
    async fn the_next_rekey_moves_what_an_earlier_one_left_behind() {
        let dir = tempfile::tempdir().unwrap();
        let old = main_agent_in(&dir);
        let owned = PerspectiveHandle::new_with_owner("left".into(), old.clone());
        crate::perspectives::add_perspective(owned.clone(), None)
            .await
            .unwrap();
        let notification = Ad4mDb::with_global_instance(|db| {
            db.add_notification(
                NotificationInput {
                    description: "left".into(),
                    app_name: "app".into(),
                    app_url: "https://app.test".into(),
                    app_icon_path: String::new(),
                    trigger: "SELECT 1".into(),
                    perspective_ids: vec![],
                    webhook_url: "https://webhook.test".into(),
                    webhook_auth: "auth".into(),
                },
                &old,
                true,
            )
        })
        .unwrap();
        // A re-key whose moves never ran.
        AgentService::with_mutable_global_instance(|svc| svc.rekey_main_key()).unwrap();

        let keys = rekey_main().await.unwrap();
        let owners = owners_of(&owned.uuid).await;
        let owner = Ad4mDb::with_global_instance(|db| db.get_notifications())
            .unwrap()
            .into_iter()
            .find(|n| n.id == notification)
            .map(|n| n.owner_did);
        crate::perspectives::remove_perspective(&owned.uuid).await;
        crate::test_utils::setup_agent();

        assert_eq!(owners, Some(vec![keys.sign.clone()]));
        assert_eq!(owner, Some(keys.sign));
    }
}

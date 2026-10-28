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
//! | perspective owners | the old DID is replaced by the new one (`update_perspective_with`) |
//! | notification owners | `Ad4mDb::move_notification_owner` |
//! | the claims the old key made (`identity.joinStatus`) | forgotten (`Ad4mDb::clear_joins`) |
//! | the published agent profile | published again under the new DID |
//!
//! Stays: the operator record (it names the operator's identity, not this key), the old key's
//! binding in `identity_agents` (history), links the old key signed.

use crate::agent::grants::Grants;
use crate::agent::{AgentContext, AgentService};
use crate::api::ws_handler::WsRpcError;
use crate::db::Ad4mDb;
use crate::events::Event;
use crate::pubsub::get_global_pubsub_sync;

use super::types::DeviceKeys;

/// Re-keys the main agent and moves every place that holds its DID (the table above). The key
/// changes first; a place that then fails to move is logged and named in a 500, the others
/// still move.
pub async fn rekey_main() -> Result<DeviceKeys, WsRpcError> {
    let rekeyed = AgentService::with_mutable_global_instance(|svc| svc.rekey_main_key())
        .map_err(|e| WsRpcError::conflict(format!("cannot re-key: {e}")))?;
    let (old, new) = (rekeyed.old_did.as_str(), rekeyed.new_did.as_str());
    log::warn!("identity.rekey: the main agent's key is now {new}; {old} is kept for history");
    let mut failed: Vec<String> = Vec::new();

    if let Err(e) = Grants::global().revoke_agent(&AgentContext::main_agent()) {
        failed.push(format!("the apps on the main agent: {e:?}"));
    }

    for instance in crate::perspectives::all_perspectives() {
        let handle = instance.persisted.lock().await.clone();
        if !handle.is_owned_by(old) {
            continue;
        }
        let moved = crate::perspectives::update_perspective_with(&handle.uuid, |h| {
            if let Some(owners) = h.owners.as_mut() {
                for owner in owners.iter_mut().filter(|o| o.as_str() == old) {
                    *owner = new.to_string();
                }
            }
        })
        .await;
        if let Err(e) = moved {
            failed.push(format!("the owners of perspective {}: {e}", handle.uuid));
        }
    }

    let rows = Ad4mDb::with_global_instance(|db| {
        db.move_notification_owner(old, new)?;
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
}

//! The rows a re-key of the main agent (`identity.rekey`) moves from the old device key's DID
//! to the new one. Perspective owners move through `perspectives::update_perspective_with`,
//! so the cache and the events follow; the rows here have no cache.

use super::{Ad4mDb, Ad4mDbResult};
use rusqlite::params;

impl Ad4mDb {
    /// The notifications owned by `old` are owned by `new`. Answers how many moved.
    pub fn move_notification_owner(&self, old: &str, new: &str) -> Ad4mDbResult<usize> {
        Ok(self.conn.execute(
            "UPDATE notifications SET owner_did = ?2 WHERE owner_did = ?1",
            params![old, new],
        )?)
    }

    /// The flow bookkeeping of `old` (`flow_replica_state`, `flow_fired_mark`) is `new`'s, so
    /// `perspective.flowStates` and `perspective.flowFired` keep answering the main agent. A
    /// row `new` already has stays, and the old one beside it stays under `old`.
    pub fn move_flow_rows(&self, old: &str, new: &str) -> Ad4mDbResult<usize> {
        let tx = self.conn.unchecked_transaction()?;
        let mut moved = 0;
        for table in ["flow_replica_state", "flow_fired_mark"] {
            moved += tx.execute(
                &format!("UPDATE OR IGNORE {table} SET agent_did = ?2 WHERE agent_did = ?1"),
                params![old, new],
            )?;
        }
        tx.commit()?;
        Ok(moved)
    }

    /// Forgets every claim this executor made for its main agent: the old key made them, and
    /// an app that confirms one now would add a key this executor no longer uses.
    pub fn clear_joins(&self) -> Ad4mDbResult<usize> {
        Ok(self.conn.execute("DELETE FROM identity_joins", [])?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::JoinRow;
    use crate::types::NotificationInput;

    #[test]
    fn a_rekey_moves_notifications_to_the_new_did_and_forgets_the_old_keys_claims() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let input = || NotificationInput {
            description: "a notification".into(),
            app_name: "app".into(),
            app_url: "https://app.test".into(),
            app_icon_path: String::new(),
            trigger: "SELECT 1".into(),
            perspective_ids: vec![],
            webhook_url: "https://webhook.test".into(),
            webhook_auth: "auth".into(),
        };
        db.add_notification(input(), "did:key:old", true).unwrap();
        db.add_notification(input(), "did:key:user", true).unwrap();
        db.add_join(&JoinRow {
            offer_id: "offer".into(),
            scid: "scid".into(),
            claimed_at: 1,
        })
        .unwrap();

        assert_eq!(
            db.move_notification_owner("did:key:old", "did:key:new")
                .unwrap(),
            1
        );
        let owners: Vec<String> = db
            .get_notifications()
            .unwrap()
            .into_iter()
            .map(|n| n.owner_did)
            .collect();
        assert!(owners.contains(&"did:key:new".to_string()), "{owners:?}");
        assert!(owners.contains(&"did:key:user".to_string()), "{owners:?}");
        assert!(!owners.contains(&"did:key:old".to_string()), "{owners:?}");

        assert_eq!(db.clear_joins().unwrap(), 1);
        assert_eq!(db.join("offer").unwrap(), None);
    }

    /// An update that read the notification before a re-key does not write the retired owner
    /// back: the owner moves only with the re-key.
    #[test]
    fn an_update_keeps_the_owner_a_rekey_moved() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let id = db
            .add_notification(
                NotificationInput {
                    description: "a notification".into(),
                    app_name: "app".into(),
                    app_url: "https://app.test".into(),
                    app_icon_path: String::new(),
                    trigger: "SELECT 1".into(),
                    perspective_ids: vec![],
                    webhook_url: "https://webhook.test".into(),
                    webhook_auth: "auth".into(),
                },
                "did:key:old",
                true,
            )
            .unwrap();
        let read_before = db.get_notification(id.clone()).unwrap().unwrap();
        db.move_notification_owner("did:key:old", "did:key:new")
            .unwrap();

        let mut update = read_before;
        update.description = "edited".into();
        db.update_notification(id.clone(), &update).unwrap();

        let stored = db.get_notification(id).unwrap().unwrap();
        assert_eq!(stored.description, "edited");
        assert_eq!(stored.owner_did, "did:key:new");
    }

    #[test]
    fn a_rekey_moves_the_flow_bookkeeping_to_the_new_did() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.record_flow_replica_pass("p", "did:key:old", "i", Some("open"), &["x".into()], "t")
            .unwrap();
        db.seed_flow_replica_state("p", "did:key:user", "i")
            .unwrap();

        assert_eq!(db.move_flow_rows("did:key:old", "did:key:new").unwrap(), 2);
        let state = db.get_flow_replica_state("p", "did:key:new", "i").unwrap();
        assert_eq!(state.unwrap().current_state.as_deref(), Some("open"));
        assert_eq!(
            db.get_flow_fired_for_instance("p", "did:key:new", "i")
                .unwrap(),
            vec!["x".to_string()]
        );
        assert!(db
            .get_flow_replica_state("p", "did:key:old", "i")
            .unwrap()
            .is_none());
        assert!(db
            .get_flow_replica_state("p", "did:key:user", "i")
            .unwrap()
            .is_some());
    }
}

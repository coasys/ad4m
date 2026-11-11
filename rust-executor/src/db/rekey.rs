//! The rows a re-key of the main agent (`identity.rekey`) moves from the old device key's DID
//! to the new one. Perspective owners move through `perspectives::update_perspective_with`,
//! so the cache and the events follow; the rows here have no cache.

use super::{sealed, Ad4mDb, Ad4mDbResult};
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
    /// row `new` already has is newer (the key changed before `new` wrote anything): it stays,
    /// and the old one beside it goes, so a later re-key does not carry it over the newer one.
    pub fn move_flow_rows(&self, old: &str, new: &str) -> Ad4mDbResult<usize> {
        let tx = self.conn.unchecked_transaction()?;
        let mut moved = 0;
        for (table, key) in [
            ("flow_replica_state", "instance"),
            ("flow_fired_mark", "proposal"),
        ] {
            tx.execute(
                &format!(
                    "DELETE FROM {table} WHERE agent_did = ?1 AND EXISTS (
                        SELECT 1 FROM {table} AS newer
                        WHERE newer.agent_did = ?2
                          AND newer.perspective = {table}.perspective
                          AND newer.{key} = {table}.{key})"
                ),
                params![old, new],
            )?;
            if table == "flow_replica_state" && self.seals() {
                // The agent is part of the row's key, which a sealed state is bound to: the
                // state is sealed again for its new row.
                moved += self.reseal_flow_states(&tx, old, new)?;
                continue;
            }
            moved += tx.execute(
                &format!("UPDATE {table} SET agent_did = ?2 WHERE agent_did = ?1"),
                params![old, new],
            )?;
        }
        tx.commit()?;
        Ok(moved)
    }

    /// Moves `old`'s `flow_replica_state` rows to `new`, each state opened for its old row and
    /// sealed for its new one. Answers how many moved.
    fn reseal_flow_states(
        &self,
        tx: &rusqlite::Transaction,
        old: &str,
        new: &str,
    ) -> Ad4mDbResult<usize> {
        let rows: Vec<(i64, String, String, Option<String>)> = tx
            .prepare(
                "SELECT rowid, perspective, instance, current_state FROM flow_replica_state
                 WHERE agent_did = ?1",
            )?
            .query_map(params![old], |row| {
                Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?))
            })?
            .collect::<Result<_, _>>()?;
        for (rowid, perspective, instance, state) in &rows {
            let state = self
                .reader(
                    &sealed::FLOW_REPLICA_STATE,
                    Some(perspective),
                    &[perspective, old, instance],
                )?
                .open_opt("current_state", state.clone())?;
            let state = self
                .writer(
                    &sealed::FLOW_REPLICA_STATE,
                    Some(perspective),
                    &[perspective, new, instance],
                    None,
                )?
                .seal_opt("current_state", state.as_deref())?;
            tx.execute(
                "UPDATE flow_replica_state SET agent_did = ?2, current_state = ?3 WHERE rowid = ?1",
                params![rowid, new, state],
            )?;
        }
        Ok(rows.len())
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

    /// A row the new DID wrote after the key change, beside the old DID's: the newer one stays
    /// and the old one goes, so the next re-key (both retired, the old one first) does not
    /// carry the stale row over the newer one.
    #[test]
    fn a_rekey_drops_an_old_row_the_new_did_wrote_again() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.record_flow_replica_pass("p", "did:key:old", "i", Some("stale"), &["a".into()], "t")
            .unwrap();
        db.record_flow_replica_pass("p", "did:key:new", "i", Some("fresh"), &["a".into()], "t")
            .unwrap();
        assert_eq!(db.move_flow_rows("did:key:old", "did:key:new").unwrap(), 0);
        assert!(db
            .get_flow_replica_state("p", "did:key:old", "i")
            .unwrap()
            .is_none());
        assert_eq!(
            db.get_flow_fired_for_instance("p", "did:key:new", "i")
                .unwrap(),
            vec!["a".to_string()]
        );
        // The next re-key: every retired DID moves to the newer one, the old one first.
        db.move_flow_rows("did:key:old", "did:key:newer").unwrap();
        db.move_flow_rows("did:key:new", "did:key:newer").unwrap();
        let state = db
            .get_flow_replica_state("p", "did:key:newer", "i")
            .unwrap()
            .and_then(|row| row.current_state);
        assert_eq!(state.as_deref(), Some("fresh"));
    }
}

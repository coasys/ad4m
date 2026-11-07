//! The private details record: one encrypted record per identity, read and written by any
//! device key of the identity without the bot mark, valid at the head, and by the current
//! master key (the restoring app). The store cannot open it; it checks that the envelope is
//! signed by a writer the log lets write (`record::verify_envelope`), that it is wrapped to
//! exactly the log's readers (`record::check_sealed`: every valid device key
//! without the bot mark, plus `"recovery"` exactly when `recoveryKey` is set), that its
//! version is the stored one plus one, an absent record counting as version 0 so the first
//! write is version 1 (`409 {version}` otherwise; a jump would let one write take the version
//! to 2^53 − 1 and end every later write). A `recoveryKey` needs the current master's
//! attestation (`400 attestation`), and a device-signed write copies the stored, still attested
//! recovery key with its attestation unchanged, or carries none when there is none
//! (`400 recovery-key`); only a master-signed write installs, replaces or drops it. After a rotation the old master's attestation no longer counts: the store
//! refuses a write that carries it, and a device writer drops the key until the new master
//! attests again.

use ad4m_identity_core::record::{check_sealed, recovery_of, SealedRecord, Writer};
use rusqlite::{params, OptionalExtension, TransactionBehavior};
use serde_json::{json, Value};

use crate::db::{from_sql, to_sql};
use crate::error::ApiError;
use crate::request::{Incoming, Signer};
use crate::{limits, Store};

impl Store {
    /// `GET /v1/private/{scid}`: the stored record, or `None` (`404`).
    pub fn get_record(
        &self,
        scid: &str,
        incoming: &Incoming<'_>,
    ) -> Result<Option<Value>, ApiError> {
        let did = ad4m_identity_core::did_of(scid);
        self.verify_request(incoming, Some(&did), Signer::AnyKey)?;
        let conn = self.db.lock()?;
        let text: Option<String> = conn
            .query_row(
                "SELECT sealed FROM records WHERE scid = ?1",
                params![scid],
                |r| r.get(0),
            )
            .optional()?;
        text.map(|t| {
            serde_json::from_str(&t).map_err(|e| ApiError::internal(format!("stored record: {e}")))
        })
        .transpose()
    }

    /// `PUT /v1/private/{scid}`: stores `record` when it passes the checks above. Answers
    /// `{version}`.
    pub fn put_record(
        &self,
        scid: &str,
        record: &SealedRecord,
        incoming: &Incoming<'_>,
    ) -> Result<Value, ApiError> {
        let did = ad4m_identity_core::did_of(scid);
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        // The request is checked against the log as this transaction sees it, so no revoke or
        // rotate commits between the check and the write.
        let verified = self.verify_request_in(&tx, incoming, Some(&did), Signer::AnyKey)?;
        let chain = &verified.chain;
        let writer =
            check_sealed(record, chain).map_err(|e| ApiError::invalid(e.name(), e.to_string()))?;
        let stored: Option<(i64, String)> = tx
            .query_row(
                "SELECT version, sealed FROM records WHERE scid = ?1",
                params![scid],
                |r| Ok((r.get(0)?, r.get(1)?)),
            )
            .optional()?;
        // An absent record is version 0, so the first write is version 1: a first write at
        // 2^53 − 1 would leave no version for any later write.
        let version = stored.as_ref().map_or(0, |(v, _)| from_sql(*v));
        if record.version != version.saturating_add(1) {
            return Err(ApiError::conflict_with(
                "version",
                json!({ "version": version }),
            ));
        }
        // A device writer copies the recovery key the stored record holds with the current
        // master's attestation, attestation and all, or none when it holds none (or one a
        // rotation made stale); only a master-signed write installs, replaces or drops it. So a
        // device cannot put back a recovery key the master replaced, even with the old
        // attestation.
        if let Writer::Device(_) = writer {
            let kept = match stored {
                Some((_, text)) => {
                    let before: SealedRecord = serde_json::from_str(&text)
                        .map_err(|e| ApiError::internal(format!("stored record: {e}")))?;
                    recovery_of(&before, chain)
                }
                None => None,
            };
            let carried = (&record.recovery_key, &record.recovery_attestation);
            let same = match &kept {
                Some(k) => carried == (&Some(k.key.clone()), &Some(k.attestation.clone())),
                None => record.recovery_key.is_none(),
            };
            if !same {
                return Err(ApiError::invalid(
                    "recovery-key",
                    "a device write keeps the recovery key the current master attested; only the master changes it",
                ));
            }
        }
        limits::take_identity_write(&tx, &self.settings, scid, now)?;
        let text = serde_json::to_string(record).map_err(|e| ApiError::internal(e.to_string()))?;
        tx.execute(
            "INSERT INTO records (scid, sealed, version, stored_at) VALUES (?1, ?2, ?3, ?4)
             ON CONFLICT(scid) DO UPDATE SET sealed = excluded.sealed, version = excluded.version,
                                             stored_at = excluded.stored_at",
            params![scid, text, to_sql(record.version)?, to_sql(now)?],
        )?;
        tx.commit()?;
        Ok(json!({ "version": record.version }))
    }
}

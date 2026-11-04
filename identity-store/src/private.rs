//! The private details record: one encrypted record per identity, read and written by any
//! device key of the identity without the bot mark, valid at the head, and by the current
//! master key (the restoring app). The store cannot open it; it checks that the record is
//! wrapped to exactly the log's readers (`record::check_sealed`: every valid device key
//! without the bot mark, plus `"recovery"` exactly when `recoveryKey` is set), that its
//! version is the stored one plus one (`409 {version}` otherwise; a jump would let one write
//! take the version to 2^53 − 1 and end every later write), and that a recovery key, once
//! set, stays.

use ad4m_identity_core::record::{check_sealed, SealedRecord};
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
        check_sealed(record, chain).map_err(|e| ApiError::invalid(e.name(), e.to_string()))?;
        let stored: Option<(i64, String)> = tx
            .query_row(
                "SELECT version, sealed FROM records WHERE scid = ?1",
                params![scid],
                |r| Ok((r.get(0)?, r.get(1)?)),
            )
            .optional()?;
        if let Some((version, text)) = stored {
            let version = from_sql(version);
            if record.version != version.saturating_add(1) {
                return Err(ApiError::conflict_with(
                    "version",
                    json!({ "version": version }),
                ));
            }
            let before: SealedRecord = serde_json::from_str(&text)
                .map_err(|e| ApiError::internal(format!("stored record: {e}")))?;
            if before.recovery_key.is_some() && before.recovery_key != record.recovery_key {
                return Err(ApiError::invalid(
                    "recovery-key",
                    "a record keeps the recovery key it has",
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

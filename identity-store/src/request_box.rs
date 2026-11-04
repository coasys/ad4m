//! The request box: short requests between a person's own devices. Any device key of the
//! identity without the bot mark, valid at the head, posts (`403 signer` for a master key);
//! the same keys and the current master key read; the poster or the current master key
//! deletes. A request lives at most 7 days, an identity holds at most 16, and a new
//! `send-master` from a key replaces its older one. The store keeps the signed request each
//! came with, so a reader checks the poster itself; a revoked key's requests go with its
//! revocation.

use ad4m_identity_core::requests::{check_post, PostedRequest, MAX_OPEN_REQUESTS};
use ad4m_ldk::identity::DeviceRequest;
use rusqlite::{params, TransactionBehavior};
use serde_json::{json, Value};

use crate::db::{self, from_sql, to_sql};
use crate::error::ApiError;
use crate::request::{decode_header, Incoming, Signer};
use crate::{limits, Store};

fn to_json<T: serde::Serialize>(value: &T) -> Result<String, ApiError> {
    serde_json::to_string(value).map_err(|e| ApiError::internal(e.to_string()))
}

impl Store {
    /// `POST /v1/requests/{scid}` with `post`, the request's own body. Answers `{id}`.
    pub fn post_request(
        &self,
        scid: &str,
        post: &PostedRequest,
        incoming: &Incoming<'_>,
    ) -> Result<Value, ApiError> {
        let did = ad4m_identity_core::did_of(scid);
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        // Checked against the log as this transaction sees it: no revoke commits in between.
        let verified = self.verify_request_in(&tx, incoming, Some(&did), Signer::AnyKey)?;
        let signed = decode_header(incoming.header.unwrap_or_default())?;
        let chain = &verified.chain;
        let state = chain.state();
        let by = &verified.key_id;
        let device = state
            .keys
            .get(by)
            .is_some_and(|r| r.scopes.is_empty() && state.key_valid_at(by, chain.head().seq));
        if !device {
            return Err(ApiError::forbidden(
                "signer",
                "only a device key without the bot mark posts a request",
            ));
        }
        check_post(post, state, chain.head().seq, now)
            .map_err(|e| ApiError::invalid(e.name(), e.to_string()))?;
        let kind = post.body.kind();
        if kind == "send-master" {
            tx.execute(
                "DELETE FROM requests WHERE scid = ?1 AND by_key = ?2 AND kind = 'send-master'",
                params![scid, by],
            )?;
        }
        let (open, first): (i64, Option<i64>) = tx.query_row(
            "SELECT COUNT(*), MIN(expires) FROM requests WHERE scid = ?1 AND expires > ?2",
            params![scid, to_sql(now)?],
            |r| Ok((r.get(0)?, r.get(1)?)),
        )?;
        if open as usize >= MAX_OPEN_REQUESTS {
            let retry = first.map(|e| from_sql(e).saturating_sub(now)).unwrap_or(60);
            return Err(ApiError::rate_limited("identity", retry.max(1)));
        }
        limits::take_identity_write(&tx, &self.settings, scid, now)?;
        let id = db::new_id();
        tx.execute(
            "INSERT INTO requests (id, scid, by_key, kind, posted, signed, created, expires)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
            params![
                id,
                scid,
                by,
                kind,
                to_json(post)?,
                to_json(&signed)?,
                to_sql(now)?,
                to_sql(post.expires)?
            ],
        )?;
        tx.commit()?;
        Ok(json!({ "id": id }))
    }

    /// `GET /v1/requests/{scid}?since=`: the live requests created at or after `since`,
    /// oldest first.
    pub fn list_requests(
        &self,
        scid: &str,
        since: u64,
        incoming: &Incoming<'_>,
    ) -> Result<Vec<DeviceRequest>, ApiError> {
        let did = ad4m_identity_core::did_of(scid);
        self.verify_request(incoming, Some(&did), Signer::AnyKey)?;
        let now = to_sql(self.now())?;
        let conn = self.db.lock()?;
        let mut stmt = conn.prepare_cached(
            "SELECT id, by_key, posted, signed, created, expires FROM requests
             WHERE scid = ?1 AND created >= ?2 AND expires > ?3 ORDER BY created, rowid",
        )?;
        let rows = stmt
            .query_map(params![scid, to_sql(since)?, now], |r| {
                Ok((
                    r.get::<_, String>(0)?,
                    r.get::<_, String>(1)?,
                    r.get::<_, String>(2)?,
                    r.get::<_, String>(3)?,
                    from_sql(r.get(4)?),
                    from_sql(r.get(5)?),
                ))
            })?
            .collect::<Result<Vec<_>, _>>()?;
        rows.into_iter()
            .map(|(id, by, posted, signed, created, expires)| {
                let posted: ad4m_ldk::identity::PostedRequest = serde_json::from_str(&posted)
                    .map_err(|e| ApiError::internal(format!("stored request: {e}")))?;
                Ok(DeviceRequest {
                    id,
                    body: posted.body,
                    by,
                    created,
                    expires,
                    signed: serde_json::from_str(&signed)
                        .map_err(|e| ApiError::internal(format!("stored request: {e}")))?,
                })
            })
            .collect()
    }

    /// `DELETE /v1/requests/{scid}/{id}`: by the poster or the current master key.
    pub fn delete_request(
        &self,
        scid: &str,
        id: &str,
        incoming: &Incoming<'_>,
    ) -> Result<(), ApiError> {
        let did = ad4m_identity_core::did_of(scid);
        let now = to_sql(self.now())?;
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        // Checked against the log as this transaction sees it: no rotate commits in between.
        let verified = self.verify_request_in(&tx, incoming, Some(&did), Signer::AnyKey)?;
        let master =
            verified.chain.state().current_master().map(|m| &m.id) == Some(&verified.key_id);
        let by: Option<String> = rusqlite::OptionalExtension::optional(tx.query_row(
            "SELECT by_key FROM requests WHERE scid = ?1 AND id = ?2 AND expires > ?3",
            params![scid, id, now],
            |r| r.get(0),
        ))?;
        let Some(by) = by else {
            return Err(ApiError::not_found("no such request"));
        };
        if by != verified.key_id && !master {
            return Err(ApiError::forbidden(
                "signer",
                "only the poster or the current master key deletes a request",
            ));
        }
        tx.execute(
            "DELETE FROM requests WHERE scid = ?1 AND id = ?2",
            params![scid, id],
        )?;
        tx.commit()?;
        Ok(())
    }
}

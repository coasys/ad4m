//! Agent records: today's agent profile under a
//! `did:key` or a `did:scid`.
//!
//! The record signature uses the `coasys:link` scheme over `{profile, version, anchor}`
//! (`anchor` is `null` for a `did:key` record), by the device key `signedBy` names. A
//! `did:key` record is signed by that key itself and needs no signed request; a bot key may
//! write it only while its scope is in force (the bot-scope check). A `did:scid` record comes
//! with a signed request of that identity and carries its anchor: the seq of the identity's log
//! that the signer knew when it signed. It is accepted only when the anchor lies at or below the
//! log's head (`400 anchor` otherwise) and `signedBy` is a non-bot device key the log lists (never
//! a master: the master never signs for the person), valid
//! both at the anchor and at the log's head when the record arrives (`403 signer` otherwise).
//! The signer picks the anchor, so the head check is what stops a key revoked at seq N from
//! backdating a new record to an anchor below N: the store orders arrivals, so a record it
//! accepted before a revocation stays valid (readers check at the anchor), and a record that
//! arrives after the revocation is refused, whatever anchor it names. The key id comes from
//! the log, the person from the record's did, so the anchor carries neither.
//!
//! The store does not read `profile`: the record signature covers it whatever it holds. The
//! agent language puts a `did:key` agent's Agent there as that agent's signed AD4M expression,
//! which the executor verifies on every read.

use ad4m_hash_chain::{decode_ed25519_multikey, decode_proof_value, MAX_SAFE_INTEGER};
use ad4m_identity_core::{scid_of, verify_signed, LINK_DOMAIN};
use ad4m_ldk::identity::AgentRecord;
use rusqlite::{params, OptionalExtension};
use serde_json::{json, Value};

use crate::db::{self, from_sql, to_sql};
use crate::error::ApiError;
use crate::request::{Incoming, Signer};
use crate::Store;

/// True when `record.sig` is `key`'s `coasys:link` signature of `record.signed_value()`.
fn record_signed_by(record: &AgentRecord, key: &str) -> bool {
    let Some(sig) = decode_proof_value(&record.sig) else {
        return false;
    };
    verify_signed(LINK_DOMAIN, &record.signed_value(), key, &sig)
}

fn signature_refused() -> ApiError {
    ApiError::forbidden("signer", "the record's signature does not verify")
}

impl Store {
    /// `GET /v1/agents/{did}`.
    pub fn agent(&self, did: &str) -> Result<Option<Value>, ApiError> {
        let conn = self.db.lock()?;
        let text: Option<String> = conn
            .query_row(
                "SELECT record FROM agents WHERE did = ?1",
                params![did],
                |r| r.get(0),
            )
            .optional()?;
        text.map(|t| serde_json::from_str(&t).map_err(|e| ApiError::internal(e.to_string())))
            .transpose()
    }

    /// `PUT /v1/agents/{did}`.
    pub fn put_agent(
        &self,
        did: &str,
        record: &AgentRecord,
        incoming: &Incoming<'_>,
    ) -> Result<(), ApiError> {
        if record.did != did {
            return Err(ApiError::invalid(
                "malformed",
                "the record names another did",
            ));
        }
        if record.version > MAX_SAFE_INTEGER {
            return Err(ApiError::invalid(
                "malformed",
                "version is not a safe integer",
            ));
        }
        let now = self.now();
        if let Some(key) = did.strip_prefix("did:key:") {
            if decode_ed25519_multikey(key).is_none() {
                return Err(ApiError::invalid(
                    "malformed",
                    "a did:key names an Ed25519 key",
                ));
            }
            if record.signed_by != did || record.anchor.is_some() {
                return Err(ApiError::forbidden(
                    "signer",
                    "a did:key record is signed by that key, with no anchor",
                ));
            }
            if !record_signed_by(record, key) {
                return Err(signature_refused());
            }
            self.check_bot_writer(key, now)?;
        } else if let Some(scid) = scid_of(did) {
            let verified = self.verify_request(incoming, Some(did), Signer::AnyKey)?;
            let anchor = record.anchor.ok_or_else(|| {
                ApiError::invalid("malformed", "a did:scid record names its anchor")
            })?;
            let key = record
                .signed_by
                .strip_prefix("did:key:")
                .ok_or_else(|| ApiError::invalid("malformed", "signedBy is a did:key"))?;
            let chain = &verified.chain;
            if verified.scid != scid {
                return Err(ApiError::forbidden(
                    "signer",
                    "the signed request is another identity's",
                ));
            }
            let head = chain.head().seq;
            if anchor > head {
                return Err(ApiError::invalid(
                    "anchor",
                    "the anchor lies past the log's head",
                ));
            }
            let state = chain.state();
            let valid_at = |id, seq| {
                state.bot_scope_at(id, seq).is_none() && state.key_valid_at_time(id, seq, now)
            };
            // Device keys only, never a master: the master never signs for the person.
            let device_id = state
                .keys
                .iter()
                .find(|(_, r)| r.sign == key)
                .map(|(id, _)| id);
            let valid = device_id.is_some_and(|id| valid_at(id, anchor) && valid_at(id, head));
            if !valid {
                return Err(ApiError::forbidden(
                    "signer",
                    "signedBy is not a non-bot device key of this log valid at the anchor and at the head",
                ));
            }
            if !record_signed_by(record, key) {
                return Err(signature_refused());
            }
        } else {
            return Err(ApiError::invalid(
                "malformed",
                "an agent record names a did:key or a did:scid",
            ));
        }

        let text = serde_json::to_string(record).map_err(|e| ApiError::internal(e.to_string()))?;
        let mut conn = self.db.lock()?;
        let tx = conn.transaction()?;
        let stored: Option<i64> = tx
            .query_row(
                "SELECT version FROM agents WHERE did = ?1",
                params![did],
                |r| r.get(0),
            )
            .optional()?;
        if let Some(stored) = stored.map(from_sql) {
            if record.version <= stored {
                return Err(ApiError::conflict_with(
                    "version",
                    json!({ "version": stored }),
                ));
            }
        }
        tx.execute(
            "INSERT INTO agents (did, record, version, stored_at) VALUES (?1, ?2, ?3, ?4)
             ON CONFLICT(did) DO UPDATE SET record = excluded.record, version = excluded.version,
                                            stored_at = excluded.stored_at",
            params![did, text, to_sql(record.version)?, to_sql(now)?],
        )?;
        tx.commit()?;
        Ok(())
    }

    /// The bot-scope check: a bot key writes its own record only while its scope is in force.
    fn check_bot_writer(&self, key: &str, now: u64) -> Result<(), ApiError> {
        let conn = self.db.lock()?;
        let Some(scid) = db::key_owner(&conn, key)? else {
            return Ok(());
        };
        let chain = db::load_chain(&conn, &scid)?
            .ok_or_else(|| ApiError::internal("key without its log"))?;
        let state = chain.state();
        let head = chain.head().seq;
        if state.bot_scope_at(key, head).is_some() && !state.key_valid_at_time(key, head, now) {
            return Err(ApiError::forbidden(
                "writer",
                "the bot key's scope is not in force",
            ));
        }
        Ok(())
    }
}

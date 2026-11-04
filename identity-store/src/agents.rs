//! Agent records: today's agent profile under a
//! `did:key` or a `did:scid`.
//!
//! The record signature is the `coasys:agent-record` domain over `{profile, version, anchor}`
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
//! `version` is unix milliseconds, and the store takes a record only when its version is
//! above the stored one and at most [`MAX_VERSION_AHEAD_S`] ahead of the store's clock. Without
//! that ceiling one write at a huge version, by a key that was valid then (a stolen device key
//! before its revocation; for a `did:key`, any code the agent's key signs for), would freeze
//! the record for good, since every later write needs a higher version. With it the owner
//! can write `stored + 1` at the latest one second after the stored write: the stored version
//! was at most the ceiling of its own write, and the ceiling grows with the clock. Revoking
//! the stolen key then stops it writing, and the owner's next record replaces its last one.
//! The rule needs no revocation, so it covers a `did:key` record too.
//!
//! The store does not read `profile`: the record signature covers it whatever it holds. The
//! agent language puts a `did:key` agent's Agent there as that agent's signed AD4M expression,
//! which the executor verifies on every read.

use ad4m_hash_chain::{decode_ed25519_multikey, decode_proof_value};
use ad4m_identity_core::{scid_of, verify_signed, AGENT_RECORD_DOMAIN};
use ad4m_ldk::identity::AgentRecord;
use rusqlite::{params, Connection, OptionalExtension, TransactionBehavior};
use serde_json::{json, Value};

use crate::db::{self, from_sql, to_sql};
use crate::error::ApiError;
use crate::request::{Incoming, Signer};
use crate::Store;

/// How far a record's `version` (unix milliseconds) may lie ahead of the store's clock.
pub const MAX_VERSION_AHEAD_S: u64 = 300;

/// The highest `version` the store takes at `now` (unix seconds): `now` plus
/// [`MAX_VERSION_AHEAD_S`], in milliseconds.
pub fn version_ceiling(now: u64) -> u64 {
    now.saturating_add(MAX_VERSION_AHEAD_S).saturating_mul(1000)
}

/// True when `record.sig` is `key`'s `coasys:agent-record` signature of
/// `record.signed_value()`.
fn record_signed_by(record: &AgentRecord, key: &str) -> bool {
    let Some(sig) = decode_proof_value(&record.sig) else {
        return false;
    };
    verify_signed(AGENT_RECORD_DOMAIN, &record.signed_value(), key, &sig)
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

    /// `PUT /v1/agents/{did}`. The signer checks, the version check and the write run in one
    /// immediate transaction, so an append that revokes the signer commits either before the
    /// check (and the record is refused) or after the write.
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
        let now = self.now();
        let ceiling = version_ceiling(now);
        if record.version > ceiling {
            return Err(ApiError::invalid(
                "version",
                format!("version is unix milliseconds, at most {ceiling} by the store's clock"),
            ));
        }
        let text = serde_json::to_string(record).map_err(|e| ApiError::internal(e.to_string()))?;
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
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
            self.check_bot_writer(&tx, key, now)?;
        } else if let Some(scid) = scid_of(did) {
            let verified = self.verify_request_in(&tx, incoming, Some(did), Signer::AnyKey)?;
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
            let device_id = state.device_key_id_of(&key.to_string());
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
}

impl Store {
    /// The bot-scope check: a bot key writes its own record only while its scope is in force.
    /// Runs in the write's transaction.
    fn check_bot_writer(&self, conn: &Connection, key: &str, now: u64) -> Result<(), ApiError> {
        let Some(scid) = db::key_owner(conn, key)? else {
            return Ok(());
        };
        let chain = self
            .held_chain(conn, &scid)?
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

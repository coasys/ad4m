//! Vouching notes: the store redeems a one-use token at the platform's vouching service and keeps
//! the note it answers beside the log (table `notes`, at most one per log).
//!
//! `POST {VOUCH_URL}/api/vouch/redeem {token, did}` with `Authorization: Bearer
//! {VOUCH_CREDENTIAL}` answers `200 VouchNote`, `409` when the address already vouched another
//! identity, `410` when the token is spent or expired; a second redeem of the same token for the
//! same `did` answers the same note. The store maps: a note for `did` that [`verify_note`]
//! passes under `VOUCH_KEYS` is taken; `409` and `410` are `403 {error:"vouch"}`; anything else
//! (no `VOUCH_URL`, the service unreachable, a `5xx`, a note that does not verify) is
//! `503 {error:"vouch-unavailable"}`: the service failed, not the client.

use std::time::Duration;

use ad4m_identity_core::{did_of, verify_note};
use ad4m_ldk::identity::VouchNote;
use reqwest::StatusCode;
use rusqlite::{params, Connection, OptionalExtension, TransactionBehavior};
use serde_json::json;

use crate::db::to_sql;
use crate::error::ApiError;
use crate::request::{Incoming, Signer};
use crate::Store;

/// How long a redeem may take.
pub const REDEEM_TIMEOUT: Duration = Duration::from_secs(10);

/// The store's credential at the vouching service. Its `Debug` hides it.
#[derive(Clone, PartialEq, Eq)]
pub struct Credential(pub String);

impl std::fmt::Debug for Credential {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Credential(…)")
    }
}

/// The HTTP client of the vouching service.
pub(crate) fn client() -> reqwest::Client {
    reqwest::Client::builder()
        .timeout(REDEEM_TIMEOUT)
        .build()
        .unwrap_or_default()
}

fn unavailable(detail: impl std::fmt::Display) -> ApiError {
    eprintln!("identity-store: vouching service: {detail}");
    ApiError::unavailable("vouch-unavailable")
}

fn refused() -> ApiError {
    ApiError::forbidden(
        "vouch",
        "the vouching token is spent, expired or vouched another identity",
    )
}

/// The core's form of a wire note.
fn core_note(note: &VouchNote) -> ad4m_identity_core::VouchNote {
    ad4m_identity_core::VouchNote {
        did: note.did.clone(),
        method: note.method.clone(),
        time: note.time,
        voucher: note.voucher.clone(),
        sig: note.sig.clone(),
    }
}

/// True when `note` vouches for `did` and verifies under one of `trusted`.
pub fn note_vouches(note: &VouchNote, did: &str, trusted: &[String]) -> bool {
    note.did == did && verify_note(&core_note(note), trusted)
}

/// The note stored beside the log `scid`.
pub(crate) fn stored_note(conn: &Connection, scid: &str) -> Result<Option<VouchNote>, ApiError> {
    let text: Option<String> = conn
        .query_row(
            "SELECT note FROM notes WHERE scid = ?1",
            params![scid],
            |r| r.get(0),
        )
        .optional()?;
    text.map(|t| {
        serde_json::from_str(&t).map_err(|e| ApiError::internal(format!("stored note: {e}")))
    })
    .transpose()
}

/// Stores `note` beside `scid`: `409 already-vouched` when the log has one.
pub(crate) fn put_note(
    conn: &Connection,
    scid: &str,
    note: &VouchNote,
    now: u64,
) -> Result<(), ApiError> {
    let text = serde_json::to_string(note).map_err(|e| ApiError::internal(e.to_string()))?;
    let inserted = conn.execute(
        "INSERT OR IGNORE INTO notes (scid, note, stored_at) VALUES (?1, ?2, ?3)",
        params![scid, text, to_sql(now)?],
    )?;
    if inserted == 0 {
        return Err(already_vouched());
    }
    Ok(())
}

fn already_vouched() -> ApiError {
    ApiError::conflict_with("already-vouched", json!({}))
}

impl Store {
    /// Redeems `token` for `did` at the vouching service (see the module's mapping).
    pub async fn redeem(&self, token: &str, did: &str) -> Result<VouchNote, ApiError> {
        let Some(base) = self.settings.vouch_url.as_deref() else {
            return Err(ApiError::unavailable("vouch-unavailable"));
        };
        let url = format!("{}/api/vouch/redeem", base.trim_end_matches('/'));
        let mut request = self
            .http
            .post(&url)
            .json(&json!({ "token": token, "did": did }));
        if let Some(credential) = &self.settings.vouch_credential {
            request = request.bearer_auth(&credential.0);
        }
        let response = request
            .send()
            .await
            .map_err(|e| unavailable(format!("redeem at {url}: {e}")))?;
        match response.status() {
            StatusCode::OK => {
                let note: VouchNote = response
                    .json()
                    .await
                    .map_err(|e| unavailable(format!("redeem answered no note: {e}")))?;
                if !note_vouches(&note, did, &self.settings.vouch_keys) {
                    return Err(unavailable(format!(
                        "redeem answered a note for {} by {} that VOUCH_KEYS does not pass",
                        note.did, note.voucher
                    )));
                }
                Ok(note)
            }
            StatusCode::CONFLICT | StatusCode::GONE => Err(refused()),
            other => Err(unavailable(format!("redeem answered {other}"))),
        }
    }

    /// `POST /v1/notes/{scid} {token}`: a signed request by a non-bot device key valid at the head
    /// or the current master key, then the redeem, then the note stored. `409 already-vouched`
    /// when the log has a note (checked before the redeem, so no token is spent on it, and again
    /// when it is stored).
    pub async fn attach_note(
        &self,
        scid: &str,
        incoming: &Incoming<'_>,
    ) -> Result<VouchNote, ApiError> {
        let did = did_of(scid);
        let signer = self
            .verify_request(incoming, Some(&did), Signer::AnyKey)?
            .key_id;
        let token = note_token(incoming.body)?;
        let vouched = {
            let conn = self.db.lock()?;
            stored_note(&conn, scid)?.is_some()
        };
        if vouched {
            return Err(already_vouched());
        }
        let note = self.redeem(&token, &did).await?;
        let now = self.now();
        {
            let mut conn = self.db.lock()?;
            let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
            // The redeem took up to its timeout: a revocation or a rotation may have retired
            // the requesting key since it was verified. It is checked again here, in the
            // transaction that takes the identity's one note slot.
            if !self.speaks_for(&tx, scid, &signer, now)? {
                return Err(ApiError::forbidden(
                    "vouch",
                    "the requesting key no longer speaks for the identity",
                ));
            }
            put_note(&tx, scid, &note, now)?;
            tx.commit()?;
        }
        Ok(note)
    }
}

impl Store {
    /// True when the key `key_id` is valid at the stored head of `scid` and the identity is not
    /// frozen: what [`Store::verify_request`] checked of the key, read again on `conn`.
    fn speaks_for(
        &self,
        conn: &Connection,
        scid: &str,
        key_id: &str,
        now: u64,
    ) -> Result<bool, ApiError> {
        let Some(chain) = self.held_chain(conn, scid)? else {
            return Ok(false);
        };
        Ok(crate::db::freezing_fork(conn, scid, &chain)?.is_none()
            && chain
                .state()
                .key_valid_at_time(key_id, chain.head().seq, now))
    }
}

/// The `token` of a `{token}` body (`400 malformed` for any other shape).
fn note_token(body: &[u8]) -> Result<String, ApiError> {
    let value: serde_json::Value =
        serde_json::from_slice(body).map_err(|e| ApiError::invalid("malformed", e.to_string()))?;
    match value.as_object() {
        Some(o) if o.len() == 1 => match o.get("token").and_then(|t| t.as_str()) {
            Some(token) => Ok(token.to_string()),
            None => Err(ApiError::invalid("malformed", "the body is {token}")),
        },
        _ => Err(ApiError::invalid("malformed", "the body is {token}")),
    }
}

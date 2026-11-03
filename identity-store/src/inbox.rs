//! The device inbox: sealed messages for a device before it joins (a pre-join address) and
//! after (its key id in the log). The store keeps only the ciphertext and who wrote it.
//!
//! Who may write and read:
//! - **Pre-join** `pre:<a>`: written by the current master key that opened an `open` offer
//!   bound to the device string's `sign`, with `pre_address(sign, r) == a` (`403 writer`
//!   otherwise); at most 2 messages; each lives until the offer's `expires`. Read and
//!   acknowledged with a pre-join read (`X-Coasys-Inbox`) by `sign` for this store.
//! - **Joined** `<scid>/<keyId>`: written by the current master key of `scid` (`403 writer`
//!   otherwise) to a device key valid at the head without the bot mark (`404` otherwise); at
//!   most 16 messages; each lives 30 days. Read and acknowledged by a signed request of `keyId`.
//!
//! Listing never deletes; an acknowledgement does. Messages go, in the transaction that ends
//! them, when their offer leaves `open`, when the receiving key is revoked, and when a
//! `rotate` replaces the master key that wrote them.

use ad4m_identity_core::inbox::{
    check_sealed, pre_address, verify_pre_auth, InboxAddress, PreInboxAuth, JOINED_TTL_S,
    MAX_JOINED_MESSAGES, MAX_PRE_MESSAGES, MAX_SEALED_BYTES, PRE_AUTH_MAX_AHEAD_S,
};
use ad4m_identity_core::KeyState;
use ad4m_ldk::identity::{InboxMessage, InboxWrite, InboxWriter};
use rusqlite::{params, Connection, TransactionBehavior};
use serde_json::{json, Value};

use crate::db::{self, from_sql, to_sql};
use crate::error::ApiError;
use crate::request::{decode_json_header, percent_decode, take_nonce, Incoming, Signer};
use crate::{limits, offers, Store};

/// The header of a pre-join read: base64url(JCS(PreInboxAuth)).
pub const PRE_INBOX_HEADER: &str = "x-coasys-inbox";

fn writer_refused(detail: impl Into<String>) -> ApiError {
    ApiError::forbidden("writer", detail)
}

fn reader_refused(detail: impl Into<String>) -> ApiError {
    ApiError::forbidden("signer", detail)
}

/// The sealed text as the store keeps it: `413` over 8 KiB, `400` when not base64url.
fn check_write(write: &InboxWrite) -> Result<(), ApiError> {
    if write.sealed.len() > MAX_SEALED_BYTES {
        return Err(ApiError::too_large());
    }
    check_sealed(&write.sealed).map_err(|e| ApiError::invalid(e.name(), e.to_string()))
}

/// `429 {scope:"identity"}` when `address` already holds `max` live messages; the retry is
/// when the oldest of them expires.
fn check_room(conn: &Connection, address: &str, max: usize, now: u64) -> Result<(), ApiError> {
    let (count, first): (i64, Option<i64>) = conn.query_row(
        "SELECT COUNT(*), MIN(expires) FROM inbox WHERE address = ?1 AND expires > ?2",
        params![address, to_sql(now)?],
        |r| Ok((r.get(0)?, r.get(1)?)),
    )?;
    if count as usize >= max {
        let retry = first.map(|e| from_sql(e).saturating_sub(now)).unwrap_or(60);
        return Err(ApiError::rate_limited("identity", retry.max(1)));
    }
    Ok(())
}

/// The joined address text of `key_id` in `scid` (`<scid>/<keyId>`), as the core writes it.
pub fn joined_address(scid: &str, key_id: &str) -> String {
    InboxAddress::Joined {
        scid: scid.to_string(),
        key_id: key_id.to_string(),
    }
    .text()
}

/// The pre-join address text of `a` (`pre:<a>`), as the core writes it.
pub fn pre_text(a: &str) -> String {
    InboxAddress::Pre(a.to_string()).text()
}

/// Deletes the pre-join messages of `scid` whose offer is no longer `open`.
pub(crate) fn drop_closed_offer_messages(conn: &Connection, scid: &str) -> Result<(), ApiError> {
    conn.execute(
        "DELETE FROM inbox WHERE scid = ?1 AND offer_id IS NOT NULL AND offer_id NOT IN
           (SELECT offer_id FROM offers WHERE scid = ?1 AND state = 'open')",
        params![scid],
    )?;
    Ok(())
}

/// After an append: deletes the joined inboxes and the requests of every key the log no
/// longer holds valid at `head_seq`, and every joined message written by a master key that is
/// no longer the current one.
pub(crate) fn purge_ended(
    conn: &Connection,
    state: &KeyState,
    head_seq: u64,
) -> Result<(), ApiError> {
    let scid = &state.scid;
    for record in state.keys.values() {
        if !state.key_valid_at(&record.id, head_seq) {
            conn.execute(
                "DELETE FROM inbox WHERE scid = ?1 AND key_id = ?2",
                params![scid, record.id],
            )?;
            conn.execute(
                "DELETE FROM requests WHERE scid = ?1 AND by_key = ?2",
                params![scid, record.id],
            )?;
        }
    }
    let current = state.current_master().map(|m| m.id.as_str());
    conn.execute(
        "DELETE FROM inbox WHERE scid = ?1 AND key_id IS NOT NULL AND writer_key IS NOT ?2",
        params![scid, current],
    )?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn insert(
    conn: &Connection,
    address: &str,
    scid: &str,
    key_id: Option<&str>,
    offer_id: Option<&str>,
    writer_key: &str,
    sealed: &str,
    now: u64,
    expires: u64,
) -> Result<Value, ApiError> {
    let id = db::new_id();
    conn.execute(
        "INSERT INTO inbox (id, address, scid, key_id, offer_id, writer_key, sealed, stored_at, expires)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
        params![
            id,
            address,
            scid,
            key_id,
            offer_id,
            writer_key,
            sealed,
            to_sql(now)?,
            to_sql(expires)?
        ],
    )?;
    Ok(json!({ "id": id, "expires": expires }))
}

impl Store {
    /// `PUT /v1/inbox/pre/{address}`: the holder that scanned a device string sends the device
    /// a message before it joins (an offer ticket). The write rule is checked in the write's
    /// transaction, so an offer that leaves `open` meanwhile takes no message.
    pub fn inbox_put_pre(
        &self,
        address: &str,
        write: &InboxWrite,
        incoming: &Incoming<'_>,
    ) -> Result<Value, ApiError> {
        check_write(write)?;
        let bind = write.bind.as_ref().ok_or_else(|| {
            ApiError::invalid("malformed", "a pre-join write names its offer in bind")
        })?;
        let did = ad4m_identity_core::did_of(&bind.scid);
        let verified = self.verify_request(incoming, Some(&did), Signer::AnyKey)?;
        if pre_address(&bind.sign, &bind.r).ok().as_deref() != Some(address) {
            return Err(writer_refused("the address is not the device string's"));
        }
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let offer = offers::open_offer_at(&tx, &bind.scid, &bind.offer_id, now)?
            .ok_or_else(|| writer_refused("no open offer of that id"))?;
        if offer.offer.device_key.as_deref() != Some(bind.sign.as_str()) {
            return Err(writer_refused(
                "the offer is not bound to the device string's key",
            ));
        }
        let chain = db::load_chain(&tx, &bind.scid)?
            .ok_or_else(|| writer_refused("no log of the identity"))?;
        let current = chain.state().current_master().map(|m| &m.id);
        if current != Some(&verified.key_id) || offer.by.key_id != verified.key_id {
            return Err(writer_refused(
                "only the current master key that opened the offer writes",
            ));
        }
        let text = pre_text(address);
        check_room(&tx, &text, MAX_PRE_MESSAGES, now)?;
        limits::take_identity_write(&tx, &self.settings, &bind.scid, now)?;
        let stored = insert(
            &tx,
            &text,
            &bind.scid,
            None,
            Some(&bind.offer_id),
            &verified.key_id,
            &write.sealed,
            now,
            offer.offer.expires,
        )?;
        tx.commit()?;
        drop(conn);
        self.changed();
        Ok(stored)
    }

    /// `PUT /v1/inbox/{scid}/{keyId}`: the current master key sends a joined device a message.
    pub fn inbox_put_joined(
        &self,
        scid: &str,
        key_id: &str,
        write: &InboxWrite,
        incoming: &Incoming<'_>,
    ) -> Result<Value, ApiError> {
        check_write(write)?;
        if write.bind.is_some() {
            return Err(ApiError::invalid(
                "malformed",
                "only a pre-join write carries bind",
            ));
        }
        let did = ad4m_identity_core::did_of(scid);
        let verified = self.verify_request(incoming, Some(&did), Signer::AnyKey)?;
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let chain = db::load_chain(&tx, scid)?
            .ok_or_else(|| ApiError::not_found("no log of the identity"))?;
        let state = chain.state();
        if state.current_master().map(|m| &m.id) != Some(&verified.key_id) {
            return Err(writer_refused("only the current master key writes"));
        }
        let receiver = state
            .keys
            .get(key_id)
            .filter(|r| r.scopes.is_empty() && state.key_valid_at(&r.id, chain.head().seq));
        if receiver.is_none() {
            return Err(ApiError::not_found(
                "no device key of that id valid at the head without the bot mark",
            ));
        }
        let address = joined_address(scid, key_id);
        check_room(&tx, &address, MAX_JOINED_MESSAGES, now)?;
        limits::take_identity_write(&tx, &self.settings, scid, now)?;
        let stored = insert(
            &tx,
            &address,
            scid,
            Some(key_id),
            None,
            &verified.key_id,
            &write.sealed,
            now,
            now.saturating_add(JOINED_TTL_S),
        )?;
        tx.commit()?;
        drop(conn);
        self.changed();
        Ok(stored)
    }

    /// Checks the reader of the joined inbox `<scid>/<keyId>`: a signed request by that key.
    pub fn check_joined_reader(
        &self,
        scid: &str,
        key_id: &str,
        incoming: &Incoming<'_>,
    ) -> Result<(), ApiError> {
        let did = ad4m_identity_core::did_of(scid);
        let verified = self.verify_request(incoming, Some(&did), Signer::AnyKey)?;
        if verified.key_id != key_id || !verified.chain.state().keys.contains_key(key_id) {
            return Err(reader_refused("only the inbox's own device key reads it"));
        }
        Ok(())
    }

    /// Checks the reader of the pre-join inbox `pre:<address>`: header `X-Coasys-Inbox` signed
    /// by the device string's key, for this store (`403 store` otherwise, as a signed request),
    /// address, method and path, not expired, with a fresh nonce.
    pub fn check_pre_reader(
        &self,
        address: &str,
        header: Option<&str>,
        method: &str,
        path: &str,
    ) -> Result<(), ApiError> {
        let header =
            header.ok_or_else(|| reader_refused("a pre-join read carries X-Coasys-Inbox"))?;
        let auth: PreInboxAuth = decode_json_header(header, "X-Coasys-Inbox")?;
        self.check_store(&auth.store)?;
        if auth.address != address {
            return Err(reader_refused("the read is signed for another address"));
        }
        verify_pre_auth(&auth).map_err(|e| reader_refused(e.to_string()))?;
        if !auth.method.eq_ignore_ascii_case(method)
            || percent_decode(path).as_deref() != Some(auth.path.as_str())
        {
            return Err(reader_refused(
                "the read is signed for another method or path",
            ));
        }
        let now = self.now();
        if auth.expires <= now || auth.expires > now.saturating_add(PRE_AUTH_MAX_AHEAD_S) {
            return Err(reader_refused(
                "the read has expired or expires more than 5 minutes ahead",
            ));
        }
        let mut conn = self.db.lock()?;
        take_nonce(&mut conn, &auth.nonce, now)
    }

    /// The live messages at `address` (`pre:<a>` or `<scid>/<keyId>`), oldest first.
    pub fn inbox_list(&self, address: &str) -> Result<Vec<InboxMessage>, ApiError> {
        let now = to_sql(self.now())?;
        let conn = self.db.lock()?;
        let mut stmt = conn.prepare_cached(
            "SELECT id, sealed, scid, key_id, offer_id, writer_key, stored_at, expires FROM inbox
             WHERE address = ?1 AND expires > ?2 ORDER BY stored_at, rowid",
        )?;
        let rows = stmt.query_map(params![address, now], |r| {
            let scid: String = r.get(2)?;
            let key_id: Option<String> = r.get(3)?;
            let offer_id: Option<String> = r.get(4)?;
            let writer_key: String = r.get(5)?;
            let writer = match (key_id, offer_id) {
                (_, Some(offer_id)) => InboxWriter::Offer { scid, offer_id },
                _ => InboxWriter::Master {
                    scid,
                    key_id: writer_key,
                },
            };
            Ok(InboxMessage {
                id: r.get(0)?,
                sealed: r.get(1)?,
                writer,
                stored_at: from_sql(r.get(6)?),
                expires: from_sql(r.get(7)?),
            })
        })?;
        Ok(rows.collect::<Result<_, _>>()?)
    }

    /// Acknowledges message `id` at `address`: it goes. `404` when it is not there.
    pub fn inbox_ack(&self, address: &str, id: &str) -> Result<(), ApiError> {
        let now = to_sql(self.now())?;
        let conn = self.db.lock()?;
        let gone = conn.execute(
            "DELETE FROM inbox WHERE address = ?1 AND id = ?2 AND expires > ?3",
            params![address, id, now],
        )?;
        drop(conn);
        if gone == 0 {
            return Err(ApiError::not_found("no such message"));
        }
        self.changed();
        Ok(())
    }
}

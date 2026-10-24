//! Offers and claims: the offer routes, the claim route, the device's state poll,
//! and the store's side of the claim check (a `Claimed` offer, spent in the append's transaction).
//!
//! An offer's time-based states are applied when the store next touches the identity's
//! offers: an `open` offer past `expires`, and a `claimed` one past `expires + 300 s`, become
//! `expired`. A terminal offer stays 24 hours as a tombstone, so the device can read why it ended.

use ad4m_identity_core::link::{
    check_add, check_secret, claim_view, verify_claim, verify_offer, Claim, ClaimView, LinkError,
    OfferKind, OfferState, SignedOffer, CONFIRM_GRACE_S, MAX_OPEN_OFFERS, MAX_WRONG_SECRETS,
};
use ad4m_identity_core::{Event, KeyState};
use rusqlite::{params, Connection, OptionalExtension, TransactionBehavior};
use serde_json::{json, Value};

use crate::db::{self, from_sql, to_sql};
use crate::error::ApiError;
use crate::Store;

/// How long a terminal offer stays readable.
pub const TOMBSTONE_S: u64 = 86_400;

struct OfferRow {
    signed: SignedOffer,
    state: String,
    wrong_secrets: u32,
    spent_seq: Option<u64>,
}

fn kind_name(kind: OfferKind) -> &'static str {
    match kind {
        OfferKind::Device => "device",
        OfferKind::Node => "node",
        OfferKind::Bot => "bot",
    }
}

fn parse<T: serde::de::DeserializeOwned>(text: &str) -> Result<T, ApiError> {
    serde_json::from_str(text).map_err(|e| ApiError::internal(format!("stored offer: {e}")))
}

fn to_json<T: serde::Serialize>(value: &T) -> Result<String, ApiError> {
    serde_json::to_string(value).map_err(|e| ApiError::internal(e.to_string()))
}

/// Applies the time-based states of `scid`'s offers and drops tombstones older than 24 hours.
fn refresh(conn: &Connection, scid: &str, now: u64) -> Result<bool, ApiError> {
    let now_sql = to_sql(now)?;
    let expired = conn.execute(
        "UPDATE offers SET state = 'expired', ended_at = ?2
         WHERE scid = ?1 AND ((state = 'open' AND expires <= ?2)
                              OR (state = 'claimed' AND expires + ?3 <= ?2))",
        params![scid, now_sql, to_sql(CONFIRM_GRACE_S)?],
    )?;
    let gone = to_sql(now.saturating_sub(TOMBSTONE_S))?;
    conn.execute(
        "DELETE FROM claims WHERE scid = ?1 AND offer_id IN
           (SELECT offer_id FROM offers WHERE scid = ?1 AND ended_at IS NOT NULL AND ended_at <= ?2)",
        params![scid, gone],
    )?;
    conn.execute(
        "DELETE FROM offers WHERE scid = ?1 AND ended_at IS NOT NULL AND ended_at <= ?2",
        params![scid, gone],
    )?;
    Ok(expired > 0)
}

fn read_offer(conn: &Connection, scid: &str, offer_id: &str) -> Result<Option<OfferRow>, ApiError> {
    let row: Option<(String, String, i64, Option<i64>)> = conn
        .query_row(
            "SELECT signed, state, wrong_secrets, spent_seq FROM offers WHERE scid = ?1 AND offer_id = ?2",
            params![scid, offer_id],
            |r| Ok((r.get(0)?, r.get(1)?, r.get(2)?, r.get(3)?)),
        )
        .optional()?;
    row.map(|(signed, state, wrong, spent)| {
        Ok(OfferRow {
            signed: parse(&signed)?,
            state,
            wrong_secrets: from_sql(wrong) as u32,
            spent_seq: spent.map(from_sql),
        })
    })
    .transpose()
}

fn read_claim(
    conn: &Connection,
    scid: &str,
    offer_id: &str,
) -> Result<Option<ClaimView>, ApiError> {
    let text: Option<String> = conn
        .query_row(
            "SELECT claim FROM claims WHERE scid = ?1 AND offer_id = ?2",
            params![scid, offer_id],
            |r| r.get(0),
        )
        .optional()?;
    text.map(|t| parse(&t)).transpose()
}

fn end(
    conn: &Connection,
    scid: &str,
    offer_id: &str,
    state: &str,
    now: u64,
) -> Result<(), ApiError> {
    conn.execute(
        "UPDATE offers SET state = ?3, ended_at = ?4 WHERE scid = ?1 AND offer_id = ?2",
        params![scid, offer_id, state, to_sql(now)?],
    )?;
    Ok(())
}

fn offer_state(conn: &Connection, scid: &str, row: &OfferRow) -> Result<OfferState, ApiError> {
    Ok(match row.state.as_str() {
        "open" => OfferState::Open,
        "claimed" => OfferState::Claimed {
            claim: read_claim(conn, scid, &row.signed.offer.offer_id)?
                .ok_or_else(|| ApiError::internal("a claimed offer without its claim"))?,
        },
        "spent" => OfferState::Spent {
            seq: row.spent_seq.unwrap_or(0),
        },
        "expired" => OfferState::Expired,
        "cancelled" => OfferState::Cancelled,
        "contested" => OfferState::Contested,
        "locked" => OfferState::Locked,
        "superseded" => OfferState::Superseded,
        other => return Err(ApiError::internal(format!("unknown offer state {other}"))),
    })
}

/// The claim check, conditions 1–5 (`identity-store/AGENTS.md`): the `add` names a `Claimed`
/// offer of this SCID, and the core's `check_add` passes against the state before the event.
/// Any failure is `403 {error:"claim"}`.
pub(crate) fn check_spend(
    conn: &Connection,
    scid: &str,
    offer_id: &str,
    before: &KeyState,
    event: &Event,
    now: u64,
) -> Result<(), ApiError> {
    refresh(conn, scid, now)?;
    let refuse = |detail: &str| ApiError::forbidden("claim", detail.to_string());
    let row = read_offer(conn, scid, offer_id)?
        .ok_or_else(|| refuse("the add names no offer of this identity"))?;
    if row.state != "claimed" {
        return Err(refuse(&format!("the offer is {}, not claimed", row.state)));
    }
    let claim =
        read_claim(conn, scid, offer_id)?.ok_or_else(|| refuse("the offer holds no claim"))?;
    check_add(&row.signed, &claim, before, event, now).map_err(|e| refuse(&e.to_string()))
}

/// The claim check, condition 6: the offer becomes `Spent { seq }` and the claim's row goes.
pub(crate) fn spend(
    conn: &Connection,
    scid: &str,
    offer_id: &str,
    seq: u64,
    now: u64,
) -> Result<(), ApiError> {
    conn.execute(
        "UPDATE offers SET state = 'spent', spent_seq = ?3, ended_at = ?4
         WHERE scid = ?1 AND offer_id = ?2 AND state = 'claimed'",
        params![scid, offer_id, to_sql(seq)?, to_sql(now)?],
    )?;
    conn.execute(
        "DELETE FROM claims WHERE scid = ?1 AND offer_id = ?2",
        params![scid, offer_id],
    )?;
    Ok(())
}

/// A `rotate` supersedes every open and claimed offer of the identity: an offer signed by the
/// old master no longer speaks for it.
pub(crate) fn supersede(conn: &Connection, scid: &str, now: u64) -> Result<(), ApiError> {
    conn.execute(
        "UPDATE offers SET state = 'superseded', ended_at = ?2 WHERE scid = ?1 AND state IN ('open', 'claimed')",
        params![scid, to_sql(now)?],
    )?;
    Ok(())
}

fn receipt(offer: &SignedOffer) -> Value {
    json!({ "state": "claimed", "kind": kind_name(offer.offer.kind), "expires": offer.offer.expires })
}

/// What the claim route answers.
pub enum ClaimAnswer {
    /// `201`: the offer moved to `claimed`.
    Claimed(Value),
    /// `200`: a byte-for-byte repeat of the stored claim.
    Repeat(Value),
}

impl Store {
    /// `POST /v1/offers/{scid}`: checks the offer against the log and stores it `open`.
    pub fn open_offer(&self, scid: &str, signed: &SignedOffer) -> Result<Value, ApiError> {
        if signed.offer.scid != scid {
            return Err(ApiError::invalid(
                "wrong-chain",
                "the offer names another identity",
            ));
        }
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let chain = db::load_chain(&tx, scid)?
            .ok_or_else(|| ApiError::not_found(format!("no log {scid}")))?;
        match verify_offer(signed, &chain, now) {
            Ok(()) => {}
            Err(LinkError::Moved) => {
                let head = db::signed_head(&tx, scid)?
                    .ok_or_else(|| ApiError::internal("log without a head"))?;
                return Err(ApiError::conflict(&head));
            }
            Err(e @ (LinkError::Signer | LinkError::Signature)) => {
                return Err(ApiError::forbidden("signer", e.to_string()))
            }
            Err(e) => return Err(ApiError::invalid(e.name(), e.to_string())),
        }
        refresh(&tx, scid, now)?;
        let open: i64 = tx.query_row(
            "SELECT COUNT(*) FROM offers WHERE scid = ?1 AND state IN ('open', 'claimed')",
            params![scid],
            |r| r.get(0),
        )?;
        if open as usize >= MAX_OPEN_OFFERS {
            let next_end: Option<i64> = tx.query_row(
                "SELECT MIN(expires) FROM offers WHERE scid = ?1 AND state = 'open'",
                params![scid],
                |r| r.get(0),
            )?;
            let retry = next_end
                .map(|e| from_sql(e).saturating_sub(now).max(1))
                .unwrap_or(60);
            return Err(ApiError::rate_limited("identity", retry));
        }
        let offer = &signed.offer;
        let inserted = tx.execute(
            "INSERT OR IGNORE INTO offers (scid, offer_id, signed, kind, commit_hash, bound_key, expires, state, created_at)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, 'open', ?8)",
            params![
                scid,
                offer.offer_id,
                to_json(signed)?,
                kind_name(offer.kind),
                offer.commit,
                offer.device_key,
                to_sql(offer.expires)?,
                to_sql(now)?
            ],
        )?;
        if inserted == 0 {
            return Err(ApiError::conflict_with(
                "offer-exists",
                json!({ "offerId": offer.offer_id }),
            ));
        }
        tx.commit()?;
        drop(conn);
        self.changed();
        Ok(json!({ "offerId": offer.offer_id }))
    }

    /// `GET /v1/offers/{scid}`: the offers in `open` or `claimed`, with their states.
    pub fn list_offers(&self, scid: &str) -> Result<Vec<Value>, ApiError> {
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let expired = refresh(&tx, scid, now)?;
        let ids: Vec<String> = {
            let mut stmt = tx.prepare_cached(
                "SELECT offer_id FROM offers WHERE scid = ?1 AND state IN ('open', 'claimed') ORDER BY created_at, offer_id",
            )?;
            let rows = stmt.query_map(params![scid], |r| r.get(0))?;
            rows.collect::<Result<_, _>>()?
        };
        let mut out = Vec::with_capacity(ids.len());
        for id in ids {
            let row =
                read_offer(&tx, scid, &id)?.ok_or_else(|| ApiError::internal("offer vanished"))?;
            let state = offer_state(&tx, scid, &row)?;
            out.push(json!({ "offer": row.signed, "state": state }));
        }
        tx.commit()?;
        drop(conn);
        if expired {
            self.changed();
        }
        Ok(out)
    }

    /// `DELETE /v1/offers/{scid}/{offerId}`: an open or claimed offer becomes `cancelled`.
    pub fn cancel_offer(&self, scid: &str, offer_id: &str) -> Result<(), ApiError> {
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        refresh(&tx, scid, now)?;
        let row =
            read_offer(&tx, scid, offer_id)?.ok_or_else(|| ApiError::not_found("no such offer"))?;
        if row.state != "open" && row.state != "claimed" {
            return Err(ApiError::not_found(format!("the offer is {}", row.state)));
        }
        end(&tx, scid, offer_id, "cancelled", now)?;
        tx.commit()?;
        drop(conn);
        self.changed();
        Ok(())
    }

    /// `POST /v1/offers/{scid}/{offerId}/claim`. Checks, in order: the offer exists and is
    /// live; a byte-for-byte repeat answers `200`; the secret (wrong ones count toward
    /// `locked`); the bound key; the co-signature; the key in no log; then an unbound offer
    /// that already holds another key's claim becomes `contested`.
    pub fn claim_offer(
        &self,
        scid: &str,
        offer_id: &str,
        claim: &Claim,
    ) -> Result<ClaimAnswer, ApiError> {
        if claim.offer_id != offer_id {
            return Err(ApiError::invalid(
                "malformed",
                "the claim names another offer",
            ));
        }
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let expired = refresh(&tx, scid, now)?;
        let Some(row) = read_offer(&tx, scid, offer_id)? else {
            // Commit what `refresh` did (expired offers, dropped tombstones).
            tx.commit()?;
            drop(conn);
            if expired {
                self.changed();
            }
            return Err(ApiError::not_found("no such offer"));
        };
        if row.state != "open" && row.state != "claimed" {
            tx.commit()?;
            drop(conn);
            if expired {
                self.changed();
            }
            return Err(ApiError::gone(&row.state));
        }
        let stored = read_claim(&tx, scid, offer_id)?;
        if let Some(stored) = &stored {
            if *stored == claim_view(claim, stored.claimed_at)
                && check_secret(&row.signed.offer, claim)
            {
                return Ok(ClaimAnswer::Repeat(receipt(&row.signed)));
            }
        }
        let wrong =
            |tx: rusqlite::Transaction<'_>, error: ApiError| -> Result<ClaimAnswer, ApiError> {
                let count = row.wrong_secrets + 1;
                tx.execute(
                    "UPDATE offers SET wrong_secrets = ?3 WHERE scid = ?1 AND offer_id = ?2",
                    params![scid, offer_id, count],
                )?;
                if count >= MAX_WRONG_SECRETS {
                    end(&tx, scid, offer_id, "locked", now)?;
                }
                tx.commit()?;
                Err(error)
            };
        if !check_secret(&row.signed.offer, claim) {
            let result = wrong(
                tx,
                ApiError::forbidden("secret", "the secret does not match the offer"),
            );
            drop(conn);
            self.changed();
            return result;
        }
        if let Some(bound) = &row.signed.offer.device_key {
            if bound != &claim.device_key {
                let result = wrong(
                    tx,
                    ApiError::forbidden("bound", "the offer is bound to another key"),
                );
                drop(conn);
                self.changed();
                return result;
            }
        }
        match verify_claim(scid, claim) {
            Ok(()) => {}
            Err(LinkError::CoSig) => {
                return Err(ApiError::forbidden(
                    "cosig",
                    "the co-signature does not verify",
                ))
            }
            Err(e) => return Err(ApiError::invalid(e.name(), e.to_string())),
        }
        if db::key_owner(&tx, &claim.device_key)?.is_some() {
            return Err(ApiError::key_taken(&claim.device_key));
        }
        if let Some(stored) = stored {
            if stored.device_key == claim.device_key {
                return Err(ApiError::conflict_with(
                    "claimed",
                    json!({ "detail": "this key already claimed the offer with another claim" }),
                ));
            }
            end(&tx, scid, offer_id, "contested", now)?;
            tx.commit()?;
            drop(conn);
            self.changed();
            return Err(ApiError::gone("contested"));
        }
        tx.execute(
            "INSERT INTO claims (scid, offer_id, claim, device_key, claimed_at) VALUES (?1, ?2, ?3, ?4, ?5)",
            params![scid, offer_id, to_json(&claim_view(claim, now))?, claim.device_key, to_sql(now)?],
        )?;
        tx.execute(
            "UPDATE offers SET state = 'claimed' WHERE scid = ?1 AND offer_id = ?2",
            params![scid, offer_id],
        )?;
        tx.commit()?;
        drop(conn);
        self.changed();
        Ok(ClaimAnswer::Claimed(receipt(&row.signed)))
    }

    /// `GET /v1/offers/{scid}/{offerId}/state?key=`: the outcome for the claiming key only
    /// (`None`, the route's `404`, for any other key).
    pub fn offer_progress(
        &self,
        scid: &str,
        offer_id: &str,
        key: &str,
    ) -> Result<Option<Value>, ApiError> {
        let now = self.now();
        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let expired = refresh(&tx, scid, now)?;
        let answer = (|| {
            let Some(row) = read_offer(&tx, scid, offer_id)? else {
                return Ok(None);
            };
            let claimant = match read_claim(&tx, scid, offer_id)? {
                Some(claim) => Some(claim.device_key),
                None if row.state == "spent" => {
                    let added: Option<(String, i64)> = tx
                        .query_row(
                            "SELECT scid, added FROM keys WHERE sign = ?1",
                            params![key],
                            |r| Ok((r.get(0)?, r.get(1)?)),
                        )
                        .optional()?;
                    added
                        .filter(|(owner, at)| owner == scid && Some(from_sql(*at)) == row.spent_seq)
                        .map(|_| key.to_string())
                }
                None => row.signed.offer.device_key.clone(),
            };
            if claimant.as_deref() != Some(key) {
                return Ok(None);
            }
            let mut out = json!({ "state": row.state });
            if let Some(seq) = row.spent_seq {
                out["seq"] = json!(seq);
            }
            Ok::<_, ApiError>(Some(out))
        })()?;
        tx.commit()?;
        drop(conn);
        if expired {
            self.changed();
        }
        Ok(answer)
    }
}

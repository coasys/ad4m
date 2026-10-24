//! Logs: the append with admission rules A1–A7 (AR-01 4.5.4), log pages, heads and the key index.

use ad4m_hash_chain::store::{SqliteChainStore, StoreError, StoredEntry};
use ad4m_hash_chain::Head;
use ad4m_identity_core::{
    error_code, pow, ChainError, DeviceKey, Event, FoldError, KelChain, KelPolicy, KeyLife, Op,
};
use ad4m_ldk::identity::{AppendRequest, KeyIndexEntry, LogPage, SignedHead};
use rusqlite::{params, Connection, TransactionBehavior};

use crate::db::{self, to_sql, KEL};
use crate::error::ApiError;
use crate::offers;
use crate::{Fault, Store};

/// Events per append.
pub const MAX_EVENTS: usize = 32;
/// Events per log page.
pub const PAGE_EVENTS: u32 = 256;
/// The furthest ahead an `expires` in an `incept` or `add` may lie (AR-02 4.3.4).
pub const MAX_KEY_LIFE_S: u64 = 400 * 86_400;

fn chain_error(e: &ChainError<FoldError>) -> ApiError {
    let code = error_code(e);
    let name = code
        .get("reason")
        .filter(|_| code["code"] == "policy")
        .or_else(|| code.get("code"))
        .and_then(|v| v.as_str())
        .unwrap_or("malformed")
        .to_string();
    ApiError::invalid(&name, e.to_string())
}

/// How a new log with no vouching note is admitted (AR-01 4.7). Slice 1 has no brake: no
/// bucket per address, no store-wide ceiling and no required proof of work. A proof that is
/// sent must still meet `POW_BITS`, so the request format and the client's work stay those of
/// AR-01. Slice 3 adds the buckets and makes the proof required here, before any replay.
pub fn admit_new_log(
    scid: &str,
    proof: Option<&ad4m_ldk::identity::Pow>,
    bits: u8,
) -> Result<(), ApiError> {
    if let Some(proof) = proof {
        let proof = pow::Pow {
            nonce: proof.nonce.clone(),
            bits: proof.bits,
        };
        if !pow::check(scid, &proof, bits) {
            return Err(ApiError::forbidden(
                "pow",
                "the proof of work does not meet the store's bits",
            ));
        }
    }
    Ok(())
}

/// The store's own check of rule 8: an `expires` in an `incept` or `add` neither in the past
/// nor more than 400 days ahead, by the store's clock.
fn check_expires(event: &Event, now: u64) -> Result<(), ApiError> {
    let keys: &[DeviceKey] = match &event.op {
        Op::Incept { add, .. } | Op::Add { add, .. } => add,
        _ => return Ok(()),
    };
    for key in keys {
        let expires = match &key.life {
            KeyLife::Standing => continue,
            KeyLife::Until(at) => *at,
            KeyLife::Bot(scope) => scope.expires,
        };
        if expires <= now {
            return Err(ApiError::invalid(
                "expires",
                format!("{} expires in the past", key.id),
            ));
        }
        if expires > now + MAX_KEY_LIFE_S {
            return Err(ApiError::invalid(
                "expires",
                format!("{} expires more than 400 days ahead", key.id),
            ));
        }
    }
    Ok(())
}

/// The signing keys an event lists: masters and device keys.
fn listed_keys(event: &Event) -> Vec<&str> {
    match &event.op {
        Op::Incept { master, add, .. } => std::iter::once(master.as_str())
            .chain(add.iter().map(|k| k.sign.as_str()))
            .collect(),
        Op::Add { add, .. } => add.iter().map(|k| k.sign.as_str()).collect(),
        Op::Rotate { master, .. } => vec![master.as_str()],
        Op::Revoke { .. } | Op::Scope { .. } => Vec::new(),
    }
}

fn current_head(conn: &Connection, scid: &str) -> Result<SignedHead, ApiError> {
    db::signed_head(conn, scid)?
        .ok_or_else(|| ApiError::internal(format!("log {scid} has no head")))
}

impl Store {
    /// `POST /v1/logs/{scid}/events`.
    pub fn append(&self, scid: &str, req: &AppendRequest) -> Result<SignedHead, ApiError> {
        if req.events.is_empty() {
            return Err(ApiError::invalid(
                "malformed",
                "an append carries at least one event",
            ));
        }
        if req.events.len() > MAX_EVENTS {
            return Err(ApiError::invalid(
                "too-many-events",
                format!("an append carries at most {MAX_EVENTS} events"),
            ));
        }
        if req.vouch.is_some() && req.pow.is_some() {
            return Err(ApiError::invalid(
                "vouch-and-pow",
                "a new log carries a vouching token or a proof of work, not both",
            ));
        }
        let events = req
            .events
            .iter()
            .map(|v| Event::from_json(v).map_err(|e| ApiError::invalid(e.name(), e.to_string())))
            .collect::<Result<Vec<_>, _>>()?;
        let now = self.now();

        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let stored = db::load_chain(&tx, scid)?;
        let new_log = stored.is_none();
        let first = &events[0];

        // A1, A2, A3: continuity, a new log's admission, replay.
        let mut chain: KelChain;
        let mut befores = Vec::with_capacity(events.len());
        let rest: &[Event];
        match stored {
            None => {
                if first.seq != 0 {
                    return Err(ApiError::not_found(format!("no log {scid}")));
                }
                if first.scid != scid {
                    return Err(ApiError::invalid(
                        "wrong-chain",
                        format!("event #0 names {}, the route {scid}", first.scid),
                    ));
                }
                chain = KelChain::genesis(&KelPolicy, first).map_err(|e| chain_error(&e))?;
                admit_new_log(scid, req.pow.as_ref(), self.settings.pow_bits)?;
                if req.vouch.is_some() {
                    // No vouching service is configured before slice 3: nothing is committed.
                    return Err(ApiError::unavailable("vouch-unavailable"));
                }
                befores.push(None);
                rest = &events[1..];
            }
            Some(existing) => {
                if req.vouch.is_some() || req.pow.is_some() {
                    return Err(ApiError::invalid(
                        "malformed",
                        "a vouching token or a proof of work comes only with event #0",
                    ));
                }
                let head = existing.head();
                if first.seq != head.seq + 1 || first.prev.as_ref() != Some(&head.hash) {
                    return Err(ApiError::conflict(&current_head(&tx, scid)?));
                }
                chain = existing;
                rest = &events[..];
            }
        }
        for event in rest {
            let before = chain.state().clone();
            chain
                .append(&KelPolicy, event)
                .map_err(|e| chain_error(&e))?;
            befores.push(Some(before));
        }

        // Rule 8 by the store's clock, A4 one key in one identity, A7 an add spends a claim.
        let mut spends = Vec::new();
        let mut supersede = false;
        for (event, before) in events.iter().zip(&befores) {
            check_expires(event, now)?;
            for key in listed_keys(event) {
                if db::key_owner(&tx, key)?.is_some() {
                    return Err(ApiError::key_taken(key));
                }
            }
            match (&event.op, before) {
                (Op::Add { offer, .. }, Some(before)) => {
                    offers::check_spend(&tx, scid, offer, before, event, now)?;
                    spends.push((offer.clone(), event.seq));
                }
                (Op::Rotate { .. }, _) => supersede = true,
                _ => {}
            }
        }

        // A6: the events, the head, the key index, the feed and the spent claims in one transaction.
        let entries = events
            .iter()
            .map(|e| StoredEntry::from_entry(e, now))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| ApiError::invalid("malformed", e.to_string()))?;
        // A1: `expect` is the head the request continues, read from the request itself.
        let expect = match (&first.prev, first.seq) {
            (Some(prev), seq) if seq > 0 => Some(Head {
                seq: seq - 1,
                hash: prev.clone(),
            }),
            _ => None,
        };
        let head = match SqliteChainStore::append_in(
            &tx,
            KEL,
            &scid.to_string(),
            expect.as_ref(),
            &entries,
        ) {
            Ok(head) => head,
            Err(StoreError::Conflict(_)) => {
                return Err(ApiError::conflict(&current_head(&tx, scid)?))
            }
            Err(e) => return Err(e.into()),
        };
        self.fault_point(Fault::AfterEvents)?;
        if new_log {
            tx.execute(
                "INSERT INTO logs (scid, created_at, admitted_by) VALUES (?1, ?2, 'brake')",
                params![scid, to_sql(now)?],
            )?;
        }
        db::put_keys(&tx, chain.state(), head.seq)?;
        for entry in &entries {
            tx.execute(
                "INSERT INTO feed (scid, seq, hash) VALUES (?1, ?2, ?3)",
                params![scid, to_sql(entry.seq)?, entry.hash],
            )?;
        }
        for (offer, seq) in &spends {
            offers::spend(&tx, scid, offer, *seq, now)?;
        }
        if supersede {
            offers::supersede(&tx, scid, now)?;
        }
        let signed = self.key.sign_head(scid, head.seq, &head.hash, now);
        db::put_head(&tx, &signed)?;
        tx.commit()?;
        drop(conn);
        self.changed();
        Ok(signed)
    }

    /// `GET /v1/logs/{scid}?from=`: at most 256 events from `from`, with the signed head.
    pub fn log_page(&self, scid: &str, from: u64) -> Result<Option<LogPage>, ApiError> {
        let conn = self.db.lock()?;
        let Some(head) = db::signed_head(&conn, scid)? else {
            return Ok(None);
        };
        let rows = db::events_from(&conn, scid, from, PAGE_EVENTS)?;
        let (events, appended_at) = rows.into_iter().unzip();
        Ok(Some(LogPage {
            events,
            appended_at,
            head,
            // No vouching notes before slice 3: `vouched` reads false and `note` is empty.
            note: None,
            vouched: false,
            pauses: Vec::new(),
        }))
    }

    /// `GET /v1/heads/{scid}`.
    pub fn head(&self, scid: &str) -> Result<Option<SignedHead>, ApiError> {
        let conn = self.db.lock()?;
        db::signed_head(&conn, scid)
    }

    /// `GET /v1/keys/{multikey}` without waiting.
    pub fn key_entry(&self, multikey: &str) -> Result<Option<KeyIndexEntry>, ApiError> {
        let conn = self.db.lock()?;
        db::key_entry(&conn, multikey)
    }
}

//! Logs: the append with the admission checks of `identity-store/AGENTS.md`, log pages, heads
//! and the key index.

use ad4m_hash_chain::store::{SqliteChainStore, StoreError, StoredEntry};
use ad4m_hash_chain::{ForkEvidence, Head, Multikey};
use ad4m_identity_core::{
    error_code, event_hash, pow, ChainError, DeviceKey, Event, FoldError, KelChain, KelPolicy,
    KeyLife, Op,
};
use ad4m_ldk::identity::{AppendRequest, KeyIndexEntry, LogPage, SignedHead};
use rusqlite::{params, Connection, TransactionBehavior};

use crate::db::{self, to_sql, KEL};
use crate::error::ApiError;
use crate::offers;
use crate::{Fault, Store, Topic};

/// Events per append.
pub const MAX_EVENTS: usize = 32;
/// Events per log page.
pub const PAGE_EVENTS: u32 = ad4m_ldk::identity::LOG_PAGE_EVENTS;
/// The furthest ahead a bot scope's `expires` in an `incept` or `add` may lie: 400 days.
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

/// The events of an append body, each parsed from its own text with `Event::from_json_text`.
/// A refused event is `400` with the format rule's name (`DuplicateMember`, `UnknownField`, …),
/// or the `FoldError` name for any other refusal.
fn events_of(body: &[u8]) -> Result<Vec<Event>, ApiError> {
    #[derive(serde::Deserialize)]
    struct Texts<'a> {
        #[serde(borrow)]
        events: Vec<&'a serde_json::value::RawValue>,
    }
    let texts: Texts =
        serde_json::from_slice(body).map_err(|e| ApiError::invalid("malformed", e.to_string()))?;
    texts
        .events
        .iter()
        .map(|text| {
            Event::from_json_text(text.get()).map_err(|e| {
                let name = match &e {
                    FoldError::Format(format) => format.name(),
                    other => other.name(),
                };
                ApiError::invalid(name, e.to_string())
            })
        })
        .collect()
}

/// How a new log with no vouching note is admitted (section 14, host admission policy, of
/// `docs-src/identity/did-scid-coasys-1.md`). There is no brake yet: no
/// bucket per address, no store-wide ceiling and no required proof of work. A proof that is
/// sent must still meet `POW_BITS`, so the request format and the client's work stay those
/// of the format. The brake adds the buckets and makes the proof required here; it runs once
/// event #0 has replayed and before the rest of the append replays.
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

/// The store's own check of expiries (the core reads no clock): a bot scope's `expires` in an
/// `incept` or `add` neither in the past nor more than 400 days ahead, by the store's clock.
/// Only a bot key expires; the core refuses an `expires` without the bot mark on replay.
fn check_expires(event: &Event, now: u64) -> Result<(), ApiError> {
    let keys: &[DeviceKey] = match &event.op {
        Op::Incept { add, .. } | Op::Add { add, .. } => add,
        _ => return Ok(()),
    };
    for key in keys {
        let KeyLife::Bot(scope) = &key.life else {
            continue;
        };
        let expires = scope.expires;
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

/// The fork `event` makes with the stored log `stored` of `scid`, when it is one: an event at a
/// seq the store holds, with another hash than the stored event there, that replays on the
/// stored events before it (every proof checked by the identity core). Answers the evidence and
/// the master that signed the event, read from that replay: the last master of the state after
/// it (an `incept` or `rotate` is signed by the master it names, every other event by the master
/// current before it). `None` for a resend of a stored event, an event past the head (a gap)
/// and an event that does not replay.
fn competing_event(
    conn: &Connection,
    scid: &str,
    stored: &KelChain,
    event: &Event,
) -> Result<Option<(ForkEvidence, Multikey)>, ApiError> {
    let Some(ours) = stored.hash_at(event.seq) else {
        return Ok(None);
    };
    let Ok(theirs) = event_hash(event) else {
        return Ok(None);
    };
    if &theirs == ours {
        return Ok(None);
    }
    let after = if event.seq == 0 {
        if event.scid != scid {
            return Ok(None);
        }
        KelChain::genesis(&KelPolicy, event).ok()
    } else {
        match db::load_chain_before(conn, scid, event.seq)? {
            Some(mut prefix) => prefix.append(&KelPolicy, event).is_ok().then_some(prefix),
            None => None,
        }
    };
    let Some(signer) = after.and_then(|a| a.state().masters.last().map(|m| m.sign.clone())) else {
        return Ok(None);
    };
    Ok(Some((
        ForkEvidence {
            chain: scid.to_string(),
            seq: event.seq,
            ours: ours.clone(),
            theirs,
        },
        signer,
    )))
}

fn current_head(conn: &Connection, scid: &str) -> Result<SignedHead, ApiError> {
    db::signed_head(conn, scid)?
        .ok_or_else(|| ApiError::internal(format!("log {scid} has no head")))
}

impl Store {
    /// `POST /v1/logs/{scid}/events`.
    /// `body` is the text `req` was parsed from: the events are parsed again from their own
    /// text there, so an event that names a member twice is refused (`DuplicateMember`), which
    /// `req.events` (each a `Value`, holding only the last of the two) cannot show.
    pub fn append(
        &self,
        scid: &str,
        req: &AppendRequest,
        body: &[u8],
    ) -> Result<SignedHead, ApiError> {
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
        let events = events_of(body)?;
        let now = self.now();

        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        let stored = db::load_chain(&tx, scid)?;
        let new_log = stored.is_none();
        let first = &events[0];

        // The continuity check, the new-log admission and the replay check.
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
                    // No vouching service exists yet: nothing is committed.
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
                // A fork signed by the held log's current master freezes the identity. The one
                // append a frozen identity takes is a `rotate`: it is signed by the committed
                // next key, which the rival's signer alone does not hold, and once appended the
                // rival's signer is no longer current, so the freeze ends.
                if !matches!(first.op, Op::Rotate { .. }) {
                    if let Some(fork) = db::freezing_fork(&tx, scid, &existing)? {
                        return Err(ApiError::forked(&current_head(&tx, scid)?, &fork));
                    }
                }
                let head = existing.head();
                if first.seq != head.seq + 1 || first.prev.as_ref() != Some(&head.hash) {
                    // A validly signed event at a seq the store holds, other than the stored
                    // one, is a fork: the store records it (with the event, so anyone can check
                    // it) and answers `409 forked`. It freezes the identity only when its signer
                    // is the held log's current master (`db::freezing_fork`). One fork is kept
                    // per signing master: a further rival by the same master is answered with
                    // the recorded fork and adds nothing. Anything else is a plain conflict.
                    if let Some((fork, signer)) = competing_event(&tx, scid, &existing, first)? {
                        let theirs = ad4m_hash_chain::entry_json(first)
                            .map_err(|e| ApiError::invalid("malformed", e.to_string()))?;
                        let fork = db::record_fork(&tx, fork, &theirs, &signer)?;
                        let head = current_head(&tx, scid)?;
                        tx.commit()?;
                        drop(conn);
                        self.changed(&[Topic::offers(scid)]);
                        return Err(ApiError::forked(&head, &fork));
                    }
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

        // Expiries by the store's clock, the one-key check and the claim check.
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

        // The one-transaction rule: the events, the head, the key index, the feed and the spent
        // claims in one transaction.
        let entries = events
            .iter()
            .map(|e| StoredEntry::from_entry(e, now))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| ApiError::invalid("malformed", e.to_string()))?;
        // The continuity check: `expect` is the head the request continues, read from the
        // request itself.
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
        let mut topics = vec![Topic::offers(scid)];
        for event in &events {
            topics.extend(listed_keys(event).into_iter().map(Topic::key));
        }
        self.changed(&topics);
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
            // No vouching notes yet: `vouched` reads false and `note` is empty.
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

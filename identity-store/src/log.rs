//! Logs: the append with the admission checks of `identity-store/AGENTS.md`, log pages, heads
//! and the key index.

use std::sync::Arc;

use ad4m_hash_chain::store::{SqliteChainStore, StoreError, StoredEntry};
use ad4m_hash_chain::{verify_proof, ForkEvidence, Head, Multikey};
use ad4m_identity_core::{
    commitment, did_of, error_code, event_hash, pow, ChainError, ChainPolicy, DeviceKey, Event,
    FoldError, FormatError, KelChain, KelPolicy, KeyLife, Op, KEL_DOMAIN,
};
use ad4m_ldk::identity::{AppendRequest, KeyIndexEntry, LogPage, SignedHead};
use rusqlite::{params, Connection, TransactionBehavior};

use crate::db::{self, to_sql, KEL};
use crate::error::ApiError;
use crate::held::{fold_bytes_of, state_before, FoldBytes};
use crate::offers;
use crate::{Fault, Store, Topic};

/// Events per append.
pub const MAX_EVENTS: usize = 32;
/// Events per log page.
pub const PAGE_EVENTS: u32 = ad4m_ldk::identity::LOG_PAGE_EVENTS;
/// The most events a log holds (seqs 0 to 16 383): what its readers read. The executor's
/// resolver pages through a log at most 64 pages of [`PAGE_EVENTS`] (256) events deep, so an
/// event past this bound would never reach a reader. An append that would grow a log past it is
/// `400 log-full`.
pub const MAX_LOG_EVENTS: u64 = 64 * PAGE_EVENTS as u64;
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

/// The key that must sign `event` (seq ≥ 1) of the stored log `scid`, by the key index alone (no
/// replay): the master current before `event.seq`, or for a `rotate` the master it names, which
/// must be the key the stored log rotated onto there or, at the current master, the key the
/// stored next-key commitment names (its id the next master id). Only masters sign events after
/// #0 (`KelPolicy`: `add`, `revoke` and `scope` by the current master, `rotate` by the master it
/// reveals); a device key signs nothing of the log after #0. `Err` is the refusal the replay
/// would give: `Ended` past the end, `NextMismatch` for a `rotate` onto another key,
/// `InceptAfterGenesis` for an `incept`.
fn indexed_signer(
    conn: &Connection,
    scid: &str,
    event: &Event,
) -> Result<Result<(String, Multikey), FoldError>, ApiError> {
    let Some(at) = db::master_at(conn, scid, event.seq)? else {
        return Ok(Err(FoldError::Ended));
    };
    Ok(match &event.op {
        Op::Incept { .. } => Err(FormatError::InceptAfterGenesis.into()),
        Op::Rotate { master, salt, .. } => {
            let named = match at.revoked {
                Some(retired) => db::master_added_at(conn, scid, retired)?
                    .filter(|successor| successor.sign == *master)
                    .map(|successor| successor.id),
                None => (db::next_commitment(conn, scid)?.as_ref()
                    == Some(&commitment(master, salt)))
                .then(|| db::master_count(conn, scid))
                .transpose()?
                .map(|count| format!("m{count}")),
            };
            named
                .map(|id| (id, master.clone()))
                .ok_or(FoldError::NextMismatch)
        }
        _ => Ok((at.id, at.sign)),
    })
}

/// The proof by `id` (`<did>#<id>`) on `event` verifies under `key` in the `coasys:kel` domain:
/// the replay's own signer check for one signer. `MissingProof` when no proof names `id`,
/// `BadProof` when more than one does or it does not verify.
fn check_proof(scid: &str, event: &Event, id: &str, key: &Multikey) -> Result<(), FoldError> {
    let method = format!("{}#{id}", did_of(scid));
    let mut matching = event
        .proof
        .iter()
        .filter(|p| p.verification_method == method);
    let proof = matching
        .next()
        .ok_or_else(|| FoldError::MissingProof(id.to_string()))?;
    if matching.next().is_some() || verify_proof(event, proof, key, KEL_DOMAIN).is_err() {
        return Err(FoldError::BadProof(id.to_string()));
    }
    Ok(())
}

/// What [`Store::authenticate_first`] found of an append's first event to a stored log.
enum First {
    /// It continues the stored head and carries the proof of the key the log lets sign it there.
    Continues,
    /// It differs from the stored event at its seq and carries the proof of the master that signs
    /// there, which has no recorded fork yet: a candidate fork.
    Rival,
}

impl Store {
    /// Whether `event`, a rival at seq ≥ 1 of the stored log `scid`, is refused on the state
    /// before its seq, rebuilt from the fold of the stored head (`Store::held_chain`: the held
    /// fold, or on a miss one fold that is then held, so a rival sent again replays nothing) by
    /// `held::state_before`, with the next-key commitment there read from the one stored event
    /// that set it, and with the checks a replay runs on it: its `prev` and chain id, then the
    /// identity core's full `apply`.
    fn refused_on_held_fold(
        &self,
        conn: &Connection,
        scid: &str,
        event: &Event,
    ) -> Result<bool, ApiError> {
        let Some(held) = self.held_chain(conn, scid)? else {
            return Ok(false);
        };
        let seq = event.seq;
        if seq == 0 || seq > held.head().seq {
            return Ok(false);
        }
        let Some(added) = held
            .state()
            .masters
            .iter()
            .rev()
            .find(|m| m.added < seq)
            .map(|m| m.added)
        else {
            return Ok(false);
        };
        let next = match db::stored_event(conn, scid, added)?.map(|e| e.op) {
            Some(Op::Incept { next, .. }) => Some(next),
            Some(Op::Rotate { next, .. }) => next,
            _ => return Ok(false),
        };
        if event.prev.as_ref() != held.hash_at(seq - 1) || event.scid != *held.id() {
            return Ok(true);
        }
        let mut before = state_before(held.state(), seq, next);
        Ok(KelPolicy.apply(&mut before, event).is_err())
    }

    /// The fork `event` makes with the stored log of `scid`, when it is one: an event at a seq
    /// the store holds, with another hash than the stored event there, that replays on the
    /// stored events before it (the stored prefix folded as the store's own, `fold_stored`; the
    /// event itself checked in full by the identity core). Answers the evidence
    /// and the master that signed the event, read from that replay: the last master of the state
    /// after it (an `incept` or `rotate` is signed by the master it names, every other event by
    /// the master current before it). `None` for an event that does not replay.
    ///
    /// Called only for an event [`Store::authenticate_first`] found to be a rival signed by the
    /// master that signs at its seq, with no recorded fork: only then is the prefix replayed
    /// (event #0 needs no replay). A rival that does not replay is refused on the fold of the
    /// stored head first ([`Store::refused_on_held_fold`]), with no replay while the log is held;
    /// one that passes there is still replayed on the stored prefix before it is recorded,
    /// which happens once per signing master.
    fn competing_event(
        &self,
        conn: &Connection,
        scid: &str,
        event: &Event,
    ) -> Result<Option<(ForkEvidence, Multikey)>, ApiError> {
        let (Some(ours), Ok(theirs)) = (db::stored_hash(conn, scid, event.seq)?, event_hash(event))
        else {
            return Ok(None);
        };
        let evidence = ForkEvidence {
            chain: scid.to_string(),
            seq: event.seq,
            ours,
            theirs,
        };
        let after = if event.seq == 0 {
            KelChain::genesis(&KelPolicy, event).ok()
        } else if self.refused_on_held_fold(conn, scid, event)? {
            None
        } else {
            self.held.count_fold();
            match db::load_chain_before(conn, scid, event.seq)? {
                Some(mut prefix) => prefix.append(&KelPolicy, event).is_ok().then_some(prefix),
                None => None,
            }
        };
        let Some(signer) = after.and_then(|a| a.state().masters.last().map(|m| m.sign.clone()))
        else {
            return Ok(None);
        };
        Ok(Some((evidence, signer)))
    }
}

impl Store {
    /// Authenticates the first event of an append to the stored log of `scid` (head `head`) from
    /// the key index, before the log is read: nobody without a master key of the log makes the
    /// store replay it. Refuses, with no replay:
    /// - a non-`rotate` while a fork by the current master freezes the identity (`409 forked`);
    /// - an event that continues the head without the proof of the key that signs there: the
    ///   replay's own refusal (`400 MissingProof`, `BadProof`, `NextMismatch`, `Ended`);
    /// - an event past the head, a resend of a stored event, and a rival without the proof of
    ///   the master that signs at its seq (`409 {head}`), and a rival by a master that already
    ///   has a recorded fork (`409 forked` with that fork).
    ///
    /// The replay still checks everything once this passes; this only decides whether the
    /// sender may make the store read the log.
    fn authenticate_first(
        &self,
        conn: &Connection,
        scid: &str,
        head: &Head,
        first: &Event,
    ) -> Result<First, ApiError> {
        // A fork signed by the current master freezes the identity. The one append a frozen
        // identity takes is a `rotate`: it is signed by the committed next key, which the
        // rival's signer alone does not hold, and once appended the rival's signer is no longer
        // current, so the freeze ends.
        if !matches!(first.op, Op::Rotate { .. }) {
            if let Some(current) = db::master_at(conn, scid, head.seq + 1)? {
                if let Some(fork) = db::signer_fork(conn, scid, &current.sign)? {
                    return Err(ApiError::forked(&current_head(conn, scid)?, &fork));
                }
            }
        }
        if first.seq == head.seq + 1 && first.prev.as_ref() == Some(&head.hash) {
            let refusal = |e: FoldError| chain_error(&ChainError::Policy(e));
            let (id, key) = indexed_signer(conn, scid, first)?.map_err(refusal)?;
            check_proof(scid, first, &id, &key).map_err(refusal)?;
            return Ok(First::Continues);
        }
        let conflict =
            || -> Result<First, ApiError> { Err(ApiError::conflict(&current_head(conn, scid)?)) };
        if first.seq > head.seq {
            return conflict();
        }
        let (Some(ours), Ok(theirs)) = (db::stored_hash(conn, scid, first.seq)?, event_hash(first))
        else {
            return conflict();
        };
        if theirs == ours {
            return conflict();
        }
        let signer = if first.seq == 0 {
            // A rival #0 names this SCID and carries its listed keys' proofs: the genesis check
            // replays nothing.
            match KelChain::genesis(&KelPolicy, first) {
                Ok(rival) if first.scid == scid => match rival.state().masters.first() {
                    Some(master) => master.sign.clone(),
                    None => return conflict(),
                },
                _ => return conflict(),
            }
        } else {
            match indexed_signer(conn, scid, first)? {
                Ok((id, key)) if check_proof(scid, first, &id, &key).is_ok() => key,
                _ => return conflict(),
            }
        };
        if let Some(fork) = db::signer_fork(conn, scid, &signer)? {
            return Err(ApiError::forked(&current_head(conn, scid)?, &fork));
        }
        Ok(First::Rival)
    }
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
        let first = &events[0];
        // The length cap: no log grows past what the readers read.
        if first.seq.saturating_add(events.len() as u64) > MAX_LOG_EVENTS {
            return Err(ApiError::invalid(
                "log-full",
                format!(
                    "a log holds at most {MAX_LOG_EVENTS} events, the most a reader pages through"
                ),
            ));
        }
        let now = self.now();

        let mut conn = self.db.lock()?;
        let tx = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        // The sender's authority before any replay: the first event must carry the proof of the
        // master the key index names for it (`authenticate_first`).
        if let Some(head) = SqliteChainStore::head_in(&tx, KEL, &scid.to_string())? {
            if req.vouch.is_some() || req.pow.is_some() {
                return Err(ApiError::invalid(
                    "malformed",
                    "a vouching token or a proof of work comes only with event #0",
                ));
            }
            if let First::Rival = self.authenticate_first(&tx, scid, &head, first)? {
                // A validly signed event at a seq the store holds, other than the stored one,
                // is a fork: the store records it (with the event, so anyone can check it) and
                // answers `409 forked`. It freezes the identity only when its signer is the
                // current master (`db::freezing_fork`). One fork is kept per signing master:
                // a further rival by the same master is answered with the recorded fork and
                // adds nothing (`authenticate_first`). Anything else is a plain conflict.
                let Some((fork, signer)) = self.competing_event(&tx, scid, first)? else {
                    return Err(ApiError::conflict(&current_head(&tx, scid)?));
                };
                let theirs = ad4m_hash_chain::entry_json(first)
                    .map_err(|e| ApiError::invalid("malformed", e.to_string()))?;
                let fork = db::record_fork(&tx, fork, &theirs, &signer)?;
                let head = current_head(&tx, scid)?;
                tx.commit()?;
                drop(conn);
                self.changed(&[Topic::offers(scid)]);
                return Err(ApiError::forked(&head, &fork));
            }
        }
        let stored = self.held_chain(&tx, scid)?;
        let new_log = stored.is_none();

        // The continuity check, the new-log admission and the replay check. The batch steps on
        // one staged chain: for a stored log, one copy of the held fold, which each event then
        // changes in place (`Chain::appended`), so a batch costs one copy of the state whatever
        // its length and wherever it fails, and the held fold stays as it is until the commit.
        let mut chain: KelChain;
        // The claim check of each `add`, run on the staged state just before the `add` steps
        // (the state the core's `check_add` reads) and answered below in the order of the
        // checks, so no state before an event is kept.
        let mut claim_checks: Vec<Option<Result<(), ApiError>>> = Vec::with_capacity(events.len());
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
                claim_checks.push(None);
                rest = &events[1..];
            }
            Some(existing) => {
                // `authenticate_first` passed: the first event continues the stored head, signed
                // by the key that signs there, and the identity is not frozen against it.
                let head = existing.head();
                if first.seq != head.seq + 1 || first.prev.as_ref() != Some(&head.hash) {
                    return Err(ApiError::conflict(&current_head(&tx, scid)?));
                }
                // The one copy of the append: the held fold stays as it is until the commit.
                chain = KelChain::clone(&existing);
                rest = &events[..];
            }
        }
        for event in rest {
            claim_checks.push(match &event.op {
                Op::Add { offer, .. } => Some(offers::check_spend(
                    &tx,
                    scid,
                    offer,
                    chain.state(),
                    event,
                    now,
                )),
                _ => None,
            });
            chain = chain
                .appended(&KelPolicy, event)
                .map_err(|e| chain_error(&e))?;
        }
        // The per-log budget, two bounds. The live part of the fold (without the records of
        // keys revoked at the head) stays within `log_fold_bytes`, so after a thief pads a log
        // with the keys of a stolen master, the owner's `rotate` and the revocation of those
        // keys free the budget again. The whole fold stays within twice it, so no log grows
        // past what one fold may take in memory by padding and revoking over and over: every
        // log the store takes can be held, and a miss on any of them costs a bounded replay.
        // An append of `rotate`s only is refused by neither: a `rotate` is the recovery from a
        // stolen master, which a thief holding that master could otherwise lock out by padding
        // the log to the budget. A `rotate` adds one master record to the fold, and the length
        // cap bounds how many there are.
        let only_rotates = events.iter().all(|e| matches!(e.op, Op::Rotate { .. }));
        if !only_rotates {
            let budget = self.settings.log_fold_bytes;
            let FoldBytes { total, live } = fold_bytes_of(&chain);
            if total > budget.saturating_mul(2) {
                return Err(ApiError::invalid(
                    "log-too-large",
                    format!(
                        "the log's whole fold would take {total} bytes, above twice the store's \
                         budget of {budget} bytes per log"
                    ),
                ));
            }
            if live > budget {
                return Err(ApiError::invalid(
                    "log-too-large",
                    format!(
                        "the live part of the log's fold (without revoked keys) would take {live} \
                         bytes, above the store's budget of {budget} bytes per log"
                    ),
                ));
            }
        }

        // Expiries by the store's clock, the one-key check and the claim check.
        let mut spends = Vec::new();
        let mut supersede = false;
        for (event, claim_check) in events.iter().zip(claim_checks) {
            check_expires(event, now)?;
            for key in listed_keys(event) {
                if db::key_owner(&tx, key)?.is_some() {
                    return Err(ApiError::key_taken(key));
                }
            }
            match (&event.op, claim_check) {
                (Op::Add { offer, .. }, Some(checked)) => {
                    checked?;
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
        // The new head's fold, held before the database lock is released, so the next request
        // checks against it (a revoked key is refused at once) with no replay.
        self.held.put(scid, Arc::new(chain));
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

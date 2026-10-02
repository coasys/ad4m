//! A per-replica memo of receipt verdicts, keyed by what the verdict is a
//! function of and nothing else, holding only what a read path uses of it.
//!
//! [`verify_receipt`] is pure: its verdict depends only on the receipt's
//! own contents and on the reader's copy of the flow the receipt names —
//! the whole `SHACLFlow` as the catalogue holds it, **element order
//! included**. Both have a content hash — the receipt's URI is the hash of
//! its body ([`FlowReceipt::uri`]), and [`held_definition_hash`] is the
//! hash of the definition exactly as held — so `(receipt URI, held
//! definition hash)` determines the verdict completely. That is the memo
//! key. Under the same key the answer cannot differ, so the memo can only
//! save work, never change an answer:
//!
//! - **A DNA edit is a different key.** The reader's held hash changes, the
//!   lookup misses, and the receipt is verified afresh — which reports
//!   [`DnaChanged`](ReceiptVerdict::DnaChanged) exactly as an un-memoised
//!   read would. Nothing is invalidated because nothing needs to be: the old
//!   entries simply stop being looked up and age out of the LRU.
//! - **A forged receipt is a different key too.** Its body hashes to another
//!   URI, so it can never be served the honest receipt's verdict.
//! - **A reload in another link order is a different key as well.** The
//!   key is deliberately *not*
//!   [`flow_dna_hash`](crate::perspectives::flow_instance::receipt::flow_dna_hash),
//!   which sorts `states` and `transitions` so that two replicas name the
//!   same DNA alike whatever order their links arrive in. The verifier
//!   reads that order: `initial_state_of` is `states.first()`, and the fold
//!   refuses a genesis other than it. The parser keeps equal-`value` states
//!   in link order, so the same DNA can load as `[open, done]` on one read
//!   and `[done, open]` on the next, verify differently, and hash the same
//!   under `flow_dna_hash` — a memo keyed on it would serve the first
//!   load's verdict on the second (CodeRabbit on #1201). Keyed on the held
//!   definition, it misses and re-verifies, as an un-memoised read would.
//!
//! # What is kept: a [`SettledRun`] or nothing
//!
//! A read path (`valid_outputs`, the `producedByFlow` filter, the role
//! gate) uses two things of a verdict: that it is `Verified`, and then the
//! run's `terminal_state` and `settled_at`. It uses nothing of a verdict
//! that is not `Verified` — rejected and undecidable exclude alike — and
//! that is why the memo keeps nothing of one. A rejected verdict's payload
//! is sized by whoever wrote the receipt: `OutputsNotCommitted.claimed` is
//! the receipt's whole outputs list, `Unfoldable.reason` an error string.
//! Memoising it made 4096 entries cost up to 4096 × a receipt body (Marvin
//! on #1201). So the memo stores the projection [`SettledRun::of`], and a
//! caller that wants the reason (the `verifyFlowReceipt` API) calls
//! [`verify_receipt`] itself. The reason is logged at debug level on the
//! one verification that computes it.
//!
//! **Every entry is bounded by a constant.** The key is two hashes. The
//! value is `None`, or a state name of the reader's own definition and one
//! vote timestamp the fold parsed as RFC 3339 — a few dozen bytes in any
//! honest run. A self-quorate writer (`{ n: 1 }`) can still sign a vote
//! whose timestamp carries a kilobyte of fractional digits, so a value over
//! [`MEMO_ENTRY_MAX_BYTES`] is returned but not stored; the next read
//! re-verifies it, as an un-memoised read would.
//!
//! # Why it exists (#1177)
//!
//! `load_flow_receipts` has no count cap any more, so a flow with N receipts
//! is N full verifications — one SHA-256 plus one ed25519 per carried link —
//! on **every** `flowValidOutputs` call and every `derive_state` of every
//! flow gated on it. That growth comes from honest use alone, and under a
//! junk flood the reader pays it for every body that hashes to its own URI
//! while the attacker paid nothing to sign. Reads repeat and syncs do not,
//! so a memo removes both costs from every read but the first.
//!
//! One memo per [`PerspectiveInstance`](crate::perspectives::perspective_instance::PerspectiveInstance),
//! bounded by an LRU ([`VERDICT_MEMO_CAPACITY`]): a flood can churn it, at
//! which point the reader is back to paying what it paid without the memo,
//! never more.

use super::{verify_receipt, ReceiptVerdict};
use crate::perspectives::flow_evaluator::canonical_json;
use crate::perspectives::flow_instance::receipt::FlowReceipt;
use crate::perspectives::shacl_parser::SHACLFlow;
use lru::LruCache;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::sync::Mutex;

/// Entries one perspective's memo holds before the least recently used is
/// evicted. Each is a key of two hashes and a value under
/// [`MEMO_ENTRY_MAX_BYTES`], so the memo is under a megabyte per
/// perspective whatever the receipts carried.
pub const VERDICT_MEMO_CAPACITY: usize = 4096;

/// The most a memoised value may weigh. A [`SettledRun`] is a state name
/// and one RFC 3339 timestamp — under a hundred bytes in any honest run —
/// and a value over this is returned but not stored. See the module
/// header, § *What is kept*.
pub const MEMO_ENTRY_MAX_BYTES: usize = 256;

/// What a read path uses of a `Verified` verdict, and all the memo keeps
/// of any verdict: the state the run settled into and when.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SettledRun {
    /// [`ReceiptVerdict::Verified::terminal_state`] — the state the
    /// reader's own fold reached.
    pub terminal_state: String,
    /// [`ReceiptVerdict::Verified::settled_at`] — the quorum-fixed time.
    pub settled_at: String,
}

impl SettledRun {
    /// The projection: `Some` for exactly a `Verified` verdict, `None` for
    /// every other kind, rejected and undecidable alike. What every read
    /// path branches on; a caller that needs the reason has the verdict.
    pub fn of(verdict: &ReceiptVerdict) -> Option<Self> {
        match verdict {
            ReceiptVerdict::Verified {
                terminal_state,
                settled_at,
                ..
            } => Some(Self {
                terminal_state: terminal_state.clone(),
                settled_at: settled_at.clone(),
            }),
            _ => None,
        }
    }

    fn bytes(&self) -> usize {
        self.terminal_state.len() + self.settled_at.len()
    }
}

/// [`verify_receipt`], projected to what a read path uses. The one place
/// a non-verified verdict's reason is still in hand, so it is logged here
/// at debug level before the projection drops it.
pub fn settle(catalogue: &HashMap<String, SHACLFlow>, receipt: &FlowReceipt) -> Option<SettledRun> {
    let verdict = verify_receipt(catalogue, receipt);
    let settled = SettledRun::of(&verdict);
    if settled.is_none() {
        log::debug!(
            "a receipt for `{}` does not verify and speaks for nothing here — {verdict}",
            receipt.flow_uri
        );
    }
    settled
}

/// `(receipt URI, held definition hash) → Option<SettledRun>` for one
/// perspective. See the module header for why this key is exact and why
/// the value is a projection.
pub struct VerdictMemo {
    settled: Mutex<LruCache<(String, String), Option<SettledRun>>>,
    /// Test-only: how many times this memo has actually run
    /// [`verify_receipt`]. What the counting test reads to prove a second
    /// read of an unchanged set re-verifies nothing.
    #[cfg(test)]
    verifications: std::sync::atomic::AtomicUsize,
}

impl Default for VerdictMemo {
    fn default() -> Self {
        Self::with_capacity(VERDICT_MEMO_CAPACITY)
    }
}

impl VerdictMemo {
    pub fn with_capacity(capacity: usize) -> Self {
        let capacity = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::MIN);
        Self {
            settled: Mutex::new(LruCache::new(capacity)),
            #[cfg(test)]
            verifications: std::sync::atomic::AtomicUsize::new(0),
        }
    }

    /// [`settle`], answered from the memo when the same receipt was already
    /// verified under the same held definition. Same answer either way.
    pub fn settled(
        &self,
        catalogue: &HashMap<String, SHACLFlow>,
        receipt: &FlowReceipt,
    ) -> Option<SettledRun> {
        let key = memo_key(catalogue, receipt);
        if let Some(key) = &key {
            if let Some(settled) = self.lock().get(key) {
                return settled.clone();
            }
        }
        #[cfg(test)]
        self.verifications
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let settled = settle(catalogue, receipt);
        let weight = settled.as_ref().map_or(0, SettledRun::bytes);
        if let Some(key) = key.filter(|_| weight <= MEMO_ENTRY_MAX_BYTES) {
            self.lock().put(key, settled.clone());
        }
        settled
    }

    /// How many verifications this memo has actually run.
    #[cfg(test)]
    pub fn verifications(&self) -> usize {
        self.verifications.load(std::sync::atomic::Ordering::SeqCst)
    }

    /// Test-only: what the memo holds, in bytes of key and value strings —
    /// the number the bound in the module header is about.
    #[cfg(test)]
    pub fn bytes_held(&self) -> usize {
        self.lock()
            .iter()
            .map(|((uri, held), settled)| {
                uri.len() + held.len() + settled.as_ref().map_or(0, SettledRun::bytes)
            })
            .sum()
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, LruCache<(String, String), Option<SettledRun>>> {
        // A panic while holding the lock leaves a cache, not an invariant, so
        // a poisoned memo is still safe to read.
        self.settled
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}

/// The key under which `receipt`'s verdict is a constant: its own
/// content-derived URI and the hash of the definition **this reader** holds
/// for the flow it names, exactly as held. `None` when either does not hash
/// or the flow is not held — nothing worth memoising, and nothing safe to.
fn memo_key(
    catalogue: &HashMap<String, SHACLFlow>,
    receipt: &FlowReceipt,
) -> Option<(String, String)> {
    let held = held_definition_hash(catalogue.get(&receipt.flow_uri)?).ok()?;
    let uri = receipt.uri().ok()?;
    Some((uri, held))
}

/// `hex(SHA256(canonical_json(flow)))` over the definition **as held**:
/// every field `verify_receipt` can read, in the order it will read it.
/// Object keys are sorted (that is what makes it canonical); arrays are
/// not, and that is the difference from
/// [`flow_dna_hash`](crate::perspectives::flow_instance::receipt::flow_dna_hash),
/// which sorts `states` and `transitions` because it names the DNA across
/// replicas. This hash names one reader's copy, and one reader's copy is
/// what the verdict is a function of — see the module header.
fn held_definition_hash(flow: &SHACLFlow) -> anyhow::Result<String> {
    let value = serde_json::to_value(flow)?;
    Ok(hex::encode(Sha256::digest(
        canonical_json(&value).as_bytes(),
    )))
}

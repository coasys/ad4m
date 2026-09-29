//! A per-replica memo of receipt verdicts, keyed by what the verdict is a
//! function of and nothing else.
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
//!   which sorts `states` and
//!   `transitions` so that two replicas name the same DNA alike whatever
//!   order their links arrive in. The verifier reads that order:
//!   `initial_state_of` is `states.first()`, and the fold refuses a genesis
//!   other than it. The parser keeps equal-`value` states in link order, so
//!   the same DNA can load as `[open, done]` on one read and `[done, open]`
//!   on the next, verify differently, and hash the same under
//!   `flow_dna_hash` — a memo keyed on it would serve the first load's
//!   verdict on the second (CodeRabbit on #1201). Keyed on the held
//!   definition, it misses and re-verifies, as an un-memoised read would.
//!
//! What is not memoised: a receipt for a flow the catalogue does not hold
//! (`FlowUnknown` costs nothing and the catalogue may fill in), and material
//! that does not hash at all. Those fall through to a plain verification.
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

/// Verdicts one perspective's memo holds before the least recently used is
/// evicted. A `Verified` verdict is a few hundred bytes (its output refs
/// and voter DIDs), so this is well under a megabyte per perspective.
pub const VERDICT_MEMO_CAPACITY: usize = 4096;

/// `(receipt URI, held definition hash) → verdict` for one perspective. See
/// the module header for why this key is exact.
pub struct VerdictMemo {
    verdicts: Mutex<LruCache<(String, String), ReceiptVerdict>>,
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
            verdicts: Mutex::new(LruCache::new(capacity)),
            #[cfg(test)]
            verifications: std::sync::atomic::AtomicUsize::new(0),
        }
    }

    /// [`verify_receipt`], answered from the memo when the same receipt was
    /// already verified under the same held definition. Same verdict either
    /// way.
    pub fn verify(
        &self,
        catalogue: &HashMap<String, SHACLFlow>,
        receipt: &FlowReceipt,
    ) -> ReceiptVerdict {
        let key = memo_key(catalogue, receipt);
        if let Some(key) = &key {
            if let Some(verdict) = self.lock().get(key) {
                return verdict.clone();
            }
        }
        #[cfg(test)]
        self.verifications
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let verdict = verify_receipt(catalogue, receipt);
        if let Some(key) = key {
            self.lock().put(key, verdict.clone());
        }
        verdict
    }

    /// How many verifications this memo has actually run.
    #[cfg(test)]
    pub fn verifications(&self) -> usize {
        self.verifications.load(std::sync::atomic::Ordering::SeqCst)
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, LruCache<(String, String), ReceiptVerdict>> {
        // A panic while holding the lock leaves a cache, not an invariant, so
        // a poisoned memo is still safe to read.
        self.verdicts
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
/// replicas.
/// This hash names one reader's copy, and one reader's copy is what the
/// verdict is a function of — see the module header.
fn held_definition_hash(flow: &SHACLFlow) -> anyhow::Result<String> {
    let value = serde_json::to_value(flow)?;
    Ok(hex::encode(Sha256::digest(
        canonical_json(&value).as_bytes(),
    )))
}

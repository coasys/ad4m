//! Atomic processing-claim reservation for neighbourhood auto-processing.
//!
//! Phase **P-A** of the AutoProcessor arc (see
//! `planning/neighbourhood-auto-processing-spec.md`). This is the reservation
//! primitive that fixes Flux's double-processing race: before a peer runs an
//! (expensive, LLM) processing pass over a batch of items, it writes a
//! **`ProcessingClaim`** into the *shared* perspective. Because shared links sync
//! across the neighbourhood, a claim already present (and unexpired) for the same
//! batch means every other peer backs off. Correctness rests on the **synced
//! claim link**, not on ephemeral signals (spec §4.4).
//!
//! Pure coordination — no LLM, no telepresence. Telepresence-based presence /
//! election (P-B) and the `AutoProcessor` subject + executor watcher build on
//! top of this.
//!
//! ## Claim shape (links, all `Shared`)
//! A batch is keyed by [`batch_key`] (order-independent hash of the source item
//! id-set — provisional per spec §8). Each claimant hangs its own claim node off
//! the shared batch node, so concurrent claimants don't clobber each other's
//! expiry/status:
//! ```text
//! batch node  ad4m://claim/<processor>/<key>
//!   -- ad4m://has_claim -->  claim node  ad4m://claim/<processor>/<key>/<did>
//! claim node
//!   -- rdf://type          --> ad4m://ProcessingClaim
//!   -- ad4m://claimant     --> <claimant did>
//!   -- ad4m://expires_at   --> <unix millis, as string>
//!   -- ad4m://claim_status --> "active" | "done"
//! ```
//!
//! ## Winner determination
//! Under a race, two peers may both write a claim before either sees the other.
//! Rather than last-writer-wins (which needs a total sync order we don't have),
//! [`try_claim`] is **deterministic**: after writing its own claim it reads *all*
//! active, unexpired claimants for the batch and the lexicographically smallest
//! DID wins. Every peer converges on the same winner once claims sync, so exactly
//! one proceeds. A losing/crashed claimant's claim simply expires (`ttl_ms`),
//! after which the batch can be re-claimed.

use crate::agent::{did_for_context, AgentContext};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkQuery, LinkStatus};
use sha2::{Digest, Sha256};

const P_TYPE: &str = "rdf://type";
const T_CLAIM: &str = "ad4m://ProcessingClaim";
const P_HAS_CLAIM: &str = "ad4m://has_claim";
const P_CLAIMANT: &str = "ad4m://claimant";
const P_EXPIRES_AT: &str = "ad4m://expires_at";
const P_STATUS: &str = "ad4m://claim_status";
const STATUS_ACTIVE: &str = "active";

/// Order-independent key for a batch of source items: a hash of the sorted,
/// de-duplicated item id-set. Independent of query/iteration order, so every
/// peer computes the same key for the same logical batch.
///
/// Provisional per spec §8 (claim unit = id-set vs link-set — id-set chosen,
/// still open for discussion). A `\0` separator between ids keeps the hash
/// injective over the id boundaries (so `["ab","c"]` and `["a","bc"]` differ).
pub fn batch_key(item_ids: &[String]) -> String {
    let mut ids: Vec<&str> = item_ids.iter().map(String::as_str).collect();
    ids.sort_unstable();
    ids.dedup();
    let mut hasher = Sha256::new();
    for id in ids {
        hasher.update(id.as_bytes());
        hasher.update([0u8]);
    }
    format!("{:x}", hasher.finalize())
}

/// Order-independent key for a batch of source items scoped to a *partition*.
///
/// Wildcard/partitioned processors (spec §6.5) bind an extra `?partition`
/// variable in their `source_scope_query`; the engine groups the SPARQL result
/// by that binding and runs one pass per partition. Each partition must claim
/// independently so different peers can process different partitions of the
/// same processor in parallel — while still guaranteeing no two peers claim the
/// *same* `(processor, partition)` pair. This function returns the batch key
/// for that per-partition claim.
///
/// Design (matches [`batch_key`] modulo the partition prefix):
/// - Partition is hashed *before* the id-set, so `(partition, item_ids)` and
///   `(item_ids)` land in different key-spaces even for the same id-set.
/// - `\0` separator between the partition and the id-set (and between ids)
///   keeps the hash injective over the partition/id boundary — `partition="ab"
///   ids=["c"]` and `partition="a" ids=["bc"]` produce distinct keys.
/// - The empty-string partition is a valid, distinct partition value; it is
///   NOT the same key-space as the unpartitioned [`batch_key`].
pub fn batch_key_for_partition(partition: &str, item_ids: &[String]) -> String {
    let mut ids: Vec<&str> = item_ids.iter().map(String::as_str).collect();
    ids.sort_unstable();
    ids.dedup();
    let mut hasher = Sha256::new();
    hasher.update(partition.as_bytes());
    hasher.update([0u8]);
    for id in ids {
        hasher.update(id.as_bytes());
        hasher.update([0u8]);
    }
    format!("{:x}", hasher.finalize())
}

/// The shared node under which every claimant's claim node for this batch hangs.
/// Deterministic in `(processor, key)` so all peers address the same batch.
pub fn batch_node(processor: &str, key: &str) -> String {
    format!("ad4m://claim/{processor}/{key}")
}

/// This claimant's own claim node under the batch — scoped by DID so concurrent
/// claimants keep independent expiry/status.
fn claim_node(processor: &str, key: &str, claimant: &str) -> String {
    format!("ad4m://claim/{processor}/{key}/{claimant}")
}

/// Outcome of a reservation attempt.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClaimOutcome {
    /// This agent holds the batch — proceed to process it.
    Won,
    /// Another agent holds it (won the deterministic tiebreak) — back off.
    BackedOff { holder: String },
}

/// Write a claim for `claimant` on `(processor, batch)` expiring at `expires_ms`,
/// as shared links. `claimant` is link *data*, so a test can simulate another
/// peer's claim by passing an arbitrary DID (the link author stays `context`'s
/// agent).
pub async fn write_claim(
    perspective: &mut PerspectiveInstance,
    processor: &str,
    key: &str,
    claimant: &str,
    expires_ms: i64,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let batch = batch_node(processor, key);
    let node = claim_node(processor, key, claimant);
    let links = vec![
        Link {
            source: node.clone(),
            predicate: Some(P_TYPE.into()),
            target: T_CLAIM.into(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_CLAIMANT.into()),
            target: claimant.into(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_EXPIRES_AT.into()),
            target: expires_ms.to_string(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_STATUS.into()),
            target: STATUS_ACTIVE.into(),
        },
        Link {
            source: batch,
            predicate: Some(P_HAS_CLAIM.into()),
            target: node,
        },
    ];
    perspective
        .add_links(links, LinkStatus::Shared, None, context)
        .await
        .map_err(|e| anyhow::anyhow!("write_claim: add_links failed: {e:#}"))?;
    Ok(())
}

/// One value of `(source, predicate)` in the perspective, or `None`.
async fn first_target(
    perspective: &PerspectiveInstance,
    source: &str,
    predicate: &str,
) -> anyhow::Result<Option<String>> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(source.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("get_links({source} {predicate}): {e:#}"))?;
    Ok(links.into_iter().next().map(|l| l.data.target))
}

/// DIDs of every claimant whose claim on the batch is `active` and not yet
/// expired at `now_ms`.
pub async fn active_claimants(
    perspective: &PerspectiveInstance,
    processor: &str,
    key: &str,
    now_ms: i64,
) -> anyhow::Result<Vec<String>> {
    let batch = batch_node(processor, key);
    let claim_nodes = perspective
        .get_links(&LinkQuery {
            source: Some(batch),
            predicate: Some(P_HAS_CLAIM.into()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("get_links(has_claim): {e:#}"))?;

    let mut out = Vec::new();
    for link in claim_nodes {
        let node = link.data.target;
        let status = first_target(perspective, &node, P_STATUS).await?;
        if status.as_deref() != Some(STATUS_ACTIVE) {
            continue;
        }
        let expires = first_target(perspective, &node, P_EXPIRES_AT)
            .await?
            .and_then(|s| s.parse::<i64>().ok());
        let Some(expires) = expires else { continue };
        if expires <= now_ms {
            continue;
        }
        if let Some(did) = first_target(perspective, &node, P_CLAIMANT).await? {
            out.push(did);
        }
    }
    out.sort();
    out.dedup();
    Ok(out)
}

/// Try to reserve a batch of items for processing. Writes this agent's claim,
/// then applies the deterministic min-DID tiebreak over all active claimants.
/// Returns [`ClaimOutcome::Won`] if this agent should process the batch.
pub async fn try_claim(
    perspective: &mut PerspectiveInstance,
    processor: &str,
    item_ids: &[String],
    ttl_ms: i64,
    now_ms: i64,
    context: &AgentContext,
) -> anyhow::Result<ClaimOutcome> {
    let me = did_for_context(context).map_err(|e| anyhow::anyhow!("did_for_context: {e:#}"))?;
    let key = batch_key(item_ids);

    write_claim(perspective, processor, &key, &me, now_ms + ttl_ms, context).await?;

    let holders = active_claimants(perspective, processor, &key, now_ms).await?;
    match holders.first() {
        // `holders` is sorted; the smallest DID wins.
        Some(winner) if winner == &me => Ok(ClaimOutcome::Won),
        Some(winner) => Ok(ClaimOutcome::BackedOff {
            holder: winner.clone(),
        }),
        // Shouldn't happen — we just wrote our own active claim — but treat an
        // empty read as "no contention, proceed".
        None => Ok(ClaimOutcome::Won),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;

    #[test]
    fn batch_key_is_order_and_dup_independent() {
        let a = batch_key(&["i1".into(), "i2".into(), "i3".into()]);
        let b = batch_key(&["i3".into(), "i1".into(), "i2".into(), "i1".into()]);
        assert_eq!(a, b, "same id-set (any order, dups) => same key");
        let c = batch_key(&["i1".into(), "i2".into()]);
        assert_ne!(a, c, "different id-set => different key");
    }

    #[test]
    fn batch_key_is_injective_over_boundaries() {
        // The `\0` separator must keep concatenation-ambiguous sets distinct.
        assert_ne!(
            batch_key(&["ab".into(), "c".into()]),
            batch_key(&["a".into(), "bc".into()]),
        );
    }

    #[test]
    fn batch_key_for_partition_is_order_and_dup_independent() {
        let a = batch_key_for_partition("p1", &["i1".into(), "i2".into(), "i3".into()]);
        let b =
            batch_key_for_partition("p1", &["i3".into(), "i1".into(), "i2".into(), "i1".into()]);
        assert_eq!(
            a, b,
            "same partition + id-set (any order, dups) => same key"
        );
        let c = batch_key_for_partition("p1", &["i1".into(), "i2".into()]);
        assert_ne!(a, c, "different id-set => different key");
    }

    #[test]
    fn batch_key_for_partition_differs_across_partitions() {
        // Same id-set under two different partitions must claim independently.
        let items = vec!["i1".to_string(), "i2".to_string()];
        let p1 = batch_key_for_partition("payments", &items);
        let p2 = batch_key_for_partition("onboarding", &items);
        assert_ne!(
            p1, p2,
            "different partition => different key (parallel claim safety)"
        );
    }

    #[test]
    fn batch_key_for_partition_is_distinct_from_unpartitioned() {
        // The empty-string partition is a valid partition — it must NOT collide
        // with the unpartitioned key-space. Otherwise a wildcard config with an
        // empty binding would silently share claims with a plain config on the
        // same id-set.
        let items = vec!["i1".to_string(), "i2".to_string()];
        assert_ne!(
            batch_key(&items),
            batch_key_for_partition("", &items),
            "empty partition != unpartitioned"
        );
        assert_ne!(
            batch_key(&items),
            batch_key_for_partition("p", &items),
            "any partition != unpartitioned"
        );
    }

    #[test]
    fn batch_key_for_partition_is_injective_over_partition_boundary() {
        // The `\0` separator must keep the partition/id-set boundary unambiguous:
        // partition="ab" ids=["c"] and partition="a" ids=["bc"] must differ.
        assert_ne!(
            batch_key_for_partition("ab", &["c".into()]),
            batch_key_for_partition("a", &["bc".into()]),
        );
        // And likewise across a partition-vs-empty-id-set flip.
        assert_ne!(
            batch_key_for_partition("a", &["b".into()]),
            batch_key_for_partition("ab", &[]),
        );
    }

    /// A sole claimant on a fresh batch wins.
    #[tokio::test]
    async fn sole_claimant_wins() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let items = vec!["i1".to_string(), "i2".to_string()];
        let outcome = try_claim(&mut p, "proc", &items, 60_000, 1_000, &ctx)
            .await
            .expect("try_claim");
        assert_eq!(outcome, ClaimOutcome::Won);
    }

    /// A peer with a smaller DID already holding an unexpired claim wins the
    /// tiebreak; the local agent backs off. (`did:key:` sorts after `aaa:`.)
    #[tokio::test]
    async fn backs_off_when_smaller_did_holds_claim() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let items = vec!["i1".to_string(), "i2".to_string()];
        let key = batch_key(&items);
        let other = "aaa:smaller-peer"; // sorts before any did:key:...
        write_claim(&mut p, "proc", &key, other, 60_000, &ctx)
            .await
            .expect("seed other claim");

        let outcome = try_claim(&mut p, "proc", &items, 60_000, 1_000, &ctx)
            .await
            .expect("try_claim");
        assert_eq!(
            outcome,
            ClaimOutcome::BackedOff {
                holder: other.to_string()
            }
        );
    }

    /// A peer with a larger DID does not block us — we win the tiebreak.
    #[tokio::test]
    async fn wins_over_larger_did() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let items = vec!["i1".to_string()];
        let key = batch_key(&items);
        let other = "zzz:larger-peer"; // sorts after any did:key:...
        write_claim(&mut p, "proc", &key, other, 60_000, &ctx)
            .await
            .expect("seed other claim");

        let outcome = try_claim(&mut p, "proc", &items, 60_000, 1_000, &ctx)
            .await
            .expect("try_claim");
        assert_eq!(outcome, ClaimOutcome::Won);
    }

    /// An expired claim by another peer is ignored — the batch is re-claimable.
    #[tokio::test]
    async fn expired_claim_is_ignored() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let items = vec!["i1".to_string()];
        let key = batch_key(&items);
        // Smaller DID, but its claim expired at t=500 and we evaluate at now=1000.
        write_claim(&mut p, "proc", &key, "aaa:stale-peer", 500, &ctx)
            .await
            .expect("seed stale claim");

        let outcome = try_claim(&mut p, "proc", &items, 60_000, 1_000, &ctx)
            .await
            .expect("try_claim");
        assert_eq!(outcome, ClaimOutcome::Won, "expired claim must not block");
    }

    /// The reservation semantics end to end: once a batch is claimed, a *second*
    /// distinct claimant on the same batch sees the existing claim and backs off
    /// (given the incumbent has the smaller DID). Proves "a claim already present
    /// for the same batch means back off".
    #[tokio::test]
    async fn second_claimant_backs_off() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let items = vec!["i1".to_string(), "i2".to_string(), "i3".to_string()];
        let key = batch_key(&items);
        // Incumbent with a guaranteed-smaller DID has already claimed.
        write_claim(&mut p, "proc", &key, "aaa:incumbent", 60_000, &ctx)
            .await
            .expect("incumbent claim");

        let outcome = try_claim(&mut p, "proc", &items, 60_000, 1_000, &ctx)
            .await
            .expect("try_claim");
        assert!(
            matches!(outcome, ClaimOutcome::BackedOff { .. }),
            "second claimant must back off; got {outcome:?}"
        );
    }
}

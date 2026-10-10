//! Atomic processing-claim reservation for neighbourhood auto-processing.
//!
//! Phase **P-A** of the AutoProcessor arc. This is the reservation
//! primitive that fixes Flux's double-processing race: before a peer runs an
//! (expensive, LLM) processing pass over a batch of items, it writes a
//! **`ProcessingClaim`** into the *shared* perspective. Because shared links sync
//! across the neighbourhood, a claim already present (and unexpired) for the same
//! batch means every other peer backs off. Correctness rests on the **synced
//! claim link**, not on ephemeral signals.
//!
//! Pure coordination — no LLM, no telepresence. Telepresence-based presence /
//! election (P-B) and the `AutoProcessor` subject + executor watcher build on
//! top of this.
//!
//! ## Claim shape
//! A batch is keyed by [`batch_key`] (order-independent hash of the source item
//! id-set — provisional). Each claimant hangs its own claim node off
//! the shared batch node, so concurrent claimants don't clobber each other's
//! expiry/status:
//! ```text
//! batch node  ad4m://claim/<processor>/<key>
//!   -- ad4m://has_claim -->  claim node  ad4m://claim/<processor>/<key>/<did>
//! ```
//! The claim node is a hard-wired **SHACL subject class**
//! ([`PROCESSING_CLAIM_SDNA`], registered on first claim by
//! [`ensure_processing_claim_class`]): written with `create_subject`, read back
//! with `model_query` scoped to the batch node, so UIs can see who holds what.
//! The batch node itself is not an instance — it is just the shared anchor the
//! `ad4m://has_claim` links (`Shared`, like every link a claim writes) hang off.
//!
//! ## Winner determination
//! Under a race, two peers may both write a claim before either sees the other.
//! Rather than last-writer-wins (which needs a total sync order we don't have),
//! [`try_claim`] is **deterministic**: after writing its own claim it reads *all*
//! active, unexpired claimants for the batch and the lexicographically smallest
//! DID wins. Every peer converges on the same winner once claims sync, so exactly
//! one proceeds. A losing/crashed claimant's claim simply expires (`ttl_ms`),
//! after which the batch can be re-claimed.
//!
//! ## The TTL vs. sync-latency window (why exactly-once is *eventual*)
//! The guarantee is only as strong as the relationship between `ttl_ms` and the
//! neighbourhood's claim-sync latency, and there are two failure edges to size
//! against:
//! * **TTL too short.** If a claim expires before the winner finishes its pass
//!   *and* before peers have converged, another peer sees no active claimant and
//!   re-claims — double processing. So `ttl_ms` must comfortably exceed
//!   `expected sync latency + a pass's worst-case runtime`.
//! * **Claims not yet synced.** Two peers that both claim inside one sync round
//!   each read only their own claim and each believe they won — until the other's
//!   claim arrives. The min-DID rule makes them *converge* on the same winner
//!   once synced, but a peer that starts its (expensive) pass before the round
//!   completes runs it too — so a genuine same-round tie can produce **duplicate
//!   writes** for that one batch (identity dedup can't catch them: neither peer
//!   has seen the other's output yet). This is the residual, accepted race: the
//!   debounce/quiet-window before a batch is eligible, plus `claim_ttl_ms` sized
//!   above sync latency, make a same-instant tie rare rather than impossible. A
//!   hard exactly-once across an unbounded-latency link would need a consensus
//!   round we deliberately avoid; the min-DID convergence covers every case
//!   *except* the sub-sync-round simultaneous claim.

use crate::agent::{did_for_context, AgentContext};
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::{Link, LinkStatus};
use sha2::{Digest, Sha256};

use super::scalar_string;
use crate::perspectives::hardwired_class::{ensure_subject_class, subject_class_registered};

/// Local subject-class name of a claim.
pub(crate) const PROCESSING_CLAIM_CLASS: &str = "ProcessingClaim";
/// Target-class URI of [`PROCESSING_CLAIM_CLASS`] — used to detect prior
/// registration.
const PROCESSING_CLAIM_TARGET_CLASS: &str = "ad4m://ProcessingClaim";
const P_HAS_CLAIM: &str = "ad4m://has_claim";
const STATUS_ACTIVE: &str = "active";

/// Hard-wired SDNA for the [`PROCESSING_CLAIM_CLASS`] subject class. `claimant`
/// is the identity property: one claim node per DID per batch, so concurrent
/// claimants keep independent expiry/status. `expires_at` is a unix-millis
/// scalar stored as a string (SHACL carries no int type-check; the parse on
/// read in [`active_claimants`] is what validates it).
const PROCESSING_CLAIM_SDNA: &str = r#"{
  "target_class":"ad4m://ProcessingClaim",
  "interpretation_hint":"One peer's time-boxed reservation of a batch of items for processing, so other peers back off.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"rdf://type","target":"ad4m://ProcessingClaim"}],
  "properties":[
    {"path":"rdf://type","name":"type","has_value":"ad4m://ProcessingClaim","min_count":1,"max_count":1},
    {"path":"ad4m://claimant","name":"claimant","identity":true,"min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://claimant","target":"value"}]},
    {"path":"ad4m://expires_at","name":"expires_at","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://expires_at","target":"value"}]},
    {"path":"ad4m://claim_status","name":"claim_status","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://claim_status","target":"value"}]}
  ]
}"#;

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

/// Idempotently register the hard-wired [`PROCESSING_CLAIM_CLASS`] subject class.
pub async fn ensure_processing_claim_class(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_subject_class(
        perspective,
        PROCESSING_CLAIM_CLASS,
        PROCESSING_CLAIM_TARGET_CLASS,
        PROCESSING_CLAIM_SDNA,
        None,
        context,
    )
    .await
}

/// Write a claim for `claimant` on `(processor, batch)` expiring at `expires_ms`
/// and hang it off the shared batch node. `claimant` is instance *data*, so a
/// test can simulate another peer's claim by passing an arbitrary DID (the link
/// author stays `context`'s agent).
pub async fn write_claim(
    perspective: &mut PerspectiveInstance,
    processor: &str,
    key: &str,
    claimant: &str,
    expires_ms: i64,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_processing_claim_class(perspective, context).await?;
    let node = claim_node(processor, key, claimant);
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(PROCESSING_CLAIM_CLASS.to_string()),
                query: None,
            },
            node.clone(),
            Some(serde_json::json!({
                "claimant": claimant,
                "expires_at": expires_ms.to_string(),
                "claim_status": STATUS_ACTIVE,
            })),
            None,
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("write_claim: create_subject failed: {e:#}"))?;
    perspective
        .add_link(
            Link {
                source: batch_node(processor, key),
                predicate: Some(P_HAS_CLAIM.into()),
                target: node,
            },
            LinkStatus::Shared,
            None,
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("write_claim: add_link(has_claim) failed: {e:#}"))?;
    Ok(())
}

/// Extend the lease: rewrite this peer's claim with `now + ttl_ms` as the new
/// expiry. Called by the [`LeaseGuard`] heartbeat at `ttl_ms / 3` intervals.
///
/// This makes the TTL a **liveness** parameter (how fast a *crashed* claimant
/// is detected and its slot freed) rather than a **capacity** parameter (how
/// long the biggest model may run) — a live pass keeps pushing the expiry
/// forward, so the TTL only bites when the pass dies silently.
pub async fn renew_claim(
    perspective: &mut PerspectiveInstance,
    processor: &str,
    key: &str,
    claimant: &str,
    ttl_ms: i64,
    context: &AgentContext,
) -> anyhow::Result<()> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64;
    renew_claim_at(
        perspective,
        processor,
        key,
        claimant,
        ttl_ms,
        now_ms,
        context,
    )
    .await
}

/// [`renew_claim`] with the clock injected, so the renewal semantics can be
/// tested without sleeping through a real TTL. `renew_claim` is this function
/// with `now_ms` read from the system clock, and holds no other logic — tests
/// that exercise this exercise the production path.
pub async fn renew_claim_at(
    perspective: &mut PerspectiveInstance,
    processor: &str,
    key: &str,
    claimant: &str,
    ttl_ms: i64,
    now_ms: i64,
    context: &AgentContext,
) -> anyhow::Result<()> {
    write_claim(
        perspective,
        processor,
        key,
        claimant,
        now_ms + ttl_ms,
        context,
    )
    .await
}

/// DIDs of every claimant whose claim on the batch is `active` and not yet
/// expired at `now_ms`. Reads the claims as subject instances scoped to the
/// batch node, so only claims actually hung off *this* batch are considered.
pub async fn active_claimants(
    perspective: &PerspectiveInstance,
    processor: &str,
    key: &str,
    now_ms: i64,
) -> anyhow::Result<Vec<String>> {
    // Nothing has ever claimed on this perspective ⇒ no class, no claims.
    if !subject_class_registered(perspective, PROCESSING_CLAIM_TARGET_CLASS).await? {
        return Ok(Vec::new());
    }
    let query = serde_json::json!({
        "parent": { "id": batch_node(processor, key), "predicate": P_HAS_CLAIM },
        "properties": ["claimant", "expires_at", "claim_status"],
    })
    .to_string();
    let result_json = perspective
        .model_query(PROCESSING_CLAIM_CLASS, &query)
        .await
        .map_err(|e| anyhow::anyhow!("active_claimants: model_query failed: {e:#}"))?;
    let result: serde_json::Value = serde_json::from_str(&result_json)
        .map_err(|e| anyhow::anyhow!("active_claimants: bad model_query result: {e:#}"))?;

    let mut out = Vec::new();
    for claim in result["instances"].as_array().into_iter().flatten() {
        if scalar_string(claim.get("claim_status")).as_deref() != Some(STATUS_ACTIVE) {
            continue;
        }
        let expires = scalar_string(claim.get("expires_at")).and_then(|s| s.parse::<i64>().ok());
        let Some(expires) = expires else { continue };
        if expires <= now_ms {
            continue;
        }
        if let Some(did) = scalar_string(claim.get("claimant")) {
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
    use crate::types::LinkQuery;

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

    /// Claims sync across the neighbourhood or they coordinate nothing: every
    /// link a claim writes — the instance's own and the batch anchor — is
    /// `Shared`.
    #[tokio::test]
    async fn claim_links_are_shared() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let key = batch_key(&["i1".to_string()]);
        write_claim(&mut p, "proc", &key, "aaa:peer", 60_000, &ctx)
            .await
            .expect("write_claim");
        for source in [
            batch_node("proc", &key),
            claim_node("proc", &key, "aaa:peer"),
        ] {
            let links = p
                .get_links(&LinkQuery {
                    source: Some(source.clone()),
                    ..Default::default()
                })
                .await
                .expect("get_links");
            assert!(!links.is_empty(), "{source} must carry links");
            for l in &links {
                assert_eq!(
                    l.status,
                    Some(LinkStatus::Shared),
                    "link {:?} must be Shared",
                    l.data
                );
            }
        }
    }

    /// A claim is scoped to its own batch: a claimant on a *different* batch
    /// must not show up as a holder here (the batch anchor is the query scope).
    #[tokio::test]
    async fn claims_on_other_batches_are_not_visible() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mine = batch_key(&["i1".to_string()]);
        let other = batch_key(&["i2".to_string()]);
        write_claim(&mut p, "proc", &other, "aaa:elsewhere", 60_000, &ctx)
            .await
            .expect("seed other-batch claim");
        let holders = active_claimants(&p, "proc", &mine, 1_000)
            .await
            .expect("active_claimants");
        assert!(holders.is_empty(), "unexpected holders: {holders:?}");
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

    /// `renew_claim` pushes the expiry forward, so a claim that would have
    /// expired at `t0 + short_ttl` remains active at `t0 + short_ttl + 1`
    /// after a renewal with a new TTL.
    ///
    /// This is the core guarantee that makes TTL a liveness parameter rather
    /// than a capacity parameter: a live pass keeps renewing, and the TTL only
    /// bites when the pass crashes silently.
    #[tokio::test]
    async fn renew_extends_expiry_past_original_ttl() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let key = batch_key(&["i1".to_string()]);
        let claimant = "did:test:renewer";
        let short_ttl_ms: i64 = 1_000; // original TTL: 1s
        let t0: i64 = 1_000_000; // arbitrary epoch-offset

        // Write the original claim expiring at t0 + 1s.
        write_claim(&mut p, "proc", &key, claimant, t0 + short_ttl_ms, &ctx)
            .await
            .expect("initial claim");

        // At t = t0 + short_ttl_ms + 1, the original claim has expired.
        let after_original_expiry = t0 + short_ttl_ms + 1;
        let before_renewal = active_claimants(&p, "proc", &key, after_original_expiry)
            .await
            .expect("active_claimants before renewal");
        assert!(
            before_renewal.is_empty(),
            "original claim should have expired; got {before_renewal:?}"
        );

        // Renew through the production path, with the clock injected rather
        // than slept through. `renew_claim` is `renew_claim_at` plus a
        // `SystemTime::now()` read, so this exercises the real renewal logic.
        let long_ttl_ms: i64 = 120_000;
        renew_claim_at(
            &mut p,
            "proc",
            &key,
            claimant,
            long_ttl_ms,
            after_original_expiry,
            &ctx,
        )
        .await
        .expect("renewal write");

        // After renewal the claimant is active again well past the original TTL.
        let after_renewal = active_claimants(&p, "proc", &key, after_original_expiry)
            .await
            .expect("active_claimants after renewal");
        assert_eq!(
            after_renewal,
            vec![claimant.to_string()],
            "renewed claim must be active past the original TTL"
        );
    }

    /// An idle claimant whose claim expired while a different peer renewed its
    /// own claim stays evicted: renewal extends *this* peer's claim, not others'.
    #[tokio::test]
    async fn renewal_does_not_resurrect_idle_claimant() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let key = batch_key(&["i1".to_string()]);
        let idle = "aaa:idle";
        let active = "zzz:active";
        let t0: i64 = 1_000_000;
        let short_ttl: i64 = 1_000;
        let long_ttl: i64 = 120_000;

        // Both claim initially with the same short TTL.
        write_claim(&mut p, "proc", &key, idle, t0 + short_ttl, &ctx)
            .await
            .expect("idle claim");
        write_claim(&mut p, "proc", &key, active, t0 + short_ttl, &ctx)
            .await
            .expect("active initial claim");

        // Only the `active` peer renews past the original TTL.
        let after_expiry = t0 + short_ttl + 1;
        renew_claim_at(&mut p, "proc", &key, active, long_ttl, after_expiry, &ctx)
            .await
            .expect("active renewal");

        let holders = active_claimants(&p, "proc", &key, after_expiry)
            .await
            .expect("active_claimants");
        assert_eq!(
            holders,
            vec![active.to_string()],
            "only the renewing peer should remain active; got {holders:?}"
        );
    }
}

//! Multi-agent contention tests for the reservation primitive.
//!
//! Spike-flavored (build-list item 4 in `planning/neighbourhood-auto-processing-spec.md`
//! §6, per Nico voice 2026-08-08). Existing [`super::claim`] tests use a single
//! `AgentContext` and simulate other claimants by writing raw DID strings into
//! link data. That covers the tiebreak logic, but not the contract that two
//! *real* agent contexts sharing one perspective converge on the same winner
//! deterministically — the property the neighbourhood auto-processor rests on.
//!
//! These tests build a second real `AgentContext` (via `for_user_email` +
//! `ensure_user_key_exists`) and drive both against the same in-memory
//! perspective. That reproduces the sync-converged view the neighbourhood
//! provides — every peer eventually sees every peer's claim — without needing
//! an actual Kitsune/link-language round-trip.
//!
//! Covers three invariants:
//!  1. **Same processor + same (unpartitioned) batch → exactly one winner**,
//!     agreed on by both agents once both claims are visible (deterministic
//!     min-DID tiebreak).
//!  2. **Same processor + same partition → exactly one winner**,
//!     using [`super::claim::try_claim_for_partition`].
//!  3. **Same processor + DIFFERENT partitions → BOTH win in parallel**,
//!     because `batch_key_for_partition` puts each partition in its own claim
//!     key-space (parallel-claim safety, spec §6.5).
//!
//! Zero interaction with `watcher.rs` / engine surface — pure composition of
//! the P-A primitives with real second-agent DIDs.

#![cfg(test)]

use super::claim::{
    active_claimants, batch_key, batch_key_for_partition, try_claim, try_claim_for_partition,
    ClaimOutcome,
};
use crate::agent::{did_for_context, AgentContext, AgentService};
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::perspective_instance::PerspectiveInstance;

const ALICE_EMAIL: &str = "alice-spike@auto-processor.test";
const BOB_EMAIL: &str = "bob-spike@auto-processor.test";

/// Bring up the standard no-LLM perspective and mint a second real agent
/// context (alice) alongside the built-in main-agent context.
///
/// Returns `(perspective, main_context, alice_context)` — both contexts have
/// distinct DIDs derived from their own wallet keys. The perspective is
/// shared: both contexts can `add_links` on it and read each other's writes.
async fn setup_two_agent_perspective() -> (PerspectiveInstance, AgentContext, AgentContext) {
    let (perspective, _shapes, main_ctx) = setup_perspective_no_llm(&[]).await;

    AgentService::ensure_user_key_exists(ALICE_EMAIL).expect("mint alice key");
    let alice_ctx = AgentContext::for_user_email(ALICE_EMAIL.to_string());
    perspective
        .ensure_prolog_engine_pool_for_context(&alice_ctx)
        .await
        .expect("alice prolog pool");

    (perspective, main_ctx, alice_ctx)
}

/// Three-agent variant used by the "converge" contention test — we want two
/// candidate claimants over one batch AND a third distinct context so we can
/// assert nothing surprising happens with the winner identity.
async fn setup_three_agent_perspective() -> (
    PerspectiveInstance,
    AgentContext,
    AgentContext,
    AgentContext,
) {
    let (perspective, main_ctx, alice_ctx) = setup_two_agent_perspective().await;

    AgentService::ensure_user_key_exists(BOB_EMAIL).expect("mint bob key");
    let bob_ctx = AgentContext::for_user_email(BOB_EMAIL.to_string());
    perspective
        .ensure_prolog_engine_pool_for_context(&bob_ctx)
        .await
        .expect("bob prolog pool");

    (perspective, main_ctx, alice_ctx, bob_ctx)
}

/// Two real agent contexts on the same processor + same (unpartitioned) batch:
/// after both have written their claims, the sync-converged view names exactly
/// one winner — the lexicographically smallest DID — and both contexts agree.
#[tokio::test]
async fn two_contexts_on_same_batch_converge_to_min_did_winner() {
    let (mut perspective, main_ctx, alice_ctx) = setup_two_agent_perspective().await;
    let main_did = did_for_context(&main_ctx).expect("main did");
    let alice_did = did_for_context(&alice_ctx).expect("alice did");
    assert_ne!(main_did, alice_did, "distinct DIDs are required");

    let processor = "proc-two-context-unpartitioned";
    let items = vec!["i1".to_string(), "i2".to_string(), "i3".to_string()];
    let key = batch_key(&items);
    let now: i64 = 1_000;
    let ttl: i64 = 60_000;

    let main_outcome = try_claim(&mut perspective, processor, &items, ttl, now, &main_ctx)
        .await
        .expect("main try_claim");
    let alice_outcome = try_claim(&mut perspective, processor, &items, ttl, now, &alice_ctx)
        .await
        .expect("alice try_claim");

    // Sync-converged view: after both writes are visible, `active_claimants`
    // returns both DIDs, so subsequent reads from either context agree on
    // the same min-DID winner regardless of who wrote first.
    let holders_from_main = active_claimants(&perspective, processor, &key, now)
        .await
        .expect("holders (main view)");
    let holders_from_alice = active_claimants(&perspective, processor, &key, now)
        .await
        .expect("holders (alice view)");
    assert_eq!(
        holders_from_main, holders_from_alice,
        "both contexts must see the same set of claimants once both claims are written"
    );
    assert_eq!(
        holders_from_main.len(),
        2,
        "both DIDs must be visible; got {holders_from_main:?}"
    );

    let expected_winner = std::cmp::min(main_did.clone(), alice_did.clone());
    let converged_winner = holders_from_main.first().expect("winner").clone();
    assert_eq!(
        converged_winner, expected_winner,
        "sync-converged winner must be the smaller DID"
    );

    // The winning-side's `try_claim` must have returned `Won`. The losing
    // side's outcome depends on write ordering: if they wrote second, they
    // saw the winner's claim and backed off. If they wrote first, they saw
    // only themselves and returned `Won` — but that stale view is corrected
    // on any subsequent `active_claimants` read (asserted above).
    let (winner_outcome, loser_outcome) = if expected_winner == main_did {
        (&main_outcome, &alice_outcome)
    } else {
        (&alice_outcome, &main_outcome)
    };
    assert_eq!(
        winner_outcome,
        &ClaimOutcome::Won,
        "smaller-DID context must always return Won from its own try_claim"
    );
    match loser_outcome {
        ClaimOutcome::Won => { /* first-writer stale-view path */ }
        ClaimOutcome::BackedOff { holder } => assert_eq!(
            holder, &expected_winner,
            "larger-DID context must back off to the smaller DID"
        ),
    }
}

/// Same as above, but on the same processor + same *partition*, using the
/// partition-aware wrapper. Verifies `try_claim_for_partition` inherits the
/// same converged min-DID guarantee.
#[tokio::test]
async fn two_contexts_on_same_partition_converge_to_min_did_winner() {
    let (mut perspective, main_ctx, alice_ctx) = setup_two_agent_perspective().await;
    let main_did = did_for_context(&main_ctx).expect("main did");
    let alice_did = did_for_context(&alice_ctx).expect("alice did");

    let processor = "proc-two-context-partitioned";
    let partition = "payments";
    let items = vec!["m1".to_string(), "m2".to_string()];
    let key = batch_key_for_partition(partition, &items);
    let now: i64 = 5_000;
    let ttl: i64 = 60_000;

    let _ = try_claim_for_partition(
        &mut perspective,
        processor,
        partition,
        &items,
        ttl,
        now,
        &main_ctx,
    )
    .await
    .expect("main try_claim_for_partition");
    let _ = try_claim_for_partition(
        &mut perspective,
        processor,
        partition,
        &items,
        ttl,
        now,
        &alice_ctx,
    )
    .await
    .expect("alice try_claim_for_partition");

    let holders = active_claimants(&perspective, processor, &key, now)
        .await
        .expect("holders");
    assert_eq!(
        holders.len(),
        2,
        "both DIDs must be visible on the partitioned batch key; got {holders:?}"
    );

    let expected_winner = std::cmp::min(main_did.clone(), alice_did.clone());
    assert_eq!(
        holders.first().expect("winner"),
        &expected_winner,
        "sync-converged winner on the partitioned batch key must be the smaller DID"
    );
}

/// Same processor, *different* partitions: because `batch_key_for_partition`
/// hashes the partition into the key, each partition lives in its own claim
/// key-space, so two agents can claim in parallel — both win their own
/// partition, neither can starve the other.
///
/// This is the property that lets a wildcard/partitioned processor scale
/// horizontally across a neighbourhood (spec §6.5).
#[tokio::test]
async fn two_contexts_on_different_partitions_both_win_in_parallel() {
    let (mut perspective, main_ctx, alice_ctx) = setup_two_agent_perspective().await;
    let main_did = did_for_context(&main_ctx).expect("main did");
    let alice_did = did_for_context(&alice_ctx).expect("alice did");

    let processor = "proc-two-context-parallel-partitions";
    let items_p1 = vec!["p1-msg1".to_string(), "p1-msg2".to_string()];
    let items_p2 = vec!["p2-msg1".to_string(), "p2-msg2".to_string()];
    let now: i64 = 2_000;
    let ttl: i64 = 60_000;

    // Main claims partition "payments"; alice claims partition "onboarding".
    let main_outcome = try_claim_for_partition(
        &mut perspective,
        processor,
        "payments",
        &items_p1,
        ttl,
        now,
        &main_ctx,
    )
    .await
    .expect("main try_claim_for_partition");
    let alice_outcome = try_claim_for_partition(
        &mut perspective,
        processor,
        "onboarding",
        &items_p2,
        ttl,
        now,
        &alice_ctx,
    )
    .await
    .expect("alice try_claim_for_partition");

    assert_eq!(
        main_outcome,
        ClaimOutcome::Won,
        "main must win 'payments'; got {main_outcome:?}"
    );
    assert_eq!(
        alice_outcome,
        ClaimOutcome::Won,
        "alice must win 'onboarding'; got {alice_outcome:?}"
    );

    // Verify from the shared perspective that each partition has exactly its
    // own claimant — no cross-partition leakage.
    let key_p1 = batch_key_for_partition("payments", &items_p1);
    let key_p2 = batch_key_for_partition("onboarding", &items_p2);
    let holders_p1 = active_claimants(&perspective, processor, &key_p1, now)
        .await
        .expect("holders p1");
    let holders_p2 = active_claimants(&perspective, processor, &key_p2, now)
        .await
        .expect("holders p2");
    assert_eq!(
        holders_p1,
        vec![main_did.clone()],
        "'payments' key-space must contain only main's DID"
    );
    assert_eq!(
        holders_p2,
        vec![alice_did.clone()],
        "'onboarding' key-space must contain only alice's DID"
    );
}

/// Three real agent contexts contending on the same partition converge on the
/// same smallest-DID winner. Confirms the min-DID tiebreak's transitivity holds
/// with more than two claimants — the property #peer-count-independent that
/// the neighbourhood spec relies on for scale.
#[tokio::test]
async fn three_contexts_on_same_partition_converge_to_min_did_winner() {
    let (mut perspective, main_ctx, alice_ctx, bob_ctx) = setup_three_agent_perspective().await;
    let main_did = did_for_context(&main_ctx).expect("main did");
    let alice_did = did_for_context(&alice_ctx).expect("alice did");
    let bob_did = did_for_context(&bob_ctx).expect("bob did");

    let processor = "proc-three-context-partitioned";
    let partition = "shared";
    let items = vec!["t1".to_string(), "t2".to_string()];
    let key = batch_key_for_partition(partition, &items);
    let now: i64 = 3_000;
    let ttl: i64 = 60_000;

    for ctx in [&main_ctx, &alice_ctx, &bob_ctx] {
        let _ = try_claim_for_partition(
            &mut perspective,
            processor,
            partition,
            &items,
            ttl,
            now,
            ctx,
        )
        .await
        .expect("try_claim_for_partition");
    }

    let holders = active_claimants(&perspective, processor, &key, now)
        .await
        .expect("holders");
    assert_eq!(
        holders.len(),
        3,
        "all three DIDs must be visible on the shared partition"
    );

    let mut all_dids = vec![main_did.clone(), alice_did.clone(), bob_did.clone()];
    all_dids.sort();
    let expected_winner = all_dids.first().expect("winner").clone();
    assert_eq!(
        holders.first().expect("holder"),
        &expected_winner,
        "sync-converged winner across 3 claimants must be the smallest DID"
    );
    // And the full holders vector must equal the sorted DID set — no ghosts
    // and no duplicates from cross-context writes.
    assert_eq!(
        holders, all_dids,
        "active_claimants must equal the sorted set of claimant DIDs"
    );
}

//! Regression test for the chunked-diff validation storm bug.
//!
//! ## The bug
//!
//! When a peer's DHT holds `StoreRecord` ops for chunked
//! `PerspectiveDiffEntryReference` entries whose chunk action hashes
//! are permanently unreachable (e.g. author committed parent + chunks,
//! the parents gossipped but the chunks didn't, then the author went
//! offline / wiped state), the integrity zome's `validate()` callback
//! returns `UnresolvedDependencies` on every retry.
//!
//! Holochain's `app_validation_workflow` re-queues such ops on a
//! cadence that decreases with the number of missing ops:
//!
//!   interval_ms = 2900.saturating_sub(missing * 100) + 100
//!
//! With more than ~29 stuck ops the interval floors at 100ms — i.e.
//! every stuck op gets re-validated ~10x/sec, indefinitely. There is
//! no abandon logic (see TODO in `validation_query.rs` around the
//! `LIMIT 10000` query). One production incident showed hundreds of
//! stuck ops generating ~190 WARN lines/sec indefinitely.
//!
//! ## What this test does
//!
//! 1. Spin up two networked conductors (Alice, Bob).
//! 2. Alice commits several chunked diffs in sequence (each diff is
//!    `> CHUNKING_THRESHOLD = 500` link expressions, which produces
//!    one parent action + one chunk action per commit on Alice's
//!    source chain).
//! 3. Read each parent action+entry+signature from Alice's authored DB.
//! 4. Construct `ChainOp::StoreRecord(sig, parent_action, RecordEntry::Present(entry))`
//!    for each parent and inject them directly into Bob's DHT db via
//!    `insert_op_dht`. Bob now holds N parent ops but *no* chunk ops.
//! 5. Poll Bob's DHT db every 1s for ~25 seconds, summing
//!    `num_validation_attempts` across all injected ops.
//!
//! ## Pass / fail
//!
//! Without the fix, validate() returns `UnresolvedDependencies` on
//! every run. The aggregate attempt count climbs without bound: with
//! N=30 stuck ops the workflow runs every ~100ms, so over 25s each op
//! gets ~250 attempts — total ~7500.
//!
//! With the fix, validate() ignores chunk reachability and returns
//! `Valid` (modulo the parent dependency which we satisfy via gossip),
//! so the ops reach a terminal state quickly and total attempts stay
//! low (well under 100 across all ops).
//!
//! The assertion is "average attempts per op < 4" — generous enough
//! to absorb transient sys-validation retries while still failing
//! decisively in the storm scenario.

use crate::utils::*;
use holochain::sweettest::SweetConductor;
use holochain_state::prelude::insert_op_dht;
use holochain_types::dht_op::{ChainOp, DhtOpHashed};
use holochain_types::prelude::*;
use perspective_diff_sync_integrity::PerspectiveDiffEntryReference;
use std::time::{Duration, Instant};

/// Number of chunked parent ops to inject. Must exceed the
/// `app_validation_workflow` "many missing → 100ms interval" threshold
/// (which kicks in around 29 missing ops) so we actually trigger the
/// pathological retry rate, not the slow happy-path retry.
const N_INJECTED_OPS: usize = 35;

/// Sum num_validation_attempts across a known set of op hashes on a
/// DHT db (separated from the conductor handle so we can drop the
/// authoring conductor while still querying the holder's state).
fn sum_validation_attempts(
    dht_db: &DbWrite<DbKindDht>,
    op_hashes: &[DhtOpHash],
) -> (i64, usize, usize) {
    let hashes: Vec<DhtOpHash> = op_hashes.to_vec();
    dht_db.test_read(move |txn| {
        let mut total_attempts: i64 = 0;
        let mut still_pending = 0usize;
        let mut terminal = 0usize;
        for h in &hashes {
            if let Ok((stage, status, attempts)) = txn.query_row(
                "SELECT validation_stage, validation_status, num_validation_attempts
                 FROM DhtOp
                 WHERE hash = :hash",
                rusqlite::named_params! { ":hash": h },
                |row| {
                    Ok((
                        row.get::<_, Option<i64>>(0)?,
                        row.get::<_, Option<i64>>(1)?,
                        row.get::<_, Option<i64>>(2)?,
                    ))
                },
            ) {
                total_attempts += attempts.unwrap_or(0);
                // Terminal = integration limbo cleared and a status is recorded.
                if stage.is_none() && status.is_some() {
                    terminal += 1;
                } else {
                    still_pending += 1;
                }
            }
        }
        (total_attempts, terminal, still_pending)
    })
}

/// Read the parent's signed action + entry from a cell's authored DB.
fn read_authored_record(
    cell: &holochain::sweettest::SweetCell,
    action_hash: ActionHash,
) -> Option<(SignedActionHashed, Entry)> {
    let authored_db = cell.authored_db().clone();
    let ah = action_hash.clone();
    let row = authored_db.test_read(move |txn| {
        txn.query_row(
            "SELECT
                Action.blob AS action_blob,
                Entry.blob  AS entry_blob
             FROM Action
             LEFT JOIN Entry ON Entry.hash = Action.entry_hash
             WHERE Action.hash = :hash",
            rusqlite::named_params! { ":hash": ah },
            |row| {
                let action_blob: Vec<u8> = row.get("action_blob")?;
                let entry_blob: Option<Vec<u8>> = row.get("entry_blob")?;
                Ok((action_blob, entry_blob))
            },
        )
        .ok()
    });

    let (action_blob, entry_blob) = row?;
    // The Action blob is encoded as a 2-field (Action, Signature) struct.
    let signed_action: SignedAction = holochain_serialized_bytes::decode(&action_blob).ok()?;
    let (action, signature): (Action, Signature) = signed_action.into();
    let action_hashed = ActionHashed::from_content_sync(action);
    let signed_action_hashed = SignedActionHashed::with_presigned(action_hashed, signature);
    let entry: Entry = match entry_blob {
        Some(blob) => holochain_serialized_bytes::decode(&blob).ok()?,
        None => return None,
    };
    Some((signed_action_hashed, entry))
}

#[tokio::test(flavor = "multi_thread")]
async fn test_chunked_diff_with_missing_chunks_does_not_storm_validation() {
    // Useful to see app_validation_workflow tracing when run with --nocapture.
    let _ = holochain_trace::test_run();

    // Two networked conductors. Networking is needed so Alice's
    // RegisterAgentActivity ops can gossip to Bob — otherwise Bob's
    // sys-validation of the injected parent StoreRecord would loop
    // forever on a missing prev_action and we'd never reach app
    // validation at all.
    let (conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    // Settle peer discovery / let agent-activity ops gossip.
    for _ in 0..3 {
        conductors.exchange_peer_info().await;
        tokio::time::sleep(Duration::from_millis(1500)).await;
    }

    // 1. Alice commits N chunked diffs.
    println!(
        "=== Alice commits {} chunked diffs (600 links each) ===",
        N_INJECTED_OPS
    );
    let mut alice_parent_hashes: Vec<ActionHash> = Vec::with_capacity(N_INJECTED_OPS);
    for i in 0..N_INJECTED_OPS {
        let h: ActionHash = call_zome(
            &conductors[0],
            alice_cell,
            "commit",
            create_commit_input_multi("alice", 600),
        )
        .await;
        if i == 0 || i + 1 == N_INJECTED_OPS {
            println!("  commit {}: parent hash {}", i, h);
        }
        alice_parent_hashes.push(h);
    }

    // Sanity: all parents are chunked.
    let first_ref: PerspectiveDiffEntryReference = call_zome(
        &conductors[0],
        alice_cell,
        "get_diff_entry_reference",
        alice_parent_hashes[0].clone(),
    )
    .await;
    assert!(
        first_ref.is_chunked(),
        "alice's parent entries must be chunked for this test"
    );

    let dna_hash = alice_cell.dna_hash().clone();
    let bob_dht_db = conductors[1]
        .get_dht_db(&dna_hash)
        .expect("Bob must have a DHT db");

    // 2 + 3. Build StoreRecord ops for each parent and inject into Bob's DHT.
    let mut injected_op_hashes: Vec<DhtOpHash> = Vec::with_capacity(N_INJECTED_OPS);
    for action_hash in &alice_parent_hashes {
        let (signed_action, entry) = read_authored_record(alice_cell, action_hash.clone())
            .expect("must be able to read parent record from Alice's authored DB");
        let signature = signed_action.signature().clone();
        let action: Action = signed_action.action().clone();

        let store_record_op = ChainOp::StoreRecord(signature, action, RecordEntry::Present(entry));
        let dht_op_hashed = DhtOpHashed::from_content_sync(store_record_op);
        let op_hash = dht_op_hashed.as_hash().clone();
        injected_op_hashes.push(op_hash.clone());

        let op_for_insert = dht_op_hashed.clone();
        // Best-effort: if gossip happens to have delivered the op already
        // (entry blob unique-constraint conflict), that's fine — the test
        // just cares about ops being present, not who inserted them.
        let _ = bob_dht_db.test_write(move |txn| insert_op_dht(txn, &op_for_insert, 0, None));
    }
    println!(
        "Injected {} StoreRecord ops into Bob's DHT (no corresponding chunks)",
        injected_op_hashes.len()
    );

    // 3b. Take Bob's conductor out of the batch, then drop Alice's so the
    // chunks become unreachable. Bob's app_validation_workflow now has
    // a real cascade miss for every chunk reference, which is the
    // production scenario (author committed chunks+parent, then went
    // offline before chunks gossipped to peers).
    //
    // Without this drop, Bob's cascade can fetch the chunks from Alice
    // over rendezvous and the storm never triggers — defeating the
    // regression check.
    println!("Dropping Alice's conductor so chunks become unreachable...");
    let mut conductor_vec: Vec<SweetConductor> = conductors.into();
    let _bob_conductor = conductor_vec.pop().expect("Bob"); // hold on so Bob keeps running
    let alice_conductor = conductor_vec.pop().expect("Alice");
    drop(alice_conductor);

    // 4. Poll Bob's DHT db for ~25s, tracking aggregate validation attempts.
    let poll_interval = Duration::from_millis(1000);
    let window = Duration::from_secs(25);
    let deadline = Instant::now() + window;

    let mut samples: Vec<(Duration, i64, usize, usize)> = Vec::new();
    let start = Instant::now();
    while Instant::now() < deadline {
        let (total_attempts, terminal, pending) =
            sum_validation_attempts(&bob_dht_db, &injected_op_hashes);
        samples.push((start.elapsed(), total_attempts, terminal, pending));
        tokio::time::sleep(poll_interval).await;
    }

    println!("\n=== Validation activity timeline ===");
    for (elapsed, total, terminal, pending) in &samples {
        println!(
            "  t={:>5}ms  total_attempts={:>6}  terminal={:>3}  pending={:>3}",
            elapsed.as_millis(),
            total,
            terminal,
            pending
        );
    }

    let final_sample = samples.last().expect("at least one sample");
    let final_attempts = final_sample.1;
    let final_terminal = final_sample.2;
    let final_pending = final_sample.3;
    let avg_attempts = final_attempts as f64 / N_INJECTED_OPS as f64;

    println!(
        "\nFinal: total_attempts={} avg_per_op={:.2} terminal={} pending={}",
        final_attempts, avg_attempts, final_terminal, final_pending
    );

    // Hard pass criteria — TWO axes:
    //
    //   (a) Average attempts per op must stay below 4.
    //       With the fix: avg ≈ 2 (one sys-validation cycle + one app
    //       validation cycle to reach Valid).
    //       Without the fix: avg ≈ 10+ (app validation re-queues each
    //       op multiple times per second for the duration of the window).
    //
    //   (b) `pending` (ops still in validation limbo) must be 0 at the
    //       end of the window.
    //       With the fix: every op terminates fast.
    //       Without the fix: some ops are stuck forever in AwaitingAppDeps.
    //
    // Either axis catches the storm decisively. The combined check is
    // robust against gossip racing in some chunks during the brief
    // window where Alice was online.
    assert!(
        avg_attempts < 4.0,
        "Average validation attempts per op = {:.2} (total {} over {} ops in {}s) — \
         too many retries means the chunked-diff validation storm is still happening. \
         Sample timeline above shows the climb. Without the fix the workflow re-runs \
         every ~100ms when many ops have unresolved deps, pushing the average up.",
        avg_attempts,
        final_attempts,
        N_INJECTED_OPS,
        window.as_secs()
    );
    assert_eq!(
        final_pending,
        0,
        "{} of {} ops still stuck in validation limbo after {}s — without the fix \
         these ops loop forever in AwaitingAppDeps because chunks are permanently \
         unreachable. With the fix every op terminates promptly.",
        final_pending,
        N_INJECTED_OPS,
        window.as_secs()
    );

    // Bob's conductor is dropped at end of scope; explicit shutdown not
    // required because SweetConductor's Drop handles cleanup.
    drop(_bob_conductor);
}

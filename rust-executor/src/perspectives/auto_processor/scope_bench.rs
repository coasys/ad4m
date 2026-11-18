//! Scope cost of the model-query transcript read (#1794), against the
//! free-SPARQL scope it replaced. `#[ignore]`d — run by hand, in release:
//!
//! ```text
//! cargo test --release -p ad4m-executor scope_bench -- --ignored --nocapture
//! ```
//!
//! For each scope size N it reports, median of 5 runs:
//!  (a) `scan`: a tick re-checking an N-turn pending queue over the kept
//!      reads — in-memory dedup + debounce bookkeeping, no store query;
//!  (b) `idle`: a tick with an empty queue. The cursor is seeded so every
//!      turn counts as processed and the warm-up tick must report `Idle`
//!      before the measurement runs; `q` counts the store reads one idle
//!      tick makes;
//!  (c) `out`/`in`: `auto_processor_writes_match` over the `Writes` of real
//!      signed links — an unrelated predicate, then a fresh message's own
//!      flag + body — the link-aware path the commit feed takes, not the
//!      predicate-name shortcut;
//!  (d) `model`/`sparql`: one transcript read both ways —
//!      `gather_transcript_model` over `cfg.transcript_query_json()` vs the
//!      old scope query plus its `proofValid` cut, rows decoded into
//!      `TranscriptTurn`s; both must return N turns;
//!  (e) `+1k`: matching the `Writes` the burst's own 1,000 messages produced
//!      (2,000 links), then the re-read over the grown scope.
//!
//! `N=parent` is the WE case: a parent-scoped processor anchored on one
//! 100-message conversation inside a perspective of 50,000 messages in
//! other conversations reads only its scope.

use super::config::{processor_node, write_processor, AutoProcessorConfig};
use super::watcher::{TickOutcome, WatcherState};
use crate::agent::signatures::TestSigner;
use crate::perspectives::interpretation::{
    decode_transcript_body, ensure_interpretation_overlay_classes, gather_transcript_model,
    TranscriptTurn,
};
use crate::perspectives::interpretation_test_support::{setup_perspective_no_llm, MESSAGE_SDNA};
use crate::perspectives::perspective_instance::subscription_triggers::{writes_of, Writes};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::sparql_store::EvalStop;
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkStatus};
use chrono::{DateTime, Utc};
use std::time::Instant;

/// The scope query the pre-#1794 config ran verbatim — the baseline — plus
/// the `proofValid` cut the model query applies before hydration, so both
/// sides report the same turn set.
const OLD_SCOPE_SPARQL: &str = "SELECT ?speaker ?text ?timestamp WHERE {
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
  ?r <ad4m://ontology/proofValid> \"true\" .
}
ORDER BY ?timestamp";

fn median(mut v: Vec<u128>) -> u128 {
    v.sort();
    v[v.len() / 2]
}

/// One signed store link. `TestSigner` signs for real so the rows survive
/// the provenance verification the model query applies — the fixture is
/// measured under the same cut production reads are.
fn signed(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: String,
    at: DateTime<Utc>,
) -> LinkExpression {
    let signed = signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target,
        }
        .normalize(),
        at,
    );
    LinkExpression {
        author: signed.author,
        timestamp: signed.timestamp,
        data: signed.data,
        proof: signed.proof,
        status: Some(LinkStatus::Local),
    }
}

/// `count` conforming `ns://Message` instances signed for real by `signer`,
/// written straight into the store (the bulk path — per-link `add_link`
/// would put the commit feed and event publish in the seed cost). Returns
/// each written link decorated, so a caller can build the `Writes` a commit
/// of exactly this batch would carry.
fn seed(
    p: &PerspectiveInstance,
    signer: &TestSigner,
    from: usize,
    count: usize,
) -> Vec<DecoratedLinkExpression> {
    let start = Utc::now() - chrono::Duration::minutes(10);
    let mut links = Vec::with_capacity(count * 2);
    for i in from..from + count {
        let msg = format!("msg://{i}");
        let at = start + chrono::Duration::milliseconds(i as i64);
        for (predicate, target) in [
            ("ns://type", "ns://message".to_string()),
            ("ns://body", format!("literal:string:message {i}")),
        ] {
            let link = signed(signer, &msg, predicate, target, at);
            p.sparql_store.add_link(&link).expect("seed link");
            links.push(DecoratedLinkExpression::from((link, LinkStatus::Local)));
        }
    }
    links
}

/// `count` messages under `conv` through `ns://child`, the way a scoped
/// transcript's membership edges look. Same bulk path as [`seed`].
fn seed_conversation(
    p: &PerspectiveInstance,
    signer: &TestSigner,
    conv: &str,
    from: usize,
    count: usize,
) -> Vec<DecoratedLinkExpression> {
    let start = Utc::now() - chrono::Duration::minutes(10);
    let mut links = Vec::with_capacity(count * 3);
    for i in from..from + count {
        let msg = format!("{conv}/msg/{i}");
        let at = start + chrono::Duration::milliseconds(i as i64);
        let edge = signed(signer, conv, "ns://child", msg.clone(), at);
        p.sparql_store.add_link(&edge).expect("seed child edge");
        links.push(DecoratedLinkExpression::from((edge, LinkStatus::Local)));
        for (predicate, target) in [
            ("ns://type", "ns://message".to_string()),
            ("ns://body", format!("literal:string:{conv} message {i}")),
        ] {
            let link = signed(signer, &msg, predicate, target, at);
            p.sparql_store.add_link(&link).expect("seed link");
            links.push(DecoratedLinkExpression::from((link, LinkStatus::Local)));
        }
    }
    links
}

/// An `InterpretationRun` covering every `id` — what a completed pass writes —
/// so the tick sees each turn as already processed and the queue stays empty.
/// The links land straight in the store like [`seed`]'s, signed so the
/// cursor's own verified read keeps them.
fn seed_cursor(p: &PerspectiveInstance, signer: &TestSigner, processor_id: &str, ids: &[String]) {
    let run = format!("ad4m://interp/run/{processor_id}-cursor");
    let at = Utc::now();
    for (predicate, target) in [
        (
            "ad4m://interp/run_id",
            format!("literal:string:{processor_id}-cursor"),
        ),
        (
            "ad4m://interp/ran_at",
            format!("literal:string:{}", at.timestamp_millis()),
        ),
        ("ad4m://interp/processor", processor_node(processor_id)),
    ] {
        p.sparql_store
            .add_link(&signed(signer, &run, predicate, target, at))
            .expect("cursor link");
    }
    for id in ids {
        p.sparql_store
            .add_link(&signed(
                signer,
                &run,
                "ad4m://interp/sources",
                format!("literal:string:{id}"),
                at,
            ))
            .expect("cursor source");
    }
}

/// `Writes` over `links` — the same batch `record_changed_predicates` hands
/// the commit feed — so trigger matching exercises the link-aware rules
/// (`joins`, `scan`, `near`), not only predicate names.
fn writes(links: &[DecoratedLinkExpression]) -> Writes {
    writes_of(links.iter()).expect("every seeded link carries a predicate")
}

/// `gather_transcript_sparql`'s row conversion at c0441ac4c~1, verbatim:
/// `?text` may be a plain literal, a `literal:` URI or an expression
/// envelope carrying a string `data`; a row binding speaker+text but not
/// timestamp failed the whole gather.
fn old_scope_turns(rows: &[serde_json::Value]) -> Vec<TranscriptTurn> {
    let mut out = Vec::with_capacity(rows.len());
    for row in rows {
        let (Some(speaker), Some(raw_text)) = (
            row.get("speaker").and_then(|v| v.as_str()),
            row.get("text").and_then(|v| v.as_str()),
        ) else {
            continue;
        };
        let timestamp = row
            .get("timestamp")
            .and_then(|v| v.as_str())
            .filter(|s| !s.is_empty())
            .expect("the scope query binds ?timestamp on every row");
        let text = if raw_text.starts_with("literal:") {
            match decode_transcript_body(raw_text) {
                Some(s) => s,
                None => continue, // non-string literal — not a message body
            }
        } else if let Some(data) = serde_json::from_str::<serde_json::Value>(raw_text)
            .ok()
            .and_then(|v| v.get("data").and_then(|d| d.as_str()).map(String::from))
        {
            data
        } else {
            raw_text.to_string()
        };
        out.push(TranscriptTurn {
            speaker: speaker.to_string(),
            text,
            timestamp: timestamp.to_string(),
        });
    }
    out
}

/// The baseline read the old scope ran, decoded into `TranscriptTurn`s the
/// same way `gather_transcript_sparql` decoded them.
fn old_scope_read(p: &PerspectiveInstance) -> Vec<TranscriptTurn> {
    let json = p
        .sparql_store
        .query_arbitrary(&EvalStop::none(), OLD_SCOPE_SPARQL)
        .expect("sparql read");
    let rows: Vec<serde_json::Value> = serde_json::from_str(&json).expect("rows json");
    old_scope_turns(&rows)
}

fn bench_config(processor_id: &str, source_query: Option<String>) -> AutoProcessorConfig {
    AutoProcessorConfig {
        processor_id: processor_id.into(),
        source_class: "ns://Message".into(),
        source_text: "body".into(),
        source_query,
        interpretation_classes: vec!["ns://Task".into()],
        debounce_ms: 600_000,
        batch_min: 1,
        batch_max: 32,
        claim_ttl_ms: 60_000,
        ..Default::default()
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
#[ignore = "benchmark: run by hand in release"]
async fn scope_read_and_wake_costs() {
    println!(
        "{:>7} | {:>12} | {:>16} | {:>16} | {:>14} | {:>14} | {:>14}",
        "N",
        "pending scan ms",
        "idle tick (q, ms)",
        "out-of-scope µs",
        "in-scope µs",
        "model read ms",
        "sparql read ms"
    );
    for n in [1_000usize, 10_000, 50_000] {
        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        let signer = TestSigner::generate();
        let seeded = Instant::now();
        seed(&p, &signer, 0, n);
        println!("seeded {n} messages in {:?}", seeded.elapsed());

        let cfg = bench_config("bench", None);
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");
        let now_ms = Utc::now().timestamp_millis();

        // (a) the pending-queue scan: N unprocessed turns queue on the first
        // tick, and each later tick re-records all of them off the kept
        // reads — the in-memory cost of one busy queue, measured on its own.
        let mut watcher = WatcherState::new();
        let outcome = p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;
        assert_eq!(
            outcome,
            TickOutcome::Pending,
            "N unprocessed turns must queue"
        );
        assert_eq!(
            watcher.pending_for("bench").map(|e| e.items.len()),
            Some(n),
            "every turn must be pending"
        );
        let mut scan_ms = Vec::new();
        for i in 0..5 {
            let t = Instant::now();
            p.run_auto_processor_tick(&mut watcher, now_ms + 60_000 * (i + 1), &ctx)
                .await;
            scan_ms.push(t.elapsed().as_millis());
        }

        // (b) the idle tick: seed the cursor so every turn reads as
        // processed, then a warm-up tick over an empty queue must report
        // Idle before the measured ticks run.
        ensure_interpretation_overlay_classes(&mut p, &ctx)
            .await
            .expect("overlay classes");
        let ids: Vec<String> = watcher
            .pending_for("bench")
            .expect("pending")
            .items
            .iter()
            .map(|t| t.id.clone())
            .collect();
        seed_cursor(&p, &signer, "bench", &ids);

        let mut watcher = WatcherState::new();
        let outcome = p
            .run_auto_processor_tick(&mut watcher, now_ms + 400_000, &ctx)
            .await;
        assert_eq!(
            outcome,
            TickOutcome::Idle,
            "an all-processed transcript must queue nothing"
        );
        let mut idle_ms = Vec::new();
        let mut idle_queries = 0usize;
        for i in 0..5 {
            let q0 = p.sparql_store.queries_run().len() + p.sparql_store.link_scans_run();
            let t = Instant::now();
            p.run_auto_processor_tick(&mut watcher, now_ms + 460_000 + 60_000 * i, &ctx)
                .await;
            idle_ms.push(t.elapsed().as_millis());
            idle_queries =
                p.sparql_store.queries_run().len() + p.sparql_store.link_scans_run() - q0;
        }

        // (c) wake-path cost, on the `Writes` of real links. Out of scope:
        // an unrelated predicate. In scope: a fresh message's own flag+body
        // — the batch its commit would carry.
        let out_of_scope = writes(&[DecoratedLinkExpression::from((
            signed(
                &signer,
                "note://reactions",
                "ns://reaction",
                "literal:string:r1".into(),
                Utc::now(),
            ),
            LinkStatus::Local,
        ))]);
        let mut out_us = Vec::new();
        for _ in 0..5 {
            let t = Instant::now();
            let matched = p.auto_processor_writes_match(&watcher, &out_of_scope);
            out_us.push(t.elapsed().as_micros());
            assert!(!matched, "an unrelated write must not match");
        }

        // (d) the equivalent read both ways: the config's own transcript
        // query through the model path, and the old scope query — each must
        // return the same N turns.
        let query_json = cfg.transcript_query_json();
        let mut model_ms = Vec::new();
        for _ in 0..5 {
            let t = Instant::now();
            let turns =
                gather_transcript_model(&p, &cfg.source_class, &cfg.source_text, &query_json)
                    .await
                    .expect("model read")
                    .turns;
            model_ms.push(t.elapsed().as_millis());
            assert_eq!(turns.len(), n, "the model read must return all N turns");
        }
        let mut sparql_ms = Vec::new();
        for _ in 0..5 {
            let t = Instant::now();
            let turns = old_scope_read(&p);
            sparql_ms.push(t.elapsed().as_millis());
            assert_eq!(
                turns.len(),
                n,
                "the SPARQL baseline must return the same N turns"
            );
        }

        // One new in-scope message, committed as a real write would be —
        // its flag+body are the `Writes` the feed reports.
        let at = Utc::now();
        let new_links: Vec<DecoratedLinkExpression> = [
            ("ns://type", "ns://message".to_string()),
            ("ns://body", "literal:string:late arrival".to_string()),
        ]
        .into_iter()
        .map(|(predicate, target)| {
            let link = signed(&signer, "msg://new", predicate, target, at);
            p.sparql_store.add_link(&link).expect("commit new message");
            DecoratedLinkExpression::from((link, LinkStatus::Local))
        })
        .collect();
        let in_scope = writes(&new_links);
        let mut in_us = Vec::new();
        for _ in 0..5 {
            let t = Instant::now();
            let matched = p.auto_processor_writes_match(&watcher, &in_scope);
            in_us.push(t.elapsed().as_micros());
            assert!(matched, "a new in-scope message must match");
        }

        // (e) a burst of 1,000 in-scope messages lands as one commit batch:
        // the `Writes` of its own 2,000 links, then one full re-read.
        let burst_at = Instant::now();
        let burst_links = seed(&p, &signer, n + 1, 1_000);
        let burst_seed_ms = burst_at.elapsed().as_millis();
        let t = Instant::now();
        let burst_match = p.auto_processor_writes_match(&watcher, &writes(&burst_links));
        let burst_match_us = t.elapsed().as_micros();
        let t = Instant::now();
        let burst = gather_transcript_model(&p, &cfg.source_class, &cfg.source_text, &query_json)
            .await
            .expect("burst read")
            .turns;
        let burst_read_ms = t.elapsed().as_millis();
        assert!(burst_match, "the burst's own writes must match");
        assert_eq!(burst.len(), n + 1 + 1_000);

        println!(
            "{n:>7} | {:>12} | {:>6} {:>7} | {:>16} | {:>14} | {:>14} | {:>14}",
            median(scan_ms),
            idle_queries,
            median(idle_ms),
            median(out_us),
            median(in_us),
            median(model_ms),
            median(sparql_ms),
        );
        println!(
            "        burst +1k: seed {burst_seed_ms} ms, match {burst_match_us} µs, \
             1 read of {} turns in {burst_read_ms} ms",
            burst.len()
        );
    }

    // (d) the WE case: a path-scoped processor — the 100 messages under one
    // conversation (children, plus their `we://thread` replies) inside a
    // perspective of 50,000 messages belonging to other conversations.
    parent_scope_case().await;
}

/// One conversation's children and their replies, inside a 50,000-message
/// perspective whose other conversations the scope cannot reach.
async fn parent_scope_case() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
    let signer = TestSigner::generate();
    let seeded = Instant::now();
    // Fifty foreign conversations of 1,000 — anchors a `near` check walks
    // past and a scoped read never touches.
    for c in 0..50 {
        seed_conversation(&p, &signer, &format!("we://conv/{c}"), 0, 1_000);
    }
    // Ours: 80 direct children and 20 replies threaded off them.
    seed_conversation(&p, &signer, "we://conv", 0, 80);
    let start = Utc::now() - chrono::Duration::minutes(10);
    for j in 0..20 {
        let reply = format!("we://conv/reply/{j}");
        let at = start + chrono::Duration::milliseconds(1_000 + j as i64);
        for (source, predicate, target) in [
            (format!("we://conv/msg/{j}"), "we://thread", reply.clone()),
            (reply.clone(), "ns://type", "ns://message".to_string()),
            (
                reply.clone(),
                "ns://body",
                format!("literal:string:reply {j}"),
            ),
        ] {
            let link = signed(&signer, &source, predicate, target, at);
            p.sparql_store.add_link(&link).expect("seed reply");
        }
    }
    println!("seeded the parent-scoped fixture in {:?}", seeded.elapsed());

    let cfg = bench_config(
        "we",
        Some(r#"{"parent":{"ids":"we://conv","path":["ns://child","we://thread"]}}"#.into()),
    );
    write_processor(&mut p, &cfg, Some(false), &ctx)
        .await
        .expect("write_processor");
    let mut watcher = WatcherState::new();
    let now_ms = Utc::now().timestamp_millis();
    p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

    let query_json = cfg.transcript_query_json();
    let mut model_ms = Vec::new();
    for _ in 0..5 {
        let t = Instant::now();
        let turns = gather_transcript_model(&p, &cfg.source_class, &cfg.source_text, &query_json)
            .await
            .expect("scoped model read")
            .turns;
        model_ms.push(t.elapsed().as_millis());
        assert_eq!(
            turns.len(),
            100,
            "the path scope must read exactly its conversation"
        );
    }

    // The membership edge under the anchor wakes the loop; the same edge
    // under a foreign conversation cannot change the read, so it must not.
    let at = Utc::now();
    let in_scope_edge = writes(&[DecoratedLinkExpression::from((
        signed(
            &signer,
            "we://conv",
            "ns://child",
            "we://conv/msg/new".into(),
            at,
        ),
        LinkStatus::Local,
    ))]);
    let foreign_edge = writes(&[DecoratedLinkExpression::from((
        signed(
            &signer,
            "we://conv/0",
            "ns://child",
            "we://conv/0/msg/new".into(),
            at,
        ),
        LinkStatus::Local,
    ))]);
    let mut edge_us = Vec::new();
    for _ in 0..5 {
        let t = Instant::now();
        let matched = p.auto_processor_writes_match(&watcher, &in_scope_edge);
        edge_us.push(t.elapsed().as_micros());
        assert!(matched, "a child edge under the anchor must match");
    }
    let mut foreign_us = Vec::new();
    for _ in 0..5 {
        let t = Instant::now();
        let matched = p.auto_processor_writes_match(&watcher, &foreign_edge);
        foreign_us.push(t.elapsed().as_micros());
        assert!(
            !matched,
            "a child edge under another conversation must not match"
        );
    }

    println!(
        "{:>7} | {:>12} | {:>16} | {:>16} | {:>14} | {:>14} | {:>14}",
        "parent",
        "-",
        "-",
        median(foreign_us),
        median(edge_us),
        median(model_ms),
        "-",
    );
}

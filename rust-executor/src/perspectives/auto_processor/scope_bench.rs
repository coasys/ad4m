//! Scope cost of the model-query transcript read (#1794), against the
//! free-SPARQL scope it replaced. `#[ignore]`d — run by hand, in release:
//!
//! ```text
//! cargo test --release -p ad4m-executor scope_bench -- --ignored --nocapture
//! ```
//!
//! For each scope size N it reports, median of 5 runs:
//!  (a) an idle tick over an unchanged store (queries + wall time — the kept
//!      reads make it ~free),
//!  (b) an out-of-scope committed write (does the trigger rule it out, and
//!      the match cost — this is the whole wake-path cost when it does),
//!  (c) one new in-scope message (the match plus the transcript re-read;
//!      the same harness against the old scope's SPARQL for comparison),
//!  (d) a burst of 1,000 new in-scope messages (one commit batch → one wake
//!      → one full read).

use super::config::{write_processor, AutoProcessorConfig};
use super::watcher::WatcherState;
use crate::perspectives::interpretation::gather_transcript_model;
use crate::perspectives::interpretation_test_support::{setup_perspective_no_llm, MESSAGE_SDNA};
use crate::perspectives::perspective_instance::subscription_triggers::Writes;
use crate::perspectives::sparql_store::EvalStop;
use crate::types::{Link, LinkExpression, LinkStatus};
use std::time::Instant;

/// The scope query the pre-#1794 config ran verbatim — kept here only as the
/// comparison baseline, dead to every other code path.
const OLD_SCOPE_SPARQL: &str = "SELECT ?speaker ?text ?timestamp WHERE {
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
}
ORDER BY ?timestamp";

fn median(mut v: Vec<u128>) -> u128 {
    v.sort();
    v[v.len() / 2]
}

/// `count` conforming `ns://Message` instances signed for real by `signer`,
/// written straight into the store (the bulk path — per-link `add_link`
/// would put the commit feed and event publish in the seed cost).
fn seed(
    p: &crate::perspectives::perspective_instance::PerspectiveInstance,
    signer: &crate::agent::signatures::TestSigner,
    from: usize,
    count: usize,
) {
    let start = chrono::Utc::now() - chrono::Duration::minutes(10);
    for i in from..from + count {
        let at = start + chrono::Duration::milliseconds(i as i64);
        for (predicate, target) in [
            ("ns://type".to_string(), "ns://message".to_string()),
            (
                "ns://body".to_string(),
                format!("literal:string:message {i}"),
            ),
        ] {
            let data = Link {
                source: format!("msg://{i}"),
                predicate: Some(predicate),
                target,
            }
            .normalize();
            let signed = signer.sign_at(data, at);
            p.sparql_store
                .add_link(&LinkExpression {
                    author: signed.author,
                    timestamp: signed.timestamp,
                    data: signed.data,
                    proof: signed.proof,
                    status: Some(LinkStatus::Local),
                })
                .expect("seed link");
        }
    }
}

fn writes_of_predicates(predicates: &[&str]) -> Writes {
    Writes::from(
        predicates
            .iter()
            .map(|p| p.to_string())
            .collect::<std::collections::HashSet<String>>(),
    )
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
#[ignore = "benchmark: run by hand in release"]
async fn scope_read_and_wake_costs() {
    println!(
        "{:>7} | {:>22} | {:>26} | {:>22} | {:>22} | {:>22}",
        "N",
        "idle tick (q, ms)",
        "out-of-scope match µs",
        "in-scope match µs",
        "model read ms",
        "SPARQL read ms"
    );
    for n in [1_000usize, 10_000, 50_000] {
        let (mut p, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        let signer = crate::agent::signatures::TestSigner::generate();
        let seeded = Instant::now();
        seed(&p, &signer, 0, n);
        println!("seeded {n} messages in {:?}", seeded.elapsed());

        let cfg = AutoProcessorConfig {
            processor_id: "bench".into(),
            source_class: "ns://Message".into(),
            source_text: "body".into(),
            source_query: None,
            interpretation_classes: vec!["ns://Task".into()],
            debounce_ms: 600_000,
            batch_min: 1,
            batch_max: 32,
            claim_ttl_ms: 60_000,
            ..Default::default()
        };
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");

        let mut watcher = WatcherState::new();
        let now_ms = chrono::Utc::now().timestamp_millis();
        // The first tick builds the trigger and the kept reads.
        p.run_auto_processor_tick(&mut watcher, now_ms, &ctx).await;

        // (a) an idle tick at an unchanged generation.
        let mut idle_ms = Vec::new();
        let mut idle_queries = 0usize;
        for i in 0..5 {
            let q0 = p.sparql_store.queries_run().len() + p.sparql_store.link_scans_run();
            let t = Instant::now();
            p.run_auto_processor_tick(&mut watcher, now_ms + 60_000 * (i + 1), &ctx)
                .await;
            idle_ms.push(t.elapsed().as_millis());
            idle_queries =
                p.sparql_store.queries_run().len() + p.sparql_store.link_scans_run() - q0;
        }

        // (b) a committed write outside the scope: the trigger must rule it
        // out and that match is the entire wake-path cost.
        let out_of_scope = writes_of_predicates(&["ns://reaction"]);
        let mut out_ms = Vec::new();
        for _ in 0..5 {
            let t = Instant::now();
            let matched = p.auto_processor_writes_match(&watcher, &out_of_scope);
            out_ms.push(t.elapsed().as_micros());
            assert!(!matched, "an unrelated write must not match");
        }

        // (c) one in-scope write: the match cost, then the full re-read —
        // the model query and the old SPARQL on the same store.
        let in_scope = writes_of_predicates(&["ns://type", "ns://body"]);
        let mut in_ms = Vec::new();
        for _ in 0..5 {
            let t = Instant::now();
            let matched = p.auto_processor_writes_match(&watcher, &in_scope);
            in_ms.push(t.elapsed().as_micros());
            assert!(matched, "a scope predicate must match");
        }
        let mut model_ms = Vec::new();
        let mut sparql_ms = Vec::new();
        let mut turns = 0usize;
        for _ in 0..5 {
            let t = Instant::now();
            let transcript = gather_transcript_model(&p, "ns://Message", "body", "{}")
                .await
                .expect("model read")
                .turns;
            model_ms.push(t.elapsed().as_millis());
            turns = transcript.len();
            let t = Instant::now();
            p.sparql_store
                .query_arbitrary(&EvalStop::none(), OLD_SCOPE_SPARQL)
                .expect("sparql read");
            sparql_ms.push(t.elapsed().as_millis());
        }

        // (d) a burst of 1,000 new in-scope messages lands as one commit
        // batch: one `Writes` match, one full re-read.
        let burst_at = Instant::now();
        seed(&p, &signer, n, 1_000);
        let burst_seed_ms = burst_at.elapsed().as_millis();
        let t = Instant::now();
        let burst_match = p.auto_processor_writes_match(&watcher, &in_scope);
        let burst_match_us = t.elapsed().as_micros();
        let t = Instant::now();
        let burst = gather_transcript_model(&p, "ns://Message", "body", "{}")
            .await
            .expect("burst read")
            .turns;
        let burst_read_ms = t.elapsed().as_millis();
        assert!(burst_match);
        assert_eq!(burst.len(), turns + 1_000);

        println!(
            "{n:>7} | {idle_queries:>10} {:>10} | {:>22} | {:>22} | {:>22} | {:>22}",
            median(idle_ms),
            median(out_ms),
            median(in_ms),
            median(model_ms),
            median(sparql_ms),
        );
        println!(
            "        burst +1k: seed {burst_seed_ms} ms, match {burst_match_us} µs, \
             1 read of {} turns in {burst_read_ms} ms",
            burst.len()
        );
        println!("        turns read: {turns}");
    }
}

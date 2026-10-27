//! A query whose `where` or `order` runs after hydration resolves its
//! language-backed properties for the page it returns, not for every row it
//! filtered: only the properties that filter and sort read are fetched for all
//! of them.
//!
//! The `#[ignore]` benchmarks at the end time the same queries over larger
//! stores; run them with
//! `cargo test --release -p ad4m-executor --lib page_before_resolve -- --ignored --nocapture --test-threads=1`.

use super::query::execute_model_query;
use super::test_helpers::{execute_model_query_from_json, StaticShapeResolver};
use super::types::ModelQueryInput;
use crate::languages::language_runtime::LanguageOperation;
use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
use crate::languages::LanguageController;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};
use serde_json::{json, Value};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

/// A file language served by the test, one call at a time as a language
/// runtime serves them, `latency` per `expressionGet`. `gets` counts those.
struct FileLanguage {
    address: String,
    gets: Arc<AtomicUsize>,
}

impl FileLanguage {
    async fn serve(latency: Duration) -> Self {
        let address = format!("QmTestFiles{}", uuid::Uuid::new_v4().simple());
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel::<
            crate::languages::language_runtime::LanguageRuntimeRequest,
        >();
        LanguageController::global_instance()
            .set_runtime_for_test(
                &address,
                Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
            )
            .await;
        let gets = Arc::new(AtomicUsize::new(0));
        let counter = gets.clone();
        tokio::spawn(async move {
            while let Some(request) = requests.recv().await {
                let LanguageOperation::Execute(script, _) = &request.operation else {
                    let _ = request.response_tx.send(Ok("null".to_string()));
                    continue;
                };
                let reply = if script.contains("expressionGet") {
                    counter.fetch_add(1, Ordering::SeqCst);
                    tokio::time::sleep(latency).await;
                    file_expression(script)
                } else {
                    // isImmutableExpression: no, so nothing is cached.
                    "false".to_string()
                };
                let _ = request.response_tx.send(Ok(reply));
            }
        });
        FileLanguage { address, gets }
    }

    fn gets(&self) -> usize {
        self.gets.load(Ordering::SeqCst)
    }

    fn reset(&self) {
        self.gets.store(0, Ordering::SeqCst);
    }

    async fn stop(self) {
        LanguageController::global_instance()
            .set_runtime_for_test(&self.address, None)
            .await;
    }
}

/// The expression behind `f<i>`: a PNG when `i` is a multiple of 3, text
/// otherwise, whose bytes are `i` itself.
fn file_expression(script: &str) -> String {
    let i: usize = script
        .split('"')
        .find_map(|part| part.strip_prefix('f')?.parse().ok())
        .expect("an expressionGet names its address");
    json!({
        "author": "did:key:test",
        "timestamp": "2026-01-01T00:00:00.000Z",
        "data": { "data_base64": format!("{i}"), "file_type": file_type(i) },
        "proof": { "key": "k", "signature": "s" }
    })
    .to_string()
}

fn file_type(i: usize) -> &'static str {
    if i % 3 == 0 {
        "image/png"
    } else {
        "text/plain"
    }
}

fn link(source: &str, predicate: &str, target: &str, ts: &str) -> LinkExpression {
    LinkExpression {
        author: "did:key:test".to_string(),
        timestamp: ts.to_string(),
        data: Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        proof: ExpressionProof {
            key: "key".to_string(),
            signature: "sig".to_string(),
        },
        status: Some(LinkStatus::Shared),
    }
}

/// Instance `i`'s links' timestamp: one second apart, in `i` order.
fn ts(i: usize) -> String {
    format!(
        "2026-01-01T{:02}:{:02}:{:02}.000Z",
        i / 3600,
        (i / 60) % 60,
        i % 60
    )
}

/// `ts(i)` in epoch milliseconds, the form a `where` on `timestamp` takes.
fn ts_ms(i: usize) -> i64 {
    chrono::DateTime::parse_from_rfc3339(&ts(i))
        .unwrap()
        .timestamp_millis()
}

fn id(i: usize) -> String {
    format!("item://{i:05}")
}

/// `n` items: even ones are posts, odd ones notes, each with a title and a
/// rank, and with a file on `files` when given.
fn seed(n: usize, files: Option<&FileLanguage>) -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    for i in 0..n {
        let (s, t) = (id(i), ts(i));
        let kind = if i % 2 == 0 { "post" } else { "note" };
        store
            .add_link(&link(&s, "ad4m://type", "item://Item", &t))
            .unwrap();
        store
            .add_link(&link(
                &s,
                "item://kind",
                &format!("literal:string:{kind}"),
                &t,
            ))
            .unwrap();
        store
            .add_link(&link(
                &s,
                "item://title",
                &format!("literal:string:t{i}"),
                &t,
            ))
            .unwrap();
        store
            .add_link(&link(
                &s,
                "item://rank",
                &format!("literal:number:{}", (i * 7) % 13),
                &t,
            ))
            .unwrap();
        if let Some(files) = files {
            store
                .add_link(&link(
                    &s,
                    "item://file",
                    &format!("{}://f{i}", files.address),
                    &t,
                ))
                .unwrap();
        }
    }
    store
}

fn shape_json(files: Option<&FileLanguage>) -> String {
    let file = files
        .map(|f| {
            format!(
                r#","file": {{ "predicate": "item://file", "resolveLanguage": "{}" }}"#,
                f.address
            )
        })
        .unwrap_or_default();
    format!(
        r#"{{
        "className": "Item",
        "properties": {{
            "type": {{ "predicate": "ad4m://type", "required": true, "flag": true, "initial": "item://Item" }},
            "kind": {{ "predicate": "item://kind" }},
            "title": {{ "predicate": "item://title" }},
            "rank": {{ "predicate": "item://rank" }}{file}
        }},
        "relations": {{}}
    }}"#
    )
}

async fn run(store: &SparqlStore, shape: &str, query: Value) -> (Vec<Value>, usize, Duration) {
    let mut query: ModelQueryInput = serde_json::from_value(query).unwrap();
    // Fixture proofs do not verify.
    query.include_unverified = Some(true);
    let started = Instant::now();
    let result = execute_model_query_from_json(store, "Item", &query, shape)
        .await
        .unwrap();
    (result.instances, result.total_count, started.elapsed())
}

fn ids(instances: &[Value]) -> Vec<String> {
    instances
        .iter()
        .map(|inst| inst["id"].as_str().unwrap().to_string())
        .collect()
}

/// The decoded file of item `i`, as the default file transform renders it.
fn data_uri(i: usize) -> Value {
    json!(format!("data:{};base64,{i}", file_type(i)))
}

/// A `where` that runs after hydration, with a page: the rows and the total
/// are dev's, and only the page's files are fetched.
#[tokio::test]
async fn a_post_hydration_where_fetches_files_for_the_page_only() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(60, Some(&files));
    let shape = shape_json(Some(&files));

    // `kind` is pushed into the store; `timestamp` is matched after hydration.
    let (rows, total, _) = run(
        &store,
        &shape,
        json!({
            "where": { "kind": "post", "timestamp": { "gt": ts_ms(10) } },
            "limit": 5,
            "offset": 3,
        }),
    )
    .await;

    // Posts after item 10 in timestamp order are 12, 14, 16, …, 58.
    let page: Vec<usize> = vec![18, 20, 22, 24, 26];
    assert_eq!(ids(&rows), page.iter().map(|&i| id(i)).collect::<Vec<_>>());
    assert_eq!(total, 24);
    for (row, &i) in rows.iter().zip(&page) {
        assert_eq!(row["file"], data_uri(i), "{row}");
    }
    assert_eq!(
        files.gets(),
        page.len(),
        "fetched files for rows off the page"
    );
    files.stop().await;
}

/// A reverse property holds the ids of the instances pointing at the row, not
/// an expression: the page pass leaves it as it is, as it did when the only
/// pass ran before reverse relations were filled in.
#[tokio::test]
async fn a_reverse_property_keeps_its_source_id() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(4, None);
    // The holder's id has the file language's scheme, so a resolve pass would fetch it.
    let holder = format!("{}://holder", files.address);
    store
        .add_link(&link(&holder, "item://holds", &id(2), &ts(2)))
        .unwrap();
    // A SHACL `belongsToOne` with `resolveLanguage` loads this way; the JSON
    // shape format has no field for it, so the parsed shape is adjusted.
    let (resolver, parsed) = StaticShapeResolver::from_json(
        "Item",
        r#"{
        "className": "Item",
        "properties": {
            "type": { "predicate": "ad4m://type", "required": true, "flag": true, "initial": "item://Item" },
            "kind": { "predicate": "item://kind" }
        },
        "relations": {
            "heldBy": { "predicate": "item://holds", "kind": "belongsToOne", "direction": "reverse", "targetClassName": "" }
        }
    }"#,
    )
    .unwrap();
    let mut shape = parsed.as_ref().clone();
    for property in shape.properties.iter_mut().filter(|p| p.name == "heldBy") {
        property.resolve_language = Some(files.address.clone());
    }
    let mut query: ModelQueryInput = serde_json::from_value(
        json!({ "where": { "kind": "post", "timestamp": { "gt": 0 } }, "limit": 10 }),
    )
    .unwrap();
    query.include_unverified = Some(true);
    let rows = execute_model_query(&store, &shape, &query, &resolver)
        .await
        .unwrap()
        .instances;

    let held = rows
        .iter()
        .find(|r| r["id"] == json!(id(2)))
        .expect("item 2");
    assert_eq!(held["heldBy"], json!(holder), "{held}");
    assert_eq!(files.gets(), 0, "resolved a reverse property's source id");
    files.stop().await;
}

/// A `where` that reads the file itself still sees every row's decoded file,
/// and each file is fetched and transformed once.
#[tokio::test]
async fn a_post_hydration_where_on_a_file_reads_the_decoded_file() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let n = 30;
    let store = seed(n, Some(&files));
    let shape = shape_json(Some(&files));

    // The `timestamp` arm keeps the whole `OR` out of the store.
    let (rows, total, _) = run(
        &store,
        &shape,
        json!({
            "where": { "OR": [
                { "file": { "contains": "image/png" } },
                { "timestamp": { "gt": ts_ms(25) } }
            ] },
            "limit": 4,
            "offset": 2,
            "order": { "timestamp": "DESC" },
        }),
    )
    .await;

    let matching: Vec<usize> = (0..n).rev().filter(|&i| i % 3 == 0 || i > 25).collect();
    let page: Vec<usize> = matching.iter().copied().skip(2).take(4).collect();
    assert_eq!(ids(&rows), page.iter().map(|&i| id(i)).collect::<Vec<_>>());
    assert_eq!(total, matching.len());
    for (row, &i) in rows.iter().zip(&page) {
        assert_eq!(row["file"], data_uri(i), "{row}");
    }
    assert_eq!(files.gets(), n, "each file is fetched once");
    files.stop().await;
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

const RUNS: usize = 5;

/// Times `query` `RUNS` times and prints the median, min and max, the rows
/// returned, the total and the file fetches per run.
async fn bench(
    label: &str,
    store: &SparqlStore,
    shape: &str,
    files: Option<&FileLanguage>,
    query: Value,
) {
    let mut times = Vec::new();
    let mut fetches = 0;
    let mut shape_of_result = None;
    for _ in 0..RUNS {
        if let Some(f) = files {
            f.reset();
        }
        let (rows, total, elapsed) = run(store, shape, query.clone()).await;
        times.push(elapsed);
        fetches = files.map(|f| f.gets()).unwrap_or(0);
        let this = (ids(&rows), total);
        if let Some(previous) = &shape_of_result {
            assert_eq!(previous, &this, "{label}: runs disagree");
        }
        shape_of_result = Some(this);
    }
    times.sort();
    let (rows, total) = shape_of_result.unwrap();
    let digest = rows.iter().fold(0u64, |h, id| {
        h.wrapping_mul(31)
            .wrapping_add(id.len() as u64 + id.bytes().map(u64::from).sum::<u64>())
    });
    println!(
        "BENCH {label}: median {:.1} ms, min {:.1} ms, max {:.1} ms, rows {}, total {}, file gets {}, first {:?}, last {:?}, digest {digest}",
        times[RUNS / 2].as_secs_f64() * 1000.0,
        times[0].as_secs_f64() * 1000.0,
        times[RUNS - 1].as_secs_f64() * 1000.0,
        rows.len(),
        total,
        fetches,
        rows.first(),
        rows.last(),
    );
}

/// The residual `where` every scenario but C uses: `kind` is pushed, the
/// `timestamp` half is matched after hydration. Keeps about three posts in
/// eight.
fn residual_where(n: usize) -> Value {
    json!({ "kind": "post", "timestamp": { "gt": ts_ms(n / 4) } })
}

/// A. A post-hydration `where` with a page of 20, scalar properties only.
#[tokio::test]
#[ignore]
async fn bench_a_post_hydration_where_scalar() {
    for n in [100, 420, 2000] {
        let store = seed(n, None);
        let shape = shape_json(None);
        bench(
            &format!("A n={n}"),
            &store,
            &shape,
            None,
            json!({ "where": residual_where(n), "limit": 20, "offset": 20 }),
        )
        .await;
    }
}

/// B. The same over 420 items with a file each, 50 ms per file fetch.
#[tokio::test]
#[ignore]
async fn bench_b_post_hydration_where_files() {
    let files = FileLanguage::serve(Duration::from_millis(50)).await;
    let n = 420;
    let store = seed(n, Some(&files));
    let shape = shape_json(Some(&files));
    bench(
        "B n=420",
        &store,
        &shape,
        Some(&files),
        json!({ "where": residual_where(n), "limit": 20, "offset": 20 }),
    )
    .await;
    files.stop().await;
}

/// C. A `where` the store runs whole, the control.
#[tokio::test]
#[ignore]
async fn bench_c_pushed_where() {
    let files = FileLanguage::serve(Duration::from_millis(50)).await;
    for n in [420, 2000] {
        let store = seed(n, None);
        bench(
            &format!("C scalar n={n}"),
            &store,
            &shape_json(None),
            None,
            json!({ "where": { "kind": "post" }, "limit": 20, "offset": 20 }),
        )
        .await;
    }
    let store = seed(420, Some(&files));
    bench(
        "C files n=420",
        &store,
        &shape_json(Some(&files)),
        Some(&files),
        json!({ "where": { "kind": "post" }, "limit": 20, "offset": 20 }),
    )
    .await;
    files.stop().await;
}

/// D. An `order` the store cannot run (two keys), with a page of 20.
#[tokio::test]
#[ignore]
async fn bench_d_post_hydration_order() {
    let files = FileLanguage::serve(Duration::from_millis(50)).await;
    let n = 420;
    let order = json!({ "rank": "ASC", "title": "DESC" });
    let store = seed(n, None);
    bench(
        "D scalar n=420",
        &store,
        &shape_json(None),
        None,
        json!({ "order": order, "limit": 20, "offset": 20 }),
    )
    .await;
    let store = seed(n, Some(&files));
    bench(
        "D files n=420",
        &store,
        &shape_json(Some(&files)),
        Some(&files),
        json!({ "order": order, "limit": 20, "offset": 20 }),
    )
    .await;
    files.stop().await;
}

/// E. A post-hydration `where` with no page: every matching row comes back.
#[tokio::test]
#[ignore]
async fn bench_e_post_hydration_where_no_limit() {
    let files = FileLanguage::serve(Duration::from_millis(50)).await;
    for n in [420, 2000] {
        let store = seed(n, None);
        bench(
            &format!("E scalar n={n}"),
            &store,
            &shape_json(None),
            None,
            json!({ "where": residual_where(n) }),
        )
        .await;
    }
    let n = 420;
    let store = seed(n, Some(&files));
    bench(
        "E files n=420",
        &store,
        &shape_json(Some(&files)),
        Some(&files),
        json!({ "where": residual_where(n) }),
    )
    .await;
    files.stop().await;
}

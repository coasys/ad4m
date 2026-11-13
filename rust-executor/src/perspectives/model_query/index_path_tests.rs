//! The id phase from the scope index gives what the SPARQL id phase gives.
//!
//! Each world is written twice: into a store whose `Message` class is indexed
//! and ready, and into a store that has no index for it. Every query runs on
//! both, and the two results (instances and total) must be equal. A counter of
//! the pages the index gave says which path answered, so each test also checks
//! that the fast path ran exactly when its conditions hold.

use rand::rngs::StdRng;
use rand::seq::SliceRandom;
use rand::{Rng, SeedableRng};
use serde_json::{json, Value};
use std::sync::Arc;

use super::containment::{ContainmentEdge, ContainmentEdges, Holder, HAS_CHILD};
use super::index_path::{index_page, PAGES_SERVED};
use super::query::execute_model_query;
use super::shape::parse_shape_from_json;
use super::test_helpers::StaticShapeResolver;
use super::types::{
    ModelQueryInput, ModelQueryResult, ModelShape, OrderDirection, SortKey, SparqlPagination,
};
use crate::agent::signatures::TestSigner;
use crate::perspectives::scope_index::test_support::{
    channel_link, flag_link, forged, message_shape, signed, IN_CHANNEL, MESSAGE, T0, TYPE,
};
use crate::perspectives::scope_index::Readiness;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::LinkExpression;

/// The flag value of class `Channel`.
const CHANNEL: &str = "si://Channel";
/// The pages the index gave on this thread so far.
fn pages_served() -> usize {
    PAGES_SERVED.with(|n| n.get())
}

fn channel_shape() -> ModelShape {
    parse_shape_from_json(
        &format!(
            r#"{{
    "className": "Channel",
    "properties": {{
        "type": {{"predicate":"{TYPE}","required":true,"flag":true,"initial":"{CHANNEL}"}}
    }},
    "relations": {{}}
}}"#
        ),
        "Channel",
    )
    .unwrap()
}

/// Two stores with the same links: `indexed` reads `Message` from the scope
/// index, `plain` only from SPARQL.
struct World {
    indexed: SparqlStore,
    plain: SparqlStore,
    resolver: StaticShapeResolver,
    shape: Arc<ModelShape>,
    channels: Vec<String>,
    /// `has_child`, and the channel's messages for an instance of `Channel`.
    edges: ContainmentEdges,
}

impl World {
    fn empty() -> Self {
        World::with_stores(
            SparqlStore::new(None).unwrap(),
            SparqlStore::new(None).unwrap(),
        )
    }

    fn with_stores(indexed: SparqlStore, plain: SparqlStore) -> Self {
        let resolver = StaticShapeResolver::new();
        let shape = Arc::new(message_shape());
        resolver.register_arc("Message", shape.clone());
        let channel = Arc::new(channel_shape());
        resolver.register_arc("Channel", channel.clone());
        World {
            indexed,
            plain,
            resolver,
            shape,
            channels: (0..4).map(|i| format!("si://ch/{i}")).collect(),
            edges: Arc::new(vec![
                ContainmentEdge::has_child(),
                ContainmentEdge {
                    predicate: IN_CHANNEL.to_string(),
                    holder: Some(Holder {
                        class: "Channel".to_string(),
                        shape: channel,
                    }),
                },
            ]),
        }
    }

    fn add(&self, link: &LinkExpression) {
        self.indexed.add_link(link).unwrap();
        self.plain.add_link(link).unwrap();
    }

    fn remove(&self, link: &LinkExpression) {
        self.indexed.remove_link(link).unwrap();
        self.plain.remove_link(link).unwrap();
    }

    /// Index `Message` in `indexed`, build it to the end, and check it is
    /// ready.
    fn index(&self) {
        let (rule, _) = self
            .indexed
            .scope_index_register(&self.shape)
            .unwrap()
            .unwrap();
        self.indexed.scope_index_build_now(&rule).unwrap();
        assert_eq!(self.indexed.scope_index_readiness(&rule), Readiness::Ready);
    }

    /// A random world from `seed`: `members` messages, many in the same
    /// millisecond, with IRIs in no relation to their times; flags by two
    /// authors and forged flags; channel links and `has_child` links, some
    /// forged; and removals. Half the links come before the index is built,
    /// half after.
    fn random(seed: u64, members: usize) -> Self {
        let world = World::empty();
        let mut rng = StdRng::seed_from_u64(seed);
        let signers = [TestSigner::generate(), TestSigner::generate()];
        let mut adds: Vec<LinkExpression> = Vec::new();
        let mut removes: Vec<LinkExpression> = Vec::new();
        // Channels 0 and 1 are instances of Channel; channel 2 only by a
        // forged flag; channel 3 not at all.
        for ch in &world.channels[..2] {
            adds.push(signed(&signers[0], ch, TYPE, CHANNEL, T0));
        }
        adds.push(forged(&signers[0], &world.channels[2], TYPE, CHANNEL, T0));
        for i in 0..members {
            let id = format!("si://m/{:08x}-{i}", rng.gen::<u32>());
            let t = T0 + rng.gen_range(0..members as u64 / 3);
            let author = &signers[rng.gen_range(0..2)];
            let flag = if rng.gen_bool(0.08) {
                forged(author, &id, TYPE, MESSAGE, t)
            } else {
                flag_link(author, &id, t)
            };
            adds.push(flag.clone());
            if rng.gen_bool(0.15) {
                // The same triple by the other author, earlier or later.
                let t2 = T0 + rng.gen_range(0..members as u64 / 3);
                adds.push(flag_link(&signers[1], &id, t2));
            }
            if rng.gen_bool(0.05) {
                // A forged flag before the valid one does not move it.
                adds.push(forged(author, &id, TYPE, MESSAGE, T0.saturating_sub(1)));
            }
            if rng.gen_bool(0.05) {
                removes.push(flag);
            }
            adds.push(signed(
                author,
                &id,
                "si://body",
                &format!("literal:string:{i}"),
                t,
            ));
            for _ in 0..rng.gen_range(0..3) {
                let ch = world.channels.choose(&mut rng).unwrap();
                let edge = if rng.gen_bool(0.05) {
                    forged(author, ch, IN_CHANNEL, &id, t)
                } else {
                    channel_link(author, ch, &id, t)
                };
                adds.push(edge.clone());
                if rng.gen_bool(0.08) {
                    removes.push(edge);
                }
            }
            if rng.gen_bool(0.4) {
                let ch = world.channels.choose(&mut rng).unwrap();
                adds.push(signed(author, ch, HAS_CHILD, &id, t));
            }
        }
        adds.shuffle(&mut rng);
        let half = adds.len() / 2;
        for l in &adds[..half] {
            world.add(l);
        }
        world.index();
        for l in &adds[half..] {
            world.add(l);
        }
        for l in &removes {
            world.remove(l);
        }
        world
    }

    async fn run(&self, store: &SparqlStore, query: &ModelQueryInput) -> ModelQueryResult {
        execute_model_query(store, &self.shape, query, &self.resolver)
            .await
            .unwrap_or_else(|e| panic!("{query:?} failed: {e}"))
    }

    /// Run `query` on both stores, check they agree, and return whether the
    /// indexed store gave the page from the index.
    async fn compare(&self, query: &ModelQueryInput, what: &str) -> bool {
        let before = pages_served();
        let fast = self.run(&self.indexed, query).await;
        let used_index = pages_served() > before;
        let slow = self.run(&self.plain, query).await;
        assert_eq!(
            ids(&fast),
            ids(&slow),
            "{what}: the ids differ (index path: {used_index})"
        );
        assert_eq!(
            fast.instances, slow.instances,
            "{what}: the instances differ"
        );
        assert_eq!(
            fast.total_count, slow.total_count,
            "{what}: the totals differ (index path: {used_index})"
        );
        used_index
    }
}

fn ids(result: &ModelQueryResult) -> Vec<String> {
    result
        .instances
        .iter()
        .map(|i| i["id"].as_str().unwrap_or_default().to_string())
        .collect()
}

fn query(value: Value) -> ModelQueryInput {
    serde_json::from_value(value).unwrap()
}

/// A random query and whether the index path must answer it.
fn random_query(rng: &mut StdRng, world: &World) -> (ModelQueryInput, bool, Value) {
    let mut eligible = true;
    let mut q = json!({});
    match rng.gen_range(0..10) {
        0 => eligible = false,
        _ => q["limit"] = json!(rng.gen_range(1..=25)),
    }
    match rng.gen_range(0..20) {
        0..=9 => {}
        10..=16 => q["offset"] = json!(rng.gen_range(0..80)),
        17..=18 => q["offset"] = json!(rng.gen_range(995..=1000)),
        _ => {
            q["offset"] = json!(rng.gen_range(1001..1100));
            eligible = false;
        }
    }
    match rng.gen_range(0..20) {
        0..=5 => {}
        6..=10 => q["order"] = json!({"timestamp": "DESC"}),
        11..=14 => q["order"] = json!({"timestamp": "ASC"}),
        15 => q["order"] = json!({"createdAt": "DESC"}),
        16 => q["order"] = json!({"updatedAt": "ASC"}),
        17..=18 => {
            q["order"] = json!({"body": "ASC"});
            eligible = false;
        }
        _ => {
            q["order"] = json!([["timestamp", "DESC"], ["body", "ASC"]]);
            eligible = false;
        }
    }
    let ch = world.channels.choose(rng).unwrap().clone();
    let mut contained = false;
    match rng.gen_range(0..20) {
        0..=4 => {}
        5..=9 => q["parent"] = json!({"id": ch, "predicate": IN_CHANNEL}),
        10..=12 => q["parent"] = json!({"model": "Channel", "id": ch, "field": IN_CHANNEL}),
        13..=17 => {
            q["parent"] = json!({ "id": ch });
            contained = true;
        }
        18 => {
            q["parent"] = json!({"model": "Channel", "id": ch});
            eligible = false;
        }
        _ => {
            // A parent with no instance under it.
            q["parent"] = json!({"id": "si://ch/none", "predicate": IN_CHANNEL});
        }
    }
    match rng.gen_range(0..25) {
        0 => {
            q["linkStatus"] = json!("shared");
            eligible = false;
        }
        1 => {
            q["includeUnverified"] = json!(true);
            eligible = false;
        }
        2 => {
            q["includeUnverified"] = json!(false);
        }
        3 => {
            q["where"] = json!({"body": "literal:string:3"});
            eligible = false;
        }
        _ => {}
    }
    if rng.gen_bool(0.2) {
        q["count"] = json!(true);
    }
    let mut input = query(q.clone());
    if contained && rng.gen_bool(0.5) {
        input.containment = Some(world.edges.clone());
        q["containment"] = json!("has_child + Channel");
    }
    (input, eligible, q)
}

/// About 1,000 random queries over three random worlds: the index path and
/// the SPARQL path give the same instances and totals, and the index path
/// answers exactly the queries its conditions admit.
#[tokio::test]
async fn the_index_path_equals_the_sparql_path_on_random_queries() {
    let mut fast = 0;
    let mut total = 0;
    for (seed, members) in [(1u64, 240usize), (2, 120), (3, 360)] {
        let world = World::random(seed, members);
        let mut rng = StdRng::seed_from_u64(seed * 1000);
        for n in 0..334 {
            let (input, eligible, q) = random_query(&mut rng, &world);
            let what = format!("world {seed}, query {n}: {q}");
            let used_index = world.compare(&input, &what).await;
            assert_eq!(
                used_index, eligible,
                "{what}: the index path ran: {used_index}"
            );
            fast += used_index as usize;
            total += 1;
        }
    }
    eprintln!("index path: {fast} of {total} random queries");
    assert!(
        fast * 2 > total,
        "most random queries should take the index path: {fast} of {total}"
    );
}

/// Every window of a world where all members share one millisecond: then the
/// order is by IRI alone, which the index reads past its window to get.
#[tokio::test]
async fn one_millisecond_orders_by_iri() {
    let world = World::empty();
    let s = TestSigner::generate();
    let ch = world.channels[0].clone();
    let mut rng = StdRng::seed_from_u64(9);
    for i in 0..60 {
        let id = format!("si://m/{:08x}-{i}", rng.gen::<u32>());
        world.add(&flag_link(&s, &id, T0));
        world.add(&channel_link(&s, &ch, &id, T0));
    }
    world.index();
    for offset in [0, 1, 7, 30, 59, 60] {
        for limit in [1, 5, 20, 100] {
            for order in ["ASC", "DESC"] {
                for parent in [json!(null), json!({"id": ch, "predicate": IN_CHANNEL})] {
                    let q = json!({
                        "offset": offset, "limit": limit,
                        "order": {"timestamp": order}, "parent": parent
                    });
                    assert!(world.compare(&query(q.clone()), &q.to_string()).await);
                }
            }
        }
    }
}

/// A member whose flag time is not in the executor's form makes the class go
/// to SPARQL, which then gives what it always gave.
#[tokio::test]
async fn a_time_not_in_the_executor_form_goes_to_sparql() {
    let world = World::empty();
    let s = TestSigner::generate();
    for i in 0..5 {
        world.add(&flag_link(&s, &format!("si://m/{i}"), T0 + i));
    }
    world.index();
    let q = query(json!({"limit": 3}));
    assert!(world.compare(&q, "canonical").await);
    let mut odd = flag_link(&s, "si://m/odd", T0 + 10);
    // The same time, without milliseconds: still signed, not canonical.
    odd.timestamp = odd.timestamp.replace(".010Z", ".01Z");
    assert!(odd.compute_proof_valid(), "the timestamp still verifies");
    world.add(&odd);
    assert!(!world.compare(&q, "noncanonical").await);
    world.remove(&odd);
    assert!(world.compare(&q, "canonical again").await);
}

/// The index is read only when it is ready and holds this shape's rule, and
/// only for a shape whose conformance and order are the flag triple.
#[tokio::test]
async fn only_a_ready_index_of_this_flag_is_read() {
    let world = World::empty();
    let s = TestSigner::generate();
    for i in 0..5 {
        world.add(&flag_link(&s, &format!("si://m/{i}"), T0 + i));
    }
    let q = query(json!({"limit": 3}));
    // Not indexed.
    assert!(!world.compare(&q, "not indexed").await);
    // Registered, not built.
    let (rule, _) = world
        .indexed
        .scope_index_register(&world.shape)
        .unwrap()
        .unwrap();
    assert_eq!(
        world.indexed.scope_index_readiness(&rule),
        Readiness::Building
    );
    assert!(!world.compare(&q, "building").await);
    world.indexed.scope_index_build_now(&rule).unwrap();
    assert!(world.compare(&q, "ready").await);

    // A shape with another required property: its conformance is not the
    // flag alone.
    let stricter = Arc::new(
        parse_shape_from_json(
            &format!(
                r#"{{"className":"Message","properties":{{
                    "type":{{"predicate":"{TYPE}","required":true,"flag":true,"initial":"{MESSAGE}"}},
                    "body":{{"predicate":"si://body","required":true}}
                }},"relations":{{}}}}"#
            ),
            "Message",
        )
        .unwrap(),
    );
    let before = pages_served();
    let strict = execute_model_query(&world.indexed, &stricter, &q, &world.resolver)
        .await
        .unwrap();
    assert_eq!(pages_served(), before, "the stricter shape goes to SPARQL");
    assert_eq!(strict.total_count, 0, "no message has a body");
}

fn median(mut samples: Vec<std::time::Duration>) -> std::time::Duration {
    samples.sort();
    samples[samples.len() / 2]
}

/// Time `query` on `store`: 2 warm-up runs, then the median of 7.
async fn time_query(
    world: &World,
    store: &SparqlStore,
    query: &ModelQueryInput,
) -> (std::time::Duration, ModelQueryResult) {
    for _ in 0..2 {
        world.run(store, query).await;
    }
    let mut samples = Vec::new();
    let mut last = None;
    for _ in 0..7 {
        let start = std::time::Instant::now();
        let result = world.run(store, query).await;
        samples.push(start.elapsed());
        last = Some(result);
    }
    (median(samples), last.unwrap())
}

/// 100,000 messages under one channel, beside 100,000 instances of another
/// class, in a RocksDB store. The same query runs on the SPARQL path, then
/// (after the first build of the index) on the index path: the whole query,
/// and the id phase alone. `BENCH_QUERY` picks the query: `parent` (the
/// newest 20 under the channel, the default), `class` (the newest 20 of the
/// class) or `offset` (20 under the channel at offset 500).
///
/// `BENCH_QUERY=parent cargo test --release --lib index_path_tests::bench -- --ignored --nocapture --test-threads=1`
#[tokio::test]
#[ignore]
async fn bench_newest_20_of_100k_under_one_parent() {
    const MEMBERS: u64 = 100_000;
    let dir = tempfile::tempdir().unwrap();
    let store = SparqlStore::new(Some(dir.path().join("store").to_str().unwrap())).unwrap();
    // Only `indexed` holds the links: it is timed before and after its
    // index is built.
    let world = World::with_stores(store, SparqlStore::new(None).unwrap());
    let s = TestSigner::generate();
    let ch = world.channels[0].clone();
    let start = std::time::Instant::now();
    let mut batch = Vec::new();
    for i in 0..MEMBERS {
        let id = format!("si://m/{i:06}");
        // Two messages in most milliseconds.
        let t = T0 + i / 2;
        batch.push(flag_link(&s, &id, t));
        batch.push(channel_link(&s, &ch, &id, t));
        batch.push(signed(
            &s,
            &id,
            "si://body",
            &format!("literal:string:{i}"),
            t,
        ));
        // The background: a note per message, with a body.
        let note = format!("si://n/{i:06}");
        batch.push(signed(&s, &note, TYPE, "si://Note", t));
        batch.push(signed(
            &s,
            &note,
            "si://body",
            &format!("literal:string:n{i}"),
            t,
        ));
        if batch.len() >= 5_000 {
            world.indexed.apply_diff(&[], &batch).unwrap();
            batch.clear();
        }
    }
    world.indexed.apply_diff(&[], &batch).unwrap();
    eprintln!("wrote {} links in {:?}", MEMBERS * 5, start.elapsed());

    let (name, q, offset) = match std::env::var("BENCH_QUERY").as_deref() {
        Ok("class") => (
            "newest 20 of the class",
            json!({"limit": 20, "order": {"timestamp": "DESC"}}),
            0,
        ),
        Ok("offset") => (
            "20 under the channel at offset 500",
            json!({"limit": 20, "offset": 500, "order": {"timestamp": "DESC"}, "parent": {"id": ch, "predicate": IN_CHANNEL}}),
            500,
        ),
        _ => (
            "newest 20 under the channel",
            json!({"limit": 20, "order": {"timestamp": "DESC"}, "parent": {"id": ch, "predicate": IN_CHANNEL}}),
            0,
        ),
    };
    let q = query(q);
    let store = &world.indexed;

    let before = store.queries_run().len();
    let (slow, slow_result) = time_query(&world, store, &q).await;
    let id_sparql = store.queries_run()[before..]
        .iter()
        .rev()
        .find(|q| q.contains("MIN(?_first_ts_v)"))
        .expect("the SPARQL path ran its id phase")
        .clone();
    let mut samples = Vec::new();
    for n in 0..9 {
        let start = std::time::Instant::now();
        store.query(&id_sparql).unwrap();
        if n >= 2 {
            samples.push(start.elapsed());
        }
    }
    let slow_ids = median(samples);

    let start = std::time::Instant::now();
    world.index();
    eprintln!("first build of the class index: {:?}", start.elapsed());

    let before = pages_served();
    let (fast, fast_result) = time_query(&world, store, &q).await;
    assert_eq!(pages_served() - before, 9, "every run took the index path");
    assert_eq!(fast_result.instances, slow_result.instances);
    assert_eq!(fast_result.total_count, slow_result.total_count);
    assert_eq!(fast_result.total_count, MEMBERS as usize);
    let pagination = SparqlPagination {
        sort_key: SortKey::Timestamp,
        direction: OrderDirection::DESC,
        offset: Some(offset),
        limit: Some(20),
    };
    let mut samples = Vec::new();
    for n in 0..9 {
        let start = std::time::Instant::now();
        let page = index_page(store, &world.shape, &q, &pagination)
            .unwrap()
            .expect("the index answers");
        if n >= 2 {
            samples.push(start.elapsed());
        }
        assert_eq!(page.ids.len(), 20);
    }
    let fast_ids = median(samples);
    let ratio = |a: std::time::Duration, b: std::time::Duration| a.as_secs_f64() / b.as_secs_f64();
    eprintln!(
        "{name} (medians of 7 after 2 warm-ups):\n  whole query: SPARQL path {slow:?}, index path {fast:?} ({:.0}x)\n  id phase:    SPARQL {slow_ids:?}, index {fast_ids:?} ({:.0}x)",
        ratio(slow, fast),
        ratio(slow_ids, fast_ids)
    );
}

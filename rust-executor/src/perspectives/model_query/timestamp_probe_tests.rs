//! The timestamp probe of the paged plan ([`build_timestamp_probe`]).
//!
//! The probe finds each instance's earliest admitted link on its flag (or
//! required) property. Oxigraph looks up a reifier by its triple term only when
//! every part of the term is bound, so a probe that starts at
//! `?_r rdf:reifies <<( ?source … )>>` with `?source` free scans every reifier
//! in the store. The probe therefore binds the plain triple
//! `?source <pred> object .` first: the store holds that triple exactly when at
//! least one reifier of it exists (`write_link_quads_proof` writes both,
//! `remove_link_quads` keeps the triple while a reifier remains), so the extra
//! pattern selects nothing the reifier pattern does not.
//!
//! The probe also opens the id phase's `WHERE`: Oxigraph's join planner starts
//! from the first of the cheapest patterns, and the plain triple must be that
//! pattern rather than a conformance check on every verified reifier.
//!
//! These tests pin the probe text, its place, and the result of a paged query over a store
//! with two links on one triple from different authors, a removed link, an
//! unverified link and links of both statuses.

use super::sparql_builder::{
    annotation_is, build_count_sparql, build_instance_sparql, build_timestamp_probe, LinkGuard,
};
use super::test_helpers::{execute_model_query_from_json, flag, prop, shape};
use super::types::{InstanceQueryPlan, ModelQueryInput, OrderDirection};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::{EvalStop, SparqlStore};
use crate::types::{Link, LinkExpression, LinkStatus};

const RDF_REIFIES: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";

/// Where `needle` starts in `probe`, or a panic that shows the probe.
fn position(probe: &str, needle: &str) -> usize {
    probe
        .find(needle)
        .unwrap_or_else(|| panic!("`{needle}` is not in the probe: {probe}"))
}

#[test]
fn the_flag_probe_binds_the_plain_triple_before_the_reifier() {
    let s = shape("Task", vec![flag("type", "pt://type", "pt://Task")]);
    for guard in [LinkGuard::ANY, LinkGuard::default()] {
        let probe = build_timestamp_probe(&s, guard);
        let plain = position(&probe, "?source <pt://type> <pt://Task> .");
        let reifier = position(&probe, RDF_REIFIES);
        assert!(plain < reifier, "the plain triple must come first: {probe}");
        assert!(
            probe.contains("<<( ?source <pt://type> <pt://Task> )>>"),
            "the reifier still decides the timestamp: {probe}"
        );
    }
}

#[test]
fn the_required_probe_binds_the_plain_triple_before_the_reifier() {
    let mut title = prop("title", "pt://title");
    title.is_required = true;
    let s = shape("Note", vec![title]);
    for guard in [LinkGuard::ANY, LinkGuard::default()] {
        let probe = build_timestamp_probe(&s, guard);
        let plain = position(&probe, "?source <pt://title> ?_cf_title .");
        let reifier = position(&probe, RDF_REIFIES);
        assert!(plain < reifier, "the plain triple must come first: {probe}");
        assert!(
            probe.contains("<<( ?source <pt://title> ?_cf_title )>>"),
            "the reifier still decides the timestamp: {probe}"
        );
    }
}

/// The guard's checks stay on the probe's reifier, so a link the guard
/// withholds still cannot give an instance its timestamp.
#[test]
fn the_guard_checks_stay_on_the_probe_reifier() {
    let s = shape("Task", vec![flag("type", "pt://type", "pt://Task")]);
    let shared = LinkStatus::Shared;
    let guard = LinkGuard {
        status: Some(&shared),
        include_unverified: None,
    };
    let probe = build_timestamp_probe(&s, guard);
    assert!(
        probe.contains(&annotation_is("?_r", "status", "_st", "Shared")),
        "status check on the probe reifier: {probe}"
    );
    assert!(
        probe.contains(&annotation_is("?_r", "proofValid", "_pv", "true")),
        "verdict check on the probe reifier: {probe}"
    );
}

/// The id phase of a page sorted by creation time, limit 1.
fn id_phase(s: &super::types::ModelShape, q: &ModelQueryInput) -> String {
    let pg = super::types::SparqlPagination {
        sort_key: super::types::SortKey::Timestamp,
        direction: OrderDirection::ASC,
        offset: None,
        limit: Some(1),
        keyset: None,
    };
    match build_instance_sparql(s, q, Some(&pg), None) {
        InstanceQueryPlan::TwoPhase {
            pagination_subquery,
            ..
        } => pagination_subquery,
        _ => panic!("expected a paged plan"),
    }
}

/// Oxigraph's join planner starts from the first of the cheapest patterns. A
/// conformance check before the probe (here the `name` property's verdict,
/// `?_rcf_name proofValid "true"`) costs as much as the probe's plain triple
/// and would make the plan scan every verified reifier. So the probe opens the
/// `WHERE`, before conformance.
#[test]
fn the_paged_id_phase_starts_with_the_probe_plain_triple() {
    let mut name = prop("name", "pt://name");
    name.is_required = true;
    let s = shape("Space", vec![name, flag("type", "pt://type", "pt://Space")]);
    let sub = id_phase(&s, &ModelQueryInput::default());
    let body = sub[position(&sub, " WHERE {") + " WHERE {".len()..].trim_start();
    assert!(
        body.starts_with("?source <pt://type> <pt://Space> ."),
        "the probe's plain triple must open the WHERE: {sub}"
    );
    assert!(
        position(&sub, "<<( ?source <pt://type> <pt://Space> )>>")
            < position(&sub, "?source <pt://name> ?_cf_name"),
        "the probe must come before conformance: {sub}"
    );
}

/// A page also counts its instances (`totalCount`) with conformance alone, and
/// the same tie would make that count scan every verified reifier. So
/// conformance names the class's IRI flags first, in shape order, ahead of a
/// required property declared before them.
#[test]
fn conformance_opens_with_the_flag_triples() {
    let mut name = prop("name", "pt://name");
    name.is_required = true;
    let s = shape(
        "Space",
        vec![
            name,
            flag("type", "pt://type", "pt://Space"),
            flag("kind", "pt://kind", "pt://Room"),
        ],
    );
    let count = build_count_sparql(&s, &ModelQueryInput::default(), None).unwrap();
    let body = count[position(&count, " WHERE {") + " WHERE {".len()..].trim_start();
    assert!(
        body.starts_with("?source <pt://type> <pt://Space> ."),
        "the first flag triple must open the count's WHERE: {count}"
    );
    let kind = position(&count, "?source <pt://kind> <pt://Room> .");
    assert!(
        position(&count, "?source <pt://type> <pt://Space> .") < kind
            && kind < position(&count, "?source <pt://name> ?_cf_name ."),
        "the flags keep their order, ahead of the required property: {count}"
    );
}

const TASK_SHAPE_JSON: &str = r#"{
    "className": "Task",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"pt://Task"}
    },
    "relations": {}
}"#;

const NOTE_SHAPE_JSON: &str = r#"{
    "className": "Note",
    "properties": {
        "title": {"predicate":"pt://title","required":true}
    },
    "relations": {}
}"#;

fn at(second: u32) -> chrono::DateTime<chrono::Utc> {
    use chrono::TimeZone;
    chrono::Utc
        .with_ymd_and_hms(2026, 9, 23, 12, 0, second)
        .unwrap()
}

/// A link signed by `signer` at `second`, with `status`.
fn signed(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
    status: LinkStatus,
) -> LinkExpression {
    let mut l = LinkExpression::from(signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        at(second),
    ));
    l.status = Some(status);
    assert!(l.compute_proof_valid(), "the fixture must verify");
    l
}

/// A Shared link that names `signer` as its author but whose signature does
/// not cover its target.
fn forged(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
) -> LinkExpression {
    let mut l = signed(
        signer,
        source,
        predicate,
        "literal:string:what-was-signed",
        second,
        LinkStatus::Shared,
    );
    l.data.target = target.to_string();
    assert!(!l.compute_proof_valid(), "the fixture must not verify");
    l
}

/// Instances `p/1` to `p/6` on `predicate` = `target`, each a case of the
/// probe's contract (the earliest admitted link, by second):
///
/// - `p/1`: one link at 10.
/// - `p/2`: the same triple from author A at 20 and author B at 5: 5.
/// - `p/3`: A at 3 and B at 1, then B's link removed: 3. The triple stays.
/// - `p/4`: one link at 2, removed: not an instance.
/// - `p/5`: an unverified link at 0 and a verified one at 15: 15, or 0 with
///   `includeUnverified`.
/// - `p/6`: a Local link at 4 and a Shared one at 25: 4, 25 under
///   `linkStatus: Shared`, 4 under `linkStatus: Local`.
fn seed(store: &SparqlStore, predicate: &str, target: &str) {
    let a = TestSigner::generate();
    let b = TestSigner::generate();
    let shared = LinkStatus::Shared;
    let link = |signer: &TestSigner, source: &str, second: u32, status: LinkStatus| {
        signed(signer, source, predicate, target, second, status)
    };

    store
        .add_link(&link(&a, "pt://p/1", 10, shared.clone()))
        .unwrap();

    store
        .add_link(&link(&a, "pt://p/2", 20, shared.clone()))
        .unwrap();
    store
        .add_link(&link(&b, "pt://p/2", 5, shared.clone()))
        .unwrap();

    store
        .add_link(&link(&a, "pt://p/3", 3, shared.clone()))
        .unwrap();
    let removed = link(&b, "pt://p/3", 1, shared.clone());
    store.add_link(&removed).unwrap();
    store.remove_link(&removed).unwrap();

    let gone = link(&a, "pt://p/4", 2, shared.clone());
    store.add_link(&gone).unwrap();
    store.remove_link(&gone).unwrap();

    store
        .add_link(&forged(&a, "pt://p/5", predicate, target, 0))
        .unwrap();
    store
        .add_link(&link(&b, "pt://p/5", 15, shared.clone()))
        .unwrap();

    store
        .add_link(&link(&a, "pt://p/6", 4, LinkStatus::Local))
        .unwrap();
    store.add_link(&link(&b, "pt://p/6", 25, shared)).unwrap();
}

/// The store invariant the probe's plain triple relies on: no reifier of a
/// link triple exists without the triple, after adds and removals.
fn assert_every_reifier_has_its_triple(store: &SparqlStore) {
    let orphans = store
        .query(
            &EvalStop::none(),
            &format!(
                "SELECT ?r WHERE {{ ?r <{RDF_REIFIES}> <<( ?s ?p ?o )>> . \
                 FILTER NOT EXISTS {{ ?s ?p ?o }} }}"
            ),
        )
        .unwrap();
    let rows: Vec<serde_json::Value> = serde_json::from_str(&orphans).unwrap();
    assert!(rows.is_empty(), "reifiers without their triple: {orphans}");
}

async fn ids(
    store: &SparqlStore,
    class: &str,
    shape_json: &str,
    q: ModelQueryInput,
) -> Vec<String> {
    execute_model_query_from_json(store, class, &q, shape_json)
        .await
        .unwrap()
        .instances
        .iter()
        .map(|i| i["id"].as_str().unwrap().to_string())
        .collect()
}

fn page(
    limit: Option<usize>,
    offset: Option<usize>,
    direction: OrderDirection,
    include_unverified: Option<bool>,
    link_status: Option<LinkStatus>,
) -> ModelQueryInput {
    ModelQueryInput {
        order: Some(vec![("createdAt".to_string(), direction)]),
        limit,
        offset,
        include_unverified,
        link_status,
        ..Default::default()
    }
}

fn sorted(mut v: Vec<String>) -> Vec<String> {
    v.sort();
    v
}

/// Every case of [`seed`] through the paged plan, for one class.
async fn assert_paged_contract(class: &str, shape_json: &str, predicate: &str, target: &str) {
    let store = SparqlStore::new(None).unwrap();
    seed(&store, predicate, target);
    assert_every_reifier_has_its_triple(&store);

    let p = |n: u32| format!("pt://p/{n}");
    let asc = OrderDirection::ASC;
    let desc = OrderDirection::DESC;

    // Default guard: verified links of any status.
    let expected = vec![p(3), p(6), p(2), p(1), p(5)];
    let got = ids(
        &store,
        class,
        shape_json,
        page(Some(10), None, asc, None, None),
    )
    .await;
    assert_eq!(got, expected, "{class}: default guard, ascending");
    let got = ids(
        &store,
        class,
        shape_json,
        page(Some(10), None, desc, None, None),
    )
    .await;
    let mut reversed = expected.clone();
    reversed.reverse();
    assert_eq!(got, reversed, "{class}: default guard, descending");
    let got = ids(
        &store,
        class,
        shape_json,
        page(Some(2), Some(1), asc, None, None),
    )
    .await;
    assert_eq!(got, vec![p(6), p(2)], "{class}: limit 2, offset 1");
    // The paged plan selects the same instances as the single plan.
    let single = ids(&store, class, shape_json, page(None, None, asc, None, None)).await;
    assert_eq!(sorted(single), sorted(expected), "{class}: single plan");

    // An unverified earlier link moves p/5 only when the query opts in.
    let got = ids(
        &store,
        class,
        shape_json,
        page(Some(10), None, asc, Some(true), None),
    )
    .await;
    assert_eq!(
        got,
        vec![p(5), p(3), p(6), p(2), p(1)],
        "{class}: includeUnverified"
    );

    // Status: the probe reads only links of the asked status.
    let got = ids(
        &store,
        class,
        shape_json,
        page(Some(10), None, asc, None, Some(LinkStatus::Shared)),
    )
    .await;
    assert_eq!(
        got,
        vec![p(3), p(2), p(1), p(5), p(6)],
        "{class}: linkStatus Shared"
    );
    let got = ids(
        &store,
        class,
        shape_json,
        page(Some(10), None, asc, None, Some(LinkStatus::Local)),
    )
    .await;
    assert_eq!(got, vec![p(6)], "{class}: linkStatus Local");
}

#[tokio::test]
async fn a_paged_flag_class_keeps_its_instances_and_order() {
    assert_paged_contract("Task", TASK_SHAPE_JSON, "ad4m://type", "pt://Task").await;
}

#[tokio::test]
async fn a_paged_required_class_keeps_its_instances_and_order() {
    assert_paged_contract("Note", NOTE_SHAPE_JSON, "pt://title", "literal:string:note").await;
}

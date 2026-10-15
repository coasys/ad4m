//! Containment through `Container` on a perspective (#1446): the edge set,
//! `Container` rows, `parent: { id }`, containment walks and their
//! subscriptions. The SDNAs are the shapes the design note names: a `Block`
//! that holds through `we://child` under the alias `children`, a `Channel`
//! whose `children` is `has_child`, a Flux-like `FluxChannel` whose
//! `messages` is `has_child` (not a Container: no `children`), and a
//! `Message` that holds nothing.

use super::super::*;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::model_query::builtin_classes::CONTAINER_URI;
use crate::perspectives::model_query::containment::HAS_CHILD;
use crate::perspectives::model_query::{ModelQueryInput, QueryClasses, Scope};
use serde_json::{json, Value};

pub(super) const BLOCK_SDNA: &str = r#"{
  "target_class":"ns://Block",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://block"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://block","min_count":1,"max_count":1},
    {"path":"we://child","name":"blocks","node_kind":"IRI","relation_kind":"hasMany","collection":true,"alias":{"Container":"children"},
     "adder":[{"action":"addLink","source":"this","predicate":"we://child","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"we://child","target":"value"}]}
  ]
}"#;

pub(super) const CHANNEL_SDNA: &str = r#"{
  "target_class":"ns://Channel",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://channel"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://channel","min_count":1,"max_count":1},
    {"path":"ad4m://has_child","name":"children","node_kind":"IRI","relation_kind":"hasMany","collection":true,
     "adder":[{"action":"addLink","source":"this","predicate":"ad4m://has_child","target":"value"}]}
  ]
}"#;

pub(super) const FLUX_CHANNEL_SDNA: &str = r#"{
  "target_class":"ns://FluxChannel",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://flux_channel"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://flux_channel","min_count":1,"max_count":1},
    {"path":"ad4m://has_child","name":"messages","node_kind":"IRI","relation_kind":"hasMany","collection":true},
    {"path":"ns://pinned","name":"pinned","node_kind":"IRI","relation_kind":"hasMany","collection":true}
  ]
}"#;

pub(super) const MESSAGE_SDNA: &str = r#"{
  "target_class":"ns://Message",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://message"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://message","min_count":1,"max_count":1},
    {"path":"ns://body","name":"body","max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://body","target":"value"}]}
  ]
}"#;

pub(super) fn class(name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(name.to_string()),
        query: None,
    }
}

pub(super) async fn create(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    name: &str,
    base: &str,
) {
    p.create_subject(class(name), base.into(), Some(json!({})), None, ctx)
        .await
        .unwrap_or_else(|e| panic!("create {name} at {base}: {e}"));
}

pub(super) async fn link(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    s: &str,
    pr: &str,
    t: &str,
) {
    p.add_link(
        Link {
            source: s.into(),
            predicate: Some(pr.into()),
            target: t.into(),
        },
        LinkStatus::Shared,
        None,
        ctx,
    )
    .await
    .unwrap_or_else(|e| panic!("link {s} {pr} {t}: {e}"));
    // Distinct timestamps, so "first link" orders are not ties.
    tokio::time::sleep(Duration::from_millis(3)).await;
}

/// The rows of a query over `classes`.
pub(super) async fn query(p: &PerspectiveInstance, classes: &str, q: Value) -> Vec<Value> {
    let json = p
        .model_query_classes(&QueryClasses::One(classes.into()), &q.to_string())
        .await
        .unwrap_or_else(|e| panic!("query {classes} {q}: {e}"));
    serde_json::from_str::<Value>(&json).unwrap()["instances"]
        .as_array()
        .unwrap()
        .clone()
}

/// The sorted ids of `rows`.
pub(super) fn ids(rows: &[Value]) -> Vec<String> {
    let mut ids: Vec<String> = rows
        .iter()
        .map(|r| r["id"].as_str().unwrap().to_string())
        .collect();
    ids.sort();
    ids
}

pub(super) async fn setup() -> (PerspectiveInstance, AgentContext) {
    let (p, _shapes, ctx) = setup_perspective_no_llm(&[
        ("Block", BLOCK_SDNA),
        ("Channel", CHANNEL_SDNA),
        ("FluxChannel", FLUX_CHANNEL_SDNA),
        ("Message", MESSAGE_SDNA),
    ])
    .await;
    (p, ctx)
}

/// E is `has_child` on any node, plus `we://child` on Blocks. Channel holds
/// through `has_child` (covered by rule a); FluxChannel has no `children`.
#[tokio::test(flavor = "multi_thread")]
async fn the_edge_set_is_has_child_plus_each_aliased_predicate() {
    let (p, _ctx) = setup().await;
    let edges = p.containment_edges().unwrap();
    let described: Vec<(String, Option<String>)> = edges
        .iter()
        .map(|e| {
            (
                e.predicate.clone(),
                e.holder.as_ref().map(|h| h.class.clone()),
            )
        })
        .collect();
    assert_eq!(
        described,
        vec![
            (HAS_CHILD.to_string(), None),
            ("we://child".to_string(), Some("Block".to_string())),
        ]
    );
    // Cached until the installed shapes change.
    assert!(Arc::ptr_eq(&edges, &p.containment_edges().unwrap()));
    p.forget_derived_shapes();
    assert!(!Arc::ptr_eq(&edges, &p.containment_edges().unwrap()));
}

/// Test 6: a record that is a Block with `we://child → a` and also holds `b`
/// by `has_child`: its Container row's `children` is `[a, b]`, and
/// `links: ["children"]` has both links, whichever class the row was read
/// as.
#[tokio::test(flavor = "multi_thread")]
async fn a_container_row_holds_every_edge_of_every_reading() {
    let (mut p, ctx) = setup().await;
    let block = "ad4m://block/1";
    create(&mut p, &ctx, "Block", block).await;
    link(&mut p, &ctx, block, "we://child", "ad4m://a").await;
    link(&mut p, &ctx, block, HAS_CHILD, "ad4m://b").await;

    let rows = query(
        &p,
        CONTAINER_URI,
        json!({"where": {"id": block}, "links": ["children"]}),
    )
    .await;
    assert_eq!(rows.len(), 1, "{rows:?}");
    let row = &rows[0];
    assert_eq!(row["children"], json!(["ad4m://a", "ad4m://b"]), "{row}");
    let links: Vec<(String, String)> = row["__links"]["children"]
        .as_array()
        .unwrap()
        .iter()
        .map(|l| {
            (
                l["data"]["predicate"].as_str().unwrap().to_string(),
                l["data"]["target"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    assert_eq!(
        links,
        vec![
            ("we://child".to_string(), "ad4m://a".to_string()),
            (HAS_CHILD.to_string(), "ad4m://b".to_string()),
        ]
    );
}

/// Rule 1 and open question 3: a node that holds something by `has_child`
/// and has no class (`ad4m://self`) is a Container row of its own, read as
/// `ad4m://Container`. A node that holds nothing is no row; a `we://child`
/// link on a node that is not a Block holds nothing (rule 2); a FluxChannel
/// holds exactly its `has_child` targets (rule 6).
#[tokio::test(flavor = "multi_thread")]
async fn class_less_holders_are_container_rows() {
    let (mut p, ctx) = setup().await;
    link(&mut p, &ctx, "ad4m://self", HAS_CHILD, "ad4m://channel/top").await;
    link(
        &mut p,
        &ctx,
        "ad4m://not-a-block",
        "we://child",
        "ad4m://m2",
    )
    .await;
    let flux = "ad4m://flux/1";
    create(&mut p, &ctx, "FluxChannel", flux).await;
    link(&mut p, &ctx, flux, HAS_CHILD, "ad4m://m3").await;
    link(&mut p, &ctx, flux, "ns://pinned", "ad4m://m4").await;

    let rows = query(
        &p,
        CONTAINER_URI,
        json!({"where": {"id": ["ad4m://self", "ad4m://not-a-block", flux]}}),
    )
    .await;
    assert_eq!(
        ids(&rows),
        vec![flux.to_string(), "ad4m://self".to_string()]
    );
    let by_id = |id: &str| rows.iter().find(|r| r["id"] == id).unwrap().clone();
    let root = by_id("ad4m://self");
    assert_eq!(root["__subjectClass"], CONTAINER_URI, "{root}");
    assert_eq!(root["children"], json!(["ad4m://channel/top"]));
    assert_eq!(by_id(flux)["children"], json!(["ad4m://m3"]));
}

/// `{ id }` is containment; `{ id, predicate }` and `{ model, id }` keep their
/// meaning; a misspelt field is an error, never a containment read.
#[test]
fn a_containment_scope_deserializes_only_from_id_alone() {
    use crate::perspectives::model_query::types::ContainedScope;
    let scope = |v: Value| serde_json::from_value::<Scope>(v);
    assert_eq!(
        scope(json!({"id": "ad4m://p"})).unwrap(),
        Scope::Contained(ContainedScope {
            id: "ad4m://p".into()
        })
    );
    assert!(matches!(
        scope(json!({"id": "ad4m://p", "predicate": "ad4m://has_child"})).unwrap(),
        Scope::Raw { .. }
    ));
    assert!(matches!(
        scope(json!({"id": "ad4m://p", "model": "Channel"})).unwrap(),
        Scope::Model { .. }
    ));
    assert!(scope(json!({"id": "ad4m://p", "predicat": "ad4m://has_child"})).is_err());
    assert!(scope(json!({"id": "ad4m://p", "feld": "children"})).is_err());
    // Through a whole query too: the typo fails the query.
    let typo = r#"{"parent":{"id":"ad4m://p","predicat":"ad4m://has_child"}}"#;
    assert!(serde_json::from_str::<ModelQueryInput>(typo).is_err());
}

/// Test 1: `Message` with `parent: { id: P }`, where `P has_child m1`,
/// returns m1, whatever P is.
#[tokio::test(flavor = "multi_thread")]
async fn a_containment_scope_reads_has_child_on_any_node() {
    let (mut p, ctx) = setup().await;
    create(&mut p, &ctx, "Message", "ad4m://m1").await;
    create(&mut p, &ctx, "Message", "ad4m://m-elsewhere").await;
    link(&mut p, &ctx, "ad4m://p1", HAS_CHILD, "ad4m://m1").await;
    let rows = query(&p, "Message", json!({"parent": {"id": "ad4m://p1"}})).await;
    assert_eq!(ids(&rows), vec!["ad4m://m1".to_string()]);
}

/// Test 2: P is a Block (`blocks` → `we://child`, alias `children`):
/// `parent: { id: P }` returns its `we://child` child beside its `has_child`
/// one.
#[tokio::test(flavor = "multi_thread")]
async fn a_containment_scope_reads_an_aliased_predicate_on_its_class() {
    let (mut p, ctx) = setup().await;
    let block = "ad4m://block/2";
    create(&mut p, &ctx, "Block", block).await;
    create(&mut p, &ctx, "Message", "ad4m://m-alias").await;
    create(&mut p, &ctx, "Message", "ad4m://m-has-child").await;
    link(&mut p, &ctx, block, "we://child", "ad4m://m-alias").await;
    link(&mut p, &ctx, block, HAS_CHILD, "ad4m://m-has-child").await;
    let rows = query(&p, "Message", json!({"parent": {"id": block}})).await;
    assert_eq!(
        ids(&rows),
        vec![
            "ad4m://m-alias".to_string(),
            "ad4m://m-has-child".to_string()
        ]
    );
}

/// Test 3, the other direction: Q is not a Block and has `Q we://child m2`:
/// `parent: { id: Q }` does not return m2. On a node that is not a Block,
/// `we://child` is not containment (rule 2).
#[tokio::test(flavor = "multi_thread")]
async fn an_aliased_predicate_holds_nothing_on_another_node() {
    let (mut p, ctx) = setup().await;
    let q = "ad4m://not-a-block/3";
    create(&mut p, &ctx, "Message", "ad4m://m2").await;
    link(&mut p, &ctx, q, "we://child", "ad4m://m2").await;
    let rows = query(&p, "Message", json!({"parent": {"id": q}})).await;
    assert!(rows.is_empty(), "{rows:?}");
    // A FluxChannel's other relations are not containment either (rule 6).
    let flux = "ad4m://flux/3";
    create(&mut p, &ctx, "FluxChannel", flux).await;
    link(&mut p, &ctx, flux, "ns://pinned", "ad4m://m2").await;
    assert!(query(&p, "Message", json!({"parent": {"id": flux}}))
        .await
        .is_empty());
}

/// Test 4, the root: `parent: { id: "ad4m://self" }` over `Container`
/// returns the class-less top-level channel (it holds a message by
/// `has_child`), and a Block hung under the root.
#[tokio::test(flavor = "multi_thread")]
async fn the_root_holds_class_less_channels() {
    let (mut p, ctx) = setup().await;
    let top = "ad4m://channel/top";
    link(&mut p, &ctx, "ad4m://self", HAS_CHILD, top).await;
    link(&mut p, &ctx, top, HAS_CHILD, "ad4m://m/top").await;
    let block = "ad4m://block/top";
    create(&mut p, &ctx, "Block", block).await;
    link(&mut p, &ctx, "ad4m://self", HAS_CHILD, block).await;
    // Not under the root.
    link(&mut p, &ctx, "ad4m://elsewhere", HAS_CHILD, "ad4m://m/x").await;

    let rows = query(&p, CONTAINER_URI, json!({"parent": {"id": "ad4m://self"}})).await;
    assert_eq!(ids(&rows), vec![block.to_string(), top.to_string()]);
    let channel = rows.iter().find(|r| r["id"] == top).unwrap();
    assert_eq!(channel["__subjectClass"], CONTAINER_URI);
    assert_eq!(channel["children"], json!(["ad4m://m/top"]));
}

/// A subscription, its passes driven by hand as in the abstract-class tests.
pub(super) struct Sub {
    pub(super) id: String,
    rx: tokio::sync::broadcast::Receiver<String>,
}

impl Sub {
    pub(super) async fn open(p: &PerspectiveInstance, class: &str, q: Value) -> (Sub, String) {
        use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};
        let pubsub = get_global_pubsub().await;
        let rx = pubsub
            .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
            .await;
        let (id, _, initial) = p
            .model_subscribe_and_query(QueryClasses::One(class.into()), q.to_string(), "c".into())
            .await
            .expect("subscribe");
        (Sub { id, rx }, initial)
    }

    /// One pass over `links` written; the update it pushed, if any, within
    /// 5 s.
    pub(super) async fn pass(
        &mut self,
        p: &PerspectiveInstance,
        links: &[(&str, &str, &str)],
    ) -> Option<String> {
        let mut writes = super::super::subscription_triggers::Writes::new();
        for (s, pr, t) in links {
            writes.record(s, pr, t);
        }
        p.check_subscribed_queries(ChangedPredicates::Specific(writes))
            .await;
        let wait = async {
            loop {
                let msg = self.rx.recv().await.expect("pubsub receiver closed");
                let update: Value = serde_json::from_str(&msg).unwrap();
                if update["perspectiveUuid"] == p.uuid && update["subscriptionId"] == self.id {
                    return update.to_string();
                }
            }
        };
        tokio::time::timeout(Duration::from_secs(5), wait)
            .await
            .ok()
    }
}

/// Test 8: a subscription on `Message` with `parent: { id: P }`. `Block` is
/// installed after it opened; then P gets Block's flag, then
/// `P we://child m3` alone in its pass: the subscription fires with m3, so its
/// trigger follows the new edge. A second subscription: the `has_child` link
/// is removed and the row leaves.
#[tokio::test(flavor = "multi_thread")]
async fn a_containment_subscription_follows_new_edges_and_removals() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("Message", MESSAGE_SDNA)]).await;
    let parent = "ad4m://later-a-block";
    create(&mut p, &ctx, "Message", "ad4m://m3").await;
    let (mut sub, initial) = Sub::open(&p, "Message", json!({"parent": {"id": parent}})).await;
    assert!(initial.contains("\"totalCount\":0"), "{initial}");

    p.add_sdna(
        "Block".into(),
        String::new(),
        SdnaType::SubjectClass,
        Some(BLOCK_SDNA.into()),
        &ctx,
    )
    .await
    .unwrap();
    sub.pass(
        &p,
        &[("ns://BlockShape", "sh://property", "ns://Block.blocks")],
    )
    .await;
    create(&mut p, &ctx, "Block", parent).await;
    sub.pass(&p, &[(parent, "ns://type", "ns://block")]).await;
    link(&mut p, &ctx, parent, "we://child", "ad4m://m3").await;
    let update = sub
        .pass(&p, &[(parent, "we://child", "ad4m://m3")])
        .await
        .expect("the new edge's write re-runs the subscription");
    assert!(update.contains("ad4m://m3"), "{update}");

    // The row leaves when its has_child link is removed.
    let p2 = "ad4m://p2";
    create(&mut p, &ctx, "Message", "ad4m://m4").await;
    let held = p
        .add_link(
            Link {
                source: p2.into(),
                predicate: Some(HAS_CHILD.into()),
                target: "ad4m://m4".into(),
            },
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .unwrap();
    let (mut sub2, initial) = Sub::open(&p, "Message", json!({"parent": {"id": p2}})).await;
    assert!(initial.contains("ad4m://m4"), "{initial}");
    p.remove_link(held.into(), None).await.unwrap();
    let update = sub2
        .pass(&p, &[(p2, HAS_CHILD, "ad4m://m4")])
        .await
        .expect("a removed containment link re-runs the subscription");
    assert!(
        !update.contains("ad4m://m4") && update.contains("\"totalCount\":0"),
        "{update}"
    );
}

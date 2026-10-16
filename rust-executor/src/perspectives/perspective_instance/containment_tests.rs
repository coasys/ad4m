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

/// The targets of `source`'s links on `predicate`, sorted.
async fn targets(p: &PerspectiveInstance, source: &str, predicate: &str) -> Vec<String> {
    let mut out: Vec<String> = p
        .get_links(&LinkQuery {
            source: Some(source.into()),
            predicate: Some(predicate.into()),
            ..Default::default()
        })
        .await
        .unwrap()
        .into_iter()
        .map(|l| l.data.target)
        .collect();
    out.sort();
    out
}

/// Writes through `Container` (open question 5): under a Block its own
/// adder runs (`we://child`); under a node no class describes, and under a
/// FluxChannel (not a Container), one `has_child` link. `updateSubject` on
/// `ad4m://Container` is the same write, adder and remover.
#[tokio::test(flavor = "multi_thread")]
async fn writes_under_a_parent_go_through_its_own_relation_or_has_child() {
    let (mut p, ctx) = setup().await;
    let block = "ad4m://block/w";
    create(&mut p, &ctx, "Block", block).await;
    let flux = "ad4m://flux/w";
    create(&mut p, &ctx, "FluxChannel", flux).await;

    for parent in [block, "ad4m://plain/w", flux] {
        p.add_to_container(parent, "ad4m://c/1", None, &ctx)
            .await
            .unwrap_or_else(|e| panic!("add under {parent}: {e}"));
    }
    assert_eq!(targets(&p, block, "we://child").await, vec!["ad4m://c/1"]);
    assert!(targets(&p, block, HAS_CHILD).await.is_empty());
    assert_eq!(
        targets(&p, "ad4m://plain/w", HAS_CHILD).await,
        vec!["ad4m://c/1"]
    );
    assert_eq!(targets(&p, flux, HAS_CHILD).await, vec!["ad4m://c/1"]);

    let through =
        |action: &str, child: &str| json!({"children": {"action": action, "value": [child]}});
    p.update_subject(
        class(CONTAINER_URI),
        "ad4m://plain/w".into(),
        through("adder", "ad4m://c/2"),
        None,
        &ctx,
    )
    .await
    .unwrap();
    p.update_subject(
        class(CONTAINER_URI),
        "ad4m://plain/w".into(),
        through("remover", "ad4m://c/1"),
        None,
        &ctx,
    )
    .await
    .unwrap();
    assert_eq!(
        targets(&p, "ad4m://plain/w", HAS_CHILD).await,
        vec!["ad4m://c/2"]
    );
    p.update_subject(
        class(CONTAINER_URI),
        block.into(),
        through("remover", "ad4m://c/1"),
        None,
        &ctx,
    )
    .await
    .unwrap();
    assert!(targets(&p, block, "we://child").await.is_empty());
}

/// The ids of `rows`, in their order.
fn ordered(rows: &[Value]) -> Vec<String> {
    rows.iter()
        .map(|r| r["id"].as_str().unwrap().to_string())
        .collect()
}

/// Test 5: a `Container` step in from X returns everything that holds X:
/// `ad4m://self`, which no class describes, and a Block holding it through
/// `we://child`. A node linking X by `we://child` that is not a Block does
/// not hold it.
#[tokio::test(flavor = "multi_thread")]
async fn a_containment_step_in_finds_every_holder() {
    let (mut p, ctx) = setup().await;
    let x = "ad4m://x/5";
    let block = "ad4m://block/5";
    create(&mut p, &ctx, "Block", block).await;
    link(&mut p, &ctx, "ad4m://self", HAS_CHILD, x).await;
    link(&mut p, &ctx, block, "we://child", x).await;
    link(&mut p, &ctx, "ad4m://not-a-block/5", "we://child", x).await;

    let rows = query(
        &p,
        CONTAINER_URI,
        json!({"parent": {"ids": [x], "direction": "in"}}),
    )
    .await;
    assert_eq!(
        ids(&rows),
        vec![block.to_string(), "ad4m://self".to_string()]
    );
}

/// Test 7: a transitive walk in from a message over mixed hops (`has_child`
/// into a class-less thread, `we://child` into a Block, `has_child` into
/// `ad4m://self`) returns thread, block and root, nearest first; `maxDepth: 1`
/// returns the parent only. A step out and a transitive walk out read the
/// same edges downwards.
#[tokio::test(flavor = "multi_thread")]
async fn a_containment_walk_follows_every_edge_nearest_first() {
    let (mut p, ctx) = setup().await;
    let m = "ad4m://m/7";
    let thread = "ad4m://thread/7";
    let block = "ad4m://block/7";
    create(&mut p, &ctx, "Message", m).await;
    create(&mut p, &ctx, "Block", block).await;
    link(&mut p, &ctx, thread, HAS_CHILD, m).await;
    link(&mut p, &ctx, block, "we://child", thread).await;
    link(&mut p, &ctx, "ad4m://self", HAS_CHILD, block).await;
    // Not containment: a we://child on a node that is not a Block.
    link(&mut p, &ctx, "ad4m://imposter/7", "we://child", thread).await;

    let up = |depth: Option<usize>| {
        let mut scope = json!({"ids": [m], "direction": "in", "transitive": true});
        if let Some(d) = depth {
            scope["maxDepth"] = json!(d);
        }
        json!({ "parent": scope })
    };
    let rows = query(&p, CONTAINER_URI, up(None)).await;
    assert_eq!(
        ordered(&rows),
        vec![
            thread.to_string(),
            block.to_string(),
            "ad4m://self".to_string()
        ]
    );
    let rows = query(&p, CONTAINER_URI, up(Some(1))).await;
    assert_eq!(ordered(&rows), vec![thread.to_string()]);
    let rows = query(&p, CONTAINER_URI, up(Some(2))).await;
    assert_eq!(ordered(&rows), vec![thread.to_string(), block.to_string()]);

    // Downwards: the Messages under the root, at any depth.
    let rows = query(
        &p,
        "Message",
        json!({"parent": {"ids": ["ad4m://self"], "transitive": true}}),
    )
    .await;
    assert_eq!(ordered(&rows), vec![m.to_string()]);
    // One step out of the block: the thread, through we://child.
    let rows = query(&p, CONTAINER_URI, json!({"parent": {"ids": [block]}})).await;
    assert_eq!(ordered(&rows), vec![thread.to_string()]);

    // maxDepth without transitive, and levels on a containment walk, are refused.
    let refused = p
        .model_query_classes(
            &QueryClasses::One("Message".into()),
            &json!({"parent": {"ids": [m], "maxDepth": 1}}).to_string(),
        )
        .await
        .unwrap_err();
    assert!(refused.to_string().contains("maxDepth"), "{refused}");
    let refused = p
        .model_query_classes(
            &QueryClasses::One("Message".into()),
            &json!({"parent": {"ids": [m], "levels": [2]}}).to_string(),
        )
        .await
        .unwrap_err();
    assert!(refused.to_string().contains("levels"), "{refused}");
}

/// The rows and `totalCount` of a query over `classes`.
async fn query_page(p: &PerspectiveInstance, classes: &str, q: Value) -> (Vec<String>, u64) {
    let json = p
        .model_query_classes(&QueryClasses::One(classes.into()), &q.to_string())
        .await
        .unwrap_or_else(|e| panic!("query {classes} {q}: {e}"));
    let result: Value = serde_json::from_str(&json).unwrap();
    (
        ordered(result["instances"].as_array().unwrap()),
        result["totalCount"].as_u64().unwrap(),
    )
}

/// A Block `h` holding `a` through `we://child` and `b` through `has_child`
/// (Lal's #1461 review): a `where` on `children` reads both edges, whichever
/// class the row is read as. Red on 8d659d6b7, where each reading filtered
/// its own relation and the rows were merged after: `NOT contains a` kept
/// `h` through the `has_child` reading, and an `AND` across the two edges
/// matched neither reading.
#[tokio::test(flavor = "multi_thread")]
async fn a_container_children_filter_reads_every_edge() {
    let (mut p, ctx) = setup().await;
    let (a, b) = ("ad4m://a/f", "ad4m://b/f");
    let h = "ad4m://block/f";
    create(&mut p, &ctx, "Block", h).await;
    link(&mut p, &ctx, h, "we://child", a).await;
    link(&mut p, &ctx, h, HAS_CHILD, b).await;
    // Holds b only, through has_child, and an empty Block.
    let g = "ad4m://thread/f";
    link(&mut p, &ctx, g, HAS_CHILD, b).await;
    let empty = "ad4m://block/empty-f";
    create(&mut p, &ctx, "Block", empty).await;

    let rows = |q: Value| {
        let p = &p;
        async move { query_page(p, CONTAINER_URI, q).await.0 }
    };
    // Negative: h holds a, through we://child.
    assert!(
        rows(json!({"where": {"id": h, "NOT": {"children": {"contains": a}}}}))
            .await
            .is_empty()
    );
    // An AND across the two edges.
    assert_eq!(
        rows(json!({"where": {"id": h, "AND": [
            {"children": {"contains": a}},
            {"children": {"contains": b}}
        ]}}))
        .await,
        vec![h.to_string()]
    );
    // Every spelling of "holds": a bare id, any of a list, `contains`, `eq`.
    for holds_a in [
        json!(a),
        json!([a, "ad4m://nothing"]),
        json!({"contains": a}),
        json!({"eq": a}),
    ] {
        assert_eq!(
            rows(json!({"where": {"children": holds_a}})).await,
            vec![h.to_string()],
            "{holds_a}"
        );
    }
    let mut holding_b = rows(json!({"where": {"children": b}})).await;
    holding_b.sort();
    assert_eq!(holding_b, vec![h.to_string(), g.to_string()]);
    // `children` beside `id` in one clause, and under `OR`.
    assert_eq!(
        rows(json!({"where": {"id": h, "children": a}})).await,
        vec![h.to_string()]
    );
    assert_eq!(
        rows(json!({"where": {"OR": [{"children": a}, {"id": empty}]}}))
            .await
            .len(),
        2
    );
    // `some: {}` and `none: {}`: holds anything, holds nothing.
    let mut some = rows(json!({"where": {"children": {"some": {}}}})).await;
    some.sort();
    assert_eq!(some, vec![h.to_string(), g.to_string()]);
    assert_eq!(
        rows(json!({"where": {"children": {"none": {}}}})).await,
        vec![empty.to_string()]
    );
    // Nothing holds c: no row, and NOT holds every row.
    assert!(rows(json!({"where": {"children": "ad4m://c/f"}}))
        .await
        .is_empty());
    assert_eq!(
        rows(json!({"where": {"NOT": {"children": "ad4m://c/f"}}}))
            .await
            .len(),
        3
    );
}

/// `totalCount` and pages over the same mixed holder: `h` holds `a` through
/// `we://child` and `b` through `has_child`, `g` holds `b`, Block `k` holds
/// `c`. "Not holding a" is `g` and `k`, counted and paged as two rows. Red
/// on 8d659d6b7: `h` passed through its `has_child` reading, so the count
/// was 3 and a page could hold `h`.
#[tokio::test(flavor = "multi_thread")]
async fn a_container_children_filter_counts_and_pages_every_edge() {
    let (mut p, ctx) = setup().await;
    let (a, b, c) = ("ad4m://a/p", "ad4m://b/p", "ad4m://c/p");
    let (h, g, k) = ("ad4m://block/h-p", "ad4m://thread/g-p", "ad4m://block/k-p");
    create(&mut p, &ctx, "Block", h).await;
    link(&mut p, &ctx, h, "we://child", a).await;
    link(&mut p, &ctx, h, HAS_CHILD, b).await;
    link(&mut p, &ctx, g, HAS_CHILD, b).await;
    create(&mut p, &ctx, "Block", k).await;
    link(&mut p, &ctx, k, "we://child", c).await;

    let not_a = |extra: Value| {
        let mut q = json!({"where": {"NOT": {"children": a}}});
        q.as_object_mut()
            .unwrap()
            .extend(extra.as_object().unwrap().clone());
        q
    };
    let (all, total) = query_page(&p, CONTAINER_URI, not_a(json!({}))).await;
    assert_eq!(total, 2, "{all:?}");
    let mut sorted = all.clone();
    sorted.sort();
    assert_eq!(sorted, vec![k.to_string(), g.to_string()]);
    let (first, total) = query_page(&p, CONTAINER_URI, not_a(json!({"limit": 1}))).await;
    assert_eq!((first.len(), total), (1, 2), "{first:?}");
    let (second, total) =
        query_page(&p, CONTAINER_URI, not_a(json!({"limit": 1, "offset": 1}))).await;
    assert_eq!((second.len(), total), (1, 2), "{second:?}");
    let mut paged = [first, second].concat();
    paged.sort();
    assert_eq!(paged, sorted);
    // A count-only read agrees.
    let (none, total) = query_page(&p, CONTAINER_URI, not_a(json!({"limit": 0}))).await;
    assert_eq!((none.len(), total), (0, 2));
    // Holding a: h alone, counted once.
    let (rows, total) = query_page(&p, CONTAINER_URI, json!({"where": {"children": a}})).await;
    assert_eq!((rows, total), (vec![h.to_string()], 1));
}

/// A count projection over `children` on the mixed holder would count one
/// reading's children (1 of `[a, b]`), and is refused; so is an `include`.
/// Red on 8d659d6b7: the projection answered `$n: 1`.
#[tokio::test(flavor = "multi_thread")]
async fn a_container_children_projection_is_refused() {
    let (mut p, ctx) = setup().await;
    let h = "ad4m://block/n";
    create(&mut p, &ctx, "Block", h).await;
    link(&mut p, &ctx, h, "we://child", "ad4m://a/n").await;
    link(&mut p, &ctx, h, HAS_CHILD, "ad4m://b/n").await;
    for (q, says) in [
        (
            json!({"where": {"id": h}, "projections": {"$n": {"from": "children", "count": true}}}),
            "projection",
        ),
        (
            json!({"where": {"id": h}, "include": {"children": true}}),
            "include",
        ),
    ] {
        let err = p
            .model_query_classes(&QueryClasses::One(CONTAINER_URI.into()), &q.to_string())
            .await
            .expect_err(&q.to_string());
        assert!(err.to_string().contains(says), "{q}: {err}");
    }
}

/// The `children` conditions a Container `where` does not answer are refused,
/// not passed to one reading: a nested clause (children are untyped), a
/// per-link author, comparisons, a dotted key.
#[tokio::test(flavor = "multi_thread")]
async fn a_container_children_filter_refuses_what_it_cannot_answer() {
    let (p, _ctx) = setup().await;
    for clause in [
        json!({"children": {"some": {"body": "x"}}}),
        json!({"children": {"none": {"body": "x"}}}),
        json!({"children": {"author": "did:key:x"}}),
        json!({"children": {"not": "ad4m://a"}}),
        json!({"children": {"gt": 1}}),
        json!({"children": {"contains": ["ad4m://a"]}}),
        json!({"NOT": {"children": {"some": {"body": "x"}}}}),
        json!({"children.body": "x"}),
    ] {
        let err = p
            .model_query_classes(
                &QueryClasses::One(CONTAINER_URI.into()),
                &json!({ "where": clause }).to_string(),
            )
            .await
            .expect_err(&clause.to_string());
        assert!(err.to_string().contains("children"), "{clause}: {err}");
    }
}

pub(super) const REVIEWABLE_SDNA: &str = r#"{
  "target_class":"ns://Reviewable",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"review://kind","target":"review://item"}],
  "properties":[
    {"path":"review://kind","name":"kind","has_value":"review://item","min_count":1,"max_count":1}
  ]
}"#;

/// A subscription on concrete `Reviewable` with a containment step in from
/// `m` (Lal's #1461 review). `h` is a Reviewable that already links `m` by
/// `we://child`, but is not a Block, so it holds nothing. When it gets
/// Block's flag it holds `m`, and enters the result: neither end of that
/// write is the anchor or a result node, and `Reviewable` does not read
/// `ns://type`. Red on 8d659d6b7. The same flag on a node that holds no
/// anchor re-runs nothing.
#[tokio::test(flavor = "multi_thread")]
async fn a_containment_step_in_sees_a_holder_become_a_container() {
    let (mut p, _shapes, ctx) =
        setup_perspective_no_llm(&[("Block", BLOCK_SDNA), ("Reviewable", REVIEWABLE_SDNA)]).await;
    let m = "ad4m://m/r";
    let h = "ad4m://h/r";
    create(&mut p, &ctx, "Reviewable", h).await;
    link(&mut p, &ctx, h, "we://child", m).await;
    let (mut sub, initial) = Sub::open(
        &p,
        "Reviewable",
        json!({"parent": {"ids": [m], "direction": "in"}}),
    )
    .await;
    assert!(initial.contains("\"totalCount\":0"), "{initial}");

    // A Block elsewhere: holds no anchor, re-runs nothing.
    let other = "ad4m://other/r";
    link(&mut p, &ctx, other, "we://child", "ad4m://elsewhere/r").await;
    link(&mut p, &ctx, other, "ns://type", "ns://block").await;
    let before = super::super::subscription_triggers::reruns::count(&sub.id);
    let mut writes = super::super::subscription_triggers::Writes::new();
    writes.record(other, "ns://type", "ns://block");
    p.check_subscribed_queries(ChangedPredicates::Specific(writes))
        .await;
    assert_eq!(
        super::super::subscription_triggers::reruns::count(&sub.id),
        before,
        "a holder of no anchor"
    );

    link(&mut p, &ctx, h, "ns://type", "ns://block").await;
    let update = sub
        .pass(&p, &[(h, "ns://type", "ns://block")])
        .await
        .expect("the holder's flag re-runs the subscription");
    assert!(update.contains(h), "{update}");
}

//! The #1237 trigger rules under #812's Local graphs.
//!
//! A model subscription is computed as its subscriber reads: their own Local
//! graph in, other agents' out. Whether a write re-runs it has to follow the
//! same rule. Another agent's Local link must never decide a re-run, and the
//! subscriber's own records must re-run it wherever they are stored, in their
//! Local graph or in a shared named graph. The union query (#1238) reads in
//! the same scope as a one-class query.

use super::reruns;
use crate::agent::AgentContext;
use crate::perspectives::local_graph_tests::{setup, user};
use crate::perspectives::model_query::QueryClasses;
use crate::perspectives::perspective_instance::{
    PerspectiveInstance, SdnaType, SubjectClassOption,
};
use crate::perspectives::sparql_store::LOCAL_GRAPH_ALIAS;
use crate::types::{Link, LinkStatus};
use serde_json::{json, Value};
use std::time::Duration;

const FLAG: &str = "ns://type";
const SHARED_GRAPH: &str = "ad4m://graph/channel-1";

/// SHACL for class `name`: flag `ns://type` = `ns://<name lowercased>`, and
/// optional string properties `(name, predicate)`.
fn shacl(name: &str, props: &[(&str, &str)]) -> String {
    let flag_value = format!("ns://{}", name.to_lowercase());
    let mut properties = vec![json!({
        "path": FLAG, "name": "type", "has_value": flag_value, "min_count": 1, "max_count": 1
    })];
    for (prop, predicate) in props {
        properties.push(json!({
            "path": predicate, "name": prop, "datatype": "xsd:string",
            "min_count": 0, "max_count": 1, "writable": true,
            "setter": [{"action": "setSingleTarget", "source": "this", "predicate": predicate, "target": "value"}]
        }));
    }
    json!({
        "target_class": format!("ns://{name}"),
        "constructor_actions": [{"action": "addLink", "source": "this", "predicate": FLAG, "target": flag_value}],
        "destructor_actions": [],
        "properties": properties,
    })
    .to_string()
}

/// Alice and Bob co-own a perspective with a `Note` (text, status) and a
/// `Task` (title) class.
async fn notes() -> (
    PerspectiveInstance,
    (AgentContext, String),
    (AgentContext, String),
) {
    let alice = user("alice");
    let bob = user("bob");
    let mut p = setup(Some(vec![alice.1.clone(), bob.1.clone()])).await;
    let main = AgentContext::main_agent();
    for (name, props) in [
        (
            "Note",
            &[("text", "ns://text"), ("status", "ns://status")][..],
        ),
        ("Task", &[("title", "ns://title")][..]),
    ] {
        p.add_sdna(
            name.to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(shacl(name, props)),
            &main,
        )
        .await
        .unwrap();
    }
    (p, alice, bob)
}

async fn note(
    p: &mut PerspectiveInstance,
    id: &str,
    text: &str,
    ctx: &AgentContext,
    graph: Option<&str>,
) {
    p.create_subject(
        SubjectClassOption {
            class_name: Some("Note".to_string()),
            query: None,
        },
        id.to_string(),
        Some(json!({ "text": text })),
        None,
        ctx,
        graph.map(str::to_string),
    )
    .await
    .unwrap();
}

async fn add(
    p: &mut PerspectiveInstance,
    (source, predicate, target): (&str, &str, &str),
    status: LinkStatus,
    ctx: &AgentContext,
    graph: Option<&str>,
) {
    p.add_link(
        Link {
            source: source.into(),
            predicate: Some(predicate.into()),
            target: target.into(),
        },
        status,
        None,
        ctx,
        graph.map(str::to_string),
    )
    .await
    .unwrap();
}

/// Run one subscription check over everything written since the last one.
/// Each write records itself on its own task, so wait for the first and let
/// the rest land.
async fn check(p: &PerspectiveInstance) {
    tokio::time::timeout(Duration::from_secs(10), async {
        while !p.subscription_check_pending() {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .expect("no write was recorded for the subscription check");
    tokio::time::sleep(Duration::from_millis(200)).await;
    p.check_subscriptions_now().await;
}

async fn subscribe(p: &PerspectiveInstance, who: &AgentContext, query: Value) -> String {
    p.model_subscribe_and_query(
        "Note".into(),
        query.to_string(),
        who.user_email.clone(),
        None,
        format!("conn-{}", who.user_email.as_deref().unwrap_or("main")),
    )
    .await
    .unwrap()
    .0
}

async fn last_result(p: &PerspectiveInstance, id: &str) -> String {
    p.subscribed_queries
        .lock()
        .await
        .get(id)
        .expect("subscribed")
        .last_result
        .clone()
}

fn ids(result: &str) -> Vec<String> {
    let v: Value = serde_json::from_str(result).unwrap();
    let mut ids: Vec<String> = v["instances"]
        .as_array()
        .unwrap_or_else(|| panic!("no instances in {result}"))
        .iter()
        .filter_map(|i| i["id"].as_str().map(str::to_string))
        .collect();
    ids.sort();
    ids
}

/// Alice creating a Note in her Local graph is a write of the Note flag,
/// which joins every Note subscription's result. It re-runs hers. Bob cannot
/// read it, so it must not re-run his.
#[tokio::test(flavor = "multi_thread")]
async fn another_agents_local_write_does_not_rerun_my_subscription() {
    let (mut p, (alice, _), (bob, _)) = notes().await;
    check(&p).await;
    let mine = subscribe(&p, &alice, json!({})).await;
    let bobs = subscribe(&p, &bob, json!({})).await;

    note(
        &mut p,
        "ns://note/alice",
        "mine",
        &alice,
        Some(LOCAL_GRAPH_ALIAS),
    )
    .await;
    check(&p).await;

    assert_eq!(
        reruns::count(&mine),
        1,
        "Alice's write re-runs her subscription"
    );
    assert_eq!(ids(&last_result(&p, &mine).await), vec!["ns://note/alice"]);
    assert_eq!(
        reruns::count(&bobs),
        0,
        "Alice's Local write re-ran Bob's subscription"
    );
}

/// A write on a record's property re-runs a subscription when the record is
/// an instance of the class (the `scan` rule's flag lookup). That lookup reads
/// where the record is stored: the subscriber's own Local graph, or a shared
/// named graph. A default-graph-only lookup misses both, and the record never
/// enters the result.
#[tokio::test(flavor = "multi_thread")]
async fn a_record_in_a_named_graph_entering_the_filter_reruns() {
    let (mut p, _, (bob, _)) = notes().await;
    let main = AgentContext::main_agent();
    note(
        &mut p,
        "ns://note/bob",
        "draft",
        &bob,
        Some(LOCAL_GRAPH_ALIAS),
    )
    .await;
    note(
        &mut p,
        "ns://note/shared",
        "draft",
        &main,
        Some(SHARED_GRAPH),
    )
    .await;
    check(&p).await;
    let bobs = subscribe(&p, &bob, json!({ "where": { "status": "done" } })).await;
    assert!(ids(&last_result(&p, &bobs).await).is_empty());

    let done = "literal:string:done";
    add(
        &mut p,
        ("ns://note/bob", "ns://status", done),
        LinkStatus::Local,
        &bob,
        Some(LOCAL_GRAPH_ALIAS),
    )
    .await;
    check(&p).await;
    assert_eq!(
        ids(&last_result(&p, &bobs).await),
        vec!["ns://note/bob"],
        "Bob's own Local Note entered his filter"
    );

    add(
        &mut p,
        ("ns://note/shared", "ns://status", done),
        LinkStatus::Shared,
        &main,
        Some(SHARED_GRAPH),
    )
    .await;
    check(&p).await;
    assert_eq!(
        ids(&last_result(&p, &bobs).await),
        vec!["ns://note/bob", "ns://note/shared"],
        "a Note in a shared named graph entered Bob's filter"
    );
}

/// One batch mixes a write only Alice reads (her Local Note) with one both
/// read (a shared Note), checked once. `visible_to(bob)` must drop the first
/// and keep the second: both subscriptions re-run once, and Bob's result,
/// and the update sent to his connection, hold the shared Note only. A
/// `visible_to` that drops too much loses Bob's re-run; one that keeps too
/// much puts Alice's Note in his result (#812 review, Marvin's note 5).
#[tokio::test(flavor = "multi_thread")]
async fn a_mixed_batch_reruns_each_subscriber_on_what_they_read() {
    use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};

    let (mut p, (alice, _), (bob, _)) = notes().await;
    let main = AgentContext::main_agent();
    check(&p).await;
    let alices = subscribe(&p, &alice, json!({})).await;
    let bobs = subscribe(&p, &bob, json!({})).await;
    let mut updates = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    note(
        &mut p,
        "ns://note/alice",
        "mine",
        &alice,
        Some(LOCAL_GRAPH_ALIAS),
    )
    .await;
    note(&mut p, "ns://note/shared", "ours", &main, None).await;
    check(&p).await;

    assert_eq!(reruns::count(&alices), 1, "one batch, one re-run for Alice");
    assert_eq!(
        reruns::count(&bobs),
        1,
        "the shared Note re-runs Bob's subscription"
    );
    assert_eq!(
        ids(&last_result(&p, &alices).await),
        vec!["ns://note/alice", "ns://note/shared"]
    );
    assert_eq!(
        ids(&last_result(&p, &bobs).await),
        vec!["ns://note/shared"],
        "Bob's result holds the shared Note only"
    );

    let mut bob_updates = vec![];
    while let Ok(msg) = updates.try_recv() {
        let v: Value = serde_json::from_str(&msg).unwrap();
        if v["subscriptionId"].as_str() == Some(bobs.as_str()) {
            bob_updates.push(v);
        }
    }
    assert_eq!(bob_updates.len(), 1, "{bob_updates:?}");
    assert_eq!(bob_updates[0]["ids"], json!(["ns://note/shared"]));
    assert!(
        !bob_updates[0].to_string().contains("ns://note/alice"),
        "Bob's update carries Alice's Local Note: {}",
        bob_updates[0]
    );
    assert_eq!(
        bob_updates[0]["connectionId"],
        json!(format!("conn-{}", bob.user_email.as_deref().unwrap())),
        "addressed to Bob's connection"
    );
}

/// Past `MAX_TRACKED_LINKS` a batch keeps only its predicates (`links:
/// None`), and every model subscription falls back to predicate matching.
/// `visible_to` must still split those predicates by graph owner: Alice's
/// Local writes alone are nothing Bob reads, and with one shared write in
/// the same batch he reads only its predicate (#812 review, Marvin's note 2).
#[test]
fn visible_to_past_the_link_cap_keeps_only_what_the_viewer_reads() {
    use super::{Writes, MAX_TRACKED_LINKS};
    use crate::perspectives::sparql_store::local_graph_iri;
    use std::collections::HashSet;

    let (alice, bob) = ("did:key:alice", "did:key:bob");
    let alice_local = local_graph_iri(alice);
    let mut writes = Writes::new();
    for i in 0..=MAX_TRACKED_LINKS {
        writes.record_in(
            Some(&alice_local),
            &format!("ns://n/{i}"),
            "ns://alice-only",
            "literal:string:x",
        );
    }
    assert!(writes.links.is_none(), "the batch is past the cap");
    assert!(
        writes.visible_to(bob).is_empty(),
        "Bob reads none of Alice's Local writes"
    );
    assert_eq!(
        writes.visible_to(alice).predicates,
        HashSet::from(["ns://alice-only".to_string()])
    );

    writes.record_in(None, "ns://n/shared", "ns://status", "literal:string:done");
    let bobs = writes.visible_to(bob);
    assert_eq!(
        bobs.predicates,
        HashSet::from(["ns://status".to_string()]),
        "Bob reads the shared predicate only"
    );
    assert!(bobs.links.is_none(), "still predicate matching");
    assert_eq!(
        writes.visible_to(alice).predicates,
        HashSet::from(["ns://alice-only".to_string(), "ns://status".to_string()])
    );
}

/// The `scan` lookup must not read another agent's Local graph either. Only
/// Alice's Local flag makes `ns://n/x` a Note, so a shared write on its
/// `status` re-runs her subscription and not Bob's.
#[tokio::test(flavor = "multi_thread")]
async fn a_flag_in_another_agents_local_graph_does_not_rerun_my_subscription() {
    let (mut p, (alice, _), (bob, _)) = notes().await;
    let main = AgentContext::main_agent();
    add(
        &mut p,
        ("ns://n/x", FLAG, "ns://note"),
        LinkStatus::Local,
        &alice,
        Some(LOCAL_GRAPH_ALIAS),
    )
    .await;
    check(&p).await;
    let filter = json!({ "where": { "status": "done" } });
    let alices = subscribe(&p, &alice, filter.clone()).await;
    let bobs = subscribe(&p, &bob, filter).await;

    add(
        &mut p,
        ("ns://n/x", "ns://status", "literal:string:done"),
        LinkStatus::Shared,
        &main,
        None,
    )
    .await;
    check(&p).await;

    assert_eq!(
        reruns::count(&alices),
        1,
        "Alice reads her flag, so the write re-runs her subscription"
    );
    assert_eq!(ids(&last_result(&p, &alices).await), vec!["ns://n/x"]);
    assert_eq!(
        reruns::count(&bobs),
        0,
        "the re-run was decided by Alice's Local flag"
    );
}

/// A union query reads in its caller's scope, as a one-class query does:
/// rows, and the classification that tags them. Alice's Local Note and her
/// Local Task flag on a shared Note are hers alone.
#[tokio::test(flavor = "multi_thread")]
async fn a_union_query_reads_in_its_callers_scope() {
    let (mut p, (alice, alice_did), (bob, bob_did)) = notes().await;
    let main = AgentContext::main_agent();
    note(&mut p, "ns://n/shared", "s", &main, None).await;
    note(&mut p, "ns://n/alice", "a", &alice, Some(LOCAL_GRAPH_ALIAS)).await;
    add(
        &mut p,
        ("ns://n/shared", FLAG, "ns://task"),
        LinkStatus::Local,
        &alice,
        Some(LOCAL_GRAPH_ALIAS),
    )
    .await;
    let union = QueryClasses::union(vec!["Note".into(), "Task".into()]);

    let classes_of = |result: &str, id: &str| -> Vec<String> {
        let v: Value = serde_json::from_str(result).unwrap();
        let row = v["instances"]
            .as_array()
            .unwrap()
            .iter()
            .find(|r| r["id"] == id)
            .unwrap_or_else(|| panic!("{id} not in {result}"))
            .clone();
        let mut classes: Vec<String> =
            serde_json::from_value(row["__subjectClasses"].clone()).unwrap();
        classes.sort();
        classes
    };

    let alices = p
        .clone()
        .for_viewer(alice_did)
        .model_query_classes(&union, "{}", None)
        .await
        .unwrap();
    assert_eq!(ids(&alices), vec!["ns://n/alice", "ns://n/shared"]);
    assert_eq!(classes_of(&alices, "ns://n/shared"), vec!["Note", "Task"]);

    let bobs = p
        .clone()
        .for_viewer(bob_did)
        .model_query_classes(&union, "{}", None)
        .await
        .unwrap();
    assert_eq!(ids(&bobs), vec!["ns://n/shared"], "Bob's union: {bobs}");
    assert_eq!(
        classes_of(&bobs, "ns://n/shared"),
        vec!["Note"],
        "Bob's row was classified by Alice's Local flag: {bobs}"
    );

    let (_, _, subscribed) = p
        .model_subscribe_and_query(
            union,
            "{}".to_string(),
            bob.user_email.clone(),
            None,
            "c".into(),
        )
        .await
        .unwrap();
    assert_eq!(ids(&subscribed), vec!["ns://n/shared"], "{subscribed}");
}

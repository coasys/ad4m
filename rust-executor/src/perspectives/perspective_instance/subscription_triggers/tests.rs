//! #1237: which writes re-run a model subscription.
//!
//! Each test writes links the way an app does, then runs one subscription
//! check the way `subscribed_queries_loop` does: wait for the writes'
//! recorder to raise the trigger, take the recorded writes, check. The
//! background loop is not running here. [`reruns::count`] says how many
//! times a subscription's query has re-run.
//!
//! Classes share the flag predicate `ns://type` and differ in its value, as
//! apps built on one base class do.

use super::super::{ChangedPredicates, PerspectiveInstance};
use super::reruns;
use crate::agent::AgentContext;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::types::{DecoratedLinkExpression, Link, LinkStatus};
use serde_json::{json, Value};
use std::collections::HashSet;
use std::sync::atomic::Ordering;
use std::time::Duration;

const FLAG: &str = "ns://type";

/// SDNA for class `name`: flag `ns://type` = `ns://<name lowercased>`,
/// string properties `(name, predicate)`, and hasMany relations
/// `(name, predicate, target class)`; an empty target class declares none
/// (a polymorphic relation).
fn sdna(name: &str, props: &[(&str, &str)], relations: &[(&str, &str, &str)]) -> String {
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
    for (rel, predicate, target) in relations {
        let mut r = json!({ "path": predicate, "name": rel, "relation_kind": "hasMany" });
        if !target.is_empty() {
            r["target_class_name"] = json!(target);
            r["class"] = json!(format!("ns://{target}Shape"));
        }
        properties.push(r);
    }
    json!({
        "target_class": format!("ns://{name}"),
        "constructor_actions": [{"action": "addLink", "source": "this", "predicate": FLAG, "target": flag_value}],
        "properties": properties,
    })
    .to_string()
}

/// Post (title, status, comments → Comment, attachments → any class),
/// Comment (text, replies → Comment), Note (title, body).
async fn blog() -> PerspectiveInstance {
    let post = sdna(
        "Post",
        &[("title", "ns://title"), ("status", "ns://status")],
        &[
            ("comments", "ns://comment", "Comment"),
            ("attachments", "ns://attachment", ""),
        ],
    );
    let comment = sdna(
        "Comment",
        &[("text", "ns://text")],
        &[("replies", "ns://reply", "Comment")],
    );
    let note = sdna(
        "Note",
        &[("title", "ns://title"), ("body", "ns://body")],
        &[],
    );
    let (p, _, _) =
        setup_perspective_no_llm(&[("Post", &post), ("Comment", &comment), ("Note", &note)]).await;
    p
}

async fn add(
    p: &mut PerspectiveInstance,
    source: &str,
    predicate: &str,
    target: &str,
) -> DecoratedLinkExpression {
    p.add_link(
        Link {
            source: source.into(),
            predicate: Some(predicate.into()),
            target: target.into(),
        },
        LinkStatus::Local,
        None,
        &AgentContext::main_agent(),
    )
    .await
    .expect("add_link")
}

/// A record of `class` (its flag link), plus `props` as `(predicate, value)`.
async fn record(p: &mut PerspectiveInstance, id: &str, class: &str, props: &[(&str, &str)]) {
    add(p, id, FLAG, &format!("ns://{}", class.to_lowercase())).await;
    for (predicate, value) in props {
        add(p, id, predicate, &format!("literal:string:{value}")).await;
    }
}

/// Take the writes recorded since the last call, as the loop does after its
/// batch window.
async fn take_writes(p: &PerspectiveInstance) -> ChangedPredicates {
    let deadline = tokio::time::Instant::now() + Duration::from_secs(10);
    while !p.trigger_prolog_subscription_check.load(Ordering::Acquire) {
        assert!(
            tokio::time::Instant::now() < deadline,
            "no write was recorded"
        );
        tokio::time::sleep(Duration::from_millis(5)).await;
    }
    // Every write's recorder runs on its own task; let the rest land.
    tokio::time::sleep(Duration::from_millis(150)).await;
    p.trigger_prolog_subscription_check
        .store(false, Ordering::Release);
    std::mem::replace(
        &mut *p.changed_predicates.lock().await,
        ChangedPredicates::NoneRecorded,
    )
}

/// One subscription check over the writes since the last one.
async fn check(p: &PerspectiveInstance) {
    let writes = take_writes(p).await;
    p.check_subscribed_queries(writes).await;
}

async fn subscribe(p: &PerspectiveInstance, class: &str, query: Value) -> (String, Value) {
    let mut query = query;
    query["includeUnverified"] = json!(true);
    let (id, result) = p
        .model_subscribe_and_query(class.into(), query.to_string(), None)
        .await
        .expect("model subscribe");
    (id, serde_json::from_str(&result).expect("result JSON"))
}

async fn last_result(p: &PerspectiveInstance, id: &str) -> Value {
    let queries = p.subscribed_queries.lock().await;
    serde_json::from_str(&queries.get(id).expect("subscribed").last_result).expect("result JSON")
}

fn ids(result: &Value) -> Vec<String> {
    result["instances"]
        .as_array()
        .expect("instances")
        .iter()
        .map(|i| i["id"].as_str().expect("id").to_string())
        .collect()
}

// ── Too narrow: writes that must re-run ──────────────────────────────────

/// An edit to a property of an included record, a predicate only the
/// included class declares. Red on `dev`: the trigger held `Post`'s
/// predicates only, so the edit re-ran nothing.
#[tokio::test(flavor = "multi_thread")]
async fn an_included_records_edit_reruns_the_including_subscription() {
    let mut p = blog().await;
    record(&mut p, "ns://p1", "Post", &[]).await;
    record(&mut p, "ns://c1", "Comment", &[]).await;
    add(&mut p, "ns://p1", "ns://comment", "ns://c1").await;
    check(&p).await;

    let (id, first) = subscribe(&p, "Post", json!({ "include": { "comments": true } })).await;
    assert!(first.to_string().contains("ns://c1"), "{first}");

    add(&mut p, "ns://c1", "ns://text", "literal:string:edited").await;
    check(&p).await;

    assert_eq!(
        reruns::count(&id),
        1,
        "the edit must re-run the subscription"
    );
    let now = last_result(&p, &id).await;
    assert!(now.to_string().contains("edited"), "{now}");
}

/// The same for a polymorphic include, whose members' class is not known
/// when subscribing. Red on `dev`.
#[tokio::test(flavor = "multi_thread")]
async fn a_polymorphic_members_edit_reruns_the_including_subscription() {
    let mut p = blog().await;
    record(&mut p, "ns://p1", "Post", &[]).await;
    record(&mut p, "ns://n1", "Note", &[]).await;
    add(&mut p, "ns://p1", "ns://attachment", "ns://n1").await;
    check(&p).await;

    let (id, first) = subscribe(
        &p,
        "Post",
        json!({ "include": { "attachments": { "polymorphic": true } } }),
    )
    .await;
    assert!(first.to_string().contains("ns://n1"), "{first}");

    add(&mut p, "ns://n1", "ns://body", "literal:string:attached").await;
    check(&p).await;

    assert_eq!(reruns::count(&id), 1);
    let now = last_result(&p, &id).await;
    assert!(now.to_string().contains("attached"), "{now}");
}

/// Records that entered the result in earlier batches are watched too: a
/// reply added under a new comment, then edited. The reply is two includes
/// deep, so only the result's own nodes say it is shown: it is linked to the
/// comment, not to the post. Red on `dev` (the reply's predicate is
/// `Comment`'s); red with the fix if the trigger kept the first result's nodes.
#[tokio::test(flavor = "multi_thread")]
async fn a_record_that_entered_later_is_watched_two_includes_deep() {
    let mut p = blog().await;
    record(&mut p, "ns://p1", "Post", &[]).await;
    check(&p).await;

    let (id, _) = subscribe(
        &p,
        "Post",
        json!({ "include": { "comments": { "include": { "replies": true } } } }),
    )
    .await;

    record(&mut p, "ns://c1", "Comment", &[]).await;
    add(&mut p, "ns://p1", "ns://comment", "ns://c1").await;
    check(&p).await;
    record(&mut p, "ns://r1", "Comment", &[]).await;
    add(&mut p, "ns://c1", "ns://reply", "ns://r1").await;
    check(&p).await;
    assert!(last_result(&p, &id).await.to_string().contains("ns://r1"));

    let before = reruns::count(&id);
    add(&mut p, "ns://r1", "ns://text", "literal:string:deep").await;
    check(&p).await;

    assert_eq!(
        reruns::count(&id),
        before + 1,
        "the reply's edit must re-run"
    );
    assert!(last_result(&p, &id).await.to_string().contains("deep"));
}

// ── Too wide: writes that must not re-run ───────────────────────────────

/// Creating a record of another class that shares the flag predicate and a
/// property predicate. Red on `dev`: both predicates are `Post`'s too.
#[tokio::test(flavor = "multi_thread")]
async fn another_classs_create_does_not_rerun_the_subscription() {
    let mut p = blog().await;
    record(&mut p, "ns://p1", "Post", &[("ns://title", "post")]).await;
    check(&p).await;

    let (posts, _) = subscribe(&p, "Post", json!({})).await;
    let (notes, _) = subscribe(&p, "Note", json!({})).await;

    record(&mut p, "ns://n2", "Note", &[("ns://title", "note")]).await;
    check(&p).await;

    assert_eq!(reruns::count(&posts), 0, "a Note is not a Post");
    assert_eq!(reruns::count(&notes), 1, "the Note subscription sees it");
    assert_eq!(ids(&last_result(&p, &notes).await), vec!["ns://n2"]);
}

// ── Guards: what predicate matching got right must stay ─────────────────

/// A new record of the subscribed class enters: its flag value says which
/// class it joins.
#[tokio::test(flavor = "multi_thread")]
async fn a_new_record_of_the_class_enters() {
    let mut p = blog().await;
    record(&mut p, "ns://p1", "Post", &[]).await;
    check(&p).await;

    let (id, _) = subscribe(&p, "Post", json!({})).await;
    record(&mut p, "ns://p2", "Post", &[("ns://title", "new")]).await;
    check(&p).await;

    assert_eq!(reruns::count(&id), 1);
    let mut now = ids(&last_result(&p, &id).await);
    now.sort();
    assert_eq!(now, vec!["ns://p1", "ns://p2"]);
}

/// Deleting a record outside the page changes `totalCount`. The record is
/// in no result and no longer carries its flag, so only the removed flag
/// link's value says it was a Post.
#[tokio::test(flavor = "multi_thread")]
async fn deleting_a_record_outside_the_page_reruns() {
    let mut p = blog().await;
    let f1 = add(&mut p, "ns://p1", FLAG, "ns://post").await;
    let f2 = add(&mut p, "ns://p2", FLAG, "ns://post").await;
    check(&p).await;

    let (id, first) = subscribe(&p, "Post", json!({ "limit": 1, "count": true })).await;
    assert_eq!(first["totalCount"], json!(2), "{first}");
    let shown = ids(&first);
    assert_eq!(shown.len(), 1, "{first}");
    let hidden = if shown[0] == "ns://p1" { f2 } else { f1 };

    p.remove_link(hidden.into(), None)
        .await
        .expect("remove_link");
    check(&p).await;

    assert_eq!(reruns::count(&id), 1);
    assert_eq!(last_result(&p, &id).await["totalCount"], json!(1));
}

/// An edit to a record in the last result re-runs.
#[tokio::test(flavor = "multi_thread")]
async fn an_edit_to_a_record_in_the_result_reruns() {
    let mut p = blog().await;
    record(&mut p, "ns://p1", "Post", &[]).await;
    check(&p).await;

    let (id, _) = subscribe(&p, "Post", json!({})).await;
    add(&mut p, "ns://p1", "ns://title", "literal:string:retitled").await;
    check(&p).await;

    assert_eq!(reruns::count(&id), 1);
    assert!(last_result(&p, &id).await.to_string().contains("retitled"));
}

/// A write that moves an existing record into a filter. The record is not in
/// the last result, so only a lookup of its flag finds it is a Post.
#[tokio::test(flavor = "multi_thread")]
async fn a_write_moving_a_record_into_a_filter_reruns() {
    let mut p = blog().await;
    record(&mut p, "ns://p3", "Post", &[]).await;
    check(&p).await;

    let (id, first) = subscribe(&p, "Post", json!({ "where": { "status": "done" } })).await;
    assert!(ids(&first).is_empty(), "{first}");

    add(&mut p, "ns://p3", "ns://status", "literal:string:done").await;
    check(&p).await;

    assert_eq!(reruns::count(&id), 1);
    assert_eq!(ids(&last_result(&p, &id).await), vec!["ns://p3"]);
}

/// A related record that did not conform yet (no flag) enters the include
/// when it gets its flag. It is in no result, and it is not a Post; only its
/// link to a Post in the result says it matters.
#[tokio::test(flavor = "multi_thread")]
async fn a_related_record_that_starts_to_conform_enters_the_include() {
    let mut p = blog().await;
    record(&mut p, "ns://p1", "Post", &[]).await;
    add(&mut p, "ns://c2", "ns://text", "literal:string:late").await;
    add(&mut p, "ns://p1", "ns://comment", "ns://c2").await;
    check(&p).await;

    let (id, first) = subscribe(&p, "Post", json!({ "include": { "comments": true } })).await;
    assert!(!first.to_string().contains("late"), "{first}");

    add(&mut p, "ns://c2", FLAG, "ns://comment").await;
    check(&p).await;

    assert_eq!(reruns::count(&id), 1);
    assert!(last_result(&p, &id).await.to_string().contains("late"));
}

/// A record linked under a parent-scope anchor enters.
#[tokio::test(flavor = "multi_thread")]
async fn a_record_linked_under_the_parent_anchor_enters() {
    let mut p = blog().await;
    record(&mut p, "ns://p4", "Post", &[]).await;
    check(&p).await;

    let (id, first) = subscribe(
        &p,
        "Post",
        json!({ "parent": { "id": "ns://board", "predicate": "ns://child" } }),
    )
    .await;
    assert!(ids(&first).is_empty(), "{first}");

    add(&mut p, "ns://board", "ns://child", "ns://p4").await;
    check(&p).await;

    assert_eq!(reruns::count(&id), 1);
    assert_eq!(ids(&last_result(&p, &id).await), vec!["ns://p4"]);
}

/// A batch too large to keep its links falls back to predicate matching.
#[tokio::test(flavor = "multi_thread")]
async fn a_batch_without_its_links_falls_back_to_predicates() {
    let p = blog().await;
    let (id, _) = subscribe(&p, "Post", json!({})).await;

    let only = |p: &str| ChangedPredicates::Specific(HashSet::from([p.to_string()]).into());
    p.check_subscribed_queries(only("ns://unrelated")).await;
    assert_eq!(reruns::count(&id), 0);
    p.check_subscribed_queries(only("ns://title")).await;
    assert_eq!(reruns::count(&id), 1);
}

// ── Measured: one app's shape ───────────────────────────────────────────

/// The issue's measurement, reproduced: 40 classes sharing the flag
/// predicate, 10 sharing `ns://title`, 32 sharing a `reactions` relation to
/// `Reaction` (class 39), one subscription per class. Prints how many
/// subscriptions each write re-ran.
#[tokio::test(flavor = "multi_thread")]
async fn forty_classes_rerun_counts() {
    let names: Vec<String> = (0..39)
        .map(|i| format!("C{i}"))
        .chain(["Reaction".to_string()])
        .collect();
    let sdnas: Vec<String> = names
        .iter()
        .enumerate()
        .map(|(i, name)| {
            let own = format!("ns://own{i}");
            let mut props = vec![("own", own.as_str())];
            if i < 10 {
                props.push(("title", "ns://title"));
            }
            let relations: &[(&str, &str, &str)] = if i < 32 {
                &[("reactions", "ns://reaction", "Reaction")]
            } else {
                &[]
            };
            sdna(name, &props, relations)
        })
        .collect();
    let classes: Vec<(&str, &str)> = names
        .iter()
        .map(String::as_str)
        .zip(sdnas.iter().map(String::as_str))
        .collect();
    let (mut p, _, _) = setup_perspective_no_llm(&classes).await;

    record(&mut p, "ns://x2", "C2", &[("ns://title", "two")]).await;
    record(&mut p, "ns://x3", "C3", &[]).await;
    record(&mut p, "ns://x7", "C7", &[]).await;
    check(&p).await;

    let mut subs = vec![];
    for name in &names {
        subs.push(subscribe(&p, name, json!({})).await.0);
    }
    let counts = |subs: &[String]| subs.iter().map(|s| reruns::count(s)).collect::<Vec<_>>();

    let mut table = vec![];
    let mut measure = |label: &'static str, before: Vec<usize>, after: Vec<usize>| {
        let n = before.iter().zip(&after).filter(|(b, a)| a > b).count();
        table.push((label, n));
        n
    };

    let before = counts(&subs);
    record(&mut p, "ns://new5", "C5", &[("ns://own5", "x")]).await;
    check(&p).await;
    let create = measure("create a C5 record", before, counts(&subs));

    let before = counts(&subs);
    record(&mut p, "ns://r1", "Reaction", &[("ns://own39", "+1")]).await;
    add(&mut p, "ns://x3", "ns://reaction", "ns://r1").await;
    check(&p).await;
    let reaction = measure("react to a C3 record", before, counts(&subs));

    let before = counts(&subs);
    add(&mut p, "ns://x2", "ns://title", "literal:string:retitled").await;
    check(&p).await;
    let title = measure("edit a C2 record's title", before, counts(&subs));

    let before = counts(&subs);
    add(&mut p, "ns://x7", "ns://own7", "literal:string:x").await;
    check(&p).await;
    let own = measure("edit a C7-only property", before, counts(&subs));

    for (label, n) in &table {
        println!("#1237 rerun count | {label:<26} | {n:>2} / 40");
    }
    assert_eq!(create, 1, "only C5");
    assert_eq!(reaction, 2, "C3 (its record) and Reaction (a new record)");
    assert_eq!(title, 1, "only C2");
    assert_eq!(own, 1, "only C7");
}

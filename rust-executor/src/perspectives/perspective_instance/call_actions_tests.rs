//! `call` actions on a shape (design §3): the constructor, a property's
//! setter and the destructor call into a language instead of writing
//! links. `mount-test` stands in for Acorn's language; its `project`
//! address records every call it served, which these tests read back.

use super::*;
use crate::languages::mount_tests::{
    interact, load_mount_test_language, mount_thing_shape,
};
use serde_json::{json, Value};

fn class(name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(name.to_string()),
        query: None,
    }
}

/// The `MountThing` shape, plus a `fail` property on the same `update`
/// call as `done`: `{"fail": "boom"}` makes the language throw
/// `update: boom`.
async fn thing_shape(lang: &str) -> String {
    let mut shape: Value = serde_json::from_str(&mount_thing_shape(lang).await).unwrap();
    let update = shape["properties"][1]["setter"].clone();
    shape["properties"].as_array_mut().unwrap().push(json!({
        "path": "mounttest://fail",
        "name": "fail",
        "max_count": 1,
        "writable": true,
        "setter": update,
    }));
    shape.to_string()
}

/// A private perspective with `shape` registered as `class_name`'s SDNA.
async fn mounted(class_name: &str, shape: String) -> (PerspectiveInstance, AgentContext) {
    let (mut p, _, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.add_sdna(
        class_name.to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shape),
        &ctx,
    )
    .await
    .unwrap();
    (p, ctx)
}

/// Every call the language has served, in order: `{address, name, params}`.
async fn calls(lang: &str) -> Vec<Value> {
    interact(&format!("{lang}://project"), "calls", json!({}))
        .await
        .unwrap()
        .as_array()
        .unwrap()
        .clone()
}

/// A MountThing with a `{"title": "a"}` record the language knows as `t1`.
async fn created_thing(lang: &str, ctx: &AgentContext, p: &mut PerspectiveInstance) -> String {
    p.create_subject(
        class("MountThing"),
        "ignored".to_string(),
        Some(json!({"title": "a"})),
        None,
        ctx,
    )
    .await
    .unwrap();
    format!("{lang}://t1")
}

async fn link_count(p: &PerspectiveInstance) -> usize {
    p.get_links(&LinkQuery::default()).await.unwrap().len()
}

#[tokio::test]
async fn create_through_a_call_returns_the_new_url() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;

    let url = p
        .create_subject(
            class("MountThing"),
            "ignored".to_string(),
            Some(json!({"title": "a"})),
            None,
            &ctx,
        )
        .await
        .unwrap();
    assert_eq!(url, Some(format!("{lang}://t1")));

    let calls = calls(&lang).await;
    assert_eq!(
        calls,
        vec![json!({"address": "class/MountThing", "name": "create", "params": {"title": "a"}})]
    );

    // The call itself wrote no links: neither the caller's address nor
    // the new one appears in the perspective.
    for source in ["ignored".to_string(), format!("{lang}://t1")] {
        assert!(
            p.get_links(&LinkQuery {
                source: Some(source.clone()),
                ..Default::default()
            })
            .await
            .unwrap()
            .is_empty(),
            "a link with source {source} exists"
        );
    }
}

#[tokio::test]
async fn two_properties_make_one_update_call() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    p.update_subject(
        class("MountThing"),
        base,
        json!({"title": "b", "done": true}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(
        updates.len(),
        1,
        "the two setters must group into one update call: {calls:?}"
    );
    assert_eq!(updates[0]["address"], json!("t1"));
    assert_eq!(updates[0]["params"], json!({"title": "b", "done": true}));
}

#[tokio::test]
async fn remove_calls_delete() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    p.remove_subject(class("MountThing"), base, None, &ctx)
        .await
        .unwrap();

    let calls = calls(&lang).await;
    assert_eq!(
        calls.last().unwrap(),
        &json!({"address": "t1", "name": "delete", "params": {}})
    );
}

#[tokio::test]
async fn a_throwing_call_fails_with_its_text_and_writes_nothing() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;
    let before = link_count(&p).await;

    let err = p
        .update_subject(
            class("MountThing"),
            base,
            json!({"fail": "boom"}),
            None,
            &ctx,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("update: boom"), "{err}");
    assert_eq!(link_count(&p).await, before);
}

#[tokio::test]
async fn a_call_inside_a_caller_batch_is_refused() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    let batch = p.create_batch().await;
    let err = p
        .update_subject(
            class("MountThing"),
            base,
            json!({"done": true}),
            Some(batch.clone()),
            &ctx,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("cannot run inside a batch"), "{err}");
    assert_eq!(calls(&lang).await.len(), 1, "the update never ran");
    p.discard_batch(&batch).await;
}

#[tokio::test]
async fn a_constructor_must_return_an_address() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    shape["target_class"] = json!("mounttest://Bad");
    // `calls` exists on `project` but answers an array, not a delta: no
    // address to name the new instance.
    shape["constructor_actions"] = json!([{
        "action": "call",
        "source": format!("{lang}://project"),
        "predicate": "calls",
        "target": "{}"
    }]);
    let (mut p, ctx) = mounted("MountBad", shape.to_string()).await;

    let err = p
        .create_subject(class("MountBad"), "ignored".to_string(), None, None, &ctx)
        .await
        .unwrap_err();
    assert!(err.to_string().contains("create returned no address"), "{err}");
}

#[tokio::test]
async fn a_mixed_action_list_is_refused() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    shape["properties"][1]["setter"]
        .as_array_mut()
        .unwrap()
        .push(json!({
            "action": "setSingleTarget",
            "source": "this",
            "predicate": "mounttest://title",
            "target": "value"
        }));
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    let err = p
        .update_subject(class("MountThing"), base, json!({"title": "b"}), None, &ctx)
        .await
        .unwrap_err();
    assert!(err.to_string().contains("mixes call and link actions"), "{err}");
}

#[tokio::test]
async fn a_call_target_is_not_parameter_substituted() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    // The call's own params are its `target` object; the caller's values
    // merge over them and are never substituted inside the text.
    shape["properties"][2]["setter"][0]["target"] = json!("{\"title\":\"fixed\"}");
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    p.update_subject(
        class("MountThing"),
        base,
        json!({"done": true}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 1);
    assert_eq!(
        updates[0]["params"],
        json!({"title": "fixed", "done": true})
    );
}

/// two setters sharing one `(source, interaction)` group merge
/// each action's `target` defaults into the group together with the
/// caller's values. A later property's default for an earlier property
/// must not overwrite the caller's explicit value: the group's defaults
/// merge first, the caller values last — so the caller wins regardless of
/// which property's contribution lands last.
#[tokio::test]
async fn grouped_calls_merge_defaults_before_caller_values() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    // `done`'s setter defaults `title`; `title`'s setter defaults `done`
    // and `tags`; `tags`' setter defaults `title`. Every setter shares the
    // `update` interaction, so all three land in one call group.
    shape["properties"][2]["setter"][0]["target"] = json!("{\"title\":\"from-done-default\"}");
    shape["properties"][1]["setter"][0]["target"] =
        json!("{\"done\":\"from-title-default\",\"tags\":\"from-title-tags\"}");
    shape["properties"][3]["setter"][0]["target"] = json!("{\"title\":\"from-tags-default\"}");
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    // done < title: `done` merges first, and `title`'s default for `done`
    // would overwrite the caller's `true`. `title`'s `tags` default is not
    // caller-supplied, so it must survive.
    p.update_subject(
        class("MountThing"),
        base.clone(),
        json!({"done": true, "title": "x"}),
        None,
        &ctx,
    )
    .await
    .unwrap();
    // tags < title: `tags` merges first, and `title`'s default for `tags`
    // would overwrite the caller's `["g"]`. `title`'s `done` default is
    // not caller-supplied here, so it must survive.
    p.update_subject(
        class("MountThing"),
        base,
        json!({"tags": ["g"], "title": "y"}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 2, "one grouped update per write: {calls:?}");
    assert_eq!(
        updates[0]["params"],
        json!({"title": "x", "done": true, "tags": "from-title-tags"}),
        "a later property's default overwrote a caller value: {calls:?}"
    );
    assert_eq!(
        updates[1]["params"],
        json!({"title": "y", "tags": ["g"], "done": "from-title-default"}),
        "caller values must win over every default, defaults over nothing: {calls:?}"
    );
}

#[tokio::test]
async fn abstract_update_groups_by_concrete_name() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    // `title` answers to the abstract `TextContent.text` through this
    // alias; the call's params carry the concrete name, `title`.
    shape["properties"][1]["alias"] = json!({"TextContent": "text"});
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    // The mounted record's class flag is a link — the A4 delta apply
    // writes it; until then the test writes it by hand.
    p.add_link(
        Link {
            source: base.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "mounttest://Thing".to_string(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .unwrap();

    p.update_subject(
        class("ad4m://TextContent"),
        base,
        json!({"text": "z"}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 1, "one grouped update: {calls:?}");
    assert_eq!(updates[0]["params"], json!({"title": "z"}));
}

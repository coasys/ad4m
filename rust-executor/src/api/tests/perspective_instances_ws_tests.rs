//! `perspective.mutate` through the real handler map.
//!
//! Written from the method contract: a valid write lands and reads back, a write that breaks the
//! class's shape is refused with the errors as data (422) and the failing op's index, a call
//! writes all its ops or none, a later op sees the earlier ones, `batch` keeps the writes
//! invisible until the caller's commit, and the guard is the perspective UPDATE guard.

use serde_json::{json, Value};

use super::multi_user::{expect_code, Executor, RegisteredPerspective};
use super::support::{admin_ctx, registered_perspective};
use crate::agent::capabilities::{perspective_query_capability, perspective_update_capability};
use crate::api::ws_handler::{build_handler_map, HandlerMap, WsRpcError};
use crate::types::{LinkQuery, RequestContext};
use std::sync::Arc;

/// A Task: `title` (required string), `status` (`sh:in` todo / doing), `rank` (integer) and
/// `labels` (a collection of URIs).
const TASK_SHACL: &str = r#"{
  "target_class": "ns://Task",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Task"}
  ],
  "properties": [
    {"path":"ns://task_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://task_title","target":"value"}]},
    {"path":"ns://task_status","name":"status","datatype":"xsd:string","min_count":0,"max_count":1,"writable":true,
     "in":[{"value":"todo"},{"value":"doing","label":"Doing"}],
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://task_status","target":"value"}]},
    {"path":"ns://task_rank","name":"rank","datatype":"xsd:integer","min_count":0,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://task_rank","target":"value"}]},
    {"path":"ns://task_label","name":"labels","collection":true,"writable":true,
     "adder":[{"action":"addLink","source":"this","predicate":"ns://task_label","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://task_label","target":"value"}]}
  ]
}"#;

/// A second `Task` — same name as `ns://Task`, another target class IRI: two
/// registered classes may share a name when their shapes sync in from peers.
const OTHER_TASK_SHACL: &str = r#"{
  "target_class": "other://Task",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"other://Task"}
  ],
  "properties": [
    {"path":"other://task_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"other://task_title","target":"value"}]}
  ]
}"#;

const ITEM: &str = "ad4m://label/urgent";

struct World {
    handlers: HandlerMap,
    uuid: String,
    _registered: super::support::Registered,
}

fn create_op(values: Value) -> Value {
    json!({ "op": "create", "class": "Task", "values": values })
}

fn create_op_at(id: &str, values: Value) -> Value {
    json!({ "op": "create", "class": "Task", "id": id, "values": values })
}

fn update_op(id: &str, body: Value) -> Value {
    let mut op = json!({ "op": "update", "class": "Task", "id": id });
    for (key, value) in body.as_object().expect("update body").iter() {
        op[key] = value.clone();
    }
    op
}

fn delete_op(id: &str) -> Value {
    json!({ "op": "delete", "class": "Task", "id": id })
}

impl World {
    async fn new() -> Self {
        let registered = registered_perspective(&[("Task", TASK_SHACL)]).await;
        World {
            handlers: build_handler_map(),
            uuid: registered.0.clone(),
            _registered: registered,
        }
    }

    async fn call(&self, method: &str, mut params: Value) -> Result<Value, WsRpcError> {
        params["uuid"] = json!(self.uuid);
        self.handlers.dispatch(method, params, admin_ctx()).await
    }

    async fn ok(&self, method: &str, params: Value) -> Value {
        self.call(method, params)
            .await
            .unwrap_or_else(|e| panic!("{method}: {} {}", e.code, e.message))
    }

    async fn mutate(&self, ops: Vec<Value>) -> Result<Value, WsRpcError> {
        self.call("perspective.mutate", json!({ "ops": ops })).await
    }

    async fn mutate_ok(&self, ops: Vec<Value>) -> Value {
        self.ok("perspective.mutate", json!({ "ops": ops })).await
    }

    async fn mutate_in(&self, batch: &str, ops: Vec<Value>) -> Result<Value, WsRpcError> {
        self.call("perspective.mutate", json!({ "ops": ops, "batch": batch }))
            .await
    }

    async fn create_batch(&self) -> String {
        self.ok("perspective.createBatch", json!({}))
            .await
            .as_str()
            .expect("batch id")
            .to_string()
    }

    async fn commit(&self, batch: &str) {
        self.ok("perspective.commitBatch", json!({ "batchId": batch }))
            .await;
    }

    /// Every link in the perspective.
    async fn link_count(&self) -> usize {
        crate::perspectives::get_perspective(&self.uuid)
            .expect("registered perspective")
            .get_links(&LinkQuery::default())
            .await
            .expect("links")
            .len()
    }

    /// The stored links that mention `id` as source or target.
    async fn links_touching(&self, id: &str) -> usize {
        let perspective =
            crate::perspectives::get_perspective(&self.uuid).expect("registered perspective");
        let mut touching = 0;
        for query in [
            LinkQuery {
                source: Some(id.to_string()),
                ..Default::default()
            },
            LinkQuery {
                target: Some(id.to_string()),
                ..Default::default()
            },
        ] {
            touching += perspective.get_links(&query).await.expect("links").len();
        }
        touching
    }

    /// The stored links `source --predicate--> target`.
    async fn links_between(&self, source: &str, predicate: Option<&str>, target: &str) -> usize {
        crate::perspectives::get_perspective(&self.uuid)
            .expect("registered perspective")
            .get_links(&LinkQuery {
                source: Some(source.to_string()),
                predicate: predicate.map(str::to_string),
                target: Some(target.to_string()),
                ..Default::default()
            })
            .await
            .expect("links")
            .len()
    }

    /// The Task at `id` as a model query reads it, or `None`.
    async fn task(&self, id: &str) -> Option<Value> {
        let raw = self
            .ok(
                "perspective.modelQuery",
                json!({
                    "class_name": "Task",
                    "query_json": json!({ "where": { "id": id }, "limit": 1 }).to_string(),
                }),
            )
            .await;
        let parsed: Value = serde_json::from_str(raw.as_str().expect("query result string"))
            .expect("query result JSON");
        parsed["instances"]
            .as_array()
            .and_then(|a| a.first())
            .cloned()
    }

    /// Create a Task with `values`; its id.
    async fn create(&self, values: Value) -> String {
        let done = self.mutate_ok(vec![create_op(values)]).await;
        done["ids"][0].as_str().expect("the new id").to_string()
    }
}

/// The refusal's validation errors, after checking it is a 422 `validation` error.
fn validation_errors(error: &WsRpcError) -> Vec<Value> {
    assert_eq!(error.code, 422, "{}", error.message);
    let data = error.data.as_ref().expect("the error carries data");
    assert_eq!(data["kind"], "validation", "{data}");
    assert_eq!(data["className"], "Task", "{data}");
    data["errors"].as_array().expect("errors array").clone()
}

fn error_for<'a>(errors: &'a [Value], property: &str) -> &'a Value {
    errors
        .iter()
        .find(|e| e["property"] == property)
        .unwrap_or_else(|| panic!("no error names {property}: {errors:?}"))
}

#[tokio::test(flavor = "multi_thread")]
async fn a_valid_create_reads_back_with_its_values() {
    let world = World::new().await;
    let id = world
        .create(json!({ "title": "Write the spec", "status": "doing", "rank": "3" }))
        .await;
    assert!(id.starts_with("ad4m://obj/"), "{id}");

    let task = world.task(&id).await.expect("the task reads back");
    assert_eq!(task["title"], "Write the spec", "{task}");
    assert_eq!(task["status"], "doing", "{task}");
    assert_eq!(task["rank"], 3, "{task}");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_create_takes_the_id_it_is_given_and_returns_ids_only_by_default() {
    let world = World::new().await;
    let done = world
        .mutate_ok(vec![create_op_at(
            "ad4m://task/given",
            json!({ "title": "t" }),
        )])
        .await;
    assert_eq!(done, json!({ "ids": ["ad4m://task/given"] }));
    assert_eq!(world.task("ad4m://task/given").await.unwrap()["title"], "t");
}

#[tokio::test(flavor = "multi_thread")]
async fn an_empty_list_of_ops_writes_nothing() {
    let world = World::new().await;
    let before = world.link_count().await;
    let done = world.mutate_ok(vec![]).await;
    assert_eq!(done, json!({ "ids": [] }));
    assert_eq!(world.link_count().await, before);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_create_with_a_relation_parent_adds_the_child_to_the_parents_collection() {
    let world = World::new().await;
    let parent = world.create(json!({ "title": "parent" })).await;

    let done = world
        .mutate_ok(vec![json!({
            "op": "create", "class": "Task", "values": { "title": "child" },
            "parent": { "id": parent, "relation": "labels", "model": "Task" },
        })])
        .await;
    let child = done["ids"][0].as_str().unwrap();
    assert_eq!(world.task(&parent).await.unwrap()["labels"], json!([child]));
    assert_eq!(world.task(child).await.unwrap()["title"], "child");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_create_with_a_predicate_parent_writes_one_link_from_the_parent() {
    let world = World::new().await;
    let parent = world.create(json!({ "title": "parent" })).await;

    let done = world
        .mutate_ok(vec![json!({
            "op": "create", "class": "Task", "values": { "title": "child" },
            "parent": { "id": parent, "predicate": "ns://has_task" },
        })])
        .await;
    let child = done["ids"][0].as_str().unwrap();
    assert_eq!(
        world
            .links_between(&parent, Some("ns://has_task"), child)
            .await,
        1
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_create_with_a_contained_parent_links_it_under_the_parent() {
    let world = World::new().await;
    let parent = world.create(json!({ "title": "parent" })).await;

    let done = world
        .mutate_ok(vec![json!({
            "op": "create", "class": "Task", "values": { "title": "child" },
            "parent": { "id": parent },
        })])
        .await;
    let child = done["ids"][0].as_str().unwrap();
    assert_eq!(world.links_between(&parent, None, child).await, 1);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_create_under_a_parent_that_does_not_exist_is_refused_and_writes_nothing() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .mutate(vec![json!({
            "op": "create", "class": "Task", "values": { "title": "child" },
            "parent": { "id": "ad4m://obj/missing", "relation": "labels", "model": "Task" },
        })])
        .await
        .expect_err("the parent is not there");
    assert_eq!(error.code, 404, "{}", error.message);
    assert_eq!(error.data.as_ref().unwrap()["opIndex"], 0);
    assert_eq!(world.link_count().await, before, "the child is not kept");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_later_op_updates_an_instance_an_earlier_op_created() {
    let world = World::new().await;
    let id = "ad4m://task/in-call";
    let done = world
        .mutate_ok(vec![
            create_op_at(id, json!({ "title": "first", "rank": 1 })),
            update_op(id, json!({ "set": { "title": "second" } })),
        ])
        .await;
    assert_eq!(done["ids"], json!([id, id]));

    let task = world.task(id).await.unwrap();
    assert_eq!(task["title"], "second", "{task}");
    assert_eq!(task["rank"], 1, "{task}");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_later_op_adds_to_a_collection_of_an_instance_an_earlier_op_created() {
    let world = World::new().await;
    let id = "ad4m://task/with-labels";
    world
        .mutate_ok(vec![
            create_op_at(id, json!({ "title": "t" })),
            update_op(id, json!({ "add": { "labels": [ITEM] } })),
        ])
        .await;
    assert_eq!(world.task(id).await.unwrap()["labels"], json!([ITEM]));
}

#[tokio::test(flavor = "multi_thread")]
async fn a_later_op_removes_an_instance_an_earlier_op_created() {
    let world = World::new().await;
    let before = world.link_count().await;
    let id = "ad4m://task/short-lived";
    world
        .mutate_ok(vec![
            create_op_at(id, json!({ "title": "t" })),
            update_op(id, json!({ "add": { "labels": [ITEM] } })),
            delete_op(id),
        ])
        .await;
    assert!(world.task(id).await.is_none());
    assert_eq!(world.link_count().await, before, "no link is left");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_later_op_does_not_find_an_instance_an_earlier_op_removed() {
    let world = World::new().await;
    let id = world.create(json!({ "title": "t" })).await;
    let before = world.link_count().await;

    let error = world
        .mutate(vec![
            delete_op(&id),
            update_op(&id, json!({ "set": { "title": "again" } })),
        ])
        .await
        .expect_err("the instance is gone for the second op");
    assert_eq!(error.code, 404, "{}", error.message);
    assert_eq!(error.data.as_ref().unwrap()["kind"], "notFound");
    assert_eq!(error.data.as_ref().unwrap()["opIndex"], 1);
    assert_eq!(world.link_count().await, before, "the delete is not kept");
    assert!(world.task(&id).await.is_some());
}

#[tokio::test(flavor = "multi_thread")]
async fn a_second_create_of_an_id_in_one_call_is_refused() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .mutate(vec![
            create_op_at("ad4m://task/dup", json!({ "title": "a" })),
            create_op_at("ad4m://task/dup", json!({ "title": "b" })),
        ])
        .await
        .expect_err("the id is taken by the first op");
    assert_eq!(error.code, 409, "{}", error.message);
    assert_eq!(error.data.as_ref().unwrap()["opIndex"], 1);
    assert_eq!(world.link_count().await, before);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_delete_removes_the_instance_and_every_link_of_it() {
    let world = World::new().await;
    let before = world.link_count().await;
    let id = world.create(json!({ "title": "Gone soon" })).await;
    world
        .mutate_ok(vec![update_op(&id, json!({ "add": { "labels": [ITEM] } }))])
        .await;
    // A link that points at the instance goes too.
    world
        .ok(
            "perspective.addLink",
            json!({ "link": { "source": "ad4m://other", "predicate": "ns://refers", "target": id } }),
        )
        .await;
    assert!(world.link_count().await > before);

    let done = world.mutate_ok(vec![delete_op(&id)]).await;
    assert_eq!(done["ids"], json!([id]));
    assert!(world.task(&id).await.is_none(), "the task is gone");
    assert_eq!(world.links_touching(&id).await, 0);
    assert_eq!(
        world.link_count().await,
        before,
        "every link of the task is gone"
    );

    // Deleting it again: nothing there.
    let error = world
        .mutate(vec![delete_op(&id)])
        .await
        .expect_err("a second delete finds nothing");
    assert_eq!(error.code, 404, "{}", error.message);
    assert_eq!(error.data.as_ref().unwrap()["kind"], "notFound");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_refused_middle_op_stages_nothing_and_names_its_index() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .mutate(vec![
            create_op_at("ad4m://task/a", json!({ "title": "a" })),
            create_op_at("ad4m://task/b", json!({ "title": "b", "rank": "high" })),
            create_op_at("ad4m://task/c", json!({ "title": "c" })),
        ])
        .await
        .expect_err("the second op breaks the shape");
    let errors = validation_errors(&error);
    assert_eq!(error_for(&errors, "rank")["problem"]["kind"], "wrongType");
    let data = error.data.as_ref().unwrap();
    assert_eq!(data["opIndex"], 1, "{data}");
    assert!(error.message.starts_with("op 1: "), "{}", error.message);
    assert_eq!(world.link_count().await, before, "not even op 0 is kept");
    assert!(world.task("ad4m://task/a").await.is_none());
    assert!(world.task("ad4m://task/c").await.is_none());
}

#[tokio::test(flavor = "multi_thread")]
async fn a_refused_op_leaves_the_committed_instances_as_they_were() {
    let world = World::new().await;
    let id = world
        .create(json!({ "title": "Keep", "status": "todo" }))
        .await;
    let before = world.link_count().await;
    let error = world
        .mutate(vec![
            update_op(&id, json!({ "set": { "title": "Changed" } })),
            update_op(&id, json!({ "set": { "status": "done" } })),
        ])
        .await
        .expect_err("status is outside sh:in");
    assert_eq!(
        error_for(&validation_errors(&error), "status")["problem"]["kind"],
        "notAllowed"
    );
    assert_eq!(error.data.as_ref().unwrap()["opIndex"], 1);
    assert_eq!(world.link_count().await, before);
    let task = world.task(&id).await.unwrap();
    assert_eq!(task["title"], "Keep", "{task}");
    assert_eq!(task["status"], "todo", "{task}");
}

#[tokio::test(flavor = "multi_thread")]
async fn an_unknown_property_is_refused_by_name_and_writes_nothing() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .mutate(vec![create_op(json!({ "title": "t", "colour": "red" }))])
        .await
        .expect_err("an unknown property is refused");
    let errors = validation_errors(&error);
    let colour = error_for(&errors, "colour");
    assert_eq!(colour["problem"]["kind"], "unknownProperty", "{colour}");
    assert!(
        colour["problem"]["available"]
            .as_array()
            .unwrap()
            .contains(&json!("title")),
        "{colour}"
    );
    assert!(error.message.contains("colour"), "{}", error.message);
    assert_eq!(
        world.link_count().await,
        before,
        "a refused write writes no link"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_value_of_the_wrong_type_is_refused() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .mutate(vec![create_op(json!({ "title": "t", "rank": "high" }))])
        .await
        .expect_err("a non-number rank is refused");
    let errors = validation_errors(&error);
    let rank = error_for(&errors, "rank");
    assert_eq!(rank["problem"]["kind"], "wrongType", "{rank}");
    assert_eq!(rank["expectedType"], "integer", "{rank}");
    assert_eq!(rank["cardinality"], json!({ "min": 0, "max": 1 }), "{rank}");
    assert_eq!(rank["received"], "high", "{rank}");
    assert_eq!(world.link_count().await, before);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_value_outside_sh_in_is_refused_with_the_allowed_values() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .mutate(vec![create_op(json!({ "title": "t", "status": "done" }))])
        .await
        .expect_err("a status outside sh:in is refused");
    let errors = validation_errors(&error);
    let status = error_for(&errors, "status");
    assert_eq!(status["problem"]["kind"], "notAllowed", "{status}");
    assert_eq!(
        status["allowedValues"],
        json!(["todo", "doing"]),
        "{status}"
    );
    assert_eq!(world.link_count().await, before);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_create_without_a_required_property_is_refused() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .mutate(vec![create_op(json!({ "status": "todo" }))])
        .await
        .expect_err("a create without title is refused");
    let errors = validation_errors(&error);
    let title = error_for(&errors, "title");
    assert_eq!(title["problem"]["kind"], "missingRequired", "{title}");
    assert_eq!(
        title["cardinality"],
        json!({ "min": 1, "max": 1 }),
        "{title}"
    );
    assert_eq!(world.link_count().await, before);
}

#[tokio::test(flavor = "multi_thread")]
async fn an_update_of_one_property_leaves_the_others() {
    let world = World::new().await;
    let id = world
        .create(json!({ "title": "Keep me", "status": "todo", "rank": 1 }))
        .await;

    world
        .mutate_ok(vec![update_op(
            &id,
            json!({ "set": { "status": "doing" } }),
        )])
        .await;

    let task = world.task(&id).await.unwrap();
    assert_eq!(task["status"], "doing", "{task}");
    assert_eq!(task["title"], "Keep me", "{task}");
    assert_eq!(task["rank"], 1, "{task}");
}

#[tokio::test(flavor = "multi_thread")]
async fn an_update_with_nothing_to_write_is_refused() {
    let world = World::new().await;
    let id = world.create(json!({ "title": "t" })).await;
    let error = world
        .mutate(vec![update_op(&id, json!({}))])
        .await
        .expect_err("nothing to write");
    assert_eq!(error.code, 400, "{}", error.message);
    assert_eq!(error.data.as_ref().unwrap()["kind"], "emptyProperties");
    assert_eq!(error.data.as_ref().unwrap()["opIndex"], 0);
}

#[tokio::test(flavor = "multi_thread")]
async fn collection_items_are_added_and_removed() {
    let world = World::new().await;
    let id = world.create(json!({ "title": "Labelled" })).await;

    world
        .mutate_ok(vec![update_op(&id, json!({ "add": { "labels": [ITEM] } }))])
        .await;
    assert_eq!(world.task(&id).await.unwrap()["labels"], json!([ITEM]));

    // Adding a member again is a no-op.
    let before = world.link_count().await;
    world
        .mutate_ok(vec![update_op(&id, json!({ "add": { "labels": [ITEM] } }))])
        .await;
    assert_eq!(world.link_count().await, before);

    world
        .mutate_ok(vec![update_op(
            &id,
            json!({ "remove": { "labels": [ITEM] } }),
        )])
        .await;
    let labels = world.task(&id).await.unwrap()["labels"].clone();
    assert!(
        labels.is_null() || labels == json!([]),
        "no label left: {labels}"
    );

    // A collection name the class does not have is refused by name.
    let before = world.link_count().await;
    let error = world
        .mutate(vec![update_op(&id, json!({ "add": { "tags": [ITEM] } }))])
        .await
        .expect_err("an unknown collection is refused");
    let tags = error_for(&validation_errors(&error), "tags").clone();
    assert_eq!(tags["problem"]["kind"], "unknownCollection", "{tags}");
    assert_eq!(world.link_count().await, before);
}

/// Two registered classes share the name `Task`: the bare name is refused
/// `ambiguousClass` naming both IRIs (400), and each IRI writes.
#[tokio::test(flavor = "multi_thread")]
async fn a_shared_class_name_is_refused_and_its_iri_resolves() {
    let registered = registered_perspective(&[("Task", TASK_SHACL)]).await;
    let mut perspective =
        crate::perspectives::get_perspective(&registered.0).expect("registered perspective");
    perspective
        .register_shape_json(OTHER_TASK_SHACL, &crate::agent::AgentContext::main_agent())
        .await
        .expect("register the second Task");
    let world = World {
        handlers: build_handler_map(),
        uuid: registered.0.clone(),
        _registered: registered,
    };

    let error = world
        .mutate(vec![create_op(json!({ "title": "t" }))])
        .await
        .expect_err("a shared class name is refused");
    assert_eq!(error.code, 400, "{}", error.message);
    let data = error.data.as_ref().expect("the error carries data");
    assert_eq!(data["kind"], "ambiguousClass", "{data}");
    assert_eq!(
        data["candidates"],
        json!(["ns://Task", "other://Task"]),
        "{data}"
    );

    // Each class key resolves: the shared name's IRIs write.
    let created = world
        .mutate_ok(vec![json!({
            "op": "create", "class": "other://Task", "values": { "title": "t" },
        })])
        .await;
    assert!(created["ids"][0].as_str().is_some(), "{created}");
}

#[tokio::test(flavor = "multi_thread")]
async fn writes_in_one_batch_are_invisible_until_the_commit() {
    let world = World::new().await;
    let batch = world.create_batch().await;
    let before = world.link_count().await;

    world
        .mutate_in(
            &batch,
            vec![
                create_op_at("ad4m://task/one", json!({ "title": "one" })),
                create_op_at("ad4m://task/two", json!({ "title": "two" })),
            ],
        )
        .await
        .expect("the ops stage");

    assert_eq!(
        world.link_count().await,
        before,
        "nothing lands before the commit"
    );
    assert!(world.task("ad4m://task/one").await.is_none());
    assert!(world.task("ad4m://task/two").await.is_none());

    world.commit(&batch).await;
    assert_eq!(world.task("ad4m://task/one").await.unwrap()["title"], "one");
    assert_eq!(world.task("ad4m://task/two").await.unwrap()["title"], "two");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_failed_call_into_a_batch_puts_the_batch_back() {
    let world = World::new().await;
    let batch = world.create_batch().await;
    world
        .mutate_in(
            &batch,
            vec![create_op_at("ad4m://task/kept", json!({ "title": "kept" }))],
        )
        .await
        .expect("the first call stages");

    let error = world
        .mutate_in(
            &batch,
            vec![
                create_op_at("ad4m://task/lost", json!({ "title": "lost" })),
                create_op_at("ad4m://task/bad", json!({ "title": "bad", "rank": "high" })),
            ],
        )
        .await
        .expect_err("the second call breaks the shape");
    assert_eq!(error.data.as_ref().unwrap()["opIndex"], 1);

    world.commit(&batch).await;
    assert_eq!(
        world.task("ad4m://task/kept").await.unwrap()["title"],
        "kept",
        "what an earlier call staged stays"
    );
    assert!(
        world.task("ad4m://task/lost").await.is_none(),
        "what the failed call staged is dropped"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_call_sees_what_an_earlier_call_staged_in_the_batch_through_its_links() {
    let world = World::new().await;
    let id = world.create(json!({ "title": "t" })).await;
    let batch = world.create_batch().await;

    // Remove then add again across two calls into one batch: the second sees the removal.
    world
        .mutate_ok(vec![update_op(&id, json!({ "add": { "labels": [ITEM] } }))])
        .await;
    world
        .mutate_in(
            &batch,
            vec![update_op(&id, json!({ "remove": { "labels": [ITEM] } }))],
        )
        .await
        .expect("remove stages");
    world
        .mutate_in(
            &batch,
            vec![update_op(&id, json!({ "add": { "labels": [ITEM] } }))],
        )
        .await
        .expect("add stages");
    world.commit(&batch).await;
    assert_eq!(world.task(&id).await.unwrap()["labels"], json!([ITEM]));
}

#[tokio::test(flavor = "multi_thread")]
async fn an_update_then_a_delete_in_one_call_leaves_no_links() {
    let world = World::new().await;
    let id = world
        .create(json!({ "title": "t", "status": "todo" }))
        .await;
    assert!(world.links_touching(&id).await > 0);

    world
        .mutate_ok(vec![
            update_op(&id, json!({ "set": { "status": "doing" } })),
            delete_op(&id),
        ])
        .await;

    assert!(world.task(&id).await.is_none(), "the task is gone");
    assert_eq!(
        world.links_touching(&id).await,
        0,
        "a link the update staged must go with the instance"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_restaged_committed_link_still_goes_with_the_instance() {
    // `perspective.addLinkExpression` can stage the exact signed expression the store
    // already holds. The batch then carries a copy of a committed link; deleting the
    // instance must still stage the committed one's removal, not just drop the copy.
    let world = World::new().await;
    let id = world.create(json!({ "title": "t" })).await;
    assert!(world.links_touching(&id).await > 0);

    // The instance's type link, signed, as queryLinks reports it.
    let links = world
        .ok("perspective.queryLinks", json!({ "source": id }))
        .await;
    let type_link = links
        .as_array()
        .expect("links")
        .iter()
        .find(|l| l["data"]["predicate"] == "rdf://type")
        .expect("the instance's type link")
        .clone();

    let batch = world.create_batch().await;
    world
        .ok(
            "perspective.addLinkExpression",
            json!({ "link": type_link, "batchId": batch }),
        )
        .await;
    world
        .mutate_in(&batch, vec![delete_op(&id)])
        .await
        .expect("the delete stages");
    world.commit(&batch).await;

    assert_eq!(
        world.links_touching(&id).await,
        0,
        "a staged copy of a committed link must not leave the committed link behind"
    );
    assert!(world.task(&id).await.is_none(), "the task is gone");
}

#[tokio::test(flavor = "multi_thread")]
async fn two_staged_copies_of_one_membership_link_remove_once() {
    // The same signed membership link staged twice (through `perspective.addLink`
    // for the signature, then `perspective.addLinkExpression` for the duplicate) is
    // one link on the batch's effective graph: removing the item cancels it once and
    // must not fall through to a store removal that finds nothing.
    let world = World::new().await;
    let id = world.create(json!({ "title": "t" })).await;

    let batch = world.create_batch().await;
    let signed = world
        .ok(
            "perspective.addLink",
            json!({ "link": { "source": id, "predicate": "ns://task_label", "target": ITEM }, "batchId": batch }),
        )
        .await;
    world
        .ok(
            "perspective.addLinkExpression",
            json!({ "link": signed, "batchId": batch }),
        )
        .await;

    world
        .mutate_in(
            &batch,
            vec![update_op(&id, json!({ "remove": { "labels": [ITEM] } }))],
        )
        .await
        .expect("the removal stages");

    world.commit(&batch).await;
    let labels = world.task(&id).await.unwrap()["labels"].clone();
    assert!(
        labels.is_null() || labels == json!([]),
        "the item is out: {labels}"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_batch_that_is_not_open_is_refused_before_writing() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .mutate_in("no-such-batch", vec![create_op(json!({ "title": "t" }))])
        .await
        .expect_err("an unknown batch is refused");
    assert_eq!(error.code, 404, "{}", error.message);
    let data = error.data.as_ref().unwrap();
    assert_eq!(data["kind"], "batchNotFound");
    assert!(data.get("opIndex").is_none(), "the call failed, not an op");
    assert_eq!(world.link_count().await, before);
}

/// A projection applies to an update that survives the call too: its row carries the asked
/// properties and not the stored `rank` it left alone.
#[tokio::test(flavor = "multi_thread")]
async fn returns_rows_projects_the_row_of_a_surviving_update() {
    let world = World::new().await;
    let existing = world.create(json!({ "title": "old", "rank": 2 })).await;
    let done = world
        .ok(
            "perspective.mutate",
            json!({
                "ops": [update_op(&existing, json!({ "set": { "title": "newer" } }))],
                "returns": { "kind": "rows", "properties": ["title"] },
            }),
        )
        .await;
    let rows = done["rows"].as_array().expect("rows");
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0]["title"], "newer", "{rows:?}");
    assert_eq!(rows[0]["id"], existing, "{rows:?}");
    assert!(
        rows[0].get("rank").is_none(),
        "rank was not asked for: {rows:?}"
    );
    // The stored rank is untouched.
    let full = world
        .ok(
            "perspective.mutate",
            json!({
                "ops": [update_op(&existing, json!({ "set": { "title": "newer" } }))],
                "returns": { "kind": "rows" },
            }),
        )
        .await;
    assert_eq!(full["rows"][0]["rank"], 2, "{full}");
}

#[tokio::test(flavor = "multi_thread")]
async fn returns_rows_reads_back_each_created_and_updated_instance() {
    let world = World::new().await;
    let existing = world.create(json!({ "title": "old", "rank": 2 })).await;
    let done = world
        .ok(
            "perspective.mutate",
            json!({
                "ops": [
                    create_op_at("ad4m://task/row", json!({ "title": "new" })),
                    update_op(&existing, json!({ "set": { "title": "newer" } })),
                    delete_op(&existing),
                ],
                "returns": { "kind": "rows", "properties": ["title"] },
            }),
        )
        .await;
    assert_eq!(done["ids"], json!(["ad4m://task/row", existing, existing]));
    let rows = done["rows"].as_array().expect("rows");
    assert_eq!(rows.len(), 3);
    assert_eq!(rows[0]["title"], "new", "{rows:?}");
    assert_eq!(rows[0]["id"], "ad4m://task/row", "{rows:?}");
    assert!(rows[0].get("rank").is_none(), "only the asked properties");
    assert!(rows[1].is_null(), "deleted later in the call: {rows:?}");
    assert!(rows[2].is_null(), "a delete has no row: {rows:?}");
}

/// An update that survives the call returns its instance as a read shows it, with the values it
/// wrote and the ones it left; an update of one instance twice returns it twice.
#[tokio::test(flavor = "multi_thread")]
async fn returns_rows_gives_a_surviving_update_its_row() {
    let world = World::new().await;
    let existing = world.create(json!({ "title": "old", "rank": 2 })).await;
    let done = world
        .ok(
            "perspective.mutate",
            json!({
                "ops": [
                    update_op(&existing, json!({ "set": { "title": "newer" } })),
                    update_op(&existing, json!({ "set": { "rank": 5 } })),
                ],
                "returns": { "kind": "rows" },
            }),
        )
        .await;
    assert_eq!(done["ids"], json!([existing, existing]));
    let rows = done["rows"].as_array().expect("rows");
    assert_eq!(rows.len(), 2);
    for row in rows {
        assert_eq!(row["id"], json!(existing), "{rows:?}");
        assert_eq!(row["title"], "newer", "{rows:?}");
        assert_eq!(row["rank"], 5, "the row is read after the commit: {rows:?}");
    }
}

/// `returns: rows` reads instance data, so the caller needs `perspective:READ` as well as
/// UPDATE; without it the call is refused before anything is written. The same call without
/// `returns` stays an update.
#[tokio::test(flavor = "multi_thread")]
async fn returns_rows_needs_the_read_capability() {
    let world = World::new().await;
    let before = world.link_count().await;
    let writer = Arc::new(RequestContext::app_for_test(vec![
        perspective_update_capability(vec!["*".to_string()]),
    ]));
    let call = |returns: Value| {
        let mut params = json!({
            "uuid": world.uuid,
            "ops": [create_op_at("ad4m://task/rows", json!({ "title": "t" }))],
        });
        if !returns.is_null() {
            params["returns"] = returns;
        }
        world
            .handlers
            .dispatch("perspective.mutate", params, writer.clone())
    };

    let refused = call(json!({ "kind": "rows" })).await;
    expect_code(refused, 403, "perspective.mutate with rows");
    assert_eq!(world.link_count().await, before, "the refused call wrote");

    call(Value::Null)
        .await
        .unwrap_or_else(|e| panic!("a write without rows needs no read: {}", e.message));
    assert!(world.task("ad4m://task/rows").await.is_some());
}

#[tokio::test(flavor = "multi_thread")]
async fn returns_rows_with_a_batch_is_refused() {
    let world = World::new().await;
    // What one staged Task adds, measured by writing a reference outside any batch.
    let start = world.link_count().await;
    world
        .mutate_ok(vec![create_op_at(
            "ad4m://task/ref",
            json!({ "title": "ref" }),
        )])
        .await;
    let one_task = world.link_count().await - start;
    let batch = world.create_batch().await;
    world
        .mutate_in(
            &batch,
            vec![create_op_at(
                "ad4m://task/staged",
                json!({ "title": "staged" }),
            )],
        )
        .await
        .expect("the first call stages");
    let error = world
        .call(
            "perspective.mutate",
            json!({
                "ops": [create_op(json!({ "title": "t" }))],
                "batch": batch,
                "returns": { "kind": "rows" },
            }),
        )
        .await
        .expect_err("a caller's batch is not committed yet");
    assert_eq!(error.code, 400, "{}", error.message);

    // The refused call touched nothing: the batch holds what the first call staged, and the
    // refused call's op is not in it.
    world.commit(&batch).await;
    assert_eq!(
        world.task("ad4m://task/staged").await.unwrap()["title"],
        "staged"
    );
    assert_eq!(
        world.link_count().await,
        start + 2 * one_task,
        "the refused call left an op in the batch"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn unknown_fields_are_refused() {
    let world = World::new().await;
    let before = world.link_count().await;
    let task = create_op(json!({ "title": "t" }));
    let mut with_extra_op_field = task.clone();
    with_extra_op_field["colour"] = json!("red");
    let calls = [
        // The call.
        json!({ "ops": [task.clone()], "colour": "red" }),
        // An op.
        json!({ "ops": [with_extra_op_field] }),
        // An op kind that does not exist.
        json!({ "ops": [{ "op": "upsert", "class": "Task", "id": "ad4m://x" }] }),
        // The `returns` field.
        json!({ "ops": [task.clone()], "returns": { "kind": "ids", "colour": "red" } }),
        // A parent that is none of the three shapes.
        json!({ "ops": [{ "op": "create", "class": "Task", "values": { "title": "t" },
                          "parent": { "id": "ad4m://obj/p", "colour": "red" } }] }),
        // A parent that mixes two shapes.
        json!({ "ops": [{ "op": "create", "class": "Task", "values": { "title": "t" },
                          "parent": { "id": "ad4m://obj/p", "predicate": "ns://p",
                                      "relation": "labels", "model": "Task" } }] }),
    ];
    for params in calls {
        let error = world
            .call("perspective.mutate", params.clone())
            .await
            .expect_err(&format!("refused: {params}"));
        assert_eq!(error.code, 400, "{params}: {}", error.message);
    }
    assert_eq!(world.link_count().await, before);
}

/// The method, with params that would write if the guard let them through.
fn write_call(id: &str) -> (&'static str, Value) {
    (
        "perspective.mutate",
        json!({ "ops": [
            create_op(json!({ "title": "intruder" })),
            update_op(id, json!({ "set": { "title": "intruder" } })),
            delete_op(id),
        ] }),
    )
}

#[tokio::test(flavor = "multi_thread")]
async fn a_caller_without_the_update_capability_is_refused() {
    let world = World::new().await;
    let id = world.create(json!({ "title": "Mine" })).await;
    let before = world.link_count().await;

    // An app on the main agent that may read every perspective, but not write.
    let reader = Arc::new(RequestContext::app_for_test(vec![
        perspective_query_capability(vec!["*".to_string()]),
    ]));
    let (method, mut params) = write_call(&id);
    params["uuid"] = json!(world.uuid);
    let result = world.handlers.dispatch(method, params, reader).await;
    expect_code(result, 403, method);
    assert_eq!(world.link_count().await, before, "the refused call wrote");
    assert_eq!(world.task(&id).await.unwrap()["title"], "Mine");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_user_cannot_write_instances_into_another_users_perspective() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    let bob = executor.user("bob");
    let perspective = RegisteredPerspective::private(Some(vec![alice.did.clone()]));
    let uuid = perspective.uuid.clone();
    let with_uuid = |mut params: Value| {
        params["uuid"] = json!(uuid);
        params
    };

    executor
        .call(
            "perspective.registerShapes",
            with_uuid(json!({
                "shapes": [serde_json::from_str::<Value>(TASK_SHACL).unwrap()],
            })),
            &alice.session,
        )
        .await
        .unwrap_or_else(|e| panic!("alice adds the class: {}", e.message));
    let created = executor
        .call(
            "perspective.mutate",
            with_uuid(json!({ "ops": [create_op(json!({ "title": "Alice's" }))] })),
            &alice.session,
        )
        .await
        .unwrap_or_else(|e| panic!("alice writes into her own perspective: {}", e.message));
    let id = created["ids"][0].as_str().unwrap().to_string();

    let links = || async {
        crate::perspectives::get_perspective(&uuid)
            .unwrap()
            .get_links(&LinkQuery::default())
            .await
            .unwrap()
            .len()
    };
    let before = links().await;

    // Bob and an app on the main agent holding every capability: not owners, so the
    // perspective is missing to them (404, as for one that does not exist).
    for session in [&bob.session, &executor.main_agent_app()] {
        let (method, params) = write_call(&id);
        let result = executor.call(method, with_uuid(params), session).await;
        expect_code(result, 404, method);
    }
    assert_eq!(links().await, before, "no refused call wrote");
}

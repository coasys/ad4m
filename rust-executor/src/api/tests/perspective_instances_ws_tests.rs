//! `perspective.instanceCreate` / `.instanceUpdate` / `.instanceRemove` /
//! `.instanceAddToCollection` / `.instanceRemoveFromCollection` through the real handler map.
//!
//! Written from the method contract: a valid write lands and reads back, a write that breaks the
//! class's shape is refused with the errors as data (422) and writes no link, `batchId` keeps the
//! writes invisible until the commit, and the guard is `perspective.createSubject`'s.

use serde_json::{json, Value};

use super::multi_user::{expect_code, Executor, RegisteredPerspective};
use super::support::{admin_ctx, registered_perspective};
use crate::agent::capabilities::perspective_query_capability;
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

struct World {
    handlers: HandlerMap,
    uuid: String,
    _registered: super::support::Registered,
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

    async fn create(&self, properties: Value) -> Value {
        self.ok(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": properties }),
        )
        .await
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
    let created = world
        .create(json!({ "title": "Write the spec", "status": "doing", "rank": "3" }))
        .await;
    assert_eq!(created["className"], "Task");
    let id = created["baseUri"].as_str().expect("baseUri");
    assert!(id.starts_with("ad4m://obj/"), "{id}");
    let mut set: Vec<&str> = created["propertiesSet"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_str().unwrap())
        .collect();
    set.sort();
    assert_eq!(set, ["rank", "status", "title"]);

    let task = world.task(id).await.expect("the task reads back");
    assert_eq!(task["title"], "Write the spec", "{task}");
    assert_eq!(task["status"], "doing", "{task}");
    assert_eq!(task["rank"], 3, "{task}");
}

#[tokio::test(flavor = "multi_thread")]
async fn an_unknown_property_is_refused_by_name_and_writes_nothing() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .call(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "title": "t", "colour": "red" } }),
        )
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
        .call(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "title": "t", "rank": "high" } }),
        )
        .await
        .expect_err("a non-number rank is refused");
    let errors = validation_errors(&error);
    let rank = error_for(&errors, "rank");
    assert_eq!(rank["problem"]["kind"], "wrongType", "{rank}");
    assert_eq!(rank["expectedType"], "integer", "{rank}");
    assert_eq!(rank["cardinality"], json!({ "min": 0, "max": 1 }), "{rank}");
    assert_eq!(rank["received"], "high", "{rank}");
    assert_eq!(
        world.link_count().await,
        before,
        "a refused write writes no link"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_value_outside_sh_in_is_refused_with_the_allowed_values() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .call(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "title": "t", "status": "done" } }),
        )
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
    assert_eq!(
        world.link_count().await,
        before,
        "a refused write writes no link"
    );

    // The same rule on update: the stored value stays.
    let created = world
        .create(json!({ "title": "t", "status": "todo" }))
        .await;
    let id = created["baseUri"].as_str().unwrap();
    let before = world.link_count().await;
    let error = world
        .call(
            "perspective.instanceUpdate",
            json!({ "className": "Task", "baseUri": id, "properties": { "status": "done" } }),
        )
        .await
        .expect_err("an update outside sh:in is refused");
    assert_eq!(
        error_for(&validation_errors(&error), "status")["problem"]["kind"],
        "notAllowed"
    );
    assert_eq!(
        world.link_count().await,
        before,
        "a refused update writes no link"
    );
    assert_eq!(world.task(id).await.unwrap()["status"], "todo");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_create_without_a_required_property_is_refused() {
    let world = World::new().await;
    let before = world.link_count().await;
    let error = world
        .call(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "status": "todo" } }),
        )
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
    assert_eq!(
        world.link_count().await,
        before,
        "a refused write writes no link"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn an_update_of_one_property_leaves_the_others() {
    let world = World::new().await;
    let created = world
        .create(json!({ "title": "Keep me", "status": "todo", "rank": 1 }))
        .await;
    let id = created["baseUri"].as_str().unwrap();

    let updated = world
        .ok(
            "perspective.instanceUpdate",
            json!({ "className": "Task", "baseUri": id, "properties": { "status": "doing" } }),
        )
        .await;
    assert_eq!(updated["updatedProperties"], json!(["status"]), "{updated}");

    let task = world.task(id).await.unwrap();
    assert_eq!(task["status"], "doing", "{task}");
    assert_eq!(task["title"], "Keep me", "{task}");
    assert_eq!(task["rank"], 1, "{task}");
}

#[tokio::test(flavor = "multi_thread")]
async fn remove_deletes_the_instance_and_its_links() {
    let world = World::new().await;
    let before = world.link_count().await;
    let created = world.create(json!({ "title": "Gone soon" })).await;
    let id = created["baseUri"].as_str().unwrap().to_string();
    assert!(world.link_count().await > before);

    let removed = world
        .ok(
            "perspective.instanceRemove",
            json!({ "className": "Task", "baseUri": id }),
        )
        .await;
    assert!(removed["linksRemoved"].as_u64().unwrap() > 0, "{removed}");
    assert!(world.task(&id).await.is_none(), "the task is gone");
    assert_eq!(
        world.link_count().await,
        before,
        "every link of the task is gone"
    );

    // Removing it again: nothing there.
    let error = world
        .call(
            "perspective.instanceRemove",
            json!({ "className": "Task", "baseUri": id }),
        )
        .await
        .expect_err("a second remove finds nothing");
    assert_eq!(error.code, 404, "{}", error.message);
    assert_eq!(error.data.as_ref().unwrap()["kind"], "notFound");
}

#[tokio::test(flavor = "multi_thread")]
async fn collection_items_are_added_and_removed() {
    let world = World::new().await;
    let created = world.create(json!({ "title": "Labelled" })).await;
    let id = created["baseUri"].as_str().unwrap();
    let item = "ad4m://label/urgent";

    let added = world
        .ok(
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item }),
        )
        .await;
    assert_eq!(added["linksAdded"], 1, "{added}");
    assert_eq!(added["alreadyMember"], false, "{added}");
    assert_eq!(world.task(id).await.unwrap()["labels"], json!([item]));

    let again = world
        .ok(
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item }),
        )
        .await;
    assert_eq!(
        again["linksAdded"], 0,
        "adding a member again is a no-op: {again}"
    );

    let removed = world
        .ok(
            "perspective.instanceRemoveFromCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item }),
        )
        .await;
    assert_eq!(removed["linksRemoved"], 1, "{removed}");
    let labels = world.task(id).await.unwrap()["labels"].clone();
    assert!(
        labels.is_null() || labels == json!([]),
        "no label left: {labels}"
    );

    // A collection name the class does not have is refused by name.
    let before = world.link_count().await;
    let error = world
        .call(
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "tags", "itemUri": item }),
        )
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
        .call(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "title": "t" } }),
        )
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
        .ok(
            "perspective.instanceCreate",
            json!({ "className": "other://Task", "properties": { "title": "t" } }),
        )
        .await;
    assert!(created["baseUri"].as_str().is_some(), "{created}");
}

#[tokio::test(flavor = "multi_thread")]
async fn writes_in_one_batch_are_invisible_until_the_commit() {
    let world = World::new().await;
    let batch_id = world.ok("perspective.createBatch", json!({})).await;
    let batch_id = batch_id.as_str().expect("batch id").to_string();
    let before = world.link_count().await;

    let first = world
        .ok(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "title": "one" }, "batchId": batch_id }),
        )
        .await;
    let second = world
        .ok(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "title": "two" }, "batchId": batch_id }),
        )
        .await;
    let (first, second) = (
        first["baseUri"].as_str().unwrap().to_string(),
        second["baseUri"].as_str().unwrap().to_string(),
    );

    assert_eq!(
        world.link_count().await,
        before,
        "nothing lands before the commit"
    );
    assert!(
        world.task(&first).await.is_none(),
        "first is not visible yet"
    );
    assert!(
        world.task(&second).await.is_none(),
        "second is not visible yet"
    );

    world
        .ok("perspective.commitBatch", json!({ "batchId": batch_id }))
        .await;
    assert_eq!(world.task(&first).await.unwrap()["title"], "one");
    assert_eq!(world.task(&second).await.unwrap()["title"], "two");
}

#[tokio::test(flavor = "multi_thread")]
async fn an_add_then_a_remove_in_one_batch_leaves_the_item_out() {
    let world = World::new().await;
    let created = world.create(json!({ "title": "t" })).await;
    let id = created["baseUri"].as_str().unwrap().to_string();
    let item = "ad4m://label/urgent";
    let batch_id = world.ok("perspective.createBatch", json!({})).await;
    let batch_id = batch_id.as_str().expect("batch id").to_string();

    world
        .ok(
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    let removed = world
        .ok(
            "perspective.instanceRemoveFromCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    assert_eq!(removed["linksRemoved"], 1, "{removed}");

    world
        .ok("perspective.commitBatch", json!({ "batchId": batch_id }))
        .await;
    let labels = world.task(&id).await.unwrap()["labels"].clone();
    assert!(
        labels.is_null() || labels == json!([]),
        "an add the same batch removed must not land: {labels}"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_remove_then_an_add_in_one_batch_keeps_the_item() {
    let world = World::new().await;
    let created = world.create(json!({ "title": "t" })).await;
    let id = created["baseUri"].as_str().unwrap().to_string();
    let item = "ad4m://label/urgent";
    world
        .ok(
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item }),
        )
        .await;
    assert_eq!(world.task(&id).await.unwrap()["labels"], json!([item]));

    let batch_id = world.ok("perspective.createBatch", json!({})).await;
    let batch_id = batch_id.as_str().expect("batch id").to_string();
    let removed = world
        .ok(
            "perspective.instanceRemoveFromCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    assert_eq!(removed["linksRemoved"], 1, "{removed}");
    let added = world
        .ok(
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    assert_eq!(
        added["linksAdded"], 1,
        "the item is out as the batch sees it, so the add is real: {added}"
    );
    assert_eq!(added["alreadyMember"], false, "{added}");

    world
        .ok("perspective.commitBatch", json!({ "batchId": batch_id }))
        .await;
    assert_eq!(
        world.task(&id).await.unwrap()["labels"],
        json!([item]),
        "a remove the same batch re-added must not land"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn an_update_then_a_remove_in_one_batch_leaves_no_links() {
    let world = World::new().await;
    let created = world
        .create(json!({ "title": "t", "status": "todo" }))
        .await;
    let id = created["baseUri"].as_str().unwrap().to_string();
    assert!(world.links_touching(&id).await > 0);
    let batch_id = world.ok("perspective.createBatch", json!({})).await;
    let batch_id = batch_id.as_str().expect("batch id").to_string();

    world
        .ok(
            "perspective.instanceUpdate",
            json!({ "className": "Task", "baseUri": id, "properties": { "status": "doing" }, "batchId": batch_id }),
        )
        .await;
    world
        .ok(
            "perspective.instanceRemove",
            json!({ "className": "Task", "baseUri": id, "batchId": batch_id }),
        )
        .await;
    world
        .ok("perspective.commitBatch", json!({ "batchId": batch_id }))
        .await;

    assert!(world.task(&id).await.is_none(), "the task is gone");
    assert_eq!(
        world.links_touching(&id).await,
        0,
        "a link the update staged inside the batch must go with the instance"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn repeated_adds_and_removes_in_one_batch_are_idempotent() {
    let world = World::new().await;
    let created = world.create(json!({ "title": "t" })).await;
    let id = created["baseUri"].as_str().unwrap().to_string();
    let item = "ad4m://label/urgent";
    let batch_id = world.ok("perspective.createBatch", json!({})).await;
    let batch_id = batch_id.as_str().expect("batch id").to_string();

    let first = world
        .ok(
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    assert_eq!(first["linksAdded"], 1, "{first}");
    let second = world
        .ok(
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    assert_eq!(
        second["linksAdded"], 0,
        "the item the same batch added is already a member: {second}"
    );
    assert_eq!(second["alreadyMember"], true, "{second}");

    let first_out = world
        .ok(
            "perspective.instanceRemoveFromCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    assert_eq!(first_out["linksRemoved"], 1, "{first_out}");
    let second_out = world
        .ok(
            "perspective.instanceRemoveFromCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    assert_eq!(
        second_out["linksRemoved"], 0,
        "the item the same batch removed is already out: {second_out}"
    );

    world
        .ok("perspective.commitBatch", json!({ "batchId": batch_id }))
        .await;
    let labels = world.task(&id).await.unwrap()["labels"].clone();
    assert!(
        labels.is_null() || labels == json!([]),
        "nothing was a member at the commit: {labels}"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_restaged_committed_link_still_goes_with_the_instance() {
    // `perspective.addLinkExpression` can stage the exact signed expression the store
    // already holds. The batch then carries a copy of a committed link; removing the
    // instance must still stage the committed one's removal, not just drop the copy.
    let world = World::new().await;
    let created = world.create(json!({ "title": "t" })).await;
    let id = created["baseUri"].as_str().unwrap().to_string();
    let before = world.links_touching(&id).await;
    assert!(before > 0);

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

    let batch_id = world.ok("perspective.createBatch", json!({})).await;
    let batch_id = batch_id.as_str().expect("batch id").to_string();
    world
        .ok(
            "perspective.addLinkExpression",
            json!({ "link": type_link, "batchId": batch_id }),
        )
        .await;

    let removed = world
        .ok(
            "perspective.instanceRemove",
            json!({ "className": "Task", "baseUri": id, "batchId": batch_id }),
        )
        .await;
    assert_eq!(
        removed["linksRemoved"].as_u64().unwrap() as usize,
        before,
        "every effective link of the instance, counted once: {removed}"
    );
    world
        .ok("perspective.commitBatch", json!({ "batchId": batch_id }))
        .await;

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
    let created = world.create(json!({ "title": "t" })).await;
    let id = created["baseUri"].as_str().unwrap().to_string();
    let item = "ad4m://label/urgent";

    let batch_id = world.ok("perspective.createBatch", json!({})).await;
    let batch_id = batch_id.as_str().expect("batch id").to_string();
    let signed = world
        .ok(
            "perspective.addLink",
            json!({ "link": { "source": id, "predicate": "ns://task_label", "target": item }, "batchId": batch_id }),
        )
        .await;
    world
        .ok(
            "perspective.addLinkExpression",
            json!({ "link": signed, "batchId": batch_id }),
        )
        .await;

    let removed = world
        .ok(
            "perspective.instanceRemoveFromCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": item, "batchId": batch_id }),
        )
        .await;
    assert_eq!(
        removed["linksRemoved"], 1,
        "one effective link leaves, however many copies were staged: {removed}"
    );

    world
        .ok("perspective.commitBatch", json!({ "batchId": batch_id }))
        .await;
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
        .call(
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "title": "t" }, "batchId": "no-such-batch" }),
        )
        .await
        .expect_err("an unknown batch is refused");
    assert_eq!(error.code, 404, "{}", error.message);
    assert_eq!(error.data.as_ref().unwrap()["kind"], "batchNotFound");
    assert_eq!(world.link_count().await, before);
}

/// The five methods, with params that would write if the guard let them through.
fn write_calls(id: &str) -> Vec<(&'static str, Value)> {
    vec![
        (
            "perspective.instanceCreate",
            json!({ "className": "Task", "properties": { "title": "intruder" } }),
        ),
        (
            "perspective.instanceUpdate",
            json!({ "className": "Task", "baseUri": id, "properties": { "title": "intruder" } }),
        ),
        (
            "perspective.instanceAddToCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": "ad4m://x" }),
        ),
        (
            "perspective.instanceRemoveFromCollection",
            json!({ "className": "Task", "baseUri": id, "collection": "labels", "itemUri": "ad4m://x" }),
        ),
        (
            "perspective.instanceRemove",
            json!({ "className": "Task", "baseUri": id }),
        ),
    ]
}

#[tokio::test(flavor = "multi_thread")]
async fn a_caller_without_the_update_capability_is_refused() {
    let world = World::new().await;
    let created = world.create(json!({ "title": "Mine" })).await;
    let id = created["baseUri"].as_str().unwrap().to_string();
    let before = world.link_count().await;

    // An app on the main agent that may read every perspective, but not write.
    let reader = Arc::new(RequestContext::app_for_test(vec![
        perspective_query_capability(vec!["*".to_string()]),
    ]));
    for (method, mut params) in write_calls(&id) {
        params["uuid"] = json!(world.uuid);
        let result = world
            .handlers
            .dispatch(method, params, reader.clone())
            .await;
        expect_code(result, 403, method);
    }
    assert_eq!(world.link_count().await, before, "no refused call wrote");
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
            "perspective.instanceCreate",
            with_uuid(json!({ "className": "Task", "properties": { "title": "Alice's" } })),
            &alice.session,
        )
        .await
        .unwrap_or_else(|e| panic!("alice writes into her own perspective: {}", e.message));
    let id = created["baseUri"].as_str().unwrap().to_string();

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
        for (method, params) in write_calls(&id) {
            let result = executor.call(method, with_uuid(params), session).await;
            expect_code(result, 404, method);
        }
    }
    assert_eq!(links().await, before, "no refused call wrote");
}

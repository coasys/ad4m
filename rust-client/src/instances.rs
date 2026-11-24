//! Validated instance writes: `perspective.mutate`.
//!
//! One call takes an ordered list of [`MutateOp`]s (`create`, `update`, `delete`) and writes them
//! in one batch with one commit. The executor checks each op against the class's SHACL shape
//! (each property's type, which are required, which take a single value, `sh:in`) before writing
//! anything, and a later op sees the earlier ones: a create followed by an update, an add to a
//! collection or a delete of the new instance works in one call.
//!
//! The call applies all its ops or none. A refused op fails the call with an [`RpcCallError`]
//! whose `data` is the executor's `InstanceError` plus `opIndex`, the op that failed
//! ([`op_index`]); [`validation_errors`] reads the per-property errors of a shape violation
//! (status 422) from it. With `batch` the ops are staged into that open batch
//! (`perspective.createBatch`) and land on its commit; a failed call leaves the batch as it was.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::collections::BTreeMap;

use crate::perspectives::PerspectivesClient;
use crate::ws_rpc::{RpcCallError, WsRpcClient};

/// One write of a [`mutate`] call.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "op", rename_all = "camelCase")]
pub enum MutateOp {
    /// Create an instance of `class`.
    Create {
        class: String,
        /// The new instance's URI; the executor mints one when absent.
        #[serde(skip_serializing_if = "Option::is_none")]
        id: Option<String>,
        /// Values by property name: one JSON value per single-valued property, an array of
        /// item URIs per collection.
        values: Map<String, Value>,
        #[serde(skip_serializing_if = "Option::is_none")]
        parent: Option<ParentRef>,
    },
    /// Change an instance that exists, or that an earlier op of the call created.
    Update {
        class: String,
        id: String,
        /// Single-valued properties to set; the others stay.
        #[serde(skip_serializing_if = "Map::is_empty")]
        set: Map<String, Value>,
        /// Items to add, by collection name.
        #[serde(skip_serializing_if = "BTreeMap::is_empty")]
        add: BTreeMap<String, Vec<String>>,
        /// Items to remove, by collection name; removed before the adds are written.
        #[serde(skip_serializing_if = "BTreeMap::is_empty")]
        remove: BTreeMap<String, Vec<String>>,
    },
    /// Remove an instance and every link from and to it.
    Delete { class: String, id: String },
}

/// Where a created instance goes.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(untagged)]
pub enum ParentRef {
    /// Into the `relation` collection of the parent, an instance of `model`.
    Relation {
        id: String,
        relation: String,
        model: String,
    },
    /// One plain link `parent --predicate--> child`.
    Predicate { id: String, predicate: String },
    /// Under the parent's own `children` relation, else `ad4m://has_child`.
    Contained { id: String },
}

/// What a [`mutate`] call returns besides the ids.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Returns {
    /// The ids only.
    None {},
    /// The ids only; the default.
    Ids {},
    /// Also each created or updated instance as a read shows it after the commit, with only
    /// `properties` of it when given. Needs a call without `batch`.
    Rows {
        #[serde(skip_serializing_if = "Option::is_none")]
        properties: Option<Vec<String>>,
    },
}

/// What a [`mutate`] call did.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct MutateResult {
    /// One id per op, in order: the new instance's id for a create, the id the op named else.
    pub ids: Vec<String>,
    /// With [`Returns::Rows`]: one entry per op, `None` for a delete.
    #[serde(default)]
    pub rows: Option<Vec<Option<Value>>>,
}

/// What is wrong with a property: `kind` (`unknownProperty`, `wrongType`, `notAllowed`,
/// `missingRequired`, `readOnly`, …) and the kind's own fields (`available`, `reason`, …).
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct ValidationProblem {
    pub kind: String,
    #[serde(flatten)]
    pub detail: Map<String, Value>,
}

/// How many values a property takes: `max` none for a collection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct Cardinality {
    pub min: u32,
    pub max: Option<u32>,
}

/// One rejected property of a refused write.
#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ValidationError {
    pub property: String,
    pub problem: ValidationProblem,
    pub expected_type: Option<String>,
    pub cardinality: Option<Cardinality>,
    pub allowed_values: Option<Vec<Value>>,
    pub received: Option<Value>,
    pub item: Option<usize>,
}

/// The per-property errors of a write the executor refused because it breaks the class's shape,
/// or `None` for any other error.
pub fn validation_errors(error: &anyhow::Error) -> Option<Vec<ValidationError>> {
    let data = error.downcast_ref::<RpcCallError>()?.data.as_ref()?;
    if data.get("kind")?.as_str()? != "validation" {
        return None;
    }
    serde_json::from_value(data.get("errors")?.clone()).ok()
}

/// The index of the op a refused [`mutate`] call failed on, or `None` when the error is not a
/// refused op (another error, or a call that failed as a whole: an unknown `batch`).
pub fn op_index(error: &anyhow::Error) -> Option<usize> {
    let data = error.downcast_ref::<RpcCallError>()?.data.as_ref()?;
    usize::try_from(data.get("opIndex")?.as_u64()?).ok()
}

/// `perspective.mutate` on `uuid`.
pub(crate) async fn mutate(
    ws: &WsRpcClient,
    uuid: &str,
    ops: Vec<MutateOp>,
    batch: Option<String>,
    returns: Option<Returns>,
) -> Result<MutateResult> {
    let mut params = serde_json::json!({ "uuid": uuid, "ops": ops });
    if let Some(batch) = batch {
        params["batch"] = Value::String(batch);
    }
    if let Some(returns) = returns {
        params["returns"] = serde_json::to_value(returns)?;
    }
    ws.call("perspective.mutate", params).await
}

impl PerspectivesClient {
    /// Write `ops` in order, all or none; see the module doc.
    pub async fn mutate(
        &self,
        uuid: &str,
        ops: Vec<MutateOp>,
        batch: Option<String>,
        returns: Option<Returns>,
    ) -> Result<MutateResult> {
        mutate(self.ws(), uuid, ops, batch, returns).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::{SinkExt, StreamExt};
    use serde_json::json;
    use std::sync::Arc;
    use tokio::sync::oneshot;
    use tokio_tungstenite::tungstenite::Message;

    /// A client on a one-call executor: it answers the first request with `reply(request)` and
    /// hands the request to the test.
    async fn one_call_executor(
        reply: impl FnOnce(&Value) -> Value + Send + 'static,
    ) -> (PerspectivesClient, oneshot::Receiver<Value>) {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        let (seen_tx, seen_rx) = oneshot::channel();
        tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut ws = tokio_tungstenite::accept_async(stream).await.unwrap();
            if let Some(Ok(Message::Text(text))) = ws.next().await {
                let request: Value = serde_json::from_str(&text).unwrap();
                let mut response = reply(&request);
                response["id"] = request["id"].clone();
                let _ = seen_tx.send(request);
                ws.send(Message::Text(response.to_string())).await.unwrap();
                // Keep the socket open until the client is done reading.
                let _ = ws.next().await;
            }
        });
        let ws = WsRpcClient::connect(&format!("http://127.0.0.1:{port}"), "")
            .await
            .unwrap();
        (PerspectivesClient::new(Arc::new(ws)), seen_rx)
    }

    #[test]
    fn ops_serialize_as_the_executor_reads_them() {
        let mut values = Map::new();
        values.insert("title".into(), "Write".into());
        let ops = vec![
            MutateOp::Create {
                class: "Task".into(),
                id: None,
                values,
                parent: Some(ParentRef::Relation {
                    id: "ad4m://obj/p".into(),
                    relation: "tasks".into(),
                    model: "Project".into(),
                }),
            },
            MutateOp::Update {
                class: "Task".into(),
                id: "ad4m://obj/t".into(),
                set: Map::new(),
                add: BTreeMap::from([("labels".to_string(), vec!["ad4m://l".to_string()])]),
                remove: BTreeMap::new(),
            },
            MutateOp::Delete {
                class: "Task".into(),
                id: "ad4m://obj/t".into(),
            },
        ];
        assert_eq!(
            serde_json::to_value(ops).unwrap(),
            json!([
                { "op": "create", "class": "Task", "values": { "title": "Write" },
                  "parent": { "id": "ad4m://obj/p", "relation": "tasks", "model": "Project" } },
                { "op": "update", "class": "Task", "id": "ad4m://obj/t",
                  "add": { "labels": ["ad4m://l"] } },
                { "op": "delete", "class": "Task", "id": "ad4m://obj/t" },
            ])
        );
        assert_eq!(
            serde_json::to_value(ParentRef::Predicate {
                id: "p".into(),
                predicate: "ns://has".into()
            })
            .unwrap(),
            json!({ "id": "p", "predicate": "ns://has" })
        );
        assert_eq!(
            serde_json::to_value(ParentRef::Contained { id: "p".into() }).unwrap(),
            json!({ "id": "p" })
        );
        assert_eq!(
            serde_json::to_value(Returns::Rows {
                properties: Some(vec!["title".into()])
            })
            .unwrap(),
            json!({ "kind": "rows", "properties": ["title"] })
        );
        assert_eq!(
            serde_json::to_value(Returns::Ids {}).unwrap(),
            json!({ "kind": "ids" })
        );
    }

    /// The round trip: the request the client sends and the result it reads back.
    #[tokio::test]
    async fn mutate_sends_the_ops_next_to_uuid_and_reads_the_ids() {
        let (client, seen) = one_call_executor(|_| {
            json!({ "result": {
                "ids": ["ad4m://obj/new", "ad4m://obj/new"],
                "rows": [{ "id": "ad4m://obj/new", "title": "Write" }, null],
            } })
        })
        .await;
        let mut values = Map::new();
        values.insert("title".into(), "Write".into());
        let result = client
            .mutate(
                "uuid-1",
                vec![
                    MutateOp::Create {
                        class: "Task".into(),
                        id: None,
                        values,
                        parent: None,
                    },
                    MutateOp::Delete {
                        class: "Task".into(),
                        id: "ad4m://obj/new".into(),
                    },
                ],
                None,
                Some(Returns::Rows { properties: None }),
            )
            .await
            .unwrap();
        assert_eq!(result.ids, vec!["ad4m://obj/new"; 2]);
        assert_eq!(
            result.rows,
            Some(vec![
                Some(json!({ "id": "ad4m://obj/new", "title": "Write" })),
                None
            ])
        );

        let request = seen.await.unwrap();
        assert_eq!(request["type"], "perspective.mutate");
        assert_eq!(
            request["params"],
            json!({
                "uuid": "uuid-1",
                "ops": [
                    { "op": "create", "class": "Task", "values": { "title": "Write" } },
                    { "op": "delete", "class": "Task", "id": "ad4m://obj/new" },
                ],
                "returns": { "kind": "rows" },
            })
        );
    }

    #[tokio::test]
    async fn mutate_sends_the_batch() {
        let (client, seen) = one_call_executor(|_| json!({ "result": { "ids": ["a"] } })).await;
        let result = client
            .mutate(
                "uuid-1",
                vec![MutateOp::Delete {
                    class: "Task".into(),
                    id: "a".into(),
                }],
                Some("batch-1".into()),
                None,
            )
            .await
            .unwrap();
        assert_eq!(result.rows, None);
        assert_eq!(seen.await.unwrap()["params"]["batch"], "batch-1");
    }

    #[tokio::test]
    async fn a_refused_write_carries_its_validation_errors_and_op_index() {
        let (client, _seen) = one_call_executor(|_| {
            json!({ "error": {
                "code": 422,
                "message": "op 1: Validation failed for class 'Task': colour (unknownProperty)",
                "data": { "kind": "validation", "className": "Task", "opIndex": 1, "errors": [{
                    "property": "colour",
                    "problem": { "kind": "unknownProperty", "class": "Task", "available": ["title"] },
                    "expectedType": null, "cardinality": null, "allowedValues": null,
                    "received": "red", "item": null,
                }] },
            } })
        })
        .await;
        let error = client
            .mutate(
                "uuid-1",
                vec![MutateOp::Update {
                    class: "Task".into(),
                    id: "ad4m://obj/t".into(),
                    set: Map::new(),
                    add: BTreeMap::new(),
                    remove: BTreeMap::new(),
                }],
                None,
                None,
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("RPC error 422"), "{error}");
        assert_eq!(error.downcast_ref::<RpcCallError>().unwrap().code, 422);
        assert_eq!(op_index(&error), Some(1));
        let errors = validation_errors(&error).expect("validation errors");
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].property, "colour");
        assert_eq!(errors[0].problem.kind, "unknownProperty");
        assert_eq!(errors[0].problem.detail["available"], json!(["title"]));
        assert_eq!(errors[0].received, Some(Value::String("red".into())));
    }
}

//! Validated instance writes: `perspective.instanceCreate`, `.instanceUpdate`, `.instanceRemove`,
//! `.instanceAddToCollection`, `.instanceRemoveFromCollection`.
//!
//! The executor checks each write against the class's SHACL shape (types, cardinality, required
//! properties, `sh:in`) before writing anything. A refused write fails with an
//! [`RpcCallError`] whose `data` is the executor's `InstanceError`; [`validation_errors`] reads
//! the per-property errors of a shape violation (status 422) from it. With `batch_id` a write
//! goes into that open batch (`perspective.createBatch`) and lands on its commit.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use crate::perspectives::PerspectivesClient;
use crate::ws_rpc::RpcCallError;

#[derive(Debug, Clone, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstanceCreate {
    pub class_name: String,
    /// Values by property name: one JSON value per single-valued property, an array of item
    /// URIs per collection.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Map<String, Value>>,
    /// The new instance's URI; a random one when absent.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub base_uri: Option<String>,
    /// A node to put the new instance under.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub batch_id: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstanceUpdate {
    pub class_name: String,
    pub base_uri: String,
    /// The single-valued properties to set; the others stay.
    pub properties: Map<String, Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub batch_id: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstanceRemove {
    pub class_name: String,
    pub base_uri: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub batch_id: Option<String>,
}

/// An item to add to, or remove from, a collection of an instance.
#[derive(Debug, Clone, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstanceCollectionItem {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    pub item_uri: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub batch_id: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct CollectionItems {
    pub collection: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InstanceCreated {
    pub class_name: String,
    pub base_uri: String,
    pub properties_set: Vec<String>,
    pub collections_set: Vec<CollectionItems>,
    pub parent: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InstanceUpdated {
    pub class_name: String,
    pub base_uri: String,
    pub updated_properties: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InstanceRemoved {
    pub class_name: String,
    pub base_uri: String,
    /// `None` through an abstract class, whose classes' destructors ran.
    pub links_removed: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CollectionItemAdded {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    pub item_uri: String,
    pub links_added: usize,
    pub already_member: bool,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CollectionItemRemoved {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    pub item_uri: String,
    pub links_removed: usize,
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

/// `{ uuid, ...body }`, the params of a `perspective.instance*` call.
fn params(uuid: &str, body: impl Serialize) -> Result<Value> {
    let mut params = serde_json::to_value(body)?;
    params["uuid"] = Value::String(uuid.to_string());
    Ok(params)
}

impl PerspectivesClient {
    pub async fn instance_create(
        &self,
        uuid: &str,
        body: InstanceCreate,
    ) -> Result<InstanceCreated> {
        self.ws()
            .call("perspective.instanceCreate", params(uuid, body)?)
            .await
    }

    pub async fn instance_update(
        &self,
        uuid: &str,
        body: InstanceUpdate,
    ) -> Result<InstanceUpdated> {
        self.ws()
            .call("perspective.instanceUpdate", params(uuid, body)?)
            .await
    }

    pub async fn instance_remove(
        &self,
        uuid: &str,
        body: InstanceRemove,
    ) -> Result<InstanceRemoved> {
        self.ws()
            .call("perspective.instanceRemove", params(uuid, body)?)
            .await
    }

    pub async fn instance_add_to_collection(
        &self,
        uuid: &str,
        body: InstanceCollectionItem,
    ) -> Result<CollectionItemAdded> {
        self.ws()
            .call("perspective.instanceAddToCollection", params(uuid, body)?)
            .await
    }

    pub async fn instance_remove_from_collection(
        &self,
        uuid: &str,
        body: InstanceCollectionItem,
    ) -> Result<CollectionItemRemoved> {
        self.ws()
            .call(
                "perspective.instanceRemoveFromCollection",
                params(uuid, body)?,
            )
            .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ws_rpc::WsRpcClient;
    use futures_util::{SinkExt, StreamExt};
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

    #[tokio::test]
    async fn instance_create_sends_the_body_next_to_uuid() {
        let (client, seen) = one_call_executor(|_| {
            serde_json::json!({ "result": {
                "className": "Task", "baseUri": "ad4m://obj/new", "propertiesSet": ["title"],
                "collectionsSet": [], "parent": null,
            } })
        })
        .await;
        let mut properties = Map::new();
        properties.insert("title".into(), "Write".into());
        let created = client
            .instance_create(
                "uuid-1",
                InstanceCreate {
                    class_name: "Task".into(),
                    properties: Some(properties),
                    batch_id: Some("batch-1".into()),
                    ..Default::default()
                },
            )
            .await
            .unwrap();
        assert_eq!(created.base_uri, "ad4m://obj/new");
        assert_eq!(created.properties_set, vec!["title".to_string()]);

        let request = seen.await.unwrap();
        assert_eq!(request["type"], "perspective.instanceCreate");
        assert_eq!(
            request["params"],
            serde_json::json!({
                "uuid": "uuid-1", "className": "Task", "properties": { "title": "Write" },
                "batchId": "batch-1",
            })
        );
    }

    #[tokio::test]
    async fn a_refused_write_carries_its_validation_errors() {
        let (client, _seen) = one_call_executor(|_| {
            serde_json::json!({ "error": {
                "code": 422,
                "message": "Validation failed for class 'Task': colour (unknownProperty)",
                "data": { "kind": "validation", "className": "Task", "errors": [{
                    "property": "colour",
                    "problem": { "kind": "unknownProperty", "class": "Task", "available": ["title"] },
                    "expectedType": null, "cardinality": null, "allowedValues": null,
                    "received": "red", "item": null,
                }] },
            } })
        })
        .await;
        let error = client
            .instance_update(
                "uuid-1",
                InstanceUpdate {
                    class_name: "Task".into(),
                    base_uri: "ad4m://obj/t".into(),
                    ..Default::default()
                },
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("RPC error 422"), "{error}");
        assert_eq!(error.downcast_ref::<RpcCallError>().unwrap().code, 422);
        let errors = validation_errors(&error).expect("validation errors");
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].property, "colour");
        assert_eq!(errors[0].problem.kind, "unknownProperty");
        assert_eq!(
            errors[0].problem.detail["available"],
            serde_json::json!(["title"])
        );
        assert_eq!(errors[0].received, Some(Value::String("red".into())));
    }
}

use std::sync::Arc;

use anyhow::Result;
use serde_json::Value;

use crate::perspective_proxy::PerspectiveProxy;
use crate::types::*;
use crate::ws_rpc::WsRpcClient;

// ── Free functions (used by CLI startup to validate token) ──

pub async fn all(executor_url: String, cap_token: String) -> Result<Vec<PerspectiveHandle>> {
    let ws = WsRpcClient::connect(&executor_url, &cap_token).await?;
    ws.call("perspective.all", serde_json::json!({})).await
}

// ── PerspectivesClient ──

pub struct PerspectivesClient {
    ws: Arc<WsRpcClient>,
}

impl PerspectivesClient {
    pub fn new(ws: Arc<WsRpcClient>) -> Self {
        Self { ws }
    }

    pub fn ws(&self) -> &Arc<WsRpcClient> {
        &self.ws
    }

    pub async fn all(&self) -> Result<Vec<PerspectiveHandle>> {
        self.ws.call("perspective.all", serde_json::json!({})).await
    }

    pub async fn add(&self, name: String) -> Result<PerspectiveHandle> {
        self.ws
            .call("perspective.create", serde_json::json!({ "name": name }))
            .await
    }

    pub async fn remove(&self, uuid: String) -> Result<bool> {
        self.ws
            .call("perspective.remove", serde_json::json!({ "uuid": uuid }))
            .await
    }

    pub async fn add_link(&self, uuid: String, link: LinkInput) -> Result<LinkExpression> {
        self.ws
            .call(
                "perspective.addLink",
                serde_json::json!({ "uuid": uuid, "link": link }),
            )
            .await
    }

    pub async fn remove_link(&self, uuid: String, link: LinkExpression) -> Result<bool> {
        let link_input: LinkExpressionInput = link.into();
        self.ws
            .call(
                "perspective.removeLink",
                serde_json::json!({ "uuid": uuid, "link": link_input }),
            )
            .await
    }

    pub async fn query_links(
        &self,
        uuid: String,
        source: Option<String>,
        target: Option<String>,
        predicate: Option<String>,
        from_date: Option<String>,
        until_date: Option<String>,
        limit: Option<i64>,
    ) -> Result<Vec<LinkExpression>> {
        let mut params = serde_json::json!({ "uuid": uuid });
        if let Some(v) = source {
            params["source"] = Value::String(v);
        }
        if let Some(v) = target {
            params["target"] = Value::String(v);
        }
        if let Some(v) = predicate {
            params["predicate"] = Value::String(v);
        }
        if let Some(v) = from_date {
            params["fromDate"] = Value::String(v);
        }
        if let Some(v) = until_date {
            params["untilDate"] = Value::String(v);
        }
        if let Some(v) = limit {
            params["limit"] = Value::Number(serde_json::Number::from(v));
        }
        self.ws.call("perspective.queryLinks", params).await
    }

    pub async fn infer(&self, uuid: String, prolog_query: String) -> Result<Value> {
        self.ws
            .call(
                "perspective.queryProlog",
                serde_json::json!({ "uuid": uuid, "query": prolog_query }),
            )
            .await
    }

    pub async fn snapshot(&self, uuid: String) -> Result<Perspective> {
        self.ws
            .call("perspective.snapshot", serde_json::json!({ "uuid": uuid }))
            .await
    }

    /// Drop an open batch without committing it — the recovery for a failed batched call:
    /// a failed call does not roll back the calls before it and may have staged part of its
    /// own work, so discard rather than commit to keep the successful calls. `true` when the
    /// batch was open and is now dropped.
    pub async fn discard_batch(&self, uuid: String, batch_id: String) -> Result<bool> {
        self.ws
            .call(
                "perspective.discardBatch",
                serde_json::json!({ "uuid": uuid, "batchId": batch_id }),
            )
            .await
    }

    pub async fn get(&self, uuid: String) -> Result<PerspectiveProxy> {
        Ok(PerspectiveProxy::new(self.ws.clone(), uuid))
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use futures_util::{SinkExt, StreamExt};
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
    async fn discard_batch_sends_uuid_and_batch_id() {
        let (client, seen) = one_call_executor(|_| serde_json::json!({ "result": true })).await;
        assert!(client
            .discard_batch("uuid-1".into(), "batch-1".into())
            .await
            .unwrap());

        let request = seen.await.unwrap();
        assert_eq!(request["type"], "perspective.discardBatch");
        assert_eq!(
            request["params"],
            serde_json::json!({ "uuid": "uuid-1", "batchId": "batch-1" })
        );
    }
}

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
        self.ws
            .call("perspective.all", serde_json::json!({}))
            .await
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
            .call(
                "perspective.snapshot",
                serde_json::json!({ "uuid": uuid }),
            )
            .await
    }

    pub async fn get(&self, uuid: String) -> Result<PerspectiveProxy> {
        Ok(PerspectiveProxy::new(self.ws.clone(), uuid))
    }
}

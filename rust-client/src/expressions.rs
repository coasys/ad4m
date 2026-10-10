use std::sync::Arc;

use anyhow::Result;

use crate::ws_rpc::WsRpcClient;

pub struct ExpressionsClient {
    ws: Arc<WsRpcClient>,
}

impl ExpressionsClient {
    pub fn new(ws: Arc<WsRpcClient>) -> Self {
        Self { ws }
    }

    pub async fn expression_create(
        &self,
        content: String,
        language_address: String,
    ) -> Result<String> {
        self.ws
            .call(
                "expression.create",
                serde_json::json!({
                    "content": content,
                    "languageAddress": language_address,
                }),
            )
            .await
    }

    pub async fn expression(&self, url: String) -> Result<serde_json::Value> {
        self.ws
            .call("expression.get", serde_json::json!({ "url": url }))
            .await
    }

    pub async fn expression_raw(&self, url: String) -> Result<String> {
        self.ws
            .call(
                "expression.get",
                serde_json::json!({ "url": url, "raw": true }),
            )
            .await
    }

    pub async fn interactions(&self, url: String) -> Result<Vec<serde_json::Value>> {
        self.ws
            .call("expression.interactions", serde_json::json!({ "url": url }))
            .await
    }

    pub async fn interact(
        &self,
        url: String,
        interaction_call: serde_json::Value,
    ) -> Result<serde_json::Value> {
        self.ws
            .call(
                "expression.interact",
                serde_json::json!({
                    "url": url,
                    "interactionCall": interaction_call,
                }),
            )
            .await
    }
}

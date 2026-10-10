use std::sync::Arc;

use anyhow::Result;

use crate::types::*;
use crate::ws_rpc::WsRpcClient;

pub struct LanguagesClient {
    ws: Arc<WsRpcClient>,
}

impl LanguagesClient {
    pub fn new(ws: Arc<WsRpcClient>) -> Self {
        Self { ws }
    }

    pub async fn by_filter(&self, filter: Option<String>) -> Result<Vec<LanguageHandle>> {
        match filter {
            Some(f) => {
                self.ws
                    .call("language.all", serde_json::json!({ "filter": f }))
                    .await
            }
            None => self.ws.call("language.all", serde_json::json!({})).await,
        }
    }

    pub async fn by_address(&self, address: String) -> Result<LanguageHandle> {
        self.ws
            .call("language.get", serde_json::json!({ "address": address }))
            .await
    }

    pub async fn write_settings(
        &self,
        address: String,
        settings: String,
    ) -> Result<serde_json::Value> {
        self.ws
            .call(
                "language.writeSettings",
                serde_json::json!({ "address": address, "settings": settings }),
            )
            .await
    }

    pub async fn apply_template_and_publish(
        &self,
        source_language_hash: String,
        template_data: String,
    ) -> Result<LanguageRef> {
        self.ws
            .call(
                "language.applyTemplate",
                serde_json::json!({
                    "sourceLanguageHash": source_language_hash,
                    "templateData": template_data,
                }),
            )
            .await
    }

    pub async fn meta(&self, address: String) -> Result<LanguageMeta> {
        self.ws
            .call("language.meta", serde_json::json!({ "address": address }))
            .await
    }

    pub async fn publish(
        &self,
        language_path: String,
        name: Option<String>,
        description: Option<String>,
        possible_template_params: Option<Vec<String>>,
        source_code_link: Option<String>,
    ) -> Result<LanguageMeta> {
        self.ws
            .call(
                "language.publish",
                serde_json::json!({
                    "languagePath": language_path,
                    "languageMeta": {
                        "name": name,
                        "description": description,
                        "possibleTemplateParams": possible_template_params,
                        "sourceCodeLink": source_code_link,
                    },
                }),
            )
            .await
    }

    pub async fn source(&self, address: String) -> Result<String> {
        self.ws
            .call("language.source", serde_json::json!({ "address": address }))
            .await
    }

    pub async fn remove(&self, address: String) -> Result<bool> {
        self.ws
            .call("language.remove", serde_json::json!({ "address": address }))
            .await
    }
}

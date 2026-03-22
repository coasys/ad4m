use std::sync::Arc;

use anyhow::Result;
use serde::Serialize;

use crate::util;
use crate::ClientInfo;

// ── Request types ──

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct CreateExpressionBody {
    pub content: String,
    pub language_address: String,
}

// ── Free functions ──

pub async fn expression_create(
    executor_url: String,
    cap_token: String,
    content: String,
    language_address: String,
) -> Result<String> {
    let body = CreateExpressionBody {
        content,
        language_address,
    };
    util::post(&executor_url, &cap_token, "/expressions", &body).await
}

pub async fn expression(
    executor_url: String,
    cap_token: String,
    url: String,
) -> Result<serde_json::Value> {
    util::get(
        &executor_url,
        &cap_token,
        &format!("/expressions/{}", urlencoding::encode(&url)),
    )
    .await
}

// ── ExpressionsClient ──

pub struct ExpressionsClient {
    info: Arc<ClientInfo>,
}

impl ExpressionsClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn expression_create(
        &self,
        content: String,
        language_address: String,
    ) -> Result<String> {
        expression_create(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            content,
            language_address,
        )
        .await
    }

    pub async fn expression(&self, url: String) -> Result<serde_json::Value> {
        expression(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            url,
        )
        .await
    }
}

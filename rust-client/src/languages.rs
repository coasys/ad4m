use std::sync::Arc;

use anyhow::Result;
use serde::Serialize;

use crate::types::*;
use crate::util;
use crate::ClientInfo;

// ── Request types ──

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct PublishLanguageBody {
    pub language_path: String,
    pub language_meta: LanguageMetaInputBody,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct LanguageMetaInputBody {
    pub name: Option<String>,
    pub description: Option<String>,
    pub possible_template_params: Option<Vec<String>>,
    pub source_code_link: Option<String>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct ApplyTemplateBody {
    pub source_language_hash: String,
    pub template_data: String,
}

// ── Free functions ──

pub async fn by_filter(
    executor_url: String,
    cap_token: String,
    filter: Option<String>,
) -> Result<Vec<LanguageHandle>> {
    let query = filter
        .map(|f| format!("?filter={}", urlencoding::encode(&f)))
        .unwrap_or_default();
    util::get(&executor_url, &cap_token, &format!("/languages{}", query)).await
}

pub async fn by_address(
    executor_url: String,
    cap_token: String,
    address: String,
) -> Result<LanguageHandle> {
    util::get(
        &executor_url,
        &cap_token,
        &format!("/languages/{}", address),
    )
    .await
}

pub async fn write_settings(
    executor_url: String,
    cap_token: String,
    address: String,
    settings: String,
) -> Result<serde_json::Value> {
    util::put(
        &executor_url,
        &cap_token,
        &format!("/languages/{}/settings", address),
        &serde_json::json!({ "settings": settings }),
    )
    .await
}

pub async fn apply_template_and_publish(
    executor_url: String,
    cap_token: String,
    source_language_hash: String,
    template_data: String,
) -> Result<LanguageRef> {
    let body = ApplyTemplateBody {
        source_language_hash,
        template_data,
    };
    util::post(
        &executor_url,
        &cap_token,
        "/languages/apply-template",
        &body,
    )
    .await
}

pub async fn meta(
    executor_url: String,
    cap_token: String,
    address: String,
) -> Result<LanguageMeta> {
    util::get(
        &executor_url,
        &cap_token,
        &format!("/languages/{}/meta", address),
    )
    .await
}

pub async fn publish(
    executor_url: String,
    cap_token: String,
    language_path: String,
    name: Option<String>,
    description: Option<String>,
    possible_template_params: Option<Vec<String>>,
    source_code_link: Option<String>,
) -> Result<LanguageMeta> {
    let body = PublishLanguageBody {
        language_path,
        language_meta: LanguageMetaInputBody {
            name,
            description,
            possible_template_params,
            source_code_link,
        },
    };
    util::post(&executor_url, &cap_token, "/languages/publish", &body).await
}

pub async fn source(executor_url: String, cap_token: String, address: String) -> Result<String> {
    util::get(
        &executor_url,
        &cap_token,
        &format!("/languages/{}/source", address),
    )
    .await
}

pub async fn remove(executor_url: String, cap_token: String, address: String) -> Result<()> {
    util::delete_no_response(
        &executor_url,
        &cap_token,
        &format!("/languages/{}", address),
    )
    .await
}

// ── LanguagesClient ──

pub struct LanguagesClient {
    info: Arc<ClientInfo>,
}

impl LanguagesClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn by_filter(&self, filter: Option<String>) -> Result<Vec<LanguageHandle>> {
        by_filter(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            filter,
        )
        .await
    }

    pub async fn by_address(&self, address: String) -> Result<LanguageHandle> {
        by_address(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
        )
        .await
    }

    pub async fn write_settings(
        &self,
        address: String,
        settings: String,
    ) -> Result<serde_json::Value> {
        write_settings(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
            settings,
        )
        .await
    }

    pub async fn apply_template_and_publish(
        &self,
        source_language_hash: String,
        template_data: String,
    ) -> Result<LanguageRef> {
        apply_template_and_publish(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            source_language_hash,
            template_data,
        )
        .await
    }

    pub async fn meta(&self, address: String) -> Result<LanguageMeta> {
        meta(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
        )
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
        publish(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            language_path,
            name,
            description,
            possible_template_params,
            source_code_link,
        )
        .await
    }

    pub async fn source(&self, address: String) -> Result<String> {
        source(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
        )
        .await
    }

    pub async fn remove(&self, address: String) -> Result<()> {
        remove(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
        )
        .await
    }
}

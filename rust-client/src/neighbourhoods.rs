use std::sync::Arc;

use anyhow::Result;
use serde::Serialize;

use crate::types::*;
use crate::util;
use crate::ClientInfo;

// ── Request types ──

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct PublishNeighbourhoodBody {
    pub perspective_uuid: String,
    pub link_language: String,
    pub meta: PerspectiveInput,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct JoinNeighbourhoodBody {
    pub url: String,
}

// ── Free functions ──

pub async fn publish(
    executor_url: String,
    cap_token: String,
    perspective_uuid: String,
    link_language: String,
    meta: Perspective,
) -> Result<String> {
    let body = PublishNeighbourhoodBody {
        perspective_uuid,
        link_language,
        meta: meta.into(),
    };
    util::post(&executor_url, &cap_token, "/neighbourhoods/publish", &body).await
}

pub async fn join(
    executor_url: String,
    cap_token: String,
    url: String,
) -> Result<PerspectiveHandle> {
    let body = JoinNeighbourhoodBody { url };
    util::post(&executor_url, &cap_token, "/neighbourhoods/join", &body).await
}

// ── NeighbourhoodsClient ──

pub struct NeighbourhoodsClient {
    info: Arc<ClientInfo>,
}

impl NeighbourhoodsClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn publish(
        &self,
        perspective_uuid: String,
        link_language: String,
        meta: Perspective,
    ) -> Result<String> {
        publish(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            perspective_uuid,
            link_language,
            meta,
        )
        .await
    }

    pub async fn join(&self, url: String) -> Result<PerspectiveHandle> {
        join(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            url,
        )
        .await
    }
}

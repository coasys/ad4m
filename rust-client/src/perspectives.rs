use std::sync::Arc;

use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::perspective_proxy::PerspectiveProxy;
use crate::types::*;
use crate::util;
use crate::ClientInfo;

// ── Request types ──

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct CreatePerspectiveBody {
    pub name: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct LinkMutationBody {
    pub additions: Option<Vec<LinkInput>>,
    pub removals: Option<Vec<LinkExpressionInput>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct QueryBody {
    pub engine: String,
    pub query: String,
}

// ── Free functions (legacy API) ──

pub async fn all(executor_url: String, cap_token: String) -> Result<Vec<PerspectiveHandle>> {
    util::get(&executor_url, &cap_token, "/perspectives").await
}

pub async fn add(
    executor_url: String,
    cap_token: String,
    name: String,
) -> Result<PerspectiveHandle> {
    let body = CreatePerspectiveBody { name };
    util::post(&executor_url, &cap_token, "/perspectives", &body).await
}

pub async fn remove(executor_url: String, cap_token: String, uuid: String) -> Result<()> {
    util::delete_no_response(
        &executor_url,
        &cap_token,
        &format!("/perspectives/{}", uuid),
    )
    .await
}

pub async fn add_link(
    executor_url: String,
    cap_token: String,
    uuid: String,
    link: LinkInput,
) -> Result<LinkExpression> {
    let body = LinkMutationBody {
        additions: Some(vec![link]),
        removals: None,
    };
    #[derive(Deserialize)]
    struct MutationResponse {
        additions: Vec<LinkExpression>,
    }
    let resp: MutationResponse = util::post(
        &executor_url,
        &cap_token,
        &format!("/perspectives/{}/links", uuid),
        &body,
    )
    .await?;
    resp.additions
        .into_iter()
        .next()
        .ok_or_else(|| anyhow::anyhow!("No link returned from add"))
}

pub async fn remove_link(
    executor_url: String,
    cap_token: String,
    uuid: String,
    link: LinkExpression,
) -> Result<()> {
    let body = LinkMutationBody {
        additions: None,
        removals: Some(vec![link.into()]),
    };
    let _resp: serde_json::Value = util::post(
        &executor_url,
        &cap_token,
        &format!("/perspectives/{}/links", uuid),
        &body,
    )
    .await?;
    Ok(())
}

pub async fn query_links(
    executor_url: String,
    cap_token: String,
    uuid: String,
    source: Option<String>,
    target: Option<String>,
    predicate: Option<String>,
    from_date: Option<String>,
    until_date: Option<String>,
    limit: Option<i64>,
) -> Result<Vec<LinkExpression>> {
    let mut params = Vec::new();
    if let Some(v) = source {
        params.push(format!("source={}", urlencoding::encode(&v)));
    }
    if let Some(v) = target {
        params.push(format!("target={}", urlencoding::encode(&v)));
    }
    if let Some(v) = predicate {
        params.push(format!("predicate={}", urlencoding::encode(&v)));
    }
    if let Some(v) = from_date {
        params.push(format!("fromDate={}", urlencoding::encode(&v)));
    }
    if let Some(v) = until_date {
        params.push(format!("untilDate={}", urlencoding::encode(&v)));
    }
    if let Some(v) = limit {
        params.push(format!("limit={}", v));
    }
    let query_string = if params.is_empty() {
        String::new()
    } else {
        format!("?{}", params.join("&"))
    };
    util::get(
        &executor_url,
        &cap_token,
        &format!("/perspectives/{}/links{}", uuid, query_string),
    )
    .await
}

pub async fn infer(
    executor_url: String,
    cap_token: String,
    uuid: String,
    prolog_query: String,
) -> Result<Value> {
    let body = QueryBody {
        engine: "prolog".to_string(),
        query: prolog_query,
    };
    util::post(
        &executor_url,
        &cap_token,
        &format!("/perspectives/{}/query", uuid),
        &body,
    )
    .await
}

pub async fn snapshot(
    executor_url: String,
    cap_token: String,
    uuid: String,
) -> Result<Perspective> {
    util::get(
        &executor_url,
        &cap_token,
        &format!("/perspectives/{}/snapshot", uuid),
    )
    .await
}

// ── PerspectivesClient ──

pub struct PerspectivesClient {
    info: Arc<ClientInfo>,
}

impl PerspectivesClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn all(&self) -> Result<Vec<PerspectiveHandle>> {
        all(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn add(&self, name: String) -> Result<PerspectiveHandle> {
        add(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            name,
        )
        .await
    }

    pub async fn remove(&self, uuid: String) -> Result<()> {
        remove(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
        )
        .await
    }

    pub async fn add_link(&self, uuid: String, link: LinkInput) -> Result<LinkExpression> {
        add_link(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
            link,
        )
        .await
    }

    pub async fn remove_link(&self, uuid: String, link: LinkExpression) -> Result<()> {
        remove_link(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
            link,
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
        query_links(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
            source,
            target,
            predicate,
            from_date,
            until_date,
            limit,
        )
        .await
    }

    pub async fn infer(&self, uuid: String, prolog_query: String) -> Result<Value> {
        infer(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
            prolog_query,
        )
        .await
    }

    pub async fn snapshot(&self, uuid: String) -> Result<Perspective> {
        snapshot(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
        )
        .await
    }

    pub async fn get(&self, uuid: String) -> Result<PerspectiveProxy> {
        Ok(PerspectiveProxy::new(self.info.clone(), uuid))
    }
}

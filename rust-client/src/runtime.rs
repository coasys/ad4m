use std::sync::Arc;

use anyhow::Result;
use serde::Serialize;

use crate::types::*;
use crate::util;
use crate::ClientInfo;

// ── Request types ──

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct VerifySignatureBody {
    pub did: String,
    pub data: String,
    pub signed_data: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
#[allow(dead_code)]
struct FriendSendMessageBody {
    pub message: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct SetStatusBody {
    pub status: serde_json::Value,
}

// ── Free functions ──

pub async fn info(executor_url: String, cap_token: String) -> Result<RuntimeInfo> {
    util::get(&executor_url, &cap_token, "/runtime/info").await
}

pub async fn quit(executor_url: String, cap_token: String) -> Result<serde_json::Value> {
    util::post(
        &executor_url,
        &cap_token,
        "/runtime/quit",
        &serde_json::json!({}),
    )
    .await
}

pub async fn add_trusted_agents(
    executor_url: String,
    cap_token: String,
    agents: Vec<String>,
) -> Result<Vec<String>> {
    util::put(&executor_url, &cap_token, "/agent/trusted", &agents).await
}

pub async fn delete_trusted_agents(
    executor_url: String,
    cap_token: String,
    agents: Vec<String>,
) -> Result<Vec<String>> {
    util::delete_with_body(&executor_url, &cap_token, "/agent/trusted", &agents).await
}

pub async fn trusted_agents(executor_url: String, cap_token: String) -> Result<Vec<String>> {
    util::get(&executor_url, &cap_token, "/agent/trusted").await
}

pub async fn link_language_templates(
    executor_url: String,
    cap_token: String,
) -> Result<Vec<String>> {
    util::get(
        &executor_url,
        &cap_token,
        "/runtime/link-language-templates",
    )
    .await
}

pub async fn add_link_language_templates(
    executor_url: String,
    cap_token: String,
    addresses: Vec<String>,
) -> Result<Vec<String>> {
    util::put(
        &executor_url,
        &cap_token,
        "/runtime/link-language-templates",
        &addresses,
    )
    .await
}

pub async fn remove_link_language_templates(
    executor_url: String,
    cap_token: String,
    addresses: Vec<String>,
) -> Result<Vec<String>> {
    util::delete_with_body(
        &executor_url,
        &cap_token,
        "/runtime/link-language-templates",
        &addresses,
    )
    .await
}

pub async fn friends(executor_url: String, cap_token: String) -> Result<Vec<String>> {
    util::get(&executor_url, &cap_token, "/runtime/friends").await
}

pub async fn add_friends(
    executor_url: String,
    cap_token: String,
    dids: Vec<String>,
) -> Result<Vec<String>> {
    util::put(&executor_url, &cap_token, "/runtime/friends", &dids).await
}

pub async fn remove_friends(
    executor_url: String,
    cap_token: String,
    dids: Vec<String>,
) -> Result<Vec<String>> {
    util::delete_with_body(&executor_url, &cap_token, "/runtime/friends", &dids).await
}

pub async fn hc_agent_infos(executor_url: String, cap_token: String) -> Result<String> {
    util::get(&executor_url, &cap_token, "/runtime/hc/agent-infos").await
}

pub async fn network_metrics(executor_url: String, cap_token: String) -> Result<String> {
    util::get(&executor_url, &cap_token, "/runtime/network-metrics").await
}

pub async fn hc_add_agent_infos(
    executor_url: String,
    cap_token: String,
    agent_infos: String,
) -> Result<serde_json::Value> {
    util::post(
        &executor_url,
        &cap_token,
        "/runtime/hc/agent-infos",
        &serde_json::json!({ "agentInfos": agent_infos }),
    )
    .await
}

pub async fn verify_string_signed_by_did(
    executor_url: String,
    cap_token: String,
    did: String,
    data: String,
    signed_data: String,
) -> Result<bool> {
    let body = VerifySignatureBody {
        did,
        data,
        signed_data,
    };
    util::post(
        &executor_url,
        &cap_token,
        "/runtime/verify-signature",
        &body,
    )
    .await
}

pub async fn set_status(
    executor_url: String,
    cap_token: String,
    status: serde_json::Value,
) -> Result<serde_json::Value> {
    let body = SetStatusBody { status };
    util::put(&executor_url, &cap_token, "/runtime/status", &body).await
}

pub async fn message_inbox(
    executor_url: String,
    cap_token: String,
    filter: Option<String>,
) -> Result<Vec<serde_json::Value>> {
    let query = filter
        .map(|f| format!("?filter={}", urlencoding::encode(&f)))
        .unwrap_or_default();
    util::get(
        &executor_url,
        &cap_token,
        &format!("/runtime/messages/inbox{}", query),
    )
    .await
}

pub async fn message_outbox(
    executor_url: String,
    cap_token: String,
    filter: Option<String>,
) -> Result<Vec<serde_json::Value>> {
    let query = filter
        .map(|f| format!("?filter={}", urlencoding::encode(&f)))
        .unwrap_or_default();
    util::get(
        &executor_url,
        &cap_token,
        &format!("/runtime/messages/outbox{}", query),
    )
    .await
}

// ── RuntimeClient ──

pub struct RuntimeClient {
    info: Arc<ClientInfo>,
}

impl RuntimeClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn info(&self) -> Result<RuntimeInfo> {
        info(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn quit(&self) -> Result<serde_json::Value> {
        quit(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn add_trusted_agents(&self, agents: Vec<String>) -> Result<Vec<String>> {
        add_trusted_agents(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            agents,
        )
        .await
    }

    pub async fn delete_trusted_agents(&self, agents: Vec<String>) -> Result<Vec<String>> {
        delete_trusted_agents(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            agents,
        )
        .await
    }

    pub async fn trusted_agents(&self) -> Result<Vec<String>> {
        trusted_agents(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn link_language_templates(&self) -> Result<Vec<String>> {
        link_language_templates(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn add_link_language_templates(&self, addresses: Vec<String>) -> Result<Vec<String>> {
        add_link_language_templates(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            addresses,
        )
        .await
    }

    pub async fn remove_link_language_templates(
        &self,
        addresses: Vec<String>,
    ) -> Result<Vec<String>> {
        remove_link_language_templates(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            addresses,
        )
        .await
    }

    pub async fn friends(&self) -> Result<Vec<String>> {
        friends(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn add_friends(&self, dids: Vec<String>) -> Result<Vec<String>> {
        add_friends(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            dids,
        )
        .await
    }

    pub async fn remove_friends(&self, dids: Vec<String>) -> Result<Vec<String>> {
        remove_friends(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            dids,
        )
        .await
    }

    pub async fn hc_agent_infos(&self) -> Result<String> {
        hc_agent_infos(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn network_metrics(&self) -> Result<String> {
        network_metrics(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn hc_add_agent_infos(&self, agent_infos: String) -> Result<serde_json::Value> {
        hc_add_agent_infos(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            agent_infos,
        )
        .await
    }

    pub async fn verify_string_signed_by_did(
        &self,
        did: String,
        data: String,
        signed_data: String,
    ) -> Result<bool> {
        verify_string_signed_by_did(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            did,
            data,
            signed_data,
        )
        .await
    }

    pub async fn set_status(&self, status: serde_json::Value) -> Result<serde_json::Value> {
        set_status(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            status,
        )
        .await
    }

    pub async fn message_inbox(&self, filter: Option<String>) -> Result<Vec<serde_json::Value>> {
        message_inbox(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            filter,
        )
        .await
    }

    pub async fn message_outbox(&self, filter: Option<String>) -> Result<Vec<serde_json::Value>> {
        message_outbox(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            filter,
        )
        .await
    }
}

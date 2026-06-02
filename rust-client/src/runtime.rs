use std::sync::Arc;

use anyhow::Result;

use crate::types::*;
use crate::ws_rpc::WsRpcClient;

pub struct RuntimeClient {
    ws: Arc<WsRpcClient>,
}

impl RuntimeClient {
    pub fn new(ws: Arc<WsRpcClient>) -> Self {
        Self { ws }
    }

    pub async fn info(&self) -> Result<RuntimeInfo> {
        self.ws.call("runtime.info", serde_json::json!({})).await
    }

    pub async fn quit(&self) -> Result<serde_json::Value> {
        self.ws.call("runtime.quit", serde_json::json!({})).await
    }

    pub async fn add_trusted_agents(&self, agents: Vec<String>) -> Result<Vec<String>> {
        self.ws
            .call(
                "agent.addTrustedAgents",
                serde_json::json!({ "agents": agents }),
            )
            .await
    }

    pub async fn delete_trusted_agents(&self, agents: Vec<String>) -> Result<Vec<String>> {
        self.ws
            .call(
                "agent.deleteTrustedAgents",
                serde_json::json!({ "agents": agents }),
            )
            .await
    }

    pub async fn trusted_agents(&self) -> Result<Vec<String>> {
        self.ws
            .call("agent.getTrustedAgents", serde_json::json!({}))
            .await
    }

    pub async fn link_language_templates(&self) -> Result<Vec<String>> {
        self.ws
            .call("runtime.linkLanguageTemplates", serde_json::json!({}))
            .await
    }

    pub async fn add_link_language_templates(&self, addresses: Vec<String>) -> Result<Vec<String>> {
        self.ws
            .call(
                "runtime.addLinkLanguageTemplates",
                serde_json::json!({ "addresses": addresses }),
            )
            .await
    }

    pub async fn remove_link_language_templates(
        &self,
        addresses: Vec<String>,
    ) -> Result<Vec<String>> {
        self.ws
            .call(
                "runtime.removeLinkLanguageTemplates",
                serde_json::json!({ "addresses": addresses }),
            )
            .await
    }

    pub async fn friends(&self) -> Result<Vec<String>> {
        self.ws.call("runtime.friends", serde_json::json!({})).await
    }

    pub async fn add_friends(&self, dids: Vec<String>) -> Result<Vec<String>> {
        self.ws
            .call("runtime.addFriends", serde_json::json!({ "dids": dids }))
            .await
    }

    pub async fn remove_friends(&self, dids: Vec<String>) -> Result<Vec<String>> {
        self.ws
            .call("runtime.removeFriends", serde_json::json!({ "dids": dids }))
            .await
    }

    pub async fn hc_agent_infos(&self) -> Result<String> {
        self.ws
            .call("runtime.hcAgentInfos", serde_json::json!({}))
            .await
    }

    pub async fn network_metrics(&self) -> Result<String> {
        self.ws
            .call("runtime.networkMetrics", serde_json::json!({}))
            .await
    }

    pub async fn hc_add_agent_infos(&self, agent_infos: Vec<String>) -> Result<serde_json::Value> {
        self.ws
            .call(
                "runtime.addHcAgentInfos",
                serde_json::json!({ "agentInfos": agent_infos }),
            )
            .await
    }

    pub async fn verify_string_signed_by_did(
        &self,
        did: String,
        did_signing_key_id: String,
        data: String,
        signed_data: String,
    ) -> Result<bool> {
        self.ws
            .call(
                "runtime.verifySignature",
                serde_json::json!({
                    "did": did,
                    "didSigningKeyId": did_signing_key_id,
                    "data": data,
                    "signedData": signed_data,
                }),
            )
            .await
    }

    pub async fn set_status(&self, status: serde_json::Value) -> Result<serde_json::Value> {
        self.ws
            .call("runtime.setStatus", serde_json::json!({ "status": status }))
            .await
    }

    pub async fn message_inbox(&self, filter: Option<String>) -> Result<Vec<serde_json::Value>> {
        self.ws
            .call("runtime.inbox", serde_json::json!({ "filter": filter }))
            .await
    }

    pub async fn message_outbox(&self, filter: Option<String>) -> Result<Vec<serde_json::Value>> {
        self.ws
            .call("runtime.outbox", serde_json::json!({ "filter": filter }))
            .await
    }

    pub async fn restart_holochain(&self) -> Result<serde_json::Value> {
        self.ws
            .call("runtime.restartHolochain", serde_json::json!({}))
            .await
    }
}

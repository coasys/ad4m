use std::sync::Arc;

use anyhow::Result;

use crate::types::*;
use crate::ws_rpc::WsRpcClient;

pub struct NeighbourhoodsClient {
    ws: Arc<WsRpcClient>,
}

impl NeighbourhoodsClient {
    pub fn new(ws: Arc<WsRpcClient>) -> Self {
        Self { ws }
    }

    pub async fn publish(
        &self,
        perspective_uuid: String,
        link_language: String,
        meta: Perspective,
    ) -> Result<String> {
        let meta_input: PerspectiveInput = meta.into();
        self.ws
            .call(
                "neighbourhood.publish",
                serde_json::json!({
                    "perspectiveUUID": perspective_uuid,
                    "linkLanguage": link_language,
                    "meta": meta_input,
                }),
            )
            .await
    }

    pub async fn join(&self, url: String) -> Result<PerspectiveHandle> {
        self.ws
            .call("neighbourhood.join", serde_json::json!({ "url": url }))
            .await
    }

    pub async fn other_agents(&self, perspective_uuid: String) -> Result<Vec<String>> {
        self.ws
            .call(
                "neighbourhood.otherAgents",
                serde_json::json!({ "uuid": perspective_uuid }),
            )
            .await
    }

    pub async fn has_telepresence_adapter(&self, perspective_uuid: String) -> Result<bool> {
        self.ws
            .call(
                "neighbourhood.hasTelepresence",
                serde_json::json!({ "uuid": perspective_uuid }),
            )
            .await
    }

    pub async fn online_agents(&self, perspective_uuid: String) -> Result<Vec<OnlineAgent>> {
        self.ws
            .call(
                "neighbourhood.onlineAgents",
                serde_json::json!({ "uuid": perspective_uuid }),
            )
            .await
    }

    pub async fn set_online_status(
        &self,
        perspective_uuid: String,
        status: serde_json::Value,
    ) -> Result<bool> {
        self.ws
            .call(
                "neighbourhood.setOnlineStatus",
                serde_json::json!({
                    "uuid": perspective_uuid,
                    "status": status,
                }),
            )
            .await
    }

    pub async fn send_signal(
        &self,
        perspective_uuid: String,
        remote_agent_did: String,
        payload: serde_json::Value,
    ) -> Result<bool> {
        self.ws
            .call(
                "neighbourhood.sendSignal",
                serde_json::json!({
                    "uuid": perspective_uuid,
                    "remoteAgentDid": remote_agent_did,
                    "payload": payload,
                }),
            )
            .await
    }

    pub async fn send_broadcast(
        &self,
        perspective_uuid: String,
        payload: serde_json::Value,
        loopback: bool,
    ) -> Result<bool> {
        self.ws
            .call(
                "neighbourhood.sendBroadcast",
                serde_json::json!({
                    "uuid": perspective_uuid,
                    "payload": payload,
                    "loopback": loopback,
                }),
            )
            .await
    }
}

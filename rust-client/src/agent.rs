use std::sync::Arc;

use anyhow::Result;

use crate::types::*;
use crate::ws_rpc::WsRpcClient;

// ── Free functions (used by CLI startup for auth flow) ──

pub async fn request_capability(
    executor_url: String,
    app_name: String,
    app_desc: String,
    app_domain: String,
    app_url: Option<String>,
    app_icon_path: Option<String>,
    capabilities: Option<Vec<Capability>>,
) -> Result<String> {
    let ws = WsRpcClient::connect(&executor_url, "").await?;
    let caps: Option<Vec<CapabilityInput>> =
        capabilities.map(|c| c.into_iter().map(CapabilityInput::from).collect());
    ws.call(
        "agent.requestCapability",
        serde_json::json!({
            "authInfo": {
                "appName": app_name,
                "appDesc": app_desc,
                "appDomain": app_domain,
                "appUrl": app_url,
                "appIconPath": app_icon_path,
                "capabilities": caps,
            }
        }),
    )
    .await
}

pub async fn retrieve_capability(
    executor_url: String,
    request_id: String,
    rand: String,
) -> Result<String> {
    let ws = WsRpcClient::connect(&executor_url, "").await?;
    ws.call(
        "agent.generateJwt",
        serde_json::json!({
            "requestId": request_id,
            "rand": rand,
        }),
    )
    .await
}

// ── AgentClient ──

pub struct AgentClient {
    ws: Arc<WsRpcClient>,
}

impl AgentClient {
    pub fn new(ws: Arc<WsRpcClient>) -> Self {
        Self { ws }
    }

    pub async fn request_capability(
        &self,
        app_name: String,
        app_desc: String,
        app_domain: String,
        app_url: Option<String>,
        app_icon_path: Option<String>,
        capabilities: Option<Vec<Capability>>,
    ) -> Result<String> {
        let caps: Option<Vec<CapabilityInput>> =
            capabilities.map(|c| c.into_iter().map(CapabilityInput::from).collect());
        self.ws
            .call(
                "agent.requestCapability",
                serde_json::json!({
                    "authInfo": {
                        "appName": app_name,
                        "appDesc": app_desc,
                        "appDomain": app_domain,
                        "appUrl": app_url,
                        "appIconPath": app_icon_path,
                        "capabilities": caps,
                    }
                }),
            )
            .await
    }

    pub async fn retrieve_capability(
        &self,
        request_id: String,
        rand: String,
    ) -> Result<String> {
        self.ws
            .call(
                "agent.generateJwt",
                serde_json::json!({
                    "requestId": request_id,
                    "rand": rand,
                }),
            )
            .await
    }

    pub async fn me(&self) -> Result<Agent> {
        self.ws.call("agent.get", serde_json::json!({})).await
    }

    pub async fn status(&self) -> Result<AgentStatus> {
        self.ws.call("agent.status", serde_json::json!({})).await
    }

    pub async fn get_apps(&self) -> Result<Vec<Apps>> {
        self.ws.call("agent.getApps", serde_json::json!({})).await
    }

    pub async fn revoke_token(&self, request_id: String) -> Result<Vec<Apps>> {
        self.ws
            .call(
                "agent.revokeToken",
                serde_json::json!({ "token": request_id }),
            )
            .await
    }

    pub async fn remove_app(&self, request_id: String) -> Result<Vec<Apps>> {
        self.ws
            .call(
                "agent.removeApp",
                serde_json::json!({ "id": request_id }),
            )
            .await
    }

    pub async fn lock(&self, passphrase: String) -> Result<AgentStatus> {
        self.ws
            .call("agent.lock", serde_json::json!({ "passphrase": passphrase }))
            .await
    }

    pub async fn unlock(&self, passphrase: String, holochain: bool) -> Result<AgentStatus> {
        self.ws
            .call(
                "agent.unlock",
                serde_json::json!({ "passphrase": passphrase, "holochain": holochain }),
            )
            .await
    }

    pub async fn by_did(&self, did: String) -> Result<Option<Agent>> {
        self.ws
            .call("agent.byDid", serde_json::json!({ "did": did }))
            .await
    }

    pub async fn generate(&self, passphrase: String) -> Result<AgentStatus> {
        self.ws
            .call(
                "agent.generate",
                serde_json::json!({ "passphrase": passphrase }),
            )
            .await
    }

    pub async fn sign_message(&self, message: String) -> Result<AgentSignature> {
        self.ws
            .call("agent.sign", serde_json::json!({ "message": message }))
            .await
    }

    pub async fn is_locked(&self) -> Result<bool> {
        self.ws.call("agent.isLocked", serde_json::json!({})).await
    }

    pub async fn add_entanglement_proofs(
        &self,
        proofs: Vec<EntanglementProof>,
    ) -> Result<Vec<serde_json::Value>> {
        self.ws
            .call(
                "agent.addEntanglementProofs",
                serde_json::json!({ "proofs": proofs }),
            )
            .await
    }

    pub async fn delete_entanglement_proofs(
        &self,
        proofs: Vec<EntanglementProof>,
    ) -> Result<Vec<serde_json::Value>> {
        self.ws
            .call(
                "agent.deleteEntanglementProofs",
                serde_json::json!({ "proofs": proofs }),
            )
            .await
    }

    pub async fn entanglement_proof_pre_flight(
        &self,
        device_key: String,
        device_key_type: String,
    ) -> Result<Vec<serde_json::Value>> {
        self.ws
            .call(
                "agent.entanglementProofPreflight",
                serde_json::json!({
                    "deviceKey": device_key,
                    "deviceKeyType": device_key_type,
                }),
            )
            .await
    }
}

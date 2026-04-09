use std::sync::Arc;

use anyhow::Result;
use serde::Serialize;

use crate::types::*;
use crate::util;
use crate::ClientInfo;

// ── Request/Response types ──

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct AuthInfoInput {
    pub app_name: String,
    pub app_desc: String,
    pub app_domain: String,
    pub app_url: Option<String>,
    pub app_icon_path: Option<String>,
    pub capabilities: Option<Vec<CapabilityInput>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct RequestCapabilityBody {
    pub auth_info: AuthInfoInput,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct GenerateJwtBody {
    pub request_id: String,
    pub rand: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct GenerateBody {
    pub passphrase: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct LockBody {
    pub passphrase: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct UnlockBody {
    pub passphrase: String,
    pub holochain: Option<bool>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct SignMessageBody {
    pub message: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct EntanglementProofInputBody {
    pub device_key: String,
    pub device_key_type: String,
    pub device_key_signed_by_did: String,
    pub did_signed_by_device_key: Option<String>,
}

// ── Free functions (legacy API) ──

pub async fn request_capability(
    executor_url: String,
    app_name: String,
    app_desc: String,
    app_domain: String,
    app_url: Option<String>,
    app_icon_path: Option<String>,
    capabilities: Option<Vec<Capability>>,
) -> Result<String> {
    let body = RequestCapabilityBody {
        auth_info: AuthInfoInput {
            app_name,
            app_desc,
            app_domain,
            app_url,
            app_icon_path,
            capabilities: capabilities
                .map(|caps| caps.into_iter().map(CapabilityInput::from).collect()),
        },
    };
    util::post(&executor_url, "", "/agent/auth/request", &body).await
}

pub async fn retrieve_capability(
    executor_url: String,
    request_id: String,
    rand: String,
) -> Result<String> {
    let body = GenerateJwtBody { request_id, rand };
    util::post(&executor_url, "", "/agent/auth/jwt", &body).await
}

pub async fn me(executor_url: String, cap_token: String) -> Result<Agent> {
    util::get(&executor_url, &cap_token, "/agent").await
}

pub async fn get_apps(executor_url: String, cap_token: String) -> Result<Vec<Apps>> {
    util::get(&executor_url, &cap_token, "/agent/apps").await
}

pub async fn revoke_token(
    executor_url: String,
    cap_token: String,
    request_id: String,
) -> Result<Vec<Apps>> {
    util::delete(
        &executor_url,
        &cap_token,
        &format!("/agent/auth/token/{}", request_id),
    )
    .await
}

pub async fn remove_app(
    executor_url: String,
    cap_token: String,
    request_id: String,
) -> Result<Vec<Apps>> {
    util::delete(
        &executor_url,
        &cap_token,
        &format!("/agent/apps/{}", request_id),
    )
    .await
}

pub async fn status(executor_url: String, cap_token: String) -> Result<AgentStatus> {
    util::get(&executor_url, &cap_token, "/agent/status").await
}

pub async fn lock(
    executor_url: String,
    cap_token: String,
    passphrase: String,
) -> Result<AgentStatus> {
    let body = LockBody { passphrase };
    util::post(&executor_url, &cap_token, "/agent/lock", &body).await
}

pub async fn unlock(
    executor_url: String,
    cap_token: String,
    passphrase: String,
    holochain: bool,
) -> Result<AgentStatus> {
    let body = UnlockBody {
        passphrase,
        holochain: Some(holochain),
    };
    util::post(&executor_url, &cap_token, "/agent/unlock", &body).await
}

pub async fn by_did(executor_url: String, cap_token: String, did: String) -> Result<Option<Agent>> {
    util::get(&executor_url, &cap_token, &format!("/agent/by-did/{}", did)).await
}

pub async fn generate(
    executor_url: String,
    cap_token: String,
    passphrase: String,
) -> Result<AgentStatus> {
    let body = GenerateBody { passphrase };
    util::post(&executor_url, &cap_token, "/agent/generate", &body).await
}

pub async fn sign_message(
    executor_url: String,
    cap_token: String,
    message: String,
) -> Result<AgentSignature> {
    let body = SignMessageBody { message };
    util::post(&executor_url, &cap_token, "/agent/sign", &body).await
}

pub async fn add_entanglement_proofs(
    executor_url: String,
    cap_token: String,
    proofs: Vec<EntanglementProof>,
) -> Result<Vec<serde_json::Value>> {
    util::post(
        &executor_url,
        &cap_token,
        "/agent/entanglement-proofs",
        &proofs,
    )
    .await
}

pub async fn delete_entanglement_proofs(
    executor_url: String,
    cap_token: String,
    proofs: Vec<EntanglementProof>,
) -> Result<Vec<serde_json::Value>> {
    util::delete_with_body(
        &executor_url,
        &cap_token,
        "/agent/entanglement-proofs",
        &proofs,
    )
    .await
}

pub async fn entanglement_proof_pre_flight(
    executor_url: String,
    cap_token: String,
    device_key: String,
    device_key_type: String,
) -> Result<Vec<serde_json::Value>> {
    let body = vec![EntanglementProofInputBody {
        device_key,
        device_key_type,
        device_key_signed_by_did: String::new(),
        did_signed_by_device_key: None,
    }];
    util::post(
        &executor_url,
        &cap_token,
        "/agent/entanglement-proofs?preflight=true",
        &body,
    )
    .await
}

// ── AgentClient ──

pub struct AgentClient {
    info: Arc<ClientInfo>,
}

impl AgentClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
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
        request_capability(
            self.info.executor_url.clone(),
            app_name,
            app_desc,
            app_domain,
            app_url,
            app_icon_path,
            capabilities,
        )
        .await
    }

    pub async fn retrieve_capability(&self, request_id: String, rand: String) -> Result<String> {
        retrieve_capability(self.info.executor_url.clone(), request_id, rand).await
    }

    pub async fn me(&self) -> Result<Agent> {
        me(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn status(&self) -> Result<AgentStatus> {
        status(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn get_apps(&self) -> Result<Vec<Apps>> {
        get_apps(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn lock(&self, passphrase: String) -> Result<AgentStatus> {
        lock(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            passphrase,
        )
        .await
    }

    pub async fn unlock(&self, passphrase: String, holochain: bool) -> Result<AgentStatus> {
        unlock(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            passphrase,
            holochain,
        )
        .await
    }

    pub async fn by_did(&self, did: String) -> Result<Option<Agent>> {
        by_did(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            did,
        )
        .await
    }

    pub async fn generate(&self, passphrase: String) -> Result<AgentStatus> {
        generate(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            passphrase,
        )
        .await
    }

    pub async fn sign_message(&self, message: String) -> Result<AgentSignature> {
        sign_message(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            message,
        )
        .await
    }

    pub async fn add_entanglement_proofs(
        &self,
        proofs: Vec<EntanglementProof>,
    ) -> Result<Vec<serde_json::Value>> {
        add_entanglement_proofs(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            proofs,
        )
        .await
    }

    pub async fn delete_entanglement_proofs(
        &self,
        proofs: Vec<EntanglementProof>,
    ) -> Result<Vec<serde_json::Value>> {
        delete_entanglement_proofs(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            proofs,
        )
        .await
    }

    pub async fn entanglement_proof_pre_flight(
        &self,
        device_key: String,
        device_key_type: String,
    ) -> Result<Vec<serde_json::Value>> {
        entanglement_proof_pre_flight(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            device_key,
            device_key_type,
        )
        .await
    }
}

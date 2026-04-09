use serde::{Deserialize, Serialize};

use crate::types::{
    AITask, Agent, AgentStatus, Apps, AuthInfoInput, DecoratedLinkExpression, InteractionCall,
    InteractionMeta, LanguageMeta, LanguageMetaInput, Link, LinkExpression, LinkInput,
    LinkMutations, LinkQuery, LinkStatus, Model, ModelType, Notification, OnlineAgent,
    PerspectiveHandle, TriggeredNotification,
};

// Re-export for use in handler files
pub use crate::agent::capabilities::user_email_from_token;

// ── Agent ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GenerateAgentRequest {
    pub passphrase: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LockAgentRequest {
    pub passphrase: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UnlockAgentRequest {
    pub passphrase: String,
    pub holochain: Option<bool>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SignMessageRequest {
    pub message: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UpdateProfileRequest {
    pub dm_language: Option<String>,
    pub public_perspective: Option<PerspectiveInput>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RequestCapabilityRequest {
    pub auth_info: AuthInfoInput,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PermitCapabilityRequest {
    pub auth: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GenerateJwtRequest {
    pub rand: String,
    pub request_id: String,
}

// ── Entanglement ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EntanglementProofInput {
    pub device_key: String,
    pub device_key_type: String,
    pub device_key_signed_by_did: String,
    pub did_signed_by_device_key: Option<String>,
}

// ── Perspectives & Links ──

#[derive(Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveInput {
    pub links: Vec<LinkInput>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreatePerspectiveRequest {
    pub name: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UpdatePerspectiveRequest {
    pub name: String,
}

/// Unified link mutation request (harmonised endpoint).
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkMutationRequest {
    pub additions: Option<Vec<LinkInput>>,
    pub removals: Option<Vec<LinkInput>>,
    pub updates: Option<Vec<LinkUpdateInput>>,
    pub status: Option<String>,
    pub batch_id: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkUpdateInput {
    pub old_link: LinkInput,
    pub new_link: LinkInput,
}

/// Unified link mutation response.
#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkMutationResponse {
    pub additions: Vec<DecoratedLinkExpression>,
    pub removals: Vec<DecoratedLinkExpression>,
    pub updates: Vec<DecoratedLinkExpression>,
}

/// Unified query request (harmonised: engine + query in one).
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct QueryRequest {
    pub engine: String, // "prolog" | "surreal"
    pub query: String,
}

// ── Neighbourhoods ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JoinNeighbourhoodRequest {
    pub url: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PublishNeighbourhoodRequest {
    pub perspective_uuid: String,
    pub link_language: String,
    pub meta: PerspectiveInput,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BroadcastRequest {
    pub payload: PerspectiveInput,
    pub signed: Option<bool>,
    pub loopback: Option<bool>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SignalRequest {
    pub remote_agent_did: String,
    pub payload: PerspectiveInput,
    pub signed: Option<bool>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetOnlineStatusRequest {
    pub status: PerspectiveInput,
    pub signed: Option<bool>,
}

// ── Expressions ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateExpressionRequest {
    pub content: String,
    pub language_address: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExpressionManyRequest {
    pub urls: Vec<String>,
}

// ── Languages ──

// ── Languages ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PublishLanguageRequest {
    pub language_path: String,
    pub language_meta: LanguageMetaInput,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ApplyTemplateRequest {
    pub source_language_hash: String,
    pub template_data: String,
}

// ── Runtime ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkLanguageTemplatesRequest {
    pub addresses: Vec<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetStatusRequest {
    pub status: serde_json::Value,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OpenLinkRequest {
    pub url: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExportRequest {
    #[serde(rename = "type")]
    pub export_type: String, // "db" | "perspective"
    pub file_path: String,
    pub perspective_uuid: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ImportRequest {
    #[serde(rename = "type")]
    pub import_type: String,
    pub file_path: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AddAgentInfosRequest {
    pub agent_infos: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FriendSendMessageRequest {
    pub message: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FriendsListRequest {
    pub dids: Vec<String>,
}

// ── Users ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateUserRequest {
    pub email: String,
    pub password: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LoginUserRequest {
    pub email: String,
    pub password: String,
    pub app_name: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VerifyEmailRequest {
    pub email: String,
    pub code: String,
    pub verification_type: Option<String>,
    pub app_name: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetMultiUserRequest {
    pub enabled: bool,
}

// ── AI ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PromptRequest {
    pub task_id: String,
    pub prompt: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmbedRequest {
    pub model_id: String,
    pub text: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetDefaultModelRequest {
    pub model_type: ModelType,
}

// ── Notifications ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NotificationInput {
    pub description: String,
    pub app_name: String,
    pub app_url: String,
    pub app_icon_path: Option<String>,
    pub trigger: String,
    pub perspective_ids: Vec<String>,
    pub webhook_url: String,
    pub webhook_auth: String,
}

// ── SDNA / Commands / Subjects ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AddSdnaRequest {
    pub name: String,
    pub sdna_code: Option<String>,
    pub sdna_type: String,
    pub shacl_json: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExecuteCommandsRequest {
    pub commands: Vec<serde_json::Value>,
    pub expression: serde_json::Value,
}

// ── Hosting ──

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct HostingInfoResponse {
    pub user_info: Option<serde_json::Value>,
    pub rates: Option<serde_json::Value>,
    pub version: Option<serde_json::Value>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct HostingWalletResponse {
    pub balance: Option<serde_json::Value>,
    pub pubkey: Option<String>,
}

// ── Verify Signature ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VerifySignatureRequest {
    pub did: String,
    pub data: String,
    pub signed_data: String,
}

// ── Dev ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailTestRequest {
    pub action: String,
    pub to: Option<String>,
    pub email: Option<String>,
    pub expiry_seconds: Option<i64>,
}

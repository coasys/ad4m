use serde::{Deserialize, Serialize};

use crate::types::{
    Agent, AgentStatus, Apps, PerspectiveHandle, Perspective,
};
use crate::types::{
    DecoratedLinkExpression, Link, Notification, TriggeredNotification,
    AITask, Model, ModelType,
};

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

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfoInput {
    pub app_desc: String,
    pub app_domain: String,
    pub app_icon_path: Option<String>,
    pub app_name: String,
    pub app_url: Option<String>,
    pub capabilities: Option<Vec<CapabilityInput>>,
    pub user_did: Option<String>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CapabilityInput {
    pub can: Vec<String>,
    pub with: ResourceInput,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ResourceInput {
    pub domain: String,
    pub pointers: Vec<String>,
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

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveInput {
    pub links: Vec<LinkInput>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkInput {
    pub source: String,
    pub target: String,
    pub predicate: Option<String>,
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
    pub removals: Option<Vec<LinkExpressionInput>>,
    pub updates: Option<Vec<LinkUpdateInput>>,
    pub status: Option<String>,
    pub batch_id: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkExpressionInput {
    pub author: String,
    pub timestamp: String,
    pub data: LinkInput,
    pub proof: Option<ExpressionProofInput>,
    pub status: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkUpdateInput {
    pub old: LinkExpressionInput,
    pub new: LinkInput,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExpressionProofInput {
    pub key: Option<String>,
    pub signature: Option<String>,
    pub valid: Option<bool>,
    pub invalid: Option<bool>,
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
    pub uuid: String,
    pub link_language: String,
    pub meta: Option<PerspectiveInput>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SendBroadcastRequest {
    pub perspective: PerspectiveInput,
    pub signed: Option<bool>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SendSignalRequest {
    pub recipient: String,
    pub payload: PerspectiveInput,
    pub signed: Option<bool>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetOnlineStatusRequest {
    pub perspective: PerspectiveInput,
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

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InteractRequest {
    pub interaction_call: String,
}

// ── Runtime ──

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
    pub password: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LoginUserRequest {
    pub email: String,
    pub password: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VerifyEmailRequest {
    pub email: String,
    pub code: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetMultiUserRequest {
    pub enabled: bool,
}

// ── AI ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AddModelRequest {
    pub model: ModelInput,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelInput {
    pub name: String,
    pub api_base_url: Option<String>,
    pub api_key: Option<String>,
    pub api_type: Option<String>,
    pub model_type: Option<String>,
    pub token_limit: Option<i32>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PromptRequest {
    pub model_id: Option<String>,
    pub prompt: String,
    pub system: Option<String>,
    pub examples: Option<Vec<PromptExample>>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PromptExample {
    pub input: String,
    pub output: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmbedRequest {
    pub model_id: Option<String>,
    pub text: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TaskRequest {
    pub action: String, // "add" | "update" | "remove"
    pub task: Option<AITaskInput>,
    pub task_id: Option<String>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AITaskInput {
    pub name: Option<String>,
    pub model_id: Option<String>,
    pub description: Option<String>,
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
    pub sdna_code: String,
    pub sdna_type: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExecuteCommandsRequest {
    pub commands: Vec<serde_json::Value>,
    pub expression: serde_json::Value,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateSubjectRequest {
    pub subject_class: String,
    pub expression_address: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetSubjectDataRequest {
    pub subject_class: String,
    pub expression_address: String,
}

// ── Link Language Templates ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkLanguageTemplatesRequest {
    pub addresses: Vec<String>,
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

// ── Dev ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmailTestRequest {
    pub action: String, // "send" | "enable" | "disable" | "get-code" | "clear" | "set-expiry"
    pub to: Option<String>,
    pub email: Option<String>,
    pub expiry_seconds: Option<i64>,
}

// ── Verify Signature ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VerifySignatureRequest {
    pub did: String,
    pub data: String,
    pub signed_data: String,
}

// ── Batch ──

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CommitBatchRequest {
    pub additions: Option<serde_json::Value>,
    pub removals: Option<serde_json::Value>,
}

// ── SSE event wrapper ──

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SseEvent<T: Serialize> {
    #[serde(rename = "type")]
    pub event_type: String,
    pub data: T,
}

use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::types::{
    AuthInfoInput, DecoratedLinkExpression, LanguageMetaInput, LinkExpression, LinkExpressionInput,
    LinkInput, LinkMutations, ModelType,
};

// Re-export for use in handler files
pub use crate::agent::capabilities::user_email_from_token;

// ── Agent ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GenerateAgentRequest {
    pub passphrase: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LockAgentRequest {
    pub passphrase: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UnlockAgentRequest {
    pub passphrase: String,
    pub holochain: Option<bool>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SignMessageRequest {
    pub message: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UpdateProfileRequest {
    pub dm_language: Option<String>,
    pub public_perspective: Option<PublicPerspectiveInput>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PublicPerspectiveInput {
    pub links: Vec<LinkExpressionInput>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RequestCapabilityRequest {
    pub auth_info: AuthInfoInput,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PermitCapabilityRequest {
    pub auth: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GenerateJwtRequest {
    pub rand: String,
    pub request_id: String,
}

// ── Entanglement ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct EntanglementProofInput {
    pub device_key: String,
    pub device_key_type: String,
    pub device_key_signed_by_did: String,
    pub did_signed_by_device_key: Option<String>,
}

// ── Perspectives & Links ──

#[derive(Deserialize, Clone, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PerspectiveInput {
    pub links: Vec<LinkInput>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreatePerspectiveRequest {
    pub name: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UpdatePerspectiveRequest {
    pub name: String,
}

/// Unified link mutation request (harmonised endpoint).
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LinkMutationRequest {
    pub additions: Option<Vec<LinkInput>>,
    pub removals: Option<Vec<LinkInput>>,
    pub updates: Option<Vec<LinkUpdateInput>>,
    pub status: Option<String>,
    pub batch_id: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LinkUpdateInput {
    pub old_link: LinkInput,
    pub new_link: LinkInput,
}

/// Unified link mutation response.
#[derive(Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LinkMutationResponse {
    pub additions: Vec<DecoratedLinkExpression>,
    pub removals: Vec<DecoratedLinkExpression>,
    pub updates: Vec<DecoratedLinkExpression>,
}

/// Unified query request (harmonised: engine + query in one).
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct QueryRequest {
    pub engine: String, // "prolog" | "surreal"
    pub query: String,
}

// ── Neighbourhoods ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct JoinNeighbourhoodRequest {
    pub url: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PublishNeighbourhoodRequest {
    #[serde(alias = "perspectiveUUID")]
    pub perspective_uuid: String,
    pub link_language: String,
    pub meta: crate::types::Perspective,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BroadcastRequest {
    pub payload: PerspectiveInput,
    pub signed: Option<bool>,
    pub loopback: Option<bool>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SignalRequest {
    pub remote_agent_did: String,
    pub payload: PerspectiveInput,
    pub signed: Option<bool>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetOnlineStatusRequest {
    pub status: PerspectiveInput,
    pub signed: Option<bool>,
}

// ── Expressions ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateExpressionRequest {
    pub content: String,
    pub language_address: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ExpressionManyRequest {
    pub urls: Vec<String>,
}

// ── Languages ──

// ── Languages ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PublishLanguageRequest {
    pub language_path: String,
    pub language_meta: LanguageMetaInput,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ApplyTemplateRequest {
    pub source_language_hash: String,
    pub template_data: String,
}

// ── Runtime ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LinkLanguageTemplatesRequest {
    pub addresses: Vec<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetStatusRequest {
    #[ts(type = "any")]
    pub status: serde_json::Value,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct OpenLinkRequest {
    pub url: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ExportRequest {
    #[serde(rename = "type")]
    pub export_type: String, // "db" | "perspective"
    pub file_path: String,
    pub perspective_uuid: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ImportRequest {
    #[serde(rename = "type")]
    pub import_type: String,
    pub file_path: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddAgentInfosRequest {
    pub agent_infos: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct FriendSendMessageRequest {
    pub message: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct FriendsListRequest {
    pub dids: Vec<String>,
}

// ── Users ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateUserRequest {
    pub email: String,
    pub password: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LoginUserRequest {
    pub email: String,
    pub password: String,
    pub app_name: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct VerifyEmailRequest {
    pub email: String,
    pub code: String,
    pub verification_type: Option<String>,
    pub app_name: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetMultiUserRequest {
    pub enabled: bool,
}

// ── AI ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PromptRequest {
    pub task_id: String,
    pub prompt: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct EmbedRequest {
    pub model_id: String,
    pub text: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetDefaultModelRequest {
    pub model_type: ModelType,
}

// ── Notifications ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
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

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddSdnaRequest {
    pub name: String,
    pub sdna_code: Option<String>,
    pub sdna_type: String,
    pub shacl_json: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ExecuteCommandsRequest {
    #[ts(type = "any[]")]
    pub commands: Vec<serde_json::Value>,
    #[ts(type = "any")]
    pub expression: serde_json::Value,
}

// ── Hosting ──

#[derive(Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostingInfoResponse {
    #[ts(type = "any | null")]
    pub user_info: Option<serde_json::Value>,
    #[ts(type = "any | null")]
    pub rates: Option<serde_json::Value>,
    #[ts(type = "any | null")]
    pub version: Option<serde_json::Value>,
}

#[derive(Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostingWalletResponse {
    #[ts(type = "any | null")]
    pub balance: Option<serde_json::Value>,
    pub pubkey: Option<String>,
}

// ── Verify Signature ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct VerifySignatureRequest {
    pub did: String,
    pub data: String,
    pub signed_data: String,
}

// ── Dev ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct EmailTestRequest {
    pub action: String,
    pub to: Option<String>,
    pub email: Option<String>,
    pub expiry_seconds: Option<i64>,
}

// ── Additional Perspective Types ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddLinkRequest {
    pub link: LinkInput,
    pub status: Option<String>,
    pub batch_id: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddLinksBulkRequest {
    pub links: Vec<LinkInput>,
    pub status: Option<String>,
    pub batch_id: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemoveLinksBulkRequest {
    pub links: Vec<LinkExpressionInput>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LinkMutationsRequest {
    pub mutations: LinkMutations,
    pub status: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddLinkExpressionRequest {
    pub link: LinkExpression,
    pub status: Option<String>,
    pub batch_id: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UpdateLinkRequest {
    pub old_link: LinkExpressionInput,
    pub new_link: LinkInput,
    pub batch_id: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemoveLinkRequest {
    pub link: LinkExpressionInput,
    pub batch_id: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CommitBatchRequest {
    pub batch_id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SubscribeQueryRequest {
    pub query: String,
}

#[derive(Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SubscribeQueryResponse {
    pub subscription_id: String,
    pub result: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct KeepAliveQueryRequest {
    pub subscription_id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DisposeQueryRequest {
    pub subscription_id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateSubjectRequest {
    pub subject_class: String,
    pub expression_address: String,
    pub initial_values: Option<String>,
    pub batch_id: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetSubjectDataRequest {
    pub subject_class: String,
    pub expression_address: String,
}

// ── Agent Import ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ImportAgentRequest {
    pub did: String,
    pub did_document: String,
    pub keystore: String,
    pub passphrase: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct EntanglementProofPreflightRequest {
    pub device_key: String,
    pub device_key_type: String,
}

// ── Trusted Agents ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TrustedAgentsRequest {
    pub agents: Vec<String>,
}

// ── Language Settings ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct WriteSettingsRequest {
    pub settings: String,
}

// ── Free Hosting ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetFreeHostingEnabledRequest {
    pub enabled: bool,
}

// ── Hosting wallet ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetHotWalletAddressRequest {
    pub address: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RequestPaymentRequest {
    #[serde(rename = "amountHOT")]
    pub amount_hot: String,
}

// ── Users: request verification ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RequestVerificationRequest {
    pub email: String,
    #[ts(type = "any | null")]
    pub app_info: Option<serde_json::Value>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use ts_rs::TS;

    #[test]
    fn export_ts_bindings() {
        // ts-rs auto-exports when #[ts(export)] is used and tests run.
        // Verify key types produce valid TypeScript declarations.
        let cfg = ts_rs::Config::default();
        
        let decl = GenerateAgentRequest::decl(&cfg);
        assert!(decl.contains("passphrase"), "GenerateAgentRequest: {}", decl);
        
        let decl = UpdateProfileRequest::decl(&cfg);
        assert!(decl.contains("publicPerspective") || decl.contains("dmLanguage"), "UpdateProfileRequest: {}", decl);
        
        let decl = AddLinkRequest::decl(&cfg);
        assert!(decl.contains("link"), "AddLinkRequest: {}", decl);
        
        let decl = LinkMutationRequest::decl(&cfg);
        assert!(decl.contains("additions"), "LinkMutationRequest: {}", decl);
        
        let decl = CreateExpressionRequest::decl(&cfg);
        assert!(decl.contains("content"), "CreateExpressionRequest: {}", decl);
        
        // Print sample declarations for verification
        println!("\n=== Generated TypeScript Types ===");
        println!("{}", GenerateAgentRequest::decl(&cfg));
        println!("{}", UpdateProfileRequest::decl(&cfg));
        println!("{}", PublicPerspectiveInput::decl(&cfg));
        println!("{}", AddLinkRequest::decl(&cfg));
        println!("{}", LinkMutationRequest::decl(&cfg));
        println!("{}", CreateExpressionRequest::decl(&cfg));
        println!("=================================\n");
    }
}

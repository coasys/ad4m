use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Link {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ExpressionProof {
    pub invalid: Option<bool>,
    pub key: Option<String>,
    pub signature: Option<String>,
    pub valid: Option<bool>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkExpression {
    pub author: String,
    pub data: Link,
    pub proof: ExpressionProof,
    pub timestamp: String,
    pub status: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PerspectiveExpression {
    pub author: String,
    pub data: Perspective,
    pub proof: ExpressionProof,
    pub timestamp: String,
}

#[derive(Debug)]
pub struct Capability {
    pub can: Vec<String>,
    pub with: Resource,
}

#[derive(Debug)]
pub struct Resource {
    pub domain: String,
    pub pointers: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

pub struct SentPerspectiveMessage {
    pub recipient: String,
    pub message: PerspectiveExpression,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Agent {
    pub did: String,
    pub direct_message_language: Option<String>,
    pub perspective: Option<Perspective>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AgentStatus {
    pub did: Option<String>,
    pub did_document: Option<String>,
    pub error: Option<String>,
    pub is_initialized: bool,
    pub is_unlocked: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AgentSignature {
    pub public_key: Option<String>,
    pub signature: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Apps {
    pub auth: serde_json::Value,
    pub request_id: String,
    pub revoked: Option<bool>,
    pub token: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EntanglementProof {
    pub device_key: String,
    pub device_key_type: String,
    pub device_key_signed_by_did: Option<String>,
    pub did_signed_by_device_key: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Neighbourhood {
    pub link_language: String,
    pub meta: Perspective,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct NeighbourhoodExpression {
    pub author: String,
    pub data: Neighbourhood,
    pub proof: ExpressionProof,
    pub timestamp: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveHandle {
    pub name: Option<String>,
    pub neighbourhood: Option<NeighbourhoodExpression>,
    pub shared_url: Option<String>,
    pub uuid: String,
    pub state: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct RuntimeInfo {
    pub ad4m_executor_version: String,
    pub is_initialized: bool,
    pub is_unlocked: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LanguageMeta {
    pub address: String,
    pub author: Option<String>,
    pub description: Option<String>,
    pub name: String,
    pub possible_template_params: Option<Vec<String>>,
    pub template_applied_params: Option<String>,
    pub template_source_language_address: Option<String>,
    pub source_code_link: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LanguageHandle {
    pub address: String,
    pub icon: Option<serde_json::Value>,
    pub name: String,
    pub settings: Option<String>,
    pub settings_icon: Option<serde_json::Value>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LanguageRef {
    pub address: String,
    pub name: String,
}

// Input types for REST requests

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkInput {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveInput {
    pub links: Vec<LinkInput>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkExpressionInput {
    pub author: String,
    pub data: LinkInput,
    pub proof: ExpressionProofInput,
    pub timestamp: String,
    pub status: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ExpressionProofInput {
    pub invalid: Option<bool>,
    pub key: Option<String>,
    pub signature: Option<String>,
    pub valid: Option<bool>,
}

impl From<LinkExpression> for LinkExpressionInput {
    fn from(link: LinkExpression) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: LinkInput {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: ExpressionProofInput {
                key: link.proof.key,
                signature: link.proof.signature,
                invalid: link.proof.invalid,
                valid: link.proof.valid,
            },
            status: link.status,
        }
    }
}

impl From<Perspective> for PerspectiveInput {
    fn from(perspective: Perspective) -> Self {
        Self {
            links: perspective
                .links
                .into_iter()
                .map(|l| LinkInput {
                    predicate: l.data.predicate,
                    source: l.data.source,
                    target: l.data.target,
                })
                .collect(),
        }
    }
}

// Capability input types for REST requests

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CapabilityInput {
    pub can: Vec<String>,
    pub with: ResourceInput,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ResourceInput {
    pub domain: String,
    pub pointers: Vec<String>,
}

impl From<Capability> for CapabilityInput {
    fn from(cap: Capability) -> Self {
        Self {
            can: cap.can,
            with: cap.with.into(),
        }
    }
}

impl From<Resource> for ResourceInput {
    fn from(res: Resource) -> Self {
        Self {
            domain: res.domain,
            pointers: res.pointers,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct OnlineAgent {
    pub did: String,
    pub status: Option<serde_json::Value>,
}

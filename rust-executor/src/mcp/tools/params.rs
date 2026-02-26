//! MCP Tool parameter types
//!
//! All parameter structs used by MCP tool handlers.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

// Tool Parameter Types
// ============================================================================

/// Parameters for listing perspectives
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ListPerspectivesParams {}

/// Parameters for listing subject classes in a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ListSubjectClassesParams {
    /// Perspective UUID
    pub perspective_id: String,
}

/// Parameters for querying subject instances
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct QuerySubjectsParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name (e.g., "Message", "Channel", "Todo")
    pub class_name: String,
    /// Optional Prolog query for filtering
    pub query: Option<String>,
}

/// Parameters for getting subject data
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectDataParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address (subject instance ID)
    pub expression_address: String,
}

/// Parameters for creating a subject
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct CreateSubjectParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address for the new subject
    pub expression_address: String,
    /// Initial property values as JSON string
    pub initial_values: Option<String>,
}

/// Parameters for executing commands on a subject
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ExecuteCommandsParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Commands to execute as JSON string (array of Command objects)
    pub commands: String,
    /// Expression address (subject instance)
    pub expression_address: String,
    /// Optional parameters as JSON string
    pub parameters: Option<String>,
}

/// Parameters for running a Prolog query
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InferParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Prolog query string
    pub query: String,
}

// ============================================================================
// Authentication Parameter Types
// ============================================================================

/// Parameters for email/password login (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct LoginEmailParams {
    /// User email address
    pub email: String,
    /// User password
    pub password: String,
}

/// Parameters for requesting a capability token (local connect flow)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RequestCapabilityParams {
    /// Application name requesting access
    pub app_name: String,
    /// Application description
    pub app_desc: String,
    /// Optional application domain
    #[serde(default)]
    pub app_domain: Option<String>,
    /// Optional application URL
    #[serde(default)]
    pub app_url: Option<String>,
}

/// Parameters for generating a JWT from a capability request
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GenerateJwtParams {
    /// Request ID returned from request_capability
    pub request_id: String,
    /// 6-digit code from the executor log
    pub code: String,
}

/// Parameters for user signup (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SignupParams {
    /// User email address
    pub email: String,
    /// User password
    pub password: String,
}

/// Parameters for requesting a login verification code (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RequestLoginVerificationParams {
    /// User email address
    pub email: String,
}

/// Parameters for verifying an email code (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct VerifyEmailCodeParams {
    /// User email address
    pub email: String,
    /// 6-digit verification code
    pub code: String,
    /// Type: "signup" or "login"
    pub verification_type: String,
}

/// Parameters for checking authentication status (no params needed)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AuthStatusParams {}

// ============================================================================
// Link & Perspective Parameter Types
// ============================================================================

/// Parameters for adding a link to a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddLinkParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Link source URI
    pub source: String,
    /// Link predicate URI
    pub predicate: String,
    /// Link target URI
    pub target: String,
}

/// Parameters for querying links in a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct QueryLinksParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Optional source URI filter
    pub source: Option<String>,
    /// Optional predicate URI filter
    pub predicate: Option<String>,
    /// Optional target URI filter
    pub target: Option<String>,
}

/// Parameters for adding SDNA (subject class definition) to a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddModelParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// SHACL shape definition as JSON string
    pub shacl_json: String,
}

/// Parameters for adding a flow definition
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddFlowParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// SHACL flow definition as JSON string
    pub shacl_json: String,
}

/// Parameters for listing flows
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetFlowsParams {
    /// Perspective UUID
    pub perspective_id: String,
}

/// Parameters for flow operations on an expression
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct FlowExprParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// Expression address
    pub expression_address: String,
}

/// Parameters for running a flow action
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct FlowRunActionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// Expression address
    pub expression_address: String,
    /// Action name to execute
    pub action_name: String,
}

/// Parameters for creating a new perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddPerspectiveParams {
    /// Name for the new perspective
    pub name: String,
}

/// Parameters for setting a property on a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetSubjectPropertyParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name (e.g., "Channel", "Message")
    pub class_name: String,
    /// Expression address of the subject instance
    pub expression_address: String,
    /// Property name to set (e.g., "name", "body")
    pub property_name: String,
    /// Value to set (will be wrapped as literal if needed)
    pub value: String,
}

/// Parameters for getting a collection from a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance
    pub expression_address: String,
    /// Collection name (e.g., "messages", "members")
    pub collection_name: String,
}

/// Parameters for adding an item to a subject collection
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddToCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance (parent)
    pub expression_address: String,
    /// Collection name
    pub collection_name: String,
    /// Expression address of the item to add
    pub item_address: String,
}

/// Parameters for removing an item from a subject collection
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RemoveFromCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance (parent)
    pub expression_address: String,
    /// Collection name
    pub collection_name: String,
    /// Expression address of the item to remove
    pub item_address: String,
}

/// Parameters for getting children of a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectChildrenParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name of the parent (optional)
    #[serde(default)]
    pub class_name: Option<String>,
    /// Expression address of the parent subject
    pub expression_address: String,
    /// Optional: filter children to only this class name
    pub child_class_name: Option<String>,
}

/// Parameters for deleting a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct DeleteSubjectParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject to delete
    pub expression_address: String,
}

// ============================================================================
// Subscription Parameter Types
// ============================================================================

/// Parameters for subscribing to model changes (e.g., new messages in a channel)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SubscribeToModelParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name to watch (e.g., "Message")
    pub class_name: String,
    /// Parent expression address to scope the subscription (e.g., a channel address).
    /// If provided, only watches for new instances that are children of this parent.
    pub parent_address: Option<String>,
    /// Entry type URI to filter by (e.g., "flux://has_message").
    /// Use this instead of hardcoded class-to-type mappings.
    /// If neither parent_address nor entry_type is provided, watches all new links.
    pub entry_type: Option<String>,
}

// ============================================================================
// Agent Profile Parameter Types
// ============================================================================

/// Parameters for getting the agent's public profile
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetAgentProfileParams {}

/// Parameters for setting the agent's profile
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetAgentProfileParams {
    /// Display username
    pub username: Option<String>,
    /// Given (first) name
    pub given_name: Option<String>,
    /// Family (last) name
    pub family_name: Option<String>,
    /// Email address
    pub email: Option<String>,
    /// Bio/description text
    pub bio: Option<String>,
}

/// Parameters for setting the agent's profile picture
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetAgentProfilePictureParams {
    /// Base64-encoded image data (raw base64, NOT a data URI)
    pub image_base64: String,
    /// Image MIME type (e.g. "image/png", "image/jpeg"). Defaults to "image/png"
    pub mime_type: Option<String>,
}

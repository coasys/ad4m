use juniper::{GraphQLInputObject, GraphQLObject, GraphQLScalarValue};

#[derive(GraphQLObject, Default)]
pub struct Agent {
    did: String,
    #[graphql(name = "directMessageLanguage")]
    direct_message_language: Option<String>,
    perspective: Option<Perspective>,
}

#[derive(GraphQLObject, Default)]
pub struct AgentSignature {
    public_key: String,
    signature: String,
}

#[derive(GraphQLObject, Default)]
pub struct AgentStatus {
    did: Option<String>,
    did_document: Option<String>,
    error: Option<String>,
    is_initialized: bool,
    is_unlocked: bool,
}

#[derive(GraphQLObject, Default)]
pub struct Apps {
    auth: AuthInfo,
    request_id: String,
    revoked: Option<bool>,
    token: String,
}

#[derive(GraphQLObject, Default)]
pub struct AuthInfo {
    app_desc: String,
    app_icon_path: Option<String>,
    app_name: String,
    app_url: String,
    capabilities: Vec<Capability>,
}

#[derive(GraphQLInputObject, Default)]
pub struct AuthInfoInput {
    #[graphql(name = "appDesc")]
    app_desc: String,
    #[graphql(name = "appDomain")]
    app_domain: String,
    #[graphql(name = "appIconPath")]
    app_icon_path: Option<String>,
    #[graphql(name = "appName")]
    app_name: String,
    #[graphql(name = "appUrl")]
    app_url: Option<String>,
    #[graphql(name = "capabilities")]
    capabilities: Vec<CapabilityInput>,
}

#[derive(GraphQLObject, Default)]
pub struct Capability {
    can: Vec<String>,
    with: Resource,
}

#[derive(GraphQLInputObject, Default)]
pub struct CapabilityInput {
    can: Vec<String>,
    with: ResourceInput,
}

#[derive(GraphQLScalarValue)]
#[graphql(transparent)]
// The javascript `Date` as string. pub struct represents date and time as the ISO Date string.
pub struct DateTime(chrono::DateTime<chrono::Utc>);

#[derive(GraphQLObject, Default)]
pub struct EntanglementProof {
    #[graphql(name = "deviceKey")]
    device_key: String,
    #[graphql(name = "deviceKeySignedByDid")]
    device_key_signed_by_did: String,
    #[graphql(name = "deviceKeyType")]
    device_key_type: String,
    #[graphql(name = "did")]
    did: String,
    #[graphql(name = "didSignedByDeviceKey")]
    did_signed_by_device_key: Option<String>,
    #[graphql(name = "didSigningKeyId")]
    did_signing_key_id: String,
}

#[derive(GraphQLInputObject, Default)]
pub struct EntanglementProofInput {
    #[graphql(name = "deviceKey")]
    device_key: String,
    #[graphql(name = "deviceKeySignedByDid")]
    device_key_signed_by_did: String,
    #[graphql(name = "deviceKeyType")]
    device_key_type: String,
    #[graphql(name = "did")]
    did: String,
    #[graphql(name = "didSignedByDeviceKey")]
    did_signed_by_device_key: String,
    #[graphql(name = "didSigningKeyId")]
    did_signing_key_id: String,
}

#[derive(GraphQLObject, Default)]
pub struct ExceptionInfo {
    addon: Option<String>,
    message: String,
    title: String,
    r#type: f64,
}

#[derive(GraphQLObject, Default)]
pub struct ExpressionProof {
    invalid: Option<bool>,
    key: Option<String>,
    signature: Option<String>,
    valid: Option<bool>,
}

#[derive(GraphQLInputObject, Default)]
pub struct ExpressionProofInput {
    invalid: Option<bool>,
    key: Option<String>,
    signature: Option<String>,
    valid: Option<bool>,
}

#[derive(GraphQLObject, Default)]
pub struct ExpressionRendered {
    pub author: String,
    pub data: String,
    pub icon: Icon,
    pub language: LanguageRef,
    pub proof: ExpressionProof,
    pub timestamp: String,
}

#[derive(GraphQLObject, Default)]
pub struct Icon {
    pub code: String,
}

#[derive(GraphQLInputObject, Default)]
pub struct InteractionCall {
    pub name: String,
    pub parameters_stringified: String,
}

#[derive(GraphQLObject, Default)]
pub struct InteractionMeta {
    pub label: Option<String>,
    pub name: String,
    pub parameters: Vec<InteractionParameter>,
}

#[derive(GraphQLObject, Default)]
pub struct InteractionParameter {
    pub name: String,
    pub type_: String,
}

#[derive(GraphQLObject, Default)]
pub struct LanguageHandle {
    pub address: String,
    pub constructor_icon: Option<Icon>,
    pub icon: Option<Icon>,
    pub name: String,
    pub settings: Option<String>,
    pub settings_icon: Option<Icon>,
}

#[derive(GraphQLObject, Default)]
pub struct LanguageMeta {
    pub address: String,
    pub author: String,
    pub description: Option<String>,
    pub name: String,
    pub possible_template_params: Option<Vec<String>>,
    pub source_code_link: Option<String>,
    pub template_applied_params: Option<String>,
    pub template_source_language_address: Option<String>,
    pub templated: Option<bool>,
}

#[derive(GraphQLInputObject, Default)]
pub struct LanguageMetaInput {
    pub description: String,
    pub name: String,
    pub possible_template_params: Vec<String>,
    pub source_code_link: Option<String>,
}

#[derive(GraphQLObject, Default)]
pub struct LanguageRef {
    pub address: String,
    pub name: String,
}

#[derive(GraphQLObject, Default)]
pub struct Link {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

#[derive(GraphQLObject, Default)]
pub struct LinkExpression {
    pub author: String,
    pub data: Link,
    pub proof: ExpressionProof,
    pub timestamp: String,
}

#[derive(GraphQLInputObject, Default)]
pub struct LinkExpressionInput {
    pub author: String,
    pub data: LinkInput,
    pub proof: ExpressionProofInput,
    pub timestamp: String,
}

#[derive(GraphQLObject, Default)]
pub struct LinkExpressionMutations {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

#[derive(GraphQLObject, Default)]
pub struct LinkExpressionUpdated {
    pub new_link: LinkExpression,
    pub old_link: LinkExpression,
}

#[derive(GraphQLInputObject, Default)]
pub struct LinkInput {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

#[derive(GraphQLInputObject, Default)]
pub struct LinkMutations {
    pub additions: Vec<LinkInput>,
    pub removals: Vec<LinkExpressionInput>,
}

#[derive(GraphQLInputObject, Default)]
pub struct LinkQuery {
    pub from_date: Option<String>,
    pub limit: Option<f64>,
    pub predicate: Option<String>,
    pub source: Option<String>,
    pub target: Option<String>,
    pub until_date: Option<String>,
}

#[derive(GraphQLObject, Default)]
pub struct Neighbourhood {
    pub link_language: String,
    pub meta: Perspective,
}

#[derive(GraphQLObject, Default)]
pub struct OnlineAgent {
    pub did: String,
    pub status: PerspectiveExpression,
}

#[derive(GraphQLObject, Default)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

#[derive(GraphQLObject, Default)]
pub struct PerspectiveExpression {
    pub author: String,
    pub data: Perspective,
    pub proof: ExpressionProof,
    pub timestamp: String,
}

#[derive(GraphQLObject, Default)]
pub struct PerspectiveHandle {
    pub name: String,
    pub neighbourhood: Option<Neighbourhood>,
    pub shared_url: Option<String>,
    pub state: String,
    pub uuid: String,
}

#[derive(GraphQLInputObject, Default)]
pub struct PerspectiveInput {
    pub links: Vec<LinkExpressionInput>,
}

#[derive(GraphQLInputObject, Default)]
pub struct PerspectiveUnsignedInput {
    pub links: Vec<LinkInput>,
}

#[derive(GraphQLObject, Default)]
pub struct Resource {
    pub domain: String,
    pub pointers: Vec<String>,
}

#[derive(GraphQLInputObject, Default)]
pub struct ResourceInput {
    pub domain: String,
    pub pointers: Vec<String>,
}

#[derive(GraphQLObject, Default)]
pub struct RuntimeInfo {
    pub ad4m_executor_version: String,
    pub is_initialized: bool,
    pub is_unlocked: bool,
}

#[derive(GraphQLObject, Default)]
pub struct SentMessage {
    pub message: PerspectiveExpression,
    pub recipient: String,
}

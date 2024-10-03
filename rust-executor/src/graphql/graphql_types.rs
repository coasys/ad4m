use crate::agent::capabilities::{AuthInfo, Capability};
use crate::agent::signatures::verify;
use crate::js_core::JsCoreHandle;
use crate::types::{
    DecoratedExpressionProof, DecoratedLinkExpression, Expression, ExpressionProof, Link,
    Notification, TriggeredNotification,
};
use coasys_juniper::{
    FieldError, FieldResult, GraphQLEnum, GraphQLInputObject, GraphQLObject, GraphQLScalar,
};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde::{de::DeserializeOwned, Deserialize, Serialize};

#[derive(Clone)]
pub struct RequestContext {
    pub capabilities: Result<Vec<Capability>, String>,
    pub js_handle: JsCoreHandle,
    pub auto_permit_cap_requests: bool,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Agent {
    pub did: String,
    #[graphql(name = "directMessageLanguage")]
    pub direct_message_language: Option<String>,
    pub perspective: Option<Perspective>,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AgentSignature {
    pub public_key: String,
    pub signature: String,
}

#[derive(GraphQLObject, Default, Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AgentStatus {
    pub did: Option<String>,
    pub did_document: Option<String>,
    pub error: Option<String>,
    pub is_initialized: bool,
    pub is_unlocked: bool,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Apps {
    pub auth: AuthInfo,
    pub request_id: String,
    pub revoked: Option<bool>,
    pub token: String,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfoInput {
    #[graphql(name = "appDesc")]
    pub app_desc: String,
    #[graphql(name = "appDomain")]
    pub app_domain: String,
    #[graphql(name = "appIconPath")]
    pub app_icon_path: Option<String>,
    #[graphql(name = "appName")]
    pub app_name: String,
    #[graphql(name = "appUrl")]
    pub app_url: Option<String>,
    #[graphql(name = "capabilities")]
    pub capabilities: Option<Vec<CapabilityInput>>,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct CapabilityInput {
    pub can: Vec<String>,
    pub with: ResourceInput,
}

#[derive(GraphQLScalar, Default, Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
#[graphql(transparent)]
// The javascript `Date` as string. pub struct represents date and time as the ISO Date string.
pub struct DateTime(chrono::DateTime<chrono::Utc>);

impl From<DateTime> for chrono::DateTime<chrono::Utc> {
    fn from(val: DateTime) -> Self {
        val.0
    }
}

impl From<chrono::DateTime<chrono::Utc>> for DateTime {
    fn from(date: chrono::DateTime<chrono::Utc>) -> Self {
        DateTime(date)
    }
}

#[derive(GraphQLObject, Default, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "camelCase")]
pub struct EntanglementProof {
    #[graphql(name = "deviceKey")]
    pub device_key: String,
    #[graphql(name = "deviceKeySignedByDid")]
    pub device_key_signed_by_did: String,
    #[graphql(name = "deviceKeyType")]
    pub device_key_type: String,
    #[graphql(name = "did")]
    pub did: String,
    #[graphql(name = "didSignedByDeviceKey")]
    pub did_signed_by_device_key: Option<String>,
    #[graphql(name = "didSigningKeyId")]
    pub did_signing_key_id: String,
}

#[derive(GraphQLInputObject, Default, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EntanglementProofInput {
    #[graphql(name = "deviceKey")]
    pub device_key: String,
    #[graphql(name = "deviceKeySignedByDid")]
    pub device_key_signed_by_did: String,
    #[graphql(name = "deviceKeyType")]
    pub device_key_type: String,
    #[graphql(name = "did")]
    pub did: String,
    #[graphql(name = "didSignedByDeviceKey")]
    pub did_signed_by_device_key: String,
    #[graphql(name = "didSigningKeyId")]
    pub did_signing_key_id: String,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ExceptionInfo {
    pub addon: Option<String>,
    pub message: String,
    pub title: String,
    pub r#type: ExceptionType,
}

#[derive(GraphQLEnum, Default, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ExceptionType {
    LanguageIsNotLoaded = 0,
    ExpressionIsNotVerified = 1,
    AgentIsUntrusted = 2,
    #[default]
    CapabilityRequested = 3,
    InstallNotificationRequest = 4,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ExpressionProofInput {
    pub invalid: Option<bool>,
    pub key: Option<String>,
    pub signature: Option<String>,
    pub valid: Option<bool>,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ExpressionRendered {
    pub author: String,
    pub data: String,
    pub icon: Icon,
    pub language: LanguageRef,
    pub proof: DecoratedExpressionProof,
    pub timestamp: String,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Icon {
    pub code: Option<String>,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InteractionCall {
    pub name: String,
    pub parameters_stringified: String,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InteractionMeta {
    pub label: String,
    pub name: String,
    pub parameters: Vec<InteractionParameter>,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InteractionParameter {
    pub name: String,
    pub type_: String,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LanguageHandle {
    pub address: String,
    pub constructor_icon: Option<Icon>,
    pub icon: Option<Icon>,
    pub name: String,
    pub settings: Option<String>,
    pub settings_icon: Option<Icon>,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
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

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LanguageMetaInput {
    pub description: String,
    pub name: String,
    pub possible_template_params: Option<Vec<String>>,
    pub source_code_link: Option<String>,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LanguageRef {
    pub address: String,
    pub name: String,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Language {
    pub name: String,
}

#[derive(GraphQLEnum, Debug, Default, Deserialize, Serialize, Clone, PartialEq)]
pub enum LinkStatus {
    #[default]
    #[serde(rename = "shared")]
    Shared,
    #[serde(rename = "local")]
    Local,
}

//Impl display for LinkStatus
impl std::fmt::Display for LinkStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            LinkStatus::Shared => write!(f, "shared"),
            LinkStatus::Local => write!(f, "local"),
        }
    }
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkExpressionInput {
    pub author: String,
    pub data: LinkInput,
    pub proof: ExpressionProofInput,
    pub timestamp: String,
    pub status: Option<LinkStatus>,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkExpressionUpdated {
    pub new_link: DecoratedLinkExpression,
    pub old_link: DecoratedLinkExpression,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkInput {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkQuery {
    pub from_date: Option<DateTime>,
    pub limit: Option<i32>,
    pub predicate: Option<String>,
    pub source: Option<String>,
    pub target: Option<String>,
    pub until_date: Option<DateTime>,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkMutations {
    pub additions: Vec<LinkInput>,
    pub removals: Vec<LinkExpressionInput>,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct DecoratedPerspectiveDiff {
    pub additions: Vec<DecoratedLinkExpression>,
    pub removals: Vec<DecoratedLinkExpression>,
}

impl DecoratedPerspectiveDiff {
    pub fn from_additions(additions: Vec<DecoratedLinkExpression>) -> DecoratedPerspectiveDiff {
        DecoratedPerspectiveDiff {
            additions,
            removals: vec![],
        }
    }

    pub fn from_removals(removals: Vec<DecoratedLinkExpression>) -> DecoratedPerspectiveDiff {
        DecoratedPerspectiveDiff {
            additions: vec![],
            removals,
        }
    }

    pub fn from(
        additions: Vec<DecoratedLinkExpression>,
        removals: Vec<DecoratedLinkExpression>,
    ) -> DecoratedPerspectiveDiff {
        DecoratedPerspectiveDiff {
            additions,
            removals,
        }
    }
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Neighbourhood {
    pub link_language: String,
    pub meta: Perspective,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct DecoratedNeighbourhoodExpression {
    pub author: String,
    pub data: Neighbourhood,
    pub proof: DecoratedExpressionProof,
    pub timestamp: String,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct OnlineAgent {
    pub did: String,
    pub status: PerspectiveExpression,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Perspective {
    pub links: Vec<DecoratedLinkExpression>,
}

impl Perspective {
    pub fn verify_link_signatures(&mut self) {
        for link in &mut self.links {
            link.verify_signature();
        }
    }
}

impl From<PerspectiveInput> for Perspective {
    fn from(input: PerspectiveInput) -> Self {
        let links = input
            .links
            .into_iter()
            .map(|link: LinkExpressionInput| DecoratedLinkExpression::try_from(link))
            .filter_map(Result::ok)
            .collect();
        Perspective { links }
    }
}

impl TryFrom<LinkExpressionInput> for DecoratedLinkExpression {
    type Error = AnyError;
    fn try_from(input: LinkExpressionInput) -> Result<Self, Self::Error> {
        let data = Link {
            predicate: input.data.predicate,
            source: input.data.source,
            target: input.data.target,
        };
        Ok(DecoratedLinkExpression {
            author: input.author,
            timestamp: input.timestamp,
            data,
            proof: DecoratedExpressionProof {
                key: input.proof.key.ok_or(anyhow!("Key is required"))?,
                signature: input.proof.signature.ok_or(anyhow!("Key is required"))?,
                valid: input.proof.valid,
                invalid: input.proof.invalid,
            },
            status: input.status,
        })
    }
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveExpression {
    pub author: String,
    pub data: Perspective,
    pub proof: DecoratedExpressionProof,
    pub timestamp: String,
}

impl From<Expression<Perspective>> for PerspectiveExpression {
    fn from(expr: Expression<Perspective>) -> Self {
        PerspectiveExpression {
            author: expr.author,
            data: expr.data,
            proof: DecoratedExpressionProof {
                key: expr.proof.key,
                signature: expr.proof.signature,
                valid: None,
                invalid: None,
            },
            timestamp: expr.timestamp,
        }
    }
}

impl PerspectiveExpression {
    pub fn verify_signatures(&mut self) {
        self.data.verify_link_signatures();

        let perspective_expression = Expression::<Perspective> {
            author: self.author.clone(),
            data: self.data.clone(),
            proof: ExpressionProof {
                key: self.proof.key.clone(),
                signature: self.proof.signature.clone(),
            },
            timestamp: self.timestamp.clone(),
        };

        let valid = verify(&perspective_expression).unwrap_or(false);

        self.proof.valid = Some(valid);
        self.proof.invalid = Some(!valid);
    }
}

#[derive(GraphQLEnum, Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub enum PerspectiveState {
    #[default]
    Private,
    NeighbourhoodCreationInitiated,
    NeighbourhoodJoinInitiated,
    LinkLanguageFailedToInstall,
    LinkLanguageInstalledButNotSynced,
    Synced,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveHandle {
    pub uuid: String,
    pub name: Option<String>,
    pub neighbourhood: Option<DecoratedNeighbourhoodExpression>,
    pub shared_url: Option<String>,
    pub state: PerspectiveState,
}

impl PerspectiveHandle {
    pub fn new(
        uuid: String,
        name: Option<String>,
        neighbourhood: Option<DecoratedNeighbourhoodExpression>,
        shared_url: Option<String>,
        state: PerspectiveState,
    ) -> Self {
        PerspectiveHandle {
            name,
            uuid,
            neighbourhood,
            shared_url,
            state,
        }
    }

    pub fn new_from_name(name: String) -> Self {
        PerspectiveHandle {
            uuid: uuid::Uuid::new_v4().to_string(),
            name: Some(name),
            neighbourhood: None,
            shared_url: None,
            state: PerspectiveState::Private,
        }
    }
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveInput {
    pub links: Vec<LinkExpressionInput>,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveUnsignedInput {
    pub links: Vec<LinkInput>,
}

#[derive(GraphQLInputObject, Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct NotificationInput {
    pub description: String,
    pub app_name: String,
    pub app_url: String,
    pub app_icon_path: String,
    pub trigger: String,
    pub perspective_ids: Vec<String>,
    pub webhook_url: String,
    pub webhook_auth: String,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Resource {
    pub domain: String,
    pub pointers: Vec<String>,
}

#[derive(GraphQLInputObject, Default, Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ResourceInput {
    pub domain: String,
    pub pointers: Vec<String>,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct RuntimeInfo {
    pub ad4m_executor_version: String,
    pub is_initialized: bool,
    pub is_unlocked: bool,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SentMessage {
    pub message: PerspectiveExpression,
    pub recipient: String,
}

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct NeighbourhoodSignalFilter {
    pub perspective: PerspectiveHandle,
    pub signal: PerspectiveExpression,
}

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct PerspectiveLinkFilter {
    pub perspective: PerspectiveHandle,
    pub link: DecoratedLinkExpression,
}

#[derive(Default, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveLinkUpdatedFilter {
    pub new_link: DecoratedLinkExpression,
    pub old_link: DecoratedLinkExpression,
    pub perspective: PerspectiveHandle,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LinkUpdated {
    pub new_link: DecoratedLinkExpression,
    pub old_link: DecoratedLinkExpression,
}

#[derive(Default, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveStateFilter {
    pub state: String,
    pub perspective: PerspectiveHandle,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ModelApiInput {
    pub base_url: String,
    pub api_key: String,
    pub api_type: String,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LocalModelInput {
    pub file_name: String,
    pub tokenizer_source: String,
    pub model_parameters: String,
}

#[derive(GraphQLInputObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ModelInput {
    pub name: String,
    pub api: Option<ModelApiInput>,
    pub local: Option<LocalModelInput>,
    #[serde(rename = "type")]
    pub model_type: ModelType,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum JsResultType<T>
where
    T: std::fmt::Debug + Serialize + 'static,
{
    Ok(T),
    Error(String),
}

impl<T> JsResultType<T>
where
    T: std::fmt::Debug + Serialize + 'static,
{
    pub fn get_graphql_result(self) -> FieldResult<T> {
        match self {
            JsResultType::Ok(result) => Ok(result),
            JsResultType::Error(error) => Err(FieldError::from(error.clone())),
        }
    }
}

// Define the trait with a generic associated type `Value`
pub trait GetValue {
    type Value: Clone + DeserializeOwned + Send + 'static + std::fmt::Debug;
    fn get_value(&self) -> Self::Value;
}

pub trait GetFilter {
    fn get_filter(&self) -> Option<String>;
}

impl GetValue for Option<Apps> {
    type Value = Option<Apps>;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

impl GetFilter for Option<Apps> {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

// Implement the trait for the `NeighbourhoodSignalFilter` struct
impl GetValue for NeighbourhoodSignalFilter {
    type Value = PerspectiveExpression;

    fn get_value(&self) -> Self::Value {
        self.signal.clone()
    }
}

// Implement the trait for the `NeighbourhoodSignalFilter` struct
impl GetFilter for NeighbourhoodSignalFilter {
    fn get_filter(&self) -> Option<String> {
        Some(self.perspective.uuid.clone())
    }
}

// Implement the trait for the `PerspectiveLinkFilter` struct
impl GetValue for PerspectiveLinkFilter {
    type Value = DecoratedLinkExpression;

    fn get_value(&self) -> Self::Value {
        self.link.clone()
    }
}

// Implement the trait for the `PerspectiveLinkFilter` struct
impl GetFilter for PerspectiveLinkFilter {
    fn get_filter(&self) -> Option<String> {
        Some(self.perspective.uuid.clone())
    }
}

// Implement the trait for the `PerspectiveLinkUpdatedFilter` struct
impl GetValue for PerspectiveLinkUpdatedFilter {
    type Value = LinkUpdated;

    fn get_value(&self) -> Self::Value {
        LinkUpdated {
            new_link: self.new_link.clone(),
            old_link: self.old_link.clone(),
        }
    }
}

// Implement the trait for the `PerspectiveLinkUpdatedFilter` struct
impl GetFilter for PerspectiveLinkUpdatedFilter {
    fn get_filter(&self) -> Option<String> {
        Some(self.perspective.uuid.clone())
    }
}

// Implement the trait for the `PerspectiveStateFilter` struct
impl GetValue for PerspectiveStateFilter {
    type Value = String;

    fn get_value(&self) -> Self::Value {
        self.state.clone()
    }
}

// Implement the trait for the `PerspectiveStateFilter` struct
impl GetFilter for PerspectiveStateFilter {
    fn get_filter(&self) -> Option<String> {
        Some(self.perspective.uuid.clone())
    }
}

// Implement the trait for the `AgentStatus` struct
impl GetValue for AgentStatus {
    type Value = AgentStatus;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

// Implement the trait for the `AgentStatus` struct
impl GetFilter for AgentStatus {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

// Implement the trait for `Agent` struct
impl GetValue for Agent {
    type Value = Agent;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

// Implement the trait for `Agent` struct
impl GetFilter for Agent {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

//Implement the trait for `ExceptionInfo` struct
impl GetValue for ExceptionInfo {
    type Value = ExceptionInfo;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

//Implement the trait for `ExceptionInfo` struct
impl GetFilter for ExceptionInfo {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

//Implement the trait for `PerspectiveHandle` struct
impl GetValue for PerspectiveHandle {
    type Value = PerspectiveHandle;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

//Implement the trait for `PerspectiveHandle` struct
impl GetFilter for PerspectiveHandle {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

//Implement the trait for `String`
impl GetValue for String {
    type Value = String;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

//Implement the trait for `String`
impl GetFilter for String {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

//Implement the trait for `PerspectiveExpression`
impl GetValue for PerspectiveExpression {
    type Value = PerspectiveExpression;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

//Implement the trait for `PerspectiveExpression`
impl GetFilter for PerspectiveExpression {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

//Implement the trait for `Notification`
impl GetValue for Notification {
    type Value = Notification;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

//Implement the trait for `Notification`
impl GetFilter for Notification {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

//Implement the trait for `Notification`
impl GetValue for TriggeredNotification {
    type Value = TriggeredNotification;

    fn get_value(&self) -> Self::Value {
        self.clone()
    }
}

//Implement the trait for `Notification`
impl GetFilter for TriggeredNotification {
    fn get_filter(&self) -> Option<String> {
        None
    }
}

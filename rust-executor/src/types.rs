use coasys_juniper::{GraphQLEnum, GraphQLObject, GraphQLValue};
use deno_core::{anyhow::anyhow, error::AnyError};
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::str::FromStr;
use url::Url;

use crate::{
    agent::signatures::verify,
    graphql::graphql_types::{
        LinkExpressionInput, LinkInput, LinkStatus, NotificationInput, PerspectiveInput,
    },
};
use regex::Regex;

#[derive(Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Expression<T: Serialize> {
    pub author: String,
    pub timestamp: String,
    pub data: T,
    pub proof: ExpressionProof,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct ExpressionProof {
    pub key: String,
    pub signature: String,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct VerifiedExpression<T: GraphQLValue + Serialize> {
    pub author: String,
    pub timestamp: String,
    pub data: T,
    pub proof: DecoratedExpressionProof,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]

pub struct DecoratedExpressionProof {
    pub key: String,
    pub signature: String,
    pub valid: Option<bool>,
    pub invalid: Option<bool>,
}

impl<T: GraphQLValue + Serialize> From<Expression<T>> for VerifiedExpression<T> {
    fn from(expr: Expression<T>) -> Self {
        let valid = verify(&expr).unwrap_or(false);
        let invalid = !valid;
        VerifiedExpression {
            author: expr.author,
            timestamp: expr.timestamp,
            data: expr.data,
            proof: DecoratedExpressionProof {
                key: expr.proof.key,
                signature: expr.proof.signature,
                valid: Some(valid),
                invalid: Some(invalid),
            },
        }
    }
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Link {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

impl From<LinkInput> for Link {
    fn from(input: LinkInput) -> Self {
        Link {
            predicate: input.predicate,
            source: input.source,
            target: input.target,
        }
    }
}

impl Link {
    pub fn normalize(&self) -> Link {
        let predicate = match self.predicate.as_deref() {
            Some("") => None,
            _ => self.predicate.clone(),
        };

        Link {
            predicate,
            source: self.source.clone(),
            target: self.target.clone(),
        }
    }
}

#[derive(GraphQLObject, Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct LinkExpression {
    pub author: String,
    pub timestamp: String,
    pub data: Link,
    pub proof: ExpressionProof,
    pub status: Option<LinkStatus>,
}

impl TryFrom<LinkExpressionInput> for LinkExpression {
    type Error = AnyError;
    fn try_from(input: LinkExpressionInput) -> Result<Self, Self::Error> {
        let data = Link {
            predicate: input.data.predicate,
            source: input.data.source,
            target: input.data.target,
        }
        .normalize();
        Ok(LinkExpression {
            author: input.author,
            timestamp: input.timestamp,
            data,
            proof: ExpressionProof {
                key: input.proof.key.ok_or(anyhow!("Key is required"))?,
                signature: input.proof.signature.ok_or(anyhow!("Key is required"))?,
            },
            status: input.status,
        })
    }
}

impl LinkExpression {
    pub fn from_input_without_proof(input: LinkExpressionInput) -> Self {
        let data = Link {
            predicate: input.data.predicate,
            source: input.data.source,
            target: input.data.target,
        }
        .normalize();
        LinkExpression {
            author: input.author,
            timestamp: input.timestamp,
            data,
            proof: ExpressionProof::default(),
            status: input.status,
        }
    }
}

impl From<LinkExpression> for Expression<Link> {
    fn from(expr: LinkExpression) -> Self {
        Expression {
            author: expr.author,
            timestamp: expr.timestamp,
            data: expr.data,
            proof: expr.proof,
        }
    }
}

impl From<Expression<Link>> for LinkExpression {
    fn from(expr: Expression<Link>) -> Self {
        LinkExpression {
            author: expr.author,
            timestamp: expr.timestamp,
            data: expr.data.normalize(),
            proof: expr.proof,
            status: None,
        }
    }
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct DecoratedLinkExpression {
    pub author: String,
    pub timestamp: String,
    pub data: Link,
    pub proof: DecoratedExpressionProof,
    pub status: Option<LinkStatus>,
}

impl DecoratedLinkExpression {
    pub fn verify_signature(&mut self) {
        let link_expr = Expression::<Link> {
            author: self.author.clone(),
            timestamp: self.timestamp.clone(),
            data: self.data.normalize(),
            proof: ExpressionProof {
                key: self.proof.key.clone(),
                signature: self.proof.signature.clone(),
            },
        };
        let valid = verify(&link_expr).unwrap_or(false);
        self.proof.valid = Some(valid);
        self.proof.invalid = Some(!valid);
    }
}

impl From<(LinkExpression, LinkStatus)> for DecoratedLinkExpression {
    fn from((expr, status): (LinkExpression, LinkStatus)) -> Self {
        let mut expr: Expression<Link> = expr.into();
        expr.data = expr.data.normalize();
        let verified_expr: VerifiedExpression<Link> = expr.into();
        DecoratedLinkExpression {
            author: verified_expr.author,
            timestamp: verified_expr.timestamp,
            data: verified_expr.data,
            proof: verified_expr.proof,
            status: Some(status),
        }
    }
}

impl From<DecoratedLinkExpression> for LinkExpression {
    fn from(decorated: DecoratedLinkExpression) -> Self {
        LinkExpression {
            author: decorated.author,
            timestamp: decorated.timestamp,
            data: decorated.data.normalize(),
            proof: ExpressionProof {
                key: decorated.proof.key,
                signature: decorated.proof.signature,
            },
            status: decorated.status,
        }
    }
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

impl From<PerspectiveInput> for Perspective {
    fn from(input: PerspectiveInput) -> Self {
        let links = input
            .links
            .into_iter()
            .map(LinkExpression::try_from)
            .filter_map(Result::ok)
            .collect();
        Perspective { links }
    }
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Neighbourhood {
    pub link_language: String,
    pub meta: Perspective,
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct NeighbourhoodExpression {
    pub author: String,
    pub data: Neighbourhood,
    pub proof: ExpressionProof,
    pub timestamp: String,
}

pub type Address = String;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct LanguageRef {
    pub name: String,
    pub address: Address,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct ExpressionRef {
    pub language: LanguageRef,
    pub expression: Address,
}

impl TryFrom<String> for ExpressionRef {
    type Error = AnyError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if let Some(stripped) = value.strip_prefix("literal://") {
            let language_ref = LanguageRef {
                address: "literal".to_string(),
                name: "literal".to_string(),
            };
            let content = stripped.to_string();
            return Ok(ExpressionRef {
                language: language_ref,
                expression: content,
            });
        }

        let uri_regexp = Regex::new(r"^([^:^\s]+):\/\/([\s\S]+)$").unwrap();
        if let Some(uri_captures) = uri_regexp.captures(&value) {
            if uri_captures.len() == 3 {
                let language = uri_captures.get(1).unwrap().as_str().to_string();
                let expression = uri_captures.get(2).unwrap().as_str().to_string();

                let language_ref = LanguageRef {
                    address: language,
                    name: "".to_string(), // Assuming name is not provided in the URI
                };

                return Ok(ExpressionRef {
                    language: language_ref,
                    expression,
                });
            }
        }

        let did_regexp = Regex::new(r"^did:([^\s]+)$").unwrap();
        if let Some(did_captures) = did_regexp.captures(&value) {
            if did_captures.len() == 2 {
                let language_ref = LanguageRef {
                    address: "did".to_string(),
                    name: "".to_string(), // Assuming name is not provided in the DID
                };

                return Ok(ExpressionRef {
                    language: language_ref,
                    expression: value,
                });
            }
        }

        Err(anyhow!(
            "Couldn't parse string as expression URL or DID: {}",
            value
        ))
    }
}

impl Display for ExpressionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.language.address == "did" {
            write!(f, "{}", self.expression)
        } else {
            write!(f, "{}://{}", self.language.address, self.expression)
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

impl PerspectiveDiff {
    pub fn from_additions(additions: Vec<LinkExpression>) -> PerspectiveDiff {
        PerspectiveDiff {
            additions,
            removals: vec![],
        }
    }

    pub fn from_removals(removals: Vec<LinkExpression>) -> PerspectiveDiff {
        PerspectiveDiff {
            additions: vec![],
            removals,
        }
    }

    pub fn from(additions: Vec<LinkExpression>, removals: Vec<LinkExpression>) -> PerspectiveDiff {
        PerspectiveDiff {
            additions,
            removals,
        }
    }

    pub fn empty() -> PerspectiveDiff {
        PerspectiveDiff {
            additions: vec![],
            removals: vec![],
        }
    }
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "camelCase")]
pub struct Notification {
    pub id: String,
    pub granted: bool,
    pub description: String,
    pub app_name: String,
    pub app_url: String,
    pub app_icon_path: String,
    pub trigger: String,
    pub perspective_ids: Vec<String>,
    pub webhook_url: String,
    pub webhook_auth: String,
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "camelCase")]
pub struct AIPromptExamples {
    pub input: String,
    pub output: String,
}

#[derive(
    GraphQLObject, Serialize, Deserialize, Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord,
)]
#[serde(rename_all = "camelCase")]
pub struct AITask {
    pub name: String,
    pub task_id: String,
    pub model_id: String,
    pub system_prompt: String,
    pub prompt_examples: Vec<AIPromptExamples>,
    pub meta_data: Option<String>,
    pub created_at: String,
    pub updated_at: String,
}

impl Notification {
    pub fn from_input_and_id(id: String, input: NotificationInput) -> Self {
        Notification {
            id,
            granted: false,
            description: input.description,
            app_name: input.app_name,
            app_url: input.app_url,
            app_icon_path: input.app_icon_path,
            trigger: input.trigger,
            perspective_ids: input.perspective_ids,
            webhook_url: input.webhook_url,
            webhook_auth: input.webhook_auth,
        }
    }
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TriggeredNotification {
    pub notification: Notification,
    pub perspective_id: String,
    pub trigger_match: String,
}

#[derive(GraphQLEnum, Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum ModelApiType {
    OpenAi,
}

impl FromStr for ModelApiType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim_matches('"');
        match s {
            "openai" => Ok(ModelApiType::OpenAi),
            "openAi" => Ok(ModelApiType::OpenAi),
            "OpenAi" => Ok(ModelApiType::OpenAi),
            "OPEN_AI" => Ok(ModelApiType::OpenAi),
            _ => Err(format!("Unknown ModelApiType: {}", s)),
        }
    }
}

#[allow(clippy::to_string_trait_impl)]
impl ToString for ModelApiType {
    fn to_string(&self) -> String {
        match self {
            ModelApiType::OpenAi => "openAi".to_string(),
        }
    }
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ModelApi {
    pub base_url: Url,
    pub api_key: String,
    pub model: String,
    pub api_type: ModelApiType,
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct LocalModel {
    pub file_name: String,
    pub tokenizer_source: Option<TokenizerSource>,
    pub huggingface_repo: Option<String>,
    pub revision: Option<String>,
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TokenizerSource {
    pub repo: String,
    pub revision: String,
    pub file_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, GraphQLEnum, PartialEq, Default)]
#[serde(rename_all = "lowercase")]
pub enum ModelType {
    #[default]
    Llm,
    Embedding,
    Transcription,
}

impl Display for ModelType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModelType::Llm => write!(f, "LLM"),
            ModelType::Embedding => write!(f, "EMBEDDING"),
            ModelType::Transcription => write!(f, "TRANSCRIPTION"),
        }
    }
}

#[derive(GraphQLObject, Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Model {
    pub id: String,
    pub name: String,
    pub api: Option<ModelApi>,
    pub local: Option<LocalModel>,
    #[serde(rename = "type")]
    pub model_type: ModelType,
}

// Internal User struct - NOT exposed via GraphQL
// Contains sensitive data like password_hash that should never be returned to clients
#[derive(Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct User {
    pub username: String,
    pub did: String,
    #[serde(skip_serializing)]
    pub password_hash: String,  // Argon2id hash - never serialize or expose
    pub last_seen: Option<i32>,
}

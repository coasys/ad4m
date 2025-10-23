use coasys_juniper::GraphQLObject;
use serde::{Deserialize, Serialize};
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfoExtended {
    pub request_id: String,
    pub auth: AuthInfo,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfo {
    pub app_name: String,
    pub app_desc: String,
    pub app_domain: Option<String>,
    pub app_url: Option<String>,
    pub app_icon_path: Option<String>,
    pub capabilities: Option<Vec<Capability>>,
    pub user_email: Option<String>, // Email field for multi-user tokens
}

impl From<crate::graphql::graphql_types::AuthInfoInput> for AuthInfo {
    fn from(input: crate::graphql::graphql_types::AuthInfoInput) -> Self {
        Self {
            app_name: input.app_name,
            app_desc: input.app_desc,
            app_domain: Some(input.app_domain),
            app_url: input.app_url,
            app_icon_path: input.app_icon_path,
            capabilities: input
                .capabilities
                .map(|vec| vec.into_iter().map(|c| c.into()).collect()),
            user_email: None, // Will be set by login process
        }
    }
}

#[derive(GraphQLObject, Default, Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Capability {
    pub with: Resource,
    pub can: Vec<String>,
}

impl From<crate::graphql::graphql_types::CapabilityInput> for Capability {
    fn from(input: crate::graphql::graphql_types::CapabilityInput) -> Self {
        Self {
            with: input.with.into(),
            can: input.can,
        }
    }
}

#[derive(GraphQLObject, Default, Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Resource {
    pub domain: String,
    pub pointers: Vec<String>,
}

impl From<crate::graphql::graphql_types::ResourceInput> for Resource {
    fn from(input: crate::graphql::graphql_types::ResourceInput) -> Self {
        Self {
            domain: input.domain,
            pointers: input.pointers,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Claims {
    iss: String,
    pub sub: Option<String>, // User email - make public so we can access it
    aud: String,
    exp: u64,
    iat: u64,
    nonce: String,
    pub capabilities: AuthInfo,
}

impl Claims {
    pub fn new(
        issuer: String,
        audience: String,
        expiration_time: u64,
        capabilities: AuthInfo,
    ) -> Self {
        let now = SystemTime::now();
        let unix_timestamp = now
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_secs();

        let nonce = uuid::Uuid::new_v4().to_string();

        Claims {
            iss: issuer,
            sub: capabilities.user_email.clone(), // Set sub to user email
            aud: audience,
            exp: unix_timestamp + expiration_time,
            iat: unix_timestamp,
            nonce,
            capabilities,
        }
    }
}

use serde::{Deserialize, Serialize};
use std::time::{SystemTime, UNIX_EPOCH};
use ts_rs::TS;

#[derive(Debug, Serialize, Deserialize, Clone, TS)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfoExtended {
    pub request_id: String,
    pub auth: AuthInfo,
}

#[derive(Default, Debug, Deserialize, Serialize, Clone, TS)]
#[serde(rename_all = "camelCase")]
pub struct AuthInfo {
    pub app_name: String,
    pub app_desc: String,
    pub app_domain: Option<String>,
    pub app_url: Option<String>,
    pub app_icon_path: Option<String>,
    pub capabilities: Option<Vec<Capability>>,
    /// The user a multi-user token is minted for: the login name of a managed agent's
    /// account. The token's `sub` carries the agent's DID; this stays as the account's name.
    pub user_email: Option<String>,
}

impl TryFrom<crate::types::AuthInfoInput> for AuthInfo {
    type Error = String;

    fn try_from(input: crate::types::AuthInfoInput) -> Result<Self, Self::Error> {
        let capabilities = match input.capabilities {
            Some(vec) if !vec.is_empty() => Some(vec.into_iter().map(|c| c.into()).collect()),
            _ => return Err("Capability request must include explicit capabilities".to_string()),
        };

        Ok(Self {
            app_name: input.app_name,
            app_desc: input.app_desc,
            app_domain: Some(input.app_domain),
            app_url: input.app_url,
            app_icon_path: input.app_icon_path,
            capabilities,
            user_email: None, // Will be set by login process
        })
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
pub struct Capability {
    pub with: Resource,
    pub can: Vec<String>,
}

impl From<crate::types::CapabilityInput> for Capability {
    fn from(input: crate::types::CapabilityInput) -> Self {
        Self {
            with: input.with.into(),
            can: input.can,
        }
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
pub struct Resource {
    pub domain: String,
    pub pointers: Vec<String>,
}

impl From<crate::types::ResourceInput> for Resource {
    fn from(input: crate::types::ResourceInput) -> Self {
        Self {
            domain: input.domain,
            pointers: input.pointers,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, TS)]
pub struct Claims {
    iss: String,
    /// The managed agent a user token acts as: its DID. Tokens minted before agents were keyed
    /// by DID carry the user's email here; `Session::open` resolves either.
    pub sub: Option<String>,
    aud: String,
    exp: u64,
    iat: u64,
    nonce: String,
    // Tokens issued before grants also carry `capabilities` (the app's `AuthInfo`). Serde
    // skips the unknown field, so they still decode; their grant (migrated from the old app
    // registry) holds the capabilities now, and nothing reads them from a token.
}

impl Claims {
    /// Claims for a token that the main agent's key `issuer` signs, naming the managed agent
    /// `sub` (`None`: the main agent).
    pub fn new(
        issuer: String,
        audience: String,
        expiration_time: u64,
        sub: Option<String>,
    ) -> Self {
        let now = SystemTime::now();
        let unix_timestamp = now
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_secs();

        let nonce = uuid::Uuid::new_v4().to_string();

        Claims {
            iss: issuer,
            sub,
            aud: audience,
            exp: unix_timestamp + expiration_time,
            iat: unix_timestamp,
            nonce,
        }
    }

    /// The DID of the key that signed the token.
    pub fn issuer(&self) -> &str {
        &self.iss
    }

    /// The token's id: a fresh v4 UUID per token.
    pub fn nonce(&self) -> &str {
        &self.nonce
    }

    /// Unix time (seconds) after which the token no longer validates.
    pub fn expires_at(&self) -> u64 {
        self.exp
    }

    /// Unix time (seconds) at which the token was issued.
    pub fn issued_at(&self) -> u64 {
        self.iat
    }
}

//! The request box: short requests between a person's own devices (spec section 17).
//!
//! A device key without the bot mark posts a [`PostedRequest`] with a signed store request;
//! apps that hold the current master key read the box and act only on the person's yes. A
//! request never opens a link, moves a key or changes the log by itself. The store keeps the
//! signed request beside the body, so a reader checks that the device posted it: its
//! `bodyHash` is the hash of `JCS({body, expires})`, the body the agent language sends.

use serde::{Deserialize, Serialize};

use crate::{KeyId, KeyState};

/// Every request lives at most 7 days.
pub const REQUEST_TTL_S: u64 = 7 * 86_400;
/// How far a poster's clock may run ahead of the store's.
pub const MAX_CLOCK_SKEW_S: u64 = 300;
/// The lifetime a poster gives a request it posts: [`REQUEST_TTL_S`] less [`MAX_CLOCK_SKEW_S`],
/// so a store whose clock is behind the poster's still accepts it.
pub const REQUEST_POST_TTL_S: u64 = REQUEST_TTL_S - MAX_CLOCK_SKEW_S;
/// Open requests per identity.
pub const MAX_OPEN_REQUESTS: usize = 16;
/// Key ids one `confirm-revoke` names, at most.
pub const MAX_REVOKE_KEYS: usize = 32;

/// What a device asks.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case", deny_unknown_fields)]
pub enum DeviceRequestBody {
    /// A device whose master key no longer counts asks for the current one. The answering app
    /// seals it to the `enc` key the log lists for the poster. A new one from the same key
    /// replaces the older. (A struct variant, so an extra field is refused like any other.)
    SendMaster {},
    /// An app without the master key asks an app with it to sign a revocation, and the
    /// rotation when `rotate` is set (that one needs the words).
    #[serde(rename_all = "camelCase")]
    ConfirmRevoke { key_ids: Vec<KeyId>, rotate: bool },
}

impl DeviceRequestBody {
    /// The wire name of the kind.
    pub fn kind(&self) -> &'static str {
        match self {
            DeviceRequestBody::SendMaster {} => "send-master",
            DeviceRequestBody::ConfirmRevoke { .. } => "confirm-revoke",
        }
    }
}

/// The body of `POST /v1/requests/{scid}`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct PostedRequest {
    pub body: DeviceRequestBody,
    /// Unix seconds; at most [`REQUEST_TTL_S`] after the store takes it.
    pub expires: u64,
}

/// Why a request is refused.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum RequestError {
    #[error("malformed: {0}")]
    Malformed(String),
    #[error("the request expires in the past or more than 7 days ahead")]
    Expires,
}

impl RequestError {
    /// A short name for error bodies.
    pub fn name(&self) -> &'static str {
        match self {
            RequestError::Malformed(_) => "malformed",
            RequestError::Expires => "expires",
        }
    }
}

/// Checks a posted request against the log at `head_seq` and the store's clock: `expires`
/// after `now` and at most 7 days ahead; a `confirm-revoke` names 1 to 32 distinct device keys
/// of the log, each valid at the head.
pub fn check_post(
    post: &PostedRequest,
    state: &KeyState,
    head_seq: u64,
    now: u64,
) -> Result<(), RequestError> {
    if post.expires <= now || post.expires > now.saturating_add(REQUEST_TTL_S) {
        return Err(RequestError::Expires);
    }
    if let DeviceRequestBody::ConfirmRevoke { key_ids, .. } = &post.body {
        if key_ids.is_empty() || key_ids.len() > MAX_REVOKE_KEYS {
            return Err(RequestError::Malformed(format!(
                "confirm-revoke names 1 to {MAX_REVOKE_KEYS} keys"
            )));
        }
        for (i, id) in key_ids.iter().enumerate() {
            if key_ids[..i].contains(id) {
                return Err(RequestError::Malformed(format!("{id} is named twice")));
            }
            if !state.keys.contains_key(id) || !state.key_valid_at(id, head_seq) {
                return Err(RequestError::Malformed(format!(
                    "{id} is not a device key valid at the head"
                )));
            }
        }
    }
    Ok(())
}

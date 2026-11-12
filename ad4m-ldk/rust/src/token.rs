//! Token wire types (interfaces only). The same types live in
//! `ad4m-ldk/js/src/types.ts`. Accounts are DIDs, amounts are decimal
//! strings, and the interface carries no signature: the language signs
//! through its context, as it does for expressions.

use serde::{Deserialize, Serialize};

use crate::errors::{ErrorCode, LanguageError};

/// A decimal string: no floats, no 2^53 limit.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Amount {
    pub value: String,
    pub decimals: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenBalance {
    pub account: String,
    pub asset: String,
    pub amount: Amount,
    pub as_of: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TransferState {
    Pending,
    Settled,
    Refused,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenTransfer {
    pub id: String,
    /// Accounts are DIDs: did:scid or did:key.
    pub from: String,
    pub to: String,
    /// The implementation's asset id, for example "unyt:<id>" or "synergy:<id>".
    pub asset: String,
    pub amount: Amount,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub memo: Option<String>,
    pub time: u64,
    pub state: TransferState,
}

/// Transfers to the agent, newest last, and the cursor to read on from.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TransferPage {
    pub items: Vec<TokenTransfer>,
    #[serde(default)]
    pub cursor: Option<String>,
}

/// The tag of the language signal that carries a token transfer.
pub const TOKEN_TRANSFER_SIGNAL: &str = "token-transfer";

/// The language signal a transfer reaches the executor as:
/// `{ "tag": "token-transfer", "transfer": TokenTransfer }`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenTransferSignal {
    pub tag: String,
    pub transfer: TokenTransfer,
}

impl TokenTransferSignal {
    pub fn new(transfer: TokenTransfer) -> Self {
        Self {
            tag: TOKEN_TRANSFER_SIGNAL.to_string(),
            transfer,
        }
    }
}

/// How the token interface refuses a call. The closed beta's agent
/// language answers `Disabled` to every call.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "code", rename_all = "kebab-case")]
pub enum TokenErrorCode {
    Disabled,
    UnknownAsset,
    UnknownAccount,
    Insufficient,
    Refused { reason: String },
    Unavailable,
}

impl TokenErrorCode {
    /// The runtime's error class for this refusal.
    pub fn error_class(&self) -> ErrorCode {
        match self {
            Self::Disabled | Self::Refused { .. } => ErrorCode::PermissionDenied,
            Self::UnknownAsset | Self::UnknownAccount => ErrorCode::NotFound,
            Self::Insufficient => ErrorCode::InvalidInput,
            Self::Unavailable => ErrorCode::Transient,
        }
    }

    /// The refusal a language error carries, or `None` when it carries none.
    pub fn from_language_error(error: &LanguageError) -> Option<Self> {
        serde_json::from_str(&error.message).ok()
    }
}

impl From<TokenErrorCode> for LanguageError {
    fn from(refusal: TokenErrorCode) -> Self {
        let message = serde_json::to_string(&refusal)
            .unwrap_or_else(|e| format!("token refusal not encodable: {e}"));
        LanguageError {
            code: refusal.error_class(),
            message,
        }
    }
}

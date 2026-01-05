use serde_json::{json, Value as JsonValue};
use std::path::PathBuf;

/// Language context that is passed to language constructors
#[derive(Debug, Clone)]
pub struct LanguageContext {
    /// Agent DID
    pub agent_did: String,

    /// Agent signing key ID
    pub agent_signing_key_id: String,

    /// Custom settings for this language
    pub custom_settings: Option<JsonValue>,

    /// Storage directory for this language
    pub storage_directory: PathBuf,

    /// Language address (IPFS hash)
    pub language_address: String,
}

impl LanguageContext {
    pub fn new(
        agent_did: String,
        agent_signing_key_id: String,
        custom_settings: Option<JsonValue>,
        storage_directory: PathBuf,
        language_address: String,
    ) -> Self {
        Self {
            agent_did,
            agent_signing_key_id,
            custom_settings,
            storage_directory,
            language_address,
        }
    }

    /// Convert the context to JSON for passing to JavaScript
    pub fn to_json(&self) -> JsonValue {
        let storage_dir_str = self.storage_directory.to_string_lossy().to_string();

        json!({
            "agent": {
                "did": self.agent_did,
                "signingKeyId": self.agent_signing_key_id,
            },
            "customSettings": self.custom_settings,
            "storageDirectory": storage_dir_str,
            // Holochain delegate is created in JS via the holochain extension ops
            "Holochain": {
                "__languageAddress": self.language_address,
            },
            // ad4mSignal callback is available via the ad4m_signal op
            "ad4mSignal": {
                "__languageAddress": self.language_address,
            }
        })
    }
}

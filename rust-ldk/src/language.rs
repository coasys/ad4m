//! Core Language trait and context for AD4M languages

use crate::types::Perspective;
use serde_json::Value as JsonValue;
use std::path::PathBuf;

/// Context passed to a language during initialization
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
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
    /// Creates a new language context
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

    /// Returns the agent DID
    pub fn agent_did(&self) -> &str {
        &self.agent_did
    }

    /// Returns the agent signing key ID
    pub fn agent_signing_key_id(&self) -> &str {
        &self.agent_signing_key_id
    }

    /// Returns the custom settings
    pub fn custom_settings(&self) -> Option<&JsonValue> {
        self.custom_settings.as_ref()
    }

    /// Returns the storage directory
    pub fn storage_directory(&self) -> &PathBuf {
        &self.storage_directory
    }

    /// Returns the language address
    pub fn language_address(&self) -> &str {
        &self.language_address
    }
}

/// The core trait that all AD4M languages must implement
///
/// This trait defines the interface between the AD4M executor and
/// language implementations. Languages compiled to WASM should
/// implement this trait and expose it via wasm-bindgen.
///
/// # Example
///
/// ```rust
/// use ad4m_ldk::{Language, LanguageContext, Perspective};
///
/// pub struct MyLanguage {
///     context: LanguageContext,
/// }
///
/// impl Language for MyLanguage {
///     const NAME: &'static str = "my-language";
///     const VERSION: &'static str = "0.1.0";
///
///     fn init(context: LanguageContext) -> Result<Self, String> {
///         Ok(Self { context })
///     }
///
///     fn get_state(&self) -> Result<Option<Perspective>, String> {
///         Ok(None)
///     }
///
///     fn receive(&self, _data: Vec<u8>) -> Result<(), String> {
///         Ok(())
///     }
/// }
/// ```
pub trait Language {
    /// The name of the language
    const NAME: &'static str;

    /// The version of the language
    const VERSION: &'static str;

    /// Initialize the language with the given context
    ///
    /// This is called once when the language is loaded by the executor.
    /// The language should use this to set up any initial state.
    fn init(language_context: LanguageContext) -> Result<(), String>
    where
        Self: Sized;

    /// Get the current state of the language
    ///
    /// Returns the current perspective state, or None if the language
    /// doesn't maintain state.
    fn get_state(&self) -> Result<Option<Perspective>, String>;

    /// Receive data from another agent
    ///
    /// This is called when the language receives a direct message
    /// from another agent.
    fn receive(&self, data: Vec<u8>) -> Result<(), String>;
}

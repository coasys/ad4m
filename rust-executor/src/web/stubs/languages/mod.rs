//! `crate::languages` on wasm32: no language runtime (Deno) in the web executor.
//!
//! Real: `capability`, `error`, `literal` (pure Rust, the native files by path).
//! Stubbed: `LanguageController` (every call is a [`LanguageError`] naming the web
//! executor), `Language` (an empty capability set, so every call answers as native does for a
//! language without that capability).

#[path = "../../../languages/capability.rs"]
pub mod capability;
#[path = "../../../languages/error.rs"]
pub mod error;
pub mod language;
#[path = "../../../languages/literal.rs"]
pub mod literal;

use crate::agent::AgentContext;
use anyhow::Error as AnyError;
use error::LanguageError;
use language::Language;
use serde_json::Value as JsonValue;

type Address = String;

const WHAT: &str = "the language runtime";

fn unavailable(address: &str) -> LanguageError {
    LanguageError::RuntimeError {
        address: address.to_string(),
        message: crate::web::stubs::NotAvailable(WHAT).to_string(),
    }
}

/// The addresses of the system languages. None is loaded in the web executor.
#[derive(Clone, Debug, Default)]
pub struct SystemLanguageAddresses {
    pub language_language: Option<String>,
    pub agent_language: Option<String>,
    pub neighbourhood_language: Option<String>,
    pub perspective_language: Option<String>,
}

/// `LanguageController` without runtimes: it loads nothing and every call is an error.
#[derive(Clone, Default)]
pub struct LanguageController;

impl LanguageController {
    pub fn global_instance() -> LanguageController {
        LanguageController
    }

    /// `Ok(())`: there are no system languages to load in the web executor, so loading all of
    /// them succeeds at once. The one stub that answers `Ok` instead of an error: `agent.generate`
    /// and `agent.unlock` load them after the keys are made, and an error here would put a
    /// standing `agent.error` on every web agent. A language that is asked for later still
    /// answers "not available" (`language_by_address`, `install_language`, ...).
    pub async fn load_core_system_languages(
        &self,
        _language_language_only: bool,
    ) -> Result<(), LanguageError> {
        Ok(())
    }

    /// No language is ever loaded.
    pub async fn is_language_loaded(&self, _language_address: &str) -> bool {
        false
    }

    pub async fn language_by_ref(&self, address: &str) -> Result<Language, LanguageError> {
        Err(unavailable(address))
    }

    pub async fn install_language(language: Address) -> Result<(), AnyError> {
        Err(unavailable(&language).into())
    }

    pub async fn language_by_address(address: Address) -> Result<Option<Language>, AnyError> {
        Err(unavailable(&address).into())
    }

    pub fn parse_expr_url(url: &str) -> Result<(String, String), LanguageError> {
        Err(unavailable(url))
    }

    pub async fn get_agent_language(&self) -> Result<Language, LanguageError> {
        Err(unavailable("agent language"))
    }

    pub async fn get_expression(
        &self,
        lang_address: &str,
        _expression_address: &str,
    ) -> Result<Option<JsonValue>, LanguageError> {
        Err(unavailable(lang_address))
    }

    pub async fn expression_create(
        &self,
        lang_address: &str,
        _content: JsonValue,
        _agent_context: &AgentContext,
    ) -> Result<String, LanguageError> {
        Err(unavailable(lang_address))
    }
}

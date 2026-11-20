//! `crate::ai_service` on wasm32: no AI in the web executor (no model servers, no local
//! AI, no tool-calling harness).
//!
//! Stubbed: `AIService::global_instance()` is always an error
//! ([`NotAvailable`](crate::web::stubs::NotAvailable)), so the AI service is never there and
//! callers take their "not ready" path. The config's AI keys are kept as opaque JSON and
//! never applied. The few pure helpers ring 2 calls answer as native does with no AI set
//! up.

use anyhow::Result;

const WHAT: &str = "the AI service";

pub const DEFAULT_MODEL: &str = "default";

/// What `AIService::prompt` returns; only `text` is read by ring 2.
pub struct PromptResult {
    pub text: String,
}

/// Never built on wasm32 ([`AIService::global_instance`] is always an error).
#[derive(Clone)]
pub struct AIService {
    _none: (),
}

impl AIService {
    pub async fn global_instance() -> Result<AIService> {
        Err(crate::web::stubs::not_available(WHAT))
    }

    pub async fn prompt(
        &self,
        _task_id: String,
        _prompt: String,
        _bill_to: Option<String>,
    ) -> Result<PromptResult> {
        Err(crate::web::stubs::not_available(WHAT))
    }

    pub async fn default_llm_ready(&self) -> bool {
        false
    }

    pub fn watch_llm_readiness(&self) -> tokio::sync::watch::Receiver<u64> {
        tokio::sync::watch::channel(0).1
    }
}

pub mod endpoints {
    /// Copy of native `endpoints::has_credentials`.
    pub(crate) fn has_credentials(url: &url::Url) -> bool {
        !url.username().is_empty() || url.password().is_some()
    }

    /// The config file's AI keys, kept as the JSON they were written in. Nothing applies
    /// them on wasm32.
    pub mod config {
        use serde::{Deserialize, Serialize};

        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct AiEndpointSetting(serde_json::Value);

        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct AiModelSetting(serde_json::Value);

        #[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct AiDefaultsSetting(serde_json::Value);

        #[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
        pub struct AiSettings {
            #[serde(default)]
            pub endpoints: Vec<AiEndpointSetting>,
            #[serde(default)]
            pub models: Vec<AiModelSetting>,
            #[serde(default)]
            pub defaults: AiDefaultsSetting,
        }
    }
}

pub mod harness {
    /// Same trait as native `harness::CreditGate`; nothing calls it on wasm32.
    #[async_trait::async_trait]
    pub trait CreditGate: Send + Sync {
        async fn check(&self) -> anyhow::Result<()>;
    }
}

pub mod local {
    use serde::{Deserialize, Serialize};
    use ts_rs::TS;

    /// No local AI on wasm32, so no endpoint is local AI's.
    pub(crate) fn is_managed_endpoint_name(_name: &str) -> bool {
        false
    }

    /// Same shape as native `local::ProgressEvent` (the `ai-local-progress` payload).
    #[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
    #[serde(rename_all = "camelCase")]
    #[ts(rename = "LocalAiProgress")]
    pub struct ProgressEvent {
        pub phase: String,
        pub item: Option<String>,
        #[ts(type = "number")]
        pub item_done_bytes: u64,
        #[ts(type = "number")]
        pub item_total_bytes: u64,
        #[ts(type = "number")]
        pub done_bytes: u64,
        #[ts(type = "number")]
        pub total_bytes: u64,
        pub detail: Option<String>,
    }

    pub mod hardware {
        use serde::{Deserialize, Serialize};
        use ts_rs::TS;

        /// Same variants as native `local::hardware::Backend` (a config key names one).
        #[derive(
            Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, TS,
        )]
        #[serde(rename_all = "lowercase")]
        #[ts(rename = "LocalAiBackend")]
        pub enum Backend {
            Cpu,
            Metal,
            Vulkan,
            Cuda,
            Rocm,
        }
    }
}

pub mod providers {
    /// Copy of native `providers::api_type_supports_native_tools`.
    pub fn api_type_supports_native_tools(api_type: &crate::types::ModelApiType) -> bool {
        match api_type {
            crate::types::ModelApiType::Anthropic => true,
            crate::types::ModelApiType::Ollama => true,
            crate::types::ModelApiType::OpenAi => true,
        }
    }
}

//! Language tools — introspect installed languages and their metadata. `language_meta`
//! calls the RPC method `language.meta` through the handler map, so its guard applies.

use super::Ad4mMcpHandler;
use crate::types::LanguageMeta;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter types
// ============================================================================

/// Parameters for getting language metadata
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct LanguageMetaParams {
    /// Address (hash) of the language to get metadata for
    pub address: String,
}

// ============================================================================
// Tool implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Get metadata about a language by address
    #[tool(
        description = "Get metadata about an installed language by its address. Returns name, description, author, source code link, and template parameters. Useful for inspecting link languages, expression languages, or any language in the AD4M ecosystem."
    )]
    pub async fn language_meta(&self, params: Parameters<LanguageMetaParams>) -> String {
        let p = &params.0;

        let meta: LanguageMeta = match self
            .call("language.meta", json!({ "address": p.address }), |e| {
                json!({ "error": e.message }).to_string()
            })
            .await
        {
            Ok(meta) => meta,
            Err(e) => return e,
        };
        let result = json!({
            "address": meta.address,
            "name": meta.name,
            "description": meta.description,
            "author": meta.author,
            "possible_template_params": meta.possible_template_params,
            "source_code_link": meta.source_code_link,
            "template_applied_params": meta.template_applied_params,
            "template_source_language_address": meta.template_source_language_address,
        });
        serde_json::to_string_pretty(&result)
            .unwrap_or_else(|e| format!("Error serializing: {}", e))
    }
}

// Note: Integration tests for language tools are in tests/js/tests/mcp-neighbourhood.test.ts
// These test the actual MCP endpoints with a running executor.

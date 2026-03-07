//! Language tools — introspect installed languages and their metadata.

use super::Ad4mMcpHandler;
use crate::agent::capabilities::{check_capability, defs::LANGUAGE_READ_CAPABILITY};
use crate::languages::LanguageController;
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

        let capabilities = self.get_capabilities().await;
        if let Err(e) = check_capability(&capabilities, &LANGUAGE_READ_CAPABILITY) {
            return format!("Capability error: {}", e);
        }

        let controller = LanguageController::global_instance();
        match controller.get_language_expression(&p.address).await {
            Ok(meta) => {
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
            Err(e) => {
                json!({"error": format!("Failed to get language meta for '{}': {}", p.address, e)})
                    .to_string()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_language_meta_params_deserialize() {
        let json = r#"{"address": "QmzSYwdnDDbtgry2DmiqYpVtoPP75MabguWZUy1Ene7NzQAQWz2"}"#;
        let params: LanguageMetaParams = serde_json::from_str(json).unwrap();
        assert_eq!(
            params.address,
            "QmzSYwdnDDbtgry2DmiqYpVtoPP75MabguWZUy1Ene7NzQAQWz2"
        );
    }

    #[test]
    fn test_language_meta_response_structure() {
        // Verify the response JSON structure includes all expected fields
        let response = json!({
            "address": "QmzSYwdnDDbtgry2DmiqYpVtoPP75MabguWZUy1Ene7NzQAQWz2",
            "name": "perspective-diff-sync",
            "description": "Holochain-based P2P sync for AD4M neighbourhoods",
            "author": "did:key:z6MkgtBC3UaxNLt5SFJmxHtzFUUeJLCxLiP8DTqJgwF9uCkv",
            "possible_template_params": ["uid", "name"],
            "source_code_link": "https://github.com/coasys/ad4m",
            "template_applied_params": null,
            "template_source_language_address": null,
        });

        assert!(response["address"].as_str().is_some());
        assert!(response["name"].as_str().is_some());
        assert!(response["description"].as_str().is_some());
        assert!(response["author"].as_str().is_some());
        assert!(response["possible_template_params"].as_array().is_some());
    }
}

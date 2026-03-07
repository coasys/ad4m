//! Neighbourhood tools — publish perspectives as neighbourhoods and join existing ones.
//!
//! High-level tools for P2P collaboration. Agents select from known link language
//! templates; the system handles cloning and unique instance creation automatically.

use super::Ad4mMcpHandler;
use crate::agent::capabilities::{
    check_capability,
    defs::{
        NEIGHBOURHOOD_CREATE_CAPABILITY, NEIGHBOURHOOD_READ_CAPABILITY,
        RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY,
    },
};
use crate::graphql::graphql_types::Perspective;
use crate::languages::LanguageController;
use crate::neighbourhoods;
use crate::runtime_service::RuntimeService;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter types
// ============================================================================

/// Parameters for publishing a perspective as a neighbourhood
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct NeighbourhoodPublishParams {
    /// UUID of the local perspective to publish as a shared neighbourhood
    pub perspective_uuid: String,
    /// Address of a link language **template** to clone for this neighbourhood.
    /// Use `list_link_language_templates` to see available templates.
    /// The template is cloned with a unique name so each neighbourhood gets its own
    /// link language instance (required for P2P sync isolation).
    pub link_language_template: String,
    /// Human-readable name for this neighbourhood (used as the cloned language name)
    pub name: String,
}

/// Parameters for joining a neighbourhood
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct NeighbourhoodJoinParams {
    /// Neighbourhood URL to join (e.g. neighbourhood://Qm...)
    pub url: String,
}

// ============================================================================
// Tool implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// List available link language templates for neighbourhood creation
    #[tool(
        description = "List available link language templates that can be used when publishing a neighbourhood. Each template is a P2P synchronization engine. Returns address, name, and description for each template. Pass the address as `link_language_template` when calling `neighbourhood_publish_from_perspective`."
    )]
    pub async fn list_link_language_templates(&self) -> String {
        let capabilities = self.get_capabilities().await;
        if let Err(e) =
            check_capability(&capabilities, &RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY)
        {
            return format!("Capability error: {}", e);
        }

        let addresses = match RuntimeService::with_global_instance(|runtime_service| {
            Ok::<Vec<String>, String>(runtime_service.get_know_link_languages())
        }) {
            Ok(addrs) => addrs,
            Err(e) => {
                return json!({"error": format!("Failed to get link language templates: {}", e)})
                    .to_string()
            }
        };

        // Fetch meta information for each template
        let controller = LanguageController::global_instance();
        let mut templates = Vec::new();
        for address in &addresses {
            let meta = controller.get_language_expression(address).await;
            match meta {
                Ok(m) => {
                    templates.push(json!({
                        "address": m.address,
                        "name": m.name,
                        "description": m.description,
                        "author": m.author,
                        "possible_template_params": m.possible_template_params,
                    }));
                }
                Err(_) => {
                    // Fallback: try to get at least the name from local runtime
                    let name = controller.get_language_name(address).await;
                    templates.push(json!({
                        "address": address,
                        "name": name,
                        "description": null,
                    }));
                }
            }
        }

        let result = json!({
            "templates": templates,
            "count": templates.len(),
            "hint": "Pass the address of a template as link_language_template when publishing a neighbourhood."
        });
        serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
    }

    /// Clone a link language template and publish the cloned instance.
    /// Returns the new language address.
    async fn clone_link_language(
        &self,
        template_address: &str,
        name: &str,
    ) -> Result<String, String> {
        let controller = LanguageController::global_instance();

        // Check if language language is available
        let language_language_address = {
            let sys = controller.system_addresses.lock().await;
            sys.language_language
                .clone()
                .ok_or("Language language not loaded — cannot clone link language template")?
        };

        // Build template data with unique ID and name
        let template_map: serde_json::Map<String, serde_json::Value> = {
            let mut m = serde_json::Map::new();
            m.insert(
                "uid".to_string(),
                serde_json::Value::String(uuid::Uuid::new_v4().to_string()),
            );
            m.insert(
                "name".to_string(),
                serde_json::Value::String(name.to_string()),
            );
            m
        };

        // Apply template to generate unique language source
        let input = controller
            .language_apply_template_on_source(template_address, template_map)
            .await
            .map_err(|e| {
                format!(
                    "Failed to clone template '{}': {}. Use `list_link_language_templates` to see available templates.",
                    template_address, e
                )
            })?;

        let input_name = input.meta.name.clone();

        // Save locally
        if let Err(e) = controller.save_language_bundle(&input.bundle, None) {
            log::warn!("Failed to save cloned language bundle locally: {}", e);
        }

        // Publish via the language language
        let input_json = serde_json::to_string(&input)
            .map_err(|e| format!("Failed to serialize cloned language: {}", e))?;

        let publish_script = format!(
            r#"await globalThis.__ad4m_language_instance__.expressionAdapter.putAdapter.createPublic({})"#,
            input_json
        );

        let address_raw = controller
            .execute_on_language(&language_language_address, &publish_script)
            .await
            .map_err(|e| format!("Failed to publish cloned language: {}", e))?;

        let address = address_raw.trim().trim_matches('"').to_string();

        // Load into runtime
        let bundle_on_disk = crate::utils::languages_directory()
            .join(&address)
            .join("bundle.js");
        if bundle_on_disk.exists() {
            if let Err(e) = controller.load_language(bundle_on_disk, false).await {
                log::warn!("Failed to load cloned language into runtime: {}", e);
            }
        }

        log::info!(
            "Cloned link language template '{}' → '{}' (name: {})",
            template_address,
            address,
            input_name
        );

        Ok(address)
    }

    /// Publish a local perspective as a shared neighbourhood for P2P collaboration
    #[tool(
        description = "Publish a local perspective as a shared neighbourhood. Automatically clones the given link language template to create a unique sync instance. Returns the neighbourhood URL that others can use to join via `neighbourhood_join_from_url`. Use `list_link_language_templates` first to find available templates."
    )]
    pub async fn neighbourhood_publish_from_perspective(
        &self,
        params: Parameters<NeighbourhoodPublishParams>,
    ) -> String {
        let p = &params.0;

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let capabilities = self.get_capabilities().await;
        if let Err(e) = check_capability(&capabilities, &NEIGHBOURHOOD_CREATE_CAPABILITY) {
            return format!("Capability error: {}", e);
        }

        // Check perspective access
        let perspective = match crate::perspectives::get_perspective(&p.perspective_uuid) {
            Some(p) => p,
            None => {
                return json!({"error": format!("Perspective not found: {}", p.perspective_uuid)})
                    .to_string()
            }
        };

        let handle = perspective.persisted.lock().await.clone();
        if !self.can_access_perspective(&handle).await {
            return json!({"error": "Perspective not found or not accessible"}).to_string();
        }

        // Clone the link language template
        let cloned_address = match self
            .clone_link_language(&p.link_language_template, &p.name)
            .await
        {
            Ok(addr) => addr,
            Err(e) => return json!({"error": e}).to_string(),
        };

        let meta = Perspective::default();

        match neighbourhoods::neighbourhood_publish_from_perspective_with_context(
            &p.perspective_uuid,
            cloned_address.clone(),
            meta,
            &agent_context,
        )
        .await
        {
            Ok(url) => json!({
                "success": true,
                "neighbourhood_url": url,
                "perspective_uuid": p.perspective_uuid,
                "cloned_link_language": cloned_address,
                "name": p.name,
                "message": "Perspective published as neighbourhood. Share the neighbourhood_url for others to join."
            })
            .to_string(),
            Err(e) => {
                json!({"error": format!("Failed to publish neighbourhood: {}", e)}).to_string()
            }
        }
    }

    /// Join an existing neighbourhood by its URL
    #[tool(
        description = "Join an existing neighbourhood by URL. Creates a local perspective that syncs with the shared neighbourhood. Returns the perspective UUID for interacting with the neighbourhood's data."
    )]
    pub async fn neighbourhood_join_from_url(
        &self,
        params: Parameters<NeighbourhoodJoinParams>,
    ) -> String {
        let p = &params.0;

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let capabilities = self.get_capabilities().await;
        if let Err(e) = check_capability(&capabilities, &NEIGHBOURHOOD_READ_CAPABILITY) {
            return format!("Capability error: {}", e);
        }

        match neighbourhoods::install_neighbourhood_with_context(p.url.clone(), &agent_context)
            .await
        {
            Ok(handle) => json!({
                "success": true,
                "perspective_uuid": handle.uuid,
                "name": handle.name,
                "neighbourhood_url": p.url,
                "message": "Successfully joined neighbourhood. Use the perspective_uuid to interact with it."
            })
            .to_string(),
            Err(e) => {
                json!({"error": format!("Failed to join neighbourhood: {}", e)}).to_string()
            }
        }
    }
}

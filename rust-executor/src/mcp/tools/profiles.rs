//! Agent profile tools
//!
//! Tools for managing agent identity, profile fields, and profile pictures.
//! Uses native Rust agent service calls (no JS runtime required).

use super::Ad4mMcpHandler;
use crate::agent::capabilities::user_email_from_token;
use crate::agent::{create_signed_expression, AgentContext, AgentService};
use crate::graphql::graphql_types::{Agent, Perspective};
use crate::languages::LanguageController;
use crate::types::{DecoratedLinkExpression, Link, VerifiedExpression};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for getting the agent's public profile
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetAgentProfileParams {}

/// Parameters for setting the agent's profile
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetAgentProfileParams {
    /// Display username
    pub username: Option<String>,
    /// Given (first) name
    pub given_name: Option<String>,
    /// Family (last) name
    pub family_name: Option<String>,
    /// Email address
    pub email: Option<String>,
    /// Bio/description text
    pub bio: Option<String>,
}

/// Parameters for setting the agent's profile picture
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetAgentProfilePictureParams {
    /// Base64-encoded image data (raw base64, NOT a data URI)
    pub image_base64: String,
    /// Image MIME type (e.g. "image/png", "image/jpeg"). Defaults to "image/png"
    pub mime_type: Option<String>,
}

/// Parameters for getting an agent's public perspective (raw links)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetAgentPublicPerspectiveParams {
    /// Agent DID to look up. If empty, returns your own public perspective.
    pub did: Option<String>,
}

/// Parameters for setting the agent's public perspective (raw links)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetAgentPublicPerspectiveParams {
    /// JSON array of links to set as the public perspective.
    /// Each link: {"source": "...", "predicate": "...", "target": "..."}
    /// WARNING: This replaces the entire public perspective — include ALL links you want to keep.
    pub links_json: String,
}

// ============================================================================
// Helper: get agent via native Rust calls
// ============================================================================

/// Get the current agent, handling both multi-user and single-user modes.
fn get_current_agent(auth_token: &str) -> Result<Agent, String> {
    let context = AgentContext::from_auth_token(auth_token.to_string());
    match AgentService::get_agent_for_context(&context) {
        Ok(agent) => Ok(agent),
        Err(_) if context.user_email.is_some() => {
            // Multi-user fallback: user exists in wallet but has no profile yet
            let user_email = context.user_email.as_ref().unwrap();
            let agent_data = AgentService::get_user_agent_data(user_email)
                .map_err(|e| format!("User agent not available: {}", e))?;
            Ok(Agent {
                did: agent_data.did,
                direct_message_language: None,
                perspective: Some(Perspective { links: vec![] }),
            })
        }
        Err(e) => Err(format!("Agent not available: {}", e)),
    }
}

/// Update the current agent's public perspective, handling both multi-user and single-user modes.
async fn update_agent_perspective(
    auth_token: &str,
    links: Vec<DecoratedLinkExpression>,
) -> Result<Agent, String> {
    let context = AgentContext::from_auth_token(auth_token.to_string());

    if let Some(user_email) = user_email_from_token(auth_token.to_string()) {
        // Multi-user mode
        let agent_data = AgentService::get_user_agent_data(&user_email)
            .map_err(|e| format!("User agent not available: {}", e))?;

        let agent = Agent {
            did: agent_data.did,
            direct_message_language: None,
            perspective: Some(Perspective { links }),
        };

        AgentService::with_global_instance(|agent_service| {
            agent_service.store_user_agent_profile(&user_email, &agent)
        })
        .map_err(|e| format!("Failed to store user profile: {}", e))?;

        if let Err(e) = AgentService::publish_agent_to_language(&context).await {
            log::warn!("Failed to publish user profile to agent language: {}", e);
        }

        Ok(agent)
    } else {
        // Single-user / admin mode
        let agent = AgentService::with_mutable_global_instance(|agent_service| {
            if let Some(ref mut agent) = agent_service.agent {
                agent.perspective = Some(Perspective { links });
                let updated = agent.clone();
                if let Some(ref passphrase) = agent_service.passphrase {
                    agent_service.save(passphrase.clone());
                }
                Ok(updated)
            } else {
                Err("Agent not initialized".to_string())
            }
        })?;

        if let Err(e) = AgentService::publish_agent_to_language(&AgentContext::main_agent()).await {
            log::warn!("Failed to publish agent expression: {}", e);
        }

        Ok(agent)
    }
}

/// Build a properly signed DecoratedLinkExpression for a profile link.
fn make_profile_link(
    source: &str,
    predicate: &str,
    target: &str,
    context: &AgentContext,
) -> Result<DecoratedLinkExpression, String> {
    let link = Link {
        source: source.to_string(),
        predicate: Some(predicate.to_string()),
        target: target.to_string(),
    };
    let signed = create_signed_expression(link, context).map_err(|e| e.to_string())?;
    let verified: VerifiedExpression<Link> = signed.into();
    Ok(DecoratedLinkExpression {
        author: verified.author,
        timestamp: verified.timestamp,
        data: verified.data,
        proof: verified.proof,
        status: None,
    })
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Get the current agent's public profile
    #[tool(
        description = "Get the current agent's public profile (username, name, bio, profile picture URLs). This is the identity that other agents and Flux users see in neighbourhoods."
    )]
    pub async fn get_agent_profile(&self, _params: Parameters<GetAgentProfileParams>) -> String {
        let token = self.get_auth_token().await.unwrap_or_default();

        let agent = match get_current_agent(&token) {
            Ok(a) => a,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };

        let mut profile = json!({"did": agent.did});

        let predicates = vec![
            ("sioc://has_username", "username"),
            ("sioc://has_given_name", "given_name"),
            ("sioc://has_family_name", "family_name"),
            ("sioc://has_email", "email"),
            ("sioc://has_bio", "bio"),
            ("sioc://has_profile_image", "profile_image"),
            ("sioc://has_profile_thumbnail_image", "profile_thumbnail"),
        ];

        if let Some(ref perspective) = agent.perspective {
            for link in &perspective.links {
                if link.data.source == "flux://profile" {
                    let predicate = link.data.predicate.as_deref().unwrap_or("");
                    for (pred_uri, field_name) in &predicates {
                        if predicate == *pred_uri {
                            let value = Self::resolve_literal_value(&link.data.target);
                            profile[field_name] = json!(value);
                        }
                    }
                }
            }
        }

        profile.to_string()
    }

    /// Set the current agent's public profile fields
    #[tool(
        description = "Set the current agent's public profile (username, name, bio, email). These fields are visible to other agents and Flux users in neighbourhoods. Only provided fields are updated; omitted fields keep their current values."
    )]
    pub async fn set_agent_profile(&self, params: Parameters<SetAgentProfileParams>) -> String {
        let _capabilities = match self.get_capabilities().await {
            Ok(c) => c,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let token = self.get_auth_token().await.unwrap_or_default();

        let agent = match get_current_agent(&token) {
            Ok(a) => a,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };

        let context = AgentContext::from_auth_token(token.clone());
        let current_links = agent
            .perspective
            .as_ref()
            .map(|p| &p.links[..])
            .unwrap_or(&[]);

        let profile_text_predicates = [
            "sioc://has_username",
            "sioc://has_given_name",
            "sioc://has_family_name",
            "sioc://has_email",
            "sioc://has_bio",
        ];

        let mut current_values = std::collections::HashMap::new();
        let mut preserved_links: Vec<DecoratedLinkExpression> = Vec::new();

        for link in current_links {
            let predicate = link.data.predicate.as_deref().unwrap_or("");
            if link.data.source == "flux://profile" && profile_text_predicates.contains(&predicate)
            {
                current_values.insert(predicate.to_string(), link.data.target.clone());
                continue;
            }
            preserved_links.push(link.clone());
        }

        let p = &params.0;

        let fields: Vec<(&str, Option<&String>)> = vec![
            ("sioc://has_username", p.username.as_ref()),
            ("sioc://has_given_name", p.given_name.as_ref()),
            ("sioc://has_family_name", p.family_name.as_ref()),
            ("sioc://has_email", p.email.as_ref()),
            ("sioc://has_bio", p.bio.as_ref()),
        ];

        let mut all_links = preserved_links;
        for (predicate, new_value) in &fields {
            let target = match new_value {
                Some(v) => Self::encode_literal(v),
                None => match current_values.get(*predicate) {
                    Some(existing) => existing.clone(),
                    None => continue,
                },
            };
            match make_profile_link("flux://profile", predicate, &target, &context) {
                Ok(link) => all_links.push(link),
                Err(e) => {
                    return json!({"error": format!("Failed to sign profile link: {}", e)})
                        .to_string()
                }
            }
        }

        match update_agent_perspective(&token, all_links).await {
            Ok(_) => {
                let mut updated = json!({"success": true});
                if let Some(u) = p.username.as_ref() {
                    updated["username"] = json!(u);
                }
                if let Some(g) = p.given_name.as_ref() {
                    updated["given_name"] = json!(g);
                }
                if let Some(f) = p.family_name.as_ref() {
                    updated["family_name"] = json!(f);
                }
                if let Some(e) = p.email.as_ref() {
                    updated["email"] = json!(e);
                }
                if let Some(b) = p.bio.as_ref() {
                    updated["bio"] = json!(b);
                }
                updated.to_string()
            }
            Err(e) => json!({"error": format!("Failed to update profile: {}", e)}).to_string(),
        }
    }

    /// Set the agent's profile picture
    #[tool(
        description = "Set the current agent's profile picture. Provide raw base64-encoded image data (NOT a data URI). The image will be uploaded to the centralized file store and linked in the agent's public profile. For best results, use a square image (Flux will display it as a circle)."
    )]
    pub async fn set_agent_profile_picture(
        &self,
        params: Parameters<SetAgentProfilePictureParams>,
    ) -> String {
        let _capabilities = match self.get_capabilities().await {
            Ok(c) => c,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let token = self.get_auth_token().await.unwrap_or_default();
        let mime = params.0.mime_type.as_deref().unwrap_or("image/png");

        // Find the file-storage language via the LanguageController
        let controller = LanguageController::global_instance();
        //let all_languages = controller.get_installed_languages(None).await;
        //let file_storage_lang = all_languages
        //    .iter()
        //    .find(|l| l.name.to_lowercase().contains("file-storage"))
        //     .map(|l| l.address.clone());

        //let file_storage_addr = match file_storage_lang {
        //    Some(addr) => addr,
        //    None => {
        //        return json!({"error": "File storage language not found in runtime"}).to_string()
        //    }
        //};

        let file_storage_addr = "QmzSYwdjqeP9D13Sfmyc5HcabM9jL3DtPyhadnF6dQXu4FjVSbQ".to_string();

        // Ensure the file-storage language is loaded before trying to create an expression.
        // language_by_ref() downloads and installs the language if it isn't already running.
        if !controller.is_language_loaded(&file_storage_addr).await {
            if let Err(e) = controller.language_by_ref(&file_storage_addr).await {
                return json!({"error": format!("Failed to load file-storage language: {}", e)})
                    .to_string();
            }
        }

        // Create expression via the language controller
        let content = json!({
            "data_base64": params.0.image_base64,
            "name": "profile-image",
            "file_type": mime,
        });
        let agent_context = crate::agent::AgentContext::from_auth_token(token.clone());
        let profile_img = match controller
            .expression_create(&file_storage_addr, content, &agent_context)
            .await
        {
            Ok(addr) => addr,
            Err(e) => {
                return json!({"error": format!("Failed to upload image: {}", e)}).to_string()
            }
        };

        // Get current agent and rebuild links with the new profile image
        let agent = match get_current_agent(&token) {
            Ok(a) => a,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };

        let context = AgentContext::from_auth_token(token.clone());
        let current_links = agent
            .perspective
            .as_ref()
            .map(|p| &p.links[..])
            .unwrap_or(&[]);

        let mut all_links: Vec<DecoratedLinkExpression> = Vec::new();
        for link in current_links {
            let predicate = link.data.predicate.as_deref().unwrap_or("");
            if predicate == "sioc://has_profile_image"
                || predicate == "sioc://has_profile_thumbnail_image"
            {
                continue;
            }
            all_links.push(link.clone());
        }

        let image_link = match make_profile_link(
            "flux://profile",
            "sioc://has_profile_image",
            &profile_img,
            &context,
        ) {
            Ok(l) => l,
            Err(e) => {
                return json!({"error": format!("Failed to sign profile link: {}", e)}).to_string()
            }
        };
        let thumb_link = match make_profile_link(
            "flux://profile",
            "sioc://has_profile_thumbnail_image",
            &profile_img,
            &context,
        ) {
            Ok(l) => l,
            Err(e) => {
                return json!({"error": format!("Failed to sign profile link: {}", e)}).to_string()
            }
        };
        all_links.push(image_link);
        all_links.push(thumb_link);

        match update_agent_perspective(&token, all_links).await {
            Ok(_) => json!({
                "success": true,
                "profile_image": profile_img,
                "message": "Profile picture updated. For best results in Flux, use a square image."
            })
            .to_string(),
            Err(e) => json!({"error": format!("Failed to update profile: {}", e)}).to_string(),
        }
    }

    /// Get an agent's public perspective as raw links
    #[tool(
        description = "Get an agent's public perspective — the set of links they publish publicly via their DID. Contains profile info, capabilities, and any other public statements. Pass a DID to look up another agent, or omit to get your own. Returns a JSON array of links."
    )]
    pub async fn get_agent_public_perspective(
        &self,
        params: Parameters<GetAgentPublicPerspectiveParams>,
    ) -> String {
        let token = self.get_auth_token().await.unwrap_or_default();

        if let Some(did) = &params.0.did {
            // Look up another agent by DID via the agent language
            let controller = LanguageController::global_instance();
            let agent_lang = match controller.get_agent_language().await {
                Ok(lang) => lang,
                Err(e) => {
                    return json!({"error": format!("Agent language not available: {}", e)})
                        .to_string()
                }
            };
            let lang_address = agent_lang.address().to_string();
            match controller.get_expression(&lang_address, did).await {
                Ok(Some(expr_json)) => {
                    let agent: Option<Agent> = serde_json::from_value(
                        expr_json
                            .get("data")
                            .cloned()
                            .unwrap_or(serde_json::Value::Null),
                    )
                    .ok();
                    match agent {
                        Some(a) => serde_json::to_string(&a).unwrap_or_else(|_| "null".into()),
                        None => json!({"error": "Agent not found"}).to_string(),
                    }
                }
                Ok(None) => json!({"error": "Agent not found"}).to_string(),
                Err(e) => {
                    json!({"error": format!("Failed to get agent perspective: {}", e)}).to_string()
                }
            }
        } else {
            // Get own agent
            match get_current_agent(&token) {
                Ok(agent) => serde_json::to_string(&agent).unwrap_or_else(|_| "null".into()),
                Err(e) => {
                    json!({"error": format!("Failed to get agent perspective: {}", e)}).to_string()
                }
            }
        }
    }

    /// Set the agent's public perspective from raw links
    #[tool(
        description = "Set the current agent's public perspective — replaces ALL public links with the provided array. This is the low-level API; use set_agent_profile for structured profile updates. WARNING: This replaces the entire perspective, so include all links you want to keep. Format: [{\"source\": \"...\", \"predicate\": \"...\", \"target\": \"...\"}]."
    )]
    pub async fn set_agent_public_perspective(
        &self,
        params: Parameters<SetAgentPublicPerspectiveParams>,
    ) -> String {
        let _agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let token = self.get_auth_token().await.unwrap_or_default();

        let links: Vec<DecoratedLinkExpression> = match serde_json::from_str(&params.0.links_json) {
            Ok(l) => l,
            Err(e) => return json!({"error": format!("Invalid links JSON: {}", e)}).to_string(),
        };

        match update_agent_perspective(&token, links).await {
            Ok(agent) => serde_json::to_string(&agent).unwrap_or_else(|_| "null".into()),
            Err(e) => {
                json!({"error": format!("Failed to update agent perspective: {}", e)}).to_string()
            }
        }
    }
}

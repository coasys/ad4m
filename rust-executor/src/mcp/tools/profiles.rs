//! Agent profile tools
//!
//! Tools for managing agent identity, profile fields, and profile pictures.

use super::Ad4mMcpHandler;
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
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Get the current agent's public profile
    #[tool(
        description = "Get the current agent's public profile (username, name, bio, profile picture URLs). This is the identity that other agents and Flux users see in neighbourhoods."
    )]
    pub async fn get_agent_profile(&self, _params: Parameters<GetAgentProfileParams>) -> String {
        let _agent_context = self.get_agent_context_for_read().await;

        let mut js = self.context.js_handle.clone();
        let script = r#"JSON.stringify(await core.callResolver("Query", "agent", {}))"#;

        let result = match js.execute(script.to_string()).await {
            Ok(r) => r,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };

        let agent: serde_json::Value = match serde_json::from_str(&result) {
            Ok(v) => v,
            Err(e) => return json!({"error": format!("Failed to parse agent: {}", e)}).to_string(),
        };

        let did = agent.get("did").and_then(|v| v.as_str()).unwrap_or("");
        let links = agent
            .get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array());

        let mut profile = json!({"did": did});

        let predicates = vec![
            ("sioc://has_username", "username"),
            ("sioc://has_given_name", "given_name"),
            ("sioc://has_family_name", "family_name"),
            ("sioc://has_email", "email"),
            ("sioc://has_bio", "bio"),
            ("sioc://has_profile_image", "profile_image"),
            ("sioc://has_profile_thumbnail_image", "profile_thumbnail"),
        ];

        if let Some(links) = links {
            for link in links {
                let source = link
                    .pointer("/data/source")
                    .and_then(|v| v.as_str())
                    .unwrap_or("");
                let predicate = link
                    .pointer("/data/predicate")
                    .and_then(|v| v.as_str())
                    .unwrap_or("");
                let target = link
                    .pointer("/data/target")
                    .and_then(|v| v.as_str())
                    .unwrap_or("");

                if source == "flux://profile" {
                    for (pred_uri, field_name) in &predicates {
                        if predicate == *pred_uri {
                            let value = Self::resolve_literal_value(target);
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

        let mut js = self.context.js_handle.clone();
        let script = r#"JSON.stringify(await core.callResolver("Query", "agent", {}))"#;
        let result = match js.execute(script.to_string()).await {
            Ok(r) => r,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };
        let agent: serde_json::Value = match serde_json::from_str(&result) {
            Ok(v) => v,
            Err(e) => return json!({"error": format!("Failed to parse agent: {}", e)}).to_string(),
        };

        let did = agent
            .get("did")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let current_links = agent
            .get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array())
            .cloned()
            .unwrap_or_default();

        let profile_text_predicates = vec![
            "sioc://has_username",
            "sioc://has_given_name",
            "sioc://has_family_name",
            "sioc://has_email",
            "sioc://has_bio",
        ];

        let mut current_values = std::collections::HashMap::new();
        let mut preserved_links: Vec<serde_json::Value> = Vec::new();

        for link in &current_links {
            let source = link
                .pointer("/data/source")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let predicate = link
                .pointer("/data/predicate")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let target = link
                .pointer("/data/target")
                .and_then(|v| v.as_str())
                .unwrap_or("");

            if source == "flux://profile" && profile_text_predicates.contains(&predicate) {
                current_values.insert(predicate.to_string(), target.to_string());
                continue;
            }
            preserved_links.push(json!({
                "author": did,
                "timestamp": link.get("timestamp").and_then(|v| v.as_str()).unwrap_or(""),
                "proof": {"invalid": false, "key": "", "signature": ""},
                "data": {"source": source, "predicate": predicate, "target": target}
            }));
        }

        let p = &params.0;
        let now = chrono::Utc::now();

        let fields: Vec<(&str, Option<&String>)> = vec![
            ("sioc://has_username", p.username.as_ref()),
            ("sioc://has_given_name", p.given_name.as_ref()),
            ("sioc://has_family_name", p.family_name.as_ref()),
            ("sioc://has_email", p.email.as_ref()),
            ("sioc://has_bio", p.bio.as_ref()),
        ];

        let mut all_links = preserved_links;
        for (i, (predicate, new_value)) in fields.iter().enumerate() {
            let target = match new_value {
                Some(v) => format!("literal://string:{}", v),
                None => match current_values.get(*predicate) {
                    Some(existing) => existing.clone(),
                    None => continue,
                },
            };
            let ts = now + chrono::Duration::milliseconds(i as i64);
            all_links.push(json!({
                "author": did,
                "timestamp": ts.to_rfc3339(),
                "proof": {"invalid": false, "key": "", "signature": ""},
                "data": {"source": "flux://profile", "predicate": predicate, "target": target}
            }));
        }

        let perspective_json = json!({"links": all_links});
        let update_script = format!(
            r#"JSON.stringify(await core.callResolver("Mutation", "agentUpdatePublicPerspective", {{ perspective: {} }}))"#,
            serde_json::to_string(&perspective_json).unwrap()
        );

        match js.execute(update_script).await {
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

        let mut js = self.context.js_handle.clone();
        let mime = params.0.mime_type.as_deref().unwrap_or("image/png");

        // Query file storage language address from the runtime instead of hardcoding
        let get_file_storage_script = r#"
            const langs = await core.callResolver("Query", "languages", { filter: "" });
            const parsed = JSON.parse(langs);
            const fileStorage = parsed.find(l => l.name && l.name.toLowerCase().includes("file-storage"));
            JSON.stringify(fileStorage ? fileStorage.address : null)
        "#;
        let file_storage_lang = match js.execute(get_file_storage_script.to_string()).await {
            Ok(r) => {
                let addr: Option<String> = serde_json::from_str(&r).unwrap_or(None);
                match addr {
                    Some(a) => a,
                    None => {
                        return json!({"error": "File storage language not found in runtime"})
                            .to_string()
                    }
                }
            }
            Err(e) => {
                return json!({"error": format!("Failed to query file storage language: {}", e)})
                    .to_string()
            }
        };

        let content = json!({
            "data_base64": params.0.image_base64,
            "name": "profile-image",
            "file_type": mime,
        });
        let create_script = format!(
            r#"JSON.stringify(await core.callResolver("Mutation", "expressionCreate", {{ content: {}, languageAddress: {:?} }}))"#,
            serde_json::to_string(&serde_json::to_string(&content).unwrap()).unwrap(),
            file_storage_lang
        );

        let profile_img = match js.execute(create_script).await {
            Ok(r) => {
                let addr: String = serde_json::from_str(&r).unwrap_or(r);
                addr
            }
            Err(e) => {
                return json!({"error": format!("Failed to upload image: {}", e)}).to_string()
            }
        };

        let script = r#"JSON.stringify(await core.callResolver("Query", "agent", {}))"#;
        let result = match js.execute(script.to_string()).await {
            Ok(r) => r,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };
        let agent: serde_json::Value = serde_json::from_str(&result).unwrap_or(json!({}));
        let did = agent
            .get("did")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let current_links = agent
            .get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array())
            .cloned()
            .unwrap_or_default();

        let mut all_links: Vec<serde_json::Value> = Vec::new();
        for link in &current_links {
            let predicate = link
                .pointer("/data/predicate")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            if predicate == "sioc://has_profile_image"
                || predicate == "sioc://has_profile_thumbnail_image"
            {
                continue;
            }
            let source = link
                .pointer("/data/source")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let target = link
                .pointer("/data/target")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            all_links.push(json!({
                "author": did,
                "timestamp": link.get("timestamp").and_then(|v| v.as_str()).unwrap_or(""),
                "proof": {"invalid": false, "key": "", "signature": ""},
                "data": {"source": source, "predicate": predicate, "target": target}
            }));
        }

        let now = chrono::Utc::now().to_rfc3339();
        all_links.push(json!({
            "author": did, "timestamp": now,
            "proof": {"invalid": false, "key": "", "signature": ""},
            "data": {"source": "flux://profile", "predicate": "sioc://has_profile_image", "target": profile_img}
        }));
        all_links.push(json!({
            "author": did, "timestamp": now,
            "proof": {"invalid": false, "key": "", "signature": ""},
            "data": {"source": "flux://profile", "predicate": "sioc://has_profile_thumbnail_image", "target": profile_img}
        }));

        let perspective_json = json!({"links": all_links});
        let update_script = format!(
            r#"JSON.stringify(await core.callResolver("Mutation", "agentUpdatePublicPerspective", {{ perspective: {} }}))"#,
            serde_json::to_string(&perspective_json).unwrap()
        );

        match js.execute(update_script).await {
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
        let _agent_context = self.get_agent_context_for_read().await;

        let mut js = self.context.js_handle.clone();

        let js_code = if let Some(did) = &params.0.did {
            format!(
                "JSON.stringify(await core.callResolver(\"Query\", \"agentByDID\", {{ did: \"{}\" }}))",
                did
            )
        } else {
            "JSON.stringify(await core.callResolver(\"Query\", \"agent\", {}))".to_string()
        };

        match js.execute(js_code).await {
            Ok(result) => result,
            Err(e) => {
                json!({"error": format!("Failed to get agent perspective: {}", e)}).to_string()
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

        let links: Vec<serde_json::Value> = match serde_json::from_str(&params.0.links_json) {
            Ok(l) => l,
            Err(e) => return json!({"error": format!("Invalid links JSON: {}", e)}).to_string(),
        };

        let perspective = json!({ "links": links });

        let mut js = self.context.js_handle.clone();

        let js_code = format!(
            "JSON.stringify(await core.callResolver(\"Mutation\", \"agentUpdatePublicPerspective\", {{ perspective: {} }}))",
            perspective
        );

        match js.execute(js_code).await {
            Ok(result) => result,
            Err(e) => {
                json!({"error": format!("Failed to update agent perspective: {}", e)}).to_string()
            }
        }
    }
}

//! Agent profile tools
//!
//! Tools for managing agent identity, profile fields, and profile pictures. They read and
//! write the caller's agent through the RPC methods `agent.get`, `agent.byDid`,
//! `agent.updateProfile` and `expression.create`, called through the handler map, so those
//! methods' guards apply.

use super::{decode, method_error, Ad4mMcpHandler};
use crate::agent::{create_signed_expression, AgentContext};
use crate::api::ws_handler::WsRpcError;
use crate::languages::LanguageController;
use crate::types::Agent;
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

/// The file-storage language that holds profile pictures.
const FILE_STORAGE_LANGUAGE: &str = "QmzSYwddqhm49PrRMzSrJf3AvmmreXMKtr1u56nbTjBFVmCzS8N";

/// The profile text fields: `(predicate, field name)`.
const PROFILE_FIELDS: [(&str, &str); 5] = [
    ("sioc://has_username", "username"),
    ("sioc://has_given_name", "given_name"),
    ("sioc://has_family_name", "family_name"),
    ("sioc://has_email", "email"),
    ("sioc://has_bio", "bio"),
];

// ============================================================================
// Helpers
// ============================================================================

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

/// A tool's answer when `agent.get` or `agent.updateProfile` fails: a refusal as the method
/// gave it, any other failure under `what`.
fn agent_error(what: &str, e: WsRpcError) -> String {
    match e.code {
        401 | 403 => method_error(e),
        _ => json!({ "error": format!("{what}: {}", e.message) }).to_string(),
    }
}

impl Ad4mMcpHandler {
    /// The caller's own agent (`agent.get`): a user's, or the node's main agent.
    async fn current_agent(&self) -> Result<Agent, WsRpcError> {
        let agent = self.call_method("agent.get", json!({})).await?;
        decode(agent).map_err(WsRpcError::internal)
    }

    /// Replace the caller's public perspective with `links` (`agent.updateProfile`). The
    /// method verifies every link's signature again.
    async fn set_public_links(
        &self,
        links: &[DecoratedLinkExpression],
    ) -> Result<Agent, WsRpcError> {
        let params = json!({ "publicPerspective": { "links": links } });
        let agent = self.call_method("agent.updateProfile", params).await?;
        decode(agent).map_err(WsRpcError::internal)
    }

    /// The caller's agent, to sign profile links as.
    async fn signer(&self) -> Result<AgentContext, String> {
        self.get_agent_context()
            .await
            .map_err(|e| method_error(WsRpcError::unauthorized(e)))
    }
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Get the current agent's DID
    #[tool(
        description = "Get the DID (Decentralized Identifier) of the current agent. Use this to identify your own messages when filtering — compare the 'author' field in message data against your DID."
    )]
    pub async fn get_my_did(&self, _params: Parameters<GetAgentProfileParams>) -> String {
        match self.current_agent().await {
            Ok(agent) => agent.did,
            Err(e) => agent_error("Failed to get agent", e),
        }
    }

    /// Get the current agent's public profile
    #[tool(
        description = "Get the current agent's public profile (username, name, bio, profile picture URLs). This is the identity that other agents and human users see in neighbourhoods."
    )]
    pub async fn get_agent_profile(&self, _params: Parameters<GetAgentProfileParams>) -> String {
        let agent = match self.current_agent().await {
            Ok(a) => a,
            Err(e) => return agent_error("Failed to get agent", e),
        };

        let mut profile = json!({"did": agent.did});

        let predicates = PROFILE_FIELDS.iter().chain(&[
            ("sioc://has_profile_image", "profile_image"),
            ("sioc://has_profile_thumbnail_image", "profile_thumbnail"),
        ]);

        if let Some(ref perspective) = agent.perspective {
            for (pred_uri, field_name) in predicates {
                for link in &perspective.links {
                    if link.data.source == "flux://profile"
                        && link.data.predicate.as_deref() == Some(*pred_uri)
                    {
                        profile[*field_name] =
                            json!(Self::resolve_literal_value(&link.data.target));
                    }
                }
            }
        }

        profile.to_string()
    }

    /// Set the current agent's public profile fields
    #[tool(
        description = "Set the current agent's public profile (username, name, bio, email). These fields are visible to other agents and to the human users of whichever app renders the neighbourhood. Only provided fields are updated; omitted fields keep their current values."
    )]
    pub async fn set_agent_profile(&self, params: Parameters<SetAgentProfileParams>) -> String {
        let agent = match self.current_agent().await {
            Ok(a) => a,
            Err(e) => return agent_error("Failed to get agent", e),
        };
        let context = match self.signer().await {
            Ok(context) => context,
            Err(e) => return e,
        };

        let current_links = agent
            .perspective
            .as_ref()
            .map(|p| &p.links[..])
            .unwrap_or(&[]);

        let mut current_values = std::collections::HashMap::new();
        let mut preserved_links: Vec<DecoratedLinkExpression> = Vec::new();

        for link in current_links {
            let predicate = link.data.predicate.as_deref().unwrap_or("");
            if link.data.source == "flux://profile"
                && PROFILE_FIELDS.iter().any(|(p, _)| *p == predicate)
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

        match self.set_public_links(&all_links).await {
            Ok(_) => {
                let mut updated = json!({"success": true});
                for ((_, field), value) in PROFILE_FIELDS.iter().zip([
                    &p.username,
                    &p.given_name,
                    &p.family_name,
                    &p.email,
                    &p.bio,
                ]) {
                    if let Some(value) = value {
                        updated[*field] = json!(value);
                    }
                }
                updated.to_string()
            }
            Err(e) => agent_error("Failed to update profile", e),
        }
    }

    /// Set the agent's profile picture
    #[tool(
        description = "Set the current agent's profile picture. Provide raw base64-encoded image data (NOT a data URI). The image will be uploaded to the centralized file store and linked in the agent's public profile. For best results, use a square image (apps usually crop profile pictures to a circle)."
    )]
    pub async fn set_agent_profile_picture(
        &self,
        params: Parameters<SetAgentProfilePictureParams>,
    ) -> String {
        let mime = params.0.mime_type.as_deref().unwrap_or("image/png");
        let content = json!({
            "data_base64": params.0.image_base64,
            "name": "profile-image",
            "file_type": mime,
        });
        let create = json!({
            "languageAddress": FILE_STORAGE_LANGUAGE,
            "content": content.to_string(),
        });

        // `expression.create` decides who may store the image. Its guard runs before the
        // file-storage language loads, so a refused caller installs nothing.
        if let Err(e) = self.admit_method("expression.create", &create).await {
            return method_error(e);
        }

        // language_by_ref() downloads and installs the language if it isn't already running.
        let controller = LanguageController::global_instance();
        if !controller.is_language_loaded(FILE_STORAGE_LANGUAGE).await {
            if let Err(e) = controller.language_by_ref(FILE_STORAGE_LANGUAGE).await {
                return json!({"error": format!("Failed to load file-storage language: {}", e)})
                    .to_string();
            }
        }

        let profile_img: String = match self.call_method("expression.create", create).await {
            Ok(address) => match decode(address) {
                Ok(address) => address,
                Err(e) => return e,
            },
            Err(e) => return agent_error("Failed to upload image", e),
        };

        // Get current agent and rebuild links with the new profile image
        let agent = match self.current_agent().await {
            Ok(a) => a,
            Err(e) => return agent_error("Failed to get agent", e),
        };
        let context = match self.signer().await {
            Ok(context) => context,
            Err(e) => return e,
        };

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

        for predicate in [
            "sioc://has_profile_image",
            "sioc://has_profile_thumbnail_image",
        ] {
            match make_profile_link("flux://profile", predicate, &profile_img, &context) {
                Ok(link) => all_links.push(link),
                Err(e) => {
                    return json!({"error": format!("Failed to sign profile link: {}", e)})
                        .to_string()
                }
            }
        }

        match self.set_public_links(&all_links).await {
            Ok(_) => json!({
                "success": true,
                "profile_image": profile_img,
                "message": "Profile picture updated. Use a square image for best results — apps usually crop it to a circle."
            })
            .to_string(),
            Err(e) => agent_error("Failed to update profile", e),
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
        let agent = match &params.0.did {
            // Another agent, through the agent language (the main agent's own DID answers
            // locally).
            Some(did) => match self.call_method("agent.byDid", json!({ "did": did })).await {
                Ok(agent) => decode::<Option<Agent>>(agent),
                Err(e) => return agent_error("Failed to get agent perspective", e),
            },
            None => self
                .current_agent()
                .await
                .map(Some)
                .map_err(|e| agent_error("Failed to get agent perspective", e)),
        };
        match agent {
            Ok(Some(agent)) => serde_json::to_string(&agent).unwrap_or_else(|_| "null".into()),
            Ok(None) => json!({"error": "Agent not found"}).to_string(),
            Err(e) => e,
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
        let links: Vec<DecoratedLinkExpression> = match serde_json::from_str(&params.0.links_json) {
            Ok(l) => l,
            Err(e) => return json!({"error": format!("Invalid links JSON: {}", e)}).to_string(),
        };

        match self.set_public_links(&links).await {
            Ok(agent) => serde_json::to_string(&agent).unwrap_or_else(|_| "null".into()),
            Err(e) => agent_error("Failed to update agent perspective", e),
        }
    }
}

/// The tools answer as their RPC methods' guards do (#851).
#[cfg(test)]
mod guard_tests {
    use super::*;
    use crate::agent::capabilities::{AGENT_READ_CAPABILITY, AGENT_UPDATE_CAPABILITY};
    use crate::agent::session::Principal;
    use crate::mcp::tools::test_support::{app, handler_as, setup, status};

    fn no_params() -> Parameters<GetAgentProfileParams> {
        Parameters(GetAgentProfileParams {})
    }

    // The profile tools checked no capability: any app token read and wrote the node's
    // profile.
    #[tokio::test]
    async fn reading_the_profile_needs_the_agent_read_capability() {
        setup();
        let refused = handler_as(app(vec![])).get_my_did(no_params()).await;
        assert_eq!(status(&refused), Some(403), "{refused}");
        let refused = handler_as(app(vec![])).get_agent_profile(no_params()).await;
        assert_eq!(status(&refused), Some(403), "{refused}");

        let reader = handler_as(app(vec![AGENT_READ_CAPABILITY.clone()]));
        assert_eq!(reader.get_my_did(no_params()).await, crate::agent::did());
        assert_eq!(
            handler_as(Principal::Operator)
                .get_my_did(no_params())
                .await,
            crate::agent::did()
        );
    }

    #[tokio::test]
    async fn writing_the_profile_needs_the_agent_update_capability() {
        setup();
        let set = |principal| async move {
            handler_as(principal)
                .set_agent_profile(Parameters(SetAgentProfileParams {
                    username: Some("mcp-name".into()),
                    given_name: None,
                    family_name: None,
                    email: None,
                    bio: None,
                }))
                .await
        };
        let read_only = set(app(vec![AGENT_READ_CAPABILITY.clone()])).await;
        assert_eq!(status(&read_only), Some(403), "{read_only}");
        assert!(
            read_only.contains("Capability is not matched"),
            "{read_only}"
        );

        let refused = handler_as(app(vec![AGENT_READ_CAPABILITY.clone()]))
            .set_agent_public_perspective(Parameters(SetAgentPublicPerspectiveParams {
                links_json: "[]".into(),
            }))
            .await;
        assert_eq!(status(&refused), Some(403), "{refused}");

        let updated = set(app(vec![
            AGENT_READ_CAPABILITY.clone(),
            AGENT_UPDATE_CAPABILITY.clone(),
        ]))
        .await;
        let updated: serde_json::Value = serde_json::from_str(&updated).expect(&updated);
        assert_eq!(updated["success"], true, "{updated}");
        assert_eq!(updated["username"], "mcp-name");
        let profile = handler_as(Principal::Operator)
            .get_agent_profile(no_params())
            .await;
        assert!(profile.contains("mcp-name"), "{profile}");
    }
}

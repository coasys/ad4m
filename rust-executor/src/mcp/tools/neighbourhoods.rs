//! Neighbourhood tools — publish perspectives as neighbourhoods and join existing ones.

use super::Ad4mMcpHandler;
use crate::agent::capabilities::{
    check_capability,
    defs::{NEIGHBOURHOOD_CREATE_CAPABILITY, NEIGHBOURHOOD_READ_CAPABILITY},
};
use crate::graphql::graphql_types::Perspective;
use crate::neighbourhoods;
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
    /// Address of the link language used for P2P synchronization (e.g. a perspective-diff-sync language)
    pub link_language: String,
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
    /// Publish a local perspective as a shared neighbourhood for P2P collaboration
    #[tool(
        description = "Publish a local perspective as a shared neighbourhood for P2P collaboration. Requires a link language address (e.g. perspective-diff-sync) that handles synchronization between peers. Returns the neighbourhood URL that others can use to join."
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

        let meta = Perspective::default();

        match neighbourhoods::neighbourhood_publish_from_perspective_with_context(
            &p.perspective_uuid,
            p.link_language.clone(),
            meta,
            &agent_context,
        )
        .await
        {
            Ok(url) => json!({
                "success": true,
                "neighbourhood_url": url,
                "perspective_uuid": p.perspective_uuid,
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
        description = "Join an existing neighbourhood by its URL. Creates a local perspective that syncs with the shared neighbourhood via its link language. Returns the perspective handle for the joined neighbourhood."
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

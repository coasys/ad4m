//! Neighbourhood tools — publish perspectives as neighbourhoods and join existing ones.
//!
//! High-level tools for P2P collaboration. Agents select from known link language
//! templates; the system handles cloning and unique instance creation automatically.
//! The tools call the RPC methods `runtime.linkLanguageTemplates`, `neighbourhood.publish` and
//! `neighbourhood.join` through the handler map, so their guards apply.

use super::{labelled_json, method_error, Ad4mMcpHandler};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;
use crate::languages::LanguageController;
use crate::types::{Perspective, PerspectiveHandle};
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
    /// Address of a link language to use for this neighbourhood.
    /// Can be a template address (will be cloned) or an already-cloned language.
    /// Use `list_link_language_templates` to see available templates.
    #[serde(alias = "link_language_template")]
    pub link_language: String,
    /// Optional human-readable name for this neighbourhood (used as the cloned language name).
    /// If not provided, a default name will be generated.
    #[serde(default = "default_neighbourhood_name")]
    pub name: String,
}

fn default_neighbourhood_name() -> String {
    "Neighbourhood".to_string()
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
        description = "List available link language templates that can be used when publishing a neighbourhood. Each template is a P2P synchronization engine. Returns address, name, and description for each template. Pass the address as `link_language` when calling `neighbourhood_publish_from_perspective`."
    )]
    pub async fn list_link_language_templates(&self) -> String {
        let addresses: Vec<String> = match self
            .call(
                "runtime.linkLanguageTemplates",
                json!({}),
                labelled_json("Failed to get link language templates"),
            )
            .await
        {
            Ok(addresses) => addresses,
            Err(e) => return e,
        };

        let templates: Vec<_> = LanguageController::global_instance()
            .language_metas(&addresses)
            .await
            .into_iter()
            .map(|m| {
                json!({
                    "address": m.address,
                    "name": m.name,
                    "description": m.description,
                    "author": m.author,
                    "possible_template_params": m.possible_template_params,
                })
            })
            .collect();

        let result = json!({
            "templates": templates,
            "count": templates.len(),
            "hint": "Pass the address of a template as link_language when publishing a neighbourhood."
        });
        serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
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

        // `neighbourhood.publish` decides who may publish this perspective, and
        // `language.applyTemplate` who may clone and publish a language, the work of
        // `clone_link_language`. Both guards run first, so a refused caller clones nothing.
        let publish = |link_language: &str| {
            json!({
                "perspectiveUuid": p.perspective_uuid,
                "linkLanguage": link_language,
                "meta": Perspective::default(),
            })
        };
        if let Err(e) = self
            .admit_method("neighbourhood.publish", &publish(&p.link_language))
            .await
        {
            return method_error(e);
        }
        let apply_template = json!({
            "sourceLanguageHash": p.link_language,
            "templateData": json!({ "name": p.name }).to_string(),
        });
        if let Err(e) = self
            .admit_method("language.applyTemplate", &apply_template)
            .await
        {
            return method_error(e);
        }
        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return method_error(WsRpcError::unauthorized(e)),
        };

        // Clone the link language template
        let cloned_address =
            match clone_link_language(&p.link_language, &p.name, &agent_context).await {
                Ok(addr) => addr,
                Err(e) => return json!({"error": e}).to_string(),
            };

        let published = self
            .call::<String>(
                "neighbourhood.publish",
                publish(&cloned_address),
                labelled_json("Failed to publish neighbourhood"),
            )
            .await;
        match published {
            Ok(url) => json!({
                "success": true,
                "neighbourhood_url": url,
                "perspective_uuid": p.perspective_uuid,
                "cloned_link_language": cloned_address,
                "name": p.name,
                "message": "Perspective published as neighbourhood. Share the neighbourhood_url for others to join."
            })
            .to_string(),
            Err(e) => e,
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

        let joined = self
            .call::<PerspectiveHandle>(
                "neighbourhood.join",
                json!({ "url": p.url }),
                labelled_json("Failed to join neighbourhood"),
            )
            .await;
        match joined {
            Ok(handle) => json!({
                "success": true,
                "perspective_uuid": handle.uuid,
                "name": handle.name,
                "neighbourhood_url": p.url,
                "message": "Successfully joined neighbourhood. Use the perspective_uuid to interact with it."
            })
            .to_string(),
            Err(e) => e,
        }
    }
}

/// Clone a link language template and publish the cloned instance as `agent_context`: a
/// user's neighbourhood publishes its link language as the user, never as the main agent.
/// Returns the new language address.
pub(crate) async fn clone_link_language(
    template_address: &str,
    name: &str,
    agent_context: &AgentContext,
) -> Result<String, String> {
    let controller = LanguageController::global_instance();

    // Check if language language is available
    let language_language_address = {
        let sys = controller.system_addresses.lock().await;
        sys.language_language
            .clone()
            .ok_or("Language language not loaded — cannot clone link language template")?
    };

    let meta = controller
        .get_language_expression(template_address)
        .await
        .map_err(|e| {
            format!(
                "Failed to get template meta for '{}': {}. Use `list_link_language_templates` to see available templates.",
                template_address, e
            )
        })?;
    let declared_params: Vec<String> = meta.possible_template_params.unwrap_or_default();

    let template_map = build_clone_template_map(&declared_params, name);

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

    // Save locally and get the path
    let (saved_hash, bundle_path) = controller
        .save_language_bundle(&input.bundle, None)
        .map_err(|e| format!("Failed to save cloned language bundle locally: {}", e))?;

    // Publish via the language language
    let input_json = serde_json::to_string(&input)
        .map_err(|e| format!("Failed to serialize cloned language: {}", e))?;

    // See the matching comment in mutation_resolvers.rs publish path:
    // wrap in JSON.stringify and parse as JSON so addresses containing
    // quotes / backslashes / whitespace don't get silently corrupted.
    // Coerce undefined/null to JSON null before stringifying for the
    // same reason as the expressionGet sweep — a bare
    // JSON.stringify(undefined) yields the JS value undefined, which
    // to_rust_string_lossy then captures as the raw string
    // "undefined" and from_str::<String> fails with a confusing
    // type-mismatch instead of a clear "no address" error.
    let publish_script = format!(
        r#"JSON.stringify((await globalThis.__ad4m_language_instance__.expressionCreate({})) ?? null)"#,
        input_json
    );

    let address_raw = controller
        .execute_on_language_with_context(
            &language_language_address,
            &publish_script,
            agent_context,
        )
        .await
        .map_err(|e| format!("Failed to publish cloned language: {}", e))?;

    let trimmed_addr_raw = address_raw.trim();
    if trimmed_addr_raw == "null" || trimmed_addr_raw.is_empty() {
        return Err(format!(
            "Language language returned no address from expressionCreate when cloning template {} (got {:?})",
            template_address, trimmed_addr_raw
        ));
    }

    let address: String = serde_json::from_str(trimmed_addr_raw).map_err(|e| {
        format!(
            "Failed to parse published cloned language address: {} ({:?})",
            e, address_raw
        )
    })?;

    // Load into runtime - use the saved bundle path
    // Verify the saved hash matches the published address
    if saved_hash != address {
        log::warn!(
            "Saved language hash ({}) doesn't match published address ({}). Using published address.",
            saved_hash, address
        );
    }

    if !bundle_path.exists() {
        return Err(format!(
            "Language bundle not found at expected path: {:?}",
            bundle_path
        ));
    }
    // Loaded in the background, as `language.applyTemplate` does: the
    // perspective's `ensure_link_language` waits for this install and its
    // state reports a failure.
    let installing = saved_hash.clone();
    tokio::spawn(async move {
        if let Err(e) = LanguageController::install_language(installing).await {
            log::warn!("Failed to load cloned language into runtime: {}", e);
        }
    });

    log::info!(
        "Cloned link language template '{}' → '{}' (name: {})",
        template_address,
        address,
        input_name
    );

    Ok(address)
}

fn build_clone_template_map(
    declared_params: &[String],
    name: &str,
) -> serde_json::Map<String, serde_json::Value> {
    let mut m = serde_json::Map::new();
    let uid = uuid::Uuid::new_v4().to_string();

    let uid_key = declared_params
        .iter()
        .find(|p| p.eq_ignore_ascii_case("uid"))
        .cloned()
        .unwrap_or_else(|| "uid".to_string());
    m.insert(uid_key, serde_json::Value::String(uid));
    m.insert(
        "name".to_string(),
        serde_json::Value::String(name.to_string()),
    );
    m
}

/// Publishing runs both of its methods' guards before any clone (#851): the order the method
/// matrix through MCP does not pin.
#[cfg(test)]
mod guard_tests {
    use super::*;
    use crate::agent::capabilities::{
        ALL_CAPABILITY, LANGUAGE_CREATE_CAPABILITY, NEIGHBOURHOOD_CREATE_CAPABILITY,
    };
    use crate::mcp::tools::test_support::{
        app, handler_as, operator, perspective, setup, status, user,
    };
    use rmcp::handler::server::wrapper::Parameters;

    async fn publish(handler: &Ad4mMcpHandler, uuid: &str) -> String {
        handler
            .neighbourhood_publish_from_perspective(Parameters(NeighbourhoodPublishParams {
                perspective_uuid: uuid.to_string(),
                link_language: "QmNoSuchTemplate".into(),
                name: "n".into(),
            }))
            .await
    }

    // The tool checked the user's own perspectives and NEIGHBOURHOOD_CREATE; the operator could
    // not publish a user's perspective, and a refusal named no status.
    #[tokio::test]
    async fn publishing_follows_the_perspective_guard_before_any_clone() {
        setup();
        let owner_did = "did:key:mcp-publisher";
        let uuid = perspective(Some(vec![owner_did.into()])).await;

        let other = handler_as(user(
            "did:key:mcp-other-publisher",
            vec![ALL_CAPABILITY.clone()],
        ));
        let refused = publish(&other, &uuid).await;
        assert_eq!(status(&refused), Some(404), "{refused}");
        assert!(refused.contains("not found"), "{refused}");

        let without = publish(&handler_as(app(vec![])), &uuid).await;
        assert_eq!(status(&without), Some(403), "{without}");

        // Past the guard the tool clones the template, which this test executor cannot do.
        for session in [
            operator(),
            user(
                owner_did,
                vec![
                    NEIGHBOURHOOD_CREATE_CAPABILITY.clone(),
                    LANGUAGE_CREATE_CAPABILITY.clone(),
                ],
            ),
        ] {
            let answer = publish(&handler_as(session), &uuid).await;
            assert_eq!(status(&answer), None, "{answer}");
            assert!(answer.contains("error"), "{answer}");
        }
        crate::perspectives::remove_perspective(&uuid).await;
    }

    // Cloning applies a template and publishes a language: `language.applyTemplate`'s work,
    // which needs LANGUAGE_CREATE. Publishing the perspective alone did not check it.
    #[tokio::test]
    async fn publishing_needs_the_language_create_capability_before_any_clone() {
        setup();
        let owner_did = "did:key:mcp-publisher-without-language";
        let uuid = perspective(Some(vec![owner_did.into()])).await;
        let owner = handler_as(user(
            owner_did,
            vec![NEIGHBOURHOOD_CREATE_CAPABILITY.clone()],
        ));
        let refused = publish(&owner, &uuid).await;
        assert_eq!(status(&refused), Some(403), "{refused}");
        assert!(!refused.contains("template"), "no clone ran: {refused}");
        crate::perspectives::remove_perspective(&uuid).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn uid_detection_matches_uppercase() {
        let params = vec!["SERVER_URL".into(), "UID".into()];
        let map = build_clone_template_map(&params, "test");
        assert!(map.contains_key("UID"), "should use declared uppercase UID");
        assert!(!map.contains_key("uid"), "should not insert lowercase uid");
    }

    #[test]
    fn uid_detection_matches_lowercase() {
        let params = vec!["uid".into(), "name".into(), "description".into()];
        let map = build_clone_template_map(&params, "test");
        assert!(map.contains_key("uid"));
    }

    #[test]
    fn uid_detection_matches_mixed_case() {
        let params = vec!["Uid".into()];
        let map = build_clone_template_map(&params, "test");
        assert!(map.contains_key("Uid"));
    }

    #[test]
    fn uid_detection_falls_back_when_absent() {
        let params = vec!["SERVER_URL".into()];
        let map = build_clone_template_map(&params, "test");
        assert!(
            map.contains_key("uid"),
            "should fall back to lowercase uid when none declared"
        );
    }

    #[test]
    fn uid_detection_falls_back_on_empty_params() {
        let map = build_clone_template_map(&[], "test");
        assert!(map.contains_key("uid"));
    }

    #[test]
    fn template_map_contains_only_uid_and_name() {
        let params = vec![
            "SERVER_URL".into(),
            "UID".into(),
            "description".into(),
            "extra_param".into(),
        ];
        let map = build_clone_template_map(&params, "my neighbourhood");
        assert_eq!(map.len(), 2, "map should contain exactly uid + name");
        assert!(map.contains_key("UID"));
        assert_eq!(map.get("name").unwrap(), "my neighbourhood");
        assert!(
            !map.contains_key("SERVER_URL"),
            "SERVER_URL must not appear — it keeps its bundle default"
        );
        assert!(!map.contains_key("description"));
        assert!(!map.contains_key("extra_param"));
    }

    #[test]
    fn uid_value_parses_as_uuid() {
        let map = build_clone_template_map(&["UID".into()], "test");
        let uid_str = map.get("UID").unwrap().as_str().unwrap();
        uuid::Uuid::parse_str(uid_str).expect("uid should parse as a valid UUID");
    }

    #[test]
    fn name_value_preserved_verbatim() {
        let map = build_clone_template_map(&[], "My Cool Neighbourhood 🌎");
        assert_eq!(map.get("name").unwrap(), "My Cool Neighbourhood 🌎");
    }

    const SLL_SOURCE: &str =
        include_str!("../../../../bootstrap-languages/server-link-language/index.ts");

    #[test]
    fn sll_declares_server_url_with_default() {
        let lines: Vec<&str> = SLL_SOURCE.lines().collect();
        let marker_positions: Vec<usize> = lines
            .iter()
            .enumerate()
            .filter(|(_, l)| l.contains("//!@ad4m-template-variable"))
            .map(|(i, _)| i)
            .collect();
        assert!(
            marker_positions.len() >= 2,
            "SLL must declare at least two template variables"
        );
        let server_url_line = lines[marker_positions[0] + 1];
        assert!(
            server_url_line.contains("SERVER_URL"),
            "first template variable should declare SERVER_URL"
        );
        assert!(
            server_url_line.contains("https://link.ad4m.dev"),
            "SERVER_URL default must point to https://link.ad4m.dev"
        );
    }

    #[test]
    fn sll_declares_uid_template_variable() {
        let lines: Vec<&str> = SLL_SOURCE.lines().collect();
        let marker_positions: Vec<usize> = lines
            .iter()
            .enumerate()
            .filter(|(_, l)| l.contains("//!@ad4m-template-variable"))
            .map(|(i, _)| i)
            .collect();
        let uid_line = lines[marker_positions[1] + 1];
        assert!(
            uid_line.contains("UID"),
            "second template variable should declare UID"
        );
    }

    #[test]
    fn sll_possible_template_params_matches_variables() {
        assert!(
            SLL_SOURCE.contains(r#"possibleTemplateParams: string[] = ["SERVER_URL", "UID"]"#),
            "possibleTemplateParams export must declare exactly SERVER_URL and UID"
        );
    }

    #[test]
    fn sll_clone_with_default_server_url() {
        let sll_params = vec!["SERVER_URL".into(), "UID".into()];
        let map = build_clone_template_map(&sll_params, "Test Neighbourhood");

        assert!(
            map.contains_key("UID"),
            "clone map must contain UID (case-matched from declared params)"
        );
        assert!(
            !map.contains_key("SERVER_URL"),
            "clone map must NOT contain SERVER_URL — the bundle default applies"
        );
        assert_eq!(map.get("name").unwrap(), "Test Neighbourhood");

        let mut lines: Vec<String> = SLL_SOURCE.lines().map(String::from).collect();
        crate::languages::LanguageController::apply_template_data(&mut lines, &map);

        let marker_positions: Vec<usize> = lines
            .iter()
            .enumerate()
            .filter(|(_, l)| l.contains("//!@ad4m-template-variable"))
            .map(|(i, _)| i)
            .collect();

        let server_url_line = &lines[marker_positions[0] + 1];
        assert!(
            server_url_line.contains("https://link.ad4m.dev"),
            "SERVER_URL must remain at bundle default after clone: got {}",
            server_url_line
        );

        let uid_line = &lines[marker_positions[1] + 1];
        assert!(
            !uid_line.contains("<to-be-filled>"),
            "UID must get replaced by the clone: got {}",
            uid_line
        );
        let uid_val = map.get("UID").unwrap().as_str().unwrap();
        assert!(
            uid_line.contains(uid_val),
            "UID line must contain the generated UUID"
        );
    }
}

//! Perspective and link operation tools
//!
//! Tools for managing perspectives (knowledge graphs) and raw links.

use super::Ad4mMcpHandler;
use crate::agent::capabilities::defs::PERSPECTIVE_CREATE_CAPABILITY;
use crate::perspectives::perspective_instance::SdnaType;
use crate::perspectives::utils::prolog_resolution_to_string;
use crate::perspectives::{add_perspective, all_perspectives};
use crate::types::Link;
use crate::types::{LinkQuery, LinkStatus, PerspectiveHandle};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for listing perspectives
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ListPerspectivesParams {}

/// Parameters for listing subject classes in a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ListSubjectClassesParams {
    /// Perspective UUID
    pub perspective_id: String,
}

/// Parameters for creating a new perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddPerspectiveParams {
    /// Name for the new perspective
    pub name: String,
}

/// Parameters for adding a link to a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddLinkParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Link source URI
    pub source: String,
    /// Link predicate URI
    pub predicate: String,
    /// Link target URI
    pub target: String,
}

/// Parameters for querying links in a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct QueryLinksParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Optional source URI filter
    pub source: Option<String>,
    /// Optional predicate URI filter
    pub predicate: Option<String>,
    /// Optional target URI filter
    pub target: Option<String>,
}

/// Parameters for adding SDNA (subject class definition) to a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddModelParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Bare subject class name, matching the local name of the shape's
    /// `target_class` (e.g. `Task` for `target_class: "board://Task"`)
    pub class_name: String,
    /// SHACL shape definition as JSON string
    pub shacl_json: String,
}

/// Local (bare) name of a `target_class` URI: the segment after the last `/`
/// or `#`. `board://Task` → `Task`; a bare `Task` is returned unchanged.
fn local_class_name(target_class: &str) -> &str {
    target_class
        .rsplit(['/', '#'])
        .next()
        .unwrap_or(target_class)
}

/// Check that `class_name` is the local name of the shape's `target_class`.
///
/// The two are stored independently — `class_name` names the SDNA entry while
/// `target_class` defines the class URI — and a mismatch registers a class that
/// looks fine in `describe_perspective` but whose property setters are never
/// found, so every write fails with "read-only". Rejecting the mismatch up
/// front turns a silent broken registration into an actionable error.
fn validate_class_name(class_name: &str, shacl_json: &str) -> Result<(), String> {
    let shape: serde_json::Value = serde_json::from_str(shacl_json)
        .map_err(|e| format!("Error: shacl_json is not valid JSON: {}", e))?;

    let target_class = shape
        .get("target_class")
        .and_then(|v| v.as_str())
        .ok_or_else(|| {
            "Error: shacl_json has no `target_class` field. Every SHACL shape must declare \
             the class URI it describes, e.g. \"target_class\": \"board://Task\"."
                .to_string()
        })?;

    let expected = local_class_name(target_class);
    if class_name != expected {
        return Err(format!(
            "Error: class_name '{}' does not match the SHACL target_class '{}'. \
             Pass class_name: \"{}\" — the bare class name, without the namespace. \
             Registering a mismatched name yields a class whose properties are all \
             read-only, because its setters are stored under the other name.",
            class_name, target_class, expected
        ));
    }

    Ok(())
}

/// Parameters for running a Prolog query
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InferParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Prolog query string
    pub query: String,
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// List all perspectives available to the current user
    #[tool(
        description = "List all AD4M perspectives. A perspective is a subjective graph database — a personal collection of links (RDF-like triples: source → predicate → target) that can be queried, modified, and optionally shared as a 'neighbourhood' for real-time P2P collaboration. Each has a UUID and a human-readable name."
    )]
    pub async fn list_perspectives(&self, _params: Parameters<ListPerspectivesParams>) -> String {
        let _agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let perspectives = all_perspectives();
        let mut result: Vec<serde_json::Value> = Vec::new();
        for p in perspectives.iter() {
            let handle = p.persisted.lock().await.clone();

            // Multi-user isolation: only show perspectives the user can access
            if !self.can_access_perspective(&handle).await {
                continue;
            }

            result.push(json!({
                "uuid": handle.uuid,
                "name": handle.name,
                "shared_url": handle.shared_url,
                "has_neighbourhood": handle.neighbourhood.is_some(),
            }));
        }
        serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
    }

    /// Get all models (subject classes) defined in a perspective
    #[tool(
        description = "Get the names of all models (SHACL subject classes) defined in a perspective. Models are schemas that give structure to the raw link graph — like database table definitions. For the full schema (properties, types, cardinality, collections, flows) call describe_perspective, then work with instances via instance_create / instance_query / instance_get / instance_update / instance_add_to_collection / instance_remove."
    )]
    pub async fn get_models(&self, params: Parameters<ListSubjectClassesParams>) -> String {
        let uuid = &params.0.perspective_id;

        match self.get_readable_perspective(uuid).await {
            Ok(perspective) => {
                let links = perspective
                    .get_links(&LinkQuery {
                        predicate: Some("rdf://type".to_string()),
                        target: Some("ad4m://SubjectClass".to_string()),
                        ..Default::default()
                    })
                    .await;

                match links {
                    Ok(class_links) => {
                        let classes: Vec<String> = class_links
                            .iter()
                            .map(|l| {
                                l.data
                                    .source
                                    .split("://")
                                    .last()
                                    .unwrap_or(&l.data.source)
                                    .to_string()
                            })
                            .collect();
                        serde_json::to_string_pretty(&classes)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error listing subject classes: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Create a new perspective
    #[tool(
        description = "Create a new perspective (local knowledge graph). Returns the UUID. You can then add links, register models (subject classes), and create typed instances within it. To share it for collaboration, convert it to a neighbourhood."
    )]
    pub async fn add_perspective(&self, params: Parameters<AddPerspectiveParams>) -> String {
        let p = &params.0;

        let _agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let capabilities = self.get_capabilities().await;
        if let Err(e) = crate::agent::capabilities::check_capability(
            &capabilities,
            &PERSPECTIVE_CREATE_CAPABILITY,
        ) {
            return format!("Capability error: {}", e);
        }

        // In multi-user mode, set the creating user as owner (reuses REST pattern)
        let user_email = self.get_user_email().await;
        let owner_did = if let Some(email) = &user_email {
            crate::agent::AgentService::get_user_did_by_email(email).ok()
        } else {
            None
        };

        let handle = if let Some(owner) = &owner_did {
            PerspectiveHandle::new_with_owner(p.name.clone(), owner.clone())
        } else {
            PerspectiveHandle::new_from_name(p.name.clone())
        };
        let uuid = handle.uuid.clone();

        match add_perspective(handle, None).await {
            Ok(_) => {
                let result = json!({
                    "success": true,
                    "uuid": uuid,
                    "name": p.name,
                });
                serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => format!("Error creating perspective: {}", e),
        }
    }

    /// Add a link to a perspective
    #[tool(
        description = "Add a link (RDF-like triple) to a perspective. Links are the fundamental data unit — all data (properties, type markers, collections) is stored as links. Example: source='did:key:abc' predicate='ad4m://name' target='literal://string:Alice'. In shared neighbourhoods, links sync to all members."
    )]
    pub async fn add_link(&self, params: Parameters<AddLinkParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                let link = Link {
                    source: p.source.clone(),
                    predicate: Some(p.predicate.clone()),
                    target: p.target.clone(),
                };

                match perspective
                    .add_link(link, LinkStatus::Shared, None, &agent_context)
                    .await
                {
                    Ok(decorated) => {
                        let result = json!({
                            "success": true,
                            "link": {
                                "source": decorated.data.source,
                                "predicate": decorated.data.predicate,
                                "target": decorated.data.target,
                                "timestamp": decorated.timestamp,
                            }
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error adding link: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Query links in a perspective
    #[tool(
        description = "Query links in a perspective. Links are RDF-like triples with source, predicate, and target. Filter by any combination — omit a filter to match all values for that field. Example: source='expr://abc' with no predicate/target returns all links from that address. Use predicate filter to find specific property values."
    )]
    pub async fn query_links(&self, params: Parameters<QueryLinksParams>) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                let query = LinkQuery {
                    source: p.source.clone(),
                    predicate: p.predicate.clone(),
                    target: p.target.clone(),
                    ..Default::default()
                };

                match perspective.get_links(&query).await {
                    Ok(links) => {
                        let result: Vec<serde_json::Value> = links
                            .iter()
                            .map(|l| {
                                json!({
                                    "source": l.data.source,
                                    "predicate": l.data.predicate,
                                    "target": l.data.target,
                                    "timestamp": l.timestamp,
                                    "author": l.author,
                                })
                            })
                            .collect();
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying links: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Add a model (subject class definition) to a perspective
    #[tool(
        description = "Register a model (subject class) using a SHACL JSON definition. This defines the schema — properties, collections, types — for typed objects in the perspective. Once registered, the class appears in describe_perspective and can be used with the generic instance_* tools by class_name (instance_create, instance_query, …). If the executor runs with dynamicClassTools enabled, per-class tools ({class}_create, {class}_set_{property}, …) are additionally generated and the tool list updates after registration."
    )]
    pub async fn add_model(&self, params: Parameters<AddModelParams>) -> String {
        let p = &params.0;

        if let Err(message) = validate_class_name(&p.class_name, &p.shacl_json) {
            return message;
        }

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                match perspective
                    .add_sdna(
                        p.class_name.clone(),
                        String::new(),
                        SdnaType::SubjectClass,
                        Some(p.shacl_json.clone()),
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => {
                        let result = json!({
                            "success": true,
                            "perspective_id": p.perspective_id,
                            "class_name": p.class_name,
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error adding SDNA: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Run a Prolog query for complex reasoning
    #[tool(
        description = "Run a Prolog query on a perspective for complex reasoning. The link graph is exposed as Prolog facts (triple/3), enabling pattern matching and inference beyond simple link queries. Example: 'triple(X, \"rdf://type\", \"ad4m://SubjectClass\")' finds all subject classes. Use for advanced queries not covered by other tools."
    )]
    pub async fn infer(&self, params: Parameters<InferParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((perspective, agent_context)) => {
                match perspective
                    .prolog_query_with_context(p.query.clone(), &agent_context)
                    .await
                {
                    Ok(result) => prolog_resolution_to_string(result),
                    Err(e) => format!("Error running query: {}", e),
                }
            }
            Err(e) => e,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{local_class_name, validate_class_name};

    fn shape(target_class: &str) -> String {
        format!(r#"{{"target_class":"{}","properties":[]}}"#, target_class)
    }

    #[test]
    fn local_name_strips_namespace() {
        assert_eq!(local_class_name("board://Task"), "Task");
        assert_eq!(local_class_name("http://example.org/ns#Task"), "Task");
        assert_eq!(local_class_name("Task"), "Task");
    }

    #[test]
    fn bare_name_matching_target_class_is_accepted() {
        assert!(validate_class_name("Task", &shape("board://Task")).is_ok());
    }

    #[test]
    fn uri_form_class_name_is_rejected() {
        // The zombie-schema case: registration used to succeed and produce a
        // class whose every property was read-only.
        let err = validate_class_name("board://Task", &shape("board://Task")).unwrap_err();
        assert!(err.contains("board://Task"), "{}", err);
        assert!(err.contains("\"Task\""), "{}", err);
    }

    #[test]
    fn unrelated_class_name_is_rejected() {
        let err = validate_class_name("SomethingElse", &shape("board://Comment")).unwrap_err();
        assert!(err.contains("SomethingElse"), "{}", err);
        assert!(err.contains("board://Comment"), "{}", err);
    }

    #[test]
    fn missing_target_class_is_rejected() {
        let err = validate_class_name("Task", r#"{"properties":[]}"#).unwrap_err();
        assert!(err.contains("target_class"), "{}", err);
    }

    #[test]
    fn invalid_json_is_rejected() {
        let err = validate_class_name("Task", "not json").unwrap_err();
        assert!(err.contains("not valid JSON"), "{}", err);
    }
}

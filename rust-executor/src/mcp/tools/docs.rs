//! `get_documentation` — the executor's own MCP documentation, served as
//! markdown so a cold agent can learn the tool surface and the data model
//! without any file on its side.
//!
//! The texts are compiled in from `rust-executor/src/mcp/docs/*.md`, so the
//! docs an agent reads always match the binary it is talking to.
//!
//! Deliberately not served here: how to get, run and unlock an executor. An
//! agent that can call this tool has already done that, so setup docs live
//! with whatever set the connection up (for the OpenClaw plugin, its skill).

use super::Ad4mMcpHandler;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

const OVERVIEW: &str = include_str!("../docs/overview.md");
const ARCHITECTURE: &str = include_str!("../docs/architecture.md");
const USAGE: &str = include_str!("../docs/usage.md");

/// Which document to return.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "lowercase")]
pub enum DocTopic {
    /// What AD4M is, the static tool surface, the workflow, and the rules
    /// that keep data usable by humans and other agents. Start here.
    Overview,
    /// Perspectives, links, languages, neighbourhoods, and the SHACL subject
    /// class (social DNA) format in detail.
    Architecture,
    /// How to actually use the tools: reading and writing instances, the tree,
    /// the Flux data model, authoring subject classes, and the common traps.
    Usage,
}

impl DocTopic {
    pub(crate) const ALL: [DocTopic; 3] =
        [DocTopic::Overview, DocTopic::Architecture, DocTopic::Usage];

    pub(crate) fn text(self) -> &'static str {
        match self {
            DocTopic::Overview => OVERVIEW,
            DocTopic::Architecture => ARCHITECTURE,
            DocTopic::Usage => USAGE,
        }
    }
}

/// Parameters for reading the executor's documentation
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetDocumentationParams {
    /// Which document: "overview" (start here), "architecture" or "usage"
    pub topic: DocTopic,
}

impl Ad4mMcpHandler {
    /// Return the executor's documentation for a topic, as markdown.
    #[tool(
        description = "Read the AD4M executor's documentation as markdown. topic='overview' explains what AD4M is, the static tool surface (describe_perspective + instance_*), the workflow and the rules for writing data other agents and humans can use — call it first if you are new to AD4M. topic='usage' is the working guide: reading and writing instances, the ad4m://has_child tree, the Flux data model (channels, messages, posts, tasks), authoring subject classes with add_model, and the traps that cost the most time. topic='architecture' covers perspectives, links, neighbourhoods and the SHACL class format in depth. No authentication needed."
    )]
    pub async fn get_documentation(&self, params: Parameters<GetDocumentationParams>) -> String {
        params.0.topic.text().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_topic_has_substantial_markdown() {
        for topic in DocTopic::ALL {
            let text = topic.text();
            assert!(text.starts_with("# "), "{topic:?} must start with a title");
            assert!(text.len() > 2_000, "{topic:?} is suspiciously short");
        }
    }

    #[test]
    fn overview_teaches_the_static_surface() {
        let overview = DocTopic::Overview.text();
        for tool in [
            "describe_perspective",
            "instance_create",
            "instance_query",
            "instance_get",
            "instance_update",
            "instance_add_to_collection",
            "instance_remove_from_collection",
            "instance_remove",
            "instance_transcript",
            "add_child",
            "get_children",
            "get_documentation",
        ] {
            assert!(overview.contains(tool), "overview must mention {tool}");
        }
    }

    /// The docs only advertise topics that exist. `setup` was removed: an
    /// agent that can call this tool is already past setup. Iterating
    /// `DocTopic::ALL` means every topic added later — `usage` included — is
    /// covered without touching this test again.
    #[test]
    fn docs_only_point_at_served_topics() {
        assert!(
            DocTopic::ALL.contains(&DocTopic::Usage),
            "usage must be in ALL so the doc guards actually check it"
        );
        for topic in DocTopic::ALL {
            let text = topic.text();
            assert!(
                !text.contains("topic=\"setup\"") && !text.contains("topic='setup'"),
                "{topic:?} still points at the removed setup topic"
            );
        }
    }

    /// No doc may teach a tool the executor no longer registers. The whole
    /// `*_subject` family, `infer` and `get_models` went away in the static-surface
    /// consolidation, so they may only appear as "this is gone" prose — which is
    /// what the leading word in each pattern below pins down.
    #[test]
    fn docs_do_not_teach_removed_tools() {
        for topic in DocTopic::ALL {
            let text = topic.text();
            for removed in [
                "query_subjects(",
                "create_subject(",
                "get_subject_children(",
                "get_children_body_parsed(",
                "get_models(",
                "infer(",
            ] {
                assert!(
                    !text.contains(removed),
                    "{topic:?} calls the removed tool {removed}"
                );
            }
            // `get_children` / `add_child` take `parent` and `child`, never the
            // old `parent_address`. Naming it as a dead parameter is fine;
            // passing it is not.
            for dead_arg in ["parent_address=", "parent_address:", "(parent_address"] {
                assert!(
                    !text.contains(dead_arg),
                    "{topic:?} still passes the removed parent_address parameter"
                );
            }
        }
    }

    /// `usage` carries the general AD4M working knowledge, addressed with bare
    /// tool names — no host-specific prefix, and nothing about a plugin.
    #[test]
    fn usage_teaches_the_working_surface_with_bare_tool_names() {
        let usage = DocTopic::Usage.text();
        for tool in [
            "instance_create",
            "instance_query",
            "instance_transcript",
            "instance_update",
            "instance_add_to_collection",
            "get_children",
            "add_child",
            "add_model",
            "describe_perspective",
            "set_agent_profile",
            "get_my_did",
            "neighbourhood_join_from_url",
            "list_perspectives",
        ] {
            assert!(usage.contains(tool), "usage must mention {tool}");
        }
        assert!(
            !usage.contains("ad4m_"),
            "usage must use bare tool names, not a host's prefix"
        );
        for host_specific in ["contracts.tools", "openclaw", "OpenClaw", "manifest"] {
            assert!(
                !usage.contains(host_specific),
                "usage must stay host-agnostic, found {host_specific}"
            );
        }
    }

    /// A cold client learns about the docs from the `initialize` response
    /// and can read them before authenticating.
    #[tokio::test]
    async fn docs_are_advertised_in_instructions_and_need_no_auth() {
        use crate::mcp::server::McpContext;
        use crate::mcp::tools::{Ad4mMcpHandler, AUTH_TOOLS};
        use rmcp::ServerHandler;
        use std::sync::Arc;
        use tokio::sync::RwLock;

        let handler = Ad4mMcpHandler::new(McpContext {
            admin_credential: Some("secret".to_string()),
            auth_token: Arc::new(RwLock::new(None)),
            dynamic_class_tools: false,
        });
        let instructions = handler.get_info().instructions.expect("instructions set");
        assert!(instructions.contains("get_documentation(topic=\"overview\")"));
        assert!(instructions.contains("topic=\"usage\""));
        assert!(instructions.contains("topic=\"architecture\""));
        assert!(instructions.contains("describe_perspective"));
        assert!(!instructions.contains("topic=\"setup\""));
        assert!(AUTH_TOOLS.contains(&"get_documentation"));

        // The tool itself: unauthenticated handler, still answers.
        let text = handler
            .get_documentation(Parameters(GetDocumentationParams {
                topic: DocTopic::Overview,
            }))
            .await;
        assert_eq!(text, DocTopic::Overview.text());
    }

    /// Every topic the enum advertises is actually served, and each answers
    /// with its own text rather than falling through to another topic's.
    #[tokio::test]
    async fn get_documentation_serves_every_topic() {
        use crate::mcp::server::McpContext;
        use std::sync::Arc;
        use tokio::sync::RwLock;

        let handler = Ad4mMcpHandler::new(McpContext {
            admin_credential: None,
            auth_token: Arc::new(RwLock::new(None)),
            dynamic_class_tools: false,
        });

        let mut served = Vec::new();
        for topic in DocTopic::ALL {
            let text = handler
                .get_documentation(Parameters(GetDocumentationParams { topic }))
                .await;
            assert_eq!(text, topic.text(), "{topic:?} served the wrong document");
            assert!(text.starts_with("# "), "{topic:?} served no markdown title");
            served.push(text);
        }
        assert_eq!(served.len(), 3, "overview, usage and architecture");
        served.sort();
        served.dedup();
        assert_eq!(served.len(), 3, "two topics served identical text");
    }

    #[test]
    fn topic_names_are_lowercase_on_the_wire() {
        assert_eq!(
            serde_json::to_string(&DocTopic::Architecture).unwrap(),
            "\"architecture\""
        );
        assert_eq!(
            serde_json::to_string(&DocTopic::Usage).unwrap(),
            "\"usage\""
        );
        let parsed: DocTopic = serde_json::from_str("\"overview\"").unwrap();
        assert_eq!(parsed, DocTopic::Overview);
        let parsed: DocTopic = serde_json::from_str("\"usage\"").unwrap();
        assert_eq!(parsed, DocTopic::Usage);
        assert!(
            serde_json::from_str::<DocTopic>("\"setup\"").is_err(),
            "setup is no longer a documentation topic"
        );
    }
}

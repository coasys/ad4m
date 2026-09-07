//! `get_documentation` — the executor's own MCP documentation, served as
//! markdown so a cold agent can learn the tool surface, the data model and
//! the setup/auth story without any file on its side.
//!
//! The texts are compiled in from `rust-executor/src/mcp/docs/*.md`, so the
//! docs an agent reads always match the binary it is talking to.

use super::Ad4mMcpHandler;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

const OVERVIEW: &str = include_str!("../docs/overview.md");
const ARCHITECTURE: &str = include_str!("../docs/architecture.md");
const SETUP: &str = include_str!("../docs/setup.md");

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
    /// Getting, running, unlocking and authenticating against an executor;
    /// deployment scenarios; troubleshooting.
    Setup,
}

impl DocTopic {
    pub(crate) const ALL: [DocTopic; 3] =
        [DocTopic::Overview, DocTopic::Architecture, DocTopic::Setup];

    pub(crate) fn text(self) -> &'static str {
        match self {
            DocTopic::Overview => OVERVIEW,
            DocTopic::Architecture => ARCHITECTURE,
            DocTopic::Setup => SETUP,
        }
    }
}

/// Parameters for reading the executor's documentation
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetDocumentationParams {
    /// Which document: "overview" (start here), "architecture" or "setup"
    pub topic: DocTopic,
}

impl Ad4mMcpHandler {
    /// Return the executor's documentation for a topic, as markdown.
    #[tool(
        description = "Read the AD4M executor's documentation as markdown. topic='overview' explains what AD4M is, the static tool surface (describe_perspective + instance_*), the workflow and the rules for writing data other agents and humans can use — call it first if you are new to AD4M. topic='architecture' covers perspectives, links, neighbourhoods and the SHACL class format; topic='setup' covers running, unlocking and authenticating against an executor. No authentication needed."
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
        assert!(instructions.contains("describe_perspective"));
        assert!(AUTH_TOOLS.contains(&"get_documentation"));

        // The tool itself: unauthenticated handler, still answers.
        let text = handler
            .get_documentation(Parameters(GetDocumentationParams {
                topic: DocTopic::Overview,
            }))
            .await;
        assert_eq!(text, DocTopic::Overview.text());
    }

    #[test]
    fn topic_names_are_lowercase_on_the_wire() {
        assert_eq!(
            serde_json::to_string(&DocTopic::Architecture).unwrap(),
            "\"architecture\""
        );
        let parsed: DocTopic = serde_json::from_str("\"setup\"").unwrap();
        assert_eq!(parsed, DocTopic::Setup);
    }
}

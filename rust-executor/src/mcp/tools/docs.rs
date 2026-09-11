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
const FLUX: &str = include_str!("../docs/flux.md");
const MODELS: &str = include_str!("../docs/models.md");

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
    /// How to actually use the tools: reading and writing instances, the
    /// `ad4m://has_child` tree, and the common traps.
    Usage,
    /// The Flux data model — channels, messages, posts and tasks — and the
    /// recipes for reading and replying in a channel humans can see.
    Flux,
    /// Authoring subject classes: when to add one, how to write a shape that
    /// is writable and findable, and what changing a class does to the
    /// instances that already exist.
    Models,
}

impl DocTopic {
    pub(crate) const ALL: [DocTopic; 5] = [
        DocTopic::Overview,
        DocTopic::Architecture,
        DocTopic::Usage,
        DocTopic::Flux,
        DocTopic::Models,
    ];

    pub(crate) fn text(self) -> &'static str {
        match self {
            DocTopic::Overview => OVERVIEW,
            DocTopic::Architecture => ARCHITECTURE,
            DocTopic::Usage => USAGE,
            DocTopic::Flux => FLUX,
            DocTopic::Models => MODELS,
        }
    }
}

/// Parameters for reading the executor's documentation
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetDocumentationParams {
    /// Which document: "overview" (start here), "usage", "flux", "models"
    /// or "architecture"
    pub topic: DocTopic,
}

impl Ad4mMcpHandler {
    /// Return the executor's documentation for a topic, as markdown.
    #[tool(
        description = "Read the AD4M executor's documentation as markdown. topic='overview' explains what AD4M is, the static tool surface (describe_perspective + instance_*), the workflow and the rules for writing data other agents and humans can use — call it first if you are new to AD4M. topic='usage' is the working guide: reading and writing instances, the ad4m://has_child tree, and the traps that cost the most time. topic='flux' is the Flux data model — channels, messages, posts, tasks — and how to read and reply in a channel; read it when the perspective you joined is a Flux space. topic='models' teaches you to author your own subject classes with add_model: when to add one, how to write a shape that is writable and findable, and what changing a class does to existing instances. topic='architecture' covers perspectives, links, neighbourhoods and the SHACL class format reference in depth. No authentication needed."
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

    /// The wire name of a topic, as `topic="…"` spells it.
    fn wire_name(topic: DocTopic) -> String {
        serde_json::to_value(topic)
            .expect("topic serializes")
            .as_str()
            .expect("topic is a string on the wire")
            .to_string()
    }

    /// Every `topic="…"` / `topic='…'` a document writes names a topic that is
    /// actually served. This is how the removed `setup` topic was caught, and
    /// it generalises: a cross-reference to a topic that was renamed, or never
    /// added, sends the reader to a tool call that errors.
    #[test]
    fn docs_only_point_at_served_topics() {
        let served: Vec<String> = DocTopic::ALL.into_iter().map(wire_name).collect();
        let mut pointers_found = 0usize;
        for topic in DocTopic::ALL {
            let text = topic.text();
            for opener in ["topic=\"", "topic='"] {
                let closer = opener.chars().last().expect("opener ends in a quote");
                for (idx, _) in text.match_indices(opener) {
                    let rest = &text[idx + opener.len()..];
                    let Some(end) = rest.find(closer) else {
                        continue;
                    };
                    let named = &rest[..end];
                    pointers_found += 1;
                    assert!(
                        served.contains(&named.to_string()),
                        "{topic:?} points at topic={named:?}, which no DocTopic serves"
                    );
                }
            }
        }
        assert!(
            pointers_found >= DocTopic::ALL.len(),
            "found only {pointers_found} topic pointers across the docs — the \
             cross-references this test guards have gone missing"
        );
    }

    /// `overview` is the entry point a cold agent is told to read first, so a
    /// topic it never names is a topic nobody discovers — which is exactly how
    /// `usage` shipped invisible in the first place. Every non-overview topic
    /// must be reachable from the overview's own prose.
    #[test]
    fn overview_points_at_every_other_topic() {
        let overview = DocTopic::Overview.text();
        for topic in DocTopic::ALL {
            if topic == DocTopic::Overview {
                continue;
            }
            let name = wire_name(topic);
            assert!(
                overview.contains(&format!("topic=\"{name}\"")),
                "overview never tells the reader that topic={name:?} exists"
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

    /// `usage` names every tool of the general working surface — the vocabulary
    /// an agent needs before its first write, independent of which app's
    /// classes the perspective happens to carry.
    #[test]
    fn usage_names_every_tool_of_the_working_surface() {
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
    }

    /// Naming a host is `overview`'s job and nobody else's. The overview says
    /// once that a host may prefix these tools, and names the OpenClaw plugin
    /// as the example; every other topic is read by clients that prefix
    /// differently or not at all, so it stays on bare tool names and mentions
    /// no particular host.
    #[test]
    fn only_overview_shows_a_host_prefix_or_names_a_host() {
        assert!(
            DocTopic::Overview.text().contains("ad4m_"),
            "overview must be where the ad4m_ prefix is explained, or the \
             exemption below is guarding nothing"
        );
        for topic in DocTopic::ALL {
            if topic == DocTopic::Overview {
                continue;
            }
            let text = topic.text();
            assert!(
                !text.contains("ad4m_"),
                "{topic:?} must use bare tool names, not a host's prefix"
            );
            for host in ["openclaw", "OpenClaw"] {
                assert!(
                    !text.contains(host),
                    "{topic:?} must stay host-agnostic, found {host}"
                );
            }
        }
    }

    /// The three how-to-work topics carry no host tool-configuration
    /// vocabulary. `architecture` is deliberately not in this list: it explains
    /// why per-class tool generation is off by default, and "plugin manifests"
    /// is the reason.
    #[test]
    fn the_how_to_work_topics_carry_no_host_configuration_vocabulary() {
        for topic in [DocTopic::Usage, DocTopic::Flux, DocTopic::Models] {
            let text = topic.text();
            for host_specific in ["contracts.tools", "manifest"] {
                assert!(
                    !text.contains(host_specific),
                    "{topic:?} must stay host-agnostic, found {host_specific}"
                );
            }
        }
    }

    /// The Flux material lives in `flux`, not in `usage`. These terms are Flux
    /// vocabulary and appear nowhere else, so "present in `flux`, absent from
    /// `usage`" is exactly what it means for the section to have moved rather
    /// than been copied — and `usage` still points the reader at where it went.
    #[test]
    fn flux_vocabulary_is_in_flux_and_gone_from_usage() {
        let flux = DocTopic::Flux.text();
        let usage = DocTopic::Usage.text();
        for term in [
            "isConversation",
            "ConversationSubgroup",
            "orderedTaskIds",
            "flux-chat-view",
            "Space channel",
            "Reply into the same parent",
        ] {
            assert!(
                flux.contains(term),
                "flux must carry the Flux term {term:?}"
            );
            assert!(
                !usage.contains(term),
                "usage still carries the Flux term {term:?}"
            );
        }
        assert!(
            usage.contains("topic=\"flux\""),
            "usage must point at the topic its Flux material moved to"
        );
    }

    /// Schema *authoring* lives in `models`, not in `usage`. The SHACL fields
    /// below are what you write when defining a class and never what you pass
    /// when using one, so `usage` naming any of them means the authoring guide
    /// leaked back in. `usage` keeps `add_model` only as the pointer.
    #[test]
    fn shacl_authoring_fields_are_in_models_and_gone_from_usage() {
        let models = DocTopic::Models.text();
        let usage = DocTopic::Usage.text();
        for field in [
            "target_class",
            "constructor_actions",
            "min_count",
            "relation_kind",
            "target_class_name",
            "has_value",
            "setter",
        ] {
            assert!(
                models.contains(field),
                "models must teach the SHACL field {field:?}"
            );
            assert!(
                !usage.contains(field),
                "usage still teaches schema authoring: {field:?}"
            );
        }
        assert!(
            usage.contains("topic=\"models\"") && usage.contains("add_model"),
            "usage must point at the authoring topic and name the tool it covers"
        );
    }

    /// `architecture` owns the SHACL wire-format reference; `models` is the
    /// authoring guide and sends the reader there instead of restating the
    /// field tables, so the two cannot drift apart.
    #[test]
    fn models_defers_to_architecture_for_the_shacl_field_reference() {
        let architecture = DocTopic::Architecture.text();
        let models = DocTopic::Models.text();
        for table in [
            "PropertyShape Fields",
            "Top-Level Fields",
            "AD4MAction Fields",
        ] {
            assert!(
                architecture.contains(table),
                "architecture must keep the {table:?} reference"
            );
            assert!(
                !models.contains(table),
                "models restates architecture's {table:?} table instead of \
                 pointing at it"
            );
        }
        assert!(
            models.contains("topic=\"architecture\""),
            "models must send the reader to the field reference"
        );
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
        for topic in DocTopic::ALL {
            let name = wire_name(topic);
            assert!(
                instructions.contains(&format!("topic=\"{name}\"")),
                "initialize never tells a cold client that topic={name:?} exists"
            );
        }
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
        assert_eq!(
            served.len(),
            DocTopic::ALL.len(),
            "every topic in ALL must be served"
        );
        served.sort();
        served.dedup();
        assert_eq!(
            served.len(),
            DocTopic::ALL.len(),
            "two topics served identical text"
        );
    }

    /// Every topic round-trips through its lowercase wire name, so the name a
    /// document prints in `topic="…"` is the one a client can actually send.
    #[test]
    fn every_topic_round_trips_through_its_lowercase_wire_name() {
        for topic in DocTopic::ALL {
            let name = wire_name(topic);
            assert_eq!(
                name,
                name.to_lowercase(),
                "{topic:?} is not lowercase on the wire"
            );
            let parsed: DocTopic = serde_json::from_str(&format!("\"{name}\""))
                .unwrap_or_else(|e| panic!("{topic:?} does not parse back from {name:?}: {e}"));
            assert_eq!(parsed, topic);
        }
        assert_eq!(wire_name(DocTopic::Flux), "flux");
        assert_eq!(wire_name(DocTopic::Models), "models");
        assert!(
            serde_json::from_str::<DocTopic>("\"setup\"").is_err(),
            "setup is no longer a documentation topic"
        );
    }
}

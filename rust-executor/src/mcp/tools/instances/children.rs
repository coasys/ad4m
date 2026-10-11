//! Class-agnostic `ad4m://has_child` tree operations: `add_child` and
//! `get_children`.
//!
//! `instance_create(parent=…)` and `instance_query(parent=…)` cover the
//! class-aware side of the tree. These two are for the raw link: parents
//! that are not subject-class instances at all (`ad4m://self`, the plain-string
//! ids some apps hand out), and listing children regardless of class.
//!
//! They call the RPC methods `perspective.addLink` and `perspective.queryLinks` through the
//! handler map, so those methods' guards apply.

use super::{
    error_json, link_target, pretty, Ad4mMcpHandler, DEFAULT_QUERY_LIMIT, HAS_CHILD,
    MAX_QUERY_LIMIT,
};
use crate::mcp::tools::{decode, perspective_error};
use crate::types::DecoratedLinkExpression;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

/// Parameters for adding a child to a parent node
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddChildParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Parent node URI (e.g. a Channel's id, or `ad4m://self` for the
    /// perspective root). A bare string is wrapped as a literal URI.
    pub parent: String,
    /// Child node URI (e.g. a Message's id). A bare string is wrapped as a
    /// literal URI.
    pub child: String,
}

/// Parameters for listing the children of a parent node
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetChildrenParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Parent node URI to list the children of. A bare string is wrapped as
    /// a literal URI.
    pub parent: String,
    /// Maximum number of children to return — the most recent ones, in
    /// chronological order (default 100, max 500). total_count reports how
    /// many there are in all.
    pub limit: Option<usize>,
}

impl Ad4mMcpHandler {
    /// Link a child node under a parent via `ad4m://has_child`.
    #[tool(
        description = "Link a child node under a parent with ad4m://has_child — the generic containment tree apps build their structure on: items in a container, containers under ad4m://self, e.g. messages in a channel or tasks in a board. Class-agnostic: neither node needs to be a subject-class instance. Bare strings are wrapped as literal URIs. Prefer instance_create(parent=…) when creating a new instance, and instance_add_to_collection when the parent's class declares the collection; use this for nodes that are not instances (e.g. parent='ad4m://self') or to re-parent an existing instance."
    )]
    pub async fn add_child(&self, params: Parameters<AddChildParams>) -> String {
        let p = &params.0;
        let parent = p.parent.trim();
        let child = p.child.trim();
        if parent.is_empty() || child.is_empty() {
            return error_json("parent and child must both be non-empty URIs");
        }
        let params = json!({
            "uuid": p.perspective_id,
            "link": {
                "source": link_target(parent),
                "predicate": HAS_CHILD,
                "target": link_target(child),
            },
            "status": "shared",
        });
        match self.call_method("perspective.addLink", params).await {
            Ok(decorated) => {
                let decorated: DecoratedLinkExpression = match decode(decorated) {
                    Ok(decorated) => decorated,
                    Err(e) => return e,
                };
                pretty(&json!({
                    "success": true,
                    "link": {
                        "source": decorated.data.source,
                        "predicate": decorated.data.predicate,
                        "target": decorated.data.target,
                        "timestamp": decorated.timestamp,
                    },
                }))
            }
            Err(e) if e.code == 500 => error_json(format!("Error adding child: {}", e.message)),
            Err(e) => perspective_error(&p.perspective_id, e),
        }
    }

    /// List the `ad4m://has_child` children of a node, regardless of class.
    #[tool(
        description = "List the children of a node linked via ad4m://has_child, regardless of their class: id, timestamp and author of each child link, oldest first. Class-agnostic — works for ad4m://self (the perspective root, whose children are the top-level channels), for plain-string ids, and for instances. limit keeps the most recent N (default 100, max 500); total_count is the full number. To read children of one class with their property values use instance_query(class_name, parent) or instance_transcript."
    )]
    pub async fn get_children(&self, params: Parameters<GetChildrenParams>) -> String {
        let p = &params.0;
        let parent = p.parent.trim();
        if parent.is_empty() {
            return error_json("parent must be a non-empty URI");
        }
        let params = json!({
            "uuid": p.perspective_id,
            "source": link_target(parent),
            "predicate": HAS_CHILD,
        });
        let mut links: Vec<DecoratedLinkExpression> =
            match self.call_method("perspective.queryLinks", params).await {
                Ok(links) => match decode(links) {
                    Ok(links) => links,
                    Err(e) => return e,
                },
                Err(e) if e.code == 500 => {
                    return error_json(format!("Error getting children: {}", e.message))
                }
                Err(e) => return perspective_error(&p.perspective_id, e),
            };
        links.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));
        let total = links.len();
        let limit = p.limit.unwrap_or(DEFAULT_QUERY_LIMIT).min(MAX_QUERY_LIMIT);
        if links.len() > limit {
            links = links.split_off(links.len() - limit);
        }
        let children: Vec<serde_json::Value> = links
            .iter()
            .map(|l| {
                json!({
                    "id": l.data.target,
                    "timestamp": l.timestamp,
                    "author": l.author,
                })
            })
            .collect();
        pretty(&json!({
            "parent": link_target(parent),
            "count": children.len(),
            "total_count": total,
            "children": children,
        }))
    }
}

/// The tools answer as their RPC methods' guards do (#851).
#[cfg(test)]
mod guard_tests {
    use super::*;
    use crate::agent::capabilities::{
        perspective_query_capability, perspective_update_capability, ALL_CAPABILITY,
        PERSPECTIVE_CREATE_CAPABILITY,
    };
    use crate::mcp::tools::test_support::{app, handler_as, perspective, setup, status, user};

    // `add_child` checked PERSPECTIVE_CREATE and `get_children` no capability; the methods
    // check the update and the query capability on the perspective.
    #[tokio::test]
    async fn the_tree_tools_need_the_perspective_capabilities() {
        setup();
        let uuid = perspective(None).await;
        let add = |handler: Ad4mMcpHandler| {
            let uuid = uuid.clone();
            async move {
                handler
                    .add_child(Parameters(AddChildParams {
                        perspective_id: uuid,
                        parent: "ad4m://self".into(),
                        child: "ad4m://obj/child".into(),
                    }))
                    .await
            }
        };
        let list = |handler: Ad4mMcpHandler| {
            let uuid = uuid.clone();
            async move {
                handler
                    .get_children(Parameters(GetChildrenParams {
                        perspective_id: uuid,
                        parent: "ad4m://self".into(),
                        limit: None,
                    }))
                    .await
            }
        };

        let creator = add(handler_as(app(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]))).await;
        assert_eq!(status(&creator), Some(403), "{creator}");
        let reader = list(handler_as(app(vec![]))).await;
        assert_eq!(status(&reader), Some(403), "{reader}");

        let added = add(handler_as(app(vec![perspective_update_capability(vec![
            uuid.clone(),
        ])])))
        .await;
        assert!(added.contains("\"success\": true"), "{added}");
        let listed = list(handler_as(app(vec![perspective_query_capability(vec![
            uuid.clone(),
        ])])))
        .await;
        let listed: serde_json::Value = serde_json::from_str(&listed).expect(&listed);
        assert_eq!(listed["count"], 1, "{listed}");
        assert_eq!(listed["children"][0]["id"], "ad4m://obj/child");

        // A user who does not own the perspective finds nothing there.
        let stranger = list(handler_as(user(
            "tree.stranger@example.org",
            "did:key:mcp-tree-stranger",
            vec![ALL_CAPABILITY.clone()],
        )))
        .await;
        assert_eq!(status(&stranger), Some(404), "{stranger}");
        crate::perspectives::remove_perspective(&uuid).await;
    }
}

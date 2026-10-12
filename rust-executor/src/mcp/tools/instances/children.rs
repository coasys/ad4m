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
        let added = self
            .call::<DecoratedLinkExpression>("perspective.addLink", params, |e| {
                error_json(format!("Error adding child: {}", e.message))
            })
            .await;
        match added {
            Ok(decorated) => pretty(&json!({
                "success": true,
                "link": {
                    "source": decorated.data.source,
                    "predicate": decorated.data.predicate,
                    "target": decorated.data.target,
                    "timestamp": decorated.timestamp,
                },
            })),
            Err(e) => e,
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
        let queried = self
            .call::<Vec<DecoratedLinkExpression>>("perspective.queryLinks", params, |e| {
                error_json(format!("Error getting children: {}", e.message))
            })
            .await;
        let mut links = match queried {
            Ok(links) => links,
            Err(e) => return e,
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

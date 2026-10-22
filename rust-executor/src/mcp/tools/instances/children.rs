//! Containment through the built-in `Container` (#1446): `add_child` and
//! `get_children`.
//!
//! A node holds X when it links X with `ad4m://has_child`, whatever the node
//! is (`ad4m://self`, a plain-string id, an instance of any class), or when it
//! is an instance of a class whose `children` relation (by name, or aliased to
//! `children`) links X, such as a We block over `we://child`. Both tools take
//! that answer from the executor and look up no predicate themselves.
//! `instance_create(parent=…)` and `instance_query(parent=…)` read and write
//! the same containment for one class.

use super::{
    error_json, link_target, pretty, run_model_query, Ad4mMcpHandler, DEFAULT_QUERY_LIMIT,
    MAX_QUERY_LIMIT,
};
use crate::perspectives::model_query::builtin_classes::CONTAINER_URI;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

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
    /// Put a child node under a parent, through `Container`.
    #[tool(
        description = "Put a child node under a parent — the containment tree apps build their structure on: items in a container, containers under ad4m://self, e.g. messages in a channel, blocks in a document, tasks in a board. The parent decides how it holds the child: if it is an instance of a class whose children relation is its own (a block over we://child), that relation's adder runs; otherwise one ad4m://has_child link is written, which holds on any node. Class-agnostic: neither node needs to be a subject-class instance. Bare strings are wrapped as literal URIs. Prefer instance_create(parent=…) when creating a new instance; use this for an existing node (e.g. to file a channel under parent='ad4m://self') or to re-parent one."
    )]
    pub async fn add_child(&self, params: Parameters<AddChildParams>) -> String {
        let p = &params.0;
        let (mut perspective, agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let parent = p.parent.trim();
        let child = p.child.trim();
        if parent.is_empty() || child.is_empty() {
            return error_json("parent and child must both be non-empty URIs");
        }
        let (parent, child) = (link_target(parent), link_target(child));
        match perspective
            .add_to_container(&parent, &child, None, &agent_context)
            .await
        {
            Ok(()) => pretty(&json!({
                "success": true,
                "parent": parent,
                "child": child,
            })),
            Err(e) => error_json(format!("Error adding child: {e:#}")),
        }
    }

    /// List the children of a node, through `Container`.
    #[tool(
        description = "List the children of a node, regardless of their class: id, timestamp and author of each child's link, oldest first. A node's children are its ad4m://has_child targets, whatever the node is, plus, when it is an instance of a class whose children relation is its own (a block over we://child), that relation's targets. Works for ad4m://self (the perspective root, whose children are the top-level channels), for plain-string ids, and for instances. limit keeps the most recent N (default 100, max 500); total_count is the full number. To read children of one class with their property values use instance_query(class_name, parent); to read the ones that carry text as a transcript, instance_transcript."
    )]
    pub async fn get_children(&self, params: Parameters<GetChildrenParams>) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let parent = p.parent.trim();
        if parent.is_empty() {
            return error_json("parent must be a non-empty URI");
        }
        let parent = link_target(parent);
        // One Container row: its `children` are everything it holds, oldest
        // first, and `__links.children` the links behind them.
        let query = json!({
            "where": { "id": parent },
            "properties": ["children"],
            "links": ["children"],
        });
        let rows = match run_model_query(&perspective, CONTAINER_URI, &query).await {
            Ok((rows, _)) => rows,
            Err(e) => return error_json(format!("Error getting children: {e}")),
        };
        let row = rows.into_iter().next().unwrap_or(Value::Null);
        let ids: Vec<String> = row["children"]
            .as_array()
            .map(|c| {
                c.iter()
                    .filter_map(|v| v.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default();
        let links = row["__links"]["children"]
            .as_array()
            .cloned()
            .unwrap_or_default();
        let total = ids.len();
        let limit = p.limit.unwrap_or(DEFAULT_QUERY_LIMIT).min(MAX_QUERY_LIMIT);
        let children: Vec<Value> = ids
            .iter()
            .skip(total.saturating_sub(limit))
            .map(|id| {
                // The earliest link that holds it.
                let link = links
                    .iter()
                    .find(|l| l["data"]["target"].as_str() == Some(id.as_str()));
                json!({
                    "id": id,
                    "timestamp": link.map(|l| l["timestamp"].clone()).unwrap_or(Value::Null),
                    "author": link.map(|l| l["author"].clone()).unwrap_or(Value::Null),
                })
            })
            .collect();
        pretty(&json!({
            "parent": parent,
            "count": children.len(),
            "total_count": total,
            "children": children,
        }))
    }
}

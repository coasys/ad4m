//! `get_replies`: what was said in reply to an item, through the reply roles
//! (`perspectives::replies`), regardless of class.
//!
//! Containment answers what an item is in; this answers what answers it. An
//! app that keeps the two apart has replies that `get_children` never lists,
//! so a bot woken in a discussion needs this to read the discussion.

use super::{
    error_json, link_target, pretty, Ad4mMcpHandler, DEFAULT_QUERY_LIMIT, MAX_QUERY_LIMIT,
};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkQuery};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::{HashMap, HashSet, VecDeque};

/// How deep a walk through replies goes when no thread index is declared.
const MAX_REPLY_DEPTH: usize = 32;

/// Parameters for listing the replies to an item
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetRepliesParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// The item whose replies to list. A bare string is wrapped as a literal
    /// URI.
    pub item: String,
    /// false (default): the direct replies only. true: every reply under the
    /// item at any depth, each with the id it answers (`in_reply_to`), so the
    /// tree can be rebuilt.
    pub thread: Option<bool>,
    /// Maximum number of replies to return — the most recent ones, in
    /// chronological order (default 100, max 500). total_count reports how
    /// many there are in all.
    pub limit: Option<usize>,
}

/// Links from `source` through any of `predicates`, one lookup per predicate.
async fn links_from(
    perspective: &PerspectiveInstance,
    source: &str,
    predicates: &[String],
) -> Result<Vec<DecoratedLinkExpression>, String> {
    let mut links = Vec::new();
    for predicate in predicates {
        links.extend(
            perspective
                .get_links(&LinkQuery {
                    source: Some(source.to_string()),
                    predicate: Some(predicate.clone()),
                    ..Default::default()
                })
                .await
                .map_err(|e| format!("{e:#}"))?,
        );
    }
    Ok(links)
}

/// One reply: the link that made it one, and what it answers.
struct Reply {
    link: DecoratedLinkExpression,
    in_reply_to: String,
}

/// Every reply under `item`, at any depth, each once.
///
/// Through the thread index when it holds anything for `item`: one hop, then
/// one reply lookup per member to say what it answers. Otherwise a
/// breadth-first walk through the replies, bounded in depth, with a visited
/// set, since nothing stops a cycle.
async fn thread_of(perspective: &PerspectiveInstance, item: &str) -> Result<Vec<Reply>, String> {
    let replies = perspective.reply_predicates().await;
    let members = links_from(perspective, item, &perspective.thread_predicates().await).await?;
    if !members.is_empty() {
        let ids: HashSet<String> = members.iter().map(|l| l.data.target.clone()).collect();
        // What each member answers: the item, or another member.
        let mut parent_of: HashMap<String, String> = HashMap::new();
        for source in std::iter::once(item.to_string()).chain(ids.iter().cloned()) {
            for link in links_from(perspective, &source, &replies).await? {
                if ids.contains(&link.data.target) {
                    parent_of
                        .entry(link.data.target.clone())
                        .or_insert(source.clone());
                }
            }
        }
        let mut seen = HashSet::new();
        return Ok(members
            .into_iter()
            .filter(|l| seen.insert(l.data.target.clone()))
            .map(|link| Reply {
                in_reply_to: parent_of
                    .get(&link.data.target)
                    .cloned()
                    .unwrap_or_default(),
                link,
            })
            .collect());
    }

    let mut out = Vec::new();
    let mut seen = HashSet::from([item.to_string()]);
    let mut queue = VecDeque::from([(item.to_string(), 0usize)]);
    while let Some((source, depth)) = queue.pop_front() {
        if depth >= MAX_REPLY_DEPTH {
            continue;
        }
        for link in links_from(perspective, &source, &replies).await? {
            if !seen.insert(link.data.target.clone()) {
                continue;
            }
            queue.push_back((link.data.target.clone(), depth + 1));
            out.push(Reply {
                link,
                in_reply_to: source.clone(),
            });
        }
    }
    Ok(out)
}

impl Ad4mMcpHandler {
    /// List the replies to an item through the reply roles, regardless of class.
    #[tool(
        description = "List what was said in reply to an item: id, timestamp and author of each reply, oldest first. Follows ad4m://has_reply and any predicate the perspective declares as one, regardless of class — replies an app keeps apart from containment, which get_children does not list. thread=false (default) gives the direct replies; thread=true gives every reply under the item at any depth, each with in_reply_to, through ad4m://has_thread_member (or a declared thread index) when there is one, else by walking the replies. limit keeps the most recent N (default 100, max 500); total_count is the full number. Read a reply's content with instance_get."
    )]
    pub async fn get_replies(&self, params: Parameters<GetRepliesParams>) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let item = p.item.trim();
        if item.is_empty() {
            return error_json("item must be a non-empty URI");
        }
        let item = link_target(item);
        let thread = p.thread.unwrap_or(false);
        let mut replies = if thread {
            match thread_of(&perspective, &item).await {
                Ok(replies) => replies,
                Err(e) => return error_json(format!("Error getting replies: {e}")),
            }
        } else {
            let predicates = perspective.reply_predicates().await;
            match links_from(&perspective, &item, &predicates).await {
                Ok(links) => {
                    let mut seen = HashSet::new();
                    links
                        .into_iter()
                        .filter(|l| seen.insert(l.data.target.clone()))
                        .map(|link| Reply {
                            link,
                            in_reply_to: item.clone(),
                        })
                        .collect()
                }
                Err(e) => return error_json(format!("Error getting replies: {e}")),
            }
        };
        replies.sort_by(|a, b| a.link.timestamp.cmp(&b.link.timestamp));
        let total = replies.len();
        let limit = p.limit.unwrap_or(DEFAULT_QUERY_LIMIT).min(MAX_QUERY_LIMIT);
        if replies.len() > limit {
            replies = replies.split_off(replies.len() - limit);
        }
        let listed: Vec<serde_json::Value> = replies
            .iter()
            .map(|r| {
                let mut entry = json!({
                    "id": r.link.data.target,
                    "timestamp": r.link.timestamp,
                    "author": r.link.author,
                });
                if thread {
                    entry["in_reply_to"] = json!(r.in_reply_to);
                }
                entry
            })
            .collect();
        pretty(&json!({
            "item": item,
            "thread": thread,
            "count": listed.len(),
            "total_count": total,
            "replies": listed,
        }))
    }
}

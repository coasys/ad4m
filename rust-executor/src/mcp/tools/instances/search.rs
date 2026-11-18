//! `instance_search` — the rows whose title is nearest to a text, across classes.
//!
//! The search is [`crate::perspectives::interpretation::search::search_instances`]; the
//! interpretation harness offers this same tool.

use super::{error_json, pretty, Ad4mMcpHandler};
use crate::perspectives::interpretation::search::{search_instances, Scoring, SearchError};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

/// Parameters for searching instances by title
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceSearchParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// The text to search for: a title, or what the row is about.
    pub text: String,
    /// Optional subject class name as listed by describe_perspective. Without it, every
    /// class with an identity (title) property is searched.
    pub class_name: Option<String>,
    /// How many rows to return (default 10, max 50).
    pub k: Option<usize>,
    /// Optional parent URI: only rows contained in this node, as instance_query's parent.
    pub parent: Option<String>,
}

impl Ad4mMcpHandler {
    /// Search instances by the meaning of their title.
    #[tool(
        description = "Find existing rows by their title: the k rows (default 10, max 50) whose title is nearest to text, best first. Each hit has id, class, title and score. class_name limits the search to one class; parent to the rows one node contains. With an embedding model on the executor, the score is the cosine similarity of title vectors (0 to 1; a paraphrase scores high). Without one, the score is word overlap and the result says so in `scoring`. Read a hit in full with instance_get(class, id). Only classes with an identity (title) property are searched."
    )]
    pub async fn instance_search(&self, params: Parameters<InstanceSearchParams>) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        if p.text.trim().is_empty() {
            return error_json("text must not be empty");
        }
        let parent = p.parent.as_deref().map(str::trim).filter(|s| !s.is_empty());
        match search_instances(&perspective, &p.text, p.class_name.as_deref(), p.k, parent).await {
            Ok(result) => {
                let mut out = json!({
                    "count": result.hits.len(),
                    "searched": result.searched,
                    "hits": result.hits.iter().map(|h| json!({
                        "id": h.id,
                        "class": h.class,
                        "title": h.title,
                        "score": (h.score * 1000.0).round() / 1000.0,
                    })).collect::<Vec<_>>(),
                });
                match result.scoring {
                    Scoring::Embedding { model } => {
                        out["scoring"] = json!("embedding");
                        out["model"] = json!(model);
                    }
                    Scoring::WordOverlap { why } => {
                        out["scoring"] = json!("word_overlap");
                        out["note"] = json!(format!(
                            "Scores are word overlap, not meaning: {why}. A row with other words \
                             for the same thing scores low."
                        ));
                    }
                }
                pretty(&out)
            }
            Err(SearchError::Class(e)) => {
                super::common_error(&e).unwrap_or_else(|| error_json(format!("{e:?}")))
            }
            Err(SearchError::NoTitle(class)) => error_json(format!(
                "{class} has no identity (title) property, so its rows cannot be searched by \
                 title. Use instance_query with a filter."
            )),
            Err(SearchError::Read(e)) => error_json(format!("Error searching: {e:#}")),
        }
    }
}

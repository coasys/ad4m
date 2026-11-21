//! `instance_search`: the rows whose title is nearest to a text, across classes.
//!
//! The MCP tool and the interpretation harness call [`search_instances`]; the harness's
//! duplicate check ([`super::run`]) ranks with [`rank_rows`]. A row's title is the value of its
//! class's identity property; a class without one has no title and is not searched.
//!
//! With an embedding model, the score is the cosine similarity of the title vectors; the
//! vectors of the rows come from the `instance_embedding` table
//! ([`super::embedding_cache`]). Without one (or when the model fails), the score is word
//! overlap, and the result says so.

use super::embedding_cache::{default_embedding_model, embed_rows};
use super::graph::identity_property;
use crate::perspectives::instances::{link_target, resolve_class, InstanceError};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::collections::HashSet;
use std::sync::Arc;

/// Rows a search returns when the caller names no `k`.
pub const DEFAULT_SEARCH_K: usize = 10;
/// The most rows one search returns.
pub const MAX_SEARCH_K: usize = 50;
/// The most rows one search compares. `e2e_instance_embedding_timings` measured 0.8 to
/// 3.8 s for one search over 50,000 cached vectors; a larger perspective is searched in
/// part, and the result says so (narrow it with `class_name` or `parent`).
pub const MAX_SEARCH_CANDIDATES: usize = 50_000;

/// One row a search found.
#[derive(Debug, Clone, PartialEq, serde::Serialize)]
pub struct SearchHit {
    pub id: String,
    pub class: String,
    pub title: String,
    /// Cosine similarity of the title vectors, or word overlap (see [`Scoring`]).
    pub score: f32,
}

/// How a search scored.
#[derive(Debug, Clone, PartialEq)]
pub enum Scoring {
    /// Title vectors of this embedding model (an id).
    Embedding { model: String },
    /// Word overlap: the shared words over all words of the two texts. `why` says why
    /// there were no vectors.
    WordOverlap { why: String },
}

/// What a search found.
#[derive(Debug, Clone, PartialEq)]
pub struct SearchResult {
    /// Best first, at most `k`.
    pub hits: Vec<SearchHit>,
    pub scoring: Scoring,
    /// Rows the search compared.
    pub searched: usize,
    /// Rows with a title in scope, when there were more than [`MAX_SEARCH_CANDIDATES`]: the
    /// search compared only the first of them.
    pub capped_from: Option<usize>,
}

/// A row a search can return: its id, class and title.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Candidate {
    pub id: String,
    pub class: String,
    pub title: String,
}

/// The rows of `classes` that have a title, under `parent` when one is given (the rows it
/// contains, as `instance_query`'s `parent` reads them).
pub(crate) async fn load_candidates(
    perspective: &PerspectiveInstance,
    classes: &[(String, Arc<ModelShape>)],
    parent: Option<&str>,
) -> anyhow::Result<Vec<Candidate>> {
    let mut out = Vec::new();
    let mut seen: HashSet<(String, String)> = HashSet::new();
    for (class, shape) in classes {
        let Some(idp) = identity_property(shape) else {
            continue;
        };
        let mut query = serde_json::json!({ "properties": [idp.name] });
        if let Some(parent) = parent {
            query["parent"] = serde_json::json!({ "id": link_target(parent) });
        }
        let result = perspective
            .model_query(class, &query.to_string())
            .await
            .map_err(|e| anyhow::anyhow!("instance_search: reading {class} rows failed: {e:#}"))?;
        let result: serde_json::Value = serde_json::from_str(&result)?;
        for row in result
            .get("instances")
            .and_then(|v| v.as_array())
            .into_iter()
            .flatten()
        {
            let (Some(id), Some(title)) = (
                row.get("id").and_then(|v| v.as_str()),
                row.get(&idp.name).and_then(|v| v.as_str()),
            ) else {
                continue;
            };
            if title.trim().is_empty() || !seen.insert((class.clone(), id.to_string())) {
                continue;
            }
            out.push(Candidate {
                id: id.to_string(),
                class: class.clone(),
                title: title.to_string(),
            });
        }
    }
    Ok(out)
}

fn words(text: &str) -> HashSet<String> {
    text.split(|c: char| !c.is_alphanumeric())
        .filter(|w| !w.is_empty())
        .map(str::to_lowercase)
        .collect()
}

/// Word overlap of two texts: the words they share over the words of both (Jaccard), from
/// 0 to 1. Case and punctuation do not count.
pub(crate) fn word_overlap(a: &str, b: &str) -> f32 {
    let (a, b) = (words(a), words(b));
    let union = a.union(&b).count();
    if union == 0 {
        return 0.0;
    }
    a.intersection(&b).count() as f32 / union as f32
}

/// The `k` best of `scored`, best first; ties by id, so the order is the same each time.
fn best(mut scored: Vec<SearchHit>, k: usize) -> Vec<SearchHit> {
    scored.sort_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| a.id.cmp(&b.id))
    });
    scored.truncate(k);
    scored
}

/// Score `candidates` against `text` and keep the `k` best. With `model` (an embedding
/// model id or name), by cosine of the title vectors, cached per row on `perspective_uuid`;
/// without one, or when embedding fails, by word overlap.
pub(crate) async fn rank_rows(
    perspective_uuid: &str,
    text: &str,
    candidates: &[Candidate],
    k: usize,
    model: Option<&str>,
) -> SearchResult {
    let searched = candidates.len();
    let why = match model {
        None => "this executor has no embedding model".to_string(),
        Some(model) => match embedding_scores(perspective_uuid, text, candidates, model).await {
            Ok(scores) => {
                let hits = candidates
                    .iter()
                    .zip(scores)
                    .map(|(c, score)| hit(c, score))
                    .collect();
                return SearchResult {
                    hits: best_off_runtime(hits, k).await,
                    scoring: Scoring::Embedding {
                        model: model.to_string(),
                    },
                    searched,
                    capped_from: None,
                };
            }
            Err(e) => {
                log::warn!("instance_search: embedding with '{model}' failed, word overlap: {e:#}");
                format!("the embedding model '{model}' failed: {e:#}")
            }
        },
    };
    let hits = candidates
        .iter()
        .map(|c| hit(c, word_overlap(text, &c.title)))
        .collect();
    SearchResult {
        hits: best_off_runtime(hits, k).await,
        scoring: Scoring::WordOverlap { why },
        searched,
        capped_from: None,
    }
}

/// [`best`] on a blocking thread: the sort of a large candidate set is not async work.
async fn best_off_runtime(scored: Vec<SearchHit>, k: usize) -> Vec<SearchHit> {
    tokio::task::spawn_blocking(move || best(scored, k))
        .await
        .unwrap_or_default()
}

fn hit(c: &Candidate, score: f32) -> SearchHit {
    SearchHit {
        id: c.id.clone(),
        class: c.class.clone(),
        title: c.title.clone(),
        score,
    }
}

/// The cosine of `text`'s vector with each candidate title's, in candidate order.
async fn embedding_scores(
    perspective_uuid: &str,
    text: &str,
    candidates: &[Candidate],
    model: &str,
) -> anyhow::Result<Vec<f32>> {
    let rows: Vec<(String, String)> = candidates
        .iter()
        .map(|c| (c.id.clone(), c.title.clone()))
        .collect();
    let (vectors, _) = embed_rows(perspective_uuid, model, &rows).await?;
    let query = super::dedup::embed_via_ai_service(model, &[text.to_string()])
        .await?
        .into_iter()
        .next()
        .ok_or_else(|| anyhow::anyhow!("no vector for the search text"))?;
    tokio::task::spawn_blocking(move || vectors.iter().map(|v| cosine(&query, v)).collect())
        .await
        .map_err(|e| anyhow::anyhow!("scoring failed: {e}"))
}

/// Cosine of two vectors; 0 when their lengths differ (vectors of two models).
pub(crate) fn cosine(a: &[f32], b: &[f32]) -> f32 {
    if a.len() != b.len() {
        return 0.0;
    }
    super::dedup::cosine_similarity(a, b)
}

/// Why a search did not run.
#[derive(Debug)]
pub enum SearchError {
    Class(InstanceError),
    /// The class has no identity property, so its rows have no title.
    NoTitle(String),
    Read(anyhow::Error),
}

/// The `k` rows (at most [`MAX_SEARCH_K`]) whose title is nearest to `text`: of `class_name`,
/// else of every class with an identity property; under `parent` when given. Scored with
/// the executor's default embedding model, else by word overlap.
pub async fn search_instances(
    perspective: &PerspectiveInstance,
    text: &str,
    class_name: Option<&str>,
    k: Option<usize>,
    parent: Option<&str>,
) -> Result<SearchResult, SearchError> {
    let classes: Vec<(String, Arc<ModelShape>)> = match class_name {
        Some(name) => {
            let (name, shape) = resolve_class(perspective, name)
                .await
                .map_err(SearchError::Class)?;
            if identity_property(&shape).is_none() {
                return Err(SearchError::NoTitle(name));
            }
            vec![(name, shape)]
        }
        None => {
            let mut names = perspective
                .get_subject_classes_from_shacl()
                .await
                .map_err(|e| SearchError::Read(anyhow::anyhow!("{e:#}")))?;
            names.sort();
            names
                .into_iter()
                .filter_map(|name| perspective.get_shape(&name).ok().map(|s| (name, s)))
                .filter(|(_, shape)| identity_property(shape).is_some())
                .collect()
        }
    };
    let mut candidates = load_candidates(perspective, &classes, parent)
        .await
        .map_err(SearchError::Read)?;
    let capped_from = cap_candidates(&mut candidates, MAX_SEARCH_CANDIDATES);
    let k = k.unwrap_or(DEFAULT_SEARCH_K).clamp(1, MAX_SEARCH_K);
    let model = default_embedding_model();
    let mut result = rank_rows(&perspective.uuid, text, &candidates, k, model.as_deref()).await;
    result.capped_from = capped_from;
    Ok(result)
}

/// Keep the first `max` of `candidates`. `Some(count before)` when some were dropped.
fn cap_candidates(candidates: &mut Vec<Candidate>, max: usize) -> Option<usize> {
    let total = candidates.len();
    (total > max).then(|| {
        candidates.truncate(max);
        total
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn candidates(rows: &[(&str, &str)]) -> Vec<Candidate> {
        rows.iter()
            .map(|(id, title)| Candidate {
                id: id.to_string(),
                class: "Topic".into(),
                title: title.to_string(),
            })
            .collect()
    }

    #[test]
    fn a_search_compares_at_most_the_candidate_cap_and_says_from_how_many() {
        let mut rows = candidates(&[("a", "x"), ("b", "y"), ("c", "z")]);
        assert_eq!(cap_candidates(&mut rows, 3), None);
        assert_eq!(rows.len(), 3);
        assert_eq!(cap_candidates(&mut rows, 2), Some(3));
        assert_eq!(rows.len(), 2);
    }

    #[test]
    fn word_overlap_is_shared_words_over_all_words() {
        assert_eq!(word_overlap("Ship the MVP", "ship the mvp!"), 1.0);
        assert_eq!(word_overlap("alpha beta", "beta gamma"), 1.0 / 3.0);
        assert_eq!(word_overlap("alpha", "gamma"), 0.0);
        assert_eq!(word_overlap("", ""), 0.0);
    }

    #[tokio::test]
    async fn without_an_embedding_model_a_search_ranks_by_word_overlap_and_says_so() {
        let rows = candidates(&[
            ("a", "Budget for the retreat"),
            ("b", "Staging database setup"),
            ("c", "Database migration plan"),
        ]);
        let result = rank_rows("p", "set up the staging database", &rows, 2, None).await;
        assert_eq!(
            result.scoring,
            Scoring::WordOverlap {
                why: "this executor has no embedding model".into()
            }
        );
        let ids: Vec<&str> = result.hits.iter().map(|h| h.id.as_str()).collect();
        assert_eq!(ids, vec!["b", "c"]);
        assert_eq!(result.searched, 3);
    }

    #[tokio::test]
    async fn a_model_that_fails_falls_back_to_word_overlap_and_names_the_failure() {
        crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        let rows = candidates(&[("a", "Staging database")]);
        let result = rank_rows("p", "staging database", &rows, 5, Some("gone")).await;
        match result.scoring {
            Scoring::WordOverlap { why } => assert!(why.contains("'gone' failed"), "{why}"),
            other => panic!("expected word overlap, got {other:?}"),
        }
        assert_eq!(result.hits[0].score, 1.0);
    }

    #[tokio::test]
    async fn with_an_embedding_model_a_search_ranks_by_cosine_of_cached_vectors() {
        use super::super::embedding_cache::tests::word_embedder;
        crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        crate::ai_service::AIService::init_global_instance()
            .await
            .unwrap();
        let mut server = mockito::Server::new_async().await;
        let (id, embedded) = word_embedder(&mut server, "search-embed").await;
        let rows = candidates(&[
            ("a", "Budget for the retreat"),
            ("b", "Calling module with WebRTC"),
            ("c", "Lunch order"),
        ]);
        let result = rank_rows("p", "finish the webrtc calls", &rows, 3, Some(&id)).await;
        assert_eq!(result.scoring, Scoring::Embedding { model: id.clone() });
        assert_eq!(result.hits[0].id, "b");
        // 3 rows + the search text.
        assert_eq!(embedded.load(std::sync::atomic::Ordering::SeqCst), 4);
        // A second search embeds only its text: the rows come from the cache.
        rank_rows("p", "webrtc", &rows, 3, Some(&id)).await;
        assert_eq!(embedded.load(std::sync::atomic::Ordering::SeqCst), 5);
    }
}

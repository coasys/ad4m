//! The duplicate check of a harness pass: which existing rows a new row may be.
//!
//! The rows are the pass's existing set (its `existing_instance_context`, so the processor's
//! `existingScope` holds), compared by title within one class. Two bands, from the
//! processor's [`DedupStrategy`]:
//!
//! - *clear*: a score at or above `threshold`. The pass merges the new row into the existing
//!   one at its end ([`PassDuplicates::clear_merges`]), and the `{class}_create` reply says so.
//! - *unclear*: a score from `unclear_from` up to `threshold`. The `{class}_create` reply
//!   lists the rows with title, id and score; the model decides with `propose_same_as`.
//!   With no decision the new row stays.
//!
//! A semantic strategy scores by the cosine of title vectors ([`super::embedding_cache`]).
//! Normalized strings have no scores: an equal title is a clear match, and there is no
//! unclear band. A pass that does not merge at its end (`dedup_on_drain` off) shows a clear
//! match as unclear: the model decides each.
//!
//! TODO(#1804): the single-shot fallback of the harness (PR 1) has no tools, so its unclear
//! rows get no decision and stay new. The design gives it one short second call with the
//! new rows, their hits and the full old rows (at most 3 for each new row).

use super::dedup::{semantic_embedding_model, DedupStrategy};
use super::embedding_cache::embed_rows;
use super::graph::{class_label, identity_property, instances_by_class, normalize_identity};
use super::search::{cosine, Candidate};
use super::types::{ExistingInstances, InterpretationOp};
use crate::ai_service::harness::propose::{DuplicateCheck, NearRow};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use async_trait::async_trait;
use serde_json::{Map, Value};
use std::collections::HashMap;
use tokio::sync::Mutex;

/// The most unclear rows one `{class}_create` reply lists.
pub(crate) const MAX_UNCLEAR_ROWS: usize = 3;

#[derive(Debug, Clone, PartialEq)]
enum Bands {
    Normalized,
    Semantic {
        model: String,
        threshold: f32,
        unclear_from: f32,
    },
}

pub(crate) struct PassDuplicates {
    perspective: PerspectiveInstance,
    rows: HashMap<String, Vec<Candidate>>,
    identity_props: HashMap<String, String>,
    bands: Bands,
    /// Whether the pass merges clear matches at its end.
    merges: bool,
    /// Per class, the vectors of `rows[class]`, once read.
    row_vectors: Mutex<HashMap<String, Vec<Vec<f32>>>>,
    /// The vectors of new titles, once embedded.
    title_vectors: Mutex<HashMap<String, Vec<f32>>>,
}

impl PassDuplicates {
    pub(crate) fn new(
        perspective: &PerspectiveInstance,
        shapes: &[ModelShape],
        existing: &ExistingInstances,
        strategy: &DedupStrategy,
        merges: bool,
    ) -> Self {
        let rows = instances_by_class(existing)
            .into_iter()
            .map(|(class, rows)| {
                let rows = rows
                    .into_iter()
                    .filter(|r| !r.title.trim().is_empty())
                    .map(|r| Candidate {
                        id: r.id.clone(),
                        class: class.clone(),
                        title: r.title.clone(),
                    })
                    .collect();
                (class, rows)
            })
            .collect();
        let identity_props = shapes
            .iter()
            .filter_map(|s| {
                identity_property(s).map(|p| (class_label(&s.target_class, shapes), p.name.clone()))
            })
            .collect();
        let bands = match (strategy, semantic_embedding_model(strategy)) {
            (
                DedupStrategy::Semantic {
                    threshold,
                    unclear_from,
                    ..
                },
                Some(model),
            ) => Bands::Semantic {
                model,
                threshold: *threshold,
                unclear_from: *unclear_from,
            },
            _ => Bands::Normalized,
        };
        Self {
            perspective: perspective.clone(),
            rows,
            identity_props,
            bands,
            merges,
            row_vectors: Mutex::new(HashMap::new()),
            title_vectors: Mutex::new(HashMap::new()),
        }
    }

    /// The title of a new row of `class` with `values`: its identity property's value.
    fn title_of<'a>(&self, class: &str, values: &'a Map<String, Value>) -> Option<&'a str> {
        let idp = self.identity_props.get(class)?;
        values
            .get(idp)
            .and_then(Value::as_str)
            .filter(|t| !t.trim().is_empty())
    }

    /// The rows of `class` with their score against `title`, best first.
    async fn scored(&self, class: &str, title: &str) -> anyhow::Result<Vec<(&Candidate, f32)>> {
        let Some(rows) = self.rows.get(class).filter(|r| !r.is_empty()) else {
            return Ok(Vec::new());
        };
        let mut scored: Vec<(&Candidate, f32)> = match &self.bands {
            Bands::Normalized => {
                let wanted = normalize_identity(title);
                rows.iter()
                    .filter(|r| normalize_identity(&r.title) == wanted)
                    .map(|r| (r, 1.0))
                    .collect()
            }
            Bands::Semantic { model, .. } => {
                let query = self.title_vector(model, title).await?;
                let vectors = self.class_vectors(model, class, rows).await?;
                rows.iter()
                    .zip(vectors.iter())
                    .map(|(r, v)| (r, cosine(&query, v)))
                    .collect()
            }
        };
        scored.sort_by(|a, b| {
            b.1.partial_cmp(&a.1)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then_with(|| a.0.id.cmp(&b.0.id))
        });
        Ok(scored)
    }

    async fn title_vector(&self, model: &str, title: &str) -> anyhow::Result<Vec<f32>> {
        if let Some(v) = self.title_vectors.lock().await.get(title) {
            return Ok(v.clone());
        }
        let v = super::dedup::embed_via_ai_service(model, &[title.to_string()])
            .await?
            .into_iter()
            .next()
            .unwrap_or_default();
        self.title_vectors
            .lock()
            .await
            .insert(title.to_string(), v.clone());
        Ok(v)
    }

    async fn class_vectors(
        &self,
        model: &str,
        class: &str,
        rows: &[Candidate],
    ) -> anyhow::Result<Vec<Vec<f32>>> {
        let mut cache = self.row_vectors.lock().await;
        if let Some(v) = cache.get(class) {
            return Ok(v.clone());
        }
        let pairs: Vec<(String, String)> = rows
            .iter()
            .map(|r| (r.id.clone(), r.title.clone()))
            .collect();
        let (vectors, stats) = embed_rows(&self.perspective.uuid, model, &pairs).await?;
        log::info!(
            "interpretation duplicates: {class} rows cached={} embedded={}",
            stats.cached,
            stats.embedded
        );
        cache.insert(class.to_string(), vectors.clone());
        Ok(vectors)
    }

    /// The band of `score`: `Some(true)` clear, `Some(false)` unclear, `None` neither.
    fn band(&self, score: f32) -> Option<bool> {
        let (threshold, unclear_from) = match &self.bands {
            Bands::Normalized => (1.0, 1.0),
            Bands::Semantic {
                threshold,
                unclear_from,
                ..
            } => (*threshold, *unclear_from),
        };
        if score >= threshold {
            Some(self.merges)
        } else if score >= unclear_from {
            Some(false)
        } else {
            None
        }
    }

    /// The existing row each Create of `ops` merges into at the end of the pass: its best
    /// match at or above the threshold of a semantic strategy. Normalized strings merge in
    /// `dedup_ops_against_existing`. A failed embedding merges nothing (with a warning):
    /// the equal-title merge still runs.
    pub(crate) async fn clear_merges(&self, ops: &[InterpretationOp]) -> HashMap<String, String> {
        let mut out = HashMap::new();
        if !self.merges || matches!(self.bands, Bands::Normalized) {
            return out;
        }
        for op in ops {
            let InterpretationOp::Create {
                base,
                class,
                values,
            } = op
            else {
                continue;
            };
            let Some(title) = self.title_of(class, values) else {
                continue;
            };
            match self.scored(class, title).await {
                Ok(scored) => {
                    if let Some((row, score)) = scored.first() {
                        if self.band(*score) == Some(true) {
                            log::info!(
                                "interpretation duplicates: {class} {title:?} → {} ({score:.3})",
                                row.id
                            );
                            out.insert(base.clone(), row.id.clone());
                        }
                    }
                }
                Err(e) => log::warn!(
                    "interpretation duplicates: scoring {class} {title:?} failed, no semantic \
                     merge: {e:#}"
                ),
            }
        }
        out
    }
}

#[async_trait]
impl DuplicateCheck for PassDuplicates {
    async fn near(&self, class: &str, values: &Map<String, Value>) -> Vec<NearRow> {
        let Some(title) = self.title_of(class, values) else {
            return Vec::new();
        };
        let scored = match self.scored(class, title).await {
            Ok(scored) => scored,
            Err(e) => {
                log::warn!("interpretation duplicates: scoring {class} {title:?} failed: {e:#}");
                return Vec::new();
            }
        };
        let mut out = Vec::new();
        for (row, score) in scored {
            match self.band(score) {
                Some(true) => {
                    // The best row is the merge; the others do not matter then.
                    return vec![NearRow {
                        id: row.id.clone(),
                        title: row.title.clone(),
                        score,
                        clear: true,
                    }];
                }
                Some(false) if out.len() < MAX_UNCLEAR_ROWS => out.push(NearRow {
                    id: row.id.clone(),
                    title: row.title.clone(),
                    score,
                    clear: false,
                }),
                _ => break,
            }
        }
        out
    }

    async fn is_row_of(&self, class: &str, id: &str) -> bool {
        if self
            .rows
            .get(class)
            .is_some_and(|rows| rows.iter().any(|r| r.id == id))
        {
            return true;
        }
        matches!(
            crate::perspectives::instances::fetch_instance(&self.perspective, class, id).await,
            Ok(Some(_))
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai_service::harness::propose::{
        class_propose_shape_from_shacl, ProposalBuffer, ProposeWritesProvider, SameAsBuffer,
        PROPOSE_SAME_AS,
    };
    use crate::ai_service::harness::provider::{ToolProvider, ToolSchema};
    use crate::perspectives::interpretation::embedding_cache::tests::{word_embedder, word_vector};
    use crate::perspectives::interpretation::{drain_pass_ops, ExistingLinks, InstanceContext};
    use crate::perspectives::interpretation_test_support::{
        existing_map, setup_perspective_no_llm,
    };
    use serde_json::json;
    use std::collections::BTreeMap;
    use std::sync::Arc;

    /// No read tools: the tests call the propose tools only.
    struct NoTools;

    #[async_trait]
    impl ToolProvider for NoTools {
        async fn tools(&self) -> Vec<ToolSchema> {
            Vec::new()
        }
        async fn call(&self, name: &str, _args: Value) -> anyhow::Result<String> {
            Err(anyhow::anyhow!("no tool {name}"))
        }
    }

    /// Task: a title (identity), an owner, and `blocks` → Task. No fixed-value property, so
    /// `task_create` needs the title only.
    const TASK: &str = r#"{
  "target_class":"ns://Task",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://task"}],
  "properties":[
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://owner","target":"value"}]},
    {"path":"ns://blocks","name":"blocks","relation_kind":"hasMany","target_class_name":"Task","class":"ns://TaskShape"}
  ]
}"#;

    const WEBRTC: &str = "soa://old/webrtc";
    const RETREAT: &str = "soa://old/retreat";

    fn existing() -> ExistingInstances {
        existing_map(
            [
                (WEBRTC, "Finish the WebRTC calling module"),
                (RETREAT, "Budget for the retreat"),
            ]
            .into_iter()
            .map(|(id, title)| InstanceContext {
                id: id.into(),
                title: title.into(),
                class: "Task".into(),
                properties: BTreeMap::new(),
                updated_at: String::new(),
            })
            .collect(),
        )
    }

    fn score(a: &str, b: &str) -> f32 {
        cosine(&word_vector(a), &word_vector(b))
    }

    /// A harness pass on a perspective with [`TASK`], the two existing
    /// rows above, and the word-hash embedder: its tools, its buffers and its check.
    struct Pass {
        perspective: PerspectiveInstance,
        shapes: Vec<ModelShape>,
        tools: ProposeWritesProvider<NoTools>,
        buffer: ProposalBuffer,
        same_as: SameAsBuffer,
        duplicates: Arc<PassDuplicates>,
        _server: mockito::ServerGuard,
    }

    async fn pass(strategy: impl FnOnce(String) -> DedupStrategy, merges: bool) -> Pass {
        let (perspective, shapes, _) = setup_perspective_no_llm(&[("Task", TASK)]).await;
        crate::ai_service::AIService::init_global_instance()
            .await
            .unwrap();
        let mut server = mockito::Server::new_async().await;
        let (model, _) =
            word_embedder(&mut server, &format!("band-{}", uuid::Uuid::new_v4())).await;
        let duplicates = Arc::new(PassDuplicates::new(
            &perspective,
            &shapes,
            &existing(),
            &strategy(model),
            merges,
        ));
        let classes = crate::mcp::shacl::load_classes(&perspective).await;
        let buffer = ProposalBuffer::new();
        let same_as = SameAsBuffer::new();
        let tools = ProposeWritesProvider::new(
            Arc::new(NoTools),
            classes.iter().map(class_propose_shape_from_shacl).collect(),
            buffer.clone(),
            "soa://ext/".into(),
        )
        .with_duplicate_check(duplicates.clone(), same_as.clone());
        Pass {
            perspective,
            shapes,
            tools,
            buffer,
            same_as,
            duplicates,
            _server: server,
        }
    }

    fn semantic(model: String) -> DedupStrategy {
        // Threshold and band set for the word-hash vectors of the titles below.
        DedupStrategy::Semantic {
            model,
            threshold: 0.9,
            unclear_from: 0.45,
        }
    }

    fn created(reply: &str) -> String {
        reply
            .lines()
            .next()
            .and_then(|l| l.strip_prefix("created: "))
            .unwrap_or_else(|| panic!("not a create reply: {reply}"))
            .to_string()
    }

    impl Pass {
        async fn create(&self, title: &str, owner: Option<&str>) -> String {
            let mut args = json!({ "title": title });
            if let Some(owner) = owner {
                args["owner"] = json!(owner);
            }
            self.tools.call("task_create", args).await.unwrap()
        }

        async fn drain(&self, merges: bool) -> Vec<InterpretationOp> {
            drain_pass_ops(
                self.buffer.drain(),
                self.same_as.drain(),
                &self.duplicates,
                merges,
                &existing(),
                &self.shapes,
                &ExistingLinks::new(),
            )
            .await
        }
    }

    // The two bands of a semantic strategy, from the create replies to the end of the pass.
    #[tokio::test]
    async fn clear_rows_merge_unclear_rows_reach_the_model_and_its_decision_merges() {
        let p = pass(semantic, true).await;
        let (paraphrase, related) = ("WebRTC calling work", "Retreat budget planning");
        assert!(
            score(paraphrase, "Finish the WebRTC calling module") >= 0.45
                && score(paraphrase, "Finish the WebRTC calling module") < 0.9,
            "fixture: {paraphrase} must be unclear"
        );
        assert!(
            score(related, "Budget for the retreat") >= 0.45
                && score(related, "Budget for the retreat") < 0.9,
            "fixture: {related} must be unclear"
        );

        // Clear: the reply says the row is merged, and names the id to link to.
        let reply = p
            .create("Finish the WebRTC calling module", Some("James"))
            .await;
        assert!(
            reply.contains(&format!("same as the existing Task {WEBRTC}")),
            "{reply}"
        );
        assert!(reply.contains("merged into it"), "{reply}");
        let clear = created(&reply);

        // Unclear: the reply lists the row with title, id and score, and the tool to decide.
        let reply = p.create(paraphrase, None).await;
        assert!(
            reply.contains(&format!(
                "- {WEBRTC} \"Finish the WebRTC calling module\" score 0."
            )),
            "{reply}"
        );
        assert!(reply.contains(PROPOSE_SAME_AS), "{reply}");
        assert!(!reply.contains("merged into it"), "{reply}");
        let decided = created(&reply);

        // Unclear, and the model does not decide: the new row stays.
        let reply = p.create(related, None).await;
        assert!(reply.contains(RETREAT), "{reply}");
        let undecided = created(&reply);

        // Neither band: the reply is the bare create.
        let reply = p.create("Lunch order", None).await;
        assert!(!reply.contains('\n'), "{reply}");
        let unrelated = created(&reply);

        // A link to the row the model will merge.
        p.tools
            .call(
                "task_propose_link_child",
                json!({ "task_uri": unrelated, "predicate": "ns://blocks", "linked_uri": decided }),
            )
            .await
            .unwrap();

        // The decision is checked, then recorded.
        let err = p
            .tools
            .call(
                PROPOSE_SAME_AS,
                json!({ "new": "soa://ext/task/made-up", "existing": WEBRTC }),
            )
            .await
            .unwrap_err();
        assert!(
            err.to_string().contains("`created:` reply of this pass"),
            "{err}"
        );
        let err = p
            .tools
            .call(
                PROPOSE_SAME_AS,
                json!({ "new": decided, "existing": "soa://nothing/here" }),
            )
            .await
            .unwrap_err();
        assert!(err.to_string().contains("existing Task row"), "{err}");
        let ok = p
            .tools
            .call(
                PROPOSE_SAME_AS,
                json!({ "new": decided, "existing": WEBRTC }),
            )
            .await
            .unwrap();
        assert!(ok.starts_with("recorded:"), "{ok}");

        let ops = p.drain(true).await;
        let creates: Vec<&str> = ops
            .iter()
            .filter_map(|op| match op {
                InterpretationOp::Create { base, .. } => Some(base.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(
            creates,
            vec![undecided.as_str(), unrelated.as_str()],
            "{ops:#?}"
        );
        // The clear row is an update of the old one, its title kept.
        assert!(ops.iter().any(|op| matches!(op,
            InterpretationOp::Update { base, values, .. }
                if base == WEBRTC && values.get("owner") == Some(&json!("James")) && !values.contains_key("title"))),
            "{ops:#?}");
        assert!(!ops.iter().any(|op| matches!(op, InterpretationOp::Create { base, .. } if *base == clear || *base == decided)));
        // The link to the merged row points to the old row.
        let links: Vec<(String, String)> = ops
            .iter()
            .filter_map(|op| match op {
                InterpretationOp::AddLinks { links, .. } => Some(links.clone()),
                _ => None,
            })
            .flatten()
            .map(|l| (l.source, l.target))
            .collect();
        assert_eq!(links, vec![(unrelated.clone(), WEBRTC.to_string())]);
        drop(p.perspective);
    }

    // A pass that does not merge at its end (the RPC path) shows a clear row as unclear: the
    // model decides each, and nothing merges without a decision.
    #[tokio::test]
    async fn a_pass_without_merges_shows_clear_rows_as_unclear() {
        let p = pass(semantic, false).await;
        let reply = p.create("Finish the WebRTC calling module", None).await;
        assert!(reply.contains(PROPOSE_SAME_AS), "{reply}");
        assert!(!reply.contains("merged into it"), "{reply}");
        let ops = p.drain(false).await;
        assert!(
            matches!(ops.as_slice(), [InterpretationOp::Create { .. }]),
            "{ops:#?}"
        );
    }

    // Normalized strings: an equal title is the clear row, merged as before; no unclear band.
    #[tokio::test]
    async fn normalized_strings_merge_equal_titles_and_have_no_unclear_band() {
        let p = pass(|_| DedupStrategy::NormalizedString, true).await;
        let reply = p.create("  finish the webrtc CALLING module ", None).await;
        assert!(
            reply.contains(&format!("same as the existing Task {WEBRTC}")),
            "{reply}"
        );
        let reply = p.create("WebRTC calling work", None).await;
        assert!(!reply.contains('\n'), "no band without scores: {reply}");
        let ops = p.drain(true).await;
        assert!(
            ops.iter()
                .any(|op| matches!(op, InterpretationOp::Update { base, .. } if base == WEBRTC)),
            "{ops:#?}"
        );
        assert_eq!(
            ops.iter()
                .filter(|op| matches!(op, InterpretationOp::Create { .. }))
                .count(),
            1,
            "{ops:#?}"
        );
    }
}

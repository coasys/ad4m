//! The `existing` block of an interpretation prompt, sized to the model's
//! window (#1804).
//!
//! [`existing_instance_context`](super::existing_instance_context) returns
//! every instance of each class in scope, with no limit. A space that holds
//! many calls grows the block on every call until the prompt no longer fits
//! the window (staging, 2026-11-18: 504 rows, about 74k tokens). This module
//! picks the rows the prompt shows:
//!
//! - **Budget.** The block gets [`EXISTING_SHARE`] of the window that is left
//!   after the rest of the prompt (system prompt, examples, schema, transcript,
//!   flows) and [`OUTPUT_RESERVE_TOKENS`].
//! - **Order.** Rows under the pass's `base_prefix` (the records this
//!   processor's scope minted) come first. Then, when the pass has a semantic
//!   dedup embedding model, the rows most similar to the transcript; else, and
//!   when embedding fails, the most recently changed rows.
//! - **Short ids** (the single-shot path only, see [`Compact`]): each id is
//!   shown as a short handle (`P1`, `P2`, ...). [`Handles`] maps the model's
//!   output back to the full ids. Values are always shown whole: a row that
//!   does not fit is left out, never shortened.
//!
//! The selection only changes what the prompt shows. Dedup
//! (`resolve_already_present*`, `dedup_ops_against_existing`) and the planner
//! still get the full existing set, so a row the prompt did not show still
//! stops a duplicate.

use super::dedup::{cosine_similarity, embed_via_ai_service};
use super::prompt::existing_entry_json;
use super::types::{ExistingInstances, InstanceContext, ProposedInstance, TranscriptTurn};
use super::{build_interpretation_input, class_label};
use crate::ai_service::{estimate_task_prompt_tokens, estimate_token_count};
use crate::perspectives::flow_context::FlowContext;
use crate::perspectives::model_query::types::ModelShape;
use crate::types::AITask;
use std::collections::{BTreeMap, HashMap, HashSet};

/// The share of the free window the `existing` block may use. The free window
/// is the window minus the rest of the prompt and [`OUTPUT_RESERVE_TOKENS`].
pub(crate) const EXISTING_SHARE: f64 = 0.25;

/// Tokens kept free for the answer: the largest output cap an interpretation
/// pass asks for (`AIService::interpretation_max_output_tokens`).
pub(crate) const OUTPUT_RESERVE_TOKENS: usize = 8_192;

/// The window assumed when the model's window is not known (no `max_num_ctx`
/// on the endpoint and no answer from the model server).
pub(crate) const ASSUMED_WINDOW_TOKENS: usize = 65_536;

/// The most characters of one secondary value in the text an existing row is
/// embedded from (the similarity order only; the prompt shows values whole).
const EMBED_VALUE_MAX_CHARS: usize = 200;

/// The most characters of one transcript turn sent to the embedding model.
const EMBED_TURN_MAX_CHARS: usize = 1_000;

/// Whether the prompt shows short handles or full ids. The single-shot path
/// reads the model's answer back through [`Handles`], so it is compact. The
/// harness path is not: its tools take full ids.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Compact {
    Yes,
    No,
}

/// The token budget of the `existing` block: [`EXISTING_SHARE`] of what the
/// window leaves after `other_tokens` (the prompt without `existing` rows,
/// system prompt and examples included) and [`OUTPUT_RESERVE_TOKENS`].
pub(crate) fn existing_budget_tokens(window: Option<usize>, other_tokens: usize) -> usize {
    let window = window.unwrap_or(ASSUMED_WINDOW_TOKENS);
    let free = window
        .saturating_sub(other_tokens)
        .saturating_sub(OUTPUT_RESERVE_TOKENS);
    (free as f64 * EXISTING_SHARE) as usize
}

/// How the model's output names the rows a compact prompt showed, and how to
/// read it back. Empty for a prompt that is not compact.
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct Handles {
    /// Handle (`P1`) → full id.
    to_id: HashMap<String, String>,
}

impl Handles {
    /// The full id `handle` stands for, if it is one of this prompt's handles.
    pub(crate) fn id_of(&self, handle: &str) -> Option<&str> {
        self.to_id.get(handle).map(String::as_str)
    }

    /// The model's proposals with every handle replaced by its full id: the
    /// upsert `id` and the refs in each forward relation field. A string that
    /// is not a handle stays as it is, so an unknown handle goes the way an
    /// unknown id goes today (the planner routes it to a `Create`). Values
    /// pass through unchanged: the prompt showed them whole.
    pub(crate) fn resolve(
        &self,
        instances: Vec<ProposedInstance>,
        shapes: &[ModelShape],
    ) -> Vec<ProposedInstance> {
        if self.to_id.is_empty() {
            return instances;
        }
        let relations: HashMap<String, HashSet<&str>> = shapes
            .iter()
            .map(|s| {
                let names = s
                    .include_relations
                    .iter()
                    .filter(|r| r.direction == "forward")
                    .map(|r| r.name.as_str())
                    .collect();
                (class_label(&s.target_class, shapes), names)
            })
            .collect();
        instances
            .into_iter()
            .map(|mut inst| {
                if let Some(full) = inst.id.as_deref().and_then(|h| self.id_of(h)) {
                    inst.id = Some(full.to_string());
                }
                if let Some(names) = relations.get(&inst.class) {
                    for name in names {
                        if let Some(value) = inst.props.get_mut(*name) {
                            self.resolve_refs(value);
                        }
                    }
                }
                inst
            })
            .collect()
    }

    fn resolve_refs(&self, value: &mut serde_json::Value) {
        match value {
            serde_json::Value::String(s) => {
                if let Some(full) = self.id_of(s) {
                    *s = full.to_string();
                }
            }
            serde_json::Value::Array(items) => items.iter_mut().for_each(|v| self.resolve_refs(v)),
            _ => {}
        }
    }
}

/// The `existing` rows one prompt shows, and how to read the answer back.
#[derive(Debug, Clone)]
pub(crate) struct PromptExisting {
    /// The rows to render, keyed by the id the prompt names them by (a handle
    /// when compact, else the full id). Pass this, not the full set, to
    /// [`build_interpretation_input`].
    pub shown: ExistingInstances,
    pub handles: Handles,
    /// Distinct instances in the full existing set.
    pub rows_total: usize,
    /// Distinct instances the prompt shows.
    pub rows_shown: usize,
    /// Estimated tokens of the shown rows.
    pub tokens: usize,
}

/// Milliseconds since the epoch of a stored timestamp (epoch milliseconds or
/// RFC 3339); 0 when it is empty or neither.
fn timestamp_millis(ts: &str) -> i64 {
    ts.parse::<i64>().ok().unwrap_or_else(|| {
        chrono::DateTime::parse_from_rfc3339(ts)
            .map(|t| t.timestamp_millis())
            .unwrap_or(0)
    })
}

/// Pick the rows of `existing` that fit in `budget_tokens`, best first:
///
/// 1. rows whose id starts with `base_prefix` (the pass's own records);
/// 2. inside each group, by `scores` (higher first) when given, then by
///    `updated_at` (newest first), then by id.
///
/// An instance in several classes is one unit: all its rows or none. The fill
/// stops at the first unit that does not fit, so a lower-ranked row never
/// takes the place of a higher-ranked one. With [`Compact::Yes`] each shown
/// id becomes a handle `P1`, `P2`, ... in rank order.
pub(crate) fn select_existing(
    existing: &ExistingInstances,
    base_prefix: &str,
    budget_tokens: usize,
    scores: Option<&HashMap<String, f32>>,
    compact: Compact,
) -> PromptExisting {
    let in_scope = |id: &str| !base_prefix.is_empty() && id.starts_with(base_prefix);
    let score = |id: &str| scores.and_then(|s| s.get(id)).copied();
    let recency = |rows: &[InstanceContext]| {
        rows.iter()
            .map(|r| timestamp_millis(&r.updated_at))
            .max()
            .unwrap_or(0)
    };

    let mut units: Vec<(&String, &Vec<InstanceContext>)> = existing.iter().collect();
    units.sort_by(|(a_id, a_rows), (b_id, b_rows)| {
        in_scope(b_id)
            .cmp(&in_scope(a_id))
            .then_with(|| {
                // `None` (no score) after every score.
                let (a, b) = (score(a_id), score(b_id));
                b.partial_cmp(&a).unwrap_or(std::cmp::Ordering::Equal)
            })
            .then_with(|| recency(b_rows).cmp(&recency(a_rows)))
            .then_with(|| a_id.cmp(b_id))
    });

    let mut out = PromptExisting {
        shown: ExistingInstances::new(),
        handles: Handles::default(),
        rows_total: existing.len(),
        rows_shown: 0,
        tokens: 0,
    };
    for (id, rows) in units {
        let shown_id = match compact {
            Compact::Yes => format!("P{}", out.rows_shown + 1),
            Compact::No => id.clone(),
        };
        let shown_rows: Vec<InstanceContext> = rows
            .iter()
            .map(|r| InstanceContext {
                id: shown_id.clone(),
                ..r.clone()
            })
            .collect();
        // Each row costs its JSON plus the comma between rows.
        let cost: usize = shown_rows
            .iter()
            .map(|r| estimate_token_count(&existing_entry_json(r).to_string()) + 1)
            .sum();
        if out.tokens + cost > budget_tokens {
            break;
        }
        out.tokens += cost;
        out.rows_shown += 1;
        if compact == Compact::Yes {
            out.handles.to_id.insert(shown_id.clone(), id.clone());
        }
        out.shown.insert(shown_id, shown_rows);
    }
    out
}

/// One text per existing instance for the similarity order: its identity
/// value and its secondary values, each cut to [`EMBED_VALUE_MAX_CHARS`].
fn row_text(rows: &[InstanceContext]) -> String {
    let mut parts: Vec<String> = Vec::new();
    for r in rows {
        if !parts.contains(&r.title) {
            parts.push(r.title.clone());
        }
        let props: BTreeMap<&String, &String> = r.properties.iter().collect();
        for value in props.values() {
            parts.push(value.chars().take(EMBED_VALUE_MAX_CHARS).collect());
        }
    }
    parts.join(". ")
}

/// Each row's similarity to the batch: its highest cosine similarity to any
/// turn. Rows or turns whose vector length differs from the first turn's are
/// left out.
pub(crate) fn similarity_from_vectors(
    turn_vectors: &[Vec<f32>],
    row_vectors: &[(String, Vec<f32>)],
) -> HashMap<String, f32> {
    let Some(dim) = turn_vectors.first().map(Vec::len) else {
        return HashMap::new();
    };
    let turns: Vec<&Vec<f32>> = turn_vectors.iter().filter(|v| v.len() == dim).collect();
    row_vectors
        .iter()
        .filter(|(_, v)| v.len() == dim)
        .map(|(id, v)| {
            let best = turns
                .iter()
                .map(|t| cosine_similarity(t, v))
                .fold(f32::MIN, f32::max);
            (id.clone(), best)
        })
        .collect()
}

/// Similarity of each existing instance to the transcript through the
/// embedding model `model`. `None` (with a warning) when embedding fails, so
/// the caller falls back to the recency order and the pass goes on.
pub(crate) async fn similarity_scores(
    model: &str,
    transcript: &[TranscriptTurn],
    existing: &ExistingInstances,
) -> Option<HashMap<String, f32>> {
    let turns: Vec<String> = transcript
        .iter()
        .map(|t| {
            t.text
                .chars()
                .take(EMBED_TURN_MAX_CHARS)
                .collect::<String>()
        })
        .filter(|t| !t.trim().is_empty())
        .collect();
    if turns.is_empty() || existing.is_empty() {
        return None;
    }
    let mut ids: Vec<&String> = existing.keys().collect();
    ids.sort();
    let mut texts = turns.clone();
    texts.extend(ids.iter().map(|id| row_text(&existing[*id])));
    let vectors = match embed_via_ai_service(model, &texts).await {
        Ok(v) if v.len() == texts.len() => v,
        Ok(v) => {
            log::warn!(
                "interpretation existing: embedding '{model}' returned {} vectors for {} texts; \
                 showing the most recent rows instead",
                v.len(),
                texts.len()
            );
            return None;
        }
        Err(e) => {
            log::warn!(
                "interpretation existing: embedding '{model}' failed, showing the most recent \
                 rows instead: {e:#}"
            );
            return None;
        }
    };
    let (turn_vectors, row_vectors) = vectors.split_at(turns.len());
    let rows: Vec<(String, Vec<f32>)> = ids
        .into_iter()
        .cloned()
        .zip(row_vectors.iter().cloned())
        .collect();
    Some(similarity_from_vectors(turn_vectors, &rows))
}

/// The `existing` rows for one pass's prompt: the budget from the model's
/// window and the rest of the prompt, then [`select_existing`]. The
/// similarity order runs only when the rows do not all fit and `embed_model`
/// is set. Logs one line: rows total, rows shown, estimated tokens.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn existing_for_prompt(
    task: &AITask,
    shapes: &[ModelShape],
    transcript: &[TranscriptTurn],
    existing: &ExistingInstances,
    active_flows: &[FlowContext],
    speaker_names: &HashMap<String, String>,
    base_prefix: &str,
    embed_model: Option<&str>,
    compact: Compact,
) -> PromptExisting {
    let window = match crate::ai_service::AIService::global_instance().await {
        Ok(service) => service.context_window_tokens(&task.model_id).await,
        Err(_) => None,
    };
    let without_rows = build_interpretation_input(
        shapes,
        transcript,
        &ExistingInstances::new(),
        active_flows,
        speaker_names,
    );
    let other_tokens = estimate_task_prompt_tokens(task, &without_rows);
    let budget = existing_budget_tokens(window, other_tokens);

    let mut view = select_existing(existing, base_prefix, budget, None, compact);
    let mut order = "recency";
    if view.rows_shown < view.rows_total {
        if let Some(model) = embed_model {
            if let Some(scores) = similarity_scores(model, transcript, existing).await {
                view = select_existing(existing, base_prefix, budget, Some(&scores), compact);
                order = "similarity";
            }
        }
    }
    log::info!(
        "🧠 interpretation existing rows_total={} rows_shown={} tokens_est={} budget={} \
         window={} order={}",
        view.rows_total,
        view.rows_shown,
        view.tokens,
        budget,
        window.map_or_else(
            || format!("{ASSUMED_WINDOW_TOKENS} (assumed)"),
            |w| w.to_string()
        ),
        order
    );
    view
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::*;
    use crate::perspectives::interpretation_test_support::*;

    fn row(id: &str, class: &str, title: &str, updated_ms: i64) -> InstanceContext {
        InstanceContext {
            id: id.to_string(),
            title: title.to_string(),
            class: class.to_string(),
            properties: BTreeMap::new(),
            updated_at: updated_ms.to_string(),
        }
    }

    fn existing_of(rows: Vec<InstanceContext>) -> ExistingInstances {
        let mut out = ExistingInstances::new();
        for r in rows {
            out.entry(r.id.clone()).or_default().push(r);
        }
        out
    }

    /// Full ids of the shown rows, in handle order (`P1` first).
    fn shown_ids(view: &PromptExisting) -> Vec<String> {
        (1..=view.rows_shown)
            .map(|n| view.handles.id_of(&format!("P{n}")).unwrap().to_string())
            .collect()
    }

    /// The token cost of one row as the prompt shows it, plus its comma.
    fn row_cost(view: &PromptExisting, shown_id: &str) -> usize {
        view.shown[shown_id]
            .iter()
            .map(|r| estimate_token_count(&existing_entry_json(r).to_string()) + 1)
            .sum()
    }

    #[test]
    fn the_budget_is_a_quarter_of_the_window_left_after_the_prompt_and_the_answer() {
        assert_eq!(
            existing_budget_tokens(Some(65_536), 10_000),
            (65_536 - 10_000 - 8_192) / 4
        );
        assert_eq!(
            existing_budget_tokens(None, 10_000),
            existing_budget_tokens(Some(65_536), 10_000),
            "an unknown window is taken as 65,536"
        );
        assert_eq!(existing_budget_tokens(Some(8_192), 4_000), 0);
    }

    /// Rows shaped like the staging space of 2026-11-18 (Marvin's
    /// measurements): ids of about 108 characters under one long call
    /// prefix, titles of about 130 characters, and a summary of about 375
    /// characters on each DiscussionTopic.
    fn staging_like_rows() -> ExistingInstances {
        let classes = [
            ("Position", 224, false),
            ("DiscussionTopic", 105, true),
            ("ActionItem", 106, false),
            ("Relationship", 43, false),
            ("Question", 26, false),
        ];
        let mut rows = Vec::new();
        let mut n = 0i64;
        for (class, count, summary) in classes {
            for i in 0..count {
                n += 1;
                let call = n % 7;
                let id = format!(
                    "we://interpreted/ad4m%3A%2F%2Fobj%2Fcall-{call}-3f9c2a7e1d4b%2Fsession/{}/{:08x}{i:04}",
                    class.to_lowercase(),
                    n * 2_654_435_761
                );
                let title = format!(
                    "{class} {i}: the group weighs whether the rollout of feature {n} should wait for the next review of the shared budget plan"
                );
                let mut r = row(&id, class, &title, 1_000 + n);
                if summary {
                    r.properties.insert(
                        "summary".into(),
                        format!(
                            "Topic {i}. {}",
                            "Participants compared the options, raised two concerns about the timeline, and agreed to revisit it once the numbers are in. "
                                .repeat(3)
                        ),
                    );
                }
                rows.push(r);
            }
        }
        existing_of(rows)
    }

    #[test]
    fn the_shown_rows_stay_within_the_budget_with_many_rows() {
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        let mut rows = Vec::new();
        for i in 0..504 {
            rows.push(row(
                &format!("soa://space/task/{i:04}"),
                "Task",
                &format!("Task number {i} with a title of a usual length for a call"),
                i,
            ));
        }
        let existing = existing_of(rows);
        let budget = 2_000;
        let view = select_existing(&existing, "", budget, None, Compact::Yes);
        assert_eq!(view.rows_total, 504);
        assert!(
            view.rows_shown > 0 && view.rows_shown < 504,
            "{}",
            view.rows_shown
        );
        assert!(view.tokens <= budget, "{} > {budget}", view.tokens);

        // What the prompt grows by is what the view counted, within the
        // brackets of the one `existing` array.
        let transcript = [TranscriptTurn::from_speaker_text("Ana", "hello")];
        let with =
            build_interpretation_input(&shapes, &transcript, &view.shown, &[], &HashMap::new());
        let without = build_interpretation_input(
            &shapes,
            &transcript,
            &ExistingInstances::new(),
            &[],
            &HashMap::new(),
        );
        let grown = estimate_token_count(&with) - estimate_token_count(&without);
        assert!(
            grown <= budget + 2,
            "the prompt grew by {grown}, budget {budget}"
        );
        assert!(
            !with.contains("soa://space/task/"),
            "full ids leak: compact prompt"
        );
    }

    #[test]
    fn rows_in_the_pass_scope_come_before_newer_rows_outside_it() {
        let existing = existing_of(vec![
            row("we://call/2/t/new", "Task", "Newest, other call", 900),
            row("we://call/1/t/old", "Task", "Old, this call", 100),
            row("we://call/2/t/newer", "Task", "Newer, other call", 800),
            row("we://call/1/t/mid", "Task", "Mid, this call", 200),
        ]);
        let all = select_existing(&existing, "we://call/1/", usize::MAX, None, Compact::Yes);
        assert_eq!(
            shown_ids(&all),
            vec![
                "we://call/1/t/mid",
                "we://call/1/t/old",
                "we://call/2/t/new",
                "we://call/2/t/newer",
            ]
        );

        // A budget for two rows shows the two in scope, though older.
        let budget = row_cost(&all, "P1") + row_cost(&all, "P2");
        let two = select_existing(&existing, "we://call/1/", budget, None, Compact::Yes);
        assert_eq!(
            shown_ids(&two),
            vec!["we://call/1/t/mid", "we://call/1/t/old"]
        );
    }

    #[test]
    fn without_scores_the_most_recent_rows_come_first() {
        let existing = existing_of(vec![
            row("soa://a", "Task", "A", 300),
            row("soa://b", "Task", "B", 100),
            row("soa://c", "Task", "C", 200),
        ]);
        let view = select_existing(&existing, "", usize::MAX, None, Compact::Yes);
        assert_eq!(shown_ids(&view), vec!["soa://a", "soa://c", "soa://b"]);

        // RFC 3339 timestamps order the same way.
        let mut rfc = existing.clone();
        rfc.get_mut("soa://a").unwrap()[0].updated_at = "2026-11-18T10:00:00.000Z".into();
        rfc.get_mut("soa://b").unwrap()[0].updated_at = "2026-11-18T12:00:00.000Z".into();
        rfc.get_mut("soa://c").unwrap()[0].updated_at = "2026-11-18T11:00:00.000Z".into();
        let view = select_existing(&rfc, "", usize::MAX, None, Compact::Yes);
        assert_eq!(shown_ids(&view), vec!["soa://b", "soa://c", "soa://a"]);
    }

    #[test]
    fn with_vectors_the_most_similar_rows_come_first_after_the_scope() {
        let existing = existing_of(vec![
            row("we://call/1/t/own", "Task", "Own call", 1),
            row("soa://far", "Task", "Unrelated", 900),
            row("soa://near", "Task", "About the budget", 2),
            row("soa://mid", "Task", "Partly related", 3),
        ]);
        // Two turns; the rows' vectors point near one of them or away.
        let turns = vec![vec![1.0, 0.0, 0.0], vec![0.0, 1.0, 0.0]];
        let rows = vec![
            ("we://call/1/t/own".to_string(), vec![0.0, 0.0, 1.0]),
            ("soa://far".to_string(), vec![0.0, 0.0, 1.0]),
            ("soa://near".to_string(), vec![0.0, 1.0, 0.1]),
            ("soa://mid".to_string(), vec![0.6, 0.0, 0.8]),
        ];
        let scores = similarity_from_vectors(&turns, &rows);
        assert!(scores["soa://near"] > scores["soa://mid"]);
        assert!(scores["soa://mid"] > scores["soa://far"]);

        let view = select_existing(
            &existing,
            "we://call/1/",
            usize::MAX,
            Some(&scores),
            Compact::Yes,
        );
        assert_eq!(
            shown_ids(&view),
            vec!["we://call/1/t/own", "soa://near", "soa://mid", "soa://far"],
            "the newest row (far) is last: similarity beats recency"
        );
    }

    #[tokio::test]
    async fn a_failed_embedding_gives_no_scores_so_the_order_falls_back_to_recency() {
        ensure_db_init();
        crate::ai_service::AIService::init_global_instance()
            .await
            .unwrap();
        let existing = existing_of(vec![row("soa://a", "Task", "A", 1)]);
        let transcript = [TranscriptTurn::from_speaker_text("Ana", "the budget")];
        assert!(
            similarity_scores("no-such-embedding-model", &transcript, &existing)
                .await
                .is_none()
        );
    }

    #[test]
    fn a_handle_in_the_answer_resolves_to_its_full_id() {
        let shapes = vec![shape_from_sdna("Task", TASK_WITH_RELATION_SDNA)];
        let existing = existing_of(vec![
            row("soa://task/first", "Task", "First", 300),
            row("soa://task/second", "Task", "Second", 200),
            row("soa://task/third", "Task", "Third", 100),
        ]);
        let view = select_existing(&existing, "", usize::MAX, None, Compact::Yes);
        let answer = parse_interpretation_response(
            r#"[
              {"class":"Task","id":"P3","title":"Third, renamed","blocks":["P1","new:Task:1"]},
              {"class":"Task","title":"A new task","blocks":"P2"}
            ]"#,
        )
        .unwrap();
        let resolved = view.handles.resolve(answer, &shapes);
        assert_eq!(resolved[0].id.as_deref(), Some("soa://task/third"));
        assert_eq!(
            resolved[0].props["blocks"],
            serde_json::json!(["soa://task/first", "new:Task:1"])
        );
        assert_eq!(resolved[1].id, None);
        assert_eq!(
            resolved[1].props["blocks"],
            serde_json::json!("soa://task/second")
        );
        // A scalar field that happens to read like a handle is left alone.
        let titled = parse_interpretation_response(r#"[{"class":"Task","title":"P2"}]"#).unwrap();
        assert_eq!(
            view.handles.resolve(titled, &shapes)[0].props["title"],
            serde_json::json!("P2")
        );
    }

    #[test]
    fn an_unknown_handle_goes_the_way_of_an_unknown_id() {
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        let existing = existing_of(vec![row("soa://task/only", "Task", "Only", 1)]);
        let view = select_existing(&existing, "", usize::MAX, None, Compact::Yes);
        let answer =
            parse_interpretation_response(r#"[{"class":"Task","id":"P9","title":"Fresh"}]"#)
                .unwrap();
        let resolved = view.handles.resolve(answer.clone(), &shapes);
        assert_eq!(resolved, answer, "an unknown handle stays as written");

        let ops =
            plan_interpretation_ops_with_context(&shapes, &resolved, "soa://minted/", &existing);
        match &ops[..] {
            [InterpretationOp::Create { base, .. }] => {
                assert!(base.starts_with("soa://minted/"), "{base}");
            }
            other => panic!("expected one Create, as for an unknown id: {other:?}"),
        }
    }

    /// Lal's P1 on #1806: the prompt used to cut long values, and an edit
    /// of the visible part deleted the hidden rest. Now a compact prompt
    /// shows the value whole, and the model's edit reaches the planner as
    /// written.
    #[test]
    fn a_compact_prompt_shows_long_values_whole_and_an_edit_passes_unchanged() {
        let shapes = vec![shape_from_sdna(
            "ConversationSubgroup",
            CONVERSATION_SUBGROUP_SDNA,
        )];
        let full = format!("{}END", "x".repeat(400));
        let mut r = row("soa://sg/1", "ConversationSubgroup", "Payments", 1);
        r.properties.insert("summary".into(), full.clone());
        let existing = existing_of(vec![r]);
        let view = select_existing(&existing, "", usize::MAX, None, Compact::Yes);
        assert_eq!(view.shown["P1"][0].properties["summary"], full);

        // The model rewords the start and keeps the end.
        let edited = format!("{}END", "y".repeat(400));
        let answer = format!(
            r#"[{{"class":"ConversationSubgroup","id":"P1","name":"Payments","summary":"{edited}"}}]"#
        );
        let resolved = view
            .handles
            .resolve(parse_interpretation_response(&answer).unwrap(), &shapes);
        assert_eq!(resolved[0].id.as_deref(), Some("soa://sg/1"));
        assert_eq!(resolved[0].props["summary"], serde_json::json!(edited));
    }

    #[test]
    fn a_prompt_that_is_not_compact_keeps_full_ids_and_values() {
        let long = "y".repeat(500);
        let mut r = row("soa://task/a", "Task", "A", 1);
        r.properties.insert("owner".into(), long.clone());
        let existing = existing_of(vec![r]);
        let view = select_existing(&existing, "", usize::MAX, None, Compact::No);
        assert_eq!(view.shown["soa://task/a"][0].properties["owner"], long);
        assert_eq!(view.handles, Handles::default());
    }

    /// Before/after numbers for #1804: the `existing` block of a staging-like
    /// space with 504 rows. Prints them; run with `--nocapture` to see.
    #[test]
    fn staging_504_rows_before_and_after() {
        let existing = staging_like_rows();
        let before = select_existing(&existing, "", usize::MAX, None, Compact::No);
        let compact = select_existing(&existing, "", usize::MAX, None, Compact::Yes);
        // 65,536 window; about 12k tokens of system prompt, examples, schema
        // and a 100-turn transcript (6.6k).
        let budget = existing_budget_tokens(Some(65_536), 12_000);
        let after = select_existing(&existing, "", budget, None, Compact::Yes);
        println!(
            "#1804 existing block, 504 rows: before {} tokens (est.), short handles {} tokens, \
             budget {budget} -> {} rows, {} tokens",
            before.tokens, compact.tokens, after.rows_shown, after.tokens
        );
        assert_eq!(before.rows_shown, 504);
        assert!(
            compact.tokens * 5 < before.tokens * 4,
            "short handles shrink the rows by over 20%"
        );
        assert!(after.tokens <= budget);
        assert!(after.rows_shown < 504);
    }

    /// The whole single-shot pass on a scripted model: the prompt shows a
    /// budgeted, compact set; the answer's handle updates the right record;
    /// a proposal that repeats a record the prompt did NOT show is still
    /// deduped, since dedup reads the full existing set.
    #[tokio::test]
    async fn a_pass_dedups_against_a_row_its_prompt_did_not_show() {
        use crate::types::{AiCapability, EndpointKind, ModelInput, ModelType};

        let (mut perspective, shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
        let task_shape = &shapes[0];
        let base_prefix = "test://call/1/";
        // Oldest row, outside the call: ranked last.
        seed_instance(
            &mut perspective,
            &ctx,
            task_shape,
            "test://other/zzz",
            "Budget review",
        )
        .await;
        tokio::time::sleep(std::time::Duration::from_millis(20)).await;
        for i in 0..40 {
            let base = format!("test://other/{i:03}");
            seed_instance(
                &mut perspective,
                &ctx,
                task_shape,
                &base,
                &format!("Old topic {i}"),
            )
            .await;
        }
        let own = "test://call/1/task/a";
        seed_instance(&mut perspective, &ctx, task_shape, own, "Draft the agenda").await;
        seed_llm_overlay(
            &mut perspective,
            &ctx,
            task_shape,
            own,
            serde_json::json!({ "title": "Draft the agenda" }),
        )
        .await;

        // A window that leaves the `existing` block about 300 tokens.
        let transcript = vec![TranscriptTurn::from_speaker_text(
            "Ana",
            "Let us rename the agenda task and look at the budget review again.",
        )];
        let task = ensure_interpretation_task_for_model(None).unwrap();
        let without_rows = build_interpretation_input(
            &shapes,
            &transcript,
            &ExistingInstances::new(),
            &[],
            &build_speaker_name_map(&transcript),
        );
        let other = estimate_task_prompt_tokens(&task, &without_rows);
        let window = other + OUTPUT_RESERVE_TOKENS + 4 * 300;

        let answer = r#"{"instances":[
            {"class":"Task","id":"P1","title":"Draft the agenda v2"},
            {"class":"Task","title":"Budget review"},
            {"class":"Task","id":"P999","title":"Fresh task"}
        ]}"#;
        let mut server = mockito::Server::new_async().await;
        let _llm = server
            .mock("POST", "/v1/chat/completions")
            .with_header("content-type", "application/json")
            .with_body(
                serde_json::json!({
                    "id": "chatcmpl-test", "object": "chat.completion", "created": 0,
                    "model": "stub",
                    "choices": [{"index": 0, "finish_reason": "stop",
                                 "message": {"role": "assistant", "content": answer}}],
                    "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2}
                })
                .to_string(),
            )
            .expect_at_least(1)
            .create_async()
            .await;
        crate::ai_service::AIService::init_global_instance()
            .await
            .unwrap();
        let service = crate::ai_service::AIService::global_instance()
            .await
            .unwrap();
        // Straight into the database, under a fresh id and key: the global
        // database outlives each test, and a mock server's URL can repeat an
        // earlier test's endpoint.
        let endpoint = crate::types::Endpoint {
            id: uuid::Uuid::new_v4().to_string(),
            name: format!("budget-llm-{}", uuid::Uuid::new_v4()),
            kind: EndpointKind::Vllm,
            base_url: url::Url::parse(&server.url()).unwrap(),
            // A key of its own: the database refuses a second endpoint on
            // the same URL with the same key.
            api_key: crate::secret::Secret::new(uuid::Uuid::new_v4().to_string()),
            capabilities: vec![AiCapability::Chat],
            max_num_ctx: Some(window as u32),
            request_timeout_secs: None,
            max_concurrent: None,
            managed: false,
            health: Default::default(),
        };
        crate::db::Ad4mDb::with_global_instance(|db| db.insert_endpoint(&endpoint)).unwrap();
        let model_id = crate::db::Ad4mDb::with_global_instance(|db| {
            db.add_model(&ModelInput {
                name: format!("budget-llm-{}", uuid::Uuid::new_v4()),
                model_type: ModelType::Llm,
                endpoint_id: endpoint.id.clone(),
                remote_model: "stub".into(),
                native_tools: None,
            })
        })
        .unwrap();
        service.reload_model(model_id.clone()).await.unwrap();
        service
            .set_default_model(ModelType::Llm, model_id)
            .await
            .unwrap();

        let outcome = run_interpretation_with_strategy_and_model(
            &mut perspective,
            &shapes,
            &transcript,
            base_prefix,
            &ctx,
            &DedupStrategy::NormalizedString,
            None,
            None,
            None,
            true,
            None,
            None,
        )
        .await
        .expect("pass");

        let prompt = outcome.debug.expect("debug capture").prompt;
        assert!(
            prompt.contains(r#""id":"P1""#),
            "compact handles in the prompt"
        );
        assert!(!prompt.contains(own), "no full ids in the prompt");
        assert!(
            !prompt.contains("Budget review"),
            "precondition: the prompt leaves the oldest row out"
        );

        let after = existing_instance_context(&perspective, &shapes, None)
            .await
            .unwrap();
        let titles = |t: &str| -> Vec<String> {
            let mut ids: Vec<String> = after
                .values()
                .flatten()
                .filter(|r| r.title == t)
                .map(|r| r.id.clone())
                .collect();
            ids.sort();
            ids
        };
        assert_eq!(
            after[own][0].title, "Draft the agenda v2",
            "P1 updated its record"
        );
        assert_eq!(
            titles("Budget review"),
            vec!["test://other/zzz"],
            "no duplicate of the row the prompt did not show"
        );
        let fresh = titles("Fresh task");
        assert_eq!(fresh.len(), 1, "{fresh:?}");
        assert!(
            fresh[0].starts_with(base_prefix),
            "P999 minted like an unknown id"
        );
        assert!(!after.contains_key("P999"));
    }
}

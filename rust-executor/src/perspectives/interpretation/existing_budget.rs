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
//! - **Order** ([`fill_stages`]). (a) Rows under the pass's `base_prefix`
//!   (the records this processor's scope minted). (b) The most recently
//!   changed rows, up to half of the budget (a) left. (c) The rest: the rows
//!   most similar to the transcript when the pass has a semantic dedup
//!   embedding model. Without similarity scores (no model, or embedding
//!   failed), (b) takes all of the budget (a) left and there is no (c).
//! - **Short ids** (the single-shot path only, see [`Compact`]): each id is
//!   shown as a short handle (`P1`, `P2`, ...). [`Handles`] maps the model's
//!   output back to the full ids. Values are always shown whole: a row that
//!   does not fit is left out, never shortened.
//!
//! The selection only changes what the prompt shows. Dedup
//! (`resolve_already_present*`, `dedup_ops_against_existing`) and the planner
//! still get the full existing set, so a row the prompt did not show still
//! stops a duplicate.

use super::class_label;
use super::dedup::{cosine_similarity, embed_via_ai_service};
use super::prompt::{
    build_interpretation_input_showing, existing_entry_json, outline_depths, outline_line,
    ExistingShown,
};
use super::types::{ExistingInstances, InstanceContext, ProposedInstance, TranscriptTurn};
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
    /// [`build_interpretation_input_showing`].
    pub shown: ExistingInstances,
    pub handles: Handles,
    /// Distinct instances in the full existing set.
    pub rows_total: usize,
    /// Distinct instances the prompt shows.
    pub rows_shown: usize,
    /// Estimated tokens of the shown rows.
    pub tokens: usize,
    /// Full ids of the shown instances, in fill order.
    pub order: Vec<String>,
    /// Each fill stage's name and how many instances it added.
    pub stages: Vec<(&'static str, usize)>,
}

impl PromptExisting {
    /// The counts for the prompt's `existing_shown` line.
    pub(crate) fn counts(&self, unshown_checked: bool) -> ExistingShown {
        ExistingShown {
            shown: self.rows_shown,
            total: self.rows_total,
            unshown_checked,
        }
    }
}

/// Milliseconds since the epoch of a stored timestamp (epoch milliseconds or
/// RFC 3339); 0 when it is empty or neither.
pub(super) fn timestamp_millis(ts: &str) -> i64 {
    ts.parse::<i64>().ok().unwrap_or_else(|| {
        chrono::DateTime::parse_from_rfc3339(ts)
            .map(|t| t.timestamp_millis())
            .unwrap_or(0)
    })
}

/// The newest timestamp of an instance's rows, in epoch milliseconds.
fn recency(rows: &[InstanceContext]) -> i64 {
    rows.iter()
        .map(|r| timestamp_millis(&r.updated_at))
        .max()
        .unwrap_or(0)
}

/// One step of the fill: the rows it offers, best first, and the share of the
/// tokens still free when it starts that it may use. Rows an earlier stage
/// took are skipped.
struct FillStage<'a> {
    name: &'static str,
    rows: Vec<&'a String>,
    share: f64,
}

/// Every instance id, newest first (then by id).
fn by_recency(existing: &ExistingInstances) -> Vec<&String> {
    let mut ids: Vec<&String> = existing.keys().collect();
    ids.sort_by(|a, b| {
        recency(&existing[*b])
            .cmp(&recency(&existing[*a]))
            .then_with(|| a.cmp(b))
    });
    ids
}

/// (a) The rows under `base_prefix` (the pass's own records), newest first.
/// They may use the whole budget.
fn scope_stage<'a>(newest_first: &[&'a String], base_prefix: &str) -> FillStage<'a> {
    FillStage {
        name: "scope",
        rows: newest_first
            .iter()
            .copied()
            .filter(|id| !base_prefix.is_empty() && id.starts_with(base_prefix))
            .collect(),
        share: 1.0,
    }
}

/// (b) The most recently changed rows, with `share` of what (a) left.
fn recent_stage<'a>(newest_first: &[&'a String], share: f64) -> FillStage<'a> {
    FillStage {
        name: "recent",
        rows: newest_first.to_vec(),
        share,
    }
}

/// (c) The rest of the budget: the rows most similar to the batch (rows
/// without a score last, newest first).
fn similar_stage<'a>(newest_first: &[&'a String], scores: &HashMap<String, f32>) -> FillStage<'a> {
    let mut rows = newest_first.to_vec();
    // Stable: equal scores keep the recency order. `None` sorts after every
    // score.
    rows.sort_by(|a, b| {
        let (a, b) = (scores.get(*a), scores.get(*b));
        b.partial_cmp(&a).unwrap_or(std::cmp::Ordering::Equal)
    });
    FillStage {
        name: "similar",
        rows,
        share: 1.0,
    }
}

/// The fill stages in order (Nico, 2026-11-18): (a) the pass's own records,
/// (b) the most recent rows up to half of the rest, (c) the most similar
/// rows. Without `scores`, (b) gets all of the rest: two recency stages of
/// half each can leave out a row that one stage would take. A new stage, for
/// example the structural neighbours of (a), goes into this list.
fn fill_stages<'a>(
    existing: &'a ExistingInstances,
    base_prefix: &str,
    scores: Option<&HashMap<String, f32>>,
) -> Vec<FillStage<'a>> {
    let newest_first = by_recency(existing);
    let scope = scope_stage(&newest_first, base_prefix);
    match scores {
        Some(scores) => vec![
            scope,
            recent_stage(&newest_first, 0.5),
            similar_stage(&newest_first, scores),
        ],
        None => vec![scope, recent_stage(&newest_first, 1.0)],
    }
}

/// Tokens of one outline line at `depth` inside the prompt's JSON string,
/// plus one token for the `"outline"` key (paid once for the block, so
/// every line carries it). The fill still prices a candidate at depth 0;
/// [`reconcile_shown_outline`] re-prices the lines that were actually nested.
fn outline_line_tokens(depth: usize, class: &str, title: &str, id: &str) -> usize {
    let line = outline_line(depth, class, title, id);
    let escaped = serde_json::to_string(&format!("{line}\n")).expect("outline line");
    let inner = &escaped[1..escaped.len() - 1];
    estimate_token_count(inner) + 1
}

/// Pick the rows of `existing` that fit in `budget_tokens`, stage by stage
/// (see [`fill_stages`]). A stage takes its rows in order and leaves out a
/// row that does not fit in its share, then goes on with the next row, so
/// one large row does not stop the fill. Rows that fit nowhere are not
/// shown.
///
/// An instance in several classes is one unit: all its rows or none. With
/// [`Compact::Yes`] each shown id becomes a handle `P1`, `P2`, ... in fill
/// order, except:
///
/// - an id in `full_ids` stays whole. These are the subjects of the active
///   flows, which the prompt names by full id under `active_flows`, so the
///   model can tie a flow to its row.
/// - a handle that is also a real id in `existing`, or that equals any
///   parent target (including a target with no row), is not used. A dangling
///   target that spelled a handle would otherwise nest under the unrelated
///   row that took that handle, and a handle in the answer must mean one row.
///
/// `shapes` is how the outline nests (the same walk as the prompt). A
/// candidate is priced with the shown id of a parent already selected, so a
/// long parent id does not reject a child that fits once that parent is a
/// handle. A parent selected later is still a full id at that point. After
/// the fill, every parent target that names a selected row is rewritten to
/// that row's shown id — the handle map is complete then, including when the
/// child was selected first. A parent the budget left out keeps its full
/// id. The outline is then charged at the depth it will render, and the
/// lowest-priority rows are dropped until the block fits. Flat rows stay
/// at the depth-0 cost the fill used.
pub(crate) fn select_existing(
    existing: &ExistingInstances,
    base_prefix: &str,
    budget_tokens: usize,
    scores: Option<&HashMap<String, f32>>,
    compact: Compact,
    full_ids: &HashSet<String>,
    shapes: &[ModelShape],
) -> PromptExisting {
    let mut out = PromptExisting {
        shown: ExistingInstances::new(),
        handles: Handles::default(),
        rows_total: existing.len(),
        rows_shown: 0,
        tokens: 0,
        order: Vec::new(),
        stages: Vec::new(),
    };
    let mut taken: HashSet<&String> = HashSet::new();
    let mut next_handle = 1usize;
    // Parent targets are reserved even when they are not rows. `P1` as a
    // dangling target must not become some other row's handle.
    let reserved_targets: HashSet<&str> = existing
        .values()
        .flat_map(|rows| {
            rows.iter()
                .flat_map(|r| r.parents.values().map(String::as_str))
        })
        .collect();
    for stage in fill_stages(existing, base_prefix, scores) {
        let free = budget_tokens - out.tokens;
        let cap = (free as f64 * stage.share) as usize;
        let mut used = 0;
        let mut rows_taken = 0;
        for id in stage.rows {
            if used >= cap {
                break;
            }
            if taken.contains(id) {
                continue;
            }
            let handle = compact == Compact::Yes && !full_ids.contains(id);
            let shown_id = if handle {
                loop {
                    let candidate = format!("P{next_handle}");
                    if !existing.contains_key(&candidate)
                        && !reserved_targets.contains(candidate.as_str())
                    {
                        break candidate;
                    }
                    next_handle += 1;
                }
            } else {
                id.clone()
            };
            // Price a parent that is already in the view by its shown id.
            // Reconciliation still rewrites parents selected later, and
            // charges the indent; it cannot add a row the fill rejected.
            let shown_rows: Vec<InstanceContext> = existing[id]
                .iter()
                .map(|r| {
                    let mut row = InstanceContext {
                        id: shown_id.clone(),
                        ..r.clone()
                    };
                    for target in row.parents.values_mut() {
                        if let Some(shown) = shown_for_selected(&out, target) {
                            *target = shown;
                        }
                    }
                    row
                })
                .collect();
            // Each row costs its JSON plus the comma between rows, and the
            // outline line the same row adds. The outline is one line per
            // instance (the first class's title), not per class row. The
            // extra token on that line pays for the `"outline"` key.
            let mut cost: usize = shown_rows
                .iter()
                .map(|r| estimate_token_count(&existing_entry_json(r).to_string()) + 1)
                .sum();
            if let Some(first) = shown_rows.first() {
                cost += outline_line_tokens(0, &first.class, &first.title, &shown_id);
            }
            if used + cost > cap {
                continue;
            }
            used += cost;
            rows_taken += 1;
            taken.insert(id);
            out.tokens += cost;
            out.rows_shown += 1;
            out.order.push(id.clone());
            if handle {
                next_handle += 1;
                out.handles.to_id.insert(shown_id.clone(), id.clone());
            }
            out.shown.insert(shown_id, shown_rows);
        }
        out.stages.push((stage.name, rows_taken));
    }
    // The fill priced parents that were already selected. Rewrite every
    // parent from the original rows now that the handle map is complete
    // (a child may have been selected before its parent), charge each
    // outline line at the depth it will render, and drop the
    // lowest-priority rows until that fits.
    reconcile_shown_outline(&mut out, existing, shapes, budget_tokens);
    out
}

/// Shown id of a row already selected into `out`. A compact row is stored
/// under its handle; a row kept whole (not compact, or a flow subject) is
/// stored under its own id.
fn shown_for_selected(out: &PromptExisting, full_id: &str) -> Option<String> {
    if out.shown.contains_key(full_id) {
        return Some(full_id.to_string());
    }
    out.handles
        .to_id
        .iter()
        .find(|(_, full)| full.as_str() == full_id)
        .map(|(shown, _)| shown.clone())
}

/// Full id → shown id for the rows in `out`. A compact handle maps back to
/// its `P*` id. A row shown under its own id (not compact, or a flow subject
/// kept whole) maps to itself. A shown handle is not a full id: handles that
/// collide with a real id are never assigned.
fn full_to_shown(out: &PromptExisting) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for (shown, full) in &out.handles.to_id {
        map.insert(full.clone(), shown.clone());
    }
    for shown in out.shown.keys() {
        map.entry(shown.clone()).or_insert_with(|| shown.clone());
    }
    map
}

/// Rewrite `parents` from the original rows. A target that is a selected row
/// becomes that row's shown id. Any other target, including a parent the
/// budget did not select, stays the full id.
fn apply_shown_parents(out: &mut PromptExisting, existing: &ExistingInstances) {
    let map = full_to_shown(out);
    let order = out.order.clone();
    for full_id in order {
        let Some(orig) = existing.get(&full_id) else {
            continue;
        };
        let shown_id = map
            .get(&full_id)
            .cloned()
            .unwrap_or_else(|| full_id.clone());
        let Some(shown_rows) = out.shown.get_mut(&shown_id) else {
            continue;
        };
        for (shown, src) in shown_rows.iter_mut().zip(orig.iter()) {
            shown.parents = src
                .parents
                .iter()
                .map(|(rel, target)| {
                    (
                        rel.clone(),
                        map.get(target).cloned().unwrap_or_else(|| target.clone()),
                    )
                })
                .collect();
        }
    }
}

/// JSON plus outline, at the depth the prompt will indent each line.
fn shown_tokens(shown: &ExistingInstances, shapes: &[ModelShape]) -> usize {
    let depths = outline_depths(shown, shapes);
    let mut tokens = 0usize;
    for (id, rows) in shown {
        tokens += rows
            .iter()
            .map(|r| estimate_token_count(&existing_entry_json(r).to_string()) + 1)
            .sum::<usize>();
        if let Some(first) = rows.first() {
            let depth = depths.get(id).copied().unwrap_or(0);
            tokens += outline_line_tokens(depth, &first.class, &first.title, id);
        }
    }
    tokens
}

/// Drop the lowest-priority shown row (the last one filled). Returns false
/// when nothing is left to drop.
fn drop_last_shown(out: &mut PromptExisting) -> bool {
    let Some(full_id) = out.order.pop() else {
        return false;
    };
    let shown_id = out
        .handles
        .to_id
        .iter()
        .find(|(_, full)| **full == full_id)
        .map(|(shown, _)| shown.clone())
        .unwrap_or(full_id);
    out.handles.to_id.remove(&shown_id);
    out.shown.remove(&shown_id);
    if let Some((_, n)) = out.stages.iter_mut().rev().find(|(_, n)| *n > 0) {
        *n -= 1;
    }
    true
}

/// Remap parents, then charge real outline depth. While the block is over
/// budget, drop the lowest-priority row and try again. Parents are always
/// taken from `existing`, so a child whose parent was just dropped goes back
/// to naming that parent by its full id.
fn reconcile_shown_outline(
    out: &mut PromptExisting,
    existing: &ExistingInstances,
    shapes: &[ModelShape],
    budget_tokens: usize,
) {
    loop {
        apply_shown_parents(out, existing);
        out.tokens = shown_tokens(&out.shown, shapes);
        out.rows_shown = out.order.len();
        if out.tokens <= budget_tokens || out.order.is_empty() {
            break;
        }
        if !drop_last_shown(out) {
            break;
        }
    }
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
/// window and the rest of the prompt, then [`select_existing`].
/// `unshown_checked` is [`ExistingShown::unshown_checked`] of the pass: it
/// picks the wording of the `existing_shown` line the budget counts. The
/// similarity order runs only when `embed_model` is set and
/// [`needs_similarity`]. Logs rows total, rows shown, estimated tokens and
/// the budget at info, and the rows per fill stage at debug.
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
    unshown_checked: bool,
) -> PromptExisting {
    let window = match crate::ai_service::AIService::global_instance().await {
        Ok(service) => service.context_window_tokens(&task.model_id).await,
        Err(_) => None,
    };
    // The `existing_shown` line counted at its longest.
    let without_rows = build_interpretation_input_showing(
        shapes,
        transcript,
        &ExistingInstances::new(),
        active_flows,
        speaker_names,
        Some(ExistingShown {
            shown: existing.len().saturating_sub(1),
            total: existing.len(),
            unshown_checked,
        }),
    );
    let other_tokens = estimate_task_prompt_tokens(task, &without_rows);
    let budget = existing_budget_tokens(window, other_tokens);

    let flow_subjects: HashSet<String> = active_flows.iter().map(|f| f.subject.clone()).collect();
    let mut view = select_existing(
        existing,
        base_prefix,
        budget,
        None,
        compact,
        &flow_subjects,
        shapes,
    );
    if let Some(model) = embed_model.filter(|_| needs_similarity(&view)) {
        if let Some(scores) = similarity_scores(model, transcript, existing).await {
            view = select_existing(
                existing,
                base_prefix,
                budget,
                Some(&scores),
                compact,
                &flow_subjects,
                shapes,
            );
        }
    }
    log::info!(
        "🧠 interpretation existing rows_total={} rows_shown={} tokens_est={} budget={} \
         window={}",
        view.rows_total,
        view.rows_shown,
        view.tokens,
        budget,
        window.map_or_else(
            || format!("{ASSUMED_WINDOW_TOKENS} (assumed)"),
            |w| w.to_string()
        ),
    );
    log::debug!(
        "interpretation existing stages={}",
        view.stages
            .iter()
            .map(|(name, n)| format!("{name}:{n}"))
            .collect::<Vec<_>>()
            .join(",")
    );
    view
}

/// Whether the similarity order can change what `view` (the fill without
/// scores) shows: some rows are left out, and a row outside the scope fits in
/// what the scope stage left. The recency stage of `view` offers every such
/// row all of that room, so it took a row exactly when one fits. Embedding
/// every row costs one call per 64 texts, so it does not run when nothing
/// more could be shown, for example when the scope rows use the whole budget.
fn needs_similarity(view: &PromptExisting) -> bool {
    view.rows_shown < view.rows_total
        && view
            .stages
            .iter()
            .any(|(name, taken)| *name == "recent" && *taken > 0)
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
            parents: BTreeMap::new(),
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

    /// Full ids of the shown rows, in fill order.
    fn shown_ids(view: &PromptExisting) -> Vec<String> {
        assert_eq!(view.order.len(), view.rows_shown);
        view.order.clone()
    }

    /// 20 rows of the same token cost: `soa://t/00` (oldest) to `soa://t/19`
    /// (newest). Scores rise with age: `soa://t/00` is the most similar.
    fn twenty_rows() -> (ExistingInstances, HashMap<String, f32>, usize) {
        let rows: Vec<InstanceContext> = (0..20)
            .map(|i| {
                row(
                    &format!("soa://t/{i:02}"),
                    "Task",
                    &format!("Task {i:02}"),
                    i,
                )
            })
            .collect();
        let scores = rows
            .iter()
            .enumerate()
            .map(|(i, r)| (r.id.clone(), 1.0 - i as f32 / 100.0))
            .collect();
        let existing = existing_of(rows);
        let one = existing_of(vec![row("soa://t/00", "Task", "Task 00", 0)]);
        let cost = select_existing(
            &one,
            "",
            usize::MAX,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        )
        .tokens;
        (existing, scores, cost)
    }

    fn ids(range: impl Iterator<Item = i64>) -> Vec<String> {
        range.map(|i| format!("soa://t/{i:02}")).collect()
    }

    #[test]
    fn the_recent_rows_use_at_most_half_of_the_budget_left_after_the_scope() {
        let (existing, scores, cost) = twenty_rows();
        // Room for 10.5 rows: the recent stage gets 5.25 rows' worth.
        let budget = 10 * cost + cost / 2;
        let view = select_existing(
            &existing,
            "",
            budget,
            Some(&scores),
            Compact::No,
            &HashSet::new(),
            &[],
        );
        assert_eq!(
            view.stages,
            vec![("scope", 0), ("recent", 5), ("similar", 5)]
        );
        assert_eq!(shown_ids(&view)[..5], ids((15..20).rev())[..]);

        // A scope row first: the recent stage gets half of what it left.
        let in_scope = select_existing(
            &existing,
            "soa://t/00",
            budget,
            Some(&scores),
            Compact::No,
            &HashSet::new(),
            &[],
        );
        assert_eq!(
            in_scope.stages,
            vec![("scope", 1), ("recent", 4), ("similar", 5)]
        );
    }

    #[test]
    fn the_most_similar_rows_fill_the_rest_of_the_budget() {
        let (existing, scores, cost) = twenty_rows();
        let budget = 10 * cost + cost / 2;
        let view = select_existing(
            &existing,
            "",
            budget,
            Some(&scores),
            Compact::No,
            &HashSet::new(),
            &[],
        );
        let mut expected = ids((15..20).rev());
        expected.extend(ids(0..5));
        assert_eq!(
            shown_ids(&view),
            expected,
            "the 5 newest, then the 5 most similar (the oldest)"
        );
        assert!(view.tokens <= budget);
    }

    #[test]
    fn without_an_embedding_model_the_order_is_pure_recency() {
        let (existing, _, cost) = twenty_rows();
        let budget = 10 * cost + cost / 2;
        let view = select_existing(
            &existing,
            "",
            budget,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        );
        assert_eq!(shown_ids(&view), ids((10..20).rev()));
        assert_eq!(view.stages, vec![("scope", 0), ("recent", 10)]);
    }

    /// CodeRabbit on #1806: without scores no similarity stage follows, so
    /// the recency order gets all of the budget in one stage. Rows of about
    /// 240, 180 and 40 tokens, newest first, and a budget of 400: a half-cap
    /// stage skips the 240 row, takes the 180 one, and the rest (220) skips
    /// the 240 row again.
    #[test]
    fn without_scores_the_recency_fallback_gets_the_whole_budget() {
        let sized = |id: &str, chars: usize, updated: i64| {
            let mut r = row(id, "Task", id, updated);
            r.properties.insert("summary".into(), "z".repeat(chars));
            r
        };
        let a = sized("soa://a", 900, 3);
        let b = sized("soa://b", 660, 2);
        let c = sized("soa://c", 100, 1);
        let cost = |r: &InstanceContext| {
            select_existing(
                &existing_of(vec![r.clone()]),
                "",
                usize::MAX,
                None,
                Compact::No,
                &HashSet::new(),
                &[],
            )
            .tokens
        };
        let (ca, cb, cc) = (cost(&a), cost(&b), cost(&c));
        let budget = 400;
        assert!(ca > budget / 2 && cb <= budget / 2 && cb + cc > budget / 2);
        assert!(ca + cb > budget && ca + cc <= budget, "{ca} {cb} {cc}");

        let view = select_existing(
            &existing_of(vec![a, b, c]),
            "",
            budget,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        );
        assert_eq!(shown_ids(&view), vec!["soa://a", "soa://c"]);
    }

    /// CodeRabbit on #1806: when the pass's own rows use the whole budget,
    /// no other row can be shown, so no row is embedded.
    #[test]
    fn a_scope_that_fills_the_budget_skips_the_similarity_order() {
        let (existing, _, cost) = twenty_rows();
        let full = select_existing(
            &existing,
            "soa://t/",
            3 * cost,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        );
        assert_eq!(full.rows_shown, 3);
        assert!(!needs_similarity(&full), "{:?}", full.stages);

        // Scope rows `soa://t/00`..`09` leave room for 5 more.
        let room = select_existing(
            &existing,
            "soa://t/0",
            15 * cost,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        );
        assert_eq!(room.stages[0], ("scope", 10));
        assert!(needs_similarity(&room));
    }

    /// CodeRabbit on #1806: the line counts distinct instances. An instance in
    /// two classes is in two `existing` arrays and counts once.
    #[test]
    fn the_shown_line_counts_an_instance_in_two_classes_once() {
        let shapes = vec![
            shape_from_sdna("Task", TASK_SDNA),
            shape_from_sdna("Question", QUESTION_SDNA),
        ];
        let transcript = [TranscriptTurn::from_speaker_text("Ana", "hello")];
        let existing = existing_of(vec![
            row("soa://both", "Task", "Both", 2),
            row("soa://both", "Question", "Both", 2),
            row("soa://old", "Task", "Old", 1),
        ]);
        let both = select_existing(
            &existing_of(vec![
                row("soa://both", "Task", "Both", 2),
                row("soa://both", "Question", "Both", 2),
            ]),
            "",
            usize::MAX,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        );
        let view = select_existing(
            &existing,
            "",
            both.tokens,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        );
        assert_eq!((view.rows_shown, view.rows_total), (1, 2));
        let prompt: serde_json::Value = serde_json::from_str(&build_interpretation_input_showing(
            &shapes,
            &transcript,
            &view.shown,
            &[],
            &HashMap::new(),
            Some(view.counts(true)),
        ))
        .unwrap();
        let entries: usize = prompt["classes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|c| c["existing"].as_array().map_or(0, Vec::len))
            .sum();
        assert_eq!(entries, 2, "one instance, two `existing` entries");
        assert!(
            prompt["existing_shown"]
                .as_str()
                .unwrap()
                .starts_with("`existing` shows 1 of 2 instances."),
            "{}",
            prompt["existing_shown"]
        );
    }

    #[test]
    fn rows_in_the_pass_scope_always_come_first() {
        let mut rows: Vec<InstanceContext> = (0..20)
            .map(|i| {
                row(
                    &format!("soa://t/{i:02}"),
                    "Task",
                    &format!("Task {i:02}"),
                    100 + i,
                )
            })
            .collect();
        // The oldest and least similar rows are the pass's own.
        rows.push(row("we://call/1/a", "Task", "Own A", 1));
        rows.push(row("we://call/1/b", "Task", "Own B", 2));
        let mut scores: HashMap<String, f32> = rows.iter().map(|r| (r.id.clone(), 0.9)).collect();
        scores.insert("we://call/1/a".into(), 0.0);
        scores.insert("we://call/1/b".into(), 0.0);
        let existing = existing_of(rows);
        for budget in [60, 200, usize::MAX] {
            for s in [None, Some(&scores)] {
                let view = select_existing(
                    &existing,
                    "we://call/1/",
                    budget,
                    s,
                    Compact::Yes,
                    &HashSet::new(),
                    &[],
                );
                assert_eq!(
                    shown_ids(&view)[..2],
                    ["we://call/1/b".to_string(), "we://call/1/a".to_string()],
                    "budget {budget}"
                );
                assert_eq!(view.stages[0], ("scope", 2));
            }
        }
    }

    /// Lal on #1806: a row larger than the budget must not stop the fill for
    /// every row after it.
    #[test]
    fn a_row_too_large_for_the_budget_does_not_stop_the_fill() {
        let mut big = row("we://call/1/big", "Task", "Big", 999);
        big.properties.insert("summary".into(), "z".repeat(4_000));
        let existing = existing_of(vec![
            big,
            row("we://call/1/small", "Task", "Small", 1),
            row("soa://newest", "Task", "Newest", 998),
            row("soa://older", "Task", "Older", 2),
        ]);
        let view = select_existing(
            &existing,
            "we://call/1/",
            200,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        assert_eq!(
            shown_ids(&view),
            vec!["we://call/1/small", "soa://newest", "soa://older"]
        );
    }

    #[test]
    fn a_zero_budget_skips_the_similarity_order() {
        let (existing, _, cost) = twenty_rows();
        let none = select_existing(&existing, "", 0, None, Compact::Yes, &HashSet::new(), &[]);
        assert_eq!(none.rows_shown, 0);
        assert!(
            !needs_similarity(&none),
            "nothing can be shown: no embedding call"
        );
        let some = select_existing(
            &existing,
            "",
            3 * cost,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        assert!(needs_similarity(&some));
        let all = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        assert!(!needs_similarity(&all), "all rows shown");
    }

    /// The token cost of one row as the prompt shows it, plus its comma,
    /// at the depth `shapes` will indent it.
    fn row_cost(view: &PromptExisting, shown_id: &str, shapes: &[ModelShape]) -> usize {
        let rows = &view.shown[shown_id];
        let json: usize = rows
            .iter()
            .map(|r| estimate_token_count(&existing_entry_json(r).to_string()) + 1)
            .sum();
        let depth = outline_depths(&view.shown, shapes)
            .get(shown_id)
            .copied()
            .unwrap_or(0);
        let line = rows
            .first()
            .map(|r| outline_line_tokens(depth, &r.class, &r.title, shown_id));
        json + line.unwrap_or(0)
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
        let view = select_existing(
            &existing,
            "",
            budget,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
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

    fn prompt_outline(shapes: &[ModelShape], shown: &ExistingInstances) -> String {
        let input = build_interpretation_input(
            shapes,
            &[TranscriptTurn::from_speaker_text("Ana", "hello")],
            shown,
            &[],
            &HashMap::new(),
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        v["outline"].as_str().unwrap_or("").to_string()
    }

    /// Lal on #1710: a compact prompt renames selected rows to `P1`, `P2`, …
    /// but used to leave `parents` as full ids. The outline keys nodes by the
    /// shown id, so a chapter and its note both in the view rendered as two
    /// roots. The child is the newer row, so it is selected first and its
    /// handle exists before the parent's does — the remap has to wait until
    /// the full-id → shown-id map is complete.
    #[test]
    fn compact_handles_nest_a_child_selected_before_its_parent() {
        let shapes = vec![
            shape_from_sdna("Chapter", AA_PARENT_SDNA),
            shape_from_sdna("Note", AA_CHILD_SDNA),
        ];
        let parent_id = "ns://chapter/1";
        let child_id = "ns://note/1";
        let mut child = row(child_id, "Note", "Note 1", 500);
        child.parents.insert("partOf".into(), parent_id.to_string());
        let existing = existing_of(vec![row(parent_id, "Chapter", "Chapter A", 100), child]);
        let view = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &shapes,
        );
        assert_eq!(
            view.order,
            vec![child_id.to_string(), parent_id.to_string()],
            "the newer child is selected before its parent"
        );
        assert_eq!(
            view.shown["P1"][0]
                .parents
                .get("partOf")
                .map(String::as_str),
            Some("P2"),
            "the parent was selected, so the row names it by its shown handle"
        );
        assert_eq!(view.handles.id_of("P1"), Some(child_id));
        assert_eq!(view.handles.id_of("P2"), Some(parent_id));
        assert_eq!(
            prompt_outline(&shapes, &view.shown),
            "- [Chapter] Chapter A (P2)\n  - [Note] Note 1 (P1)"
        );
    }

    /// A parent the budget does not show keeps its full id. That id is not a
    /// handle in the view, so the outline renders the child as a root and the
    /// row still says the parent sits outside the view.
    #[test]
    fn a_parent_the_budget_left_out_keeps_its_full_id() {
        let shapes = vec![
            shape_from_sdna("Chapter", AA_PARENT_SDNA),
            shape_from_sdna("Note", AA_CHILD_SDNA),
        ];
        let parent_id = "ns://chapter/outside";
        let child_id = "ns://note/inside";
        let mut child = row(child_id, "Note", "Note 1", 500);
        child.parents.insert("partOf".into(), parent_id.to_string());
        let mut parent = row(parent_id, "Chapter", "Outside", 100);
        parent
            .properties
            .insert("summary".into(), "z".repeat(8_000));
        let child_only = existing_of(vec![child.clone()]);
        let child_cost = select_existing(
            &child_only,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &shapes,
        )
        .tokens;
        let existing = existing_of(vec![parent, child]);
        let view = select_existing(
            &existing,
            "",
            child_cost,
            None,
            Compact::Yes,
            &HashSet::new(),
            &shapes,
        );
        assert_eq!(view.rows_shown, 1, "only the child fits");
        assert_eq!(view.order, vec![child_id.to_string()]);
        assert_eq!(
            view.shown["P1"][0]
                .parents
                .get("partOf")
                .map(String::as_str),
            Some(parent_id),
            "an unselected parent stays the full id, outside the shown handles"
        );
        assert_eq!(prompt_outline(&shapes, &view.shown), "- [Note] Note 1 (P1)");
    }

    /// A child whose parent is already selected is priced with that parent's
    /// shown handle. Pricing the full id rejects a note that fits, including
    /// the extra indent of nesting under the chapter, and reconciliation only
    /// drops rows, so the note never comes back.
    #[test]
    fn a_child_priced_with_a_selected_parents_handle_still_fits() {
        let shapes = vec![
            shape_from_sdna("Chapter", AA_PARENT_SDNA),
            shape_from_sdna("Note", AA_CHILD_SDNA),
        ];
        let parent_id = format!("ns://chapter/{}", "x".repeat(200));
        let child_id = "ns://note/1";
        // The chapter is newer, so the fill selects it before the note and
        // its handle exists while the note is being priced.
        let chapter = row(&parent_id, "Chapter", "A", 200);
        let mut note = row(child_id, "Note", "B", 100);
        note.parents.insert("partOf".into(), parent_id.clone());

        let mut chapter_shown = chapter.clone();
        chapter_shown.id = "P1".into();
        let chapter_cost = estimate_token_count(&existing_entry_json(&chapter_shown).to_string())
            + 1
            + outline_line_tokens(0, "Chapter", "A", "P1");

        let mut note_short = note.clone();
        note_short.id = "P2".into();
        note_short.parents.insert("partOf".into(), "P1".into());
        let note_at_depth = estimate_token_count(&existing_entry_json(&note_short).to_string())
            + 1
            + outline_line_tokens(1, "Note", "B", "P2");

        let mut note_long = note.clone();
        note_long.id = "P2".into();
        let note_with_full_parent =
            estimate_token_count(&existing_entry_json(&note_long).to_string())
                + 1
                + outline_line_tokens(0, "Note", "B", "P2");
        assert!(
            note_with_full_parent > note_at_depth,
            "the full parent id must be what pushes the note over the budget"
        );

        let budget = chapter_cost + note_at_depth;
        let existing = existing_of(vec![chapter, note]);
        let view = select_existing(
            &existing,
            "",
            budget,
            None,
            Compact::Yes,
            &HashSet::new(),
            &shapes,
        );
        assert_eq!(
            view.order,
            vec![parent_id.clone(), child_id.to_string()],
            "the note fits once its parent is the handle P1"
        );
        assert_eq!(
            view.shown["P2"][0]
                .parents
                .get("partOf")
                .map(String::as_str),
            Some("P1")
        );
        assert!(view.tokens <= budget, "{} > {budget}", view.tokens);
        assert_eq!(
            prompt_outline(&shapes, &view.shown),
            "- [Chapter] A (P1)\n  - [Note] B (P2)"
        );
    }

    /// A parent target that is not a row can still spell a compact handle.
    /// Allocating that handle to an unrelated row makes the outline nest the
    /// note under it. The target stays the original string, and the handle
    /// is skipped.
    #[test]
    fn a_dangling_parent_target_is_not_reused_as_a_compact_handle() {
        let shapes = vec![
            shape_from_sdna("Chapter", AA_PARENT_SDNA),
            shape_from_sdna("Note", AA_CHILD_SDNA),
        ];
        let chapter_id = "ns://chapter/1";
        let note_id = "ns://note/1";
        let chapter = row(chapter_id, "Chapter", "A", 200);
        let mut note = row(note_id, "Note", "B", 100);
        note.parents.insert("partOf".into(), "P1".into());
        let existing = existing_of(vec![chapter, note]);
        let view = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &shapes,
        );
        assert_eq!(view.handles.id_of("P1"), None, "P1 is a dangling target");
        assert_eq!(view.handles.id_of("P2"), Some(chapter_id));
        assert_eq!(view.handles.id_of("P3"), Some(note_id));
        assert_eq!(
            view.shown["P3"][0]
                .parents
                .get("partOf")
                .map(String::as_str),
            Some("P1"),
            "the target is not a selected row, so it stays P1"
        );
        assert_eq!(
            prompt_outline(&shapes, &view.shown),
            "- [Chapter] A (P2)\n- [Note] B (P3)"
        );
    }

    /// Depth-0 outline accounting under-counts a chain: the renderer indents
    /// two spaces per level (`outline_line`). A budget that pays for every
    /// row at depth 0 must not let the rendered prompt grow past it.
    #[test]
    fn a_deep_chain_stays_within_the_outline_budget() {
        let shapes = vec![shape_from_sdna("Note", AA_CHILD_SDNA)];
        let n = 24;
        let mut rows = Vec::new();
        for i in 0..n {
            let id = format!("ns://n/{i:02}");
            let mut r = row(&id, "Note", &format!("N{i}"), i as i64);
            if i > 0 {
                r.parents
                    .insert("partOf".into(), format!("ns://n/{:02}", i - 1));
            }
            rows.push(r);
        }
        // What the selector charges when every line is depth 0 and shown
        // under its full id (the harness path, `Compact::No`).
        let mut budget = 0usize;
        for r in &rows {
            budget += estimate_token_count(&existing_entry_json(r).to_string()) + 1;
            let line = outline_line(0, &r.class, &r.title, &r.id);
            let escaped = serde_json::to_string(&format!("{line}\n")).expect("line");
            let inner = &escaped[1..escaped.len() - 1];
            budget += estimate_token_count(inner) + 1;
        }
        let existing = existing_of(rows);
        let view = select_existing(
            &existing,
            "",
            budget,
            None,
            Compact::No,
            &HashSet::new(),
            &shapes,
        );
        assert!(
            view.tokens <= budget,
            "counted {} > budget {budget}",
            view.tokens
        );
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
            "the prompt grew by {grown}, budget {budget}, shown {}/{}",
            view.rows_shown,
            view.rows_total
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
        let all = select_existing(
            &existing,
            "we://call/1/",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
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
        let budget = row_cost(&all, "P1", &[]) + row_cost(&all, "P2", &[]);
        let two = select_existing(
            &existing,
            "we://call/1/",
            budget,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
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
        let view = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        assert_eq!(shown_ids(&view), vec!["soa://a", "soa://c", "soa://b"]);

        // RFC 3339 timestamps order the same way.
        let mut rfc = existing.clone();
        rfc.get_mut("soa://a").unwrap()[0].updated_at = "2026-11-18T10:00:00.000Z".into();
        rfc.get_mut("soa://b").unwrap()[0].updated_at = "2026-11-18T12:00:00.000Z".into();
        rfc.get_mut("soa://c").unwrap()[0].updated_at = "2026-11-18T11:00:00.000Z".into();
        let view = select_existing(
            &rfc,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        assert_eq!(shown_ids(&view), vec!["soa://b", "soa://c", "soa://a"]);
    }

    #[test]
    fn a_row_scores_its_highest_similarity_to_any_turn() {
        // Two turns; the rows' vectors point near one of them or away.
        let turns = vec![vec![1.0, 0.0, 0.0], vec![0.0, 1.0, 0.0]];
        let rows = vec![
            ("soa://far".to_string(), vec![0.0, 0.0, 1.0]),
            ("soa://near".to_string(), vec![0.0, 1.0, 0.1]),
            ("soa://mid".to_string(), vec![0.6, 0.0, 0.8]),
            ("soa://other-dim".to_string(), vec![1.0, 0.0]),
        ];
        let scores = similarity_from_vectors(&turns, &rows);
        assert!(scores["soa://near"] > scores["soa://mid"]);
        assert!(scores["soa://mid"] > scores["soa://far"]);
        assert!(!scores.contains_key("soa://other-dim"));
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
        let view = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        assert_eq!(
            (1..=3)
                .map(|n| view.handles.id_of(&format!("P{n}")).unwrap())
                .collect::<Vec<_>>(),
            shown_ids(&view),
            "handles in fill order"
        );
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
        let view = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
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
        let view = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
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

    /// Lal on #1806: a real id `P1` must not be redirected to the row that
    /// got handle `P1`.
    #[test]
    fn a_handle_never_takes_the_name_of_a_real_id() {
        let shapes = vec![shape_from_sdna("Task", TASK_WITH_RELATION_SDNA)];
        let existing = existing_of(vec![
            row("P1", "Task", "A record whose id reads like a handle", 300),
            row("soa://task/other", "Task", "Other", 200),
        ]);
        let view = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        assert_eq!(shown_ids(&view), vec!["P1", "soa://task/other"]);
        assert_eq!(view.handles.id_of("P1"), None, "no handle P1");
        assert_eq!(view.handles.id_of("P2"), Some("P1"));
        assert_eq!(view.handles.id_of("P3"), Some("soa://task/other"));

        let answer = parse_interpretation_response(
            r#"[{"class":"Task","id":"P3","title":"Other v2","blocks":["P2"]}]"#,
        )
        .unwrap();
        let resolved = view.handles.resolve(answer, &shapes);
        assert_eq!(resolved[0].id.as_deref(), Some("soa://task/other"));
        assert_eq!(resolved[0].props["blocks"], serde_json::json!(["P1"]));
    }

    /// Lal on #1806: `active_flows[].subject` names a row by its full id, so
    /// that row keeps its full id in `existing`, and the model can tie the
    /// flow to it.
    #[tokio::test]
    async fn a_row_a_flow_rides_on_keeps_its_full_id() {
        use crate::perspectives::flow_context::ContentionStatus;
        ensure_db_init();
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        let existing = existing_of(vec![
            row("soa://task/flowed", "Task", "Ship the release", 2),
            row("soa://task/plain", "Task", "Ship the docs", 1),
        ]);
        let flow = FlowContext {
            flow_name: "Delivery".into(),
            instance_uri: "ad4m://flow/instance/1".into(),
            subject: "soa://task/flowed".into(),
            current_state: "doing".into(),
            flow_interpretation_hint: None,
            reachable_next_states: vec![],
            consensus_rule: None,
            contested: ContentionStatus::NotContested,
        };
        let transcript = [TranscriptTurn::from_speaker_text(
            "Ana",
            "The release is done.",
        )];
        let task = ensure_interpretation_task_for_model(None).unwrap();
        let view = existing_for_prompt(
            &task,
            &shapes,
            &transcript,
            &existing,
            std::slice::from_ref(&flow),
            &HashMap::new(),
            "",
            None,
            Compact::Yes,
            true,
        )
        .await;
        assert_eq!(view.rows_shown, 2);
        assert!(view.shown.contains_key("soa://task/flowed"));
        assert_eq!(view.handles.id_of("P1"), Some("soa://task/plain"));

        let prompt = build_interpretation_input(
            &shapes,
            &transcript,
            &view.shown,
            std::slice::from_ref(&flow),
            &HashMap::new(),
        );
        let json: serde_json::Value = serde_json::from_str(&prompt).unwrap();
        let ids: Vec<&str> = json["classes"][0]["existing"]
            .as_array()
            .unwrap()
            .iter()
            .map(|e| e["id"].as_str().unwrap())
            .collect();
        assert!(
            ids.contains(&json["active_flows"][0]["subject"].as_str().unwrap()),
            "the flow's subject is a row id in the prompt: {ids:?}"
        );
        assert!(!prompt.contains("soa://task/plain"));
    }

    #[test]
    fn the_prompt_says_how_many_rows_it_shows_only_when_some_are_left_out() {
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        let transcript = [TranscriptTurn::from_speaker_text("Ana", "hello")];
        let existing = existing_of(vec![row("soa://a", "Task", "A", 1)]);
        let build = |rows| {
            let prompt = build_interpretation_input_showing(
                &shapes,
                &transcript,
                &existing,
                &[],
                &HashMap::new(),
                rows,
            );
            serde_json::from_str::<serde_json::Value>(&prompt).unwrap()
        };
        let counts = |shown, total, unshown_checked| {
            Some(ExistingShown {
                shown,
                total,
                unshown_checked,
            })
        };
        assert_eq!(
            build(counts(1, 5, true))["existing_shown"],
            "`existing` shows 1 of 5 instances. Instances that are not shown are still checked \
             for duplicates."
        );
        // The WS-RPC harness call does not dedup on drain: the line must not
        // say it does.
        assert_eq!(
            build(counts(1, 5, false))["existing_shown"],
            "`existing` shows 1 of 5 instances. Instances that are not shown are not checked \
             for duplicates: query the class before you create an instance that may already \
             exist."
        );
        assert!(build(counts(1, 1, true)).get("existing_shown").is_none());
        assert_eq!(
            build(None),
            serde_json::from_str::<serde_json::Value>(&build_interpretation_input(
                &shapes,
                &transcript,
                &existing,
                &[],
                &HashMap::new()
            ))
            .unwrap()
        );
    }

    #[test]
    fn a_prompt_that_is_not_compact_keeps_full_ids_and_values() {
        let long = "y".repeat(500);
        let mut r = row("soa://task/a", "Task", "A", 1);
        r.properties.insert("owner".into(), long.clone());
        let existing = existing_of(vec![r]);
        let view = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        );
        assert_eq!(view.shown["soa://task/a"][0].properties["owner"], long);
        assert_eq!(view.handles, Handles::default());
    }

    /// Before/after numbers for #1804: the `existing` block of a staging-like
    /// space with 504 rows. Prints them; run with `--nocapture` to see.
    #[test]
    fn staging_504_rows_before_and_after() {
        let existing = staging_like_rows();
        let before = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::No,
            &HashSet::new(),
            &[],
        );
        let compact = select_existing(
            &existing,
            "",
            usize::MAX,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        // 65,536 window; about 12k tokens of system prompt, examples, schema
        // and a 100-turn transcript (6.6k).
        let budget = existing_budget_tokens(Some(65_536), 12_000);
        let after = select_existing(
            &existing,
            "",
            budget,
            None,
            Compact::Yes,
            &HashSet::new(),
            &[],
        );
        println!(
            "#1804 existing block, 504 rows: before {} tokens (est.), short handles {} tokens, \
             budget {budget} -> {} rows, {} tokens, stages {:?}",
            before.tokens, compact.tokens, after.rows_shown, after.tokens, after.stages
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
        let json: serde_json::Value = serde_json::from_str(&prompt).unwrap();
        let line = json["existing_shown"]
            .as_str()
            .expect("existing_shown line");
        assert!(
            line.ends_with(
                "of 42 instances. Instances that are not shown are still checked for duplicates."
            ),
            "{line}"
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

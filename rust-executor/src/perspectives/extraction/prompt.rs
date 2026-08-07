use super::{class_local_name, relation_predicates, InstanceContext};
use crate::db::Ad4mDb;
use crate::perspectives::model_query::types::ModelShape;
use crate::types::{AIPromptExamples, AITask};
use std::collections::HashMap;

/// assemble the per-call LLM input from the target shapes' extraction hints
/// plus the transcript. Pure — this is exactly where `extraction_hint` enters
/// the prompt. Shape (matches the system prompt):
/// `{ "classes": [{ "name", "hint",
///                  "existing": [{ "id", "title", "class" }, …],
///                  "fields": [{ "name", "required", "hint" }],
///                  "relations": [{ "name", "targetClass", "hint" }] }],
///    "transcript": [{ "speaker", "text" }] }`.
///
/// `existing` maps a class's local name to the instances already in the graph
/// (`id` = base URI, `title` = human identity, `class` = local class name).
/// The `id` gives the LLM the handle it needs to emit an upsert: when an
/// extracted item continues an existing entry, the LLM outputs that entry's
/// `id` and [`plan_extraction_ops`] routes it into the update path instead of
/// creating a duplicate. Titles still drive the deterministic dedup safety net
/// in [`filter_already_present`]. Pass an empty map for none.
///
/// `relations` is the forward-direction, single-target-cardinality relation set
/// (`hasOne` / `belongsToOne`) declared on the shape — the endpoints the LLM
/// can fill with instance *references* (either an existing `id` from the same
/// class's `existing` list, or a `new:<Class>:<n>` placeholder pointing at
/// another instance minted in the same response). Reverse-direction relations
/// and `belongsToMany` / `hasMany` collections are omitted in this phase and
/// will be added when the parser learns to resolve arrays and inverse
/// predicates. `hint` on each relation is the SDNA `extractionHint` declared on
/// the property whose predicate matches the relation (matched via the
/// `properties`/`include_relations` overlap that `load_shape` guarantees);
/// `None` when no hint was declared.
pub fn build_extraction_input(
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    existing: &HashMap<String, Vec<InstanceContext>>,
) -> String {
    let classes: Vec<serde_json::Value> = shapes
        .iter()
        .map(|s| {
            let rel_preds = relation_predicates(s);
            let fields: Vec<serde_json::Value> = s
                .properties
                .iter()
                // The type flag is set by instance_links, not the LLM;
                // relations are rendered in the `relations` block below, not
                // as fields.
                .filter(|p| !p.is_flag && !rel_preds.contains(p.predicate.as_str()))
                .map(|p| {
                    serde_json::json!({
                        "name": p.name,
                        "required": p.is_required,
                        "hint": p.extraction_hint,
                    })
                })
                .collect();
            // Relation-predicate -> property hint. load_shape lists each
            // relation both in `properties` (carrying its extractionHint) and
            // in `include_relations` (carrying the target class); we surface
            // the hint via this predicate join.
            let rel_hint_by_pred: HashMap<&str, Option<&str>> = s
                .properties
                .iter()
                .filter(|p| rel_preds.contains(p.predicate.as_str()))
                .map(|p| (p.predicate.as_str(), p.extraction_hint.as_deref()))
                .collect();
            let relations: Vec<serde_json::Value> = s
                .include_relations
                .iter()
                // Phase 2 renders forward relations only. `belongsToOne` /
                // `belongsToMany` are inherently reverse (target class holds
                // the outbound edge), so writing them requires resolving the
                // inverse predicate — out of scope until Phase 3. Forward
                // `hasOne` and `hasMany` both surface here; cardinality is
                // enforced downstream when the parser resolves refs.
                .filter(|r| r.direction == "forward")
                .map(|r| {
                    serde_json::json!({
                        "name": r.name,
                        "targetClass": r.target_class_name,
                        "hint": rel_hint_by_pred.get(r.predicate.as_str()).and_then(|h| *h),
                    })
                })
                .collect();
            let name = class_local_name(&s.target_class);
            let existing_json: Vec<serde_json::Value> = existing
                .get(name)
                .map(|rows| {
                    rows.iter()
                        .map(|r| {
                            serde_json::json!({
                                "id": r.id,
                                "title": r.title,
                                "class": r.class,
                            })
                        })
                        .collect()
                })
                .unwrap_or_default();
            // `"hint"` is the prompt-facing key (short, cheap in tokens and what
            // the system prompt + few-shot examples reference); its value is the
            // schema's `extractionHint` decorator, surfaced here as
            // `extraction_hint`. The key name is deliberately not "extractionHint"
            // — the LLM never sees the decorator name, only this compact field.
            serde_json::json!({
                "name": name,
                "hint": s.extraction_hint,
                "existing": existing_json,
                "fields": fields,
                "relations": relations,
            })
        })
        .collect();
    let turns: Vec<serde_json::Value> = transcript
        .iter()
        .map(|(speaker, text)| serde_json::json!({ "speaker": speaker, "text": text }))
        .collect();
    serde_json::json!({ "classes": classes, "transcript": turns }).to_string()
}

/// name under which the generic extraction task is registered with
/// `AIService`. Kept stable so `ensure_extraction_task` can find it across
/// executor restarts and multiple callers.
pub const EXTRACTION_TASK_NAME: &str = "adam://extraction";

/// system prompt sent with every extraction call. Instance-specific
/// scaffolding (available classes, their hints, the transcript) is added by
/// `build_extraction_input`, so this stays stable across calls and the
/// task can be reused.
pub const EXTRACTION_SYSTEM_PROMPT: &str = "\
You extract typed instances from a conversation transcript.

You receive a JSON object with these fields:
  - `classes`: available subject classes. Each has a `name`, a natural-language
    `hint` describing when to instantiate it, a list of `fields` (each with a
    `name`, optional `hint`, and `required` flag), and an `existing` array of
    instances already present in the graph for that class. Each existing entry
    is `{id, title, class}` — `id` is that instance's stable handle.
  - `transcript`: an array of turns `{speaker, text}`.

Emit a JSON array. Each element is `{\"class\": <class name>, ...fields}`, where
the fields' values are strings drawn from what participants actually said or
committed to in the transcript.

How to decide what to extract:
  - Consider EACH class independently against the WHOLE transcript, using its
    `hint`. A turn can match one class, several, or none.
  - Do not skip a clearly-stated item just because another one is also present:
    a direct question is a Question even amid tasks; a stated claim or opinion is
    a Belief; a reported fact or measurement is an Observation; a commitment to
    act is a Task/Intention. Capture each on its own merits.
  - At the same time, do not invent items the transcript does not support, and
    do not manufacture instances from greetings or small talk.
  - Only include a field if its value is present or clearly implied; omit
    optional fields you cannot fill.

Worked examples follow (as prior turns) before your real input — study how
every co-present item is captured, then apply the same to your input.

Output rules:
  - Return valid JSON only — no prose, no markdown fences, no <think> blocks.
  - Return an empty array `[]` if nothing matches.
  - Do not invent classes or fields not listed in `classes`.
  - Dedup / update: an `existing` entry is a real instance already in the
    graph. If the transcript adds new field information for the SAME item
    (a missing owner, a small rewording), include that entry's `id` on your
    output to update it. A new item that is merely related or adjacent to an
    existing entry is still NEW: emit it WITHOUT `id`. Never invent an `id`
    that isn't in the `existing` list.
";

/// idempotently register the generic extraction task in the AI-task DB.
///
/// If a task with `EXTRACTION_TASK_NAME` already exists, returns it unchanged
/// (so callers can safely invoke this on every executor startup or before every
/// extraction run). Otherwise inserts a new row bound to the `\"default\"` LLM
/// model — `AIService::replace_model_variables` resolves this to whatever LLM
/// the user has configured as default at prompt time, so extraction works with
/// any model without hard-coding one here.
///
/// DB-only: does not touch the running `AIService`. The runtime path is
/// expected to call `service.spawn_task(task)` separately when it needs the
/// model loaded for a `prompt` call; this split keeps registration testable in
/// CI without a GPU.
/// Few-shot examples sent as prior User/Assistant turns (via `prompt_examples`)
/// ahead of the real input. Three generic, non-test scenarios that teach the
/// failure modes small models hit: (1) a belief and a task in the same snippet
/// must BOTH be captured; (2) a question raised amid tasks must be captured;
/// (3) a transcript that refines an existing entry must emit that entry's `id`
/// as an upsert instead of minting a duplicate. The upsert case is added last
/// (recency-weighted in small LLMs) but the negative "adjacent tasks don't
/// upsert" case is left to the prose rules — extra negative examples were
/// found to bias small models toward under-extraction (dropped modalities).
/// Inputs mirror the JSON shape `build_extraction_input` produces (existing
/// entries carry `id`/`title`/`class`).
fn extraction_examples() -> Vec<AIPromptExamples> {
    let ex1_in = serde_json::json!({
        "classes": [
            {"name":"Task","hint":"An action someone commits to doing.","existing":[],
             "fields":[{"name":"title","required":true,"hint":"Imperative summary."},
                       {"name":"owner","required":false,"hint":"Who will do it."}]},
            {"name":"Belief","hint":"A claim someone holds to be true.","existing":[],
             "fields":[{"name":"title","required":true,"hint":"The claim."}]}
        ],
        "transcript":[
            {"speaker":"A","text":"Our error rate doubled after the last deploy."},
            {"speaker":"B","text":"I'll roll back that deploy this afternoon."}
        ]
    })
    .to_string();
    let ex1_out = serde_json::json!([
        {"class":"Belief","title":"The error rate doubled after the last deploy"},
        {"class":"Task","title":"Roll back the last deploy","owner":"B"}
    ])
    .to_string();

    let ex2_in = serde_json::json!({
        "classes": [
            {"name":"Task","hint":"An action someone commits to doing.","existing":[],
             "fields":[{"name":"title","required":true,"hint":"Imperative summary."},
                       {"name":"owner","required":false,"hint":"Who will do it."}]},
            {"name":"Question","hint":"An open question that needs an answer.","existing":[],
             "fields":[{"name":"title","required":true,"hint":"The question."}]}
        ],
        "transcript":[
            {"speaker":"A","text":"I'll write the migration script today."},
            {"speaker":"B","text":"Should we run it against staging first?"}
        ]
    })
    .to_string();
    let ex2_out = serde_json::json!([
        {"class":"Task","title":"Write the migration script","owner":"A"},
        {"class":"Question","title":"Should we run the migration against staging first?"}
    ])
    .to_string();

    // Upsert example: a Task already exists ("Draft the design doc"). The
    // transcript continues that same task with more detail and adds a new
    // Question. The model must emit the existing Task's `id` (so it upserts
    // the title/owner) rather than creating a duplicate Task.
    let ex3_in = serde_json::json!({
        "classes": [
            {"name":"Task","hint":"An action someone commits to doing.",
             "existing":[
                 {"id":"soa://existing/task/design-doc","title":"Draft the design doc","class":"Task"}
             ],
             "fields":[{"name":"title","required":true,"hint":"Imperative summary."},
                       {"name":"owner","required":false,"hint":"Who will do it."}]},
            {"name":"Question","hint":"An open question that needs an answer.","existing":[],
             "fields":[{"name":"title","required":true,"hint":"The question."}]}
        ],
        "transcript":[
            {"speaker":"A","text":"About that design doc — I'll draft it and circulate it to the team by Friday."},
            {"speaker":"B","text":"Should we include the migration section in v1?"}
        ]
    })
    .to_string();
    let ex3_out = serde_json::json!([
        {"class":"Task","id":"soa://existing/task/design-doc",
         "title":"Draft the design doc and circulate it to the team by Friday","owner":"A"},
        {"class":"Question","title":"Should we include the migration section in v1?"}
    ])
    .to_string();

    // Order matters: small LLMs weight the last example most (recency).
    // Put the upsert case in the MIDDLE so it's learned but doesn't dominate;
    // end on ex1 (belief + task in one turn) so "capture every modality"
    // remains the freshest signal — losing that was the failure mode when
    // ex3 was placed last.
    vec![
        AIPromptExamples {
            input: ex2_in,
            output: ex2_out,
        },
        AIPromptExamples {
            input: ex3_in,
            output: ex3_out,
        },
        AIPromptExamples {
            input: ex1_in,
            output: ex1_out,
        },
    ]
}

pub fn ensure_extraction_task() -> anyhow::Result<AITask> {
    if let Some(existing) = Ad4mDb::with_global_instance(|db| db.get_tasks())?
        .into_iter()
        .find(|t| t.name == EXTRACTION_TASK_NAME)
    {
        return Ok(existing);
    }
    let task_id = Ad4mDb::with_global_instance(|db| {
        db.add_task(
            EXTRACTION_TASK_NAME.to_string(),
            "default".to_string(),
            EXTRACTION_SYSTEM_PROMPT.to_string(),
            extraction_examples(),
            None,
        )
    })?;
    let task = Ad4mDb::with_global_instance(|db| db.get_task(task_id))?
        .ok_or_else(|| anyhow::anyhow!("extraction task vanished immediately after insert"))?;
    Ok(task)
}

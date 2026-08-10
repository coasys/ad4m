use super::class_local_name;
use crate::db::Ad4mDb;
use crate::perspectives::model_query::types::ModelShape;
use crate::types::{AIPromptExamples, AITask};
use std::collections::{HashMap, HashSet};

/// assemble the per-call LLM input from the target shapes' interpretation hints
/// plus the transcript. Pure — this is exactly where `interpretation_hint` enters
/// the prompt. Shape (matches the system prompt):
/// `{ "classes": [{ "name", "hint", "identity"?: <field name>,
///                  "existing": [value,…],
///                  "fields": [{ "name", "required", "hint" }] }],
///    "transcript": [{ "speaker", "text" }] }`.
///
/// `existing` maps a class's local name to the identity values of instances
/// already in the graph, so the model can avoid re-proposing them (soft dedup;
/// the hard guarantee is [`filter_already_present`]). Pass an empty map for none.
/// `identity_props` maps a class's local name to the NAME of the property it
/// declares as its identity (e.g. `"title"`, `"name"`) — surfaced in the prompt
/// as `identity` so the model knows which field the `existing` values belong to
/// and which field it must use in its own output for dedup to work. Classes
/// missing from this map have no declared identity (no dedup) and no `identity`
/// key is emitted for them.
pub fn build_interpretation_input(
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    existing: &HashMap<String, Vec<String>>,
    identity_props: &HashMap<String, String>,
) -> String {
    let classes: Vec<serde_json::Value> = shapes
        .iter()
        .map(|s| {
            // Relations are link-typed (targets are instance URIs, not literals)
            // and handled in a later PR, so they're excluded from the fields
            // offered to the LLM. `load_shape` lists each relation in both
            // `properties` and `include_relations`; we key off the latter.
            let rel_preds: HashSet<&str> = s
                .include_relations
                .iter()
                .map(|r| r.predicate.as_str())
                .collect();
            let fields: Vec<serde_json::Value> = s
                .properties
                .iter()
                // The type flag is set by the class constructor, not the LLM;
                // relations are link-typed and handled in a later PR.
                .filter(|p| !p.is_flag && !rel_preds.contains(p.predicate.as_str()))
                .map(|p| {
                    serde_json::json!({
                        "name": p.name,
                        "required": p.is_required,
                        "hint": p.interpretation_hint,
                    })
                })
                .collect();
            let name = class_local_name(&s.target_class);
            // `"hint"` is the prompt-facing key (short, cheap in tokens and what
            // the system prompt + few-shot examples reference); its value is the
            // schema's `interpretationHint` decorator, surfaced here as
            // `interpretation_hint`. The key name is deliberately not "interpretationHint"
            // — the LLM never sees the decorator name, only this compact field.
            let mut class_obj = serde_json::json!({
                "name": name,
                "hint": s.interpretation_hint,
                "existing": existing.get(name).cloned().unwrap_or_default(),
                "fields": fields,
            });
            // Only emit `identity` when the class declared one — otherwise
            // the LLM should not infer a dedup field where none exists.
            if let Some(id_field) = identity_props.get(name) {
                class_obj["identity"] = serde_json::Value::String(id_field.clone());
            }
            class_obj
        })
        .collect();
    let turns: Vec<serde_json::Value> = transcript
        .iter()
        .map(|(speaker, text)| serde_json::json!({ "speaker": speaker, "text": text }))
        .collect();
    serde_json::json!({ "classes": classes, "transcript": turns }).to_string()
}

/// name under which the generic interpretation task is registered with
/// `AIService`. Kept stable so `ensure_interpretation_task` can find it across
/// executor restarts and multiple callers.
pub const INTERPRETATION_TASK_NAME: &str = "adam://interpretation";

/// system prompt sent with every interpretation call. Instance-specific
/// scaffolding (available classes, their hints, the transcript) is added by
/// `build_interpretation_input`, so this stays stable across calls and the
/// task can be reused.
pub const INTERPRETATION_SYSTEM_PROMPT: &str = "\
You extract typed instances from a conversation transcript.

You receive a JSON object with these fields:
  - `classes`: available subject classes. Each has a `name`, a natural-language
    `hint` describing when to instantiate it, a list of `fields` (each with a
    `name`, optional `hint`, and `required` flag), an optional `identity`
    naming the dedup field, and an `existing` array of identity values already
    present in the graph for that class.
  - `transcript`: an array of turns `{speaker, text}`.

Emit a JSON array. Each element is `{\"class\": <class name>, ...fields}`, where
the fields' values are strings drawn from what participants actually said or
committed to in the transcript.

How to decide what to extract:
  - Consider EACH class independently against the WHOLE transcript, using its
    `hint`. A turn can match one class, several, or none.
  - Do not skip a clearly-stated item just because another one is also present:
    a single turn may satisfy several classes at once. Judge each class only by
    its own `hint`, and capture every item that clearly matches it on its own
    merits — independent of whatever other classes are also present.
  - At the same time, do not invent items the transcript does not support, and
    do not manufacture instances from greetings or small talk.
  - Only include a field if its value is present or clearly implied; omit
    optional fields you cannot fill.

Two worked examples follow (as prior turns) before your real input — study how
every co-present item is captured, then apply the same to your input.

Output rules:
  - Return valid JSON only — no prose, no markdown fences, no <think> blocks.
  - Return an empty array `[]` if nothing matches.
  - Do not invent classes or fields not listed in `classes`.
  - Dedup: skip an item ONLY when its `identity` value clearly matches one
    already in that class's `existing` list. A brand-new item still counts
    even if an older, different item of the same class exists — always
    extract genuinely new items.
";

/// idempotently register the generic interpretation task in the AI-task DB.
///
/// If a task with `INTERPRETATION_TASK_NAME` already exists, returns it unchanged
/// (so callers can safely invoke this on every executor startup or before every
/// interpretation run). Otherwise inserts a new row bound to the `\"default\"` LLM
/// model — `AIService::replace_model_variables` resolves this to whatever LLM
/// the user has configured as default at prompt time, so interpretation works with
/// any model without hard-coding one here.
///
/// DB-only: does not touch the running `AIService`. The runtime path is
/// expected to call `service.spawn_task(task)` separately when it needs the
/// model loaded for a `prompt` call; this split keeps registration testable in
/// CI without a GPU.
/// Few-shot examples sent as prior User/Assistant turns (via `prompt_examples`)
/// ahead of the real input. Two generic, non-test scenarios that teach the
/// failure modes small models hit: (1) a belief and a task in the same snippet
/// must BOTH be captured; (2) a question raised amid tasks must be captured.
/// Inputs mirror the JSON shape `build_interpretation_input` produces.
fn interpretation_examples() -> Vec<AIPromptExamples> {
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

    vec![
        AIPromptExamples {
            input: ex1_in,
            output: ex1_out,
        },
        AIPromptExamples {
            input: ex2_in,
            output: ex2_out,
        },
    ]
}

pub fn ensure_interpretation_task() -> anyhow::Result<AITask> {
    if let Some(existing) = Ad4mDb::with_global_instance(|db| db.get_tasks())?
        .into_iter()
        .find(|t| t.name == INTERPRETATION_TASK_NAME)
    {
        return Ok(existing);
    }
    let task_id = Ad4mDb::with_global_instance(|db| {
        db.add_task(
            INTERPRETATION_TASK_NAME.to_string(),
            "default".to_string(),
            INTERPRETATION_SYSTEM_PROMPT.to_string(),
            interpretation_examples(),
            None,
        )
    })?;
    let task = Ad4mDb::with_global_instance(|db| db.get_task(task_id))?
        .ok_or_else(|| anyhow::anyhow!("interpretation task vanished immediately after insert"))?;
    Ok(task)
}

use super::{class_local_name, instances_by_class, relation_predicates, ExistingInstances};
use crate::db::Ad4mDb;
use crate::perspectives::model_query::types::ModelShape;
use crate::types::{AIPromptExamples, AITask};
use std::collections::HashMap;

/// assemble the per-call LLM input from the target shapes' interpretation hints
/// plus the transcript. Pure — this is exactly where `interpretation_hint` enters
/// the prompt. Shape (matches the system prompt):
/// `{ "classes": [{ "name", "hint",
///                  "existing": [{ "id", "title", "class" }, …],
///                  "fields": [{ "name", "required", "hint" }],
///                  "relations": [{ "name", "targetClass", "hint" }] }],
///    "transcript": [{ "speaker", "text" }] }`.
///
/// `existing` maps a class's local name to the instances already in the graph
/// (`id` = base URI, `title` = the class's declared identity value, `class` =
/// local class name). The `id` gives the LLM the handle it needs to emit an
/// upsert: when an interpreted item continues an existing entry, the LLM outputs
/// that entry's `id` and [`plan_interpretation_ops_with_context`] routes it into
/// the update path instead of creating a duplicate. Identity values still drive
/// the deterministic dedup safety net in [`filter_already_present`]. Pass an
/// empty map for none.
///
/// The prompt key stays `"title"` regardless of which property a class declares
/// as its `identity` — it is the LLM-facing name for "the human-readable handle
/// of this instance", and the system prompt plus few-shot examples all reference
/// it under that name.
///
/// `relations` is the forward-direction relation set declared on the shape — the
/// endpoints the LLM can fill with instance *references* (either an existing
/// `id` from the target class's `existing` list, or a `new:<Class>:<n>`
/// placeholder pointing at another instance minted in the same response).
/// Reverse-direction relations are omitted in this phase: writing one requires
/// resolving the inverse predicate on the target class. `hint` on each relation
/// is the SDNA `interpretationHint` declared on the property whose predicate
/// matches the relation (matched via the `properties`/`include_relations`
/// overlap that `load_shape` guarantees); `None` when no hint was declared.
pub fn build_interpretation_input(
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    existing: &ExistingInstances,
) -> String {
    // Group the id-keyed source by class once for the per-class `existing`
    // blocks below (deterministically ordered — see `instances_by_class`).
    let existing_by_class = instances_by_class(existing);
    let classes: Vec<serde_json::Value> = shapes
        .iter()
        .map(|s| {
            let rel_preds = relation_predicates(s);
            let fields: Vec<serde_json::Value> = s
                .properties
                .iter()
                // The type flag is set by the class constructor, not the LLM;
                // relations are rendered in the `relations` block below, not
                // as fields.
                .filter(|p| !p.is_flag && !rel_preds.contains(p.predicate.as_str()))
                .map(|p| {
                    serde_json::json!({
                        "name": p.name,
                        "required": p.is_required,
                        "hint": p.interpretation_hint,
                    })
                })
                .collect();
            // Relation-predicate -> property hint. load_shape lists each
            // relation both in `properties` (carrying its interpretationHint)
            // and in `include_relations` (carrying the target class); we surface
            // the hint via this predicate join.
            let rel_hint_by_pred: HashMap<&str, Option<&str>> = s
                .properties
                .iter()
                .filter(|p| rel_preds.contains(p.predicate.as_str()))
                .map(|p| (p.predicate.as_str(), p.interpretation_hint.as_deref()))
                .collect();
            let relations: Vec<serde_json::Value> = s
                .include_relations
                .iter()
                // Phase 2 renders forward relations only. `belongsToOne` /
                // `belongsToMany` are inherently reverse (target class holds
                // the outbound edge), so writing them requires resolving the
                // inverse predicate — out of scope until Phase 3. Forward
                // `hasOne` and `hasMany` both surface here; cardinality is
                // enforced downstream when the planner resolves refs.
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
            let existing_json: Vec<serde_json::Value> = existing_by_class
                .get(name)
                .map(|rows| {
                    rows.iter()
                        .map(|r| {
                            // `properties` is the current secondary-scalar
                            // state (e.g. rolling `summary` on a
                            // ConversationSubgroup). Rendered only when
                            // present so the prompt stays compact for
                            // identity-only classes.
                            let mut entry = serde_json::Map::new();
                            entry.insert("id".into(), serde_json::json!(r.id));
                            entry.insert("title".into(), serde_json::json!(r.title));
                            entry.insert("class".into(), serde_json::json!(r.class));
                            if !r.properties.is_empty() {
                                let props: serde_json::Map<String, serde_json::Value> = r
                                    .properties
                                    .iter()
                                    .map(|(k, v)| (k.clone(), serde_json::json!(v)))
                                    .collect();
                                entry.insert("properties".into(), serde_json::Value::Object(props));
                            }
                            serde_json::Value::Object(entry)
                        })
                        .collect()
                })
                .unwrap_or_default();
            // `"hint"` is the prompt-facing key (short, cheap in tokens and what
            // the system prompt + few-shot examples reference); its value is the
            // schema's `interpretationHint` decorator, surfaced here as
            // `interpretation_hint`. The key name is deliberately not "interpretationHint"
            // — the LLM never sees the decorator name, only this compact field.
            serde_json::json!({
                "name": name,
                "hint": s.interpretation_hint,
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
    `name`, optional `hint`, and `required` flag), a `relations` list of
    forward instance-reference slots (each with a `name`, `targetClass`, and
    optional `hint`), and an `existing` array of instances already present in
    the graph for that class. Each existing entry is `{id, title, class}`, and
    may also carry a `properties` object holding the instance's current
    secondary-scalar values (e.g. a rolling summary). `id` is the stable
    handle you emit to update that entry; `title` is its identity label; the
    optional `properties` object shows its *current state* so you can judge
    whether new turns continue that instance or belong to a fresh one.
  - `transcript`: an array of turns `{speaker, text}`.

Emit a JSON array. Each element is `{\"class\": <class name>, ...fields, ...relations}`,
where fields carry strings drawn from what participants actually said or
committed to, and relations carry *references* to other instances (see below).

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

Relations (linking instances together):
  - A relation's value is an *instance reference*, not a plain string. Two forms:
    (a) an existing entry's `id` from the target class's `existing` list, or
    (b) `\"new:<TargetClass>:<n>\"` — a 1-based ordinal pointing at the nth
        element of `<TargetClass>` in the array you return this turn (so
        `\"new:Task:2\"` links to the second Task element in your output).
  - Only set a relation when the transcript clearly identifies the target;
    omit the relation field otherwise. Never invent an `id`, and never emit a
    `\"new:<Class>:<n>\"` ref for which no matching output element exists.

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
  - When an existing entry carries `properties` (e.g. a `summary`), read both
    its `title` AND its `properties` before deciding whether to reuse its
    `id`. If the current turns are on a clearly different topic from what
    that entry's title + properties describe, mint a NEW instance (no `id`)
    even when it is the only existing entry — reusing an `id` for an
    unrelated topic silently overwrites the existing state and destroys data.
  - Partition, do not broadcast: each turn's content belongs to exactly ONE
    instance per class. When some turns open a new topic and you mint a NEW
    instance for them, put that content ONLY in the new instance. Do NOT also
    emit an update to an unrelated existing entry (e.g. do not grow another
    subgroup's `summary` with the new topic). Emit an existing entry's `id`
    ONLY when the current turns are about THAT entry's own topic; otherwise
    leave it out of your output entirely so it stays exactly as it is. It is
    correct and expected to both mint a new instance AND leave every unrelated
    existing entry untouched in the same response.
";

/// Few-shot examples sent as prior User/Assistant turns (via `prompt_examples`)
/// ahead of the real input. Four generic, non-test scenarios that teach the
/// failure modes small models hit: (1) a belief and a task in the same snippet
/// must BOTH be captured; (2) a question raised amid tasks must be captured;
/// (3) a transcript that refines an existing entry must emit that entry's `id`
/// as an upsert instead of minting a duplicate; (4) instances co-created in one
/// pass can be linked by relation using the `new:<Class>:<n>` ref syntax
/// (Phase 2). The *upsert* example is added LAST — it's the most fragile
/// behavior on small models, and putting the relations example (all new
/// instances) in the recency slot made gemma3:12b create-happy and stopped it
/// emitting `id`s to update. Relations sit second and still fire reliably.
/// Inputs mirror the JSON shape `build_interpretation_input` produces (existing
/// entries carry `id`/`title`/`class`; each class carries a `relations` block).
pub(crate) fn interpretation_examples() -> Vec<AIPromptExamples> {
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

    // Topic-shift example: a Task already exists, and the transcript is on a
    // completely unrelated topic. The model must NOT reuse the existing Task's
    // `id` for the new work — reusing an id silently overwrites the existing
    // instance and destroys data. The correct output is a fresh Task without
    // an `id`, plus whatever else the transcript introduces. This directly
    // counter-teaches the bias gemma3:12b shows without it: whenever an
    // `existing` entry is present, unconditionally upsert against it.
    let ex_shift_in = serde_json::json!({
        "classes": [
            {"name":"Task","hint":"An action someone commits to doing.",
             "existing":[
                 {"id":"soa://existing/task/design-doc","title":"Draft the design doc","class":"Task"}
             ],
             "fields":[{"name":"title","required":true,"hint":"Imperative summary."},
                       {"name":"owner","required":false,"hint":"Who will do it."}]}
        ],
        "transcript":[
            {"speaker":"A","text":"Switching topics — the CI pipeline has been flaky for a week, I'll dig into the retry stanza tomorrow."}
        ]
    })
    .to_string();
    let ex_shift_out = serde_json::json!([
        {"class":"Task","title":"Investigate CI pipeline flakiness in the retry stanza","owner":"A"}
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

    // Relations example (Phase 2): two Messages and two Topics are minted in
    // the same pass, and two SemanticRelationships link each Topic to a
    // Message via `new:<Class>:<n>` refs. Teaches the LLM (a) the per-class
    // 1-based ordinal counting, (b) that BOTH endpoints can be freshly-minted
    // siblings, (c) how relation fields carry references not free-form
    // strings. A dedicated Message class is preferred over stuffing a
    // literal-URI value into `expression` — using only `new:` refs keeps the
    // example consistent with the "never invent an `id`" rule in the system
    // prompt.
    let ex4_in = serde_json::json!({
        "classes": [
            {"name":"Message","hint":"An utterance exchanged in the transcript.","existing":[],
             "fields":[{"name":"content","required":true,"hint":"Short summary of what was said."}]},
            {"name":"Topic","hint":"A subject the participants discuss.","existing":[],
             "fields":[{"name":"title","required":true,"hint":"Short topic label."}]},
            {"name":"SemanticRelationship",
             "hint":"An edge that tags a Message with a Topic and a relevance score.",
             "existing":[],
             "fields":[{"name":"relevance","required":true,"hint":"0..1 confidence that the tag applies."}],
             "relations":[
                 {"name":"tag","targetClass":"Topic","hint":"The topic being tagged."},
                 {"name":"expression","targetClass":"Message","hint":"The message the topic tags."}
             ]}
        ],
        "transcript":[
            {"speaker":"A","text":"We should log all failed webhook retries — that would help debug the payments outage."},
            {"speaker":"B","text":"Agreed. Retry logging is basically an observability question."}
        ]
    })
    .to_string();
    let ex4_out = serde_json::json!([
        {"class":"Message","content":"We should log all failed webhook retries to debug the payments outage."},
        {"class":"Message","content":"Retry logging is basically an observability question."},
        {"class":"Topic","title":"Webhook retry logging"},
        {"class":"Topic","title":"Observability"},
        {"class":"SemanticRelationship","relevance":0.9,
         "tag":"new:Topic:1","expression":"new:Message:1"},
        {"class":"SemanticRelationship","relevance":0.8,
         "tag":"new:Topic:2","expression":"new:Message:2"}
    ])
    .to_string();

    // Order matters: small LLMs weight the last example most (recency).
    // - ex2 (question amid tasks) first — teaches modal separation.
    // - ex4 (relations) second — teaches the `new:<Class>:<n>` ref syntax.
    // - ex1 (belief + task) third — keeps modality completeness fresh
    //   (`e2e_intention_and_belief` also wraps this in `run_e2e_retrying`).
    // - ex_shift (existing + unrelated new) fourth — counter-teaches the
    //   always-upsert-when-existing bias without displacing ex3 from recency.
    // - ex3 (upsert) LAST — the id-upsert behavior is the most fragile on
    //   small models: with ex4's all-new-instances output in the last slot,
    //   gemma3:12b became create-happy and stopped emitting `id` to update an
    //   existing node (regressed `e2e_updates_existing_instance_via_id` 0/5).
    //   Relations proved robust even off the last slot (the topic-relation e2e
    //   still fires 3/3), so upsert gets the recency bump instead. ex_shift
    //   sits just before it so the two adjacent examples teach the full
    //   attach-vs-mint decision paired.
    vec![
        AIPromptExamples {
            input: ex2_in,
            output: ex2_out,
        },
        AIPromptExamples {
            input: ex4_in,
            output: ex4_out,
        },
        AIPromptExamples {
            input: ex1_in,
            output: ex1_out,
        },
        AIPromptExamples {
            input: ex_shift_in,
            output: ex_shift_out,
        },
        AIPromptExamples {
            input: ex3_in,
            output: ex3_out,
        },
    ]
}

/// Deterministic task name for a per-model interpretation task. `None` returns
/// the shared default name ([`INTERPRETATION_TASK_NAME`]); `Some(model_id)`
/// returns `"adam://interpretation?model=<model_id>"`. Model ids may contain
/// colons (e.g. `"gemma3:12b"`); the `?model=` query-style separator keeps the
/// name unambiguous regardless of embedded punctuation.
pub fn interpretation_task_name_for_model(model_id: Option<&str>) -> String {
    match model_id {
        None => INTERPRETATION_TASK_NAME.to_string(),
        Some(id) => format!("{INTERPRETATION_TASK_NAME}?model={id}"),
    }
}

/// DB-only registration for the shared-default interpretation task. Delegates to
/// [`register_interpretation_task_for_model`] with `None`. Test-only: production
/// paths always carry an explicit (possibly `None`) model through the per-model
/// variant, so this shared-row convenience wrapper only backs the unit tests.
#[cfg(test)]
pub(crate) fn register_interpretation_task() -> anyhow::Result<(AITask, bool)> {
    register_interpretation_task_for_model(None)
}

/// idempotently register the generic interpretation task row in the AI-task DB,
/// optionally bound to a specific model (`Some(model_id)` → a distinct
/// `?model=<id>` row; `None` → the shared-default row).
///
/// DB-only, synchronous, and does not touch the running `AIService` — so task
/// registration stays unit-testable without a model/GPU. Returns `(task,
/// created)`; `created` is `true` only when this call inserted the row. The
/// async entry points [`ensure_interpretation_task`] /
/// [`ensure_interpretation_task_for_model`] wrap this and spawn the task.
pub(crate) fn register_interpretation_task_for_model(
    model_id: Option<&str>,
) -> anyhow::Result<(AITask, bool)> {
    let name = interpretation_task_name_for_model(model_id);
    if let Some(existing) = Ad4mDb::with_global_instance(|db| db.get_tasks())?
        .into_iter()
        .find(|t| t.name == name)
    {
        return Ok((existing, false));
    }
    let db_model_id = model_id.unwrap_or("default").to_string();
    let task_id = Ad4mDb::with_global_instance(|db| {
        db.add_task(
            name.clone(),
            db_model_id,
            INTERPRETATION_SYSTEM_PROMPT.to_string(),
            interpretation_examples(),
            None,
        )
    })?;
    let task = Ad4mDb::with_global_instance(|db| db.get_task(task_id))?
        .ok_or_else(|| anyhow::anyhow!("interpretation task vanished immediately after insert"))?;
    Ok((task, true))
}

/// Return a ready-to-prompt interpretation task: register the DB row if absent
/// (via [`register_interpretation_task`]) AND ensure it is spawned into its LLM
/// worker, so the caller can immediately `AIService::prompt` it.
///
/// The spawn is what makes this async and AIService-dependent. `register_...`
/// only writes the DB row; a freshly-registered task is invisible to the worker
/// until the next `set_default_model`/restart `load()` sweep, so its first
/// `prompt` would fail with "Task not spawned". We spawn it here, but only when
/// this call actually minted the row — a pre-existing row is already spawned
/// (boot-time `load()` sweep, or the call that first created it), so re-spawning
/// would force a redundant local-model warmup on every interpretation run.
pub async fn ensure_interpretation_task() -> anyhow::Result<AITask> {
    ensure_interpretation_task_for_model(None).await
}

/// Per-model variant of [`ensure_interpretation_task`]: register the row for
/// `model_id` if absent (via [`register_interpretation_task_for_model`]) AND
/// ensure it is spawned into its LLM worker, so the caller can immediately
/// `AIService::prompt` it. `None` targets the shared-default row. Spawns only on
/// the call that minted the row (a pre-existing row is already spawned), avoiding
/// a redundant local-model warmup on every interpretation run.
pub async fn ensure_interpretation_task_for_model(
    model_id: Option<&str>,
) -> anyhow::Result<AITask> {
    let (task, created) = register_interpretation_task_for_model(model_id)?;
    if created {
        crate::ai_service::AIService::global_instance()
            .await
            .map_err(|e| anyhow::anyhow!("ensure_interpretation_task: AIService not ready: {e:#}"))?
            .spawn_registered_task(task.clone())
            .await
            .map_err(|e| {
                anyhow::anyhow!(
                    "ensure_interpretation_task: failed to spawn interpretation task: {e:#}"
                )
            })?;
    }
    Ok(task)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::*;
    use crate::perspectives::interpretation_test_support::*;
    use std::collections::BTreeMap;

    #[test]
    fn interpretation_hint_lands_in_prompt() {
        let shapes = vec![
            shape_from_sdna("Belief", BELIEF_SDNA),
            shape_from_sdna("Intention", INTENTION_SDNA),
        ];
        let input = build_interpretation_input(
            &shapes,
            &[(
                "Nico".into(),
                "I'll extract the LLM processing into ADAM".into(),
            )],
            &no_existing(),
        );

        // class-level hints reach the prompt
        assert!(input.contains("A claim a participant holds to be true"));
        assert!(input.contains("A first-person commitment to do something"));
        // per-field hint + required flag
        assert!(input.contains("Imperative summary of the work"));
        assert!(input.contains("\"required\":true"));
        // transcript included
        assert!(input.contains("Nico") && input.contains("extract the LLM processing"));

        // valid JSON, two classes, type-flag excluded from fields
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        assert_eq!(v["classes"].as_array().unwrap().len(), 2);
        let intention = v["classes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|c| c["name"] == "Intention")
            .expect("Intention class in prompt");
        let field_names: Vec<&str> = intention["fields"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|f| f["name"].as_str())
            .collect();
        assert!(field_names.contains(&"title") && field_names.contains(&"owner"));
        assert!(
            !field_names.contains(&"type"),
            "type flag must not be a field"
        );
        assert!(
            intention["relations"].as_array().unwrap().is_empty(),
            "relation-free shape must render an empty relations block"
        );
    }

    #[test]
    fn existing_context_renders_id_title_class_in_prompt() {
        // With the richer `existing_instance_context` snapshot in play,
        // build_interpretation_input must render each existing entry as an object
        // carrying `id`, `title`, and `class` — that's the handle the LLM needs to
        // emit an upsert instead of a duplicate create. Also proves the system
        // prompt still describes the `id` upsert path.
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        let existing = existing_map(vec![InstanceContext {
            id: "soa://existing/task/42".to_string(),
            title: "Draft the design doc".to_string(),
            class: "Task".to_string(),
            properties: BTreeMap::new(),
        }]);
        let input = build_interpretation_input(
            &shapes,
            &[("Nico".into(), "About that design doc…".into())],
            &existing,
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        let task_class = v["classes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|c| c["name"] == "Task")
            .expect("Task class in prompt");
        let existing_arr = task_class["existing"]
            .as_array()
            .expect("existing must be an array");
        assert_eq!(existing_arr.len(), 1);
        assert_eq!(existing_arr[0]["id"], "soa://existing/task/42");
        assert_eq!(existing_arr[0]["title"], "Draft the design doc");
        assert_eq!(existing_arr[0]["class"], "Task");
        // Empty `properties` map must NOT render — keeps the prompt compact for
        // identity-only classes (regression guard for the render-when-non-empty
        // rule in `build_interpretation_input`).
        assert!(
            existing_arr[0].get("properties").is_none(),
            "empty properties map must be omitted from the rendered entry, got {:?}",
            existing_arr[0]
        );
        // System prompt still teaches the id-upsert semantics — regression guard
        // against silently dropping that instruction while refactoring the schema.
        assert!(
            INTERPRETATION_SYSTEM_PROMPT.contains("id"),
            "system prompt must document the `id` upsert semantics"
        );
    }

    #[test]
    fn existing_context_with_properties_renders_them_into_prompt() {
        // When an `InstanceContext` carries secondary scalars (e.g. the rolling
        // `summary` on a ConversationSubgroup), the prompt must include a
        // `properties` object on the corresponding `existing` entry. This is what
        // gives the LLM enough state to decide whether new turns continue an
        // existing instance or belong to a fresh one on a different topic — the
        // topic-shift discrimination the identity-only view could not support.
        let shapes = vec![shape_from_sdna(
            "ConversationSubgroup",
            CONVERSATION_SUBGROUP_SDNA,
        )];
        let mut properties: BTreeMap<String, String> = BTreeMap::new();
        properties.insert(
            "summary".to_string(),
            "The team discussed dropped webhook retries during a recent payments outage."
                .to_string(),
        );
        let existing = existing_map(vec![InstanceContext {
            id: "soa://existing/subgroup/payments".to_string(),
            title: "Payments infrastructure".to_string(),
            class: "ConversationSubgroup".to_string(),
            properties,
        }]);
        let input = build_interpretation_input(
            &shapes,
            &[("Ana".into(), "Switching topics — Q3 retro planning.".into())],
            &existing,
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        let sg_class = v["classes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|c| c["name"] == "ConversationSubgroup")
            .expect("ConversationSubgroup class in prompt");
        let entry = &sg_class["existing"].as_array().expect("existing array")[0];
        let rendered_props = entry["properties"]
            .as_object()
            .expect("populated properties must render as an object");
        assert_eq!(
            rendered_props.get("summary").and_then(|v| v.as_str()),
            Some("The team discussed dropped webhook retries during a recent payments outage.")
        );
        // System prompt must instruct the model to actually READ `properties`, not
        // just be handed them silently — otherwise the topic-shift fix is dead on
        // arrival for smaller local models.
        assert!(
            INTERPRETATION_SYSTEM_PROMPT.contains("properties"),
            "system prompt must document the `properties` field on existing entries"
        );
    }

    #[test]
    fn system_prompt_documents_relation_ref_syntax() {
        // The system prompt must teach the LLM both the shape of the per-class
        // `relations` block AND the two allowed reference forms it can put into a
        // relation value. Without this instruction the LLM only sees an unfamiliar
        // array in the input schema — the planner can only resolve refs it was told
        // to emit.
        let p = INTERPRETATION_SYSTEM_PROMPT;
        assert!(
            p.contains("`relations`"),
            "system prompt must introduce the relations block on each class"
        );
        assert!(
            p.contains("instance-reference") || p.contains("instance reference"),
            "system prompt must frame a relation value as an instance reference"
        );
        // The `new:<TargetClass>:<n>` placeholder is the only way to link two
        // freshly-minted siblings in the same response; a missing description
        // would silently downgrade sibling-linking to unresolved refs at plan
        // time. Assert both the literal token and the 1-based ordinal wording.
        assert!(
            p.contains("new:<TargetClass>:<n>"),
            "system prompt must document the `new:<TargetClass>:<n>` ref form"
        );
        assert!(
            p.contains("1-based"),
            "system prompt must state the ordinal is 1-based"
        );
        // Existing-id path must still be described alongside the new-ref path so
        // the LLM picks the right form per case (upsert-target vs sibling-mint).
        assert!(
            p.contains("existing") && p.contains("`id`") && p.contains("existing entry's `id`"),
            "system prompt must document linking via an existing entry's id"
        );
        // Guardrail wording: unresolved refs and fabricated ids are the two
        // failure modes; without explicit prohibitions small LLMs invent both.
        assert!(
            p.contains("Never invent an `id`"),
            "system prompt must forbid fabricated ids"
        );
        assert!(
            p.contains("never emit a") || p.contains("Never emit a"),
            "system prompt must forbid unresolved `new:` refs"
        );
    }

    #[test]
    fn relations_few_shot_example_is_present_and_upsert_is_last() {
        // The few-shot set must include a dedicated relations example demonstrating
        // the `new:<Class>:<n>` ref syntax the system prompt teaches. It need NOT be
        // last: empirically, putting the relations example (all-new-instances) in
        // the recency slot made gemma3:12b create-happy and regressed the id-upsert
        // behavior (`e2e_updates_existing_instance_via_id` 0/5). So the *upsert*
        // example owns the last slot; relations sit earlier and still fire reliably.
        let examples = interpretation_examples();
        assert_eq!(examples.len(), 5, "expected exactly five few-shot examples");

        // Find the relations example wherever it sits: the one whose output uses
        // the `new:<Class>:<n>` ref syntax on both endpoints of a
        // SemanticRelationship.
        let relations_ex = examples
            .iter()
            .find(|e| {
                e.output.contains("\"new:Topic:1\"") && e.output.contains("\"new:Message:1\"")
            })
            .expect(
                "a relations few-shot example demonstrating `new:<Class>:<n>` refs must be present",
            );
        // Its input must render the `relations` block so the LLM sees the schema
        // half of the ref-syntax lesson.
        let v: serde_json::Value = serde_json::from_str(&relations_ex.input).unwrap();
        let rel_class = v["classes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|c| c["name"] == "SemanticRelationship")
            .expect("relations few-shot must declare a SemanticRelationship class");
        let rels = rel_class["relations"]
            .as_array()
            .expect("SemanticRelationship must render a relations block");
        let rel_names: Vec<&str> = rels.iter().filter_map(|r| r["name"].as_str()).collect();
        assert!(
        rel_names.contains(&"tag") && rel_names.contains(&"expression"),
        "relations example must declare both `tag` and `expression` relations; got {rel_names:?}"
    );

        // The LAST example must be the upsert one — it carries an `existing` entry
        // with an `id` and re-emits that `id` in its output (the fragile behavior
        // that needs the recency slot). Guard against a future reshuffle silently
        // regressing upsert recall again.
        let last = examples.last().unwrap();
        let lv: serde_json::Value = serde_json::from_str(&last.input).unwrap();
        let has_existing_with_id = lv["classes"].as_array().unwrap().iter().any(|c| {
            c["existing"]
                .as_array()
                .map(|xs| xs.iter().any(|x| x.get("id").is_some()))
                .unwrap_or(false)
        });
        assert!(
            has_existing_with_id,
            "the last few-shot example must be the upsert one (an `existing` entry \
         carrying an `id`), so id-upsert keeps the recency slot; last input was: {}",
            last.input
        );
    }
}

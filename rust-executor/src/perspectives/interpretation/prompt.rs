use super::{
    class_label, instances_by_class, relation_predicates, ExistingInstances, TranscriptTurn,
};
use crate::db::Ad4mDb;
use crate::perspectives::flow_context::{render_consensus_rule, FlowContext, FlowTokens};
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
///    "transcript": [{ "speaker", "text", "timestamp"? }] }`.
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
///
/// `active_flows` is the slice 10.2 payoff: any FlowInstance currently running
/// on the extraction scope is summarized (via [`FlowContext`]) so the LLM can
/// see the current state, every reachable next-state, and the English rendering
/// of each next-state's `requires` guard + optional `semanticCheck` hint. When
/// non-empty, an `active_flows` key is added to the prompt JSON; when empty,
/// the key is OMITTED entirely so passes with no live flows spend zero prompt
/// tokens on flow scaffolding (and the LLM never sees an empty section that
/// might confuse it into inventing flows). Callers with no active flows can
/// safely pass `&[]`.
pub fn build_interpretation_input(
    shapes: &[ModelShape],
    transcript: &[TranscriptTurn],
    existing: &ExistingInstances,
    active_flows: &[FlowContext],
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
                    // Collision-aware label (CodeRabbit #881 review): the
                    // planner indexes instances by whatever `class_label`
                    // returns for the target class, so relation `targetClass`
                    // MUST match that label. Bare `target_class_name` would
                    // fork the ref lookup when two shapes share a local name
                    // (e.g. `flux://Task` vs `soa://Task`) — the LLM's
                    // `new:Task:1` would resolve to the wrong bucket or drop
                    // the relation entirely. `target_class_uri` is the
                    // *shape* URI (`ns://TaskShape`), not the class URI, so
                    // we resolve the target shape by matching `shape_uri`
                    // and then feed its `target_class` to `class_label`. No
                    // match (or empty `target_class_uri`) falls back to the
                    // bare name — same fallback the rest of the pipeline
                    // uses.
                    let target_class_label = shapes
                        .iter()
                        .find(|s| !s.shape_uri.is_empty() && s.shape_uri == r.target_class_uri)
                        .map(|s| class_label(&s.target_class, shapes))
                        .unwrap_or_else(|| r.target_class_name.clone());
                    serde_json::json!({
                        "name": r.name,
                        "targetClass": target_class_label,
                        "hint": rel_hint_by_pred.get(r.predicate.as_str()).and_then(|h| *h),
                    })
                })
                .collect();
            let name = class_label(&s.target_class, shapes);
            let name = name.as_str();
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
        .map(|t| {
            let mut obj = serde_json::json!({ "speaker": t.speaker, "text": t.text });
            if !t.timestamp.is_empty() {
                obj["timestamp"] = serde_json::json!(t.timestamp);
            }
            obj
        })
        .collect();
    let mut out = serde_json::json!({ "classes": classes, "transcript": turns });
    if !active_flows.is_empty() {
        out["active_flows"] = serde_json::Value::Array(
            active_flows
                .iter()
                .map(render_active_flow_for_prompt)
                .collect(),
        );
    }
    out.to_string()
}

/// Render one live [`FlowContext`] as the JSON object the LLM sees under
/// `active_flows[]`. Pure. Shape:
/// ```json
/// {
///   "instance": "ad4m://flow/instance/…",   // instance URI (stable handle)
///   "subject":  "ad4m://…",                 // the base expression the flow rides on
///   "flow":     "Delivery",                  // flow name (matches SHACLFlow.name)
///   "currentState": "doing",
///   "hint":     "…",                         // flow-level interpretationHint (omitted if unset)
///   "consensus":"1 signer",                  // flow-level default consensus (omitted if unset)
///   "nextStates": [
///     {
///       "name":         "review",
///       "hint":         "…",                 // per-state interpretationHint (omitted if unset)
///       "requires":     "at least 1 match of Review where owner = \"…\"",  // "" if none
///       "semanticCheck":"…",                 // per-state semanticCheck hint (omitted if unset)
///       "consensus":    "2 signers"          // per-state override (omitted if unset)
///     }, …
///   ]
/// }
/// ```
/// Optional fields are omitted when unset so the prompt scales linearly with
/// active-flow count and each state's cost stays proportional to what the flow
/// actually declares. `requires` is included even when empty as `""` — a
/// zero-length string is the explicit "no evidence guard, LLM decides on hint
/// alone" signal (distinct from the field being absent, which shouldn't happen
/// for a well-formed FlowContext).
fn render_active_flow_for_prompt(fc: &FlowContext) -> serde_json::Value {
    // Tokens for this specific flow instance — substitute `$flow.base` /
    // `$flow.instance` in consensus-rule role-gates (J#4). Note:
    // `requires_human_readable` on each next-state has already been
    // substituted by `summarize_next_state` when the FlowContext was
    // built, so we don't touch it again here.
    let tokens = FlowTokens::from_context(fc);
    let mut obj = serde_json::Map::new();
    obj.insert("instance".into(), serde_json::json!(fc.instance_uri));
    obj.insert("subject".into(), serde_json::json!(fc.subject));
    obj.insert("flow".into(), serde_json::json!(fc.flow_name));
    obj.insert("currentState".into(), serde_json::json!(fc.current_state));
    if let Some(hint) = fc.flow_interpretation_hint.as_ref() {
        if !hint.is_empty() {
            obj.insert("hint".into(), serde_json::json!(hint));
        }
    }
    if let Some(rule) = fc.consensus_rule.as_ref() {
        obj.insert(
            "consensus".into(),
            serde_json::json!(render_consensus_rule(rule, &tokens)),
        );
    }
    let next: Vec<serde_json::Value> = fc
        .reachable_next_states
        .iter()
        .map(|ns| {
            let mut o = serde_json::Map::new();
            o.insert("name".into(), serde_json::json!(ns.name));
            if let Some(h) = ns.interpretation_hint.as_ref() {
                if !h.is_empty() {
                    o.insert("hint".into(), serde_json::json!(h));
                }
            }
            o.insert(
                "requires".into(),
                serde_json::json!(ns.requires_human_readable),
            );
            if let Some(sc) = ns.semantic_check.as_ref() {
                if !sc.is_empty() {
                    o.insert("semanticCheck".into(), serde_json::json!(sc));
                }
            }
            if let Some(rule) = ns.consensus_rule.as_ref() {
                o.insert(
                    "consensus".into(),
                    serde_json::json!(render_consensus_rule(rule, &tokens)),
                );
            }
            serde_json::Value::Object(o)
        })
        .collect();
    obj.insert("nextStates".into(), serde_json::Value::Array(next));
    serde_json::Value::Object(obj)
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
  - `transcript`: an array of turns `{speaker, text}` and, when known,
    `timestamp` (the source link's RFC3339 time).
  - `active_flows` (OPTIONAL — present only when flows are running on this
    scope): an array of live `FlowInstance` summaries. Each entry has an
    `instance` URI, the `subject` base expression it rides on, the `flow`
    name, the `currentState`, an optional flow-level `hint`, an optional
    default `consensus` rule, and a `nextStates` array. Every `nextStates`
    entry carries a `name`, an optional `hint` (when to advance to this
    state), a `requires` field (English rendering of the evidence guard —
    the empty string means \"no structural guard, decide on hint alone\"),
    an optional `semanticCheck` (extra 2nd-pass hint), and an optional
    per-state `consensus` override. Read this section BEFORE deciding
    what to extract: when the transcript matches a `nextStates` entry's
    `hint`, prefer extracting instances that will satisfy its `requires`
    guard so the flow can advance. When `active_flows` is absent, extract
    freely on the class hints alone.

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
            &[TranscriptTurn::from_speaker_text(
                "Nico",
                "I'll extract the LLM processing into ADAM",
            )],
            &no_existing(),
            &[],
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
    fn transcript_timestamp_lands_in_prompt_when_set() {
        let shapes = vec![shape_from_sdna("Intention", INTENTION_SDNA)];
        let mut turn = TranscriptTurn::from_speaker_text("Nico", "I'll ship it");
        turn.timestamp = "2026-08-13T12:00:00.000Z".into();
        let input = build_interpretation_input(&shapes, &[turn], &no_existing(), &[]);
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        assert_eq!(v["transcript"][0]["timestamp"], "2026-08-13T12:00:00.000Z");
        let without = build_interpretation_input(
            &shapes,
            &[TranscriptTurn::from_speaker_text("Nico", "I'll ship it")],
            &no_existing(),
            &[],
        );
        let v2: serde_json::Value = serde_json::from_str(&without).unwrap();
        assert!(
            v2["transcript"][0].get("timestamp").is_none(),
            "empty timestamp must be omitted from the prompt"
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
            &[TranscriptTurn::from_speaker_text(
                "Nico",
                "About that design doc…",
            )],
            &existing,
            &[],
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
            &[TranscriptTurn::from_speaker_text(
                "Ana",
                "Switching topics — Q3 retro planning.",
            )],
            &existing,
            &[],
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

    // ========================================================================
    // Slice 10.2 — active_flows section of the interpretation prompt
    // ========================================================================
    //
    // The prompt-side payoff: an extraction pass over a scope with running
    // FlowInstances now sees them (current state + reachable next-states +
    // requires-in-English + optional semanticCheck + consensus rendering) so
    // the LLM can preferentially extract instances that advance the flow.
    // Passes with no active flows must be byte-identical to the pre-slice-10.2
    // prompt (guarded below).

    use crate::perspectives::flow_context::{FlowContext, NextStateSummary};
    use crate::perspectives::shacl_parser::ConsensusRule;

    fn sample_delivery_context() -> FlowContext {
        FlowContext {
            flow_name: "Delivery".to_string(),
            instance_uri: "ad4m://flow/instance/abc".to_string(),
            subject: "ad4m://tasks/onboard-users".to_string(),
            current_state: "doing".to_string(),
            flow_interpretation_hint: Some("How this task moves from ready to done".to_string()),
            reachable_next_states: vec![NextStateSummary {
                name: "review".to_string(),
                interpretation_hint: Some("Someone volunteers to review the work".to_string()),
                requires_human_readable:
                    "at least 1 match of Review where target = \"the current task\"".to_string(),
                semantic_check: Some(
                    "The reviewer explicitly claims the work is testable".to_string(),
                ),
                consensus_rule: Some(ConsensusRule {
                    n: 2,
                    from_role: None,
                }),
            }],
            consensus_rule: Some(ConsensusRule {
                n: 1,
                from_role: None,
            }),
        }
    }

    #[test]
    fn active_flows_absent_when_empty_and_prompt_shape_matches_pre_slice_10_2() {
        // Passes with no live flows spend zero tokens on flow scaffolding AND
        // never show the LLM an `active_flows` key at all. Guards two failure
        // modes: (a) prompt bloat on the common path, (b) an empty `[]` value
        // that could confuse small models into inventing flow scaffolding.
        let shapes = vec![shape_from_sdna("Belief", BELIEF_SDNA)];
        let input = build_interpretation_input(
            &shapes,
            &[TranscriptTurn::from_speaker_text(
                "Ana",
                "The metric is down",
            )],
            &no_existing(),
            &[],
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        assert!(
            v.get("active_flows").is_none(),
            "empty active_flows must be omitted from the prompt entirely, got: {input}"
        );
        // Also proves nothing else in the JSON shape moved — same keys, same order-agnostic set.
        let keys: Vec<&str> = v.as_object().unwrap().keys().map(|s| s.as_str()).collect();
        assert!(
            keys.len() == 2 && keys.contains(&"classes") && keys.contains(&"transcript"),
            "pre-slice-10.2 keys must be preserved when active_flows is empty; got {keys:?}"
        );
    }

    #[test]
    fn active_flows_renders_full_shape_when_non_empty() {
        // One live FlowInstance → one `active_flows[0]` entry carrying: instance
        // URI, subject, flow name, current state, flow-level hint, flow-level
        // consensus rule (English), and one `nextStates` entry with every
        // optional field populated (hint, requires-in-English, semanticCheck,
        // per-state consensus override).
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        let flows = [sample_delivery_context()];
        let input = build_interpretation_input(
            &shapes,
            &[TranscriptTurn::from_speaker_text(
                "Ana",
                "I'll review the onboarding task now",
            )],
            &no_existing(),
            &flows,
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        let arr = v["active_flows"]
            .as_array()
            .expect("active_flows must render as an array when non-empty");
        assert_eq!(arr.len(), 1);
        let fc = &arr[0];
        assert_eq!(fc["instance"], "ad4m://flow/instance/abc");
        assert_eq!(fc["subject"], "ad4m://tasks/onboard-users");
        assert_eq!(fc["flow"], "Delivery");
        assert_eq!(fc["currentState"], "doing");
        assert_eq!(fc["hint"], "How this task moves from ready to done");
        assert_eq!(fc["consensus"], "1 signer");

        let ns = fc["nextStates"]
            .as_array()
            .expect("nextStates must render as an array");
        assert_eq!(ns.len(), 1);
        let review = &ns[0];
        assert_eq!(review["name"], "review");
        assert_eq!(review["hint"], "Someone volunteers to review the work");
        assert_eq!(
            review["requires"],
            "at least 1 match of Review where target = \"the current task\""
        );
        assert_eq!(
            review["semanticCheck"],
            "The reviewer explicitly claims the work is testable"
        );
        assert_eq!(review["consensus"], "2 signers");
    }

    #[test]
    fn active_flows_omits_optional_fields_when_unset() {
        // A minimal FlowContext (no flow-level hint, no consensus, one bare
        // next-state with no hint/semanticCheck/consensus override) must NOT
        // render empty keys — every optional field is omitted when unset so
        // the prompt scales linearly with declared content. `requires` is the
        // ONE exception: the empty string is explicitly rendered as a signal
        // that the state has no structural guard (LLM decides on hint alone).
        let shapes = vec![shape_from_sdna("Belief", BELIEF_SDNA)];
        let bare = FlowContext {
            flow_name: "Like".to_string(),
            instance_uri: "ad4m://flow/instance/xyz".to_string(),
            subject: "ad4m://post/1".to_string(),
            current_state: "initial".to_string(),
            flow_interpretation_hint: None,
            reachable_next_states: vec![NextStateSummary {
                name: "liked".to_string(),
                interpretation_hint: None,
                requires_human_readable: String::new(),
                semantic_check: None,
                consensus_rule: None,
            }],
            consensus_rule: None,
        };
        let input = build_interpretation_input(
            &shapes,
            &[TranscriptTurn::from_speaker_text("Ana", "I like this")],
            &no_existing(),
            &[bare],
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        let fc = &v["active_flows"][0];
        assert!(
            fc.get("hint").is_none(),
            "flow-level hint must be omitted when unset; got {fc:?}"
        );
        assert!(
            fc.get("consensus").is_none(),
            "flow-level consensus must be omitted when unset; got {fc:?}"
        );
        let ns = &fc["nextStates"][0];
        assert!(
            ns.get("hint").is_none(),
            "per-state hint must be omitted when unset; got {ns:?}"
        );
        assert!(
            ns.get("semanticCheck").is_none(),
            "per-state semanticCheck must be omitted when unset; got {ns:?}"
        );
        assert!(
            ns.get("consensus").is_none(),
            "per-state consensus must be omitted when unset; got {ns:?}"
        );
        // `requires` is the deliberate exception — rendered as "" so an empty
        // guard is legible to the LLM as "no structural guard" (distinct from
        // the field being absent, which would break the prompt contract).
        assert_eq!(
            ns["requires"], "",
            "empty requires must render as the empty string, not be omitted"
        );
    }

    #[test]
    fn system_prompt_documents_active_flows_section() {
        // Regression guard: the system prompt must tell the LLM (a) that the
        // section is optional, (b) how to read `nextStates`, (c) that
        // `requires` is English, and (d) that seeing this section should
        // *bias* extraction toward advancing the flow — otherwise the whole
        // slice 10.2 payload is on the LLM's blindside.
        let p = INTERPRETATION_SYSTEM_PROMPT;
        assert!(
            p.contains("active_flows"),
            "system prompt must introduce the active_flows section"
        );
        assert!(
            p.contains("OPTIONAL"),
            "system prompt must state that active_flows is optional (absent when no flows run)"
        );
        assert!(
            p.contains("nextStates") && p.contains("requires"),
            "system prompt must describe the nextStates + requires shape"
        );
        assert!(
            p.contains("advance"),
            "system prompt must instruct the LLM to bias toward extractions that advance the flow"
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

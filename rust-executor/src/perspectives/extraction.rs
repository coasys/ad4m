//! Generic LLM extraction: turn conversation text into typed subject-class
//! instances, steered by the natural-language `extraction_hint` declared on each
//! class/property (see `shacl_parser` S0 + `model_query` S1).
//!
//! Build sequence (planning/generic-extraction-spec.md §9):
//!   S2 — `parse_extraction_response`: LLM JSON -> `ProposedInstance`s
//!   S3 — `build_extraction_input`   : shapes' hints + transcript -> prompt
//!   S4 — `instance_links`           : `ProposedInstance` -> perspective links
//!   S5 — `EXTRACTION_SYSTEM_PROMPT` + `ensure_extraction_task`
//!   S6 — `apply_extraction_raw` + `retry_extraction_parse` + `run_extraction`
//!          async shell + retry harness, wiring S1-S5 through
//!          `AIService::prompt` and `PerspectiveInstance::add_links`, plus
//!          existing-instance dedup (`existing_instance_titles` +
//!          `filter_already_present`).
//!
//! The pure/DB-only units here (S2-S6, no LLM) are unit-tested in-file. The
//! real-LLM end-to-end suite lives in `extraction_e2e.rs`, and the shared test
//! fixtures/harness in `extraction_test_support.rs`.

use crate::agent::AgentContext;
use crate::db::Ad4mDb;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{AIPromptExamples, AITask, Link, LinkStatus};
use serde::Deserialize;
use std::collections::HashMap;
use uuid::Uuid;

/// One instance the LLM proposes creating: the target class name plus a flat
/// map of field-name -> value. Extra/unknown fields are tolerated (kept in
/// `props`); `instance_links` (S4) filters them against the class shape.
#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct ProposedInstance {
    pub class: String,
    #[serde(flatten)]
    pub props: HashMap<String, serde_json::Value>,
}

/// Parse a raw LLM response into proposed instances.
///
/// Local models wrap JSON in reasoning/markdown noise, so we first strip the
/// common wrappers (mirrors Flux `LLMutils.ts`): `<think>…</think>` blocks,
/// ```-fences, and trailing commas. Then parse as a JSON array.
pub fn parse_extraction_response(raw: &str) -> anyhow::Result<Vec<ProposedInstance>> {
    let cleaned = clean_llm_json(raw);
    let instances: Vec<ProposedInstance> = serde_json::from_str(&cleaned).map_err(|e| {
        anyhow::anyhow!("extraction JSON parse failed: {e}; cleaned payload: {cleaned}")
    })?;
    Ok(instances)
}

/// Strip the reasoning/markdown noise local models add around JSON.
fn clean_llm_json(raw: &str) -> String {
    // 1. Drop <think>…</think> reasoning blocks (non-greedy, across newlines).
    let think = regex::Regex::new(r"(?s)<think>.*?</think>").unwrap();
    let s = think.replace_all(raw, "");

    // 2. Drop code fences ```json / ``` (keep the inner content).
    let fence = regex::Regex::new(r"```[a-zA-Z0-9]*").unwrap();
    let s = fence.replace_all(&s, "");

    // 3. Remove trailing commas before a closing } or ] (invalid JSON, common).
    let trailing = regex::Regex::new(r",(\s*[}\]])").unwrap();
    let s = trailing.replace_all(&s, "$1");

    s.trim().to_string()
}

/// S3: assemble the per-call LLM input from the target shapes' extraction hints
/// plus the transcript. Pure — this is exactly where `extraction_hint` enters
/// the prompt. Shape (matches the system prompt in S5):
/// `{ "classes": [{ "name", "hint", "existing": [title,…],
///                  "fields": [{ "name", "required", "hint" }] }],
///    "transcript": [{ "speaker", "text" }] }`.
///
/// `existing` maps a class's local name to the titles of instances already in
/// the graph, so the model can avoid re-proposing them (soft dedup; the hard
/// guarantee is [`filter_already_present`]). Pass an empty map for none.
pub fn build_extraction_input(
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    existing: &HashMap<String, Vec<String>>,
) -> String {
    let classes: Vec<serde_json::Value> = shapes
        .iter()
        .map(|s| {
            let rel_preds = relation_predicates(s);
            let fields: Vec<serde_json::Value> = s
                .properties
                .iter()
                // The type flag is set by instance_links (S4), not the LLM;
                // relations are link-typed and handled in a later PR.
                .filter(|p| !p.is_flag && !rel_preds.contains(p.predicate.as_str()))
                .map(|p| {
                    serde_json::json!({
                        "name": p.name,
                        "required": p.is_required,
                        "hint": p.extraction_hint,
                    })
                })
                .collect();
            let name = class_local_name(&s.target_class);
            serde_json::json!({
                "name": name,
                "hint": s.extraction_hint,
                "existing": existing.get(name).cloned().unwrap_or_default(),
                "fields": fields,
            })
        })
        .collect();
    let turns: Vec<serde_json::Value> = transcript
        .iter()
        .map(|(speaker, text)| serde_json::json!({ "speaker": speaker, "text": text }))
        .collect();
    serde_json::json!({ "classes": classes, "transcript": turns }).to_string()
}

/// Deterministic dedup safety-net (pure): drop proposed instances whose
/// (class, title) already exists in the graph, case-insensitively. This is the
/// hard guarantee behind the soft `existing` hint in [`build_extraction_input`]
/// — even if the model re-proposes a known item, it never becomes a link.
///
/// `existing` maps a class's local name to the titles already present. Only the
/// `title` field is compared (the human-facing identity of an SoA node);
/// instances without a `title` are always kept.
pub fn filter_already_present(
    instances: Vec<ProposedInstance>,
    existing: &HashMap<String, Vec<String>>,
) -> Vec<ProposedInstance> {
    let known: HashMap<&String, std::collections::HashSet<String>> = existing
        .iter()
        .map(|(class, titles)| (class, titles.iter().map(|t| t.to_lowercase()).collect()))
        .collect();
    instances
        .into_iter()
        .filter(|inst| {
            let Some(title) = inst.props.get("title").and_then(|v| v.as_str()) else {
                return true; // no title to compare on — keep it
            };
            let already = known
                .get(&inst.class)
                .map(|set| set.contains(&title.to_lowercase()))
                .unwrap_or(false);
            if already {
                log::debug!(
                    "extraction: dropping already-present {} '{}'",
                    inst.class,
                    title
                );
            }
            !already
        })
        .collect()
}

/// Predicates of the shape's relation (link-typed) properties. `load_shape`
/// lists every relation both in `properties` (so the query pipeline sees its
/// predicate) *and* in `include_relations`; we key off the latter to recognise
/// them. Relations are excluded from generic scalar extraction: their targets
/// are instance URIs, not literals, so we neither offer them to the LLM nor
/// write LLM-proposed values through `value_to_literal_uri` (which would encode
/// e.g. an array as a bogus `literal:json:` URI). Relation extraction is a
/// later PR.
fn relation_predicates(shape: &ModelShape) -> std::collections::HashSet<&str> {
    shape
        .include_relations
        .iter()
        .map(|r| r.predicate.as_str())
        .collect()
}

/// Local class name from a class URI: `ns://Intention` -> `Intention`.
pub(crate) fn class_local_name(target_class: &str) -> &str {
    target_class
        .rsplit(|c| c == '/' || c == ':')
        .find(|seg| !seg.is_empty())
        .unwrap_or(target_class)
}

/// S4: turn a `ProposedInstance` (parsed LLM output) into perspective links
/// anchored at `base`. Pure — no store, no LLM. Emits, in shape order:
///   1. one link per type-flag property (predicate = flag path, target = the
///      flag's constant `initial_value`), so downstream queries recognise the
///      class;
///   2. one link per non-flag shape property that appears in `inst.props`
///      (predicate = property path, target = literal-encoded value).
///
/// Unknown/extra fields in `inst.props` are dropped — the LLM cannot inject
/// links outside the declared class shape.
pub fn instance_links(shape: &ModelShape, inst: &ProposedInstance, base: &str) -> Vec<Link> {
    let mut out = Vec::new();
    let rel_preds = relation_predicates(shape);
    for prop in &shape.properties {
        if prop.is_flag {
            if let Some(target) = prop.initial_value.as_ref() {
                out.push(Link {
                    source: base.to_string(),
                    predicate: Some(prop.predicate.clone()),
                    target: target.clone(),
                });
            }
            continue;
        }
        // Skip relation properties: their targets are instance URIs, not
        // literals. Writing an LLM-proposed value here would mint a bogus
        // literal link. Relation extraction is a later PR.
        if rel_preds.contains(prop.predicate.as_str()) {
            continue;
        }
        if let Some(value) = inst.props.get(&prop.name) {
            if let Some(target) = value_to_literal_uri(value) {
                out.push(Link {
                    source: base.to_string(),
                    predicate: Some(prop.predicate.clone()),
                    target,
                });
            }
        }
    }
    out
}

/// Encode a JSON scalar into an AD4M `literal:` URI (matches the encoding used
/// by `languages/literal.rs`, mirrored in `model_query::utils`). Skips `null`.
fn value_to_literal_uri(value: &serde_json::Value) -> Option<String> {
    use percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC};
    match value {
        serde_json::Value::Null => None,
        serde_json::Value::String(s) => Some(format!(
            "literal:string:{}",
            utf8_percent_encode(s, NON_ALPHANUMERIC)
        )),
        serde_json::Value::Number(n) => Some(format!("literal:number:{n}")),
        serde_json::Value::Bool(b) => Some(format!("literal:boolean:{b}")),
        other => Some(format!(
            "literal:json:{}",
            utf8_percent_encode(&other.to_string(), NON_ALPHANUMERIC)
        )),
    }
}

/// S5: name under which the generic extraction task is registered with
/// `AIService`. Kept stable so `ensure_extraction_task` can find it across
/// executor restarts and multiple callers.
pub const EXTRACTION_TASK_NAME: &str = "adam://extraction";

/// S5: system prompt sent with every extraction call. Instance-specific
/// scaffolding (available classes, their hints, the transcript) is added by
/// `build_extraction_input` (S3), so this stays stable across calls and the
/// task can be reused.
pub const EXTRACTION_SYSTEM_PROMPT: &str = "\
You extract typed instances from a conversation transcript.

You receive a JSON object with these fields:
  - `classes`: available subject classes. Each has a `name`, a natural-language
    `hint` describing when to instantiate it, a list of `fields` (each with a
    `name`, optional `hint`, and `required` flag), and an `existing` array of
    titles already present in the graph for that class.
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

Two worked examples follow (as prior turns) before your real input — study how
every co-present item is captured, then apply the same to your input.

Output rules:
  - Return valid JSON only — no prose, no markdown fences, no <think> blocks.
  - Return an empty array `[]` if nothing matches.
  - Do not invent classes or fields not listed in `classes`.
  - Dedup: skip an item ONLY when its title clearly matches one already in that
    class's `existing` list. A brand-new item still counts even if an older,
    different item of the same class exists — always extract genuinely new items.
";

/// S5: idempotently register the generic extraction task in the AI-task DB.
///
/// If a task with `EXTRACTION_TASK_NAME` already exists, returns it unchanged
/// (so callers can safely invoke this on every executor startup or before every
/// extraction run). Otherwise inserts a new row bound to the `\"default\"` LLM
/// model — `AIService::replace_model_variables` resolves this to whatever LLM
/// the user has configured as default at prompt time, so extraction works with
/// any model without hard-coding one here.
///
/// DB-only: does not touch the running `AIService`. The runtime path (S6) is
/// expected to call `service.spawn_task(task)` separately when it needs the
/// model loaded for a `prompt` call; this split keeps registration testable in
/// CI without a GPU.
/// Few-shot examples sent as prior User/Assistant turns (via `prompt_examples`)
/// ahead of the real input. Two generic, non-test scenarios that teach the
/// failure modes small models hit: (1) a belief and a task in the same snippet
/// must BOTH be captured; (2) a question raised amid tasks must be captured.
/// Inputs mirror the JSON shape `build_extraction_input` produces.
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

// -----------------------------------------------------------------------------
// S6: async shell + retry harness
// -----------------------------------------------------------------------------

/// Max attempts for [`retry_extraction_parse`]. Mirrors Flux's `LLMutils`
/// retry-×5 loop: local models occasionally emit half-valid JSON, so we ask
/// again a few times before giving up on the whole call.
pub const EXTRACTION_MAX_ATTEMPTS: u8 = 5;

/// S6 (pure): parse a raw LLM response and turn it into the set of links that
/// would be written into the perspective. Callers minted a fresh instance base
/// URI per proposed instance under `base_prefix` and delegate to
/// [`instance_links`] (S4) for the actual shape-driven link construction.
///
/// The lookup from `inst.class` to a `ModelShape` is by local class name
/// (final segment of `target_class`). Proposed instances whose class doesn't
/// match any provided shape are silently dropped — the LLM cannot inject
/// links outside the caller's declared shape set.
///
/// Returned tuples pair each fresh base URI with the links anchored on it, so
/// the caller ([`run_extraction`] or a test) can decide how to persist them.
pub fn apply_extraction_raw(
    shapes: &[ModelShape],
    raw: &str,
    base_prefix: &str,
) -> anyhow::Result<Vec<(String, Vec<Link>)>> {
    let proposed = parse_extraction_response(raw)?;
    Ok(place_instances(shapes, &proposed, base_prefix))
}

/// Core of [`apply_extraction_raw`], factored out so [`run_extraction`] can
/// reuse it without a redundant JSON round-trip. Same semantics: unknown-class
/// instances are dropped; every kept instance gets a fresh UUID-tagged base.
pub fn place_instances(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
) -> Vec<(String, Vec<Link>)> {
    let mut out = Vec::with_capacity(proposed.len());
    for inst in proposed {
        let Some(shape) = shapes
            .iter()
            .find(|s| class_local_name(&s.target_class) == inst.class)
        else {
            log::debug!(
                "extraction: dropping proposed instance for unknown class '{}'",
                inst.class
            );
            continue;
        };
        let base = format!(
            "{base_prefix}{}/{}",
            inst.class.to_lowercase(),
            Uuid::new_v4()
        );
        let links = instance_links(shape, inst, &base);
        out.push((base, links));
    }
    out
}

/// S6: run `prompt_fn` up to [`EXTRACTION_MAX_ATTEMPTS`] times, parsing each
/// response as an extraction JSON payload. Returns the first successful parse;
/// the last parse error propagates if every attempt fails. `prompt_fn` is an
/// async closure so callers can inject anything (real `AIService`, a canned
/// script, a mock) without a live LLM.
///
/// This is deliberately a thin generic wrapper: it never mutates state, and it
/// is the only place we tolerate LLM flake. Any bug in prompt assembly should
/// fail deterministically in [`build_extraction_input`], not here.
pub async fn retry_extraction_parse<F, Fut>(
    mut prompt_fn: F,
) -> anyhow::Result<Vec<ProposedInstance>>
where
    F: FnMut(u8) -> Fut,
    Fut: std::future::Future<Output = anyhow::Result<String>>,
{
    let mut last_err: Option<anyhow::Error> = None;
    for attempt in 1..=EXTRACTION_MAX_ATTEMPTS {
        let raw = match prompt_fn(attempt).await {
            Ok(r) => r,
            Err(e) => {
                log::warn!("extraction: prompt attempt {attempt} failed: {e:#}");
                last_err = Some(e);
                continue;
            }
        };
        match parse_extraction_response(&raw) {
            Ok(instances) => return Ok(instances),
            Err(e) => {
                log::warn!(
                    "extraction: parse attempt {attempt} failed: {e:#}; will retry (max {EXTRACTION_MAX_ATTEMPTS})"
                );
                last_err = Some(e);
            }
        }
    }
    Err(last_err.unwrap_or_else(|| {
        anyhow::anyhow!(
            "extraction: failed after {EXTRACTION_MAX_ATTEMPTS} attempts with no captured error"
        )
    }))
}

/// S6: minimal transcript gatherer. Reads links `source ⇒ predicate ⇒ literal`
/// from a perspective where `predicate` matches `message_predicate` and the
/// target is a `literal:string:` URI (i.e., a message body). Returns turns in
/// the order the store returned them. Speaker is the link author.
///
/// Kept intentionally small — flows/channel-aware traversal is deferred to a
/// later PR. Callers that already have a curated `Vec<(speaker, text)>` should
/// pass it straight to [`run_extraction`] and skip this helper.
pub async fn gather_transcript(
    perspective: &PerspectiveInstance,
    source: &str,
    message_predicate: &str,
) -> anyhow::Result<Vec<(String, String)>> {
    use crate::types::LinkQuery;
    let query = LinkQuery {
        source: Some(source.to_string()),
        predicate: Some(message_predicate.to_string()),
        ..Default::default()
    };
    let links = perspective
        .get_links(&query)
        .await
        .map_err(|e| anyhow::anyhow!("gather_transcript: get_links failed: {e:#}"))?;
    let mut out = Vec::with_capacity(links.len());
    for l in links {
        if let Some(body) = decode_literal_string(&l.data.target) {
            out.push((l.author, body));
        }
    }
    Ok(out)
}

fn decode_literal_string(uri: &str) -> Option<String> {
    let rest = uri.strip_prefix("literal:string:")?;
    percent_encoding::percent_decode_str(rest)
        .decode_utf8()
        .ok()
        .map(|c| c.into_owned())
}

/// S6: read the titles of instances already present in the perspective for each
/// target class, keyed by the class's local name. Used to steer the LLM away
/// from re-proposing known items ([`build_extraction_input`]) and to enforce
/// dedup deterministically ([`filter_already_present`]).
///
/// An instance is located by its class type-flag link (predicate + constant
/// value); its identity is the `title` property. Classes without a type flag or
/// a `title` property are skipped (no dedup key).
pub async fn existing_instance_titles(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> anyhow::Result<HashMap<String, Vec<String>>> {
    use crate::types::LinkQuery;
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    for shape in shapes {
        let Some(flag) = shape
            .properties
            .iter()
            .find(|p| p.is_flag && p.initial_value.is_some())
        else {
            continue;
        };
        let Some(title_prop) = shape.properties.iter().find(|p| p.name == "title") else {
            continue;
        };
        let flag_value = flag.initial_value.as_ref().unwrap();

        // All instances of this class = sources of the type-flag link.
        let flag_links = perspective
            .get_links(&LinkQuery {
                predicate: Some(flag.predicate.clone()),
                ..Default::default()
            })
            .await
            .map_err(|e| {
                anyhow::anyhow!("existing_instance_titles: get_links(flag) failed: {e:#}")
            })?;
        let bases: Vec<String> = flag_links
            .into_iter()
            .filter(|l| &l.data.target == flag_value)
            .map(|l| l.data.source)
            .collect();

        let mut titles = Vec::new();
        for base in bases {
            let title_links = perspective
                .get_links(&LinkQuery {
                    source: Some(base),
                    predicate: Some(title_prop.predicate.clone()),
                    ..Default::default()
                })
                .await
                .map_err(|e| {
                    anyhow::anyhow!("existing_instance_titles: get_links(title) failed: {e:#}")
                })?;
            for tl in title_links {
                if let Some(title) = decode_literal_string(&tl.data.target) {
                    titles.push(title);
                }
            }
        }
        if !titles.is_empty() {
            out.insert(class_local_name(&shape.target_class).to_string(), titles);
        }
    }
    Ok(out)
}

/// S6: end-to-end extraction driver. Wires everything: build the input from
/// shapes' hints + transcript (S3), call `AIService::prompt` on the registered
/// extraction task (S5), retry parsing up to 5× (S6), then for every proposed
/// instance write its shape-driven links (S4) into the perspective via
/// `add_link`. Returns the fresh base URI + links written per instance.
///
/// The `shapes` argument is exactly the classes to consider — callers pick
/// which subject classes to extract into (usually all classes carrying an
/// `extraction_hint`). `base_prefix` is the URI namespace under which new
/// instance identities are minted, e.g. `"soa://ext/"`.
///
/// `link_status` is the caller's choice of [`LinkStatus`] for the written
/// links. Pass [`LinkStatus::Local`] (the usual default) so LLM-generated
/// links on shared/neighbourhood perspectives are not auto-published; pass
/// [`LinkStatus::Shared`] only when the extraction is meant to sync.
pub async fn run_extraction(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    base_prefix: &str,
    link_status: LinkStatus,
    context: &AgentContext,
) -> anyhow::Result<Vec<(String, Vec<Link>)>> {
    let task = ensure_extraction_task()?;
    // Dedup context: what the graph already holds, so the model is steered away
    // from re-proposing known items and we can enforce it deterministically.
    let existing = existing_instance_titles(perspective, shapes).await?;
    let prompt = build_extraction_input(shapes, transcript, &existing);

    let service = crate::ai_service::AIService::global_instance()
        .await
        .map_err(|e| anyhow::anyhow!("run_extraction: AIService not ready: {e:#}"))?;

    let instances = retry_extraction_parse(|_attempt| {
        let service = service.clone();
        let task_id = task.task_id.clone();
        let prompt = prompt.clone();
        async move {
            let result = service
                .prompt(task_id, prompt)
                .await
                .map_err(|e| anyhow::anyhow!("AIService::prompt failed: {e:#}"))?;
            Ok(result.text)
        }
    })
    .await?;

    // Hard dedup guarantee: even if the model ignored the `existing` hint, an
    // already-present (class, title) never becomes a new instance.
    let instances = filter_already_present(instances, &existing);
    let placements = place_instances(shapes, &instances, base_prefix);

    // Write all instance links in a single PerspectiveDiff (add_links) so a
    // mid-write failure can't leave a half-formed instance — e.g. one carrying
    // its `ns://type` flag but missing its `ns://title`. Status is the caller's
    // choice (see `link_status`).
    let all_links: Vec<Link> = placements
        .iter()
        .flat_map(|(_base, links)| links.iter().cloned())
        .collect();
    if !all_links.is_empty() {
        perspective
            .add_links(all_links, link_status, None, context)
            .await
            .map_err(|e| anyhow::anyhow!("run_extraction: add_links failed: {e:#}"))?;
    }
    Ok(placements)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::extraction_test_support::*;

    #[test]
    fn extraction_hint_lands_in_prompt() {
        let shapes = vec![
            shape_from_sdna("Belief", BELIEF_SDNA),
            shape_from_sdna("Intention", INTENTION_SDNA),
        ];
        let input = build_extraction_input(
            &shapes,
            &[(
                "Nico".into(),
                "I'll extract the LLM processing into ADAM".into(),
            )],
            &HashMap::new(),
        );

        // class-level hints reach the prompt
        assert!(input.contains("A claim a participant holds to be true"));
        assert!(input.contains("actionable outcome with a plausible owner"));
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
    }

    fn titles(instances: &[ProposedInstance]) -> Vec<&str> {
        instances
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect()
    }

    #[test]
    fn parses_clean_json_array() {
        let raw = r#"[
          {"class":"Intention","title":"Extract LLM processing from Flux into ADAM","owner":"Nico"},
          {"class":"Belief","title":"Graph viz is the hardest part"}
        ]"#;
        let out = parse_extraction_response(raw).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].class, "Intention");
        assert_eq!(out[0].props.get("owner").unwrap().as_str(), Some("Nico"));
        assert_eq!(
            titles(&out),
            vec![
                "Extract LLM processing from Flux into ADAM",
                "Graph viz is the hardest part"
            ]
        );
    }

    #[test]
    fn strips_code_fences() {
        let raw = "```json\n[{\"class\":\"Belief\",\"title\":\"X\"}]\n```";
        let out = parse_extraction_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].class, "Belief");
    }

    #[test]
    fn strips_think_block() {
        let raw =
            "<think>Let me find the intentions...</think>\n[{\"class\":\"Intention\",\"title\":\"Do X\"}]";
        let out = parse_extraction_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].class, "Intention");
    }

    #[test]
    fn tolerates_trailing_commas() {
        let raw = r#"[
          {"class":"Task","title":"A",},
          {"class":"Task","title":"B"},
        ]"#;
        let out = parse_extraction_response(raw).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(titles(&out), vec!["A", "B"]);
    }

    #[test]
    fn empty_array_yields_no_instances() {
        assert!(parse_extraction_response("[]").unwrap().is_empty());
        // and empty inside a fence / with whitespace
        assert!(parse_extraction_response("```json\n[]\n```")
            .unwrap()
            .is_empty());
    }

    #[test]
    fn garbage_is_an_error_not_a_panic() {
        assert!(parse_extraction_response("not json at all").is_err());
    }

    // ---- S4: instance_links ---------------------------------------------

    fn find_shape<'a>(shapes: &'a [ModelShape], class_uri: &str) -> &'a ModelShape {
        shapes
            .iter()
            .find(|s| s.target_class == class_uri)
            .unwrap_or_else(|| panic!("shape not found: {class_uri}"))
    }

    #[test]
    fn instance_links_emit_type_flag_and_scalar_fields() {
        let shapes = vec![
            shape_from_sdna("Belief", BELIEF_SDNA),
            shape_from_sdna("Intention", INTENTION_SDNA),
        ];
        let raw = r#"[
          {"class":"Intention","title":"Extract LLM processing","owner":"Nico"},
          {"class":"Belief","title":"This will work"}
        ]"#;
        let proposed = parse_extraction_response(raw).unwrap();

        let intent_links = instance_links(
            find_shape(&shapes, "ns://Intention"),
            &proposed[0],
            "soa://i1",
        );
        // Type flag: predicate = the flag's path, target = the flag's constant value.
        assert!(
            intent_links.iter().any(
                |l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://intention"
            ),
            "expected intention type flag; got {intent_links:#?}"
        );
        // Owner (literal-string) landed as a link at the correct predicate.
        assert!(intent_links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://owner")
                && l.target == "literal:string:Nico"));
        // Title (literal-string, percent-encoded space).
        assert!(intent_links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://title")
                && l.target == "literal:string:Extract%20LLM%20processing"));
        // Every emitted link is anchored at the given base.
        assert!(intent_links.iter().all(|l| l.source == "soa://i1"));

        let belief_links =
            instance_links(find_shape(&shapes, "ns://Belief"), &proposed[1], "soa://b1");
        // Belief has no `owner` field → no owner link even though the JSON above
        // does not carry it either (defensive: shape drives what gets emitted).
        assert!(!belief_links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://owner")));
        // Belief's own type flag with its own constant value.
        assert!(belief_links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://belief"));
    }

    #[test]
    fn instance_links_drop_unknown_fields() {
        // The LLM hallucinates a `secret` field the shape doesn't declare.
        // instance_links must NOT emit a link for it (shape is the source of truth).
        let shape = shape_from_sdna("Belief", BELIEF_SDNA);
        let raw = r#"[{"class":"Belief","title":"X","secret":"leaked"}]"#;
        let proposed = parse_extraction_response(raw).unwrap();
        let links = instance_links(&shape, &proposed[0], "soa://b1");
        assert!(
            !links.iter().any(|l| l.target.contains("leaked")),
            "unknown field must not become a link; got {links:#?}"
        );
    }

    #[test]
    fn instance_links_skip_missing_optional_fields() {
        // Intention shape has an optional `owner`; instance omits it → no owner link.
        let shape = shape_from_sdna("Intention", INTENTION_SDNA);
        let raw = r#"[{"class":"Intention","title":"Ship it"}]"#;
        let proposed = parse_extraction_response(raw).unwrap();
        let links = instance_links(&shape, &proposed[0], "soa://i2");
        assert!(!links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://owner")));
        assert!(links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://title")));
    }

    #[test]
    fn relation_properties_are_excluded_from_extraction() {
        // A shape whose extraction hint also declares a link-typed relation
        // (`blocks`). load_shape lists that relation in `properties` too, so
        // without the guard it would be offered to the LLM and — if the LLM
        // emits it — written through value_to_literal_uri as a bogus literal.
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        // Sanity: the relation really is present in both lists.
        assert!(
            shape.include_relations.iter().any(|r| r.name == "blocks"),
            "fixture must declare a `blocks` relation"
        );
        assert!(
            shape.properties.iter().any(|p| p.name == "blocks"),
            "load_shape is expected to also list the relation in properties"
        );

        // 1. build_extraction_input must not offer the relation as a field.
        let input = build_extraction_input(
            &[shape.clone()],
            &[("Nico".into(), "block it".into())],
            &HashMap::new(),
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        let field_names: Vec<&str> = v["classes"][0]["fields"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|f| f["name"].as_str())
            .collect();
        assert!(field_names.contains(&"title"), "scalar field must remain");
        assert!(
            !field_names.contains(&"blocks"),
            "relation must not be offered to the LLM; got {field_names:?}"
        );

        // 2. instance_links must not write a link even if the LLM emits the
        //    relation (here as an array — exactly the bogus literal:json case).
        let raw = r#"[{"class":"Task","title":"Do X","blocks":["soa://t2","soa://t3"]}]"#;
        let proposed = parse_extraction_response(raw).unwrap();
        let links = instance_links(&shape, &proposed[0], "soa://t1");
        assert!(
            !links
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://blocks")),
            "relation must not become a link; got {links:#?}"
        );
        assert!(
            !links.iter().any(|l| l.target.starts_with("literal:json:")),
            "no bogus literal:json relation target; got {links:#?}"
        );
        // The scalar title still lands.
        assert!(links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://title")));
    }

    #[test]
    fn ensure_extraction_task_registers_and_is_idempotent() {
        ensure_db_init();

        // Guard: some other test may have inserted the row already; wipe just
        // our name so the first call below is a real insert. (Global DB is
        // shared across the single-threaded test run.)
        let existing: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .unwrap()
            .into_iter()
            .filter(|t| t.name == EXTRACTION_TASK_NAME)
            .collect();
        for t in existing {
            Ad4mDb::with_global_instance(|db| db.remove_task(t.task_id.clone())).unwrap();
        }

        let first = ensure_extraction_task().unwrap();
        assert_eq!(first.name, EXTRACTION_TASK_NAME);
        assert_eq!(first.model_id, "default");
        assert!(first.system_prompt.contains("You extract typed instances"));
        assert!(!first.task_id.is_empty());

        // Second call must find the same row, not insert a duplicate.
        let second = ensure_extraction_task().unwrap();
        assert_eq!(first.task_id, second.task_id);

        let rows: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .unwrap()
            .into_iter()
            .filter(|t| t.name == EXTRACTION_TASK_NAME)
            .collect();
        assert_eq!(rows.len(), 1, "expected exactly one extraction task row");
    }

    // ---- S6: apply_extraction_raw + retry_extraction_parse ---------------

    #[test]
    fn apply_extraction_raw_wires_parse_and_links() {
        // Hand-fed raw = what the LLM would return; no live model in the loop.
        // We verify the whole S3→S4 wiring: each proposed instance gets a fresh
        // base under our prefix, its links are shape-driven (type flag + fields
        // only), and multi-class output is split correctly.
        let shapes = vec![
            shape_from_sdna("Belief", BELIEF_SDNA),
            shape_from_sdna("Intention", INTENTION_SDNA),
        ];
        let raw = r#"[
          {"class":"Intention","title":"Extract LLM processing","owner":"Nico"},
          {"class":"Belief","title":"This will work"}
        ]"#;

        let placements = apply_extraction_raw(&shapes, raw, "soa://ext/").unwrap();
        assert_eq!(placements.len(), 2, "expected two placements");

        // Bases are unique, prefixed, and class-tagged (lowercased).
        let (b0, links0) = &placements[0];
        let (b1, links1) = &placements[1];
        assert_ne!(b0, b1, "each instance must get its own base URI");
        assert!(b0.starts_with("soa://ext/intention/"));
        assert!(b1.starts_with("soa://ext/belief/"));
        assert!(links0.iter().all(|l| &l.source == b0));
        assert!(links1.iter().all(|l| &l.source == b1));

        // Intention: type flag + title + owner reached the link set.
        assert!(links0
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://intention"));
        assert!(links0
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://title")
                && l.target == "literal:string:Extract%20LLM%20processing"));
        assert!(links0
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://owner")
                && l.target == "literal:string:Nico"));

        // Belief: type flag with its own constant, no owner predicate.
        assert!(links1
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://belief"));
        assert!(!links1
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://owner")));
    }

    #[test]
    fn apply_extraction_raw_drops_unknown_class() {
        // Only Belief is registered; the LLM hallucinates a Frob. It must be
        // silently dropped (defensive: shapes are the source of truth for which
        // classes can be instantiated).
        let shapes = vec![shape_from_sdna("Belief", BELIEF_SDNA)];
        let raw = r#"[
          {"class":"Belief","title":"A"},
          {"class":"Frob","title":"B"}
        ]"#;
        let placements = apply_extraction_raw(&shapes, raw, "soa://ext/").unwrap();
        assert_eq!(placements.len(), 1);
        assert!(placements[0].0.starts_with("soa://ext/belief/"));
    }

    #[test]
    fn apply_extraction_raw_empty_array_yields_no_placements() {
        let shapes = vec![shape_from_sdna("Belief", BELIEF_SDNA)];
        assert!(apply_extraction_raw(&shapes, "[]", "soa://ext/")
            .unwrap()
            .is_empty());
    }

    #[tokio::test]
    async fn retry_extraction_parse_succeeds_on_first_attempt() {
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let out = retry_extraction_parse(move |_| {
            let a = attempts_clone.clone();
            async move {
                a.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                Ok(r#"[{"class":"Belief","title":"X"}]"#.to_string())
            }
        })
        .await
        .unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(attempts.load(std::sync::atomic::Ordering::SeqCst), 1);
    }

    #[tokio::test]
    async fn retry_extraction_parse_recovers_after_bad_parse() {
        // First attempt returns unparseable garbage; second returns valid JSON.
        // retry_extraction_parse must call again and succeed within budget.
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let out = retry_extraction_parse(move |_| {
            let a = attempts_clone.clone();
            async move {
                let n = a.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1;
                if n == 1 {
                    Ok("total garbage, not json".to_string())
                } else {
                    Ok(r#"[{"class":"Intention","title":"Y"}]"#.to_string())
                }
            }
        })
        .await
        .unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(attempts.load(std::sync::atomic::Ordering::SeqCst), 2);
    }

    #[tokio::test]
    async fn retry_extraction_parse_fails_after_max_attempts() {
        // Every attempt returns garbage → we exhaust EXTRACTION_MAX_ATTEMPTS
        // and propagate the last parse error rather than looping forever.
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let result: anyhow::Result<Vec<ProposedInstance>> = retry_extraction_parse(move |_| {
            let a = attempts_clone.clone();
            async move {
                a.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                Ok("never parseable".to_string())
            }
        })
        .await;
        assert!(result.is_err());
        assert_eq!(
            attempts.load(std::sync::atomic::Ordering::SeqCst),
            EXTRACTION_MAX_ATTEMPTS
        );
    }

    #[test]
    fn filter_already_present_drops_known_titles() {
        // Two Tasks proposed; one duplicates an existing title (case-insensitive),
        // one is new. Only the new one survives; a same-title item of a DIFFERENT
        // class is untouched (dedup is per class).
        let proposed = parse_extraction_response(
            r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Write the docs"},
              {"class":"Belief","title":"ship the mvp"}
            ]"#,
        )
        .unwrap();
        let mut existing = HashMap::new();
        existing.insert("Task".to_string(), vec!["ship the MVP".to_string()]);

        let kept = filter_already_present(proposed, &existing);
        let kept_titles: Vec<&str> = kept
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect();
        assert!(
            !kept_titles.contains(&"Ship the MVP"),
            "existing Task title must be dropped (case-insensitive); got {kept_titles:?}"
        );
        assert!(
            kept_titles.contains(&"Write the docs"),
            "new Task must survive"
        );
        assert!(
            kept_titles.contains(&"ship the mvp"),
            "same title on a different class must NOT be dropped; got {kept_titles:?}"
        );
    }
}

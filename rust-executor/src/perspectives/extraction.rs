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
//!          `AIService::prompt` and `PerspectiveInstance::add_link`.
//!   S7 — `#[ignore]` real-LLM e2e (this commit): end-to-end sanity check
//!          against a locally-installed default LLM. Skipped in CI; run
//!          manually on Marvin CUDA / any box with a model installed.
//!
//! S2–S6 are pure/DB-only (no LLM) and CI-tested; the real-LLM e2e is S7.

use crate::agent::AgentContext;
use crate::db::Ad4mDb;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{AITask, Link, LinkStatus};
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
/// `{ "classes": [{ "name", "hint", "fields": [{ "name", "required", "hint" }] }],
///    "transcript": [{ "speaker", "text" }] }`.
pub fn build_extraction_input(shapes: &[ModelShape], transcript: &[(String, String)]) -> String {
    let classes: Vec<serde_json::Value> = shapes
        .iter()
        .map(|s| {
            let fields: Vec<serde_json::Value> = s
                .properties
                .iter()
                // The type flag is set by instance_links (S4), not the LLM.
                .filter(|p| !p.is_flag)
                .map(|p| {
                    serde_json::json!({
                        "name": p.name,
                        "required": p.is_required,
                        "hint": p.extraction_hint,
                    })
                })
                .collect();
            serde_json::json!({
                "name": class_local_name(&s.target_class),
                "hint": s.extraction_hint,
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

You receive a JSON object with two fields:
  - `classes`: available subject classes. Each has a `name`, a natural-language
    `hint` describing when to instantiate it, and a list of `fields`. Each field
    has a `name`, optional `hint`, and `required` flag.
  - `transcript`: an array of turns `{speaker, text}`.

Emit a JSON array. Each element is `{\"class\": <class name>, ...fields}`, where
the fields' values are strings drawn from what participants actually said or
committed to in the transcript. Only include a class if the transcript clearly
supports it — err on the side of fewer instances. Only include a field if the
value is present or clearly implied; omit optional fields you cannot fill.

Output rules:
  - Return valid JSON only — no prose, no markdown fences, no <think> blocks.
  - Return an empty array `[]` if nothing matches.
  - Do not invent classes or fields not listed in `classes`.
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
            vec![],
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
pub async fn run_extraction(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(String, String)],
    base_prefix: &str,
    context: &AgentContext,
) -> anyhow::Result<Vec<(String, Vec<Link>)>> {
    let task = ensure_extraction_task()?;
    let prompt = build_extraction_input(shapes, transcript);

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

    let placements = place_instances(shapes, &instances, base_prefix);

    for (_base, links) in &placements {
        for link in links {
            perspective
                .add_link(link.clone(), LinkStatus::Shared, None, context)
                .await
                .map_err(|e| anyhow::anyhow!("run_extraction: add_link failed: {e:#}"))?;
        }
    }
    Ok(placements)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::model_query::shape::load_shape;
    use crate::perspectives::shacl_parser::parse_shacl_to_links;
    use crate::perspectives::sparql_store::SparqlStore;
    use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
    use std::sync::Once;

    static INIT_DB: Once = Once::new();

    fn ensure_db_init() {
        INIT_DB.call_once(|| {
            Ad4mDb::init_global_instance(":memory:").unwrap();
        });
    }

    const BELIEF_SDNA: &str = r#"{
      "target_class":"ns://Belief",
      "extraction_hint":"A claim a participant holds to be true about the world or the group. Not a task or a question.",
      "properties":[
        {"path":"ns://type","name":"type","has_value":"ns://belief","min_count":1,"max_count":1},
        {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"One-sentence statement in the claimant's framing."}
      ]
    }"#;

    const INTENTION_SDNA: &str = r#"{
      "target_class":"ns://Intention",
      "extraction_hint":"Something a participant commits to doing - an actionable outcome with a plausible owner.",
      "properties":[
        {"path":"ns://type","name":"type","has_value":"ns://intention","min_count":1,"max_count":1},
        {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"Imperative summary of the work."},
        {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","extraction_hint":"Who committed to it, if stated."}
      ]
    }"#;

    /// Build a ModelShape via the real writer -> store -> loader path, so the
    /// class/property `extraction_hint`s are actually populated (the direct
    /// JSON path sets them to None).
    fn shape_from_sdna(class: &str, sdna: &str) -> ModelShape {
        let store = SparqlStore::new(None).unwrap();
        let target = format!("ns://{class}");
        let shape_uri = format!("ns://{class}Shape");
        let mut links = vec![
            Link {
                source: target.clone(),
                predicate: Some("rdf://type".into()),
                target: "ad4m://SubjectClass".into(),
            },
            Link {
                source: target,
                predicate: Some("ad4m://shape".into()),
                target: shape_uri,
            },
        ];
        links.extend(parse_shacl_to_links(sdna, class).unwrap());
        for l in links {
            store
                .add_link(&DecoratedLinkExpression {
                    author: "did:key:test".into(),
                    timestamp: "1700000000000".into(),
                    data: l,
                    proof: DecoratedExpressionProof {
                        key: "k".into(),
                        signature: "s".into(),
                        valid: Some(true),
                        invalid: Some(false),
                    },
                    status: None,
                })
                .unwrap();
        }
        load_shape(&store, class).unwrap()
    }

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

    // ---- S7: end-to-end with a real LLM (ignored in CI) ------------------
    //
    // Exercises the whole pipeline against an actual local model:
    //   shapes -> prompt -> AIService::prompt -> parse (with retry) ->
    //   shape-driven links -> add_link on a real PerspectiveInstance.
    //
    // Skipped in CI. Run manually where a default LLM is available:
    //
    //   cargo test --release -p ad4m-executor \
    //     perspectives::extraction::tests::e2e_run_extraction_with_real_llm \
    //     -- --ignored --nocapture --test-threads=1

    #[ignore]
    #[tokio::test]
    async fn e2e_run_extraction_with_real_llm() {
        use crate::agent::{AgentContext, AgentService};
        use crate::ai_service::AIService;
        use crate::prolog_service::init_prolog_service;
        use crate::test_utils::setup_wallet;
        use crate::types::{
            LinkQuery, LocalModelInput, ModelInput, ModelType, PerspectiveHandle, PerspectiveState,
        };

        setup_wallet();
        ensure_db_init();
        AgentService::init_global_test_instance();
        init_prolog_service().await;

        // Spin up AIService and register a small local LLM as the default so
        // that the `ensure_extraction_task` task (model_id = "default") has
        // something to talk to.
        AIService::init_global_instance()
            .await
            .expect("AIService to initialize");
        let service = AIService::global_instance()
            .await
            .expect("AIService global instance");
        let model_id = service
            .add_model(ModelInput {
                name: "e2e extraction LLM".into(),
                model_type: ModelType::Llm,
                local: Some(LocalModelInput {
                    file_name: "llama_tiny_1_1b_chat".into(),
                    tokenizer_source: None,
                    huggingface_repo: None,
                    revision: None,
                }),
                api: None,
            })
            .await
            .expect("add_model");
        service
            .set_default_model(ModelType::Llm, model_id.clone())
            .await
            .expect("set_default_model(Llm)");

        // Real perspective — same setup pattern as PerspectiveInstance::tests.
        let mut perspective = PerspectiveInstance::new(
            PerspectiveHandle {
                uuid: uuid::Uuid::new_v4().to_string(),
                name: Some("Extraction e2e".into()),
                shared_url: None,
                neighbourhood: None,
                state: PerspectiveState::Private,
                owners: None,
            },
            None,
        );
        let ctx = AgentContext::main_agent();
        perspective
            .ensure_prolog_engine_pool_for_context(&ctx)
            .await
            .expect("prolog engine pool");

        let shapes = vec![
            shape_from_sdna("Belief", BELIEF_SDNA),
            shape_from_sdna("Intention", INTENTION_SDNA),
        ];
        let transcript = vec![
            (
                "Nico".into(),
                "I'll extract the LLM call-processing from Flux into a generic \
                 AD4M core service."
                    .into(),
            ),
            (
                "James".into(),
                "Cool. One English hint per class should be enough to steer this.".into(),
            ),
        ];

        let placements = run_extraction(&mut perspective, &shapes, &transcript, "soa://ext/", &ctx)
            .await
            .expect("run_extraction against real LLM to succeed");

        println!("e2e placements: {} instance(s)", placements.len());
        assert!(
            !placements.is_empty(),
            "expected at least one extracted instance from real LLM"
        );

        // Every claimed instance base must actually have links in the
        // perspective — this is what proves add_link ran, not just that
        // placements were computed.
        for (base, links) in &placements {
            assert!(!links.is_empty(), "empty link set for {base}");
            let stored = perspective
                .get_links(&LinkQuery {
                    source: Some(base.clone()),
                    ..Default::default()
                })
                .await
                .expect("get_links after write");
            assert!(
                !stored.is_empty(),
                "expected links written into perspective for {base}"
            );
        }
    }
}

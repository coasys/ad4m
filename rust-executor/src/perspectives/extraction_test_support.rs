//! Shared test scaffolding for the generic-extraction tests.
//!
//! Split out of `extraction.rs` so the pure unit tests there and the real-LLM
//! e2e suite in `extraction_e2e.rs` share one set of fixtures + one harness
//! (DRY). Nothing here is compiled outside `cfg(test)`.
//!
//! The real-LLM harness targets an **OpenAI-compatible endpoint** (Ollama),
//! env-overridable via `EXTRACTION_E2E_BASE_URL` / `EXTRACTION_E2E_MODEL` /
//! `EXTRACTION_E2E_API_KEY`; the defaults hit Ollama at `localhost:11434`
//! (directly on the CI runner = Marvin, or over an SSH tunnel from a dev box).

#![cfg(test)]

use super::extraction::{
    class_local_name, instance_links, run_extraction, ExtractionOp, ProposedInstance,
};
use super::model_query::shape::load_shape;
use super::model_query::types::ModelShape;
use super::perspective_instance::PerspectiveInstance;
use super::shacl_parser::parse_shacl_to_links;
use super::sparql_store::SparqlStore;
use crate::agent::AgentContext;
use crate::db::Ad4mDb;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link, LinkStatus};
use std::collections::HashMap;
use std::sync::Once;

static INIT_DB: Once = Once::new();

pub(crate) fn ensure_db_init() {
    INIT_DB.call_once(|| {
        Ad4mDb::init_global_instance(":memory:").unwrap();
    });
}

// ---- SoA class fixtures (one `extraction_hint` per class + per property) ----

pub(crate) const BELIEF_SDNA: &str = r#"{
  "target_class":"ns://Belief",
  "extraction_hint":"A claim a participant holds to be true about the world or the group. Not a task or a question.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://belief","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"One-sentence statement in the claimant's framing."}
  ]
}"#;

pub(crate) const INTENTION_SDNA: &str = r#"{
  "target_class":"ns://Intention",
  "extraction_hint":"A first-person commitment to do something - the speaker themselves saying they will act (e.g., 'I'll write X', 'I plan to do Y'). If work is being assigned to someone else, that is a Task, not an Intention.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://intention","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"Imperative summary of the work."},
    {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","extraction_hint":"Who committed to it, if stated."}
  ]
}"#;

pub(crate) const TASK_SDNA: &str = r#"{
  "target_class":"ns://Task",
  "extraction_hint":"A concrete unit of work assigned to a person, typically by someone else (e.g., 'X, can you do Y by Z?', 'we need X to happen'). If the speaker is themselves committing to it in first person, that is an Intention. Not a belief or a vague aspiration.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://task","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"Imperative summary of the task."},
    {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","extraction_hint":"Person responsible for the task, if stated."}
  ]
}"#;

/// A class carrying both a scalar (`title`) and a link-typed relation
/// (`blocks`, hasMany -> Task). `load_shape` lists `blocks` in both
/// `properties` and `include_relations`; the extractor must exclude it.
pub(crate) const TASK_WITH_RELATION_SDNA: &str = r#"{
  "target_class":"ns://Task",
  "extraction_hint":"A concrete, actionable unit of work to be done.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://task","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"Imperative summary of the task."},
    {"path":"ns://blocks","name":"blocks","relation_kind":"hasMany","target_class_name":"Task","class":"ns://TaskShape","extraction_hint":"Other tasks this one blocks."}
  ]
}"#;

pub(crate) const OBSERVATION_SDNA: &str = r#"{
  "target_class":"ns://Observation",
  "extraction_hint":"A factual observation or reported state of the world or system - something seen, measured or reported, not an opinion, plan or task.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://observation","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"The observed fact, stated plainly."}
  ]
}"#;

pub(crate) const QUESTION_SDNA: &str = r#"{
  "target_class":"ns://Question",
  "extraction_hint":"An open question raised in the conversation that still needs an answer.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://question","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"The question, phrased as a question."}
  ]
}"#;

pub(crate) const VISION_SDNA: &str = r#"{
  "target_class":"ns://Vision",
  "extraction_hint":"A long-term aspiration or desired future state - directional and motivating, not a concrete task or plan.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://vision","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"The aspiration, stated concisely."}
  ]
}"#;

pub(crate) const PLAN_SDNA: &str = r#"{
  "target_class":"ns://Plan",
  "extraction_hint":"A concrete approach or sequence of steps intended to achieve a goal.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://plan","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"Summary of the plan or approach."},
    {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","extraction_hint":"Who owns the plan, if stated."}
  ]
}"#;

/// Build a `ModelShape` via the real writer -> store -> loader path, so the
/// class/property `extraction_hint`s are actually populated (the direct JSON
/// path sets them to `None`).
pub(crate) fn shape_from_sdna(class: &str, sdna: &str) -> ModelShape {
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

// ---- real-LLM harness (Ollama over the OpenAI-compatible API) --------------

/// Bring up a fresh private perspective + shapes + agent context, without
/// standing up `AIService` — for unit tests that exercise perspective writes
/// (upsert / apply_extraction_ops) but don't need to call a model.
pub(crate) async fn setup_perspective_no_llm(
    class_sdnas: &[(&str, &str)],
) -> (PerspectiveInstance, Vec<ModelShape>, AgentContext) {
    use crate::agent::AgentService;
    use crate::prolog_service::init_prolog_service;
    use crate::test_utils::setup_wallet;
    use crate::types::{PerspectiveHandle, PerspectiveState};

    setup_wallet();
    ensure_db_init();
    AgentService::init_global_test_instance();
    init_prolog_service().await;

    let perspective = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: uuid::Uuid::new_v4().to_string(),
            name: Some("Extraction test".into()),
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

    let shapes: Vec<ModelShape> = class_sdnas
        .iter()
        .map(|(class, sdna)| shape_from_sdna(class, sdna))
        .collect();

    (perspective, shapes, ctx)
}

/// Stand up `AIService` with the Ollama-backed default model on top of
/// [`setup_perspective_no_llm`]. Returns everything a test needs to drive
/// extraction against a real LLM (optionally after pre-seeding the perspective).
pub(crate) async fn setup_extraction_e2e(
    class_sdnas: &[(&str, &str)],
) -> (PerspectiveInstance, Vec<ModelShape>, AgentContext) {
    use crate::ai_service::AIService;
    use crate::types::{ModelApiInput, ModelInput, ModelType};

    let (perspective, shapes, ctx) = setup_perspective_no_llm(class_sdnas).await;

    // init_global_instance re-inits; each test adds its own model immediately
    // after, so re-init between tests (--test-threads=1) is safe.
    AIService::init_global_instance()
        .await
        .expect("AIService to initialize");
    let service = AIService::global_instance()
        .await
        .expect("AIService global instance");
    let base_url = std::env::var("EXTRACTION_E2E_BASE_URL")
        .unwrap_or_else(|_| "http://localhost:11434/v1".into());
    let model = std::env::var("EXTRACTION_E2E_MODEL").unwrap_or_else(|_| "gemma3:12b".into());
    eprintln!("[e2e] extraction against model '{model}' at {base_url}");
    let model_id = service
        .add_model(ModelInput {
            name: "e2e extraction LLM".into(),
            model_type: ModelType::Llm,
            local: None,
            api: Some(ModelApiInput {
                base_url,
                api_key: std::env::var("EXTRACTION_E2E_API_KEY")
                    .unwrap_or_else(|_| "ollama".into()),
                model,
                api_type: crate::types::ModelApiType::OpenAi.to_string(),
            }),
        })
        .await
        .expect("add_model");
    service
        .set_default_model(ModelType::Llm, model_id)
        .await
        .expect("set_default_model(Llm)");

    (perspective, shapes, ctx)
}

/// Run extraction against the real LLM under the standard `soa://ext/` prefix,
/// writing `Local` links, and dump the placements for the test log.
pub(crate) async fn run_extraction_e2e(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(&str, &str)],
    ctx: &AgentContext,
) -> Vec<(String, Vec<Link>)> {
    let transcript: Vec<(String, String)> = transcript
        .iter()
        .map(|(s, t)| (s.to_string(), t.to_string()))
        .collect();
    let ops = run_extraction(
        perspective,
        shapes,
        &transcript,
        "soa://ext/",
        LinkStatus::Local,
        ctx,
    )
    .await
    .expect("run_extraction against real LLM to succeed");
    let placements = ops_to_placements(&ops);
    print_placements(&placements);
    placements
}

/// Flatten [`ExtractionOp`]s into the `(base, links_written)` shape the e2e
/// assertion helpers expect. Both Create and Update contribute their base +
/// the links that ended up on it — Update writes its `set` (scalar replacements),
/// Create writes flags + scalars. This keeps the assertion surface uniform even
/// once the extractor starts emitting upserts.
pub(crate) fn ops_to_placements(ops: &[ExtractionOp]) -> Vec<(String, Vec<Link>)> {
    ops.iter()
        .map(|op| match op {
            ExtractionOp::Create { base, links } => (base.clone(), links.clone()),
            ExtractionOp::Update { base, set } => (base.clone(), set.clone()),
            // Additive relation edges onto an existing instance — surface them
            // as placements too so e2e assertions can see relation links.
            ExtractionOp::AddLinks { base, links } => (base.clone(), links.clone()),
        })
        .collect()
}

/// Convenience for the simple single-shot tests: set up + run in one call.
pub(crate) async fn run_e2e(
    class_sdnas: &[(&str, &str)],
    transcript: &[(&str, &str)],
) -> (PerspectiveInstance, Vec<(String, Vec<Link>)>) {
    let (mut perspective, shapes, ctx) = setup_extraction_e2e(class_sdnas).await;
    let placements = run_extraction_e2e(&mut perspective, &shapes, transcript, &ctx).await;
    (perspective, placements)
}

/// Like [`run_e2e`], but retries the whole setup+run up to `max_attempts` times
/// until `predicate` returns true. Each attempt uses a fresh perspective so
/// previous writes don't bias the next extraction. Returns the first placement
/// set that satisfies the predicate; if none do, returns the last attempt (so
/// the caller's regular assertions still fire with a real failure message).
///
/// Use only for tests that probe small-model modality coverage on gemma3:12b —
/// intermittent under-extraction is a known behaviour of small models, not a
/// regression to punish on every CI run. Keep `max_attempts` ≤ 3 so the test
/// stays fast (≤ ~30 s wall-clock even in the worst case).
pub(crate) async fn run_e2e_retrying<F>(
    class_sdnas: &[(&str, &str)],
    transcript: &[(&str, &str)],
    max_attempts: usize,
    predicate: F,
) -> (PerspectiveInstance, Vec<(String, Vec<Link>)>)
where
    F: Fn(&[(String, Vec<Link>)]) -> bool,
{
    assert!(max_attempts >= 1, "max_attempts must be >= 1");
    let mut last: Option<(PerspectiveInstance, Vec<(String, Vec<Link>)>)> = None;
    for attempt in 1..=max_attempts {
        let (perspective, placements) = run_e2e(class_sdnas, transcript).await;
        if predicate(&placements) {
            if attempt > 1 {
                println!("[e2e] predicate satisfied on attempt {attempt}/{max_attempts}");
            }
            return (perspective, placements);
        }
        println!("[e2e] attempt {attempt}/{max_attempts} did not satisfy predicate; retrying");
        last = Some((perspective, placements));
    }
    last.expect("retry loop ran at least once")
}

pub(crate) fn print_placements(placements: &[(String, Vec<Link>)]) {
    println!("e2e placements: {} instance(s)", placements.len());
    for (base, links) in placements {
        println!("  instance {base}");
        for l in links {
            println!(
                "      {} -> {}",
                l.predicate.as_deref().unwrap_or("(none)"),
                l.target
            );
        }
    }
}

/// Pre-seed the perspective with an already-existing typed instance (its
/// type-flag + a `title`), exactly as `instance_links` would have written it.
/// Used to test the selector against a non-empty graph and dedup against
/// existing state.
pub(crate) async fn seed_instance(
    perspective: &mut PerspectiveInstance,
    ctx: &AgentContext,
    shape: &ModelShape,
    base: &str,
    title: &str,
) {
    let mut props = HashMap::new();
    props.insert("title".to_string(), serde_json::Value::String(title.into()));
    let inst = ProposedInstance {
        class: class_local_name(&shape.target_class).to_string(),
        id: None,
        props,
    };
    let links = instance_links(shape, &inst, base);
    perspective
        .add_links(links, LinkStatus::Local, None, ctx)
        .await
        .expect("seed_instance add_links");
}

// ---- assertions / accessors over placements --------------------------------

/// Local `ns://type` names of the placed instances (e.g. "intention").
pub(crate) fn placed_type_names(placements: &[(String, Vec<Link>)]) -> Vec<String> {
    placements
        .iter()
        .filter_map(|(_, links)| {
            links
                .iter()
                .find(|l| l.predicate.as_deref() == Some("ns://type"))
                .map(|l| class_local_name(&l.target).to_string())
        })
        .collect()
}

/// Count of placed instances per local type name.
pub(crate) fn count_by_type(placements: &[(String, Vec<Link>)]) -> HashMap<String, usize> {
    let mut counts = HashMap::new();
    for t in placed_type_names(placements) {
        *counts.entry(t).or_insert(0) += 1;
    }
    counts
}

/// Decode a `literal:string:...` URI back to its raw string. Used by e2e
/// assertions that inspect placed `ns://title` links.
pub(crate) fn decode_literal_string(uri: &str) -> Option<String> {
    let rest = uri.strip_prefix("literal:string:")?;
    percent_encoding::percent_decode_str(rest)
        .decode_utf8()
        .ok()
        .map(|c| c.into_owned())
}

/// Every placement's links must actually be readable back from the perspective
/// (proves the write happened, not just that placements were computed).
pub(crate) async fn assert_persisted(
    perspective: &PerspectiveInstance,
    placements: &[(String, Vec<Link>)],
) {
    use crate::types::LinkQuery;
    for (base, links) in placements {
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

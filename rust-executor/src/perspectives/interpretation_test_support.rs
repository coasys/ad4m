//! Shared test scaffolding for the generic-interpretation tests.
//!
//! Split out of `interpretation.rs` so the pure unit tests there and the real-LLM
//! e2e suite in `interpretation_e2e.rs` share one set of fixtures + one harness
//! (DRY). Nothing here is compiled outside `cfg(test)`.
//!
//! The real-LLM harness targets an **OpenAI-compatible endpoint** (Ollama),
//! env-overridable via `INTERPRETATION_E2E_BASE_URL` / `INTERPRETATION_E2E_MODEL` /
//! `INTERPRETATION_E2E_API_KEY`; the defaults hit Ollama at `localhost:11434`
//! (directly on the CI runner = Marvin, or over an SSH tunnel from a dev box).

#![cfg(test)]

use super::interpretation::{class_local_name, run_interpretation};
use super::model_query::shape::load_shape;
use super::model_query::types::ModelShape;
use super::perspective_instance::{PerspectiveInstance, SdnaType, SubjectClassOption};
use super::shacl_parser::parse_shacl_to_links;
use super::sparql_store::SparqlStore;
use crate::agent::AgentContext;
use crate::db::Ad4mDb;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
use std::collections::HashMap;
use std::sync::Once;

static INIT_DB: Once = Once::new();

pub(crate) fn ensure_db_init() {
    INIT_DB.call_once(|| {
        Ad4mDb::init_global_instance(":memory:").unwrap();
    });
}

// ---- SoA class fixtures (one `interpretation_hint` per class + per property) ----

pub(crate) const BELIEF_SDNA: &str = r#"{
  "target_class":"ns://Belief",
  "interpretation_hint":"A claim a participant holds to be true — about the world, the work, or the group. Includes opinions, assessments, and judgments asserted as true, such as 'X is the right approach' or 'Y should be enough'. Not a task or a question.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://belief"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://belief","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"One-sentence statement in the claimant's framing.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

pub(crate) const INTENTION_SDNA: &str = r#"{
  "target_class":"ns://Intention",
  "interpretation_hint":"Something a participant commits to doing - an actionable outcome with a plausible owner.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://intention"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://intention","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"Imperative summary of the work.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","interpretation_hint":"Who committed to it, if stated.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://owner","target":"value"}]}
  ]
}"#;

pub(crate) const TASK_SDNA: &str = r#"{
  "target_class":"ns://Task",
  "interpretation_hint":"A concrete, actionable unit of work to be done, ideally with an owner. Not a belief or a vague aspiration.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://task"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://task","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"Imperative summary of the task.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","interpretation_hint":"Person responsible for the task, if stated.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://owner","target":"value"}]}
  ]
}"#;

/// A class carrying both a scalar (`title`) and a link-typed relation
/// (`blocks`, hasMany -> Task). `load_shape` lists `blocks` in both
/// `properties` and `include_relations`; the extractor must exclude it.
pub(crate) const TASK_WITH_RELATION_SDNA: &str = r#"{
  "target_class":"ns://Task",
  "interpretation_hint":"A concrete, actionable unit of work to be done.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://task"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://task","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"Imperative summary of the task.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://blocks","name":"blocks","relation_kind":"hasMany","target_class_name":"Task","class":"ns://TaskShape","interpretation_hint":"Other tasks this one blocks."}
  ]
}"#;

pub(crate) const OBSERVATION_SDNA: &str = r#"{
  "target_class":"ns://Observation",
  "interpretation_hint":"A factual observation or reported state of the world or system - something seen, measured or reported, not an opinion, plan or task.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://observation"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://observation","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"The observed fact, stated plainly.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

pub(crate) const QUESTION_SDNA: &str = r#"{
  "target_class":"ns://Question",
  "interpretation_hint":"An open question raised in the conversation that still needs an answer.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://question"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://question","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"The question, phrased as a question.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

pub(crate) const VISION_SDNA: &str = r#"{
  "target_class":"ns://Vision",
  "interpretation_hint":"A long-term aspiration or desired future state - directional and motivating, not a concrete task or plan.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://vision"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://vision","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"The aspiration, stated concisely.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

/// A class whose declared identity is `name`, not the default `title`. Used to
/// verify the prompt exposes the identity field name so the model does not
/// silently emit `title` for a non-`title`-identity class and bypass dedup.
pub(crate) const PERSON_SDNA: &str = r#"{
  "target_class":"ns://Person",
  "interpretation_hint":"A person mentioned in the conversation.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://person"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://person","min_count":1,"max_count":1},
    {"path":"ns://name","name":"name","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"Person's name as used in the transcript.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://name","target":"value"}]}
  ]
}"#;

pub(crate) const PLAN_SDNA: &str = r#"{
  "target_class":"ns://Plan",
  "interpretation_hint":"A concrete approach or sequence of steps intended to achieve a goal.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://plan"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://plan","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"Summary of the plan or approach.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","interpretation_hint":"Who owns the plan, if stated.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://owner","target":"value"}]}
  ]
}"#;

/// Build a `ModelShape` via the real writer -> store -> loader path, so the
/// class/property `interpretation_hint`s are actually populated (the direct JSON
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

/// Stand up `AIService` with the Ollama-backed default model plus a fresh
/// private perspective and the given SoA classes. Returns everything a test
/// needs to drive interpretation (optionally after pre-seeding the perspective).
pub(crate) async fn setup_interpretation_e2e(
    class_sdnas: &[(&str, &str)],
) -> (PerspectiveInstance, Vec<ModelShape>, AgentContext) {
    use crate::agent::AgentService;
    use crate::ai_service::AIService;
    use crate::prolog_service::init_prolog_service;
    use crate::test_utils::setup_wallet;
    use crate::types::{ModelApiInput, ModelInput, ModelType, PerspectiveHandle, PerspectiveState};

    setup_wallet();
    ensure_db_init();
    AgentService::init_global_test_instance();
    init_prolog_service().await;

    // init_global_instance re-inits; each test adds its own model immediately
    // after, so re-init between tests (--test-threads=1) is safe.
    AIService::init_global_instance()
        .await
        .expect("AIService to initialize");
    let service = AIService::global_instance()
        .await
        .expect("AIService global instance");
    let base_url = std::env::var("INTERPRETATION_E2E_BASE_URL")
        .unwrap_or_else(|_| "http://localhost:11434/v1".into());
    let model = std::env::var("INTERPRETATION_E2E_MODEL").unwrap_or_else(|_| "gemma3:12b".into());
    eprintln!("[e2e] interpretation against model '{model}' at {base_url}");
    let model_id = service
        .add_model(ModelInput {
            name: "e2e interpretation LLM".into(),
            model_type: ModelType::Llm,
            local: None,
            api: Some(ModelApiInput {
                base_url,
                api_key: std::env::var("INTERPRETATION_E2E_API_KEY")
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

    let mut perspective = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: uuid::Uuid::new_v4().to_string(),
            name: Some("Interpretation e2e".into()),
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

    // Register each class as a REAL subject class (constructor + setter actions)
    // in the perspective, so `create_subject` can read its SDNA. Without this it
    // errors with "No SHACL constructor found".
    for (class, sdna) in class_sdnas {
        perspective
            .add_sdna(
                (*class).to_string(),
                String::new(),
                SdnaType::SubjectClass,
                Some((*sdna).to_string()),
                &ctx,
            )
            .await
            .expect("add_sdna");
    }

    let shapes: Vec<ModelShape> = class_sdnas
        .iter()
        .map(|(class, sdna)| shape_from_sdna(class, sdna))
        .collect();

    (perspective, shapes, ctx)
}

/// Run interpretation against the real LLM under the standard `soa://ext/` prefix,
/// and dump the affected instance base URIs for the test log.
pub(crate) async fn run_interpretation_e2e(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(&str, &str)],
    ctx: &AgentContext,
) -> Vec<String> {
    let transcript: Vec<(String, String)> = transcript
        .iter()
        .map(|(s, t)| (s.to_string(), t.to_string()))
        .collect();
    let bases = run_interpretation(perspective, shapes, &transcript, "soa://ext/", ctx)
        .await
        .expect("run_interpretation against real LLM to succeed");
    print_bases(&bases);
    bases
}

/// Convenience for the simple single-shot tests: set up + run in one call.
/// Returns the perspective, the shapes (so tests can read graph state back via
/// `model_query`), and the affected instance base URIs.
pub(crate) async fn run_e2e(
    class_sdnas: &[(&str, &str)],
    transcript: &[(&str, &str)],
) -> (PerspectiveInstance, Vec<ModelShape>, Vec<String>) {
    let (mut perspective, shapes, ctx) = setup_interpretation_e2e(class_sdnas).await;
    let bases = run_interpretation_e2e(&mut perspective, &shapes, transcript, &ctx).await;
    (perspective, shapes, bases)
}

/// Like [`run_e2e`], but retries the whole interpretation up to `attempts` times
/// until `ok(&graph_count_by_type)` holds — a guard for LLM non-determinism on
/// borderline classifications (e.g. gemma3 occasionally files an action item as
/// `intention` instead of `task`). Returns the first satisfying run, or the last
/// attempt (so the caller's own assertions still fire with full detail).
pub(crate) async fn run_e2e_until(
    class_sdnas: &[(&str, &str)],
    transcript: &[(&str, &str)],
    attempts: u8,
    ok: impl Fn(&HashMap<String, usize>) -> bool,
) -> (PerspectiveInstance, Vec<ModelShape>, Vec<String>) {
    let mut last = None;
    for i in 1..=attempts {
        let (p, shapes, bases) = run_e2e(class_sdnas, transcript).await;
        let counts = graph_count_by_type(&p, &shapes).await;
        if ok(&counts) {
            return (p, shapes, bases);
        }
        eprintln!(
            "[e2e] attempt {i}/{attempts} did not satisfy retry guard (got {counts:?}); retrying"
        );
        last = Some((p, shapes, bases));
    }
    last.expect("run_e2e_until: attempts must be >= 1")
}

pub(crate) fn print_bases(bases: &[String]) {
    println!("e2e affected instances: {}", bases.len());
    for base in bases {
        println!("  instance {base}");
    }
}

/// Pre-seed the perspective with an already-existing typed instance (its
/// type-flag + a `title`) via `create_subject`, the same write path interpretation
/// uses. Used to test the selector against a non-empty graph and dedup against
/// existing state.
pub(crate) async fn seed_instance(
    perspective: &mut PerspectiveInstance,
    ctx: &AgentContext,
    shape: &ModelShape,
    base: &str,
    title: &str,
) {
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(class_local_name(&shape.target_class).to_string()),
                query: None,
            },
            base.to_string(),
            Some(serde_json::json!({ "title": title })),
            None,
            ctx,
        )
        .await
        .expect("seed_instance create_subject");
}

// ---- graph-state accessors / assertions (read back via `model_query`) -------
//
// These read the *final graph state* through `PerspectiveInstance::model_query`
// — the symmetric counterpart to the write side (`create_subject`) and the read
// side (`existing_instance_identities`) — rather than inspecting the placement links
// `run_interpretation` returned. Tests assert what's actually persisted in the
// perspective, decoded through each class's own shape/getters.

/// Read back the instances of `class` via the model-query API, requesting the
/// given `props`. Returns the parsed `instances` array.
///
/// Fails loud on `model_query` errors or malformed responses: this is a
/// test-only assertion helper, and a silently-empty result would let e2e tests
/// pass with false positives (an assertion of "no instances present" would
/// succeed even when the class truly can't be queried). model_query returns Ok
/// with an empty `instances` array when the class is registered but has no
/// persisted instances — that path is preserved as `Vec::new()`.
pub(crate) async fn model_instances(
    perspective: &PerspectiveInstance,
    class: &str,
    props: &[&str],
) -> Vec<serde_json::Value> {
    let query = serde_json::json!({ "properties": props }).to_string();
    let result_json = perspective
        .model_query(class, &query)
        .await
        .unwrap_or_else(|e| panic!("model_instances: model_query({class}) failed: {e:#}"));
    let v: serde_json::Value = serde_json::from_str(&result_json).unwrap_or_else(|e| {
        panic!("model_instances: bad model_query result for {class}: {e:#}; raw={result_json}")
    });
    v.get("instances")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default()
}

/// Count of persisted instances per lower-cased class local name, read from the
/// graph. Lower-cased so assertions stay case-insensitive (matching the old
/// placement-derived `count_by_type`, which keyed off the lower-case type-flag
/// value). Classes with zero instances are omitted (so `.len()` = distinct
/// classes present and `.keys().all(..)` reflects only what's actually there).
pub(crate) async fn graph_count_by_type(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> HashMap<String, usize> {
    let mut counts = HashMap::new();
    for shape in shapes {
        let class = class_local_name(&shape.target_class);
        let n = model_instances(perspective, class, &["title"]).await.len();
        if n > 0 {
            counts.insert(class.to_lowercase(), n);
        }
    }
    counts
}

/// Every persisted instance's `title` across the given classes, lower-cased for
/// order-independent, case-insensitive comparison in assertions.
pub(crate) async fn graph_titles_lower(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> Vec<String> {
    let mut titles = Vec::new();
    for shape in shapes {
        let class = class_local_name(&shape.target_class);
        for inst in model_instances(perspective, class, &["title"]).await {
            if let Some(t) = inst.get("title").and_then(|t| t.as_str()) {
                titles.push(t.to_lowercase());
            }
        }
    }
    titles
}

/// Every persisted instance's `owner` (where the class carries one and a value
/// is present), lower-cased. Used for the owner assertions.
pub(crate) async fn graph_owners_lower(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> Vec<String> {
    let mut owners = Vec::new();
    for shape in shapes {
        if !shape.properties.iter().any(|p| p.name == "owner") {
            continue;
        }
        let class = class_local_name(&shape.target_class);
        for inst in model_instances(perspective, class, &["title", "owner"]).await {
            if let Some(o) = inst.get("owner").and_then(|o| o.as_str()) {
                if !o.is_empty() {
                    owners.push(o.to_lowercase());
                }
            }
        }
    }
    owners
}

/// Every affected base must be readable back as a persisted instance via
/// `model_query` (proves the writes happened, not just that the interpretation
/// computed some ids). Reads each class's instances through the model-query API
/// — hydrated instances carry their base URI under `id` — and asserts every
/// returned base appears among them.
pub(crate) async fn assert_persisted(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
    bases: &[String],
) {
    let mut persisted_ids = std::collections::HashSet::new();
    for shape in shapes {
        let class = class_local_name(&shape.target_class);
        for inst in model_instances(perspective, class, &["title"]).await {
            if let Some(id) = inst.get("id").and_then(|v| v.as_str()) {
                persisted_ids.insert(id.to_string());
            }
        }
    }
    for base in bases {
        assert!(
            persisted_ids.contains(base),
            "base {base} not readable back as a model instance; persisted ids: {persisted_ids:?}"
        );
    }
}

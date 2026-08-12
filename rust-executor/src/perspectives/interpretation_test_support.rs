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

use super::interpretation::{
    apply_interpretation_ops, class_local_name, existing_instance_context,
    plan_interpretation_ops_with_context, run_interpretation, run_interpretation_with_strategy,
    DedupStrategy, ExistingInstances, ExistingLinks, InstanceContext, InterpretationOp,
    ProposedInstance,
};
use super::model_query::shape::load_shape;
use super::model_query::types::{ModelShape, ParentScope};
use super::perspective_instance::{PerspectiveInstance, SdnaType, SubjectClassOption};
use super::shacl_parser::parse_shacl_to_links;
use super::sparql_store::SparqlStore;
use crate::agent::AgentContext;
use crate::db::Ad4mDb;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
use std::collections::{BTreeMap, HashMap};
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
  "interpretation_hint":"A first-person commitment to do something - the speaker themselves saying they will act (e.g., 'I'll write X', 'I plan to do Y'). If work is being assigned to someone else, that is a Task, not an Intention.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://intention"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://intention","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"Imperative summary of the work.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","interpretation_hint":"Who committed to it, if stated.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://owner","target":"value"}]}
  ]
}"#;

pub(crate) const TASK_SDNA: &str = r#"{
  "target_class":"ns://Task",
  "interpretation_hint":"A concrete unit of work assigned to a person, typically by someone else (e.g., 'X, can you do Y by Z?', 'we need X to happen'). If the speaker is themselves committing to it in first person, that is an Intention. Not a belief or a vague aspiration.",
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

/// The Flux-shaped grouping class: `name` is the identity (dedup key), `summary`
/// is a mutable rolling scalar the extractor is asked to *grow* rather than
/// replace. Used by the Flux-grouping / persistent-topics e2e tests: the model
/// must resolve continuing turns to an existing subgroup's `id` (upsert path)
/// and a topic shift must mint a fresh one.
pub(crate) const CONVERSATION_SUBGROUP_SDNA: &str = r#"{
  "target_class":"ns://ConversationSubgroup",
  "interpretation_hint":"A coherent conversational thread — a set of turns focused on the same topic. Group turns discussing the same subject under one subgroup; a clear shift in subject starts a new subgroup. When an existing subgroup already covers the topic being discussed, REUSE its id (via the `id` field on the proposed instance) instead of creating a duplicate. CRITICAL DECISION RULE: read each `existing` entry's `title` (its topic name) BEFORE deciding whether to reuse an id. Only reuse an existing subgroup's id when the current turns are clearly on the SAME topic as that subgroup's title. If the current turns are on a different topic — even if there is only one existing subgroup — leave `id` unset and mint a NEW subgroup. Reusing an id for a genuinely unrelated topic silently overwrites the existing subgroup and destroys data.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://conversationsubgroup"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://conversationsubgroup","min_count":1,"max_count":1},
    {"path":"ns://name","name":"name","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"Short label for the topic (2-5 words).","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://name","target":"value"}]},
    {"path":"ns://summary","name":"summary","min_count":0,"max_count":1,"resolve_language":"literal","interpretation_hint":"1-2 sentence rolling summary of what has been discussed in THIS subgroup specifically — its own topic only. When updating an existing subgroup, incorporate ONLY the new turns that belong to this subgroup's topic, extending the existing summary rather than replacing it. NEVER fold in turns about a different topic: if the current turns are on a new topic they belong to a NEW subgroup, so leave this one's `id` and `summary` out of your output entirely and let it stay exactly as it is.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://summary","target":"value"}]}
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

// ---- harnesses --------------------------------------------------------------

/// Bring up a fresh private perspective with the given SoA classes registered as
/// REAL subject classes, **without** standing up `AIService` — for unit tests
/// that exercise perspective writes (upsert, `apply_interpretation_ops`) but
/// never call a model.
///
/// The `add_sdna` registration is what makes `create_subject` / `update_subject`
/// work: they read each class's `ad4m://constructor` + per-property
/// `ad4m://setter` actions from the perspective's store, not from the in-memory
/// `ModelShape`. Without it they error with "No SHACL constructor found".
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

    let mut perspective = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: uuid::Uuid::new_v4().to_string(),
            name: Some("Interpretation test".into()),
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

// ---- real-LLM harness (Ollama over the OpenAI-compatible API) --------------

/// Stand up `AIService` with the Ollama-backed default model on top of
/// [`setup_perspective_no_llm`]. Returns everything a test needs to drive
/// interpretation (optionally after pre-seeding the perspective).
pub(crate) async fn setup_interpretation_e2e(
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
    // Insert the interpretation task row BEFORE setting the default model:
    // `ensure_interpretation_task` only writes the DB row, and it's
    // `set_default_model`'s respawn loop (over `model_id == "default"` tasks)
    // that actually registers the task with the LLM worker. Priming it here
    // makes every e2e test self-contained — otherwise running one in isolation
    // (with no earlier test having inserted the row) leaves the task unspawned
    // and the first `prompt()` fails with "Task ... not spawned".
    let _ = crate::perspectives::interpretation::ensure_interpretation_task();
    service
        .set_default_model(ModelType::Llm, model_id)
        .await
        .expect("set_default_model(Llm)");

    (perspective, shapes, ctx)
}

/// Register (and load) an `AIService` embedding model named `interpretation-embed`
/// — a local candle Bert (CPU) — so the `DedupStrategy::Semantic` path can embed
/// identity strings through `AIService::embed` rather than any external endpoint.
/// The channel is keyed by the model *name*, which is what `semantic_from_env`
/// (default `interpretation-embed`) passes to `embed`. Idempotent-ish: only the
/// one semantic-dedup e2e needs it, so it isn't part of the default setup (Bert
/// load is not free).
pub(crate) async fn register_interpretation_embedding_model() {
    use crate::types::{ModelInput, ModelType};
    let service = crate::ai_service::AIService::global_instance()
        .await
        .expect("AIService global instance");
    service
        .add_model(ModelInput {
            name: "interpretation-embed".into(),
            model_type: ModelType::Embedding,
            local: None,
            api: None,
        })
        .await
        .expect("add_model(Embedding)");
}

/// Read back the links written under each affected `base`. Production
/// `run_interpretation` is link-free (it returns only base URIs), but a few e2e
/// tests assert on the *edges* a run wrote (a resolved `topic-of` relation, a
/// reified `SemanticRelationship`), which live on links rather than in
/// model_query scalar state. The test harness reads them back here so those
/// tests keep working without production returning links.
pub(crate) async fn read_back_placements(
    perspective: &PerspectiveInstance,
    bases: &[String],
) -> Vec<(String, Vec<Link>)> {
    let mut out = Vec::with_capacity(bases.len());
    for base in bases {
        let stored = perspective
            .get_links(&crate::types::LinkQuery {
                source: Some(base.clone()),
                ..Default::default()
            })
            .await
            .expect("get_links readback");
        let links: Vec<Link> = stored.into_iter().map(|d| d.data.clone()).collect();
        out.push((base.clone(), links));
    }
    out
}

/// Run interpretation against the real LLM under the standard `soa://ext/` prefix,
/// returning the affected instances with their written links read back (see
/// [`read_back_placements`]).
pub(crate) async fn run_interpretation_e2e(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(&str, &str)],
    ctx: &AgentContext,
) -> Vec<(String, Vec<Link>)> {
    let transcript: Vec<(String, String)> = transcript
        .iter()
        .map(|(s, t)| (s.to_string(), t.to_string()))
        .collect();
    let bases = run_interpretation(perspective, shapes, &transcript, "soa://ext/", ctx, None)
        .await
        .expect("run_interpretation against real LLM to succeed");
    let placements = read_back_placements(perspective, &bases).await;
    print_placements(&placements);
    placements
}

/// Like [`run_interpretation_e2e`] but with an explicit [`DedupStrategy`] —
/// used by the semantic-dedup e2e to opt into the embedding-based dedup path
/// without changing the default that every other e2e test relies on.
pub(crate) async fn run_interpretation_e2e_with_strategy(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(&str, &str)],
    ctx: &AgentContext,
    strategy: &DedupStrategy,
) -> Vec<(String, Vec<Link>)> {
    let transcript: Vec<(String, String)> = transcript
        .iter()
        .map(|(s, t)| (s.to_string(), t.to_string()))
        .collect();
    let bases = run_interpretation_with_strategy(
        perspective,
        shapes,
        &transcript,
        "soa://ext/",
        ctx,
        strategy,
        None,
    )
    .await
    .expect("run_interpretation_with_strategy against real LLM to succeed");
    let placements = read_back_placements(perspective, &bases).await;
    print_placements(&placements);
    placements
}

/// Like [`run_interpretation_e2e`] but with an explicit existing-instance
/// `scope` (a [`ParentScope`]) — the channel-scoping a Flux-style processor
/// applies so the model only sees the subgroups belonging to *this* channel.
/// Returns the affected instance bases so the caller can wire fresh instances
/// into the scoped sub-graph between passes.
pub(crate) async fn run_interpretation_e2e_scoped(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    transcript: &[(&str, &str)],
    ctx: &AgentContext,
    scope: Option<&ParentScope>,
) -> Vec<String> {
    let transcript: Vec<(String, String)> = transcript
        .iter()
        .map(|(s, t)| (s.to_string(), t.to_string()))
        .collect();
    let bases = run_interpretation(perspective, shapes, &transcript, "soa://ext/", ctx, scope)
        .await
        .expect("run_interpretation (scoped) against real LLM to succeed");
    let placements = read_back_placements(perspective, &bases).await;
    print_placements(&placements);
    bases
}

/// Convenience for the simple single-shot tests: set up + run in one call.
/// Returns the perspective, the shapes (so tests can read graph state back via
/// `model_query`), and the affected instance base URIs.
pub(crate) async fn run_e2e(
    class_sdnas: &[(&str, &str)],
    transcript: &[(&str, &str)],
) -> (
    PerspectiveInstance,
    Vec<ModelShape>,
    Vec<(String, Vec<Link>)>,
) {
    let (mut perspective, shapes, ctx) = setup_interpretation_e2e(class_sdnas).await;
    let placements = run_interpretation_e2e(&mut perspective, &shapes, transcript, &ctx).await;
    (perspective, shapes, placements)
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
) -> (
    PerspectiveInstance,
    Vec<ModelShape>,
    Vec<(String, Vec<Link>)>,
) {
    let mut last = None;
    for i in 1..=attempts {
        let (p, shapes, placements) = run_e2e(class_sdnas, transcript).await;
        let counts = graph_count_by_type(&p, &shapes).await;
        if ok(&counts) {
            return (p, shapes, placements);
        }
        eprintln!(
            "[e2e] attempt {i}/{attempts} did not satisfy retry guard (got {counts:?}); retrying"
        );
        last = Some((p, shapes, placements));
    }
    last.expect("run_e2e_until: attempts must be >= 1")
}

/// Like [`run_e2e_until`], but the retry guard inspects the *placements* rather
/// than graph counts — for properties that live on the links a run wrote (e.g.
/// "a relation edge resolved to a co-minted sibling") rather than on how many
/// instances of each class ended up in the graph.
pub(crate) async fn run_e2e_until_placements(
    class_sdnas: &[(&str, &str)],
    transcript: &[(&str, &str)],
    attempts: u8,
    ok: impl Fn(&[(String, Vec<Link>)]) -> bool,
) -> (
    PerspectiveInstance,
    Vec<ModelShape>,
    Vec<(String, Vec<Link>)>,
) {
    let mut last = None;
    for i in 1..=attempts {
        let (p, shapes, placements) = run_e2e(class_sdnas, transcript).await;
        if ok(&placements) {
            return (p, shapes, placements);
        }
        eprintln!("[e2e] attempt {i}/{attempts} did not satisfy retry guard; retrying");
        last = Some((p, shapes, placements));
    }
    last.expect("run_e2e_until_placements: attempts must be >= 1")
}

pub(crate) fn print_placements(placements: &[(String, Vec<Link>)]) {
    println!("e2e placements: {} instance(s)", placements.len());
    for (base, links) in placements {
        println!("  instance {base} ({} link(s))", links.len());
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
    seed_instance_with_props(
        perspective,
        ctx,
        shape,
        base,
        serde_json::json!({ "title": title }),
    )
    .await;
}

/// Like [`seed_instance`] but accepts an arbitrary props object — used when the
/// class's identity is a non-`title` field (e.g. `ConversationSubgroup.name`) or
/// the seed needs to carry secondary scalars (e.g. `summary`) so a subsequent
/// interpretation pass can *update* them in place.
pub(crate) async fn seed_instance_with_props(
    perspective: &mut PerspectiveInstance,
    ctx: &AgentContext,
    shape: &ModelShape,
    base: &str,
    props: serde_json::Value,
) {
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(class_local_name(&shape.target_class).to_string()),
                query: None,
            },
            base.to_string(),
            Some(props),
            None,
            ctx,
        )
        .await
        .expect("seed_instance_with_props create_subject");
}

/// Seed an `InterpretationOverlay` over an already-seeded instance so the §4
/// human-divergence gate treats it as **LLM-authored** (as it would be in
/// production, where every instance is minted by an interpretation pass that
/// also writes the overlay). Without this, a directly-`seed_instance`'d base
/// carries no overlay and the gate — correctly — refuses to let a later pass
/// overwrite its scalars (protecting human/seed data). Use it whenever a test
/// seeds an instance that a subsequent interpretation pass is expected to
/// *update in place* (e.g. a persistent `ConversationSubgroup` whose rolling
/// `summary` grows). `props` are the same property names/values seeded onto the
/// instance; they are mapped to their real predicates via the class shape so the
/// overlay's `inferred/<p>` equals the seeded real value.
pub(crate) async fn seed_llm_overlay(
    perspective: &mut PerspectiveInstance,
    ctx: &AgentContext,
    shape: &ModelShape,
    base: &str,
    props: serde_json::Value,
) {
    let obj = props
        .as_object()
        .expect("seed_llm_overlay: props must be a JSON object");
    let mut inferred: BTreeMap<String, serde_json::Value> = BTreeMap::new();
    for (name, value) in obj {
        let pred = shape
            .properties
            .iter()
            .find(|p| &p.name == name)
            .unwrap_or_else(|| {
                panic!(
                    "seed_llm_overlay: class {} has no property {name}",
                    shape.target_class
                )
            })
            .predicate
            .clone();
        inferred.insert(pred, value.clone());
    }
    super::interpretation::seed_overlay(perspective, base, inferred, ctx)
        .await
        .expect("seed_llm_overlay: seed_overlay");
}

// ---- graph-state accessors / assertions (read back via `model_query`) -------
//
// These read the *final graph state* through `PerspectiveInstance::model_query`
// — the symmetric counterpart to the write side (`create_subject`) and the read
// side (`existing_instance_context`) — rather than inspecting the placement links
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
    placements: &[(String, Vec<Link>)],
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
    for (base, _links) in placements {
        assert!(
            persisted_ids.contains(base),
            "base {base} not readable back as a model instance; persisted ids: {persisted_ids:?}"
        );
    }
}

// ---- relocated interpretation unit-test fixtures ----

/// An empty existing-instance context, typed — the interpretation path takes
/// the id-keyed [`ExistingInstances`] map now, so a bare `HashMap::new()` can't
/// be inferred.
pub(crate) fn no_existing() -> ExistingInstances {
    HashMap::new()
}

/// Build an [`ExistingInstances`] map (id → context) from a list of instances,
/// keyed by each instance's own `id`. The single-source shape the production
/// code threads everywhere; tests that used to hand-build class→identity or
/// id-set projections construct this instead.
pub(crate) fn existing_map(instances: Vec<InstanceContext>) -> ExistingInstances {
    instances.into_iter().map(|i| (i.id.clone(), i)).collect()
}

/// Convenience for planner tests that only exercise id membership (Create vs
/// Update routing / relation-ref validation) and don't read identity/props:
/// build minimal entries keyed by the given ids.
pub(crate) fn existing_ids(ids: &[&str]) -> ExistingInstances {
    existing_map(
        ids.iter()
            .map(|id| InstanceContext {
                id: (*id).to_string(),
                title: String::new(),
                class: String::new(),
                properties: BTreeMap::new(),
            })
            .collect(),
    )
}

/// Convenience for dedup tests that only care about (class, identity) pairs:
/// synthesize a deterministic id per entry so the instance is addressable in
/// the id-keyed map without the test spelling one out.
pub(crate) fn existing_by_identity(entries: &[(&str, &str)]) -> ExistingInstances {
    existing_map(
        entries
            .iter()
            .enumerate()
            .map(|(i, (class, title))| InstanceContext {
                id: format!("test://existing/{class}/{i}"),
                title: (*title).to_string(),
                class: (*class).to_string(),
                properties: BTreeMap::new(),
            })
            .collect(),
    )
}

/// Pull a named property's string value off each parsed instance. These are
/// pure parse-level assertions over the raw LLM JSON — there is no graph and no
/// dedup here, so this takes the field name explicitly rather than assuming a
/// `title`. (Dedup identity is class-declared and handled graph-side in
/// `filter_already_present` / `existing_instance_context`.)
pub(crate) fn prop_values<'a>(instances: &'a [ProposedInstance], key: &str) -> Vec<&'a str> {
    instances
        .iter()
        .filter_map(|i| i.props.get(key).and_then(|v| v.as_str()))
        .collect()
}

/// Base URI of the Nth (0-based) `Create` op, in op order. Panics if absent.
pub(crate) fn nth_create_base(ops: &[InterpretationOp], n: usize) -> String {
    ops.iter()
        .filter_map(|op| match op {
            InterpretationOp::Create { base, .. } => Some(base.clone()),
            _ => None,
        })
        .nth(n)
        .expect("expected a Create op at that index")
}

/// The links of the `AddLinks` op anchored on `source`, or an empty slice.
pub(crate) fn addlinks_for<'a>(ops: &'a [InterpretationOp], source: &str) -> &'a [Link] {
    ops.iter()
        .find_map(|op| match op {
            InterpretationOp::AddLinks { source: s, links } if s == source => {
                Some(links.as_slice())
            }
            _ => None,
        })
        .unwrap_or(&[])
}

pub(crate) fn targets_of(links: &[Link], predicate: &str) -> Vec<String> {
    links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some(predicate))
        .map(|l| l.target.clone())
        .collect()
}

/// Collect the `(source, predicate, target)` triples an op set's `AddLinks` would
/// write — the existing-link state a *subsequent* planner pass reads back to stay
/// idempotent (James #883 #4). Mirrors what `existing_relation_links` returns
/// after those ops are applied, without needing a live perspective.
pub(crate) fn links_from_ops(ops: &[InterpretationOp]) -> ExistingLinks {
    let mut out = ExistingLinks::new();
    for op in ops {
        if let InterpretationOp::AddLinks { links, .. } = op {
            for l in links {
                out.insert((
                    l.source.clone(),
                    l.predicate.clone().unwrap_or_default(),
                    l.target.clone(),
                ));
            }
        }
    }
    out
}

/// Decoded targets of `(base, predicate)` in the store, sorted — the shape
/// assertions want, independent of the non-deterministic signed-envelope
/// encoding a `literal` resolve-language produces.
pub(crate) async fn decoded_targets(
    perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
    base: &str,
    predicate: &str,
) -> Vec<serde_json::Value> {
    use crate::perspectives::model_query::utils::parse_literal_value;
    use crate::types::LinkQuery;
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links");
    let mut out: Vec<serde_json::Value> = links
        .iter()
        .map(|l| parse_literal_value(&l.data.target))
        .collect();
    out.sort_by_key(|v| v.to_string());
    out
}

/// Plan + apply a single proposal against a live perspective. Mirrors
/// `run_interpretation`: seeds `known_existing_ids` from
/// `existing_instance_context` so an `id` on the proposal is trusted only when
/// the perspective actually holds that base — a hallucinated id routes to
/// Create, just like in production.
pub(crate) async fn apply_one(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    shapes: &[crate::perspectives::model_query::types::ModelShape],
    ctx: &crate::agent::AgentContext,
    inst: ProposedInstance,
) -> Vec<InterpretationOp> {
    let existing_ctx = existing_instance_context(perspective, shapes, None)
        .await
        .expect("existing_instance_context");
    let ops = plan_interpretation_ops_with_context(
        shapes,
        std::slice::from_ref(&inst),
        "soa://ext/",
        &existing_ctx,
    );
    apply_interpretation_ops(perspective, &ops, ctx)
        .await
        .expect("apply_interpretation_ops");
    ops
}

pub(crate) fn proposal(
    class: &str,
    id: Option<&str>,
    props: &[(&str, serde_json::Value)],
) -> ProposedInstance {
    ProposedInstance {
        class: class.to_string(),
        id: id.map(|s| s.to_string()),
        props: props
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect(),
    }
}

/// Seed the perspective with `(msg_uri, author, body_text)` triples wired as
/// two direct links per message: `<msg> <ns://body> <literal:string:...>` and
/// `<msg> <ns://author> <did:key:...>`. Mirrors the shape a Flux-style channel
/// perspective would present and the SPARQL queries in these tests target.
pub(crate) async fn seed_message(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    ctx: &crate::agent::AgentContext,
    msg_uri: &str,
    author: &str,
    body: &str,
    body_predicate: &str,
) {
    use crate::types::{Link, LinkStatus};
    perspective
        .add_link(
            Link {
                source: msg_uri.into(),
                predicate: Some(body_predicate.into()),
                target: format!("literal:string:{body}"),
            },
            LinkStatus::Local,
            None,
            ctx,
        )
        .await
        .expect("seed_message body");
    perspective
        .add_link(
            Link {
                source: msg_uri.into(),
                predicate: Some("ns://author".into()),
                target: author.into(),
            },
            LinkStatus::Local,
            None,
            ctx,
        )
        .await
        .expect("seed_message author");
}

//! A/B of the two interpretation paths on one batch (#1804): arm A is the
//! single-shot pass with the budgeted `existing` block of #1806, arm B the
//! harness pass with the seed and the walk tools, with an explicit cap.
//!
//! The fixture is a graph of 6 earlier sources: 40 topics, each with 9
//! points (400 rows), all under the processor's prefix. The batch continues
//! two old topics and opens one new one. Each arm counts:
//!
//! - duplicates: new topics about a subject that an old topic already holds;
//! - wrong attaches: new points or decisions whose topic is not the marked
//!   topic (the old topic, or the one new topic);
//! - fallbacks (arm B), completions and tool calls, seconds per pass.
//!
//! It prints one `AB` line per pass and asserts nothing about quality.
//! Gated like the other real-model suites:
//!
//!   INTERPRETATION_E2E_BASE_URL=http://127.0.0.1:12460/v1 \
//!   INTERPRETATION_E2E_MODEL=qwen3.8-27b-ud-q4_k_m \
//!   INTERPRETATION_E2E_ENDPOINT_KIND=LLAMA_SERVER \
//!   AB_RUNS=3 cargo test --release --lib perspectives::interpretation_ab_e2e \
//!     -- --ignored --test-threads=1 --nocapture

#![cfg(test)]

use super::interpretation::{
    run_interpretation_with_strategy_and_model, DedupStrategy, TranscriptTurn,
};
use super::interpretation_test_support::*;
use crate::ai_service::harness::provider::ToolSchema;
use crate::ai_service::harness::{CompletionSource, HarnessCompletion};
use crate::types::{Link, LinkQuery, LinkStatus};
use anyhow::Result;
use serde_json::Value;
use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

const TOPIC: &str = r#"{
  "target_class":"ns://Topic",
  "interpretation_hint":"A subject the group discusses, over one or many calls. When the talk continues a subject that a topic already holds, use that topic and do not create a new one.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://topic"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://topic","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"The subject in 2-6 words.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

const POINT: &str = r#"{
  "target_class":"ns://Point",
  "interpretation_hint":"One statement or fact someone said, about one topic.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://point"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://point","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"The statement in one sentence.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://about","name":"about","relation_kind":"hasOne","target_class_name":"Topic","class":"ns://TopicShape","interpretation_hint":"The topic this point is about.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://about","target":"value"}]}
  ]
}"#;

const DECISION: &str = r#"{
  "target_class":"ns://Decision",
  "interpretation_hint":"Something the group decided, or a task someone took on.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://decision"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://decision","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"The decision in one sentence.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://about","name":"about","relation_kind":"hasOne","target_class_name":"Topic","class":"ns://TopicShape","interpretation_hint":"The topic this decision is about.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://about","target":"value"}]}
  ]
}"#;

const PREFIX: &str = "proc://interp/";

/// The 40 subjects of the earlier sources. The batch continues the first two.
const SUBJECTS: [&str; 40] = [
    "GPU box budget",
    "coasys.app website copy",
    "Holochain gossip timing",
    "Membrane admission spec",
    "IIW language devkit",
    "Staging deploy script",
    "Flow runner state machine",
    "SHACL render annotations",
    "Agent mail between bots",
    "Telegram topic naming",
    "Credit gate for model calls",
    "Neighbourhood join from a link",
    "Profile pictures in Flux",
    "Mobile app login",
    "Desktop app auto update",
    "Release notes process",
    "CircleCI runner disk space",
    "Oxigraph store migration",
    "SPARQL query timeouts",
    "Perspective sharing rules",
    "Debate app prototype",
    "ATProto bridge",
    "Video call layout",
    "Recording consent",
    "Docs site navigation",
    "Bootstrap languages publish",
    "Key rotation for agents",
    "Signal bot account",
    "Email triage session",
    "Nightly model tests",
    "Embedding model choice",
    "Dedup thresholds",
    "Task board for the sprint",
    "Funding application",
    "Community call format",
    "Open source license",
    "Onboarding for new members",
    "Hosting costs",
    "Security review of the MCP server",
    "Offline sync for mobile",
];

const BATCH: [(&str, &str); 6] = [
    (
        "Nico",
        "Back to the GPU box budget from last week: the second quote came in at 4,200 euros.",
    ),
    (
        "Josh",
        "That is inside the budget. Let's order the second GPU box this week, I will do it.",
    ),
    (
        "James",
        "On the coasys.app website copy: the hero text still says P2P everywhere. It should \
         say centralised now, P2P being built.",
    ),
    (
        "Nico",
        "Agreed. James rewrites the hero text of the website by Friday.",
    ),
    (
        "Josh",
        "Something new: the Whisper transcription on staging drops the last sentence of \
         each call.",
    ),
    (
        "Nico",
        "Then we add a test that feeds a recorded call and checks that the last sentence \
         is in the transcript.",
    ),
];

/// Which marked topic a new row belongs to, from its words: the two old
/// topics, or the new subject.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Mark {
    Gpu,
    Website,
    Whisper,
}

fn mark_of(text: &str) -> Option<Mark> {
    let t = text.to_lowercase();
    if t.contains("gpu") || t.contains("quote") || t.contains("4,200") || t.contains("4200") {
        Some(Mark::Gpu)
    } else if t.contains("website") || t.contains("hero") || t.contains("coasys.app") {
        Some(Mark::Website)
    } else if t.contains("whisper")
        || t.contains("transcri")
        || t.contains("last sentence")
        || t.contains("recorded call")
    {
        Some(Mark::Whisper)
    } else {
        None
    }
}

/// Counts the completions and tool calls of a harness pass.
struct Counting {
    inner: Arc<dyn CompletionSource>,
    completions: AtomicUsize,
    tool_calls: AtomicUsize,
}

#[async_trait::async_trait]
impl CompletionSource for Counting {
    async fn complete(
        &self,
        model_id: &str,
        messages: &[Value],
        tools: Vec<ToolSchema>,
    ) -> Result<HarnessCompletion> {
        self.completions.fetch_add(1, Ordering::SeqCst);
        let out = self.inner.complete(model_id, messages, tools).await?;
        self.tool_calls
            .fetch_add(out.tool_calls.len(), Ordering::SeqCst);
        Ok(out)
    }

    async fn tool_result_max_bytes(&self, model_id: &str) -> usize {
        self.inner.tool_result_max_bytes(model_id).await
    }
}

/// The model the e2e variables name, as the default LLM.
async fn add_model() {
    use crate::ai_service::AIService;
    use crate::types::{ModelInput, ModelType};
    crate::test_utils::setup_wallet();
    ensure_db_init();
    crate::agent::AgentService::init_global_test_instance();
    AIService::init_global_instance()
        .await
        .expect("AIService to initialize");
    let service = AIService::global_instance().await.expect("AIService");
    let base_url = std::env::var("INTERPRETATION_E2E_BASE_URL")
        .unwrap_or_else(|_| "http://127.0.0.1:12460/v1".into());
    let model = std::env::var("INTERPRETATION_E2E_MODEL")
        .unwrap_or_else(|_| "qwen3.8-27b-ud-q4_k_m".into());
    let kind = match std::env::var("INTERPRETATION_E2E_ENDPOINT_KIND")
        .unwrap_or_else(|_| "LLAMA_SERVER".into())
        .as_str()
    {
        "OLLAMA" => crate::types::EndpointKind::Ollama,
        _ => crate::types::EndpointKind::LlamaServer,
    };
    let mut endpoint = crate::types::Endpoint::for_test("ab-llm", kind, &base_url, "none");
    endpoint.max_num_ctx = Some(
        std::env::var("INTERPRETATION_E2E_MAX_NUM_CTX")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(57_344),
    );
    crate::db::Ad4mDb::with_global_instance(|db| db.insert_endpoint(&endpoint))
        .expect("save the endpoint");
    eprintln!("[ab] model '{model}' at {base_url} ({kind:?})");
    let model_id = service
        .add_model(ModelInput {
            name: "ab interpretation LLM".into(),
            model_type: ModelType::Llm,
            endpoint_id: endpoint.id,
            remote_model: model,
            native_tools: None,
        })
        .await
        .expect("add_model");
    let _ = crate::perspectives::interpretation::register_interpretation_task();
    service
        .set_default_model(ModelType::Llm, model_id)
        .await
        .expect("set_default_model");
}

/// The fixture graph: 40 old topics with 9 points each.
async fn fixture() -> (
    crate::perspectives::perspective_instance::PerspectiveInstance,
    Vec<crate::perspectives::model_query::types::ModelShape>,
    crate::agent::AgentContext,
) {
    let (mut p, shapes, ctx) =
        setup_perspective_no_llm(&[("Topic", TOPIC), ("Point", POINT), ("Decision", DECISION)])
            .await;
    for (i, subject) in SUBJECTS.iter().enumerate() {
        let topic = format!("{PREFIX}topic/{i}");
        seed_instance(&mut p, &ctx, &shapes[0], &topic, subject).await;
        for j in 0..9 {
            let point = format!("{PREFIX}point/{i}-{j}");
            let title = format!("{subject}: remark {j} from call {}", 1 + (i + j) % 6);
            seed_instance(&mut p, &ctx, &shapes[1], &point, &title).await;
            p.add_link(
                Link {
                    source: point,
                    predicate: Some("ns://about".into()),
                    target: topic.clone(),
                },
                LinkStatus::Shared,
                None,
                &ctx,
            )
            .await
            .unwrap();
        }
    }
    (p, shapes, ctx)
}

fn topic_of_mark(mark: Mark) -> Option<String> {
    match mark {
        Mark::Gpu => Some(format!("{PREFIX}topic/0")),
        Mark::Website => Some(format!("{PREFIX}topic/1")),
        Mark::Whisper => None,
    }
}

/// One pass of one arm, measured and printed.
async fn one_pass(arm: &str, run: usize) {
    let (mut p, shapes, ctx) = fixture().await;
    let uuid = p.persisted.lock().await.uuid.clone();
    crate::perspectives::register_perspective(uuid.clone(), p.clone());
    let transcript: Vec<TranscriptTurn> = BATCH
        .iter()
        .map(|(s, t)| TranscriptTurn::from_speaker_text(*s, *t))
        .collect();
    let started = std::time::Instant::now();
    let counting = Arc::new(Counting {
        inner: Arc::new(
            crate::api::openai_compat::harness_bridge::OpenAiCompatBridge::new(
                Arc::new(
                    crate::ai_service::AIService::global_instance()
                        .await
                        .expect("AIService"),
                ),
                None,
            ),
        ),
        completions: AtomicUsize::new(0),
        tool_calls: AtomicUsize::new(0),
    });
    let outcome = if arm == "A" {
        run_interpretation_with_strategy_and_model(
            &mut p,
            &shapes,
            &transcript,
            PREFIX,
            &ctx,
            &DedupStrategy::default(),
            None,
            None,
            None,
            false,
            None,
            None,
        )
        .await
    } else {
        crate::perspectives::interpretation::run_harness_pass(
            &mut p,
            &shapes,
            &transcript,
            PREFIX,
            &ctx,
            None,
            None,
            None,
            40,
            None,
            None,
            // As the auto-processor runs it.
            true,
            None,
            None,
            &DedupStrategy::default(),
            false,
            Some(counting.clone()),
        )
        .await
    };
    let secs = started.elapsed().as_secs_f64();
    crate::perspectives::unregister_perspective(&uuid);
    let outcome = match outcome {
        Ok(o) => o,
        Err(e) => {
            eprintln!("AB arm={arm} run={run} error secs={secs:.0} {e:#}");
            return;
        }
    };

    // New topics, and duplicates among them.
    let topics = model_instances(&p, "Topic", &["title"]).await;
    let new_topics: Vec<(String, String)> = topics
        .iter()
        .filter_map(|t| {
            let id = t["id"].as_str()?.to_string();
            let old = id.starts_with(&format!("{PREFIX}topic/"))
                && id[format!("{PREFIX}topic/").len()..]
                    .parse::<usize>()
                    .is_ok();
            (!old).then(|| (id, t["title"].as_str().unwrap_or_default().to_string()))
        })
        .collect();
    let duplicates = new_topics
        .iter()
        .filter(|(_, title)| matches!(mark_of(title), Some(Mark::Gpu | Mark::Website)))
        .count();
    let whisper_topics: HashSet<&String> = new_topics
        .iter()
        .filter(|(_, title)| mark_of(title) == Some(Mark::Whisper))
        .map(|(id, _)| id)
        .collect();

    // New points and decisions, and where they attach.
    let mut rows = 0;
    let mut wrong = Vec::new();
    for class in ["Point", "Decision"] {
        for inst in model_instances(&p, class, &["title"]).await {
            let id = inst["id"].as_str().unwrap_or_default().to_string();
            if id.starts_with(&format!("{PREFIX}point/")) {
                continue;
            }
            rows += 1;
            let title = inst["title"].as_str().unwrap_or_default().to_string();
            let parents: Vec<String> = p
                .get_links(&LinkQuery {
                    source: Some(id.clone()),
                    predicate: Some("ns://about".into()),
                    ..Default::default()
                })
                .await
                .unwrap()
                .into_iter()
                .map(|l| l.data.target)
                .collect();
            let ok = match mark_of(&title) {
                Some(Mark::Whisper) => parents.iter().any(|p| whisper_topics.contains(p)),
                Some(m) => parents.contains(&topic_of_mark(m).unwrap()),
                None => true,
            };
            if !ok {
                wrong.push(format!("{class} '{title}' -> {parents:?}"));
            }
        }
    }
    eprintln!(
        "AB arm={arm} run={run} mode={} secs={secs:.0} completions={} tool_calls={} \
         new_topics={} duplicates={duplicates} rows={rows} wrong_attaches={} topics={:?} wrong={:?}",
        outcome.mode,
        counting.completions.load(Ordering::SeqCst),
        counting.tool_calls.load(Ordering::SeqCst),
        new_topics.len(),
        wrong.len(),
        new_topics.iter().map(|(_, t)| t).collect::<Vec<_>>(),
        wrong,
    );
}

#[tokio::test(flavor = "multi_thread")]
#[ignore = "llm-e2e"]
async fn ab_single_shot_and_harness_on_one_batch() {
    add_model().await;
    let runs: usize = std::env::var("AB_RUNS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1);
    let arms = std::env::var("AB_ARMS").unwrap_or_else(|_| "A,B".into());
    for run in 1..=runs {
        for arm in arms.split(',') {
            one_pass(arm.trim(), run).await;
        }
    }
}

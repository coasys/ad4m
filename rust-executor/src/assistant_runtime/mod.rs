//! Server-side AI-assistant run subsystem.
//!
//! Runs assistant turns inside the executor — persistent, concurrent, and
//! surviving client disconnect. It watches perspectives for new completed user
//! messages (the WE `Message` subject class), runs the context → model → tools
//! → persist loop, and writes replies back into the perspective as `we://`
//! links so WE re-renders live.
//!
//! Boot: [`start`] is spawned as a subsystem thread beside the MCP + REST
//! servers in `lib.rs`, gated on `config.enable_assistants` (default on). It
//! resumes interrupted runs from durable `RunState`/message links, then watches
//! `PERSPECTIVE_LINK_ADDED_TOPIC` for new turns.
//!
//! Layout:
//! * [`entities`] — the WE subject-class model (pure link (de)serialisation).
//! * [`store`]    — perspective read/write helpers.
//! * [`sdna`]     — SDNA subject-class registration (bootstrap).
//! * [`context`]  — context assembly for `prompt_messages` (pure).
//! * [`tools`]    — built-in graph tools + the MCP-client follow-up seam.
//! * [`run`]      — the per-thread loop.
//! * [`registry`] — one live run per thread.

pub mod context;
pub mod entities;
pub mod registry;
pub mod run;
pub mod sdna;
pub mod store;
pub mod tools;

use std::collections::{HashMap, HashSet};
use std::sync::{Mutex, OnceLock};

use tokio::sync::broadcast::error::RecvError;

use crate::agent::AgentContext;
use crate::perspectives::{all_perspectives, get_perspective};
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_LINK_ADDED_TOPIC};
use crate::types::PerspectiveLinkWithOwner;

use entities::{Message, CLASS_ASSISTANT, CLASS_MESSAGE, CLASS_THREAD};
use registry::RunRegistry;
use run::RunInput;

/// Perspectives for which SDNA subject classes have already been ensured this
/// process (avoids repeated SDNA writes).
static ENSURED: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();

fn ensured_set() -> &'static Mutex<HashSet<String>> {
    ENSURED.get_or_init(|| Mutex::new(HashSet::new()))
}

/// Entry point — never returns. Bootstraps, then watches for new turns.
pub async fn start() {
    log::info!("assistant_runtime: starting");
    let registry = RunRegistry::new();

    // Give the executor a moment to finish loading perspectives before the
    // boot-time resume scan; the live watcher below catches everything after.
    tokio::time::sleep(std::time::Duration::from_secs(5)).await;

    bootstrap(&registry).await;

    let pubsub = get_global_pubsub().await;
    let mut rx = pubsub.subscribe(&PERSPECTIVE_LINK_ADDED_TOPIC).await;
    log::info!("assistant_runtime: watching for new assistant turns");

    loop {
        match rx.recv().await {
            Ok(payload) => {
                if let Some(uuid) = we_perspective_of(&payload) {
                    let reg = registry.clone();
                    tokio::spawn(async move {
                        scan_perspective(&uuid, &reg).await;
                    });
                }
            }
            Err(RecvError::Lagged(n)) => {
                log::warn!("assistant_runtime: lagged {n} link events");
            }
            Err(RecvError::Closed) => {
                log::warn!("assistant_runtime: link event channel closed; stopping watcher");
                break;
            }
        }
    }
}

/// Parse a `PERSPECTIVE_LINK_ADDED_TOPIC` payload and return the perspective
/// uuid iff the added link is a `we://` link (i.e. WE activity we should scan).
fn we_perspective_of(payload: &str) -> Option<String> {
    let evt: PerspectiveLinkWithOwner = serde_json::from_str(payload).ok()?;
    let predicate = evt.link.data.predicate.as_deref().unwrap_or("");
    if predicate.starts_with("we://") {
        Some(evt.perspective_uuid)
    } else {
        None
    }
}

/// On boot: register SDNA classes where WE is active, pick up unanswered user
/// turns, and resume interrupted runs from durable `RunState`.
async fn bootstrap(registry: &RunRegistry) {
    for p in all_perspectives() {
        let uuid = p.uuid.clone();
        maybe_ensure_classes(&uuid).await;
        scan_perspective(&uuid, registry).await;
    }

    // Explicitly resume runs whose durable RunState is still active but whose
    // thread was left mid-stream (a trailing streaming assistant message, which
    // the user-turn trigger alone would not re-fire).
    for (perspective_uuid, rs) in store::active_run_states().await {
        log::info!(
            "assistant_runtime: resuming run for thread {} (status {})",
            rs.thread_id,
            rs.status
        );
        registry
            .start(RunInput {
                perspective_uuid,
                thread_id: rs.thread_id,
            })
            .await;
    }
}

/// Scan a perspective's threads and start a run for any that has a trailing
/// completed user turn or a half-written (interrupted) assistant reply.
async fn scan_perspective(perspective_uuid: &str, registry: &RunRegistry) {
    let Some(conv) = get_perspective(perspective_uuid) else {
        return;
    };

    // Group every message by thread.
    let mut by_thread: HashMap<String, Vec<Message>> = HashMap::new();
    for base in store::message_bases(&conv).await {
        let msg = store::load_message(&conv, &base).await;
        if msg.thread_id.is_empty() {
            continue;
        }
        by_thread.entry(msg.thread_id.clone()).or_default().push(msg);
    }

    for (thread_id, mut msgs) in by_thread {
        msgs.sort_by(|a, b| a.ts.cmp(&b.ts));
        let should_run =
            entities::trigger_message(&msgs).is_some() || entities::needs_resume(&msgs);
        if should_run {
            registry
                .start(RunInput {
                    perspective_uuid: perspective_uuid.to_string(),
                    thread_id,
                })
                .await;
        }
    }
}

/// Register the WE SDNA subject classes in a perspective the first time we see
/// WE activity there. No-op if already ensured this process or if the classes
/// are already present (e.g. published by WE with SHACL).
async fn maybe_ensure_classes(perspective_uuid: &str) {
    {
        let guard = ensured_set().lock().unwrap();
        if guard.contains(perspective_uuid) {
            return;
        }
    }

    let Some(mut p) = get_perspective(perspective_uuid) else {
        return;
    };

    // Only touch perspectives that actually carry WE instances.
    let has_we = !store::find_instances(&p, CLASS_ASSISTANT).await.is_empty()
        || !store::find_instances(&p, CLASS_THREAD).await.is_empty()
        || !store::find_instances(&p, CLASS_MESSAGE).await.is_empty();
    if !has_we {
        return;
    }

    let ctx = AgentContext::main_agent();
    sdna::ensure_subject_classes(&mut p, &ctx).await;

    ensured_set()
        .lock()
        .unwrap()
        .insert(perspective_uuid.to_string());
}

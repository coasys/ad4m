//! Step signals for the auto-processor (P-B2c).
//!
//! The watch loop and [`super::watcher::run_one_pass`] publish an
//! [`AutoProcessorEvent`] at each meaningful step of a pass onto the global
//! [`AUTO_PROCESSOR_EVENT_TOPIC`] PubSub topic. This is the same mechanism
//! every GraphQL subscription already rides, so:
//!   * **tests** can `subscribe()` and `await` a specific step instead of
//!     polling the graph or sleeping — the whole pass is observable without
//!     manually driving interpretation, and
//!   * **the WebSocket layer (#881)** can forward the topic to clients as a
//!     GraphQL subscription for near-free, so a Flux-style UI can show
//!     "collecting → running LLM → done" and await the next batch.
//!
//! Emission is fire-and-forget: a pass never fails or blocks because nobody is
//! listening (`broadcast::Sender::send` drops silently with no receivers).

use crate::pubsub::{
    get_global_pubsub, AUTO_PROCESSOR_EVENT_TOPIC, AUTO_PROCESSOR_NEIGHBOURHOOD_STATE_TOPIC,
};
use serde::{Deserialize, Serialize};
use tokio::sync::broadcast;

/// One step in an auto-processor pass. Ordered roughly by lifecycle; a single
/// pass emits a subset (e.g. `BatchReady → BackedOff` when a peer already holds
/// the claim, or `BatchReady → Claimed → GatheringTranscript →
/// RunningInterpretation → Processed` for the peer that wins).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum AutoProcessorStep {
    /// A debounced batch reached its threshold and a pass is about to run.
    BatchReady,
    /// This peer won the processing claim for the batch — it will process.
    Claimed,
    /// Another peer holds an active claim; this peer backed off (their write
    /// reaches us via link sync). `detail` = the holder's DID.
    BackedOff,
    /// No author of the batch is currently online; nobody processes this round
    /// (the "only participants process" rule). Waits for an author to return.
    AwaitingAuthor,
    /// An online author earlier in message order was elected; this peer stood
    /// down before claiming. `detail` = the elected author's DID.
    NotCandidate,
    /// Gathering the batch transcript via the processor's SPARQL scope.
    GatheringTranscript,
    /// Running the LLM interpretation over the gathered transcript.
    RunningInterpretation,
    /// The pass completed and wrote `bases` (the created/updated instance
    /// URIs; may be empty if the model proposed nothing new).
    Processed,
    /// Won the claim, but a configured class shape had not synced yet — the
    /// pass was skipped and will retry once the SDNA lands.
    ShapesMissing,
    /// Won the claim, but the batch transcript was empty — nothing to interpret.
    EmptyTranscript,
}

/// A single step-signal from one auto-processor pass on one perspective.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AutoProcessorEvent {
    /// UUID of the perspective the processor runs on.
    pub perspective_uuid: String,
    /// The processor's id (`AutoProcessorConfig::processor_id`).
    pub processor_id: String,
    /// DID of the agent this executor ran the pass as — lets a multi-user /
    /// multi-executor observer see *which* peer claimed/processed vs backed off.
    #[serde(default)]
    pub agent_did: Option<String>,
    /// Which step this event marks.
    pub step: AutoProcessorStep,
    /// The batch's source item ids (present from `BatchReady` onward).
    #[serde(default)]
    pub item_ids: Vec<String>,
    /// Instance base URIs written by the pass (present on `Processed`).
    #[serde(default)]
    pub bases: Vec<String>,
    /// Free-form context for the step (a holder/elected DID, an error, …).
    #[serde(default)]
    pub detail: Option<String>,
    /// Live-debug raw LLM prompt for this pass. Present on `Processed` only
    /// when the processor was configured with `AutoProcessorConfig.debug_mode
    /// = true`. Absent (`None`) in the normal path — LLM prompts are 10s of
    /// KB and would otherwise inflate every event.
    #[serde(default)]
    pub llm_input: Option<String>,
    /// Live-debug raw LLM response for this pass. Same rules as `llm_input`.
    #[serde(default)]
    pub llm_output: Option<String>,
}

impl AutoProcessorEvent {
    pub fn new(perspective_uuid: &str, processor_id: &str, step: AutoProcessorStep) -> Self {
        Self {
            perspective_uuid: perspective_uuid.to_string(),
            processor_id: processor_id.to_string(),
            agent_did: None,
            step,
            item_ids: Vec::new(),
            bases: Vec::new(),
            detail: None,
            llm_input: None,
            llm_output: None,
        }
    }
    pub fn with_agent_did(mut self, did: &str) -> Self {
        self.agent_did = Some(did.to_string());
        self
    }
    pub fn with_items(mut self, item_ids: &[String]) -> Self {
        self.item_ids = item_ids.to_vec();
        self
    }
    pub fn with_bases(mut self, bases: &[String]) -> Self {
        self.bases = bases.to_vec();
        self
    }
    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }
    /// Attach live-debug LLM I/O — only set from a pass whose
    /// `AutoProcessorConfig.debug_mode` is `true`. Payload sizes are large
    /// (10s of KB); the wire-level DID filter (Nico's 2026-08-19 call) keeps
    /// this from leaking to observers who did not run the pass.
    pub fn with_llm_io(mut self, input: String, output: String) -> Self {
        self.llm_input = Some(input);
        self.llm_output = Some(output);
        self
    }
}

/// Publish an event on the global auto-processor topic. Fire-and-forget: never
/// errors, never blocks a pass on the absence of listeners.
pub async fn emit(event: AutoProcessorEvent) {
    match serde_json::to_string(&event) {
        Ok(json) => {
            get_global_pubsub()
                .await
                .publish(&AUTO_PROCESSOR_EVENT_TOPIC, &json)
                .await;
        }
        Err(e) => log::warn!("auto_processor::events: failed to serialize event: {e:#}"),
    }
}

/// Raw subscription to the auto-processor event topic (JSON strings). The WS
/// layer and typed helpers build on this.
pub async fn subscribe() -> broadcast::Receiver<String> {
    get_global_pubsub()
        .await
        .subscribe(&AUTO_PROCESSOR_EVENT_TOPIC)
        .await
}

/// Test/consumer helper: block until an [`AutoProcessorEvent`] satisfying
/// `pred` is published, or `timeout` elapses. Returns the matching event, or
/// `None` on timeout. Deserialization failures and lagged messages are skipped.
pub async fn next_event_matching<F>(
    rx: &mut broadcast::Receiver<String>,
    timeout: std::time::Duration,
    mut pred: F,
) -> Option<AutoProcessorEvent>
where
    F: FnMut(&AutoProcessorEvent) -> bool,
{
    let deadline = tokio::time::Instant::now() + timeout;
    loop {
        let remaining = deadline.saturating_duration_since(tokio::time::Instant::now());
        if remaining.is_zero() {
            return None;
        }
        match tokio::time::timeout(remaining, rx.recv()).await {
            Ok(Ok(json)) => {
                if let Ok(ev) = serde_json::from_str::<AutoProcessorEvent>(&json) {
                    if pred(&ev) {
                        return Some(ev);
                    }
                }
            }
            // Lagged: keep going. Closed / timed-out: give up.
            Ok(Err(broadcast::error::RecvError::Lagged(_))) => continue,
            Ok(Err(broadcast::error::RecvError::Closed)) => return None,
            Err(_) => return None,
        }
    }
}

// ── Neighbourhood state event (Nico 2026-08-19) ────────────────────────────
//
// Small, high-signal event that fires when THIS executor claims or finishes
// a batch. Perspective-scoped (delivered to anyone with read access), no
// batch payload — the point is "someone is auto-processing", not what they
// are processing. Cross-executor sync via Holochain is NOT covered here;
// clients that need to see peer claims from other nodes subscribe to
// `link-added` and filter for the `has_claim` predicate.

/// Coarse-grained pass phase for the observability stream. Distinct from
/// `AutoProcessorStep` — that has 10 fine-grained steps for the pass owner;
/// this has just the two transitions a neighbour cares about.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum NeighbourhoodPhase {
    /// This executor wrote a `ProcessingClaim` — a pass has started on this
    /// node.
    Claimed,
    /// The pass this executor claimed has completed successfully.
    Finished,
    /// The pass this executor started did NOT commit — either short-circuited
    /// (missing shape / empty batch) or errored out; the claim will TTL-expire.
    Abandoned,
}

/// One observation of an auto-processor pass by THIS executor. Small on
/// purpose: perspective + processor + claimant DID + batch key + phase.
/// Consumers merge these across ticks + across peers (via link-added on
/// `has_claim`) to render "who is currently processing what."
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AutoProcessorNeighbourhoodState {
    /// UUID of the perspective the pass runs on.
    pub perspective_uuid: String,
    /// The processor's id (`AutoProcessorConfig::processor_id`).
    pub processor_id: String,
    /// DID that claimed the batch — the executor's own DID for locally
    /// initiated passes.
    pub claimant_did: String,
    /// Content hash of the batch (see [`super::claim::batch_key`]) — lets a
    /// consumer merge `Claimed` and `Finished` events for the same batch
    /// without duplicating rows.
    pub batch_key: String,
    /// Which lifecycle transition this event marks.
    pub phase: NeighbourhoodPhase,
}

impl AutoProcessorNeighbourhoodState {
    pub fn new(
        perspective_uuid: &str,
        processor_id: &str,
        claimant_did: &str,
        batch_key: &str,
        phase: NeighbourhoodPhase,
    ) -> Self {
        Self {
            perspective_uuid: perspective_uuid.to_string(),
            processor_id: processor_id.to_string(),
            claimant_did: claimant_did.to_string(),
            batch_key: batch_key.to_string(),
            phase,
        }
    }
}

/// Publish a neighbourhood-state event on the dedicated topic.
/// Fire-and-forget, same rules as [`emit`].
pub async fn emit_neighbourhood_state(event: AutoProcessorNeighbourhoodState) {
    match serde_json::to_string(&event) {
        Ok(json) => {
            get_global_pubsub()
                .await
                .publish(&AUTO_PROCESSOR_NEIGHBOURHOOD_STATE_TOPIC, &json)
                .await;
        }
        Err(e) => {
            log::warn!("auto_processor::events: failed to serialize neighbourhood state: {e:#}")
        }
    }
}

/// Raw subscription to the neighbourhood-state topic (JSON strings).
pub async fn subscribe_neighbourhood_state() -> broadcast::Receiver<String> {
    get_global_pubsub()
        .await
        .subscribe(&AUTO_PROCESSOR_NEIGHBOURHOOD_STATE_TOPIC)
        .await
}

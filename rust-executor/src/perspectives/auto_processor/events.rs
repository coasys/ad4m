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

use crate::pubsub::{get_global_pubsub, AUTO_PROCESSOR_EVENT_TOPIC};
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

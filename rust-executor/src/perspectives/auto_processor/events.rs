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
    /// The LLM prompt has been built and dispatched — the pass is now
    /// waiting on the model. Emitted only when the processor has
    /// `emit_debug_events: true`; carries the prompt on `llm_input`.
    /// Paired with [`AutoProcessorStep::LlmResponseReceived`] so a UI
    /// can render a "waiting on LLM" state between the two events (LLM
    /// calls take seconds-to-minutes on local models).
    LlmRequestSent,
    /// The LLM response has arrived — the pass is about to plan and
    /// commit writes. Emitted only when the processor has
    /// `emit_debug_events: true`; carries the response on `llm_output`.
    LlmResponseReceived,
    /// The harness LLM issued a tool call. Emitted mid-loop (once per
    /// tool_call in the round), carries `tool_name` + `tool_args_json` so
    /// a UI can render "LLM asked for X with these args" live, before the
    /// tool has returned. Fires only from the tool-calling harness path
    /// (`AutoProcessorConfig.max_tool_calls > 0`) AND only when the
    /// processor has `emit_debug_events: true`.
    ToolCall,
    /// The tool dispatched by the harness returned. Emitted right after
    /// `ToolCall`; carries `tool_name` + `tool_result` (truncated to a
    /// bounded prefix, so a large query result doesn't inflate every
    /// event). Same gating as `ToolCall`.
    ToolResult,
    /// The pass completed and wrote `bases` (the created/updated instance
    /// URIs; may be empty if the model proposed nothing new).
    Processed,
    /// Won the claim, but a configured class shape had not synced yet — the
    /// pass was skipped and will retry once the SDNA lands.
    ShapesMissing,
    /// Won the claim, but the batch transcript was empty — nothing to interpret.
    EmptyTranscript,
    /// The pass ran and did not complete: the model errored, the provider
    /// timed out, or a write failed. `detail` carries the reason.
    ///
    /// Distinct from [`AutoProcessorStep::EmptyTranscript`] and
    /// [`AutoProcessorStep::ShapesMissing`], which are the pass correctly
    /// deciding there is nothing to do. Those are answers; this is a
    /// failure, and a UI reporting "nothing to extract" when the LLM
    /// endpoint was unreachable sends someone looking at their transcript
    /// instead of at their model configuration.
    Failed,
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
    /// Content hash of the batch — the same
    /// [`super::claim::batch_key`] value the perspective-scoped
    /// [`AutoProcessorNeighbourhoodState`] carries.
    ///
    /// Present from `BatchReady` onward, and it is what makes the two
    /// streams joinable. Without it a consumer holding both has no way to
    /// say "this `LlmRequestSent` belongs to the row I opened on that
    /// `Claimed`" — the fine-grained stream carries `item_ids` and the
    /// neighbourhood stream carries only the hash of them, so correlating
    /// meant re-implementing this hash client-side and matching the Rust
    /// serialization exactly. A consumer that merges a peer's coarse
    /// phases with its own fine ones needs one shared key, and this is it.
    #[serde(default)]
    pub batch_key: Option<String>,
    /// Instance base URIs written by the pass (present on `Processed`).
    #[serde(default)]
    pub bases: Vec<String>,
    /// Free-form context for the step (a holder/elected DID, an error, …).
    #[serde(default)]
    pub detail: Option<String>,
    /// Live-debug raw LLM prompt for this pass. Present ONLY on
    /// [`AutoProcessorStep::LlmRequestSent`] events, and only when the
    /// processor was configured with
    /// `AutoProcessorConfig.emit_debug_events = true`. Never carried on
    /// `Processed` (which carries `bases` only). Absent (`None`) in the
    /// normal path — LLM prompts are 10s of KB and would otherwise
    /// inflate every event.
    #[serde(default)]
    pub llm_input: Option<String>,
    /// Live-debug raw LLM response for this pass. Present ONLY on
    /// [`AutoProcessorStep::LlmResponseReceived`] events, and only when
    /// the processor was configured with
    /// `AutoProcessorConfig.emit_debug_events = true`. Never carried on
    /// `Processed`. Same size / privacy rules as `llm_input`.
    #[serde(default)]
    pub llm_output: Option<String>,
    /// Name of the tool the harness LLM invoked (or that just returned).
    /// Present on [`AutoProcessorStep::ToolCall`] +
    /// [`AutoProcessorStep::ToolResult`] events; absent on every other
    /// step. Present only when the processor has `emit_debug_events: true`.
    #[serde(default)]
    pub tool_name: Option<String>,
    /// JSON-encoded arguments the LLM sent to the tool. Present ONLY on
    /// [`AutoProcessorStep::ToolCall`]; absent on `ToolResult` (where
    /// `tool_result` carries the return text instead). Same size / gating
    /// rules as `llm_input` / `llm_output`.
    #[serde(default)]
    pub tool_args_json: Option<String>,
    /// The tool's return text. Present ONLY on
    /// [`AutoProcessorStep::ToolResult`]; may be truncated by the emitter
    /// when the tool returns a large payload (e.g. a `_query` result).
    /// Same size / gating rules as `llm_input` / `llm_output`.
    #[serde(default)]
    pub tool_result: Option<String>,
}

impl AutoProcessorEvent {
    pub fn new(perspective_uuid: &str, processor_id: &str, step: AutoProcessorStep) -> Self {
        Self {
            perspective_uuid: perspective_uuid.to_string(),
            processor_id: processor_id.to_string(),
            agent_did: None,
            step,
            item_ids: Vec::new(),
            batch_key: None,
            bases: Vec::new(),
            detail: None,
            llm_input: None,
            llm_output: None,
            tool_name: None,
            tool_args_json: None,
            tool_result: None,
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
    /// Tag this event with the batch it belongs to, so a consumer can join
    /// it to the perspective-scoped neighbourhood stream. Callers pass the
    /// [`super::claim::batch_key`] of the same `item_ids` — derived once
    /// per pass rather than recomputed per event, since the hash is over a
    /// set that does not change mid-pass.
    pub fn with_batch_key(mut self, batch_key: &str) -> Self {
        self.batch_key = Some(batch_key.to_string());
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
    /// `AutoProcessorConfig.emit_debug_events` is `true`. Payload sizes are
    /// large (10s of KB); the wire-level DID filter (Nico's 2026-08-19 call)
    /// keeps this from leaking to observers who did not run the pass.
    pub fn with_llm_io(mut self, input: String, output: String) -> Self {
        self.llm_input = Some(input);
        self.llm_output = Some(output);
        self
    }
    /// Attach only the LLM prompt — used for
    /// [`AutoProcessorStep::LlmRequestSent`] so a UI can render the
    /// dispatched prompt without waiting for the response.
    pub fn with_llm_input(mut self, input: String) -> Self {
        self.llm_input = Some(input);
        self
    }
    /// Attach only the LLM response — used for
    /// [`AutoProcessorStep::LlmResponseReceived`] so a UI can render the
    /// raw model output as soon as it lands, before the planner + writes
    /// finish.
    pub fn with_llm_output(mut self, output: String) -> Self {
        self.llm_output = Some(output);
        self
    }
    /// Attach a tool name + JSON-encoded args — used for
    /// [`AutoProcessorStep::ToolCall`] so a UI can render "LLM asked for
    /// `<name>`" live, before the tool returns.
    pub fn with_tool_call(mut self, name: &str, args_json: String) -> Self {
        self.tool_name = Some(name.to_string());
        self.tool_args_json = Some(args_json);
        self
    }
    /// Attach a tool name + return text — used for
    /// [`AutoProcessorStep::ToolResult`] so a UI can render what the tool
    /// answered. `result` may be pre-truncated by the emitter (query tools
    /// can return sizable payloads).
    pub fn with_tool_result(mut self, name: &str, result: String) -> Self {
        self.tool_name = Some(name.to_string());
        self.tool_result = Some(result);
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

/// Everything the interpretation engine needs to emit its own
/// `AutoProcessorEvent`s (`LlmRequestSent`, `LlmResponseReceived`) without
/// pulling in the watcher's context. Populated by the watcher and passed
/// down when a processor has `emit_debug_events: true`; `None` skips all
/// engine-side emissions.
///
/// The engine emits directly here (via [`emit`]) rather than via a
/// callback / channel to avoid the sync/async closure gymnastics — the
/// only cost is a compile-time dep on this module, which is acceptable
/// because live-debug telemetry IS an auto-processor concern.
#[derive(Debug, Clone)]
pub struct InterpretationEmitContext {
    pub perspective_uuid: String,
    pub processor_id: String,
    pub agent_did: String,
    pub item_ids: Vec<String>,
    /// The pass's batch key, so the mid-pass LLM events join to the same
    /// row as the watcher's own signals. Carried here rather than
    /// recomputed from `item_ids` because the caller already has it, and
    /// because the one-shot path (which has no claim and no batch) supplies
    /// a synthetic key instead — see `run_interpretation_handler`.
    pub batch_key: String,
}

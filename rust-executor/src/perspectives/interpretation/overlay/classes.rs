//! Hard-wired subject classes for the provenance overlay + per-pass run node,
//! their idempotent registration, and run-node minting.

use crate::agent::AgentContext;
use crate::perspectives::hardwired_class::ensure_subject_class;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::AITask;
use sha2::{Digest, Sha256};

/// Local subject-class name of the per-pass run node.
pub(crate) const INTERP_RUN_CLASS: &str = "InterpretationRun";
/// Local subject-class name of the per-base overlay node.
pub(crate) const INTERP_OVERLAY_CLASS: &str = "InterpretationOverlay";

/// Target-class URI of [`INTERP_OVERLAY_CLASS`] — used to detect prior
/// registration.
const INTERP_OVERLAY_TARGET_CLASS: &str = "ad4m://InterpretationOverlay";
/// Target-class URI of [`INTERP_RUN_CLASS`] — used to detect prior registration.
const INTERP_RUN_TARGET_CLASS: &str = "ad4m://InterpretationRun";

/// Hard-wired SDNA for the [`INTERP_RUN_CLASS`] subject class. No dedicated
/// `ad4m://type` flag — Nico 2026-08-19: "type flags are an anti-pattern
/// for subject classes; match over all the properties instead." Conformance
/// is by the presence of `run_id` (identity), same pattern
/// `InterpretationOverlay` already uses (`kind` is its discriminator).
/// None of the scalars use `resolveLanguage` — they are deterministic
/// `literal:string:` targets, which keeps provenance stable and cheaply
/// decodable (no signed-envelope round-trip).
// See auto_processor::config for why the SDNA blobs are external JSON files
// loaded via `include_str!`. #903 adds `debug_prompt` + `debug_response`
// scalar properties to the JSON side of the parity pair.
const INTERP_RUN_SDNA: &str = include_str!("../../hardwired_sdna/interpretation_run.json");

/// AutoProcessor cursor extras on an [`InterpretationRun`]: the processor
/// instance URI (`ad4m://autoprocessor/<id>`) and the turn IDs this pass
/// consumed. One-shot / manual interpretation omits this — those runs do not
/// participate in the processed-turn cursor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterpretationRunCursor {
    pub processor: String,
    pub sources: Vec<String>,
}

/// Hard-wired SDNA for the [`INTERP_OVERLAY_CLASS`] subject class.
///
/// The overlay carries **no dedicated type flag**: the mandatory `kind`
/// (`create` | `update`) is the class discriminator, so the constructor mints a
/// `kind` link (a placeholder value that the per-instance `kind` setter always
/// overwrites in the same `create_subject` call). This keeps exactly one
/// identifying link on the base instead of a redundant `type` + `kind` pair.
/// `run` carries no `resolveLanguage` so its value (the run node's URI) is stored
/// as a plain link target rather than literal-encoded. The dynamic `inferred/<p>`
/// links are NOT declared here — their predicates vary per instance, so they are
/// written directly as parallel links (see [`super::write::write_overlay`]).
const INTERP_OVERLAY_SDNA: &str = include_str!("../../hardwired_sdna/interpretation_overlay.json");

/// Identity + provenance for one interpretation pass — minted once per
/// [`crate::perspectives::interpretation::run_interpretation`] call and threaded
/// onto every overlay that pass writes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct InterpretationRunMeta {
    pub run_id: String,
    pub model: String,
    /// `sha256(system_prompt + few-shot examples)` — the versioned identity of
    /// the prompt that produced this pass's inferences.
    pub prompt_version: String,
    pub ran_at: String,
    /// Optional live-debug prompt string persisted on the run when the caller
    /// enables observability (AutoProcessor `emitDebugEvents`). `None` in the
    /// normal path — LLM prompts are large and syncing them across a
    /// neighbourhood by default would blow the shared-graph payload.
    pub debug_prompt: Option<String>,
    /// Optional live-debug response string, same rules as `debug_prompt`.
    pub debug_response: Option<String>,
}

impl InterpretationRunMeta {
    /// Derive a pass's provenance from the registered interpretation task plus a
    /// caller-supplied `run_id`/`ran_at` (kept as params so this stays pure and
    /// unit-testable without a clock). `prompt_version` hashes the system prompt
    /// and every few-shot example, so any prompt change yields a new version.
    pub(crate) fn from_task(task: &AITask, run_id: String, ran_at: String) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(task.system_prompt.as_bytes());
        for ex in &task.prompt_examples {
            hasher.update(b"\x1fin\x1f");
            hasher.update(ex.input.as_bytes());
            hasher.update(b"\x1fout\x1f");
            hasher.update(ex.output.as_bytes());
        }
        let prompt_version = hex::encode(hasher.finalize());
        Self {
            run_id,
            model: task.model_id.clone(),
            prompt_version,
            ran_at,
            debug_prompt: None,
            debug_response: None,
        }
    }
}

/// Idempotently register the hard-wired [`INTERP_RUN_CLASS`] +
/// [`INTERP_OVERLAY_CLASS`] subject classes into the perspective, mirroring the
/// `add_sdna` path the SoA classes use. A no-op once the overlay class is
/// present *and* the run class has the cursor properties (`processor` /
/// `sources`); otherwise the run class is refreshed so a perspective that
/// registered an older SDNA picks up the new setters.
pub(crate) async fn ensure_interpretation_overlay_classes(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_subject_class(
        perspective,
        INTERP_OVERLAY_CLASS,
        INTERP_OVERLAY_TARGET_CLASS,
        INTERP_OVERLAY_SDNA,
        None,
        context,
    )
    .await?;
    // `debug_response` is the newest property on the run class, so a
    // perspective that registered the pre-debug SDNA is refreshed rather
    // than left with a shape whose `debug_prompt`/`debug_response` setters
    // do not exist — `write_processor` would silently drop those values.
    ensure_subject_class(
        perspective,
        INTERP_RUN_CLASS,
        INTERP_RUN_TARGET_CLASS,
        INTERP_RUN_SDNA,
        Some("ad4m://interp/debug_response"),
        context,
    )
    .await
}

/// Mint the per-pass [`INTERP_RUN_CLASS`] node in the `ad4m://interp/run/`
/// coordination namespace and return its URI (to thread onto the pass's overlays
/// as `run`). It lives *outside* the interpreted data tree — like the
/// auto-processor's `ad4m://claim/…` nodes — so it never clutters the SoA graph;
/// it is reached only by traversal from each affected base's overlay `run` link.
///
/// `batch_id` groups the run-node write with the pass's overlay writes so a
/// partial Phase 3 failure rolls back atomically (see the guarded batch in
/// `super::gate_apply_and_persist`). Test helpers that only need the run node
/// on its own can pass `None`.
pub(crate) async fn mint_interpretation_run(
    perspective: &mut PerspectiveInstance,
    meta: &InterpretationRunMeta,
    cursor: Option<&InterpretationRunCursor>,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<String> {
    let run_uri = format!("ad4m://interp/run/{}", meta.run_id);
    let mut values = serde_json::json!({
        "runId": meta.run_id,
        "model": meta.model,
        "promptVersion": meta.prompt_version,
        "ranAt": meta.ran_at,
    });
    let mut rest_sources: Vec<String> = Vec::new();
    if let Some(c) = cursor {
        values["processor"] = c.processor.clone().into();
        if let Some((first, rest)) = c.sources.split_first() {
            values["sources"] = first.clone().into();
            rest_sources.extend(rest.iter().cloned());
        }
    }
    // Debug-mode: persist raw LLM I/O onto the run so a UI can look it up
    // post-hoc, not just via the live `Processed` event (which a slow client
    // could miss). Omitted when the caller left them `None` — the normal
    // non-debug pass.
    if let Some(prompt) = &meta.debug_prompt {
        values["debugPrompt"] = prompt.clone().into();
    }
    if let Some(response) = &meta.debug_response {
        values["debugResponse"] = response.clone().into();
    }
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(INTERP_RUN_CLASS.to_string()),
                query: None,
            },
            run_uri.clone(),
            Some(values),
            batch_id.clone(),
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("mint_interpretation_run: create_subject failed: {e:#}"))?;
    // `create_subject` applies one value per property; remaining collection
    // members go through the same `addLink` setter one at a time. Threaded on
    // the same `batch_id` so the whole run mint (initial values + follow-on
    // source-id bumps) commits atomically with the pass's overlay writes.
    for id in rest_sources {
        perspective
            .update_subject(
                SubjectClassOption {
                    class_name: Some(INTERP_RUN_CLASS.to_string()),
                    query: None,
                },
                run_uri.clone(),
                serde_json::json!({ "sources": id }),
                batch_id.clone(),
                context,
            )
            .await
            .map_err(|e| {
                anyhow::anyhow!("mint_interpretation_run: update_subject(sources) failed: {e:#}")
            })?;
    }
    Ok(run_uri)
}

// SDNA-parity tests live TS-side in
// `tests/js/tests/model/interpretation-models.test.ts` — they read the same
// `hardwired_sdna/*.json` files this module `include_str!`s and compare the
// (path, name) pairs against `@Model.generateSHACL().shape.properties`. A
// hardcoded Rust-side reference set here would fork the source of truth
// (2026-08-20 bug: paths matched, names diverged, both Rust and TS parity
// tests passed while `create_subject` writes silently no-op'd).

//! Hard-wired subject classes for the provenance overlay + per-pass run node,
//! their idempotent registration, and run-node minting.

use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::{
    PerspectiveInstance, SdnaType, SubjectClassOption,
};
use crate::types::{AITask, LinkQuery};
use sha2::{Digest, Sha256};

/// Local subject-class name of the per-pass run node.
pub(crate) const INTERP_RUN_CLASS: &str = "InterpretationRun";
/// Local subject-class name of the per-base overlay node.
pub(crate) const INTERP_OVERLAY_CLASS: &str = "InterpretationOverlay";

/// Target-class URI of [`INTERP_OVERLAY_CLASS`] — used to detect prior
/// registration.
const INTERP_OVERLAY_TARGET_CLASS: &str = "ad4m://InterpretationOverlay";

/// Hard-wired SDNA for the [`INTERP_RUN_CLASS`] subject class. Mirrors the
/// interpretation SoA fixtures' SHACL shape: a `type` flag plus literal scalars.
/// None of the scalars use `resolveLanguage` — they are deterministic
/// `literal:string:` targets, which keeps provenance stable and cheaply
/// decodable (no signed-envelope round-trip).
const INTERP_RUN_SDNA: &str = r#"{
  "target_class":"ad4m://InterpretationRun",
  "interpretation_hint":"One interpretation pass: the model + prompt version that wrote a batch of inferred data.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ad4m://type","target":"ad4m://interpretation-run"}],
  "properties":[
    {"path":"ad4m://type","name":"type","has_value":"ad4m://interpretation-run","min_count":1,"max_count":1},
    {"path":"ad4m://interp/run_id","name":"run_id","identity":true,"min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://interp/run_id","target":"value"}]},
    {"path":"ad4m://interp/model","name":"model","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://interp/model","target":"value"}]},
    {"path":"ad4m://interp/prompt_version","name":"prompt_version","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://interp/prompt_version","target":"value"}]},
    {"path":"ad4m://interp/ran_at","name":"ran_at","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://interp/ran_at","target":"value"}]}
  ]
}"#;

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
const INTERP_OVERLAY_SDNA: &str = r#"{
  "target_class":"ad4m://InterpretationOverlay",
  "interpretation_hint":"Provenance overlay marking an instance as LLM-inferred, with the last-inferred value snapshot.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ad4m://interp/kind","target":"create"}],
  "properties":[
    {"path":"ad4m://interp/kind","name":"kind","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://interp/kind","target":"value"}]},
    {"path":"ad4m://interp/run","name":"run","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://interp/run","target":"value"}]}
  ]
}"#;

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
        }
    }
}

/// Idempotently register the hard-wired [`INTERP_RUN_CLASS`] +
/// [`INTERP_OVERLAY_CLASS`] subject classes into the perspective, mirroring the
/// `add_sdna` path the SoA classes use. A no-op once the overlay class is
/// present, so a continuous processor calling it every pass costs one cheap
/// link scan rather than a SHACL rewrite.
pub(crate) async fn ensure_interpretation_overlay_classes(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    if overlay_classes_present(perspective).await? {
        return Ok(());
    }
    perspective
        .add_sdna(
            INTERP_RUN_CLASS.to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(INTERP_RUN_SDNA.to_string()),
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("ensure overlay classes: add_sdna(run) failed: {e:#}"))?;
    perspective
        .add_sdna(
            INTERP_OVERLAY_CLASS.to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(INTERP_OVERLAY_SDNA.to_string()),
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("ensure overlay classes: add_sdna(overlay) failed: {e:#}"))?;
    Ok(())
}

/// True once the overlay target class has been registered as a SubjectClass in
/// this perspective.
async fn overlay_classes_present(perspective: &PerspectiveInstance) -> anyhow::Result<bool> {
    let links = perspective
        .get_links(&LinkQuery {
            predicate: Some("rdf://type".to_string()),
            target: Some("ad4m://SubjectClass".to_string()),
            ..Default::default()
        })
        .await?;
    Ok(links
        .iter()
        .any(|l| l.data.source == INTERP_OVERLAY_TARGET_CLASS))
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
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<String> {
    let run_uri = format!("ad4m://interp/run/{}", meta.run_id);
    let values = serde_json::json!({
        "run_id": meta.run_id,
        "model": meta.model,
        "prompt_version": meta.prompt_version,
        "ran_at": meta.ran_at,
    });
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(INTERP_RUN_CLASS.to_string()),
                query: None,
            },
            run_uri.clone(),
            Some(values),
            batch_id,
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("mint_interpretation_run: create_subject failed: {e:#}"))?;
    Ok(run_uri)
}

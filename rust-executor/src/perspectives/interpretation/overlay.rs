//! Interpretation provenance overlay (#883).
//!
//! The whole mechanism is **one extra subject class instantiated over the same
//! base URI** as the instance the LLM writes — an `InterpretationOverlay` — plus
//! one `InterpretationRun` per pass. The overlay carries, in one instance:
//!   * `kind` (`create` | `update`) — how the engine minted the node,
//!   * `run` → the [`InterpretationRun`] that wrote it (model / prompt_version /
//!     ran_at — the *who* is already the link author's DID, so no `agent` field),
//!   * `inferred/<realPredicate>` → the LLM's value for each affected property.
//!
//! `inferred/<p>` are **parallel links** (`ad4m://interp/inferred/<realPredicate>`)
//! so they never collide with the real property links and stay RDF-clean /
//! `model_query`-able. They double as (a) the LLM's provenance snapshot and (b)
//! the last-inferred baseline used to detect human divergence.
//!
//! The overlay is written *additively* over the same base: readers that don't
//! know about it ignore it and see the plain instance. There is **exactly one
//! overlay per base**, updated in place across passes — no per-run accumulation.
//!
//! ## The one rule that protects humans (Update routing, §4)
//! The engine overwrites a real value **only when it can prove the LLM owns it**;
//! otherwise it stages the proposal in the overlay and leaves the real value
//! untouched:
//!   * real field unset → the LLM is filling in a fresh value → write it.
//!   * base has an overlay and real == last-recorded `inferred/<p>` → the LLM
//!     wrote it and it's untouched → overwrite *and* bump `inferred/<p>` to match.
//!   * base has an overlay and real ≠ `inferred/<p>` → a human diverged → **leave
//!     the real prop untouched**, only set `inferred/<p> = proposed` (suggestion).
//!   * base has **no overlay** → the LLM never wrote this instance (human/seed
//!     data) → **never overwrite**; mint an overlay recording `inferred/<p> =
//!     proposed` as a suggestion only.

use super::{class_local_name, InterpretationOp};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::{
    PerspectiveInstance, SdnaType, SubjectClassOption,
};
use crate::types::{AITask, Link, LinkExpression, LinkQuery, LinkStatus};
use ad4m_client::literal::Literal;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;

// ---- class + predicate vocabulary ------------------------------------------

/// Local subject-class name of the per-pass run node.
pub(crate) const INTERP_RUN_CLASS: &str = "InterpretationRun";
/// Local subject-class name of the per-base overlay node.
pub(crate) const INTERP_OVERLAY_CLASS: &str = "InterpretationOverlay";

/// Target-class URI of [`INTERP_OVERLAY_CLASS`] — used to detect prior
/// registration and to test whether a base already carries an overlay.
const INTERP_OVERLAY_TARGET_CLASS: &str = "ad4m://InterpretationOverlay";
/// The overlay's mandatory `kind` predicate. It doubles as the class's
/// discriminator: an overlay carries no separate type flag — its presence of a
/// `kind` link (a required property) is what identifies a base as overlay-marked
/// (both for [`overlay_exists`] and for `model_query` conformance matching).
const OVERLAY_KIND_PRED: &str = "ad4m://interp/kind";
/// Parallel-predicate prefix for the LLM's per-property value snapshot: the full
/// predicate is `ad4m://interp/inferred/<realPredicate>`.
const INFERRED_PREFIX: &str = "ad4m://interp/inferred/";

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
/// written directly as parallel links (see [`write_overlay`]).
const INTERP_OVERLAY_SDNA: &str = r#"{
  "target_class":"ad4m://InterpretationOverlay",
  "interpretation_hint":"Provenance overlay marking an instance as LLM-inferred, with the last-inferred value snapshot.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ad4m://interp/kind","target":"create"}],
  "properties":[
    {"path":"ad4m://interp/kind","name":"kind","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://interp/kind","target":"value"}]},
    {"path":"ad4m://interp/run","name":"run","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://interp/run","target":"value"}]}
  ]
}"#;

// ---- run metadata ----------------------------------------------------------

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

// ---- class registration ----------------------------------------------------

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
pub(crate) async fn mint_interpretation_run(
    perspective: &mut PerspectiveInstance,
    meta: &InterpretationRunMeta,
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
            None,
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("mint_interpretation_run: create_subject failed: {e:#}"))?;
    Ok(run_uri)
}

// ---- overlay write plan ----------------------------------------------------

/// Whether the engine minted the node (`create`) or patched an existing one
/// (`update`) — recorded on the overlay's `kind`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OverlayKind {
    Create,
    Update,
}

impl OverlayKind {
    fn as_str(self) -> &'static str {
        match self {
            OverlayKind::Create => "create",
            OverlayKind::Update => "update",
        }
    }
}

/// One base's overlay write: its `kind` plus the `inferred/<predicate> = value`
/// snapshot to record (keyed by the *real* predicate IRI).
struct OverlayWrite {
    base: String,
    kind: OverlayKind,
    inferred: BTreeMap<String, serde_json::Value>,
}

/// Plan + apply the provenance overlay for a fully-planned set of real ops, and
/// apply the (possibly gated) real ops themselves. This is the single tail-step
/// [`crate::perspectives::interpretation::run_interpretation`] runs after
/// planning + no-op stripping. Returns the affected instance bases (same set the
/// pre-overlay path returned: every base a Create/Update/AddLinks touched).
///
/// Order of operations matters: the human-divergence gate and the last-inferred
/// baseline are read from the graph state *before* this pass writes anything, so
/// all reads happen first, then the gated real ops are applied, then the
/// overlays. The run node is minted lazily — only when there is at least one
/// overlay to anchor to it.
pub(crate) async fn apply_with_overlay(
    perspective: &mut PerspectiveInstance,
    shapes: &[ModelShape],
    ops: Vec<InterpretationOp>,
    task: &AITask,
    run_id: String,
    ran_at: String,
    context: &AgentContext,
) -> anyhow::Result<Vec<String>> {
    if ops.is_empty() {
        return Ok(Vec::new());
    }

    // Affected bases are reported for the *planned* set, independent of whether
    // the gate later drops a real write (a base that only gains an overlay
    // suggestion was still affected).
    let affected = touched_bases(&ops);

    ensure_interpretation_overlay_classes(perspective, context).await?;

    // Phase 1 (reads only): compute per-base overlay writes + the gated real ops.
    let mut overlays: Vec<OverlayWrite> = Vec::new();
    let mut real_ops: Vec<InterpretationOp> = Vec::new();
    for op in ops {
        match op {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => {
                // A create is entirely the LLM's own: snapshot every value, real
                // write kept verbatim.
                let inferred = inferred_snapshot(shapes, &class, &values);
                overlays.push(OverlayWrite {
                    base: base.clone(),
                    kind: OverlayKind::Create,
                    inferred,
                });
                real_ops.push(InterpretationOp::Create {
                    base,
                    class,
                    values,
                });
            }
            InterpretationOp::Update {
                base,
                class,
                values,
            } => {
                // Human-divergence gate: keep only the real writes still owned by
                // the LLM; every proposed value is snapshotted as inferred.
                let (kept, inferred) =
                    gate_update(perspective, shapes, &base, &class, &values).await?;
                // A newly-minted overlay for an Update records kind=update; if an
                // overlay already exists (LLM-created earlier), write_overlay keeps
                // its original kind=create.
                overlays.push(OverlayWrite {
                    base: base.clone(),
                    kind: OverlayKind::Update,
                    inferred,
                });
                if !kept.is_empty() {
                    real_ops.push(InterpretationOp::Update {
                        base,
                        class,
                        values: kept,
                    });
                }
            }
            // Relations grow the graph additively; provenance for links is out of
            // scope for chunks 1+2 (scalar snapshot only). Pass through unchanged.
            other @ InterpretationOp::AddLinks { .. } => real_ops.push(other),
        }
    }

    // Phase 2: apply the gated real ops (own batch, atomic).
    super::apply_interpretation_ops(perspective, &real_ops, context).await?;

    // Phase 3: mint the run + write the overlays (only if any were planned).
    if !overlays.is_empty() {
        let meta = InterpretationRunMeta::from_task(task, run_id, ran_at);
        let run_uri = mint_interpretation_run(perspective, &meta, context).await?;
        for ow in &overlays {
            write_overlay(perspective, ow, &run_uri, context).await?;
        }
    }

    Ok(affected)
}

/// The scalar snapshot for an op's values, keyed by the *real* predicate IRI
/// (`ns://title`, …) so it can be stored as the parallel `inferred/<p>` link.
/// Values with no resolvable predicate on the class shape are skipped.
fn inferred_snapshot(
    shapes: &[ModelShape],
    class: &str,
    values: &serde_json::Map<String, serde_json::Value>,
) -> BTreeMap<String, serde_json::Value> {
    let mut out = BTreeMap::new();
    for (name, value) in values {
        if let Some(pred) = predicate_for(shapes, class, name) {
            out.insert(pred, value.clone());
        }
    }
    out
}

/// The real predicate IRI a class declares for a property name (`title` ->
/// `ns://title`). `None` if the class/property is unknown to the shapes.
fn predicate_for(shapes: &[ModelShape], class: &str, name: &str) -> Option<String> {
    shapes
        .iter()
        .find(|s| class_local_name(&s.target_class) == class)?
        .properties
        .iter()
        .find(|p| p.name == name)
        .map(|p| p.predicate.clone())
}

/// The human-divergence gate for an Update (§4). The engine only ever overwrites
/// a real value it can prove the LLM owns — otherwise it stages the proposal in
/// the overlay and leaves the real value alone. Reads the base's current real
/// values (through the same `model_query` read path app code uses) and the
/// overlay's last-inferred snapshot, then per property decides:
///   * real unset → nothing to overwrite → **keep** the real write (a fresh
///     field the LLM is filling in).
///   * the base has **no overlay** yet → the LLM did *not* write this instance
///     (it's human/seed-created), so its real values are **not** the LLM's to
///     overwrite → **drop** the real write; only stage `inferred = proposed` as
///     an overlay suggestion. (This is the §4 protection: an existing value the
///     LLM never authored is never silently clobbered.)
///   * overlay exists and real == overlay `inferred/<p>` → the LLM wrote it and
///     it hasn't been touched since → **keep** the real write and bump
///     `inferred/<p>` (the LLM refining its own inference).
///   * overlay exists and real ≠ overlay `inferred/<p>` → a human diverged from
///     the last inference → **drop** the real write; only record the proposed
///     value as a suggestion.
///
/// Returns `(kept_real_values, inferred_snapshot)`. The inferred snapshot always
/// carries every proposed property (kept or suggested), keyed by predicate — so
/// even a fully-gated (suggestion-only) update still establishes/updates the
/// overlay baseline.
async fn gate_update(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
    base: &str,
    class: &str,
    values: &serde_json::Map<String, serde_json::Value>,
) -> anyhow::Result<(
    serde_json::Map<String, serde_json::Value>,
    BTreeMap<String, serde_json::Value>,
)> {
    let real = read_real_values(perspective, class, base, values).await?;
    let inferred_now = read_inferred_values(perspective, base).await?;
    let has_overlay = overlay_exists(perspective, base).await?;

    let mut kept = serde_json::Map::new();
    let mut inferred = BTreeMap::new();
    for (name, proposed) in values {
        let Some(pred) = predicate_for(shapes, class, name) else {
            // Unknown property: keep the write (create_subject ignores it anyway)
            // but don't fabricate an inferred snapshot for a predicate we can't name.
            kept.insert(name.clone(), proposed.clone());
            continue;
        };
        inferred.insert(pred.clone(), proposed.clone());

        let real_val = real.get(name);
        let inferred_val = inferred_now.get(&pred);
        let llm_owns = match real_val {
            // Fresh/unset field → the LLM filling it in is always allowed.
            None => true,
            // No overlay → the LLM never wrote this instance, so an existing real
            // value is not its to overwrite → suggestion-only (protect the human).
            Some(_) if !has_overlay => false,
            // Overlay present → overwrite only while the value is still identical
            // to the last inference (untouched); a human edit diverges it → drop.
            Some(rv) => inferred_val == Some(rv),
        };
        if llm_owns {
            kept.insert(name.clone(), proposed.clone());
        }
    }
    Ok((kept, inferred))
}

/// Read the base's current real values for the given property names via
/// `model_query` (decoded through each property's own getter). Returns a map of
/// property-name -> decoded value for the fields actually present on the row.
async fn read_real_values(
    perspective: &PerspectiveInstance,
    class: &str,
    base: &str,
    values: &serde_json::Map<String, serde_json::Value>,
) -> anyhow::Result<serde_json::Map<String, serde_json::Value>> {
    let props: Vec<&str> = values.keys().map(String::as_str).collect();
    let query = serde_json::json!({ "properties": props }).to_string();
    let result_json = perspective
        .model_query(class, &query)
        .await
        .map_err(|e| anyhow::anyhow!("gate_update: model_query({class}) failed: {e:#}"))?;
    let result: serde_json::Value = serde_json::from_str(&result_json)
        .map_err(|e| anyhow::anyhow!("gate_update: bad model_query result for {class}: {e:#}"))?;
    let row = result
        .get("instances")
        .and_then(|v| v.as_array())
        .and_then(|rows| {
            rows.iter()
                .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(base))
        });
    let mut out = serde_json::Map::new();
    if let Some(obj) = row.and_then(|r| r.as_object()) {
        for name in values.keys() {
            if let Some(v) = obj.get(name) {
                if !v.is_null() {
                    out.insert(name.clone(), v.clone());
                }
            }
        }
    }
    Ok(out)
}

/// Read the base's overlay `inferred/<predicate>` snapshot, decoded to JSON
/// values keyed by the *real* predicate IRI. Empty when the base has no overlay.
async fn read_inferred_values(
    perspective: &PerspectiveInstance,
    base: &str,
) -> anyhow::Result<BTreeMap<String, serde_json::Value>> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            ..Default::default()
        })
        .await?;
    let mut out = BTreeMap::new();
    for l in links {
        if let Some(pred) = l.data.predicate.as_deref() {
            if let Some(real_pred) = pred.strip_prefix(INFERRED_PREFIX) {
                out.insert(real_pred.to_string(), parse_literal_value(&l.data.target));
            }
        }
    }
    Ok(out)
}

/// True once `base` carries an overlay — detected by its mandatory `kind` link
/// (the overlay's sole discriminator; there is no separate type flag).
async fn overlay_exists(perspective: &PerspectiveInstance, base: &str) -> anyhow::Result<bool> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            predicate: Some(OVERLAY_KIND_PRED.to_string()),
            ..Default::default()
        })
        .await?;
    Ok(!links.is_empty())
}

/// Write (or update in place) the overlay for one base: ensure the overlay
/// subject exists with its `kind` + `run`, then replace each `inferred/<p>`
/// parallel link with the new snapshot value. Exactly one overlay per base — a
/// second pass bumps `run` and the inferred values rather than accumulating.
async fn write_overlay(
    perspective: &mut PerspectiveInstance,
    ow: &OverlayWrite,
    run_uri: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
    if overlay_exists(perspective, &ow.base).await? {
        // Overlay already minted (earlier pass): keep its original `kind`, just
        // bump the `run` pointer to this pass.
        perspective
            .update_subject(
                SubjectClassOption {
                    class_name: Some(INTERP_OVERLAY_CLASS.to_string()),
                    query: None,
                },
                ow.base.clone(),
                serde_json::json!({ "run": run_uri }),
                None,
                context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("write_overlay: update_subject(run) failed: {e:#}"))?;
    } else {
        // First overlay on this base: mint it (type flag) with kind + run.
        perspective
            .create_subject(
                SubjectClassOption {
                    class_name: Some(INTERP_OVERLAY_CLASS.to_string()),
                    query: None,
                },
                ow.base.clone(),
                Some(serde_json::json!({ "kind": ow.kind.as_str(), "run": run_uri })),
                None,
                context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("write_overlay: create_subject(overlay) failed: {e:#}"))?;
    }

    for (pred, value) in &ow.inferred {
        let inferred_pred = format!("{INFERRED_PREFIX}{pred}");
        replace_link(
            perspective,
            &ow.base,
            &inferred_pred,
            &encode_inferred_literal(value)?,
            context,
        )
        .await?;
    }
    Ok(())
}

/// Encode an inferred value as a deterministic `literal:` URI target that
/// [`parse_literal_value`] round-trips back to the same JSON value — so a later
/// pass can compare the real value (decoded via `model_query`) against it.
fn encode_inferred_literal(value: &serde_json::Value) -> anyhow::Result<String> {
    let url = match value {
        serde_json::Value::String(s) => Literal::from_string(s.clone()).to_url(),
        serde_json::Value::Number(n) => match n.as_f64() {
            Some(f) => Literal::from_number(f).to_url(),
            None => Literal::from_string(n.to_string()).to_url(),
        },
        serde_json::Value::Bool(b) => Ok(format!("literal:boolean:{b}")),
        other => Literal::from_json(other.clone()).to_url(),
    };
    url.map_err(|e| anyhow::anyhow!("encode_inferred_literal: {e:#}"))
}

/// Replace `(base, predicate)` with a single link to `target`: remove every
/// existing link under that predicate, then add the new one. Keeps `inferred/<p>`
/// (and `run`) single-valued and up to date across passes.
async fn replace_link(
    perspective: &mut PerspectiveInstance,
    base: &str,
    predicate: &str,
    target: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let existing = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await?;
    if !existing.is_empty() {
        let exprs: Vec<LinkExpression> = existing.into_iter().map(Into::into).collect();
        perspective.remove_links(exprs, None).await?;
    }
    perspective
        .add_link(
            Link {
                source: base.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            },
            LinkStatus::Shared,
            None,
            context,
        )
        .await?;
    Ok(())
}

/// The instance bases an op set touches, in op order and de-duplicated (same
/// definition [`crate::perspectives::interpretation::run_interpretation`] used
/// before overlays existed).
fn touched_bases(ops: &[InterpretationOp]) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    let mut out = Vec::new();
    for op in ops {
        let base = match op {
            InterpretationOp::Create { base, .. } | InterpretationOp::Update { base, .. } => base,
            InterpretationOp::AddLinks { source, .. } => source,
        };
        if seen.insert(base.clone()) {
            out.push(base.clone());
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::*;
    use serde_json::json;

    /// The predicate a provenance snapshot uses for a real predicate.
    fn inferred_pred(real: &str) -> String {
        format!("{INFERRED_PREFIX}{real}")
    }

    /// A minimal interpretation task — enough to derive run provenance without a
    /// live AIService (`from_task` only reads the prompt + examples + model_id).
    fn dummy_task() -> AITask {
        AITask {
            model_id: "gemma3:12b".to_string(),
            system_prompt: "You extract typed instances.".to_string(),
            prompt_examples: Vec::new(),
            ..Default::default()
        }
    }

    async fn run_ops(
        perspective: &mut PerspectiveInstance,
        shapes: &[ModelShape],
        ops: Vec<InterpretationOp>,
        run_id: &str,
        ctx: &AgentContext,
    ) -> Vec<String> {
        apply_with_overlay(
            perspective,
            shapes,
            ops,
            &dummy_task(),
            run_id.to_string(),
            "1700000000000".to_string(),
            ctx,
        )
        .await
        .expect("apply_with_overlay")
    }

    fn create_op(base: &str, values: &[(&str, serde_json::Value)]) -> InterpretationOp {
        InterpretationOp::Create {
            base: base.to_string(),
            class: "Intention".to_string(),
            values: values
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
        }
    }

    fn update_op(base: &str, values: &[(&str, serde_json::Value)]) -> InterpretationOp {
        InterpretationOp::Update {
            base: base.to_string(),
            class: "Intention".to_string(),
            values: values
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
        }
    }

    async fn real_title(perspective: &PerspectiveInstance, base: &str) -> Option<String> {
        model_instances(perspective, "Intention", &["title", "owner"])
            .await
            .into_iter()
            .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(base))
            .and_then(|r| r.get("title").and_then(|t| t.as_str()).map(str::to_string))
    }

    // ---- chunk 1: overlay instantiation + inferred snapshot ----------------

    #[tokio::test]
    async fn create_yields_real_instance_and_overlay_snapshot() {
        // A create must land a real Intention AND an overlay over the same base
        // whose `inferred/<p>` equals every written value, with kind=create and a
        // run link into a minted InterpretationRun.
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://ext/intention/create-1";

        let affected = run_ops(
            &mut p,
            &shapes,
            vec![create_op(
                base,
                &[
                    ("title", json!("Ship the parser")),
                    ("owner", json!("Nico")),
                ],
            )],
            "run-1",
            &ctx,
        )
        .await;
        assert_eq!(
            affected,
            vec![base.to_string()],
            "the create base is affected"
        );

        // Real instance is present and readable through the normal model path.
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Ship the parser"),
            "the real Intention must be written and readable"
        );

        // Overlay inferred snapshot == the written values (parallel links).
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://title")).await,
            vec![json!("Ship the parser")],
            "inferred/title must equal the written title"
        );
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://owner")).await,
            vec![json!("Nico")],
            "inferred/owner must equal the written owner"
        );

        // kind=create, and exactly one overlay flag on the base.
        assert_eq!(
            decoded_targets(&p, base, "ad4m://interp/kind").await,
            vec![json!("create")],
            "a create overlay records kind=create"
        );
        let flags = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some(OVERLAY_KIND_PRED.to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(
            flags.len(),
            1,
            "exactly one overlay kind link identifies the base (no separate type flag)"
        );

        // The run link points at a minted InterpretationRun carrying our run_id.
        let run_links = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some("ad4m://interp/run".to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(run_links.len(), 1, "overlay carries exactly one run link");
        let run_uri = run_links[0].data.target.clone();
        assert_eq!(
            decoded_targets(&p, &run_uri, "ad4m://interp/run_id").await,
            vec![json!("run-1")],
            "the run node carries the pass's run_id"
        );
        assert_eq!(
            decoded_targets(&p, &run_uri, "ad4m://interp/model").await,
            vec![json!("gemma3:12b")],
            "the run node records the model"
        );
    }

    #[tokio::test]
    async fn single_overlay_per_base_updated_in_place_across_passes() {
        // Chunk 1 (one overlay per base, no accumulation) + chunk 2a (an
        // unchanged, still-LLM-owned node is overwritten in place on the 2nd pass
        // and the overlay's inferred value is bumped to match).
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://ext/intention/roll-1";

        // Pass 1: create with title v1.
        run_ops(
            &mut p,
            &shapes,
            vec![create_op(base, &[("title", json!("Draft the doc"))])],
            "run-1",
            &ctx,
        )
        .await;

        // Pass 2: the LLM refines the SAME (still-LLM-owned) node to v2.
        run_ops(
            &mut p,
            &shapes,
            vec![update_op(
                base,
                &[("title", json!("Draft and circulate the doc"))],
            )],
            "run-2",
            &ctx,
        )
        .await;

        // Real value overwritten in place (LLM refining its own inference).
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Draft and circulate the doc"),
            "an unchanged (LLM-owned) node is overwritten in place on pass 2"
        );

        // Exactly ONE overlay kind link — updated in place, not accumulated per run.
        let flags = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some(OVERLAY_KIND_PRED.to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(flags.len(), 1, "one overlay per base across passes");

        // inferred/title bumped to v2, single link (old value replaced).
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://title")).await,
            vec![json!("Draft and circulate the doc")],
            "the overlay's inferred snapshot is bumped in place to the new value"
        );

        // run link bumped to pass 2's run.
        let run_uri = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some("ad4m://interp/run".to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(run_uri.len(), 1, "still a single run link after pass 2");
        assert_eq!(
            decoded_targets(&p, &run_uri[0].data.target, "ad4m://interp/run_id").await,
            vec![json!("run-2")],
            "the run pointer is bumped to the latest pass"
        );
    }

    // ---- chunk 2: human-divergence gate ------------------------------------

    #[tokio::test]
    async fn human_edited_value_is_protected_overlay_only_gets_suggestion() {
        // Chunk 2b: once a human has edited the real value away from the overlay's
        // last-inferred baseline, a later LLM pass must NOT overwrite the real
        // value — it only updates the overlay's inferred/<p> as a suggestion.
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://ext/intention/human-1";

        // Pass 1: LLM creates (real == inferred == v1).
        run_ops(
            &mut p,
            &shapes,
            vec![create_op(base, &[("title", json!("Original LLM title"))])],
            "run-1",
            &ctx,
        )
        .await;

        // Human edits the real value in place (diverging from the baseline).
        p.update_subject(
            SubjectClassOption {
                class_name: Some("Intention".to_string()),
                query: None,
            },
            base.to_string(),
            json!({ "title": "Human corrected title" }),
            None,
            &ctx,
        )
        .await
        .expect("human edit");
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Human corrected title"),
            "sanity: the human edit landed on the real value"
        );

        // Pass 2: LLM proposes a new value for the now-diverged field.
        run_ops(
            &mut p,
            &shapes,
            vec![update_op(
                base,
                &[("title", json!("Second LLM suggestion"))],
            )],
            "run-2",
            &ctx,
        )
        .await;

        // Real value is LEFT UNTOUCHED — the human's edit survives.
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Human corrected title"),
            "a human-diverged real value must not be overwritten by a later pass"
        );
        // The overlay records the LLM's proposal as a suggestion only.
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://title")).await,
            vec![json!("Second LLM suggestion")],
            "the overlay's inferred/<p> is updated to the new suggestion"
        );
    }

    #[tokio::test]
    async fn non_llm_instance_is_never_overwritten_only_gets_overlay_suggestion() {
        // §4 core protection (review fix): a base the LLM did NOT write (no
        // overlay — e.g. human/seed-created) must never have its real values
        // overwritten by an interpretation pass. The pass may only stage the
        // proposal in a freshly-minted overlay as a suggestion.
        let (mut p, shapes, ctx) = setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://ext/intention/seed-1";

        // A non-LLM actor creates the instance directly (no overlay is minted).
        p.create_subject(
            SubjectClassOption {
                class_name: Some("Intention".to_string()),
                query: None,
            },
            base.to_string(),
            Some(json!({ "title": "Human authored title" })),
            None,
            &ctx,
        )
        .await
        .expect("seed create");
        assert!(
            !overlay_exists(&p, base).await.unwrap(),
            "sanity: a plain create leaves no overlay on the base"
        );

        // The LLM proposes a different value for the same field on its first pass.
        run_ops(
            &mut p,
            &shapes,
            vec![update_op(base, &[("title", json!("LLM proposed title"))])],
            "run-1",
            &ctx,
        )
        .await;

        // Real value is LEFT UNTOUCHED — the LLM never owned this instance.
        assert_eq!(
            real_title(&p, base).await.as_deref(),
            Some("Human authored title"),
            "an instance the LLM never wrote must not be overwritten on first touch"
        );
        // An overlay is now established, recording the proposal as a suggestion.
        assert!(
            overlay_exists(&p, base).await.unwrap(),
            "the pass still establishes an overlay baseline"
        );
        assert_eq!(
            decoded_targets(&p, base, &inferred_pred("ns://title")).await,
            vec![json!("LLM proposed title")],
            "the overlay records the LLM's value as an inferred suggestion only"
        );
        assert_eq!(
            decoded_targets(&p, base, "ad4m://interp/kind").await,
            vec![json!("update")],
            "a suggestion-only overlay on an existing node records kind=update"
        );
    }

    #[tokio::test]
    async fn from_task_prompt_version_tracks_prompt_changes() {
        // prompt_version must be a stable hash of system prompt + few-shots, so a
        // prompt change yields a new version and an identical prompt reproduces it.
        let a = InterpretationRunMeta::from_task(&dummy_task(), "r".into(), "0".into());
        let b = InterpretationRunMeta::from_task(&dummy_task(), "r".into(), "0".into());
        assert_eq!(
            a.prompt_version, b.prompt_version,
            "same prompt → same version"
        );

        let mut changed = dummy_task();
        changed.system_prompt = "Different system prompt.".into();
        let c = InterpretationRunMeta::from_task(&changed, "r".into(), "0".into());
        assert_ne!(
            a.prompt_version, c.prompt_version,
            "a changed system prompt → a different prompt_version"
        );
        assert_eq!(a.model, "gemma3:12b", "model is carried from the task");
    }
}

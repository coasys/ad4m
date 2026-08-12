//! Neighbourhood auto-processing — run the interpretation engine automatically
//! and coordinate *which* peer runs each pass, inside AD4M rather than in app UI
//! code.
//!
//! Built in phases, stacked on the tree-aware interpretation engine (#883):
//!   * **P-A — [`claim`]**: the atomic reservation primitive (`ProcessingClaim`
//!     instances in the shared perspective) that fixes Flux's double-processing
//!     race.
//!   * **P-B1 — [`config`]**: `AutoProcessorConfig` + the `AutoProcessor`
//!     subject class the executor watcher reads back to know which processors
//!     to schedule. *(this commit)*
//!   * **P-B2 — [`watcher`]**: the executor watcher — the pure debounce +
//!     batch-cap state plus [`watcher::run_one_pass`], the standalone runner
//!     that ties claim + shape-load + SPARQL transcript + interpretation
//!     together. Real event-stream wire-up (subscribe → debounce loop →
//!     telepresence-bounded candidacy) is the P-B2b follow-up. *(this
//!     commit — pure logic + standalone one-pass runner)*
//!   * **P-C**: a Flux-parity integration demo.

pub mod claim;
pub mod config;
pub mod events;
pub mod watcher;

use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SdnaType};
use crate::types::LinkQuery;

/// True once `target_class` has been registered as a SubjectClass in this
/// perspective — the presence guard both hard-wired classes below use, and the
/// "is there anything to read?" check on the load paths (a perspective that
/// never wrote a processor/claim has no shape to `model_query` against).
pub(crate) async fn subject_class_registered(
    perspective: &PerspectiveInstance,
    target_class: &str,
) -> anyhow::Result<bool> {
    let links = perspective
        .get_links(&LinkQuery {
            predicate: Some("rdf://type".to_string()),
            target: Some("ad4m://SubjectClass".to_string()),
            ..Default::default()
        })
        .await?;
    Ok(links.iter().any(|l| l.data.source == target_class))
}

/// Idempotently register a hard-wired subject class into the perspective,
/// mirroring the `add_sdna` path the interpretation overlay classes use. A
/// no-op once the class is present, so a continuous processor calling it on
/// every write costs one cheap link scan rather than a SHACL rewrite.
pub(crate) async fn ensure_subject_class(
    perspective: &mut PerspectiveInstance,
    class_name: &str,
    target_class: &str,
    sdna: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
    if subject_class_registered(perspective, target_class).await? {
        return Ok(());
    }
    perspective
        .add_sdna(
            class_name.to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(sdna.to_string()),
            context,
        )
        .await
        .map_err(|e| {
            anyhow::anyhow!("ensure_subject_class({class_name}): add_sdna failed: {e:#}")
        })?;
    Ok(())
}

/// Render a hydrated `model_query` scalar as a `String`. Both classes write
/// their scalars as `literal:string:` targets, but a value that hydrates to a
/// JSON number/bool (hand-edited graph, or a peer that wrote a typed literal)
/// is rendered rather than dropped — the numeric parse at the call site is what
/// decides whether it is usable.
pub(crate) fn scalar_string(value: Option<&serde_json::Value>) -> Option<String> {
    match value? {
        serde_json::Value::String(s) if !s.is_empty() => Some(s.clone()),
        serde_json::Value::Number(n) => Some(n.to_string()),
        serde_json::Value::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}

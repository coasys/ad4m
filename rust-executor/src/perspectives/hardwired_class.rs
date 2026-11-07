//! Registration primitives for **hard-wired** subject classes — the SHACL
//! shapes the executor itself owns (`AutoProcessor`, `ProcessingClaim`,
//! `InterpretationRun`, `InterpretationOverlay`) rather than ones an app
//! declares.
//!
//! These live outside both `auto_processor` and `interpretation` so neither
//! module has to depend on the other for what is really one primitive: "make
//! sure these classes are registered with the shapes this executor has".

use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::SHACLShape;
use crate::types::LinkQuery;

/// True once `target_class` has been registered as a SubjectClass in this
/// perspective — the presence guard the hard-wired classes use, and the "is
/// there anything to read?" check on the load paths (a perspective that never
/// wrote an instance has no shape to `model_query` against).
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

/// Register hard-wired subject classes, each given as its SHACL JSON, in one
/// `register_shapes` call. Writes nothing while the stored shapes are these;
/// rewrites a class whose stored shape is from another revision.
pub(crate) async fn ensure_subject_classes(
    perspective: &mut PerspectiveInstance,
    sdnas: &[&str],
    context: &AgentContext,
) -> anyhow::Result<()> {
    let shapes = sdnas
        .iter()
        .map(|sdna| serde_json::from_str::<SHACLShape>(sdna))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| anyhow::anyhow!("hard-wired SHACL JSON: {e}"))?;
    perspective
        .register_shapes(shapes, context)
        .await
        .map_err(|e| anyhow::anyhow!("registering hard-wired classes: {e:#}"))?;
    Ok(())
}

//! Registration primitives for **hard-wired** subject classes — the SHACL
//! shapes the executor itself owns (`AutoProcessor`, `ProcessingClaim`,
//! `InterpretationRun`, `InterpretationOverlay`) rather than ones an app
//! declares.
//!
//! These live outside both `auto_processor` and `interpretation` so neither
//! module has to depend on the other for what is really one primitive: "make
//! sure this class is registered, and refresh it if the registered shape
//! predates a property we now need".

use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SdnaType};
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

/// True when **`target_class`'s own shape** declares a property at this
/// `sh://path`. Distinguishes a class registered from an older SDNA revision
/// from one carrying the property set the caller needs.
///
/// Scoped via the shape graph (`target_class ad4m://shape → sh://property →
/// sh://path`) rather than a bare perspective-wide `sh://path` query: any
/// *app-declared* class may legitimately declare a property at the same path
/// (e.g. `ad4m://flow`), and an unscoped hit would permanently suppress the
/// refresh while `write_processor`'s links land against a shape that never
/// gained the property.
pub(crate) async fn shacl_path_present(
    perspective: &PerspectiveInstance,
    target_class: &str,
    path: &str,
) -> anyhow::Result<bool> {
    let shape_links = perspective
        .get_links(&LinkQuery {
            source: Some(target_class.to_string()),
            predicate: Some("ad4m://shape".to_string()),
            ..Default::default()
        })
        .await?;
    let mut prop_nodes = std::collections::HashSet::new();
    for shape in &shape_links {
        let props = perspective
            .get_links(&LinkQuery {
                source: Some(shape.data.target.clone()),
                predicate: Some("sh://property".to_string()),
                ..Default::default()
            })
            .await?;
        prop_nodes.extend(props.into_iter().map(|l| l.data.target));
    }

    let path_links = perspective
        .get_links(&LinkQuery {
            predicate: Some("sh://path".to_string()),
            target: Some(path.to_string()),
            ..Default::default()
        })
        .await?;
    Ok(path_links
        .iter()
        .any(|l| prop_nodes.contains(&l.data.source)))
}

/// Idempotently register a hard-wired subject class. A no-op once the class is
/// present, so a continuous processor calling it on every write costs one cheap
/// link scan rather than a SHACL rewrite.
///
/// `required_path` names a `sh://path` that the *current* SDNA declares but an
/// older revision did not. When the class is already registered without it, the
/// shape is re-registered so the new setters land — `add_sdna` purges the prior
/// SHACL graph on refresh, so this replaces rather than duplicates the shape.
pub(crate) async fn ensure_subject_class(
    perspective: &mut PerspectiveInstance,
    class_name: &str,
    target_class: &str,
    sdna: &str,
    required_path: Option<&str>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    if subject_class_registered(perspective, target_class).await? {
        let up_to_date = match required_path {
            Some(path) => shacl_path_present(perspective, target_class, path).await?,
            None => true,
        };
        if up_to_date {
            return Ok(());
        }
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

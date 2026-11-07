//! Registration primitives for **hard-wired** subject classes — the SHACL
//! shapes the executor itself owns (`AutoProcessor`, `ProcessingClaim`,
//! `InterpretationRun`, `InterpretationOverlay`, `FlowInstance`,
//! `FlowTransitionProposal`) rather than ones an app declares.
//!
//! These live outside both `auto_processor` and `interpretation` so neither
//! module has to depend on the other for what is really one primitive: "make
//! sure this class is registered, and upgrade it if the registered shape is
//! older than the one this executor has".
//!
//! Upgrade only: executors of two versions share a neighbourhood, and each
//! writing its own revision whenever the stored one differs would have them
//! rewrite each other's shape on every call. A stored shape is *older* when it
//! lacks the path the newest revision added, or still declares a path a later
//! revision removed; any other stored shape (this revision or a newer one) is
//! left alone.

use crate::agent::AgentContext;
use crate::perspectives::model_query::shape::find_class;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{class_name_of, SHACLShape};
use crate::types::LinkQuery;
use std::sync::OnceLock;

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

/// A hard-wired class: its SHACL JSON, and how to tell a stored shape of it
/// is older than this one.
#[derive(Debug, Clone, Copy)]
pub(crate) struct HardwiredClass {
    pub(crate) sdna: &'static str,
    /// The `sh://path` the newest revision added: a stored shape without it
    /// is older.
    pub(crate) newest_path: Option<&'static str>,
    /// A `sh://path` a later revision removed: a stored shape declaring it
    /// is older.
    pub(crate) removed_path: Option<&'static str>,
}

impl HardwiredClass {
    /// A class whose every revision so far has the same properties.
    pub(crate) const fn new(sdna: &'static str) -> Self {
        Self {
            sdna,
            newest_path: None,
            removed_path: None,
        }
    }

    /// Whether `stored`, a registered shape of this class, is older than the
    /// one this executor has.
    fn is_older(&self, stored: &SHACLShape) -> bool {
        let declares = |path: &str| stored.properties.iter().any(|p| p.path == path);
        self.newest_path.is_some_and(|path| !declares(path))
            || self.removed_path.is_some_and(declares)
    }
}

/// Register the hard-wired classes that are missing or stored in an older
/// revision, in one `register_shapes` call. Writes nothing while every one is
/// stored in this revision or a newer one: a newer stored shape is never
/// replaced by this executor's older one.
pub(crate) async fn ensure_subject_classes(
    perspective: &mut PerspectiveInstance,
    classes: &[HardwiredClass],
    context: &AgentContext,
) -> anyhow::Result<()> {
    let registry = perspective
        .shape_registry()
        .map_err(|e| anyhow::anyhow!("reading the registered classes: {e:#}"))?;
    let mut stale = Vec::new();
    for class in classes {
        let shape = serde_json::from_str::<SHACLShape>(class.sdna)
            .map_err(|e| anyhow::anyhow!("hard-wired SHACL JSON: {e}"))?;
        let up_to_date = registry
            .iter()
            .find(|s| s.shape.target_class == shape.target_class)
            .is_some_and(|stored| !class.is_older(&stored.shape));
        if !up_to_date {
            stale.push(shape);
        }
    }
    if stale.is_empty() {
        return Ok(());
    }
    perspective
        .register_shapes(stale, context)
        .await
        .map_err(|e| anyhow::anyhow!("registering hard-wired classes: {e:#}"))?;
    Ok(())
}

/// Every hard-wired class, with its target class IRI.
fn all() -> &'static [(String, HardwiredClass)] {
    static ALL: OnceLock<Vec<(String, HardwiredClass)>> = OnceLock::new();
    ALL.get_or_init(|| {
        use crate::perspectives::auto_processor::{
            claim::PROCESSING_CLAIM, config::AUTO_PROCESSOR,
        };
        use crate::perspectives::flow_classes::{FLOW_INSTANCE, FLOW_TRANSITION_PROPOSAL};
        use crate::perspectives::interpretation::overlay::{
            INTERPRETATION_OVERLAY, INTERPRETATION_RUN,
        };
        [
            FLOW_INSTANCE,
            FLOW_TRANSITION_PROPOSAL,
            INTERPRETATION_OVERLAY,
            INTERPRETATION_RUN,
            AUTO_PROCESSOR,
            PROCESSING_CLAIM,
        ]
        .into_iter()
        .map(|class| {
            let shape: SHACLShape =
                serde_json::from_str(class.sdna).expect("hard-wired SHACL JSON parses");
            (shape.target_class, class)
        })
        .collect()
    })
}

/// The hard-wired class `class_name` names (by its name or its target class
/// IRI, the match `find_class` makes) when it is not stored yet.
pub(crate) fn unstored(
    perspective: &PerspectiveInstance,
    class_name: &str,
) -> anyhow::Result<Option<HardwiredClass>> {
    let Some((_, class)) = all()
        .iter()
        .find(|(target, _)| target == class_name || class_name_of(target) == class_name)
    else {
        return Ok(None);
    };
    Ok(find_class(&perspective.sparql_store, class_name)?
        .is_none()
        .then_some(*class))
}

/// Before an instance of `class_name` is written (`create_subject`): a
/// hard-wired class not stored yet is registered first, so an app writes
/// through the executor's shape without registering it. The executor's own
/// write paths upgrade an older stored one (`ensure_subject_classes`).
pub(crate) async fn ensure_before_write(
    perspective: &mut PerspectiveInstance,
    class_name: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
    match unstored(perspective, class_name)? {
        Some(class) => ensure_subject_classes(perspective, &[class], context).await,
        None => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;

    const SDNA: &str = r#"{
      "target_class": "ad4m://Hardwired",
      "properties": [
        {"path": "ad4m://hw/a", "name": "a", "max_count": 1,
         "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://hw/a", "target": "value"}]},
        {"path": "ad4m://hw/b", "name": "b", "max_count": 1,
         "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://hw/b", "target": "value"}]}
      ]
    }"#;

    /// This executor's revision: `b` is its newest path, `old` was removed.
    const CLASS: HardwiredClass = HardwiredClass {
        sdna: SDNA,
        newest_path: Some("ad4m://hw/b"),
        removed_path: Some("ad4m://hw/old"),
    };

    fn shape() -> SHACLShape {
        serde_json::from_str(SDNA).unwrap()
    }

    fn property(path: &str, name: &str) -> crate::perspectives::shacl_parser::PropertyShape {
        serde_json::from_value(serde_json::json!({"path": path, "name": name, "max_count": 1}))
            .unwrap()
    }

    fn stored(perspective: &PerspectiveInstance) -> SHACLShape {
        perspective
            .shapes(Some(vec!["Hardwired".into()]))
            .unwrap()
            .pop()
            .expect("the class is registered")
            .shape
    }

    fn paths(shape: &SHACLShape) -> Vec<String> {
        shape.properties.iter().map(|p| p.path.clone()).collect()
    }

    #[tokio::test]
    async fn a_missing_class_is_written() {
        let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
        ensure_subject_classes(&mut perspective, &[CLASS], &ctx)
            .await
            .unwrap();
        assert_eq!(paths(&stored(&perspective)), ["ad4m://hw/a", "ad4m://hw/b"]);
    }

    #[tokio::test]
    async fn an_older_stored_class_is_upgraded() {
        // Lacks the newest path.
        let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
        let mut older = shape();
        older.properties.retain(|p| p.path != "ad4m://hw/b");
        perspective
            .register_shapes(vec![older], &ctx)
            .await
            .unwrap();
        ensure_subject_classes(&mut perspective, &[CLASS], &ctx)
            .await
            .unwrap();
        assert_eq!(paths(&stored(&perspective)), ["ad4m://hw/a", "ad4m://hw/b"]);

        // Still declares a removed path.
        let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
        let mut older = shape();
        older.properties.push(property("ad4m://hw/old", "old"));
        perspective
            .register_shapes(vec![older], &ctx)
            .await
            .unwrap();
        ensure_subject_classes(&mut perspective, &[CLASS], &ctx)
            .await
            .unwrap();
        assert_eq!(paths(&stored(&perspective)), ["ad4m://hw/a", "ad4m://hw/b"]);
    }

    #[tokio::test]
    async fn a_newer_stored_class_is_left_alone() {
        let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
        // A later executor's revision: one more property, the same `b`.
        let mut newer = shape();
        newer.properties.push(property("ad4m://hw/c", "c"));
        perspective
            .register_shapes(vec![newer], &ctx)
            .await
            .unwrap();
        let before = perspective.sparql_store.get_all_links().unwrap().len();

        ensure_subject_classes(&mut perspective, &[CLASS], &ctx)
            .await
            .unwrap();

        assert_eq!(
            paths(&stored(&perspective)),
            ["ad4m://hw/a", "ad4m://hw/b", "ad4m://hw/c"]
        );
        assert_eq!(
            perspective.sparql_store.get_all_links().unwrap().len(),
            before
        );
    }

    fn triples(perspective: &PerspectiveInstance) -> Vec<(String, Option<String>, String)> {
        let mut triples: Vec<_> = perspective
            .sparql_store
            .get_all_links()
            .unwrap()
            .into_iter()
            .map(|l| (l.data.source, l.data.predicate, l.data.target))
            .collect();
        triples.sort();
        triples
    }

    /// Every hard-wired class stored in a revision this executor did not write
    /// (an app's `@Model` copy, a newer executor's shape: here each with one
    /// more property): three rounds of every ensure, and of the check a create
    /// makes, write nothing and leave the shapes generation where it was.
    #[tokio::test]
    async fn a_stored_revision_of_each_hardwired_class_stays_over_three_rounds() {
        use crate::perspectives::auto_processor::{
            claim::ensure_processing_claim_class, config::ensure_auto_processor_class,
        };
        use crate::perspectives::flow_classes::ensure_flow_model_classes;
        use crate::perspectives::interpretation::overlay::ensure_interpretation_overlay_classes;

        let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
        let foreign: Vec<SHACLShape> = all()
            .iter()
            .map(|(_, class)| {
                let mut shape: SHACLShape = serde_json::from_str(class.sdna).unwrap();
                shape.properties.push(property("app://extra", "extra"));
                shape
            })
            .collect();
        assert_eq!(foreign.len(), 6);
        perspective
            .register_shapes(foreign.clone(), &ctx)
            .await
            .unwrap();
        let links = triples(&perspective);
        let generation = perspective.shapes_generation();
        let registered = |perspective: &PerspectiveInstance| {
            let mut shapes: Vec<SHACLShape> = perspective
                .shapes(None)
                .unwrap()
                .into_iter()
                .map(|s| s.shape)
                .collect();
            shapes.sort_by(|a, b| a.target_class.cmp(&b.target_class));
            serde_json::to_value(&shapes).unwrap()
        };
        let stored_before = registered(&perspective);

        for round in 1..=3 {
            ensure_flow_model_classes(&mut perspective, &ctx)
                .await
                .unwrap();
            ensure_interpretation_overlay_classes(&mut perspective, &ctx)
                .await
                .unwrap();
            ensure_auto_processor_class(&mut perspective, &ctx)
                .await
                .unwrap();
            ensure_processing_claim_class(&mut perspective, &ctx)
                .await
                .unwrap();
            for (target_class, _) in all() {
                ensure_before_write(&mut perspective, &class_name_of(target_class), &ctx)
                    .await
                    .unwrap();
            }
            assert_eq!(perspective.shapes_generation(), generation, "round {round}");
            assert_eq!(triples(&perspective), links, "round {round}");
            assert_eq!(registered(&perspective), stored_before, "round {round}");
        }
        // The stored revisions, the extra property included, are the ones read.
        let stored: Vec<SHACLShape> = perspective
            .shapes(None)
            .unwrap()
            .into_iter()
            .map(|s| s.shape)
            .collect();
        assert_eq!(stored.len(), 6);
        for shape in &stored {
            assert!(
                shape.properties.iter().any(|p| p.path == "app://extra"),
                "{}",
                shape.target_class
            );
        }
    }

    /// An app reads and writes a hard-wired class without registering it: the
    /// read of a class not stored yet is empty and writes nothing, the first
    /// create registers the executor's shape, and later ensures leave it.
    #[tokio::test]
    async fn an_app_uses_a_hardwired_class_without_registering_it() {
        use crate::perspectives::flow_classes::{
            ensure_flow_model_classes, FLOW_INSTANCE_CLASS, FLOW_INSTANCE_SDNA,
        };
        use crate::perspectives::perspective_instance::SubjectClassOption;

        let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
        let before = triples(&perspective);
        for class_name in ["FlowInstance", "FlowTransitionProposal", "AutoProcessor"] {
            let read: serde_json::Value = serde_json::from_str(
                &perspective
                    .model_query(class_name, r#"{"count": true}"#)
                    .await
                    .unwrap(),
            )
            .unwrap();
            assert_eq!(read["instances"], serde_json::json!([]), "{class_name}");
            assert_eq!(read["totalCount"], 0, "{class_name}");
        }
        assert_eq!(triples(&perspective), before, "a read writes nothing");

        perspective
            .create_subject(
                SubjectClassOption {
                    class_name: Some("FlowInstance".into()),
                    query: None,
                },
                "ad4m://flow/instance/app".into(),
                Some(serde_json::json!({"flowUri": "x://DFlow", "subject": "ad4m://task/1"})),
                None,
                &ctx,
            )
            .await
            .unwrap();
        let stored = perspective
            .shapes(Some(vec!["FlowInstance".into()]))
            .unwrap()
            .pop()
            .expect("the create registered the class")
            .shape;
        assert_eq!(stored.target_class, FLOW_INSTANCE_CLASS);
        // The executor's revision: the links its shape gives.
        let sdna: SHACLShape = serde_json::from_str(FLOW_INSTANCE_SDNA).unwrap();
        let links_of = |shape: &SHACLShape| {
            let mut links: Vec<_> =
                crate::perspectives::shacl_parser::shape_links(shape.clone(), "FlowInstance")
                    .unwrap()
                    .into_iter()
                    .map(|l| (l.source, l.predicate, l.target))
                    .collect();
            links.sort();
            links
        };
        assert_eq!(links_of(&stored), links_of(&sdna));
        let read: serde_json::Value =
            serde_json::from_str(&perspective.model_query("FlowInstance", "{}").await.unwrap())
                .unwrap();
        assert_eq!(read["instances"].as_array().unwrap().len(), 1);

        // The executor's own flow path adds the proposal class; from then on
        // nothing changes.
        ensure_flow_model_classes(&mut perspective, &ctx)
            .await
            .unwrap();
        let links = triples(&perspective);
        let generation = perspective.shapes_generation();
        for round in 1..=3 {
            ensure_flow_model_classes(&mut perspective, &ctx)
                .await
                .unwrap();
            for class_name in ["FlowInstance", "FlowTransitionProposal"] {
                ensure_before_write(&mut perspective, class_name, &ctx)
                    .await
                    .unwrap();
            }
            assert_eq!(perspective.shapes_generation(), generation, "round {round}");
            assert_eq!(triples(&perspective), links, "round {round}");
        }
    }

    /// An app class may share a hard-wired class's name: `app://FlowInstance`
    /// is stored beside `ad4m://FlowInstance`, and the executor's flow paths,
    /// which address their classes by IRI, still register and read the
    /// executor's class.
    #[tokio::test]
    async fn an_app_class_of_a_hardwired_name_coexists_with_it() {
        use crate::perspectives::flow_classes::{ensure_flow_model_classes, FLOW_INSTANCE_CLASS};
        use crate::perspectives::perspective_instance::SubjectClassOption;

        let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
        let apps: Vec<SHACLShape> = all()
            .iter()
            .map(|(target_class, _)| {
                let name = class_name_of(target_class);
                serde_json::from_value(serde_json::json!({
                    "target_class": format!("app://{name}"),
                    "constructor_actions": [{"action": "addLink", "source": "this",
                        "predicate": "app://step", "target": "app://start"}],
                    "properties": [{"path": "app://step", "name": "step",
                        "has_value": "app://start", "min_count": 1, "max_count": 1}]
                }))
                .unwrap()
            })
            .collect();
        perspective.register_shapes(apps, &ctx).await.unwrap();
        let app_flow = perspective
            .shapes(Some(vec!["FlowInstance".into()]))
            .unwrap();
        assert_eq!(app_flow.len(), 1);
        assert_eq!(app_flow[0].shape.target_class, "app://FlowInstance");
        // The bare name reads the one class of that name stored so far.
        assert_eq!(
            perspective.get_shape("FlowInstance").unwrap().target_class,
            "app://FlowInstance"
        );

        // The executor's flow path registers its own class by IRI.
        ensure_flow_model_classes(&mut perspective, &ctx)
            .await
            .unwrap();
        let stored: Vec<String> = perspective
            .shapes(Some(vec!["FlowInstance".into()]))
            .unwrap()
            .into_iter()
            .map(|s| s.shape.target_class)
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();
        assert_eq!(stored, [FLOW_INSTANCE_CLASS, "app://FlowInstance"]);

        // A create and a read through the executor's IRI use its class only;
        // the app's IRI reads the app's class only.
        perspective
            .create_subject(
                SubjectClassOption {
                    class_name: Some(FLOW_INSTANCE_CLASS.into()),
                    query: None,
                },
                "ad4m://flow/instance/1".into(),
                Some(serde_json::json!({"flowUri": "x://DFlow", "subject": "ad4m://task/1"})),
                None,
                &ctx,
            )
            .await
            .unwrap();
        perspective
            .create_subject(
                SubjectClassOption {
                    class_name: Some("app://FlowInstance".into()),
                    query: None,
                },
                "app://flow/1".into(),
                None,
                None,
                &ctx,
            )
            .await
            .unwrap();
        for (class, id) in [
            (FLOW_INSTANCE_CLASS, "ad4m://flow/instance/1"),
            ("app://FlowInstance", "app://flow/1"),
        ] {
            let read: serde_json::Value =
                serde_json::from_str(&perspective.model_query(class, "{}").await.unwrap()).unwrap();
            let ids: Vec<_> = read["instances"]
                .as_array()
                .unwrap()
                .iter()
                .map(|i| i["id"].as_str().unwrap().to_string())
                .collect();
            assert_eq!(ids, [id], "{class}");
        }
        assert_eq!(
            perspective
                .get_shape(FLOW_INSTANCE_CLASS)
                .unwrap()
                .target_class,
            FLOW_INSTANCE_CLASS
        );
    }

    #[tokio::test]
    async fn this_revision_stored_writes_nothing() {
        let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
        ensure_subject_classes(&mut perspective, &[CLASS], &ctx)
            .await
            .unwrap();
        let before = perspective.sparql_store.get_all_links().unwrap().len();
        ensure_subject_classes(&mut perspective, &[CLASS], &ctx)
            .await
            .unwrap();
        assert_eq!(
            perspective.sparql_store.get_all_links().unwrap().len(),
            before
        );
    }
}

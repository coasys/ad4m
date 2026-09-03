//! Deterministic flow-spawn candidate selection.
//!
//! Design authority: `docs/flow-interpretation-hints-design.md` §8 (available
//! actions — the spawn pass) and §10, which puts **"simple deterministic
//! flow-spawn (on new instance matching any `inputTypes` URI)"** in v1 and
//! defers **"LLM-driven flow *creation* via `creationHint`"** to v1.5+. So
//! nothing here consults `creationHint` or an LLM: a candidate is decided
//! entirely from the item's registered subject classes and the live instances
//! already on the graph.
//!
//! This is the read half of the spawn pass. It answers *which flows should be
//! started on this item*; minting them is
//! [`mint_flow_instance`](super::flow_classes::mint_flow_instance), which the
//! wiring slice calls for each candidate. Keeping the decision pure means the
//! rule can be tested exhaustively without a perspective, an agent context, or
//! a writable graph — the same onion-shell cut the `requires` / consensus
//! primitives use.

use std::collections::HashMap;

use super::flow_context::FlowInstanceRecord;
use super::shacl_parser::SHACLFlow;

/// One flow that should be started on a given item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpawnCandidate {
    /// Canonical flow URI (`{namespace}{name}Flow`) — the identity
    /// `FlowInstance.flowUri` is keyed by (PR #929 R5), never the bare name.
    pub flow_uri: String,
    /// The item the instance would be anchored on (`FlowInstance.subject`).
    pub subject: String,
    /// State the fresh instance starts in: the flow's first state by declared
    /// `value`. `None` for a **zero-state flow**, which design §10 makes
    /// first-class as an atomic action — it has no state to begin in, and the
    /// caller fires it rather than tracking it.
    pub initial_state: Option<String>,
}

/// Every flow that should be spawned on `subject`, given the perspective's
/// flow catalogue and the instances already live on it.
///
/// A flow is a candidate when **both** hold (design §8, spawn pass):
///
/// 1. one of `subject_classes` appears in the flow's `inputTypes`, and
/// 2. no live `FlowInstance` already pairs that flow with that subject.
///
/// `subject_classes` is what
/// [`subject_classes_of`](super::subject_classes_of::subject_classes_of)
/// returns for the item — most-specific first, and *absent* (here: empty) when
/// the URI matches no registered class. An item of no known class spawns
/// nothing, which is the same answer as an item whose classes no flow accepts.
///
/// **A flow with empty `inputTypes` is never a deterministic spawn candidate.**
/// `PerspectiveProxy.availableFlows` reads empty (or `"any"`) as "applies to
/// everything", but that is a *menu* — a human picks from it. Auto-spawning on
/// the same reading would mint an instance of such a flow on every item the
/// extraction pass ever produces. Requiring an explicit class keeps the
/// automatic path opt-in; a catch-all flow is still startable by hand.
///
/// Output is sorted by `flow_uri`, since the catalogue is a `HashMap` and an
/// unstable spawn order would make the resulting mints unreproducible.
pub fn spawn_candidates(
    flows: &HashMap<String, SHACLFlow>,
    live_instances: &[FlowInstanceRecord],
    subject: &str,
    subject_classes: &[String],
) -> Vec<SpawnCandidate> {
    if subject.is_empty() || subject_classes.is_empty() {
        return Vec::new();
    }

    let mut out: Vec<SpawnCandidate> = flows
        .iter()
        .filter(|(_, flow)| {
            !flow.input_types.is_empty()
                && flow
                    .input_types
                    .iter()
                    .any(|accepted| subject_classes.iter().any(|c| c == accepted))
        })
        .filter(|(flow_uri, _)| {
            !live_instances
                .iter()
                .any(|inst| inst.flow_uri == **flow_uri && inst.subject == subject)
        })
        .map(|(flow_uri, flow)| SpawnCandidate {
            flow_uri: flow_uri.clone(),
            subject: subject.to_string(),
            initial_state: initial_state_of(flow),
        })
        .collect();

    out.sort_by(|a, b| a.flow_uri.cmp(&b.flow_uri));
    out
}

/// The state a fresh instance of `flow` begins in.
///
/// `states[0]` by the ordering convention documented on TS
/// `SHACLFlow.fromLinks` and enforced in `parse_flow_from_links`: states are
/// sorted by declared `value`, because link order isn't preserved on the graph.
/// `None` for a zero-state (atomic-action) flow.
pub fn initial_state_of(flow: &SHACLFlow) -> Option<String> {
    flow.states.first().map(|s| s.name.clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn flow(namespace: &str, input_types: &[&str], states: &[(&str, f64)]) -> SHACLFlow {
        serde_json::from_value(serde_json::json!({
            "name": "Delivery",
            "namespace": namespace,
            "states": states
                .iter()
                .map(|(name, value)| serde_json::json!({ "name": name, "value": value }))
                .collect::<Vec<_>>(),
            "transitions": [],
            "inputTypes": input_types,
            "outputTypes": [],
        }))
        .expect("fixture flow deserializes")
    }

    fn catalogue(entries: Vec<(&str, SHACLFlow)>) -> HashMap<String, SHACLFlow> {
        entries
            .into_iter()
            .map(|(uri, flow)| (uri.to_string(), flow))
            .collect()
    }

    fn instance(flow_uri: &str, subject: &str) -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_uri: flow_uri.to_string(),
            instance_uri: format!("ad4m://flow/instance/{flow_uri}-{subject}"),
            subject: subject.to_string(),
            current_state: "identified".to_string(),
            created_at: None,
        }
    }

    const TASK: &str = "ad4m://Task";
    const ITEM: &str = "ad4m://task/onboarding";

    #[test]
    fn class_match_with_no_live_instance_is_a_candidate() {
        let flows = catalogue(vec![(
            "delivery://DeliveryFlow",
            flow(
                "delivery://",
                &[TASK],
                &[("identified", 0.0), ("done", 1.0)],
            ),
        )]);

        let got = spawn_candidates(&flows, &[], ITEM, &[TASK.to_string()]);

        assert_eq!(
            got,
            vec![SpawnCandidate {
                flow_uri: "delivery://DeliveryFlow".to_string(),
                subject: ITEM.to_string(),
                initial_state: Some("identified".to_string()),
            }]
        );
    }

    #[test]
    fn a_live_instance_of_the_same_flow_suppresses_the_candidate() {
        let flows = catalogue(vec![(
            "delivery://DeliveryFlow",
            flow("delivery://", &[TASK], &[("identified", 0.0)]),
        )]);

        assert!(
            spawn_candidates(
                &flows,
                &[instance("delivery://DeliveryFlow", ITEM)],
                ITEM,
                &[TASK.to_string()]
            )
            .is_empty(),
            "a flow already running on this item must not spawn twice"
        );
    }

    #[test]
    fn suppression_is_scoped_to_the_pair_not_the_flow_or_the_subject() {
        let flows = catalogue(vec![(
            "delivery://DeliveryFlow",
            flow("delivery://", &[TASK], &[("identified", 0.0)]),
        )]);

        // Same flow, *different* item → still a candidate.
        assert_eq!(
            spawn_candidates(
                &flows,
                &[instance("delivery://DeliveryFlow", "ad4m://task/other")],
                ITEM,
                &[TASK.to_string()]
            )
            .len(),
            1,
            "an instance on another item must not suppress this one"
        );

        // Same item, *different* flow → still a candidate.
        assert_eq!(
            spawn_candidates(
                &flows,
                &[instance("other://OtherFlow", ITEM)],
                ITEM,
                &[TASK.to_string()]
            )
            .len(),
            1,
            "an instance of another flow must not suppress this one"
        );
    }

    #[test]
    fn class_mismatch_and_unknown_class_spawn_nothing() {
        let flows = catalogue(vec![(
            "delivery://DeliveryFlow",
            flow("delivery://", &[TASK], &[("identified", 0.0)]),
        )]);

        assert!(
            spawn_candidates(&flows, &[], ITEM, &["ad4m://Message".to_string()]).is_empty(),
            "a class no flow accepts spawns nothing"
        );
        // Empty classes is what `subject_classes_of` absence looks like.
        assert!(
            spawn_candidates(&flows, &[], ITEM, &[]).is_empty(),
            "an item of no registered class spawns nothing"
        );
        assert!(
            spawn_candidates(&flows, &[], "", &[TASK.to_string()]).is_empty(),
            "an empty subject spawns nothing"
        );
    }

    #[test]
    fn empty_input_types_never_spawns_automatically() {
        // `availableFlows` treats empty inputTypes as "applies to everything",
        // but that surface is a menu a human picks from. Auto-spawning on the
        // same reading would mint this flow on every extracted item.
        let flows = catalogue(vec![(
            "catchall://DeliveryFlow",
            flow("catchall://", &[], &[("identified", 0.0)]),
        )]);

        assert!(
            spawn_candidates(&flows, &[], ITEM, &[TASK.to_string()]).is_empty(),
            "a catch-all flow must not auto-spawn"
        );
    }

    #[test]
    fn any_matching_class_suffices_and_output_is_sorted() {
        // `subject_classes_of` returns the whole conformance chain, most
        // specific first; a flow accepting the *parent* class still matches.
        let flows = catalogue(vec![
            (
                "zeta://DeliveryFlow",
                flow("zeta://", &["ad4m://Item"], &[("identified", 0.0)]),
            ),
            (
                "alpha://DeliveryFlow",
                flow("alpha://", &[TASK], &[("identified", 0.0)]),
            ),
        ]);

        let got = spawn_candidates(
            &flows,
            &[],
            ITEM,
            &[TASK.to_string(), "ad4m://Item".to_string()],
        );

        assert_eq!(
            got.iter().map(|c| c.flow_uri.as_str()).collect::<Vec<_>>(),
            vec!["alpha://DeliveryFlow", "zeta://DeliveryFlow"],
            "both match, and the order must not depend on HashMap iteration"
        );
    }

    #[test]
    fn zero_state_flow_is_a_candidate_with_no_initial_state() {
        // Design §10 makes zero-state flows first-class atomic actions.
        let flows = catalogue(vec![(
            "atomic://DeliveryFlow",
            flow("atomic://", &[TASK], &[]),
        )]);

        let got = spawn_candidates(&flows, &[], ITEM, &[TASK.to_string()]);

        assert_eq!(got.len(), 1);
        assert_eq!(
            got[0].initial_state, None,
            "a zero-state flow has no state to start in"
        );
    }

    #[test]
    fn initial_state_is_lowest_value_not_declaration_order() {
        // The parser sorts by `value`, so `states[0]` is the initial state even
        // when the JSON declares them out of order. This is the invariant a
        // spawn depends on — mint in the wrong state and every subsequent
        // transition guard reads against the wrong `fromState`.
        let f = flow(
            "delivery://",
            &[TASK],
            &[("done", 1.0), ("identified", 0.0), ("scoped", 0.5)],
        );
        // Fixture goes through serde, not the link parser, so sort here to
        // model post-parse state; asserts the accessor honours position 0.
        let mut sorted = f;
        sorted
            .states
            .sort_by(|a, b| a.value.partial_cmp(&b.value).unwrap());
        assert_eq!(initial_state_of(&sorted), Some("identified".to_string()));
    }
}

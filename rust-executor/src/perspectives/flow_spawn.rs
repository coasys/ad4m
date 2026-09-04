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

use super::flow_classes::mint_flow_instance;
use super::flow_context::{load_flow_instances, load_shacl_flows, FlowInstanceRecord};
use super::perspective_instance::PerspectiveInstance;
use super::shacl_parser::SHACLFlow;
use crate::agent::AgentContext;

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
/// Matching is on class **names**, not target-class URIs, because that is what
/// `subject_classes_of` yields and what TS `availableFlows` compares against
/// (`exprClasses.some(cls => flow.inputTypes.includes(cls))`) — a flow declares
/// `inputTypes: ["Task"]`. Comparing URIs here would make every typed flow
/// silently unmatchable, the same failure J#3 fixed on the TS side.
///
/// **A catch-all flow is never a deterministic spawn candidate** — neither
/// empty `inputTypes` nor the `"any"` wildcard. `availableFlows` reads both as
/// "applies to everything", but that surface is a *menu* a human picks from.
/// Auto-spawning on the same reading would mint an instance of such a flow on
/// every item the extraction pass ever produces. Requiring an explicit class
/// keeps the automatic path opt-in; a catch-all flow is still startable by hand.
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
                && !flow.input_types.iter().any(|t| t == "any")
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
///
/// Also `None` when `states[0]` has an empty name. A `hasState` edge whose
/// `stateName` link hasn't synced yet parses as `""` at value `0.0` and sorts
/// to (or ties for) the front — partially-observed revisions are a real
/// phenomenon (see the 2026-08-20 flake notes in `interpretation/overlay`).
/// An instance minted at `currentState: ""` would be permanently stuck: no
/// transition leaves `""`, and dedup suppresses on `(flow_uri, subject)`
/// regardless of state, so the correct instance could never be minted either.
/// A half-synced definition must spawn nothing.
pub fn initial_state_of(flow: &SHACLFlow) -> Option<String> {
    flow.states
        .first()
        .filter(|s| !s.name.is_empty())
        .map(|s| s.name.clone())
}

/// One `FlowInstance` minted by [`run_flow_spawn_pass`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpawnOutcome {
    pub flow_uri: String,
    pub subject: String,
    pub instance_uri: String,
    pub initial_state: String,
}

/// Mint a `FlowInstance` for every flow that should start on each freshly
/// created item — the write half of the spawn pass.
///
/// `created_bases` is deliberately the **created** items of an interpretation
/// run, not every base it touched. `InterpretationOutcome.bases` also contains
/// items that were merely updated or given a relation, and design §10 scopes v1
/// spawn to "a new instance matching any `inputTypes` URI". Passing updated
/// items too would start flows on pre-existing content the first time anything
/// edits it, which is a different (and much louder) behaviour than the one
/// specified.
///
/// **Soft-fail throughout — but, unlike `run_engine_proposal_pass`, NOT
/// self-healing.** The proposal pass re-derives eligibility from graph state
/// every pass, so a dropped write there costs one cycle. Spawn eligibility
/// comes from `created_bases` — the op list of the one run that created the
/// item — so every skipped or failed mint here is *permanent*: nothing ever
/// re-offers the item. The same gap means editing a processor's `flows`
/// selection does not backfill items created before the edit. The missing
/// piece is a reconciliation read ("which items of a selected flow's
/// `inputTypes` carry no instance of it?") — deliberately a later slice,
/// because whether pre-existing items should auto-spawn at all is an open
/// product call (see the #954 review discussion on spawn boundedness).
/// Until then, failures below log at `error` level because there is no retry.
/// Extraction has already committed its writes by the time this runs, so
/// returning an error would fail a run whose actual work succeeded.
///
/// **Zero-state flows are skipped.** They are candidates in the pure rule — the
/// affordance is real — but an instance with no state has nothing to track, and
/// design §8 makes their spawn row the *atomic-action* row: they are fired, not
/// instantiated. Firing them is its own slice; minting a stateless instance now
/// would leave rows no transition can ever advance.
pub async fn run_flow_spawn_pass(
    perspective: &mut PerspectiveInstance,
    created_bases: &[String],
    context: &AgentContext,
    flow_filter: Option<&[String]>,
) -> Vec<SpawnOutcome> {
    if created_bases.is_empty() {
        return Vec::new();
    }

    let mut flows = match load_shacl_flows(perspective).await {
        Ok(flows) if !flows.is_empty() => flows,
        Ok(_) => return Vec::new(),
        Err(e) => {
            log::warn!("run_flow_spawn_pass: load_shacl_flows failed, no flows spawned: {e:#}");
            return Vec::new();
        }
    };
    super::flow_context::retain_selected_flows(&mut flows, flow_filter);
    if flows.is_empty() {
        return Vec::new();
    }

    let classes_by_uri = match perspective.subject_classes_of(created_bases) {
        Ok(map) => map,
        Err(e) => {
            log::error!(
                "run_flow_spawn_pass: subject_classes_of failed, no flows spawned — \
                 spawn is not retried for these items: {e:#}"
            );
            return Vec::new();
        }
    };

    // One batch around the whole fan-out: a pass that creates 20 items
    // matching 3 flows would otherwise publish 60 separate `Shared`
    // p-diff-sync revisions, and peers running their own spawn passes would
    // read the dedup set mid-fan-out. All-or-nothing on failure — a mint
    // error may have left partial writes in the batch, and committing those
    // would publish a half-minted instance (the exact 2026-08-20 bug shape).
    let batch_id = perspective.create_batch().await;
    let mut batch_failed = false;

    let mut spawned = Vec::new();
    'bases: for base in created_bases {
        let Some(classes) = classes_by_uri.get(base) else {
            // Absent means no *registered* class matched — see
            // `subject_classes_of`. Nothing to match `inputTypes` against.
            continue;
        };

        let live = match load_flow_instances(perspective, std::slice::from_ref(base)).await {
            Ok(live) => live,
            Err(e) => {
                log::error!(
                    "run_flow_spawn_pass: load_flow_instances({base}) failed — \
                     spawn is not retried for this item: {e:#}"
                );
                continue;
            }
        };

        for candidate in spawn_candidates(&flows, &live, base, classes) {
            let Some(initial_state) = candidate.initial_state.clone() else {
                let is_atomic = flows
                    .get(&candidate.flow_uri)
                    .is_some_and(|f| f.states.is_empty());
                if is_atomic {
                    log::debug!(
                        "run_flow_spawn_pass: {} is zero-state (atomic action) — not instantiating on {base}",
                        candidate.flow_uri
                    );
                } else {
                    // States exist but the first has no name — a half-synced
                    // definition (see `initial_state_of`). Spawn will NOT be
                    // retried for this item once the definition finishes
                    // syncing, hence the loud level.
                    log::error!(
                        "run_flow_spawn_pass: {} looks half-synced (first state unnamed) — not instantiating on {base}, and spawn is not retried",
                        candidate.flow_uri
                    );
                }
                continue;
            };

            let instance_id = uuid::Uuid::new_v4().to_string();
            match mint_flow_instance(
                perspective,
                &candidate.flow_uri,
                base,
                &initial_state,
                &instance_id,
                Some(batch_id.clone()),
                context,
            )
            .await
            {
                Ok(instance_uri) => {
                    log::info!(
                        "🌱 flow spawned: {} on {base} at state `{initial_state}` ({instance_uri})",
                        candidate.flow_uri
                    );
                    spawned.push(SpawnOutcome {
                        flow_uri: candidate.flow_uri,
                        subject: base.clone(),
                        instance_uri,
                        initial_state,
                    });
                }
                Err(e) => {
                    // The failed mint may have written partial links into the
                    // shared batch; committing the rest would publish them.
                    log::error!(
                        "run_flow_spawn_pass: mint_flow_instance({}, {base}) failed — \
                         discarding the pass's batch; spawn is not retried: {e:#}",
                        candidate.flow_uri
                    );
                    batch_failed = true;
                    break 'bases;
                }
            }
        }
    }

    if batch_failed {
        let _ = perspective.discard_batch(&batch_id).await;
        return Vec::new();
    }
    if spawned.is_empty() {
        // Nothing written — drop the empty batch rather than committing a
        // no-op revision.
        let _ = perspective.discard_batch(&batch_id).await;
        return spawned;
    }
    if let Err(e) = perspective.commit_batch(batch_id.clone(), context).await {
        // Defense-in-depth, matching `write_processor`: `commit_batch` removes
        // the batch on failure per its contract, but drop it explicitly so a
        // control-flow change there can't strand a stale batch.
        let _ = perspective.discard_batch(&batch_id).await;
        log::error!(
            "run_flow_spawn_pass: commit_batch failed — nothing spawned, \
             and spawn is not retried for these items: {e:#}"
        );
        return Vec::new();
    }

    spawned
}

/// The base URIs an interpretation pass is about to **create**, in op order,
/// deduplicated.
///
/// This is the input contract of [`run_flow_spawn_pass`]: design §10 scopes v1
/// spawn to *new* instances, so `Update` and `AddLinks` bases are excluded —
/// see the doc comment on [`run_flow_spawn_pass`] for why passing updated
/// items too would be a different behaviour. Computed from the planned ops
/// rather than `apply_with_overlay`'s return value because the latter mixes
/// created and updated bases indistinguishably.
pub fn created_bases_of(ops: &[super::interpretation::InterpretationOp]) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for op in ops {
        if let super::interpretation::InterpretationOp::Create { base, .. } = op {
            if !out.iter().any(|b| b == base) {
                out.push(base.clone());
            }
        }
    }
    out
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

        // The `"any"` wildcard is the other spelling of the same thing, and
        // `availableFlows` treats them identically — so this must not depend on
        // "any" merely failing to match a class name by accident.
        let wildcard = catalogue(vec![(
            "catchall://DeliveryFlow",
            flow("catchall://", &["any"], &[("identified", 0.0)]),
        )]);
        assert!(
            spawn_candidates(&wildcard, &[], ITEM, &[TASK.to_string()]).is_empty(),
            "the `any` wildcard must not auto-spawn either"
        );
        // Even when the item genuinely is of a class the flow also lists.
        let wildcard_plus = catalogue(vec![(
            "catchall://DeliveryFlow",
            flow("catchall://", &["any", TASK], &[("identified", 0.0)]),
        )]);
        assert!(
            spawn_candidates(&wildcard_plus, &[], ITEM, &[TASK.to_string()]).is_empty(),
            "a wildcard anywhere in inputTypes opts the flow out of auto-spawn"
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
    fn half_synced_flow_with_empty_state_name_yields_no_initial_state() {
        // A `hasState` edge whose `stateName` link hasn't synced parses as
        // `""` at value 0.0 and sorts to the front. Minting `currentState: ""`
        // would wedge the instance forever (no transition leaves `""`) AND
        // suppress the correct mint via (flow_uri, subject) dedup — so the
        // accessor must answer "spawn nothing" instead.
        let f = flow("delivery://", &[TASK], &[("", 0.0), ("identified", 0.5)]);
        assert_eq!(initial_state_of(&f), None);

        // Through the candidate rule the flow still appears (the affordance is
        // real once the definition finishes syncing) but carries no initial
        // state, which the write half skips exactly like a zero-state flow.
        let flows = catalogue(vec![("delivery://DeliveryFlow", {
            let mut f = flow("delivery://", &[TASK], &[("", 0.0), ("identified", 0.5)]);
            f.states
                .sort_by(|a, b| a.value.partial_cmp(&b.value).unwrap());
            f
        })]);
        let got = spawn_candidates(&flows, &[], ITEM, &[TASK.to_string()]);
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].initial_state, None);
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

    // ---------------------------------------------------------------------
    // Write half — against a real PerspectiveInstance, no LLM.
    // ---------------------------------------------------------------------

    /// Delivery flow accepting `Todo`, the class `setup_perspective_no_llm`
    /// registers, so a created Todo is a genuine spawn target.
    fn delivery_flow_json_for(input_type: &str) -> String {
        serde_json::json!({
            "name": "Delivery",
            "namespace": "delivery://",
            "start_action": [],
            "states": [
                { "name": "identified", "value": 0.0 },
                { "name": "scoped", "value": 0.5 },
            ],
            "transitions": [
                {
                    "action_name": "Scope",
                    "from_state": "identified",
                    "to_state": "scoped",
                    "actions": []
                }
            ],
            "inputTypes": [input_type],
            "outputTypes": [],
        })
        .to_string()
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn spawn_pass_mints_an_instance_and_is_idempotent() {
        use crate::perspectives::flow_context::load_flow_instances;
        use crate::perspectives::interpretation_test_support::{
            setup_perspective_no_llm, TASK_SDNA,
        };
        use crate::perspectives::shacl_parser::parse_flow_to_links;
        use crate::types::LinkStatus;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;

        // `inputTypes` holds class *names* — what `subject_classes_of` returns
        // and what TS `availableFlows` compares against.
        for link in parse_flow_to_links(&delivery_flow_json_for("Task"), "Delivery")
            .expect("parse_flow_to_links")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition)");
        }

        // Create a Task the way the extraction pass does, so the spawn pass
        // sees exactly what a real run hands it.
        let base = "ad4m://task/onboarding".to_string();
        perspective
            .create_subject(
                crate::perspectives::perspective_instance::SubjectClassOption {
                    class_name: Some("Task".to_string()),
                    query: None,
                },
                base.clone(),
                Some(serde_json::json!({ "title": "write the spawn pass" })),
                None,
                &ctx,
            )
            .await
            .expect("create_subject(Task)");

        // Stage checks, so a failure names the broken link in the chain rather
        // than just "nothing spawned".
        let catalogue_on_graph = load_shacl_flows(&perspective)
            .await
            .expect("load_shacl_flows");
        let on_graph = catalogue_on_graph
            .get("delivery://DeliveryFlow")
            .unwrap_or_else(|| {
                panic!(
                    "flow definition must be readable back, got {:?}",
                    catalogue_on_graph.keys().collect::<Vec<_>>()
                )
            });
        assert_eq!(
            on_graph.input_types,
            vec!["Task".to_string()],
            "inputTypes must survive the write/read round-trip"
        );
        assert_eq!(
            on_graph
                .states
                .iter()
                .map(|s| s.name.as_str())
                .collect::<Vec<_>>(),
            vec!["identified", "scoped"],
            "states must round-trip in value order"
        );
        let classes = perspective
            .subject_classes_of(&[base.clone()])
            .expect("subject_classes_of");
        assert_eq!(
            classes.get(&base).map(|v| v.as_slice()),
            Some(["Task".to_string()].as_slice()),
            "the created item must classify as Task, got {classes:?}"
        );

        let live = load_flow_instances(&perspective, &[base.clone()]).await;
        let candidates = spawn_candidates(
            &catalogue_on_graph,
            live.as_deref().unwrap_or(&[]),
            &base,
            classes.get(&base).expect("classified above"),
        );
        assert_eq!(
            candidates.len(),
            1,
            "the pure rule must produce a candidate on these exact inputs; live={live:?}"
        );

        let spawned = run_flow_spawn_pass(&mut perspective, &[base.clone()], &ctx, None).await;

        assert_eq!(
            spawned.len(),
            1,
            "one matching flow ⇒ one mint, got {spawned:?}"
        );
        assert_eq!(spawned[0].flow_uri, "delivery://DeliveryFlow");
        assert_eq!(spawned[0].subject, base);
        assert_eq!(
            spawned[0].initial_state, "identified",
            "must start at the lowest-value state"
        );

        // It's on the graph, not just in the return value.
        let live = load_flow_instances(&perspective, &[base.clone()])
            .await
            .expect("load_flow_instances");
        assert_eq!(live.len(), 1, "the mint must be readable back");
        assert_eq!(live[0].instance_uri, spawned[0].instance_uri);
        assert_eq!(live[0].current_state, "identified");
        assert_eq!(live[0].flow_uri, "delivery://DeliveryFlow");

        // Running again must not double-spawn — the live instance suppresses
        // the candidate. This is what makes the pass safe to call on every
        // interpretation run.
        let again = run_flow_spawn_pass(&mut perspective, &[base.clone()], &ctx, None).await;
        assert!(
            again.is_empty(),
            "second pass must mint nothing, got {again:?}"
        );
        assert_eq!(
            load_flow_instances(&perspective, &[base])
                .await
                .expect("load_flow_instances")
                .len(),
            1,
            "still exactly one instance on the graph"
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn spawn_pass_mints_nothing_when_no_flow_accepts_the_class() {
        use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
        use crate::perspectives::shacl_parser::parse_flow_to_links;
        use crate::types::LinkStatus;

        let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;

        // Flow accepts a class nothing on this perspective is.
        for link in parse_flow_to_links(&delivery_flow_json_for("SomethingElse"), "Delivery")
            .expect("parse_flow_to_links")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition)");
        }

        let spawned = run_flow_spawn_pass(
            &mut perspective,
            &["ad4m://task/unknown".to_string()],
            &ctx,
            None,
        )
        .await;

        assert!(
            spawned.is_empty(),
            "an unregistered item must not spawn anything, got {spawned:?}"
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn spawn_pass_on_empty_input_is_a_no_op() {
        use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;

        let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        assert!(run_flow_spawn_pass(&mut perspective, &[], &ctx, None)
            .await
            .is_empty());
    }

    /// Flow targeting (Nico 2026-09-04): the pass mints only for flows the
    /// caller selected. An empty selection is flow-blind; an unrelated
    /// selection spawns nothing; naming the flow spawns it.
    #[tokio::test(flavor = "multi_thread")]
    async fn spawn_pass_honours_flow_selection() {
        use crate::perspectives::flow_context::load_flow_instances;
        use crate::perspectives::interpretation_test_support::{
            setup_perspective_no_llm, TASK_SDNA,
        };
        use crate::perspectives::shacl_parser::parse_flow_to_links;
        use crate::types::LinkStatus;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
        for link in parse_flow_to_links(&delivery_flow_json_for("Task"), "Delivery")
            .expect("parse_flow_to_links")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition)");
        }
        let base = "ad4m://task/selection".to_string();
        perspective
            .create_subject(
                crate::perspectives::perspective_instance::SubjectClassOption {
                    class_name: Some("Task".to_string()),
                    query: None,
                },
                base.clone(),
                Some(serde_json::json!({ "title": "target the flow" })),
                None,
                &ctx,
            )
            .await
            .expect("create_subject(Task)");

        // Empty selection = flow-blind pass.
        let none_selected: Vec<String> = vec![];
        let spawned = run_flow_spawn_pass(
            &mut perspective,
            &[base.clone()],
            &ctx,
            Some(&none_selected),
        )
        .await;
        assert!(spawned.is_empty(), "empty selection must be flow-blind");

        // A selection naming some other flow spawns nothing either.
        let other = vec!["coasys://SomethingElseFlow".to_string()];
        let spawned =
            run_flow_spawn_pass(&mut perspective, &[base.clone()], &ctx, Some(&other)).await;
        assert!(spawned.is_empty(), "unrelated selection must not spawn");
        let live = load_flow_instances(&perspective, &[base.clone()])
            .await
            .expect("load_flow_instances");
        assert!(
            live.is_empty(),
            "no instance minted under a non-matching selection"
        );

        // Naming the flow spawns it.
        let selected = vec!["delivery://DeliveryFlow".to_string()];
        let spawned =
            run_flow_spawn_pass(&mut perspective, &[base.clone()], &ctx, Some(&selected)).await;
        assert_eq!(spawned.len(), 1, "selected flow must spawn: {spawned:?}");
        assert_eq!(spawned[0].flow_uri, "delivery://DeliveryFlow");
    }

    mod created_bases {
        use crate::perspectives::flow_spawn::created_bases_of;
        use crate::perspectives::interpretation::InterpretationOp;

        fn create(base: &str) -> InterpretationOp {
            InterpretationOp::Create {
                base: base.to_string(),
                class: "Task".to_string(),
                values: serde_json::Map::new(),
            }
        }

        fn update(base: &str) -> InterpretationOp {
            InterpretationOp::Update {
                base: base.to_string(),
                class: "Task".to_string(),
                values: serde_json::Map::new(),
            }
        }

        #[test]
        fn keeps_creates_only_in_op_order() {
            let ops = vec![
                update("ad4m://a"),
                create("ad4m://b"),
                InterpretationOp::AddLinks {
                    source: "ad4m://c".to_string(),
                    links: vec![],
                },
                create("ad4m://d"),
            ];
            assert_eq!(created_bases_of(&ops), vec!["ad4m://b", "ad4m://d"]);
        }

        #[test]
        fn dedups_repeated_create_bases() {
            let ops = vec![create("ad4m://a"), create("ad4m://b"), create("ad4m://a")];
            assert_eq!(created_bases_of(&ops), vec!["ad4m://a", "ad4m://b"]);
        }

        #[test]
        fn update_on_a_created_base_does_not_remove_it() {
            let ops = vec![create("ad4m://a"), update("ad4m://a")];
            assert_eq!(created_bases_of(&ops), vec!["ad4m://a"]);
        }

        #[test]
        fn empty_ops_yield_no_bases() {
            assert!(created_bases_of(&[]).is_empty());
        }
    }
}

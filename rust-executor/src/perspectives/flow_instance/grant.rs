//! `producedByFlow` on a role: membership granted by another flow
//! completing.
//!
//! An ordinary `fromRole` gate asks the graph a question — *is there a
//! `Reviewer` instance whose `agent` is this DID?* — and dates the answer from
//! the assignment link, a timestamp its own author stamped on it
//! ([`roles`](super::roles) § *Accepted caveat*). `producedByFlow` adds a
//! second condition to the same question and replaces the dating:
//!
//! > the matched instance must be a **valid output of a completed run of flow
//! > F** in the named state, and the grant begins at the moment that run
//! > reached quorum.
//!
//! It is the same name, and the same check, as the model-query filter
//! `where: { producedByFlow }` (#1127).
//!
//! # A thin layer on `produced`
//!
//! "Valid output of flow F" is [`produced`](super::produced)'s question, and
//! this module does not answer it a second time:
//!
//! ```text
//!   F --ad4m://flow/flow_receipt--> receipt      (F's index, anyone writes)
//!            │ load_flow_receipts: scoped to F, refuses over budget
//!            ▼
//!   produced::first_produced_at(own catalogue, F, state)
//!            │ verify_receipt per receipt; flow, state, (class, id)
//!            ▼
//!   produced_at = earliest settled_at per id     (absent = not a member)
//!            │ carried in the read-set, per instance
//!            ▼
//!   RoleGrantEvidence::resolve: window from produced_at, no fallback
//! ```
//!
//! Everything a receipt must clear is `produced`'s: it verifies under the
//! replica's own catalogue (signatures, DNA hash, re-fold, outputs hashing to
//! the commitment the final edge's quorum signed), it was minted for F, the
//! replica's own fold settled it into the named state, and it names the
//! instance as **`(class, id)`**. The class is the role query's `className`
//! read from the replica's own flow definition. A node committed as a `Task`
//! is not a produced `Reviewer`, even with the same id.
//!
//! `settled_at` is the moment the n-th distinct eligible voter signed the
//! final edge. No participant picks it, and back-dating it means producing a
//! different quorum.
//!
//! # Where the check runs, and what travels
//!
//! Eligibility is checked **inside the neighbourhood**, by each replica
//! against its own graph: [`resolve_role_grants`](super::roles::resolve_role_grants)
//! calls [`produced_at_by_instance`] while it collects the evidence. The
//! read-set then carries only the result, per instance:
//! [`RoleInstanceHistory::produced_at`](super::roles::RoleInstanceHistory::produced_at).
//! The receipt that granted it does not travel.
//!
//! ## What a receipt of a gated flow proves
//!
//! A receipt minted for a gated flow re-folds its read-set with the carried
//! `produced_at` dates, and trusts them. So it proves that n people signed,
//! and when. It does **not**, on its own, prove to a reader outside the
//! neighbourhood that those people were allowed to sign: that was decided by
//! the minting replica against its graph, and the reader takes the date on
//! the same footing as it takes the carried grant links. Verification does
//! not recurse into the granting flow's receipts, so there is no depth to
//! bound and nothing to multiply.
//!
//! Referencing the granting receipt by hash, optionally carrying it, and
//! transitive verification with a depth budget are deferred to #1140.
//!
//! # A receipt flood is an error, never "not a member"
//!
//! Anyone can write F's index. Over
//! [`MAX_FLOW_RECEIPTS`](super::produced::MAX_FLOW_RECEIPTS) the read is a
//! [`ReceiptBudgetExceeded`](super::produced::ReceiptBudgetExceeded) error,
//! and [`resolve_role_grants`](super::roles::resolve_role_grants) propagates
//! it, so the flow whose rule asked cannot derive a state until the flood is
//! gone. "I could not read every receipt" is neither "not granted" (a
//! truncated read hides the witness) nor "granted". It is loud, and it
//! stops only the flows gated on F.
//!
//! A granting flow this replica holds no definition for is an error for the
//! same reason: no receipt for it can be verified. That is the rule
//! [`flow_valid_outputs`](super::produced::flow_valid_outputs) applies to an
//! unknown flow.
//!
//! # Not being granted is not an error
//!
//! An instance with no `produced_at` contributes **no window**, exactly as if
//! the role query had not matched it. It is an ordinary "not a member"
//! answer, not the fail-closed abort that
//! [`RoleGrantEvidence::resolve`](super::roles::RoleGrantEvidence::resolve)
//! raises for a grant it cannot place in time.
//!
//! **There is no fallback.** When a role query carries `producedByFlow`,
//! neither the assignment links nor `asserted_instance_timestamp` can date the
//! grant — if they could, writing a plain assignment link would grant the role
//! with no receipt at all, and the gate would be decorative.
//!
//! **A `count` satisfied by zero is refused** (`{ max: 0 }` with the gate):
//! it would make every reason a receipt fails to verify a reason to grant.
//! See [`RoleGrantEvidence::resolve`](super::roles::RoleGrantEvidence::resolve).
//!
//! # Revocation: what a receipt freezes and what it does not
//!
//! **A verified receipt is permanent, and undoing the flow does not undo the
//! grant.** That is the ratchet ([`super::receipt`], [`super::verify`]):
//! retracting a settling vote moves the *live* flow back, while the receipt —
//! which froze the links as they stood — keeps verifying. Somebody who
//! completes a grant flow and then withdraws their own vote still holds the
//! role. This is deliberate, and it is the whole reason receipts exist, but it
//! is not what most people assume when they configure a gate.
//!
//! What *can* end a `producedByFlow` membership is the ordinary role-grant
//! tombstone — a new signed event, per [`roles`](super::roles) § *What a grant
//! is*: `instance --ad4m://flow/role_grant_revoked--> did`, honoured from its
//! own timestamp, and only from an author
//! [`revocation_authorised`](super::roles::evidence::revocation_authorised) accepts.
//! So, concretely, for anyone writing social DNA:
//!
//! | Role query's `where.author` | Who can un-grant |
//! | --- | --- |
//! | absent | anyone — the DNA declared no authority, so it grants none |
//! | `"$did"` | only the holder, on themselves |
//! | a specific DID | only that DID; **if it is unreachable, nobody, and the grant is permanent** |
//!
//! There is no way to make a `producedByFlow` grant expire, and no way to make
//! it conditional on the run *staying* complete. Anything that must be
//! revocable needs an authority in the role query that is going to be there
//! later.

use super::produced::first_produced_at;
use crate::perspectives::flow_evaluator::RequiresQueryable;
use crate::perspectives::shacl_parser::ProducedByFlow;
use std::collections::BTreeMap;

/// When flow `spec` first produced each instance of `role_class`, by
/// instance id — the `produced_at` a `producedByFlow` gate carries.
///
/// The whole check is [`first_produced_at`] over this replica's own
/// catalogue and F's receipts: the receipt verifies, is for `spec.flow`,
/// settled into `spec.state`, and names the instance as
/// `(role_class, id)`. Earliest wins when several receipts qualify. An id
/// missing from the map is an ordinary "not a member".
///
/// `role_class` must be the role query's `className` from this replica's own
/// flow definition: an output committed as another class is not a produced
/// member of this one.
///
/// Errors, never an empty map, when the answer cannot be decided: F is not in
/// this replica's catalogue, or F's index is over budget (module header).
pub(crate) async fn produced_at_by_instance<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    role_class: &str,
    spec: &ProducedByFlow,
) -> anyhow::Result<BTreeMap<String, String>> {
    let catalogue = perspective.flow_catalogue().await?;
    if !catalogue.contains_key(&spec.flow) {
        anyhow::bail!(
            "producedByFlow: the `{role_class}` role gate is granted by flow `{}`, which is not in this replica's catalogue, so no receipt for it can be verified and no candidate's membership can be decided (fail-closed)",
            spec.flow
        );
    }
    let receipts = perspective.flow_receipts(&spec.flow).await.map_err(|e| {
        e.context(format!(
            "producedByFlow: the `{role_class}` role gate's granting flow `{}` could not be \
             read, so no candidate's membership can be decided",
            spec.flow
        ))
    })?;
    Ok(
        first_produced_at(&catalogue, &spec.flow, Some(&spec.state), &receipts)
            .into_iter()
            .filter(|(output, _)| output.class_name == role_class)
            .map(|(output, at)| (output.id, at))
            .collect(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_evaluator::EvidenceItem;
    use crate::perspectives::flow_instance::atom::{
        outputs_hash, proposal_uri, OutputRef, EVIDENCE_HASHES_PREDICATE, FLOW_INSTANCE_PREDICATE,
        FROM_STATE_PREDICATE, OUTPUTS_HASH_PREDICATE, OUTPUT_PREDICATE, PROPOSAL_NONCE_PREDICATE,
        PROPOSER_PREDICATE, ROLE_GRANT_REVOKED_PREDICATE, TO_STATE_PREDICATE,
    };
    use crate::perspectives::flow_instance::fold_read_set;
    use crate::perspectives::flow_instance::produced::{produced_by_flow, ReceiptBudgetExceeded};
    use crate::perspectives::flow_instance::receipt::FlowReceipt;
    use crate::perspectives::flow_instance::roles::{
        resolve_role_grants, RoleGrant, RoleGrantEvidence, RoleGrantWindow, RoleInstanceHistory,
    };
    use crate::perspectives::flow_instance::test_support::{
        did_of, literal, signed_link, INSTANCE, T1, T2, T3,
    };
    use crate::perspectives::flow_instance::verify::{verify_receipt, ReceiptVerdict};
    use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};
    use crate::perspectives::shacl_parser::{ModelQuery, ModelQueryCount, SHACLFlow};
    use crate::types::LinkExpression;
    use async_trait::async_trait;
    use serde_json::{json, Value};
    use std::collections::HashMap;
    use std::sync::Mutex;

    const BASE: &str = "ad4m://task/t1";
    /// The node that is both the granting run's output and the role instance
    /// the gate matches. Using one URI for both is what a grant flow *is*:
    /// the thing the run produced is the membership.
    const ROLE_INSTANCE: &str = "ad4m://role/reviewer/r0";
    /// A second role instance for the same DID, for the per-instance binding.
    const ROLE_INSTANCE_2: &str = "ad4m://role/reviewer/r1";
    const SOMEBODY_ELSE: &str = "ad4m://role/reviewer/somebody-else";
    const ROLE: &str = "coasys://Reviewer";
    /// Another class a flow might commit the very same node as.
    const TASK_CLASS: &str = "coasys://Task";
    const ALICE: &str = "alice";
    const BOB: &str = "bob";
    const SEAL: &str = "seal-1";
    /// Earlier than the receipt's quorum time, and earlier than the vote. If
    /// a grant is ever dated from here rather than from the receipt, the
    /// window opens too early and the test that looks for it says so.
    const ASSIGNMENT_LINK_AT: &str = "2025-12-01T00:00:00.000Z";

    // ---- fixtures --------------------------------------------------------

    fn flow_json(name: &str, states: Value, transitions: Value) -> SHACLFlow {
        serde_json::from_value(json!({
            "name": name,
            "namespace": "coasys://",
            "states": states,
            "transitions": transitions,
        }))
        .expect("fixture flow parses")
    }

    /// The flow that *grants*: ungated, `open → done`, settles on one vote.
    fn granting_flow(name: &str) -> SHACLFlow {
        flow_json(
            name,
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    }

    /// The `Reviewer` role query, gated on `spec` when given. `None` is the
    /// same query as an ordinary `didProperty` role.
    fn role(spec: Option<&ProducedByFlow>) -> ModelQuery {
        let mut query = json!({ "className": ROLE, "didProperty": "agent" });
        if let Some(spec) = spec {
            query["producedByFlow"] = serde_json::to_value(spec).expect("spec serialises");
        }
        serde_json::from_value(query).expect("role query parses")
    }

    /// The flow that *consumes* a grant: only a member of the gated role may
    /// settle `done`.
    fn gated_flow(spec: &ProducedByFlow) -> SHACLFlow {
        let role = serde_json::to_value(role(Some(spec))).expect("role serialises");
        flow_json(
            "Delivery",
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 1, "fromRole": role } },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    }

    fn spec(flow_uri: &str, state: &str) -> ProducedByFlow {
        ProducedByFlow {
            flow: flow_uri.into(),
            state: state.into(),
        }
    }

    /// `id` as the quorum commits to it, as an instance of `class`.
    fn item(class: &str, id: &str) -> EvidenceItem {
        EvidenceItem {
            id: id.to_string(),
            class_name: class.to_string(),
            content: json!({ "id": id }).to_string(),
        }
    }

    /// `id` as a committed `Reviewer`: what a grant flow produces.
    fn role_item(id: &str) -> EvidenceItem {
        item(ROLE, id)
    }

    /// Alice's self-voted proposal `open → to` at `at`, committing to
    /// `outputs` (#1104). Built link by link rather than through the shared
    /// `signed_terminal_proposal` fixture, because that one names every output
    /// as a `coasys://Deliverable`, and the class of the output is exactly
    /// what several tests here vary.
    fn proposal(to: &str, at: &str, outputs: &[EvidenceItem]) -> ProposalLinks {
        let committed = outputs_hash(outputs);
        let nonce = format!("{to}@{at}");
        let uri = proposal_uri(
            INSTANCE,
            "open",
            to,
            SEAL,
            Some(&committed),
            did_of(ALICE),
            &nonce,
        );
        let signed = |predicate: &str, target: &str| {
            signed_link(&uri, predicate, target, ALICE, true, None, at)
        };
        let mut links = vec![
            signed(PROPOSER_PREDICATE, did_of(ALICE)),
            signed(FLOW_INSTANCE_PREDICATE, INSTANCE),
            signed(FROM_STATE_PREDICATE, &literal("open")),
            signed(TO_STATE_PREDICATE, &literal(to)),
            signed(EVIDENCE_HASHES_PREDICATE, &literal(SEAL)),
            signed(PROPOSAL_NONCE_PREDICATE, &literal(&nonce)),
        ];
        links.extend(
            outputs
                .iter()
                .map(|o| signed(OUTPUT_PREDICATE, &literal(&OutputRef::of(o).encode()))),
        );
        links.push(signed(OUTPUTS_HASH_PREDICATE, &literal(&committed)));
        ProposalLinks { uri, links }
    }

    fn read_set(
        to: &str,
        at: &str,
        outputs: &[EvidenceItem],
        role_grants: Vec<RoleGrantEvidence>,
    ) -> ReadSet {
        ReadSet {
            instance_uri: INSTANCE.to_string(),
            subject: BASE.to_string(),
            genesis: "open".to_string(),
            proposals: vec![proposal(to, at, outputs)],
            role_grants,
        }
    }

    /// A receipt for a run of `flow` into `to` at `at`, committing to and
    /// carrying `outputs`.
    fn receipt_for(flow: &SHACLFlow, to: &str, at: &str, outputs: &[EvidenceItem]) -> FlowReceipt {
        FlowReceipt::mint(
            flow,
            read_set(to, at, outputs, Vec::new()),
            outputs.to_vec(),
            Vec::new(),
        )
        .expect("the fixture read-set mints")
    }

    /// The gated flow's own run: Alice votes `open → done` at T3, carrying
    /// `role_grants`.
    fn gated_run(role_grants: Vec<RoleGrantEvidence>) -> ReadSet {
        read_set("done", T3, &[item(TASK_CLASS, BASE)], role_grants)
    }

    /// Alice's genuine, signed assignment link on `id`: exactly what an
    /// ordinary `didProperty` gate grants from.
    fn assignment(id: &str) -> LinkExpression {
        signed_link(
            id,
            "agent",
            did_of(ALICE),
            "admin",
            true,
            None,
            ASSIGNMENT_LINK_AT,
        )
        .into()
    }

    /// Alice's evidence for `ROLE_INSTANCE` as the loader would carry it,
    /// with `produced_at` as given — the pure half's whole input.
    fn carried(produced_at: Option<&str>, revocations: Vec<LinkExpression>) -> RoleGrantEvidence {
        RoleGrantEvidence {
            to_state: "done".into(),
            role_class: ROLE.into(),
            did: did_of(ALICE).into(),
            instances: vec![RoleInstanceHistory {
                instance_id: ROLE_INSTANCE.into(),
                grant_links: vec![assignment(ROLE_INSTANCE)],
                revocation_links: revocations,
                asserted_instance_timestamp: Some(ASSIGNMENT_LINK_AT.into()),
                produced_at: produced_at.map(str::to_string),
            }],
        }
    }

    /// The role query as `role_grant_views` translates it for `did` before
    /// `resolve` sees it. No `where.author`, so anyone may revoke — the row
    /// the module doc's table calls out.
    fn translated(did: &str) -> Value {
        json!({ "className": ROLE, "where": { "agent": did } })
    }

    fn window_for<'w>(view: &'w RoleGrant, instance_id: &str) -> Option<&'w RoleGrantWindow> {
        view.windows.iter().find(|w| w.instance_id == instance_id)
    }

    /// A store that answers the role query with `instances` for Alice (and
    /// nothing for anyone else), hands out Alice's signed assignment link on
    /// each, holds `catalogue`, and returns `receipts` as the granting flow's
    /// index. Records which flows' receipts were asked for.
    ///
    /// The history comes back the way `model_query` answers `links` (#1103):
    /// under `__links`, one array per requested key — the tombstone
    /// predicate gets no rows, any other key Alice's assignment link.
    struct GateStore {
        instances: Vec<&'static str>,
        catalogue: HashMap<String, SHACLFlow>,
        receipts: Result<Vec<FlowReceipt>, ReceiptBudgetExceeded>,
        asked: Mutex<Vec<String>>,
    }

    #[async_trait]
    impl RequiresQueryable for GateStore {
        async fn model_query(&self, _class: &str, query_json: &str) -> anyhow::Result<String> {
            let query: Value = serde_json::from_str(query_json)?;
            let keys: Vec<String> = query["links"]
                .as_array()
                .into_iter()
                .flatten()
                .filter_map(Value::as_str)
                .map(str::to_string)
                .collect();
            let instances: Vec<Value> = if query_json.contains(did_of(ALICE)) {
                self.instances
                    .iter()
                    .map(|id| {
                        let links: serde_json::Map<String, Value> = keys
                            .iter()
                            .map(|key| {
                                let rows = if key == ROLE_GRANT_REVOKED_PREDICATE {
                                    Vec::new()
                                } else {
                                    vec![assignment(id)]
                                };
                                (key.clone(), json!(rows))
                            })
                            .collect();
                        json!({ "id": id, "timestamp": ASSIGNMENT_LINK_AT, "__links": links })
                    })
                    .collect()
            } else {
                Vec::new()
            };
            Ok(json!({ "totalCount": instances.len(), "instances": instances }).to_string())
        }

        async fn flow_receipts(&self, flow_uri: &str) -> anyhow::Result<Vec<FlowReceipt>> {
            self.asked.lock().unwrap().push(flow_uri.to_string());
            self.receipts.clone().map_err(anyhow::Error::from)
        }

        async fn flow_catalogue(&self) -> anyhow::Result<HashMap<String, SHACLFlow>> {
            Ok(self.catalogue.clone())
        }
    }

    fn store(flows: &[&SHACLFlow], receipts: Vec<FlowReceipt>) -> GateStore {
        GateStore {
            instances: vec![ROLE_INSTANCE],
            catalogue: flows.iter().map(|f| (f.flow_uri(), (*f).clone())).collect(),
            receipts: Ok(receipts),
            asked: Mutex::new(Vec::new()),
        }
    }

    /// The loader, for Alice and Bob, against the gated flow's run record.
    async fn collect(
        db: &GateStore,
        gate: &ModelQuery,
        gated: &SHACLFlow,
    ) -> anyhow::Result<Vec<RoleGrantEvidence>> {
        let record = gated_run(Vec::new()).as_record(gated);
        let candidates = [did_of(ALICE).to_string(), did_of(BOB).to_string()];
        resolve_role_grants(db, "done", gate, &record, &candidates).await
    }

    /// Alice's grant through the whole path: the loader decides the gate
    /// against `db`, and the pure half resolves the carried result.
    async fn alice_grant(db: &GateStore, gate_spec: &ProducedByFlow) -> RoleGrant {
        let gate = role(Some(gate_spec));
        let evidence = collect(db, &gate, &gated_flow(gate_spec))
            .await
            .expect("the gate can be decided");
        evidence
            .iter()
            .find(|e| e.did == did_of(ALICE))
            .expect("evidence for Alice")
            .resolve(&translated(did_of(ALICE)), &gate)
            .expect("resolves")
    }

    // ---- the grant itself, through the loader ------------------------------

    /// **The point of the whole feature.** A completed run grants the role,
    /// the grant is dated from the run's *quorum*, not from the assignment
    /// link sitting on the same instance, and the gated flow settles on it.
    ///
    /// Asserting only that a window exists would also pass on an
    /// implementation that ignored the receipt and dated the grant from the
    /// assignment link. `ASSIGNMENT_LINK_AT` is earlier than the quorum, so
    /// the two answers are distinguishable and the wrong one is the *wider*
    /// window.
    ///
    /// Red if the loader asks for any flow but the gate's, if `resolve` falls
    /// through to the grant-link dating, or if the gate is not applied at all.
    #[tokio::test]
    async fn a_verified_receipt_grants_the_role_and_dates_it_from_the_quorum() {
        let granting = granting_flow("Onboarding");
        let gate_spec = spec(&granting.flow_uri(), "done");
        let gated = gated_flow(&gate_spec);
        let receipt = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)]);
        let db = store(&[&granting], vec![receipt]);

        let evidence = collect(&db, &role(Some(&gate_spec)), &gated)
            .await
            .expect("decidable");
        assert_eq!(
            *db.asked.lock().unwrap(),
            vec![granting.flow_uri()],
            "the granting flow's index, read once for the whole role"
        );
        let alice = evidence
            .iter()
            .find(|e| e.did == did_of(ALICE))
            .expect("evidence for Alice");
        assert_eq!(
            alice.instances[0].produced_at.as_deref(),
            Some(T1),
            "the read-set carries the grant result, dated from the quorum"
        );

        let view = alice
            .resolve(&translated(did_of(ALICE)), &role(Some(&gate_spec)))
            .expect("resolves");
        assert_eq!(
            view.windows
                .iter()
                .map(|w| w.granted_at.as_str())
                .collect::<Vec<_>>(),
            vec![T1],
            "the grant begins at the granting run's QUORUM time, not at the assignment link \
             ({ASSIGNMENT_LINK_AT}) that sits on the same instance"
        );

        assert_eq!(
            fold_read_set(&gated, &gated_run(evidence).reverified())
                .expect("folds")
                .state,
            "done",
            "the vote counts, so the gated edge settles"
        );
    }

    /// **The live check is `produced`'s full verification.** The #1104
    /// re-mint: take a genuine, fully signed run that committed to somebody
    /// else's node, and rewrite the carried `outputs` to name this one —
    /// right class, right id. Every signature in it is real. Only the outputs
    /// commitment the final edge's quorum signed stops it.
    ///
    /// The verdict is pinned, not just the absent window, so it is the
    /// commitment check answering and not some earlier failure in the
    /// fixture. Red if the loader trusts `receipt.outputs` without verifying.
    #[tokio::test]
    async fn a_forged_re_mint_naming_the_instance_grants_nothing() {
        let granting = granting_flow("Onboarding");
        let gate_spec = spec(&granting.flow_uri(), "done");
        let genuine = receipt_for(&granting, "done", T1, &[role_item(SOMEBODY_ELSE)]);
        let mut forged = genuine.clone();
        forged.outputs = vec![role_item(ROLE_INSTANCE)];

        let cat: HashMap<String, SHACLFlow> =
            std::iter::once((granting.flow_uri(), granting.clone())).collect();
        assert!(
            matches!(
                verify_receipt(&cat, &forged),
                ReceiptVerdict::OutputsNotCommitted { .. }
            ),
            "precondition: the forgery is refused by the outputs commitment"
        );
        assert!(
            alice_grant(&store(&[&granting], vec![forged]), &gate_spec)
                .await
                .windows
                .is_empty(),
            "a re-mint naming this instance grants nothing"
        );

        // Control: an honest receipt naming the instance, same flow, grants.
        let honest = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)]);
        assert_eq!(
            alice_grant(&store(&[&granting], vec![honest]), &gate_spec)
                .await
                .windows
                .len(),
            1
        );
    }

    /// **The gate is `produced`'s check and nothing else.** Hold the node
    /// fixed and vary what the receipt names, because the receipt is the half
    /// an attacker controls. For every case the gate's answer must equal
    /// [`produced_by_flow`]'s answer for `(Reviewer, ROLE_INSTANCE)` over the
    /// same receipts.
    ///
    /// `names the node among others` names it **second**, which is what
    /// separates `contains` from `first()`; two strangers must not grant
    /// anybody (Lal's arity finding on the first version of this PR).
    #[tokio::test]
    async fn a_receipt_grants_exactly_the_nodes_it_names() {
        let granting = granting_flow("Onboarding");
        let gate_spec = spec(&granting.flow_uri(), "done");
        let cat: HashMap<String, SHACLFlow> =
            std::iter::once((granting.flow_uri(), granting.clone())).collect();
        let this = OutputRef {
            class_name: ROLE.into(),
            id: ROLE_INSTANCE.into(),
        };
        let minted = |ids: &[&str]| {
            let outputs: Vec<EvidenceItem> = ids.iter().map(|id| role_item(id)).collect();
            receipt_for(&granting, "done", T1, &outputs)
        };

        for (label, receipt, expected) in [
            ("names the node", minted(&[ROLE_INSTANCE]), 1),
            ("names a different node", minted(&[SOMEBODY_ELSE]), 0),
            (
                "names the node among others",
                minted(&[SOMEBODY_ELSE, ROLE_INSTANCE]),
                1,
            ),
            (
                "names several nodes, none of them this one",
                minted(&[
                    "ad4m://role/reviewer/other-a",
                    "ad4m://role/reviewer/other-b",
                ]),
                0,
            ),
        ] {
            let receipts = vec![receipt];
            let windows = alice_grant(&store(&[&granting], receipts.clone()), &gate_spec)
                .await
                .windows
                .len();
            assert_eq!(
                windows, expected,
                "{label}: a receipt grants exactly the nodes it names"
            );
            assert_eq!(
                windows == 1,
                produced_by_flow(&cat, &this, &granting.flow_uri(), Some("done"), &receipts),
                "{label}: the role gate and `produced_by_flow` must give ONE answer"
            );
        }
    }

    /// **Same id, another class, grants nothing.** A node committed as a
    /// `Task` is not a produced `Reviewer`: the quorum signed the node's
    /// content as read through `Task`, and said nothing about it as anything
    /// else (#1108's `speaks_for` rule). The class is the role query's
    /// `className` from the replica's own flow definition.
    ///
    /// Red if the binding compares ids only.
    #[tokio::test]
    async fn the_same_id_committed_as_another_class_does_not_grant() {
        let granting = granting_flow("Onboarding");
        let gate_spec = spec(&granting.flow_uri(), "done");

        let as_task = receipt_for(&granting, "done", T1, &[item(TASK_CLASS, ROLE_INSTANCE)]);
        assert!(
            alice_grant(&store(&[&granting], vec![as_task]), &gate_spec)
                .await
                .windows
                .is_empty(),
            "the node was committed as a Task; a Reviewer gate must not count it"
        );

        // The control: the same node, committed as a Reviewer, grants.
        let as_reviewer = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)]);
        assert_eq!(
            alice_grant(&store(&[&granting], vec![as_reviewer]), &gate_spec)
                .await
                .windows
                .len(),
            1,
            "so the refusal above is the class, not a receipt that never verified"
        );
    }

    /// The binding is **per instance**: two matched instances of the same
    /// role for the same agent, and a receipt that names only the second.
    /// Only the second is granted, and each is dated from the run that
    /// granted *it*.
    ///
    /// The positive half settles the two runs at different times, because
    /// handing `r1` its sibling's `T1` is a mistake a window count cannot
    /// see (@marvin-bot-coasys's finding on the first version of this PR).
    #[tokio::test]
    async fn a_receipt_does_not_grant_a_sibling_instance_of_the_same_role() {
        let granting = granting_flow("Onboarding");
        let gate_spec = spec(&granting.flow_uri(), "done");
        let names_second = receipt_for(&granting, "done", T2, &[role_item(ROLE_INSTANCE_2)]);
        let names_first = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)]);
        let two = |receipts: Vec<FlowReceipt>| GateStore {
            instances: vec![ROLE_INSTANCE, ROLE_INSTANCE_2],
            ..store(&[&granting], receipts)
        };

        let view = alice_grant(&two(vec![names_second.clone()]), &gate_spec).await;
        assert!(
            window_for(&view, ROLE_INSTANCE).is_none(),
            "a receipt naming `{ROLE_INSTANCE_2}` does not grant its sibling"
        );
        assert_eq!(
            window_for(&view, ROLE_INSTANCE_2).map(|w| w.granted_at.as_str()),
            Some(T2)
        );

        let view = alice_grant(&two(vec![names_second, names_first]), &gate_spec).await;
        assert_eq!(
            window_for(&view, ROLE_INSTANCE).map(|w| w.granted_at.as_str()),
            Some(T1),
            "`{ROLE_INSTANCE}` is dated from the run that granted IT"
        );
        assert_eq!(
            window_for(&view, ROLE_INSTANCE_2).map(|w| w.granted_at.as_str()),
            Some(T2),
            "`{ROLE_INSTANCE_2}` is dated from the run that granted IT, not from its sibling's \
             earlier quorum"
        );
    }

    /// A receipt that verifies perfectly still grants nothing when it is for
    /// a different flow, or for a different ending of the right flow.
    ///
    /// **The two cases are built so that neither check can stand in for the
    /// other.** The wrong-flow receipt settles into a state spelled exactly
    /// like the one its gate names (`done`), so only the flow URI separates
    /// them; the wrong-ending receipt is for exactly the flow its gate names,
    /// so only the state does. Each case is paired with the gate that DOES
    /// accept the same receipt.
    ///
    /// Red if `produced`'s flow pre-check or its state filter is dropped.
    #[tokio::test]
    async fn a_verified_receipt_for_another_flow_or_another_ending_grants_nothing() {
        let onboarding = granting_flow("Onboarding");
        let training = granting_flow("Training");
        let forked = flow_json(
            "Review",
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "approved", "value": 1.0 },
                { "name": "rejected", "value": 1.0 },
            ]),
            json!([
                { "action_name": "Approve", "from_state": "open", "to_state": "approved", "actions": [] },
                { "action_name": "Reject", "from_state": "open", "to_state": "rejected", "actions": [] },
            ]),
        );
        let flows = [&onboarding, &training, &forked];

        let wrong_flow = receipt_for(&onboarding, "done", T1, &[role_item(ROLE_INSTANCE)]);
        let wrong_ending = receipt_for(&forked, "rejected", T1, &[role_item(ROLE_INSTANCE)]);

        for (label, receipt, refused_by, accepted_by) in [
            (
                "a receipt for another flow that ends in the same-named state",
                wrong_flow,
                spec(&training.flow_uri(), "done"),
                spec(&onboarding.flow_uri(), "done"),
            ),
            (
                "a receipt for the right flow's other ending",
                wrong_ending,
                spec(&forked.flow_uri(), "approved"),
                spec(&forked.flow_uri(), "rejected"),
            ),
        ] {
            assert!(
                alice_grant(&store(&flows, vec![receipt.clone()]), &refused_by)
                    .await
                    .windows
                    .is_empty(),
                "{label} must not grant"
            );
            assert_eq!(
                alice_grant(&store(&flows, vec![receipt]), &accepted_by)
                    .await
                    .windows
                    .len(),
                1,
                "{label}: but the gate it DOES match grants on the same receipt, so the \
                 refusal above is the comparison and not a verification failure"
            );
        }
    }

    /// Two runs both granted the same instance: the membership began at the
    /// **first** of them. Both orderings are asserted, because a comparison
    /// that returned whichever it was handed first (or last) would pass one.
    ///
    /// Red with `produced::earlier_of` returning the later of the two.
    #[tokio::test]
    async fn two_granting_runs_date_the_membership_from_the_first() {
        let granting = granting_flow("Onboarding");
        let gate_spec = spec(&granting.flow_uri(), "done");
        let at = |t: &str| receipt_for(&granting, "done", t, &[role_item(ROLE_INSTANCE)]);
        let (early, late) = (at(T1), at(T2));
        assert_ne!(early, late, "precondition: two distinct receipts");

        for (label, receipts) in [
            ("earlier first", vec![early.clone(), late.clone()]),
            ("later first", vec![late, early]),
        ] {
            let view = alice_grant(&store(&[&granting], receipts), &gate_spec).await;
            assert_eq!(
                view.windows
                    .iter()
                    .map(|w| w.granted_at.as_str())
                    .collect::<Vec<_>>(),
                vec![T1],
                "{label}: one membership, beginning at the first run that granted it"
            );
        }
    }

    // ---- what the loader refuses to decide ---------------------------------

    /// **A receipt flood is an error, not a silent deny.** Lal's review of
    /// #1127, applied to roles: if the granting flow's index is over budget,
    /// the role cannot be decided, and `resolve_role_grants` must say so with
    /// the typed error. Mapping it to "no receipts" would answer "not a
    /// member" for every candidate on a read that did not finish.
    ///
    /// And a role WITHOUT `producedByFlow` never reads receipts, so the same
    /// flood cannot touch it.
    ///
    /// Red if the loader error is swallowed into an empty list, or if the
    /// context wrapping hides the typed error from `downcast_ref`.
    #[tokio::test]
    async fn a_receipt_flood_is_a_budget_error_not_a_silent_deny() {
        let granting = granting_flow("Onboarding");
        let gate_spec = spec(&granting.flow_uri(), "done");
        let gated = gated_flow(&gate_spec);
        let flood = ReceiptBudgetExceeded {
            flow: granting.flow_uri(),
            found: 257,
            cap: 256,
        };
        let flooded = || GateStore {
            receipts: Err(flood.clone()),
            ..store(&[&granting], Vec::new())
        };

        let err = collect(&flooded(), &role(Some(&gate_spec)), &gated)
            .await
            .expect_err("an over-budget index must not resolve to anything");
        assert_eq!(
            err.downcast_ref::<ReceiptBudgetExceeded>(),
            Some(&flood),
            "the typed budget error reaches the caller: {err:#}"
        );

        // An ordinary role never asks, so F's flood cannot reach it.
        let plain = flooded();
        collect(&plain, &role(None), &gated)
            .await
            .expect("a role without producedByFlow does not read receipts");
        assert!(plain.asked.lock().unwrap().is_empty());
    }

    /// **A gate on a flow this replica does not hold is an error.** The same
    /// rule `flow_valid_outputs` applies (#1127): "I do not have F's rules"
    /// is not "no receipt grants this". Answering "not a member" instead
    /// would let a replica that has not synced F silently derive a different
    /// state for the gated flow than one that has.
    ///
    /// The control is the same store holding F: granted.
    #[tokio::test]
    async fn a_gate_naming_a_flow_this_replica_does_not_hold_is_an_error() {
        let granting = granting_flow("Onboarding");
        let gate_spec = spec(&granting.flow_uri(), "done");
        let gated = gated_flow(&gate_spec);
        let receipt = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)]);

        let err = collect(
            &store(&[], vec![receipt.clone()]),
            &role(Some(&gate_spec)),
            &gated,
        )
        .await
        .expect_err("a gate on a flow this replica does not hold cannot be decided");
        assert!(
            format!("{err:#}").contains(&granting.flow_uri()),
            "the error names the missing flow, got: {err:#}"
        );

        assert_eq!(
            alice_grant(&store(&[&granting], vec![receipt]), &gate_spec)
                .await
                .windows
                .len(),
            1
        );
    }

    // ---- the reading side: the carried result ------------------------------

    /// **No fallback.** The same instance with no `produced_at`: the gate
    /// must not date the grant from the assignment link.
    ///
    /// The evidence always carries a genuine, signed, correctly targeted
    /// assignment link AND a parseable instance timestamp — exactly what an
    /// ordinary `didProperty` gate grants from. The control resolves the very
    /// same evidence under the same role query minus `producedByFlow`, and it
    /// grants, so what changed the answer is the gate.
    ///
    /// Red if the `producedByFlow` branch in `resolve` falls through to the
    /// grant-link dating instead of skipping the instance.
    #[test]
    fn without_a_produced_date_the_assignment_link_does_not_grant() {
        let gate_spec = spec("coasys://OnboardingFlow", "done");
        let ev = carried(None, Vec::new());
        let view = ev
            .resolve(&translated(did_of(ALICE)), &role(Some(&gate_spec)))
            .expect("an ungranted instance is not an error — it is not a member");
        assert!(
            view.windows.is_empty(),
            "no produced date, no grant — got a window at {:?}",
            view.windows.first().map(|w| &w.granted_at)
        );
        assert_eq!(
            fold_read_set(
                &gated_flow(&gate_spec),
                &gated_run(vec![ev.clone()]).reverified()
            )
            .expect("an ungranted candidate is not a fold error")
            .state,
            "open",
            "and the gated edge does not settle"
        );

        let ungated = ev
            .resolve(&translated(did_of(ALICE)), &role(None))
            .expect("resolves");
        assert_eq!(
            ungated
                .windows
                .iter()
                .map(|w| w.granted_at.as_str())
                .collect::<Vec<_>>(),
            vec![ASSIGNMENT_LINK_AT],
            "an ordinary didProperty gate grants on this very evidence"
        );
    }

    /// **The reading side trusts the carried date and does not recurse.**
    /// A serialised read-set of a gated run, handed to a reader that holds
    /// only the gated flow's definition — not the granting flow, and none of
    /// its receipts. The carried `produced_at` survives the wire and
    /// `reverified`, and the fold settles on it. Take the date away and the
    /// same read-set does not settle.
    ///
    /// This is the light version's contract (#1140 has the rest): a receipt
    /// of a gated flow proves that n people signed; that they were allowed to
    /// was decided by the replica that collected the evidence.
    #[test]
    fn a_carried_grant_date_survives_the_wire_and_settles_the_gated_run() {
        let gate_spec = spec("coasys://OnboardingFlow", "done");
        let gated = gated_flow(&gate_spec);

        let wire = serde_json::to_string(&gated_run(vec![carried(Some(T1), Vec::new())]))
            .expect("serialises");
        let parsed: ReadSet = serde_json::from_str(&wire).expect("deserialises");
        assert_eq!(
            parsed.role_grants[0].instances[0].produced_at.as_deref(),
            Some(T1),
            "the grant result travels with the instance"
        );
        assert_eq!(
            fold_read_set(&gated, &parsed.reverified())
                .expect("folds")
                .state,
            "done"
        );

        let mut stripped = parsed;
        stripped.role_grants[0].instances[0].produced_at = None;
        assert_eq!(
            fold_read_set(&gated, &stripped.reverified())
                .expect("folds")
                .state,
            "open",
            "without the carried grant the vote does not count"
        );
    }

    /// The claim the module doc makes to anyone configuring a gate, made
    /// falsifiable: a receipt is permanent, a *membership* is not.
    ///
    /// Red if `resolve`'s `producedByFlow` branch builds its window without
    /// calling `revocations_on`. Then the grant would be genuinely
    /// irrevocable and the doc would be wrong.
    #[test]
    fn a_granted_role_is_still_ended_by_a_signed_tombstone() {
        let gate_spec = spec("coasys://OnboardingFlow", "done");
        let tombstone = signed_link(
            ROLE_INSTANCE,
            ROLE_GRANT_REVOKED_PREDICATE,
            did_of(ALICE),
            BOB,
            true,
            None,
            T2,
        );
        let ev = carried(Some(T1), vec![tombstone.into()]);
        let view = ev
            .resolve(&translated(did_of(ALICE)), &role(Some(&gate_spec)))
            .expect("resolves");
        assert_eq!(view.windows.len(), 1, "a tombstone ends a membership");
        assert_eq!(view.windows[0].granted_at, T1, "which began at the quorum");
        assert_eq!(view.windows[0].revoked_at(), Some(T2));
        assert!(
            !view.windows[0].open_at(T3),
            "a vote after it is not eligible"
        );
        assert!(view.windows[0].open_at(T1));

        assert_eq!(
            fold_read_set(&gated_flow(&gate_spec), &gated_run(vec![ev]).reverified())
                .expect("folds")
                .state,
            "open",
            "the revoked member cannot settle the gated edge"
        );
    }

    /// `producedByFlow` with a `count` satisfied by zero instances inverts the
    /// gate: "eligible while no verified receipt exists". Every reason a
    /// receipt might fail — an un-synced definition, a broken signature —
    /// would then become a reason to GRANT.
    ///
    /// The second half shows the refused behaviour is real: with no windows
    /// at all, `{max: 0}` reports the candidate as eligible.
    ///
    /// Red if the `cardinality_satisfied(count, 0)` guard is dropped from
    /// `resolve`.
    #[test]
    fn produced_by_flow_with_a_count_satisfied_by_zero_is_refused() {
        let gate_spec = spec("coasys://OnboardingFlow", "done");
        let ev = carried(None, Vec::new());
        let zero_ok = ModelQueryCount {
            min: None,
            max: Some(0),
        };
        let mut zero_gate = role(Some(&gate_spec));
        zero_gate.count = Some(zero_ok.clone());

        let err = ev
            .resolve(&translated(did_of(ALICE)), &zero_gate)
            .expect_err("a gate that grants when verification FAILS is not a gate");
        assert!(
            format!("{err:#}").contains("satisfied by zero instances"),
            "the refusal must name the inversion, got: {err:#}"
        );

        let ungranted = ev
            .resolve(&translated(did_of(ALICE)), &role(Some(&gate_spec)))
            .expect("resolves");
        assert!(ungranted.windows.is_empty());
        assert!(
            ungranted.eligible_at(T3, Some(&zero_ok)),
            "this is why the combination is refused rather than merely discouraged"
        );
    }
}

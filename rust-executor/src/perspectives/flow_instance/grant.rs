//! `grantedByFlow`: a role whose membership is granted by another flow
//! completing.
//!
//! An ordinary `fromRole` gate asks the graph a question — *is there a
//! `Reviewer` instance whose `agent` is this DID?* — and dates the answer from
//! the assignment link, a timestamp its own author stamped on it
//! ([`roles`](super::roles) § *Accepted caveat*). `grantedByFlow` adds a
//! second condition to the same question and replaces the dating:
//!
//! > the matched instance must be an **output of a completed run of flow F**,
//! > and the grant begins at the moment that run reached quorum.
//!
//! ```text
//!   role instance  --ad4m://flow/granted_by-->  receipt
//!   (the output)         (unsigned edge)        (verified here)
//!                                                   │
//!                                     outputs must name the instance
//!                                     flow_uri must be F
//!                                     terminal_state must be the named one
//!                                                   ▼
//!                             granted_at = the receipt's settled_at
//! ```
//!
//! That is what [`roles`](super::roles)' module doc promises when it says
//! roles granted as flow outputs "will carry a quorum-fixed time no single
//! party can back-date". [`SettledEdge::settled_at`](super::fold::SettledEdge)
//! is the moment the n-th distinct eligible voter signed; no participant picks
//! it, and back-dating it means producing a different quorum.
//!
//! # The gate is existential, and that decides the failure direction
//!
//! An instance is granted iff **some** carried receipt verifies for it under
//! the named flow and terminal state. So declining to evaluate a candidate
//! receipt — for any reason, including running out of depth budget — can only
//! remove a possible witness. It narrows, never widens.
//!
//! That is why an over-deep chain refuses *the receipt* rather than aborting
//! the fold. Aborting would be a denial-of-service handed to anybody: the
//! `granted_by` edges are unsigned multi-edges (see
//! [`verify`](super::verify) § *The binding check*), so a stranger who can
//! write links to a role instance could hang a deliberately deep chain on it
//! and stop an honest flow from deriving a state at all. Refusing the planted
//! receipt costs the attacker nothing and buys them nothing.
//!
//! # Not being granted is not an error
//!
//! An instance with no verifying receipt contributes **no window**, exactly as
//! if the role query had not matched it. It is an ordinary "not a member"
//! answer, not the fail-closed abort that
//! [`RoleGrantEvidence::resolve`](super::roles::RoleGrantEvidence::resolve)
//! raises for a grant it cannot place in time. The two differ because the
//! abort exists for a grant that *is* claimed and cannot be dated, while this
//! is a membership test returning false.
//!
//! **There is no fallback.** When a role query carries `grantedByFlow`,
//! neither the assignment links nor `asserted_instance_timestamp` can date the
//! grant — if they could, writing a plain assignment link would grant the role
//! with no receipt at all, and the gate would be decorative.
//!
//! # Why there is a depth cap
//!
//! Not cycles: a receipt's URI is the hash of its content, so a receipt cannot
//! contain itself and a cycle cannot be constructed.
//!
//! Not bytes either: nested receipts travel inside their parent, and
//! [`MAX_RECEIPT_BYTES`](super::receipt::MAX_RECEIPT_BYTES) already bounds
//! that.
//!
//! The cap exists for **DAG expansion**. Take N receipts where each names the
//! previous one twice among its role evidence. The material is O(N) bytes, and
//! folding the outermost one costs 2^N folds, because nothing memoises a
//! sub-receipt that two parents both point at. A reader asked to verify one
//! small artifact would sit there indefinitely. The cap bounds the *tree*,
//! which is the thing that grows.
//!
//! ## What the cap counts, and the consequence of that
//!
//! [`MAX_GRANT_DEPTH`] counts `granted_by` edges **followed from material the
//! reader already holds** — not receipts. A reader handed a receipt directly
//! may follow four levels below it; the live engine folding a perspective may
//! follow four levels below that fold.
//!
//! One consequence is worth stating rather than discovering: **verification is
//! not compositional past the cap.** A receipt that verifies standalone may
//! fail to verify once nested inside another, because the budget it had at the
//! top is smaller once some of it has been spent getting there. The direction
//! is fail-closed — nesting can only refuse more grants, never allow one — so
//! a deep chain degrades into a refusal rather than into a wrong answer. Flow
//! compositions this deep are not a shape anybody has asked for; if one ever
//! is, the fix is memoisation by receipt URI, not a larger constant.
//!
//! **Open: which kind of denial that is.** Running out of budget currently
//! surfaces as a [`Rejected`](super::verify::VerdictKind::Rejected) verdict,
//! because the gate simply fails and the fold reports an ordinary state
//! mismatch. It arguably belongs in
//! [`Undecidable`](super::verify::VerdictKind::Undecidable) instead: the same
//! bytes verify for a reader handed the sub-receipt directly, so "I could not
//! reach that far from where I stand" is a finding about the reader, not about
//! the material — and `Undecidable` is where this module's own three-kind
//! doctrine puts those. The two buckets differ in what a payout system may
//! conclude: `Rejected` is evidence against the receipt, `Undecidable` is not.
//! Nothing is unsafe either way — both refuse the grant — so this is a
//! taxonomy question, not a hole, and it is left open deliberately rather than
//! settled in passing. Raised by @lal-bot-coasys reviewing the `grantedByFlow`
//! PR; `a_chain_one_deeper_than_the_budget_is_refused_not_allowed` therefore
//! pins the refusal and not the bucket.
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
//! What *can* end a `grantedByFlow` membership is the ordinary role-grant
//! tombstone — a new signed event, per [`roles`](super::roles) § *What a grant
//! is*: `instance --ad4m://flow/role_grant_revoked--> did`, honoured from its
//! own timestamp, and only from an author
//! [`revocation_authorised`](super::roles::revocation_authorised) accepts.
//! So, concretely, for anyone writing social DNA:
//!
//! | Role query's `where.author` | Who can un-grant |
//! | --- | --- |
//! | absent | anyone — the DNA declared no authority, so it grants none |
//! | `"$did"` | only the holder, on themselves |
//! | a specific DID | only that DID; **if it is unreachable, nobody, and the grant is permanent** |
//!
//! There is no way to make a `grantedByFlow` grant expire, and no way to make
//! it conditional on the run *staying* complete. Anything that must be
//! revocable needs an authority in the role query that is going to be there
//! later. (Tombstoning an output that is not itself the role instance remains
//! unbuilt — the deferred item in the design doc's §6.)

#[cfg(test)]
use super::receipt::flow_dna_hash;
use super::receipt::FlowReceipt;
use super::verify::{verify_receipt_within, ReceiptVerdict};
use crate::perspectives::shacl_parser::{GrantedByFlow, SHACLFlow};
#[cfg(test)]
use crate::types::DecoratedLinkExpression;
use std::collections::HashMap;

/// How many `ad4m://flow/granted_by` edges a reader will follow away from
/// material it already holds. See the module header, § *Why there is a depth
/// cap* — this bounds a tree, not a byte count, and it is a crate constant
/// every replica shares so that mint and verify reach the same answer.
pub const MAX_GRANT_DEPTH: usize = 4;

/// What a reader needs to decide a `grantedByFlow` gate: the definitions to
/// verify nested receipts against, and how much further it may go.
///
/// Threaded rather than ambient, and not defaultable, because both halves are
/// load-bearing: a reader with the wrong catalogue reaches `FlowUnknown` on
/// every nested receipt and silently grants nothing, and a reader with no
/// budget accounting recurses on a planted chain.
#[derive(Debug, Clone, Copy)]
pub struct GrantContext<'a> {
    catalogue: &'a HashMap<String, SHACLFlow>,
    remaining: usize,
}

impl<'a> GrantContext<'a> {
    /// A reader starting from material it already holds — a perspective it is
    /// folding, or a receipt it was handed — with the full budget.
    pub fn root(catalogue: &'a HashMap<String, SHACLFlow>) -> Self {
        GrantContext {
            catalogue,
            remaining: MAX_GRANT_DEPTH,
        }
    }

    /// The definitions this reader holds. Nested receipts are verified against
    /// the reader's **own** catalogue, never anything carried — the same rule
    /// that makes a top-level receipt un-forgeable by its minter.
    pub fn catalogue(&self) -> &'a HashMap<String, SHACLFlow> {
        self.catalogue
    }

    /// The context for following one more `granted_by` edge, or `None` at the
    /// cap.
    ///
    /// `None` is a refusal, never a pass: the only caller
    /// ([`granted_by_flow_at`]) treats it as "this receipt does not grant".
    /// Returning an `Option` rather than a saturating counter is deliberate —
    /// a counter that stopped decrementing would let the recursion continue
    /// forever at zero, which is the failure this exists to prevent.
    pub fn deeper(&self) -> Option<GrantContext<'a>> {
        self.remaining.checked_sub(1).map(|remaining| GrantContext {
            catalogue: self.catalogue,
            remaining,
        })
    }

    /// Edges still followable. Diagnostics only.
    pub fn remaining(&self) -> usize {
        self.remaining
    }

    /// A root context over an empty catalogue, for fixtures whose flows carry
    /// no `grantedByFlow` gate.
    ///
    /// Test-only and deliberately not a production convenience: with nothing
    /// in the catalogue every nested receipt is
    /// [`FlowUnknown`](super::verify::ReceiptVerdict::FlowUnknown) and grants
    /// nothing, which is the right answer for a fixture that has no grant
    /// gate and a silently wrong one for anything that does.
    #[cfg(test)]
    pub fn empty() -> GrantContext<'static> {
        use std::sync::LazyLock;
        static NOTHING: LazyLock<HashMap<String, SHACLFlow>> = LazyLock::new(HashMap::new);
        GrantContext::root(&NOTHING)
    }
}

/// When flow `spec` granted `instance`, if any carried receipt says so.
///
/// `Some(settled_at)` is the quorum-fixed moment the granting run completed —
/// what [`RoleGrantWindow::granted_at`](super::roles::RoleGrantWindow) becomes
/// for this instance. `None` means no carried receipt granted it, which is an
/// ordinary "not a member" and not an error (module header).
///
/// Every candidate must clear **four** independent things, and dropping any
/// one of them would make the gate decorative:
///
/// 1. the receipt verifies under the reader's own catalogue — the whole of
///    [`verify_receipt_within`], signatures, DNA hash, replay and all;
/// 2. it was reached from a node it names (`arrived_from = instance`) —
///    otherwise anyone hangs a stranger's genuine receipt off their own role
///    instance and collects the grant;
/// 3. its `flow_uri` is the flow the gate named — otherwise completing *any*
///    flow grants *every* `grantedByFlow` role;
/// 4. its terminal state is the state the gate named — otherwise reaching a
///    flow's `rejected` state grants what its `approved` state was meant to.
///
/// Earliest wins when several receipts qualify. That is the semantically right
/// answer — the membership began at the first run that granted it — and it is
/// safe to prefer the wider one here for a reason that does not hold for grant
/// links: every candidate has been fully verified, so widening the window
/// requires producing a whole quorum, not writing a link.
pub fn granted_by_flow_at(
    ctx: GrantContext<'_>,
    instance_id: &str,
    spec: &GrantedByFlow,
    receipts: &[FlowReceipt],
) -> Option<String> {
    let mut earliest: Option<String> = None;
    for receipt in receipts {
        let Some(deeper) = ctx.deeper() else {
            log::debug!(
                "grantedByFlow: `{instance_id}`: not following any further `granted_by` edge — \
                 the chain is already {MAX_GRANT_DEPTH} deep. The grant is refused, not assumed."
            );
            break;
        };
        let verdict = verify_receipt_within(deeper, receipt, Some(instance_id));
        let ReceiptVerdict::Verified {
            terminal_state,
            settled_at,
            ..
        } = &verdict
        else {
            log::debug!(
                "grantedByFlow: `{instance_id}`: a carried receipt does not grant it — {verdict}"
            );
            continue;
        };
        if receipt.flow_uri != spec.flow {
            log::debug!(
                "grantedByFlow: `{instance_id}`: a receipt for `{}` does not grant a role gated \
                 on `{}`",
                receipt.flow_uri,
                spec.flow
            );
            continue;
        }
        if *terminal_state != spec.terminal_state {
            log::debug!(
                "grantedByFlow: `{instance_id}`: a receipt for `{}` settled into `{terminal_state}`, \
                 not the `{}` the gate names",
                receipt.flow_uri,
                spec.terminal_state
            );
            continue;
        }
        earliest = Some(match earliest {
            None => settled_at.clone(),
            Some(current) => earlier_of(current, settled_at.clone()),
        });
    }
    earliest
}

/// The earlier of two settle times **by parsed instant**, never by string:
/// they are client-asserted RFC 3339 and clients disagree on flavour, so
/// string order diverges from instant order inside a second (#1000).
///
/// An unparseable settle time sorts **first** and therefore wins, which is the
/// fail-closed direction and not an accident: `granted_at` runs through
/// [`parse_link_timestamp`](super::time::parse_link_timestamp) in
/// [`RoleGrantWindow::open_at`](super::roles::RoleGrantWindow::open_at), where
/// a value that cannot be placed in time means the window never opens. So one
/// undatable receipt among several closes the grant rather than letting a
/// datable sibling carry it — the same rule
/// [`RoleGrantWindow::revoked_at`](super::roles::RoleGrantWindow::revoked_at)
/// applies to tombstones. The string tiebreaker keeps two equally unparseable
/// values resolving identically on every replica.
fn earlier_of(a: String, b: String) -> String {
    use super::time::parse_link_timestamp;
    let key = |s: &String| (parse_link_timestamp(s), s.clone());
    if key(&b) < key(&a) {
        b
    } else {
        a
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_instance::atom::fixtures::{
        did_of, signed_link, signed_proposal, T1, T2, T3,
    };
    use crate::perspectives::flow_instance::atom::ROLE_GRANT_REVOKED_PREDICATE;
    use crate::perspectives::flow_instance::fold_read_set;
    use crate::perspectives::flow_instance::roles::{RoleGrantEvidence, RoleInstanceHistory};
    use crate::perspectives::flow_instance::verify::VerdictKind;
    use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};
    use crate::perspectives::shacl_parser::ModelQueryCount;
    use serde_json::{json, Value};

    const INSTANCE: &str = "ad4m://flow/instance/i1";
    const BASE: &str = "ad4m://task/t1";
    /// The node that is both the granting run's output and the role instance
    /// the gate matches. Using one URI for both is what a grant flow *is*:
    /// the thing the run produced is the membership.
    const ROLE_INSTANCE: &str = "ad4m://role/reviewer/r0";
    const ROLE: &str = "coasys://Reviewer";
    const ALICE: &str = "alice";
    const BOB: &str = "bob";
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

    /// A `grantedByFlow` role gate, as a `fromRole` query.
    fn gate(flow_uri: &str, terminal_state: &str) -> Value {
        json!({
            "className": ROLE,
            "didProperty": "agent",
            "grantedByFlow": { "flow": flow_uri, "terminalState": terminal_state },
        })
    }

    /// The flow that *consumes* a grant: only a member of the gated role may
    /// settle `done`.
    fn gated_flow(name: &str, role: Value) -> SHACLFlow {
        flow_json(
            name,
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 1, "fromRole": role } },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    }

    fn catalogue(flows: Vec<SHACLFlow>) -> HashMap<String, SHACLFlow> {
        flows.into_iter().map(|f| (f.flow_uri(), f)).collect()
    }

    fn read_set(to: &str, at: &str, role_grants: Vec<RoleGrantEvidence>) -> ReadSet {
        ReadSet {
            instance_uri: INSTANCE.to_string(),
            subject: BASE.to_string(),
            genesis: "open".to_string(),
            proposals: vec![ProposalLinks {
                uri: "ad4m://p/1".to_string(),
                links: signed_proposal("ad4m://p/1", ALICE, "open", to, "seal-1", at),
            }],
            role_grants,
        }
    }

    /// A receipt for `flow`, minted from a settled run, bound to
    /// `ROLE_INSTANCE`. `outputs` is what the binding check reads.
    fn receipt_for(
        flow: &SHACLFlow,
        rs: ReadSet,
        outputs: &[&str],
        cat: &HashMap<String, SHACLFlow>,
    ) -> FlowReceipt {
        FlowReceipt::mint(
            flow,
            rs,
            outputs.iter().map(|o| o.to_string()).collect(),
            Vec::new(),
            GrantContext::root(cat),
        )
        .expect("the fixture read-set mints")
    }

    /// The evidence the gated flow's read-set carries for Alice: one matched
    /// role instance, an assignment link and instance timestamp that a
    /// `grantedByFlow` gate must ignore, and whatever receipts were found on
    /// the instance's `granted_by` edges.
    fn evidence(
        receipts: Vec<FlowReceipt>,
        revocations: Vec<DecoratedLinkExpression>,
    ) -> Vec<RoleGrantEvidence> {
        vec![RoleGrantEvidence {
            to_state: "done".into(),
            role_class: ROLE.into(),
            did: did_of(ALICE).into(),
            instances: vec![RoleInstanceHistory {
                instance_id: ROLE_INSTANCE.into(),
                grant_links: vec![signed_link(
                    ROLE_INSTANCE,
                    "agent",
                    did_of(ALICE),
                    "admin",
                    true,
                    None,
                    ASSIGNMENT_LINK_AT,
                )],
                revocation_links: revocations,
                asserted_instance_timestamp: Some(ASSIGNMENT_LINK_AT.into()),
                granting_receipts: receipts,
            }],
        }]
    }

    /// The role query as `role_grant_views` translates it before `resolve`
    /// sees it. No `where.author`, so anyone may revoke — the row the
    /// module doc's table calls out.
    fn translated() -> Value {
        json!({ "className": ROLE, "where": { "agent": did_of(ALICE) } })
    }

    fn resolve(
        ev: &RoleGrantEvidence,
        spec: Option<&GrantedByFlow>,
        cat: &HashMap<String, SHACLFlow>,
    ) -> anyhow::Result<crate::perspectives::flow_instance::roles::RoleGrant> {
        ev.resolve(&translated(), spec, None, GrantContext::root(cat))
    }

    fn spec(flow_uri: &str, terminal_state: &str) -> GrantedByFlow {
        GrantedByFlow {
            flow: flow_uri.into(),
            terminal_state: terminal_state.into(),
        }
    }

    /// A granting run plus the gated flow that consumes it: the receipt, the
    /// catalogue holding both definitions, and the gated flow itself.
    fn one_level() -> (FlowReceipt, SHACLFlow, HashMap<String, SHACLFlow>) {
        let granting = granting_flow("Onboarding");
        let gated = gated_flow("Delivery", gate(&granting.flow_uri(), "done"));
        let cat = catalogue(vec![granting.clone(), gated.clone()]);
        let receipt = receipt_for(
            &granting,
            read_set("done", T1, Vec::new()),
            &[ROLE_INSTANCE],
            &cat,
        );
        (receipt, gated, cat)
    }

    // ---- the grant itself --------------------------------------------------

    /// **The point of the whole feature.** A completed run grants the role,
    /// and the grant is dated from the run's *quorum*, not from the
    /// assignment link sitting on the same instance.
    ///
    /// The second assertion is the one that carries it. Asserting only that a
    /// window exists would also pass on an implementation that ignored the
    /// receipt entirely and dated the grant from the assignment link — which
    /// is the pre-existing behaviour, and the thing `grantedByFlow` replaces.
    /// `ASSIGNMENT_LINK_AT` is deliberately earlier than the quorum, so the
    /// two answers are distinguishable and the wrong one is the *wider*
    /// window.
    ///
    /// Red if `resolve`'s `granted_by` branch falls through to the grant-link
    /// dating (`granted_at` becomes `ASSIGNMENT_LINK_AT`), and red if the
    /// branch does not run at all (no window, so `windows` is empty).
    #[test]
    fn a_verified_receipt_grants_the_role_and_dates_it_from_the_quorum() {
        let (receipt, gated, cat) = one_level();
        let granting_uri = receipt.flow_uri.clone();

        let ev = evidence(vec![receipt], Vec::new());
        let view = resolve(&ev[0], Some(&spec(&granting_uri, "done")), &cat)
            .expect("a granted instance resolves");

        assert_eq!(
            view.windows.len(),
            1,
            "the receipt-backed instance counts as a grant"
        );
        assert_eq!(
            view.windows[0].granted_at, T1,
            "the grant begins at the granting run's QUORUM time, not at the assignment link \
             ({ASSIGNMENT_LINK_AT}) that sits on the same instance"
        );

        // And end to end: the gated flow settles only because that grant
        // makes Alice's vote eligible.
        let gated_rs = read_set("done", T3, ev);
        assert_eq!(
            fold_read_set(&gated, &gated_rs.reverified(), GrantContext::root(&cat))
                .expect("folds")
                .state,
            "done",
            "the vote counts, so the gated edge settles"
        );
    }

    /// The same receipt, the same everything, with the grant removed: the
    /// gate must not fall back to the assignment link.
    ///
    /// This is the negative control for the test above and the reason
    /// `evidence()` always carries a genuine, signed, correctly-targeted
    /// assignment link AND a parseable instance timestamp. Both are exactly
    /// what an ordinary `didProperty` gate needs to grant — so if
    /// `grantedByFlow` ever degraded to "use the receipt if there is one",
    /// writing that one link would grant the role and the whole gate would be
    /// decorative.
    ///
    /// Red if the `granted_by` branch in `resolve` falls through to the
    /// grant-link dating instead of `continue`-ing.
    #[test]
    fn without_a_receipt_the_assignment_link_does_not_grant() {
        let (receipt, gated, cat) = one_level();
        let granting_uri = receipt.flow_uri.clone();

        let ev = evidence(Vec::new(), Vec::new());
        let view = resolve(&ev[0], Some(&spec(&granting_uri, "done")), &cat)
            .expect("an ungranted instance is not an error — it is not a member");
        assert!(
            view.windows.is_empty(),
            "no receipt, no grant — got a window at {:?}",
            view.windows.first().map(|w| &w.granted_at)
        );
        assert_eq!(
            fold_read_set(
                &gated,
                &read_set("done", T3, ev.clone()).reverified(),
                GrantContext::root(&cat)
            )
            .expect("an ungranted candidate is not a fold error")
            .state,
            "open",
            "and the gated edge does not settle"
        );

        // The control that makes the assertions above mean something: the
        // very same evidence, with the SAME role query minus `grantedByFlow`,
        // does grant — from the assignment link. So what changed the answer
        // is the gate, not some other defect in the fixture.
        let ungated = resolve(&ev[0], None, &cat).expect("resolves");
        assert_eq!(
            ungated.windows.len(),
            1,
            "an ordinary didProperty gate grants on this very evidence"
        );
        assert_eq!(ungated.windows[0].granted_at, ASSIGNMENT_LINK_AT);
    }

    /// The binding check, reached through the resolver rather than through
    /// `verify_receipt` directly — which is what proves the resolver passes
    /// `Some(instance)` rather than `None`.
    ///
    /// A receipt is a public artifact and minting is permissionless, so the
    /// attack is just: take a genuine receipt for somebody else's run and
    /// hang a `granted_by` edge off your own role instance. Nothing about the
    /// receipt is forged. Only the binding stops it.
    ///
    /// Both halves run against the **same receipt**: it grants the instance
    /// it names and not the one it does not. Without that pair the test
    /// cannot tell a working binding check from a resolver that never grants.
    ///
    /// Red if the resolver passes `None` as `arrived_from`, and red if the
    /// binding check is dropped from `verify_receipt` — in both cases the
    /// planted instance is granted.
    #[test]
    fn a_receipt_planted_on_a_node_it_does_not_name_grants_nothing() {
        let granting = granting_flow("Onboarding");
        let gated = gated_flow("Delivery", gate(&granting.flow_uri(), "done"));
        let cat = catalogue(vec![granting.clone(), gated]);
        // Minted for somebody else's node entirely.
        let receipt = receipt_for(
            &granting,
            read_set("done", T1, Vec::new()),
            &["ad4m://role/reviewer/somebody-else"],
            &cat,
        );

        let mut ev = evidence(vec![receipt.clone()], Vec::new());
        let view =
            resolve(&ev[0], Some(&spec(&granting.flow_uri(), "done")), &cat).expect("resolves");
        assert!(
            view.windows.is_empty(),
            "a receipt that speaks for another node grants nothing here, however genuine"
        );

        // The same receipt, reached from the node it does name.
        ev[0].instances[0].instance_id = "ad4m://role/reviewer/somebody-else".into();
        let view =
            resolve(&ev[0], Some(&spec(&granting.flow_uri(), "done")), &cat).expect("resolves");
        assert_eq!(
            view.windows.len(),
            1,
            "and it grants the node it DOES name — otherwise this test would pass against a \
             resolver that refuses every receipt"
        );
    }

    /// The same check from the **attacker's** side: hold the node fixed and
    /// vary what the receipt claims, because the receipt is the half an
    /// attacker controls.
    ///
    /// The test above varies `instance_id` — the defender's side — which shows
    /// the binding is read but never produces a receipt naming *nothing*. That
    /// leaves a mutant alive:
    ///
    /// ```text
    /// outputs.is_empty() || outputs.contains(node)   // "binds to nothing = speaks for anything"
    /// ```
    ///
    /// It passes every assertion in the test above. **It does not, however,
    /// grant anything** — and that is worth writing down, because the review
    /// that proposed it assumed otherwise and I only found out by running it.
    /// [`NoOutputs`](super::verify::ReceiptVerdict::NoOutputs) at step 3
    /// catches an empty binding independently, so with that mutant applied the
    /// receipt sails through step 0 and is refused three steps later. Both
    /// paths refuse the grant; the mutant is behaviour-equivalent for the
    /// *grant* question and survives a windows-only assertion.
    ///
    /// What it changes is **which finding the reader is handed**:
    /// `OutputUnbound` ("this receipt speaks for other nodes") versus
    /// `NoOutputs` ("this receipt was not made by `mint`"). Those are
    /// different claims about different parties, so the last assertion pins
    /// the verdict and not just the absence of a window — which is what
    /// actually kills the mutant. `speaks_for` is `any(|o| o == node)` and
    /// already answers `false` for an empty `outputs`; this is what keeps it
    /// that way.
    ///
    /// The empty case is hand-built because `mint` refuses an empty binding,
    /// which is the mint half of the same guard.
    ///
    /// Raised by @lal-bot-coasys reviewing this PR: the earlier pair varied
    /// the wrong operand for the threat model, which is that the artifact
    /// arrives carrying whatever its sender chose. The operand was the real
    /// gap; the exploit that motivated it was not one.
    #[test]
    fn the_receipt_must_name_the_node_it_is_reached_from() {
        let granting = granting_flow("Onboarding");
        let gated = gated_flow("Delivery", gate(&granting.flow_uri(), "done"));
        let cat = catalogue(vec![granting.clone(), gated]);
        let gate_spec = spec(&granting.flow_uri(), "done");

        let claiming = |outputs: Vec<String>| FlowReceipt {
            flow_uri: granting.flow_uri(),
            flow_dna_hash: flow_dna_hash(&granting).expect("hash"),
            terminal_state: "done".into(),
            outputs,
            read_set: read_set("done", T1, Vec::new()),
            evidence_preimage: Vec::new(),
        };

        // Every case is reached from the SAME node; only the receipt's claim
        // about what it speaks for differs.
        for (label, outputs, expected) in [
            (
                "names the node it is reached from",
                vec![ROLE_INSTANCE.to_string()],
                1,
            ),
            (
                "names a different node",
                vec!["ad4m://role/reviewer/somebody-else".to_string()],
                0,
            ),
            ("names nothing at all", Vec::new(), 0),
        ] {
            let ev = evidence(vec![claiming(outputs)], Vec::new());
            assert_eq!(
                ev[0].instances[0].instance_id, ROLE_INSTANCE,
                "{label}: precondition — the node reached from is held fixed across cases"
            );
            assert_eq!(
                resolve(&ev[0], Some(&gate_spec), &cat)
                    .expect("resolves")
                    .windows
                    .len(),
                expected,
                "{label}: a receipt grants exactly the nodes it names — and one that names \
                 nothing grants nothing, rather than everything"
            );
        }

        // And the finding itself, for the empty case, because the absence of a
        // window does not distinguish which guard produced it: with
        // `speaks_for` widened to "empty binds to anything", step 0 passes and
        // `NoOutputs` refuses at step 3 instead. Same refusal, different claim
        // — so this is the assertion that holds the binding check to its own
        // job rather than letting a later backstop cover for it.
        assert!(
            matches!(
                verify_receipt_within(
                    GrantContext::root(&cat),
                    &claiming(Vec::new()),
                    Some(ROLE_INSTANCE)
                ),
                ReceiptVerdict::OutputUnbound { .. }
            ),
            "a receipt naming nothing, reached from a node, is unbound to THAT node — the \
             binding check answers first and `NoOutputs` is not what should speak here"
        );
    }

    /// A receipt that verifies perfectly still grants nothing when it is for
    /// a different flow, or for a different terminal state of the right flow.
    ///
    /// Without the flow check, completing *any* flow grants *every*
    /// `grantedByFlow` role. Without the state check, reaching a flow's
    /// `rejected` state grants what its `approved` state was meant to — the
    /// sharper of the two, because the receipt is then for exactly the flow
    /// the gate names.
    ///
    /// Each case is paired with the gate that DOES accept the same receipt,
    /// in the same fixture, so a refusal cannot be mistaken for a receipt
    /// that simply fails to verify.
    ///
    /// **The two cases are built so that neither check can stand in for the
    /// other.** The wrong-flow receipt settles into a state spelled exactly
    /// like the one its gate names (`done`), so only the flow URI separates
    /// them; the wrong-ending receipt is for exactly the flow its gate names,
    /// so only the state does. An earlier version of this test used a
    /// wrong-flow receipt whose terminal state also differed, and deleting
    /// the `flow_uri` comparison left it green — the state check was catching
    /// both, and the test could not see it.
    ///
    /// Red if either the `flow_uri` or the `terminal_state` comparison is
    /// dropped from `granted_by_flow_at`.
    #[test]
    fn a_verified_receipt_for_another_flow_or_another_ending_grants_nothing() {
        // Two flows that both end in a state called `done`.
        let onboarding = granting_flow("Onboarding");
        let training = granting_flow("Training");
        // And one that can complete into either of two endings.
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
        let cat = catalogue(vec![onboarding.clone(), training.clone(), forked.clone()]);
        assert_ne!(
            onboarding.flow_uri(),
            training.flow_uri(),
            "precondition: two different organisms whose runs end in the same-named state"
        );

        let wrong_flow = receipt_for(
            &onboarding,
            read_set("done", T1, Vec::new()),
            &[ROLE_INSTANCE],
            &cat,
        );
        let wrong_ending = receipt_for(
            &forked,
            read_set("rejected", T1, Vec::new()),
            &[ROLE_INSTANCE],
            &cat,
        );

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
            let ev = evidence(vec![receipt], Vec::new());
            assert!(
                resolve(&ev[0], Some(&refused_by), &cat)
                    .expect("resolves")
                    .windows
                    .is_empty(),
                "{label} must not grant"
            );
            assert_eq!(
                resolve(&ev[0], Some(&accepted_by), &cat)
                    .expect("resolves")
                    .windows
                    .len(),
                1,
                "{label}: but the gate it DOES match grants on the same receipt, so the \
                 refusal above is the comparison and not a verification failure"
            );
        }
    }

    /// Two runs both granted the same instance: the membership began at the
    /// **first** of them.
    ///
    /// Preferring the earlier — the wider window — is safe here in a way it
    /// is not for grant links, and the difference is worth keeping straight.
    /// A grant link widens a window for the price of writing a link, which is
    /// why `reverified_history` drops unverified ones and suppresses the
    /// fallback with them. A receipt widens it for the price of producing a
    /// whole quorum under the reader's own rules. So the cheap widening is
    /// refused and the expensive one is simply the truth.
    ///
    /// Both orderings are asserted, because a comparison that ignored its
    /// arguments and returned the first (or the last) one it was handed would
    /// pass whichever single order the fixture happened to use.
    ///
    /// Red with `earlier_of` returning the later of the two, and red if it
    /// compares as strings while the fixture's instants disagree with string
    /// order — see #1000.
    #[test]
    fn two_granting_runs_date_the_membership_from_the_first() {
        let granting = granting_flow("Onboarding");
        let cat = catalogue(vec![granting.clone()]);
        let at = |t: &str| {
            receipt_for(
                &granting,
                read_set("done", t, Vec::new()),
                &[ROLE_INSTANCE],
                &cat,
            )
        };
        let (early, late) = (at(T1), at(T2));
        assert_ne!(
            early, late,
            "precondition: two distinct receipts, settling at different times"
        );

        for (label, receipts) in [
            ("earlier first", vec![early.clone(), late.clone()]),
            ("later first", vec![late, early]),
        ] {
            let ev = evidence(receipts, Vec::new());
            let view =
                resolve(&ev[0], Some(&spec(&granting.flow_uri(), "done")), &cat).expect("resolves");
            assert_eq!(
                view.windows.len(),
                1,
                "{label}: two receipts for one instance are one membership, not two"
            );
            assert_eq!(
                view.windows[0].granted_at, T1,
                "{label}: the membership began at the first run that granted it"
            );
        }
    }

    // ---- revocation: what the receipt does NOT freeze -----------------------

    /// The claim the module doc makes to anyone configuring a gate, made
    /// falsifiable: a receipt is permanent, a *membership* is not.
    ///
    /// The tombstone is an ordinary signed role-grant revocation, and this
    /// role query declares no `where.author`, so anyone may write one — the
    /// first row of the table in the module doc. The receipt still verifies
    /// afterwards; what changes is the window it opened.
    ///
    /// Red if `resolve`'s `granted_by` branch builds its window without
    /// calling `revocations_on` — a plausible shape, since the branch has its
    /// own dating and could easily have grown its own window construction
    /// too. Then the grant would be genuinely irrevocable and the doc would
    /// be wrong.
    #[test]
    fn a_granted_role_is_still_ended_by_a_signed_tombstone() {
        let (receipt, gated, cat) = one_level();
        let granting_uri = receipt.flow_uri.clone();
        let tombstone = signed_link(
            ROLE_INSTANCE,
            ROLE_GRANT_REVOKED_PREDICATE,
            did_of(ALICE),
            BOB,
            true,
            None,
            T2,
        );

        let ev = evidence(vec![receipt], vec![tombstone]);
        let view = resolve(&ev[0], Some(&spec(&granting_uri, "done")), &cat).expect("resolves");
        assert_eq!(
            view.windows.len(),
            1,
            "the grant is still there — a tombstone ends a membership, it does not delete one"
        );
        assert_eq!(
            view.windows[0].granted_at, T1,
            "and it still begins at the quorum"
        );
        assert_eq!(
            view.windows[0].revoked_at(),
            Some(T2),
            "the tombstone closes it from its own timestamp"
        );
        assert!(
            !view.windows[0].open_at(T3),
            "so a vote after the tombstone is not eligible"
        );
        assert!(
            view.windows[0].open_at(T1),
            "while one cast while the membership held still is — the ratchet is about the \
             RECEIPT, not about the window it opened"
        );

        // End to end, against the vote at T3 the other tests rely on.
        assert_eq!(
            fold_read_set(
                &gated,
                &read_set("done", T3, ev).reverified(),
                GrantContext::root(&cat)
            )
            .expect("folds")
            .state,
            "open",
            "the revoked member cannot settle the gated edge"
        );
    }

    // ---- the depth guard ---------------------------------------------------

    /// A chain of grant flows: `Gate1` is ungated, and each `Gate{k}` after it
    /// can only be settled by somebody the previous gate granted. Returns the
    /// catalogue, every definition, and — when it can be minted — the receipt
    /// for the top flow, which is the artifact a reader would be handed.
    ///
    /// `Err` is a real outcome here rather than a fixture failure: past the
    /// cap the top flow cannot settle, so there is no completion to mint.
    fn chain(
        levels: usize,
    ) -> (
        HashMap<String, SHACLFlow>,
        Vec<SHACLFlow>,
        anyhow::Result<FlowReceipt>,
    ) {
        let mut flows = vec![granting_flow("Gate1")];
        for k in 2..=levels {
            let previous = flows[k - 2].flow_uri();
            flows.push(gated_flow(&format!("Gate{k}"), gate(&previous, "done")));
        }
        let cat = catalogue(flows.clone());

        let mut receipt = Ok(receipt_for(
            &flows[0],
            read_set("done", T1, Vec::new()),
            &[ROLE_INSTANCE],
            &cat,
        ));
        for flow in flows.iter().skip(1) {
            let Ok(below) = receipt else { break };
            let rs = read_set("done", T3, evidence(vec![below], Vec::new()));
            receipt = FlowReceipt::mint(
                flow,
                rs,
                vec![ROLE_INSTANCE.to_string()],
                Vec::new(),
                GrantContext::root(&cat),
            );
        }
        (cat, flows, receipt)
    }

    /// The budget is a countdown that **stops**, and stopping means `None`
    /// rather than zero-forever. A counter that saturated at zero would let
    /// the recursion continue indefinitely at the bottom — the very failure
    /// the cap exists to prevent, wearing the shape of a cap.
    ///
    /// Red with `remaining: self.remaining.saturating_sub(1)` in `deeper`.
    #[test]
    fn the_budget_runs_out_rather_than_bottoming_out() {
        let cat = catalogue(vec![granting_flow("Gate1")]);
        let mut ctx = GrantContext::root(&cat);
        assert_eq!(ctx.remaining(), MAX_GRANT_DEPTH);

        for step in 1..=MAX_GRANT_DEPTH {
            ctx = ctx
                .deeper()
                .unwrap_or_else(|| panic!("edge {step} of {MAX_GRANT_DEPTH} is within budget"));
            assert_eq!(ctx.remaining(), MAX_GRANT_DEPTH - step);
        }
        assert!(
            ctx.deeper().is_none(),
            "edge {} is refused, not followed at zero forever",
            MAX_GRANT_DEPTH + 1
        );
    }

    /// **The boundary, and which way it falls.** A chain of grant flows one
    /// longer than the budget must be *refused*, never waved through.
    ///
    /// Both lengths are written in terms of `MAX_GRANT_DEPTH` rather than
    /// spelled out, so the test follows the constant instead of pinning a
    /// number next to it: holding the top of a `MAX_GRANT_DEPTH + 1` chain
    /// means following exactly `MAX_GRANT_DEPTH` edges, the last length that
    /// fits.
    ///
    /// The final block is what makes the refusal mean anything. The over-long
    /// chain is refused, and the receipt *carried inside it* — the same
    /// bytes, read with a full budget — verifies. So the depth is what
    /// changed the answer: not a broken fixture, not a chain that was never
    /// valid, not a verifier that refuses everything nested. That is the
    /// module doc's "verification is not compositional past the cap", made
    /// falsifiable.
    ///
    /// Red with `deeper()` never returning `None` — and red in the opposite,
    /// much worse direction if running out of budget were ever treated as
    /// "grant anyway", since the over-long chain would then verify.
    #[test]
    fn a_chain_one_deeper_than_the_budget_is_refused_not_allowed() {
        let (cat, flows, at_the_cap) = chain(MAX_GRANT_DEPTH + 1);
        let at_the_cap = at_the_cap.expect("a chain exactly as deep as the budget still mints");
        let verdict =
            verify_receipt_within(GrantContext::root(&cat), &at_the_cap, Some(ROLE_INSTANCE));
        assert!(
            verdict.is_verified(),
            "{MAX_GRANT_DEPTH} edges is exactly the budget, so this must still verify — got: \
             {verdict}"
        );

        // One level deeper. `mint` refuses it first: the top flow's gate
        // cannot be satisfied, the run never settles, and there is no
        // completion to claim. That is the mint half of the symmetry.
        let (cat, flows_over, over) = chain(MAX_GRANT_DEPTH + 2);
        assert_eq!(
            flows.len() + 1,
            flows_over.len(),
            "precondition: the two chains differ by exactly one grant flow"
        );
        let err = over.expect_err("a chain past the budget has nothing to mint");
        assert!(
            format!("{err:#}").contains("can still transition out"),
            "the top flow's gated edge must simply not settle, got: {err:#}"
        );

        // And the verify half, reached the only way it can be: a receipt that
        // did not come from `mint`, claiming the completion `mint` refused.
        let deepest = flows_over.last().expect("a top flow");
        let nested = chain(MAX_GRANT_DEPTH + 1)
            .2
            .expect("the chain below it mints");
        let hand_built = FlowReceipt {
            flow_uri: deepest.flow_uri(),
            flow_dna_hash: flow_dna_hash(deepest).expect("hash"),
            terminal_state: "done".into(),
            outputs: vec![ROLE_INSTANCE.to_string()],
            read_set: read_set("done", T3, evidence(vec![nested.clone()], Vec::new())),
            evidence_preimage: Vec::new(),
        };
        let verdict =
            verify_receipt_within(GrantContext::root(&cat), &hand_built, Some(ROLE_INSTANCE));
        // Deliberately NOT asserting which *kind* of denial this is.
        //
        // It currently lands in `Rejected`, because the budget running out
        // makes the gated edge fail to settle and the fold reports an
        // ordinary state mismatch. @lal-bot-coasys argues on this PR that
        // this is the wrong bucket and should be `Undecidable`: the same
        // bytes verify for a reader handed the sub-receipt directly, so
        // "I could not reach that far from where I stand" is a finding about
        // the reader, not about the material — the very distinction #1075
        // put in the type two commits ago.
        //
        // I think that argument is right, and fixing it means threading
        // budget exhaustion out of the fold into a verdict of its own rather
        // than renaming a bucket, which is more than this PR should carry.
        // So this test pins the property that must never regress — the
        // over-cap chain does NOT verify, fail-closed — and leaves the
        // bucketing to the follow-up, rather than pinning today's answer as
        // the contract and making the test the reason it can't change.
        assert_ne!(
            verdict.outcome(),
            VerdictKind::Verified,
            "running out of budget must DENY the grant, never allow it — got: {verdict}"
        );

        let inner = verify_receipt_within(GrantContext::root(&cat), &nested, Some(ROLE_INSTANCE));
        assert!(
            inner.is_verified(),
            "the nested chain is sound on its own — so the refusal above is the budget, not a \
             fixture that never verified — got: {inner}"
        );
    }

    // ---- the inversion that must never be configurable ---------------------

    /// `grantedByFlow` with a `count` satisfied by zero instances inverts the
    /// gate: "eligible while no verifiable receipt exists". Every reason a
    /// receipt might fail — an un-synced definition, a broken signature, a
    /// chain past the depth cap — would then become a reason to GRANT, which
    /// is fail-open dressed as a cardinality constraint.
    ///
    /// The second half is what makes this a real finding rather than a
    /// stylistic refusal: with the check removed, `{max: 0}` really does make
    /// an ungranted candidate eligible. `cardinality_satisfied(Some({max:0}),
    /// 0)` is `true`, so this is the behaviour being refused, not a
    /// hypothetical one.
    ///
    /// Red if the `cardinality_satisfied(count, 0)` guard is dropped from
    /// `resolve`.
    #[test]
    fn granted_by_flow_with_a_count_satisfied_by_zero_is_refused() {
        let (receipt, _gated, cat) = one_level();
        let granting_uri = receipt.flow_uri.clone();
        let ev = evidence(Vec::new(), Vec::new());
        let zero_ok = ModelQueryCount {
            min: None,
            max: Some(0),
        };

        let err = ev[0]
            .resolve(
                &translated(),
                Some(&spec(&granting_uri, "done")),
                Some(&zero_ok),
                GrantContext::root(&cat),
            )
            .expect_err("a gate that grants when verification FAILS is not a gate");
        assert!(
            format!("{err:#}").contains("satisfied by zero instances"),
            "the refusal must name the inversion, got: {err:#}"
        );

        // The behaviour being refused, demonstrated: with no windows at all,
        // `{max: 0}` reports the candidate as eligible.
        let ungranted =
            resolve(&ev[0], Some(&spec(&granting_uri, "done")), &cat).expect("resolves");
        assert!(ungranted.windows.is_empty());
        assert!(
            ungranted.eligible_at(T3, Some(&zero_ok)),
            "this is why the combination is refused rather than merely discouraged"
        );
    }
}

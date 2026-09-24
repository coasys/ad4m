//! `grantedByFlow`: a role whose membership is granted by another flow
//! completing.
//!
//! An ordinary `fromRole` gate asks the graph a question — *is there a
//! `Reviewer` instance whose `agent` is this DID?* — and dates the answer from
//! the assignment link, a timestamp its own author stamped on it
//! ([`roles`](super::roles) § *Accepted caveat*). `grantedByFlow` adds a
//! second condition to the same question and replaces the dating:
//!
//! > the matched instance must be a **valid output of a completed run of flow
//! > F** in the named terminal state, and the grant begins at the moment that
//! > run reached quorum.
//!
//! # A thin layer on `produced`
//!
//! "Valid output of flow F" is [`produced`](super::produced)'s question, and
//! this module does not answer it a second time. It asks
//! [`first_produced_at`](super::produced::first_produced_at) and uses the
//! answer as `granted_at`:
//!
//! ```text
//!   F --ad4m://flow/flow_receipt--> receipt      (F's index, anyone writes)
//!            │ load_flow_receipts: scoped to F, refuses over budget
//!            ▼
//!   receipts naming (role class, instance id)    (discovery narrowing only)
//!            │ carried in the read-set, per instance
//!            ▼
//!   produced::first_produced_at(ctx.deeper(), (class, id), F, state)
//!            │ verify_receipt_within per receipt; flow, state, (class, id)
//!            ▼
//!   granted_at = earliest settled_at             (None = not a member)
//! ```
//!
//! Everything a receipt must clear is `produced`'s: it verifies under the
//! reader's own catalogue (signatures, DNA hash, re-fold, outputs hashing to
//! the commitment the final edge's quorum signed), it was minted for F, the
//! reader's own fold settled it into the named state, and it names the
//! instance as **`(class, id)`**. The class is the role query's `className`
//! read from the reader's own flow definition. It is never
//! [`RoleGrantEvidence::role_class`](super::roles::RoleGrantEvidence), which
//! is carried and so is the minter's word. A node committed as a `Task` is
//! not a produced `Reviewer`, even with the same id.
//!
//! `settled_at` is the moment the n-th distinct eligible voter signed the
//! final edge. No participant picks it, and back-dating it means producing a
//! different quorum. That is what [`roles`](super::roles)' module doc means
//! when it says roles granted as flow outputs "carry a quorum-fixed time no
//! single party can back-date".
//!
//! ## What this module keeps, and why `produced` does not cover it
//!
//! - **The gate semantics**: no fallback to the assignment link, the
//!   `count`-satisfied-by-zero refusal, and revocation by tombstone. These
//!   are about roles, not outputs ([`roles`](super::roles)).
//! - **The depth budget** ([`GrantContext`], [`MAX_GRANT_DEPTH`]). A receipt
//!   for a gated flow carries the receipts that made its voters eligible, so
//!   verification recurses. `produced` has no recursion of its own; it takes
//!   the budget as a parameter from here.
//!
//! ## What was deleted, and what replaced it
//!
//! The first version of this PR had its own receipt path: it read the
//! role instance's `ad4m://flow/granted_by` edges, sorted them by URI,
//! **truncated at 8**, and verified with a binding check (`arrived_from`)
//! of its own. The truncation was the flood hole Lal found in #1127's first
//! loader. Eight junk edges that sort low hide the real receipt, and the
//! gate then answers a confident "not a member". All of it is gone.
//! Discovery is F's index, read by
//! [`load_flow_receipts`](super::produced::load_flow_receipts). The binding
//! is `produced`'s `(class, id)` match.
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
//! # The gate is existential, but the fold is not monotone
//!
//! An instance is granted iff **some** carried receipt verifies for it under
//! the named flow and terminal state. So a receipt that fails to verify can
//! only remove a possible witness, and membership can only narrow.
//!
//! That does not make "not a member" a safe answer to give when the gate
//! *could not look*. The fold's outcome is not monotone in membership:
//! [`Contention`](super::fold) fires only while two edges out of a state are
//! both quorate, so taking one voter away can de-quorate one edge and let the
//! other one fire. `role_grant_views` refuses the whole fold for that reason
//! (see [`fold_read_set`](super::fold_read_set)), and the depth cap follows the
//! same rule below.
//!
//! # Running out of depth is undecidable
//!
//! At the cap, a carried receipt for F that would still need verifying is a
//! [`GrantDepthExceeded`] error, and the fold that asked aborts. It used to
//! be `None`, the same value as "not a member". That was quiet (Lal's note on
//! #1076), and it was also fail-open:
//! `at_the_cap_a_voter_dropped_for_depth_cannot_break_a_tie` builds a receipt
//! that is `Contested` for a reader with budget left and, under the old
//! answer, `Verified` for a reader at the cap. Nesting made it verify.
//!
//! **How far the error travels.** A fold only runs at the cap inside another
//! receipt's verification. The reader's own fold starts from
//! [`GrantContext::root`], and the only way to spend budget is
//! [`GrantContext::deeper`] here, on the way into verifying a receipt. And
//! [`verify_receipt_within`](super::verify::verify_receipt_within) turns a
//! fold that aborts into an `Unfoldable` verdict for *that receipt*. So the
//! error refuses the receipt one level up, and never aborts the fold the reader
//! asked about. That matters, because aborting the reader's own fold would
//! give anybody a denial of service: F's index is unsigned and anyone may
//! write it (see [`verify`](super::verify) § *The binding is the consumer's
//! check*), so a stranger could plant a deep chain there and stop an honest
//! flow from deriving a state at all. As it is, the planted receipt is
//! refused. That costs the attacker nothing and gets them nothing.
//! `a_chain_one_deeper_than_the_budget_is_refused_not_allowed` pins both
//! halves.
//!
//! The error is also logged at `warn` where it is raised, naming the instance
//! and the flow, because the verdict that carries it up is only logged at
//! `debug`.
//!
//! A receipt flood (above) is a different kind of error. It is a bound on
//! how much of F's index a reader will look at, not a verdict on any one
//! receipt, and it does reach the reader's own fold: refusing to look is not
//! the same as looking and finding nothing.
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
//! Three answers are errors instead, because each is "I could not decide",
//! not "no": a granting flow whose index is over budget (above), a receipt the
//! depth budget cannot reach (above), and a granting flow this replica holds
//! no definition for, which is the rule
//! [`flow_valid_outputs`](super::produced::flow_valid_outputs) applies to an
//! unknown flow.
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
//! [`MAX_GRANT_DEPTH`] counts **levels of receipt nesting below material the
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
//! surfaces as a [`Rejected`](super::verify::VerdictKind::Rejected) verdict:
//! the receipt whose fold hit the cap is `Unfoldable`, and its reason names
//! the [`GrantDepthExceeded`]. Further up, every level just sees a grant that
//! did not happen. It arguably belongs in
//! [`Undecidable`](super::verify::VerdictKind::Undecidable) instead: the same
//! bytes verify for a reader handed the sub-receipt directly, so "I could not
//! reach that far from where I stand" is a finding about the reader, not about
//! the material — and `Undecidable` is where this module's own three-kind
//! doctrine puts those. The two buckets differ in what a payout system may
//! conclude: `Rejected` is evidence against the receipt, `Undecidable` is not.
//! Nothing is unsafe either way — both refuse the grant — so this is a
//! taxonomy question, not a hole, and it is left open deliberately rather than
//! settled in passing. Raised by @lal-bot-coasys reviewing the `grantedByFlow`
//! PR and tracked as #1077. (#1077's other half, about `OutputUnbound`, is
//! moot after the rebuild on `produced`: that verdict no longer exists.)
//! `a_chain_one_deeper_than_the_budget_is_refused_not_allowed` therefore pins
//! the refusal and not the bucket.
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

use super::atom::OutputRef;
use super::produced::first_produced_at;
#[cfg(test)]
use super::receipt::flow_dna_hash;
use super::receipt::FlowReceipt;
use crate::perspectives::shacl_parser::{GrantedByFlow, SHACLFlow};
use std::collections::HashMap;

/// How many levels of nested receipt a reader will verify below material it
/// already holds. See the module header, § *Why there is a depth
/// cap* — this bounds a tree, not a byte count, and it is a crate constant
/// every replica shares so that mint and verify reach the same answer.
///
/// **The value `4` is a judgement call, and nothing here measured it.** The
/// 2^N argument in the module header establishes that the tree must be
/// bounded; it does not select a bound, and no fold-cost benchmark was run to
/// pick this one. What it rests on is that no flow composition anybody has
/// asked for nests grants more than a level or two, so `4` leaves room
/// without being a number a reader has to trust. Stated plainly because the
/// paragraph above it argues well enough for *a* cap that it could be
/// mistaken for an argument for *this* cap.
///
/// What would change it is evidence, and of two different kinds: a real
/// composition that legitimately needs more depth, or a measured fold cost
/// showing `4` is already too expensive. Note that the first would not
/// justify raising the constant — per the module header, verification is not
/// compositional past the cap, so the fix for legitimate depth is memoisation
/// by receipt URI. Raising the number only moves where the same cliff sits.
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

    /// The context for verifying one more level of nested receipt, or `None`
    /// at the cap.
    ///
    /// `None` is a refusal, never a pass: the only caller
    /// ([`granted_by_flow_at`]) turns it into a [`GrantDepthExceeded`] error.
    /// Returning an `Option` rather than a saturating counter is deliberate —
    /// a counter that stopped decrementing would let the recursion continue
    /// forever at zero, which is the failure this exists to prevent.
    pub fn deeper(&self) -> Option<GrantContext<'a>> {
        self.remaining.checked_sub(1).map(|remaining| GrantContext {
            catalogue: self.catalogue,
            remaining,
        })
    }

    /// Nesting levels still verifiable. Diagnostics only.
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

/// A `grantedByFlow` gate was reached with no depth budget left while a
/// carried receipt for its flow was still waiting to be verified.
///
/// This is "I could not decide", not "not a member", which is why it is an
/// error and not a `None`. The two answers must stay apart for the same reason
/// that [`ReceiptBudgetExceeded`](super::produced::ReceiptBudgetExceeded) and
/// an unknown granting flow are errors: at the call site a silent "no" and a
/// checked "no" look the same. See the module header, § *Running out of depth
/// is undecidable*, for how far the error travels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GrantDepthExceeded {
    /// The role instance whose grant could not be decided.
    pub output: OutputRef,
    /// The granting flow whose receipt would have had to be verified.
    pub flow: String,
}

impl std::fmt::Display for GrantDepthExceeded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "grantedByFlow: cannot decide whether `{}` `{}` was granted by flow `{}`: its \
             receipt would be verified more than {MAX_GRANT_DEPTH} nested grant levels below \
             the material being read, past the grant depth budget; refusing to decide rather \
             than answering \"not a member\"",
            self.output.class_name, self.output.id, self.flow
        )
    }
}

impl std::error::Error for GrantDepthExceeded {}

/// When flow `spec` granted the role instance `output`, if a carried receipt
/// says so.
///
/// `Ok(Some(settled_at))` is the quorum-fixed moment the granting run
/// completed — what [`RoleGrantWindow::granted_at`](super::roles::RoleGrantWindow)
/// becomes for this instance. `Ok(None)` means no carried receipt granted it,
/// which is an ordinary "not a member" and not an error (module header).
///
/// The whole check is [`first_produced_at`]: the receipt verifies, is for
/// `spec.flow`, settled into `spec.terminal_state`, and names `output` as
/// `(class, id)`. Earliest wins when several receipts qualify. The only
/// thing added here is the budget: every receipt this gate verifies is one
/// level deeper than the material that carried it.
///
/// At the cap, a carried receipt for `spec.flow` that would need verifying is
/// a [`GrantDepthExceeded`] error, never `Ok(None)`. With no such receipt,
/// nothing was left unchecked, so `Ok(None)` is still a checked "no". Receipts
/// for other flows do not count: [`first_produced_at`] skips them before it
/// verifies anything.
pub fn granted_by_flow_at(
    ctx: GrantContext<'_>,
    output: &OutputRef,
    spec: &GrantedByFlow,
    receipts: &[FlowReceipt],
) -> Result<Option<String>, GrantDepthExceeded> {
    let Some(deeper) = ctx.deeper() else {
        if !receipts.iter().any(|r| r.flow_uri == spec.flow) {
            return Ok(None);
        }
        let err = GrantDepthExceeded {
            output: output.clone(),
            flow: spec.flow.clone(),
        };
        // `warn`, not `debug`: a legitimately deep ontology reaching this
        // would otherwise be "why is nobody in this role" with nothing to pull
        // on (Lal, #1076).
        log::warn!("{err}");
        return Err(err);
    };
    Ok(first_produced_at(
        deeper,
        output,
        &spec.flow,
        Some(&spec.terminal_state),
        receipts,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_evaluator::{EvidenceItem, RequiresQueryable};
    use crate::perspectives::flow_instance::atom::fixtures::{
        did_of, literal, signed_link, INSTANCE, T1, T2, T3,
    };
    use crate::perspectives::flow_instance::atom::{
        outputs_hash, proposal_uri, EVIDENCE_HASHES_PREDICATE, FLOW_INSTANCE_PREDICATE,
        FROM_STATE_PREDICATE, OUTPUTS_HASH_PREDICATE, OUTPUT_PREDICATE, PROPOSAL_NONCE_PREDICATE,
        PROPOSER_PREDICATE, ROLE_GRANT_REVOKED_PREDICATE, TO_STATE_PREDICATE,
    };
    use crate::perspectives::flow_instance::fold_read_set;
    use crate::perspectives::flow_instance::produced::{produced_by_flow, ReceiptBudgetExceeded};
    use crate::perspectives::flow_instance::roles::{
        resolve_role_grants, RoleGrant, RoleGrantEvidence, RoleGrantWindow, RoleInstanceHistory,
    };
    use crate::perspectives::flow_instance::verify::{
        verify_receipt_within, ReceiptVerdict, VerdictKind,
    };
    use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};
    use crate::perspectives::shacl_parser::{ModelQuery, ModelQueryCount};
    use crate::types::LinkExpression;
    use async_trait::async_trait;
    use serde_json::{json, Value};
    use std::sync::Mutex;

    const BASE: &str = "ad4m://task/t1";
    /// The node that is both the granting run's output and the role instance
    /// the gate matches. Using one URI for both is what a grant flow *is*:
    /// the thing the run produced is the membership.
    const ROLE_INSTANCE: &str = "ad4m://role/reviewer/r0";
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
    /// carrying `outputs`, with no role evidence.
    fn receipt_for(
        flow: &SHACLFlow,
        to: &str,
        at: &str,
        outputs: &[EvidenceItem],
        cat: &HashMap<String, SHACLFlow>,
    ) -> FlowReceipt {
        FlowReceipt::mint(
            flow,
            read_set(to, at, outputs, Vec::new()),
            outputs.to_vec(),
            Vec::new(),
            GrantContext::root(cat),
        )
        .expect("the fixture read-set mints")
    }

    /// One matched role instance `id` for Alice, with an assignment link and
    /// an instance timestamp that a `grantedByFlow` gate must ignore, and
    /// whatever receipts were collected for it.
    fn instance(
        id: &str,
        receipts: Vec<FlowReceipt>,
        revocations: Vec<LinkExpression>,
    ) -> RoleInstanceHistory {
        RoleInstanceHistory {
            instance_id: id.into(),
            grant_links: vec![signed_link(
                id,
                "agent",
                did_of(ALICE),
                "admin",
                true,
                None,
                ASSIGNMENT_LINK_AT,
            )
            .into()],
            revocation_links: revocations,
            asserted_instance_timestamp: Some(ASSIGNMENT_LINK_AT.into()),
            granting_receipts: receipts,
        }
    }

    /// The evidence the gated flow's read-set carries for Alice: one matched
    /// role instance, `ROLE_INSTANCE`.
    fn evidence(
        receipts: Vec<FlowReceipt>,
        revocations: Vec<LinkExpression>,
    ) -> Vec<RoleGrantEvidence> {
        vec![RoleGrantEvidence {
            to_state: "done".into(),
            role_class: ROLE.into(),
            did: did_of(ALICE).into(),
            instances: vec![instance(ROLE_INSTANCE, receipts, revocations)],
        }]
    }

    /// The role query as `role_grant_views` translates it before `resolve`
    /// sees it. No `where.author`, so anyone may revoke — the row the
    /// module doc's table calls out.
    fn translated() -> Value {
        json!({ "className": ROLE, "where": { "agent": did_of(ALICE) } })
    }

    /// The `Reviewer` role query, gated on `spec` when given. `None` is the
    /// same query as an ordinary `didProperty` role.
    fn role(spec: Option<&GrantedByFlow>) -> ModelQuery {
        let mut query = json!({ "className": ROLE, "didProperty": "agent" });
        if let Some(spec) = spec {
            query["grantedByFlow"] = serde_json::to_value(spec).expect("spec serialises");
        }
        serde_json::from_value(query).expect("role query parses")
    }

    fn resolve(
        ev: &RoleGrantEvidence,
        spec: Option<&GrantedByFlow>,
        cat: &HashMap<String, SHACLFlow>,
    ) -> anyhow::Result<RoleGrant> {
        ev.resolve(&translated(), &role(spec), GrantContext::root(cat))
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
        let receipt = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)], &cat);
        (receipt, gated, cat)
    }

    /// The gated flow's own run: Alice votes `open → done` at T3, carrying
    /// `role_grants`.
    fn gated_run(role_grants: Vec<RoleGrantEvidence>) -> ReadSet {
        read_set("done", T3, &[item(TASK_CLASS, BASE)], role_grants)
    }

    /// A second role instance for the same DID. One agent, two matched
    /// instances, is the shape every other fixture here is missing: with a
    /// single instance the loop in `resolve` has nothing to bind *per*
    /// instance, so "each instance gets the receipt that names it" and "every
    /// instance gets whichever receipt resolved first" are the same answer.
    const ROLE_INSTANCE_2: &str = "ad4m://role/reviewer/r1";

    /// Two matched instances for Alice, each carrying exactly the receipts
    /// given for it.
    fn evidence_two(first: Vec<FlowReceipt>, second: Vec<FlowReceipt>) -> Vec<RoleGrantEvidence> {
        vec![RoleGrantEvidence {
            to_state: "done".into(),
            role_class: ROLE.into(),
            did: did_of(ALICE).into(),
            instances: vec![
                instance(ROLE_INSTANCE, first, Vec::new()),
                instance(ROLE_INSTANCE_2, second, Vec::new()),
            ],
        }]
    }

    /// The window for one instance, looked up by `instance_id` rather than by
    /// position: `resolve` happens to push in instance order today, and a
    /// test that silently depended on that would start asserting about the
    /// wrong instance the day it stopped.
    fn window_for<'w>(view: &'w RoleGrant, instance_id: &str) -> Option<&'w RoleGrantWindow> {
        view.windows.iter().find(|w| w.instance_id == instance_id)
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
        assert_eq!(
            fold_read_set(
                &gated,
                &gated_run(ev).reverified(),
                GrantContext::root(&cat)
            )
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
    /// `instance()` always carries a genuine, signed, correctly-targeted
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
                &gated_run(ev.clone()).reverified(),
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

    // ---- the binding: (class, id), through `produced` ----------------------

    /// **The gate is `produced`'s check and nothing else.** Hold the node
    /// fixed and vary what the receipt names, because the receipt is the half
    /// an attacker controls. For every case the gate's answer must equal
    /// [`produced_by_flow`]'s answer for `(Reviewer, ROLE_INSTANCE)` over the
    /// same receipts: one definition of "valid output of F", used by the
    /// app-facing surfaces and by roles alike.
    ///
    /// `outputs` is plural by design — one run granting several roles at once
    /// is the ordinary case — so arity ≥ 2 is covered in both directions:
    ///
    /// ```text
    /// outputs.contains(node) || outputs.len() >= 2   // two strangers grant anybody
    /// outputs.first() == Some(node)                  // named second, silently dropped
    /// ```
    ///
    /// `names the node among others` names it **second**, which is what
    /// separates `contains` from `first()`. (Lal's arity finding on the first
    /// version of this PR; the cases moved over unchanged.)
    ///
    /// Red if the gate binds by anything looser than the verified
    /// `(class, id)` — by `receipt.outputs` before verification, by id
    /// alone, or not at all.
    #[test]
    fn a_receipt_grants_exactly_the_nodes_it_names() {
        let granting = granting_flow("Onboarding");
        let cat = catalogue(vec![granting.clone()]);
        let gate_spec = spec(&granting.flow_uri(), "done");
        let this = OutputRef {
            class_name: ROLE.into(),
            id: ROLE_INSTANCE.into(),
        };

        // A receipt naming nothing is not something `mint` produces, so it is
        // built by hand: what a stranger could write to F's index.
        let names_nothing = FlowReceipt {
            flow_uri: granting.flow_uri(),
            flow_dna_hash: flow_dna_hash(&granting).expect("hash"),
            terminal_state: "done".into(),
            outputs: Vec::new(),
            read_set: read_set("done", T1, &[], Vec::new()),
            evidence_preimage: Vec::new(),
        };
        let minted = |ids: &[&str]| {
            let outputs: Vec<EvidenceItem> = ids.iter().map(|id| role_item(id)).collect();
            receipt_for(&granting, "done", T1, &outputs, &cat)
        };

        for (label, receipt, expected) in [
            ("names the node", minted(&[ROLE_INSTANCE]), 1),
            ("names a different node", minted(&[SOMEBODY_ELSE]), 0),
            ("names nothing at all", names_nothing, 0),
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
            let ev = evidence(receipts.clone(), Vec::new());
            let windows = resolve(&ev[0], Some(&gate_spec), &cat)
                .expect("resolves")
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
    /// else (#1108's `speaks_for` rule, which `produced` applies).
    ///
    /// And the class is the one in the **reader's** role query, never the
    /// carried `role_class`: rewriting the evidence to say `Task` does not
    /// make the `Task` receipt count, because a minter who could name the
    /// class would be naming the rule its receipt is judged by.
    ///
    /// Red if `resolve` builds the `OutputRef` from `self.role_class`, or if
    /// the binding compares ids only.
    #[test]
    fn the_same_id_committed_as_another_class_does_not_grant() {
        let granting = granting_flow("Onboarding");
        let cat = catalogue(vec![granting.clone()]);
        let gate_spec = spec(&granting.flow_uri(), "done");

        let as_task = receipt_for(
            &granting,
            "done",
            T1,
            &[item(TASK_CLASS, ROLE_INSTANCE)],
            &cat,
        );
        let ev = evidence(vec![as_task.clone()], Vec::new());
        assert!(
            resolve(&ev[0], Some(&gate_spec), &cat)
                .expect("resolves")
                .windows
                .is_empty(),
            "the node was committed as a Task; a Reviewer gate must not count it"
        );

        let mut relabelled = ev.clone();
        relabelled[0].role_class = TASK_CLASS.into();
        assert!(
            resolve(&relabelled[0], Some(&gate_spec), &cat)
                .expect("resolves")
                .windows
                .is_empty(),
            "the carried role_class is the minter's word and must not choose the class"
        );

        // The control: the same node, committed as a Reviewer, grants.
        let as_reviewer = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)], &cat);
        let ev = evidence(vec![as_reviewer], Vec::new());
        assert_eq!(
            resolve(&ev[0], Some(&gate_spec), &cat)
                .expect("resolves")
                .windows
                .len(),
            1,
            "so the refusals above are the class, not a receipt that never verified"
        );
    }

    /// **A forged receipt grants nothing.** The #1104 re-mint: take a
    /// genuine, fully signed run that committed to somebody else's node, and
    /// rewrite the carried `outputs` to name this one — right class, right
    /// id. Every signature in it is real. Only the outputs commitment the
    /// final edge's quorum signed stops it.
    ///
    /// The verdict is pinned, not just the absent window, so it is the
    /// commitment check answering and not some earlier failure in the
    /// fixture.
    #[test]
    fn a_forged_re_mint_naming_the_instance_grants_nothing() {
        let granting = granting_flow("Onboarding");
        let cat = catalogue(vec![granting.clone()]);
        let gate_spec = spec(&granting.flow_uri(), "done");

        let genuine = receipt_for(&granting, "done", T1, &[role_item(SOMEBODY_ELSE)], &cat);
        let mut forged = genuine.clone();
        forged.outputs = vec![role_item(ROLE_INSTANCE)];

        assert!(
            matches!(
                verify_receipt_within(GrantContext::root(&cat), &forged),
                ReceiptVerdict::OutputsNotCommitted { .. }
            ),
            "precondition: the forgery is refused by the outputs commitment"
        );
        let ev = evidence(vec![forged], Vec::new());
        assert!(
            resolve(&ev[0], Some(&gate_spec), &cat)
                .expect("resolves")
                .windows
                .is_empty(),
            "a re-mint naming this instance grants nothing"
        );

        // Control: an honest receipt naming the instance, same flow, grants.
        let honest = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)], &cat);
        let ev = evidence(vec![honest], Vec::new());
        assert_eq!(
            resolve(&ev[0], Some(&gate_spec), &cat)
                .expect("resolves")
                .windows
                .len(),
            1
        );
    }

    /// The binding is **per instance**, not per candidate: two matched
    /// instances of the same role for the same agent, each carrying a receipt
    /// that names only the *other* one. Neither is granted.
    ///
    /// The regression this guards reads as an **optimisation**: receipt
    /// verification is the expensive part of `resolve`, so hoisting
    /// `granted_by_flow_at` out of the per-instance loop and reusing one
    /// answer looks like free work saved. It keeps every single-instance
    /// fixture green and lets a receipt naming instance A grant instance B.
    ///
    /// The positive control asserts `granted_at` **per instance**, and the
    /// two runs settle at different times, because a narrower hoist
    /// (resolve for `instances[0]` only, reuse for all) survives the crossed
    /// half and is only caught by handing `r1` its sibling's `T1`. A window
    /// count cannot see that. (@marvin-bot-coasys's finding on the first
    /// version of this PR; both hoists were run then and turned this red.)
    #[test]
    fn a_receipt_does_not_grant_a_sibling_instance_of_the_same_role() {
        let granting = granting_flow("Onboarding");
        let cat = catalogue(vec![granting.clone()]);
        let gate_spec = spec(&granting.flow_uri(), "done");

        let names_second = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE_2)], &cat);
        let names_first = receipt_for(&granting, "done", T2, &[role_item(ROLE_INSTANCE)], &cat);

        let crossed = evidence_two(vec![names_second.clone()], vec![names_first.clone()]);
        let view = resolve(&crossed[0], Some(&gate_spec), &cat)
            .expect("a candidate no receipt grants is not an error — it is not a member");
        assert!(
            view.windows.is_empty(),
            "each instance carries a receipt for the OTHER one, so neither is granted; got \
             windows {:?}",
            view.windows
                .iter()
                .map(|w| (&w.instance_id, &w.granted_at))
                .collect::<Vec<_>>()
        );

        let uncrossed = evidence_two(
            vec![receipt_for(
                &granting,
                "done",
                T1,
                &[role_item(ROLE_INSTANCE)],
                &cat,
            )],
            vec![receipt_for(
                &granting,
                "done",
                T2,
                &[role_item(ROLE_INSTANCE_2)],
                &cat,
            )],
        );
        let view = resolve(&uncrossed[0], Some(&gate_spec), &cat).expect("resolves");
        assert_eq!(
            view.windows.len(),
            2,
            "both instances carry a receipt naming themselves, so both are granted"
        );
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
    /// a different flow, or for a different terminal state of the right flow.
    ///
    /// **The two cases are built so that neither check can stand in for the
    /// other.** The wrong-flow receipt settles into a state spelled exactly
    /// like the one its gate names (`done`), so only the flow URI separates
    /// them; the wrong-ending receipt is for exactly the flow its gate names,
    /// so only the state does. Each case is paired with the gate that DOES
    /// accept the same receipt, in the same fixture.
    ///
    /// Red if `produced`'s flow pre-check or its state filter is dropped.
    #[test]
    fn a_verified_receipt_for_another_flow_or_another_ending_grants_nothing() {
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
        let cat = catalogue(vec![onboarding.clone(), training.clone(), forked.clone()]);
        assert_ne!(
            onboarding.flow_uri(),
            training.flow_uri(),
            "precondition: two different organisms whose runs end in the same-named state"
        );

        let wrong_flow = receipt_for(&onboarding, "done", T1, &[role_item(ROLE_INSTANCE)], &cat);
        let wrong_ending = receipt_for(&forked, "rejected", T1, &[role_item(ROLE_INSTANCE)], &cat);

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
    /// is not for grant links. A grant link widens a window for the price of
    /// writing a link; a receipt widens it for the price of producing a whole
    /// quorum under the reader's own rules.
    ///
    /// Both orderings are asserted, because a comparison that ignored its
    /// arguments and returned the first (or the last) one it was handed would
    /// pass whichever single order the fixture happened to use.
    ///
    /// Red with `produced::earlier_of` returning the later of the two.
    #[test]
    fn two_granting_runs_date_the_membership_from_the_first() {
        let granting = granting_flow("Onboarding");
        let cat = catalogue(vec![granting.clone()]);
        let at = |t: &str| receipt_for(&granting, "done", t, &[role_item(ROLE_INSTANCE)], &cat);
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
    /// Red if `resolve`'s `granted_by` branch builds its window without
    /// calling `revocations_on`. Then the grant would be genuinely
    /// irrevocable and the doc would be wrong.
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

        let ev = evidence(vec![receipt], vec![tombstone.into()]);
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

        assert_eq!(
            fold_read_set(
                &gated,
                &gated_run(ev).reverified(),
                GrantContext::root(&cat)
            )
            .expect("folds")
            .state,
            "open",
            "the revoked member cannot settle the gated edge"
        );
    }

    // ---- discovery and the receipt budget, through the I/O half -----------

    /// A store that answers the role query with `ROLE_INSTANCE` for the
    /// member DIDs, and hands back `receipts` as the granting flow's index.
    /// Records which flows' receipts were asked for.
    struct GateStore {
        members: Vec<String>,
        receipts: Result<Vec<FlowReceipt>, ReceiptBudgetExceeded>,
        asked: Mutex<Vec<String>>,
    }

    #[async_trait]
    impl RequiresQueryable for GateStore {
        async fn model_query(&self, _class: &str, query_json: &str) -> anyhow::Result<String> {
            let instances: Vec<Value> = if self.members.iter().any(|d| query_json.contains(d)) {
                vec![json!({ "id": ROLE_INSTANCE, "timestamp": ASSIGNMENT_LINK_AT })]
            } else {
                Vec::new()
            };
            Ok(json!({ "totalCount": instances.len(), "instances": instances }).to_string())
        }

        async fn flow_receipts(&self, flow_uri: &str) -> anyhow::Result<Vec<FlowReceipt>> {
            self.asked.lock().unwrap().push(flow_uri.to_string());
            self.receipts.clone().map_err(anyhow::Error::from)
        }
    }

    fn store(receipts: Result<Vec<FlowReceipt>, ReceiptBudgetExceeded>) -> GateStore {
        GateStore {
            members: vec![did_of(ALICE).to_string()],
            receipts,
            asked: Mutex::new(Vec::new()),
        }
    }

    /// **Holder in, non-holder out, through the whole path**: the I/O half
    /// reads the granting flow's receipts once, carries each instance only
    /// the receipts that claim it, and the pure half grants from them.
    ///
    /// Red if the loader is asked for any flow but the gate's (a gate reading
    /// the gated flow's own receipts would grant on the wrong runs), if the
    /// narrowing carries receipts for other instances (the read-set would
    /// carry the whole index, and a crossed binding could then only be caught
    /// by `produced`), or if the holder is not granted.
    #[tokio::test]
    async fn the_gate_reads_the_granting_flows_receipts_and_grants_only_the_holder() {
        let granting = granting_flow("Onboarding");
        let gated = gated_flow("Delivery", gate(&granting.flow_uri(), "done"));
        let cat = catalogue(vec![granting.clone(), gated.clone()]);
        let mine = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)], &cat);
        let theirs = receipt_for(&granting, "done", T2, &[role_item(SOMEBODY_ELSE)], &cat);
        let db = store(Ok(vec![theirs, mine.clone()]));

        let gate_role = role(Some(&spec(&granting.flow_uri(), "done")));
        let record = gated_run(Vec::new()).as_record(&gated);
        let candidates = [did_of(ALICE).to_string(), did_of(BOB).to_string()];
        let evidence = resolve_role_grants(&db, "done", &gate_role, &record, &candidates)
            .await
            .expect("the index is within budget");

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
            alice.instances[0].granting_receipts,
            vec![mine],
            "only the receipt that claims this instance travels with it"
        );

        let translate = |did: &str| json!({ "className": ROLE, "where": { "agent": did } });
        let alice_grant = alice
            .resolve(
                &translate(did_of(ALICE)),
                &gate_role,
                GrantContext::root(&cat),
            )
            .expect("resolves");
        assert_eq!(
            alice_grant
                .windows
                .iter()
                .map(|w| w.granted_at.as_str())
                .collect::<Vec<_>>(),
            vec![T1],
            "the holder is granted from her run's quorum"
        );

        let bob = evidence
            .iter()
            .find(|e| e.did == did_of(BOB))
            .expect("evidence for Bob");
        let bob_grant = bob
            .resolve(
                &translate(did_of(BOB)),
                &gate_role,
                GrantContext::root(&cat),
            )
            .expect("resolves");
        assert!(
            bob_grant.windows.is_empty(),
            "Bob holds no role instance, so no receipt can grant him anything"
        );

        assert_eq!(
            fold_read_set(
                &gated,
                &gated_run(evidence).reverified(),
                GrantContext::root(&cat)
            )
            .expect("folds")
            .state,
            "done",
            "and the evidence the I/O half collected settles the gated edge"
        );
    }

    /// **A receipt flood is an error, not a silent deny.** Lal's review of
    /// #1127, applied to roles: if the granting flow's index is over budget,
    /// the role cannot be decided, and `resolve_role_grants` must say so with
    /// the typed error. Mapping it to "no receipts" would answer "not a
    /// member" for every candidate on a read that did not finish, which is
    /// the eviction attack in another form.
    ///
    /// The control is the same store answering within budget: Alice is
    /// granted. And a role WITHOUT `grantedByFlow` never reads receipts, so
    /// the same flood cannot touch it.
    ///
    /// Red if the loader error is swallowed into an empty list (Alice then
    /// resolves to "not a member" with `Ok`), or if the context wrapping
    /// hides the typed error from `downcast_ref`.
    #[tokio::test]
    async fn a_receipt_flood_is_a_budget_error_not_a_silent_deny() {
        let granting = granting_flow("Onboarding");
        let gated = gated_flow("Delivery", gate(&granting.flow_uri(), "done"));
        let cat = catalogue(vec![granting.clone(), gated.clone()]);
        let gate_role = role(Some(&spec(&granting.flow_uri(), "done")));
        let record = gated_run(Vec::new()).as_record(&gated);
        let alice = [did_of(ALICE).to_string()];

        let flood = ReceiptBudgetExceeded {
            flow: granting.flow_uri(),
            found: 257,
            cap: 256,
        };
        let err = resolve_role_grants(
            &store(Err(flood.clone())),
            "done",
            &gate_role,
            &record,
            &alice,
        )
        .await
        .expect_err("an over-budget index must not resolve to anything");
        assert_eq!(
            err.downcast_ref::<ReceiptBudgetExceeded>(),
            Some(&flood),
            "the typed budget error reaches the caller: {err:#}"
        );

        // Control: within budget, the same store grants.
        let honest = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)], &cat);
        let evidence = resolve_role_grants(
            &store(Ok(vec![honest])),
            "done",
            &gate_role,
            &record,
            &alice,
        )
        .await
        .expect("within budget");
        let grant = evidence[0]
            .resolve(&translated(), &gate_role, GrantContext::root(&cat))
            .expect("resolves");
        assert_eq!(grant.windows.len(), 1, "so the error above is the flood");

        // An ordinary role never asks, so F's flood cannot reach it.
        let plain = store(Err(flood));
        resolve_role_grants(&plain, "done", &role(None), &record, &alice)
            .await
            .expect("a role without grantedByFlow does not read receipts");
        assert!(plain.asked.lock().unwrap().is_empty());
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
        let outputs = [role_item(ROLE_INSTANCE)];

        let mut receipt = Ok(receipt_for(&flows[0], "done", T1, &outputs, &cat));
        for flow in flows.iter().skip(1) {
            let Ok(below) = receipt else { break };
            let rs = read_set("done", T3, &outputs, evidence(vec![below], Vec::new()));
            receipt = FlowReceipt::mint(
                flow,
                rs,
                outputs.to_vec(),
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
                .unwrap_or_else(|| panic!("level {step} of {MAX_GRANT_DEPTH} is within budget"));
            assert_eq!(ctx.remaining(), MAX_GRANT_DEPTH - step);
        }
        assert!(
            ctx.deeper().is_none(),
            "level {} is refused, not followed at zero forever",
            MAX_GRANT_DEPTH + 1
        );
    }

    /// **The boundary, and which way it falls.** A chain of grant flows one
    /// longer than the budget must be *refused*, never waved through.
    ///
    /// The final block is what makes the refusal mean anything. The over-long
    /// chain is refused, and the receipt *carried inside it* — the same
    /// bytes, read with a full budget — verifies. So the depth is what
    /// changed the answer: not a broken fixture, not a verifier that refuses
    /// everything nested. That is the module doc's "verification is not
    /// compositional past the cap", made falsifiable.
    ///
    /// Red with `deeper()` never returning `None` — and red in the opposite,
    /// much worse direction if running out of budget were ever treated as
    /// "grant anyway", since the over-long chain would then verify.
    #[test]
    fn a_chain_one_deeper_than_the_budget_is_refused_not_allowed() {
        let (cat, flows, at_the_cap) = chain(MAX_GRANT_DEPTH + 1);
        let at_the_cap = at_the_cap.expect("a chain exactly as deep as the budget still mints");
        let verdict = verify_receipt_within(GrantContext::root(&cat), &at_the_cap);
        assert!(
            verdict.is_verified(),
            "{MAX_GRANT_DEPTH} levels is exactly the budget, so this must still verify — got: \
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
        let outputs = [role_item(ROLE_INSTANCE)];
        let hand_built = FlowReceipt {
            flow_uri: deepest.flow_uri(),
            flow_dna_hash: flow_dna_hash(deepest).expect("hash"),
            terminal_state: "done".into(),
            outputs: outputs.to_vec(),
            read_set: read_set(
                "done",
                T3,
                &outputs,
                evidence(vec![nested.clone()], Vec::new()),
            ),
            evidence_preimage: Vec::new(),
        };
        let verdict = verify_receipt_within(GrantContext::root(&cat), &hand_built);
        // Deliberately NOT asserting which *kind* of denial this is: #1077
        // (Rejected vs Undecidable for budget exhaustion) is open, and this
        // test pins only the property that must never regress.
        assert_ne!(
            verdict.outcome(),
            VerdictKind::Verified,
            "running out of budget must DENY the grant, never allow it — got: {verdict}"
        );

        // The depth error refuses the receipt one level up. It never aborts
        // the reader's own fold (module header, § *Running out of depth is
        // undecidable*): if it did, anybody could plant a deep chain in F's
        // index and stop the gated flow from deriving a state at all.
        let own = fold_read_set(
            deepest,
            &hand_built.read_set.reverified(),
            GrantContext::root(&cat),
        )
        .expect("an over-deep chain in the carried evidence must not abort the reader's own fold");
        assert_eq!(
            own.state, "open",
            "the grant the chain claims is simply not there"
        );

        let inner = verify_receipt_within(GrantContext::root(&cat), &nested);
        assert!(
            inner.is_verified(),
            "the nested chain is sound on its own — so the refusal above is the budget, not a \
             fixture that never verified — got: {inner}"
        );
    }

    /// A root context over `cat`, walked down until `remaining` levels are
    /// left: the context a gate sees that many levels of nesting below the
    /// cap.
    fn with_remaining(cat: &HashMap<String, SHACLFlow>, remaining: usize) -> GrantContext<'_> {
        let mut ctx = GrantContext::root(cat);
        while ctx.remaining() > remaining {
            ctx = ctx.deeper().expect("within the budget");
        }
        ctx
    }

    /// **Running out of depth is "I could not decide", not "not a member".**
    /// Lal's approval note on #1076: it was the third place in this stack
    /// where the two were one value. The other two (a receipt flood, an
    /// unknown granting flow) were already errors.
    ///
    /// The typed error has to survive two layers, because each of them wraps
    /// it: `resolve`, and `fold_read_set`, whose `role_grant_views` adds
    /// context. A downcast at the fold is what a caller actually holds.
    ///
    /// Two controls. With one level of budget left, the same receipt grants,
    /// so the error is the depth and not the receipt. And at the cap, an
    /// instance carrying **no** receipt for the flow is still an ordinary
    /// "not a member": nothing was left unchecked, so "no" is a checked
    /// answer there.
    ///
    /// Red while `granted_by_flow_at` answers `None` at the cap (`resolve`
    /// returns `Ok` with no window).
    #[test]
    fn running_out_of_depth_is_a_typed_error_not_a_non_member() {
        let (receipt, gated, cat) = one_level();
        let granting_uri = receipt.flow_uri.clone();
        let gate_role = role(Some(&spec(&granting_uri, "done")));
        let ev = evidence(vec![receipt], Vec::new());
        let at_cap = with_remaining(&cat, 0);
        let expected = GrantDepthExceeded {
            output: OutputRef {
                class_name: ROLE.into(),
                id: ROLE_INSTANCE.into(),
            },
            flow: granting_uri.clone(),
        };

        let err = ev[0]
            .resolve(&translated(), &gate_role, at_cap)
            .expect_err("a receipt the budget cannot reach leaves the grant undecided");
        assert_eq!(
            err.downcast_ref::<GrantDepthExceeded>(),
            Some(&expected),
            "the typed depth error names the instance and the flow: {err:#}"
        );

        let err = fold_read_set(&gated, &gated_run(ev.clone()).reverified(), at_cap)
            .expect_err("an undecidable grant aborts the fold that needed it");
        assert_eq!(
            err.downcast_ref::<GrantDepthExceeded>(),
            Some(&expected),
            "the typed error survives `role_grant_views`' context: {err:#}"
        );

        let one_left = ev[0]
            .resolve(&translated(), &gate_role, with_remaining(&cat, 1))
            .expect("one level of budget is enough for an ungated granting flow");
        assert_eq!(
            one_left
                .windows
                .iter()
                .map(|w| w.granted_at.as_str())
                .collect::<Vec<_>>(),
            vec![T1],
            "so the error above is the depth, not the receipt"
        );

        let nothing_to_check = evidence(Vec::new(), Vec::new());
        let view = nothing_to_check[0]
            .resolve(&translated(), &gate_role, at_cap)
            .expect("with no receipt for the flow to verify, the answer is a checked \"no\"");
        assert!(view.windows.is_empty());
    }

    /// **Why "not a member" at the cap was fail-OPEN, not just quiet.** A
    /// dropped candidate can break a tie. `role_grant_views` already refuses
    /// the whole fold for exactly this reason: `Contention` fires only while
    /// both edges out of a state are quorate, so taking one voter away
    /// de-quorates one edge and lets the other one fire.
    ///
    /// Here Alice holds the role and votes on both edges out of `open`: the
    /// gated `done` and the ungated `alt`. Both are terminal, so a reader
    /// with budget left sees a contested run. That is not a completion. A
    /// reader at the cap that answered "Alice is not a member" would drop
    /// her `done` vote, see only `alt`, and verify a receipt claiming `alt`.
    /// The receipt would verify ONLY because it was nested deep. The module
    /// doc promises the opposite: nesting can refuse more, never allow more.
    ///
    /// The refusal is pinned to the depth error (the reason text), not only
    /// to "not verified", so it is the depth guard that answered and not
    /// some other failure in a hand-built receipt. Which verdict *kind* that
    /// is stays open (#1077).
    ///
    /// Red while `granted_by_flow_at` answers `None` at the cap: the receipt
    /// then verifies at the cap.
    #[test]
    fn at_the_cap_a_voter_dropped_for_depth_cannot_break_a_tie() {
        let granting = granting_flow("Onboarding");
        let split = flow_json(
            "Split",
            json!([
                { "name": "open", "value": 0.0 },
                {
                    "name": "done",
                    "value": 1.0,
                    "consensusRule": { "n": 1, "fromRole": gate(&granting.flow_uri(), "done") },
                },
                { "name": "alt", "value": 1.0 },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
                { "action_name": "Divert", "from_state": "open", "to_state": "alt", "actions": [] },
            ]),
        );
        let cat = catalogue(vec![granting.clone(), split.clone()]);
        let alice_granted = receipt_for(&granting, "done", T1, &[role_item(ROLE_INSTANCE)], &cat);

        let outputs = [item(TASK_CLASS, BASE)];
        let claims_alt = FlowReceipt {
            flow_uri: split.flow_uri(),
            flow_dna_hash: flow_dna_hash(&split).expect("hash"),
            terminal_state: "alt".into(),
            outputs: outputs.to_vec(),
            read_set: ReadSet {
                instance_uri: INSTANCE.to_string(),
                subject: BASE.to_string(),
                genesis: "open".to_string(),
                proposals: vec![
                    proposal("done", T2, &outputs),
                    proposal("alt", T3, &outputs),
                ],
                role_grants: evidence(vec![alice_granted], Vec::new()),
            },
            evidence_preimage: Vec::new(),
        };

        let with_budget = verify_receipt_within(GrantContext::root(&cat), &claims_alt);
        assert!(
            matches!(with_budget, ReceiptVerdict::Contested { .. }),
            "precondition: with budget left, Alice counts on both edges and the run is \
             contested — got: {with_budget}"
        );

        let at_cap = verify_receipt_within(with_remaining(&cat, 0), &claims_alt);
        assert!(
            !at_cap.is_verified(),
            "a receipt must not verify because it was nested past the cap — got: {at_cap}"
        );
        assert!(
            format!("{at_cap}").contains("past the grant depth budget"),
            "and it is the depth guard that refused it — got: {at_cap}"
        );
    }

    // ---- a gate this replica cannot evaluate --------------------------------

    /// **A gate naming a flow this replica does not hold is an error**, the
    /// rule `flow_valid_outputs` already applies (#1127): "I do not have F's
    /// rules" is not "no receipt grants this". Answering "not a member"
    /// instead would let a replica that has not synced F silently derive a
    /// different state for the gated flow than one that has.
    ///
    /// The control is the same evidence and the same gate over a catalogue
    /// that holds F: granted. So the error is the missing definition.
    ///
    /// Red while `resolve` lets every carried receipt come back
    /// `FlowUnknown` and answers `Ok` with no window.
    #[test]
    fn a_gate_naming_a_flow_this_replica_does_not_hold_is_an_error() {
        let (receipt, _gated, cat) = one_level();
        let granting_uri = receipt.flow_uri.clone();
        let ev = evidence(vec![receipt], Vec::new());

        let unsynced = catalogue(Vec::new());
        let err = resolve(&ev[0], Some(&spec(&granting_uri, "done")), &unsynced)
            .expect_err("a gate on a flow this replica does not hold cannot be decided");
        assert!(
            format!("{err:#}").contains(&granting_uri),
            "the error names the missing flow, got: {err:#}"
        );

        assert_eq!(
            resolve(&ev[0], Some(&spec(&granting_uri, "done")), &cat)
                .expect("with the definition synced, it resolves")
                .windows
                .len(),
            1
        );
    }

    // ---- the inversion that must never be configurable ---------------------

    /// `grantedByFlow` with a `count` satisfied by zero instances inverts the
    /// gate: "eligible while no verifiable receipt exists". Every reason a
    /// receipt might fail — an un-synced definition, a broken signature, a
    /// chain past the depth cap — would then become a reason to GRANT.
    ///
    /// The second half shows the refused behaviour is real: with no windows
    /// at all, `{max: 0}` reports the candidate as eligible.
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
        let mut zero_gate = role(Some(&spec(&granting_uri, "done")));
        zero_gate.count = Some(zero_ok.clone());

        let err = ev[0]
            .resolve(&translated(), &zero_gate, GrantContext::root(&cat))
            .expect_err("a gate that grants when verification FAILS is not a gate");
        assert!(
            format!("{err:#}").contains("satisfied by zero instances"),
            "the refusal must name the inversion, got: {err:#}"
        );

        let ungranted =
            resolve(&ev[0], Some(&spec(&granting_uri, "done")), &cat).expect("resolves");
        assert!(ungranted.windows.is_empty());
        assert!(
            ungranted.eligible_at(T3, Some(&zero_ok)),
            "this is why the combination is refused rather than merely discouraged"
        );
    }
}

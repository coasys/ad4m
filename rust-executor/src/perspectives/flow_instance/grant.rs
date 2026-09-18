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

use super::receipt::FlowReceipt;
use super::verify::{verify_receipt_within, ReceiptVerdict};
use crate::perspectives::shacl_parser::{GrantedByFlow, SHACLFlow};
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

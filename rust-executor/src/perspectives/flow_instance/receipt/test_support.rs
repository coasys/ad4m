//! Fixtures shared by the receipt tests.

pub(super) use crate::perspectives::flow_instance::grant::GrantContext;

pub(super) use crate::perspectives::flow_instance::test_support::{
    delivered, did_of, flow_json, hash_of, item, out_item, outs, preimage, signed_proposal,
    signed_terminal_proposal, signed_vote, two_state_flow, ATTACKER, BASE, DELIVERABLE, INSTANCE,
    OUTPUT, T1, T2, T3,
};
use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};
/// Persona names rather than DIDs: every proposal below is now signed for
/// real, because `mint` folds through
/// [`ReadSet::reverified`](crate::perspectives::flow_instance::ReadSet::reverified)
/// and a `did:key:alice` placeholder signs nothing.
pub(super) const ALICE: &str = "alice";
pub(super) const BOB: &str = "bob";

/// One proposal, self-proposed and therefore self-voted: under the
/// default `{ n: 1 }` rule that is a settled edge. `proposer` is a
/// persona *name*; the links are signed with that persona's real key.
/// `nonce` salts the content-addressed URI the fixture computes — the
/// old per-fixture URI strings serve as nonces now.
pub(super) fn proposal(
    nonce: &str,
    proposer: &str,
    from: &str,
    to: &str,
    seal: &str,
    at: &str,
) -> ProposalLinks {
    let (uri, links) = signed_proposal(nonce, proposer, from, to, seal, at);
    ProposalLinks { uri, links }
}

/// A proposal into a terminal state: signed like [`proposal`], and
/// additionally naming `outputs` and signing `committed` as their
/// `outputs_hash`. Honest when `committed == hash_of(outputs)`.
#[allow(clippy::too_many_arguments)]
pub(super) fn committing(
    nonce: &str,
    proposer: &str,
    from: &str,
    to: &str,
    seal: &str,
    outputs: &[&str],
    committed: &str,
    at: &str,
) -> ProposalLinks {
    let (uri, links) =
        signed_terminal_proposal(nonce, proposer, from, to, seal, outputs, committed, at);
    ProposalLinks { uri, links }
}

/// Alice's honest final proposal `open → done`, committing to [`OUTPUT`].
pub(super) fn final_proposal(at: &str) -> ProposalLinks {
    committing(
        "ad4m://p/1",
        ALICE,
        "open",
        "done",
        &delivered().seal,
        &[OUTPUT],
        &hash_of(&[OUTPUT]),
        at,
    )
}

pub(super) fn read_set(genesis: &str, proposals: Vec<ProposalLinks>) -> ReadSet {
    ReadSet {
        instance_uri: INSTANCE.to_string(),
        subject: BASE.to_string(),
        genesis: genesis.to_string(),
        proposals,
        role_grants: Vec::new(),
    }
}

pub(super) fn completed() -> ReadSet {
    read_set("open", vec![final_proposal(T1)])
}

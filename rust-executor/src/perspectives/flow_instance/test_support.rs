//! Test fixtures shared by the flow-instance modules: claimed-verdict
//! links, cryptographically signed personas, and the fixture outputs.

use super::atom::*;
use crate::perspectives::flow_evaluator::EvidenceItem;
use crate::types::DecoratedLinkExpression;
use crate::types::{DecoratedExpressionProof, Link};

pub const INSTANCE: &str = "ad4m://flow/instance/i1";
pub const PROPOSAL: &str = "ad4m://flow/proposal/p1";
/// The nonce every claimed-verdict fixture proposal salts its URI with.
/// Tests that need twins pass their own distinct nonces instead.
pub const NONCE: &str = "fixture-nonce";
pub const ALICE: &str = "did:key:alice";
pub const BOB: &str = "did:key:bob";
pub const MALLORY: &str = "did:key:mallory";
pub const T1: &str = "2026-01-01T00:00:00.000Z";
pub const T2: &str = "2026-01-02T00:00:00.000Z";
pub const T3: &str = "2026-01-03T00:00:00.000Z";

pub fn literal(s: &str) -> String {
    format!("literal:string:{}", urlencoding::encode(s))
}

/// One link as `get_links` returns it. Author, signature verdict and
/// timestamp are all inputs the checks read, so every fixture states
/// them explicitly.
pub fn link(
    predicate: &str,
    target: &str,
    author: &str,
    valid: bool,
    timestamp: &str,
) -> DecoratedLinkExpression {
    DecoratedLinkExpression {
        author: author.to_string(),
        timestamp: timestamp.to_string(),
        data: Link {
            source: PROPOSAL.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        proof: DecoratedExpressionProof {
            key: format!("{author}#key"),
            signature: "sig".to_string(),
            valid: Some(valid),
            invalid: Some(!valid),
        },
        status: None,
    }
}

/// The six links `write_flow_transition_proposal` emits, all signed by
/// the proposer.
pub fn honest_proposal(
    proposer: &str,
    from: &str,
    to: &str,
    seal: &str,
    at: &str,
) -> Vec<DecoratedLinkExpression> {
    vec![
        link(PROPOSER_PREDICATE, proposer, proposer, true, at),
        link(FLOW_INSTANCE_PREDICATE, INSTANCE, proposer, true, at),
        link(FROM_STATE_PREDICATE, &literal(from), proposer, true, at),
        link(TO_STATE_PREDICATE, &literal(to), proposer, true, at),
        link(
            EVIDENCE_HASHES_PREDICATE,
            &literal(seal),
            proposer,
            true,
            at,
        ),
        link(
            PROPOSAL_NONCE_PREDICATE,
            &literal(NONCE),
            proposer,
            true,
            at,
        ),
    ]
}

/// The URI these links address: recomputed from the proposer-signed
/// fields exactly as [`TransitionAtom::from_links`] recomputes it, so a
/// field-focused test exercises field logic rather than tripping the URI
/// check with a stale fixture constant. Falls back to [`PROPOSAL`] when
/// a field is missing or ambiguous — `from_links` rejects those shapes
/// before it reaches the URI either way. Tests **of** the URI check call
/// `from_links` with a deliberately different URI instead.
pub fn addressed_uri(links: &[DecoratedLinkExpression]) -> String {
    let Ok(proposer) = self_authored_proposer(links) else {
        return PROPOSAL.to_string();
    };
    let field = |p: &'static str| unique_field(links, p, &proposer).ok();
    let (Some(from), Some(to), Some(seal), Some(nonce)) = (
        field(FROM_STATE_PREDICATE),
        field(TO_STATE_PREDICATE),
        field(EVIDENCE_HASHES_PREDICATE),
        field(PROPOSAL_NONCE_PREDICATE),
    ) else {
        return PROPOSAL.to_string();
    };
    let outputs_hash = field(OUTPUTS_HASH_PREDICATE);
    proposal_uri(
        INSTANCE,
        &from,
        &to,
        &seal,
        outputs_hash.as_deref(),
        &proposer,
        &nonce,
    )
}

pub fn atom_of(links: &[DecoratedLinkExpression]) -> Result<TransitionAtom, AtomRejection> {
    TransitionAtom::from_links(INSTANCE, &addressed_uri(links), links)
}

// -----------------------------------------------------------------------
// Cryptographically honest fixtures
//
// The builders above state a signature verdict; these ones *earn* it.
// Both kinds are needed and neither replaces the other:
//
// - `link` / `honest_proposal` exercise the checks that read a carried
//   verdict (`signed_by`, `from_links`), where the verdict is the input
//   under test and inventing a keypair would only obscure it;
// - `signed_*` exercise anything downstream of
//   [`ReadSet::reverified`](super::super::ReadSet::reverified), which
//   recomputes the verdict from the signature — there a fixture that
//   merely *claims* `valid: true` is exactly the minter's word the
//   reader no longer takes, so it has to sign for real.
// -----------------------------------------------------------------------

use crate::agent::signatures::TestSigner;
use crate::types::{Link as CoreLink, LinkExpression, LinkStatus as CoreLinkStatus};
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

/// A named persona holding a **real** Ed25519 keypair. Leaked on first use
/// so the DIDs read like the `&'static str` constants they stand beside.
/// One keypair per persona per process.
pub fn persona(name: &str) -> &'static TestSigner {
    static SIGNERS: LazyLock<Mutex<HashMap<String, &'static TestSigner>>> =
        LazyLock::new(|| Mutex::new(HashMap::new()));
    *SIGNERS
        .lock()
        .expect("persona registry")
        .entry(name.to_string())
        .or_insert_with(|| Box::leak(Box::new(TestSigner::generate())))
}

/// `persona(name).did`, for fixtures that need the identity rather than
/// the key.
pub fn did_of(name: &str) -> &'static str {
    &persona(name).did
}

/// One link as `get_links` returns it, with its `valid` flag honoured
/// **cryptographically**: a valid link is signed by `author_name`'s own
/// key over its own data and timestamp, and a forged one carries a
/// signature from a key that is not theirs.
///
/// `proof.valid` is still pre-set to match, because that is what a store
/// hands back — but every reader downstream of the ingest recomputes it,
/// so a fixture whose claim and signature disagree gets ruled on by the
/// signature. That disagreement is itself a fixture: pass
/// `claims_valid = Some(true)` with `valid = false` to build the forgery
/// a dishonest minter would carry.
pub fn signed_link(
    source: &str,
    predicate: &str,
    target: &str,
    author_name: &str,
    valid: bool,
    claims_valid: Option<bool>,
    timestamp: &str,
) -> DecoratedLinkExpression {
    let at = chrono::DateTime::parse_from_rfc3339(timestamp)
        .unwrap_or_else(|e| panic!("fixture timestamp `{timestamp}`: {e}"))
        .with_timezone(&chrono::Utc);
    let author = persona(author_name);
    let signing_key = if valid { author } else { persona("forger") };
    let mut expr = signing_key.sign_at(
        CoreLink {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        }
        .normalize(),
        at,
    );
    expr.author = author.did.clone();
    expr.proof.key = author.key_id.clone();
    let mut link =
        DecoratedLinkExpression::from((LinkExpression::from(expr), CoreLinkStatus::Shared));
    let claimed = claims_valid.unwrap_or(valid);
    link.proof.valid = Some(claimed);
    link.proof.invalid = Some(!claimed);
    link
}

/// The six links `write_flow_transition_proposal` emits, all genuinely
/// signed by the proposer, sourced at the proposal's **content-addressed
/// URI** — computed here from the same fields, so the fixture and the
/// production writer agree by construction. `nonce` distinguishes twins;
/// the old fixture URI strings ("ad4m://p/1") make fine nonces. Returns
/// the URI alongside the links because everything downstream (votes, the
/// read-set) names the proposal by it.
pub fn signed_proposal(
    nonce: &str,
    proposer_name: &str,
    from: &str,
    to: &str,
    seal: &str,
    at: &str,
) -> (String, Vec<DecoratedLinkExpression>) {
    let uri = proposal_uri(INSTANCE, from, to, seal, None, did_of(proposer_name), nonce);
    let signed = |predicate: &str, target: &str| {
        signed_link(&uri, predicate, target, proposer_name, true, None, at)
    };
    let links = vec![
        signed(PROPOSER_PREDICATE, did_of(proposer_name)),
        signed(FLOW_INSTANCE_PREDICATE, INSTANCE),
        signed(FROM_STATE_PREDICATE, &literal(from)),
        signed(TO_STATE_PREDICATE, &literal(to)),
        signed(EVIDENCE_HASHES_PREDICATE, &literal(seal)),
        signed(PROPOSAL_NONCE_PREDICATE, &literal(nonce)),
    ];
    (uri, links)
}

/// [`signed_proposal`] into a terminal state: the six links plus one
/// signed `output` link per id in `outputs` (as [`out_ref`]) and a signed
/// `outputs_hash` equal to `committed` — which is part of the URI
/// preimage, so the returned URI covers it. Pass [`hash_of`]`(outputs)`
/// for an honest proposal; anything else builds a proposal whose
/// commitment does not match its named outputs.
#[allow(clippy::too_many_arguments)]
pub fn signed_terminal_proposal(
    nonce: &str,
    proposer_name: &str,
    from: &str,
    to: &str,
    seal: &str,
    outputs: &[&str],
    committed: &str,
    at: &str,
) -> (String, Vec<DecoratedLinkExpression>) {
    let uri = proposal_uri(
        INSTANCE,
        from,
        to,
        seal,
        Some(committed),
        did_of(proposer_name),
        nonce,
    );
    let links = signed_terminal_links_at(
        &uri,
        proposer_name,
        from,
        to,
        seal,
        outputs,
        committed,
        nonce,
        at,
    );
    (uri, links)
}

/// The links of [`signed_terminal_proposal`] sourced at an **arbitrary**
/// `uri` — the raw material of the post-co-sign swap: a proposer who
/// re-signs `outputs_hash` (or the seal) under a URI that was addressed
/// for other fields produces exactly this shape, genuinely signed.
/// Honest fixtures use [`signed_terminal_proposal`], which computes the
/// URI these links actually address.
#[allow(clippy::too_many_arguments)]
pub fn signed_terminal_links_at(
    uri: &str,
    proposer_name: &str,
    from: &str,
    to: &str,
    seal: &str,
    outputs: &[&str],
    committed: &str,
    nonce: &str,
    at: &str,
) -> Vec<DecoratedLinkExpression> {
    let signed = |predicate: &str, target: &str| {
        signed_link(uri, predicate, target, proposer_name, true, None, at)
    };
    let mut links = vec![
        signed(PROPOSER_PREDICATE, did_of(proposer_name)),
        signed(FLOW_INSTANCE_PREDICATE, INSTANCE),
        signed(FROM_STATE_PREDICATE, &literal(from)),
        signed(TO_STATE_PREDICATE, &literal(to)),
        signed(EVIDENCE_HASHES_PREDICATE, &literal(seal)),
        signed(PROPOSAL_NONCE_PREDICATE, &literal(nonce)),
    ];
    links.extend(
        outputs
            .iter()
            .map(|id| signed(OUTPUT_PREDICATE, &literal(&out_ref(id).encode()))),
    );
    links.push(signed(OUTPUTS_HASH_PREDICATE, &literal(committed)));
    links
}

/// The class every fixture output is an instance of.
pub const OUT_CLASS: &str = "coasys://Deliverable";

/// `id` as an output of [`OUT_CLASS`].
pub fn out_ref(id: &str) -> OutputRef {
    OutputRef {
        class_name: OUT_CLASS.to_string(),
        id: id.to_string(),
    }
}

/// `id`'s content as the fixture graph holds it: a function of the id, so
/// a proposer and a receipt built apart agree on it.
pub fn out_item(id: &str) -> EvidenceItem {
    EvidenceItem {
        id: id.to_string(),
        class_name: OUT_CLASS.to_string(),
        content: serde_json::json!({ "id": id, "title": format!("the content of {id}") })
            .to_string(),
    }
}

/// [`out_item`] for each id.
pub fn out_items(ids: &[&str]) -> Vec<EvidenceItem> {
    ids.iter().map(|id| out_item(id)).collect()
}

/// [`outputs_hash`] over [`out_items`], for fixtures.
pub fn hash_of(ids: &[&str]) -> String {
    outputs_hash(&out_items(ids))
}

/// A genuinely signed `proposal --acceptedBy--> voter` co-signature.
pub fn signed_vote(proposal_uri: &str, voter_name: &str, at: &str) -> DecoratedLinkExpression {
    signed_link(
        proposal_uri,
        ACCEPTED_BY_PREDICATE,
        did_of(voter_name),
        voter_name,
        true,
        None,
        at,
    )
}

pub const D1: &str = "ad4m://deliverable/d1";
pub const D2: &str = "ad4m://deliverable/d2";
pub const ATTACKER: &str = "ad4m://attacker/node";

pub fn with_outputs(outputs: &[&str], committed: &str) -> Vec<DecoratedLinkExpression> {
    let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
    links.extend(outputs.iter().map(|id| {
        link(
            OUTPUT_PREDICATE,
            &literal(&out_ref(id).encode()),
            ALICE,
            true,
            T1,
        )
    }));
    links.push(link(
        OUTPUTS_HASH_PREDICATE,
        &literal(committed),
        ALICE,
        true,
        T1,
    ));
    links
}

// -----------------------------------------------------------------------
// The receipt fixtures, shared by the receipt and verify tests
// -----------------------------------------------------------------------

use crate::perspectives::flow_evaluator::evidence_hash;
use crate::perspectives::flow_instance::receipt::EvidencePreimage;
use crate::perspectives::shacl_parser::SHACLFlow;
use serde_json::Value;

/// The run's base expression.
pub const BASE: &str = "ad4m://task/t1";
/// What most fixture flows' terminal `done` state requires. Incidental
/// to the outputs since #1104: they are what the final proposal names.
pub const DELIVERABLE: &str = OUT_CLASS;
/// The node every honest final proposal names as the run's output.
pub const OUTPUT: &str = D1;

/// A flow named `Delivery` with the given states and transitions.
pub fn flow_json(states: Value, transitions: Value) -> SHACLFlow {
    serde_json::from_value(serde_json::json!({
        "name": "Delivery",
        "namespace": "coasys://",
        "states": states,
        "transitions": transitions,
    }))
    .expect("fixture flow parses")
}

/// `open → done`, `done` terminal and guarded by [`DELIVERABLE`], default
/// `{ n: 1 }` quorum. The guard is incidental to the outputs since #1104.
pub fn two_state_flow() -> SHACLFlow {
    flow_json(
        serde_json::json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0, "requires": [{ "className": DELIVERABLE }] },
        ]),
        serde_json::json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
        ]),
    )
}

/// Each id's preimage as the fixture graph holds it ([`out_items`]).
pub fn outs(ids: &[&str]) -> Vec<EvidenceItem> {
    out_items(ids)
}

/// What `done`'s guard matched on the honest run: one deliverable.
pub fn delivered() -> EvidencePreimage {
    deliverables(&[OUTPUT])
}

pub fn deliverables(ids: &[&str]) -> EvidencePreimage {
    preimage(
        &[DELIVERABLE],
        ids.iter()
            .map(|id| item(id, DELIVERABLE, &format!("{{\"id\":\"{id}\"}}")))
            .collect(),
    )
}

/// A preimage sealed over `items` under `class_names`.
pub fn preimage(class_names: &[&str], items: Vec<EvidenceItem>) -> EvidencePreimage {
    let class_names: Vec<String> = class_names.iter().map(|s| s.to_string()).collect();
    EvidencePreimage {
        seal: evidence_hash(&class_names, &items),
        class_names,
        items,
    }
}

pub fn item(id: &str, class_name: &str, content: &str) -> EvidenceItem {
    EvidenceItem {
        id: id.to_string(),
        class_name: class_name.to_string(),
        content: content.to_string(),
    }
}

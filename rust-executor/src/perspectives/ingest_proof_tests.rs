//! A link's proof (key, signature, verdict and signed target bytes) is one
//! unit that only the signature's owner can replace (#1146).
//!
//! Two layers hold that, and each is tested here through the API a reader
//! uses (`get_all_links`):
//!
//! - **Ingest** (`diff_from_link_language`): a remote addition whose proof
//!   does not verify against its own `author` is dropped, and a remote
//!   addition or removal never touches a link stored as `Local`.
//! - **Store** (`SparqlStore::add_link`): a re-insert on an existing reifier
//!   writes the proof only as an upgrade (stored proof missing or not
//!   verified, incoming verified). This covers local write paths such as
//!   `add_link_expression`, which store a caller-supplied foreign link.
//!
//! Alice is the genuine author, Mallory a second member replaying Alice's
//! link fields. Every fixture that is meant to collide asserts that it lands
//! on the same reifier as Alice's link, by reading back a single link.

use crate::agent::signatures::TestSigner;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkStatus, PerspectiveDiff};

const RAW_TARGET: &str = "literal:string:Write the guide";
const CANONICAL_TARGET: &str = "literal:string:Write%20the%20guide";

fn signed_at(signer: &TestSigner, source: &str, target: &str, ts: &str) -> LinkExpression {
    let data = Link {
        source: source.to_string(),
        predicate: Some("ingest://p".to_string()),
        target: target.to_string(),
    };
    let signed = signer.sign_at(data.normalize(), ts.parse().expect("fixture timestamp"));
    LinkExpression {
        author: signed.author,
        timestamp: signed.timestamp,
        data: signed.data,
        proof: signed.proof,
        status: Some(LinkStatus::Shared),
    }
}

fn signed(signer: &TestSigner, source: &str, target: &str) -> LinkExpression {
    signed_at(signer, source, target, "2026-09-29T10:00:00.000Z")
}

/// Alice's five reifier fields with a signature that does not verify
/// (same length, still hex, wrong bytes).
fn with_garbage_signature(genuine: &LinkExpression) -> LinkExpression {
    let mut forged = genuine.clone();
    forged.proof.signature = forged
        .proof
        .signature
        .chars()
        .map(|c| if c == '0' { '1' } else { '0' })
        .collect();
    assert!(!forged.compute_proof_valid(), "fixture must not verify");
    forged
}

/// The one link `links` must hold, with its proof.
fn only_link(links: Vec<DecoratedLinkExpression>) -> DecoratedLinkExpression {
    assert_eq!(
        links.len(),
        1,
        "the colliding fixtures must land on one reifier and read back once: {links:?}"
    );
    links.into_iter().next().unwrap()
}

/// The stored row is `genuine`'s: same key, signature and target bytes, a
/// verified verdict, and the verdict agrees with a fresh check of the row
/// (the #1115 invariant).
fn assert_row_is(row: &DecoratedLinkExpression, genuine: &LinkExpression) {
    assert_eq!(row.proof.key, genuine.proof.key, "stored proofKey");
    assert_eq!(
        row.proof.signature, genuine.proof.signature,
        "stored proofSignature"
    );
    assert_eq!(
        row.data.target, genuine.data.target,
        "read-back target bytes"
    );
    assert_eq!(row.proof.valid, Some(true), "stored proofValid");
    assert!(
        row.compute_proof_valid(),
        "the stored verdict must equal a fresh check of the stored row"
    );
}

// ── Store: the proof is replaced only on an upgrade ──

/// A re-insert whose proof does not verify leaves a verified proof alone:
/// a tampered signature, another agent's key with that agent's own real
/// signature, and no proof at all.
#[test]
fn store_a_bad_proof_does_not_replace_a_verified_one() {
    let alice = TestSigner::generate();
    let other = TestSigner::generate();
    let genuine = signed(&alice, "ingest://keep", "ingest://t");

    let mut other_key = genuine.clone();
    other_key.proof = signed(&other, "ingest://keep", "ingest://t").proof;
    let mut no_proof = genuine.clone();
    no_proof.proof.key = String::new();
    no_proof.proof.signature = String::new();

    for (label, forged) in [
        ("tampered signature", with_garbage_signature(&genuine)),
        ("different key", other_key),
        ("no proof", no_proof),
    ] {
        assert!(!forged.compute_proof_valid(), "{label}: must not verify");
        let store = SparqlStore::new(None).unwrap();
        store.add_link(&genuine).unwrap();
        store.add_link(&forged).unwrap();
        let row = only_link(store.get_all_links().unwrap());
        assert_row_is(&row, &genuine);
    }
}

/// A stored proof that does not verify is replaced, as a whole, by one
/// that does. A second proof that does not verify is not an upgrade: the
/// first one stays.
#[test]
fn store_a_valid_proof_upgrades_an_unverified_one() {
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://upgrade", "ingest://t");
    let first_bad = with_garbage_signature(&genuine);
    let mut second_bad = genuine.clone();
    second_bad.proof.signature = "cafebabe".to_string();

    let store = SparqlStore::new(None).unwrap();
    store.add_link(&first_bad).unwrap();
    store.add_link(&second_bad).unwrap();
    let row = only_link(store.get_all_links().unwrap());
    assert_eq!(row.proof.signature, first_bad.proof.signature);
    assert_eq!(row.proof.valid, Some(false));

    store.add_link(&genuine).unwrap();
    let row = only_link(store.get_all_links().unwrap());
    assert_row_is(&row, &genuine);
}

/// Verification never reads `proof.key`, so Alice's real signature under a
/// garbage key still verifies. valid→valid is not an upgrade: the replay
/// must not rewrite Alice's stored key. Both row shapes: one with no
/// `wireTarget` (the heal's shape, which writes only `wireTarget`) and one
/// with a `wireTarget` (which writes nothing).
#[test]
fn store_a_replayed_signature_with_another_key_keeps_the_stored_key() {
    let alice = TestSigner::generate();
    for target in ["ingest://t", RAW_TARGET] {
        let genuine = signed(&alice, "ingest://key", target);
        let mut replay = genuine.clone();
        replay.proof.key = "not-alices-key".to_string();
        assert!(
            replay.compute_proof_valid(),
            "fixture: verification ignores the key"
        );

        let store = SparqlStore::new(None).unwrap();
        store.add_link(&genuine).unwrap();
        store.add_link(&replay).unwrap();
        assert_row_is(&only_link(store.get_all_links().unwrap()), &genuine);
    }
}

/// Mallory re-sends Alice's link over an equivalent encoding of the target
/// (same stored value, so the same reifier) under Alice's author, timestamp
/// and signature. The signature does not cover those bytes, and the read-back
/// must keep Alice's signed bytes.
#[test]
fn store_an_equivalent_encoding_under_the_authors_signature_keeps_the_signed_bytes() {
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://wire", RAW_TARGET);
    let mut forged = genuine.clone();
    forged.data.target = CANONICAL_TARGET.to_string();
    assert!(!forged.compute_proof_valid(), "fixture must not verify");

    let store = SparqlStore::new(None).unwrap();
    store.add_link(&genuine).unwrap();
    store.add_link(&forged).unwrap();
    assert_row_is(&only_link(store.get_all_links().unwrap()), &genuine);
}

/// T3 in reverse: Alice signs the canonical target, so no `wireTarget` is
/// stored, and a caller re-sends the raw encoding under her signature. Same
/// reifier, same signature, no `wireTarget`: only the incoming verdict keeps
/// the heal from writing the unsigned bytes.
#[test]
fn store_a_canonical_row_is_not_healed_by_an_unverified_encoding() {
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://rev", CANONICAL_TARGET);
    let mut forged = genuine.clone();
    forged.data.target = RAW_TARGET.to_string();
    assert!(!forged.compute_proof_valid(), "fixture must not verify");

    let store = SparqlStore::new(None).unwrap();
    store.add_link(&genuine).unwrap();
    store.add_link(&forged).unwrap();
    assert_row_is(&only_link(store.get_all_links().unwrap()), &genuine);
}

/// Heal for rows written before #1141, which kept no `wireTarget`: the
/// same statement (same signature) re-ingested writes the signed target
/// bytes back, so the read-back verifies again.
///
/// Green on dev too, where every re-insert rewrites the whole row. It pins
/// the heal against an upgrade-only rule that forgets it.
#[test]
fn store_the_same_signature_restores_a_missing_wire_target() {
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://heal", RAW_TARGET);
    let store = SparqlStore::new(None).unwrap();
    store.add_link(&genuine).unwrap();
    store.remove_wire_target_annotation(&genuine).unwrap();
    let pre_1141 = only_link(store.get_all_links().unwrap());
    assert_eq!(
        pre_1141.data.target, CANONICAL_TARGET,
        "setup: without wireTarget the row reads back the canonical rendering"
    );
    assert!(
        !pre_1141.compute_proof_valid(),
        "setup: which does not verify"
    );

    store.add_link(&genuine).unwrap();
    assert_row_is(&only_link(store.get_all_links().unwrap()), &genuine);
}

/// Negative control for the heal: a different valid signature on the same
/// reifier is a second statement, not the same one. It writes nothing, not
/// even the `wireTarget` the row is missing.
#[test]
fn store_a_different_valid_signature_does_not_heal_or_replace() {
    let alice = TestSigner::generate();
    let ts = "2026-09-29T10:00:00.000Z";
    let raw = signed_at(&alice, "ingest://heal-neg", RAW_TARGET, ts);
    let canonical = signed_at(&alice, "ingest://heal-neg", CANONICAL_TARGET, ts);
    assert!(canonical.compute_proof_valid());
    assert_ne!(raw.proof.signature, canonical.proof.signature);

    let store = SparqlStore::new(None).unwrap();
    store.add_link(&canonical).unwrap();
    store.add_link(&raw).unwrap();
    assert_row_is(&only_link(store.get_all_links().unwrap()), &canonical);

    let store = SparqlStore::new(None).unwrap();
    store.add_link(&raw).unwrap();
    store.remove_wire_target_annotation(&raw).unwrap();
    store.add_link(&canonical).unwrap();
    let row = only_link(store.get_all_links().unwrap());
    assert_eq!(
        row.proof.signature, raw.proof.signature,
        "the stored signature stays the first one"
    );
    assert_eq!(
        row.data.target, CANONICAL_TARGET,
        "no wireTarget is written from another signature"
    );
}

// ── Ingest: remote additions must verify, Local rows are not touched ──

async fn perspective() -> PerspectiveInstance {
    setup_perspective_no_llm(&[]).await.0
}

async fn ingest(
    p: &PerspectiveInstance,
    additions: Vec<LinkExpression>,
    removals: Vec<LinkExpression>,
) {
    let mut diff = PerspectiveDiff::from_additions(additions);
    diff.removals = removals;
    p.diff_from_link_language(diff)
        .await
        .expect("diff_from_link_language");
}

fn links_from(p: &PerspectiveInstance, source: &str) -> Vec<DecoratedLinkExpression> {
    p.sparql_store
        .get_all_links()
        .unwrap()
        .into_iter()
        .filter(|l| l.data.source == source)
        .collect()
}

/// A remote addition whose signature does not verify against its author is
/// not stored: no row, verified or not.
#[tokio::test]
async fn ingest_drops_an_addition_that_does_not_verify() {
    let p = perspective().await;
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://drop", "ingest://t");
    ingest(&p, vec![with_garbage_signature(&genuine)], vec![]).await;
    assert_eq!(links_from(&p, "ingest://drop"), vec![]);

    // Control: the genuine link is stored.
    ingest(&p, vec![genuine.clone()], vec![]).await;
    assert_row_is(&only_link(links_from(&p, "ingest://drop")), &genuine);
}

/// T1, verdict flip: Mallory re-asserts Alice's link with a garbage
/// signature. Alice's row keeps its key, signature and verdict.
#[tokio::test]
async fn ingest_a_garbage_signature_does_not_flip_a_verified_link() {
    let p = perspective().await;
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://flip", "ingest://t");
    ingest(&p, vec![genuine.clone()], vec![]).await;
    ingest(&p, vec![with_garbage_signature(&genuine)], vec![]).await;
    assert_row_is(&only_link(links_from(&p, "ingest://flip")), &genuine);
}

/// T3, `wireTarget` overwrite: Mallory sends the percent-encoded form of
/// Alice's raw target with Alice's author, timestamp and signature. The
/// read-back stays Alice's bytes and still verifies.
#[tokio::test]
async fn ingest_an_equivalent_encoding_does_not_overwrite_the_signed_bytes() {
    let p = perspective().await;
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://wire", RAW_TARGET);
    let mut forged = genuine.clone();
    forged.data.target = CANONICAL_TARGET.to_string();

    ingest(&p, vec![genuine.clone()], vec![]).await;
    ingest(&p, vec![forged], vec![]).await;
    assert_row_is(&only_link(links_from(&p, "ingest://wire")), &genuine);
}

/// T4: a remote diff never touches a link stored as `Local`. A valid copy
/// of it does not relabel it `Shared`, and a removal does not delete it.
#[tokio::test]
async fn ingest_leaves_a_local_link_alone() {
    let mut p = perspective().await;
    let alice = TestSigner::generate();
    let local = signed(&alice, "ingest://local", "ingest://t");
    p.add_link_expression(local.clone(), LinkStatus::Local, None)
        .await
        .unwrap();

    ingest(&p, vec![local.clone()], vec![]).await;
    let row = only_link(links_from(&p, "ingest://local"));
    assert_eq!(
        row.status,
        Some(LinkStatus::Local),
        "addition relabelled it"
    );

    ingest(&p, vec![], vec![local.clone()]).await;
    let row = only_link(links_from(&p, "ingest://local"));
    assert_eq!(row.status, Some(LinkStatus::Local));
    assert_row_is(&row, &local);
}

/// Control: removal semantics for Shared links are unchanged here (#1146
/// PR 2 adds the author check). A remote removal of a Shared link applies.
#[tokio::test]
async fn ingest_still_applies_a_removal_of_a_shared_link() {
    let p = perspective().await;
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://rm", "ingest://t");
    ingest(&p, vec![genuine.clone()], vec![]).await;
    assert_eq!(links_from(&p, "ingest://rm").len(), 1);
    ingest(&p, vec![], vec![genuine]).await;
    assert_eq!(links_from(&p, "ingest://rm"), vec![]);
}

/// #1146 PR 2: removals are applied before additions, so one diff that
/// removes Alice's link and re-adds it with her signature under another key
/// starts from an empty reifier and writes the garbage key. The tombstone
/// and anti-resurrection rule in PR 2 must keep Alice's key.
#[tokio::test]
#[ignore = "#1146 PR 2: remove + replay in one diff resets the proof unit"]
async fn ingest_remove_then_replay_in_one_diff_keeps_the_stored_key() {
    let p = perspective().await;
    let alice = TestSigner::generate();
    let genuine = signed(&alice, "ingest://rr", "ingest://t");
    ingest(&p, vec![genuine.clone()], vec![]).await;

    let mut replay = genuine.clone();
    replay.proof.key = "not-alices-key".to_string();
    ingest(&p, vec![replay], vec![genuine.clone()]).await;
    assert_row_is(&only_link(links_from(&p, "ingest://rr")), &genuine);
}

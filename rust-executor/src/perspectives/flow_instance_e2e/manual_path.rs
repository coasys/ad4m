use super::*;
// ---------------------------------------------------------------------------
// The manual path: two humans, one button
// ---------------------------------------------------------------------------
//
// The dedup key `(evidence_hash, instance, to_state)` carries no proposer, so
// the second agent to press a button matches the first agent's proposal. These
// pin what that must mean: join it, do not skip it. Before the fix the second
// agent minted nothing, voted on nothing, and was told the empty vec the API
// documents as "queued for other voters", so `{n: 2}` was unreachable for two
// humans pressing one button.

/// Every proposal on the graph targeting `to_state`, live or settled.
async fn proposals_to(f: &Fixture, to_state: &str) -> Vec<String> {
    let mut uris: Vec<String> = f
        .perspective
        .get_links(&LinkQuery {
            predicate: Some(TO_STATE_PREDICATE.to_string()),
            target: Some(literal(to_state)),
            ..Default::default()
        })
        .await
        .expect("get_links")
        .into_iter()
        .map(|l| l.data.source)
        .collect();
    uris.sort();
    uris.dedup();
    uris
}

async fn accepted_by_count(f: &Fixture, proposal: &str) -> usize {
    f.links_by_predicate(proposal)
        .await
        .get(ACCEPTED_BY_PREDICATE)
        .map_or(0, Vec::len)
}

/// Two distinct DIDs reach for the propose API on ONE edge, and nobody calls
/// `accept`. Bob proposed on his replica and it synced in carrying his vote;
/// this replica's agent presses the same button. Same graph ⇒ same seal ⇒ the
/// whole dedup key matches, so this is exactly the collision that used to
/// discard the second vote in silence.
#[tokio::test(flavor = "multi_thread")]
async fn two_dids_proposing_one_edge_reach_quorum_without_anybody_accepting() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let bobs = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "Bob's own vote alone is 1 < n = 2"
    );

    propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect("propose must land this replica's vote on the edge");

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "scoped",
        "two distinct DIDs on one edge IS quorum at n = 2"
    );
    assert_eq!(derived.settled.len(), 1);
    let mut expected = vec![acting_did(&f), bob.did.clone()];
    expected.sort();
    assert_eq!(
        derived.settled[0].voters, expected,
        "both agents' votes are counted"
    );
    assert_eq!(
        derived.settled[0].atom_uris,
        vec![bobs.clone()],
        "one proposal, co-signed — not a second, unreachable twin"
    );
    assert_eq!(
        proposals_to(&f, "scoped").await,
        vec![bobs.clone()],
        "no duplicate proposal was written"
    );
    assert_eq!(
        accepted_by_count(&f, &bobs).await,
        1,
        "exactly one co-sign link, by this replica"
    );
}

/// The same agent pressing the button twice stays a no-op. This is the other
/// half of the fix: joining an existing proposal must be conditional on not
/// having voted on it, or a re-press would append a redundant `acceptedBy`
/// link to the agent's own proposal. It also must not become a second vote —
/// `atom::valid_votes` dedups per DID, and this pins that from the outside.
#[tokio::test(flavor = "multi_thread")]
async fn a_second_propose_by_the_same_did_writes_nothing_and_is_still_one_vote() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect("first propose mints");
    let after_first = proposals_to(&f, "scoped").await;
    assert_eq!(after_first.len(), 1, "one proposal after the first press");

    propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect("second propose is a no-op, not an error");

    assert_eq!(
        proposals_to(&f, "scoped").await,
        after_first,
        "re-pressing must not mint a twin"
    );
    assert_eq!(
        accepted_by_count(&f, &after_first[0]).await,
        0,
        "the proposer's own vote IS the proposal; re-pressing must add no acceptedBy link"
    );
    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "one DID is 1 of 2 however many times they press"
    );
    assert!(derived.settled.is_empty());
}

/// Two terminal branches out of one state: whichever settles first forecloses
/// the other, which is the shape the fold reports as contention.
fn fork_flow() -> serde_json::Value {
    let guard = serde_json::json!([{ "className": "ns://Task", "count": { "min": 1 } }]);
    serde_json::json!({
        "name": "Fork",
        "namespace": "fork://",
        "states": [
            { "name": "start", "value": 0.0, "requires": guard },
            { "name": "left", "value": 1.0, "requires": guard },
            { "name": "right", "value": 1.0, "requires": guard },
        ],
        "transitions": [
            { "action_name": "GoLeft", "from_state": "start", "to_state": "left", "actions": [] },
            { "action_name": "GoRight", "from_state": "start", "to_state": "right", "actions": [] },
        ],
    })
}

/// A contested instance refuses a manual proposal, as the engine pass already
/// refuses to mint into one (#998). Two edges out of `start` carry quorum, so
/// the fold can never settle a third; minting would have handed the caller an
/// empty result indistinguishable from "queued, waiting for other voters".
#[tokio::test(flavor = "multi_thread")]
async fn proposing_into_a_contested_instance_is_refused() {
    let mut f = seed_flow(fork_flow(), "start").await;
    f.seed_task(TASK, "Fork the road").await;
    let instance = f.instance_uri.clone();

    propose(&mut f, "left-1", "start", "left").await;
    propose(&mut f, "right-1", "start", "right").await;
    consensus_pass(&mut f).await;

    let derived = f.derived().await;
    assert!(
        derived.contested.is_some(),
        "fixture must actually be contested, else this test proves nothing: {derived:?}"
    );
    assert_eq!(derived.state, "start", "the walk stopped without choosing");

    let err = propose_flow_transition(&mut f.perspective, &instance, "left", &[], None, &f.ctx)
        .await
        .expect_err("a contested instance must refuse a new proposal");
    assert!(
        format!("{err:#}").contains("contested"),
        "the error must name the reason: {err:#}"
    );
}

/// The return shape. `Vec<FireOutcome>` could not tell "your vote landed,
/// waiting for others" from "you had already voted" from "it fired" — all
/// three were the empty vec or indistinguishable from it — and it discarded
/// the proposal URI a co-signer needs. Every branch is exercised here,
/// including the camelCase wire shape the TS `FlowProposeResult` mirrors.
#[tokio::test(flavor = "multi_thread")]
async fn propose_outcome_distinguishes_fired_queued_and_no_op() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    // 1. Queued: the vote landed, the edge is short of quorum.
    let queued =
        propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
            .await
            .expect("mint");
    assert!(queued.minted && queued.recorded_vote);
    assert!(queued.outcomes.is_empty(), "1 of 2 must not fire");
    assert_eq!(queued.derived_state, "identified");
    assert!(!queued.contested);
    assert!(
        proposals_to(&f, "scoped")
            .await
            .contains(&queued.proposal_uri),
        "the URI names the proposal actually written — a co-signer's handle"
    );

    // 2. No-op: same agent, same button, nothing written.
    let repeat =
        propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
            .await
            .expect("re-press");
    assert!(
        !repeat.minted && !repeat.recorded_vote,
        "a re-press is distinguishable from a queued vote: {repeat:?}"
    );
    assert_eq!(
        repeat.proposal_uri, queued.proposal_uri,
        "a no-op still names the live proposal"
    );

    // 3. The wire shape the TS `FlowProposeResult` mirrors. Locked by field
    //    count as well as by name, so a field added here without a matching
    //    TS field fails in Rust rather than silently at a client.
    //    (The join branch needs a second key; it is the next test.)
    let wire = serde_json::to_value(&queued).expect("serialize");
    let obj = wire.as_object().expect("object");
    assert_eq!(obj.len(), 6, "unexpected field count on the wire: {obj:?}");
    for key in [
        "proposalUri",
        "minted",
        "recordedVote",
        "outcomes",
        "derivedState",
        "contested",
    ] {
        assert!(
            obj.contains_key(key),
            "missing `{key}` on the wire: {obj:?}"
        );
    }
}

/// The join branch's return shape: `minted: false` (we did not write it) with
/// `recordedVote: true` (we voted on it) and the fire in `outcomes`. Nothing
/// in the old `Vec<FireOutcome>` could say the first two.
#[tokio::test(flavor = "multi_thread")]
async fn joining_someone_elses_proposal_reports_minted_false_and_a_recorded_vote() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let bobs = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
    consensus_pass(&mut f).await;

    let joined =
        propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
            .await
            .expect("join");
    assert_eq!(joined.proposal_uri, bobs, "we joined Bob's proposal");
    assert!(!joined.minted, "we did not write it");
    assert!(joined.recorded_vote, "we voted on it");
    assert_eq!(joined.outcomes.len(), 1, "quorum met in this call");
    assert_eq!(joined.outcomes[0].to_state, "scoped");
    assert_eq!(joined.derived_state, "scoped");
    assert!(!joined.contested);
}

// ---------------------------------------------------------------------------
// The other side of the proposer-less key: the engine must NOT reach quorum
// ---------------------------------------------------------------------------
//
// `find_live_proposals`' key carries no proposer, and the two tests above rely
// on that: it is what lets a second human co-sign instead of minting an
// unreachable twin. The cost of that choice is that the key is *shared* with
// `run_engine_proposal_pass`, and this is the test that pins what the engine
// owes in exchange.
//
// Nothing automated co-signs. `accept_flow_proposal` — the only production
// writer of `acceptedBy` — has exactly two callers, `api/perspectives_ws.rs`
// and `mcp/tools/flows.rs`, and both are a request arriving from outside. So
// the engine contributes at most ONE vote to an edge however many replicas run
// its pass, and `consensusRule {n: 2}` means two agents, not two machines.
//
// The failure this guards is silent: make the shared key proposer-aware — a
// plausible "fix" for some future twin-mint bug — and N replicas mint N
// proposals carrying N distinct proposer votes. `{n: 2}` is then satisfied by
// two robots agreeing with themselves, nothing errors, and no other test in
// this file goes red, because every one of them drives the *manual* path.

/// One GUARDED edge, the engine pass run three times: twice as this replica
/// and once as a second DID. Exactly one proposal, exactly one vote, and the
/// `{n: 2}` edge does not move.
#[tokio::test(flavor = "multi_thread")]
async fn the_engine_pass_never_reaches_quorum_by_itself_however_many_dids_run_it() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    // `scoped` carries `requires: [ns://Task count.min 1]` and the fixture
    // seeded the Task, so this edge is guarded AND satisfied — the only shape
    // the engine ever mints on.
    let minted = f.run_pass(&[], None).await;
    assert_eq!(
        minted.len(),
        1,
        "the engine mints the first proposal: {minted:?}"
    );

    let rerun = f.run_pass(&[], None).await;
    assert!(
        rerun.is_empty(),
        "the same replica re-running its pass must find its own proposal: {rerun:?}"
    );

    // A second replica's evaluator over the same graph: same guard, same
    // evidence, same seal, so the whole dedup key matches — and a different
    // acting DID, which is exactly what the key deliberately ignores.
    let bob = second_agent("engine-replica-bob@example.com");
    let bobs_did = crate::agent::did_for_context(&bob).expect("did_for_context(bob)");
    assert_ne!(
        bobs_did,
        acting_did(&f),
        "the fixture must really be two DIDs"
    );
    let bobs = f.run_pass_as(&bob).await;
    assert!(
        bobs.is_empty(),
        "a second DID's engine pass must find the live proposal, not mint its own: {bobs:?}"
    );

    assert_eq!(
        proposals_to(&f, "scoped").await,
        minted,
        "three passes, one proposal"
    );
    assert_eq!(
        accepted_by_count(&f, &minted[0]).await,
        0,
        "nothing automated co-signs — `accept_flow_proposal` is reached only from a client"
    );

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "one engine vote is 1 of 2; `{{n: 2}}` must mean two agents, not two machines"
    );
    assert!(
        derived.settled.is_empty(),
        "no edge settled: {:?}",
        derived.settled
    );
}

/// **The real `get_links` seam, not a stub of it.**
///
/// Every other test that touches role grants asserts on a *derived state*, and
/// the unit suite in `flow_instance/roles.rs` hands `resolve` grant links that
/// it constructed itself — links the production path could not have fetched.
/// So when `didProperty` resolution was broken (a property **name** sent to
/// `get_links`, which wants an RDF **predicate**), a fully green suite said
/// nothing: the seam that was broken was precisely the seam the tests stubbed.
/// Since #1027 that meant every `didProperty` role grant was dated from the
/// instance's own timestamp instead of the assignment link — a wider
/// eligibility window than any rule asked for.
///
/// This test walks the production path and pins the contract at the store
/// boundary itself, so the next spelling drift is a red test rather than a
/// silently widened window:
///
/// 1. the property **name** finds the assignment link through the class shape;
/// 2. the predicate spelling finds the same link (a hand-written SDNA may use
///    either, and a role rule must not gate differently depending on which);
/// 3. a name the class does not declare is an `Err`, never an empty predicate;
/// 4. the window `resolve` recomputes is dated from the **assignment**, and is
///    strictly later than the fallback it used to silently take.
///
/// Fails on `8bb33678d~1` at assertion 1: `grant_links` comes back empty.
#[tokio::test(flavor = "multi_thread")]
async fn a_did_property_grant_link_travels_through_the_real_store() {
    use crate::perspectives::flow_evaluator::{requires_query_input, RequiresQueryable};
    use crate::perspectives::flow_instance::roles::resolve_role_grants;
    use crate::perspectives::flow_instance::time::parse_link_timestamp;
    use crate::perspectives::shacl_parser::ModelQuery;

    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_RULE).await;
    // The instance's own links are written first; the assignment comes after,
    // so the two datings are distinguishable and the fallback is the earlier.
    tick().await;
    grant_owner_role(&mut f).await;
    let me = acting_did(&f);

    // 1. The store boundary: `owner` is the SDNA property NAME; the graph
    //    holds `ns://owner`. Before the fix this vector was empty.
    let by_name = f
        .perspective
        .role_grant_links("ns://Task", TASK, Some("owner"), &me)
        .await
        .expect("role_grant_links by property name");
    assert_eq!(
        by_name.grant_links.len(),
        1,
        "the assignment link must be reachable by the didProperty NAME the SDNA declares, \
         not only by the predicate the graph stores: {:?}",
        by_name.grant_links
    );
    assert_eq!(
        by_name.grant_links[0].data.predicate.as_deref(),
        Some("ns://owner"),
        "and the link found must be the assignment itself"
    );

    // 2. Either spelling, one answer.
    let by_predicate = f
        .perspective
        .role_grant_links("ns://Task", TASK, Some("ns://owner"), &me)
        .await
        .expect("role_grant_links by predicate");
    assert_eq!(
        by_predicate.grant_links, by_name.grant_links,
        "name and predicate spellings must resolve to the same links"
    );

    // 3. Unresolvable fails closed rather than degrading to an empty
    //    predicate — which is what "granted since forever" looked like.
    let err = f
        .perspective
        .role_grant_links("ns://Task", TASK, Some("noSuchProperty"), &me)
        .await
        .expect_err("a didProperty the class does not declare must be an Err");
    assert!(
        format!("{err:#}").contains("noSuchProperty"),
        "the error must name the property that could not be resolved: {err:#}"
    );

    // 4. End to end: the evidence that travels in a receipt carries the
    //    assignment, and the window is dated from it.
    let role: ModelQuery =
        serde_json::from_str(r#"{"className":"ns://Task","didProperty":"owner"}"#)
            .expect("role query");
    let record = f.instances().await.remove(0);
    let evidence = resolve_role_grants(
        &f.perspective,
        "delivery://Delivery.scoped",
        &role,
        &record,
        std::slice::from_ref(&me),
    )
    .await
    .expect("resolve_role_grants");

    let instance = evidence[0]
        .instances
        .iter()
        .find(|i| i.instance_id == TASK)
        .expect("the owned task is a matched role instance");
    assert_eq!(
        instance.grant_links.len(),
        1,
        "the receipt must carry the assignment link, not just the instance's word: {:?}",
        instance.grant_links
    );

    let translated = requires_query_input(&role, &record, &me).expect("role query translates");
    let grant = evidence[0].resolve(&translated).expect("resolve");
    let window = grant
        .windows
        .iter()
        .find(|w| w.instance_id == TASK)
        .expect("a window for the owned task");
    assert_eq!(
        window.granted_at, instance.grant_links[0].timestamp,
        "granted_at is the assignment link's own timestamp"
    );

    let fallback = instance
        .asserted_instance_timestamp
        .clone()
        .expect("the instance is datable, so the fallback exists and is the wrong answer");
    assert!(
        parse_link_timestamp(&window.granted_at) > parse_link_timestamp(&fallback),
        "the assignment must date the grant STRICTLY LATER than the instance fallback \
         ({} vs {}) — taking the fallback is what widened every didProperty window",
        window.granted_at,
        fallback
    );
}

/// #1111 + #1112: everything `role_grant_links` reads through raw `get_links`
/// is reachable through `model_query`, on the real store with real signatures.
///
/// `role_grant_links` exists because the class layer could see neither half of
/// a role's history: the assignment link's *own* timestamp (only the
/// instance's `createdAt` was exposed) and the revocation tombstone (a
/// predicate the class does not declare, so never fetched). With `links` both
/// come back as the stored `LinkExpression`s, and the same collection-side
/// predicates the evaluator applies select the same links. That is the
/// precondition for #1103 replacing the raw read; the replacement itself is
/// out of scope here.
///
/// Fails on `dev`: `__links` is absent — the `links` option does not exist.
#[tokio::test(flavor = "multi_thread")]
async fn role_grant_evidence_is_reachable_through_model_query() {
    use crate::perspectives::flow_evaluator::{
        did_literal_url, grant_link_names_did, revocation_link_counts_for_did, RequiresQueryable,
    };

    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_RULE).await;
    tick().await;
    grant_owner_role(&mut f).await;
    tick().await;
    revoke_own_role(&mut f, TASK).await;
    let me = acting_did(&f);
    let me_literal = did_literal_url(&me).expect("literal");

    let raw = f
        .perspective
        .role_grant_links("ns://Task", TASK, Some("owner"), &me)
        .await
        .expect("role_grant_links");
    assert_eq!(raw.grant_links.len(), 1, "fixture: one assignment");
    assert_eq!(raw.revocation_links.len(), 1, "fixture: one tombstone");

    let query = serde_json::json!({ "links": ["owner", ROLE_GRANT_REVOKED_PREDICATE] });
    let result: serde_json::Value = serde_json::from_str(
        &RequiresQueryable::model_query(&f.perspective, "ns://Task", &query.to_string())
            .await
            .expect("model_query with links"),
    )
    .expect("result json");
    let task = result["instances"]
        .as_array()
        .and_then(|a| a.iter().find(|i| i["id"] == serde_json::json!(TASK)))
        .unwrap_or_else(|| panic!("TASK among the instances: {result}"))
        .clone();
    let carried = |key: &str| -> Vec<LinkExpression> {
        task["__links"][key]
            .as_array()
            .unwrap_or_else(|| panic!("`{key}` rows through model_query: {task}"))
            .iter()
            .map(|r| serde_json::from_value(r.clone()).expect("row is a LinkExpression"))
            .collect()
    };

    let grants: Vec<LinkExpression> = carried("owner")
        .into_iter()
        .filter(|l| grant_link_names_did(l, &me, &me_literal))
        .collect();
    let revocations: Vec<LinkExpression> = carried(ROLE_GRANT_REVOKED_PREDICATE)
        .into_iter()
        .filter(|l| revocation_link_counts_for_did(l, &me, &me_literal))
        .collect();
    assert_eq!(grants, raw.grant_links, "the assignment, byte for byte");
    assert_eq!(
        revocations, raw.revocation_links,
        "the tombstone, with a signature that still verifies"
    );

    // #1112: the assignment is dated by its own link, later than the instance.
    let created_at = task["createdAt"].as_str().expect("createdAt");
    assert!(
        grants[0].timestamp.as_str() > created_at,
        "grant at {} must postdate the instance's createdAt {created_at}",
        grants[0].timestamp
    );
}

// ---------------------------------------------------------------------------
// The manual path: two candidates on one dedup key, foreign one first
// ---------------------------------------------------------------------------

/// Two guard-identical edges into ONE state. The dedup key
/// `(evidence_hash, instance, to_state)` carries no `from_state`, so a
/// proposal on `elsewhere → merged` shares the whole key of a call proposing
/// `here → merged`. What orders the two in the store is their URIs, which say
/// nothing about which edge either sits on.
///
/// Two separate things are being held apart here, and collapsing either one
/// breaks the test:
///
/// * **`requires` is IDENTICAL on every state.** That is the mechanism: the
///   seal is computed from the target state's guard, so guard-identical edges
///   produce the same `evidence_hash` and therefore the same dedup key. Give
///   the states different guards and the two proposals stop colliding, and
///   the test stops covering anything.
/// * **`value` is DISTINCT on every state.** `value` does not enter the seal
///   — it is only the state ordering. But genesis is `states[0]`
///   (`flow_spawn::initial_state_of`) and the parser's sort by `value` is
///   *stable*, so equal values leave the tie to graph link-discovery order,
///   which `shacl_parser` itself documents as arbitrary. `here` and
///   `elsewhere` both at `0.0` therefore made the folded genesis undefined
///   rather than `here`, and CI folded it to `elsewhere`.
///
/// Note that `seed_flow`'s `initial_state` argument cannot rescue this: it
/// writes the `currentState` **cache**, and the fold never reads the cache —
/// `read_set` takes its genesis from the flow definition alone.
fn merge_flow() -> serde_json::Value {
    let guard = serde_json::json!([{ "className": "ns://Task", "count": { "min": 1 } }]);
    serde_json::json!({
        "name": "Merge",
        "namespace": "merge://",
        "states": [
            // Lowest value, so genesis is `here` — deterministically, which is
            // the whole point of not sharing a value with `elsewhere`.
            { "name": "here", "value": 0.0, "requires": guard },
            { "name": "elsewhere", "value": 0.5, "requires": guard },
            { "name": "merged", "value": 1.0, "requires": guard },
        ],
        "transitions": [
            { "action_name": "FromHere", "from_state": "here", "to_state": "merged", "actions": [] },
            { "action_name": "FromElsewhere", "from_state": "elsewhere", "to_state": "merged", "actions": [] },
        ],
    })
}

/// A joinable proposal sitting BEHIND a foreign one is still the one co-signed.
///
/// This is the ordering the fix exists for, and the only test that pins it:
/// two live proposals share the call's dedup key, the FOREIGN one is first in
/// scan order, and the joinable one is second. A first-match lookup — or a
/// classification loop replaced by `.first()`, or one that `break`s on the
/// first non-joinable candidate — classifies the foreign proposal, never
/// reaches Bob's, and mints. That mint is invariant 4 broken in the one shape
/// it exists to cover: the vote is split across two atoms on one edge, and the
/// next press mints again.
///
/// Every other test on this path has exactly one candidate, so all of them
/// pass on the broken code.
#[tokio::test(flavor = "multi_thread")]
async fn a_joinable_proposal_behind_a_foreign_one_is_still_the_one_co_signed() {
    let mut f = seed_flow(merge_flow(), "here").await;
    f.seed_task(TASK, "Merge the two branches").await;
    set_consensus_rule(&mut f, "merge://Merge.merged", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
    // One seal for `merged`, so both proposals below carry the whole key.
    let seal = seal_for(&f, "merged").await;

    // Scan order is `find_live_proposals`' sort, which is by URI — NOT the
    // write order, and not the store's own iteration order, which is
    // arbitrary. URIs are content addresses now (#1108), so the sort is hash
    // order over per-run random keys — no fixed nonce can pin it. The nonce
    // search below is what puts the foreign proposal first: `proposal_uri`
    // is pure, so Carol's nonce is picked until her URI sorts below Bob's.
    let carol = TestSigner::generate();
    let bob = TestSigner::generate();
    let empty = outputs_hash(&[]);
    let uri_for = |signer: &TestSigner, from: &str, nonce: &str| {
        crate::perspectives::flow_instance::atom::proposal_uri(
            &instance,
            from,
            "merged",
            &seal,
            Some(&empty),
            &signer.did,
            nonce,
        )
    };
    let joinable_uri = uri_for(&bob, "here", "joinable-1");
    let foreign_nonce = (0..)
        .map(|i| format!("foreign-{i}"))
        .find(|nonce| uri_for(&carol, "elsewhere", nonce) < joinable_uri)
        .expect("some nonce hashes below Bob's URI");
    let foreign =
        sync_proposal_from(&mut f, &carol, &foreign_nonce, "elsewhere", "merged", &seal).await;
    let joinable = sync_proposal_from(&mut f, &bob, "joinable-1", "here", "merged", &seal).await;

    assert!(
        foreign < joinable,
        "this test only exercises the ordering bug while the FOREIGN proposal is scanned \
         first, and scan order is the URI sort; rename the two above until it is — do not \
         drop this assertion, without it the test can pass vacuously"
    );
    assert_eq!(
        f.derived().await.state,
        "here",
        "one vote each at n = 2 settles nothing, so the instance is still in genesis — and \
         genesis must be `here`, because that is the edge the call below is on. If this \
         reads `elsewhere`, `merge_flow`'s state VALUES have been collapsed back together \
         and genesis has gone arbitrary; fix the values, do NOT flip this expectation — \
         with genesis `elsewhere` the joinable proposal sorts FIRST and the ordering bug \
         is no longer exercised at all"
    );

    let out = propose_flow_transition(&mut f.perspective, &instance, "merged", &[], None, &f.ctx)
        .await
        .expect("propose must reach past the foreign candidate");

    assert_eq!(
        out.proposal_uri, joinable,
        "the proposal on OUR edge is the one co-signed, not the one leaving `elsewhere`"
    );
    assert!(
        !out.minted,
        "minting past a joinable proposal splits the vote and mints again on the next \
         press: {out:?}"
    );
    assert!(out.recorded_vote, "our vote landed on Bob's proposal");
    assert_eq!(
        out.outcomes.len(),
        1,
        "two DIDs on one edge IS quorum at n = 2"
    );
    assert_eq!(out.outcomes[0].to_state, "merged");

    let mut both = vec![foreign.clone(), joinable.clone()];
    both.sort();
    assert_eq!(
        proposals_to(&f, "merged").await,
        both,
        "no third proposal was written"
    );
    assert_eq!(
        accepted_by_count(&f, &joinable).await,
        1,
        "exactly one co-sign, on the joinable proposal"
    );
    assert_eq!(
        accepted_by_count(&f, &foreign).await,
        0,
        "and none on the foreign one — signing it would vote on an edge we are not on"
    );

    let derived = f.derived().await;
    assert_eq!(derived.state, "merged", "the edge settled");
    assert_eq!(derived.settled.len(), 1);
    assert_eq!(
        derived.settled[0].atom_uris,
        vec![joinable],
        "settled by the co-signed proposal alone"
    );
    let mut expected = vec![acting_did(&f), bob.did.clone()];
    expected.sort();
    assert_eq!(
        derived.settled[0].voters, expected,
        "both DIDs on the `here → merged` edge are counted"
    );
}

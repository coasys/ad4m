use super::*;
use crate::perspectives::flow_instance::replica_state::FlowStateChangedEvent;
use crate::perspectives::perspective_instance::SubjectClassOption;
use crate::pubsub::{get_global_pubsub, FLOW_STATE_CHANGED_TOPIC};

// ---------------------------------------------------------------------------
// The cache is a cache
// ---------------------------------------------------------------------------

/// Test 9. A wrong recorded state moves no engine decision: the fold starts at
/// genesis, the evaluator still mints from genesis, and the pass heals the
/// record. The wrong value here is terminal, which before the fold
/// suppressed every mint on this instance.
#[tokio::test(flavor = "multi_thread")]
async fn a_forged_current_state_moves_nothing() {
    let mut f = seed_satisfied_fixture(None).await;
    forge_cached_state(&mut f, "scoped").await;

    assert_eq!(
        f.derived().await.state,
        "identified",
        "the fold starts at genesis"
    );
    assert_eq!(f.cached_state().await, "scoped", "the forgery is recorded");

    let minted = f.mint_one().await;
    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(outcomes.len(), 1, "got {outcomes:?}");
    assert_eq!(
        (
            outcomes[0].from_state.as_str(),
            outcomes[0].to_state.as_str()
        ),
        ("identified", "scoped"),
        "the mint left the DERIVED state, not the forged one"
    );
    assert_eq!(outcomes[0].contributing_proposal_uris, vec![minted]);
    assert_eq!(f.derived().await.state, "scoped");
}

/// The pass's whole job, and its idempotency: it records the state and the
/// marks, and a second run has nothing left to record.
#[tokio::test(flavor = "multi_thread")]
async fn the_pass_writes_the_cache_and_the_marks_and_then_has_nothing_to_do() {
    let mut f = seed_satisfied_fixture(None).await;
    let minted = f.mint_one().await;

    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "the default {{n: 1}} rule settles at mint"
    );
    assert_eq!(outcomes[0].voters, vec![acting_did(&f)]);
    assert_eq!(f.cached_state().await, "scoped", "the state was recorded");
    assert!(
        f.marked().await.contains(&minted),
        "and the settling proposal was marked"
    );
    assert_eq!(f.derived().await.state, "scoped");

    let rerun = consensus_pass(&mut f).await;
    assert!(
        rerun.is_empty(),
        "re-run recorded something twice: {rerun:?}"
    );
    assert!(
        proposal_exists(&f, &minted).await,
        "the pass deletes nothing, ever"
    );
}

// ---------------------------------------------------------------------------
// The bookkeeping is this agent's own, and not in the graph (#1424)
// ---------------------------------------------------------------------------

/// Nothing the pass records is a link: the mint's initial state, the healed
/// state and the fired marks all land in the executor's tables. Before #1424
/// they were `Local` links, readable by every agent on the executor through any
/// link query; this is the test that fails on that executor.
#[tokio::test(flavor = "multi_thread")]
async fn the_pass_writes_no_links() {
    let mut f = seed_satisfied_fixture(None).await;
    assert_eq!(
        f.cached_state().await,
        "identified",
        "the mint recorded its genesis"
    );
    let minted = f.mint_one().await;
    assert_eq!(consensus_pass(&mut f).await.len(), 1);
    assert_eq!(f.cached_state().await, "scoped");
    assert!(f.marked().await.contains(&minted));

    let links = bookkeeping_links(&f).await;
    assert!(
        links.is_empty(),
        "no cache or mark link of any status may exist: {links:?}"
    );
    assert!(
        links_of(&f, &f.instance_uri)
            .await
            .iter()
            .all(|l| l.status != Some(LinkStatus::Local)),
        "the flow instance carries no Local link at all"
    );
}

/// A second agent on the same executor — a co-owner — has no view of the first
/// agent's bookkeeping: not through the graph (there is nothing there, see
/// `the_pass_writes_no_links`), and not through the table, which is keyed by
/// agent. Its own pass keeps its own rows.
#[tokio::test(flavor = "multi_thread")]
async fn another_agent_on_the_node_sees_none_of_this_agents_bookkeeping() {
    let mut f = seed_satisfied_fixture(None).await;
    let minted = f.mint_one().await;
    assert_eq!(consensus_pass(&mut f).await.len(), 1);

    let bob = second_agent("bob-bookkeeping@flow.local");
    let bob_did = crate::agent::did_for_context(&bob).expect("bob's did");
    assert_ne!(bob_did, acting_did(&f));
    let uuid = f.perspective.uuid.clone();
    assert!(FlowReplicaState::states(&uuid, &bob_did, None)
        .expect("states")
        .is_empty());
    assert!(!FlowReplicaState::is_fired(&uuid, &bob_did, &minted).expect("is_fired"));

    // Bob's own first pass is his catch-up: silent, and it fills his rows.
    let bobs = run_flow_consensus_pass(&mut f.perspective, None, &bob, None, None).await;
    assert!(bobs.is_empty(), "bob's first pass catches up: {bobs:?}");
    assert_eq!(
        FlowReplicaState::current_state(&uuid, &bob_did, &f.instance_uri)
            .expect("bob's state")
            .as_deref(),
        Some("scoped")
    );
    assert!(FlowReplicaState::is_fired(&uuid, &bob_did, &minted).expect("is_fired"));
}

/// A second agent whose FIRST contact with an instance is its own vote is
/// told what that vote settled. Its pass after the vote would otherwise be
/// its first — a silent catch-up — so `accept_flow_proposal` catches it up
/// before writing the vote (`catch_up_before_own_vote`). History it never
/// voted on stays unreported.
#[tokio::test(flavor = "multi_thread")]
async fn a_second_agents_first_vote_reports_what_it_settles() {
    let mut f = seed_review_flow().await;
    // History Bob was not around for: review → changes_requested → review.
    settle(&mut f, "h1", "review", "changes_requested").await;
    settle(&mut f, "h2", "changes_requested", "review").await;
    set_consensus_rule(&mut f, "review://Review.approved", r#"{"n":2}"#).await;
    let open = propose(&mut f, "h3", "review", "approved").await;
    assert!(consensus_pass(&mut f).await.is_empty(), "1 < n = 2");

    let bob = second_agent("bob-first-vote@flow.local");
    let fired = accept_flow_proposal(&mut f.perspective, &open, &bob)
        .await
        .expect("bob co-signs");
    assert_eq!(
        fired
            .iter()
            .map(|o| (o.from_state.as_str(), o.to_state.as_str()))
            .collect::<Vec<_>>(),
        vec![("review", "approved")],
        "bob is told what his vote settled, and nothing from before he arrived"
    );
}

/// Links the `Local`-link engine left behind are dead data: a `Local`
/// `currentState` saying something else and a `Local` mark on the very
/// proposal that settles. Neither is read — the mark does not mute this
/// agent's event, the cache does not show through the loader — and neither
/// is deleted.
#[tokio::test(flavor = "multi_thread")]
async fn pre_1424_local_bookkeeping_links_are_dead_data() {
    let mut f = seed_satisfied_fixture(None).await;
    let instance = f.instance_uri.clone();
    f.link(
        &instance,
        LEGACY_FLOW_CURRENT_STATE_PREDICATE,
        &literal("scoped"),
        LinkStatus::Local,
    )
    .await;
    let minted = f.mint_one().await;
    f.link(
        &minted,
        LEGACY_RESOLVED_AS_PREDICATE,
        &literal("fired"),
        LinkStatus::Local,
    )
    .await;

    assert_eq!(
        f.instances().await[0].current_state,
        "",
        "the loader shows no state from a leftover link"
    );
    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "a leftover Local mark does not mute the event: {outcomes:?}"
    );
    assert_eq!(f.cached_state().await, "scoped");
    assert_eq!(
        bookkeeping_links(&f).await.len(),
        2,
        "the pass deletes nothing, leftovers included"
    );
}

/// A peer's legacy mark link cannot mute this replica's once-only
/// `FireOutcome`: only this agent's own table says "I already recorded
/// this".
#[tokio::test(flavor = "multi_thread")]
async fn a_peer_written_shared_mark_does_not_mute_our_fire_outcome() {
    let mut f = seed_satisfied_fixture(None).await;
    let minted = f.mint_one().await;
    let bob = TestSigner::generate();
    sync_fired_mark_from(&mut f, &bob, &minted).await;

    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "the peer's mark is not this replica's: {outcomes:?}"
    );
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "our own mark, once written, is"
    );
}

/// The flow instance every OTHER replica sees: no row for it in this agent's
/// table. It loads as an instance with the empty state meaning "not read
/// yet", and the pass then records it.
#[tokio::test(flavor = "multi_thread")]
async fn an_instance_without_a_record_still_loads_and_the_pass_fills_it() {
    let mut f = seed_satisfied_fixture(None).await;
    forget_bookkeeping(&f);

    let records = f.instances().await;
    assert_eq!(records.len(), 1, "the flow instance is recognised");
    assert_eq!(records[0].current_state, "", "the row carries no state");
    assert_eq!(f.cached_state().await, "", "and nothing is recorded");

    consensus_pass(&mut f).await;
    assert_eq!(
        f.cached_state().await,
        "identified",
        "the pass records the fold's answer"
    );
}

/// Deliver links the way the link language delivers them: through
/// `diff_from_link_language`, which is where the sync trigger hangs.
async fn sync_in(f: &Fixture, links: Vec<LinkExpression>) {
    f.perspective
        .diff_from_link_language(PerspectiveDiff::from_additions(links))
        .await
        .expect("diff_from_link_language");
}

/// Poll the recorded state until it reads `state` or the budget runs out;
/// `true` when it got there. The sync-triggered pass is debounced and runs
/// on a spawned task, so a test cannot await it directly.
async fn cache_reaches(f: &Fixture, state: &str) -> bool {
    for _ in 0..50 {
        if f.cached_state().await == state {
            return true;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
    false
}

/// A peer's vote arriving through sync — not through this replica's own
/// accept or mint — must bring this agent's state and marks up to date by
/// itself: they are this replica's own, so nobody else can. No pass is
/// called here; the one `diff_from_link_language` queues does the work.
#[tokio::test(flavor = "multi_thread")]
async fn a_synced_vote_triggers_this_replicas_own_pass() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let minted = f.mint_one().await;
    assert_eq!(f.cached_state().await, "identified", "1 < n = 2 at mint");

    let bob = TestSigner::generate();
    let vote = bob.sign(
        Link {
            source: minted.clone(),
            predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
            target: bob.did.clone(),
        }
        .normalize(),
    );
    sync_in(&f, vec![LinkExpression::from(vote)]).await;

    assert!(
        cache_reaches(&f, "scoped").await,
        "the synced vote must trigger the pass that heals the state"
    );
    assert!(
        f.marked().await.contains(&minted),
        "and that pass marks the settling proposal"
    );
    assert!(bookkeeping_links(&f).await.is_empty(), "and writes no link");
}

/// A peer's change to the flow's definition re-derives every instance: the
/// fold reads the rules the reader holds now, so the recorded state must
/// follow when they change. Raising Scoped's rule to `{n: 2}` after one vote
/// settled it derives Identified again.
#[tokio::test(flavor = "multi_thread")]
async fn a_synced_rule_change_rederives_the_cache() {
    let mut f = seed_satisfied_fixture(None).await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(
        f.cached_state().await,
        "scoped",
        "the default {{n: 1}} settles on the mint's vote"
    );

    let peer = TestSigner::generate();
    let rule = peer.sign(
        Link {
            source: "delivery://Delivery.scoped".to_string(),
            predicate: Some("ad4m://consensusRule".to_string()),
            target: literal(r#"{"n":2}"#),
        }
        .normalize(),
    );
    sync_in(&f, vec![LinkExpression::from(rule)]).await;

    assert!(
        cache_reaches(&f, "identified").await,
        "the synced rule change must trigger the pass that re-derives the state"
    );
    assert_eq!(f.derived().await.state, "identified");
}

/// The same change written on this replica. The author's own node has no
/// vote or mint to run the pass after, so the local write queues it itself.
#[tokio::test(flavor = "multi_thread")]
async fn a_local_rule_change_rederives_the_cache() {
    let mut f = seed_satisfied_fixture(None).await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(f.cached_state().await, "scoped");

    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    assert!(
        cache_reaches(&f, "identified").await,
        "a local rule change must re-derive this agent's own state"
    );
}

/// A rule changed through `update_link` re-derives too. An update publishes its
/// own topic rather than going through `pubsub_publish_diff`, so it needs the
/// definition hook of its own.
#[tokio::test(flavor = "multi_thread")]
async fn a_rule_updated_in_place_rederives_the_cache() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":1}"#).await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(f.cached_state().await, "scoped");

    let rule = links_of(&f, "delivery://Delivery.scoped")
        .await
        .into_iter()
        .find(|l| l.data.predicate.as_deref() == Some("ad4m://consensusRule"))
        .expect("the rule just written");
    let ctx = f.ctx.clone();
    f.perspective
        .update_link(
            LinkExpression::from(rule),
            Link {
                source: "delivery://Delivery.scoped".to_string(),
                predicate: Some("ad4m://consensusRule".to_string()),
                target: literal(r#"{"n":2}"#),
            },
            None,
            &ctx,
        )
        .await
        .expect("update_link");

    assert!(
        cache_reaches(&f, "identified").await,
        "a rule updated in place must re-derive the state"
    );
}

/// A synced chat message queues nothing: the trigger is keyed on the flow
/// vocabulary, so ordinary traffic never re-derives a flow. Pinned by the
/// recorded state staying put where a pass would have healed it.
#[tokio::test(flavor = "multi_thread")]
async fn a_synced_chat_message_does_not_trigger_a_pass() {
    let mut f = seed_satisfied_fixture(None).await;
    forge_cached_state(&mut f, "scoped").await;
    let bob = TestSigner::generate();
    let chat = bob.sign(
        Link {
            source: "flux://message/1".to_string(),
            predicate: Some("flux://body".to_string()),
            target: literal("hello"),
        }
        .normalize(),
    );
    sync_in(&f, vec![LinkExpression::from(chat)]).await;
    // Waits out any pass the message queued, which is what a sleep stood in for.
    f.perspective.settle_flow_passes().await;
    assert_eq!(
        f.cached_state().await,
        "scoped",
        "no pass ran: the wrong state was not healed"
    );
}

/// Copy every link of `proposal_uris` from replica `from` into replica
/// `to`, as sync would (Shared, signatures intact), without going through
/// the sync trigger so the test controls when the pass runs.
async fn replicate_proposals(from: &Fixture, to: &mut Fixture, proposal_uris: &[&str]) {
    for uri in proposal_uris {
        for link in links_of(from, uri).await {
            to.perspective
                .add_link_expression(LinkExpression::from(link), LinkStatus::Shared, None)
                .await
                .expect("replicate a proposal link");
        }
    }
}

/// Catch-up is silent. A replica joining a flow with history finds every
/// settled edge unmarked — marks are per replica — and must not report
/// them all as new. Its first pass over the instance marks them and records
/// the state without emitting; the next edge to settle is reported. The
/// creating replica is not a newcomer (its row is seeded at the mint), so
/// its first settle IS reported — pinned by
/// `the_pass_writes_the_cache_and_the_marks_and_then_has_nothing_to_do`.
#[tokio::test(flavor = "multi_thread")]
async fn a_newcomers_first_pass_catches_up_silently_then_reports_normally() {
    // Replica B is set up FIRST: every fixture re-keys the process's one main
    // agent, so whichever is created last is the DID both replicas act as.
    // Creating B after A's settles would make A's own later passes those of
    // an agent A never saw — a catch-up on A as well.
    let mut b = seed_review_flow().await;
    let mut a = seed_review_flow().await;
    let h1 = settle(&mut a, "h1", "review", "changes_requested").await;
    let h2 = settle(&mut a, "h2", "changes_requested", "review").await;

    // B: same definition, the flow instance as sync delivers it (no row),
    // and A's history.
    forget_bookkeeping(&b);
    replicate_proposals(&a, &mut b, &[&h1, &h2]).await;

    let first = consensus_pass(&mut b).await;
    assert!(first.is_empty(), "catch-up is silent, got {first:?}");
    assert_eq!(
        b.cached_state().await,
        "review",
        "but the state is recorded (review → changes_requested → review)"
    );
    let marked = b.marked().await;
    assert!(
        marked.contains(&h1) && marked.contains(&h2),
        "and the settled history is marked: {marked:?}"
    );
    assert!(
        consensus_pass(&mut b).await.is_empty(),
        "nothing left to record after catch-up"
    );

    // An edge that settles after catch-up is an event for B.
    let h3 = settle(&mut a, "h3", "review", "approved").await;
    replicate_proposals(&a, &mut b, &[&h3]).await;
    let later = consensus_pass(&mut b).await;
    assert_eq!(
        later.len(),
        1,
        "an edge settling after catch-up fires here once: {later:?}"
    );
    assert_eq!(
        (later[0].from_state.as_str(), later[0].to_state.as_str()),
        ("review", "approved")
    );
    assert_eq!(b.cached_state().await, "approved");
    assert!(consensus_pass(&mut b).await.is_empty(), "and only once");
}

/// A newcomer with nothing to catch up on: the first pass records the state
/// (silently, trivially) and the FIRST edge to settle afterwards is
/// reported — catch-up must not eat the first real event.
#[tokio::test(flavor = "multi_thread")]
async fn a_newcomer_with_no_history_reports_the_first_settle_after_its_first_pass() {
    let mut f = seed_satisfied_fixture(None).await;
    forget_bookkeeping(&f);
    assert!(consensus_pass(&mut f).await.is_empty());
    assert_eq!(f.cached_state().await, "identified");

    f.mint_one().await;
    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(outcomes.len(), 1, "got {outcomes:?}");
    assert_eq!(f.cached_state().await, "scoped");
}

/// The upgrade from the `Local`-link bookkeeping: an executor that ran the
/// old engine has history marked by `Local` links and empty tables. Its
/// first pass must treat every instance as a catch-up — no outcomes, no
/// flood of settles that were reported before the upgrade — and report what
/// settles after it. The catch-up does publish one `caughtUp` event: it is
/// the state this agent's clients can now show, and their only signal of it.
#[tokio::test(flavor = "multi_thread")]
async fn after_an_upgrade_the_first_pass_is_a_silent_catch_up() {
    let mut f = seed_review_flow().await;
    let h1 = settle(&mut f, "h1", "review", "changes_requested").await;
    let h2 = settle(&mut f, "h2", "changes_requested", "review").await;

    // What the pre-#1424 executor left on disk; the new tables start empty.
    let instance = f.instance_uri.clone();
    f.link(
        &instance,
        LEGACY_FLOW_CURRENT_STATE_PREDICATE,
        &literal("review"),
        LinkStatus::Local,
    )
    .await;
    for h in [&h1, &h2] {
        f.link(
            h,
            LEGACY_RESOLVED_AS_PREDICATE,
            &literal("fired"),
            LinkStatus::Local,
        )
        .await;
    }
    forget_bookkeeping(&f);

    let mut events = get_global_pubsub()
        .await
        .subscribe(&FLOW_STATE_CHANGED_TOPIC)
        .await;
    let first = consensus_pass(&mut f).await;
    assert!(first.is_empty(), "no flood after an upgrade: {first:?}");
    assert_eq!(
        raw_flow_events(&mut events, &f.perspective.uuid),
        vec![serde_json::json!({
            "perspectiveUuid": f.perspective.uuid,
            "agentDid": acting_did(&f),
            "instance": instance,
            "from": null,
            "to": "review",
            "firedProposals": [],
            "caughtUp": true,
        })],
        "one caught-up event: the state, and no settles"
    );
    assert_eq!(f.cached_state().await, "review");

    let h3 = settle(&mut f, "h3", "review", "approved").await;
    let reported = flow_events(&mut events, &f.perspective.uuid);
    assert_eq!(
        reported.len(),
        1,
        "the next settle is reported: {reported:?}"
    );
    assert_eq!(reported[0].fired_proposals, vec![h3]);
}

// ---------------------------------------------------------------------------
// Mint seeding: whoever starts an instance reports its first settle
// ---------------------------------------------------------------------------

/// A client's `FlowInstance.start` writes the row through `createSubject`;
/// no Rust mint runs. The local write still seeds this agent's row, so its
/// first pass is not a catch-up and the first settle is reported — which is
/// what the client's `Local` cache link did before #1424. (That a seeded row
/// reports is `a_minted_row_has_derived_but_no_state` plus the pass's
/// `PriorRow::Minted` arm, exercised end to end by the JS suite.)
#[tokio::test(flavor = "multi_thread")]
async fn a_client_minted_instance_is_seeded_for_its_minter() {
    let mut f = seed_satisfied_fixture(None).await;
    let client_minted = "ad4m://flow/instance/client-minted";
    let flow_uri = f.flow_uri.clone();
    let ctx = f.ctx.clone();
    f.perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some("FlowInstance".to_string()),
                query: None,
            },
            client_minted.to_string(),
            Some(serde_json::json!({
                "flowUri": flow_uri,
                "subject": "ad4m://task/client-minted",
            })),
            None,
            &ctx,
        )
        .await
        .expect("create the FlowInstance as a client would");
    assert!(
        FlowReplicaState::has_derived(&f.perspective.uuid, &acting_did(&f), client_minted)
            .expect("has_derived"),
        "the local write seeded the minting agent's row"
    );
    assert_eq!(
        FlowReplicaState::current_state(&f.perspective.uuid, &acting_did(&f), client_minted)
            .expect("current_state"),
        None,
        "seeded, not derived: there is no state to show yet"
    );
}

/// And a seeded row does what it is for: with no state recorded yet, the
/// minter's first pass is still not a catch-up, so the first settle is
/// reported. The row is seeded the client's way — a local write of the
/// instance's `flow/flow_uri` link — after forgetting the Rust mint's row.
/// Without the seed this is `a_newcomers_first_pass…`: silent.
#[tokio::test(flavor = "multi_thread")]
async fn a_seeded_row_reports_the_first_settle() {
    use crate::perspectives::flow_classes::FLOW_URI_PREDICATE;
    let mut f = seed_satisfied_fixture(None).await;
    forget_bookkeeping(&f);
    let instance = f.instance_uri.clone();
    let identity = links_of(&f, &instance)
        .await
        .into_iter()
        .find(|l| l.data.predicate.as_deref() == Some(FLOW_URI_PREDICATE))
        .expect("the instance's flow/flow_uri link");
    tick().await;
    f.link(
        &instance,
        FLOW_URI_PREDICATE,
        &identity.data.target,
        LinkStatus::Shared,
    )
    .await;
    assert!(
        FlowReplicaState::has_derived(&f.perspective.uuid, &acting_did(&f), &instance)
            .expect("has_derived"),
        "seeded"
    );
    assert_eq!(f.cached_state().await, "", "with no state yet");

    let minted = f.mint_one().await;
    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "a seeded row is not a catch-up: {outcomes:?}"
    );
    assert_eq!(outcomes[0].contributing_proposal_uris, vec![minted]);
    assert_eq!(f.cached_state().await, "scoped");
}

/// The other side of seeding: a `FlowInstance` minted by someone else and
/// delivered by sync is not seeded — this replica did not watch it start.
#[tokio::test(flavor = "multi_thread")]
async fn a_synced_instance_is_not_seeded() {
    let f = seed_satisfied_fixture(None).await;
    let bob = TestSigner::generate();
    let theirs = "ad4m://flow/instance/bobs";
    let row = bob.sign(
        Link {
            source: theirs.to_string(),
            predicate: Some(crate::perspectives::flow_classes::FLOW_URI_PREDICATE.to_string()),
            target: f.flow_uri.clone(),
        }
        .normalize(),
    );
    sync_in(&f, vec![LinkExpression::from(row)]).await;
    f.perspective.settle_flow_passes().await;
    assert!(
        !FlowReplicaState::has_derived(&f.perspective.uuid, &acting_did(&f), theirs)
            .expect("has_derived"),
        "a peer's mint is not seeded here"
    );
    assert!(
        !FlowReplicaState::has_derived(&f.perspective.uuid, &bob.did, theirs).expect("has_derived"),
        "nor for the peer, whose bookkeeping is on its own executor"
    );
}

// ---------------------------------------------------------------------------
// flow-state-changed
// ---------------------------------------------------------------------------

/// [`flow_events`] as the JSON the socket carries.
fn raw_flow_events(
    rx: &mut tokio::sync::broadcast::Receiver<String>,
    uuid: &str,
) -> Vec<serde_json::Value> {
    let mut out = Vec::new();
    while let Ok(msg) = rx.try_recv() {
        let event: serde_json::Value = serde_json::from_str(&msg).expect("JSON");
        if event["perspectiveUuid"] == uuid {
            out.push(event);
        }
    }
    out
}

/// The events on `rx` so far that belong to `uuid`, oldest first.
fn flow_events(
    rx: &mut tokio::sync::broadcast::Receiver<String>,
    uuid: &str,
) -> Vec<FlowStateChangedEvent> {
    let mut out = Vec::new();
    while let Ok(msg) = rx.try_recv() {
        let event: FlowStateChangedEvent =
            serde_json::from_str(&msg).expect("a FlowStateChangedEvent");
        if event.perspective_uuid == uuid {
            out.push(event);
        }
    }
    out
}

/// One event per change, naming the agent, the move and the newly fired
/// proposals; nothing when nothing changed.
#[tokio::test(flavor = "multi_thread")]
async fn a_settle_publishes_one_flow_state_changed_event() {
    let mut f = seed_satisfied_fixture(None).await;
    let mut events = get_global_pubsub()
        .await
        .subscribe(&FLOW_STATE_CHANGED_TOPIC)
        .await;
    let minted = f.mint_one().await;
    assert_eq!(consensus_pass(&mut f).await.len(), 1);

    let got = flow_events(&mut events, &f.perspective.uuid);
    assert_eq!(
        got,
        vec![FlowStateChangedEvent {
            perspective_uuid: f.perspective.uuid.clone(),
            agent_did: acting_did(&f),
            instance: f.instance_uri.clone(),
            from: Some("identified".into()),
            to: "scoped".into(),
            fired_proposals: vec![minted],
            caught_up: false,
        }]
    );

    assert!(consensus_pass(&mut f).await.is_empty());
    assert!(
        flow_events(&mut events, &f.perspective.uuid).is_empty(),
        "an idle pass publishes nothing"
    );
}

/// A move without a settle — a withdrawn vote takes the flow back — is a
/// change too, with no fired proposals.
#[tokio::test(flavor = "multi_thread")]
async fn a_regression_publishes_an_event_without_fired_proposals() {
    let mut f = seed_satisfied_fixture(None).await;
    let minted = f.mint_one().await;
    assert_eq!(consensus_pass(&mut f).await.len(), 1);
    let mut events = get_global_pubsub()
        .await
        .subscribe(&FLOW_STATE_CHANGED_TOPIC)
        .await;

    let ctx = f.ctx.clone();
    reject_flow_proposal(&mut f.perspective, &minted, &ctx)
        .await
        .expect("withdraw our own proposal");
    consensus_pass(&mut f).await;

    let got = flow_events(&mut events, &f.perspective.uuid);
    assert_eq!(got.len(), 1, "got {got:?}");
    assert_eq!(
        (got[0].from.as_deref(), got[0].to.as_str()),
        (Some("scoped"), "identified")
    );
    assert!(got[0].fired_proposals.is_empty());
}

/// A seeded row whose fold is still at genesis: the pass writes the genesis
/// as the recorded state and publishes nothing. `from` — a seeded row's
/// flow-first state — equals `to`, no edge settled, and the row exists, so
/// it is no catch-up either. The client already shows the first state for
/// a seeded row; there is no news (Marvin, #1425).
#[tokio::test(flavor = "multi_thread")]
async fn a_seeded_row_at_genesis_records_the_genesis_and_publishes_nothing() {
    let mut f = seed_satisfied_fixture(None).await;
    f.perspective.settle_flow_passes().await;
    forget_bookkeeping(&f);
    let did = acting_did(&f);
    assert!(FlowReplicaState::seed_minted(
        &f.perspective.uuid,
        &did,
        &f.instance_uri,
        &f.perspective.is_teardown,
    )
    .expect("seed"));
    assert_eq!(
        f.derived().await.state,
        "identified",
        "the fold is at genesis"
    );

    let mut events = get_global_pubsub()
        .await
        .subscribe(&FLOW_STATE_CHANGED_TOPIC)
        .await;
    let outcomes = consensus_pass(&mut f).await;
    assert!(outcomes.is_empty(), "nothing settled: {outcomes:?}");
    assert_eq!(
        f.cached_state().await,
        "identified",
        "the genesis is written over the seeded row"
    );
    assert!(f.marked().await.is_empty(), "and no mark");
    assert_eq!(
        raw_flow_events(&mut events, &f.perspective.uuid),
        Vec::<serde_json::Value>::new(),
        "no event: from == to, no fired proposals, no catch-up"
    );
}

// ---------------------------------------------------------------------------
// The fired mark is an index
// ---------------------------------------------------------------------------

/// Test 10. The mark is bookkeeping, so it cannot move the state in either
/// direction: marking an unquorate proposal does not fabricate history, and
/// the absence of a mark does not hide a quorate one from the fold.
#[tokio::test(flavor = "multi_thread")]
async fn a_forged_fired_mark_moves_nothing() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let unquorate = propose(&mut f, "unquorate", "identified", "scoped").await;
    forge_fired_mark(&mut f, &unquorate).await;

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "1 < n = 2 is not a consensus event"
    );
    assert!(derived.settled.is_empty());

    // The other direction: quorum with nobody having marked anything.
    let bob = TestSigner::generate();
    sync_vote_from(&mut f, &bob, &unquorate).await;
    let derived = f.derived().await;
    assert_eq!(
        derived.state, "scoped",
        "an unmarked but quorate edge is history the moment its votes exist"
    );
    assert_eq!(derived.settled[0].voters.len(), 2);
}

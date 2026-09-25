use super::*;
// ---------------------------------------------------------------------------
// The cache is a cache
// ---------------------------------------------------------------------------

/// Test 9. A peer writes the one link that used to BE the state, and no
/// engine decision moves: the fold starts at genesis, the evaluator still
/// mints from genesis, and the pass heals the cache. The forged value here is
/// terminal, which before the fold suppressed every mint on this instance.
#[tokio::test(flavor = "multi_thread")]
async fn a_forged_current_state_moves_nothing() {
    let mut f = seed_satisfied_fixture(None).await;
    forge_cached_state(&mut f, "scoped").await;

    assert_eq!(
        f.derived().await.state,
        "identified",
        "the fold starts at genesis"
    );
    assert_eq!(
        f.cached_state().await,
        "scoped",
        "the forgery is on the graph"
    );

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

/// The pass's whole job, and its idempotency: it writes the cache and the
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
    assert_eq!(f.cached_state().await, "scoped", "the cache was written");
    assert!(
        f.read_set().await.marked_proposals().contains(&minted),
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
// The cache and the marks are this replica's own (#987)
// ---------------------------------------------------------------------------

/// Nothing the pass records ever leaves this replica: the mint's initial
/// cache, the healed cache and the fired marks are all `Local` links, and the
/// cache stays single-valued across writes.
#[tokio::test(flavor = "multi_thread")]
async fn the_cache_and_the_marks_are_local_links() {
    let mut f = seed_satisfied_fixture(None).await;
    let initial = current_state_links(&f).await;
    assert_eq!(initial.len(), 1, "the mint wrote one cache link");
    assert_eq!(initial[0].status, Some(LinkStatus::Local));

    let minted = f.mint_one().await;
    assert_eq!(consensus_pass(&mut f).await.len(), 1);

    let healed = current_state_links(&f).await;
    assert_eq!(
        healed.len(),
        1,
        "the old cache link is replaced, not joined"
    );
    assert_eq!(healed[0].status, Some(LinkStatus::Local));
    assert_eq!(f.cached_state().await, "scoped");

    let marks: Vec<_> = links_of(&f, &minted)
        .await
        .into_iter()
        .filter(|l| l.data.predicate.as_deref() == Some(RESOLVED_AS_PREDICATE))
        .collect();
    assert_eq!(marks.len(), 1, "one settling atom, one mark");
    assert_eq!(marks[0].status, Some(LinkStatus::Local));
}

/// A `Shared` `currentState` from a peer (what the pre-#987 executor wrote)
/// is neither believed nor deleted: hydration withholds it because the class
/// declares `currentState` `local: true`, this replica keeps deriving its own
/// value, and the peer's link stays — the pass deletes nothing shared.
///
/// The withholding is #1028's `local_status_filter`, not anything this module
/// does: the peer's link is the *later* one, so a latest-wins hydration would
/// serve `"scoped"` here. That is what the pre-#1028 executor did, and what
/// `local_cached_state` had to read raw links to work around.
#[tokio::test(flavor = "multi_thread")]
async fn a_peer_written_shared_cache_is_overridden_not_deleted() {
    let mut f = seed_satisfied_fixture(None).await;
    let bob = TestSigner::generate();
    let peer_cache = bob.sign(
        Link {
            source: f.instance_uri.clone(),
            predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
            target: literal("scoped"),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(peer_cache), LinkStatus::Shared, None)
        .await
        .expect("sync a peer's currentState");
    // The peer's link really is on the graph, and really does say something
    // else — so the assertion below is about withholding, not about a link
    // that never arrived.
    assert!(
        current_state_links(&f).await.iter().any(|l| {
            l.status == Some(LinkStatus::Shared)
                && l.author == bob.did
                && l.data.target == literal("scoped")
        }),
        "the peer's Shared currentState is on the graph"
    );
    assert_eq!(
        f.cached_state().await,
        "identified",
        "hydration serves our own Local cache and withholds the peer's Shared one"
    );

    consensus_pass(&mut f).await;
    assert_eq!(
        f.cached_state().await,
        "identified",
        "our own derivation is written and wins"
    );
    let links = current_state_links(&f).await;
    assert!(
        links
            .iter()
            .any(|l| l.status == Some(LinkStatus::Shared) && l.author == bob.did),
        "the peer's link is left in place: {links:?}"
    );
    assert_eq!(
        links
            .iter()
            .filter(|l| l.status == Some(LinkStatus::Local))
            .count(),
        1,
        "and exactly one Local cache link is ours"
    );
}

/// A peer's mark cannot mute this replica's once-only `FireOutcome`: only a
/// `Local` mark says "I already recorded this", so a `Shared` one — forged
/// or honest — is not ours, and the event still fires here exactly once.
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

/// The flow instance every OTHER replica sees: the creator's cache is `Local`, so a
/// synced `FlowInstance` carries no `currentState` at all. It must still load
/// as an instance — `currentState` is optional on the shape — with the empty
/// state meaning "not yet derived here", and the pass then fills it.
#[tokio::test(flavor = "multi_thread")]
async fn an_instance_without_a_cache_still_loads_and_the_pass_fills_it() {
    let mut f = seed_satisfied_fixture(None).await;
    let cache: Vec<LinkExpression> = current_state_links(&f)
        .await
        .into_iter()
        .map(LinkExpression::from)
        .collect();
    assert!(!cache.is_empty());
    f.perspective
        .remove_links(cache, None)
        .await
        .expect("drop the creator's local cache");

    let records = f.instances().await;
    assert_eq!(
        records.len(),
        1,
        "the flow instance is recognised with or without its cache"
    );
    assert_eq!(
        records[0].current_state, "",
        "absent cache = not yet derived"
    );

    consensus_pass(&mut f).await;
    assert_eq!(
        f.cached_state().await,
        "identified",
        "the pass writes the fold's answer"
    );
    assert_eq!(
        current_state_links(&f).await[0].status,
        Some(LinkStatus::Local)
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

/// Poll the cache until it reads `state` or the budget runs out; `true`
/// when it got there. The sync-triggered pass is debounced and runs on a
/// spawned task, so a test cannot await it directly.
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
/// accept or mint — must bring this replica's cache and marks up to date
/// by itself: they are `Local`, so nobody else can. No pass is called here;
/// the one `diff_from_link_language` queues does the work.
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
        "the synced vote must trigger the pass that heals the cache"
    );
    assert!(
        f.read_set().await.marked_proposals().contains(&minted),
        "and that pass marks the settling proposal"
    );
    assert_eq!(
        current_state_links(&f).await.len(),
        1,
        "single Local cache link, as always"
    );
}

/// A synced chat message queues nothing: the trigger is keyed on the flow
/// vocabulary, so ordinary traffic never re-derives a flow. Pinned by the
/// cache staying put where a pass would have healed it.
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
    tokio::time::sleep(std::time::Duration::from_millis(800)).await;
    assert_eq!(
        f.cached_state().await,
        "scoped",
        "no pass ran: the wrong cache was not healed"
    );
}

/// The flow instance as sync delivers it to every other replica: no `Local` cache.
async fn drop_local_cache(f: &mut Fixture) {
    let cache: Vec<LinkExpression> = current_state_links(f)
        .await
        .into_iter()
        .map(LinkExpression::from)
        .collect();
    f.perspective
        .remove_links(cache, None)
        .await
        .expect("drop the local cache");
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
/// them all as new. Its first pass over the instance marks them and writes
/// the cache without emitting; the next edge to settle is reported. The
/// creating replica is not a newcomer (it wrote its cache at the mint), so
/// its first settle IS reported — pinned by
/// `the_pass_writes_the_cache_and_the_marks_and_then_has_nothing_to_do`.
#[tokio::test(flavor = "multi_thread")]
async fn a_newcomers_first_pass_catches_up_silently_then_reports_normally() {
    let mut a = seed_review_flow().await;
    let h1 = settle(&mut a, "h1", "review", "changes_requested").await;
    let h2 = settle(&mut a, "h2", "changes_requested", "review").await;

    // Replica B: same definition, the flow instance as sync delivers it (no
    // cache), and A's history.
    let mut b = seed_review_flow().await;
    drop_local_cache(&mut b).await;
    replicate_proposals(&a, &mut b, &[&h1, &h2]).await;

    let first = consensus_pass(&mut b).await;
    assert!(first.is_empty(), "catch-up is silent, got {first:?}");
    assert_eq!(
        b.cached_state().await,
        "review",
        "but the cache is written (review → changes_requested → review)"
    );
    let marked = b.read_set().await.marked_proposals();
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

/// A co-owner cannot switch the catch-up off. Mallory, a second user of
/// replica B, writes a Local `currentState` on an instance B has never
/// derived. It is her own link, which she may write, but it says nothing
/// about what B's main agent has derived: B's first pass is still a silent
/// catch-up and reports none of A's history as new.
#[tokio::test(flavor = "multi_thread")]
async fn a_co_owners_planted_cache_does_not_switch_off_the_catch_up() {
    let mut a = seed_review_flow().await;
    let h1 = settle(&mut a, "h1", "review", "changes_requested").await;
    let h2 = settle(&mut a, "h2", "changes_requested", "review").await;

    let mut b = seed_review_flow().await;
    drop_local_cache(&mut b).await;
    replicate_proposals(&a, &mut b, &[&h1, &h2]).await;

    let mallory = second_agent("mallory-catch-up@e2e.test");
    b.perspective
        .add_link(
            Link {
                source: b.instance_uri.clone(),
                predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
                target: literal("review"),
            },
            LinkStatus::Local,
            None,
            &mallory,
        )
        .await
        .expect("a co-owner may write a Local link of their own");

    let first = consensus_pass(&mut b).await;
    assert!(
        first.is_empty(),
        "the main agent never derived this instance: its first pass is a silent catch-up, got {first:?}"
    );
    let marked = b.read_set().await.marked_proposals();
    assert!(
        marked.contains(&h1) && marked.contains(&h2),
        "and the history is marked: {marked:?}"
    );
}

/// A newcomer with nothing to catch up on: the first pass writes the cache
/// (silently, trivially) and the FIRST edge to settle afterwards is
/// reported — catch-up must not eat the first real event.
#[tokio::test(flavor = "multi_thread")]
async fn a_newcomer_with_no_history_reports_the_first_settle_after_its_first_pass() {
    let mut f = seed_satisfied_fixture(None).await;
    drop_local_cache(&mut f).await;
    assert!(consensus_pass(&mut f).await.is_empty());
    assert_eq!(f.cached_state().await, "identified");

    f.mint_one().await;
    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(outcomes.len(), 1, "got {outcomes:?}");
    assert_eq!(f.cached_state().await, "scoped");
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

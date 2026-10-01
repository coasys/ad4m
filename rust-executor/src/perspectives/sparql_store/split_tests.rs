//! The split store (#1224): shared links in the default graph, each user's
//! `Local` links in their own named graph, and reads that see the shared
//! links plus the reader's graph only.

use super::*;
use crate::agent::signatures::TestSigner;
use crate::perspectives::migration::{migrate_local_links_to_user_graphs, LocalLinkOwners};

fn signed(signer: &TestSigner, source: &str, target: &str, status: LinkStatus) -> LinkExpression {
    let data = Link {
        source: source.to_string(),
        predicate: Some("ad4m://p".to_string()),
        target: target.to_string(),
    };
    let signed = signer.sign(data.normalize());
    LinkExpression {
        author: signed.author,
        timestamp: signed.timestamp,
        data: signed.data,
        proof: signed.proof,
        status: Some(status),
    }
}

fn targets(links: &[DecoratedLinkExpression]) -> Vec<String> {
    let mut t: Vec<String> = links.iter().map(|l| l.data.target.clone()).collect();
    t.sort();
    t
}

/// The targets of every `?s <ad4m://p> ?t` row `reader` gets from a SPARQL
/// query, duplicates included.
fn sparql_targets(store: &SparqlStore, reader: &str, query: &str) -> Vec<String> {
    let rows: Vec<serde_json::Value> =
        serde_json::from_str(&store.read_as(Some(reader)).query_arbitrary(query).unwrap()).unwrap();
    let mut t: Vec<String> = rows
        .iter()
        .filter_map(|r| r["t"].as_str().map(str::to_string))
        .collect();
    t.sort();
    t
}

fn all_quads(store: &SparqlStore) -> Vec<Quad> {
    let mut quads: Vec<Quad> = store
        .store
        .quads_for_pattern(None, None, None, None)
        .collect::<Result<_, _>>()
        .unwrap();
    quads.sort_by_key(|q| q.to_string());
    quads
}

#[test]
fn a_local_link_is_stored_in_the_graph_of_the_user_who_wrote_it() {
    let alice = TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    store
        .add_link(&signed(&alice, "ad4m://s", "ad4m://t", LinkStatus::Local))
        .unwrap();

    let quads = all_quads(&store);
    // 1 direct triple + 1 reifier + 6 metadata
    assert_eq!(quads.len(), 8, "{quads:#?}");
    let graph = GraphName::from(local_graph(&alice.did));
    assert!(
        quads.iter().all(|q| q.graph_name == graph),
        "every quad in {graph}: {quads:#?}"
    );
    assert_eq!(
        local_graph(&alice.did).as_str(),
        format!("ad4m://local/{}", alice.did)
    );
}

#[test]
fn each_reader_sees_the_shared_links_and_only_their_own_local_links() {
    let (alice, bob, carol) = (
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
    );
    let store = SparqlStore::new(None).unwrap();
    store
        .add_link(&signed(
            &alice,
            "ad4m://s",
            "ad4m://alice",
            LinkStatus::Local,
        ))
        .unwrap();
    store
        .add_link(&signed(&bob, "ad4m://s", "ad4m://bob", LinkStatus::Local))
        .unwrap();
    store
        .add_link(&signed(
            &carol,
            "ad4m://s",
            "ad4m://shared",
            LinkStatus::Shared,
        ))
        .unwrap();

    for (reader, own) in [(&alice, "ad4m://alice"), (&bob, "ad4m://bob")] {
        let view = store.read_as(Some(&reader.did));
        let expected = vec![own.to_string(), "ad4m://shared".to_string()];
        assert_eq!(
            targets(
                &view
                    .query_links(Some("ad4m://s"), None, None, None, None, None)
                    .unwrap()
            ),
            expected
        );
        assert_eq!(
            targets(
                &view
                    .query_links_top_n_by_timestamp(
                        Some("ad4m://s"),
                        None,
                        None,
                        None,
                        None,
                        10,
                        false
                    )
                    .unwrap()
            ),
            expected
        );
        assert_eq!(targets(&view.get_all_links().unwrap()), expected);
        assert_eq!(
            sparql_targets(&store, &reader.did, "SELECT ?t WHERE { ?s <ad4m://p> ?t }"),
            expected,
            "raw SPARQL reads the same view"
        );
    }
    // Carol has no Local links: she reads the shared link only.
    assert_eq!(
        sparql_targets(&store, &carol.did, "SELECT ?t WHERE { ?s <ad4m://p> ?t }"),
        vec!["ad4m://shared".to_string()]
    );
}

/// The dataset is set on the parsed query, so neither `FROM`, `FROM NAMED`
/// nor `GRAPH` in the query text reaches another user's graph.
#[test]
fn a_query_cannot_name_another_users_graph() {
    let (alice, bob) = (TestSigner::generate(), TestSigner::generate());
    let store = SparqlStore::new(None).unwrap();
    store
        .add_link(&signed(
            &alice,
            "ad4m://s",
            "ad4m://alice",
            LinkStatus::Local,
        ))
        .unwrap();
    let alices = local_graph(&alice.did).as_str().to_string();

    for query in [
        format!("SELECT ?t FROM <{alices}> WHERE {{ ?s <ad4m://p> ?t }}"),
        format!(
            "SELECT ?t FROM NAMED <{alices}> WHERE {{ GRAPH <{alices}> {{ ?s <ad4m://p> ?t }} }}"
        ),
        "SELECT ?t WHERE { GRAPH ?g { ?s <ad4m://p> ?t } }".to_string(),
        "SELECT ?t WHERE { ?r <ad4m://ontology/author> ?t }".to_string(),
    ] {
        let rows: Vec<serde_json::Value> = serde_json::from_str(
            &store
                .read_as(Some(&bob.did))
                .query_arbitrary(&query)
                .unwrap(),
        )
        .unwrap();
        assert!(rows.is_empty(), "{query} gave Bob {rows:?}");
    }
    // Alice's own `GRAPH ?g` sees her graph.
    assert_eq!(
        sparql_targets(
            &store,
            &alice.did,
            "SELECT ?t WHERE { GRAPH ?g { ?s <ad4m://p> ?t } }"
        ),
        vec!["ad4m://alice".to_string()]
    );
}

/// The same triple asserted by Bob's shared link and Alice's Local link is
/// one triple in Alice's view, with two links on it. When the shared link
/// goes, the triple moves to Alice's graph; when hers goes, it is gone.
#[test]
fn a_triple_both_shared_and_local_is_in_a_view_once() {
    let (alice, bob) = (TestSigner::generate(), TestSigner::generate());
    let store = SparqlStore::new(None).unwrap();
    let hers = signed(&alice, "ad4m://s", "ad4m://t", LinkStatus::Local);
    let his = signed(&bob, "ad4m://s", "ad4m://t", LinkStatus::Shared);
    store.add_link(&hers).unwrap();
    store.add_link(&his).unwrap();

    let bare = "SELECT ?t WHERE { ?s <ad4m://p> ?t }";
    let reified = "SELECT ?t WHERE { ?s <ad4m://p> ?t . ?r rdf:reifies <<( ?s <ad4m://p> ?t )>> }";
    let reified = format!("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> {reified}");
    assert_eq!(sparql_targets(&store, &alice.did, bare).len(), 1);
    assert_eq!(
        sparql_targets(&store, &alice.did, &reified).len(),
        2,
        "one row per link"
    );
    assert_eq!(
        sparql_targets(&store, &bob.did, &reified).len(),
        1,
        "Bob's link only"
    );
    let alices_view = store.read_as(Some(&alice.did));
    assert_eq!(
        alices_view
            .query_links(Some("ad4m://s"), None, None, None, None, None)
            .unwrap()
            .len(),
        2
    );

    store.remove_link(&his).unwrap();
    assert_eq!(
        sparql_targets(&store, &alice.did, bare).len(),
        1,
        "moved to her graph"
    );
    assert_eq!(
        alices_view
            .query_links(Some("ad4m://s"), None, None, None, None, None)
            .unwrap()
            .len(),
        1
    );
    assert!(sparql_targets(&store, &bob.did, bare).is_empty());

    store.remove_link(&hers).unwrap();
    assert!(all_quads(&store).is_empty(), "{:#?}", all_quads(&store));
}

/// A write goes to the graph of the user it acts for, not to the graph of
/// the author the link names, so nobody can put a link in Alice's view.
#[test]
fn a_local_write_goes_to_the_writers_graph_not_the_named_authors() {
    let (alice, bob) = (TestSigner::generate(), TestSigner::generate());
    let store = SparqlStore::new(None).unwrap();
    let alices_link = signed(&alice, "ad4m://s", "ad4m://t", LinkStatus::Local);
    store.add_link_as(&alices_link, &bob.did).unwrap();

    let seen_by = |did: &str| {
        store
            .read_as(Some(did))
            .query_links(Some("ad4m://s"), None, None, None, None, None)
            .unwrap()
    };
    assert!(seen_by(&alice.did).is_empty(), "not in Alice's view");
    assert_eq!(seen_by(&bob.did).len(), 1, "in Bob's, who wrote it");
    assert_eq!(
        seen_by(&bob.did)[0].author,
        alice.did,
        "authorship is unchanged"
    );
}

/// Two users hold the same Local link expression; one removing it leaves the
/// other's copy, and a removal never reaches another user's graph.
#[test]
fn removing_as_one_user_leaves_another_users_copy() {
    let (alice, bob) = (TestSigner::generate(), TestSigner::generate());
    let store = SparqlStore::new(None).unwrap();
    let link = signed(&alice, "ad4m://s", "ad4m://t", LinkStatus::Local);
    store.add_link_as(&link, &alice.did).unwrap();
    store.add_link_as(&link, &bob.did).unwrap();

    store.remove_link_as(&link, &bob.did).unwrap();
    let alices = store
        .read_as(Some(&alice.did))
        .query_links(Some("ad4m://s"), None, None, None, None, None)
        .unwrap();
    assert_eq!(alices.len(), 1, "Alice keeps hers");
    assert!(store
        .read_as(Some(&bob.did))
        .query_links(Some("ad4m://s"), None, None, None, None, None)
        .unwrap()
        .is_empty());
}

fn store_as_before_1224(store: &SparqlStore, link: &LinkExpression) {
    store.add_link_as_before_1224(link);
}

/// The one-time migration (#1224): a store holding everyone's Local links in
/// the shared graph ends up with each user seeing exactly their own, a Local
/// link whose author is no user here in the main agent's graph, the shared
/// links where they were, no quad lost, and a second run that changes
/// nothing.
#[test]
fn migration_moves_each_local_link_to_its_owners_graph_once() {
    let (main, alice, bob, stranger, peer) = (
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
    );
    let store = SparqlStore::new(None).unwrap();
    let links = [
        signed(&main, "ad4m://s", "ad4m://main", LinkStatus::Local),
        signed(&alice, "ad4m://s", "ad4m://alice", LinkStatus::Local),
        signed(&bob, "ad4m://s", "ad4m://bob", LinkStatus::Local),
        // A LinkExpression another agent signed, stored Local by some user
        // of this executor; the store never said which one.
        signed(&stranger, "ad4m://s", "ad4m://stranger", LinkStatus::Local),
        signed(&peer, "ad4m://s", "ad4m://shared", LinkStatus::Shared),
        // The same triple as Alice's Local link, shared by a peer.
        signed(&peer, "ad4m://s", "ad4m://alice", LinkStatus::Shared),
    ];
    for link in &links {
        store_as_before_1224(&store, link);
    }
    let seen_by = |did: &str| {
        targets(
            &store
                .read_as(Some(did))
                .query_links(Some("ad4m://s"), None, None, None, None, None)
                .unwrap(),
        )
    };
    // Before: everything is in the shared graph, so Bob reads Alice's Local
    // link.
    assert!(seen_by(&bob.did).contains(&"ad4m://alice".to_string()));
    let quads_before = all_quads(&store).len();

    let owners = LocalLinkOwners::new(
        main.did.clone(),
        vec![alice.did.clone(), bob.did.clone()],
        vec![],
    );
    assert_eq!(
        migrate_local_links_to_user_graphs(&store, &owners).unwrap(),
        4
    );

    let shared = ["ad4m://alice", "ad4m://shared"];
    let with = |own: &[&str]| {
        let mut all: Vec<String> = own
            .iter()
            .chain(shared.iter())
            .map(|t| t.to_string())
            .collect();
        all.sort();
        all
    };
    assert_eq!(
        seen_by(&alice.did).len(),
        3,
        "her Local link and both shared ones"
    );
    assert_eq!(seen_by(&bob.did), with(&["ad4m://bob"]));
    assert_eq!(
        seen_by(&main.did),
        with(&["ad4m://main", "ad4m://stranger"])
    );
    assert_eq!(
        seen_by(&stranger.did),
        with(&[]),
        "the stranger is no user here"
    );
    assert_eq!(
        sparql_targets(&store, &bob.did, "SELECT ?t WHERE { ?s <ad4m://p> ?t }"),
        with(&["ad4m://bob"]),
        "raw SPARQL agrees, the shared triple once"
    );
    // Only direct triples change: Alice's `ad4m://alice` triple stays in the
    // shared graph, since a shared link asserts it; the other three Local
    // triples move with their links.
    assert_eq!(
        all_quads(&store).len(),
        quads_before,
        "no quad lost or added"
    );

    let after_first = all_quads(&store);
    assert_eq!(
        migrate_local_links_to_user_graphs(&store, &owners).unwrap(),
        0
    );
    assert_eq!(
        all_quads(&store),
        after_first,
        "a second run changes nothing"
    );
}

/// The move is one transaction per link, on disk too: a crash before the
/// second link's commit leaves every link wholly in its old layout or wholly
/// moved, with no quad lost, and the next run moves the rest (Data's
/// abort-before-commit probe from the #1226 review).
#[test]
fn a_move_stopped_before_a_commit_leaves_each_link_old_or_moved() {
    let (main, alice, bob, peer) = (
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
    );
    let dir = tempfile::TempDir::new().unwrap();
    let path = dir.path().to_str().unwrap();
    let local = [
        signed(&alice, "ad4m://s", "ad4m://alice1", LinkStatus::Local),
        signed(&bob, "ad4m://s", "ad4m://bob", LinkStatus::Local),
        signed(&alice, "ad4m://s", "ad4m://alice2", LinkStatus::Local),
    ];
    let shared = signed(&peer, "ad4m://s", "ad4m://shared", LinkStatus::Shared);
    let quads_before = {
        let store = SparqlStore::new(Some(path)).unwrap();
        for link in local.iter().chain([&shared]) {
            store_as_before_1224(&store, link);
        }
        all_quads(&store).len()
    };
    let owners = LocalLinkOwners::new(
        main.did.clone(),
        vec![alice.did.clone(), bob.did.clone()],
        vec![],
    );

    {
        let store = SparqlStore::new(Some(path)).unwrap();
        ABORT_MOVE_BEFORE_COMMIT.with(|n| n.set(Some(1)));
        let stopped = migrate_local_links_to_user_graphs(&store, &owners);
        ABORT_MOVE_BEFORE_COMMIT.with(|n| n.set(None));
        assert!(stopped.is_err());
    }

    let store = SparqlStore::new(Some(path)).unwrap();
    let quads = all_quads(&store);
    assert_eq!(quads.len(), quads_before, "no quad lost or added");
    let mut moved = 0;
    for link in &local {
        let reifier: NamedOrBlankNode = make_reifier_iri(link).into();
        let graphs: HashSet<GraphName> = quads
            .iter()
            .filter(|q| q.subject == reifier)
            .map(|q| q.graph_name.clone())
            .collect();
        let target = Term::from(NamedNode::new_unchecked(&link.data.target));
        let triple_in: HashSet<GraphName> = quads
            .iter()
            .filter(|q| q.object == target && q.predicate.as_str() == "ad4m://p")
            .map(|q| q.graph_name.clone())
            .collect();
        let own = GraphName::from(local_graph(&link.author));
        if graphs == HashSet::from([own.clone()]) && triple_in == HashSet::from([own]) {
            moved += 1;
        } else {
            assert!(
                graphs == HashSet::from([GraphName::DefaultGraph])
                    && triple_in == HashSet::from([GraphName::DefaultGraph]),
                "{} is half moved: reifier in {graphs:?}, triple in {triple_in:?}",
                link.data.target
            );
        }
    }
    assert_eq!(moved, 1, "the first link committed, the second did not");

    assert_eq!(
        migrate_local_links_to_user_graphs(&store, &owners).unwrap(),
        2
    );
    let seen_by = |did: &str| {
        targets(
            &store
                .read_as(Some(did))
                .query_links(Some("ad4m://s"), None, None, None, None, None)
                .unwrap(),
        )
    };
    assert_eq!(
        seen_by(&alice.did),
        vec!["ad4m://alice1", "ad4m://alice2", "ad4m://shared"]
    );
    assert_eq!(seen_by(&bob.did), vec!["ad4m://bob", "ad4m://shared"]);
    assert_eq!(all_quads(&store).len(), quads_before);
}

/// A Local link whose author is no user here was stored by one of the
/// perspective's owners, who all saw it before #1224: each owner gets a
/// copy, and nobody else. Without owners it goes to the main agent.
#[test]
fn migration_gives_a_foreign_local_link_to_each_perspective_owner() {
    let (main, alice, bob, carol, stranger) = (
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
    );
    let link = signed(&stranger, "ad4m://s", "ad4m://foreign", LinkStatus::Local);
    let users = vec![alice.did.clone(), bob.did.clone(), carol.did.clone()];
    let sees = |store: &SparqlStore, did: &str| {
        !store
            .read_as(Some(did))
            .query_links(Some("ad4m://s"), None, None, None, None, None)
            .unwrap()
            .is_empty()
    };

    // Owned by Alice and Bob (and a DID that is no user here).
    let store = SparqlStore::new(None).unwrap();
    store_as_before_1224(&store, &link);
    let owners = LocalLinkOwners::new(
        main.did.clone(),
        users.clone(),
        vec![alice.did.clone(), bob.did.clone(), stranger.did.clone()],
    );
    assert_eq!(
        migrate_local_links_to_user_graphs(&store, &owners).unwrap(),
        1
    );
    assert!(sees(&store, &alice.did) && sees(&store, &bob.did));
    assert!(!sees(&store, &carol.did) && !sees(&store, &main.did));
    assert!(!sees(&store, &stranger.did), "the stranger is no user here");

    // One owner: only she gets it.
    let store = SparqlStore::new(None).unwrap();
    store_as_before_1224(&store, &link);
    let owners = LocalLinkOwners::new(main.did.clone(), users.clone(), vec![carol.did.clone()]);
    migrate_local_links_to_user_graphs(&store, &owners).unwrap();
    assert!(sees(&store, &carol.did));
    assert!(!sees(&store, &alice.did) && !sees(&store, &main.did));

    // No owner: the main agent.
    let store = SparqlStore::new(None).unwrap();
    store_as_before_1224(&store, &link);
    let owners = LocalLinkOwners::new(main.did.clone(), users, vec![]);
    migrate_local_links_to_user_graphs(&store, &owners).unwrap();
    assert!(sees(&store, &main.did));
    assert!(!sees(&store, &alice.did));
}

/// Storing a shared link Local is refused: it would take the link out of
/// the shared graph, hiding it from every other user here. The shared link
/// stays where it was.
#[test]
fn a_shared_link_is_not_stored_local() {
    let (alice, bob, peer) = (
        TestSigner::generate(),
        TestSigner::generate(),
        TestSigner::generate(),
    );
    let store = SparqlStore::new(None).unwrap();
    let shared = signed(&peer, "ad4m://s", "ad4m://t", LinkStatus::Shared);
    store.add_link(&shared).unwrap();
    let as_local = LinkExpression {
        status: Some(LinkStatus::Local),
        ..shared.clone()
    };
    let before = all_quads(&store);
    assert!(store.add_link_as(&as_local, &alice.did).is_err());
    assert_eq!(all_quads(&store), before, "nothing changed");
    assert_eq!(
        targets(
            &store
                .read_as(Some(&bob.did))
                .query_links(Some("ad4m://s"), None, None, None, None, None)
                .unwrap()
        ),
        vec!["ad4m://t".to_string()]
    );
}

/// Which graph holds a link expression is decided by its identity (author,
/// timestamp, triple), not by its triple: a Shared copy of Alice's Local
/// expression, as a link language delivers it, moves that expression to the
/// shared graph with every annotation but its status unchanged.
#[test]
fn a_shared_copy_of_a_local_expression_moves_it_unchanged() {
    let (alice, bob) = (TestSigner::generate(), TestSigner::generate());
    let store = SparqlStore::new(None).unwrap();
    let local = signed(&alice, "ad4m://s", "ad4m://t", LinkStatus::Local);
    store.add_link_as(&local, &alice.did).unwrap();
    let annotations = |store: &SparqlStore| -> Vec<(String, String, GraphName)> {
        let mut a: Vec<_> = store
            .store
            .quads_for_pattern(
                Some(make_reifier_iri(&local).as_ref().into()),
                None,
                None,
                None,
            )
            .map(|q| q.unwrap())
            .filter(|q| q.predicate.as_str() != ONT_STATUS)
            .map(|q| (q.predicate.to_string(), q.object.to_string(), q.graph_name))
            .collect();
        a.sort_by_key(|x| format!("{x:?}"));
        a
    };
    let before = annotations(&store);
    assert!(before
        .iter()
        .all(|(_, _, g)| *g == GraphName::from(local_graph(&alice.did))));

    // Ingested Shared, as `persist_link_diff` stores a link language's diff.
    store
        .add_link(&LinkExpression {
            status: Some(LinkStatus::Shared),
            ..local.clone()
        })
        .unwrap();

    let after = annotations(&store);
    let content = |a: &[(String, String, GraphName)]| -> Vec<(String, String)> {
        a.iter().map(|(p, o, _)| (p.clone(), o.clone())).collect()
    };
    assert_eq!(
        content(&after),
        content(&before),
        "the content is identical"
    );
    assert!(
        after.iter().all(|(_, _, g)| g.is_default_graph()),
        "{after:?}"
    );
    for reader in [&alice.did, &bob.did] {
        let links = store
            .read_as(Some(reader))
            .query_links(Some("ad4m://s"), None, None, None, None, None)
            .unwrap();
        assert_eq!(links.len(), 1, "one copy, in the shared graph");
        assert_eq!(links[0].author, local.author);
        assert_eq!(links[0].timestamp, local.timestamp);
        assert_eq!(links[0].proof.signature, local.proof.signature);
        assert_eq!(links[0].status, Some(LinkStatus::Shared));
    }
}

//! A file import writes the scope index entries of its links itself: the
//! classes that were ready are ready when it returns, with the index the
//! write paths keep.

use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::collections::HashSet;

use super::build::{self, BuildState};
use super::import::TEST_LIMITS;
use super::test_support::*;
use super::writer::ClassRule;
use super::{build_key, Readiness};
use crate::agent::signatures::TestSigner;
use crate::perspectives::model_query::shape::parse_shape_from_json;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::LinkExpression;

const CHANNELS: [&str; 3] = ["si://ch/a", "si://ch/b", "si://ch/c"];

/// Sets the import's test limits (members per transaction, transactions
/// before a simulated stop) and clears them when the test ends.
struct Limits;

impl Limits {
    fn set(chunk: Option<usize>, stop_after: Option<usize>) -> Self {
        TEST_LIMITS.with(|l| l.set((chunk, stop_after)));
        Limits
    }
}

impl Drop for Limits {
    fn drop(&mut self) {
        TEST_LIMITS.with(|l| l.set((None, None)));
    }
}

/// A history of adds over `members` messages: flags (some by two authors,
/// some forged), channel links (some forged), body links. Seeded.
fn adds(seed: u64, members: usize) -> Vec<LinkExpression> {
    let mut rng = StdRng::seed_from_u64(seed);
    let signers = [TestSigner::generate(), TestSigner::generate()];
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    let mut push = |l: LinkExpression| {
        if seen.insert((
            l.author.clone(),
            l.timestamp.clone(),
            l.data.source.clone(),
            l.data.predicate.clone(),
            l.data.target.clone(),
        )) {
            out.push(l);
        }
    };
    for i in 0..members {
        let id = format!("si://m/{i:05}");
        // Many members share a millisecond, as a burst of messages does.
        let t = T0 + rng.gen_range(0..(members as u64 / 2).max(1));
        let author = &signers[rng.gen_range(0..2)];
        push(if rng.gen_bool(0.05) {
            forged(author, &id, TYPE, MESSAGE, t)
        } else {
            flag_link(author, &id, t)
        });
        if rng.gen_bool(0.2) {
            push(flag_link(&signers[1], &id, t + rng.gen_range(0..50)));
        }
        if rng.gen_bool(0.6) {
            let ch = CHANNELS[rng.gen_range(0..CHANNELS.len())];
            push(if rng.gen_bool(0.1) {
                forged(author, ch, IN_CHANNEL, &id, t + 1)
            } else {
                channel_link(author, ch, &id, t + 1)
            });
        }
        push(signed(
            author,
            &id,
            "si://body",
            &format!("literal:string:{i}"),
            t,
        ));
    }
    out
}

/// The store a class registered first and links written one by one make.
fn incremental(links: &[LinkExpression]) -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    store.scope_index_build_now(&rule).unwrap();
    for link in links {
        store.add_link(link).unwrap();
    }
    store
}

/// A store with `rule`'s class ready and `links` written one by one.
fn ready_store_with(links: &[LinkExpression]) -> (SparqlStore, ClassRule) {
    let store = incremental(links);
    let rule = store
        .scope_index_rule("Message")
        .expect("the class is indexed");
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    (store, rule)
}

fn file_of(links: &[LinkExpression]) -> tempfile::NamedTempFile {
    let file = tempfile::NamedTempFile::new().unwrap();
    let snapshot = crate::types::domain::Perspective {
        links: links
            .iter()
            .cloned()
            .map(|l| (l, crate::types::LinkStatus::Shared).into())
            .collect(),
    };
    std::fs::write(file.path(), serde_json::to_string(&snapshot).unwrap()).unwrap();
    file
}

fn mark_of(store: &SparqlStore, rule: &ClassRule) -> BuildState {
    let bytes = store
        .raw_store()
        .aux_get(&build_key(rule.id))
        .unwrap()
        .expect("the class has a build mark");
    build::BuildMark::decode(&bytes).unwrap().state
}

fn other_shape() -> ModelShape {
    parse_shape_from_json(
        r#"{
    "className": "Other",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"si://Other"}
    },
    "relations": {}
}"#,
        "Other",
    )
    .unwrap()
}

/// Into an empty store: ready when the import returns, with no build queued,
/// the index the write paths make, and the same counters and noncanonical
/// count.
#[test]
fn an_import_into_an_empty_store_is_ready_with_the_incremental_index() {
    let links = adds(1, 700);
    let expected = incremental(&links);

    let (store, rule) = ready_store_with(&[]);
    let _limits = Limits::set(Some(64), None);
    let stats = store
        .import_links_file(file_of(&links).path())
        .expect("the import");
    assert_eq!(stats.imported, links.len());

    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    assert_eq!(mark_of(&store, &rule), BuildState::Ready);
    assert!(
        !store.scope_index_build_queued(&rule),
        "a ready class needs no build"
    );
    assert_consistent(&store);
    assert_eq!(index_view(&store), index_view(&expected));
    assert_eq!(
        store.scope_reader().count(&rule.class_scope()).unwrap(),
        expected.scope_reader().count(&rule.class_scope()).unwrap()
    );
}

/// Into a store that has members: a member's earlier flag link in the file
/// moves its key, a member gets a new parent from the file, a new member has
/// a parent link the store held before its flag arrived, and counters agree.
#[test]
fn an_import_into_a_store_with_members_ends_with_the_incremental_index() {
    let all = adds(2, 600);
    let (stored, file) = all.split_at(all.len() / 3);
    let signer = TestSigner::generate();
    let mut stored = stored.to_vec();
    let mut file = file.to_vec();
    // An old member whose flag time the file moves earlier, and a second
    // parent the file gives it.
    stored.push(flag_link(&signer, "si://old/1", T0 + 500));
    stored.push(channel_link(&signer, CHANNELS[0], "si://old/1", T0 + 501));
    file.push(flag_link(&TestSigner::generate(), "si://old/1", T0 + 100));
    file.push(channel_link(&signer, CHANNELS[1], "si://old/1", T0 + 502));
    // An old member that the file only gives a parent.
    stored.push(flag_link(&signer, "si://old/2", T0 + 300));
    file.push(channel_link(&signer, CHANNELS[2], "si://old/2", T0 + 503));
    // A new member whose parent link the store held before its flag arrives.
    stored.push(channel_link(&signer, CHANNELS[0], "si://new/1", T0 + 20));
    file.push(flag_link(&signer, "si://new/1", T0 + 30));

    let mut every = stored.clone();
    every.extend(file.iter().cloned());
    let expected = incremental(&every);

    let (store, rule) = ready_store_with(&stored);
    let _limits = Limits::set(Some(50), None);
    store
        .import_links_file(file_of(&file).path())
        .expect("the import");

    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    assert_consistent(&store);
    assert_eq!(index_view(&store), index_view(&expected));
    for (member, parents) in [("si://old/1", 2), ("si://old/2", 1), ("si://new/1", 1)] {
        let in_parents = CHANNELS
            .iter()
            .filter(|ch| {
                store
                    .scope_reader()
                    .contains(&rule.under(ch, IN_CHANNEL), member)
                    .unwrap()
            })
            .count();
        assert_eq!(in_parents, parents, "{member} is under its parents");
    }
}

/// Membership is a verified flag link, and a parent is a verified link: a
/// forged flag makes no member, a forged channel link no `Under` entry.
#[test]
fn an_import_indexes_only_verified_links() {
    let signer = TestSigner::generate();
    let links = vec![
        flag_link(&signer, "si://m/good", T0 + 1),
        forged(&signer, "si://m/forged", TYPE, MESSAGE, T0 + 2),
        channel_link(&signer, CHANNELS[0], "si://m/good", T0 + 3),
        forged(&signer, CHANNELS[1], IN_CHANNEL, "si://m/good", T0 + 4),
        channel_link(&signer, CHANNELS[0], "si://m/forged", T0 + 5),
        // A member with one forged and one verified flag link.
        forged(&signer, "si://m/mixed", TYPE, MESSAGE, T0 + 1),
        flag_link(&signer, "si://m/mixed", T0 + 9),
    ];
    let (store, rule) = ready_store_with(&[]);
    store.import_links_file(file_of(&links).path()).unwrap();

    let reader = store.scope_reader();
    assert!(reader.contains(&rule.class_scope(), "si://m/good").unwrap());
    assert!(!reader
        .contains(&rule.class_scope(), "si://m/forged")
        .unwrap());
    assert!(reader
        .contains(&rule.under(CHANNELS[0], IN_CHANNEL), "si://m/good")
        .unwrap());
    assert!(!reader
        .contains(&rule.under(CHANNELS[1], IN_CHANNEL), "si://m/good")
        .unwrap());
    assert!(!reader
        .contains(&rule.under(CHANNELS[0], IN_CHANNEL), "si://m/forged")
        .unwrap());
    assert_eq!(reader.count(&rule.class_scope()).unwrap(), 2);
    assert_eq!(index_view(&store), index_view(&incremental(&links)));
}

/// A class registered before the import is ready after it; one registered
/// after it, and one that was still building when it came, are built from the
/// store (the import wrote no entry for them) and end with the same index.
#[test]
fn classes_registered_after_or_during_the_import_are_built() {
    let links = adds(3, 200);
    let expected = incremental(&links);

    // Registered before: ready. Registered after the import: building, then
    // built.
    let (store, rule) = ready_store_with(&[]);
    store
        .import_links_file_without_builds(file_of(&links).path())
        .unwrap();
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    let (other, _) = store.scope_index_register(&other_shape()).unwrap().unwrap();
    assert_eq!(store.scope_index_readiness(&other), Readiness::Building);

    // Building when the import came: not ready after it, and its build
    // covers the file's links.
    let store = SparqlStore::new(None).unwrap();
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Building);
    store
        .import_links_file_without_builds(file_of(&links).path())
        .unwrap();
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Building);
    store.scope_index_build_now(&rule).unwrap();
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    assert_consistent(&store);
    assert_eq!(index_view(&store), index_view(&expected));
}

/// A process that stops while the import writes its entries leaves the
/// classes building. The next open resumes their builds, and the index they
/// end with is the one the write paths keep.
#[test]
fn a_restart_in_the_middle_of_an_import_resumes_with_the_build() {
    let links = adds(4, 400);
    let expected = incremental(&links);
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().to_str().unwrap();

    {
        let store = SparqlStore::new(Some(path)).unwrap();
        let (rule, _) = store
            .scope_index_register(&message_shape())
            .unwrap()
            .unwrap();
        store.scope_index_build_now(&rule).unwrap();
        // Three transactions of 64 members, then the process "dies".
        let _limits = Limits::set(Some(64), Some(3));
        store
            .import_links_file_without_builds(file_of(&links).path())
            .unwrap();
        assert_eq!(store.scope_index_readiness(&rule), Readiness::Building);
        assert!(matches!(
            mark_of(&store, &rule),
            BuildState::Building { .. }
        ));
        assert!(
            store.scope_reader().count(&rule.class_scope()).unwrap()
                < expected.scope_reader().count(&rule.class_scope()).unwrap(),
            "the entries are written in part"
        );
    }

    let store = SparqlStore::new(Some(path)).unwrap();
    let rule = store
        .scope_index_rule("Message")
        .expect("the rule is loaded");
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Building);
    store.scope_index_build_now(&rule).unwrap();
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    assert_consistent(&store);
    assert_eq!(index_view(&store), index_view(&expected));
}

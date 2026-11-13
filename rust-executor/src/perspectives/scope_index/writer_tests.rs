//! The index the write paths keep: membership by verified flag links, the
//! key from the earliest of them, `Under` scopes, counts and support.

use super::compact::{HEAD_WINDOW_MS, THRESHOLD};
use super::test_support::*;
use super::writer::{ClassRule, ScopeChange};
use super::{Readiness, Scope};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;

const CHANNEL: &str = "si://channel/1";

/// A store with `Message` indexed and ready (built over an empty store).
fn indexed_store(path: Option<&str>) -> (SparqlStore, ClassRule) {
    let store = SparqlStore::new(path).unwrap();
    let (rule, new) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .expect("Message has a flag");
    assert!(new);
    store.scope_index_build_now(&rule).unwrap();
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    (store, rule)
}

fn page(store: &SparqlStore, scope: &Scope) -> Vec<(String, u64)> {
    store
        .scope_reader()
        .page(scope, None, 1000)
        .unwrap()
        .members
        .into_iter()
        .map(|(m, k)| (m, k.t_ms()))
        .collect()
}

fn count(store: &SparqlStore, scope: &Scope) -> i64 {
    store.scope_reader().count(scope).unwrap()
}

#[test]
fn a_flag_link_makes_a_member_newest_first() {
    let (store, rule) = indexed_store(None);
    let s = TestSigner::generate();
    for (i, id) in ["si://m/a", "si://m/b", "si://m/c"].iter().enumerate() {
        store.add_link(&flag_link(&s, id, T0 + i as u64)).unwrap();
    }
    let class = rule.class_scope();
    assert_eq!(
        page(&store, &class),
        vec![
            ("si://m/c".into(), T0 + 2),
            ("si://m/b".into(), T0 + 1),
            ("si://m/a".into(), T0)
        ]
    );
    assert_eq!(count(&store, &class), 3);
    assert!(store.scope_reader().contains(&class, "si://m/b").unwrap());
    assert!(!store.scope_reader().contains(&class, "si://m/x").unwrap());
    // A link that is not the flag does nothing to the class.
    store
        .add_link(&signed(
            &s,
            "si://m/x",
            "si://body",
            "literal:string:hi",
            T0,
        ))
        .unwrap();
    assert_eq!(count(&store, &class), 3);
    assert_consistent(&store);
}

#[test]
fn both_arrival_orders_give_one_index() {
    let s = TestSigner::generate();
    let flag = flag_link(&s, "si://m/1", T0 + 5);
    let edge = channel_link(&s, CHANNEL, "si://m/1", T0 + 9);

    let (first, rule) = indexed_store(None);
    first.add_link(&flag).unwrap();
    first.add_link(&edge).unwrap();

    let (second, _) = indexed_store(None);
    second.add_link(&edge).unwrap();
    second.add_link(&flag).unwrap();

    let under = rule.under(CHANNEL, IN_CHANNEL);
    for store in [&first, &second] {
        assert_eq!(page(store, &under), vec![("si://m/1".into(), T0 + 5)]);
        assert_eq!(count(store, &under), 1);
        assert_eq!(count(store, &rule.class_scope()), 1);
        assert_consistent(store);
    }
    assert_eq!(index_view(&first), index_view(&second));

    // In one diff, in either order, the same again.
    let (third, _) = indexed_store(None);
    third
        .apply_diff(&[], &[edge.clone(), flag.clone()])
        .unwrap();
    assert_eq!(index_view(&third), index_view(&first));
}

#[test]
fn one_triple_from_two_authors_is_one_entry_keyed_by_the_earliest() {
    let (store, rule) = indexed_store(None);
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();
    let late = flag_link(&alice, "si://m/1", T0 + 100);
    let early = flag_link(&bob, "si://m/1", T0 + 10);
    let class = rule.class_scope();

    store.add_link(&late).unwrap();
    assert_eq!(page(&store, &class), vec![("si://m/1".into(), T0 + 100)]);
    // The earlier link moves the key; still one entry.
    store.add_link(&early).unwrap();
    assert_eq!(page(&store, &class), vec![("si://m/1".into(), T0 + 10)]);
    assert_eq!(count(&store, &class), 1);
    assert_consistent(&store);

    // Removing the earlier link moves it back; removing the last ends it.
    store.remove_link(&early).unwrap();
    assert_eq!(page(&store, &class), vec![("si://m/1".into(), T0 + 100)]);
    store.remove_link(&late).unwrap();
    assert!(page(&store, &class).is_empty());
    assert_eq!(count(&store, &class), 0);
    assert_consistent(&store);
}

#[test]
fn an_invalid_flag_makes_no_member() {
    let (store, rule) = indexed_store(None);
    let s = TestSigner::generate();
    let class = rule.class_scope();
    store
        .add_link(&forged(&s, "si://m/1", TYPE, MESSAGE, T0))
        .unwrap();
    assert!(page(&store, &class).is_empty());

    // A forged channel link puts a real member under no channel.
    store.add_link(&flag_link(&s, "si://m/2", T0 + 1)).unwrap();
    store
        .add_link(&forged(&s, CHANNEL, IN_CHANNEL, "si://m/2", T0 + 2))
        .unwrap();
    assert_eq!(count(&store, &rule.under(CHANNEL, IN_CHANNEL)), 0);

    // A verified flag whose verdict goes away ends the membership.
    let flag = flag_link(&s, "si://m/3", T0 + 3);
    store.add_link(&flag).unwrap();
    assert_eq!(count(&store, &class), 2);
    store.remove_proof_valid_annotation(&flag).unwrap();
    assert_eq!(page(&store, &class), vec![("si://m/2".into(), T0 + 1)]);
    assert_consistent(&store);
}

#[test]
fn an_entry_goes_only_when_no_support_remains() {
    let (store, rule) = indexed_store(None);
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();
    let under = rule.under(CHANNEL, IN_CHANNEL);
    store.add_link(&flag_link(&alice, "si://m/1", T0)).unwrap();
    let edge_a = channel_link(&alice, CHANNEL, "si://m/1", T0 + 1);
    let edge_b = channel_link(&bob, CHANNEL, "si://m/1", T0 + 2);
    store.add_link(&edge_a).unwrap();
    store.add_link(&edge_b).unwrap();
    assert_eq!(count(&store, &under), 1);

    store.remove_link(&edge_a).unwrap();
    assert_eq!(page(&store, &under), vec![("si://m/1".into(), T0)]);
    store.remove_link(&edge_b).unwrap();
    assert!(page(&store, &under).is_empty());
    assert_eq!(count(&store, &under), 0);

    // A member that leaves the class leaves every scope of it.
    store.add_link(&edge_a).unwrap();
    assert_eq!(count(&store, &under), 1);
    let deltas = store
        .apply_diff(&[flag_link(&alice, "si://m/1", T0)], &[])
        .unwrap();
    assert_eq!(
        deltas
            .iter()
            .filter(|d| d.change == ScopeChange::Removed)
            .count(),
        2,
        "the class scope and the channel"
    );
    assert_eq!(count(&store, &under), 0);
    assert_eq!(count(&store, &rule.class_scope()), 0);
    assert_consistent(&store);
}

#[test]
fn applying_a_diff_twice_changes_nothing_the_second_time() {
    let (store, _) = indexed_store(None);
    let s = TestSigner::generate();
    let links = vec![
        flag_link(&s, "si://m/1", T0),
        channel_link(&s, CHANNEL, "si://m/1", T0 + 1),
    ];
    assert_eq!(store.apply_diff(&[], &links).unwrap().len(), 2);
    let before = index_view(&store);
    assert!(store.apply_diff(&[], &links).unwrap().is_empty());
    assert_eq!(index_view(&store), before);
}

#[test]
fn counts_and_readiness_survive_a_reopen() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().to_str().unwrap();
    let s = TestSigner::generate();
    let rule = {
        let (store, rule) = indexed_store(Some(path));
        for i in 0..40u64 {
            let id = format!("si://m/{i}");
            store.add_link(&flag_link(&s, &id, T0 + i)).unwrap();
            if i % 2 == 0 {
                store
                    .add_link(&channel_link(&s, CHANNEL, &id, T0 + i))
                    .unwrap();
            }
        }
        store.remove_link(&flag_link(&s, "si://m/0", T0)).unwrap();
        store.flush().unwrap();
        rule
    };
    let store = SparqlStore::new(Some(path)).unwrap();
    assert_eq!(store.scope_index_rule(&rule.class), Some(rule.clone()));
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    assert_eq!(count(&store, &rule.class_scope()), 39);
    assert_eq!(count(&store, &rule.under(CHANNEL, IN_CHANNEL)), 19);
    // Writes after the reopen keep counting from there, with fresh keys.
    store
        .add_link(&flag_link(&s, "si://m/new", T0 + 39))
        .unwrap();
    assert_eq!(count(&store, &rule.class_scope()), 40);
    assert_consistent(&store);
}

#[test]
fn a_time_that_is_not_canonical_is_counted_for_the_class() {
    let (store, rule) = indexed_store(None);
    let s = TestSigner::generate();
    // Signed at a whole second, stored without the milliseconds: it verifies
    // (the signature covers the time, not its text), but its string does not
    // sort as the executor's form does.
    let mut odd = flag_link(&s, "si://m/odd", T0 - T0 % 1000);
    odd.timestamp = odd.timestamp.replace(".000Z", "Z");
    assert!(odd.compute_proof_valid());
    store.add_link(&flag_link(&s, "si://m/1", T0)).unwrap();
    store.add_link(&odd).unwrap();
    assert_eq!(store.scope_reader().noncanonical(&rule.class).unwrap(), 1);
    assert_eq!(count(&store, &rule.class_scope()), 2);
    store.remove_link(&odd).unwrap();
    assert_eq!(store.scope_reader().noncanonical(&rule.class).unwrap(), 0);
    assert_consistent(&store);
}

#[test]
fn reader_windows_and_catch_up() {
    let (store, rule) = indexed_store(None);
    let s = TestSigner::generate();
    for i in 0..10u64 {
        store
            .add_link(&flag_link(&s, &format!("si://m/{i}"), T0 + i * 10))
            .unwrap();
    }
    let reader = store.scope_reader();
    let class = rule.class_scope();
    let first = reader.page(&class, None, 4).unwrap();
    let ids: Vec<&str> = first.members.iter().map(|(m, _)| m.as_str()).collect();
    assert_eq!(ids, ["si://m/9", "si://m/8", "si://m/7", "si://m/6"]);
    let second = reader.page(&class, first.after, 4).unwrap();
    assert_eq!(second.members[0].0, "si://m/5");

    let window = reader.window(&class, T0 + 20, T0 + 50, None, 10).unwrap();
    let ids: Vec<&str> = window.members.iter().map(|(m, _)| m.as_str()).collect();
    assert_eq!(ids, ["si://m/4", "si://m/3", "si://m/2"]);

    let newest = first.newest.unwrap();
    assert!(reader
        .newer_than(&class, newest, 10)
        .unwrap()
        .members
        .is_empty());
    store
        .add_link(&flag_link(&s, "si://m/late", T0 + 1000))
        .unwrap();
    let caught = reader.newer_than(&class, newest, 10).unwrap();
    assert_eq!(caught.members.len(), 1);
    assert_eq!(caught.members[0].0, "si://m/late");

    let parents = vec![
        (CHANNEL.to_string(), rule.under(CHANNEL, IN_CHANNEL)),
        ("all".to_string(), class.clone()),
    ];
    let pages = reader.children(&parents, 2).unwrap();
    assert!(pages[0].members.is_empty());
    assert_eq!(pages[1].members.len(), 2);
}

#[test]
fn many_head_removals_start_one_compaction() {
    let (store, rule) = indexed_store(None);
    let s = TestSigner::generate();
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64;
    let compactor = store.scope_compactor();
    let links: Vec<_> = (0..THRESHOLD as u64)
        .map(|i| flag_link(&s, &format!("si://m/{i}"), now - 1000 + i))
        .collect();
    store.apply_diff(&[], &links).unwrap();
    assert_eq!(compactor.started(), 0);
    // Removals older than the head window do not count.
    let old = flag_link(&s, "si://m/old", now - HEAD_WINDOW_MS - 60_000);
    store.add_link(&old).unwrap();
    store.remove_link(&old).unwrap();
    assert_eq!(compactor.started(), 0);
    // One removal short of the threshold, then the one that crosses it.
    store
        .apply_diff(&links[..THRESHOLD as usize - 1], &[])
        .unwrap();
    assert_eq!(compactor.started(), 0);
    store
        .apply_diff(&links[THRESHOLD as usize - 1..], &[])
        .unwrap();
    assert_eq!(compactor.started(), 1);
    assert_eq!(count(&store, &rule.class_scope()), 0);
}

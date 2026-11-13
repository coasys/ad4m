//! The first build of a class over links already in the store: resumable from
//! its mark, equal to the index the write paths keep.

use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::time::{Duration, Instant};

use super::build::{self, BuildMark, BuildProgress, BuildState, BATCH};
use super::test_support::*;
use super::writer::ClassRule;
use super::{build_key, Readiness};
use crate::agent::signatures::TestSigner;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::LinkExpression;

const CHANNELS: [&str; 3] = ["si://ch/a", "si://ch/b", "si://ch/c"];

/// A history of link writes over `members` messages: flags (some by two
/// authors, some forged), channel links, body links, and removals of some of
/// each. Seeded, so every run writes the same history.
fn history(seed: u64, members: usize) -> Vec<(bool, LinkExpression)> {
    let mut rng = StdRng::seed_from_u64(seed);
    let signers = [TestSigner::generate(), TestSigner::generate()];
    let mut out = Vec::new();
    for i in 0..members {
        let id = format!("si://m/{i:05}");
        // Many members share a millisecond, as a burst of messages does.
        let t = T0 + rng.gen_range(0..(members as u64 / 2).max(1));
        let author = &signers[rng.gen_range(0..2)];
        let flag = if rng.gen_bool(0.05) {
            forged(author, &id, TYPE, MESSAGE, t)
        } else {
            flag_link(author, &id, t)
        };
        out.push((true, flag.clone()));
        if rng.gen_bool(0.1) {
            out.push((true, flag_link(&signers[1], &id, t + rng.gen_range(0..50))));
        }
        if rng.gen_bool(0.6) {
            let ch = CHANNELS[rng.gen_range(0..CHANNELS.len())];
            let edge = channel_link(author, ch, &id, t + 1);
            out.push((true, edge.clone()));
            if rng.gen_bool(0.1) {
                out.push((false, edge));
            }
        }
        out.push((
            true,
            signed(author, &id, "si://body", &format!("literal:string:{i}"), t),
        ));
        if rng.gen_bool(0.05) {
            out.push((false, flag));
        }
    }
    out
}

fn write(store: &SparqlStore, history: &[(bool, LinkExpression)]) {
    for (add, link) in history {
        if *add {
            store.add_link(link).unwrap();
        } else {
            store.remove_link(link).unwrap();
        }
    }
}

/// The index kept by the write paths: the class registered first, then the
/// history.
fn incremental(history: &[(bool, LinkExpression)]) -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    store.scope_index_build_now(&rule).unwrap();
    write(&store, history);
    store
}

fn mark(store: &SparqlStore, rule: &ClassRule) -> BuildMark {
    BuildMark::decode(
        &store
            .raw_store()
            .aux_get(&build_key(rule.id))
            .unwrap()
            .expect("a mark"),
    )
    .unwrap()
}

fn wait_ready(store: &SparqlStore, rule: &ClassRule) {
    let start = Instant::now();
    while store.scope_index_readiness(rule) != Readiness::Ready {
        assert!(
            start.elapsed() < Duration::from_secs(60),
            "the resumed build did not finish"
        );
        std::thread::sleep(Duration::from_millis(20));
    }
}

#[test]
fn a_resumed_build_equals_the_incremental_index() {
    let members = BATCH * 2 + 300;
    let all = history(7, members);
    // The links written while the build runs.
    let split = all.len() * 9 / 10;
    let (before, during) = all.split_at(split);

    let expected = incremental(&all);
    assert_consistent(&expected);

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().to_str().unwrap();
    let rule = {
        let store = SparqlStore::new(Some(path)).unwrap();
        write(&store, before);
        let (rule, _) = store
            .scope_index_register(&message_shape())
            .unwrap()
            .unwrap();
        // Phase 0 (an empty class scope), then one batch of phase 1.
        assert_eq!(
            store.scope_index_build_batches(&rule, 2).unwrap(),
            BuildProgress::Paused
        );
        let BuildState::Building { phase, cursor } = mark(&store, &rule).state else {
            panic!("the build is not finished");
        };
        assert_eq!(phase, 1);
        assert!(!cursor.is_empty(), "the mark says where to go on");
        assert_eq!(store.scope_index_readiness(&rule), Readiness::Building);
        // Writes during the build index the class like any other.
        write(&store, &during[..during.len() / 2]);
        store.flush().unwrap();
        rule
    };
    // The reopened store goes on from the mark by itself.
    let store = SparqlStore::new(Some(path)).unwrap();
    write(&store, &during[during.len() / 2..]);
    wait_ready(&store, &rule);
    assert_eq!(mark(&store, &rule).state, BuildState::Ready);
    assert_consistent(&store);
    assert_eq!(index_view(&store), index_view(&expected));
}

#[test]
fn a_build_in_one_go_equals_the_incremental_index() {
    let all = history(11, 400);
    let expected = incremental(&all);
    let store = SparqlStore::new(None).unwrap();
    write(&store, &all);
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    assert_eq!(
        store.scope_index_build_now(&rule).unwrap(),
        BuildProgress::Done
    );
    assert_eq!(index_view(&store), index_view(&expected));
    // Building again changes nothing.
    assert_eq!(
        store.scope_index_build_now(&rule).unwrap(),
        BuildProgress::Done
    );
    assert_eq!(index_view(&store), index_view(&expected));
}

fn shape_with_flag(value: &str) -> ModelShape {
    let mut shape = message_shape();
    for p in &mut shape.properties {
        if p.is_flag {
            p.initial_value = Some(value.to_string());
        }
    }
    shape
}

#[test]
fn a_changed_flag_rebuilds_the_class() {
    let s = TestSigner::generate();
    let other = "si://Note";
    let links: Vec<LinkExpression> = (0..30u64)
        .flat_map(|i| {
            let id = format!("si://m/{i}");
            let value = if i % 3 == 0 { other } else { MESSAGE };
            [
                signed(&s, &id, TYPE, value, T0 + i),
                channel_link(&s, CHANNELS[0], &id, T0 + i),
            ]
        })
        .collect();

    let store = SparqlStore::new(None).unwrap();
    let (first, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    store.scope_index_build_now(&first).unwrap();
    for l in &links {
        store.add_link(l).unwrap();
    }
    assert_eq!(
        store.scope_reader().count(&first.class_scope()).unwrap(),
        20
    );

    let (second, new) = store
        .scope_index_register(&shape_with_flag(other))
        .unwrap()
        .unwrap();
    assert!(new);
    assert_eq!(second.id, first.id, "the same class");
    assert_eq!(store.scope_index_readiness(&first), Readiness::NotIndexed);
    assert_eq!(store.scope_index_readiness(&second), Readiness::Building);
    store.scope_index_build_now(&second).unwrap();
    assert_eq!(store.scope_index_readiness(&second), Readiness::Ready);
    assert_consistent(&store);

    // The index equals one built for the new flag from the start.
    let fresh = SparqlStore::new(None).unwrap();
    let (rule, _) = fresh
        .scope_index_register(&shape_with_flag(other))
        .unwrap()
        .unwrap();
    fresh.scope_index_build_now(&rule).unwrap();
    for l in &links {
        fresh.add_link(l).unwrap();
    }
    assert_eq!(index_view(&store), index_view(&fresh));
    assert_eq!(
        store.scope_reader().count(&second.class_scope()).unwrap(),
        10
    );
    assert_eq!(
        store
            .scope_reader()
            .count(&second.under(CHANNELS[0], IN_CHANNEL))
            .unwrap(),
        10
    );
}

#[test]
fn a_build_whose_rule_was_replaced_stops() {
    let store = SparqlStore::new(None).unwrap();
    let (first, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    store
        .scope_index_register(&shape_with_flag("si://Other"))
        .unwrap()
        .unwrap();
    assert_eq!(
        store.scope_index_build_now(&first).unwrap(),
        BuildProgress::Superseded
    );
}

#[test]
fn a_cleared_store_has_an_empty_ready_index() {
    let s = TestSigner::generate();
    let store = incremental(&[(true, flag_link(&s, "si://m/1", T0))]);
    let rule = store.scope_index_rule("Message").unwrap();
    assert_eq!(store.scope_reader().count(&rule.class_scope()).unwrap(), 1);
    store.clear().unwrap();
    assert_eq!(store.scope_reader().count(&rule.class_scope()).unwrap(), 0);
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    assert!(index_view(&store).entries.is_empty());
    // And it indexes again.
    store.add_link(&flag_link(&s, "si://m/2", T0)).unwrap();
    assert_eq!(store.scope_reader().count(&rule.class_scope()).unwrap(), 1);
    assert_consistent(&store);
}

#[test]
fn build_marks_round_trip() {
    let rule = ClassRule::new("Message", TYPE, MESSAGE);
    for state in [
        BuildState::Ready,
        BuildState::Building {
            phase: 0,
            cursor: vec![],
        },
        BuildState::Building {
            phase: 1,
            cursor: b"si://m/00042".to_vec(),
        },
    ] {
        let mark = BuildMark {
            rule: rule.clone(),
            state,
        };
        assert_eq!(BuildMark::decode(&mark.encode()).unwrap(), mark);
    }
    assert!(BuildMark::decode(&[9, 0, 0]).is_err(), "another version");
    assert!(BuildMark::decode(&[1, 0]).is_err(), "truncated");
    let store = SparqlStore::new(None).unwrap();
    store.scope_index_register(&message_shape()).unwrap();
    assert_eq!(build::load_marks(store.raw_store()).unwrap().len(), 1);
}

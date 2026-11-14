//! The first build of a class over links already in the store: resumable from
//! its mark, equal to the index the write paths keep.

use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::time::{Duration, Instant};

use super::build::{self, BatchLimits, BuildMark, BuildProgress, BuildQueue, BuildState, BATCH};
use super::test_support::*;
use super::writer::ClassRule;
use super::{build_key, Readiness};
use crate::agent::signatures::TestSigner;
use crate::perspectives::model_query::shape::parse_shape_from_json;
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

/// Limits under which every batch stops after its first member.
const ONE_MEMBER: BatchLimits = BatchLimits {
    time: Duration::ZERO,
    pause: Duration::ZERO,
};

/// A batch stops at its time bound, its mark names the last member it
/// checked, and the build that goes on from there equals the incremental
/// index: in phase 1 (a first build) and in phase 0 (a changed flag).
#[test]
fn a_batch_stops_at_its_time_bound_and_the_build_goes_on() {
    let other = "si://Note";
    let all = history(17, 200);
    let store = SparqlStore::new(None).unwrap();
    write(&store, &all);
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    // Phase 0 over an empty class scope, then one member of phase 1.
    assert_eq!(
        store
            .scope_index_build_limited(&rule, Some(2), ONE_MEMBER)
            .unwrap(),
        BuildProgress::Paused
    );
    let first = all
        .iter()
        .filter(|(_, l)| l.data.predicate.as_deref() == Some(TYPE))
        .map(|(_, l)| l.data.source.clone())
        .min()
        .unwrap();
    assert_eq!(
        mark(&store, &rule).state,
        BuildState::Building {
            phase: 1,
            cursor: first.into_bytes()
        }
    );
    assert_eq!(
        store
            .scope_index_build_limited(&rule, None, ONE_MEMBER)
            .unwrap(),
        BuildProgress::Done
    );
    assert_consistent(&store);
    assert_eq!(index_view(&store), index_view(&incremental(&all)));

    // A changed flag: phase 0 goes over the members the class holds.
    let (second, _) = store
        .scope_index_register(&shape_with_flag(other))
        .unwrap()
        .unwrap();
    let before = store.scope_reader().count(&rule.class_scope()).unwrap();
    assert!(before > 2);
    assert_eq!(
        store
            .scope_index_build_limited(&second, Some(1), ONE_MEMBER)
            .unwrap(),
        BuildProgress::Paused
    );
    let BuildState::Building { phase: 0, cursor } = mark(&store, &second).state else {
        panic!("one member of phase 0 is checked, and phase 0 goes on");
    };
    assert_eq!(cursor.len(), 8, "a member key");
    assert_eq!(
        store.scope_reader().count(&second.class_scope()).unwrap(),
        before - 1,
        "one member is checked: it is not a Note"
    );
    assert_eq!(
        store
            .scope_index_build_limited(&second, None, ONE_MEMBER)
            .unwrap(),
        BuildProgress::Done
    );
    assert_consistent(&store);
    let fresh = SparqlStore::new(None).unwrap();
    let (rule, _) = fresh
        .scope_index_register(&shape_with_flag(other))
        .unwrap()
        .unwrap();
    fresh.scope_index_build_now(&rule).unwrap();
    write(&fresh, &all);
    assert_eq!(index_view(&store), index_view(&fresh));
}

/// A link write waits for at most one batch: with every batch stopped after
/// its first member, a build of many members leaves the lock to each write.
#[test]
fn writes_go_on_while_a_build_runs() {
    let all = history(19, BATCH);
    let store = SparqlStore::new(None).unwrap();
    write(&store, &all);
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    let s = TestSigner::generate();
    let late: Vec<LinkExpression> = (0..20u64)
        .map(|i| flag_link(&s, &format!("si://late/{i}"), T0 + i))
        .collect();
    let builder = {
        let store = store.clone();
        let rule = rule.clone();
        std::thread::spawn(move || {
            store.scope_index_build_limited(
                &rule,
                None,
                BatchLimits {
                    time: Duration::ZERO,
                    pause: build::BATCH_PAUSE,
                },
            )
        })
    };
    for link in &late {
        store.add_link(link).unwrap();
    }
    // Every write ended before the build: none waited for the whole build.
    assert_ne!(
        store.scope_index_readiness(&rule),
        Readiness::Ready,
        "the writes waited for the build"
    );
    assert_eq!(builder.join().unwrap().unwrap(), BuildProgress::Done);
    assert_consistent(&store);
    let mut everything = all.clone();
    everything.extend(late.iter().map(|l| (true, l.clone())));
    assert_eq!(index_view(&store), index_view(&incremental(&everything)));
}

/// A build that failed starts again at the next `scope_index_ensure`.
#[test]
fn a_failed_build_starts_again_at_the_next_ensure() {
    let all = history(23, 50);
    let store = SparqlStore::new(None).unwrap();
    write(&store, &all);
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    let set_mark = |value: &[u8]| {
        let mut tx = store.raw_store().start_transaction().unwrap();
        tx.aux_put(&build_key(rule.id), value);
        tx.commit().unwrap();
    };
    set_mark(b"not a mark");
    assert!(
        store.scope_index_build_now(&rule).is_err(),
        "the build fails"
    );
    assert!(!store.scope_index_build_queued(&rule));
    set_mark(
        &BuildMark {
            rule: rule.clone(),
            state: BuildState::Building {
                phase: 0,
                cursor: Vec::new(),
            },
        }
        .encode(),
    );
    assert_eq!(
        store.scope_index_ensure(&message_shape()).unwrap(),
        Some(rule.clone())
    );
    wait_ready(&store, &rule);
    assert_eq!(index_view(&store), index_view(&incremental(&all)));
}

/// `Note`: flag `ad4m://type = si://Note`.
fn note_shape() -> ModelShape {
    parse_shape_from_json(
        &format!(
            r#"{{
    "className": "Note",
    "properties": {{
        "type": {{"predicate":"{TYPE}","required":true,"flag":true,"initial":"si://Note"}}
    }},
    "relations": {{}}
}}"#
        ),
        "Note",
    )
    .expect("the test shape parses")
}

/// One build runs at a time; the others wait in order, a waiting build of a
/// class is replaced by its newer rule, and the worker stops when the queue
/// is empty.
#[test]
fn builds_wait_in_a_queue() {
    let message = ClassRule::new("Message", TYPE, MESSAGE);
    let note = ClassRule::new("Note", TYPE, "si://Note");
    let note_again = ClassRule::new("Note", TYPE, "si://Note2");
    let mut queue = BuildQueue::default();
    assert!(
        queue.push(message.clone()),
        "the first build starts a worker"
    );
    assert!(!queue.push(note.clone()), "the worker runs it later");
    assert!(!queue.push(message.clone()), "queued already");
    assert_eq!(queue.take_next(), Some(message.clone()));
    assert!(!queue.push(message.clone()), "running already");
    assert!(!queue.push(note_again.clone()), "replaces the waiting Note");
    assert!(!queue.contains(&note));
    assert_eq!(queue.take_next(), Some(note_again.clone()));
    assert!(!queue.contains(&message), "the earlier build ended");
    assert_eq!(queue.take_next(), None);
    assert!(queue.push(note.clone()), "a stopped worker starts again");
    queue.abandon();
    assert!(!queue.contains(&note));
    assert!(queue.push(note), "an abandoned queue starts a worker");

    // Two classes queued in one store both get ready.
    let s = TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    for i in 0..20u64 {
        store
            .add_link(&flag_link(&s, &format!("si://m/{i}"), T0 + i))
            .unwrap();
        store
            .add_link(&signed(
                &s,
                &format!("si://n/{i}"),
                TYPE,
                "si://Note",
                T0 + i,
            ))
            .unwrap();
    }
    let message = store.scope_index_ensure(&message_shape()).unwrap().unwrap();
    let note = store.scope_index_ensure(&note_shape()).unwrap().unwrap();
    wait_ready(&store, &message);
    wait_ready(&store, &note);
    assert_eq!(
        store.scope_reader().count(&message.class_scope()).unwrap(),
        20
    );
    assert_eq!(store.scope_reader().count(&note.class_scope()).unwrap(), 20);
}

/// A shape whose queries always go to SPARQL (its conformance is more than
/// the flag) gets no rule, no build and no upkeep on writes.
#[test]
fn a_shape_the_index_never_serves_is_not_registered() {
    let store = SparqlStore::new(None).unwrap();
    let shape = parse_shape_from_json(
        &format!(
            r#"{{"className":"Message","properties":{{
                "type":{{"predicate":"{TYPE}","required":true,"flag":true,"initial":"{MESSAGE}"}},
                "body":{{"predicate":"si://body","required":true}}
            }},"relations":{{}}}}"#
        ),
        "Message",
    )
    .unwrap();
    let rule = store.scope_index_ensure(&shape).unwrap();
    assert_eq!(
        rule, None,
        "a class whose queries always go to SPARQL should not get an index"
    );
    assert_eq!(store.scope_index_rule("Message"), None);
    assert!(
        store
            .scope_index_ensure(&message_shape())
            .unwrap()
            .is_some(),
        "a shape the index serves is registered"
    );
}

/// An executor that finds another layout version writes without index upkeep.
/// It removes the build marks, so an executor of the layout the marks were
/// written under does not read an index that misses those writes.
#[test]
fn writes_made_with_the_index_off_do_not_leave_it_ready() {
    use super::{Scope, VERSION_KEY};
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().to_str().unwrap().to_string();
    let s = TestSigner::generate();
    let set_version = |store: &SparqlStore, v: u8| {
        let mut tx = store.raw_store().start_transaction().unwrap();
        tx.aux_put(&VERSION_KEY, &[v]);
        tx.commit().unwrap();
    };
    let rule = {
        let store = SparqlStore::new(Some(&path)).unwrap();
        let (rule, _) = store
            .scope_index_register(&message_shape())
            .unwrap()
            .unwrap();
        store.scope_index_build_now(&rule).unwrap();
        store.add_link(&flag_link(&s, "si://m/1", T0)).unwrap();
        set_version(&store, 2); // now written by a newer layout
        rule
    };
    {
        let store = SparqlStore::new(Some(&path)).unwrap(); // index off, still writes
        assert_eq!(store.scope_index_readiness(&rule), Readiness::NotIndexed);
        store.add_link(&flag_link(&s, "si://m/2", T0 + 1)).unwrap();
        set_version(&store, 1); // back to the layout the marks were written under
    }
    let store = SparqlStore::new(Some(&path)).unwrap();
    let ready = store.scope_index_readiness(&rule) == Readiness::Ready;
    let page: Vec<String> = store
        .scope_reader()
        .page(
            &Scope::Class {
                class: rule.class.clone(),
            },
            None,
            10,
        )
        .unwrap()
        .members
        .into_iter()
        .map(|(m, _)| m)
        .collect();
    assert!(
        !ready || page.contains(&"si://m/2".to_string()),
        "the index reports Ready but misses a member written while it was off: {page:?}"
    );
    // Registered again, the class is built again and holds both members.
    let rule = store.scope_index_ensure(&message_shape()).unwrap().unwrap();
    wait_ready(&store, &rule);
    assert_eq!(store.scope_reader().count(&rule.class_scope()).unwrap(), 2);
    assert_consistent(&store);
}

/// The pause without the lock comes before every batch of a build but the
/// first, also when the build goes on in a later call (the worker runs a
/// build in calls of a few batches).
#[test]
fn the_pause_comes_before_every_batch_but_the_first() {
    let s = TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    for i in 0..3u64 {
        store
            .add_link(&flag_link(&s, &format!("si://m/{i}"), T0 + i))
            .unwrap();
    }
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    let pause = Duration::from_secs(1);
    let limits = BatchLimits {
        time: Duration::ZERO,
        pause,
    };
    let start = Instant::now();
    assert_eq!(
        store
            .scope_index_build_limited(&rule, Some(1), limits)
            .unwrap(),
        BuildProgress::Paused
    );
    assert!(start.elapsed() < pause, "no pause before the first batch");
    let start = Instant::now();
    assert_eq!(
        store
            .scope_index_build_limited(&rule, Some(1), limits)
            .unwrap(),
        BuildProgress::Paused
    );
    assert!(
        start.elapsed() >= pause,
        "the first batch of a later call pauses too: {:?}",
        start.elapsed()
    );
}

/// A changed flag while the build of the old one runs: the new rule's build
/// waits behind it and runs when the old one ends as superseded.
#[test]
fn a_new_rule_of_a_running_class_runs_after_it() {
    let old = ClassRule::new("Message", TYPE, MESSAGE);
    let new = ClassRule::new("Message", TYPE, "si://Message2");
    let mut queue = BuildQueue::default();
    assert!(queue.push(old.clone()));
    assert_eq!(queue.take_next(), Some(old.clone()));
    assert!(!queue.push(new.clone()), "the running worker takes it");
    assert!(queue.contains(&new));
    assert_eq!(
        queue.take_next(),
        Some(new.clone()),
        "the worker runs it after the superseded build"
    );
    assert_eq!(queue.take_next(), None);

    let s = TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    let mut links = Vec::new();
    for i in 0..(BATCH as u64 * 4) {
        let id = format!("si://m/{i}");
        links.push(flag_link(&s, &id, T0 + i));
        links.push(signed(&s, &id, TYPE, "si://Message2", T0 + i));
    }
    store.apply_diff(&[], &links).unwrap();
    let old = store.scope_index_ensure(&message_shape()).unwrap().unwrap();
    let new = store
        .scope_index_ensure(&shape_with_flag("si://Message2"))
        .unwrap()
        .unwrap();
    assert_ne!(old, new);
    wait_ready(&store, &new);
    assert_eq!(store.scope_index_readiness(&old), Readiness::NotIndexed);
    assert_eq!(
        store.scope_reader().count(&new.class_scope()).unwrap(),
        BATCH as i64 * 4
    );
    assert_consistent(&store);
}

/// A build that runs in calls of one batch (as the worker runs it in calls of
/// a few) scans and sorts the flag subjects once for its phase 1, not once
/// per call.
#[test]
fn one_build_takes_one_snapshot_across_its_calls() {
    let s = TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    for i in 0..3u64 {
        store
            .add_link(&flag_link(&s, &format!("si://m/{i}"), T0 + i))
            .unwrap();
    }
    let (rule, _) = store
        .scope_index_register(&message_shape())
        .unwrap()
        .unwrap();
    let limits = BatchLimits {
        time: Duration::ZERO,
        pause: Duration::ZERO,
    };
    let mut run = build::BuildRun::default();
    let mut calls = 0;
    loop {
        calls += 1;
        match store
            .scope_index_build_run(&rule, Some(1), limits, &mut run)
            .unwrap()
        {
            BuildProgress::Paused => continue,
            BuildProgress::Done => break,
            other => panic!("unexpected {other:?}"),
        }
    }
    assert_eq!(calls, 4, "phase 0, then one member per call");
    assert_eq!(run.scans(), 1);
    assert_eq!(store.scope_index_readiness(&rule), Readiness::Ready);
    assert_eq!(store.scope_reader().count(&rule.class_scope()).unwrap(), 3);
}

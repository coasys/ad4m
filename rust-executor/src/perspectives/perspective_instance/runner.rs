//! The one runner every model query goes through: a `perspective.modelQuery`
//! call, a model subscription's first run and its re-check after a write.
//!
//! Identical runs in flight share one store run. Sharing stops at the result:
//! each caller keeps its own socket, revision and lifetime, and gets its own
//! copy of the serialized rows.
//!
//! A run is spawned on its own, under its own deadline gate, and takes one of
//! the executor's blocking slots ([`RECHECK_SLOTS`]), so a burst of reads never
//! blocks async workers. A waiter that stops (its deadline, a cancel, a closed
//! socket) leaves the run going for the others; the last waiter to leave stops
//! it, as a lone caller's gate always did: a run still queued for a slot never
//! takes it, one running ends with its stop. A read made inside a run goes
//! through [`InRun`], never back through the runner.

use super::subscriptions::{blocking_check, RECHECK_SLOTS};
use super::PerspectiveInstance;
use crate::perspectives::flow_evaluator::RequiresQueryable;
use crate::perspectives::model_query::QueryClasses;
use crate::perspectives::sparql_store::{query_deadline, EvalGate, EvalStop};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use futures::future::{BoxFuture, Shared};
use futures::FutureExt;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio_util::sync::CancellationToken;

/// What a run is the answer to. Two calls with equal keys get the same rows.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) struct RunKey {
    classes: QueryClasses,
    /// The query text as the caller sent it: key order can matter (a
    /// multi-key `order` ranks by it), so no spelling is normalised. The SDK
    /// sends one text for one call. No agent: a result does not depend on who
    /// asks, so agents share a run.
    query: String,
    /// How long a missing class is waited for: it decides whether the run fails.
    shape_wait: Duration,
    /// The store's write generation when the call started: a read after a
    /// write never joins a run that began before it.
    writes: u64,
    /// The installed shapes' generation: a shape change alters a result
    /// without a link write.
    shapes: u64,
}

/// A settled run's answer; the error is its message.
type Reply = Result<Arc<str>, Arc<str>>;

struct Run {
    id: u64,
    reply: Shared<BoxFuture<'static, Reply>>,
    waiters: usize,
    /// Fired when the last waiter leaves.
    cancel: CancellationToken,
}

#[derive(Default)]
struct State {
    next_id: u64,
    runs: HashMap<RunKey, Run>,
}

/// A perspective's model query runs in flight.
#[derive(Clone, Default)]
pub(super) struct Runs(Arc<Mutex<State>>);

/// One waiter's claim on a run; leaving it (settled, stopped or dropped)
/// stops the run when no other waiter is left.
struct Waiter<'a> {
    runs: &'a Runs,
    key: RunKey,
    id: u64,
}

impl Drop for Waiter<'_> {
    fn drop(&mut self) {
        let mut state = self.runs.0.lock().unwrap_or_else(|e| e.into_inner());
        let Some(run) = state.runs.get_mut(&self.key).filter(|r| r.id == self.id) else {
            return;
        };
        run.waiters -= 1;
        if run.waiters == 0 {
            run.cancel.cancel();
            state.runs.remove(&self.key);
        }
    }
}

impl PerspectiveInstance {
    /// Run a model query through the runner and return its serialized result.
    /// `stop` ends this caller's wait, not a run others wait for.
    pub(super) async fn run_model_query(
        &self,
        stop: &EvalStop,
        classes: &QueryClasses,
        query_json: &str,
        shape_wait: Duration,
    ) -> Result<String, AnyError> {
        if let Some(err) = stop.stopped_err("query") {
            return Err(err);
        }
        let key = self.run_key(classes, query_json, shape_wait);
        let (reply, _waiter) = self.join_run(key, classes, query_json);
        let reply = tokio::select! {
            reply = reply => reply,
            err = stopped(stop) => return Err(err),
        };
        reply
            .map(|rows| rows.to_string())
            .map_err(|msg| anyhow!(msg.to_string()))
    }

    fn run_key(&self, classes: &QueryClasses, query_json: &str, shape_wait: Duration) -> RunKey {
        RunKey {
            classes: classes.clone(),
            query: query_json.to_string(),
            shape_wait,
            // Read before the run starts, as `write_generation` asks.
            writes: self.sparql_store.write_generation(),
            shapes: self.shapes_generation(),
        }
    }

    /// The run `key` names: the one in flight, or a new one.
    fn join_run(
        &self,
        key: RunKey,
        classes: &QueryClasses,
        query_json: &str,
    ) -> (Shared<BoxFuture<'static, Reply>>, Waiter<'_>) {
        let mut state = self.runs.0.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(run) = state.runs.get_mut(&key) {
            run.waiters += 1;
            let waiter = Waiter {
                runs: &self.runs,
                key,
                id: run.id,
            };
            return (run.reply.clone(), waiter);
        }
        state.next_id += 1;
        let id = state.next_id;
        let cancel = CancellationToken::new();
        let task = tokio::spawn({
            let perspective = self.clone();
            let classes = classes.clone();
            let query_json = query_json.to_string();
            let shape_wait = key.shape_wait;
            let cancel = cancel.clone();
            let key = key.clone();
            async move {
                let reply = perspective
                    .execute_run(classes, query_json, shape_wait, cancel)
                    .await;
                // Settled: later calls start their own run.
                let mut state = perspective.runs.0.lock().unwrap_or_else(|e| e.into_inner());
                if state.runs.get(&key).is_some_and(|r| r.id == id) {
                    state.runs.remove(&key);
                }
                reply
            }
        });
        let reply: BoxFuture<'static, Reply> = async move {
            task.await
                .unwrap_or_else(|e| Err(format!("model query run failed: {e}").into()))
        }
        .boxed();
        let reply = reply.shared();
        state.runs.insert(
            key.clone(),
            Run {
                id,
                reply: reply.clone(),
                waiters: 1,
                cancel,
            },
        );
        (
            reply,
            Waiter {
                runs: &self.runs,
                key,
                id,
            },
        )
    }

    /// The run itself, on a blocking slot and under its own deadline gate. A
    /// run nobody waits for any more leaves the slot queue without running.
    async fn execute_run(
        &self,
        classes: QueryClasses,
        query_json: String,
        shape_wait: Duration,
        cancel: CancellationToken,
    ) -> Reply {
        const CANCELLED: &str = "model query run cancelled";
        let permit = tokio::select! {
            permit = RECHECK_SLOTS.clone().acquire_owned() => {
                permit.expect("the re-check semaphore is never closed")
            }
            _ = cancel.cancelled() => return Err(CANCELLED.into()),
        };
        if cancel.is_cancelled() {
            return Err(CANCELLED.into());
        }
        let perspective = self.clone();
        let run = async move {
            #[cfg(test)]
            super::subscriptions::evaluations::note(&perspective.uuid);
            // The deadline starts once the slot is held. Dropping the gate
            // with the run stops the evaluation.
            let gate = EvalGate::request(query_deadline(), Some(cancel));
            perspective
                .model_query_classes_within(&gate.stop(), &classes, &query_json, shape_wait)
                .await
                .map(Arc::<str>::from)
                .map_err(|e| Arc::<str>::from(e.to_string()))
        };
        let runtime = tokio::runtime::Handle::current();
        tokio::task::spawn_blocking(blocking_check(runtime, permit, run.boxed()))
            .await
            .unwrap_or_else(|e| Err(format!("model query run failed: {e}").into()))
    }
}

/// Test-only: take every run slot but `left`, so a test runs with `left`
/// permits. Tests run serially, so the slots are free between them.
#[cfg(test)]
pub(crate) fn hold_all_but(left: usize) -> tokio::sync::OwnedSemaphorePermit {
    let free = RECHECK_SLOTS.available_permits();
    RECHECK_SLOTS
        .clone()
        .try_acquire_many_owned((free - left) as u32)
        .expect("the slots are free between tests")
}

/// The reads a query makes while it runs (the outputs of a `producedByFlow`
/// filter). It already holds a run slot and runs under the run's stop, so
/// they evaluate in that slot under that stop: going back through
/// [`PerspectiveInstance::run_model_query`] would queue for a second slot,
/// which with every slot held by such runs never frees.
pub(super) struct InRun<'a> {
    pub(super) perspective: &'a PerspectiveInstance,
    pub(super) stop: &'a EvalStop,
    pub(super) shape_wait: Duration,
}

#[async_trait::async_trait]
impl RequiresQueryable for InRun<'_> {
    async fn model_query(&self, class_name: &str, query_json: &str) -> Result<String, AnyError> {
        let classes = QueryClasses::One(class_name.to_string());
        self.perspective
            .model_query_classes_within(self.stop, &classes, query_json, self.shape_wait)
            .await
    }
}

/// Resolves, with its reason, once the caller's gate has stopped. The gate's
/// watchdog fires at 25 ms granularity, so this looks that often.
pub(super) async fn stopped(stop: &EvalStop) -> AnyError {
    loop {
        if let Some(err) = stop.stopped_err("query") {
            return err;
        }
        tokio::time::sleep(Duration::from_millis(25)).await;
    }
}

#[cfg(test)]
mod tests {
    use super::super::subscriptions::evaluations;
    use super::*;
    use crate::agent::signatures::TestSigner;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::types::{Link, LinkExpression, LinkStatus, PerspectiveDiff};
    use std::time::Instant;

    const TODO_SDNA: &str = r#"{
      "target_class": "test://Todo",
      "constructor_actions": [
        {"action":"addLink","source":"this","predicate":"rdf://type","target":"test://Todo"}
      ],
      "properties": [
        {"path":"test://title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
         "setter":[{"action":"setSingleTarget","source":"this","predicate":"test://title","target":"value"}]},
        {"path":"test://rank","name":"rank","datatype":"xsd:string","min_count":0,"max_count":1,"writable":true,
         "setter":[{"action":"setSingleTarget","source":"this","predicate":"test://rank","target":"value"}]}
      ]
    }"#;
    const QUERY: &str = r#"{"includeUnverified": true}"#;

    async fn perspective() -> PerspectiveInstance {
        setup_perspective_no_llm(&[("Todo", TODO_SDNA)]).await.0
    }

    fn signed(source: &str, predicate: &str, target: &str) -> LinkExpression {
        let data = Link {
            source: source.into(),
            predicate: Some(predicate.into()),
            target: target.into(),
        };
        let mut link = LinkExpression::from(TestSigner::generate().sign(data));
        link.status = Some(LinkStatus::Shared);
        link
    }

    fn write_todo(p: &PerspectiveInstance, n: usize) {
        for link in [
            signed(&format!("test://t{n}"), "rdf://type", "test://Todo"),
            signed(
                &format!("test://t{n}"),
                "test://title",
                "literal://string:t",
            ),
        ] {
            p.sparql_store.add_link(&link).unwrap();
        }
    }

    fn todos() -> QueryClasses {
        "Todo".into()
    }

    /// Every run slot taken, so a run queues until the permits drop.
    fn hold_slots() -> tokio::sync::OwnedSemaphorePermit {
        let free = RECHECK_SLOTS.available_permits();
        RECHECK_SLOTS
            .clone()
            .try_acquire_many_owned(free as u32)
            .expect("the slots are free between tests")
    }

    async fn settle() {
        for _ in 0..5 {
            tokio::task::yield_now().await;
        }
    }

    fn in_flight(p: &PerspectiveInstance) -> usize {
        p.runs.0.lock().unwrap().runs.len()
    }

    #[tokio::test]
    async fn identical_concurrent_queries_share_one_run() {
        let p = perspective().await;
        write_todo(&p, 1);
        let before = evaluations::count(&p.uuid);
        let replies = futures::future::join_all((0..8).map(|_| p.model_query("Todo", QUERY))).await;
        assert_eq!(evaluations::count(&p.uuid) - before, 1);
        let first = replies[0].as_ref().unwrap();
        assert!(first.contains("test://t1"), "{first}");
        assert!(replies.iter().all(|r| r.as_ref().unwrap() == first));
        assert_eq!(in_flight(&p), 0, "a settled run leaves the map");
    }

    #[tokio::test]
    async fn different_queries_and_classes_run_apart() {
        let p = perspective().await;
        let before = evaluations::count(&p.uuid);
        let paged = r#"{"includeUnverified": true, "limit": 1}"#;
        let (a, b) = tokio::join!(p.model_query("Todo", QUERY), p.model_query("Todo", paged));
        a.unwrap();
        b.unwrap();
        assert_eq!(evaluations::count(&p.uuid) - before, 2);
    }

    #[tokio::test]
    async fn a_settled_run_removed_for_a_new_one_is_not_touched_by_the_old_waiter() {
        let p = perspective().await;
        let slots = hold_slots();
        let key = p.run_key(&todos(), QUERY, Duration::ZERO);
        let (_, old) = p.join_run(key.clone(), &todos(), QUERY);
        // The old run settled and left the map, then a new run took its key.
        p.runs.0.lock().unwrap().runs.remove(&key);
        let (_, new) = p.join_run(key.clone(), &todos(), QUERY);
        assert_ne!(old.id, new.id);
        drop(old);
        assert_eq!(in_flight(&p), 1, "the old waiter left the new run alone");
        assert_eq!(p.runs.0.lock().unwrap().runs[&key].waiters, 1);
        drop(new);
        assert_eq!(in_flight(&p), 0);
        drop(slots);
    }

    #[tokio::test]
    async fn a_dropped_waiter_leaves_the_run_for_the_other() {
        let p = perspective().await;
        write_todo(&p, 1);
        let before = evaluations::count(&p.uuid);
        let slots = hold_slots();
        let leaving = tokio::spawn({
            let p = p.clone();
            async move { p.model_query("Todo", QUERY).await }
        });
        let staying = tokio::spawn({
            let p = p.clone();
            async move { p.model_query("Todo", QUERY).await }
        });
        settle().await;
        assert_eq!(in_flight(&p), 1);
        leaving.abort();
        let _ = leaving.await;
        settle().await;
        assert_eq!(in_flight(&p), 1, "the other waiter still has its run");
        drop(slots);
        let rows = staying.await.unwrap().unwrap();
        assert!(rows.contains("test://t1"), "{rows}");
        assert_eq!(evaluations::count(&p.uuid) - before, 1);
    }

    #[tokio::test]
    async fn the_last_waiter_leaving_stops_the_run() {
        let p = perspective().await;
        let slots = hold_slots();
        let before = evaluations::count(&p.uuid);
        let only = tokio::spawn({
            let p = p.clone();
            async move { p.model_query("Todo", QUERY).await }
        });
        settle().await;
        assert_eq!(in_flight(&p), 1);
        only.abort();
        let _ = only.await;
        assert_eq!(in_flight(&p), 0);
        drop(slots);
        // The run nobody wants never takes the slot it queued for.
        tokio::time::sleep(Duration::from_millis(200)).await;
        assert_eq!(evaluations::count(&p.uuid), before, "the run started");
        // A later call starts its own run and is answered.
        p.model_query("Todo", QUERY).await.unwrap();
        assert_eq!(evaluations::count(&p.uuid) - before, 1);
    }

    /// A run stopped while it waits for a class that is not stored yet ends
    /// with the stop, not after the shape-wait budget.
    #[tokio::test]
    async fn a_stopped_run_leaves_its_shape_wait() {
        let p = perspective().await;
        p.force_shape_sync_wait
            .store(true, std::sync::atomic::Ordering::SeqCst);
        let gate = EvalGate::request(Duration::from_millis(200), None);
        let started = Instant::now();
        let err = p
            .model_query_classes_within(
                &gate.stop(),
                &"Missing".into(),
                QUERY,
                Duration::from_secs(10),
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("SPARQL deadline"), "{err}");
        assert!(
            started.elapsed() < Duration::from_secs(3),
            "{:?}",
            started.elapsed()
        );
    }

    fn write_ranked(p: &PerspectiveInstance, n: usize, title: &str, rank: &str) {
        for (predicate, target) in [
            ("rdf://type", "test://Todo".to_string()),
            ("test://title", format!("literal://string:{title}")),
            ("test://rank", format!("literal://string:{rank}")),
        ] {
            p.sparql_store
                .add_link(&signed(&format!("test://t{n}"), predicate, &target))
                .unwrap();
        }
    }

    /// `order`'s key order is its priority: the same keys in another order are
    /// another query, with another first page.
    #[tokio::test]
    async fn reversed_multi_key_order_priority_runs_apart() {
        let p = perspective().await;
        write_ranked(&p, 1, "a", "2");
        write_ranked(&p, 2, "b", "1");
        let by_title =
            r#"{"includeUnverified": true, "order": {"title": "ASC", "rank": "ASC"}, "limit": 1}"#;
        let by_rank =
            r#"{"includeUnverified": true, "order": {"rank": "ASC", "title": "ASC"}, "limit": 1}"#;
        let before = evaluations::count(&p.uuid);
        let (a, b) = tokio::join!(
            p.model_query("Todo", by_title),
            p.model_query("Todo", by_rank)
        );
        let (a, b) = (a.unwrap(), b.unwrap());
        assert!(a.contains("test://t1") && !a.contains("test://t2"), "{a}");
        assert!(b.contains("test://t2") && !b.contains("test://t1"), "{b}");
        assert_eq!(evaluations::count(&p.uuid) - before, 2);
    }

    #[tokio::test]
    async fn a_waiters_own_stop_ends_its_wait_only() {
        let p = perspective().await;
        write_todo(&p, 1);
        let slots = hold_slots();
        let gate = EvalGate::request(Duration::from_millis(100), None);
        let stop = gate.stop();
        let classes = todos();
        let stopping = p.model_query_classes_stopped(&stop, &classes, QUERY);
        let staying = tokio::spawn({
            let p = p.clone();
            async move { p.model_query("Todo", QUERY).await }
        });
        let err = stopping.await.unwrap_err().to_string();
        assert!(err.contains("SPARQL deadline"), "{err}");
        drop(slots);
        assert!(staying.await.unwrap().unwrap().contains("test://t1"));
    }

    #[tokio::test]
    async fn a_write_between_two_queries_runs_the_second_again() {
        let p = perspective().await;
        let before = evaluations::count(&p.uuid);
        let first = p.model_query("Todo", QUERY).await.unwrap();
        write_todo(&p, 1);
        let second = p.model_query("Todo", QUERY).await.unwrap();
        assert_eq!(evaluations::count(&p.uuid) - before, 2);
        assert!(!first.contains("test://t1") && second.contains("test://t1"));
    }

    #[tokio::test]
    async fn a_read_after_a_write_does_not_join_an_older_run() {
        let p = perspective().await;
        let slots = hold_slots();
        let before = evaluations::count(&p.uuid);
        let older = tokio::spawn({
            let p = p.clone();
            async move { p.model_query("Todo", QUERY).await }
        });
        settle().await;
        write_todo(&p, 1);
        let newer = tokio::spawn({
            let p = p.clone();
            async move { p.model_query("Todo", QUERY).await }
        });
        settle().await;
        assert_eq!(in_flight(&p), 2);
        drop(slots);
        older.await.unwrap().unwrap();
        assert!(newer.await.unwrap().unwrap().contains("test://t1"));
        assert_eq!(evaluations::count(&p.uuid) - before, 2);
    }

    #[tokio::test]
    async fn a_shape_change_runs_the_next_query_again() {
        let p = perspective().await;
        let before = evaluations::count(&p.uuid);
        // Every slot taken: the first run stays in flight, so the second call
        // meets its entry and only its key can tell them apart.
        let slots = hold_slots();
        let call = |p: &PerspectiveInstance| {
            let p = p.clone();
            tokio::spawn(async move { p.model_query("Todo", QUERY).await })
        };
        let first = call(&p);
        settle().await;
        assert_eq!(in_flight(&p), 1);
        // Control: the same call, shapes unchanged, joins the first entry.
        let joined = call(&p);
        settle().await;
        assert_eq!(in_flight(&p), 1);
        let shapes = p.shapes_generation();
        p.abstract_cache.write().unwrap().generation += 1;
        assert_ne!(p.shapes_generation(), shapes);
        let after_change = call(&p);
        settle().await;
        assert_eq!(in_flight(&p), 2, "a shape change is a distinct entry");
        drop(slots);
        for call in [first, joined, after_change] {
            call.await.unwrap().unwrap();
        }
        assert_eq!(evaluations::count(&p.uuid) - before, 2);
    }

    #[tokio::test]
    async fn a_peer_diff_advances_the_write_generation() {
        let p = perspective().await;
        let link = signed("test://peer", "rdf://type", "test://Todo");
        let present = |p: &PerspectiveInstance| {
            p.sparql_store
                .get_link(
                    &EvalStop::none(),
                    "test://peer",
                    Some("rdf://type"),
                    "test://Todo",
                    &link.author,
                    &link.timestamp,
                )
                .unwrap()
                .is_some()
        };
        let before = p.sparql_store.write_generation();
        p.diff_from_link_language(PerspectiveDiff::from_additions(vec![link.clone()]))
            .await
            .unwrap();
        assert!(present(&p), "the added link is stored");
        let after_add = p.sparql_store.write_generation();
        assert!(after_add > before, "an addition advances the generation");
        let mut removal = PerspectiveDiff::from_additions(vec![]);
        removal.removals = vec![link.clone()];
        p.diff_from_link_language(removal).await.unwrap();
        assert!(!present(&p), "the removed link is gone");
        assert!(
            p.sparql_store.write_generation() > after_add,
            "a removal advances the generation"
        );
    }

    #[tokio::test]
    async fn many_subscriptions_on_one_query_run_one_recheck_per_write() {
        let p = perspective().await;
        // Main agent's and two managed agents' subscriptions: the result does
        // not depend on the agent, so they share one re-check.
        let subscribers = [
            None,
            None,
            Some("did:test:alice"),
            Some("did:test:bob"),
            None,
        ];
        for (n, subscriber) in subscribers.into_iter().enumerate() {
            p.model_subscribe_and_query(
                &EvalStop::none(),
                todos(),
                QUERY.into(),
                uuid::Uuid::new_v4().to_string(),
                format!("c{n}"),
                subscriber.map(String::from),
            )
            .await
            .unwrap();
        }
        for n in 1..=2 {
            write_todo(&p, n);
            let before = evaluations::count(&p.uuid);
            p.check_subscribed_queries(super::super::ChangedPredicates::CheckAll)
                .await;
            assert_eq!(evaluations::count(&p.uuid) - before, 1, "write {n}");
        }
    }

    /// N identical concurrent model queries: each on its own, as before the
    /// runner (every call evaluates on an async worker), and through it.
    /// `cargo test --release --lib runner_benchmark -- --ignored --nocapture`
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    #[ignore = "a benchmark: run it in release"]
    async fn runner_benchmark() {
        let p = perspective().await;
        for n in 0..3000 {
            write_todo(&p, n);
        }
        let calls = 64;
        let classes = todos();
        let report = |label: &str, started: Instant, runs: usize| {
            println!(
                "{label}: {calls} identical concurrent queries, {runs} store runs, {:?}",
                started.elapsed()
            );
        };
        for round in 0..2 {
            let started = Instant::now();
            let runs = futures::future::join_all((0..calls).map(|_| {
                let (p, classes) = (p.clone(), classes.clone());
                tokio::spawn(async move {
                    evaluations::note(&p.uuid);
                    let gate = EvalGate::request(query_deadline(), None);
                    p.model_query_classes_within(&gate.stop(), &classes, QUERY, Duration::ZERO)
                        .await
                })
            }))
            .await;
            runs.into_iter().for_each(|r| {
                r.unwrap().unwrap();
            });
            report(&format!("round {round} without the runner"), started, calls);

            let before = evaluations::count(&p.uuid);
            let started = Instant::now();
            let replies = futures::future::join_all((0..calls).map(|_| {
                let (p, classes) = (p.clone(), classes.clone());
                tokio::spawn(async move { p.model_query_classes(&classes, QUERY).await })
            }))
            .await;
            replies.into_iter().for_each(|r| {
                r.unwrap().unwrap();
            });
            report(
                &format!("round {round} with the runner"),
                started,
                evaluations::count(&p.uuid) - before,
            );
        }
    }
}

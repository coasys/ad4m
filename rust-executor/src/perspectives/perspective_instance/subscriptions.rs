//! Live query subscriptions on `PerspectiveInstance`: delta updates, resync
//! and disposal. A subscription belongs to the RPC connection that opened it.
//!
//! A child module of `perspective_instance` so it can reach the private
//! subscription registry without widening its visibility.

use deno_core::error::AnyError;
use futures::future::BoxFuture;
use serde_json::{json, Map, Value};
use std::collections::HashMap;

use super::{PerspectiveInstance, SubscribedQuery};
use crate::agent::{did_for_context, AgentContext};
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub;

impl PerspectiveInstance {
    /// Resync after a revision gap (`perspective.resyncSubscription`): the
    /// subscription's current revision and the result it describes, read
    /// together under the registry lock. `None` unless `connection_id`
    /// opened `subscription_id`.
    pub async fn subscription_state(
        &self,
        subscription_id: &str,
        connection_id: &str,
    ) -> Option<(u64, String)> {
        let queries = self.subscribed_queries.lock().await;
        let query = queries
            .get(subscription_id)
            .filter(|q| q.connection == connection_id)?;
        Some((query.revision, query.last_result.clone()))
    }

    /// Register `query` under a new id, then compute its first result with
    /// `first_result`. Registering first means a write that lands meanwhile
    /// is still checked, and reported. Until the first result is stored,
    /// `last_result` is empty, so an update published before then carries
    /// the whole `result`. Returns the id, and the revision and result the
    /// subscribe reply carries: revision 0 and the first result, or the
    /// newer revision a check already published.
    pub(super) async fn open_subscription(
        &self,
        query: SubscribedQuery,
        first_result: impl std::future::Future<Output = Result<String, AnyError>>,
    ) -> Result<(String, u64, String), AnyError> {
        let id = uuid::Uuid::new_v4().to_string();
        log::info!(
            "🔗 subscription {} created for connection {} [{}]",
            id,
            query.connection,
            self.uuid
        );
        self.subscribed_queries
            .lock()
            .await
            .insert(id.clone(), query);
        // A call the client cancels (`request.cancel`, or its connection closing) drops this
        // future while the first result computes. Take the subscription out again then, or it
        // stays registered and re-runs on every change for a query nobody holds (#1595).
        let mut unregister = UnregisterOnDrop {
            queries: self.subscribed_queries.clone(),
            id: Some(id.clone()),
        };
        let first = first_result.await;
        // Disarm only with the lock held: a call dropped while it waits for the lock gets no
        // reply either, so nobody would ever hold its subscription.
        let mut queries = self.subscribed_queries.lock().await;
        unregister.id = None;
        let result = match first {
            Ok(result) => result,
            Err(e) => {
                queries.remove(&id);
                return Err(e);
            }
        };
        Ok(match queries.get_mut(&id) {
            Some(q) if q.revision > 0 => (id, q.revision, q.last_result.clone()),
            Some(q) => {
                // A model trigger was built before this result existed;
                // point it at the nodes of the first result, so an edit to
                // an instance in it re-runs the query (#1237).
                if let Some(params) = q.model_query_params.as_mut() {
                    params.trigger = params.trigger.with_result(&result);
                }
                q.last_result = result.clone();
                (id, 0, result)
            }
            None => (id, 0, result),
        })
    }

    /// Publish one `query-subscription-update` event:
    /// `{ perspectiveUuid, uuid, subscriptionId, revision, ... }`
    /// (`QuerySubscriptionUpdate`) plus the change (see [`result_delta`]).
    ///
    /// It goes to the socket of the RPC connection that opened the query
    /// (`Event::for_connection`), and there only if that socket's session is
    /// in the audience and may query the perspective. The audience is the
    /// subscriber (`SubscribedQuery::subscriber`, `None` for the main agent)
    /// while it is one of the perspective's owners
    /// ([`super::super::event_recipients`], read here): the query ran as the
    /// subscriber, so a co-owner does not get its results, and an agent that
    /// lost the perspective gets nothing more.
    pub(crate) async fn send_delta_update(
        &self,
        subscription_id: String,
        connection_id: String,
        subscriber: Option<String>,
        revision: u64,
        mut payload: Map<String, Value>,
    ) {
        payload.insert("perspectiveUuid".into(), json!(self.uuid));
        payload.insert("uuid".into(), json!(self.uuid));
        payload.insert("subscriptionId".into(), json!(subscription_id));
        payload.insert("revision".into(), json!(revision));
        let main = || did_for_context(&AgentContext::main_agent()).ok();
        let subscriber = subscriber.or_else(main);
        let audience = super::super::event_recipients(&*self.persisted.lock().await)
            .into_iter()
            .filter(|owner| Some(owner) == subscriber.as_ref())
            .collect();
        let event = Event::new(
            Topic::PerspectiveQuerySubscription,
            Audience::Dids(audience),
            Value::Object(payload),
        )
        .for_connection(connection_id);
        get_global_pubsub().await.publish_event(event);
    }

    /// End one subscription that `connection_id` opened. `false` when there
    /// is no such subscription.
    pub async fn dispose_query_subscription(
        &self,
        subscription_id: &str,
        connection_id: &str,
    ) -> bool {
        self.dispose_where(|id, q| id == subscription_id && q.connection == connection_id)
            .await
            > 0
    }

    /// End every subscription `connection_id` opened (its socket closed).
    pub async fn dispose_connection_subscriptions(&self, connection_id: &str) -> usize {
        self.dispose_where(|_, q| q.connection == connection_id)
            .await
    }

    async fn dispose_where(&self, matches: impl Fn(&str, &SubscribedQuery) -> bool) -> usize {
        let removed = {
            let mut queries = self.subscribed_queries.lock().await;
            let before = queries.len();
            queries.retain(|id, q| !matches(id, q));
            before - queries.len()
        };
        if removed > 0 {
            log::info!("🔗 {} subscription(s) dropped [{}]", removed, self.uuid);
        }
        removed
    }
}

/// Parse a stored result string as JSON; a string that is not JSON stays a
/// JSON string.
pub(crate) fn result_json(result: &str) -> Value {
    serde_json::from_str(result).unwrap_or_else(|_| Value::String(result.to_string()))
}

/// The change from `old` to `new`.
///
/// - Model results (`{ instances, totalCount }`): `ids` (the new order, which
///   also defines membership), `upsert` (instances that are new or differ) and
///   `totalCount`.
/// - Query results (a JSON array of rows): a row has no id, so rows are a
///   multiset: `added` and `removed` rows, or `result` when applying them
///   would not give the new row order.
/// - Anything else: `result`, which replaces the old result.
pub(crate) fn result_delta(old: &str, new: &str, is_model: bool) -> Map<String, Value> {
    let (old, new) = (result_json(old), result_json(new));
    let delta = if is_model {
        model_delta(&old, &new)
    } else {
        rows_delta(&old, &new)
    };
    delta.unwrap_or_else(|| Map::from_iter([("result".into(), new)]))
}

fn model_delta(old: &Value, new: &Value) -> Option<Map<String, Value>> {
    let instances = |v: &Value| -> Option<Vec<(String, Value)>> {
        v.get("instances")?
            .as_array()?
            .iter()
            .map(|i| Some((i.get("id")?.as_str()?.to_string(), i.clone())))
            .collect()
    };
    let old: HashMap<String, Value> = instances(old)?.into_iter().collect();
    let (ids, upsert): (Vec<String>, Vec<Option<Value>>) = instances(new)?
        .into_iter()
        .map(|(id, v)| {
            let changed = old.get(&id) != Some(&v);
            (id, changed.then_some(v))
        })
        .unzip();
    Some(Map::from_iter([
        ("ids".into(), json!(ids)),
        (
            "upsert".into(),
            json!(upsert.into_iter().flatten().collect::<Vec<_>>()),
        ),
        (
            "totalCount".into(),
            new.get("totalCount").cloned().unwrap_or(Value::Null),
        ),
    ]))
}

fn rows_delta(old: &Value, new: &Value) -> Option<Map<String, Value>> {
    let (old, new) = (old.as_array()?, new.as_array()?);
    // Count each old row once by its serialised form; new rows consume counts.
    let mut unmatched: HashMap<String, (usize, &Value)> = HashMap::new();
    for row in old {
        unmatched.entry(row.to_string()).or_insert((0, row)).0 += 1;
    }
    let mut added = vec![];
    for row in new {
        match unmatched.get_mut(&row.to_string()) {
            Some((n, _)) if *n > 0 => *n -= 1,
            _ => added.push(row),
        }
    }
    let removed: Vec<&Value> = unmatched
        .into_values()
        .flat_map(|(n, row)| std::iter::repeat_n(row, n))
        .collect();
    // The client drops the first match of each removed row, then appends the
    // added rows. When that does not give `new` in order (an `ORDER BY`
    // put a row mid-list), send the whole result instead.
    let mut expected: Vec<&Value> = old.iter().collect();
    for row in &removed {
        if let Some(i) = expected.iter().position(|v| v == row) {
            expected.remove(i);
        }
    }
    expected.extend(added.iter().copied());
    if !expected.into_iter().eq(new.iter()) {
        return None;
    }
    Some(Map::from_iter([
        ("added".into(), json!(added)),
        ("removed".into(), json!(removed)),
    ]))
}

/// Removes subscription `id` from `queries` when dropped, unless `id` was taken
/// first: what `open_subscription` leaves behind when its caller drops it before
/// the first result is in.
struct UnregisterOnDrop {
    queries: std::sync::Arc<tokio::sync::Mutex<HashMap<String, SubscribedQuery>>>,
    id: Option<String>,
}

impl Drop for UnregisterOnDrop {
    fn drop(&mut self) {
        let Some(id) = self.id.take() else { return };
        // Drop cannot await the lock: take it if free, else remove from a task.
        if let Ok(mut queries) = self.queries.try_lock() {
            queries.remove(&id);
            return;
        }
        let queries = self.queries.clone();
        if let Ok(runtime) = tokio::runtime::Handle::try_current() {
            runtime.spawn(async move {
                queries.lock().await.remove(&id);
            });
        }
    }
}

/// The most subscription re-checks of one pass that run at once.
const MAX_CONCURRENT_RECHECKS: usize = 8;

/// How many re-checks of one pass run at once: one per core, at most
/// [`MAX_CONCURRENT_RECHECKS`]. A perspective can hold many subscriptions,
/// and each running re-check holds a blocking-pool thread.
pub(super) fn recheck_concurrency() -> usize {
    std::thread::available_parallelism()
        .map_or(1, |n| n.get())
        .min(MAX_CONCURRENT_RECHECKS)
}

/// Run one pass's subscription re-checks off the async workers, at most
/// `limit` at a time, and return their outputs in the order they finish.
///
/// A re-check reads the SPARQL store synchronously: a raw SPARQL
/// subscription through `query_arbitrary`, a model subscription in parts
/// of `execute_model_query`. Each re-check runs on a blocking-pool thread
/// that drives its future with `Handle::block_on`, so the store work never
/// holds an async worker, nor the task that runs the perspective's
/// background loops (`start_background_tasks` joins them on one task).
/// A re-check is spawned only when a slot is free; one that panics is
/// logged and yields nothing.
pub(super) async fn run_rechecks<T: Send + 'static>(
    checks: Vec<BoxFuture<'static, T>>,
    limit: usize,
) -> Vec<T> {
    let runtime = tokio::runtime::Handle::current();
    let mut waiting = checks.into_iter();
    let mut running = tokio::task::JoinSet::new();
    let mut outputs = Vec::with_capacity(waiting.len());
    loop {
        while running.len() < limit.max(1) {
            let Some(check) = waiting.next() else { break };
            let runtime = runtime.clone();
            running.spawn_blocking(move || runtime.block_on(check));
        }
        match running.join_next().await {
            Some(Ok(output)) => outputs.push(output),
            Some(Err(e)) => log::error!("❌ 🔗 subscription re-check failed: {}", e),
            None => return outputs,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::ChangedPredicates;
    use super::result_delta;
    use crate::agent::AgentContext;
    use crate::events::Topic;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::pubsub::get_global_pubsub;
    use crate::types::{Link, LinkStatus};
    use serde_json::{json, Value};
    use std::time::Duration;

    // ── Delta updates ───────────────────────────────────────────────────

    #[test]
    fn model_delta_sends_the_new_order_and_changed_instances() {
        let old = json!({ "instances": [
            { "id": "a", "title": "A" }, { "id": "b", "title": "B" }, { "id": "c", "title": "C" }
        ], "totalCount": 3 });
        let new = json!({ "instances": [
            { "id": "c", "title": "C" }, { "id": "b", "title": "B2" }, { "id": "d", "title": "D" }
        ], "totalCount": 3 });
        let d = Value::Object(result_delta(&old.to_string(), &new.to_string(), true));
        assert_eq!(
            d,
            json!({
                "ids": ["c", "b", "d"],
                "upsert": [{ "id": "b", "title": "B2" }, { "id": "d", "title": "D" }],
                "totalCount": 3
            })
        );
    }

    #[test]
    fn query_delta_is_a_row_multiset() {
        let old = json!([{ "s": "x" }, { "s": "x" }, { "s": "y" }]);
        let new = json!([{ "s": "x" }, { "s": "z" }]);
        let d = result_delta(&old.to_string(), &new.to_string(), false);
        assert_eq!(d["added"], json!([{ "s": "z" }]));
        let mut removed = d["removed"].as_array().unwrap().clone();
        removed.sort_by_key(|r| r.to_string());
        assert_eq!(removed, vec![json!({ "s": "x" }), json!({ "s": "y" })]);
    }

    #[test]
    fn ordered_insertion_mid_list_sends_the_whole_result() {
        let old = json!([{ "s": "a" }, { "s": "c" }]);
        let new = json!([{ "s": "a" }, { "s": "b" }, { "s": "c" }]);
        let d = Value::Object(result_delta(&old.to_string(), &new.to_string(), false));
        assert_eq!(d, json!({ "result": new }));
    }

    #[test]
    fn appended_rows_stay_a_delta() {
        let old = json!([{ "s": "a" }, { "s": "b" }, { "s": "c" }]);
        let new = json!([{ "s": "a" }, { "s": "c" }, { "s": "d" }]);
        let d = Value::Object(result_delta(&old.to_string(), &new.to_string(), false));
        assert_eq!(
            d,
            json!({ "added": [{ "s": "d" }], "removed": [{ "s": "b" }] })
        );
    }

    #[test]
    fn unkeyable_results_are_replaced() {
        let d = Value::Object(result_delta("true", "false", false));
        assert_eq!(d, json!({ "result": false }));
        let d = Value::Object(result_delta("[]", "not json", true));
        assert_eq!(d, json!({ "result": "not json" }));
    }

    const TODO_SDNA: &str = r#"{
      "target_class": "test://Todo",
      "constructor_actions": [
        {"action":"addLink","source":"this","predicate":"rdf://type","target":"test://Todo"}
      ],
      "properties": [
        {"path":"test://title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
         "setter":[{"action":"setSingleTarget","source":"this","predicate":"test://title","target":"value"}]}
      ]
    }"#;

    async fn add(p: &mut super::PerspectiveInstance, source: &str, predicate: &str, target: &str) {
        p.add_link(
            Link {
                source: source.into(),
                predicate: Some(predicate.into()),
                target: target.into(),
            },
            LinkStatus::Local,
            None,
            &AgentContext::main_agent(),
        )
        .await
        .unwrap();
    }

    /// Updates published for `id` after one subscription check: each payload,
    /// with the event's routing key (`Event::connection`) as `connectionId`.
    async fn updates_after_check(p: &super::PerspectiveInstance, id: &str) -> Vec<Value> {
        let mut rx = get_global_pubsub().await.subscribe_events();
        p.check_subscribed_queries(ChangedPredicates::CheckAll)
            .await;
        let mut out = vec![];
        let deadline = tokio::time::Instant::now() + Duration::from_millis(300);
        while let Ok(Ok(event)) = tokio::time::timeout_at(deadline, rx.recv()).await {
            if event.topic != Topic::PerspectiveQuerySubscription
                || event.payload["subscriptionId"] != id
            {
                continue;
            }
            let mut v = event.payload.clone();
            v["connectionId"] = json!(event.connection);
            out.push(v);
        }
        out
    }

    #[tokio::test]
    async fn model_subscription_sends_keyed_changes_with_revisions() {
        let (mut p, _, _) = setup_perspective_no_llm(&[("Todo", TODO_SDNA)]).await;
        let query = r#"{"includeUnverified": true}"#.to_string();
        let (id, _, initial) = p
            .model_subscribe_and_query("Todo".into(), query.clone(), "c".into(), None)
            .await
            .unwrap();
        let initial: Value = serde_json::from_str(&initial).unwrap();
        assert_eq!(initial["instances"], json!([]));

        add(&mut p, "test://t1", "rdf://type", "test://Todo").await;
        add(&mut p, "test://t1", "test://title", "literal://string:one").await;
        let updates = updates_after_check(&p, &id).await;
        assert_eq!(updates.len(), 1, "{updates:?}");
        let u = &updates[0];
        assert_eq!(u["revision"], json!(1));
        assert_eq!(u["connectionId"], json!("c"), "addressed to its connection");
        assert_eq!(u["upsert"][0]["id"], json!("test://t1"));
        assert_eq!(u["ids"], json!(["test://t1"]));
        assert!(u.get("result").is_none(), "no whole result on a delta");
        let typed: crate::types::QuerySubscriptionUpdate =
            serde_json::from_value(u.clone()).expect("the event_specs payload type");
        assert_eq!(typed.perspective_uuid, p.uuid, "events.watch scopes by it");
        assert_eq!(typed.revision, 1);

        // Nothing changed: no update, revision stays.
        assert!(updates_after_check(&p, &id).await.is_empty());

        add(&mut p, "test://t2", "rdf://type", "test://Todo").await;
        add(&mut p, "test://t2", "test://title", "literal://string:two").await;
        let u = &updates_after_check(&p, &id).await[0];
        assert_eq!(u["revision"], json!(2));
        assert_eq!(
            u["upsert"].as_array().unwrap().len(),
            1,
            "t1 is unchanged: {u}"
        );
        assert_eq!(u["upsert"][0]["id"], json!("test://t2"));
        assert_eq!(u["totalCount"], json!(2));
    }

    #[tokio::test]
    async fn resync_state_matches_the_last_update() {
        let (mut p, _, _) = setup_perspective_no_llm(&[]).await;
        let q = "SELECT ?s ?o WHERE { ?s <test://p> ?o }".to_string();
        let (id, _, _) = p
            .subscribe_and_query(q.clone(), "c".into(), None)
            .await
            .unwrap();
        let (revision, result) = p.subscription_state(&id, "c").await.unwrap();
        assert_eq!((revision, super::result_json(&result)), (0, json!([])));

        add(&mut p, "test://a", "test://p", "test://b").await;
        let updates = updates_after_check(&p, &id).await;
        assert_eq!(updates[0]["revision"], json!(1));
        let (revision, result) = p.subscription_state(&id, "c").await.unwrap();
        assert_eq!(revision, 1, "the revision of the last update sent");
        let rows = super::result_json(&result);
        assert_eq!(rows.as_array().unwrap().len(), 1);
        assert_eq!(
            json!(updates[0]["added"]),
            rows,
            "the result the update led to"
        );
    }

    #[tokio::test]
    async fn a_write_while_the_first_result_is_computed_is_reported() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let q = "SELECT ?s ?o WHERE { ?s <test://p> ?o }".to_string();
        let query = super::SubscribedQuery::new(q.clone(), "c".into(), None);
        // The first result is read, then a write and its check land before
        // the subscribe completes.
        let stale = p.sparql_query(q).unwrap();
        let mut writer = p.clone();
        let first_result = async move {
            add(&mut writer, "test://a", "test://p", "test://b").await;
            writer
                .check_subscribed_queries(ChangedPredicates::CheckAll)
                .await;
            Ok(stale)
        };
        let (id, revision, result) = p.open_subscription(query, first_result).await.unwrap();
        assert_eq!(revision, 1, "the reply carries the write");
        assert_eq!(super::result_json(&result).as_array().unwrap().len(), 1);
        assert_eq!(p.subscription_state(&id, "c").await.unwrap().0, 1);
    }

    #[tokio::test]
    async fn each_subscriber_gets_its_own_subscription_from_revision_zero() {
        let (mut p, _, _) = setup_perspective_no_llm(&[]).await;
        let q = "SELECT ?s ?o WHERE { ?s <test://p> ?o }".to_string();
        let (first, _, _) = p
            .subscribe_and_query(q.clone(), "c".into(), None)
            .await
            .unwrap();
        add(&mut p, "test://a", "test://p", "test://b").await;
        assert_eq!(
            updates_after_check(&p, &first).await[0]["revision"],
            json!(1)
        );

        let (second, _, _) = p.subscribe_and_query(q, "c".into(), None).await.unwrap();
        assert_ne!(first, second, "not shared");
        assert_eq!(p.subscription_state(&second, "c").await.unwrap().0, 0);
    }

    #[tokio::test]
    async fn a_prolog_live_query_is_refused_and_not_registered() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let err = p
            .subscribe_and_query(r#"triple(X, _, "test://b")."#.into(), "c".into(), None)
            .await
            .expect_err("a Prolog live query is refused");
        assert_eq!(err.to_string(), super::super::PROLOG_LIVE_QUERY_UNSUPPORTED);
        assert!(p.subscribed_queries.lock().await.is_empty());
    }

    #[tokio::test]
    async fn a_subscribe_dropped_before_its_first_result_is_not_left_registered() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let query = super::SubscribedQuery::new(
            "SELECT ?s ?o WHERE { ?s <test://p> ?o }".into(),
            "c".into(),
            None,
        );
        // The client cancels (or its connection closes) while the first
        // result is still being computed: the call future is dropped (#1595).
        let never = std::future::pending::<Result<String, deno_core::error::AnyError>>();
        let opening = p.open_subscription(query, never);
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(50), opening)
                .await
                .is_err(),
            "still computing when dropped"
        );
        tokio::task::yield_now().await;
        assert!(p.subscribed_queries.lock().await.is_empty());
    }

    #[tokio::test]
    async fn a_subscribe_dropped_while_it_stores_its_first_result_is_not_left_registered() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let query = super::SubscribedQuery::new(
            "SELECT ?s ?o WHERE { ?s <test://p> ?o }".into(),
            "c".into(),
            None,
        );
        let (computed, first_result) = tokio::sync::oneshot::channel();
        let mut opening =
            Box::pin(p.open_subscription(query, async move { first_result.await.unwrap() }));
        let short = std::time::Duration::from_millis(20);
        assert!(tokio::time::timeout(short, opening.as_mut()).await.is_err());
        // A check of the live queries holds the registry as the first result comes in,
        // and the client cancels while the call waits for it.
        let held = p.subscribed_queries.lock().await;
        computed.send(Ok("[]".to_string())).unwrap();
        assert!(
            tokio::time::timeout(short, opening.as_mut()).await.is_err(),
            "waiting for the registry"
        );
        drop(opening);
        drop(held);
        tokio::time::sleep(short).await;
        assert!(p.subscribed_queries.lock().await.is_empty());
    }

    // ── Re-check passes ─────────────────────────────────────────────────

    /// A pass never runs more re-checks at once than its limit, and every
    /// re-check's output comes back.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn a_pass_runs_at_most_its_limit_of_rechecks_at_once() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::Arc;

        let running = Arc::new(AtomicUsize::new(0));
        let peak = Arc::new(AtomicUsize::new(0));
        let checks = (0..20usize)
            .map(|i| {
                let (running, peak) = (running.clone(), peak.clone());
                Box::pin(async move {
                    let now = running.fetch_add(1, Ordering::SeqCst) + 1;
                    peak.fetch_max(now, Ordering::SeqCst);
                    tokio::time::sleep(Duration::from_millis(20)).await;
                    running.fetch_sub(1, Ordering::SeqCst);
                    i
                }) as futures::future::BoxFuture<'static, usize>
            })
            .collect();

        let mut outputs = super::run_rechecks(checks, 3).await;

        outputs.sort();
        assert_eq!(outputs, (0..20).collect::<Vec<_>>());
        assert_eq!(peak.load(Ordering::SeqCst), 3);
    }

    /// A re-check runs on a blocking-pool thread, not on an async worker.
    #[tokio::test]
    async fn a_recheck_runs_off_the_async_workers() {
        let caller = std::thread::current().id();
        let check = Box::pin(async { std::thread::current().id() })
            as futures::future::BoxFuture<'static, std::thread::ThreadId>;

        let ran_on = super::run_rechecks(vec![check], 1).await;

        assert_eq!(ran_on.len(), 1);
        assert_ne!(ran_on[0], caller);
    }

    /// A re-check that panics yields nothing; the rest of the pass is kept.
    #[tokio::test]
    async fn a_panicking_recheck_drops_only_its_own_output() {
        let checks: Vec<futures::future::BoxFuture<'static, u8>> = vec![
            Box::pin(async { 1 }),
            Box::pin(async { panic!("re-check failed") }),
            Box::pin(async { 3 }),
        ];

        let mut outputs = super::run_rechecks(checks, 2).await;

        outputs.sort();
        assert_eq!(outputs, vec![1, 3]);
    }
}

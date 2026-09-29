//! Live query subscriptions on `PerspectiveInstance`: delta updates, resync
//! and disposal. A subscription belongs to the RPC connection that opened it.
//!
//! A child module of `perspective_instance` so it can reach the private
//! subscription registry without widening its visibility.

use serde_json::{json, Map, Value};
use std::collections::{HashMap, HashSet};

use super::{PerspectiveInstance, SubscribedQuery};
use crate::prolog_service::get_prolog_service;
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};

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

    /// Publish one update on the `query-subscription-update` topic:
    /// `{ uuid, subscriptionId, revision, added, removed, changed, ... }` (see
    /// [`result_delta`]) plus `connectionId`, which the RPC socket uses to
    /// deliver it to the subscription's own connection only.
    pub(super) async fn send_delta_update(
        &self,
        subscription_id: String,
        connection_id: String,
        revision: u64,
        mut payload: Map<String, Value>,
    ) {
        payload.insert("uuid".into(), json!(self.uuid));
        payload.insert("subscriptionId".into(), json!(subscription_id));
        payload.insert("revision".into(), json!(revision));
        payload.insert("connectionId".into(), json!(connection_id));
        get_global_pubsub()
            .await
            .publish(
                &PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC,
                &Value::Object(payload).to_string(),
            )
            .await;
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
        let removed: Vec<SubscribedQuery> = {
            let mut queries = self.subscribed_queries.lock().await;
            let ids: Vec<String> = queries
                .iter()
                .filter(|(id, q)| matches(id, q))
                .map(|(id, _)| id.clone())
                .collect();
            ids.iter().filter_map(|id| queries.remove(id)).collect()
        };
        for query in &removed {
            if let Err(e) = get_prolog_service()
                .await
                .subscription_ended(self.uuid.clone(), query.query.clone())
                .await
            {
                log::warn!("Failed to notify prolog service of subscription end: {}", e);
            }
        }
        removed.len()
    }
}

/// Parse a stored result string as JSON; a string that is not JSON stays a
/// JSON string.
pub(crate) fn result_json(result: &str) -> Value {
    serde_json::from_str(result).unwrap_or_else(|_| Value::String(result.to_string()))
}

/// The change from `old` to `new`, keyed by row.
///
/// - Model results (`{ instances, totalCount }`): rows are keyed by `id`.
///   `added` / `changed` carry whole instances, `removed` carries ids, plus
///   `ids` (the new order) and `totalCount`.
/// - Query results (a JSON array of bindings): a binding row has no id, so the
///   row itself is its key (multiset). `added` / `removed` carry rows;
///   `changed` is always empty.
/// - Anything else: `reset: true` with the whole `result`.
pub(crate) fn result_delta(old: &str, new: &str, is_model: bool) -> Map<String, Value> {
    let (old_v, new_v) = (result_json(old), result_json(new));
    let delta = if is_model {
        model_delta(&old_v, &new_v)
    } else {
        rows_delta(&old_v, &new_v)
    };
    delta.unwrap_or_else(|| {
        let mut m = Map::new();
        m.insert("reset".into(), json!(true));
        m.insert("result".into(), new_v);
        m
    })
}

fn model_delta(old: &Value, new: &Value) -> Option<Map<String, Value>> {
    let rows = |v: &Value| -> Option<Vec<(String, Value)>> {
        v.get("instances")?
            .as_array()?
            .iter()
            .map(|i| Some((i.get("id")?.as_str()?.to_string(), i.clone())))
            .collect()
    };
    let (old_rows, new_rows) = (rows(old)?, rows(new)?);
    let old_by_id: HashMap<&str, &Value> =
        old_rows.iter().map(|(id, v)| (id.as_str(), v)).collect();
    let new_ids: HashSet<&str> = new_rows.iter().map(|(id, _)| id.as_str()).collect();

    let mut added = vec![];
    let mut changed = vec![];
    for (id, v) in &new_rows {
        match old_by_id.get(id.as_str()) {
            None => added.push(v.clone()),
            Some(prev) if *prev != v => changed.push(v.clone()),
            Some(_) => {}
        }
    }
    let removed: Vec<&str> = old_rows
        .iter()
        .map(|(id, _)| id.as_str())
        .filter(|id| !new_ids.contains(id))
        .collect();

    let mut m = Map::new();
    m.insert("added".into(), json!(added));
    m.insert("removed".into(), json!(removed));
    m.insert("changed".into(), json!(changed));
    m.insert(
        "ids".into(),
        json!(new_rows.iter().map(|(id, _)| id).collect::<Vec<_>>()),
    );
    m.insert(
        "totalCount".into(),
        new.get("totalCount").cloned().unwrap_or(Value::Null),
    );
    if let Some(cursor) = new.get("nextCursor") {
        m.insert("nextCursor".into(), cursor.clone());
    }
    Some(m)
}

fn rows_delta(old: &Value, new: &Value) -> Option<Map<String, Value>> {
    let (old_rows, new_rows) = (old.as_array()?, new.as_array()?);
    let mut remaining: HashMap<String, usize> = HashMap::new();
    for row in old_rows {
        *remaining.entry(row.to_string()).or_default() += 1;
    }
    let mut added = vec![];
    for row in new_rows {
        match remaining.get_mut(&row.to_string()) {
            Some(n) if *n > 0 => *n -= 1,
            _ => added.push(row.clone()),
        }
    }
    // Whatever of `old` was not matched by `new` was removed.
    let mut removed = vec![];
    for row in old_rows {
        if let Some(n) = remaining.get_mut(&row.to_string()) {
            if *n > 0 {
                *n -= 1;
                removed.push(row.clone());
            }
        }
    }
    let mut m = Map::new();
    m.insert("added".into(), json!(added));
    m.insert("removed".into(), json!(removed));
    m.insert("changed".into(), json!([]));
    Some(m)
}

#[cfg(test)]
mod tests {
    use super::super::ChangedPredicates;
    use super::result_delta;
    use crate::agent::AgentContext;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};
    use crate::types::{Link, LinkStatus};
    use serde_json::{json, Value};
    use std::time::Duration;

    const QUERY: &str = "SELECT ?s WHERE { ?s ?p ?o }";

    #[tokio::test]
    async fn closing_a_connection_ends_only_its_subscriptions() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let (a1, _) = p
            .subscribe_and_query(QUERY.into(), None, "a".into())
            .await
            .unwrap();
        let (a2, _) = p
            .subscribe_and_query(QUERY.into(), None, "a".into())
            .await
            .unwrap();
        let (b, _) = p
            .subscribe_and_query(QUERY.into(), None, "b".into())
            .await
            .unwrap();

        assert_eq!(p.dispose_connection_subscriptions("a").await, 2);
        assert!(p.subscription_state(&a1, "a").await.is_none());
        assert!(p.subscription_state(&a2, "a").await.is_none());
        assert!(
            p.subscription_state(&b, "b").await.is_some(),
            "b is untouched"
        );
    }

    #[tokio::test]
    async fn only_the_owning_connection_disposes_or_reads_a_subscription() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let (id, _) = p
            .subscribe_and_query(QUERY.into(), None, "a".into())
            .await
            .unwrap();
        assert!(p.subscription_state(&id, "b").await.is_none());
        assert!(!p.dispose_query_subscription(&id, "b").await);
        assert!(p.dispose_query_subscription(&id, "a").await);
        assert!(
            !p.dispose_query_subscription(&id, "a").await,
            "already gone"
        );
    }

    // ── Delta updates ───────────────────────────────────────────────────

    #[test]
    fn model_delta_is_keyed_by_id() {
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
                "added": [{ "id": "d", "title": "D" }],
                "removed": ["a"],
                "changed": [{ "id": "b", "title": "B2" }],
                "ids": ["c", "b", "d"],
                "totalCount": 3
            })
        );
    }

    #[test]
    fn query_delta_is_a_row_multiset() {
        let old = json!([{ "s": "x" }, { "s": "x" }, { "s": "y" }]);
        let new = json!([{ "s": "x" }, { "s": "z" }]);
        let d = Value::Object(result_delta(&old.to_string(), &new.to_string(), false));
        assert_eq!(
            d,
            json!({ "added": [{ "s": "z" }], "removed": [{ "s": "x" }, { "s": "y" }], "changed": [] })
        );
    }

    #[test]
    fn unkeyable_results_reset() {
        let d = Value::Object(result_delta("true", "false", false));
        assert_eq!(d, json!({ "reset": true, "result": false }));
        let d = Value::Object(result_delta("[]", "not json", true));
        assert_eq!(d, json!({ "reset": true, "result": "not json" }));
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

    /// Updates published for `id` after one subscription check.
    async fn updates_after_check(p: &super::PerspectiveInstance, id: &str) -> Vec<Value> {
        let mut rx = get_global_pubsub()
            .await
            .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
            .await;
        p.check_subscribed_queries(ChangedPredicates::CheckAll)
            .await;
        let mut out = vec![];
        let deadline = tokio::time::Instant::now() + Duration::from_millis(300);
        while let Ok(Ok(msg)) = tokio::time::timeout_at(deadline, rx.recv()).await {
            let v: Value = serde_json::from_str(&msg).unwrap();
            if v["subscriptionId"] == id {
                out.push(v);
            }
        }
        out
    }

    #[tokio::test]
    async fn model_subscription_sends_keyed_changes_with_revisions() {
        let (mut p, _, _) = setup_perspective_no_llm(&[("Todo", TODO_SDNA)]).await;
        let query = r#"{"includeUnverified": true}"#.to_string();
        let (id, initial) = p
            .model_subscribe_and_query("Todo".into(), query.clone(), None, "c".into())
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
        assert_eq!(u["added"][0]["id"], json!("test://t1"));
        assert_eq!(u["removed"], json!([]));
        assert_eq!(u["ids"], json!(["test://t1"]));
        assert!(u.get("result").is_none(), "no whole result on a delta");

        // Nothing changed: no update, revision stays.
        assert!(updates_after_check(&p, &id).await.is_empty());

        add(&mut p, "test://t2", "rdf://type", "test://Todo").await;
        add(&mut p, "test://t2", "test://title", "literal://string:two").await;
        let u = &updates_after_check(&p, &id).await[0];
        assert_eq!(u["revision"], json!(2));
        assert_eq!(u["added"][0]["id"], json!("test://t2"));
        assert_eq!(u["totalCount"], json!(2));
    }

    #[tokio::test]
    async fn resync_state_matches_the_last_update() {
        let (mut p, _, _) = setup_perspective_no_llm(&[]).await;
        let q = "SELECT ?s ?o WHERE { ?s <test://p> ?o }".to_string();
        let (id, _) = p
            .subscribe_and_query(q.clone(), None, "c".into())
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

        assert!(
            p.subscription_state(&id, "other").await.is_none(),
            "another connection cannot read it"
        );
        assert!(p.subscription_state("nope", "c").await.is_none());
    }

    #[tokio::test]
    async fn each_subscriber_gets_its_own_subscription_from_revision_zero() {
        let (mut p, _, _) = setup_perspective_no_llm(&[]).await;
        let q = "SELECT ?s ?o WHERE { ?s <test://p> ?o }".to_string();
        let (first, _) = p
            .subscribe_and_query(q.clone(), None, "c".into())
            .await
            .unwrap();
        add(&mut p, "test://a", "test://p", "test://b").await;
        assert_eq!(
            updates_after_check(&p, &first).await[0]["revision"],
            json!(1)
        );

        let (second, _) = p.subscribe_and_query(q, None, "c".into()).await.unwrap();
        assert_ne!(first, second, "not shared");
        assert_eq!(p.subscription_state(&second, "c").await.unwrap().0, 0);
    }
}

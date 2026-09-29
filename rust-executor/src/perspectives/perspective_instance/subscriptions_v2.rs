//! Protocol v2 subscription features on `PerspectiveInstance`.
//!
//! A child module of `perspective_instance` so it can reach the private
//! subscription registry without widening its visibility.

use deno_core::error::AnyError;
use serde_json::{json, Map, Value};
use std::collections::{HashMap, HashSet};
use tokio::time::Instant;

use super::PerspectiveInstance;
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};

impl PerspectiveInstance {
    /// Subscribe to a SPARQL/Prolog query; updates carry the whole result
    /// string (the v1 behaviour).
    pub async fn subscribe_and_query(
        &self,
        query: String,
        user_email: Option<String>,
    ) -> Result<(String, String), AnyError> {
        self.subscribe_and_query_mode(query, user_email, false)
            .await
    }

    /// Subscribe to a model query; updates carry the whole result string
    /// (the v1 behaviour).
    pub async fn model_subscribe_and_query(
        &self,
        class_name: String,
        query_json: String,
        user_email: Option<String>,
    ) -> Result<(String, String), AnyError> {
        self.model_subscribe_and_query_mode(class_name, query_json, user_email, false)
            .await
    }

    /// Publish one delta update (protocol feature `subscriptions.delta`) on
    /// the usual `query-subscription-update` topic:
    /// `{ uuid, subscriptionId, delta: true, revision, added, removed, changed, ... }`.
    /// See [`result_delta`] for the row keys.
    pub(super) async fn send_delta_update(
        &self,
        subscription_id: String,
        old: &str,
        new: &str,
        revision: u64,
        is_model: bool,
    ) {
        let mut payload = result_delta(old, new, is_model);
        payload.insert("uuid".into(), json!(self.uuid));
        payload.insert("subscriptionId".into(), json!(subscription_id));
        payload.insert("delta".into(), json!(true));
        payload.insert("revision".into(), json!(revision));
        get_global_pubsub()
            .await
            .publish(
                &PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC,
                &Value::Object(payload).to_string(),
            )
            .await;
    }

    /// Lease renewal (`perspective.keepAliveLease`): reset the keepalive of
    /// every query and model subscription in this perspective owned by
    /// `user_email` (`None` = the main agent). Returns how many it renewed.
    ///
    /// Subscriptions are keyed by id and owner, not by socket, so this
    /// renews every subscription of that owner in this perspective,
    /// including those opened by the owner's other sockets.
    pub async fn renew_subscriptions_of(&self, user_email: &Option<String>) -> usize {
        let now = Instant::now();
        let mut queries = self.subscribed_queries.lock().await;
        let mut renewed = 0;
        for query in queries.values_mut() {
            if &query.user_email == user_email {
                query.last_keepalive = now;
                renewed += 1;
            }
        }
        renewed
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
/// - Anything else (a result that is not in either form): `reset: true` with
///   the whole `result` as JSON.
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
    use super::super::{ChangedPredicates, QUERY_SUBSCRIPTION_TIMEOUT};
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use std::time::Duration;
    use tokio::time::Instant;

    const QUERY: &str = "SELECT ?s WHERE { ?s ?p ?o }";

    /// Age a subscription past the timeout without sleeping.
    async fn expire(p: &super::PerspectiveInstance, id: &str) {
        let past = Instant::now()
            .checked_sub(Duration::from_secs(QUERY_SUBSCRIPTION_TIMEOUT + 5))
            .expect("uptime longer than the timeout");
        p.subscribed_queries
            .lock()
            .await
            .get_mut(id)
            .unwrap()
            .last_keepalive = past;
    }

    #[tokio::test]
    async fn lease_renews_only_the_owners_subscriptions() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let (mine, _) = p.subscribe_and_query(QUERY.into(), None).await.unwrap();
        let (theirs, _) = p
            .subscribe_and_query(QUERY.into(), Some("other@example.com".into()))
            .await
            .unwrap();
        expire(&p, &mine).await;
        expire(&p, &theirs).await;

        assert_eq!(p.renew_subscriptions_of(&None).await, 1);

        // The renewed one survives the timeout sweep; the other owner's does not.
        p.check_subscribed_queries(ChangedPredicates::CheckAll).await;
        let subs = p.subscribed_queries.lock().await;
        assert!(subs.contains_key(&mine), "renewed subscription was kept");
        assert!(!subs.contains_key(&theirs), "other owner's subscription timed out");
    }

    #[tokio::test]
    async fn lease_renews_model_subscriptions_too() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let (query_sub, _) = p.subscribe_and_query(QUERY.into(), None).await.unwrap();
        // A model subscription is the same registry entry with model params.
        let model_sub = "model-sub".to_string();
        {
            let mut subs = p.subscribed_queries.lock().await;
            let mut entry = subs.get(&query_sub).unwrap().clone();
            entry.model_query_params = Some(super::super::ModelSubscriptionParams {
                class_name: "Todo".into(),
                query_json: "{}".into(),
            });
            subs.insert(model_sub.clone(), entry);
        }
        expire(&p, &query_sub).await;
        expire(&p, &model_sub).await;
        assert_eq!(p.renew_subscriptions_of(&None).await, 2);
    }

    #[tokio::test]
    async fn without_a_lease_subscriptions_still_time_out() {
        let (p, _, _) = setup_perspective_no_llm(&[]).await;
        let (id, _) = p.subscribe_and_query(QUERY.into(), None).await.unwrap();
        expire(&p, &id).await;
        p.check_subscribed_queries(ChangedPredicates::CheckAll).await;
        assert!(!p.subscribed_queries.lock().await.contains_key(&id));
    }

    // ── X2: delta subscriptions ─────────────────────────────────────────

    use super::result_delta;
    use crate::agent::AgentContext;
    use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};
    use crate::types::{Link, LinkStatus};
    use serde_json::{json, Value};

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
        p.check_subscribed_queries(ChangedPredicates::CheckAll).await;
        let mut out = vec![];
        // Legacy updates are published from a spawned task; give it a moment.
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
    async fn delta_model_subscription_sends_keyed_changes_with_revisions() {
        let (mut p, _, _) = setup_perspective_no_llm(&[("Todo", TODO_SDNA)]).await;
        let query = r#"{"includeUnverified": true}"#.to_string();
        let (id, initial) = p
            .model_subscribe_and_query_mode("Todo".into(), query.clone(), None, true)
            .await
            .unwrap();
        let initial: Value = serde_json::from_str(&initial).unwrap();
        assert_eq!(initial["instances"], json!([]));

        add(&mut p, "test://t1", "rdf://type", "test://Todo").await;
        add(&mut p, "test://t1", "test://title", "literal://string:one").await;
        let updates = updates_after_check(&p, &id).await;
        assert_eq!(updates.len(), 1, "{updates:?}");
        let u = &updates[0];
        assert_eq!(u["delta"], json!(true));
        assert_eq!(u["revision"], json!(1));
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
    async fn legacy_subscription_still_gets_the_whole_result_string() {
        let (mut p, _, _) = setup_perspective_no_llm(&[]).await;
        let q = "SELECT ?s ?o WHERE { ?s <test://p> ?o }".to_string();
        let (legacy, _) = p.subscribe_and_query(q.clone(), None).await.unwrap();
        let (delta, _) = p.subscribe_and_query_mode(q.clone(), None, true).await.unwrap();
        assert_ne!(legacy, delta, "a delta subscription is never shared");
        let (again, _) = p.subscribe_and_query(q, None).await.unwrap();
        assert_eq!(again, legacy, "legacy dedup unchanged");

        add(&mut p, "test://a", "test://p", "test://b").await;
        let mut rx = get_global_pubsub()
            .await
            .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
            .await;
        p.check_subscribed_queries(ChangedPredicates::CheckAll).await;
        let (mut legacy_updates, mut delta_updates) = (vec![], vec![]);
        let deadline = tokio::time::Instant::now() + Duration::from_millis(300);
        while let Ok(Ok(msg)) = tokio::time::timeout_at(deadline, rx.recv()).await {
            let v: Value = serde_json::from_str(&msg).unwrap();
            if v["subscriptionId"] == legacy {
                legacy_updates.push(v);
            } else if v["subscriptionId"] == delta {
                delta_updates.push(v);
            }
        }
        let l = &legacy_updates[0];
        assert_eq!(
            l.as_object().unwrap().keys().collect::<Vec<_>>(),
            vec!["uuid", "subscriptionId", "result"],
            "v1 payload shape"
        );
        assert!(l["result"].is_string());
        let d = &delta_updates[0];
        assert_eq!(d["revision"], json!(1));
        assert_eq!(d["added"].as_array().unwrap().len(), 1);
    }
}

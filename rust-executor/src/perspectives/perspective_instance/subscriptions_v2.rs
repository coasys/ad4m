//! Protocol v2 subscription features on `PerspectiveInstance`.
//!
//! A child module of `perspective_instance` so it can reach the private
//! subscription registry without widening its visibility.

use tokio::time::Instant;

use super::PerspectiveInstance;

impl PerspectiveInstance {
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
}

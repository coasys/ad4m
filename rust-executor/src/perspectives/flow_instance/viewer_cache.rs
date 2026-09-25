//! Per-user derivation of a `FlowInstance`'s `currentState` on read.
//!
//! The `currentState` cache is a `Local` link, and on a multi-user host a
//! Local link is private to its author (#1024, `link_visibility`). The engine
//! writes the cache under whichever user's request ran the pass, so after Bob
//! moved a flow, Alice — a co-owner of the same perspective — does not see
//! Bob's cache. She must not: the cache is his. What she gets instead is her
//! own derivation. When a user reads `FlowInstance` rows
//! (`perspective.modelQuery` on that class, which is what the SDK's
//! `FlowInstance.findAll` / `findOne` / `currentStateName` use), the executor
//! derives the state of the instances that read covers for that user, and
//! writes or replaces the user's **own** Local cache wherever it is missing
//! or differs. Nobody else's cache link is read or touched. The query that
//! follows then hydrates the derived state from the user's own link.
//!
//! The hook is the `modelQuery` RPC handler, gated on the class name, so no
//! other model query pays for a fold. The set derived is narrowed to what the
//! user's query selects on (`where.id` / `where.flowUri` / `where.subject`
//! when they are plain strings), never to its page: a `limit` must not leave
//! the rows outside the page stale, and a `where` on `currentState` itself
//! must not be answered from a stale cache.
//!
//! The refresh is best effort ([`refresh_for_read`]). It is not billed and
//! needs no credits (the writer skips `add_link`'s billing), a failure is
//! logged and the query still answers from whatever cache the user holds,
//! and a token that may only read the perspective gets no refresh at all:
//! it must not make the executor sign links for the user.
//!
//! [`crate::perspectives::flow_instance::derive_states`] is the derivation
//! (`read_set` + `fold_read_set`), and
//! [`crate::perspectives::flow_classes::write_local_current_state`] the
//! writer, which replaces only the acting user's own Local link.

use crate::agent::capabilities::{check_capability, perspective_update_capability, Capability};
use crate::agent::AgentContext;
use crate::perspectives::flow_classes::{write_local_current_state, FLOW_INSTANCE_CLASS};
use crate::perspectives::flow_context::{load_shacl_flows, parse_flow_instance_from_hydrated};
use crate::perspectives::flow_instance::derive_states;
use crate::perspectives::link_visibility::viewer_did_for_context;
use crate::perspectives::perspective_instance::PerspectiveInstance;

/// What a `FlowInstance` read did to the reader's own cache.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ReadRefresh {
    /// The token may not write to the perspective, so nothing was written.
    ReadOnly,
    /// This many cache links were written.
    Refreshed(usize),
    /// The refresh failed and was logged; the read goes on.
    Failed,
}

/// The `perspective.modelQuery` hook for a `FlowInstance` read: refresh the
/// reader's own cache if their token may write to perspective `uuid`, and
/// never fail the read because of it.
pub(crate) async fn refresh_for_read(
    perspective: &mut PerspectiveInstance,
    uuid: &str,
    query_json: &str,
    capabilities: &Result<Vec<Capability>, String>,
    context: &AgentContext,
) -> ReadRefresh {
    if check_capability(
        capabilities,
        &perspective_update_capability(vec![uuid.to_string()]),
    )
    .is_err()
    {
        return ReadRefresh::ReadOnly;
    }
    match sync_for_context(perspective, query_json, context).await {
        Ok(written) => ReadRefresh::Refreshed(written),
        Err(e) => {
            log::warn!(
                "FlowInstance read on {uuid}: refreshing the reader's currentState cache failed, answering from the cache as it is: {e:#}"
            );
            ReadRefresh::Failed
        }
    }
}

/// Derive the state of the `FlowInstance`s that `query_json` selects, as the
/// agent behind `context`, and bring that agent's own Local `currentState`
/// cache in line with it.
///
/// Returns how many cache links were written. Records whose flow is not
/// registered on the perspective, or whose state cannot be derived, are left
/// as they are (`derive_states` logs and skips them). A perspective on which
/// the `FlowInstance` class is not registered yet has nothing to derive.
pub(crate) async fn sync_for_context(
    perspective: &mut PerspectiveInstance,
    query_json: &str,
    context: &AgentContext,
) -> anyhow::Result<usize> {
    let viewer = viewer_did_for_context(context)?;
    let json = match perspective
        .model_query_for_viewer(
            FLOW_INSTANCE_CLASS,
            &narrowed_query(query_json),
            viewer.as_deref(),
        )
        .await
    {
        Ok(j) => j,
        Err(e) => {
            // No `FlowInstance` class: no instance was ever minted or synced
            // here. The caller's own query reports it the same way.
            let msg = format!("{e:#}").to_lowercase();
            if msg.contains("no shacl shape stored") || msg.contains("shape not found") {
                return Ok(0);
            }
            return Err(anyhow::anyhow!("loading the flow instances failed: {msg}"));
        }
    };
    let parsed: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| anyhow::anyhow!("flow instance rows are not JSON: {e:#}"))?;
    // `currentState` on each record is the viewer's own cache, or empty:
    // hydration read the rows in the viewer's scope.
    let records: Vec<_> = parsed
        .get("instances")
        .and_then(|v| v.as_array())
        .into_iter()
        .flatten()
        .filter_map(parse_flow_instance_from_hydrated)
        .collect();
    if records.is_empty() {
        return Ok(0);
    }
    let flows = load_shacl_flows(perspective).await?;
    let mut written = 0;
    for derived in derive_states(perspective, &records, &flows).await {
        let cached = records
            .iter()
            .find(|r| r.instance_uri == derived.record.instance_uri)
            .map(|r| r.current_state.as_str());
        if cached == Some(derived.record.current_state.as_str()) {
            continue;
        }
        write_local_current_state(
            perspective,
            &derived.record.instance_uri,
            &derived.record.current_state,
            None,
            context,
        )
        .await?;
        written += 1;
    }
    Ok(written)
}

/// The part of the user's query that decides *which* instances to derive:
/// the plain-string `id` / `flowUri` / `subject` leaves of its `where`.
/// Everything else — pagination, ordering, includes, and a `where` on
/// `currentState` — is dropped, so the derivation covers every instance the
/// user's query could select, not only the page or the rows its stale cache
/// happens to match.
fn narrowed_query(query_json: &str) -> String {
    let parsed: serde_json::Value = serde_json::from_str(query_json).unwrap_or_default();
    let mut narrowed = serde_json::Map::new();
    if let Some(filter) = parsed.get("where").and_then(|w| w.as_object()) {
        for key in ["id", "flowUri", "subject"] {
            if let Some(value) = filter.get(key).and_then(|v| v.as_str()) {
                narrowed.insert(
                    key.to_string(),
                    serde_json::Value::String(value.to_string()),
                );
            }
        }
    }
    if narrowed.is_empty() {
        "{}".to_string()
    } else {
        serde_json::json!({ "where": narrowed }).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::{did_for_context, AgentService};
    use crate::perspectives::flow_classes::{
        advance_flow_instance_state, mint_flow_instance, FLOW_CURRENT_STATE_PREDICATE,
    };
    use crate::perspectives::interpretation_test_support::{
        delivery_flow_json_for, seed_flow, setup_perspective_no_llm,
    };
    use crate::types::{DecoratedLinkExpression, Link, LinkQuery, LinkStatus};
    use ad4m_client::literal::{Literal, LiteralValue};

    const OTHER_EMAIL: &str = "other-user@1058-flow.test";
    const BASE: &str = "ad4m://task/viewer-cache";
    const FLOW_URI: &str = "delivery://DeliveryFlow";

    /// A second managed user with a real key: its reads are scoped to its
    /// DID and its cache link is signed by it.
    fn other_user() -> (AgentContext, String) {
        AgentService::ensure_user_key_exists(OTHER_EMAIL).expect("other user key");
        let ctx = AgentContext::for_user_email(OTHER_EMAIL.to_string());
        let did = did_for_context(&ctx).expect("other user DID");
        (ctx, did)
    }

    /// Every `currentState` link on `uri`, in executor scope.
    async fn cache_links(
        perspective: &PerspectiveInstance,
        uri: &str,
    ) -> Vec<DecoratedLinkExpression> {
        perspective
            .get_links(&LinkQuery {
                source: Some(uri.to_string()),
                predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
                ..Default::default()
            })
            .await
            .expect("get_links")
    }

    fn state_of(link: &DecoratedLinkExpression) -> String {
        match Literal::from_url(link.data.target.clone())
            .expect("literal")
            .get()
            .expect("value")
        {
            LiteralValue::String(s) => s,
            other => panic!("currentState is a string literal, got {other:?}"),
        }
    }

    /// The `currentState` the viewer's own model query hydrates for `uri`.
    async fn state_seen_by(perspective: &PerspectiveInstance, uri: &str, did: &str) -> String {
        let json = perspective
            .model_query_for_viewer(
                FLOW_INSTANCE_CLASS,
                &serde_json::json!({ "where": { "id": uri } }).to_string(),
                Some(did),
            )
            .await
            .expect("model_query_for_viewer");
        let rows: serde_json::Value = serde_json::from_str(&json).expect("JSON");
        rows["instances"][0]["currentState"]
            .as_str()
            .unwrap_or_default()
            .to_string()
    }

    /// The main agent mints a Delivery instance (its own cache at genesis).
    async fn seeded() -> (PerspectiveInstance, AgentContext, String, String) {
        let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        seed_flow(
            &mut perspective,
            &ctx,
            &delivery_flow_json_for("Task"),
            "Delivery",
        )
        .await;
        let uri = mint_flow_instance(
            &mut perspective,
            FLOW_URI,
            BASE,
            "identified",
            "viewer-cache-1",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");
        let did = did_for_context(&ctx).expect("main DID");
        (perspective, ctx, did, uri)
    }

    /// The hook: a read by a user who holds no cache writes exactly one
    /// Local `currentState` link, authored by that user, carrying the derived
    /// state — and leaves the other user's link alone. A second read by the
    /// same user writes nothing.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_read_by_a_user_without_a_cache_writes_one_local_link_of_their_own() {
        let (mut perspective, _main_ctx, main_did, uri) = seeded().await;
        let (other_ctx, other_did) = other_user();

        let before = cache_links(&perspective, &uri).await;
        assert_eq!(before.len(), 1, "the minter's cache only: {before:?}");
        assert_eq!(before[0].author, main_did);

        let written = sync_for_context(&mut perspective, "{}", &other_ctx)
            .await
            .expect("sync_for_context");
        assert_eq!(written, 1, "one cache link for the reading user");

        let after = cache_links(&perspective, &uri).await;
        assert_eq!(after.len(), 2, "one link per user: {after:?}");
        let mine: Vec<_> = after.iter().filter(|l| l.author == other_did).collect();
        assert_eq!(mine.len(), 1, "exactly one link authored by the reader");
        assert_eq!(mine[0].status, Some(LinkStatus::Local));
        assert_eq!(
            state_of(mine[0]),
            "identified",
            "the derived (genesis) state"
        );
        let theirs: Vec<_> = after.iter().filter(|l| l.author == main_did).collect();
        assert_eq!(theirs.len(), 1, "the minter's link is untouched");
        assert_eq!(theirs[0].timestamp, before[0].timestamp);

        // Idempotent: the cache now matches, so nothing is written.
        let again = sync_for_context(&mut perspective, "{}", &other_ctx)
            .await
            .expect("second sync");
        assert_eq!(again, 0);
        assert_eq!(cache_links(&perspective, &uri).await.len(), 2);
    }

    /// Each user's cache is visible only to its author: after both have read,
    /// the store holds two Local `currentState` links, and a viewer-scoped
    /// read returns only the viewer's own.
    #[tokio::test(flavor = "multi_thread")]
    async fn each_users_cache_link_is_visible_only_to_its_author() {
        let (mut perspective, _main_ctx, main_did, uri) = seeded().await;
        let (other_ctx, other_did) = other_user();
        sync_for_context(&mut perspective, "{}", &other_ctx)
            .await
            .expect("sync");

        let query = LinkQuery {
            source: Some(uri.clone()),
            predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
            ..Default::default()
        };
        for (viewer, expected_author) in [(&main_did, &main_did), (&other_did, &other_did)] {
            let seen = perspective
                .get_links_for_viewer(&query, Some(viewer))
                .await
                .expect("get_links_for_viewer");
            assert_eq!(seen.len(), 1, "{viewer} sees one cache link: {seen:?}");
            assert_eq!(&seen[0].author, expected_author, "and it is their own");
        }
        assert_eq!(cache_links(&perspective, &uri).await.len(), 2);
    }

    /// A bogus Local `currentState` a user writes for themselves changes only
    /// their own view until their next read; the other user still reads the
    /// derived state, and the bogus link is never taken as anyone else's.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_bogus_own_cache_affects_only_its_author() {
        let (mut perspective, main_ctx, main_did, uri) = seeded().await;
        let (other_ctx, other_did) = other_user();

        // The other user plants a state the fold never derived.
        perspective
            .add_link(
                Link {
                    source: uri.clone(),
                    predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
                    target: Literal::from_string("delivered".to_string())
                        .to_url()
                        .expect("literal"),
                },
                LinkStatus::Local,
                None,
                &other_ctx,
            )
            .await
            .expect("a user may write their own Local link");
        assert_eq!(
            state_seen_by(&perspective, &uri, &other_did).await,
            "delivered",
            "before a read, the author sees what they wrote"
        );

        // The main agent's read derives for the main agent: genesis.
        sync_for_context(&mut perspective, "{}", &main_ctx)
            .await
            .expect("sync as main");
        assert_eq!(
            state_seen_by(&perspective, &uri, &main_did).await,
            "identified",
            "the other user's bogus cache does not reach the main agent"
        );

        // The other user's own read replaces their bogus link with the
        // derived state.
        let written = sync_for_context(&mut perspective, "{}", &other_ctx)
            .await
            .expect("sync as other");
        assert_eq!(written, 1);
        assert_eq!(
            state_seen_by(&perspective, &uri, &other_did).await,
            "identified"
        );
        let links = cache_links(&perspective, &uri).await;
        assert_eq!(
            links.len(),
            2,
            "one link per user, the bogus one replaced: {links:?}"
        );
        assert!(links.iter().all(|l| state_of(l) == "identified"));
    }

    /// The engine's own writer replaces only the acting user's cache link.
    #[tokio::test(flavor = "multi_thread")]
    async fn advancing_the_state_replaces_only_the_acting_users_cache() {
        let (mut perspective, main_ctx, main_did, uri) = seeded().await;
        let (other_ctx, other_did) = other_user();
        sync_for_context(&mut perspective, "{}", &other_ctx)
            .await
            .expect("sync");

        advance_flow_instance_state(&mut perspective, &uri, "scoped", None, &main_ctx)
            .await
            .expect("advance as main");

        let links = cache_links(&perspective, &uri).await;
        assert_eq!(links.len(), 2, "{links:?}");
        let by_author = |did: &str| {
            links
                .iter()
                .filter(|l| l.author == did)
                .map(state_of)
                .collect::<Vec<_>>()
        };
        assert_eq!(by_author(&main_did), vec!["scoped".to_string()]);
        assert_eq!(by_author(&other_did), vec!["identified".to_string()]);
    }

    fn capabilities_of(can: Capability) -> Result<Vec<Capability>, String> {
        Ok(vec![can])
    }

    /// A token that may only read the perspective gets no refresh: the
    /// executor does not sign links for the user on its behalf.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_read_only_token_gets_no_refresh() {
        use crate::agent::capabilities::perspective_query_capability;
        let (mut perspective, _main_ctx, _main_did, uri) = seeded().await;
        let (other_ctx, _other_did) = other_user();
        let uuid = perspective.persisted.lock().await.uuid.clone();

        let outcome = refresh_for_read(
            &mut perspective,
            &uuid,
            "{}",
            &capabilities_of(perspective_query_capability(vec![uuid.clone()])),
            &other_ctx,
        )
        .await;
        assert_eq!(outcome, ReadRefresh::ReadOnly);
        assert_eq!(
            cache_links(&perspective, &uri).await.len(),
            1,
            "only the minter's cache"
        );

        let outcome = refresh_for_read(
            &mut perspective,
            &uuid,
            "{}",
            &capabilities_of(perspective_update_capability(vec![uuid.clone()])),
            &other_ctx,
        )
        .await;
        assert_eq!(outcome, ReadRefresh::Refreshed(1), "a token that may write");
    }

    /// A failed refresh does not fail the read: the hook reports it and the
    /// reader's own query still answers.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_failed_refresh_leaves_the_read_answering() {
        let (mut perspective, _main_ctx, _main_did, uri) = seeded().await;
        let (other_ctx, other_did) = other_user();
        let uuid = perspective.persisted.lock().await.uuid.clone();

        perspective.fail_next_add_link(0);
        let outcome = refresh_for_read(
            &mut perspective,
            &uuid,
            "{}",
            &capabilities_of(perspective_update_capability(vec![uuid.clone()])),
            &other_ctx,
        )
        .await;
        assert_eq!(outcome, ReadRefresh::Failed);
        assert_eq!(
            state_seen_by(&perspective, &uri, &other_did).await,
            "",
            "the query answers, from the reader's cache as it is (none yet)"
        );
    }

    /// Restores free hosting when a test that switched it off ends.
    struct FreeHostingOff;
    impl FreeHostingOff {
        fn new() -> Self {
            crate::db::Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(false))
                .expect("switch free hosting off");
            FreeHostingOff
        }
    }
    impl Drop for FreeHostingOff {
        fn drop(&mut self) {
            let _ = crate::db::Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(true));
        }
    }

    /// The cache refresh a read does is bookkeeping, not a user write: a
    /// user without credits on a host with free hosting off can still read,
    /// and the refresh is not billed.
    #[tokio::test(flavor = "multi_thread")]
    async fn the_refresh_needs_no_credits_and_is_not_billed() {
        let (mut perspective, _main_ctx, _main_did, uri) = seeded().await;
        let (other_ctx, other_did) = other_user();
        crate::db::Ad4mDb::with_global_instance(|db| {
            db.add_user(OTHER_EMAIL, &other_did, "pw")?;
            db.set_user_credits(OTHER_EMAIL, 0.0)
        })
        .expect("a user row with no credits");
        let _free_hosting_off = FreeHostingOff::new();
        crate::billing::test_seam::reset();

        let written = sync_for_context(&mut perspective, "{}", &other_ctx)
            .await
            .expect("a read refresh does not need credits");
        assert_eq!(written, 1);
        assert_eq!(
            state_seen_by(&perspective, &uri, &other_did).await,
            "identified"
        );
        let billed: Vec<_> = crate::billing::test_seam::calls()
            .into_iter()
            .filter(|c| c.email == OTHER_EMAIL)
            .collect();
        assert!(billed.is_empty(), "the refresh is not billed: {billed:?}");
    }

    #[test]
    fn the_narrowed_query_keeps_only_the_selecting_leaves() {
        assert_eq!(narrowed_query("{}"), "{}");
        assert_eq!(narrowed_query("not json"), "{}");
        assert_eq!(
            narrowed_query(r#"{"where":{"currentState":"Done"},"limit":1,"offset":3}"#),
            "{}"
        );
        let narrowed = narrowed_query(
            r#"{"where":{"subject":"ad4m://t/1","flowUri":"f://F","currentState":"Done"},"limit":1}"#,
        );
        let parsed: serde_json::Value = serde_json::from_str(&narrowed).unwrap();
        assert_eq!(
            parsed,
            serde_json::json!({"where": {"subject": "ad4m://t/1", "flowUri": "f://F"}})
        );
    }
}

//! Viewer-scoped read entry points on [`PerspectiveInstance`].
//!
//! These are the `*_for_viewer` forms of the instance's link and model-query
//! reads. The executor-scope forms (`get_links`, `model_query`) stay in
//! `perspective_instance.rs` as thin wrappers that pass `None`. The visibility
//! rule itself is in [`link_visibility`](crate::perspectives::link_visibility).

use super::perspective_instance::{PerspectiveInstance, MODEL_QUERY_SHAPE_WAIT};
use crate::agent::AgentContext;
use crate::perspectives::link_visibility::viewer_did_for_context;
use crate::perspectives::model_query::types::ShapeResolver;
use crate::types::{DecoratedLinkExpression, LinkQuery};
use chrono::DateTime;
use deno_core::error::AnyError;

impl PerspectiveInstance {
    /// [`Self::get_links_local_decorated`] in the visibility scope of
    /// `viewer_did` — see
    /// [`link_visibility`](crate::perspectives::link_visibility).
    pub(super) fn get_links_local_decorated_for_viewer(
        &self,
        query: &LinkQuery,
        viewer_did: Option<&str>,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let from_date = query.from_date.as_ref().map(|d| {
            let dt: chrono::DateTime<chrono::Utc> = d.clone().into();
            dt.to_rfc3339()
        });
        let until_date = query.until_date.as_ref().map(|d| {
            let dt: chrono::DateTime<chrono::Utc> = d.clone().into();
            dt.to_rfc3339()
        });

        Ok(self.sparql_store.query_links_for_viewer(
            query.source.as_deref(),
            query.predicate.as_deref(),
            query.target.as_deref(),
            from_date.as_deref(),
            until_date.as_deref(),
            None, // limit is applied after sorting in get_links()
            viewer_did,
        )?)
    }

    /// [`Self::get_links`] in the visibility scope of `viewer_did`:
    /// `Local` links authored by someone else are not returned.
    pub async fn get_links_for_viewer(
        &self,
        q: &LinkQuery,
        viewer_did: Option<&str>,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let mut reverse = false;
        let mut query = q.clone();

        if let Some(until_date) = query.until_date.as_ref() {
            if let Some(from_date) = query.from_date.as_ref() {
                let chrono_from_date: chrono::DateTime<chrono::Utc> = from_date.clone().into();
                let chrono_until_date: chrono::DateTime<chrono::Utc> = until_date.clone().into();
                if chrono_from_date > chrono_until_date {
                    reverse = true;
                    query.from_date.clone_from(&q.until_date);
                    query.until_date.clone_from(&q.from_date);
                }
            }
        }

        // When the caller supplies a `limit`, push it down into the store via
        // a bounded top-N heap. This keeps memory at O(limit) regardless of
        // how many links match — the previous materialise-sort-truncate path
        // allocated the full Vec even for a 10-item page, which on large
        // perspectives was a substantial regression in the same hot path this
        // PR is trying to shrink.
        //
        // Otherwise (no limit) fall back to the in-memory sort path. We still
        // pull the decorated form directly from the store so we don't pay the
        // triple-materialisation tax described below.
        if let Some(limit) = query.limit {
            let from_date = query.from_date.as_ref().map(|d| {
                let dt: chrono::DateTime<chrono::Utc> = d.clone().into();
                dt.to_rfc3339()
            });
            let until_date = query.until_date.as_ref().map(|d| {
                let dt: chrono::DateTime<chrono::Utc> = d.clone().into();
                dt.to_rfc3339()
            });
            return Ok(self
                .sparql_store
                .query_links_top_n_by_timestamp_for_viewer(
                    query.source.as_deref(),
                    query.predicate.as_deref(),
                    query.target.as_deref(),
                    from_date.as_deref(),
                    until_date.as_deref(),
                    limit as usize,
                    reverse,
                    viewer_did,
                )?);
        }

        // No limit: pull the already-decorated form from the SPARQL store and
        // sort in-place. Previously this path materialised Vec<...> three
        // times: once as DecoratedLinkExpression in get_links_local_decorated,
        // again as Vec<(LinkExpression, LinkStatus)> in get_links_local
        // (unwrap), and a third time after sort by re-wrapping each pair via
        // `DecoratedLinkExpression::from((link, status))` — which re-runs
        // Ed25519 signature verification per link. For a 10K-link result
        // that's ~30K extra small allocations + 10K crypto ops every call.
        // The wind-tunnel S9 query path was the dominant remaining source of
        // RSS growth; this collapses it to a single Vec.
        let mut links = self.get_links_local_decorated_for_viewer(&query, viewer_did)?;

        links.sort_by(|a, b| {
            let a_time = DateTime::parse_from_rfc3339(&a.timestamp).unwrap_or_default();
            let b_time = DateTime::parse_from_rfc3339(&b.timestamp).unwrap_or_default();
            if reverse {
                b_time.cmp(&a_time)
            } else {
                a_time.cmp(&b_time)
            }
        });

        Ok(links)
    }

    /// [`Self::get_links_for_viewer`] in the scope of the agent that a write
    /// acts for.
    ///
    /// A write that first looks up the links it will remove or replace must
    /// use this, not `get_links`. A lookup in executor scope also finds other
    /// users' `Local` links, and the write then deletes them (#1024).
    pub async fn get_links_for_context(
        &self,
        q: &LinkQuery,
        context: &AgentContext,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let viewer = viewer_did_for_context(context)?;
        self.get_links_for_viewer(q, viewer.as_deref()).await
    }

    /// [`Self::model_query`] in the visibility scope of `viewer_did`: instance
    /// properties, relations and projections built from another user's `Local`
    /// links are not hydrated, so an instance that exists only in those links
    /// does not appear at all.
    pub async fn model_query_for_viewer(
        &self,
        class_name: &str,
        query_json: &str,
        viewer_did: Option<&str>,
    ) -> Result<String, deno_core::anyhow::Error> {
        let query_input: crate::perspectives::model_query::ModelQueryInput =
            serde_json::from_str(query_json)
                .map_err(|e| deno_core::anyhow::anyhow!("Failed to parse model query: {}", e))?;

        // Cross-peer safety: on a shared perspective we may be asked about
        // a class whose SHACL hasn't synced yet. Poll briefly rather than
        // fail immediately. The subsequent recursive resolves inside
        // `execute_model_query` use the plain (non-waiting) resolver
        // because at that point the top-level shape has been resolved so
        // referenced target-classes are extremely likely to also be
        // present already — a nested wait per relation would multiply
        // latency for a case we haven't seen bite in practice.
        let _ = self
            .get_shape_or_wait(class_name, MODEL_QUERY_SHAPE_WAIT)
            .await?;
        let resolver = self.shape_resolver();
        let shape = resolver.get_shape(class_name)?;
        let result = crate::perspectives::model_query::execute_model_query(
            &self.sparql_store,
            shape.as_ref(),
            &query_input,
            &resolver,
            viewer_did,
        )
        .await?;

        serde_json::to_string(&result).map_err(|e| {
            deno_core::anyhow::anyhow!("Failed to serialize model query result: {}", e)
        })
    }
}

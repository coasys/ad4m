//! Reads on [`PerspectiveInstance`] as one user.
//!
//! These are the `*_for_viewer` forms of the instance's link and model-query
//! reads, and [`PerspectiveInstance::read_as`]. They only choose whose view a
//! read runs in; the store decides what that view holds: the shared links
//! plus the viewer's own `Local` links
//! ([`SparqlStore`](crate::perspectives::sparql_store::SparqlStore), #1224).
//! `viewer_did == None` reads as the instance does: the main agent, unless
//! the instance was scoped with [`PerspectiveInstance::read_as`]. The plain
//! forms (`get_links`, `model_query`) pass `None`.

use super::perspective_instance::{PerspectiveInstance, MODEL_QUERY_SHAPE_WAIT};
use crate::agent::{did_for_context, AgentContext};
use crate::perspectives::model_query::types::ShapeResolver;
use crate::types::{DecoratedLinkExpression, LinkQuery};
use chrono::DateTime;
use deno_core::error::AnyError;

/// The viewer a request is read as: the DID of the agent it is attributed to.
///
/// Fails closed: a request whose DID cannot be resolved is an error, not a
/// read as the main agent. This includes the main agent itself:
/// `is_main_agent` is true for every token that carries no user email.
pub fn viewer_did_for_context(context: &AgentContext) -> Result<Option<String>, AnyError> {
    did_for_context(context).map(Some)
}

impl PerspectiveInstance {
    /// This instance as `did` reads it: every read through the returned
    /// clone (links, model queries, SPARQL) sees the shared links plus
    /// `did`'s own `Local` links. Writes are unaffected; they always go to
    /// the graph of the user they act for.
    ///
    /// The flow engine runs each pass through such a clone, so a pass for one
    /// user reads that user's view only.
    pub fn read_as(&self, did: &str) -> PerspectiveInstance {
        let mut scoped = self.clone();
        scoped.sparql_store = std::sync::Arc::new(self.sparql_store.read_as(Some(did)));
        scoped
    }

    /// [`Self::read_as`] the user `context` acts for. The flow engine's entry
    /// points (a consensus pass, a proposal, an accept, a read's refresh)
    /// start with this: the engine reads as the one user it acts for.
    pub fn read_as_context(&self, context: &AgentContext) -> Result<PerspectiveInstance, AnyError> {
        Ok(self.read_as(&did_for_context(context)?))
    }

    /// [`Self::get_links_local_decorated`] as `viewer_did` reads.
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

        Ok(self.sparql_store.read_as(viewer_did).query_links(
            query.source.as_deref(),
            query.predicate.as_deref(),
            query.target.as_deref(),
            from_date.as_deref(),
            until_date.as_deref(),
            None, // limit is applied after sorting in get_links()
        )?)
    }

    /// [`Self::get_links`] as `viewer_did` reads: another user's `Local`
    /// links are not in that view.
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
                .read_as(viewer_did)
                .query_links_top_n_by_timestamp(
                    query.source.as_deref(),
                    query.predicate.as_deref(),
                    query.target.as_deref(),
                    from_date.as_deref(),
                    until_date.as_deref(),
                    limit as usize,
                    reverse,
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

    /// [`Self::get_links_for_viewer`] as the agent that a write acts for.
    ///
    /// A write that first looks up the links it will remove or replace uses
    /// this, not `get_links`, so it only finds links that agent may remove
    /// (#1024).
    pub async fn get_links_for_context(
        &self,
        q: &LinkQuery,
        context: &AgentContext,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let viewer = viewer_did_for_context(context)?;
        self.get_links_for_viewer(q, viewer.as_deref()).await
    }

    /// [`Self::model_query`] as `viewer_did` reads: another user's `Local`
    /// links are not in that view, so they neither hydrate nor select, and an
    /// instance that exists only in them does not appear at all.
    pub async fn model_query_for_viewer(
        &self,
        class_name: &str,
        query_json: &str,
        viewer_did: Option<&str>,
    ) -> Result<String, deno_core::anyhow::Error> {
        let mut query_input: crate::perspectives::model_query::ModelQueryInput =
            serde_json::from_str(query_json)
                .map_err(|e| deno_core::anyhow::anyhow!("Failed to parse model query: {}", e))?;

        // `where.producedByFlow` is resolved here, not in the pipeline: it
        // needs the perspective (receipts, flow catalogue) and signature
        // verification, neither of which SPARQL or the post-hydration filter
        // has. Extracted BEFORE the query runs and turned into an id
        // constraint the store applies ahead of `limit`/`offset` — so a page
        // of N is N *valid* outputs, never N rows later thinned. Malformed or
        // mis-placed filters error rather than silently admit everything.
        let produced_by_flow =
            crate::perspectives::model_query::take_produced_by_flow(&mut query_input)
                .map_err(deno_core::anyhow::Error::msg)?;

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

        if let Some(filter) = produced_by_flow {
            // Which outputs a valid receipt vouches for is the flow engine's
            // derivation, read as the same viewer. It only narrows the ids.
            let scoped = match viewer_did {
                Some(did) => self.read_as(did),
                None => self.clone(),
            };
            let valid = crate::perspectives::flow_instance::produced::flow_valid_outputs(
                &scoped,
                &filter.flow,
                filter.state.as_deref(),
            )
            .await?;
            // Only outputs committed as the queried class pass; see
            // `output_matches_class` for why conformance alone is not enough.
            let allowed: std::collections::BTreeSet<String> = valid
                .into_iter()
                .filter(|v| {
                    crate::perspectives::flow_instance::produced::output_matches_class(
                        &v.output,
                        class_name,
                        &shape.target_class,
                    )
                })
                .map(|v| v.output.id)
                .collect();
            if !crate::perspectives::model_query::constrain_ids(&mut query_input, allowed)
                .map_err(deno_core::anyhow::Error::msg)?
            {
                // No valid output survives; answer directly rather than
                // handing the store an empty VALUES block.
                return serde_json::to_string(
                    &crate::perspectives::model_query::ModelQueryResult {
                        instances: vec![],
                        total_count: 0,
                    },
                )
                .map_err(|e| {
                    deno_core::anyhow::anyhow!("Failed to serialize model query result: {}", e)
                });
            }
        }

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentService;

    /// A main-agent request with no resolvable DID must be refused, not read
    /// as some default view. `is_main_agent` is true for any token without a
    /// user email, so this is not only the fresh-executor case.
    #[test]
    fn main_agent_without_a_did_fails_closed() {
        crate::test_utils::setup_wallet();
        AgentService::init_global_test_instance();

        let saved = AgentService::with_mutable_global_instance(|a| a.did.take());
        let result = viewer_did_for_context(&AgentContext::main_agent());
        AgentService::with_mutable_global_instance(|a| a.did = saved);

        assert!(
            result.is_err(),
            "an unresolvable main-agent DID must not become a read, got {result:?}"
        );
    }

    #[test]
    fn main_agent_with_a_did_reads_as_that_did() {
        crate::test_utils::setup_wallet();
        AgentService::init_global_test_instance();

        let did = AgentService::with_global_instance(|a| a.did.clone()).expect("test agent DID");
        assert_eq!(
            viewer_did_for_context(&AgentContext::main_agent()).unwrap(),
            Some(did)
        );
    }
}

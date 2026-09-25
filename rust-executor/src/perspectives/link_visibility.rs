//! Viewer-scoped visibility for [`LinkStatus::Local`] links.
//!
//! # Why
//!
//! `LinkStatus::Local` means "not replicated by the link language". On a
//! single-user executor that happens to coincide with "private to me", so the
//! two readings were never distinguished. On a multi-user executor they come
//! apart: managed users who join the same neighbourhood co-own one
//! [`PerspectiveInstance`](crate::perspectives::perspective_instance::PerspectiveInstance)
//! (`PerspectiveHandle.owners`) backed by a single row set, so a Local link
//! written by user A was fully visible — and query-able, and removable — for
//! user B on the same executor. It was only hidden from *other* executors.
//!
//! # Rule
//!
//! A link with `status = Local` is visible to a viewer iff
//! `link.author == viewer_did`. Links with `Shared` (or no) status are visible
//! to everyone who can reach the perspective, exactly as before.
//!
//! There is no exception for the flow engine's `currentState` cache: it is a
//! Local link like any other, private to the user whose request wrote it.
//! Every user derives the state for themselves and keeps their own cache
//! (`flow_instance::viewer_cache`).
//!
//! # The two scopes
//!
//! Visibility is expressed as `Option<&str>`:
//!
//! - `None` — **executor scope**. Nothing is filtered. This is the scope of the
//!   executor's own derivations: the flow engine's `currentState` cache and
//!   `resolved_as` marks, the auto-processor, SDNA loading, and the Prolog fact
//!   base built for engine use. Those passes are executor code reasoning over
//!   executor state, so executor-private is the correct scope for them — see the
//!   closing note on issue #1024. Keeping `None` as a real, named scope is what
//!   lets the user-facing surfaces be filtered without breaking them.
//! - `Some(did)` — **agent scope**. A request arriving through a user-facing
//!   surface (WS RPC, MCP) is attributed to the DID behind its auth token, and
//!   sees only its own Local links. The main agent is not special-cased: it is
//!   an agent like any other and sees the Local links it authored.
//!
//! # Where the filter is applied
//!
//! At query time, in two places, both of which read the `author` and `status`
//! that the SPARQL store already reifies on every link
//! (`ad4m://ontology/author`, `ad4m://ontology/status`):
//!
//! 1. [`link_visible_to`] — the row-level predicate, used by the store's link
//!    scan so that `limit` is applied to the *visible* set rather than to a
//!    superset that is then trimmed (a post-filter would silently return short
//!    pages).
//! 2. [`viewer_author_filter`] — the SPARQL fragment injected into the
//!    generated model-query SQL, so the model-query engine filters inside the
//!    store instead of hydrating rows it must then throw away.
//! 3. [`viewer_reifier_filter`] — the same rule inside the model-query link
//!    guard (`model_query::sparql_builder::LinkGuard`), which every link that
//!    *selects* an instance passes: `where`, the class's flags, the count,
//!    order keys, scopes and walks, and the targets a typed relation's
//!    generated getter lists. It sits on the same reifier as the
//!    `linkStatus` and signature checks, so two different links cannot pass
//!    them between them.
//!
//! Neither is a new mechanism nor a per-user materialised cache: both are
//! query-time predicates over data that is already stored per link.
//!
//! The per-owner `link-added`, `link-removed` and `link-updated` events go
//! only to owners [`decorated_visible_to`] admits. The user-facing reads that
//! still run in executor scope (raw SPARQL, `evaluateGetters`, hand-written
//! getters, `getSubjectData`'s author and timestamp) are tracked in
//! <https://github.com/coasys/ad4m/issues/1152>.

use crate::agent::{did_for_context, AgentContext};
use crate::perspectives::model_query::utils::escape_sparql_string;
use crate::types::{DecoratedLinkExpression, LinkStatus};
use deno_core::anyhow::Error as AnyError;

/// The `ad4m://ontology/status` value written for a Local link.
const STATUS_LOCAL_LITERAL: &str = "Local";

/// Resolve the visibility scope of a request from the agent it is attributed
/// to — the translation from "who is calling" to the `Option<&str>` the rest
/// of this module speaks.
///
/// Always agent scope: a request is never promoted to executor scope (`None`).
///
/// Fails closed: a request whose DID cannot be resolved is returned as an
/// error, because executor scope would show it every other user's Local links.
/// This includes the main agent. `is_main_agent` is true for every token that
/// carries no user email, not only on a fresh executor. An executor that has
/// managed users but no main-agent DID must not answer that request with
/// everyone's Local links.
pub fn viewer_did_for_context(context: &AgentContext) -> Result<Option<String>, AnyError> {
    did_for_context(context).map(Some)
}

/// Is a link with this `author` / `status` visible to `viewer_did`?
///
/// `viewer_did == None` is executor scope and sees everything; see the module
/// docs for why that scope still exists.
pub fn link_visible_to(
    author: &str,
    status: Option<&LinkStatus>,
    viewer_did: Option<&str>,
) -> bool {
    match viewer_did {
        // Executor scope: the executor's own derivations read the whole row set.
        None => true,
        // Agent scope: Local links are private to their author; everything
        // else is unaffected.
        Some(did) => !matches!(status, Some(LinkStatus::Local)) || author == did,
    }
}

/// [`link_visible_to`] for an already-decorated link.
pub fn decorated_visible_to(link: &DecoratedLinkExpression, viewer_did: Option<&str>) -> bool {
    link_visible_to(&link.author, link.status.as_ref(), viewer_did)
}

/// Drop the links `viewer_did` may not see.
///
/// Used by read paths that receive a materialised `Vec` they did not build
/// themselves. Paths that own their scan should pass the viewer down into the
/// scan instead, so that `limit` counts visible rows.
pub fn filter_visible(
    links: Vec<DecoratedLinkExpression>,
    viewer_did: Option<&str>,
) -> Vec<DecoratedLinkExpression> {
    match viewer_did {
        None => links,
        Some(_) => links
            .into_iter()
            .filter(|l| decorated_visible_to(l, viewer_did))
            .collect(),
    }
}

/// SPARQL fragment restricting `Local` links to those authored by the viewer.
///
/// Emitted into generated model queries next to `local_status_filter`'s output;
/// the two compose rather than compete. `local_status_filter` (#1028)
/// asks *"is this link's status right for a property the class declared local?"*
/// — a question about the shape. This asks *"may this viewer see a Local link?"*
/// — a question about the requesting agent. A row must pass both.
///
/// The caller supplies the variable names already bound in the surrounding
/// query: `reifier_var` is the reification subject carrying the annotations and
/// `author_var` is the already-selected author, both **without** the leading
/// `?`. A distinct `?_viewer_status` variable is bound here so this fragment
/// never collides with `local_status_filter`'s `?_status`.
///
/// Returns an empty string in executor scope, leaving the generated query
/// byte-identical to before.
///
/// # Unbound status
///
/// Unlike `local_status_filter`, an *unannotated* link passes this filter: a
/// link with no status is not Local, so it is not user-private, and hiding it
/// would hide ordinary Shared data from its own author. The unbound case is
/// handled explicitly with `!BOUND(...)` rather than left to an evaluation
/// error.
pub fn viewer_author_filter(
    viewer_did: Option<&str>,
    reifier_var: &str,
    author_var: &str,
) -> String {
    author_filter(viewer_did, reifier_var, author_var, "_viewer_status")
}

/// The visibility check on an already-joined `reifier` (given **with** the
/// leading `?`), for the model-query link guard, which puts every check a link
/// must pass on one reifier (`model_query::sparql_builder::LinkGuard`).
///
/// Binds the link's author and status as `{reifier}_va` / `{reifier}_vs`, so
/// the names are unique as long as the reifier is.
///
/// Unlike [`viewer_author_filter`], the status is a required pattern here, not
/// an `OPTIONAL`: a link with no status does not pass. The store writes a
/// status on every link and refuses one without it
/// (`SparqlStore::add_link`), so only data written before that can lack one,
/// and for the links that *select* instances withholding it is the fail-closed
/// side. It is also what keeps the guard cheap: an `OPTIONAL` inside the
/// selection join made a viewer's page of 50 over 3000 instances take seconds
/// (`test_perf_guarded_selection_paginated_query`).
///
/// Returns an empty string in executor scope.
pub fn viewer_reifier_filter(viewer_did: Option<&str>, reifier: &str) -> String {
    let Some(did) = viewer_did else {
        return String::new();
    };
    let bare = reifier.trim_start_matches('?');
    format!(
        " ?{bare} <ad4m://ontology/author> ?{bare}_va . ?{bare} <ad4m://ontology/status> ?{bare}_vs . FILTER(?{bare}_vs != \"{STATUS_LOCAL_LITERAL}\" || ?{bare}_va = \"{}\")",
        escape_sparql_string(did)
    )
}

/// [`viewer_author_filter`] with a caller-chosen status variable.
fn author_filter(
    viewer_did: Option<&str>,
    reifier_var: &str,
    author_var: &str,
    status_var: &str,
) -> String {
    let Some(did) = viewer_did else {
        return String::new();
    };

    // The DID lands inside a SPARQL string literal; escape the characters that
    // could otherwise terminate it. DIDs are not attacker-chosen here (they come
    // from the wallet / agent store, not from request parameters), but a read
    // filter is the wrong place to rely on that.
    let escaped = escape_sparql_string(did);

    format!(
        "    OPTIONAL {{ ?{reifier_var} <ad4m://ontology/status> ?{status_var} . }}\n    FILTER(!BOUND(?{status_var}) || ?{status_var} != \"{STATUS_LOCAL_LITERAL}\" || ?{author_var} = \"{escaped}\")\n"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    const ALICE: &str = "did:key:alice";
    const BOB: &str = "did:key:bob";

    #[test]
    fn executor_scope_sees_every_link() {
        assert!(link_visible_to(ALICE, Some(&LinkStatus::Local), None));
        assert!(link_visible_to(ALICE, Some(&LinkStatus::Shared), None));
        assert!(link_visible_to(ALICE, None, None));
    }

    #[test]
    fn local_links_are_private_to_their_author() {
        assert!(link_visible_to(
            ALICE,
            Some(&LinkStatus::Local),
            Some(ALICE)
        ));
        assert!(!link_visible_to(ALICE, Some(&LinkStatus::Local), Some(BOB)));
    }

    #[test]
    fn shared_and_unannotated_links_stay_visible_to_everyone() {
        assert!(link_visible_to(ALICE, Some(&LinkStatus::Shared), Some(BOB)));
        assert!(link_visible_to(ALICE, None, Some(BOB)));
    }

    #[test]
    fn filter_is_identity_in_executor_scope() {
        let filter = viewer_author_filter(None, "_reifier", "author");
        assert_eq!(filter, "");
    }

    #[test]
    fn filter_binds_its_own_status_variable_and_the_viewer_did() {
        let filter = viewer_author_filter(Some(ALICE), "_reifier", "author");
        // Must not reuse `?_status`, which `local_status_filter` owns.
        assert!(!filter.contains("?_status"));
        assert!(filter.contains("?_viewer_status"));
        assert!(filter.contains(ALICE));
        assert!(filter.contains("?author ="));
    }

    #[test]
    fn reifier_filter_is_identity_in_executor_scope() {
        assert_eq!(viewer_reifier_filter(None, "?_sr"), "");
    }

    #[test]
    fn reifier_filter_names_its_variables_after_the_reifier() {
        let filter = viewer_reifier_filter(Some(ALICE), "?_sr");
        assert!(filter.contains("?_sr <ad4m://ontology/author> ?_sr_va ."));
        // Required, not OPTIONAL: see the function's docs.
        assert!(filter.contains("?_sr <ad4m://ontology/status> ?_sr_vs ."));
        assert!(!filter.contains("OPTIONAL"));
        assert!(filter.contains(&format!("?_sr_va = \"{ALICE}\"")));
    }

    /// A main-agent request with no resolvable DID must be refused. Before this
    /// change it resolved to `Ok(None)`, which is executor scope, so the request
    /// saw every managed user's Local links. `is_main_agent` is true for any
    /// token without a user email, so this is not only the fresh-executor case.
    #[test]
    fn main_agent_without_a_did_fails_closed() {
        use crate::agent::AgentService;
        crate::test_utils::setup_wallet();
        AgentService::init_global_test_instance();

        let saved = AgentService::with_mutable_global_instance(|a| a.did.take());
        let result = viewer_did_for_context(&AgentContext::main_agent());
        AgentService::with_mutable_global_instance(|a| a.did = saved);

        assert!(
            result.is_err(),
            "an unresolvable main-agent DID must not become executor scope, got {result:?}"
        );
    }

    #[test]
    fn main_agent_with_a_did_is_agent_scope() {
        use crate::agent::AgentService;
        crate::test_utils::setup_wallet();
        AgentService::init_global_test_instance();

        let did = AgentService::with_global_instance(|a| a.did.clone()).expect("test agent DID");
        assert_eq!(
            viewer_did_for_context(&AgentContext::main_agent()).unwrap(),
            Some(did)
        );
    }

    #[test]
    fn filter_escapes_quotes_in_the_viewer_did() {
        let filter = viewer_author_filter(Some("did:key:a\"b"), "_reifier", "author");
        assert!(filter.contains("did:key:a\\\"b"));
    }
}

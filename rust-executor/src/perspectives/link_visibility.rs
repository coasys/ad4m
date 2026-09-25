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
//! One exception: a Local link on an [`ENGINE_DERIVED_PREDICATES`] predicate
//! is visible to every viewer. Those links are the executor's own
//! derivations (the flow engine's `currentState` cache). The engine writes
//! them under whichever agent's request triggered the pass, but they describe
//! the executor's view, not that agent's. Hiding them per author would show a
//! co-owner an instance with no state after another user moved it. Because
//! the exemption would otherwise admit *any* author's Local link on these
//! predicates, the same list is reserved for the engine on the write side:
//! [`ensure_not_engine_reserved`] refuses them on every user-facing write.
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
use crate::perspectives::flow_classes::FLOW_CURRENT_STATE_PREDICATE;
use crate::perspectives::model_query::utils::escape_sparql_string;
use crate::types::{DecoratedLinkExpression, Link, LinkStatus};
use deno_core::anyhow::{anyhow, Error as AnyError};

/// The `ad4m://ontology/status` value written for a Local link.
const STATUS_LOCAL_LITERAL: &str = "Local";

/// Predicates whose Local links are the executor's own derivations.
///
/// This one list drives both halves of the rule:
///
/// - **Read:** a Local link on one of these predicates is visible to every
///   viewer, whoever authored it ([`link_visible_to`], and the SPARQL
///   fragments below).
/// - **Write:** no user-facing write may add a link on one of these
///   predicates ([`ensure_not_engine_reserved`]). Only engine code running
///   inside [`engine_derivation`] may.
///
/// The write half is what keeps the read half safe. Without it, a second user
/// on the same executor could add a Local `currentState` for someone else's
/// flow instance, and the exemption would show it to everyone.
pub const ENGINE_DERIVED_PREDICATES: &[&str] = &[FLOW_CURRENT_STATE_PREDICATE];

/// Is `predicate` one of the [`ENGINE_DERIVED_PREDICATES`]?
pub fn is_engine_derived(predicate: Option<&str>) -> bool {
    predicate.is_some_and(|p| ENGINE_DERIVED_PREDICATES.contains(&p))
}

tokio::task_local! {
    /// Set while engine code writes one of its derivations. See
    /// [`engine_derivation`].
    static ENGINE_DERIVATION: ();
}

/// Run `write` as an engine derivation, the only context in which links on
/// [`ENGINE_DERIVED_PREDICATES`] may be added.
///
/// The scope is task-local, so it covers exactly the awaited future. A
/// request handler that is not inside this scope cannot enter it, whichever
/// agent it runs as.
pub async fn engine_derivation<F: std::future::Future>(write: F) -> F::Output {
    ENGINE_DERIVATION.scope((), write).await
}

/// Refuse a link write that would add a link on an engine-reserved predicate,
/// unless it runs inside [`engine_derivation`].
///
/// Called by every `PerspectiveInstance` method that adds locally authored
/// links (`add_link_expression`, and through it `add_link`; `add_links`;
/// `link_mutations`; `update_link`). Every user-facing surface reaches the
/// store through one of those, including model create/update, SDNA commands
/// and the MCP tools, so this one check covers all of them. Links arriving
/// from the link language (peer sync) take a different path and are not
/// checked: a peer's `Shared` link on these predicates is not a Local link,
/// so the read-side exemption never applies to it.
///
/// Removals are not checked. Removing another user's cache only clears it,
/// and the engine writes it again on its next pass.
pub fn ensure_not_engine_reserved<'a>(
    links: impl IntoIterator<Item = &'a Link>,
) -> Result<(), AnyError> {
    if ENGINE_DERIVATION.try_with(|_| ()).is_ok() {
        return Ok(());
    }
    for link in links {
        if is_engine_derived(link.predicate.as_deref()) {
            return Err(anyhow!(
                "the predicate `{}` is reserved for the executor's flow engine and cannot be written directly (source `{}`)",
                link.predicate.as_deref().unwrap_or_default(),
                link.source
            ));
        }
    }
    Ok(())
}

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

/// Is a link with this `author` / `status` / `predicate` visible to
/// `viewer_did`?
///
/// `viewer_did == None` is executor scope and sees everything; see the module
/// docs for why that scope still exists.
pub fn link_visible_to(
    author: &str,
    status: Option<&LinkStatus>,
    predicate: Option<&str>,
    viewer_did: Option<&str>,
) -> bool {
    match viewer_did {
        // Executor scope: the executor's own derivations read the whole row set.
        None => true,
        // Agent scope: Local links are private to their author, except the
        // engine's derivations; everything else is unaffected.
        Some(did) => {
            !matches!(status, Some(LinkStatus::Local))
                || author == did
                || is_engine_derived(predicate)
        }
    }
}

/// [`link_visible_to`] for an already-decorated link.
pub fn decorated_visible_to(link: &DecoratedLinkExpression, viewer_did: Option<&str>) -> bool {
    link_visible_to(
        &link.author,
        link.status.as_ref(),
        link.data.predicate.as_deref(),
        viewer_did,
    )
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
/// query: `reifier_var` is the reification subject carrying the annotations,
/// `author_var` is the already-selected author and `predicate_var` the
/// link's predicate, all **without** the leading `?`. `predicate_var` is what
/// lets [`ENGINE_DERIVED_PREDICATES`] through for every viewer. A distinct `?_viewer_status` variable is bound here so this fragment
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
    predicate_var: &str,
) -> String {
    author_filter(
        viewer_did,
        reifier_var,
        author_var,
        "_viewer_status",
        &format!("?{predicate_var}"),
    )
}

/// The visibility check on an already-joined `reifier` (given **with** the
/// leading `?`), for the model-query link guard, which puts every check a link
/// must pass on one reifier (`model_query::sparql_builder::LinkGuard`).
///
/// Binds the link's author and status as `{reifier}_va` / `{reifier}_vs`, so
/// the names are unique as long as the reifier is. `predicate_term` is the
/// link's predicate (`<iri>` or a bound variable), which lets the
/// [`ENGINE_DERIVED_PREDICATES`] through.
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
pub fn viewer_reifier_filter(
    viewer_did: Option<&str>,
    reifier: &str,
    predicate_term: &str,
) -> String {
    let Some(did) = viewer_did else {
        return String::new();
    };
    let bare = reifier.trim_start_matches('?');
    format!(
        " ?{bare} <ad4m://ontology/author> ?{bare}_va . ?{bare} <ad4m://ontology/status> ?{bare}_vs . FILTER(?{bare}_vs != \"{STATUS_LOCAL_LITERAL}\" || ?{bare}_va = \"{}\" || {predicate_term} IN ({}))",
        escape_sparql_string(did),
        engine_derived_terms()
    )
}

/// [`ENGINE_DERIVED_PREDICATES`] as a SPARQL term list for `IN ( … )`.
fn engine_derived_terms() -> String {
    ENGINE_DERIVED_PREDICATES
        .iter()
        .map(|p| format!("<{p}>"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// [`viewer_author_filter`] with a caller-chosen status variable, and the
/// predicate given as a SPARQL term (`?var` or `<iri>`).
fn author_filter(
    viewer_did: Option<&str>,
    reifier_var: &str,
    author_var: &str,
    status_var: &str,
    predicate_term: &str,
) -> String {
    let Some(did) = viewer_did else {
        return String::new();
    };

    // The DID lands inside a SPARQL string literal; escape the characters that
    // could otherwise terminate it. DIDs are not attacker-chosen here (they come
    // from the wallet / agent store, not from request parameters), but a read
    // filter is the wrong place to rely on that.
    let escaped = escape_sparql_string(did);
    let engine_derived = engine_derived_terms();

    format!(
        "    OPTIONAL {{ ?{reifier_var} <ad4m://ontology/status> ?{status_var} . }}\n    FILTER(!BOUND(?{status_var}) || ?{status_var} != \"{STATUS_LOCAL_LITERAL}\" || ?{author_var} = \"{escaped}\" || {predicate_term} IN ({engine_derived}))\n"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    const ALICE: &str = "did:key:alice";
    const BOB: &str = "did:key:bob";

    #[test]
    fn executor_scope_sees_every_link() {
        assert!(link_visible_to(ALICE, Some(&LinkStatus::Local), None, None));
        assert!(link_visible_to(
            ALICE,
            Some(&LinkStatus::Shared),
            None,
            None
        ));
        assert!(link_visible_to(ALICE, None, None, None));
    }

    #[test]
    fn local_links_are_private_to_their_author() {
        assert!(link_visible_to(
            ALICE,
            Some(&LinkStatus::Local),
            None,
            Some(ALICE)
        ));
        assert!(!link_visible_to(
            ALICE,
            Some(&LinkStatus::Local),
            None,
            Some(BOB)
        ));
    }

    #[test]
    fn shared_and_unannotated_links_stay_visible_to_everyone() {
        assert!(link_visible_to(
            ALICE,
            Some(&LinkStatus::Shared),
            None,
            Some(BOB)
        ));
        assert!(link_visible_to(ALICE, None, None, Some(BOB)));
    }

    #[test]
    fn filter_is_identity_in_executor_scope() {
        let filter = viewer_author_filter(None, "_reifier", "author", "predicate");
        assert_eq!(filter, "");
    }

    #[test]
    fn filter_binds_its_own_status_variable_and_the_viewer_did() {
        let filter = viewer_author_filter(Some(ALICE), "_reifier", "author", "predicate");
        // Must not reuse `?_status`, which `local_status_filter` owns.
        assert!(!filter.contains("?_status"));
        assert!(filter.contains("?_viewer_status"));
        assert!(filter.contains(ALICE));
        assert!(filter.contains("?author ="));
    }

    #[test]
    fn reifier_filter_is_identity_in_executor_scope() {
        assert_eq!(viewer_reifier_filter(None, "?_sr", "<ad4m://p>"), "");
    }

    #[test]
    fn reifier_filter_names_its_variables_after_the_reifier() {
        let filter = viewer_reifier_filter(Some(ALICE), "?_sr", "<ad4m://p>");
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
        let filter = viewer_author_filter(Some("did:key:a\"b"), "_reifier", "author", "predicate");
        assert!(filter.contains("did:key:a\\\"b"));
    }

    const CACHE: Option<&str> = Some(FLOW_CURRENT_STATE_PREDICATE);

    #[test]
    fn engine_derived_local_links_are_visible_to_every_viewer() {
        assert!(link_visible_to(
            ALICE,
            Some(&LinkStatus::Local),
            CACHE,
            Some(BOB)
        ));
        // The exemption is for the listed predicates only.
        assert!(!link_visible_to(
            ALICE,
            Some(&LinkStatus::Local),
            Some("ad4m://flow/currentstate-lookalike"),
            Some(BOB)
        ));
    }

    #[test]
    fn filters_let_engine_derived_predicates_through() {
        let filter = viewer_author_filter(Some(ALICE), "_reifier", "author", "predicate");
        assert!(filter.contains(&format!("?predicate IN (<{FLOW_CURRENT_STATE_PREDICATE}>)")));
        let guard = viewer_reifier_filter(Some(ALICE), "?_sr", "<ad4m://p>");
        assert!(guard.contains(&format!("<ad4m://p> IN (<{FLOW_CURRENT_STATE_PREDICATE}>)")));
    }

    fn cache_link() -> Link {
        Link {
            source: "ad4m://flow/instance/1".to_string(),
            predicate: CACHE.map(str::to_string),
            target: "literal:string:Done".to_string(),
        }
    }

    #[tokio::test]
    async fn reserved_predicates_are_refused_outside_the_engine_scope() {
        let err = ensure_not_engine_reserved([&cache_link()]).unwrap_err();
        assert!(err.to_string().contains("reserved"), "{err}");

        let other = Link {
            predicate: Some("ad4m://other".to_string()),
            ..cache_link()
        };
        assert!(ensure_not_engine_reserved([&other]).is_ok());

        let inside = engine_derivation(async { ensure_not_engine_reserved([&cache_link()]) }).await;
        assert!(inside.is_ok(), "the engine scope may write it: {inside:?}");
    }
}

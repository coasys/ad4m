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
//!
//! Neither is a new mechanism nor a per-user materialised cache: both are
//! query-time predicates over data that is already stored per link.

use crate::agent::{did_for_context, AgentContext};
use crate::types::{DecoratedLinkExpression, LinkStatus};
use deno_core::anyhow::Error as AnyError;

/// The `ad4m://ontology/status` value written for a Local link.
const STATUS_LOCAL_LITERAL: &str = "Local";

/// Resolve the visibility scope of a request from the agent it is attributed
/// to — the translation from "who is calling" to the `Option<&str>` the rest
/// of this module speaks.
///
/// Fails closed: a request that carries a user identity whose DID cannot be
/// resolved is returned as an error rather than silently promoted to executor
/// scope, because executor scope would show that user everyone else's Local
/// links. The main agent is the one case where an unresolvable DID is not an
/// error: before the agent is initialised or unlocked there is no DID to
/// resolve and no other agent on the executor to hide anything from.
pub fn viewer_did_for_context(context: &AgentContext) -> Result<Option<String>, AnyError> {
    match did_for_context(context) {
        Ok(did) => Ok(Some(did)),
        Err(_) if context.is_main_agent => Ok(None),
        Err(e) => Err(e),
    }
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
    let Some(did) = viewer_did else {
        return String::new();
    };

    // The DID lands inside a SPARQL string literal; escape the characters that
    // could otherwise terminate it. DIDs are not attacker-chosen here (they come
    // from the wallet / agent store, not from request parameters), but a read
    // filter is the wrong place to rely on that.
    let escaped = escape_sparql_literal(did);

    format!(
        "    OPTIONAL {{ ?{reifier_var} <ad4m://ontology/status> ?_viewer_status . }}\n    FILTER(!BOUND(?_viewer_status) || ?_viewer_status != \"{STATUS_LOCAL_LITERAL}\" || ?{author_var} = \"{escaped}\")\n"
    )
}

/// [`viewer_author_filter`] for a query that matches a *triple pattern* rather
/// than one that has already bound a reifier and an author.
///
/// Generated queries that walk edges directly — reverse relations, projection
/// counts — read the raw triple, which carries no author at all. This binds the
/// reifier for `triple_pattern` (given without the surrounding `<<( )>>`, e.g.
/// `?parent <ad4m://has> ?t`), binds its author, and applies the same rule.
/// Emit it at most once per query: like [`viewer_author_filter`] it uses fixed
/// variable names.
///
/// Returns an empty string in executor scope.
pub fn viewer_triple_filter(viewer_did: Option<&str>, triple_pattern: &str) -> String {
    let filter = viewer_author_filter(viewer_did, "_vis_reifier", "_vis_author");
    if filter.is_empty() {
        return String::new();
    }
    format!(
        "    ?_vis_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( {triple_pattern} )>> .\n    ?_vis_reifier <ad4m://ontology/author> ?_vis_author .\n{filter}"
    )
}

/// Escape a value for inclusion in a double-quoted SPARQL string literal.
fn escape_sparql_literal(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out
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
    fn triple_filter_is_identity_in_executor_scope() {
        assert_eq!(viewer_triple_filter(None, "?parent <ad4m://p> ?t"), "");
    }

    #[test]
    fn triple_filter_binds_the_reifier_of_the_pattern_it_is_given() {
        let filter = viewer_triple_filter(Some(ALICE), "?parent <ad4m://p> ?t");
        assert!(filter.contains("<<( ?parent <ad4m://p> ?t )>>"));
        assert!(filter.contains("?_vis_reifier <ad4m://ontology/author> ?_vis_author"));
        assert!(filter.contains(ALICE));
    }

    #[test]
    fn filter_escapes_quotes_in_the_viewer_did() {
        let filter = viewer_author_filter(Some("did:key:a\"b"), "_reifier", "author");
        assert!(filter.contains("did:key:a\\\"b"));
    }
}

//! Telepresence status of the agents this executor hosts, per neighbourhood.
//!
//! All users of a multi-user executor who join one neighbourhood share the
//! node's single link-language instance, and a link language holds one online
//! status per node. Without this module the last user to set a status
//! overwrites everyone else's, and co-located users see each other with an
//! empty status (the link language never reports its own node).
//!
//! This module keeps each hosted agent's own signed status:
//!
//! - Co-located users read each other's status from here
//!   ([`status_of`], used by `PerspectiveInstance::online_agents`).
//! - Remote nodes get every hosted agent's status in the one status the link
//!   language publishes: a bundle whose links each carry one agent's signed
//!   status ([`to_publish`]). The receiving executor unpacks it into one
//!   `OnlineAgent` per agent ([`unpack`]). It keeps a bundled status only if
//!   the signature verifies against the status's own author, so the publishing
//!   node cannot speak for an agent it does not hold a signature from.
//!
//! ```text
//!   node X (Alice, Bob)                              node Y (Carol)
//!   Alice: set_online_status ─┐
//!   Bob:   set_online_status ─┼─► statuses[nh] = {Alice, Bob}
//!                             └─► link language ── bundle{Alice, Bob} ──► unpack ─► [Alice, Bob]
//!   Bob:   online_agents ◄── statuses[nh][Alice]
//! ```
//!
//! A node whose only status is its main agent's publishes that status
//! unchanged, so a single-user executor looks the same to every peer as
//! before. The store is in memory: after a restart, agents announce again.

use crate::agent::{create_signed_expression, AgentContext};
use crate::types::domain::Perspective;
use crate::types::{
    DecoratedLinkExpression, Link, LinkExpression, LinkStatus, OnlineAgent, PerspectiveExpression,
};
use ad4m_client::literal::{Literal, LiteralValue};
use deno_core::error::AnyError;
use std::collections::HashMap;
use std::sync::Mutex;

/// Predicate of a bundle link. Source: the hosted agent's DID. Target: a
/// `literal:json:` of that agent's signed status.
pub const HOSTED_STATUS_PREDICATE: &str = "ad4m://telepresence/hosted-agent-status";

lazy_static! {
    /// Neighbourhood URL → agent DID → that agent's latest signed status.
    static ref STATUSES: Mutex<HashMap<String, HashMap<String, PerspectiveExpression>>> =
        Mutex::new(HashMap::new());
}

/// Record `status` as its author's status in `neighbourhood`. A status with no
/// links clears it.
pub fn record(neighbourhood: &str, status: &PerspectiveExpression) {
    let mut statuses = STATUSES.lock().expect("hosted status lock poisoned");
    let by_agent = statuses.entry(neighbourhood.to_string()).or_default();
    if status.data.links.is_empty() {
        by_agent.remove(&status.author);
    } else {
        by_agent.insert(status.author.clone(), status.clone());
    }
}

/// The status `did` last set in `neighbourhood` on this executor.
pub fn status_of(neighbourhood: &str, did: &str) -> Option<PerspectiveExpression> {
    STATUSES
        .lock()
        .expect("hosted status lock poisoned")
        .get(neighbourhood)
        .and_then(|by_agent| by_agent.get(did))
        .cloned()
}

/// The status the link language should publish for this node after `latest`
/// was recorded. `is_current(did)` says whether a hosted agent still counts as
/// online; `latest`'s author always does, because it just made a request.
///
/// - No status left: `latest` itself (it has no links, so it clears the node's
///   status).
/// - Only the main agent's status: that status, unchanged.
/// - Otherwise: a bundle, signed by `signer`, with one link per agent.
pub fn to_publish(
    neighbourhood: &str,
    latest: &PerspectiveExpression,
    main_agent_did: &str,
    is_current: impl Fn(&str) -> bool,
    signer: &AgentContext,
) -> Result<PerspectiveExpression, AnyError> {
    let mut current: Vec<PerspectiveExpression> = STATUSES
        .lock()
        .expect("hosted status lock poisoned")
        .get(neighbourhood)
        .map(|by_agent| {
            by_agent
                .values()
                .filter(|s| s.author == latest.author || is_current(&s.author))
                .cloned()
                .collect()
        })
        .unwrap_or_default();
    // Stable order, so the same set of statuses publishes the same bundle.
    current.sort_by(|a, b| a.author.cmp(&b.author));

    match current.as_slice() {
        [] => Ok(latest.clone()),
        [only] if only.author == main_agent_did => Ok(only.clone()),
        _ => bundle(&current, signer),
    }
}

fn bundle(
    statuses: &[PerspectiveExpression],
    signer: &AgentContext,
) -> Result<PerspectiveExpression, AnyError> {
    let mut links = Vec::with_capacity(statuses.len());
    for status in statuses {
        let link = Link {
            source: status.author.clone(),
            predicate: Some(HOSTED_STATUS_PREDICATE.to_string()),
            target: Literal::from_json(serde_json::to_value(status)?).to_url()?,
        };
        let signed = LinkExpression::from(create_signed_expression(link, signer)?);
        links.push(DecoratedLinkExpression::from((signed, LinkStatus::Shared)));
    }
    Ok(PerspectiveExpression::from(create_signed_expression(
        Perspective { links },
        signer,
    )?))
}

/// One `OnlineAgent` per agent a remote node reported, with every status's
/// signatures checked.
///
/// A node's status that is a bundle becomes one entry per bundled status whose
/// signature verifies against its own author; the node's own entry is dropped
/// (its DID is whichever agent the link language happens to name for the
/// node). Any other status passes through, as before. The first entry for a
/// DID wins.
pub fn unpack(agents: Vec<OnlineAgent>) -> Vec<OnlineAgent> {
    let mut out: Vec<OnlineAgent> = Vec::new();
    let push = |agent: OnlineAgent, out: &mut Vec<OnlineAgent>| {
        if !out.iter().any(|a| a.did == agent.did) {
            out.push(agent);
        }
    };
    for mut agent in agents {
        let bundled: Vec<&DecoratedLinkExpression> = agent
            .status
            .data
            .links
            .iter()
            .filter(|l| l.data.predicate.as_deref() == Some(HOSTED_STATUS_PREDICATE))
            .collect();
        if bundled.is_empty() {
            agent.status.verify_signatures();
            push(agent, &mut out);
            continue;
        }
        for link in bundled {
            let Some(mut status) = bundled_status(&link.data.target) else {
                log::warn!(
                    "hosted_status: unreadable bundled status for {} — skipped",
                    link.data.source
                );
                continue;
            };
            status.verify_signatures();
            if status.proof.valid != Some(true) || status.author.is_empty() {
                log::warn!(
                    "hosted_status: bundled status for {} does not verify — skipped",
                    status.author
                );
                continue;
            }
            push(
                OnlineAgent {
                    did: status.author.clone(),
                    status,
                },
                &mut out,
            );
        }
    }
    out
}

fn bundled_status(target: &str) -> Option<PerspectiveExpression> {
    match Literal::from_url(target.to_string()).ok()?.get().ok()? {
        LiteralValue::Json(json) => serde_json::from_value(json).ok(),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::{did_for_context, AgentService};
    use crate::test_utils::{setup_agent, setup_wallet};
    use std::sync::OnceLock;

    static SETUP: OnceLock<()> = OnceLock::new();

    fn ensure_setup() {
        SETUP.get_or_init(|| {
            setup_wallet();
            setup_agent();
        });
    }

    fn user(email: &str) -> (AgentContext, String) {
        AgentService::ensure_user_key_exists(email).expect("user key");
        let ctx = AgentContext::for_user_email(email.to_string());
        let did = did_for_context(&ctx).expect("user did");
        (ctx, did)
    }

    fn status(
        ctx: &AgentContext,
        did: &str,
        predicate: &str,
        target: &str,
    ) -> PerspectiveExpression {
        let link = Link {
            source: did.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        };
        let signed = LinkExpression::from(create_signed_expression(link, ctx).unwrap());
        let links = vec![DecoratedLinkExpression::from((signed, LinkStatus::Shared))];
        PerspectiveExpression::from(create_signed_expression(Perspective { links }, ctx).unwrap())
    }

    fn empty(ctx: &AgentContext) -> PerspectiveExpression {
        PerspectiveExpression::from(
            create_signed_expression(Perspective { links: vec![] }, ctx).unwrap(),
        )
    }

    /// What a remote node reads: the link language names the node by one DID.
    fn as_remote(node_did: &str, published: PerspectiveExpression) -> Vec<OnlineAgent> {
        // Through JSON, as the status travels.
        let json = serde_json::to_string(&published).unwrap();
        vec![OnlineAgent {
            did: node_did.to_string(),
            status: serde_json::from_str(&json).unwrap(),
        }]
    }

    fn targets(agent: &OnlineAgent) -> Vec<String> {
        agent
            .status
            .data
            .links
            .iter()
            .map(|l| l.data.target.clone())
            .collect()
    }

    #[test]
    fn two_hosted_users_both_reach_a_remote_node_and_each_other() {
        ensure_setup();
        let nh = "neighbourhood://hosted-two-users";
        let (alice_ctx, alice) = user("alice@hosted-status");
        let (bob_ctx, bob) = user("bob@hosted-status");
        let main = did_for_context(&AgentContext::main_agent()).unwrap();

        let alice_status = status(&alice_ctx, &alice, "ad4m://session/in-call", "room-1");
        record(nh, &alice_status);
        let bob_status = status(&bob_ctx, &bob, "ad4m://session/in-call", "room-1");
        record(nh, &bob_status);
        let published = to_publish(nh, &bob_status, &main, |_| true, &bob_ctx).unwrap();

        // Co-located: each reads the other's own status.
        assert_eq!(status_of(nh, &alice).map(|s| s.author), Some(alice.clone()));
        assert_eq!(status_of(nh, &bob).map(|s| s.author), Some(bob.clone()));

        // Remote: the node shows up as both users, each with a verified status,
        // not as the last writer.
        let seen = unpack(as_remote(&alice, published));
        let mut dids: Vec<&str> = seen.iter().map(|a| a.did.as_str()).collect();
        dids.sort();
        let mut expected = vec![alice.as_str(), bob.as_str()];
        expected.sort();
        assert_eq!(dids, expected);
        for agent in &seen {
            assert_eq!(agent.status.proof.valid, Some(true), "{}", agent.did);
            assert_eq!(targets(agent), vec!["room-1".to_string()]);
        }

        // Clearing one status removes only that agent from what is published.
        let cleared = empty(&alice_ctx);
        record(nh, &cleared);
        let published = to_publish(nh, &cleared, &main, |_| true, &alice_ctx).unwrap();
        let seen = unpack(as_remote(&alice, published));
        assert_eq!(
            seen.iter().map(|a| a.did.clone()).collect::<Vec<_>>(),
            vec![bob.clone()]
        );
        assert!(status_of(nh, &alice).is_none());
    }

    #[test]
    fn a_bundled_status_that_does_not_verify_is_dropped() {
        ensure_setup();
        let nh = "neighbourhood://hosted-forged";
        let (alice_ctx, alice) = user("alice@hosted-forged");
        let (bob_ctx, bob) = user("bob@hosted-forged");
        let main = did_for_context(&AgentContext::main_agent()).unwrap();

        // Alice's genuine status, and one claiming to be Bob's that Alice signed.
        let genuine = status(&alice_ctx, &alice, "ad4m://session/in-call", "room-1");
        let mut forged = status(&alice_ctx, &alice, "ad4m://session/in-call", "room-1");
        forged.author = bob.clone();
        record(nh, &genuine);
        record(nh, &forged);
        let published = to_publish(nh, &genuine, &main, |_| true, &bob_ctx).unwrap();

        let seen = unpack(as_remote(&main, published));
        assert_eq!(
            seen.iter().map(|a| a.did.clone()).collect::<Vec<_>>(),
            vec![alice]
        );
    }

    #[test]
    fn main_agent_alone_publishes_its_status_unchanged() {
        ensure_setup();
        let nh = "neighbourhood://hosted-main-only";
        let main_ctx = AgentContext::main_agent();
        let main = did_for_context(&main_ctx).unwrap();

        let own = status(&main_ctx, &main, "ad4m://app/mood", "literal:string:ok");
        record(nh, &own);
        let published = to_publish(nh, &own, &main, |_| true, &main_ctx).unwrap();
        assert_eq!(published.proof.signature, own.proof.signature);

        // A remote node reads it as before: the node's own entry, verified.
        let seen = unpack(as_remote(&main, published));
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].did, main);
        assert_eq!(seen[0].status.proof.valid, Some(true));
    }

    /// The link language reports an agent that never set a status with
    /// `status: null`; that must not fail the whole list.
    #[test]
    fn an_agent_without_a_status_reads_as_empty() {
        let agents: Vec<OnlineAgent> =
            serde_json::from_str(r#"[{"did":"did:key:no-status","status":null}]"#).unwrap();
        let seen = unpack(agents);
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].did, "did:key:no-status");
        assert!(seen[0].status.data.links.is_empty());
    }

    #[test]
    fn a_hosted_agent_no_longer_current_is_left_out() {
        ensure_setup();
        let nh = "neighbourhood://hosted-stale";
        let (alice_ctx, alice) = user("alice@hosted-stale");
        let (bob_ctx, bob) = user("bob@hosted-stale");
        let main = did_for_context(&AgentContext::main_agent()).unwrap();

        record(
            nh,
            &status(&alice_ctx, &alice, "ad4m://session/in-call", "room-1"),
        );
        let bob_status = status(&bob_ctx, &bob, "ad4m://session/in-call", "room-1");
        record(nh, &bob_status);
        // Alice went quiet; Bob is the one publishing, so he always counts.
        let published = to_publish(nh, &bob_status, &main, |did| did != alice, &bob_ctx).unwrap();
        let seen = unpack(as_remote(&main, published));
        assert_eq!(
            seen.iter().map(|a| a.did.clone()).collect::<Vec<_>>(),
            vec![bob]
        );
    }
}

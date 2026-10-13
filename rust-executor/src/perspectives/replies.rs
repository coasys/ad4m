//! Which predicates link an item to the replies to it, and a thread's root to
//! everything under it.
//!
//! Containment says what an item is *in*. A reply is something else: it is
//! about an item, and an app that keeps the two apart cannot read a reply as
//! a child. So replies are two roles of their own (`declared_roles.rs`):
//!
//! ```text
//! <app://reply>  <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <ad4m://has_reply>
//! <app://thread> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <ad4m://has_thread_member>
//! ```
//!
//! - `has_reply` runs from an item to each direct reply to it. A reply to a
//!   reply is linked from the reply.
//! - `has_thread_member` runs from a thread's root — the first item above a
//!   reply that is not itself a reply — to every reply under it, at any depth.
//!   It is an index: everything it holds is reachable through `has_reply`
//!   too, in one hop instead of a walk.
//!
//! Both roles are optional, and each degrades on its own. A perspective with
//! no reply declaration still has the base role, which an app may write
//! directly. One that declares replies but no thread index is walked through
//! the replies instead.
//!
//! A declaration means the predicate points the same way as the role. An app
//! whose links point from the reply back to what it answers cannot declare
//! them yet.
//!
//! ## Writing a reply
//!
//! A reply the executor writes (`instance_create(reply_to=…)`) is linked from
//! what it answers through the reply predicate (`reply_write_predicate`), and,
//! where the perspective declares exactly one thread index, from its thread's
//! root too. The thread index is written only where it is declared: a
//! perspective that never asked for one does not get one.

use super::declared_roles::write_predicate;
use super::perspective_instance::PerspectiveInstance;
use crate::types::LinkQuery;
use std::collections::HashSet;
use std::sync::Arc;

/// How far up the replies a walk for a thread's root goes before giving up.
const MAX_THREAD_DEPTH: usize = 32;

/// The role a predicate plays when it links an item to a direct reply to it.
pub const HAS_REPLY: &str = "ad4m://has_reply";

/// The role a predicate plays when it links a thread's root to a reply under it.
pub const HAS_THREAD_MEMBER: &str = "ad4m://has_thread_member";

/// A role's base IRI first, then each predicate declared to play it.
fn with_base(base: &str, declared: &[String]) -> Vec<String> {
    let mut all = vec![base.to_string()];
    all.extend(declared.iter().filter(|p| p.as_str() != base).cloned());
    all
}

impl PerspectiveInstance {
    /// The predicates linking an item to its direct replies: `has_reply`, then
    /// each one this perspective's authors declared.
    pub async fn reply_predicates(&self) -> Arc<Vec<String>> {
        let declared = self.declared_for(HAS_REPLY).await;
        Arc::new(with_base(HAS_REPLY, &declared))
    }

    /// The predicate a reply the executor writes is linked through: the one
    /// declared, when there is exactly one, else `has_reply`.
    pub async fn reply_write_predicate(&self) -> String {
        write_predicate(HAS_REPLY, &self.declared_for(HAS_REPLY).await)
    }

    /// The predicate a reply the executor writes is indexed under its thread's
    /// root through — only where exactly one thread index is declared.
    pub async fn thread_write_predicate(&self) -> Option<String> {
        match self.declared_for(HAS_THREAD_MEMBER).await.as_slice() {
            [only] => Some(only.clone()),
            _ => None,
        }
    }

    /// The root of the thread `item` is in: the first item above it, through
    /// the reply predicates, that is not itself a reply — `item` itself when it
    /// answers nothing. Bounded, and a cycle ends the walk where it closes.
    pub async fn thread_root_of(&self, item: &str) -> String {
        let replies = self.reply_predicates().await;
        let mut current = item.to_string();
        let mut seen = HashSet::from([current.clone()]);
        for _ in 0..MAX_THREAD_DEPTH {
            let mut answered = None;
            for predicate in replies.iter() {
                let links = self
                    .get_links(&LinkQuery {
                        predicate: Some(predicate.clone()),
                        target: Some(current.clone()),
                        ..Default::default()
                    })
                    .await
                    .unwrap_or_default();
                if let Some(link) = links.first() {
                    answered = Some(link.data.source.clone());
                    break;
                }
            }
            match answered {
                Some(parent) if seen.insert(parent.clone()) => current = parent,
                _ => break,
            }
        }
        current
    }

    /// The predicates linking a thread's root to every reply under it:
    /// `has_thread_member`, then each one this perspective's authors declared.
    pub async fn thread_predicates(&self) -> Arc<Vec<String>> {
        let declared = self.declared_for(HAS_THREAD_MEMBER).await;
        Arc::new(with_base(HAS_THREAD_MEMBER, &declared))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::declared_roles::{resolve_declared, tests::declaration};

    #[test]
    fn the_base_role_alone_without_declarations() {
        assert_eq!(with_base(HAS_REPLY, &[]), vec![HAS_REPLY.to_string()]);
    }

    #[test]
    fn the_base_role_first_then_the_trusted_declarations() {
        let trusted = vec!["did:owner".to_string()];
        let links = vec![
            declaration("app://response", HAS_REPLY, "did:owner", true),
            declaration("app://stray", HAS_REPLY, "did:member", true),
            declaration("app://thread", HAS_THREAD_MEMBER, "did:owner", true),
        ];
        assert_eq!(
            with_base(HAS_REPLY, &resolve_declared(&links, &trusted, HAS_REPLY)),
            vec![HAS_REPLY.to_string(), "app://response".to_string()]
        );
        assert_eq!(
            with_base(
                HAS_THREAD_MEMBER,
                &resolve_declared(&links, &trusted, HAS_THREAD_MEMBER)
            ),
            vec![HAS_THREAD_MEMBER.to_string(), "app://thread".to_string()]
        );
    }
}

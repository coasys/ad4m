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

use super::perspective_instance::PerspectiveInstance;
use std::sync::Arc;

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

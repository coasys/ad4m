//! Reads on [`PerspectiveInstance`] as one user.
//!
//! [`PerspectiveInstance::read_as`] returns the instance as one user reads
//! it; every read through it (links, model queries, SPARQL) sees the shared
//! links plus that user's own `Local` links. The store decides what that
//! view holds ([`SparqlStore`](crate::perspectives::sparql_store::SparqlStore),
//! #1224); nothing above it filters. A request surface scopes its handle
//! once, where it resolves the caller (`get_perspective_with_access`,
//! `get_perspective_with_auth`, `get_readable_perspective`), and an engine
//! pass where it starts. An instance that was not scoped reads as the main
//! agent.

use super::perspective_instance::PerspectiveInstance;
use crate::agent::{did_for_context, AgentContext};
use crate::types::{DecoratedLinkExpression, LinkQuery};
use deno_core::error::AnyError;

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

    /// This instance with the shared links only: every read through the
    /// returned clone sees no user's `Local` links, its reader's included
    /// ([`SparqlStore::shared_only`](crate::perspectives::sparql_store::SparqlStore::shared_only)).
    /// For reads whose rows end up in shared links, such as the
    /// auto-processor's existing-instance context, which is rendered into the
    /// prompt and routes Create-vs-Update of the Shared links a pass writes.
    pub fn shared_only(&self) -> PerspectiveInstance {
        let mut scoped = self.clone();
        scoped.sparql_store = std::sync::Arc::new(self.sparql_store.shared_only());
        scoped
    }

    /// [`Self::read_as`] the user `context` acts for. Fails closed: a
    /// context whose DID cannot be resolved is an error, not a read as the
    /// main agent (`is_main_agent` is true for every token without a user
    /// email, so this is not only the fresh-executor case).
    pub fn read_as_context(&self, context: &AgentContext) -> Result<PerspectiveInstance, AnyError> {
        Ok(self.read_as(&did_for_context(context)?))
    }

    /// [`Self::get_links`] as the agent that a write acts for.
    ///
    /// A write that first looks up the links it will remove or replace uses
    /// this, not `get_links`, so it only finds links that agent may remove
    /// (#1024).
    pub async fn get_links_for_context(
        &self,
        q: &LinkQuery,
        context: &AgentContext,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        self.read_as_context(context)?.get_links(q).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentService;
    use crate::types::PerspectiveHandle;

    /// A main-agent request with no resolvable DID must be refused, not read
    /// as some default view.
    #[test]
    fn main_agent_without_a_did_fails_closed() {
        crate::test_utils::setup_wallet();
        AgentService::init_global_test_instance();
        let perspective = PerspectiveInstance::new(
            PerspectiveHandle::new_from_name("fails closed".into()),
            None,
        );

        let saved = AgentService::with_mutable_global_instance(|a| a.did.take());
        let result = perspective.read_as_context(&AgentContext::main_agent());
        AgentService::with_mutable_global_instance(|a| a.did = saved);

        assert!(
            result.is_err(),
            "an unresolvable main-agent DID must not become a read"
        );
    }
}

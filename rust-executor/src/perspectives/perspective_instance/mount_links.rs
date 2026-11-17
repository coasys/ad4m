//! The links a mount writes, told apart from everyone else's by a ledger
//! in the store: every `Local` link a mount writes (records, its version
//! link, its class claims) is stored together with an
//! `ad4m://ontology/mountedBy <lang>` mark on its reifier
//! (`SparqlStore::add_mount_links`), in one transaction. The mark is in the
//! store's own metadata vocabulary, which every link write and import
//! refuses (`is_link_metadata`), so only the mount engine can set it; it
//! goes with the reifier when the link is removed.
//!
//! A mount removes only links it marked ([`MountWritten`]). A user's or
//! app's own link under `<lang>://` — any author, any timestamp, even the
//! same triple as a mounted value — is never swept or adopted, and a
//! `Shared` link on a bookkeeping predicate is never touched.

use std::collections::HashMap;
use std::collections::HashSet;

use deno_core::error::AnyError;

use super::PerspectiveInstance;
use crate::agent::{create_signed_expression, AgentContext};
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{
    DecoratedLinkExpression, DecoratedPerspectiveDiff, Link, LinkExpression, LinkStatus,
};

/// The reifiers one mount wrote, as the ledger lists them.
pub(super) struct MountWritten(HashSet<String>);

impl MountWritten {
    /// Whether the mount wrote `link` (and it is still `Local`).
    pub(super) fn contains(&self, link: &DecoratedLinkExpression) -> bool {
        link.status == Some(LinkStatus::Local)
            && self
                .0
                .contains(&SparqlStore::reifier_of(&LinkExpression::from(
                    link.clone(),
                )))
    }
}

/// [`MountWritten`] per mounted language, read once per language.
pub(super) struct MountWrittenByLanguage<'a> {
    perspective: &'a PerspectiveInstance,
    read: HashMap<String, MountWritten>,
}

impl MountWrittenByLanguage<'_> {
    /// Whether the mount of `lang` wrote `link`.
    pub(super) fn wrote(
        &mut self,
        lang: &str,
        link: &DecoratedLinkExpression,
    ) -> Result<bool, AnyError> {
        if !self.read.contains_key(lang) {
            let written = self.perspective.mount_written(lang)?;
            self.read.insert(lang.to_string(), written);
        }
        Ok(self.read[lang].contains(link))
    }
}

impl PerspectiveInstance {
    /// The links the mount of `lang` wrote.
    pub(super) fn mount_written(&self, lang: &str) -> Result<MountWritten, AnyError> {
        Ok(MountWritten(self.sparql_store.mount_written(lang)?))
    }

    /// A [`MountWrittenByLanguage`] over this perspective.
    pub(super) fn mount_written_by_language(&self) -> MountWrittenByLanguage<'_> {
        MountWrittenByLanguage {
            perspective: self,
            read: HashMap::new(),
        }
    }

    /// Write `links` as the mount of `lang`'s own `Local` links, signed by
    /// the main agent: stored and marked in one transaction (under
    /// `persist_lock`, as every store write), then Prolog facts and the
    /// link events as for any write (`Local`: no commit). Returns how many
    /// it wrote.
    pub(super) async fn add_mount_links(
        &mut self,
        lang: &str,
        links: Vec<Link>,
    ) -> Result<usize, AnyError> {
        let context = AgentContext::main_agent();
        let mut expressions = Vec::with_capacity(links.len());
        for link in links {
            link.validate()?;
            let mut expression =
                LinkExpression::from(create_signed_expression(link.normalize(), &context)?);
            expression.status = Some(LinkStatus::Local);
            expressions.push(expression);
        }
        {
            let _persisting = self
                .persist_lock
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            self.sparql_store.add_mount_links(&expressions, lang)?;
        }
        let count = expressions.len();
        let decorated = DecoratedPerspectiveDiff::from_additions(
            expressions
                .into_iter()
                .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Local)))
                .collect(),
        );
        self.spawn_prolog_facts_update(decorated.clone(), None);
        self.pubsub_publish_diff(&decorated).await;
        Ok(count)
    }
}

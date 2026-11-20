//! The revocation notices this executor writes into a perspective
//! ([`crate::identity::verdict::NOTICE_PREDICATE`]): one per link a now-revoked key signed
//! that this store still counts — the links this executor held as `valid` when the
//! revocation reached it. A notice names the link's reifier as its source and the
//! revoking identity's `did:scid` as its target, and a still-valid device key of the
//! same identity signs it anchored at the seq the `revoke` sits at or past it: impossible
//! before the revocation was learnt, so a `valid` notice is shared, checkable proof the
//! vouched link entered the space first ([`SparqlStore::vouched_by_notice`]). That is what
//! orders link and revocation for a reader that synced after both — it sees the vouched
//! link `valid` where arrival order alone would leave it `flagged`.

use deno_core::error::AnyError;

use super::PerspectiveInstance;
use crate::agent::{create_signed_link, AgentContext};
use crate::identity::verdict::{LinkVerdict, NOTICE_PREDICATE};
use crate::perspectives::sparql_store::reifier_of;
use crate::types::{Link, LinkExpression, LinkStatus};

/// Links of the revoked signer scanned per page, as [`super::RECHECK_BATCH`] bounds a
/// re-check's pages.
const NOTICE_BATCH: usize = super::RECHECK_BATCH;

impl PerspectiveInstance {
    /// Writes a revocation notice for every stored `valid` link of `author` (a `did:key`)
    /// that no notice covers yet, signed by `context` — a device key of `person`'s
    /// identity the revocation left valid. `Shared` while the perspective is a
    /// neighbourhood's (the notice must reach the space for a late syncer to check it),
    /// `Local` otherwise. The number of notices written.
    pub(crate) async fn publish_revocation_notices(
        &self,
        author: &str,
        person: &str,
        context: &AgentContext,
    ) -> Result<usize, AnyError> {
        let shared = self.persisted.lock().await.neighbourhood.is_some();
        let status = if shared {
            LinkStatus::Shared
        } else {
            LinkStatus::Local
        };
        let mut written = 0;
        let mut after: Option<String> = None;
        loop {
            let stored = self.sparql_store.links_by_authors(
                &[author.to_string()],
                after.as_deref(),
                NOTICE_BATCH,
            )?;
            let full = stored.len() == NOTICE_BATCH;
            after = stored.last().map(reifier_of);
            for link in stored {
                if link.verdict != Some(LinkVerdict::Valid) {
                    continue;
                }
                let expression = LinkExpression::from(link.clone());
                if self.sparql_store.vouched_by_notice(&expression)? {
                    continue;
                }
                let notice: LinkExpression = create_signed_link(
                    Link {
                        source: reifier_of(&link),
                        predicate: Some(NOTICE_PREDICATE.into()),
                        target: person.to_string(),
                    },
                    context,
                )?
                .into();
                self.clone()
                    .add_link_expression_inner(notice, status.clone(), None, true)
                    .await?;
                written += 1;
            }
            if !full {
                break;
            }
        }
        Ok(written)
    }
}

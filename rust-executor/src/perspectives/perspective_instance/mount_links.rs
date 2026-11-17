//! The links a mount writes, told apart from everyone else's. A mount
//! signs every `Local` link it writes (records, its version link, its
//! class claims) as the main agent at one fixed timestamp,
//! [`MOUNT_TIMESTAMP`], instead of the time of writing. So:
//!
//! - a mount removes only links that carry it ([`is_mount_written`]): a
//!   user's own `Local` link under `<lang>://`, or a `Shared` one on a
//!   bookkeeping predicate, is never swept, even when its triple equals
//!   one the mount wrote (the store keeps a triple while any reifier
//!   holds it);
//! - the same link always has the same identity (the reifier hashes
//!   author, triple and timestamp), so a link written twice is one link,
//!   and the mirror can be dropped and rebuilt to the same state.
//!
//! The mirror is derived data: nothing reads a mounted link's time.

use chrono::{DateTime, Utc};
use deno_core::error::AnyError;

use super::PerspectiveInstance;
use crate::agent::{did_for_context, sign_for_context, signing_key_id_for_context, AgentContext};
use crate::types::{
    DecoratedLinkExpression, DecoratedPerspectiveDiff, Expression, ExpressionProof, Link,
    LinkExpression, LinkStatus, PerspectiveDiff,
};

/// The timestamp of every link a mount writes: the Unix epoch, as
/// `create_signed_expression` renders a time.
pub(super) const MOUNT_TIMESTAMP: &str = "1970-01-01T00:00:00.000Z";

/// Whether the mount wrote `link`: `Local`, at [`MOUNT_TIMESTAMP`].
pub(super) fn is_mount_written(link: &DecoratedLinkExpression) -> bool {
    link.status == Some(LinkStatus::Local) && link.timestamp == MOUNT_TIMESTAMP
}

/// `link` signed by the main agent at [`MOUNT_TIMESTAMP`].
fn mount_link_expression(link: Link) -> Result<LinkExpression, AnyError> {
    let context = AgentContext::main_agent();
    let data = link.normalize();
    let signature = hex::encode(sign_for_context(
        &crate::agent::signatures::hash_data_and_timestamp(&data, &DateTime::<Utc>::UNIX_EPOCH),
        &context,
    )?);
    Ok(LinkExpression::from(Expression {
        author: did_for_context(&context)?,
        timestamp: MOUNT_TIMESTAMP.to_string(),
        data,
        proof: ExpressionProof {
            key: signing_key_id_for_context(&context)?,
            signature,
        },
    }))
}

impl PerspectiveInstance {
    /// Write `links` as the mount's own `Local` links — `add_links`
    /// without a batch, signed at [`MOUNT_TIMESTAMP`]: stored, then Prolog
    /// facts and the link events as for any write (`Local`: no commit).
    /// Returns how many it wrote.
    pub(super) async fn add_mount_links(&mut self, links: Vec<Link>) -> Result<usize, AnyError> {
        let mut expressions = Vec::with_capacity(links.len());
        for link in links {
            link.validate()?;
            let mut expression = mount_link_expression(link)?;
            expression.status = Some(LinkStatus::Local);
            expressions.push(expression);
        }
        let count = expressions.len();
        let mut decorated = DecoratedPerspectiveDiff::from_additions(
            expressions
                .iter()
                .cloned()
                .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Local)))
                .collect(),
        );
        self.persist_link_diff(&PerspectiveDiff::from_additions(expressions))
            .await?
            .fold_into(&mut decorated);
        self.spawn_prolog_facts_update(decorated.clone(), None);
        self.pubsub_publish_diff(&decorated).await;
        Ok(count)
    }
}

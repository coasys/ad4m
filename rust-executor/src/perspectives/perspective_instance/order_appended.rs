//! The ordering entry for members just appended to an ordered collection.

use super::PerspectiveInstance;
use crate::agent::AgentContext;
use crate::types::LinkStatus;
use deno_core::error::AnyError;

impl PerspectiveInstance {
    /// Write the ordering entry for `added` members just appended to `source`'s `predicate`
    /// collection, whose members before the append were `members` (target, timestamp). A
    /// collection whose class declares no ordering gets nothing. The members keep their order
    /// and the new ones follow, so this is `apply_collection_ordering` for an append.
    #[allow(clippy::too_many_arguments)]
    pub(crate) async fn order_appended(
        &mut self,
        source: &str,
        predicate: &str,
        members: &[(String, String)],
        added: &[String],
        status: LinkStatus,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        let mut desired: Vec<String> = members.iter().map(|(target, _)| target.clone()).collect();
        for target in added {
            if !desired.contains(target) {
                desired.push(target.clone());
            }
        }
        self.apply_collection_ordering(
            source, predicate, members, &desired, true, status, batch_id, context,
        )
        .await
    }
}

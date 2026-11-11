//! How many of a perspective's shared links one pass of
//! `share_links_missing_remotely` may send. The fallback sync catches up the
//! few links a commit missed; when it finds most of the shared set missing,
//! the link language's view of the remote is wrong (an empty or partial
//! mirror), and sending would push the room's whole set again.

use super::perspective_instance::{links_missing_remotely, PerspectiveInstance};
use crate::types::{LinkExpression, LinkStatus, PerspectiveDiff};

/// Who asks for the shared links the remote lacks.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ShareScope {
    /// The background fallback sync (`fallback_sync_loop`).
    Fallback,
    /// The neighbourhood's creator, at publish: the links made before the
    /// neighbourhood existed were never sent, so all of them may be missing.
    Publish,
}

/// A fallback pass may always send up to this many links.
pub(crate) const FALLBACK_ALWAYS_SENDS: usize = 20;

/// Whether a pass that finds `missing` of the perspective's `shared` links
/// absent remotely must send nothing: a fallback pass that would send more
/// than `FALLBACK_ALWAYS_SENDS` links and more than half the shared set.
pub(crate) fn fallback_push_refused(missing: usize, shared: usize, scope: ShareScope) -> bool {
    scope == ShareScope::Fallback && missing > FALLBACK_ALWAYS_SENDS && missing * 2 > shared
}

impl PerspectiveInstance {
    /// The background fallback sync: commits the shared links of the
    /// perspective's local agents (`local_agents`: its owners) that the link
    /// language's `render()` lacks, unless that is most of them
    /// (`fallback_push_refused`). Other agents' links are their authors' to
    /// share. False when it sent nothing it should have, so the caller's
    /// backoff retries.
    pub async fn ensure_public_links_are_shared(&self) -> bool {
        self.share_links_missing_remotely(ShareScope::Fallback)
            .await
    }

    /// The creator's first push after publishing a perspective as a
    /// neighbourhood: every shared link the new room lacks, however many.
    /// When that push fails (the language refuses the commit, or cannot say
    /// yet what the room holds), the links go to the pending-diffs queue: the
    /// background fallback refuses a push of the whole set, and the queue
    /// retries with backoff, across restarts, until the language takes them.
    pub async fn share_links_after_publish(&self) -> bool {
        self.share_links_missing_remotely(ShareScope::Publish).await
            || self.queue_unsent_shared_links()
    }

    /// Queues, as one pending diff, every Shared link that no queued or parked
    /// diff holds. A link the room already has is sent again; the link
    /// language drops the duplicate. False when the queue could not be written.
    fn queue_unsent_shared_links(&self) -> bool {
        let shared: Vec<(LinkExpression, LinkStatus)> = match self.sparql_store.get_all_links() {
            Ok(links) => links
                .into_iter()
                .filter(|l| l.status == Some(LinkStatus::Shared))
                .map(|l| (LinkExpression::from(l), LinkStatus::Shared))
                .collect(),
            Err(e) => {
                log::error!(
                    "share_links_after_publish: could not read the links of perspective {}: {}",
                    self.uuid,
                    e
                );
                return false;
            }
        };
        let db = crate::db_backend::db_backend();
        let unsent = match db.get_unsent_additions(&self.uuid) {
            Ok(unsent) => unsent,
            Err(e) => {
                log::error!(
                    "share_links_after_publish: could not read the queued diffs of perspective {}: {}",
                    self.uuid, e
                );
                return false;
            }
        };
        let additions = links_missing_remotely(&shared, &[], &unsent);
        if additions.is_empty() {
            return true;
        }
        let count = additions.len();
        match db.add_pending_diff(
            &self.uuid,
            &PerspectiveDiff {
                additions,
                removals: vec![],
            },
        ) {
            Ok(()) => {
                log::warn!(
                    "share_links_after_publish: the push to the new neighbourhood failed; queued {} shared links of perspective {} for the pending-diffs loop",
                    count, self.uuid
                );
                true
            }
            Err(e) => {
                log::error!(
                    "share_links_after_publish: could not queue {} shared links of perspective {}: {}",
                    count, self.uuid, e
                );
                false
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_fallback_pass_short_of_most_of_the_shared_set_is_refused() {
        // Staging, 2026-11-10: the mirror held about 500 of 12,803 links.
        assert!(fallback_push_refused(12_303, 12_803, ShareScope::Fallback));
        // An empty mirror (staging.6, 2026-11-05).
        assert!(fallback_push_refused(74_838, 74_838, ShareScope::Fallback));
        assert!(fallback_push_refused(21, 41, ShareScope::Fallback));
    }

    #[test]
    fn a_small_catch_up_is_sent() {
        assert!(!fallback_push_refused(0, 12_803, ShareScope::Fallback));
        assert!(!fallback_push_refused(5, 12_803, ShareScope::Fallback));
        assert!(!fallback_push_refused(30, 100, ShareScope::Fallback));
        // Half the set exactly, or at most 20 links, is still a catch-up.
        assert!(!fallback_push_refused(21, 42, ShareScope::Fallback));
        assert!(!fallback_push_refused(20, 20, ShareScope::Fallback));
    }

    #[test]
    fn the_push_after_publish_sends_the_whole_set() {
        assert!(!fallback_push_refused(12_803, 12_803, ShareScope::Publish));
    }
}

//! How many of a perspective's shared links one pass of
//! `share_links_missing_remotely` may send. The fallback sync catches up the
//! few links a commit missed; when it finds most of the shared set missing,
//! the link language's view of the remote is wrong (an empty or partial
//! mirror), and sending would push the room's whole set again.

use super::perspective_instance::PerspectiveInstance;

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
    /// The background fallback sync: commits the shared links the link
    /// language's `render()` lacks, unless that is most of them
    /// (`fallback_push_refused`). False when it sent nothing it should have,
    /// so the caller's backoff retries.
    pub async fn ensure_public_links_are_shared(&self) -> bool {
        self.share_links_missing_remotely(ShareScope::Fallback)
            .await
    }

    /// The creator's first push after publishing a perspective as a
    /// neighbourhood: every shared link the new room lacks, however many.
    pub async fn share_links_after_publish(&self) -> bool {
        self.share_links_missing_remotely(ShareScope::Publish).await
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

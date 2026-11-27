//! How many of a perspective's shared links one pass of
//! `share_links_missing_remotely` may send. The fallback sync catches up the
//! few links a commit missed; when it finds most of the shared set missing,
//! the link language's view of the remote is wrong (an empty or partial
//! mirror), and sending would push the room's whole set again.
//!
//! A link language with `perspectiveSyncMissing` says which links it lacks
//! (`ask_missing`); one without it renders the whole room
//! (`rendered_remote_links`).

use super::perspective_instance::{links_missing_remotely, PerspectiveInstance};
use super::sparql_store::EvalStop;
use crate::db::link_identity;
use crate::languages::capability::Capability;
use crate::languages::language::Language;
use crate::types::{LinkExpression, LinkStatus, PerspectiveDiff};
use std::collections::HashSet;

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

/// How many links one `perspectiveSyncMissing` call asks about. The shared
/// set goes to the language in calls of at most this many links, so no call
/// carries the whole set, and each answer is at most this long.
pub(crate) const MISSING_BATCH: usize = 500;

/// Whether a pass that finds `missing` of the perspective's `shared` links
/// absent remotely must send nothing: a fallback pass that would send more
/// than `FALLBACK_ALWAYS_SENDS` links and more than half the shared set.
pub(crate) fn fallback_push_refused(missing: usize, shared: usize, scope: ShareScope) -> bool {
    scope == ShareScope::Fallback && missing > FALLBACK_ALWAYS_SENDS && missing * 2 > shared
}

/// The links of `links` the link language lacks, asked in calls of
/// `MISSING_BATCH`. None when an answer is unknown (null) or fails: the links
/// after it are not asked about.
async fn ask_missing(
    link_language: &mut Language,
    links: &[LinkExpression],
    uuid: &str,
) -> Option<Vec<LinkExpression>> {
    let mut missing = Vec::new();
    for batch in links.chunks(MISSING_BATCH) {
        match link_language.missing(batch).await {
            Ok(Some(lacking)) => missing.extend(lacking),
            Ok(None) => {
                log::warn!(
                    "fallback sync: the link language cannot say yet which links it lacks for perspective {} - skipping this sync attempt",
                    uuid
                );
                return None;
            }
            Err(e) => {
                log::warn!(
                    "fallback sync: missing() failed for perspective {}: {:?} - skipping this sync attempt",
                    uuid, e
                );
                return None;
            }
        }
    }
    Some(missing)
}

/// The links the link language holds, from its whole `render()`. None when the
/// remote state is unknown.
///
/// `current_revision()` returns `Ok(None)` both when the link language
/// genuinely has no committed revision yet AND when it lacks the
/// `PerspectiveCurrentRevision` capability at all (see
/// `Language::current_revision`) — the two cases are indistinguishable from
/// the return value alone. Only trust `Ok(None)` as "nothing committed yet"
/// when the capability is actually present; otherwise go straight to
/// `render()`, which has its own capability check and safely reports
/// "unavailable" as `Ok(None)` too.
///
/// Any other non-success outcome (a transport/timeout error from either call,
/// or an unexpected `Ok(None)` from `render()` despite a revision existing)
/// means we don't actually know the remote state. Treating that the same as
/// "remote is empty" makes every local shared link look "missing", so a single
/// transient timeout resubmits the perspective's *entire* shared link set
/// (SDNA included) as a fresh batch of creates. Return None and let the
/// caller's backoff loop retry once the link language is reachable again.
async fn rendered_remote_links(
    link_language: &mut Language,
    uuid: &str,
) -> Option<Vec<LinkExpression>> {
    if link_language.has(Capability::PerspectiveCurrentRevision) {
        match link_language.current_revision().await {
            Ok(None) => return Some(vec![]),
            Ok(Some(_)) => {}
            Err(e) => {
                log::warn!(
                    "ensure_public_links_are_shared: current_revision() failed for perspective {}: {:?} — skipping this sync attempt",
                    uuid, e
                );
                return None;
            }
        }
    }
    match link_language.render().await {
        Ok(Some(rendered)) => Some(rendered.links),
        Ok(None) => {
            log::warn!(
                "ensure_public_links_are_shared: render() returned no perspective for {} — skipping this sync attempt rather than risk resubmitting the whole shared link set",
                uuid
            );
            None
        }
        Err(e) => {
            log::warn!(
                "ensure_public_links_are_shared: render() failed for perspective {}: {:?} — skipping this sync attempt",
                uuid, e
            );
            None
        }
    }
}

impl PerspectiveInstance {
    /// The background fallback sync: commits the shared links the link
    /// language lacks, unless that is most of them
    /// (`fallback_push_refused`). False when it sent nothing it should have,
    /// so the caller's backoff retries.
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

    /// Commits the Shared links the link language lacks. Callers:
    /// `ensure_public_links_are_shared` and `share_links_after_publish`;
    /// `scope` says how many it may send. A language with
    /// `perspectiveSyncMissing` is asked which links it lacks, in batches
    /// (`ask_missing`), and asked again before the commit; one without it is
    /// compared with its whole `render()` (`rendered_remote_links`).
    pub(crate) async fn share_links_missing_remotely(&self, scope: ShareScope) -> bool {
        let _sharing = self.share_links_mutex.lock().await;
        let uuid = self.uuid.clone();

        // Clone link_language without holding the lock
        let Some(mut link_language) = self.link_language.read().await.clone() else {
            return false;
        };
        let Some(local_links) = self.shared_links("ensure_public_links_are_shared") else {
            return false;
        };

        // A queued or parked diff's links stay Shared in the store, but the queue owns
        // them: a queued one would be sent twice, a parked one after its successors.
        let unsent = match crate::db_backend::db_backend().get_unsent_additions(&uuid) {
            Ok(unsent) => unsent,
            Err(e) => {
                log::warn!(
                    "ensure_public_links_are_shared: could not read queued and parked diffs for perspective {}: {} — skipping this sync attempt",
                    uuid, e
                );
                return false;
            }
        };

        let asks_missing = link_language.has(Capability::PerspectiveMissing);
        let missing = if asks_missing {
            let candidates = links_missing_remotely(&local_links, &[], &unsent);
            ask_missing(&mut link_language, &candidates, &uuid).await
        } else {
            rendered_remote_links(&mut link_language, &uuid)
                .await
                .map(|remote| links_missing_remotely(&local_links, &remote, &unsent))
        };
        let Some(mut links_to_commit) = missing else {
            return false;
        };
        if links_to_commit.is_empty() {
            return true;
        }

        // A fallback sync catches up the few links a commit missed. Most of
        // the shared set missing means the language's view of the remote is
        // wrong (an empty or partial mirror): sending would push the room's
        // whole set again. Send nothing; the caller's backoff retries.
        if fallback_push_refused(links_to_commit.len(), local_links.len(), scope) {
            log::error!(
                "ensure_public_links_are_shared: refusing to commit {} of {} local shared links for perspective {}: the link language lacks most of them, so its view of the remote is incomplete, not the remote",
                links_to_commit.len(), local_links.len(), uuid
            );
            return false;
        }

        // The scan took many calls: ask again about the links it found, so a
        // link the room got meanwhile is not sent twice. An immediate commit
        // (outside `share_links_mutex`) that lands after this ask still makes
        // a duplicate. The link server keeps one row and one revision for it
        // (`applyDiffAndAppend`, `INSERT OR IGNORE`), but appends and
        // broadcasts a second diff, which every member drops.
        if asks_missing {
            let Some(still_missing) =
                ask_missing(&mut link_language, &links_to_commit, &uuid).await
            else {
                return false;
            };
            links_to_commit = still_missing;
        }

        // A link removed while the language was asked is no longer Shared here.
        // The link server keeps no tombstone, so sending it would bring it back
        // for every member. The commit await below still leaves a short window.
        let Some(shared_now) = self.shared_links("ensure_public_links_are_shared") else {
            return false;
        };
        let shared_now: HashSet<_> = shared_now.iter().map(|(l, _)| link_identity(l)).collect();
        links_to_commit.retain(|l| shared_now.contains(&link_identity(l)));
        if links_to_commit.is_empty() {
            return true;
        }

        let links_count = links_to_commit.len();
        if let Err(e) = link_language
            .commit(PerspectiveDiff {
                additions: links_to_commit,
                removals: vec![],
            })
            .await
        {
            log::error!(
                "Error calling link language's commit in ensure_public_links_are_shared: {:?}",
                e
            );
            return false;
        }
        log::debug!(
            "Successfully committed {} links to link language in fallback sync",
            links_count
        );
        true
    }

    /// The perspective's Shared links, read from the SPARQL store. None, after
    /// logging for `caller`, when the store cannot be read.
    fn shared_links(&self, caller: &str) -> Option<Vec<(LinkExpression, LinkStatus)>> {
        match self.sparql_store.get_all_links(&EvalStop::none()) {
            Ok(links) => Some(
                links
                    .into_iter()
                    .filter(|l| l.status == Some(LinkStatus::Shared))
                    .map(|l| (LinkExpression::from(l), LinkStatus::Shared))
                    .collect(),
            ),
            Err(e) => {
                log::error!(
                    "{}: could not read the links of perspective {}: {}",
                    caller,
                    self.uuid,
                    e
                );
                None
            }
        }
    }

    /// Queues, as one pending diff, every Shared link that no queued or parked
    /// diff holds. A link the room already has is sent again; the link
    /// language drops the duplicate. False when the queue could not be written.
    fn queue_unsent_shared_links(&self) -> bool {
        let Some(shared) = self.shared_links("share_links_after_publish") else {
            return false;
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

//! Executor-side cache of the agent profiles `LanguageController::get_expression` reads from
//! the agent language.
//!
//! The seeded agent language answers `expressionGet(did)` with one HTTPS round trip to the
//! identity store, and language calls run one at a time, so `agent.byDIDs` over N authors
//! costs N round trips on every load; an author who never published costs one each time,
//! because a 404 is no expression and nothing kept it. This cache keeps both answers.
//!
//! The key is (language address, DID): an agent language that changes never serves the old
//! one's entries. The cache holds no errors (a caller that failed asks again) and the clock
//! is the caller's: every method takes `now`, so a test moves time without sleeping.

use lru::LruCache;
use serde_json::Value;
use std::num::NonZeroUsize;
use tokio::time::{Duration, Instant};

/// How long a profile the agent language returned is served from the cache. The SDK's own
/// profile cache lives as long (`AgentClient.REMOTE_AGENT_TTL_MS`) and accepts that a profile
/// changed on another executor shows after it expires, so five minutes adds no staleness the
/// client did not already accept. This executor's own writes do not wait for it: see
/// [`AgentProfileCache::invalidate`].
pub(crate) const FOUND_TTL: Duration = Duration::from_secs(5 * 60);

/// How long "no such profile" is served. Shorter than [`FOUND_TTL`]: an agent that publishes
/// its first profile on another executor should show up soon, while a feed full of authors
/// that never published must still stop asking the store for each of them on every load.
pub(crate) const NOT_FOUND_TTL: Duration = Duration::from_secs(60);

/// Entries kept before the least recently used goes. A profile is a few hundred bytes to a few
/// kilobytes, so the cap bounds the cache at some tens of megabytes while holding every
/// author a large community shows, and a flood of distinct DIDs can only churn it.
pub(crate) const CAPACITY: usize = 10_000;

struct Entry {
    /// The expression the agent language returned; `None`: it had none.
    profile: Option<Value>,
    fetched_at: Instant,
}

pub(crate) struct AgentProfileCache {
    entries: LruCache<(String, String), Entry>,
}

impl AgentProfileCache {
    pub(crate) fn new() -> Self {
        Self::with_capacity(CAPACITY)
    }

    fn with_capacity(capacity: usize) -> Self {
        Self {
            entries: LruCache::new(NonZeroUsize::new(capacity.max(1)).expect("capacity is >= 1")),
        }
    }

    /// The cached answer for `did` in the agent language `language` (`Some(None)`: cached as
    /// not found), or `None` when there is none or it has expired at `now`. An expired entry
    /// is dropped.
    pub(crate) fn get(&mut self, language: &str, did: &str, now: Instant) -> Option<Option<Value>> {
        let key = (language.to_string(), did.to_string());
        let entry = self.entries.get(&key)?;
        let ttl = if entry.profile.is_some() {
            FOUND_TTL
        } else {
            NOT_FOUND_TTL
        };
        if now.saturating_duration_since(entry.fetched_at) < ttl {
            return Some(entry.profile.clone());
        }
        self.entries.pop(&key);
        None
    }

    /// Keeps the answer the agent language gave for `did`, fetched at `now`.
    pub(crate) fn put(&mut self, language: &str, did: &str, profile: Option<Value>, now: Instant) {
        self.entries.put(
            (language.to_string(), did.to_string()),
            Entry {
                profile,
                fetched_at: now,
            },
        );
    }

    /// Forgets `did` in the agent language `language`: this executor wrote its profile, so the
    /// next read must see the write.
    pub(crate) fn invalidate(&mut self, language: &str, did: &str) {
        self.entries.pop(&(language.to_string(), did.to_string()));
    }

    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    const LANG: &str = "QmAgentLang";

    #[test]
    fn a_found_profile_is_served_until_five_minutes_have_passed() {
        let t0 = Instant::now();
        let mut cache = AgentProfileCache::new();
        cache.put(LANG, "did:a", Some(json!({"data": 1})), t0);
        assert_eq!(
            cache.get(LANG, "did:a", t0 + FOUND_TTL - Duration::from_millis(1)),
            Some(Some(json!({"data": 1})))
        );
        assert_eq!(cache.get(LANG, "did:a", t0 + FOUND_TTL), None);
        assert_eq!(cache.len(), 0, "an expired entry is dropped");
    }

    #[test]
    fn a_not_found_answer_is_served_for_a_minute_only() {
        let t0 = Instant::now();
        let mut cache = AgentProfileCache::new();
        cache.put(LANG, "did:a", None, t0);
        assert_eq!(
            cache.get(LANG, "did:a", t0 + NOT_FOUND_TTL - Duration::from_millis(1)),
            Some(None)
        );
        assert_eq!(cache.get(LANG, "did:a", t0 + NOT_FOUND_TTL), None);
    }

    #[test]
    fn the_key_holds_the_language_so_another_agent_language_misses() {
        let t0 = Instant::now();
        let mut cache = AgentProfileCache::new();
        cache.put(LANG, "did:a", Some(json!(1)), t0);
        assert_eq!(cache.get("QmOtherLang", "did:a", t0), None);
        assert_eq!(cache.get(LANG, "did:b", t0), None);
    }

    #[test]
    fn invalidate_drops_one_did_and_keeps_the_others() {
        let t0 = Instant::now();
        let mut cache = AgentProfileCache::new();
        cache.put(LANG, "did:a", Some(json!(1)), t0);
        cache.put(LANG, "did:b", Some(json!(2)), t0);
        cache.invalidate(LANG, "did:a");
        assert_eq!(cache.get(LANG, "did:a", t0), None);
        assert_eq!(cache.get(LANG, "did:b", t0), Some(Some(json!(2))));
    }

    #[test]
    fn the_least_recently_used_entry_goes_at_the_cap() {
        let t0 = Instant::now();
        let mut cache = AgentProfileCache::with_capacity(2);
        cache.put(LANG, "did:a", Some(json!(1)), t0);
        cache.put(LANG, "did:b", Some(json!(2)), t0);
        assert!(cache.get(LANG, "did:a", t0).is_some());
        cache.put(LANG, "did:c", Some(json!(3)), t0);
        assert_eq!(cache.get(LANG, "did:b", t0), None);
        assert!(cache.get(LANG, "did:a", t0).is_some());
        assert!(cache.get(LANG, "did:c", t0).is_some());
    }
}

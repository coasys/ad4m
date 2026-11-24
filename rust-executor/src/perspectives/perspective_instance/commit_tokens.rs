//! Pacing a perspective's commits to its link language. Every
//! `link_language.commit` of a perspective (the immediate send, the pending
//! batch and its refusal-path sends, the fallback sync) takes a token from one
//! bucket, so steady writes become batches instead of one request per diff.

use super::PerspectiveInstance;
use std::time::Duration;
use tokio::time::Instant;

/// Commits a perspective may send to its link language at once. The server link
/// language (the default neighbourhood language) lets a room token POST 60
/// commits a minute, and setup writes on a fresh neighbourhood must go out
/// without waiting (see `auto_processor/config.rs`).
pub(super) const COMMIT_BURST: usize = 20;
/// One commit token comes back this often: 30 a minute. In any 60 s window a
/// perspective sends at most `COMMIT_BURST` + 30 = 50 commits, under the
/// server's 60 a minute per room token.
pub(super) const COMMIT_REFILL_INTERVAL: Duration = Duration::from_secs(2);

/// The token bucket every `link_language.commit` of a perspective takes from.
pub(super) struct CommitTokens {
    tokens: usize,
    /// When the next token is counted from; reset when the bucket is full.
    refilled_at: Instant,
}

impl CommitTokens {
    pub(super) fn new(tokens: usize) -> Self {
        Self {
            tokens,
            refilled_at: Instant::now(),
        }
    }

    /// Take a token if there is one, after counting those refilled since the
    /// last call.
    fn take(&mut self) -> bool {
        let now = Instant::now();
        let refilled = (now - self.refilled_at).as_nanos() / COMMIT_REFILL_INTERVAL.as_nanos();
        self.tokens = (self.tokens + refilled as usize).min(COMMIT_BURST);
        if self.tokens == COMMIT_BURST {
            self.refilled_at = now;
        } else {
            self.refilled_at += COMMIT_REFILL_INTERVAL * refilled as u32;
        }
        if self.tokens > 0 {
            self.tokens -= 1;
            true
        } else {
            false
        }
    }
}

impl PerspectiveInstance {
    /// Take one token of the commit bucket; false when it is empty.
    pub(super) async fn take_commit_token(&self) -> bool {
        self.commit_tokens.lock().await.take()
    }

    /// Set the tokens left in the commit bucket (tests drive the pacing with it).
    pub async fn set_commit_tokens(&self, count: usize) {
        *self.commit_tokens.lock().await = CommitTokens::new(count);
    }
}

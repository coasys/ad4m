//! What the log of a person says about one of their bots' keys: whether it is listed with the
//! bot mark, valid, and the scope (actions, expiry) the log gives it. `crate::bots` reads it on
//! every call a bot makes within its grant, so the log is the ceiling of what a grant allows.

use ad4m_identity_core::BotScope;

use super::resolver::SessionKeyError;
use super::service::IdentityService;
use super::verdict::KeyVerdict;

/// How old a log may be when a bot's call reads it: the key sessions' re-check interval.
pub const BOT_KEY_MAX_AGE: u64 = super::key_login::KEY_RECHECK_INTERVAL.as_secs();

/// A bot's key in its owner's log, at the log's trusted seq.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BotKey {
    /// The key's id in the log (`d3`).
    pub key_id: String,
    pub verdict: KeyVerdict,
    /// The bot mark's scope; `None` for a key without the bot mark.
    pub scope: Option<BotScope>,
}

/// Why a bot's key could not be read.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BotKeyError {
    /// The log lists no such device key at its trusted seq: the holder has not confirmed the
    /// bot's claim yet, or the log is not here.
    NotListed,
    /// The store did not answer and the cached log is too old.
    StoreUnavailable,
}

impl IdentityService {
    /// The key `key` (a multikey) in the log of `scid`, read from the store first when the
    /// cached log is older than [`BOT_KEY_MAX_AGE`].
    pub async fn bot_key(&self, scid: &str, key: &str) -> Result<BotKey, BotKeyError> {
        let found = self
            .resolver
            .session_key(scid, key, BOT_KEY_MAX_AGE)
            .await
            .map_err(|e| match e {
                SessionKeyError::NotListed => BotKeyError::NotListed,
                SessionKeyError::StoreUnavailable => BotKeyError::StoreUnavailable,
            })?;
        let scope = match self.cache.trusted(&found.scid).await {
            Ok(Some((chain, seq))) => chain.state().bot_scope_at(&found.key_id, seq).cloned(),
            Ok(None) => return Err(BotKeyError::NotListed),
            Err(e) => {
                log::warn!("identity: reading the log of {} for a bot: {e}", found.scid);
                return Err(BotKeyError::StoreUnavailable);
            }
        };
        Ok(BotKey {
            key_id: found.key_id,
            verdict: found.verdict,
            scope,
        })
    }
}

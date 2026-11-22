//! The signing fence: device keys (`did:key`) that may not sign while their unlink is pending.
//!
//! `identity.unlinkSelf` records an intent (`db/identity.rs`, `identity_unlinks`) and fences the
//! key before the `unlink` reaches the store, so no link signed with the key after that point
//! can carry an anchor the unlink will not cover. The fence lives in memory and is filled from
//! the intents at startup ([`load`]); the intent is the durable part. Every signing path of
//! the executor goes through [`super::sign_for_context`], which refuses a fenced key. The one
//! exception is the `unlink` event itself ([`super::sign_for_context_unfenced`]).

use std::collections::HashSet;
use std::sync::RwLock;

static FENCED: once_cell::sync::Lazy<RwLock<HashSet<String>>> =
    once_cell::sync::Lazy::new(|| RwLock::new(HashSet::new()));

/// Fences the key `did`: nothing signs with it until [`lift`].
pub fn raise(did: &str) {
    FENCED
        .write()
        .unwrap_or_else(|e| e.into_inner())
        .insert(did.to_string());
}

/// Lifts the fence of the key `did`.
pub fn lift(did: &str) {
    FENCED
        .write()
        .unwrap_or_else(|e| e.into_inner())
        .remove(did);
}

/// Whether the key `did` is fenced.
pub fn is_raised(did: &str) -> bool {
    FENCED
        .read()
        .unwrap_or_else(|e| e.into_inner())
        .contains(did)
}

/// Whether any key is fenced (the common case is none, and then nothing looks up a DID).
pub fn any() -> bool {
    !FENCED.read().unwrap_or_else(|e| e.into_inner()).is_empty()
}

/// Fences every key in `dids` (the pending unlinks the database holds).
pub fn load(dids: impl IntoIterator<Item = String>) {
    let mut fenced = FENCED.write().unwrap_or_else(|e| e.into_inner());
    fenced.extend(dids);
}

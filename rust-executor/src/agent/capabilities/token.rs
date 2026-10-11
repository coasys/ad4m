use super::types::*;
use crate::config::get_global_config;
use crate::wallet::wallet_backend;
use deno_core::{anyhow::anyhow, error::AnyError};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};

/// Resolve the signing key name from global config.
pub fn signing_key_name() -> String {
    get_global_config().signing_key_name()
}

pub fn generate_jwt(
    audience: String,
    expiration_time: u64,
    capabilities: AuthInfo,
) -> Result<String, AnyError> {
    let backend = wallet_backend();
    let name = signing_key_name();

    if !backend.is_unlocked() {
        return Err(anyhow!(
            "Executor is locked: the wallet has not been unlocked since the last restart. \
             Ask the executor operator to call unlockAgent, then retry."
        ));
    }

    let secret_key = backend.get_secret_key(&name).ok_or(anyhow!(
        "{} key not found. call createMainKey() first",
        name
    ))?;

    let did_document = backend.get_did_document(&name).ok_or(anyhow!(
        "{} did not found. call createMainKey() first",
        name
    ))?;

    let payload = Claims::new(did_document.id, audience, expiration_time, capabilities);

    let token = encode(
        &Header::default(),
        &payload,
        &EncodingKey::from_secret(secret_key.as_slice()),
    )?;

    Ok(token)
}

pub fn decode_jwt(token: String) -> Result<Claims, AnyError> {
    let backend = wallet_backend();
    let name = signing_key_name();

    // Same guard as generate_jwt: every authenticated request verifies its JWT
    // here, so a locked wallet must surface as "locked", not as the internal
    // "key not found" — the caller (e.g. a remote multi-user agent) can do
    // nothing about the key, but can ask the operator to unlock.
    if !backend.is_unlocked() {
        return Err(anyhow!(
            "Executor is locked: the wallet has not been unlocked since the last restart. \
             Ask the executor operator to call unlockAgent, then retry."
        ));
    }

    let secret_key = backend
        .get_secret_key(&name)
        .ok_or(anyhow!("{} key not found", name))?;

    let result = jsonwebtoken::decode::<Claims>(
        &token,
        &DecodingKey::from_secret(secret_key.as_slice()),
        &jsonwebtoken::Validation::new(Algorithm::HS256),
    )?;

    Ok(result.claims)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{set_global_config, Ad4mConfig};
    use crate::wallet::{try_init_wallet_backend, LocalWallet, WalletBackend};
    use std::sync::Arc;

    /// decode_jwt is the verification step of every authenticated request, so a
    /// locked wallet must surface as the operator-facing "Executor is locked"
    /// message generate_jwt already uses — not as the internal "key not found".
    /// A remote multi-user agent hit exactly this in the 2026-09-15 wake test:
    /// after a node restart, the waker's subscribe 403'd with
    /// "main key not found. call createMainKey() first", which reads like a
    /// broken installation instead of "ask the node's operator to unlock".
    #[test]
    fn decode_jwt_on_locked_wallet_names_the_locked_state() {
        set_global_config(Ad4mConfig::default());
        let _ = try_init_wallet_backend(Arc::new(LocalWallet::new()) as Arc<dyn WalletBackend>);
        let backend = wallet_backend();

        // The wallet backend is a process-wide singleton shared across the test
        // binary (tests run with --test-threads=1), so force the locked state
        // instead of assuming it: lock() encrypts and clears in-memory keys.
        let was_unlocked = backend.is_unlocked();
        if was_unlocked {
            backend.lock("decode-jwt-locked-test");
        }

        let err = decode_jwt("some.jwt.token".to_string());

        // Restore before asserting so a failing assertion cannot leave the
        // shared wallet locked for later tests.
        if was_unlocked {
            backend
                .unlock("decode-jwt-locked-test")
                .expect("restore shared wallet for later tests");
        }

        let msg = err
            .expect_err("decode_jwt must fail on a locked wallet")
            .to_string();
        assert!(msg.contains("Executor is locked"), "got: {msg}");
        assert!(
            !msg.contains("key not found"),
            "must not leak the internal key-lookup error; got: {msg}"
        );
    }
}

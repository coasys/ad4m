use super::types::*;
use crate::config::get_global_config;
use crate::wallet::wallet_backend;
use deno_core::{anyhow::anyhow, error::AnyError};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};

/// Resolve the signing key name from global config.
pub fn signing_key_name() -> String {
    get_global_config().signing_key_name()
}

/// Signs a token for `audience` that expires `expiration_time` seconds from now and names the
/// managed agent `sub` (`None`: the main agent). The token carries no capabilities: a token
/// works only while its grant does, and the grant holds them. Only `agent::grants::mint`
/// calls this, so every token has a grant.
pub(crate) fn sign_token(
    audience: String,
    expiration_time: u64,
    sub: Option<String>,
) -> Result<(String, Claims), AnyError> {
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

    let claims = Claims::new(did_document.id, audience, expiration_time, sub);

    let token = encode(
        &Header::default(),
        &claims,
        &EncodingKey::from_secret(secret_key.as_slice()),
    )?;

    Ok((token, claims))
}

pub fn decode_jwt(token: String) -> Result<Claims, AnyError> {
    let backend = wallet_backend();
    let name = signing_key_name();

    // Same guard as sign_token: every authenticated request verifies its JWT
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

/// The claims of a token, read without the wallet and without the signature check.
///
/// Never use it to decide who a caller is. It serves revocation and the migration of the old
/// app registry: a token names its grant by id, and the operator may revoke it while the
/// wallet is locked. Revoking needs the grant, which only a token this executor issued has.
/// `None` when the token does not parse.
pub fn read_stored_claims(token: &str) -> Option<Claims> {
    let mut validation = jsonwebtoken::Validation::new(Algorithm::HS256);
    validation.insecure_disable_signature_validation();
    validation.validate_exp = false;
    validation.required_spec_claims.clear();
    jsonwebtoken::decode::<Claims>(token, &DecodingKey::from_secret(&[]), &validation)
        .ok()
        .map(|data| data.claims)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{set_global_config, Ad4mConfig};
    use crate::wallet::{try_init_wallet_backend, LocalWallet, WalletBackend};
    use std::sync::Arc;

    // Revocation reads a token's id without the wallet, so the operator can revoke while it
    // is locked.
    #[test]
    fn a_tokens_id_reads_without_the_wallet() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let (token, signed) = sign_token("id test".to_string(), 3600, None).unwrap();
        let claims = decode_jwt(token.clone()).unwrap();
        assert_eq!(claims.nonce(), signed.nonce());

        let backend = wallet_backend();
        backend.lock("stored-token-id-test");
        let read = read_stored_claims(&token);
        backend
            .unlock("stored-token-id-test")
            .expect("restore the shared wallet");

        let read = read.expect("the token parses");
        assert_eq!(read.nonce(), claims.nonce());
        assert_eq!(read.expires_at(), claims.expires_at());
        assert!(read_stored_claims("not-a-jwt").is_none());
    }

    /// decode_jwt is the verification step of every authenticated request, so a
    /// locked wallet must surface as the operator-facing "Executor is locked"
    /// message sign_token already uses — not as the internal "key not found".
    /// A remote multi-user agent hit exactly this in the 2026-09-15 wake test:
    /// after an executor restart, the waker's subscribe 403'd with
    /// "main key not found. call createMainKey() first", which reads like a
    /// broken installation instead of "ask the executor's operator to unlock".
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

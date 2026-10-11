use super::types::*;
use crate::agent::session::TokenId;
use crate::config::get_global_config;
use crate::secret::Secret;
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
    generate_jwt_issued_from(audience, expiration_time, capabilities, 0)
}

/// A token as issued: the JWT for its one receiver, and the id and expiry the executor keeps.
pub struct IssuedToken {
    pub jwt: Secret,
    pub id: TokenId,
    pub expires_at: u64,
}

/// [`generate_jwt`], with the new token's id and expiry beside it.
pub fn issue_jwt(
    audience: String,
    expiration_time: u64,
    capabilities: AuthInfo,
) -> Result<IssuedToken, AnyError> {
    issue_jwt_from(audience, expiration_time, capabilities, 0)
}

/// [`generate_jwt`] with an issue time (`iat`) of `earliest` at the soonest (Unix seconds).
/// A user's tokens are issued after the user's session cutoff this way, even within the
/// second of a `user.revokeAllSessions`.
pub fn generate_jwt_issued_from(
    audience: String,
    expiration_time: u64,
    capabilities: AuthInfo,
    earliest: u64,
) -> Result<String, AnyError> {
    issue_jwt_from(audience, expiration_time, capabilities, earliest).map(|t| t.jwt.expose_once())
}

fn issue_jwt_from(
    audience: String,
    expiration_time: u64,
    capabilities: AuthInfo,
    earliest: u64,
) -> Result<IssuedToken, AnyError> {
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

    let payload = Claims::new(did_document.id, audience, expiration_time, capabilities)
        .issued_no_earlier_than(earliest);

    let token = encode(
        &Header::default(),
        &payload,
        &EncodingKey::from_secret(secret_key.as_slice()),
    )?;

    Ok(IssuedToken {
        jwt: Secret::new(token),
        id: TokenId(payload.nonce().to_string()),
        expires_at: payload.expires_at(),
    })
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

/// The id and expiry of a token, read without the wallet.
///
/// Skips the signature check, so never use it to decide who a caller is. It serves
/// revocation, which the operator may do while the wallet is locked: revoking an app by its
/// JWT, and migrating an app registry file that still holds raw tokens. A forged token
/// names only an id; revoking that id refuses no genuine token that the id does not belong
/// to. `None` when the token does not parse: such a token opens no session either.
pub fn read_stored_token_id(token: &str) -> Option<(crate::agent::session::TokenId, u64)> {
    let mut validation = jsonwebtoken::Validation::new(Algorithm::HS256);
    validation.insecure_disable_signature_validation();
    validation.validate_exp = false;
    validation.required_spec_claims.clear();
    let claims = jsonwebtoken::decode::<Claims>(token, &DecodingKey::from_secret(&[]), &validation)
        .ok()?
        .claims;
    Some((
        crate::agent::session::TokenId(claims.nonce().to_string()),
        claims.expires_at(),
    ))
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
    /// after an executor restart, the waker's subscribe 403'd with
    /// "main key not found. call createMainKey() first", which reads like a
    /// broken installation instead of "ask the executor's operator to unlock".
    // Revocation reads a stored token's id without the wallet, so the operator can revoke
    // while it is locked.
    #[test]
    fn a_stored_token_id_reads_without_the_wallet() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let token = generate_jwt("id test".to_string(), 3600, AuthInfo::default()).unwrap();
        let claims = decode_jwt(token.clone()).unwrap();

        let backend = wallet_backend();
        backend.lock("stored-token-id-test");
        let read = read_stored_token_id(&token);
        backend
            .unlock("stored-token-id-test")
            .expect("restore the shared wallet");

        let (id, expires_at) = read.expect("the token parses");
        assert_eq!(id.0, claims.nonce());
        assert_eq!(expires_at, claims.expires_at());
        assert!(read_stored_token_id("not-a-jwt").is_none());
    }

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

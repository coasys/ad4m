use super::types::*;
use crate::wallet::Wallet;
use deno_core::{anyhow::anyhow, error::AnyError};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};

pub fn generate_jwt(
    audience: String,
    expiration_time: u64,
    capabilities: AuthInfo,
) -> Result<String, AnyError> {
    let wallet = Wallet::instance();
    let wallet_lock = wallet.lock().expect("wallet lock");
    let wallet_ref = wallet_lock.as_ref().expect("wallet instance");

    if !wallet_ref.is_unlocked() {
        return Err(anyhow!(
            "Wallet is locked. The agent must be unlocked (agentUnlock) before generating JWTs."
        ));
    }

    let name = "main".to_string();

    let secret_key = wallet_ref.get_secret_key(&name).ok_or(anyhow!(
        "main signing key not found. Agent may not have been initialized (agentGenerate)."
    ))?;

    let did_document = wallet_ref.get_did_document(&name).ok_or(anyhow!(
        "main DID document not found. Agent may not have been initialized (agentGenerate)."
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
    let wallet = Wallet::instance();
    let wallet_lock = wallet.lock().expect("wallet lock");
    let wallet_ref = wallet_lock.as_ref().expect("wallet instance");

    if !wallet_ref.is_unlocked() {
        return Err(anyhow!(
            "Wallet is locked. The agent must be unlocked (agentUnlock) before generating JWTs."
        ));
    }

    let name = "main".to_string();

    let secret_key = wallet_ref.get_secret_key(&name).ok_or(anyhow!(
        "main signing key not found. Agent may not have been initialized (agentGenerate)."
    ))?;

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
    use crate::wallet::Wallet;

    // NOTE: These tests mutate the global Wallet singleton. They MUST run
    // with --test-threads=1 (which the executor test suite already enforces)
    // to avoid data races. If this ever changes, add #[serial] from
    // the serial_test crate.

    fn test_auth_info() -> AuthInfo {
        AuthInfo {
            app_name: "test-app".to_string(),
            app_desc: "test".to_string(),
            app_domain: None,
            app_url: None,
            app_icon_path: None,
            capabilities: None,
            user_email: None,
        }
    }

    /// Reproduces the original bug: calling generate_jwt when the wallet is locked
    /// used to return "main key not found. call createMainKey() first" which was
    /// misleading. Now it should return a clear "Wallet is locked" error.
    #[test]
    fn generate_jwt_on_locked_wallet_gives_clear_error() {
        let wallet_instance = Wallet::instance();
        {
            let mut wallet = wallet_instance.lock().unwrap();
            let w = wallet.as_mut().unwrap();
            // Generate a key, then lock — simulates post-restart state
            w.generate_keypair("main".to_string());
            w.lock("test-passphrase".to_string());
            assert!(!w.is_unlocked());
        }

        let result = generate_jwt("test-audience".to_string(), 9999999999, test_auth_info());
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("Wallet is locked"),
            "Expected 'Wallet is locked' error, got: {}",
            err_msg
        );
        // Must NOT contain the old misleading message
        assert!(
            !err_msg.contains("main key not found"),
            "Should not contain old misleading error message"
        );

        // Cleanup: unlock so other tests aren't affected
        {
            let mut wallet = wallet_instance.lock().unwrap();
            let w = wallet.as_mut().unwrap();
            w.unlock("test-passphrase".to_string()).unwrap();
        }
    }

    /// Reproduces the original bug for decode_jwt on a locked wallet.
    #[test]
    fn decode_jwt_on_locked_wallet_gives_clear_error() {
        let wallet_instance = Wallet::instance();
        {
            let mut wallet = wallet_instance.lock().unwrap();
            let w = wallet.as_mut().unwrap();
            w.generate_keypair("main".to_string());
            w.lock("test-passphrase".to_string());
            assert!(!w.is_unlocked());
        }

        let result = decode_jwt("some.fake.token".to_string());
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("Wallet is locked"),
            "Expected 'Wallet is locked' error, got: {}",
            err_msg
        );
        assert!(
            !err_msg.contains("main key not found"),
            "Should not contain old misleading error message"
        );

        // Cleanup
        {
            let mut wallet = wallet_instance.lock().unwrap();
            let w = wallet.as_mut().unwrap();
            w.unlock("test-passphrase".to_string()).unwrap();
        }
    }

    /// Verifies generate_jwt succeeds when wallet is unlocked with a "main" key.
    #[test]
    fn generate_jwt_succeeds_when_unlocked() {
        let wallet_instance = Wallet::instance();
        {
            let mut wallet = wallet_instance.lock().unwrap();
            let w = wallet.as_mut().unwrap();
            w.generate_keypair("main".to_string());
            assert!(w.is_unlocked());
        }

        let result = generate_jwt("test-audience".to_string(), 9999999999, test_auth_info());
        assert!(
            result.is_ok(),
            "generate_jwt should succeed when wallet is unlocked: {:?}",
            result.err()
        );

        let token = result.unwrap();
        assert!(!token.is_empty());

        // Verify round-trip: decode should also work
        let decoded = decode_jwt(token);
        assert!(
            decoded.is_ok(),
            "decode_jwt should succeed: {:?}",
            decoded.err()
        );
    }

    /// Verifies the error message when wallet is unlocked but "main" key is missing
    /// (agent never initialized). Should say "not found" not "locked".
    #[test]
    fn generate_jwt_without_main_key_gives_not_found_error() {
        let wallet_instance = Wallet::instance();
        {
            let mut wallet = wallet_instance.lock().unwrap();
            let w = wallet.as_mut().unwrap();
            // Reset wallet to a fresh state with only a non-"main" key.
            // Lock first to clear any existing keys from prior tests,
            // then create a fresh wallet with only "other-key".
            *w = Wallet::new();
            w.generate_keypair("other-key".to_string());
            assert!(w.is_unlocked());
            assert!(w.get_secret_key(&"main".to_string()).is_none());
        }

        let result = generate_jwt("test-audience".to_string(), 9999999999, test_auth_info());
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("not found") && err_msg.contains("agentGenerate"),
            "Expected 'not found' + 'agentGenerate' error, got: {}",
            err_msg
        );
        assert!(
            !err_msg.contains("Wallet is locked"),
            "Should not say wallet is locked when it's unlocked"
        );

        // Cleanup: restore a "main" key so other tests aren't affected
        {
            let mut wallet = wallet_instance.lock().unwrap();
            let w = wallet.as_mut().unwrap();
            w.generate_keypair("main".to_string());
        }
    }
}

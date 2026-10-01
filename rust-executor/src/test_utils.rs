use std::sync::Arc;

use crate::agent::AgentService;
use crate::config::{set_global_config, Ad4mConfig};
use crate::wallet::{try_init_wallet_backend, LocalWallet, WalletBackend};

pub fn setup_wallet() {
    let local = Arc::new(LocalWallet::new());
    local.generate_keypair("main").expect("generate main key");
    // Try to init; if already initialised (from a prior test), just ensure
    // the key exists. OnceCell prevents double-init panics.
    let _ = try_init_wallet_backend(local as Arc<dyn WalletBackend>);
}

pub fn setup_agent() {
    // create_new_keys() reads the global config for signing_key_name().
    // Ensure a default config exists so it does not panic.
    set_global_config(Ad4mConfig::default());

    AgentService::init_global_instance(String::from("test_data"));
    AgentService::global_instance()
        .lock()
        .expect("couldn't get lock on AgentService")
        .as_mut()
        .expect("Must be some because was initalized above")
        .create_new_keys();
}

/// Ensure the global wallet has a "main" keypair (decode_jwt signs and
/// verifies with it) and return a user JWT whose `sub` is `email`.
///
/// Mints against the *current* global "main" key. Safe under the project's
/// single-threaded test convention (`--test-threads=1`, see package.json);
/// a parallel test rotating the key via `test_utils::setup_wallet()`
/// between mint and decode would break verification.
pub fn user_jwt_token(email: &str) -> String {
    user_jwt_token_with(
        email,
        serde_json::json!({ "appName": "test", "appDesc": "test" }),
    )
}

/// [`user_jwt_token`] carrying `capabilities` as its `AuthInfo` claim.
pub fn user_jwt_token_with(email: &str, capabilities: serde_json::Value) -> String {
    use jsonwebtoken::{encode, EncodingKey, Header};
    // Use the trait-based wallet_backend (same path as decode_jwt) so the
    // signing and verification keys match.
    let local = std::sync::Arc::new(crate::wallet::LocalWallet::new());
    let _ = crate::wallet::try_init_wallet_backend(
        local as std::sync::Arc<dyn crate::wallet::WalletBackend>,
    );
    crate::config::set_global_config(crate::config::Ad4mConfig::default());

    let backend = crate::wallet::wallet_backend();
    let key_name = crate::agent::capabilities::token::signing_key_name();
    if !backend.key_exists(&key_name) {
        backend
            .generate_keypair(&key_name)
            .expect("generate signing key");
    }
    let secret = backend
        .get_secret_key(&key_name)
        .expect("signing key must exist");
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    encode(
        &Header::default(),
        &serde_json::json!({
            "iss": "ad4m-test",
            "sub": email,
            "aud": "ad4m-test",
            "exp": now + 3600,
            "iat": now,
            "nonce": "test-nonce",
            "capabilities": capabilities,
        }),
        &EncodingKey::from_secret(secret.as_slice()),
    )
    .unwrap()
}

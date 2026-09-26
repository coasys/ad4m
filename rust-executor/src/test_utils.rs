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

/// Turns multi-user mode on, on a fresh in-memory database, and off again when dropped,
/// even if the test panics.
pub struct MultiUserMode;

impl MultiUserMode {
    pub fn on() -> Self {
        let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
        crate::db::Ad4mDb::with_global_instance(|db| db.set_multi_user_enabled(true)).unwrap();
        MultiUserMode
    }
}

impl Drop for MultiUserMode {
    fn drop(&mut self) {
        let _ = crate::db::Ad4mDb::with_global_instance(|db| db.set_multi_user_enabled(false));
    }
}

/// A user token the node signed, with its expiry one hour in the past.
/// Needs `setup_wallet()` and `setup_agent()` first.
pub fn expired_user_token(email: &str) -> String {
    use base64::Engine;
    let valid = crate::user_management::generate_user_jwt(email, "test").unwrap();
    let payload = valid.split('.').nth(1).unwrap();
    let mut claims: serde_json::Value = serde_json::from_slice(
        &base64::engine::general_purpose::URL_SAFE_NO_PAD
            .decode(payload)
            .unwrap(),
    )
    .unwrap();
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    claims["exp"] = serde_json::json!(now - 3600);
    let secret = crate::wallet::wallet_backend()
        .get_secret_key(&crate::agent::capabilities::signing_key_name())
        .expect("the test wallet holds the signing key");
    jsonwebtoken::encode(
        &jsonwebtoken::Header::default(),
        &claims,
        &jsonwebtoken::EncodingKey::from_secret(&secret),
    )
    .unwrap()
}

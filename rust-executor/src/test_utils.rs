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

/// The private keys of the Ed25519 keypair `public`/`secret`, in the form a DID
/// document built with secrets carries them: base58 for the signing key and
/// for the derived X25519 key-agreement key. These are the values #1229 leaked;
/// a test that finds none of them in a reply proves the reply holds no secret,
/// whatever field name it might travel under.
pub fn private_key_values(public: &[u8], secret: &[u8]) -> Vec<String> {
    use did_key::{DIDCore, Ed25519KeyPair, KeyFormat};
    let document = did_key::from_existing_key::<Ed25519KeyPair>(public, Some(secret))
        .get_did_document(did_key::CONFIG_LD_PRIVATE);
    let values: Vec<String> = document
        .verification_method
        .into_iter()
        .filter_map(|method| match method.private_key {
            Some(KeyFormat::Base58(value)) => Some(value),
            _ => None,
        })
        .collect();
    assert_eq!(
        values.len(),
        2,
        "expected the signing key and the key-agreement key; without them the check is vacuous"
    );
    values
}

/// `private_key_values` for a key held by the global wallet backend.
pub fn wallet_private_key_values(key_name: &str) -> Vec<String> {
    let backend = crate::wallet::wallet_backend();
    let public = backend.get_public_key(key_name).expect("public key");
    let secret = backend.get_secret_key(key_name).expect("secret key");
    private_key_values(&public, &secret)
}

/// Fails if `serialized` — a reply, an event payload, a file — carries a
/// private key: either a `privateKey…` member or one of the `secrets` values.
/// A document can be nested as a JSON string (multi-user status does that),
/// which is why this checks the text rather than walking the JSON.
pub fn assert_no_private_keys(serialized: &str, secrets: &[String], what: &str) {
    assert!(
        !serialized.contains("privateKey"),
        "{what} carries a privateKey member: {serialized}"
    );
    for secret in secrets {
        assert!(
            !serialized.contains(secret.as_str()),
            "{what} carries a private key value"
        );
    }
}

/// Starts the V8 platform once for the whole test process, with the flags `lib.rs` uses.
/// V8 freezes its flags when the platform starts, and setting a flag after that aborts the
/// process, so every test that runs V8 calls this before anything else can start it.
pub fn init_v8_platform() {
    static V8_PLATFORM: std::sync::Once = std::sync::Once::new();
    V8_PLATFORM.call_once(|| {
        deno_core::v8::V8::set_flags_from_string("--max-opt=0");
        deno_core::JsRuntime::init_platform(None);
    });
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

/// A user `email` with a managed agent, as `user.create` leaves one: a key in the global
/// wallet named by the agent's DID, and a `users` row naming that DID. Returns the DID. Opens
/// an in-memory database if none is open. Needs `setup_wallet()` first.
pub fn create_test_user(email: &str) -> String {
    if crate::db::Ad4mDb::global_instance()
        .lock()
        .expect("db lock")
        .is_none()
    {
        crate::db::Ad4mDb::init_global_instance(":memory:").expect("in-memory db");
    }
    if let Ok(user) = crate::db::Ad4mDb::with_global_instance(|db| db.get_user(email)) {
        let key = crate::wallet::wallet_backend().get_did_document(&user.did);
        if key.is_some_and(|doc| doc.id == user.did) {
            return user.did;
        }
    }
    let did = AgentService::create_managed_agent_key(email).expect("managed agent key");
    crate::db::Ad4mDb::with_global_instance(|db| {
        // `add_user_prehashed` keeps a row whose email exists: point it at the new agent.
        db.add_user_prehashed(email, &did, "test-hash")?;
        db.set_user_did(email, &did)
    })
    .expect("users row");
    did
}

/// A user token for `email`, whose `users` row names an agent no wallet key derives: the token
/// is valid, and no session can act as its agent. Needs `setup_wallet()` and `setup_agent()`.
pub fn keyless_user_token(email: &str) -> String {
    if crate::db::Ad4mDb::global_instance()
        .lock()
        .expect("db lock")
        .is_none()
    {
        crate::db::Ad4mDb::init_global_instance(":memory:").expect("in-memory db");
    }
    let keyless_did = format!("did:key:z6MkKeyless{}", email.replace(['@', '.'], ""));
    crate::db::Ad4mDb::with_global_instance(|db| {
        db.add_user_prehashed(email, &keyless_did, "test-hash")?;
        db.set_user_did(email, &keyless_did)
    })
    .expect("users row");
    crate::user_management::generate_user_jwt(email, "test").unwrap()
}

/// A user token the node signed, with its expiry one hour in the past.
/// Needs `setup_wallet()` and `setup_agent()` first.
/// Creates the user if it does not exist.
pub fn expired_user_token(email: &str) -> String {
    create_test_user(email);
    let valid = crate::user_management::generate_user_jwt(email, "test").unwrap();
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    resigned_token(&valid, |claims| {
        claims["exp"] = serde_json::json!(now - 3600)
    })
}

/// `token`, a JWT the executor signed, with its claims changed by `edit` and signed again
/// with the executor's key: a token of a shape the executor no longer mints, such as one
/// from before agents were keyed by DID. Needs `setup_wallet()` and `setup_agent()` first.
pub fn resigned_token(token: &str, edit: impl FnOnce(&mut serde_json::Value)) -> String {
    use base64::Engine;
    let payload = token.split('.').nth(1).unwrap();
    let mut claims: serde_json::Value = serde_json::from_slice(
        &base64::engine::general_purpose::URL_SAFE_NO_PAD
            .decode(payload)
            .unwrap(),
    )
    .unwrap();
    edit(&mut claims);
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

/// Points the app registry at one temp file for the whole test process. Tests that insert
/// apps call this and use their own unique keys: a temp dir per test would race when tests
/// run in parallel, because the registry's file path is global.
pub fn use_test_apps_file() {
    static APPS_DIR: std::sync::OnceLock<tempfile::TempDir> = std::sync::OnceLock::new();
    let dir =
        APPS_DIR.get_or_init(|| tempfile::tempdir().expect("a temp dir for the app registry"));
    crate::agent::capabilities::apps_map::set_data_file_path(
        dir.path().join("apps.json").to_string_lossy().into(),
    );
}

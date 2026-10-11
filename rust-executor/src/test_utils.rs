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

/// A new managed user with a unique email: a wallet key and a row in the open database.
/// Needs `setup_wallet()`, `setup_agent()` and an open database first.
pub fn managed_user(name: &str) -> String {
    let email = format!("{name}-{}@example.org", uuid::Uuid::new_v4().simple());
    crate::agent::AgentService::ensure_user_key_exists(&email).unwrap();
    let did = crate::agent::AgentService::get_user_did_by_email(&email).unwrap();
    crate::db::Ad4mDb::with_global_instance(|db| db.add_user_prehashed(&email, &did, "unused"))
        .unwrap();
    email
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

/// Points the app registry at one temp file for the whole test process. Tests that insert
/// apps call this and use their own unique keys: a temp dir per test would race when tests
/// run in parallel, because the registry's file path is global. Also opens an in-memory
/// database for the revoked tokens when no test has opened one.
pub fn use_test_apps_file() {
    {
        let db = crate::db::Ad4mDb::global_instance();
        let mut db = db.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        if db.is_none() {
            *db = Some(crate::db::Ad4mDb::new(":memory:").expect("an in-memory database"));
        }
    }
    static APPS_DIR: std::sync::OnceLock<tempfile::TempDir> = std::sync::OnceLock::new();
    let dir =
        APPS_DIR.get_or_init(|| tempfile::tempdir().expect("a temp dir for the app registry"));
    crate::agent::capabilities::apps_map::set_data_file_path(
        dir.path().join("apps.json").to_string_lossy().into(),
    );
}

/// Registers an approved app for `token`, as `agent.generateJwt` does: under the token's id.
pub fn insert_app_with_token(
    request_id: String,
    info: crate::agent::capabilities::AuthInfoExtended,
    token: &str,
) {
    let (id, expires_at) =
        crate::agent::capabilities::read_stored_token_id(token).expect("a token this node issued");
    crate::agent::capabilities::apps_map::insert_app(request_id, info, id, expires_at)
        .expect("the app registry takes the app");
}

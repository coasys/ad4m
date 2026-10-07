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

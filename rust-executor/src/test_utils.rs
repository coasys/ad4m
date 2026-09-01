use std::sync::Arc;

use crate::agent::AgentService;
use crate::billing_backend::{try_init_billing_backend, BillingBackend, LocalBillingBackend};
use crate::config::{set_global_config, Ad4mConfig};
use crate::wallet::{try_init_wallet_backend, LocalWallet, WalletBackend};

pub fn setup_wallet() {
    let local = Arc::new(LocalWallet::new());
    local.generate_keypair("main").expect("generate main key");
    // Try to init; if already initialised (from a prior test), just ensure
    // the key exists. OnceCell prevents double-init panics.
    let _ = try_init_wallet_backend(local as Arc<dyn WalletBackend>);
}

pub fn setup_billing() {
    let local = Arc::new(LocalBillingBackend::new());
    // Try to init; OnceCell prevents double-init panics across tests.
    let _ = try_init_billing_backend(local as Arc<dyn BillingBackend>);
}

pub fn setup_agent() {
    // create_new_keys() reads the global config for signing_key_name().
    // Ensure a default config exists so it does not panic.
    set_global_config(Ad4mConfig::default());

    // Ensure billing backend exists — billing functions (bill_compute,
    // check_compute_credits) call billing_backend() which panics if
    // uninitialised. OnceCell prevents double-init across tests.
    setup_billing();

    AgentService::init_global_instance(String::from("test_data"));
    AgentService::global_instance()
        .lock()
        .expect("couldn't get lock on AgentService")
        .as_mut()
        .expect("Must be some because was initalized above")
        .create_new_keys();
}

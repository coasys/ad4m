use crate::agent::AgentService;
use crate::wallet::Wallet;

pub fn setup_wallet() {
    let wallet_instance = Wallet::instance();
    let mut wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_mut().expect("wallet instance");
    // Idempotent: generate the shared "main" test key only once per process.
    // `generate_keypair` overwrites, so re-running it mid-suite swaps the wallet
    // key out from under the init-once global agent (whose DID is fixed on first
    // init). Signing would then use a key whose DID no longer matches
    // AgentService.did, and every sign->verify round-trip across that boundary
    // (e.g. snapshot proofs) would fail as "signature invalid".
    if wallet_ref.get_did_document(&"main".to_string()).is_none() {
        wallet_ref.generate_keypair("main".to_string());
    }
}

pub fn setup_agent() {
    AgentService::init_global_instance(String::from("test_data"));
    AgentService::global_instance()
        .lock()
        .expect("couldn't get lock on AgentService")
        .as_mut()
        .expect("Must be some because was initalized above")
        .create_new_keys();
}

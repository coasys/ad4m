use crate::agent::AgentService;
use crate::wallet::Wallet;

pub fn setup_wallet() {
    let wallet_instance = Wallet::instance();
    let mut wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_mut().expect("wallet instance");
    wallet_ref.generate_keypair("main".to_string());
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

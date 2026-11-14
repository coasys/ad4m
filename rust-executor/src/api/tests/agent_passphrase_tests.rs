//! The agent passphrase from the keyring: a new agent's is derived and nobody types it; an
//! existing agent's is held the first time it opens the wallet file, and later starts unlock
//! with it; a wrong one is never held; `agent.lock` still needs its own passphrase back; the
//! keyring file never holds a passphrase in plaintext.
//!
//! These share the global wallet and `AgentService` with other tests; each one ends by
//! re-running `setup_agent()` so the next test finds them consistent.

use std::path::Path;

use zeroize::Zeroizing;

use crate::agent::AgentService;
use crate::at_rest::agent_passphrase::{for_new_agent, from_keyring};
use crate::at_rest::keyring_file::{path_in, AgentPassphrase, KeyringFile};
use crate::at_rest::{DataKeys, KeyringError};
use crate::test_utils::{setup_agent, setup_wallet};
use crate::wallet::wallet_backend;

const SECRET: [u8; 32] = [42u8; 32];
const OLD_PASSPHRASE: &str = "the passphrase an operator typed";

/// The keyring of `folder` as a start would open it: read from disk, unlocked by the secret.
fn open_keys(folder: &Path) -> DataKeys {
    let keys = DataKeys::load(path_in(folder)).unwrap();
    keys.unlock(Zeroizing::new(SECRET)).unwrap();
    keys
}

fn keyring_text(folder: &Path) -> String {
    std::fs::read_to_string(path_in(folder)).unwrap()
}

fn assert_keyring_holds_no(folder: &Path, passphrase: &str) {
    let text = keyring_text(folder);
    assert!(
        !text.contains(passphrase),
        "keyring.json holds the passphrase"
    );
    assert!(
        !text.contains(&hex::encode(passphrase)),
        "keyring.json holds the passphrase in hex"
    );
}

/// An agent file made before the keyring, under `OLD_PASSPHRASE`, in `app_path`.
fn existing_agent(app_path: &str) {
    std::fs::create_dir_all(format!("{app_path}/ad4m")).unwrap();
    let mut service = AgentService::new(app_path.to_string());
    service.create_new_keys();
    service.save(OLD_PASSPHRASE.to_string());
}

/// The agent of `app_path` as a start finds it: the wallet locked, the agent file loaded.
fn restarted_agent(app_path: &str) -> AgentService {
    wallet_backend().lock("a passphrase from before the restart");
    let mut service = AgentService::new(app_path.to_string());
    service.load();
    assert!(!service.is_unlocked());
    service
}

#[test]
fn a_new_agent_derives_its_passphrase_and_nobody_types_it() {
    let folder = tempfile::tempdir().unwrap();
    let keys = open_keys(folder.path());
    assert_eq!(
        from_keyring(Some(&keys)),
        None,
        "a new keyring gives no passphrase"
    );

    let derived = for_new_agent(Some(&keys), "typed by a caller").unwrap();
    assert_eq!(derived.len(), 64);
    assert_ne!(derived.as_str(), "typed by a caller");
    assert_eq!(
        for_new_agent(Some(&keys), "another").unwrap(),
        derived,
        "the same passphrase each time"
    );

    // The next start gives it without anybody typing it.
    let reopened = open_keys(folder.path());
    assert_eq!(from_keyring(Some(&reopened)).unwrap(), derived);
    let file = KeyringFile::read(&path_in(folder.path())).unwrap().unwrap();
    assert!(matches!(
        file.agent_passphrase,
        Some(AgentPassphrase::Derived { .. })
    ));
    assert_keyring_holds_no(folder.path(), &derived);

    // Without a keyring the typed passphrase serves, as before.
    assert_eq!(for_new_agent(None, "typed").unwrap().as_str(), "typed");
}

#[test]
fn a_sealed_keyring_gives_no_passphrase() {
    let folder = tempfile::tempdir().unwrap();
    open_keys(folder.path())
        .hold_agent_passphrase(OLD_PASSPHRASE)
        .unwrap();
    let sealed = DataKeys::load(path_in(folder.path())).unwrap();
    assert_eq!(from_keyring(Some(&sealed)), None);
    assert_eq!(from_keyring(None), None);
}

#[test]
fn an_existing_agent_is_held_on_its_first_unlock_and_the_next_start_needs_no_passphrase() {
    setup_wallet();
    setup_agent();
    let tmp = tempfile::tempdir().unwrap();
    let app_path = tmp.path().to_str().unwrap().to_string();
    let data = tempfile::tempdir().unwrap();
    existing_agent(&app_path);

    // First start with a keyring: a wrong passphrase opens nothing and is not held.
    let keys = open_keys(data.path());
    let mut service = restarted_agent(&app_path);
    assert!(service
        .unlock_with_keys("a wrong passphrase".into(), Some(&keys))
        .is_err());
    assert!(!service.is_unlocked());
    assert_eq!(keys.agent_passphrase().unwrap(), None);
    assert_keyring_holds_no(data.path(), "a wrong passphrase");

    // The right one opens the wallet and is held, sealed.
    service
        .unlock_with_keys(OLD_PASSPHRASE.into(), Some(&keys))
        .unwrap();
    assert!(service.is_unlocked());
    assert_eq!(
        keys.agent_passphrase()
            .unwrap()
            .as_deref()
            .map(String::as_str),
        Some(OLD_PASSPHRASE)
    );
    let file = KeyringFile::read(&path_in(data.path())).unwrap().unwrap();
    assert!(matches!(
        file.agent_passphrase,
        Some(AgentPassphrase::Held { .. })
    ));
    assert_keyring_holds_no(data.path(), OLD_PASSPHRASE);

    // Next start: the keyring unlocks the agent, whatever a caller passes.
    let keys = open_keys(data.path());
    let mut service = restarted_agent(&app_path);
    service
        .unlock_with_keys("anything".into(), Some(&keys))
        .unwrap();
    assert!(service.is_unlocked());
    assert_eq!(service.passphrase.as_deref(), Some(OLD_PASSPHRASE));

    // The wallet file still opens with the agent's passphrase: nothing rewrote it.
    let store: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(format!("{app_path}/ad4m/agent.json")).unwrap(),
    )
    .unwrap();
    let scratch = crate::wallet::LocalWallet::new();
    scratch.load(store["keystore"].as_str().unwrap());
    assert!(scratch.unlock(OLD_PASSPHRASE).is_ok());

    // The keyring holds one passphrase: another is refused.
    assert!(matches!(
        keys.hold_agent_passphrase("another passphrase"),
        Err(KeyringError::AgentPassphraseExists)
    ));
    assert!(!keys.hold_agent_passphrase(OLD_PASSPHRASE).unwrap());
    setup_agent();
}

#[test]
fn agent_lock_still_needs_its_own_passphrase_and_is_never_held() {
    setup_wallet();
    setup_agent();
    let tmp = tempfile::tempdir().unwrap();
    let app_path = tmp.path().to_str().unwrap().to_string();
    let data = tempfile::tempdir().unwrap();
    existing_agent(&app_path);

    // A keyring that holds nothing yet: a passphrase that only undoes `agent.lock` in memory
    // is not the agent's, so it is not held.
    let keys = open_keys(data.path());
    let mut service = AgentService::new(app_path.clone());
    service.load();
    service.passphrase = Some(OLD_PASSPHRASE.to_string());
    service.lock("a lock passphrase".into());
    assert!(!service.is_unlocked());
    service
        .unlock_with_keys("a lock passphrase".into(), Some(&keys))
        .unwrap();
    assert_eq!(keys.agent_passphrase().unwrap(), None);
    assert_keyring_holds_no(data.path(), "a lock passphrase");

    // With the passphrase held, a lock is undone only by its own passphrase, and the
    // agent's passphrase stays the one the saves use.
    keys.hold_agent_passphrase(OLD_PASSPHRASE).unwrap();
    service.passphrase = Some(OLD_PASSPHRASE.to_string());
    service.lock("q".into());
    assert!(service
        .unlock_with_keys("not q".into(), Some(&keys))
        .is_err());
    assert!(!service.is_unlocked());
    service.unlock_with_keys("q".into(), Some(&keys)).unwrap();
    assert!(service.is_unlocked());
    assert_eq!(service.passphrase.as_deref(), Some(OLD_PASSPHRASE));
    setup_agent();
}

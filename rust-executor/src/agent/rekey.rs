//! A new main wallet key for the main agent (`identity.rekey`): the executor's own part. The
//! old key stays in the wallet under [`retired_key_name`], for history only: nothing signs with
//! it again, and what it signed still verifies, since a `did:key` carries its public key.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;

use super::{did_document, signing_key_id, AgentService};
use crate::wallet::{wallet_backend, WalletBackend, KEY_NAME_MAIN};

/// The old and the new device `did:key` of the main agent.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rekeyed {
    pub old_did: String,
    pub new_did: String,
}

/// Counts the re-keys of this run, so a session that read the main agent's DID reads it again
/// after one (`Session::acts_as_did`).
static MAIN_KEY_GENERATION: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// How many times the main key changed in this run.
pub fn main_key_generation() -> u64 {
    MAIN_KEY_GENERATION.load(std::sync::atomic::Ordering::SeqCst)
}

/// The wallet name of the main agent's key `did` once a re-key retired it.
pub fn retired_key_name(did: &str) -> String {
    format!("retired-main:{did}")
}

/// Retires the main key `old_did` and makes a new `main` key. When the new key cannot be made,
/// the old key is `main` again.
fn replace_main_key(backend: &dyn WalletBackend, old_did: &str) -> Result<(), AnyError> {
    backend.retire_key(KEY_NAME_MAIN, &retired_key_name(old_did))?;
    if let Err(e) = backend.generate_keypair(KEY_NAME_MAIN) {
        put_old_main_key_back(backend, old_did)
            .map_err(|undo| anyhow!("{e}; and the old key is not back: {undo}"))?;
        return Err(e);
    }
    Ok(())
}

/// Undoes [`replace_main_key`]: a new `main` key, if there is one, goes under its own retired
/// name, and the retired key `old_did` is `main` again.
fn put_old_main_key_back(backend: &dyn WalletBackend, old_did: &str) -> Result<(), AnyError> {
    if let Some(new) = backend.get_did_document(KEY_NAME_MAIN) {
        backend.retire_key(KEY_NAME_MAIN, &retired_key_name(&new.id))?;
    }
    backend.retire_key(&retired_key_name(old_did), KEY_NAME_MAIN)
}

impl AgentService {
    /// Gives the main agent a new wallet key and keeps the old one under its retired name.
    /// Moves this service's own copies of the DID: the DID, its document, the signing key id
    /// and the profile (the profile's DID; its links stay as signed), and saves the agent
    /// file and the profile file; when the agent file cannot be saved, it changes nothing
    /// (the new key stays only as a retired key). Needs an unlocked agent whose passphrase is in memory, and
    /// a wallet that can rename keys (not the shared one).
    pub fn rekey_main_key(&mut self) -> Result<Rekeyed, AnyError> {
        self.signing_checks()?;
        // The profile from disk first: a load later would read the old DID back.
        self.ensure_main_agent_loaded();
        let passphrase = self.passphrase.clone().ok_or_else(|| {
            anyhow!("the agent's passphrase is not in memory: unlock the agent, then re-key")
        })?;
        let backend = wallet_backend();
        if !backend.can_rename_keys() {
            return Err(anyhow!(
                "this wallet backend cannot keep the old key: no re-key"
            ));
        }
        let old_did = self
            .did
            .clone()
            .ok_or_else(|| anyhow!("the main agent has no DID"))?;
        let before = (
            self.did_document.clone(),
            self.signing_key_id.clone(),
            self.agent.clone(),
        );
        replace_main_key(backend.as_ref(), &old_did)?;
        let document = did_document();
        let new_did = document.id.clone();
        self.did_document = Some(serde_json::to_string(&document)?);
        self.did = Some(new_did.clone());
        self.signing_key_id = Some(signing_key_id());
        if let Some(agent) = self.agent.as_mut() {
            agent.did = new_did.clone();
        }
        // The agent file holds the keystore: without it saved, the old key is `main` again
        // and the new one, which signed nothing, stays retired.
        if let Err(e) = self.try_save(&passphrase) {
            (self.did_document, self.signing_key_id, self.agent) = before;
            self.did = Some(old_did.clone());
            put_old_main_key_back(backend.as_ref(), &old_did)
                .map_err(|undo| anyhow!("{e}; and the old key is not back: {undo}"))?;
            return Err(e);
        }
        // The new key is saved, so the re-key stands. A profile file left behind is repaired at
        // the next load, from the profile the agent file holds.
        if self.agent.is_some() {
            if let Err(e) = self.write_agent_profile() {
                log::warn!("identity.rekey: could not write the profile file: {e}");
            }
        }
        MAIN_KEY_GENERATION.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Ok(Rekeyed { old_did, new_did })
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::agent::{did_for_context, AgentContext};
    use crate::wallet::LocalWallet;

    /// A local wallet with faults: it may refuse to rename keys (as a shared wallet does), and
    /// its key generation may fail.
    pub(crate) struct TestWallet {
        pub(crate) inner: LocalWallet,
        pub(crate) renames: bool,
        pub(crate) generate_fails: bool,
    }

    impl WalletBackend for TestWallet {
        fn generate_keypair(&self, name: &str) -> Result<(), AnyError> {
            if self.generate_fails {
                return Err(anyhow!("the wallet cannot make a key"));
            }
            self.inner.generate_keypair(name)
        }
        fn get_secret_key(&self, name: &str) -> Option<Vec<u8>> {
            self.inner.get_secret_key(name)
        }
        fn get_public_key(&self, name: &str) -> Option<Vec<u8>> {
            self.inner.get_public_key(name)
        }
        fn get_did_document(&self, name: &str) -> Option<did_key::Document> {
            WalletBackend::get_did_document(&self.inner, name)
        }
        fn sign(&self, name: &str, message: &[u8]) -> Option<Vec<u8>> {
            self.inner.sign(name, message)
        }
        fn list_key_names(&self) -> Vec<String> {
            self.inner.list_key_names()
        }
        fn key_exists(&self, name: &str) -> bool {
            self.inner.key_exists(name)
        }
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }
        fn can_rename_keys(&self) -> bool {
            self.renames
        }
        fn retire_key(&self, name: &str, retired_name: &str) -> Result<(), AnyError> {
            self.inner.retire_key(name, retired_name)
        }
    }

    /// A re-key whose new key cannot be made puts the old key back as `main`, and leaves no
    /// retired key behind.
    #[test]
    fn a_rekey_whose_new_key_cannot_be_made_keeps_the_old_key() {
        let inner = LocalWallet::new();
        inner.generate_keypair(KEY_NAME_MAIN).unwrap();
        let old_did = WalletBackend::get_did_document(&inner, KEY_NAME_MAIN)
            .unwrap()
            .id;
        let wallet = TestWallet {
            inner,
            renames: true,
            generate_fails: true,
        };

        let refused = replace_main_key(&wallet, &old_did);

        assert_eq!(
            refused.unwrap_err().to_string(),
            "the wallet cannot make a key"
        );
        assert_eq!(
            wallet.get_did_document(KEY_NAME_MAIN).map(|d| d.id),
            Some(old_did)
        );
        assert_eq!(wallet.list_key_names(), vec![KEY_NAME_MAIN.to_string()]);
    }

    /// A re-key makes a new main key, keeps the old one under its retired name, moves the
    /// service's DID and profile, and leaves old signatures verifiable.
    #[test]
    fn a_rekey_makes_a_new_main_key_and_keeps_the_old_one_for_history() {
        let dir = tempfile::tempdir().unwrap();
        let app_path = dir.path().to_string_lossy().into_owned();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        AgentService::init_global_instance(app_path);
        let old_did = AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("rekey-test".into());
            svc.store_agent_profile();
            svc.save("rekey-test".into());
            svc.did.clone().unwrap()
        });
        let message = "signed before the re-key".to_string();
        let old_signature = crate::agent::sign_string_hex(message.clone()).unwrap();

        let rekeyed =
            AgentService::with_mutable_global_instance(|svc| svc.rekey_main_key()).unwrap();

        assert_eq!(rekeyed.old_did, old_did);
        assert_ne!(rekeyed.new_did, old_did);
        // The main agent's DID, wherever the executor reads it.
        assert_eq!(
            did_for_context(&AgentContext::main_agent()).unwrap(),
            rekeyed.new_did
        );
        assert_eq!(crate::agent::did_document().id, rekeyed.new_did);
        let profile = AgentService::get_agent_for_context(&AgentContext::main_agent()).unwrap();
        assert_eq!(profile.did, rekeyed.new_did);
        // The files on disk carry the new DID.
        let agent_file = std::fs::read_to_string(dir.path().join("ad4m/agent.json")).unwrap();
        assert!(agent_file.contains(&rekeyed.new_did));
        let profile_file =
            std::fs::read_to_string(dir.path().join("ad4m/agentProfile.json")).unwrap();
        assert!(profile_file.contains(&rekeyed.new_did));
        assert!(!profile_file.contains(&old_did));
        // The old key stays, under its retired name, and what it signed still verifies.
        let backend = wallet_backend();
        let retired = backend
            .get_did_document(&retired_key_name(&old_did))
            .expect("the old key stays in the wallet");
        assert_eq!(retired.id, old_did);
        assert!(crate::agent::signatures::verify_string_signed_by_did(
            &old_did,
            &message,
            &old_signature
        )
        .unwrap());
        // New signatures are the new key's, and not the old one's.
        let new_signature = crate::agent::sign_string_hex(message.clone()).unwrap();
        assert!(crate::agent::signatures::verify_string_signed_by_did(
            &rekeyed.new_did,
            &message,
            &new_signature
        )
        .unwrap());
        assert!(!crate::agent::signatures::verify_string_signed_by_did(
            &old_did,
            &message,
            &new_signature
        )
        .unwrap());

        // A restart reads the same from the saved files: the new key signs as the new DID,
        // and the old key is in the saved keystore under its retired name.
        wallet_backend().lock("scratch");
        AgentService::with_mutable_global_instance(|svc| {
            svc.passphrase = None;
            svc.load();
            svc.unlock("rekey-test".to_string())
        })
        .unwrap();
        assert_eq!(
            did_for_context(&AgentContext::main_agent()).unwrap(),
            rekeyed.new_did
        );
        let reloaded_signature = crate::agent::sign_string_hex(message.clone()).unwrap();
        assert!(crate::agent::signatures::verify_string_signed_by_did(
            &rekeyed.new_did,
            &message,
            &reloaded_signature
        )
        .unwrap());
        assert_eq!(
            wallet_backend()
                .get_did_document(&retired_key_name(&old_did))
                .map(|document| document.id),
            Some(old_did)
        );
        crate::test_utils::setup_agent();
    }

    /// A re-key whose agent file cannot be saved changes nothing: the old key is `main` again,
    /// the service keeps the old DID, and the profile file is not written.
    #[test]
    fn a_rekey_that_cannot_save_keeps_the_old_key() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        let old_did = AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("p".into());
            svc.store_agent_profile();
            svc.save("p".into());
            svc.did.clone().unwrap()
        });
        // A folder where the agent file is: the save cannot replace it.
        let agent_file = dir.path().join("ad4m/agent.json");
        std::fs::remove_file(&agent_file).unwrap();
        std::fs::create_dir(&agent_file).unwrap();

        let refused = AgentService::with_mutable_global_instance(|svc| svc.rekey_main_key());
        let did = did_for_context(&AgentContext::main_agent()).unwrap();
        let main_key = wallet_backend()
            .get_did_document(KEY_NAME_MAIN)
            .map(|d| d.id);
        let profile_file =
            std::fs::read_to_string(dir.path().join("ad4m/agentProfile.json")).unwrap();
        crate::test_utils::setup_agent();

        assert!(refused.is_err());
        assert_eq!(did, old_did);
        assert_eq!(main_key, Some(old_did.clone()));
        assert!(profile_file.contains(&old_did), "{profile_file}");
    }

    /// A re-key that saved the agent file and stopped before the profile file: the next load
    /// takes the profile the agent file holds, and writes it back to the profile file.
    #[test]
    fn a_load_after_a_rekey_cut_short_takes_the_agent_files_profile() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("p".into());
            svc.store_agent_profile();
            svc.save("p".into());
        });
        let profile_path = dir.path().join("ad4m/agentProfile.json");
        let old_profile = std::fs::read_to_string(&profile_path).unwrap();

        let rekeyed =
            AgentService::with_mutable_global_instance(|svc| svc.rekey_main_key()).unwrap();
        // The process stopped before the profile file was written.
        std::fs::write(&profile_path, &old_profile).unwrap();
        let profile_did = AgentService::with_mutable_global_instance(|svc| {
            svc.load();
            svc.agent.as_ref().map(|agent| agent.did.clone())
        });
        let profile_file = std::fs::read_to_string(&profile_path).unwrap();
        crate::test_utils::setup_agent();

        assert_eq!(profile_did, Some(rekeyed.new_did.clone()));
        assert!(profile_file.contains(&rekeyed.new_did), "{profile_file}");
    }

    /// A re-key, or a start-up repair, that stopped in the middle of writing the profile file
    /// left a truncated one: the next load takes the profile the agent file holds, and writes
    /// it back.
    #[test]
    fn a_load_after_a_profile_write_cut_short_takes_the_agent_files_profile() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("p".into());
            svc.store_agent_profile();
            svc.save("p".into());
        });
        let profile_path = dir.path().join("ad4m/agentProfile.json");

        let rekeyed =
            AgentService::with_mutable_global_instance(|svc| svc.rekey_main_key()).unwrap();
        // The process stopped in the middle of the profile file write, inside a two-byte
        // character (`é` is 0xC3 0xA9), so the file is neither JSON nor UTF-8.
        std::fs::write(&profile_path, b"{\"did\":\"did:key:z\xC3").unwrap();
        let profile_did = AgentService::with_mutable_global_instance(|svc| {
            svc.load();
            svc.agent.as_ref().map(|agent| agent.did.clone())
        });
        let profile_file = std::fs::read_to_string(&profile_path).unwrap();
        crate::test_utils::setup_agent();

        assert_eq!(profile_did, Some(rekeyed.new_did.clone()));
        assert!(profile_file.contains(&rekeyed.new_did), "{profile_file}");
    }

    /// A profile file that is there but cannot be read (here: no read permission) is not one a
    /// crash cut short: the load serves the agent file's profile in memory and leaves the file
    /// alone, so a profile edit newer than the agent file survives a transient read error.
    #[cfg(unix)]
    #[test]
    fn a_load_that_cannot_read_the_profile_file_leaves_it_alone() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("p".into());
            svc.store_agent_profile();
            svc.save("p".into());
            // A profile edit after the save: only the profile file holds it.
            let mut agent = svc.agent.clone().unwrap();
            agent.direct_message_language = Some("lang://edited".into());
            svc.save_agent_profile(agent);
        });
        let profile_path = dir.path().join("ad4m/agentProfile.json");
        let edited = std::fs::read_to_string(&profile_path).unwrap();
        std::fs::set_permissions(&profile_path, std::fs::Permissions::from_mode(0o000)).unwrap();
        if std::fs::read(&profile_path).is_ok() {
            // root reads a 0o000 file: nothing to probe here.
            crate::test_utils::setup_agent();
            return;
        }

        let loaded = AgentService::with_mutable_global_instance(|svc| {
            svc.load();
            svc.agent
                .as_ref()
                .and_then(|agent| agent.direct_message_language.clone())
        });
        std::fs::set_permissions(&profile_path, std::fs::Permissions::from_mode(0o600)).unwrap();
        let profile_file = std::fs::read_to_string(&profile_path).unwrap();
        crate::test_utils::setup_agent();

        assert_eq!(loaded, None, "the agent file's profile serves in memory");
        assert_eq!(profile_file, edited, "the unreadable file is left alone");
    }

    #[test]
    fn a_rekey_needs_the_passphrase_in_memory() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        let refused = AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("p".into());
            svc.store_agent_profile();
            svc.save("p".into());
            svc.passphrase = None;
            svc.rekey_main_key()
        });
        crate::test_utils::setup_agent();
        assert!(refused.unwrap_err().to_string().contains("passphrase"));
    }
}

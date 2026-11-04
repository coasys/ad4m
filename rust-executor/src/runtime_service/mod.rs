use std::io::Read;
use std::{fs::File, sync::Mutex};
pub(crate) mod notification_access;
pub mod runtime_service_extension;
use std::sync::Arc;

/// A network's bootstrap seed (`mainnet_seed.seed` in the data folder, written by `init`): its
/// system languages, the agents whose languages it installs, and its identity store.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BootstrapSeed {
    #[serde(rename = "trustedAgents")]
    pub trusted_agents: Vec<String>,
    #[serde(rename = "knownLinkLanguages")]
    pub known_link_languages: Vec<String>,
    #[serde(rename = "directMessageLanguage")]
    pub direct_message_language: String,
    #[serde(rename = "agentLanguage")]
    pub agent_language: String,
    #[serde(rename = "perspectiveLanguage")]
    pub perspective_language: String,
    #[serde(rename = "neighbourhoodLanguage")]
    pub neighbourhood_language: String,
    #[serde(rename = "languageLanguageBundle")]
    pub language_language_bundle: String,
    /// The network's identity store (`did:scid:coasys:1` key logs), used when the config names
    /// none (`identity::IdentitySettings::from_config`). Absent: the network has no store.
    #[serde(
        rename = "identityStore",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub identity_store: Option<IdentityStoreSeed>,
}

/// A seed's `identityStore`: the store's base URL and the keys (bare Ed25519 multikeys, `z6Mk…`)
/// its signed heads must carry. A list, so the store's key can rotate.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct IdentityStoreSeed {
    pub url: String,
    pub keys: Vec<String>,
}

use serde::{Deserialize, Serialize};

use crate::events::Event;
use crate::pubsub::get_global_pubsub;
use crate::types::{ExceptionInfo, ExceptionType, NotificationInput};
use crate::{agent::did, types::SentMessage};

lazy_static! {
    static ref RUNTIME_INSTANCE: Arc<Mutex<Option<RuntimeService>>> = Arc::new(Mutex::new(None));
}

pub struct RuntimeService {
    seed: BootstrapSeed,
}

impl RuntimeService {
    pub fn init_global_instance(mainnet_seed_file_path: String) {
        let mut instance = RUNTIME_INSTANCE.lock().unwrap();
        match RuntimeService::new(&mainnet_seed_file_path) {
            Ok(service) => *instance = Some(service),
            Err(e) => {
                log::warn!(
                    "Failed to load mainnet seed from '{}': {}. RuntimeService will not be available.",
                    mainnet_seed_file_path, e
                );
            }
        }
    }

    fn new(mainnet_seed_file_path: &str) -> Result<RuntimeService, deno_core::anyhow::Error> {
        let mut file = File::open(mainnet_seed_file_path).map_err(|e| {
            deno_core::anyhow::anyhow!("Cannot open seed file '{}': {}", mainnet_seed_file_path, e)
        })?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| deno_core::anyhow::anyhow!("Cannot read seed file: {}", e))?;
        let seed: BootstrapSeed = serde_json::from_str(&contents)
            .map_err(|e| deno_core::anyhow::anyhow!("Cannot parse seed file: {}", e))?;

        Ok(RuntimeService { seed })
    }

    pub fn global_instance() -> Arc<Mutex<Option<RuntimeService>>> {
        RUNTIME_INSTANCE.clone()
    }

    pub fn with_global_instance<F, R>(func: F) -> R
    where
        F: FnOnce(&RuntimeService) -> R,
    {
        let global_instance_arc = RuntimeService::global_instance();
        let lock_result = global_instance_arc.lock();
        let runtime_lock = lock_result.expect("Couldn't get lock on Ad4mDb");
        let runtime_ref = runtime_lock.as_ref().expect("Ad4mDb not initialized");
        func(runtime_ref)
    }

    /// The seed's identity store; `None` when the network has none.
    pub fn identity_store(&self) -> Option<IdentityStoreSeed> {
        self.seed.identity_store.clone()
    }

    /// The loaded seed's identity store; `None` when the network has none or no seed is
    /// loaded (the service did not start).
    pub fn seed_identity_store() -> Option<IdentityStoreSeed> {
        RUNTIME_INSTANCE
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .as_ref()
            .and_then(|service| service.identity_store())
    }

    pub fn get_language_language_bundle(&self) -> String {
        self.seed.language_language_bundle.clone()
    }

    pub fn get_agent_language(&self) -> String {
        self.seed.agent_language.clone()
    }

    pub fn get_neighbourhood_language(&self) -> String {
        self.seed.neighbourhood_language.clone()
    }

    pub fn get_perspective_language(&self) -> String {
        self.seed.perspective_language.clone()
    }

    pub fn get_direct_message_language(&self) -> String {
        self.seed.direct_message_language.clone()
    }

    pub fn get_trusted_agents(&self) -> Vec<String> {
        let mut trusted_agents: Vec<String> = self.seed.trusted_agents.clone();
        let mut stored_agents = crate::db_backend::db_backend()
            .get_all_trusted_agents()
            .map_err(|e| e.to_string())
            .unwrap_or_default();
        trusted_agents.push(did());
        trusted_agents.append(&mut stored_agents);
        trusted_agents.sort();
        trusted_agents.dedup();
        trusted_agents
    }

    pub fn add_trusted_agent(&self, new_agents: Vec<String>) {
        let _ = crate::db_backend::db_backend()
            .add_trusted_agents(new_agents)
            .map_err(|e| e.to_string());
    }

    pub fn remove_trusted_agent(&self, agents_to_remove: Vec<String>) {
        let _ = crate::db_backend::db_backend()
            .remove_trusted_agents(agents_to_remove)
            .map_err(|e| e.to_string());
    }

    pub fn get_know_link_languages(&self) -> Vec<String> {
        let mut languages: Vec<String> = self.seed.known_link_languages.clone();
        let mut stored_languages = crate::db_backend::db_backend()
            .get_all_known_link_languages()
            .map_err(|e| e.to_string())
            .unwrap_or_default();
        languages.append(&mut stored_languages);
        languages.sort();
        languages.dedup();
        languages
    }

    pub fn add_know_link_language(&self, language: Vec<String>) {
        let _ = crate::db_backend::db_backend()
            .add_known_link_languages(language)
            .map_err(|e| e.to_string());
    }

    pub fn remove_know_link_language(&self, language_to_remove: Vec<String>) {
        let _ = crate::db_backend::db_backend()
            .remove_known_link_languages(language_to_remove)
            .map_err(|e| e.to_string());
    }

    pub fn get_friends(&self) -> Vec<String> {
        crate::db_backend::db_backend()
            .get_all_friends()
            .map_err(|e| e.to_string())
            .unwrap_or_default()
    }

    pub fn add_friend(&self, friends: Vec<String>) {
        let _ = crate::db_backend::db_backend()
            .add_friends(friends)
            .map_err(|e| e.to_string());
    }

    pub fn remove_friend(&self, friend_to_remove: Vec<String>) {
        let _ = crate::db_backend::db_backend()
            .remove_friends(friend_to_remove)
            .map_err(|e| e.to_string());
    }

    pub fn get_outbox(&self) -> Vec<SentMessage> {
        crate::db_backend::db_backend()
            .get_all_from_outbox()
            .map_err(|e| e.to_string())
            .unwrap_or_default()
    }

    pub fn add_message_to_outbox(&self, message: SentMessage) {
        let _ = crate::db_backend::db_backend()
            .add_to_outbox(&message.message, message.recipient)
            .map_err(|e| e.to_string());
    }

    pub async fn request_install_notification(
        notification_input: NotificationInput,
        owner_did: String,
        managed_user: bool,
    ) -> Result<String, String> {
        // A managed user's notification is granted now: the caller has refused
        // every perspective the user does not own (see `notification_access`).
        // The main agent's notification waits for operator approval.
        let notification_id = crate::db_backend::db_backend()
            .add_notification(notification_input, &owner_did, managed_user)
            .map_err(|e| e.to_string())?;

        if !managed_user {
            // Main agent needs manual approval via ADAM Launcher
            let notification = crate::db_backend::db_backend()
                .get_notification(notification_id.clone())
                .map_err(|e| e.to_string())?
                .ok_or("Notification with given id not found")?;

            let exception_info = ExceptionInfo {
                title: "Request to install notifications for the app".to_string(),
                message: format!(
                    "{} is waiting for notifications to be authenticated, open the ADAM Launcher for more information.",
                    notification.app_name
                ),
                r#type: ExceptionType::InstallNotificationRequest,
                // The redacted view: the request goes to every approver's session.
                addon: Some(serde_json::to_string(&notification.view()).unwrap()),
            };

            // The main agent's sessions that may approve apps answer it.
            let target = crate::agent::AgentContext::main_agent();
            get_global_pubsub()
                .await
                .publish_event(Event::approval_request(&exception_info, &target));
        }

        Ok(notification_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The seed this build writes at `init` names its network's identity store
    /// (`https://store.ad4m.dev`) with a key the executor can match against a signed head.
    #[test]
    fn the_embedded_seed_names_the_networks_identity_store() {
        let seed: BootstrapSeed = serde_json::from_str(crate::globals::MAINNET_JSON).unwrap();
        let store = seed
            .identity_store
            .expect("the seed names an identity store");
        assert_eq!(
            store,
            IdentityStoreSeed {
                url: "https://store.ad4m.dev".into(),
                keys: vec!["z6MkhPbF9K34GSKpCo9n7XVdAoCctP2Kw7SNJeRWLx7bu9Fj".into()],
            }
        );
        assert_eq!(crate::identity::check_store_keys(&store.keys), Ok(()));
        assert_eq!(crate::identity::check_store_url(Some(&store.url)), Ok(()));
    }

    /// A seed without `identityStore` (every seed written before the field, or a network
    /// without a store) parses, has no store, and writes no `identityStore` back.
    #[test]
    fn a_seed_without_an_identity_store_parses_and_round_trips() {
        let json = serde_json::json!({
            "trustedAgents": [], "knownLinkLanguages": [], "directMessageLanguage": "",
            "agentLanguage": "", "perspectiveLanguage": "", "neighbourhoodLanguage": "",
            "languageLanguageBundle": "",
        });
        let parsed: BootstrapSeed = serde_json::from_value(json.clone()).unwrap();
        assert_eq!(parsed.identity_store, None);
        assert_eq!(serde_json::to_value(&parsed).unwrap(), json);
    }

    /// An `identityStore` without its keys, or with a field it does not know, is refused: a
    /// seed must name the keys of the store it names.
    #[test]
    fn an_identity_store_needs_its_url_and_keys() {
        let seed = |store: serde_json::Value| {
            serde_json::from_value::<BootstrapSeed>(serde_json::json!({
                "trustedAgents": [], "knownLinkLanguages": [], "directMessageLanguage": "",
                "agentLanguage": "", "perspectiveLanguage": "", "neighbourhoodLanguage": "",
                "languageLanguageBundle": "", "identityStore": store,
            }))
        };
        assert!(seed(serde_json::json!({ "url": "https://s" })).is_err());
        assert!(seed(serde_json::json!({ "keys": [] })).is_err());
        assert!(seed(serde_json::json!({ "url": "https://s", "keys": [], "key": "z" })).is_err());
        assert_eq!(
            seed(serde_json::json!({ "url": "https://s", "keys": ["z6MkA"] }))
                .unwrap()
                .identity_store,
            Some(IdentityStoreSeed {
                url: "https://s".into(),
                keys: vec!["z6MkA".into()],
            })
        );
    }
}

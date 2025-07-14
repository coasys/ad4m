use std::io::Read;
use std::{fs::File, sync::Mutex};
pub mod runtime_service_extension;
use chrono::{DateTime, Utc};
use std::collections::VecDeque;
use std::sync::Arc;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DebugStringEntry {
    pub language_address: String,
    pub debug_string: String,
    pub operation: String,
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug, Serialize, Deserialize)]
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
}

use serde::{Deserialize, Serialize};

use crate::graphql::graphql_types::{ExceptionInfo, ExceptionType, NotificationInput};
use crate::pubsub::{get_global_pubsub, EXCEPTION_OCCURRED_TOPIC};
use crate::{agent::did, db::Ad4mDb, graphql::graphql_types::SentMessage};

lazy_static! {
    static ref RUNTIME_INSTANCE: Arc<Mutex<Option<RuntimeService>>> = Arc::new(Mutex::new(None));
    static ref DEBUG_STRINGS: Arc<Mutex<VecDeque<DebugStringEntry>>> =
        Arc::new(Mutex::new(VecDeque::new()));
}

const MAX_DEBUG_STRINGS: usize = 100; // Keep last 100 debug strings

pub struct RuntimeService {
    seed: BootstrapSeed,
}

impl RuntimeService {
    pub fn init_global_instance(mainnet_seed_file_path: String) {
        let mut instance = RUNTIME_INSTANCE.lock().unwrap();
        *instance = Some(RuntimeService::new(mainnet_seed_file_path));
    }

    fn new(mainnet_seed_file_path: String) -> RuntimeService {
        let mut file = File::open(mainnet_seed_file_path).unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        let seed: BootstrapSeed = serde_json::from_str(&contents).unwrap();

        RuntimeService { seed }
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

    pub fn get_trusted_agents(&self) -> Vec<String> {
        let mut trusted_agents: Vec<String> = self.seed.trusted_agents.clone();
        let mut stored_agents = Ad4mDb::with_global_instance(|db| db.get_all_trusted_agents())
            .map_err(|e| e.to_string())
            .unwrap_or_default();
        trusted_agents.push(did());
        trusted_agents.append(&mut stored_agents);
        trusted_agents.sort();
        trusted_agents.dedup();
        trusted_agents
    }

    pub fn add_trusted_agent(&self, new_agents: Vec<String>) {
        let _ = Ad4mDb::with_global_instance(|db| db.add_trusted_agents(new_agents))
            .map_err(|e| e.to_string());
    }

    pub fn remove_trusted_agent(&self, agents_to_remove: Vec<String>) {
        let _ = Ad4mDb::with_global_instance(|db| db.remove_trusted_agents(agents_to_remove))
            .map_err(|e| e.to_string());
    }

    pub fn get_know_link_languages(&self) -> Vec<String> {
        let mut languages: Vec<String> = self.seed.known_link_languages.clone();
        let mut stored_languages =
            Ad4mDb::with_global_instance(|db| db.get_all_known_link_languages())
                .map_err(|e| e.to_string())
                .unwrap_or_default();
        languages.append(&mut stored_languages);
        languages.sort();
        languages.dedup();
        languages
    }

    pub fn add_know_link_language(&self, language: Vec<String>) {
        let _ = Ad4mDb::with_global_instance(|db| db.add_known_link_languages(language))
            .map_err(|e| e.to_string());
    }

    pub fn remove_know_link_language(&self, language_to_remove: Vec<String>) {
        let _ =
            Ad4mDb::with_global_instance(|db| db.remove_known_link_languages(language_to_remove))
                .map_err(|e| e.to_string());
    }

    pub fn get_friends(&self) -> Vec<String> {
        Ad4mDb::with_global_instance(|db| db.get_all_friends())
            .map_err(|e| e.to_string())
            .unwrap_or_default()
    }

    pub fn add_friend(&self, friends: Vec<String>) {
        let _ =
            Ad4mDb::with_global_instance(|db| db.add_friends(friends)).map_err(|e| e.to_string());
    }

    pub fn remove_friend(&self, friend_to_remove: Vec<String>) {
        let _ = Ad4mDb::with_global_instance(|db| db.remove_friends(friend_to_remove))
            .map_err(|e| e.to_string());
    }

    pub fn get_outbox(&self) -> Vec<SentMessage> {
        Ad4mDb::with_global_instance(|db| db.get_all_from_outbox())
            .map_err(|e| e.to_string())
            .unwrap_or_default()
    }

    pub fn add_message_to_outbox(&self, message: SentMessage) {
        let _ = Ad4mDb::with_global_instance(|db| {
            db.add_to_outbox(&message.message, message.recipient)
        })
        .map_err(|e| e.to_string());
    }

    pub async fn request_install_notification(
        notification_input: NotificationInput,
    ) -> Result<String, String> {
        let notification_id =
            Ad4mDb::with_global_instance(|db| db.add_notification(notification_input))
                .map_err(|e| e.to_string())?;

        let notification =
            Ad4mDb::with_global_instance(|db| db.get_notification(notification_id.clone()))
                .map_err(|e| e.to_string())?
                .ok_or("Notification with given id not found")?;

        let exception_info = ExceptionInfo {
            title: "Request to install notifications for the app".to_string(),
            message: format!(
                "{} is waiting for notifications to be authenticated, open the ADAM Launcher for more information.",
                notification.app_name
            ),
            r#type: ExceptionType::InstallNotificationRequest,
            addon: Some(serde_json::to_string(&notification).unwrap()),
        };

        get_global_pubsub()
            .await
            .publish(
                &EXCEPTION_OCCURRED_TOPIC,
                &serde_json::to_string(&exception_info).unwrap(),
            )
            .await;

        Ok(notification_id)
    }

    pub fn add_debug_string(language_address: String, debug_string: String, operation: String) {
        let entry = DebugStringEntry {
            language_address,
            debug_string,
            operation,
            timestamp: Utc::now(),
        };

        let mut debug_strings = DEBUG_STRINGS.lock().unwrap();
        debug_strings.push_back(entry);

        // Keep only the last MAX_DEBUG_STRINGS entries
        while debug_strings.len() > MAX_DEBUG_STRINGS {
            debug_strings.pop_front();
        }
    }

    pub fn get_debug_strings(language_address: Option<String>) -> Vec<DebugStringEntry> {
        let debug_strings = DEBUG_STRINGS.lock().unwrap();

        match language_address {
            Some(address) => debug_strings
                .iter()
                .filter(|entry| entry.language_address == address)
                .cloned()
                .collect(),
            None => debug_strings.iter().cloned().collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use std::sync::Mutex;

    // Use a test mutex to ensure tests run sequentially
    static TEST_MUTEX: Mutex<()> = Mutex::new(());

    #[test]
    fn test_add_and_get_debug_strings() {
        let _guard = TEST_MUTEX.lock().unwrap();

        // Clear any existing debug strings
        {
            let mut debug_strings = DEBUG_STRINGS.lock().unwrap();
            debug_strings.clear();
        }

        // Add some test debug strings
        RuntimeService::add_debug_string(
            "Qm123test1".to_string(),
            "digraph { 0 -> 1 }".to_string(),
            "merge".to_string(),
        );

        RuntimeService::add_debug_string(
            "Qm123test2".to_string(),
            "digraph { 1 -> 2 }".to_string(),
            "pull".to_string(),
        );

        RuntimeService::add_debug_string(
            "Qm123test1".to_string(),
            "digraph { 2 -> 3 }".to_string(),
            "commit".to_string(),
        );

        // Test getting all debug strings
        let all_strings = RuntimeService::get_debug_strings(None);
        assert_eq!(all_strings.len(), 3);

        // Test filtering by language address
        let filtered_strings = RuntimeService::get_debug_strings(Some("Qm123test1".to_string()));
        assert_eq!(filtered_strings.len(), 2);
        assert!(filtered_strings
            .iter()
            .all(|s| s.language_address == "Qm123test1"));

        let filtered_strings2 = RuntimeService::get_debug_strings(Some("Qm123test2".to_string()));
        assert_eq!(filtered_strings2.len(), 1);
        assert_eq!(filtered_strings2[0].language_address, "Qm123test2");
        assert_eq!(filtered_strings2[0].debug_string, "digraph { 1 -> 2 }");
        assert_eq!(filtered_strings2[0].operation, "pull");

        // Test non-existent language address
        let empty_strings = RuntimeService::get_debug_strings(Some("nonexistent".to_string()));
        assert_eq!(empty_strings.len(), 0);
    }

    #[test]
    fn test_debug_strings_max_limit() {
        let _guard = TEST_MUTEX.lock().unwrap();

        // Clear any existing debug strings
        {
            let mut debug_strings = DEBUG_STRINGS.lock().unwrap();
            debug_strings.clear();
        }

        // Add more than MAX_DEBUG_STRINGS entries
        for i in 0..(MAX_DEBUG_STRINGS + 10) {
            RuntimeService::add_debug_string(
                format!("Qm{}", i),
                format!("digraph {{ {} -> {} }}", i, i + 1),
                "test".to_string(),
            );
        }

        // Should only keep the last MAX_DEBUG_STRINGS entries
        let all_strings = RuntimeService::get_debug_strings(None);
        assert_eq!(all_strings.len(), MAX_DEBUG_STRINGS);

        // The first entries should be the ones from index 10 onwards
        assert_eq!(all_strings[0].language_address, "Qm10");
        assert_eq!(
            all_strings[all_strings.len() - 1].language_address,
            format!("Qm{}", MAX_DEBUG_STRINGS + 9)
        );
    }

    #[test]
    fn test_debug_string_entry_fields() {
        let _guard = TEST_MUTEX.lock().unwrap();

        // Clear any existing debug strings
        {
            let mut debug_strings = DEBUG_STRINGS.lock().unwrap();
            debug_strings.clear();
        }

        let before_time = Utc::now();

        RuntimeService::add_debug_string(
            "Qm123test".to_string(),
            "digraph { test -> node }".to_string(),
            "merge_operation".to_string(),
        );

        let after_time = Utc::now();

        let strings = RuntimeService::get_debug_strings(None);
        assert_eq!(strings.len(), 1);

        let entry = &strings[0];
        assert_eq!(entry.language_address, "Qm123test");
        assert_eq!(entry.debug_string, "digraph { test -> node }");
        assert_eq!(entry.operation, "merge_operation");

        // Check timestamp is within reasonable bounds
        assert!(entry.timestamp >= before_time);
        assert!(entry.timestamp <= after_time);
    }
}

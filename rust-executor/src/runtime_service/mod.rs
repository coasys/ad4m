use std::{ collections::HashMap, env, fs::{self, File}, io, path::Path, sync::Mutex};
use std::io::{Read};
pub(crate) mod runtime_service_extension;

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

use crate::{agent::did, db::Ad4mDb, graphql::graphql_types::SentMessage};

lazy_static! {
    static ref MAINNETSEED_FILE: Mutex<String> = Mutex::new(format!("{}/mainnet_seed.seed", env::var("APPS_DATA_PATH").unwrap_or_else(|_| "".to_string())));
}

fn get_mainnet_seed_file_path() -> String {
    MAINNETSEED_FILE.lock().unwrap().clone()
}

fn load_mainnet_seed_from_file() -> io::Result<BootstrapSeed> {
    let file_path = get_mainnet_seed_file_path();
    let mut file = File::open(&file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let seed: BootstrapSeed = serde_json::from_str(&contents)?;
    Ok(seed)
}

pub fn get_trusted_agents() -> Vec<String> {
    let seed = load_mainnet_seed_from_file().unwrap();
    let mut trusted_agents: Vec<String> = seed.trusted_agents.clone();
    let mut stored_agents = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .get_all_trusted_agents()
        .map_err(|e| e.to_string())
        .unwrap_or(vec![]);
    trusted_agents.push(did());
    trusted_agents.append(&mut stored_agents);
    trusted_agents.sort();
    trusted_agents.dedup();
    trusted_agents
}

pub fn add_trusted_agent(new_agents: Vec<String>) {
    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .add_trusted_agents(new_agents)
        .map_err(|e| e.to_string())
        .unwrap_or(());
}

pub fn remove_trusted_agent(agents_to_remove: Vec<String>) {
    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .remove_trusted_agents(agents_to_remove)
        .map_err(|e| e.to_string())
        .unwrap_or(());
}

pub fn get_know_link_languages() -> Vec<String> {
    let seed = load_mainnet_seed_from_file().unwrap();
    let mut languages: Vec<String> = seed.known_link_languages.clone();
    let mut stored_languages = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .get_all_known_link_languages()
        .map_err(|e| e.to_string())
        .unwrap_or(vec![]);
    languages.append(&mut stored_languages);
    languages.sort();
    languages.dedup();
    languages
}

pub fn add_know_link_language(language: Vec<String>) {
    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .add_known_link_languages(language)
        .map_err(|e| e.to_string())
        .unwrap_or(());
}

pub fn remove_know_link_language(language_to_remove: Vec<String>) {
    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .remove_known_link_languages(language_to_remove)
        .map_err(|e| e.to_string())
        .unwrap_or(());
}

pub fn get_friends() -> Vec<String> {
    let friends = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .get_all_friends()
        .map_err(|e| e.to_string())
        .unwrap_or(vec![]);

    friends
}

pub fn add_friend(friend: Vec<String>) {
    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .add_friends(friend)
        .map_err(|e| e.to_string())
        .unwrap_or(());
}

pub fn remove_friend(friend_to_remove: Vec<String>) {
    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .remove_friends(friend_to_remove)
        .map_err(|e| e.to_string())
        .unwrap_or(());
}

pub fn get_outbox() -> Vec<SentMessage> {
    let outbox = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .get_all_from_outbox()
        .map_err(|e| e.to_string())
        .unwrap_or(vec![]);

    outbox
}

pub fn add_message_to_outbox(message: SentMessage) {
    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .add_to_outbox(&message.message, message.recipient)
        .map_err(|e| e.to_string())
        .unwrap_or(());
}

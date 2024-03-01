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

use crate::{agent::did, graphql::graphql_types::SentMessage};

lazy_static! {
    static ref TRUSTED_AGENTS_FILE: Mutex<String> = Mutex::new(format!("{}/trusted_agents.json", env::var("APPS_DATA_PATH").unwrap_or_else(|_| "".to_string())));
    static ref KNOW_LINK_LANGUAGES_FILE: Mutex<String> = Mutex::new(format!("{}/know_link_languages.json", env::var("APPS_DATA_PATH").unwrap_or_else(|_| "".to_string())));
    static ref FRIENDS_FILE: Mutex<String> = Mutex::new(format!("{}/friends.json", env::var("APPS_DATA_PATH").unwrap_or_else(|_| "".to_string())));
    static ref OUTBOX_FILE: Mutex<String> = Mutex::new(format!("{}/outbox.json", env::var("APPS_DATA_PATH").unwrap_or_else(|_| "".to_string())));
    static ref MAINNETSEED_FILE: Mutex<String> = Mutex::new(format!("{}/mainnet_seed.seed", env::var("APPS_DATA_PATH").unwrap_or_else(|_| "".to_string())));
}

fn get_mainnet_seed_file_path() -> String {
    MAINNETSEED_FILE.lock().unwrap().clone()
}

fn get_trusted_agents_file_path() -> String {
    TRUSTED_AGENTS_FILE.lock().unwrap().clone()
}

fn get_know_link_languages_file_path() -> String {
    KNOW_LINK_LANGUAGES_FILE.lock().unwrap().clone()
}

fn get_friends_file_path() -> String {
    FRIENDS_FILE.lock().unwrap().clone()
}

fn get_outbox_file_path() -> String {
    OUTBOX_FILE.lock().unwrap().clone()
}

fn load_mainnet_seed_from_file() -> io::Result<BootstrapSeed> {
    let file_path = get_mainnet_seed_file_path();
    let mut file = File::open(&file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let seed: BootstrapSeed = serde_json::from_str(&contents)?;
    Ok(seed)
}

fn load_agents_from_file() -> io::Result<Vec<String>> {
    let file_path = get_trusted_agents_file_path();
    let mut file = File::open(&file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let agents: Vec<String> = serde_json::from_str(&contents)?;
    Ok(agents)
}

fn load_know_link_languages_from_file() -> io::Result<Vec<String>> {
    let file_path = get_know_link_languages_file_path();
    let mut file = File::open(&file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let agents: Vec<String> = serde_json::from_str(&contents)?;
    Ok(agents)
}

fn load_friends_from_file() -> io::Result<Vec<String>> {
    let file_path = get_friends_file_path();
    let mut file = File::open(&file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let friends: Vec<String> = serde_json::from_str(&contents)?;
    Ok(friends)
}

fn load_outbox_from_file() -> io::Result<Vec<SentMessage>> {
    let file_path = get_outbox_file_path();
    let mut file = File::open(&file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let outbox: Vec<SentMessage> = serde_json::from_str(&contents)?;
    Ok(outbox)
}

fn persist_agents_to_file(agents: &Vec<String>) -> io::Result<()> {
    let file_path = get_trusted_agents_file_path();
    let serialized_agents = serde_json::to_string(agents)?;
    fs::write(file_path, serialized_agents)?;
    Ok(())
}

fn persist_know_link_language_to_file(languages: &Vec<String>) -> io::Result<()> {
    let file_path = get_know_link_languages_file_path();
    let serialized_languages = serde_json::to_string(languages)?;
    fs::write(file_path, serialized_languages)?;
    Ok(())
}

fn persist_friends_to_file(friends: &Vec<String>) -> io::Result<()> {
    let file_path = get_friends_file_path();
    let serialized_friends = serde_json::to_string(friends)?;
    fs::write(file_path, serialized_friends)?;
    Ok(())
}

fn persist_outbox_to_file(message: &Vec<SentMessage>) -> io::Result<()> {
    let file_path = get_outbox_file_path();
    let serialized_message = serde_json::to_string(message)?;
    fs::write(file_path, serialized_message)?;
    Ok(())
}

lazy_static! {
    static ref TRUSTED_AGENTS: Mutex<Vec<String>> = {
        let agents = if Path::new(&get_trusted_agents_file_path()).exists() {
            load_agents_from_file().unwrap_or_else(|_| Vec::new())
        } else {
            Vec::new()
        };
        Mutex::new(agents)
    };
    static ref KNOW_LINK_LANGUAGES: Mutex<Vec<String>> = {
        let link_lanuages = if Path::new(&get_know_link_languages_file_path()).exists() {
            load_know_link_languages_from_file().unwrap_or_else(|_| Vec::new())
        } else {
            Vec::new()
        };
        Mutex::new(link_lanuages)
    };
    static ref FRIENDS: Mutex<Vec<String>> = {
        let friends = if Path::new(&get_friends_file_path()).exists() {
            load_friends_from_file().unwrap_or_else(|_| Vec::new())
        } else {
            Vec::new()
        };
        Mutex::new(friends)
    };
    static ref OUTBOX: Mutex<Vec<SentMessage>> = {
        let outbox = if Path::new(&get_outbox_file_path()).exists() {
            load_outbox_from_file().unwrap_or_else(|_| Vec::new())
        } else {
            Vec::new()
        };
        Mutex::new(outbox)
    };
}

pub fn get_trusted_agents() -> Vec<String> {
    let seed = load_mainnet_seed_from_file().unwrap();
    let mut trusted_agents: Vec<String> = seed.trusted_agents.clone();
    trusted_agents.push(did());
    trusted_agents.append(TRUSTED_AGENTS.lock().unwrap().clone().as_mut());
    trusted_agents.sort();
    trusted_agents.dedup();
    trusted_agents
}

pub fn add_trusted_agent(new_agents: Vec<String>) {
    let mut agents = TRUSTED_AGENTS.lock().unwrap();
    agents.append(new_agents.clone().as_mut());
    persist_agents_to_file(&agents).unwrap();
}

pub fn remove_trusted_agent(agents_to_remove: Vec<String>) {
    let mut agents = TRUSTED_AGENTS.lock().unwrap();
    for agent in agents_to_remove {
        agents.retain(|a| a != &agent);
    }
    persist_agents_to_file(&agents).unwrap();
}

pub fn get_know_link_languages() -> Vec<String> {
    let seed = load_mainnet_seed_from_file().unwrap();
    let mut languages: Vec<String> = seed.known_link_languages.clone();
    languages.append(KNOW_LINK_LANGUAGES.lock().unwrap().clone().as_mut());
    languages.sort();
    languages.dedup();
    languages
}

pub fn add_know_link_language(language: Vec<String>) {
    let mut languages = KNOW_LINK_LANGUAGES.lock().unwrap();
    languages.append(language.clone().as_mut());
    persist_know_link_language_to_file(&languages).unwrap();
}

pub fn remove_know_link_language(language_to_remove: Vec<String>) {
    let mut languages = KNOW_LINK_LANGUAGES.lock().unwrap();
    for language in language_to_remove {
        languages.retain(|a| a != &language);
    }
    persist_know_link_language_to_file(&languages).unwrap();
}

pub fn get_friends() -> Vec<String> {
    let mut friends = FRIENDS.lock().unwrap().clone();
    friends.sort();
    friends.dedup();
    friends
}

pub fn add_friend(friend: Vec<String>) {
    let mut friends = FRIENDS.lock().unwrap();
    friends.append(friend.clone().as_mut());
    persist_friends_to_file(&friends).unwrap();
}

pub fn remove_friend(friend_to_remove: Vec<String>) {
    let mut friends = FRIENDS.lock().unwrap();
    for friend in friend_to_remove {
        friends.retain(|a| a != &friend);
    }
    persist_friends_to_file(&friends).unwrap();
}

pub fn get_outbox() -> Vec<SentMessage> {
    OUTBOX.lock().unwrap().clone()
}

pub fn add_message_to_outbox(message: SentMessage) {
    let mut outbox = OUTBOX.lock().unwrap();
    outbox.push(message);
    persist_outbox_to_file(&outbox).unwrap();
}   

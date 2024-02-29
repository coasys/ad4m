use std::{ env, fs::{self, File}, io, path::Path, sync::Mutex};
use ad4m_client::types::PerspectiveExpression;
use serde::{Deserialize, Serialize};
use std::io::{Read};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Message {
    recipient: String,
    message: PerspectiveExpression,
}

lazy_static! {
    static ref TRUSTED_AGENTS_FILE: Mutex<String> =
        Mutex::new(env::var("APPS_DATA_FILE").unwrap_or_else(|_| "trusted_agents.json".to_string()));
    static ref KNOW_LINK_LANGUAGES_FILE: Mutex<String> =
        Mutex::new(env::var("APPS_DATA_FILE").unwrap_or_else(|_| "know_link_languages.json".to_string()));
    static ref FRIENDS_FILE: Mutex<String> =
        Mutex::new(env::var("APPS_DATA_FILE").unwrap_or_else(|_| "friends.json".to_string()));
    static ref OUTBOX_FILE: Mutex<String> =
        Mutex::new(env::var("APPS_DATA_FILE").unwrap_or_else(|_| "outbox.json".to_string()));
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

fn load_outbox_from_file() -> io::Result<Vec<Message>> {
    let file_path = get_outbox_file_path();
    let mut file = File::open(&file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let outbox: Vec<Message> = serde_json::from_str(&contents)?;
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

fn persist_outbox_to_file(message: &Vec<Message>) -> io::Result<()> {
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
    static ref OUTBOX: Mutex<Vec<Message>> = {
        let outbox = if Path::new(&get_outbox_file_path()).exists() {
            load_outbox_from_file().unwrap_or_else(|_| Vec::new())
        } else {
            Vec::new()
        };
        Mutex::new(outbox)
    };
}

pub fn get_trusted_agents() -> Vec<String> {
    TRUSTED_AGENTS.lock().unwrap().clone()
}

pub fn add_trusted_agent(agent: String) {
    let mut agents = TRUSTED_AGENTS.lock().unwrap();
    agents.push(agent);
    persist_agents_to_file(&agents).unwrap();
}

pub fn remove_trusted_agent(agent: String) {
    let mut agents = TRUSTED_AGENTS.lock().unwrap();
    agents.retain(|a| a != &agent);
    persist_agents_to_file(&agents).unwrap();
}

pub fn get_know_link_languages() -> Vec<String> {
    KNOW_LINK_LANGUAGES.lock().unwrap().clone()
}

pub fn add_know_link_language(language: String) {
    let mut languages = KNOW_LINK_LANGUAGES.lock().unwrap();
    languages.push(language);
    persist_know_link_language_to_file(&languages).unwrap();
}

pub fn remove_know_link_language(language: String) {
    let mut languages = KNOW_LINK_LANGUAGES.lock().unwrap();
    languages.retain(|l| l != &language);
    persist_know_link_language_to_file(&languages).unwrap();
}

pub fn get_friends() -> Vec<String> {
    FRIENDS.lock().unwrap().clone()
}

pub fn add_friend(friend: String) {
    let mut friends = FRIENDS.lock().unwrap();
    friends.push(friend);
    persist_friends_to_file(&friends).unwrap();
}

pub fn remove_friend(friend: String) {
    let mut friends = FRIENDS.lock().unwrap();
    friends.retain(|f| f != &friend);
    persist_friends_to_file(&friends).unwrap();
}

pub fn get_outbox() -> Vec<Message> {
    OUTBOX.lock().unwrap().clone()
}

pub fn add_message_to_outbox(message: Message) {
    let mut outbox = OUTBOX.lock().unwrap();
    outbox.push(message);
    persist_outbox_to_file(&outbox).unwrap();
}   

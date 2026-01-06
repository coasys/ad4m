use crate::encryption::{decrypt_password, encrypt_password};
use dirs::home_dir;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::fmt;
use std::fs::{create_dir_all, File, OpenOptions};
use std::io::prelude::*;
use std::path::PathBuf;

pub static FILE_NAME: &str = "launcher-state.json";

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct AgentConfigDir {
    pub name: String,
    pub path: PathBuf,
    pub bootstrap: Option<PathBuf>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TlsConfig {
    pub enabled: bool,
    pub cert_file_path: String,
    pub key_file_path: String,
    pub tls_port: Option<u16>, // Port for HTTPS/WSS server (defaults to main_port + 1)
}

#[derive(Clone)]
pub struct SmtpConfig {
    pub host: String,
    pub port: u16,
    pub username: String,
    pub password: String, // Plain password in memory, encrypted on disk
    pub from_address: String,
    #[allow(dead_code)]
    password_encrypted: Option<String>, // Internal: encrypted password for serialization
    #[allow(dead_code)]
    password_migrated: bool, // True if password was loaded as plaintext (migration case)
}

impl fmt::Debug for SmtpConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SmtpConfig")
            .field("host", &self.host)
            .field("port", &self.port)
            .field("username", &self.username)
            .field("password", &"<redacted>")
            .field("from_address", &self.from_address)
            .field("password_encrypted", &self.password_encrypted.as_ref().map(|_| "<redacted>"))
            .field("password_migrated", &self.password_migrated)
            .finish()
    }
}

impl SmtpConfig {
    /// Create a new SmtpConfig with plain password
    pub fn new(
        host: String,
        port: u16,
        username: String,
        password: String,
        from_address: String,
    ) -> Self {
        SmtpConfig {
            host,
            port,
            username,
            password,
            from_address,
            password_encrypted: None,
            password_migrated: false,
        }
    }

    /// Get the plain password
    pub fn get_password(&self) -> &str {
        &self.password
    }

    /// Set the plain password
    pub fn set_password(&mut self, password: String) {
        self.password = password;
        self.password_encrypted = None; // Clear encrypted version to force re-encryption
        self.password_migrated = false; // Reset migration flag when password is set
    }
}

impl Serialize for SmtpConfig {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::SerializeStruct;

        // Always encrypt password on save to ensure it's encrypted
        let encrypted = encrypt_password(&self.password)
            .map_err(|e| serde::ser::Error::custom(format!("Failed to encrypt password: {}", e)))?;

        let mut state = serializer.serialize_struct("SmtpConfig", 5)?;
        state.serialize_field("host", &self.host)?;
        state.serialize_field("port", &self.port)?;
        state.serialize_field("username", &self.username)?;
        state.serialize_field("password", &encrypted)?;
        state.serialize_field("from_address", &self.from_address)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for SmtpConfig {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct SmtpConfigHelper {
            host: String,
            port: u16,
            username: String,
            password: String, // This will be the encrypted password
            from_address: String,
        }

        let helper = SmtpConfigHelper::deserialize(deserializer)?;

        // Try to decrypt the password
        // If decryption fails, assume it's plain text (backwards compatibility)
        let encrypted_password = helper.password.clone();
        let (plain_password, password_migrated) = match decrypt_password(&helper.password) {
            Ok(decrypted) => (decrypted, false),
            Err(e) => {
                // If decryption fails, assume it's plain text (for backwards compatibility)
                // This allows migration from unencrypted to encrypted storage
                log::warn!(
                    "SMTP password decryption failed for user '{}' on host '{}': {}. \
                     Treating as plaintext for backwards compatibility migration.",
                    helper.username,
                    helper.host,
                    e
                );
                (helper.password, true)
            }
        };

        Ok(SmtpConfig {
            host: helper.host,
            port: helper.port,
            username: helper.username,
            password: plain_password,
            from_address: helper.from_address,
            password_encrypted: if password_migrated {
                None // Don't store encrypted version if it was plaintext
            } else {
                Some(encrypted_password) // Store encrypted version
            },
            password_migrated,
        })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct MultiUserConfig {
    pub enabled: bool,
    pub smtp_config: Option<SmtpConfig>,
    pub tls_config: Option<TlsConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct LauncherState {
    pub agent_list: Vec<AgentConfigDir>,
    pub selected_agent: Option<AgentConfigDir>,
    pub log_config: Option<HashMap<String, String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tls_config: Option<TlsConfig>, // Deprecated - use multi_user_config.tls_config instead
    pub multi_user_config: Option<MultiUserConfig>,
}

fn file_path() -> PathBuf {
    let path = home_dir().expect("Could not get home dir").join(".ad4m");
    // Create directories if they don't exist
    create_dir_all(&path).expect("Failed to create directory");
    path.join(FILE_NAME)
}

impl LauncherState {
    pub fn save(&mut self) -> std::io::Result<()> {
        let mut file = File::create(file_path())?;
        let data = serde_json::to_string(&self).unwrap();
        file.write_all(data.as_bytes())?;
        Ok(())
    }

    pub fn load() -> std::io::Result<LauncherState> {
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(file_path())?;
        let mut data = String::new();
        file.read_to_string(&mut data)?;

        let state = match serde_json::from_str(&data) {
            Ok(state) => state,
            Err(_) => {
                let agent = AgentConfigDir {
                    name: "Main Net".to_string(),
                    path: home_dir().expect("Could not get home dir").join(".ad4m"),
                    bootstrap: None,
                };

                LauncherState {
                    agent_list: vec![{ agent.clone() }],
                    selected_agent: Some(agent),
                    log_config: None,
                    tls_config: None,
                    multi_user_config: None,
                }
            }
        };

        Ok(state)
    }

    pub fn add_agent(&mut self, agent: AgentConfigDir) {
        if !self.is_agent_taken(&agent.name, &agent.path) {
            self.agent_list.push(agent);
        }
    }

    pub fn remove_agent(&mut self, agent: AgentConfigDir) {
        self.agent_list
            .retain(|a| a.name != agent.name && a.path != agent.path);
    }

    pub fn is_agent_taken(&self, new_name: &str, new_path: &PathBuf) -> bool {
        self.agent_list
            .iter()
            .any(|agent| agent.name == new_name && (&agent.path == new_path))
    }
}

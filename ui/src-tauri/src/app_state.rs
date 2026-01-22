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
    pub enabled: bool,
    pub host: String,
    pub port: u16,
    pub username: String,
    pub password: String, // Plain password in memory, encrypted on disk
    pub from_address: String,
}

impl fmt::Debug for SmtpConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SmtpConfig")
            .field("enabled", &self.enabled)
            .field("host", &self.host)
            .field("port", &self.port)
            .field("username", &self.username)
            .field("password", &"<redacted>")
            .field("from_address", &self.from_address)
            .finish()
    }
}

impl SmtpConfig {
    /// Create a new SmtpConfig with plain password
    pub fn new(
        enabled: bool,
        host: String,
        port: u16,
        username: String,
        password: String,
        from_address: String,
    ) -> Self {
        SmtpConfig {
            enabled,
            host,
            port,
            username,
            password,
            from_address,
        }
    }

    /// Get the plain password
    #[allow(dead_code)]
    pub fn get_password(&self) -> &str {
        &self.password
    }

    /// Set the plain password
    #[allow(dead_code)]
    pub fn set_password(&mut self, password: String) {
        self.password = password;
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

        let mut state = serializer.serialize_struct("SmtpConfig", 6)?;
        state.serialize_field("enabled", &self.enabled)?;
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
            enabled: Option<bool>, // Optional for backward compatibility
            host: String,
            port: u16,
            username: String,
            password: String, // This will be the encrypted password
            from_address: String,
        }

        let helper = SmtpConfigHelper::deserialize(deserializer)?;

        // Try to decrypt the password.
        // If decryption fails, assume it's plain text (backwards compatibility).
        let plain_password = match decrypt_password(&helper.password) {
            Ok(decrypted) => decrypted,
            Err(e) => {
                // If decryption fails, assume it's plain text (for backwards compatibility)
                // This allows migration from unencrypted to encrypted storage.
                log::warn!(
                    "SMTP password decryption failed for user '{}' on host '{}': {}. \
                     Treating as plaintext for backwards compatibility migration.",
                    helper.username,
                    helper.host,
                    e
                );
                helper.password
            }
        };

        Ok(SmtpConfig {
            enabled: helper.enabled.unwrap_or(true), // Default to enabled for existing configs
            host: helper.host,
            port: helper.port,
            username: helper.username,
            password: plain_password,
            from_address: helper.from_address,
        })
    }
}

/// DTO for SMTP config sent to frontend (plain password, no encryption)
/// This is separate from SmtpConfig to avoid encrypting passwords when sending to UI
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SmtpConfigDto {
    pub enabled: bool,
    pub host: String,
    pub port: u16,
    pub username: String,
    pub password: String, // Plain password for UI display/editing
    pub from_address: String,
}

impl From<&SmtpConfig> for SmtpConfigDto {
    fn from(config: &SmtpConfig) -> Self {
        SmtpConfigDto {
            enabled: config.enabled,
            host: config.host.clone(),
            port: config.port,
            username: config.username.clone(),
            password: config.password.clone(), // Plain password
            from_address: config.from_address.clone(),
        }
    }
}

impl From<SmtpConfigDto> for SmtpConfig {
    fn from(dto: SmtpConfigDto) -> Self {
        SmtpConfig::new(
            dto.enabled,
            dto.host,
            dto.port,
            dto.username,
            dto.password, // Plain password will be encrypted on save
            dto.from_address,
        )
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

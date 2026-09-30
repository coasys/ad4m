use crate::encryption::{decrypt_password, encrypt_password};
use dirs::home_dir;
use rust_executor::config_file::{
    ExecutorConfigFile, MultiUserSettings, SmtpSettings, TlsSettings,
};
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

/// The shared shape of `tls_config`; the TLS port defaults to main port + 1.
pub type TlsConfig = TlsSettings;

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

    /// The shared SMTP settings; the password travels separately.
    pub fn settings(&self) -> SmtpSettings {
        SmtpSettings {
            enabled: self.enabled,
            host: self.host.clone(),
            port: self.port,
            username: self.username.clone(),
            from_address: self.from_address.clone(),
            password_file: None,
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

#[derive(Clone)]
pub struct HostRegistration {
    pub index_url: String,
    pub host_id: String,
    pub auth_token: String, // Plain token in memory, encrypted on disk
    pub email: String,
}

impl fmt::Debug for HostRegistration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HostRegistration")
            .field("index_url", &self.index_url)
            .field("host_id", &self.host_id)
            .field("auth_token", &"<redacted>")
            .field("email", &self.email)
            .finish()
    }
}

impl Serialize for HostRegistration {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::SerializeStruct;

        // Note: auth_token is NOT encrypted because JWTs are already signed by the server.
        // The signing key is server-side, so encryption provides no additional security.
        // Encrypting breaks the token format and causes deserialization issues.

        let mut state = serializer.serialize_struct("HostRegistration", 4)?;
        state.serialize_field("index_url", &self.index_url)?;
        state.serialize_field("host_id", &self.host_id)?;
        state.serialize_field("auth_token", &self.auth_token)?;
        state.serialize_field("email", &self.email)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for HostRegistration {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct HostRegistrationHelper {
            index_url: String,
            host_id: String,
            auth_token: String,
            email: String,
        }

        let helper = HostRegistrationHelper::deserialize(deserializer)?;

        // auth_token is stored in plain text (it's a JWT, not a password)
        // No decryption needed

        Ok(HostRegistration {
            index_url: helper.index_url,
            host_id: helper.host_id,
            auth_token: helper.auth_token,
            email: helper.email,
        })
    }
}

/// The shared `multi_user_config` shape, with the launcher's SMTP config
/// (password keyring-encrypted on disk) in place of the config file's
/// `password_file`.
pub type MultiUserConfig = MultiUserSettings<SmtpConfig>;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct LauncherState {
    pub agent_list: Vec<AgentConfigDir>,
    pub selected_agent: Option<AgentConfigDir>,
    pub log_config: Option<HashMap<String, String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tls_config: Option<TlsConfig>, // Deprecated - use multi_user_config.tls_config instead
    pub multi_user_config: Option<MultiUserConfig>,
    #[serde(default)]
    pub host_registration: Option<HostRegistration>,
    #[serde(default)]
    pub mcp_enabled: Option<bool>,
    #[serde(default)]
    pub mcp_port: Option<u16>,
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
                    host_registration: None,
                    mcp_enabled: None,
                    mcp_port: None,
                }
            }
        };

        Ok(state)
    }

    /// The executor settings this state describes, for the selected agent's
    /// `app_path` on `port`, plus the plain SMTP password. The deprecated
    /// top-level `tls_config` counts only when there is no
    /// `multi_user_config`. The plain port binds to loopback unless TLS is on
    /// (with TLS the executor keeps it on loopback anyway).
    pub fn executor_config(
        &self,
        app_path: String,
        port: u16,
    ) -> (ExecutorConfigFile, Option<String>) {
        let (multi_user_config, smtp_password) = match &self.multi_user_config {
            Some(multi_user) => (
                MultiUserSettings {
                    enabled: multi_user.enabled,
                    smtp_config: multi_user.smtp_config.as_ref().map(SmtpConfig::settings),
                    tls_config: multi_user.tls_config.clone(),
                },
                multi_user.smtp_config.as_ref().map(|s| s.password.clone()),
            ),
            None => (
                MultiUserSettings {
                    enabled: false,
                    smtp_config: None,
                    tls_config: self.tls_config.clone(),
                },
                None,
            ),
        };
        let tls_enabled = multi_user_config
            .tls_config
            .as_ref()
            .is_some_and(|tls| tls.enabled);
        let file = ExecutorConfigFile {
            app_data_path: Some(app_path),
            port: Some(port),
            localhost: Some(!tls_enabled),
            run_dapp_server: Some(true),
            multi_user_config: Some(multi_user_config),
            log_config: self.log_config.clone(),
            mcp_enabled: self.mcp_enabled,
            mcp_port: self.mcp_port,
            ..Default::default()
        };
        (file, smtp_password)
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

#[cfg(test)]
mod tests {
    use super::*;
    use rust_executor::config_file::ExecutorSecrets;

    /// The shape `launcher-state.json` has on a running team node today:
    /// `smtp_config` from before its `enabled` key, the deprecated top-level
    /// `tls_config` beside `multi_user_config.tls_config`, no
    /// `host_registration`. Values are made up; `__ENCRYPTED__` is replaced
    /// by the test key's ciphertext of "smtp-secret".
    const LAUNCHER_STATE: &str = r#"{
        "agent_list": [
            {"name": "Main Net", "path": "/home/op/.ad4m", "bootstrap": null},
            {"name": "Test", "path": "/home/op/.ad4m-test", "bootstrap": null}
        ],
        "selected_agent": {"name": "Main Net", "path": "/home/op/.ad4m", "bootstrap": null},
        "log_config": {
            "rust_executor": "info", "wasmer_compiler_cranelift": "warn",
            "warp::server": "info", "holochain": "warn"
        },
        "tls_config": {
            "enabled": true, "cert_file_path": "/etc/ssl/legacy.pem",
            "key_file_path": "/etc/ssl/legacy.key", "tls_port": 12100
        },
        "multi_user_config": {
            "enabled": true,
            "smtp_config": {
                "host": "smtp.example.org", "port": 465, "username": "ad4m@example.org",
                "password": "__ENCRYPTED__", "from_address": "ad4m@example.org"
            },
            "tls_config": {
                "enabled": true, "cert_file_path": "/etc/ssl/node.pem",
                "key_file_path": "/etc/ssl/node.key", "tls_port": 12100
            }
        },
        "mcp_enabled": true,
        "mcp_port": 3001
    }"#;

    fn fixture() -> String {
        LAUNCHER_STATE.replace(
            "__ENCRYPTED__",
            &encrypt_password("smtp-secret").expect("test key encrypts"),
        )
    }

    #[test]
    fn todays_launcher_state_still_loads_and_round_trips() {
        let state: LauncherState = serde_json::from_str(&fixture()).expect("fixture parses");
        let multi_user = state.multi_user_config.as_ref().unwrap();
        let smtp = multi_user.smtp_config.as_ref().unwrap();
        assert!(multi_user.enabled);
        assert!(smtp.enabled, "a missing smtp enabled key means enabled");
        assert_eq!(smtp.password, "smtp-secret");
        assert_eq!(smtp.host, "smtp.example.org");
        let tls = multi_user.tls_config.as_ref().unwrap();
        assert_eq!(
            (tls.cert_file_path.as_str(), tls.tls_port),
            ("/etc/ssl/node.pem", Some(12100))
        );
        assert_eq!(
            state.tls_config.as_ref().unwrap().cert_file_path,
            "/etc/ssl/legacy.pem"
        );
        assert_eq!(
            (state.mcp_enabled, state.mcp_port),
            (Some(true), Some(3001))
        );
        assert_eq!(state.agent_list.len(), 2);

        // Written back, every key the file had keeps its value (the password
        // is re-encrypted with a fresh nonce, so it is compared decrypted).
        let written: serde_json::Value =
            serde_json::from_str(&serde_json::to_string(&state).unwrap()).unwrap();
        let mut original: serde_json::Value = serde_json::from_str(&fixture()).unwrap();
        let written_password = written["multi_user_config"]["smtp_config"]["password"]
            .as_str()
            .unwrap();
        assert_eq!(decrypt_password(written_password).unwrap(), "smtp-secret");
        original["multi_user_config"]["smtp_config"]["password"] = written_password.into();
        for (key, value) in original.as_object().unwrap() {
            if key == "multi_user_config" {
                for (inner, value) in value.as_object().unwrap() {
                    if inner == "smtp_config" {
                        for (field, value) in value.as_object().unwrap() {
                            assert_eq!(&written[key][inner][field], value, "{key}.{inner}.{field}");
                        }
                    } else {
                        assert_eq!(&written[key][inner], value, "{key}.{inner}");
                    }
                }
            } else {
                assert_eq!(&written[key], value, "{key}");
            }
        }
        let reloaded: LauncherState = serde_json::from_value(written).unwrap();
        assert_eq!(
            reloaded
                .multi_user_config
                .unwrap()
                .smtp_config
                .unwrap()
                .password,
            "smtp-secret"
        );
    }

    #[test]
    fn launcher_state_maps_to_the_executor_config_the_launcher_starts() {
        let state: LauncherState = serde_json::from_str(&fixture()).unwrap();
        let (file, smtp_password) = state.executor_config("/home/op/.ad4m".into(), 12005);
        let config = file
            .to_ad4m_config(&ExecutorSecrets {
                admin_credential: Some("uuid".into()),
                smtp_password,
                unlock_passphrase: None,
            })
            .unwrap();
        let tls = config.tls.as_ref().unwrap();
        assert_eq!(
            (tls.cert_file_path.as_str(), tls.tls_port),
            ("/etc/ssl/node.pem", 12100),
            "multi_user_config.tls_config wins over the deprecated key"
        );
        let smtp = config.smtp_config.as_ref().unwrap();
        assert_eq!(
            (smtp.host.as_str(), smtp.password.as_str()),
            ("smtp.example.org", "smtp-secret")
        );
        assert_eq!(config.enable_multi_user, Some(true));
        assert_eq!(config.localhost, Some(false));
        assert_eq!(config.port, Some(12005));
        assert_eq!(config.app_data_path.as_deref(), Some("/home/op/.ad4m"));
        assert_eq!(
            (config.enable_mcp, config.mcp_port),
            (Some(true), Some(3001))
        );
        assert_eq!(config.admin_credential.as_deref(), Some("uuid"));
        assert_eq!(config.run_dapp_server, Some(true));
        assert_eq!(config.auto_permit_cap_requests, None);

        // Without multi_user_config the deprecated tls_config applies, and
        // its port defaults to the main port + 1.
        let mut legacy = state.clone();
        legacy.multi_user_config = None;
        legacy.tls_config.as_mut().unwrap().tls_port = None;
        let (file, smtp_password) = legacy.executor_config("/home/op/.ad4m".into(), 12005);
        let config = file
            .to_ad4m_config(&ExecutorSecrets {
                smtp_password,
                ..Default::default()
            })
            .unwrap();
        let tls = config.tls.unwrap();
        assert_eq!(
            (tls.cert_file_path.as_str(), tls.tls_port),
            ("/etc/ssl/legacy.pem", 12006)
        );
        assert!(config.smtp_config.is_none());
        assert_ne!(config.enable_multi_user, Some(true));
    }
}

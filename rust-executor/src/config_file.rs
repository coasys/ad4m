//! The executor's config file: the settings a headless `ad4m-executor run
//! --config <path>` and the launcher (`~/.ad4m/launcher-state.json`) share,
//! with the launcher's key names, and the one mapping from them to
//! [`Ad4mConfig`].
//!
//! ```text
//!  config file (JSON) ─┐
//!  AD4M_* env, flags ──┼─ cli merges ─> ExecutorConfigFile ─┐
//!  launcher state ─────┘                                    ├─ to_ad4m_config ─> Ad4mConfig
//!  AD4M_*_FILE, AD4M_SMTP_PASSWORD ─> ExecutorSecrets ──────┘
//! ```
//!
//! The file holds no secret values, only paths to them: an inline
//! `admin_credential` or `smtp_config.password` fails the parse, and so does
//! any other key this module does not know. That check lives in
//! [`ExecutorConfigFile::parse`], not on the types: the launcher reads its
//! `launcher-state.json` through [`MultiUserSettings`] and [`TlsSettings`]
//! too, and must keep reading a file a later launcher added keys to.
//!
//! No string value may be empty or only whitespace. A secret must not be
//! empty, and a secret file must not be readable by
//! group or others (mode 0600 or 0400).

use crate::config::{Ad4mConfig, SmtpConfig, TlsConfig, DEFAULT_PORT};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};

/// Why a config file or a secret could not be used. The messages name files
/// and variables, never a secret's value.
#[derive(Debug)]
pub enum ConfigFileError {
    Read {
        path: PathBuf,
        error: std::io::Error,
    },
    Parse {
        path: PathBuf,
        error: serde_json::Error,
    },
    InlineSecret {
        path: PathBuf,
        message: &'static str,
    },
    SecretFileMode {
        path: PathBuf,
        mode: u32,
    },
    ConflictingSecretSources {
        value_var: String,
        file_var: String,
    },
    UnknownKey {
        path: PathBuf,
        key: String,
    },
    /// A string value that is empty or only whitespace.
    EmptyValue {
        path: PathBuf,
        key: String,
    },
    /// `source` names the variable or file.
    EmptySecret {
        source: String,
    },
    MissingSmtpPassword,
    TlsPortOverflow,
}

impl fmt::Display for ConfigFileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Read { path, error } => write!(f, "cannot read {}: {}", path.display(), error),
            Self::Parse { path, error } => {
                write!(f, "invalid config file {}: {}", path.display(), error)
            }
            Self::InlineSecret { path, message } => {
                write!(f, "invalid config file {}: {}", path.display(), message)
            }
            Self::SecretFileMode { path, mode } => write!(
                f,
                "secret file {} has mode {:04o}; it must not be readable by group or others \
                 (chmod 600)",
                path.display(),
                mode
            ),
            Self::ConflictingSecretSources {
                value_var,
                file_var,
            } => write!(f, "both {value_var} and {file_var} are set; set only one"),
            Self::UnknownKey { path, key } => {
                write!(
                    f,
                    "invalid config file {}: unknown key `{key}`",
                    path.display()
                )
            }
            Self::EmptyValue { path, key } => write!(
                f,
                "invalid config file {}: `{key}` is empty or only whitespace; give it a value",
                path.display()
            ),
            Self::EmptySecret { source } => write!(f, "the secret in {source} is empty"),
            Self::MissingSmtpPassword => write!(
                f,
                "smtp_config is enabled but no SMTP password is set: use \
                 smtp_config.password_file, AD4M_SMTP_PASSWORD_FILE or AD4M_SMTP_PASSWORD"
            ),
            Self::TlsPortOverflow => write!(
                f,
                "tls_config.tls_port is unset and port + 1 is above 65535; set tls_port"
            ),
        }
    }
}

impl std::error::Error for ConfigFileError {}

impl ConfigFileError {
    fn at(self, file: &Path) -> Self {
        match self {
            Self::Parse { error, .. } => Self::Parse {
                path: file.to_path_buf(),
                error,
            },
            Self::InlineSecret { message, .. } => Self::InlineSecret {
                path: file.to_path_buf(),
                message,
            },
            Self::UnknownKey { key, .. } => Self::UnknownKey {
                path: file.to_path_buf(),
                key,
            },
            Self::EmptyValue { key, .. } => Self::EmptyValue {
                path: file.to_path_buf(),
                key,
            },
            other => other,
        }
    }
}

/// `multi_user_config.tls_config`: the HTTPS/WSS listener.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct TlsSettings {
    pub enabled: bool,
    pub cert_file_path: String,
    pub key_file_path: String,
    /// Defaults to `port + 1`.
    pub tls_port: Option<u16>,
}

/// `multi_user_config.smtp_config`: the mail account verification emails
/// are sent from. The password is not a field: it comes from
/// `password_file` or the environment (see [`ExecutorSecrets::from_env`]).
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct SmtpSettings {
    pub enabled: bool,
    pub host: String,
    pub port: u16,
    pub username: String,
    pub from_address: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub password_file: Option<String>,
}

/// `multi_user_config`. Generic over the SMTP shape because the launcher
/// keeps its own keyring-encrypted password inside `smtp_config`; the
/// config file uses [`SmtpSettings`].
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct MultiUserSettings<Smtp = SmtpSettings> {
    pub enabled: bool,
    pub smtp_config: Option<Smtp>,
    pub tls_config: Option<TlsSettings>,
}

/// The config file. Every key is optional; an unset key keeps the
/// executor's default.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ExecutorConfigFile {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub app_data_path: Option<String>,
    /// The HTTP/WS RPC port (default 12000).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<u16>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hc_admin_port: Option<u16>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hc_app_port: Option<u16>,
    /// Bind the plain RPC port to 127.0.0.1 (default true). With TLS the
    /// plain port is loopback-only regardless.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub localhost: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub run_dapp_server: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auto_permit_cap_requests: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub multi_user_config: Option<MultiUserSettings>,
    /// Log level per crate, over the defaults. `RUST_LOG` overrides it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub log_config: Option<HashMap<String, String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp_port: Option<u16>,
}

/// Secret keys someone may paste into the config file, as JSON pointers,
/// with where the secret belongs instead. Checked before parsing so the
/// error says why, not just "unknown field".
const INLINE_SECRETS: &[(&str, &str)] = &[
    (
        "/admin_credential",
        "admin_credential is not allowed in the config file; set \
         AD4M_ADMIN_CREDENTIAL_FILE or AD4M_ADMIN_CREDENTIAL",
    ),
    (
        "/multi_user_config/smtp_config/password",
        "smtp_config.password is not allowed in the config file; put the password in a \
         file and set smtp_config.password_file or AD4M_SMTP_PASSWORD_FILE",
    ),
];

/// The secret values, resolved outside the config file. `Debug` shows only
/// which ones are set.
#[derive(Clone, Default)]
pub struct ExecutorSecrets {
    pub admin_credential: Option<String>,
    pub smtp_password: Option<String>,
    pub unlock_passphrase: Option<String>,
}

impl fmt::Debug for ExecutorSecrets {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let shown = |s: &Option<String>| s.as_ref().map(|_| REDACTED);
        f.debug_struct("ExecutorSecrets")
            .field("admin_credential", &shown(&self.admin_credential))
            .field("smtp_password", &shown(&self.smtp_password))
            .field("unlock_passphrase", &shown(&self.unlock_passphrase))
            .finish()
    }
}

pub const REDACTED: &str = "<redacted>";

impl ExecutorSecrets {
    /// Reads the secrets from the environment (`env` is `std::env::var`
    /// in production), falling back to the config file's
    /// `smtp_config.password_file`:
    ///
    /// | secret | value variable | file variable | config key |
    /// |---|---|---|---|
    /// | admin credential | `AD4M_ADMIN_CREDENTIAL` | `AD4M_ADMIN_CREDENTIAL_FILE` | |
    /// | SMTP password | `AD4M_SMTP_PASSWORD` | `AD4M_SMTP_PASSWORD_FILE` | `smtp_config.password_file` |
    /// | unlock passphrase | | `AD4M_UNLOCK_PASSPHRASE_FILE` | |
    ///
    /// Setting both variables of one secret is an error.
    pub fn from_env(
        env: impl Fn(&str) -> Option<String>,
        file: &ExecutorConfigFile,
    ) -> Result<Self, ConfigFileError> {
        let smtp_password_file = file
            .multi_user_config
            .as_ref()
            .and_then(|m| m.smtp_config.as_ref())
            .and_then(|s| s.password_file.as_deref());
        Ok(ExecutorSecrets {
            admin_credential: secret_from_env(
                &env,
                Some("AD4M_ADMIN_CREDENTIAL"),
                "AD4M_ADMIN_CREDENTIAL_FILE",
                None,
            )?,
            smtp_password: secret_from_env(
                &env,
                Some("AD4M_SMTP_PASSWORD"),
                "AD4M_SMTP_PASSWORD_FILE",
                smtp_password_file,
            )?,
            unlock_passphrase: secret_from_env(&env, None, "AD4M_UNLOCK_PASSPHRASE_FILE", None)?,
        })
    }
}

fn secret_from_env(
    env: &impl Fn(&str) -> Option<String>,
    value_var: Option<&str>,
    file_var: &str,
    config_file_path: Option<&str>,
) -> Result<Option<String>, ConfigFileError> {
    let value = value_var.and_then(env);
    let file = env(file_var);
    match (value, file) {
        (Some(_), Some(_)) => Err(ConfigFileError::ConflictingSecretSources {
            value_var: value_var.unwrap_or_default().to_string(),
            file_var: file_var.to_string(),
        }),
        (Some(value), None) if value.is_empty() => Err(ConfigFileError::EmptySecret {
            source: value_var.unwrap_or_default().to_string(),
        }),
        (Some(value), None) => Ok(Some(value)),
        (None, Some(path)) => read_secret_file(Path::new(&path)).map(Some),
        (None, None) => config_file_path
            .map(|path| read_secret_file(Path::new(path)))
            .transpose(),
    }
}

/// Reads a secret from `path`, which must not be readable by group or
/// others. One trailing newline (`\n` or `\r\n`) is dropped, so
/// `echo secret > file` works; what is left must not be empty.
pub fn read_secret_file(path: &Path) -> Result<String, ConfigFileError> {
    let read_error = |error| ConfigFileError::Read {
        path: path.to_path_buf(),
        error,
    };
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = std::fs::metadata(path)
            .map_err(read_error)?
            .permissions()
            .mode()
            & 0o777;
        if mode & 0o077 != 0 {
            return Err(ConfigFileError::SecretFileMode {
                path: path.to_path_buf(),
                mode,
            });
        }
    }
    let mut secret = std::fs::read_to_string(path).map_err(read_error)?;
    if secret.ends_with('\n') {
        secret.pop();
        if secret.ends_with('\r') {
            secret.pop();
        }
    }
    if secret.is_empty() {
        return Err(ConfigFileError::EmptySecret {
            source: path.display().to_string(),
        });
    }
    Ok(secret)
}

/// A key's path in the file, `multi_user_config.tls_config.tls_prot`.
fn key_path(path: &serde_ignored::Path) -> String {
    use serde_ignored::Path;
    let join = |parent: &Path, key: &dyn fmt::Display| match key_path(parent) {
        parent if parent.is_empty() => key.to_string(),
        parent => format!("{parent}.{key}"),
    };
    match path {
        Path::Root => String::new(),
        Path::Seq { parent, index } => join(parent, index),
        Path::Map { parent, key } => join(parent, key),
        Path::Some { parent }
        | Path::NewtypeStruct { parent }
        | Path::NewtypeVariant { parent } => key_path(parent),
    }
}

/// The dotted path of the first string value in `value` that is empty or
/// only whitespace. `at` is the path of `value` itself.
fn first_blank_string(value: &serde_json::Value, at: &str) -> Option<String> {
    let join = |key: &dyn fmt::Display| match at {
        "" => key.to_string(),
        at => format!("{at}.{key}"),
    };
    match value {
        serde_json::Value::String(text) if text.trim().is_empty() => Some(at.to_string()),
        serde_json::Value::Object(object) => object
            .iter()
            .find_map(|(key, value)| first_blank_string(value, &join(key))),
        serde_json::Value::Array(items) => items
            .iter()
            .enumerate()
            .find_map(|(index, value)| first_blank_string(value, &join(&index))),
        _ => None,
    }
}

impl ExecutorConfigFile {
    pub fn load(path: &Path) -> Result<Self, ConfigFileError> {
        let text = std::fs::read_to_string(path).map_err(|error| ConfigFileError::Read {
            path: path.to_path_buf(),
            error,
        })?;
        Self::parse(&text).map_err(|error| error.at(path))
    }

    /// Parses a config file's text: [`INLINE_SECRETS`] first, then the
    /// shape, rejecting any key the types do not know, at any depth, then
    /// any empty or whitespace-only string value (an unset key keeps the
    /// executor's default, an empty one would not).
    pub fn parse(text: &str) -> Result<Self, ConfigFileError> {
        let unlocated = |error| ConfigFileError::Parse {
            path: PathBuf::new(),
            error,
        };
        let json: serde_json::Value = serde_json::from_str(text).map_err(unlocated)?;
        if let Some((_, message)) = INLINE_SECRETS
            .iter()
            .find(|(pointer, _)| json.pointer(pointer).is_some())
        {
            return Err(ConfigFileError::InlineSecret {
                path: PathBuf::new(),
                message,
            });
        }
        let blank = first_blank_string(&json, "");
        let mut unknown = None;
        let file = serde_ignored::deserialize(json, |path| {
            unknown.get_or_insert_with(|| key_path(&path));
        })
        .map_err(unlocated)?;
        if let Some(key) = unknown {
            return Err(ConfigFileError::UnknownKey {
                path: PathBuf::new(),
                key,
            });
        }
        match blank {
            Some(key) => Err(ConfigFileError::EmptyValue {
                path: PathBuf::new(),
                key,
            }),
            None => Ok(file),
        }
    }

    /// The executor config these settings and secrets describe. A key the
    /// file does not set stays unset, and `run` gives it its default (see
    /// [`Ad4mConfig::prepare`]); the TLS port defaults to `port + 1`.
    pub fn to_ad4m_config(&self, secrets: &ExecutorSecrets) -> Result<Ad4mConfig, ConfigFileError> {
        let multi_user = self.multi_user_config.as_ref();

        let tls = match multi_user.and_then(|m| m.tls_config.as_ref()) {
            Some(tls) if tls.enabled => {
                let tls_port = match tls.tls_port {
                    Some(port) => port,
                    None => self
                        .port
                        .unwrap_or(DEFAULT_PORT)
                        .checked_add(1)
                        .ok_or(ConfigFileError::TlsPortOverflow)?,
                };
                Some(TlsConfig {
                    cert_file_path: tls.cert_file_path.clone(),
                    key_file_path: tls.key_file_path.clone(),
                    tls_port,
                })
            }
            _ => None,
        };

        let smtp_config = match multi_user.and_then(|m| m.smtp_config.as_ref()) {
            Some(smtp) => {
                let password = match (&secrets.smtp_password, smtp.enabled) {
                    (Some(password), _) => password.clone(),
                    (None, false) => String::new(),
                    (None, true) => return Err(ConfigFileError::MissingSmtpPassword),
                };
                Some(SmtpConfig {
                    enabled: smtp.enabled,
                    host: smtp.host.clone(),
                    port: smtp.port,
                    username: smtp.username.clone(),
                    password,
                    from_address: smtp.from_address.clone(),
                })
            }
            None => None,
        };

        Ok(Ad4mConfig {
            app_data_path: self.app_data_path.clone(),
            port: self.port,
            hc_admin_port: self.hc_admin_port,
            hc_app_port: self.hc_app_port,
            localhost: self.localhost,
            run_dapp_server: self.run_dapp_server,
            auto_permit_cap_requests: self.auto_permit_cap_requests,
            tls,
            enable_multi_user: multi_user.map(|m| m.enabled),
            smtp_config,
            log_config: self.log_config.clone(),
            enable_mcp: self.mcp_enabled,
            mcp_port: self.mcp_port,
            admin_credential: secrets.admin_credential.clone(),
            ..Ad4mConfig::unprepared()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(json: &str) -> Result<ExecutorConfigFile, ConfigFileError> {
        ExecutorConfigFile::parse(json)
    }

    fn scratch_dir(name: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("ad4m-config-file-{}-{}", name, std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[cfg(unix)]
    fn write_secret(path: &Path, content: &str, mode: u32) {
        use std::os::unix::fs::PermissionsExt;
        std::fs::write(path, content).unwrap();
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(mode)).unwrap();
    }

    const SMTP_FILE: &str = r#"{
        "port": 12400,
        "multi_user_config": {
            "enabled": true,
            "tls_config": null,
            "smtp_config": {
                "enabled": true, "host": "smtp.example", "port": 465,
                "username": "ad4m@example", "from_address": "ad4m@example"
            }
        }
    }"#;

    /// The launcher (`ui/src-tauri`, whose own tests CI does not run) reads
    /// `multi_user_config` and both `tls_config` keys of
    /// `launcher-state.json` through these types. This is that file's
    /// shape on a running node, SMTP password encrypted, values made up.
    #[test]
    fn the_shared_types_read_the_launcher_state_shape() {
        let launcher_state: serde_json::Value = serde_json::from_str(
            r#"{
                "agent_list": [{"name": "Main Net", "path": "/home/op/.ad4m", "bootstrap": null}],
                "selected_agent": {"name": "Main Net", "path": "/home/op/.ad4m", "bootstrap": null},
                "log_config": {"rust_executor": "info", "holochain": "warn"},
                "tls_config": {"enabled": true, "cert_file_path": "/c", "key_file_path": "/k",
                               "tls_port": 12100},
                "multi_user_config": {
                    "enabled": true,
                    "smtp_config": {"host": "smtp.example.org", "port": 465, "username": "u",
                                    "password": "bm9uY2VjaXBoZXJ0ZXh0", "from_address": "f"},
                    "tls_config": {"enabled": true, "cert_file_path": "/c", "key_file_path": "/k",
                                   "tls_port": 12100}
                },
                "mcp_enabled": true,
                "mcp_port": 3001
            }"#,
        )
        .unwrap();
        let multi_user: MultiUserSettings<serde_json::Value> =
            serde_json::from_value(launcher_state["multi_user_config"].clone()).unwrap();
        assert!(multi_user.enabled);
        assert_eq!(multi_user.tls_config.unwrap().tls_port, Some(12100));
        let legacy: TlsSettings =
            serde_json::from_value(launcher_state["tls_config"].clone()).unwrap();
        assert_eq!(legacy.cert_file_path, "/c");
        let log_config: HashMap<String, String> =
            serde_json::from_value(launcher_state["log_config"].clone()).unwrap();
        assert_eq!(log_config["holochain"], "warn");
    }

    /// `launcher-state.json` written by a later launcher may carry keys this
    /// version does not know. The launcher must still read it: a failed
    /// parse makes `LauncherState::load` fall back to the default state,
    /// and the next save overwrites the file.
    #[test]
    fn the_shared_types_accept_keys_they_do_not_know() {
        let multi_user: MultiUserSettings<serde_json::Value> = serde_json::from_str(
            r#"{"enabled": true, "smtp_config": null, "future_key": 1,
                "tls_config": {"enabled": true, "cert_file_path": "/c", "key_file_path": "/k",
                               "tls_port": 12100, "future_tls_key": "x"}}"#,
        )
        .expect("unknown multi_user_config keys are ignored");
        assert_eq!(multi_user.tls_config.unwrap().tls_port, Some(12100));
    }

    #[test]
    fn an_unknown_nested_key_in_the_config_file_is_rejected() {
        for (json, key) in [
            (
                r#"{"multi_user_config": {"enabled": true, "tls": null}}"#,
                "multi_user_config.tls",
            ),
            (
                r#"{"multi_user_config": {"enabled": true, "tls_config":
                    {"enabled": false, "cert_file_path": "", "key_file_path": "",
                     "tls_prot": 1}}}"#,
                "multi_user_config.tls_config.tls_prot",
            ),
            (
                &SMTP_FILE.replace(r#""port": 465,"#, r#""port": 465, "hots": "x","#),
                "multi_user_config.smtp_config.hots",
            ),
        ] {
            let err = parse(json).unwrap_err().to_string();
            assert!(err.contains(&format!("unknown key `{key}`")), "{err}");
        }
    }

    /// `"app_data_path": ""` would parse and keep the executor's default
    /// unapplied (`prepare()` fills in only `None`), so an empty or
    /// whitespace-only string is an error that names its key, at any depth.
    #[test]
    fn an_empty_or_whitespace_only_string_value_is_rejected() {
        let tls = |cert: &str| {
            format!(
                r#"{{"multi_user_config": {{"enabled": true, "smtp_config": null,
                    "tls_config": {{"enabled": true, "cert_file_path": "{cert}",
                                   "key_file_path": "/k", "tls_port": null}}}}}}"#
            )
        };
        for blank in ["", " ", "\t", " \n "] {
            let blank_json = serde_json::to_string(blank).unwrap();
            for (json, key) in [
                (
                    format!(r#"{{"app_data_path": {blank_json}}}"#),
                    "app_data_path",
                ),
                (
                    tls(blank_json.trim_matches('"')),
                    "multi_user_config.tls_config.cert_file_path",
                ),
                (
                    SMTP_FILE.replace(r#""smtp.example""#, &blank_json),
                    "multi_user_config.smtp_config.host",
                ),
                (
                    format!(r#"{{"log_config": {{"holochain": {blank_json}}}}}"#),
                    "log_config.holochain",
                ),
            ] {
                let err = parse(&json)
                    .err()
                    .unwrap_or_else(|| panic!("{key} = {blank:?} parsed"))
                    .to_string();
                assert!(
                    err.contains(&format!("`{key}`")),
                    "{key} = {blank:?}: {err}"
                );
                assert!(err.contains("empty"), "{key} = {blank:?}: {err}");
            }
        }
        assert!(parse(&tls("/c")).is_ok());
    }

    #[test]
    fn an_inline_smtp_password_is_rejected() {
        let err = parse(&SMTP_FILE.replace(r#""port": 465,"#, r#""port": 465, "password": "x","#))
            .unwrap_err()
            .to_string();
        assert!(err.contains("smtp_config.password is not allowed"), "{err}");
    }

    #[test]
    fn an_inline_admin_credential_is_rejected() {
        let err = parse(r#"{"admin_credential": "x"}"#)
            .unwrap_err()
            .to_string();
        assert!(err.contains("admin_credential is not allowed"), "{err}");
    }

    #[test]
    fn an_unknown_key_is_rejected() {
        let err = parse(r#"{"mcp_prot": 3003}"#).unwrap_err().to_string();
        assert!(err.contains("unknown key `mcp_prot`"), "{err}");
    }

    #[test]
    fn smtp_settings_and_the_password_reach_ad4m_config() {
        let file = parse(SMTP_FILE).unwrap();
        let secrets = ExecutorSecrets {
            smtp_password: Some("pw".into()),
            ..Default::default()
        };
        let config = file.to_ad4m_config(&secrets).unwrap();
        let smtp = config.smtp_config.expect("smtp_config is set");
        assert_eq!(
            (
                smtp.enabled,
                smtp.host.as_str(),
                smtp.port,
                smtp.password.as_str()
            ),
            (true, "smtp.example", 465, "pw")
        );
        assert_eq!(config.enable_multi_user, Some(true));
        assert_eq!(config.port, Some(12400));
    }

    #[test]
    fn enabled_smtp_without_a_password_is_an_error() {
        let file = parse(SMTP_FILE).unwrap();
        assert!(matches!(
            file.to_ad4m_config(&ExecutorSecrets::default()),
            Err(ConfigFileError::MissingSmtpPassword)
        ));
    }

    #[test]
    fn the_tls_port_defaults_to_the_main_port_plus_one() {
        let file = parse(
            r#"{"port": 12400, "multi_user_config": {"enabled": false, "smtp_config": null,
                "tls_config": {"enabled": true, "cert_file_path": "c", "key_file_path": "k",
                               "tls_port": null}}}"#,
        )
        .unwrap();
        let tls = file
            .to_ad4m_config(&ExecutorSecrets::default())
            .unwrap()
            .tls
            .unwrap();
        assert_eq!(tls.tls_port, 12401);

        let mut disabled = file.clone();
        disabled
            .multi_user_config
            .as_mut()
            .unwrap()
            .tls_config
            .as_mut()
            .unwrap()
            .enabled = false;
        assert!(disabled
            .to_ad4m_config(&ExecutorSecrets::default())
            .unwrap()
            .tls
            .is_none());
    }

    #[cfg(unix)]
    #[test]
    fn a_secret_file_readable_by_others_is_rejected() {
        let dir = scratch_dir("mode");
        let path = dir.join("secret");
        for mode in [0o644, 0o640, 0o604, 0o660] {
            write_secret(&path, "s3cret\n", mode);
            let err = read_secret_file(&path).unwrap_err();
            assert!(
                matches!(err, ConfigFileError::SecretFileMode { mode: m, .. } if m == mode),
                "{mode:o}: {err}"
            );
            assert!(!err.to_string().contains("s3cret"));
        }
        for mode in [0o600, 0o400] {
            write_secret(&path, "s3cret\n", mode);
            assert_eq!(read_secret_file(&path).unwrap(), "s3cret", "{mode:o}");
        }
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn a_secret_file_loses_exactly_one_trailing_newline() {
        let dir = scratch_dir("newline");
        let path = dir.join("secret");
        for (content, expected) in [
            ("a", "a"),
            ("a\n", "a"),
            ("a\r\n", "a"),
            ("a\n\n", "a\n"),
            (" a \n", " a "),
        ] {
            write_secret(&path, content, 0o600);
            assert_eq!(read_secret_file(&path).unwrap(), expected, "{content:?}");
        }
        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// An empty admin credential matches the empty token an unauthenticated
    /// client sends, so an empty secret is an error wherever it is read.
    #[cfg(unix)]
    #[test]
    fn an_empty_secret_is_an_error() {
        let dir = scratch_dir("empty");
        let path = dir.join("secret");
        for content in ["", "\n", "\r\n"] {
            write_secret(&path, content, 0o600);
            let err = read_secret_file(&path)
                .err()
                .unwrap_or_else(|| panic!("{content:?} is not a secret"));
            assert!(err.to_string().contains("is empty"), "{err}");
        }

        let file = ExecutorConfigFile::default();
        let path = path.to_string_lossy().into_owned();
        for (var, value) in [
            ("AD4M_ADMIN_CREDENTIAL_FILE", path.as_str()),
            ("AD4M_ADMIN_CREDENTIAL", ""),
            ("AD4M_SMTP_PASSWORD", ""),
            ("AD4M_UNLOCK_PASSPHRASE_FILE", path.as_str()),
        ] {
            let env = |name: &str| (name == var).then(|| value.to_string());
            let err = ExecutorSecrets::from_env(env, &file)
                .err()
                .unwrap_or_else(|| panic!("empty {var} is accepted"));
            assert!(err.to_string().contains("is empty"), "{var}: {err}");
        }
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn secrets_come_from_the_environment_before_the_config_file() {
        let dir = scratch_dir("env");
        let from_config = dir.join("smtp-from-config");
        let from_env = dir.join("smtp-from-env");
        let unlock = dir.join("unlock");
        write_secret(&from_config, "config-pw\n", 0o600);
        write_secret(&from_env, "env-file-pw\n", 0o400);
        write_secret(&unlock, "passphrase\n", 0o600);
        let mut file = parse(SMTP_FILE).unwrap();
        file.multi_user_config
            .as_mut()
            .unwrap()
            .smtp_config
            .as_mut()
            .unwrap()
            .password_file = Some(from_config.to_string_lossy().into_owned());

        let env_of = |vars: Vec<(&'static str, String)>| {
            move |name: &str| {
                vars.iter()
                    .find(|(k, _)| *k == name)
                    .map(|(_, v)| v.clone())
            }
        };

        let secrets = ExecutorSecrets::from_env(env_of(vec![]), &file).unwrap();
        assert_eq!(secrets.smtp_password.as_deref(), Some("config-pw"));
        assert_eq!(secrets.admin_credential, None);
        assert_eq!(secrets.unlock_passphrase, None);

        let path = |p: &PathBuf| p.to_string_lossy().into_owned();
        let secrets = ExecutorSecrets::from_env(
            env_of(vec![
                ("AD4M_SMTP_PASSWORD_FILE", path(&from_env)),
                ("AD4M_UNLOCK_PASSPHRASE_FILE", path(&unlock)),
                ("AD4M_ADMIN_CREDENTIAL_FILE", path(&unlock)),
            ]),
            &file,
        )
        .unwrap();
        assert_eq!(secrets.smtp_password.as_deref(), Some("env-file-pw"));
        assert_eq!(secrets.unlock_passphrase.as_deref(), Some("passphrase"));
        assert_eq!(secrets.admin_credential.as_deref(), Some("passphrase"));

        let secrets = ExecutorSecrets::from_env(
            env_of(vec![("AD4M_SMTP_PASSWORD", "env-pw".to_string())]),
            &file,
        )
        .unwrap();
        assert_eq!(secrets.smtp_password.as_deref(), Some("env-pw"));

        let err = ExecutorSecrets::from_env(
            env_of(vec![
                ("AD4M_SMTP_PASSWORD", "env-pw".to_string()),
                ("AD4M_SMTP_PASSWORD_FILE", path(&from_env)),
            ]),
            &file,
        )
        .unwrap_err();
        assert!(matches!(
            err,
            ConfigFileError::ConflictingSecretSources { .. }
        ));

        let debug = format!("{secrets:?}");
        assert!(!debug.contains("env-pw"), "{debug}");
        std::fs::remove_dir_all(&dir).unwrap();
    }
}

//! The executor's config file as RPC: `runtime.config` reads it, `runtime.setConfig` writes it.
//! Handlers in `api/config_ws.rs`.
//!
//! Every key the calls know has a class ([`KeyClass`]):
//!
//! - `restart`: any operator session writes it; it takes effect at the next start.
//! - `machine`: paths, listeners, ports and trust settings of this machine. Only the operator
//!   on this machine writes it (`Session::is_operator_on_this_machine`); every other session
//!   gets the refusal `local-only` for that key.
//!
//! A write changes only the file the executor started with (`--config`), atomically (a
//! temporary file in the same folder, `fsync`, rename), mode 0600, and keeps every key it does
//! not touch. A key that a flag or an environment variable overrides at this start is refused
//! `pinned-by-config`: the write would not take effect while the override stays. Each written
//! key must leave a file the executor starts with (`ExecutorConfigFile::parse`), and passes
//! the validator its owning module registered ([`register_config_key`]), within 10 seconds.
//! A write publishes `config-changed` to the operator.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, LazyLock, RwLock};
use std::time::Duration;

use futures::future::BoxFuture;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use ts_rs::TS;

use crate::config_file::ExecutorConfigFile;

/// Who may write a key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum KeyClass {
    /// Any operator session; it takes effect at the next start.
    Restart,
    /// Only the operator on this machine; it takes effect at the next start.
    Machine,
}

/// Why a key's value in the file is not what runs: a flag or an environment variable of this
/// start overrides it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum Pinned {
    Config,
}

/// One key `runtime.config` lists.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ConfigKeyInfo {
    pub key: String,
    pub class: KeyClass,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub pinned: Option<Pinned>,
}

/// `runtime.config` result.
#[derive(Clone, Debug, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RuntimeConfig {
    /// The file's keys and values: what the next start reads. The file holds no secret (a
    /// secret is named by a `*_file` path), so neither does this.
    #[ts(type = "Record<string, unknown>")]
    pub values: Map<String, Value>,
    /// Every key `runtime.setConfig` knows, with its class.
    pub keys: Vec<ConfigKeyInfo>,
    /// A write since this start waits for the next start.
    pub restart_pending: bool,
    /// The file the executor started with, `null` without `--config`.
    pub file: Option<String>,
}

/// `runtime.setConfig` params: keys to write, keys to remove (each then takes its default).
#[derive(Clone, Debug, Default, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct SetConfigParams {
    #[serde(default)]
    #[ts(optional, type = "Record<string, unknown>")]
    pub set: Option<Map<String, Value>>,
    #[serde(default)]
    #[ts(optional)]
    pub unset: Option<Vec<String>>,
}

/// Why `runtime.setConfig` did not write a key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum RefusalReason {
    /// A `machine` key from a session that is not the operator on this machine.
    LocalOnly,
    /// A flag or an environment variable overrides the key at this start.
    PinnedByConfig,
    /// The value fails the file's shape or the key's validator; `detail` says why.
    Invalid,
    /// No such key.
    UnknownKey,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct ConfigRefusal {
    pub key: String,
    pub reason: RefusalReason,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub detail: Option<String>,
}

/// `runtime.setConfig` result. The keys not in `refused` are written.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetConfigResult {
    pub restart_pending: bool,
    pub refused: Vec<ConfigRefusal>,
}

/// `config-changed`: keys written to the config file. No values: the operator reads them with
/// `runtime.config`.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ConfigChanged {
    pub keys: Vec<String>,
    pub restart_pending: bool,
}

/// A check a key's owning module runs on a value before it is written. A message is the
/// refusal's `detail`.
pub type ConfigValidator =
    Arc<dyn Fn(Value) -> BoxFuture<'static, Result<(), String>> + Send + Sync>;

/// A key `runtime.setConfig` knows. `key` names a top-level key of the config file, or a
/// nested one with dots (`link_server.url`).
#[derive(Clone)]
pub struct ConfigKeySpec {
    pub key: &'static str,
    pub class: KeyClass,
    pub validator: Option<ConfigValidator>,
}

/// How long a validator may run before its key is refused ("check timed out").
pub const VALIDATOR_LIMIT: Duration = Duration::from_secs(10);

/// The keys of the config file and their classes. Machine: paths, listening addresses and
/// ports, the multi-user and TLS settings, and the identity store's address and keys.
const BUILT_IN_KEYS: &[(&str, KeyClass)] = &[
    ("auto_permit_cap_requests", KeyClass::Restart),
    ("log_config", KeyClass::Restart),
    ("mcp_enabled", KeyClass::Restart),
    ("mcp_port", KeyClass::Restart),
    ("app_data_path", KeyClass::Machine),
    ("hc_admin_port", KeyClass::Machine),
    ("hc_app_port", KeyClass::Machine),
    ("identity", KeyClass::Machine),
    ("localhost", KeyClass::Machine),
    ("log_dir", KeyClass::Machine),
    ("multi_user_config", KeyClass::Machine),
    ("port", KeyClass::Machine),
    ("run_dapp_server", KeyClass::Machine),
    ("sfu_bind_addr", KeyClass::Machine),
    ("sfu_public_addr", KeyClass::Machine),
    ("sfu_stun_server", KeyClass::Machine),
    ("sfu_udp_port", KeyClass::Machine),
];

static KEYS: LazyLock<RwLock<Vec<ConfigKeySpec>>> = LazyLock::new(|| {
    RwLock::new(
        BUILT_IN_KEYS
            .iter()
            .map(|&(key, class)| ConfigKeySpec {
                key,
                class,
                validator: None,
            })
            .collect(),
    )
});

/// Adds `spec` to the keys `runtime.setConfig` knows, or replaces the spec of its key. Each
/// module that owns a key calls it once at start, with the key's validator if it has one.
pub fn register_config_key(spec: ConfigKeySpec) {
    let mut keys = KEYS.write().unwrap_or_else(|e| e.into_inner());
    keys.retain(|known| known.key != spec.key);
    keys.push(spec);
}

fn spec_of(key: &str) -> Option<ConfigKeySpec> {
    KEYS.read()
        .unwrap_or_else(|e| e.into_inner())
        .iter()
        .find(|spec| spec.key == key)
        .cloned()
}

/// Every key with its class, sorted, marked when this start pins it.
pub fn key_infos(pinned: &[String]) -> Vec<ConfigKeyInfo> {
    let mut infos: Vec<ConfigKeyInfo> = KEYS
        .read()
        .unwrap_or_else(|e| e.into_inner())
        .iter()
        .map(|spec| ConfigKeyInfo {
            key: spec.key.to_string(),
            class: spec.class,
            pinned: pinned
                .iter()
                .any(|p| p == spec.key)
                .then_some(Pinned::Config),
        })
        .collect();
    infos.sort_by(|a, b| a.key.cmp(&b.key));
    infos
}

static RESTART_PENDING: AtomicBool = AtomicBool::new(false);

/// Whether a write since this start waits for the next start.
pub fn restart_pending() -> bool {
    RESTART_PENDING.load(Ordering::SeqCst)
}

/// Serializes writes, so two calls never read the same file and each drop the other's keys.
static WRITE: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

/// Why `runtime.setConfig` wrote nothing.
#[derive(Debug)]
pub enum SetConfigError {
    /// The executor started without `--config` (409).
    NoConfigFile,
    /// The file cannot be read, parsed or written (500).
    File(String),
}

/// The keys and values of the config file at `file`.
pub fn read_values(file: &Path) -> Result<Map<String, Value>, String> {
    let text = std::fs::read_to_string(file)
        .map_err(|e| format!("cannot read {}: {e}", file.display()))?;
    match serde_json::from_str(&text) {
        Ok(Value::Object(values)) => Ok(values),
        Ok(_) => Err(format!("{} is not a JSON object", file.display())),
        Err(e) => Err(format!("cannot parse {}: {e}", file.display())),
    }
}

/// `runtime.config` for an executor that started with `file` and `pinned` keys.
pub fn runtime_config(file: Option<&Path>, pinned: &[String]) -> Result<RuntimeConfig, String> {
    Ok(RuntimeConfig {
        values: match file {
            Some(file) => read_values(file)?,
            None => Map::new(),
        },
        keys: key_infos(pinned),
        restart_pending: restart_pending(),
        file: file.map(|f| f.display().to_string()),
    })
}

/// The path segments of a key: `link_server.url` is `["link_server", "url"]`.
fn segments(key: &str) -> Vec<&str> {
    key.split('.').collect()
}

fn set_at(values: &mut Map<String, Value>, key: &str, value: Value) {
    let path = segments(key);
    let (last, parents) = path.split_last().expect("a key has a segment");
    let mut object = values;
    for parent in parents {
        let entry = object
            .entry(parent.to_string())
            .or_insert_with(|| Value::Object(Map::new()));
        if !entry.is_object() {
            *entry = Value::Object(Map::new());
        }
        object = entry.as_object_mut().expect("just made an object");
    }
    object.insert(last.to_string(), value);
}

fn remove_at(values: &mut Map<String, Value>, key: &str) {
    let path = segments(key);
    let (last, parents) = path.split_last().expect("a key has a segment");
    let mut object = values;
    for parent in parents {
        match object.get_mut(*parent).and_then(Value::as_object_mut) {
            Some(inner) => object = inner,
            None => return,
        }
    }
    object.remove(*last);
}

/// Whether the executor starts with `values` as its config file.
fn starts_with(values: &Map<String, Value>) -> Result<(), String> {
    let text = serde_json::to_string(values).map_err(|e| e.to_string())?;
    ExecutorConfigFile::parse(&text)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

/// Writes `values` to `file`: a temporary file in the same folder, mode 0600, `fsync`, then a
/// rename over `file`, so a crash leaves the old file or the new one, never half of one.
fn write_atomically(file: &Path, values: &Map<String, Value>) -> Result<(), String> {
    use std::io::Write;
    let folder = file
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    let name = file
        .file_name()
        .ok_or_else(|| format!("{} names no file", file.display()))?
        .to_string_lossy();
    let temporary: PathBuf = folder.join(format!(".{name}.{}.tmp", std::process::id()));
    let text = serde_json::to_string_pretty(values).map_err(|e| e.to_string())? + "\n";
    let write = || -> std::io::Result<()> {
        let mut options = std::fs::OpenOptions::new();
        options.write(true).create(true).truncate(true);
        #[cfg(unix)]
        {
            use std::os::unix::fs::OpenOptionsExt;
            options.mode(0o600);
        }
        let mut out = options.open(&temporary)?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            out.set_permissions(std::fs::Permissions::from_mode(0o600))?;
        }
        out.write_all(text.as_bytes())?;
        out.sync_all()?;
        std::fs::rename(&temporary, file)?;
        if let Ok(dir) = std::fs::File::open(folder) {
            let _ = dir.sync_all();
        }
        Ok(())
    };
    write().map_err(|e| {
        let _ = std::fs::remove_file(&temporary);
        format!("cannot write {}: {e}", file.display())
    })
}

/// `runtime.setConfig`: writes the keys of `params` that the caller may write to `file`, and
/// answers the others with their refusals. `on_machine`: the caller is the operator on this
/// machine. `limit`: how long each validator may run ([`VALIDATOR_LIMIT`]).
pub async fn set_config(
    file: Option<&Path>,
    pinned: &[String],
    on_machine: bool,
    params: SetConfigParams,
    limit: Duration,
) -> Result<SetConfigResult, SetConfigError> {
    let file = file.ok_or(SetConfigError::NoConfigFile)?;
    let _write = WRITE.lock().await;
    let mut values = read_values(file).map_err(SetConfigError::File)?;

    let mut refused = Vec::new();
    let mut written = BTreeSet::new();
    let refuse = |key: &str, reason: RefusalReason, detail: Option<String>| ConfigRefusal {
        key: key.to_string(),
        reason,
        detail,
    };
    let changes: Vec<(String, Option<Value>)> = params
        .set
        .unwrap_or_default()
        .into_iter()
        .map(|(key, value)| (key, Some(value)))
        .chain(
            params
                .unset
                .unwrap_or_default()
                .into_iter()
                .map(|key| (key, None)),
        )
        .collect();

    for (key, value) in changes {
        let Some(spec) = spec_of(&key) else {
            refused.push(refuse(&key, RefusalReason::UnknownKey, None));
            continue;
        };
        if spec.class == KeyClass::Machine && !on_machine {
            refused.push(refuse(&key, RefusalReason::LocalOnly, None));
            continue;
        }
        if pinned.iter().any(|p| p == &key) {
            refused.push(refuse(&key, RefusalReason::PinnedByConfig, None));
            continue;
        }
        let mut candidate = values.clone();
        match &value {
            Some(value) => set_at(&mut candidate, &key, value.clone()),
            None => remove_at(&mut candidate, &key),
        }
        if let Err(detail) = starts_with(&candidate) {
            refused.push(refuse(&key, RefusalReason::Invalid, Some(detail)));
            continue;
        }
        if let (Some(value), Some(validator)) = (&value, &spec.validator) {
            let verdict = match tokio::time::timeout(limit, validator(value.clone())).await {
                Ok(verdict) => verdict,
                Err(_) => Err("check timed out".to_string()),
            };
            if let Err(detail) = verdict {
                refused.push(refuse(&key, RefusalReason::Invalid, Some(detail)));
                continue;
            }
        }
        values = candidate;
        written.insert(key);
    }

    if !written.is_empty() {
        write_atomically(file, &values).map_err(SetConfigError::File)?;
        RESTART_PENDING.store(true, Ordering::SeqCst);
        let event = ConfigChanged {
            keys: written.into_iter().collect(),
            restart_pending: true,
        };
        crate::pubsub::get_global_pubsub_sync().publish_event(crate::events::Event::new(
            crate::events::Topic::ConfigChanged,
            crate::events::Audience::Operator,
            event,
        ));
    }
    Ok(SetConfigResult {
        restart_pending: restart_pending(),
        refused,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn config_file(text: &str) -> (tempfile::TempDir, PathBuf) {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("config.json");
        std::fs::write(&path, text).unwrap();
        (dir, path)
    }

    fn set(pairs: Value) -> SetConfigParams {
        SetConfigParams {
            set: pairs.as_object().cloned(),
            unset: None,
        }
    }

    fn reasons(result: &SetConfigResult) -> Vec<(String, RefusalReason)> {
        result
            .refused
            .iter()
            .map(|r| (r.key.clone(), r.reason))
            .collect()
    }

    #[test]
    fn every_key_has_a_class_and_the_listener_keys_are_machine_keys() {
        let infos = key_infos(&["port".to_string()]);
        let class = |key: &str| infos.iter().find(|i| i.key == key).map(|i| i.class);
        for key in [
            "mcp_enabled",
            "mcp_port",
            "log_config",
            "auto_permit_cap_requests",
        ] {
            assert_eq!(class(key), Some(KeyClass::Restart), "{key}");
        }
        for key in [
            "app_data_path",
            "port",
            "hc_admin_port",
            "hc_app_port",
            "localhost",
            "log_dir",
            "multi_user_config",
        ] {
            assert_eq!(class(key), Some(KeyClass::Machine), "{key}");
        }
        let port = infos.iter().find(|i| i.key == "port").unwrap();
        assert_eq!(port.pinned, Some(Pinned::Config));
        // Every key of the file the CLI starts with is known.
        let file = serde_json::to_value(ExecutorConfigFile {
            app_data_path: Some("a".into()),
            port: Some(1),
            hc_admin_port: Some(1),
            hc_app_port: Some(1),
            localhost: Some(true),
            run_dapp_server: Some(true),
            auto_permit_cap_requests: Some(true),
            multi_user_config: Some(Default::default()),
            log_config: Some(Default::default()),
            mcp_enabled: Some(true),
            mcp_port: Some(1),
            sfu_bind_addr: Some("a".into()),
            sfu_udp_port: Some(1),
            sfu_public_addr: Some("a".into()),
            sfu_stun_server: Some("a".into()),
            identity: Some(Default::default()),
            log_dir: Some("a".into()),
        })
        .unwrap();
        for key in file.as_object().unwrap().keys() {
            assert!(class(key).is_some(), "{key} has no class");
        }
    }

    #[tokio::test]
    async fn a_restart_key_is_written_atomically_with_mode_0600_keeping_the_other_keys() {
        let (_dir, path) = config_file(r#"{"port": 12000, "mcp_port": 3001}"#);
        let result = set_config(
            Some(&path),
            &[],
            false,
            set(json!({
                "auto_permit_cap_requests": false,
                "log_config": { "rust_executor": "debug" }
            })),
            VALIDATOR_LIMIT,
        )
        .await
        .unwrap();
        assert!(result.refused.is_empty(), "{result:?}");
        assert!(result.restart_pending);
        let values = read_values(&path).unwrap();
        assert_eq!(values.get("auto_permit_cap_requests"), Some(&json!(false)));
        assert_eq!(
            values.get("log_config"),
            Some(&json!({ "rust_executor": "debug" }))
        );
        assert_eq!(
            values.get("mcp_port"),
            Some(&json!(3001)),
            "a key not set stays"
        );
        assert_eq!(
            values.get("port"),
            Some(&json!(12000)),
            "a key not set stays"
        );
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(&path).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode, 0o600);
        }
        let leftovers: Vec<_> = std::fs::read_dir(path.parent().unwrap())
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_name().to_string_lossy().ends_with(".tmp"))
            .collect();
        assert!(leftovers.is_empty(), "no temporary file stays");
    }

    #[tokio::test]
    async fn a_machine_key_is_local_only_and_a_pinned_or_unknown_key_is_refused() {
        let (_dir, path) = config_file(r#"{"port": 12000}"#);
        let params = set(json!({
            "port": 12100,
            "localhost": false,
            "mcp_port": 3009,
            "no_such_key": 1
        }));
        let remote = set_config(
            Some(&path),
            &["mcp_port".to_string()],
            false,
            params.clone(),
            VALIDATOR_LIMIT,
        )
        .await
        .unwrap();
        let mut got = reasons(&remote);
        got.sort();
        assert_eq!(
            got,
            vec![
                ("localhost".to_string(), RefusalReason::LocalOnly),
                ("mcp_port".to_string(), RefusalReason::PinnedByConfig),
                ("no_such_key".to_string(), RefusalReason::UnknownKey),
                ("port".to_string(), RefusalReason::LocalOnly),
            ]
        );
        assert_eq!(read_values(&path).unwrap().get("port"), Some(&json!(12000)));

        // The operator on this machine writes machine keys.
        let local = set_config(Some(&path), &[], true, params, VALIDATOR_LIMIT)
            .await
            .unwrap();
        assert_eq!(
            reasons(&local),
            vec![("no_such_key".to_string(), RefusalReason::UnknownKey)]
        );
        assert_eq!(read_values(&path).unwrap().get("port"), Some(&json!(12100)));
    }

    #[tokio::test]
    async fn without_a_config_file_nothing_is_written() {
        let result = set_config(
            None,
            &[],
            true,
            set(json!({"mcp_port": 1})),
            VALIDATOR_LIMIT,
        )
        .await;
        assert!(matches!(result, Err(SetConfigError::NoConfigFile)));
    }

    #[tokio::test]
    async fn a_value_the_executor_would_refuse_at_start_is_invalid_and_unset_removes_a_key() {
        let (_dir, path) = config_file(r#"{"mcp_port": 3001, "mcp_enabled": true}"#);
        let result = set_config(
            Some(&path),
            &[],
            true,
            SetConfigParams {
                set: json!({ "mcp_port": "not a port" }).as_object().cloned(),
                unset: Some(vec!["mcp_enabled".to_string()]),
            },
            VALIDATOR_LIMIT,
        )
        .await
        .unwrap();
        assert_eq!(
            reasons(&result),
            vec![("mcp_port".to_string(), RefusalReason::Invalid)]
        );
        assert!(result.refused[0].detail.is_some());
        let values = read_values(&path).unwrap();
        assert_eq!(values.get("mcp_port"), Some(&json!(3001)));
        assert!(!values.contains_key("mcp_enabled"));
    }

    /// A key's validator runs before the write: a value it refuses is refused `invalid` with
    /// its message and stays out of the file; a value it passes is written.
    #[tokio::test]
    async fn a_validator_decides_before_the_write_within_its_limit() {
        fn validator(
            check: fn(Value) -> BoxFuture<'static, Result<(), String>>,
        ) -> Option<ConfigValidator> {
            Some(Arc::new(check))
        }
        register_config_key(ConfigKeySpec {
            key: "mcp_port",
            class: KeyClass::Restart,
            validator: validator(|value| {
                Box::pin(async move {
                    if value.as_u64().is_some_and(|port| port >= 1024) {
                        Ok(())
                    } else {
                        Err("below 1024".to_string())
                    }
                })
            }),
        });
        let (_dir, path) = config_file("{}");
        let passed = set_config(
            Some(&path),
            &[],
            true,
            set(json!({ "mcp_port": 4000 })),
            Duration::from_millis(100),
        )
        .await
        .unwrap();
        assert!(passed.refused.is_empty(), "{passed:?}");
        assert_eq!(
            read_values(&path).unwrap().get("mcp_port"),
            Some(&json!(4000))
        );

        let refused = set_config(
            Some(&path),
            &[],
            true,
            set(json!({ "mcp_port": 80 })),
            Duration::from_millis(100),
        )
        .await
        .unwrap();
        assert_eq!(
            refused.refused,
            vec![ConfigRefusal {
                key: "mcp_port".to_string(),
                reason: RefusalReason::Invalid,
                detail: Some("below 1024".to_string()),
            }]
        );
        assert_eq!(
            read_values(&path).unwrap().get("mcp_port"),
            Some(&json!(4000))
        );
        // The built-in spec again, for the other tests.
        register_config_key(ConfigKeySpec {
            key: "mcp_port",
            class: KeyClass::Restart,
            validator: None,
        });
    }

    /// The limit: a validator that does not answer in time refuses its key.
    #[tokio::test]
    async fn a_validator_past_its_limit_refuses_its_key() {
        register_config_key(ConfigKeySpec {
            key: "mcp_enabled",
            class: KeyClass::Restart,
            validator: Some(Arc::new(|_| {
                Box::pin(async {
                    tokio::time::sleep(Duration::from_secs(5)).await;
                    Ok(())
                })
            })),
        });
        let (_dir, path) = config_file("{}");
        let result = set_config(
            Some(&path),
            &[],
            true,
            set(json!({ "mcp_enabled": true })),
            Duration::from_millis(50),
        )
        .await
        .unwrap();
        register_config_key(ConfigKeySpec {
            key: "mcp_enabled",
            class: KeyClass::Restart,
            validator: None,
        });
        assert_eq!(
            result.refused,
            vec![ConfigRefusal {
                key: "mcp_enabled".to_string(),
                reason: RefusalReason::Invalid,
                detail: Some("check timed out".to_string()),
            }]
        );
        assert!(!read_values(&path).unwrap().contains_key("mcp_enabled"));
    }
}

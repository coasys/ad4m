//! What `ad4m-executor run` starts the executor with. Three sources, a later
//! one winning per setting: the config file (`--config` / `AD4M_CONFIG`),
//! then `AD4M_<FLAG>` environment variables, then flags. clap resolves env
//! against flags; [`RunArgs::resolve`] lays the result over the file.
//! Secrets never come from the file or a flag (bar the legacy
//! `--admin-credential`): see [`ExecutorSecrets::from_env`].
//! `ad4m-executor config print` shows the merged result, secrets redacted.

use anyhow::{bail, Context, Result};
use rust_executor::config_file::{ExecutorConfigFile, ExecutorSecrets, REDACTED};
use rust_executor::Ad4mConfig;
use std::path::PathBuf;

/// Flags of `ad4m-executor run`. Each also reads `AD4M_<FLAG>`, e.g.
/// `--mcp-port` reads `AD4M_MCP_PORT`.
#[derive(clap::Args, Debug)]
pub struct RunArgs {
    /// JSON config file with the launcher's key names (see the docs'
    /// "Executor config file" page). There is no default path.
    #[arg(long, env = "AD4M_CONFIG")]
    pub config: Option<PathBuf>,
    #[arg(short, long, action, env = "AD4M_APP_DATA_PATH")]
    pub app_data_path: Option<String>,
    #[arg(short, long, action, env = "AD4M_NETWORK_BOOTSTRAP_SEED")]
    pub network_bootstrap_seed: Option<String>,
    #[arg(short, long, action, env = "AD4M_LANGUAGE_LANGUAGE_ONLY")]
    pub language_language_only: Option<bool>,
    #[arg(long, action, env = "AD4M_RUN_DAPP_SERVER")]
    pub run_dapp_server: Option<bool>,
    #[arg(short = 'p', long = "port", action, env = "AD4M_PORT")]
    pub port: Option<u16>,
    #[arg(long, action, env = "AD4M_HC_ADMIN_PORT")]
    pub hc_admin_port: Option<u16>,
    #[arg(long, action, env = "AD4M_HC_APP_PORT")]
    pub hc_app_port: Option<u16>,
    #[arg(long, action, env = "AD4M_HC_USE_BOOTSTRAP")]
    pub hc_use_bootstrap: Option<bool>,
    #[arg(long, action, env = "AD4M_HC_USE_LOCAL_PROXY")]
    pub hc_use_local_proxy: Option<bool>,
    #[arg(long, action, env = "AD4M_HC_USE_MDNS")]
    pub hc_use_mdns: Option<bool>,
    #[arg(long, action, env = "AD4M_HC_USE_PROXY")]
    pub hc_use_proxy: Option<bool>,
    #[arg(long, action, env = "AD4M_HC_PROXY_URL")]
    pub hc_proxy_url: Option<String>,
    #[arg(long, action, env = "AD4M_HC_BOOTSTRAP_URL")]
    pub hc_bootstrap_url: Option<String>,
    #[arg(long, action, env = "AD4M_HC_RELAY_URL")]
    pub hc_relay_url: Option<String>,
    #[arg(short, long, action, env = "AD4M_CONNECT_HOLOCHAIN")]
    pub connect_holochain: Option<bool>,
    #[arg(long, action, env = "AD4M_RUN_HOLOCHAIN")]
    pub run_holochain: Option<bool>,
    /// Admin credential granting full capabilities to whoever presents it.
    /// Prefer AD4M_ADMIN_CREDENTIAL_FILE (or the AD4M_ADMIN_CREDENTIAL
    /// environment variable): a flag value is visible to every user on the
    /// host via `ps` and stays in shell history.
    #[arg(long, action, env = "AD4M_ADMIN_CREDENTIAL", hide_env_values = true)]
    pub admin_credential: Option<String>,
    #[arg(long, action, env = "AD4M_LOCALHOST")]
    pub localhost: Option<bool>,
    #[arg(long, action, env = "AD4M_TLS_CERT_FILE")]
    pub tls_cert_file: Option<String>,
    #[arg(long, action, env = "AD4M_TLS_KEY_FILE")]
    pub tls_key_file: Option<String>,
    /// HTTPS/WSS port. Default: the RPC port + 1.
    #[arg(long, action, env = "AD4M_TLS_PORT")]
    pub tls_port: Option<u16>,
    #[arg(long, action, env = "AD4M_LOG_HOLOCHAIN_METRICS")]
    pub log_holochain_metrics: Option<bool>,
    #[arg(long, action, env = "AD4M_ENABLE_MULTI_USER")]
    pub enable_multi_user: Option<bool>,
    #[arg(long, action, env = "AD4M_ENABLE_MCP")]
    pub enable_mcp: Option<bool>,
    #[arg(long, action, env = "AD4M_MCP_PORT")]
    pub mcp_port: Option<u16>,
    /// Grant capability requests without the user confirming them. Default:
    /// true without --config (the CLI's historic behaviour), false with it.
    #[arg(long, action, env = "AD4M_AUTO_PERMIT_CAP_REQUESTS")]
    pub auto_permit_cap_requests: Option<bool>,
    /// Expose dynamic per-class SHACL tools ({class}_create, {class}_set_{prop}, …)
    /// over MCP in addition to the static instance_* tools. Default: false.
    #[arg(
        long,
        num_args = 0..=1,
        default_missing_value = "true",
        env = "AD4M_DYNAMIC_CLASS_TOOLS"
    )]
    pub dynamic_class_tools: Option<bool>,
    /// Write the executor PID to this file on startup (removed on clean shutdown).
    /// Useful for test harnesses that need targeted process cleanup.
    #[arg(long, env = "AD4M_PID_FILE")]
    pub pid_file: Option<String>,
}

/// The executor config plus the one secret `run` uses itself rather than
/// handing to the executor's config.
pub struct ResolvedRun {
    pub config: Ad4mConfig,
    pub unlock_passphrase: Option<String>,
}

fn overlay<T: Clone>(slot: &mut Option<T>, value: &Option<T>) {
    if value.is_some() {
        slot.clone_from(value);
    }
}

impl RunArgs {
    /// The config file with env and flag values laid over it.
    fn merged_file(&self) -> Result<ExecutorConfigFile> {
        let mut file = match &self.config {
            Some(path) => ExecutorConfigFile::load(path)?,
            None => ExecutorConfigFile::default(),
        };
        overlay(&mut file.app_data_path, &self.app_data_path);
        overlay(&mut file.port, &self.port);
        overlay(&mut file.hc_admin_port, &self.hc_admin_port);
        overlay(&mut file.hc_app_port, &self.hc_app_port);
        overlay(&mut file.localhost, &self.localhost);
        overlay(&mut file.run_dapp_server, &self.run_dapp_server);
        overlay(&mut file.mcp_enabled, &self.enable_mcp);
        overlay(&mut file.mcp_port, &self.mcp_port);
        overlay(
            &mut file.auto_permit_cap_requests,
            &self.auto_permit_cap_requests,
        );
        if self.config.is_none() && file.auto_permit_cap_requests.is_none() {
            file.auto_permit_cap_requests = Some(true);
        }

        if let Some(enabled) = self.enable_multi_user {
            file.multi_user_config
                .get_or_insert_with(Default::default)
                .enabled = enabled;
        }
        if self.tls_cert_file.is_some() || self.tls_key_file.is_some() || self.tls_port.is_some() {
            let tls = file
                .multi_user_config
                .get_or_insert_with(Default::default)
                .tls_config
                .get_or_insert_with(Default::default);
            if let Some(cert) = &self.tls_cert_file {
                tls.cert_file_path.clone_from(cert);
                tls.enabled = true;
            }
            if let Some(key) = &self.tls_key_file {
                tls.key_file_path.clone_from(key);
                tls.enabled = true;
            }
            overlay(&mut tls.tls_port, &self.tls_port);
        }
        if let Some(tls) = file
            .multi_user_config
            .as_ref()
            .and_then(|m| m.tls_config.as_ref())
        {
            if tls.enabled && (tls.cert_file_path.is_empty() || tls.key_file_path.is_empty()) {
                bail!(
                    "TLS needs both a certificate and a key: --tls-cert-file and \
                     --tls-key-file, or tls_config.cert_file_path and key_file_path"
                );
            }
        }
        Ok(file)
    }

    /// Resolves the settings `run` starts with. `env` looks up a variable
    /// (`std::env::var` outside tests) for the secrets; clap has already
    /// read the `AD4M_<FLAG>` ones.
    pub fn resolve(self, env: impl Fn(&str) -> Option<String>) -> Result<ResolvedRun> {
        let file = self.merged_file()?;
        let mut secrets =
            ExecutorSecrets::from_env(env, &file).context("cannot read the executor's secrets")?;
        // A flag beats the environment, as for every other setting.
        overlay(&mut secrets.admin_credential, &self.admin_credential);

        let mut config = file.to_ad4m_config(&secrets)?;
        config.network_bootstrap_seed = self.network_bootstrap_seed;
        config.language_language_only = self.language_language_only;
        config.hc_use_bootstrap = self.hc_use_bootstrap;
        config.hc_use_local_proxy = self.hc_use_local_proxy;
        config.hc_use_mdns = self.hc_use_mdns;
        config.hc_use_proxy = self.hc_use_proxy;
        config.hc_proxy_url = self.hc_proxy_url;
        config.hc_bootstrap_url = self.hc_bootstrap_url;
        config.hc_relay_url = self.hc_relay_url;
        config.connect_holochain = self.connect_holochain;
        config.run_holochain = self.run_holochain;
        config.log_holochain_metrics = self.log_holochain_metrics;
        config.dynamic_class_tools = self.dynamic_class_tools;
        config.pid_file = self.pid_file;
        Ok(ResolvedRun {
            config,
            unlock_passphrase: secrets.unlock_passphrase,
        })
    }
}

impl ResolvedRun {
    /// `config print`: the config with every default filled in, secrets
    /// replaced by `"<redacted>"` (unset ones stay `null`).
    pub fn redacted_json(&self) -> serde_json::Value {
        let mut config = self.config.clone();
        config.prepare();
        let mut json = config.redacted_json();
        json["unlockPassphrase"] = match self.unlock_passphrase {
            Some(_) => REDACTED.into(),
            None => serde_json::Value::Null,
        };
        json
    }
}

/// `std::env::var`, as the lookup [`RunArgs::resolve`] takes.
pub fn process_env(name: &str) -> Option<String> {
    std::env::var(name).ok()
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use clap::Parser;
    use std::path::Path;
    use std::sync::{Mutex, MutexGuard};

    /// The process environment is global and clap reads it, so every test
    /// that sets an `AD4M_*` variable holds this lock. Poisoning is
    /// irrelevant: it guards variables, not an invariant.
    static ENV: Mutex<()> = Mutex::new(());

    pub(crate) fn lock_env() -> MutexGuard<'static, ()> {
        ENV.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// Sets variables for the life of the guard, removing them on drop.
    struct EnvVars(Vec<&'static str>);

    impl EnvVars {
        fn set(vars: &[(&'static str, &str)]) -> Self {
            for (name, value) in vars {
                std::env::set_var(name, value);
            }
            EnvVars(vars.iter().map(|(name, _)| *name).collect())
        }
    }

    impl Drop for EnvVars {
        fn drop(&mut self) {
            for name in &self.0 {
                std::env::remove_var(name);
            }
        }
    }

    #[derive(Parser)]
    struct Cli {
        #[command(flatten)]
        run: RunArgs,
    }

    fn resolve(argv: &[&str], env: &[(&'static str, &str)]) -> Result<ResolvedRun> {
        let _vars = EnvVars::set(env);
        let mut full = vec!["ad4m-executor"];
        full.extend_from_slice(argv);
        Cli::try_parse_from(full)?.run.resolve(process_env)
    }

    fn scratch_dir(name: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("ad4m-run-config-{}-{}", name, std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn write_config(dir: &Path, json: &str) -> String {
        let path = dir.join("executor-config.json");
        std::fs::write(&path, json).unwrap();
        path.to_string_lossy().into_owned()
    }

    #[cfg(unix)]
    fn write_secret(path: &Path, content: &str) -> String {
        use std::os::unix::fs::PermissionsExt;
        std::fs::write(path, content).unwrap();
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o600)).unwrap();
        path.to_string_lossy().into_owned()
    }

    #[test]
    fn a_flag_beats_the_environment_which_beats_the_file() {
        let _env = lock_env();
        let dir = scratch_dir("precedence");
        let config = write_config(
            &dir,
            r#"{"port": 14400, "mcp_enabled": true, "mcp_port": 14403, "hc_admin_port": 14401,
                "app_data_path": "/from/file"}"#,
        );

        let only_file = resolve(&["--config", &config], &[]).unwrap().config;
        assert_eq!(only_file.port, Some(14400));
        assert_eq!(only_file.mcp_port, Some(14403));
        assert_eq!(only_file.enable_mcp, Some(true));
        assert_eq!(only_file.app_data_path.as_deref(), Some("/from/file"));

        let env_over_file = resolve(
            &["--config", &config],
            &[("AD4M_PORT", "14500"), ("AD4M_ENABLE_MCP", "false")],
        )
        .unwrap()
        .config;
        assert_eq!(env_over_file.port, Some(14500));
        assert_eq!(env_over_file.enable_mcp, Some(false));
        assert_eq!(env_over_file.mcp_port, Some(14403), "untouched by env");

        let flag_over_env = resolve(
            &["--config", &config, "--port", "14600"],
            &[("AD4M_PORT", "14500")],
        )
        .unwrap()
        .config;
        assert_eq!(flag_over_env.port, Some(14600));
        assert_eq!(flag_over_env.hc_admin_port, Some(14401));

        let config_from_env = resolve(&[], &[("AD4M_CONFIG", &config)]).unwrap().config;
        assert_eq!(config_from_env.port, Some(14400));
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn auto_permit_defaults_to_true_without_a_config_file_and_false_with_one() {
        let _env = lock_env();
        let dir = scratch_dir("auto-permit");
        let config = write_config(&dir, "{}");
        assert_eq!(
            resolve(&[], &[]).unwrap().config.auto_permit_cap_requests,
            Some(true)
        );
        assert!(!resolve(&["--config", &config], &[])
            .unwrap()
            .config
            .auto_permit_cap_requests
            .unwrap_or_default());
        assert_eq!(
            resolve(
                &["--config", &config, "--auto-permit-cap-requests", "true"],
                &[]
            )
            .unwrap()
            .config
            .auto_permit_cap_requests,
            Some(true)
        );
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn tls_flags_lay_over_the_file_and_the_port_defaults_to_port_plus_one() {
        let _env = lock_env();
        let dir = scratch_dir("tls");
        let config = write_config(
            &dir,
            r#"{"port": 14400, "multi_user_config": {"enabled": true, "smtp_config": null,
                "tls_config": {"enabled": true, "cert_file_path": "/file/cert.pem",
                               "key_file_path": "/file/key.pem", "tls_port": null}}}"#,
        );
        let tls = resolve(&["--config", &config], &[])
            .unwrap()
            .config
            .tls
            .unwrap();
        assert_eq!(
            (tls.cert_file_path.as_str(), tls.tls_port),
            ("/file/cert.pem", 14401)
        );

        let tls = resolve(
            &["--config", &config, "--tls-cert-file", "/flag/cert.pem"],
            &[("AD4M_TLS_PORT", "14999")],
        )
        .unwrap()
        .config
        .tls
        .unwrap();
        assert_eq!(
            (
                tls.cert_file_path.as_str(),
                tls.key_file_path.as_str(),
                tls.tls_port
            ),
            ("/flag/cert.pem", "/file/key.pem", 14999)
        );

        let no_file = resolve(
            &["--tls-cert-file", "c.pem", "--tls-key-file", "k.pem"],
            &[],
        )
        .unwrap()
        .config;
        assert_eq!(no_file.tls.unwrap().tls_port, 12001);

        let err = resolve(&["--tls-cert-file", "c.pem"], &[])
            .err()
            .expect("a certificate without a key is an error");
        assert!(err.to_string().contains("TLS needs both"), "{err}");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn an_inline_secret_in_the_config_file_stops_run() {
        let _env = lock_env();
        let dir = scratch_dir("inline");
        let config = write_config(&dir, r#"{"admin_credential": "s3cret"}"#);
        let err = resolve(&["--config", &config], &[])
            .err()
            .expect("an inline secret is an error");
        let message = format!("{err:#}");
        assert!(
            message.contains("admin_credential is not allowed"),
            "{message}"
        );
        assert!(!message.contains("s3cret"), "{message}");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn secrets_come_from_files_and_config_print_redacts_them() {
        let _env = lock_env();
        let dir = scratch_dir("secrets");
        let smtp = write_secret(&dir.join("smtp"), "smtp-secret\n");
        let admin = write_secret(&dir.join("admin"), "admin-secret\n");
        let unlock = write_secret(&dir.join("unlock"), "unlock-secret\n");
        let config = write_config(
            &dir,
            &format!(
                r#"{{"multi_user_config": {{"enabled": true, "tls_config": null,
                    "smtp_config": {{"enabled": true, "host": "smtp.example", "port": 465,
                        "username": "u", "from_address": "f", "password_file": "{smtp}"}}}}}}"#
            ),
        );
        let resolved = resolve(
            &["--config", &config],
            &[
                ("AD4M_ADMIN_CREDENTIAL_FILE", &admin),
                ("AD4M_UNLOCK_PASSPHRASE_FILE", &unlock),
            ],
        )
        .unwrap();
        assert_eq!(
            resolved.config.admin_credential.as_deref(),
            Some("admin-secret")
        );
        assert_eq!(
            resolved.config.smtp_config.as_ref().unwrap().password,
            "smtp-secret"
        );
        assert_eq!(resolved.unlock_passphrase.as_deref(), Some("unlock-secret"));

        let printed = serde_json::to_string_pretty(&resolved.redacted_json()).unwrap();
        for secret in ["smtp-secret", "admin-secret", "unlock-secret"] {
            assert!(!printed.contains(secret), "{printed}");
        }
        let json = resolved.redacted_json();
        assert_eq!(json["adminCredential"], REDACTED);
        assert_eq!(json["smtpConfig"]["password"], REDACTED);
        assert_eq!(json["unlockPassphrase"], REDACTED);
        assert_eq!(json["smtpConfig"]["host"], "smtp.example");
        assert!(format!("{:?}", resolved.config).find("secret").is_none());

        // A flag beats AD4M_ADMIN_CREDENTIAL_FILE.
        let resolved = resolve(
            &["--config", &config, "--admin-credential", "from-flag"],
            &[("AD4M_ADMIN_CREDENTIAL_FILE", &admin)],
        )
        .unwrap();
        assert_eq!(
            resolved.config.admin_credential.as_deref(),
            Some("from-flag")
        );

        let err = resolve(
            &["--config", &config],
            &[
                ("AD4M_ADMIN_CREDENTIAL", "a"),
                ("AD4M_ADMIN_CREDENTIAL_FILE", &admin),
            ],
        )
        .err()
        .expect("two sources for one secret is an error");
        assert!(format!("{err:#}").contains("set only one"), "{err:#}");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn a_group_readable_secret_file_stops_run() {
        use std::os::unix::fs::PermissionsExt;
        let _env = lock_env();
        let dir = scratch_dir("mode");
        let unlock = write_secret(&dir.join("unlock"), "unlock-secret\n");
        std::fs::set_permissions(&unlock, std::fs::Permissions::from_mode(0o640)).unwrap();
        let err = resolve(&[], &[("AD4M_UNLOCK_PASSPHRASE_FILE", &unlock)])
            .err()
            .expect("a group-readable secret file is an error");
        let message = format!("{err:#}");
        assert!(message.contains("mode 0640"), "{message}");
        assert!(!message.contains("unlock-secret"), "{message}");
        std::fs::remove_dir_all(&dir).unwrap();
    }
}

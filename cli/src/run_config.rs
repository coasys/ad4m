//! What `ad4m-executor run` starts the executor with. Three sources, a later
//! one winning per setting: the config file (`--config` / `AD4M_CONFIG`),
//! then `AD4M_<FLAG>` environment variables, then flags. clap resolves env
//! against flags; [`RunArgs::resolve`] lays the result over the file.
//! Secrets never come from the file or a flag (bar the legacy
//! `--admin-credential`): see [`ExecutorSecrets::from_env`].
//! `ad4m-executor config print` shows the merged result, secrets redacted.

use anyhow::{bail, Context, Result};
use clap::builder::{BoolValueParser, PathBufValueParser, StringValueParser, TypedValueParser};
use clap::error::ErrorKind;
use clap::parser::ValueSource;
use clap::value_parser;
use rust_executor::config_file::{ExecutorConfigFile, ExecutorSecrets, REDACTED};
use rust_executor::Ad4mConfig;
use std::path::PathBuf;

/// Refuses an empty value, then parses with `P`. clap hands an empty
/// `AD4M_<FLAG>` to the parser as `""`, so without this
/// `AD4M_APP_DATA_PATH=${DATA_DIR}` with `DATA_DIR` unset would lay `""`
/// over the file and put the data directory under the working directory.
/// The error names the variable or the flag the value came from, never the
/// value.
#[derive(Clone)]
struct NonEmpty<P>(P);

fn non_empty<P: TypedValueParser>(parser: P) -> NonEmpty<P> {
    NonEmpty(parser)
}

impl<P: TypedValueParser> TypedValueParser for NonEmpty<P> {
    type Value = P::Value;

    fn parse_ref(
        &self,
        cmd: &clap::Command,
        arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        self.parse_ref_(cmd, arg, value, ValueSource::CommandLine)
    }

    fn parse_ref_(
        &self,
        cmd: &clap::Command,
        arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
        source: ValueSource,
    ) -> Result<Self::Value, clap::Error> {
        if !value.is_empty() {
            return self.0.parse_ref_(cmd, arg, value, source);
        }
        let var = arg
            .and_then(|arg| arg.get_env())
            .map(|var| var.to_string_lossy());
        let flag = arg.and_then(|arg| arg.get_long());
        let message = match (source, var, flag) {
            (ValueSource::EnvVariable, Some(var), _) => {
                format!("{var} is set but empty: unset it or give it a value")
            }
            (_, _, Some(flag)) => format!("--{flag} needs a value, not an empty string"),
            _ => "an empty value is not allowed".to_string(),
        };
        Err(clap::Error::raw(ErrorKind::ValueValidation, message).format(&mut cmd.clone()))
    }
}

/// Flags of `ad4m-executor run`. Each also reads `AD4M_<FLAG>`, e.g.
/// `--mcp-port` reads `AD4M_MCP_PORT`. Each goes through [`non_empty`], so
/// an empty variable or flag value is an error rather than a value.
#[derive(clap::Args, Debug)]
pub struct RunArgs {
    /// JSON config file with the launcher's key names (see the docs'
    /// "Executor config file" page). There is no default path.
    #[arg(long, env = "AD4M_CONFIG", value_parser = non_empty(PathBufValueParser::new()))]
    pub config: Option<PathBuf>,
    #[arg(
        short,
        long,
        action,
        env = "AD4M_APP_DATA_PATH",
        value_parser = non_empty(StringValueParser::new())
    )]
    pub app_data_path: Option<String>,
    #[arg(
        short,
        long,
        action,
        env = "AD4M_NETWORK_BOOTSTRAP_SEED",
        value_parser = non_empty(StringValueParser::new())
    )]
    pub network_bootstrap_seed: Option<String>,
    #[arg(
        short,
        long,
        action,
        env = "AD4M_LANGUAGE_LANGUAGE_ONLY",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub language_language_only: Option<bool>,
    #[arg(
        long,
        action,
        env = "AD4M_RUN_DAPP_SERVER",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub run_dapp_server: Option<bool>,
    #[arg(
        short = 'p',
        long = "port",
        action,
        env = "AD4M_PORT",
        value_parser = non_empty(value_parser!(u16))
    )]
    pub port: Option<u16>,
    #[arg(long, action, env = "AD4M_HC_ADMIN_PORT", value_parser = non_empty(value_parser!(u16)))]
    pub hc_admin_port: Option<u16>,
    #[arg(long, action, env = "AD4M_HC_APP_PORT", value_parser = non_empty(value_parser!(u16)))]
    pub hc_app_port: Option<u16>,
    #[arg(
        long,
        action,
        env = "AD4M_HC_USE_BOOTSTRAP",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub hc_use_bootstrap: Option<bool>,
    #[arg(
        long,
        action,
        env = "AD4M_HC_USE_LOCAL_PROXY",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub hc_use_local_proxy: Option<bool>,
    #[arg(long, action, env = "AD4M_HC_USE_MDNS", value_parser = non_empty(BoolValueParser::new()))]
    pub hc_use_mdns: Option<bool>,
    #[arg(long, action, env = "AD4M_HC_USE_PROXY", value_parser = non_empty(BoolValueParser::new()))]
    pub hc_use_proxy: Option<bool>,
    #[arg(long, action, env = "AD4M_HC_PROXY_URL", value_parser = non_empty(StringValueParser::new()))]
    pub hc_proxy_url: Option<String>,
    #[arg(
        long,
        action,
        env = "AD4M_HC_BOOTSTRAP_URL",
        value_parser = non_empty(StringValueParser::new())
    )]
    pub hc_bootstrap_url: Option<String>,
    #[arg(long, action, env = "AD4M_HC_RELAY_URL", value_parser = non_empty(StringValueParser::new()))]
    pub hc_relay_url: Option<String>,
    #[arg(
        short,
        long,
        action,
        env = "AD4M_CONNECT_HOLOCHAIN",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub connect_holochain: Option<bool>,
    #[arg(long, action, env = "AD4M_RUN_HOLOCHAIN", value_parser = non_empty(BoolValueParser::new()))]
    pub run_holochain: Option<bool>,
    /// Admin credential granting full capabilities to whoever presents it.
    /// Required: `run` refuses to start without one unless
    /// --insecure-no-admin-credential is set.
    /// Prefer AD4M_ADMIN_CREDENTIAL_FILE (or the AD4M_ADMIN_CREDENTIAL
    /// environment variable): a flag value is visible to every user on the
    /// host via `ps` and stays in shell history. Must not be empty.
    #[arg(
        long,
        action,
        env = "AD4M_ADMIN_CREDENTIAL",
        hide_env_values = true,
        value_parser = non_empty(StringValueParser::new())
    )]
    pub admin_credential: Option<String>,
    /// For tests and local development only: start without an admin
    /// credential. An empty token then has full admin access, so anyone
    /// who can reach the executor's port controls it.
    /// AD4M_INSECURE_NO_ADMIN_CREDENTIAL enables it only with `true`;
    /// `false`, `0`, `no` or `off` leave it off. Must not be empty.
    #[arg(
        long,
        env = "AD4M_INSECURE_NO_ADMIN_CREDENTIAL",
        action = clap::ArgAction::Set,
        num_args = 0..=1,
        default_value = "false",
        default_missing_value = "true",
        value_parser = non_empty(parse_insecure_flag)
    )]
    pub insecure_no_admin_credential: bool,
    #[arg(long, action, env = "AD4M_LOCALHOST", value_parser = non_empty(BoolValueParser::new()))]
    pub localhost: Option<bool>,
    #[arg(
        long,
        action,
        env = "AD4M_TLS_CERT_FILE",
        value_parser = non_empty(StringValueParser::new())
    )]
    pub tls_cert_file: Option<String>,
    #[arg(long, action, env = "AD4M_TLS_KEY_FILE", value_parser = non_empty(StringValueParser::new()))]
    pub tls_key_file: Option<String>,
    /// HTTPS/WSS port. Default: the RPC port + 1.
    #[arg(long, action, env = "AD4M_TLS_PORT", value_parser = non_empty(value_parser!(u16)))]
    pub tls_port: Option<u16>,
    #[arg(
        long,
        action,
        env = "AD4M_LOG_HOLOCHAIN_METRICS",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub log_holochain_metrics: Option<bool>,
    #[arg(
        long,
        action,
        env = "AD4M_ENABLE_MULTI_USER",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub enable_multi_user: Option<bool>,
    #[arg(long, action, env = "AD4M_ENABLE_MCP", value_parser = non_empty(BoolValueParser::new()))]
    pub enable_mcp: Option<bool>,
    #[arg(long, action, env = "AD4M_MCP_PORT", value_parser = non_empty(value_parser!(u16)))]
    pub mcp_port: Option<u16>,
    /// Grant capability requests without the user confirming them. Default:
    /// true without --config (the CLI's historic behaviour), false with it.
    #[arg(
        long,
        action,
        env = "AD4M_AUTO_PERMIT_CAP_REQUESTS",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub auto_permit_cap_requests: Option<bool>,
    /// Expose dynamic per-class SHACL tools ({class}_create, {class}_set_{prop}, …)
    /// over MCP in addition to the static instance_* tools. Default: false.
    #[arg(
        long,
        num_args = 0..=1,
        default_missing_value = "true",
        env = "AD4M_DYNAMIC_CLASS_TOOLS",
        value_parser = non_empty(BoolValueParser::new())
    )]
    pub dynamic_class_tools: Option<bool>,
    /// Write the executor PID to this file on startup (removed on clean shutdown).
    /// Useful for test harnesses that need targeted process cleanup.
    #[arg(long, env = "AD4M_PID_FILE", value_parser = non_empty(StringValueParser::new()))]
    pub pid_file: Option<String>,
}

/// Only the literal `true` enables the insecure mode; the usual "off"
/// spellings disable it. Anything else is an error rather than a guess. An
/// empty value never gets here: [`non_empty`] names the variable or flag.
fn parse_insecure_flag(value: &str) -> Result<bool, String> {
    match value {
        "true" => Ok(true),
        "false" | "0" | "no" | "off" => Ok(false),
        other => Err(format!(
            "`{other}`: use `true` to enable, or `false`, `0`, `no` or `off` to disable"
        )),
    }
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
        config.insecure_no_admin_credential = Some(self.insecure_no_admin_credential);
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
    use rust_executor::config_file::{MultiUserSettings, TlsSettings};
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

    fn merged(argv: &[&str], env: &[(&'static str, &str)]) -> ExecutorConfigFile {
        let _vars = EnvVars::set(env);
        let mut full = vec!["ad4m-executor"];
        full.extend_from_slice(argv);
        Cli::try_parse_from(full)
            .unwrap()
            .run
            .merged_file()
            .unwrap()
    }

    /// Every config-file key with a flag, one row each: its JSON pointer,
    /// its variable, its flag, and two values unlike the file's (`true`
    /// for the booleans).
    const OVERRIDABLE: &[(&str, &str, &str, &str, &str)] = &[
        (
            "/app_data_path",
            "AD4M_APP_DATA_PATH",
            "--app-data-path",
            "/b",
            "/c",
        ),
        ("/port", "AD4M_PORT", "--port", "14200", "14300"),
        (
            "/hc_admin_port",
            "AD4M_HC_ADMIN_PORT",
            "--hc-admin-port",
            "14201",
            "14301",
        ),
        (
            "/hc_app_port",
            "AD4M_HC_APP_PORT",
            "--hc-app-port",
            "14202",
            "14302",
        ),
        (
            "/localhost",
            "AD4M_LOCALHOST",
            "--localhost",
            "false",
            "true",
        ),
        (
            "/run_dapp_server",
            "AD4M_RUN_DAPP_SERVER",
            "--run-dapp-server",
            "false",
            "true",
        ),
        (
            "/auto_permit_cap_requests",
            "AD4M_AUTO_PERMIT_CAP_REQUESTS",
            "--auto-permit-cap-requests",
            "false",
            "true",
        ),
        (
            "/mcp_enabled",
            "AD4M_ENABLE_MCP",
            "--enable-mcp",
            "false",
            "true",
        ),
        ("/mcp_port", "AD4M_MCP_PORT", "--mcp-port", "14204", "14304"),
        (
            "/multi_user_config/enabled",
            "AD4M_ENABLE_MULTI_USER",
            "--enable-multi-user",
            "false",
            "true",
        ),
        (
            "/multi_user_config/tls_config/cert_file_path",
            "AD4M_TLS_CERT_FILE",
            "--tls-cert-file",
            "/b/cert",
            "/c/cert",
        ),
        (
            "/multi_user_config/tls_config/key_file_path",
            "AD4M_TLS_KEY_FILE",
            "--tls-key-file",
            "/b/key",
            "/c/key",
        ),
        (
            "/multi_user_config/tls_config/tls_port",
            "AD4M_TLS_PORT",
            "--tls-port",
            "14203",
            "14303",
        ),
    ];

    /// For every row of [`OVERRIDABLE`]: the variable alone, the flag alone,
    /// and the flag over the variable each change that key and no other.
    /// The file is a struct literal, so a new `ExecutorConfigFile` field
    /// does not compile here until it is set, and then fails the
    /// completeness check until it has a row (or is listed as flagless).
    #[test]
    fn every_key_is_overridden_by_its_variable_and_flag_and_nothing_else() {
        let _env = lock_env();
        let dir = scratch_dir("precedence-table");
        let file = ExecutorConfigFile {
            app_data_path: Some("/a".into()),
            port: Some(14100),
            hc_admin_port: Some(14101),
            hc_app_port: Some(14102),
            localhost: Some(true),
            run_dapp_server: Some(true),
            auto_permit_cap_requests: Some(true),
            multi_user_config: Some(MultiUserSettings {
                enabled: true,
                smtp_config: None,
                tls_config: Some(TlsSettings {
                    enabled: true,
                    cert_file_path: "/a/cert".into(),
                    key_file_path: "/a/key".into(),
                    tls_port: Some(14103),
                }),
            }),
            log_config: Some([("holochain".to_string(), "warn".to_string())].into()),
            mcp_enabled: Some(true),
            mcp_port: Some(14104),
        };
        let file_json = serde_json::to_value(&file).unwrap();
        let config = write_config(&dir, &file_json.to_string());

        fn leaves(value: &serde_json::Value, at: String, out: &mut Vec<String>) {
            match value.as_object() {
                Some(object) => {
                    for (key, value) in object {
                        leaves(value, format!("{at}/{key}"), out);
                    }
                }
                None => out.push(at),
            }
        }
        let mut keys = Vec::new();
        leaves(&file_json, String::new(), &mut keys);
        let flagless = [
            "/log_config/holochain",
            "/multi_user_config/smtp_config",
            "/multi_user_config/tls_config/enabled",
        ];
        for key in keys.iter().filter(|key| !flagless.contains(&key.as_str())) {
            assert!(
                OVERRIDABLE.iter().any(|row| row.0 == key),
                "{key} has no row in OVERRIDABLE"
            );
        }

        let as_json = |value: &str| {
            serde_json::from_str(value).unwrap_or_else(|_| serde_json::Value::from(value))
        };
        assert_eq!(
            serde_json::to_value(merged(&["--config", &config], &[])).unwrap(),
            file_json
        );
        for &(pointer, var, flag, b, c) in OVERRIDABLE {
            let expect = |value: &str| {
                let mut expected = file_json.clone();
                *expected.pointer_mut(pointer).unwrap() = as_json(value);
                expected
            };
            let actual = |argv: &[&str], env: &[(&'static str, &str)]| {
                let mut full = vec!["--config", config.as_str()];
                full.extend_from_slice(argv);
                serde_json::to_value(merged(&full, env)).unwrap()
            };
            assert_eq!(actual(&[], &[(var, b)]), expect(b), "{var} over the file");
            assert_eq!(actual(&[flag, b], &[]), expect(b), "{flag} over the file");
            assert_eq!(
                actual(&[flag, c], &[(var, b)]),
                expect(c),
                "{flag} over {var}"
            );
        }
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

    /// An empty admin credential would grant every capability to a client
    /// that sends no token, so it stops `run` from every source.
    #[cfg(unix)]
    #[test]
    fn an_empty_admin_credential_stops_run() {
        let _env = lock_env();
        let dir = scratch_dir("empty-admin");
        let empty = write_secret(&dir.join("admin"), "\n");
        let err = resolve(&[], &[("AD4M_ADMIN_CREDENTIAL_FILE", &empty)])
            .err()
            .expect("an empty credential file is an error");
        assert!(format!("{err:#}").contains("is empty"), "{err:#}");

        assert!(
            resolve(&["--admin-credential", ""], &[]).is_err(),
            "an empty --admin-credential is an error"
        );
        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// An empty `AD4M_<FLAG>` (say `AD4M_APP_DATA_PATH=${DATA_DIR}` with
    /// `DATA_DIR` unset) is an error that names the variable, never a value
    /// laid over the file. An empty flag value is an error that names the
    /// flag. The test walks every flag, so a new one is covered too.
    #[test]
    fn an_empty_variable_or_flag_is_an_error_that_names_it() {
        use clap::CommandFactory;
        let _env = lock_env();
        let dir = scratch_dir("empty-value");
        let config = write_config(&dir, r#"{"app_data_path": "/from/file"}"#);
        // Every flag is checked before failing, so the message lists them all.
        let mut wrong = Vec::new();
        let mut check =
            |argv: &[&str], env: &[(&'static str, &str)], name: &str| match resolve(argv, env) {
                Ok(resolved) => wrong.push(format!(
                    "empty {name} accepted, app_data_path {:?}",
                    resolved.config.app_data_path
                )),
                Err(err) if !format!("{err:#}").contains(name) => {
                    wrong.push(format!("empty {name}: {}", format!("{err:#}").trim()))
                }
                Err(_) => {}
            };
        let mut checked = 0;
        for arg in Cli::command().get_arguments() {
            let Some(var) = arg.get_env().and_then(|var| var.to_str()) else {
                continue;
            };
            let var: &'static str = Box::leak(var.to_owned().into_boxed_str());
            let flag = format!("--{}", arg.get_long().unwrap());
            let file: &[&str] = if var == "AD4M_CONFIG" {
                &[]
            } else {
                &["--config", &config]
            };
            check(file, &[(var, "")], var);
            let mut argv = file.to_vec();
            argv.extend([flag.as_str(), ""]);
            check(&argv, &[], &flag);
            checked += 1;
        }
        assert!(wrong.is_empty(), "{}", wrong.join("\n"));
        assert_eq!(checked, 30, "every flag of run has a variable");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// A whitespace-only value is as empty as `""`: `AD4M_APP_DATA_PATH=" "`
    /// would otherwise create a directory named ` ` under the working
    /// directory. Every flag and its variable refuse it, naming themselves.
    #[test]
    fn a_whitespace_only_variable_or_flag_is_an_error_that_names_it() {
        use clap::CommandFactory;
        let _env = lock_env();
        let dir = scratch_dir("blank-value");
        let config = write_config(&dir, r#"{"app_data_path": "/from/file"}"#);
        let mut wrong = Vec::new();
        for arg in Cli::command().get_arguments() {
            let Some(var) = arg.get_env().and_then(|var| var.to_str()) else {
                continue;
            };
            let var: &'static str = Box::leak(var.to_owned().into_boxed_str());
            let flag = format!("--{}", arg.get_long().unwrap());
            let file: &[&str] = if var == "AD4M_CONFIG" {
                &[]
            } else {
                &["--config", &config]
            };
            for blank in [" ", "\t", " \n "] {
                let mut argv = file.to_vec();
                argv.extend([flag.as_str(), blank]);
                for (name, result) in [
                    (var, resolve(file, &[(var, blank)])),
                    (flag.as_str(), resolve(&argv, &[])),
                ] {
                    match result {
                        Ok(resolved) => wrong.push(format!(
                            "{name}={blank:?} accepted, app_data_path {:?}",
                            resolved.config.app_data_path
                        )),
                        Err(err) if !format!("{err:#}").contains(name) => {
                            wrong.push(format!("{name}={blank:?}: {}", format!("{err:#}").trim()))
                        }
                        Err(_) => {}
                    }
                }
            }
        }
        assert!(wrong.is_empty(), "{}", wrong.join("\n"));
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

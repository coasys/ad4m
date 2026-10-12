use env_logger::{Builder, Target};
use log::LevelFilter;
use std::env;
use std::io::Write;
use std::sync::Mutex;

static LOGGER_INITIALIZED: Mutex<bool> = Mutex::new(false);

/// Represents different log levels that can be set
#[derive(Debug, Clone)]
pub enum LogLevel {
    Error,
    Warn,
    Info,
    Debug,
    Trace,
}

impl LogLevel {
    pub fn from_string(level: &str) -> Option<LogLevel> {
        match level.to_lowercase().as_str() {
            "error" => Some(LogLevel::Error),
            "warn" => Some(LogLevel::Warn),
            "info" => Some(LogLevel::Info),
            "debug" => Some(LogLevel::Debug),
            "trace" => Some(LogLevel::Trace),
            _ => None,
        }
    }

    pub fn to_level_filter(&self) -> LevelFilter {
        match self {
            LogLevel::Error => LevelFilter::Error,
            LogLevel::Warn => LevelFilter::Warn,
            LogLevel::Info => LevelFilter::Info,
            LogLevel::Debug => LevelFilter::Debug,
            LogLevel::Trace => LevelFilter::Trace,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            LogLevel::Error => "error".to_string(),
            LogLevel::Warn => "warn".to_string(),
            LogLevel::Info => "info".to_string(),
            LogLevel::Debug => "debug".to_string(),
            LogLevel::Trace => "trace".to_string(),
        }
    }
}

/// Get default log configuration as HashMap - single source of truth
pub fn get_default_log_config() -> std::collections::HashMap<String, String> {
    let mut defaults = std::collections::HashMap::new();
    defaults.insert("holochain".to_string(), "warn".to_string());
    defaults.insert("wasmer_compiler_cranelift".to_string(), "warn".to_string());
    defaults.insert("rust_executor".to_string(), "info".to_string());
    defaults.insert("warp::server".to_string(), "info".to_string());
    defaults
}

/// Build RUST_LOG string from a HashMap of crate -> log level
pub fn build_rust_log_from_config(
    log_config: &std::collections::HashMap<String, String>,
) -> String {
    if log_config.is_empty() {
        // If empty, use defaults
        return build_rust_log_from_config(&get_default_log_config());
    }

    // Start with defaults, then apply user overrides
    let mut final_config = get_default_log_config();
    for (crate_name, level) in log_config {
        final_config.insert(crate_name.clone(), level.clone());
    }

    // Allowed log levels
    const ALLOWED_LEVELS: &[&str] = &["trace", "debug", "info", "warn", "error"];

    // Build the final string, skipping invalid levels
    let mut parts = Vec::new();
    for (crate_name, level) in final_config {
        if ALLOWED_LEVELS.contains(&level.as_str()) {
            parts.push(format!("{}={}", crate_name, level));
        } else {
            log::warn!(
                "Invalid log level '{}' for crate '{}', skipping.",
                level,
                crate_name
            );
        }
    }
    parts.join(",")
}

/// `rust_log` with rmcp held at `warn`, unless it names rmcp itself. rmcp logs every new MCP
/// session's id at `info` (`session::local`, "create new session"), and that id is a bearer:
/// it carries the session's remembered token, which for an admin credential never expires.
/// rmcp logs through `tracing`, whose `log` feature forwards to this logger when no tracing
/// subscriber is set (the CLI), and the launcher's tracing filter reads the same `RUST_LOG`.
/// A global level (`RUST_LOG=info`) would otherwise log it. An operator who names rmcp
/// (`rmcp=debug`) gets what they asked for.
pub fn without_mcp_session_ids(rust_log: &str) -> String {
    let names_rmcp = rust_log.split(',').any(|directive| {
        let target = directive.split('=').next().unwrap_or_default().trim();
        target == "rmcp" || target.starts_with("rmcp::")
    });
    if names_rmcp {
        rust_log.to_string()
    } else if rust_log.trim().is_empty() {
        "rmcp=warn".to_string()
    } else {
        format!("{rust_log},rmcp=warn")
    }
}

/// Set `RUST_LOG`: the environment's if set, else the one `log_config` (or the defaults)
/// builds; either way without MCP session ids ([`without_mcp_session_ids`]).
fn set_rust_log(log_config: Option<&std::collections::HashMap<String, String>>) {
    let rust_log = env::var("RUST_LOG").unwrap_or_else(|_| {
        let default_config = get_default_log_config();
        build_rust_log_from_config(log_config.unwrap_or(&default_config))
    });
    env::set_var("RUST_LOG", without_mcp_session_ids(&rust_log));
}

/// Initialize logging for CLI (stdout)
pub fn init_cli_logging(log_config: Option<&std::collections::HashMap<String, String>>) {
    set_rust_log(log_config);

    let mut initialized = LOGGER_INITIALIZED.lock().unwrap();
    if !*initialized {
        // Use parse_default_env() to respect RUST_LOG environment variable
        if let Err(e) = env_logger::Builder::new().parse_default_env().try_init() {
            eprintln!("Failed to initialize CLI logger: {}", e);
        }
        *initialized = true;
    }
}

/// Create a formatted logger builder for launcher (file output)
fn create_launcher_logger_builder<W: Write + Send + 'static>(target: Box<W>) -> Builder {
    use chrono::Local;
    use colored::Colorize;

    let mut builder = Builder::new();
    builder
        .target(Target::Pipe(target))
        .parse_default_env() // This reads RUST_LOG environment variable
        .format(|buf, record| {
            let level = match record.level() {
                log::Level::Error => record.level().as_str().red(),
                log::Level::Warn => record.level().as_str().yellow(),
                log::Level::Info => record.level().as_str().green(),
                log::Level::Debug => record.level().as_str().blue(),
                log::Level::Trace => record.level().as_str().purple(),
            };
            writeln!(
                buf,
                "[{} {} {}:{}] {}",
                Local::now()
                    .format("%Y-%m-%d %H:%M:%S%.3f")
                    .to_string()
                    .as_str()
                    .dimmed(),
                level,
                record
                    .file()
                    .unwrap_or("unknown")
                    .to_string()
                    .as_str()
                    .dimmed(),
                record.line().unwrap_or(0).to_string().as_str().dimmed(),
                record.args().to_string().as_str().bold(),
            )
        });
    builder
}

/// Initialize logging for launcher (file output)
pub fn init_launcher_logging<W: Write + Send + 'static>(
    target: Box<W>,
    log_config: Option<&std::collections::HashMap<String, String>>,
) -> Result<(), Box<dyn std::error::Error>> {
    set_rust_log(log_config);

    let mut initialized = LOGGER_INITIALIZED.lock().unwrap();
    if *initialized {
        // Logger is already initialized, cannot reinitialize env_logger
        return Ok(());
    }

    if let Err(e) = create_launcher_logger_builder(target).try_init() {
        eprintln!("Failed to initialize launcher logger: {}", e);
    }

    *initialized = true;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A config file's `log_config` sets levels over the defaults: a crate
    /// it does not name keeps its default level.
    #[test]
    fn a_log_config_keeps_the_default_levels_it_does_not_set() {
        let overrides = [("rust_executor".to_string(), "debug".to_string())].into();
        let mut rust_log: Vec<String> = build_rust_log_from_config(&overrides)
            .split(',')
            .map(String::from)
            .collect();
        rust_log.sort();
        assert_eq!(
            rust_log,
            [
                "holochain=warn",
                "rust_executor=debug",
                "warp::server=info",
                "wasmer_compiler_cranelift=warn"
            ]
        );
    }

    /// Whether a logger filtered by `rust_log` logs `target` at `level`.
    fn logs(rust_log: &str, target: &str, level: log::Level) -> bool {
        let filter = env_logger::filter::Builder::new().parse(rust_log).build();
        filter.enabled(&log::Metadata::builder().target(target).level(level).build())
    }

    const RMCP_SESSIONS: &str = "rmcp::transport::streamable_http_server::session::local";

    /// rmcp logs each new MCP session's id at info; the id is a bearer for the session's
    /// remembered token. A global level must not log it, and the executor's own info stays.
    #[test]
    fn mcp_session_ids_are_not_logged_at_a_global_level() {
        for rust_log in ["info", "debug", "warn,rust_executor=info", ""] {
            let quiet = without_mcp_session_ids(rust_log);
            assert!(
                !logs(&quiet, RMCP_SESSIONS, log::Level::Info),
                "{rust_log:?} -> {quiet:?} logs MCP session ids"
            );
            assert!(logs(&quiet, RMCP_SESSIONS, log::Level::Warn), "{quiet:?}");
        }
        let quiet = without_mcp_session_ids("info");
        assert!(logs(&quiet, "rust_executor::mcp", log::Level::Info));
        let defaults = without_mcp_session_ids(&build_rust_log_from_config(
            &std::collections::HashMap::new(),
        ));
        assert!(!logs(&defaults, RMCP_SESSIONS, log::Level::Info));
        assert!(logs(&defaults, "rust_executor", log::Level::Info));
    }

    /// An operator who names rmcp gets what they asked for.
    #[test]
    fn naming_rmcp_keeps_its_level() {
        assert_eq!(
            without_mcp_session_ids("info,rmcp=debug"),
            "info,rmcp=debug"
        );
        assert!(logs(
            &without_mcp_session_ids("rmcp::transport=info"),
            RMCP_SESSIONS,
            log::Level::Info
        ));
    }
}

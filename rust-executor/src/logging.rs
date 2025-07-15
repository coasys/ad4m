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
    defaults.insert("warp".to_string(), "info".to_string());
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
            log::warn!("Invalid log level '{}' for crate '{}', skipping.", level, crate_name);
        }
    }
    parts.join(",")
}

/// Initialize logging for CLI (stdout)
pub fn init_cli_logging(log_config: Option<&std::collections::HashMap<String, String>>) {
    // Only set RUST_LOG if it's not already set in environment
    if env::var("RUST_LOG").is_err() {
        let default_config = get_default_log_config();
        let config = log_config.unwrap_or(&default_config);
        let rust_log = build_rust_log_from_config(config);
        env::set_var("RUST_LOG", &rust_log);
    }

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
    // Only set RUST_LOG if it's not already set in environment
    if env::var("RUST_LOG").is_err() {
        let default_config = get_default_log_config();
        let config = log_config.unwrap_or(&default_config);
        let rust_log = build_rust_log_from_config(config);
        env::set_var("RUST_LOG", &rust_log);
    }

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


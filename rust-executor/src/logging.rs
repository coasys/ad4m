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

/// Initialize logging for CLI (stdout)
pub fn init_cli_logging(log_level: Option<LogLevel>) {
    let level = log_level.as_ref().unwrap_or(&LogLevel::Info);

    // Only set RUST_LOG if it's not already set in environment
    if env::var("RUST_LOG").is_err() {
        let rust_log = build_rust_log_string(level);
        env::set_var("RUST_LOG", &rust_log);
    }

    let mut initialized = LOGGER_INITIALIZED.lock().unwrap();
    if !*initialized {
        // Use parse_default_env() to respect RUST_LOG environment variable
        Builder::new().parse_default_env().init();
        *initialized = true;
    }
}

/// Initialize logging for launcher (file output)
pub fn init_launcher_logging<W: Write + Send + 'static>(
    target: Box<W>,
    log_level: Option<LogLevel>,
) -> Result<(), Box<dyn std::error::Error>> {
    use chrono::Local;
    use colored::Colorize;

    let level = log_level.as_ref().unwrap_or(&LogLevel::Info);

    // Only set RUST_LOG if it's not already set in environment
    if env::var("RUST_LOG").is_err() {
        let rust_log = build_rust_log_string(level);
        env::set_var("RUST_LOG", &rust_log);
    }

    let mut initialized = LOGGER_INITIALIZED.lock().unwrap();
    if *initialized {
        // Logger is already initialized, we need to reinitialize it
        return reinitialize_launcher_logging(target, log_level);
    }

    Builder::new()
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
        })
        .init();

    *initialized = true;
    Ok(())
}

/// Reinitialize logging for launcher (file output) - this is the key function for runtime changes
pub fn reinitialize_launcher_logging<W: Write + Send + 'static>(
    target: Box<W>,
    log_level: Option<LogLevel>,
) -> Result<(), Box<dyn std::error::Error>> {
    use chrono::Local;
    use colored::Colorize;

    let level = log_level.as_ref().unwrap_or(&LogLevel::Info);

    // Only set RUST_LOG if it's not already set in environment
    if env::var("RUST_LOG").is_err() {
        let rust_log = build_rust_log_string(level);
        env::set_var("RUST_LOG", &rust_log);
    }

    // Create a new logger with the updated configuration
    Builder::new()
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
        })
        .init();

    // Note: env_logger doesn't support runtime reconfiguration well
    // Runtime changes will require process restart or different approach

    Ok(())
}

/// Get the current log level from environment or return default
pub fn get_current_log_level() -> LogLevel {
    if let Ok(rust_log) = env::var("RUST_LOG") {
        // Parse the RUST_LOG string to extract the rust_executor log level
        if let Some(level) = extract_rust_executor_level(&rust_log) {
            return level;
        }
    }
    LogLevel::Info // Default
}

/// Build the RUST_LOG string with the specified log level (fallback for simple case)
fn build_rust_log_string(level: &LogLevel) -> String {
    let level_str = level.to_string();
    format!(
        "holochain=warn,wasmer_compiler_cranelift=warn,rust_executor={},warp=info,warp::server=info",
        level_str
    )
}

/// Build RUST_LOG string from a HashMap of crate -> log level
pub fn build_rust_log_from_config(
    log_config: &std::collections::HashMap<String, String>,
) -> String {
    if log_config.is_empty() {
        // Default configuration if none specified
        return "holochain=warn,wasmer_compiler_cranelift=warn,rust_executor=info,warp=info,warp::server=info".to_string();
    }

    let mut parts = Vec::new();
    for (crate_name, level) in log_config {
        parts.push(format!("{}={}", crate_name, level));
    }
    parts.join(",")
}

/// Extract the rust_executor log level from RUST_LOG string
fn extract_rust_executor_level(rust_log: &str) -> Option<LogLevel> {
    for part in rust_log.split(',') {
        if part.trim().starts_with("rust_executor=") {
            let level_str = part.trim().split('=').nth(1)?;
            return LogLevel::from_string(level_str);
        }
    }
    None
}

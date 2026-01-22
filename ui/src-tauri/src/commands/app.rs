extern crate remove_dir_all;
use crate::app_state::{
    AgentConfigDir, LauncherState, MultiUserConfig, SmtpConfig, SmtpConfigDto, TlsConfig,
};
use crate::util::create_tray_message_windows;
use crate::{config::data_path, get_main_window};
use rust_executor::logging::{build_rust_log_from_config, get_default_log_config, LogLevel};
use std::collections::HashMap;

use remove_dir_all::*;

use dirs::home_dir;
use tauri::Size;
use tauri::{LogicalSize, Manager};
use tauri_plugin_positioner::{Position, WindowExt};

#[tauri::command]
pub fn close_application(app_handle: tauri::AppHandle) {
    app_handle.exit(0);
    std::process::exit(0);
}

#[tauri::command]
pub fn close_main_window(app_handle: tauri::AppHandle) {
    let window = get_main_window(&app_handle);
    if let Ok(true) = window.is_visible() {
        let _ = window.hide();
    }
}

#[tauri::command]
pub fn show_main_window(app_handle: tauri::AppHandle) {
    let window = get_main_window(&app_handle);
    let _ = window.show();
    let _ = window.set_focus();
}

#[tauri::command]
pub fn open_tray(app_handle: tauri::AppHandle) {
    let window = get_main_window(&app_handle);
    if let Ok(true) = window.is_visible() {
        let _ = window.hide();
    } else {
        let _ = window.show();
        let _ = window.move_window(Position::TopRight);
        let _ = window.set_size(Size::Logical(LogicalSize {
            width: 400.0,
            height: 700.0,
        }));
        let _ = window.set_decorations(false);
        let _ = window.set_always_on_top(true);
    }
}

#[tauri::command]
pub fn add_app_agent_state(agent: AgentConfigDir) {
    let mut state = LauncherState::load().unwrap();

    let mut new_agent = agent.clone();

    new_agent.path = home_dir().unwrap().join(agent.path);

    state.add_agent(new_agent);

    state.save().unwrap();
}

#[tauri::command]
pub fn remove_app_agent_state(agent: AgentConfigDir) {
    let mut state = LauncherState::load().unwrap();

    state.remove_agent(agent.clone());

    state.save().unwrap();
}

#[tauri::command]
pub fn set_selected_agent(agent: AgentConfigDir) {
    let mut state = LauncherState::load().unwrap();

    state.selected_agent = Some(agent);

    state.save().unwrap();
}

#[tauri::command]
pub fn get_app_agent_list() -> Option<String> {
    let state = LauncherState::load().unwrap();

    serde_json::to_string(&state).ok()
}

#[tauri::command]
#[cfg(feature = "custom-protocol")]
pub fn open_tray_message(app_handle: tauri::AppHandle) {
    match app_handle.get_webview_window("TrayMessage") {
        Some(tray_window) => {
            if let Ok(true) = tray_window.is_visible() {
                let _ = tray_window.hide();
            } else {
                let _ = tray_window.show();
            }
        }
        None => {
            create_tray_message_windows(&app_handle);
        }
    }
}

#[tauri::command]
#[cfg(not(feature = "custom-protocol"))]
pub fn open_tray_message(_app_handle: tauri::AppHandle) {
    println!("In debug mode won't open tray message");
}

#[tauri::command]
pub fn get_data_path() -> String {
    data_path().to_string_lossy().into_owned()
}

#[tauri::command]
pub fn clear_state(app_handle: tauri::AppHandle) {
    let _ = remove_dir_all(data_path());

    app_handle.restart();
}

#[tauri::command]
pub fn open_dapp() {
    if webbrowser::open("http://localhost:8080/").is_err() {
        println!("Failed to open URL");
    }
}

#[tauri::command]
pub fn get_log_config() -> HashMap<String, String> {
    let state = LauncherState::load().ok();
    if let Some(state) = state {
        if let Some(user_config) = state.log_config {
            // Start with defaults, then apply user overrides
            let mut final_config = get_default_log_config();

            // Apply user overrides
            for (crate_name, level) in user_config {
                final_config.insert(crate_name, level);
            }

            return final_config;
        }
    }

    // Default log configuration
    get_default_log_config()
}

#[tauri::command]
pub fn set_log_config(config: HashMap<String, String>) -> Result<(), String> {
    // Validate the log levels
    for (crate_name, level) in &config {
        LogLevel::from_string(level)
            .ok_or_else(|| format!("Invalid log level '{}' for crate '{}'", level, crate_name))?;
    }

    // Load current state
    let mut state =
        LauncherState::load().map_err(|e| format!("Failed to load launcher state: {}", e))?;

    // Update the log config in state
    state.log_config = Some(config.clone());

    // Save the updated state
    state
        .save()
        .map_err(|e| format!("Failed to save launcher state: {}", e))?;

    // Update RUST_LOG environment variable for current session
    let rust_log = build_rust_log_from_config(&config);
    std::env::set_var("RUST_LOG", &rust_log);

    // Note: Full effect requires restart since env_logger doesn't support runtime reconfiguration
    Ok(())
}

#[tauri::command]
pub fn get_tls_config() -> Option<TlsConfig> {
    let state = LauncherState::load().ok()?;

    // Prefer multi_user_config, fallback to deprecated tls_config for backwards compatibility
    if let Some(multi_user_config) = &state.multi_user_config {
        multi_user_config.tls_config.clone()
    } else {
        state.tls_config
    }
}

#[tauri::command]
pub fn set_tls_config(config: TlsConfig) -> Result<(), String> {
    // Validate file paths exist if TLS is enabled
    if config.enabled {
        if !std::path::Path::new(&config.cert_file_path).exists() {
            return Err(format!(
                "Certificate file not found: {}",
                config.cert_file_path
            ));
        }
        if !std::path::Path::new(&config.key_file_path).exists() {
            return Err(format!("Key file not found: {}", config.key_file_path));
        }
    }

    // Load current state
    let mut state =
        LauncherState::load().map_err(|e| format!("Failed to load launcher state: {}", e))?;

    // Get or create multi_user_config
    // IMPORTANT: Preserve existing SMTP config if migrating
    let mut multi_user_config =
        state
            .multi_user_config
            .clone()
            .unwrap_or_else(|| MultiUserConfig {
                enabled: true,
                smtp_config: None,
                tls_config: None,
            });

    // Update TLS config in multi_user_config (new location)
    multi_user_config.tls_config = Some(config.clone());
    state.multi_user_config = Some(multi_user_config);

    // Also update deprecated field for backwards compatibility
    state.tls_config = Some(config);

    // Save updated state
    state
        .save()
        .map_err(|e| format!("Failed to save launcher state: {}", e))?;

    Ok(())
}

#[tauri::command]
pub async fn test_smtp_config(config: SmtpConfigDto, test_email: String) -> Result<bool, String> {
    use lettre::transport::smtp::authentication::Credentials;
    use lettre::{Message, SmtpTransport, Transport};

    // Build test email
    let email = Message::builder()
        .from(config.from_address.parse().map_err(|e| format!("Invalid from address: {}", e))?)
        .to(test_email.parse().map_err(|e| format!("Invalid to address: {}", e))?)
        .subject("AD4M SMTP Configuration Test")
        .body("This is a test email from your AD4M instance. Your SMTP configuration is working correctly!".to_string())
        .map_err(|e| format!("Failed to build email: {}", e))?;

    // Build SMTP transport based on port
    let creds = Credentials::new(config.username.clone(), config.password.clone());

    let mailer = if config.port == 465 {
        // Port 465 uses implicit TLS (wrapper mode)
        use lettre::transport::smtp::client::{Tls, TlsParameters};

        let tls_params = TlsParameters::builder(config.host.clone())
            .dangerous_accept_invalid_certs(false)
            .build()
            .map_err(|e| format!("Failed to create TLS parameters: {}", e))?;

        SmtpTransport::builder_dangerous(&config.host)
            .port(config.port)
            .credentials(creds)
            .tls(Tls::Wrapper(tls_params))
            .build()
    } else if config.port == 25 {
        // Port 25 usually plain text or opportunistic TLS
        use lettre::transport::smtp::client::Tls;
        SmtpTransport::builder_dangerous(&config.host)
            .port(config.port)
            .credentials(creds)
            .tls(Tls::None)
            .build()
    } else {
        // Port 587, 2525, etc. use STARTTLS
        use lettre::transport::smtp::client::{Tls, TlsParameters};

        let tls_params = TlsParameters::builder(config.host.clone())
            .dangerous_accept_invalid_certs(false)
            .build()
            .map_err(|e| format!("Failed to create TLS parameters: {}", e))?;

        SmtpTransport::builder_dangerous(&config.host)
            .port(config.port)
            .credentials(creds)
            .tls(Tls::Required(tls_params))
            .build()
    };

    // Send email in blocking task
    tokio::task::spawn_blocking(move || {
        mailer
            .send(&email)
            .map_err(|e| format!("Failed to send email: {}", e))
    })
    .await
    .map_err(|e| format!("Task error: {}", e))??;

    Ok(true)
}

#[tauri::command]
pub fn get_smtp_config() -> Option<SmtpConfigDto> {
    let state = LauncherState::load().ok()?;
    state
        .multi_user_config?
        .smtp_config
        .map(|config| SmtpConfigDto::from(&config))
}

#[tauri::command]
pub fn set_smtp_config(config: SmtpConfigDto) -> Result<(), String> {
    // Validate SMTP config only if enabled
    if config.enabled {
        if config.host.is_empty() {
            return Err("SMTP host cannot be empty".to_string());
        }
        if config.username.is_empty() {
            return Err("SMTP username cannot be empty".to_string());
        }
        if config.from_address.is_empty() {
            return Err("SMTP from address cannot be empty".to_string());
        }
    }

    // Convert DTO to SmtpConfig (password will be encrypted on save)
    let smtp_config: SmtpConfig = config.into();

    // Load current state
    let mut state =
        LauncherState::load().map_err(|e| format!("Failed to load launcher state: {}", e))?;

    // Get or create multi_user_config
    // IMPORTANT: Preserve existing TLS config from deprecated field if migrating
    let mut multi_user_config = state.multi_user_config.clone().unwrap_or_else(|| {
        MultiUserConfig {
            enabled: true,
            smtp_config: None,
            tls_config: state.tls_config.clone(), // Preserve existing TLS config!
        }
    });

    // Update SMTP config
    multi_user_config.smtp_config = Some(smtp_config);
    state.multi_user_config = Some(multi_user_config);

    // Save updated state
    state
        .save()
        .map_err(|e| format!("Failed to save launcher state: {}", e))?;

    Ok(())
}

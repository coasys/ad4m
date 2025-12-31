extern crate remove_dir_all;
use crate::app_state::{AgentConfigDir, LauncherState, TlsConfig};
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
    state.tls_config
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
            return Err(format!(
                "Key file not found: {}",
                config.key_file_path
            ));
        }
    }

    // Load current state
    let mut state = LauncherState::load()
        .map_err(|e| format!("Failed to load launcher state: {}", e))?;

    // Update TLS config
    state.tls_config = Some(config);

    // Save updated state
    state
        .save()
        .map_err(|e| format!("Failed to save launcher state: {}", e))?;

    Ok(())
}

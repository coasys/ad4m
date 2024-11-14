extern crate remove_dir_all;
use crate::app_state::{AgentList, LauncherState};
use crate::util::create_tray_message_windows;
use crate::{config::data_path, get_main_window};

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
pub fn add_app_agent_state(agent: AgentList) {
    let mut state = LauncherState::load().unwrap();

    let mut new_agent = agent.clone();

    new_agent.path = home_dir().unwrap().join(agent.path);

    state.add_agent(new_agent);

    state.save().unwrap();
}

#[tauri::command]
pub fn remove_app_agent_state(agent: AgentList) {
    let mut state = LauncherState::load().unwrap();

    state.remove_agent(agent.clone());

    state.save().unwrap();
}

#[tauri::command]
pub fn set_selected_agent(agent: AgentList) {
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
pub fn open_tray_message(app_handle: tauri::AppHandle) {
    println!("In debug mode won't open tray message");
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

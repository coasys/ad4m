extern crate remove_dir_all;
use crate::{config::data_path, get_main_window, util::find_and_kill_processes};

use remove_dir_all::*;

use tauri::LogicalSize;
use tauri::Size;
use tauri_plugin_positioner::{Position, WindowExt};

#[tauri::command]
pub fn close_application(app_handle: tauri::AppHandle) {
    app_handle.exit(0)
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
pub fn clear_state(app_handle: tauri::AppHandle) {
    find_and_kill_processes("ad4m-host");

    find_and_kill_processes("holochain");

    let _ = remove_dir_all(data_path());

    app_handle.restart();
}

extern crate remove_dir_all;
use std::time::{Duration, SystemTime};

use crate::Payload;
use crate::{config::data_path, get_main_window, util::find_and_kill_processes};

use remove_dir_all::*;

use tauri::{LogicalSize, Manager};
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
pub fn open_tray_message(app_handle: tauri::AppHandle) {
    let tray_window = app_handle.get_window("TrayMessage").unwrap();
    let _ = tray_window.show();

    let _ = tray_window.emit("tray_message_open", Payload {message: "".to_string()});

    let seconds = Duration::from_secs(5);

    let start = SystemTime::now();
    loop {
        std::thread::sleep(Duration::new(5, 0));

        match start.elapsed() {
            Ok(elapsed) if elapsed > seconds => {
                let _ = tray_window.hide();
                return;
            }
            _ => (),
        }
    }
}

#[tauri::command]
pub fn clear_state(app_handle: tauri::AppHandle) {
    find_and_kill_processes("ad4m-host");

    find_and_kill_processes("holochain");

    let _ = remove_dir_all(data_path());

    app_handle.restart();
}

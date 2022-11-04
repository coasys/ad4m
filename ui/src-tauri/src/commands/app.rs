extern crate remove_dir_all;
use remove_dir_all::*;

use crate::{get_main_window, config::{data_path, binary_path}, util::find_and_kill_processes};


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
pub fn clear_state(app_handle: tauri::AppHandle) {
  find_and_kill_processes("ad4m");

  find_and_kill_processes("holochain");

  find_and_kill_processes("lair-keystore");
  
  remove_dir_all(data_path());

  app_handle.restart();
}
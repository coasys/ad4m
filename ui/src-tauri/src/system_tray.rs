use tauri::{
    AppHandle, CustomMenuItem, Manager, SystemTray, SystemTrayMenu, SystemTrayMenuItem, Wry,
};
use crate::util::find_and_kill_processes;
use crate::create_main_window;
use crate::Payload;
use crate::config::executor_port_path;
use std::fs::remove_file;
use std::env;

pub fn build_system_tray() -> SystemTray {
    if env::consts::OS == "linux" {
        let toggle_window = CustomMenuItem::new("toggle_window".to_string(), "Show Window");
        let quit = CustomMenuItem::new("quit".to_string(), "Quit");
    
        let sys_tray_menu = SystemTrayMenu::new()
            .add_item(toggle_window)
            .add_native_item(SystemTrayMenuItem::Separator)
            .add_item(quit);
    
        SystemTray::new().with_menu(sys_tray_menu)
    } else {
        SystemTray::new()
    }
}

pub fn handle_system_tray_event(app: &AppHandle<Wry>, event_id: String) {
    match event_id.as_str() {
        "toggle_window" => {
            let ad4min_window = app.get_window("AD4MIN");

            if let Some(window) = ad4min_window {
                if let Ok(true) = window.is_visible() {
                    let _ = window.hide();
                } else {
                    window.show().unwrap();
                    window.set_focus().unwrap();                
                }
            } else {                
                create_main_window(app);
                let main = app.get_window("AD4MIN").unwrap();
                main.emit("ready", Payload { message: "ad4m-executor is ready".into() }).unwrap();
            }
        }
        "quit" => {
            find_and_kill_processes("ad4m");

            find_and_kill_processes("holochain");

            find_and_kill_processes("lair-keystore");

            let _ = remove_file(executor_port_path());

            app.exit(0);
        }
        _ => log::error!("Event is not defined."),
    }
}
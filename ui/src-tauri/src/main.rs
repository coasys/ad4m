#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use tauri::LogicalSize;
use tauri::Size;
use std::sync::Mutex;
extern crate remove_dir_all;
use remove_dir_all::*;

use config::holochain_binary_path;
use config::app_url;
use logs::setup_logs;
use menu::build_menu;
use system_tray::{ build_system_tray, handle_system_tray_event };
use tauri::{
    AppHandle,
    api::process::{Command, CommandEvent},
    RunEvent, SystemTrayEvent,
    Window
};
use tauri_plugin_positioner::{ WindowExt, Position, on_tray_event};
use tokio::sync::broadcast;
use uuid::Uuid;

mod config;
mod util;
mod logs;
mod system_tray;
mod menu;
mod commands;

use tauri::api::dialog;
use tauri::Manager;
use crate::commands::proxy::{get_proxy, setup_proxy, stop_proxy};
use crate::commands::state::{get_port, request_credential};
use crate::commands::app::{close_application, close_main_window, clear_state};
use crate::config::data_path;
use crate::util::find_port;
use crate::menu::{handle_menu_event, open_logs_folder};
use crate::util::{find_and_kill_processes, create_main_window, save_executor_port};

// the payload type must implement `Serialize` and `Clone`.
#[derive(Clone, serde::Serialize)]
struct Payload {
  message: String,
}

pub struct ProxyState(Mutex<Option<ProxyService>>);

pub struct ProxyService {
    endpoint: String,
    shutdown_signal: broadcast::Sender<()>,
}

pub struct AppState {
    graphql_port: u16,
    req_credential: String,
}

fn main() {
    if data_path().exists() {
        if !data_path().join("ad4m").join("agent.json").exists() {
            remove_dir_all(data_path());
        }
    }
    
    if let Err(err) = setup_logs() {
        println!("Error setting up the logs: {:?}", err);
    }

    let free_port = find_port(12000, 13000);

    log::info!("Free port: {:?}", free_port);
    
    save_executor_port(free_port);

    find_and_kill_processes("ad4m");

    find_and_kill_processes("holochain");

    find_and_kill_processes("lair-keystore");
    if !holochain_binary_path().exists() {
        log::info!("init command by copy holochain binary");
        let status = Command::new_sidecar("ad4m")
            .expect("Failed to create ad4m command")
            .args(["init"])
            .status()
            .expect("Failed to run ad4m init");
        assert!(status.success());
    }

    let req_credential = Uuid::new_v4().to_string();

    let state = AppState {
        graphql_port: free_port,
        req_credential: req_credential.clone(),
    };

    let builder_result = tauri::Builder::default()
        .plugin(tauri_plugin_positioner::init())
        .manage(state)
        .manage(ProxyState(Default::default()))
        .menu(build_menu())
        .on_menu_event(|event| handle_menu_event(event.menu_item_id(), event.window()))
        .system_tray(build_system_tray())
        .invoke_handler(tauri::generate_handler![
            get_port,
            request_credential,
            setup_proxy,
            get_proxy,
            stop_proxy,
            close_application,
            close_main_window,
            clear_state
        ])
        .setup(move |app| {
            let splashscreen = app.get_window("splashscreen").unwrap();

            let splashscreen_clone = splashscreen.clone();

            let _id = splashscreen.listen("copyLogs", |event| {
                log::info!("got window event-name with payload {:?} {:?}", event, event.payload());

                open_logs_folder();
            });

            let (mut rx, _child) = Command::new_sidecar("ad4m")
            .expect("Failed to create ad4m command")
            .args([
                "serve",
                "--port", &free_port.to_string(),
                "--reqCredential", &req_credential,
            ])
            .spawn()
            .expect("Failed to spawn ad4m serve");

            let handle = app.handle().clone();
    
            tauri::async_runtime::spawn(async move {
                while let Some(event) = rx.recv().await {
                    match event.clone() {
                        CommandEvent::Stdout(line) => {
                            log::info!("{}", line);

                            if line.contains("GraphQL server started, Unlock the agent to start holohchain") {
                                let url = app_url();
                                log::info!("Executor started on: {:?}", url);
                                let _ = splashscreen_clone.hide();
                                let main = get_main_window(&handle);
                                main.emit("ready", Payload { message: "ad4m-executor is ready".into() }).unwrap();
                            }
                        },
                        CommandEvent::Stderr(line) => log::error!("{}", line),
                        CommandEvent::Terminated(line) => {
                            log::info!("Terminated {:?}", line);
                            let main = get_main_window(&handle);

                            if let Ok(true) = &splashscreen_clone.is_visible() {
                                log_error(&splashscreen_clone, "Something went wrong while starting ad4m-executor please check the logs");
                            }

                            if let Ok(true) = main.is_visible() {
                                log_error(&main, "There was an error with AD4MIN. Restarting may fix this, otherwise please contact the AD4M team for support.");
                            }

                            log::info!("Terminated {:?}", line);
                        },
                        CommandEvent::Error(line) => log::info!("Error {:?}", line),
                        _ => log::error!("{:?}", event),
                    }
                }
            });

            Ok(())
        })
        .on_system_tray_event(move |app, event| {
            on_tray_event(app, &event);
            match event {
                SystemTrayEvent::LeftClick { position: _, size: _, .. } => {
                    let window = get_main_window(&app);
                    let _ = window.set_size(Size::Logical(LogicalSize { width: 400.0, height: 700.0 }));
                    let _ = window.set_decorations(false);
                    let _ = window.set_always_on_top(true);
                    let _ = window.move_window(Position::TrayCenter);

                    if let Ok(true) = window.is_visible() {
                        let _ = window.hide();
                    } else {
                        window.show().unwrap();
                        window.set_focus().unwrap();                
                    }
                },
                SystemTrayEvent::MenuItemClick { id, .. } => {
                    handle_system_tray_event(app, id)
                },
                _ => {}
            }
        })
        .build(tauri::generate_context!());

    match builder_result {
        Ok(builder) => {
            builder.run(|_app_handle, event| {
                match event {
                    RunEvent::ExitRequested { api, .. } => {
                        api.prevent_exit();
                    },
                    _ => {}
                }
            });
        }
        Err(err) => log::error!("Error building the app: {:?}", err),
    }
}

fn get_main_window(handle: &AppHandle) -> Window {
    let main = handle.get_window("AD4MIN");
    if let Some(window) = main {
        window
    } else {
        create_main_window(&handle);
        let main = handle.get_window("AD4MIN");                
        main.expect("Couldn't get main window right after creating it")
    }
}

fn log_error(window: &Window, message: &str) {
    dialog::message(
        Some(window), 
        "Error", 
        message
    );
}
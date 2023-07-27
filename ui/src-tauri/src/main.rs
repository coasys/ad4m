#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use tracing::{info, error};
use rust_executor::Ad4mConfig;
use tauri::LogicalSize;
use tauri::Size;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt::format;
use std::env;
use std::fs;
use std::fs::File;
use std::sync::Arc;
use std::sync::Mutex;
extern crate remove_dir_all;
use remove_dir_all::*;

use config::app_url;
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
mod system_tray;
mod menu;
mod commands;

use tauri::api::dialog;
use tauri::Manager;
use crate::commands::proxy::{get_proxy, login_proxy, setup_proxy, stop_proxy};
use crate::commands::state::{get_port, request_credential};
use crate::commands::app::{close_application, close_main_window, clear_state, open_tray, open_tray_message};
use crate::config::data_path;
use crate::config::log_path;
use crate::util::create_tray_message_windows;
use crate::util::find_port;
use crate::menu::{handle_menu_event, open_logs_folder};
use crate::util::has_processes_running;
use crate::util::{find_and_kill_processes, create_main_window, save_executor_port};
use std::io::{self, Write};
use tracing_subscriber::{fmt::format::FmtSpan, FmtSubscriber};

// the payload type must implement `Serialize` and `Clone`.
#[derive(Clone, serde::Serialize)]
struct Payload {
  message: String,
}

pub struct ProxyState(Mutex<ProxyService>);

#[derive(Default)]
pub struct ProxyService {
    credential: Option<String>,
    endpoint: Option<String>,
    shutdown_signal: Option<broadcast::Sender<()>>,
}

pub struct AppState {
    graphql_port: u16,
    req_credential: String,
}

fn main() {
    env::set_var("RUST_LOG", "rust_executor=info,error,warn,debugad4m-launcher=info,warn,error");

    let _ = fs::remove_file(log_path());

    let file = File::create(log_path()).unwrap();
    let file = Arc::new(Mutex::new(file));

    let format = format::debug_fn(move |writer, _field, value| {
        let _ = writeln!(file.lock().unwrap(), "{:?}" value);
        write!(writer, "{:?}", value)
    });

    let filter = EnvFilter::from_default_env();

    let subscriber = tracing_subscriber::fmt()
        .with_env_filter(filter)
        .fmt_fields(format)
        .finish();

    tracing::subscriber::set_global_default(subscriber)
        .expect("Failed to set tracing subscriber");

    let app_name = if std::env::consts::OS == "windows" { "AD4M.exe" } else { "AD4M" };
    if has_processes_running(app_name) > 1 {
        println!("AD4M is already running");
        return;
    }

    if data_path().exists() && !data_path().join("ad4m").join("agent.json").exists() {
        let _ = remove_dir_all(data_path());
    }

    let free_port = find_port(12000, 13000);

    info!("Free port: {:?}", free_port);

    save_executor_port(free_port);

    find_and_kill_processes("ad4m-host");

    find_and_kill_processes("holochain");

    match rust_executor::init::init(
        Some(String::from(data_path().to_str().unwrap())),
         None
        ) {
        Ok(()) => {
            println!("Ad4m initialized sucessfully");
        },
        Err(e) => {
            println!("Ad4m initialization failed: {}", e);
            std::process::exit(1);
        }
    };

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
            login_proxy,
            setup_proxy,
            get_proxy,
            stop_proxy,
            close_application,
            close_main_window,
            clear_state,
            open_tray,
            open_tray_message
        ])
        .setup(move |app| {
            // Hides the dock icon
            #[cfg(target_os = "macos")]
            app.set_activation_policy(tauri::ActivationPolicy::Accessory);

            let splashscreen = app.get_window("splashscreen").unwrap();

            let _id = splashscreen.listen("copyLogs", |event| {
                info!("got window event-name with payload {:?} {:?}", event, event.payload());

                open_logs_folder();
            });

            let mut config = Ad4mConfig::default();
            config.admin_credential = Some(req_credential.to_string());
            config.app_data_path = Some(String::from(data_path().to_str().unwrap()));
            config.gql_port = Some(free_port);
            config.network_bootstrap_seed = None;

            let handle = app.handle();

            async fn spawn_executor(config: Ad4mConfig, splashscreen_clone: Window, handle: &AppHandle) {
                let my_closure = || {
                    let url = app_url();
                    info!("Executor clone on: {:?}", url);
                    let _ = splashscreen_clone.hide();
                    create_tray_message_windows(&handle);
                    let main = get_main_window(&handle);
                    main.emit("ready", Payload { message: "ad4m-executor is ready".into() }).unwrap();
                };

                match rust_executor::run(config.clone()).await {
                    () => {
                        my_closure();

                        info!("GraphQL server stopped.")
                    }
                }
            }

            tauri::async_runtime::spawn(async move {
                spawn_executor(config.clone(), splashscreen.clone(), &handle).await
            });

            Ok(())
        })
        .on_system_tray_event(move |app, event| {
            on_tray_event(app, &event);
            match event {
                SystemTrayEvent::LeftClick { position: _, size: _, .. } => {
                    let window = get_main_window(app);
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
                if let RunEvent::ExitRequested { api, .. } = event {
                    api.prevent_exit();
                };
            });
        }
        Err(err) => error!("Error building the app: {:?}", err),
    }
}

fn get_main_window(handle: &AppHandle) -> Window {
    let main = handle.get_window("AD4M");
    if let Some(window) = main {
        window
    } else {
        create_main_window(handle);
        let main = handle.get_window("AD4M");
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
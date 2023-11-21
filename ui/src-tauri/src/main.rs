#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

extern crate env_logger;
use chrono::Local;
use log::LevelFilter;
use log::{info, error, debug};
use rust_executor::Ad4mConfig;
use tauri::LogicalSize;
use tauri::Size;
use std::env;
use std::fs;
use std::fs::File;
use std::sync::Mutex;
#[cfg(not(target_os = "windows"))]
use libc::{rlimit, RLIMIT_NOFILE, setrlimit};
use std::io;
use std::io::Write;
use colored::Colorize;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt::format;



extern crate remove_dir_all;

use config::app_url;
use menu::build_menu;
use system_tray::{ build_system_tray, handle_system_tray_event };
use tauri::{
    AppHandle,
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
use crate::util::{create_main_window, save_executor_port};


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

#[cfg(not(target_os = "windows"))]
fn rlim_execute() {
    let mut rlim: rlimit = rlimit { rlim_cur: 0, rlim_max: 0 };
    // Get the current file limit
    unsafe {
        if libc::getrlimit(RLIMIT_NOFILE, &mut rlim) != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }
    let rlim_max = 1000 as u64;
    
    println!("Current RLIMIT_NOFILE: current: {}, max: {}", rlim.rlim_cur, rlim_max);

    // Attempt to increase the limit
    rlim.rlim_cur = rlim_max;
    unsafe {
        if setrlimit(RLIMIT_NOFILE, &rlim) != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }

    // Check the updated limit
    unsafe {
        if libc::getrlimit(RLIMIT_NOFILE, &mut rlim) != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }
    println!("Updated RLIMIT_NOFILE: current: {}, max: {}", rlim.rlim_cur, rlim_max);

}

fn main() {
    env::set_var("RUST_LOG", "holochain=warn,wasmer_compiler_cranelift=warn,rust_executor=info,warp::server");

    #[cfg(not(target_os = "windows"))]
    rlim_execute();

    if !data_path().exists() {
        let _ = fs::create_dir_all(data_path());
    }

    if log_path().exists() {
        let _ = fs::remove_file(log_path());
    }
    
    let target = Box::new(File::create(log_path()).expect("Can't create file"));

    env_logger::Builder::new()
        .target(env_logger::Target::Pipe(target))
        .filter(Some("holochain"), LevelFilter::Warn)
        .filter(Some("wasmer_compiler_cranelift"), LevelFilter::Warn)
        .filter(Some("rust_executor"), LevelFilter::Debug)
        .filter(Some("warp::server"), LevelFilter::Debug)
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
                Local::now().format("%Y-%m-%d %H:%M:%S%.3f").to_string().as_str().dimmed(),
                level,
                record.file().unwrap_or("unknown").to_string().as_str().dimmed(),
                record.line().unwrap_or(0).to_string().as_str().dimmed(),
                record.args().to_string().as_str().bold(),
            )
        })
        .init();
 
    let format = format::debug_fn(move |writer, _field, value| {
            debug!("TRACE: {:?}", value);
            write!(writer, "{:?}", value)
        });
    
    let filter = EnvFilter::from_default_env();

    let subscriber = tracing_subscriber::fmt()
        .with_env_filter(filter)
        .fmt_fields(format)
        .finish();
    
    tracing::subscriber::set_global_default(subscriber)
        .expect("Failed to set tracing subscriber");

    let free_port = find_port(12000, 13000);

    info!("Free port: {:?}", free_port);

    save_executor_port(free_port);

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
            config.run_dapp_server = Some(true);
            config.hc_use_bootstrap = Some(true);
            config.hc_use_mdns = Some(false);
            config.hc_use_proxy = Some(true);

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

                match rust_executor::run_with_tokio(config.clone()).await {
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

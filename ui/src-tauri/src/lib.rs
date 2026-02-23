#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

extern crate env_logger;
#[cfg(not(target_os = "windows"))]
use libc::{rlimit, setrlimit, RLIMIT_NOFILE};
use log::{debug, error, info};
use rust_executor::config::TlsConfig as ExecutorTlsConfig;
use rust_executor::logging::{get_default_log_config, init_launcher_logging};
use rust_executor::utils::find_port;
use rust_executor::Ad4mConfig;
use std::env;
use std::fs;
use std::fs::File;
use std::io;
use std::sync::Mutex;
use tauri::{Emitter, Listener, WebviewWindow};
//use tauri::Size;
use tracing_subscriber::fmt::format;
use tracing_subscriber::EnvFilter;

extern crate remove_dir_all;

use config::app_url;
use menu::build_menu;
use system_tray::build_system_tray;
use tauri::{AppHandle, RunEvent};
use tokio::sync::broadcast;
use uuid::Uuid;

mod app_state;
mod commands;
mod config;
mod encryption;
mod menu;
mod system_tray;
mod util;

use crate::app_state::LauncherState;
use crate::commands::app::{
    add_app_agent_state, clear_state, close_application, close_main_window, get_app_agent_list,
    get_data_path, get_log_config, get_smtp_config, get_tls_config, open_dapp, open_tray,
    open_tray_message, remove_app_agent_state, set_log_config, set_selected_agent, set_smtp_config,
    set_tls_config, show_main_window, test_smtp_config,
};
use crate::commands::proxy::{get_proxy, login_proxy, setup_proxy, stop_proxy};
use crate::commands::state::{get_port, request_credential};
use crate::config::log_path;

use crate::menu::reveal_log_file;
use crate::util::{create_main_window, save_executor_port};
use tauri::Manager;

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
    graphql_port: u16, // Local HTTP port (always for local access)
    req_credential: String,
    tls_enabled: bool,
}

#[cfg(not(target_os = "windows"))]
fn rlim_execute() {
    let mut rlim: rlimit = rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // Get the current file limit
    unsafe {
        if libc::getrlimit(RLIMIT_NOFILE, &mut rlim) != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }
    let rlim_max = 1000_u64;

    println!(
        "Current RLIMIT_NOFILE: current: {}, max: {}",
        rlim.rlim_cur, rlim_max
    );

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
    println!(
        "Updated RLIMIT_NOFILE: current: {}, max: {}",
        rlim.rlim_cur, rlim_max
    );
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    let state = LauncherState::load().unwrap();

    // Validate TLS config if enabled
    if let Some(tls_cfg) = &state.tls_config {
        if tls_cfg.enabled {
            if !std::path::Path::new(&tls_cfg.cert_file_path).exists() {
                error!(
                    "TLS is enabled but certificate file not found: {}. \
                     TLS will be disabled. Please check your TLS settings.",
                    tls_cfg.cert_file_path
                );
            }
            if !std::path::Path::new(&tls_cfg.key_file_path).exists() {
                error!(
                    "TLS is enabled but key file not found: {}. \
                     TLS will be disabled. Please check your TLS settings.",
                    tls_cfg.key_file_path
                );
            }
        }
    }

    let selected_agent = state.selected_agent.clone().unwrap();
    let app_path = selected_agent.path;
    let bootstrap_path = selected_agent.bootstrap;

    #[cfg(not(target_os = "windows"))]
    rlim_execute();

    if !app_path.exists() {
        let _ = fs::create_dir_all(app_path.clone());
    }

    if log_path().exists() {
        let _ = fs::remove_file(log_path());
    }

    let target = Box::new(File::create(log_path()).expect("Can't create file"));

    // Set up logging configuration and pass it to the logger
    let log_config = if let Some(user_config) = &state.log_config {
        // Start with defaults, then apply user overrides
        let mut final_config = get_default_log_config();
        for (crate_name, level) in user_config {
            final_config.insert(crate_name.clone(), level.clone());
        }
        Some(final_config)
    } else {
        None
    };

    init_launcher_logging(target, log_config.as_ref()).expect("Failed to initialize logging");

    let format = format::debug_fn(move |writer, _field, value| {
        debug!("TRACE: {:?}", value);
        write!(writer, "{:?}", value)
    });

    let filter = EnvFilter::from_default_env();

    let subscriber = tracing_subscriber::fmt()
        .with_env_filter(filter)
        .fmt_fields(format)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("Failed to set tracing subscriber");

    let free_port = find_port(12000, 13000).unwrap_or_else(|e| {
        let error_string = format!("Failed to find free main executor interface port: {}", e);
        error!("{}", error_string);
        panic!("{}", error_string);
    });

    info!("Free port: {:?}", free_port);

    save_executor_port(free_port);

    match rust_executor::init::init(
        Some(String::from(app_path.to_str().unwrap())),
        bootstrap_path.and_then(|path| path.to_str().map(|s| s.to_string())),
    ) {
        Ok(()) => {
            println!("Ad4m initialized sucessfully");
        }
        Err(e) => {
            println!("Ad4m initialization failed: {}", e);
            std::process::exit(1);
        }
    };

    let req_credential = Uuid::new_v4().to_string();

    // Check if TLS is enabled
    let tls_enabled = state
        .tls_config
        .as_ref()
        .map(|config| config.enabled)
        .unwrap_or(false);

    let app_state = AppState {
        graphql_port: free_port, // Always the local HTTP port
        req_credential: req_credential.clone(),
        tls_enabled,
    };

    let builder_result = tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_shell::init())
        .plugin(tauri_plugin_positioner::init())
        .plugin(tauri_plugin_notification::init())
        .manage(app_state)
        .manage(ProxyState(Default::default()))
        .invoke_handler(tauri::generate_handler![
            get_port,
            request_credential,
            login_proxy,
            setup_proxy,
            get_proxy,
            stop_proxy,
            close_application,
            close_main_window,
            show_main_window,
            clear_state,
            open_tray,
            open_tray_message,
            open_dapp,
            add_app_agent_state,
            get_app_agent_list,
            set_selected_agent,
            remove_app_agent_state,
            get_data_path,
            get_log_config,
            set_log_config,
            get_smtp_config,
            set_smtp_config,
            test_smtp_config,
            get_tls_config,
            set_tls_config
        ])
        .setup(move |app| {
            // Hides the dock icon
            #[cfg(target_os = "macos")]
            app.set_activation_policy(tauri::ActivationPolicy::Accessory);

            let splashscreen = app.get_webview_window("splashscreen").unwrap();

            let app_handle = app.handle().clone();
            let _id = splashscreen.listen("revealLogFile", move |event| {
                info!(
                    "got window event-name with payload {:?} {:?}",
                    event,
                    event.payload()
                );

                reveal_log_file(&app_handle);
            });

            build_menu(app.handle())?;
            build_system_tray(app.handle())?;

            // Convert TlsConfig and SmtpConfig if enabled
            let launcher_state = LauncherState::load().unwrap();

            // Prefer multi_user_config over deprecated tls_config
            let (tls_config, smtp_config, enable_multi_user) =
                if let Some(multi_user_config) = &launcher_state.multi_user_config {
                    let tls = multi_user_config.tls_config.as_ref().and_then(|config| {
                        if config.enabled {
                            let tls_port = config.tls_port.unwrap_or(free_port + 1);
                            Some(ExecutorTlsConfig {
                                cert_file_path: config.cert_file_path.clone(),
                                key_file_path: config.key_file_path.clone(),
                                tls_port,
                            })
                        } else {
                            None
                        }
                    });

                    let smtp = multi_user_config.smtp_config.as_ref().map(|config| {
                        rust_executor::config::SmtpConfig {
                            enabled: config.enabled,
                            host: config.host.clone(),
                            port: config.port,
                            username: config.username.clone(),
                            password: config.password.clone(),
                            from_address: config.from_address.clone(),
                        }
                    });

                    (tls, smtp, Some(multi_user_config.enabled))
                } else {
                    // Fallback to deprecated tls_config for backwards compatibility
                    let tls = launcher_state.tls_config.as_ref().and_then(|config| {
                        if config.enabled {
                            let tls_port = config.tls_port.unwrap_or(free_port + 1);
                            Some(ExecutorTlsConfig {
                                cert_file_path: config.cert_file_path.clone(),
                                key_file_path: config.key_file_path.clone(),
                                tls_port,
                            })
                        } else {
                            None
                        }
                    });
                    (tls, None, None)
                };

            // TLS enabled = bind to 0.0.0.0, TLS disabled = bind to 127.0.0.1
            let localhost = tls_config.is_none();

            let config = rust_executor::Ad4mConfig {
                admin_credential: Some(req_credential.to_string()),
                app_data_path: Some(String::from(app_path.to_str().unwrap())),
                gql_port: Some(free_port),
                network_bootstrap_seed: None,
                run_dapp_server: Some(true),
                hc_use_bootstrap: Some(true),
                hc_use_mdns: Some(false),
                hc_use_proxy: Some(true),
                tls: tls_config,
                localhost: Some(localhost),
                enable_multi_user,
                smtp_config,
                ..Default::default()
            };

            let handle = app.handle().clone();

            async fn spawn_executor(
                config: Ad4mConfig,
                splashscreen_clone: WebviewWindow,
                handle: &AppHandle,
            ) {
                rust_executor::run(config.clone()).await;
                let url = app_url();
                info!("Executor clone on: {:?}", url);
                let _ = splashscreen_clone.hide();
                let main = get_main_window(handle);
                main.emit(
                    "ready",
                    Payload {
                        message: "ad4m-executor is ready".into(),
                    },
                )
                .unwrap();
            }

            tauri::async_runtime::spawn(async move {
                spawn_executor(config.clone(), splashscreen.clone(), &handle).await
            });

            Ok(())
        })
        .on_window_event(|window, event| {
            if let tauri::WindowEvent::CloseRequested { api, .. } = event {
                if let Err(e) = window.hide() {
                    println!("Error trying to hide window: {:?}", e);
                } else {
                    api.prevent_close();
                }
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

fn get_main_window(handle: &AppHandle) -> WebviewWindow {
    let main = handle.get_webview_window("AD4M");
    if let Some(window) = main {
        window
    } else {
        create_main_window(handle);
        let main = handle.get_webview_window("AD4M");
        main.expect("Couldn't get main window right after creating it")
    }
}

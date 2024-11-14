use crate::app_url;
use crate::config::app_tray_message_url;
use crate::config::executor_port_path;
use crate::menu::open_logs_folder;
use std::fs::remove_file;
use std::fs::File;
use std::io::prelude::*;
use sysinfo::Process;
use sysinfo::{System, SystemExt};
use tauri::Listener;
use tauri::{AppHandle, Manager, WebviewUrl, WebviewWindowBuilder, WindowEvent, Wry};
use tauri_plugin_positioner::Position;
use tauri_plugin_positioner::WindowExt;

pub fn find_port(start_port: u16, end_port: u16) -> u16 {
    for x in start_port..end_port {
        if portpicker::is_free(x) {
            return x;
        }
    }

    panic!(
        "No open port found between: [{:?}, {:?}]",
        start_port, end_port
    );
}

pub fn _has_processes_running(name: &str) -> usize {
    let processes = System::new_all();
    let processes_by_name: Vec<&Process> = processes.processes_by_exact_name(name).collect();
    processes_by_name.len()
}

pub fn create_main_window(app: &AppHandle<Wry>) {
    let url = app_url();

    let new_ad4m_window = WebviewWindowBuilder::new(app, "AD4M", WebviewUrl::App(url.into()))
        .center()
        .focused(true)
        .inner_size(1000.0, 700.0)
        .title("ADAM Launcher");

    let _ = new_ad4m_window.build();

    let tray_window = app.get_webview_window("AD4M").unwrap();
    let _ = tray_window.set_decorations(true);
    let _ = tray_window.set_always_on_top(true);
    //let _ = tray_window.move_window(Position::TrayCenter);

    let _id = tray_window.listen("copyLogs", |event| {
        log::info!(
            "got window event-name with payload {:?} {:?}",
            event,
            event.payload()
        );

        open_logs_folder();
    });

    let window_clone = tray_window.clone();
    tray_window.on_window_event(move |event| {
        //println!("window event: {:?}", event);
        if let WindowEvent::Focused(f) = event {
            //println!("focused: {}", f);
            if let Some(monitor) = window_clone.current_monitor().unwrap() {
                let final_width = window_clone
                    .inner_size()
                    .unwrap()
                    .to_logical::<f64>(monitor.scale_factor())
                    .width;

                if !f && final_width == 400.0 {
                    let _ = window_clone.hide();
                }
            }
        }
    });
}

pub fn create_tray_message_windows(app: &AppHandle<Wry>) {
    let url = app_tray_message_url();

    let new_ad4m_window =
        WebviewWindowBuilder::new(app, "TrayMessage", WebviewUrl::App(url.into()))
            .center()
            .focused(true)
            .inner_size(360.0, 120.0)
            .title("TrayMessage")
            .visible(true);

    let _ = new_ad4m_window.build();

    let tray_window = app.get_webview_window("TrayMessage").unwrap();
    let _ = tray_window.move_window(Position::TopRight);
    let _ = tray_window.set_decorations(false);
    let _ = tray_window.set_always_on_top(true);
    // Hide window after 5 seconds
    let window_clone = tray_window.clone();
    std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_secs(5));
        let _ = window_clone.hide();
    });
}

pub fn save_executor_port(port: u16) {
    let _ = remove_file(executor_port_path());

    let mut file = File::create(executor_port_path()).unwrap();

    file.write_all(port.to_string().as_bytes()).unwrap();
}

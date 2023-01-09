use crate::app_url;
use crate::config::executor_port_path;
use crate::menu::open_logs_folder;
use std::fs::remove_file;
use std::fs::File;
use std::io::prelude::*;
use sysinfo::Process;
use sysinfo::{ProcessExt, Signal, System, SystemExt};
use tauri::{AppHandle, Manager, WindowBuilder, WindowEvent, WindowUrl, Wry};

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

pub fn find_and_kill_processes(name: &str) {
    let processes = System::new_all();

    for process in processes.processes_by_exact_name(name) {
        log::info!("Prosses running: {} {}", process.pid(), process.name());

        if process.kill_with(Signal::Term) == None {
            log::error!("This signal isn't supported on this platform");
        }
    }
}

pub fn has_process_running(name: &str) -> bool {
    let processes = System::new_all();
    let processes_by_name: Vec<&Process> = processes.processes_by_exact_name(name).collect();
    let mut running = false;

    for process in processes_by_name {
        log::info!("Prosses running: {} {}", process.pid(), process.name());
        running = true;
    }
    running
}

pub fn create_main_window(app: &AppHandle<Wry>) {
    let url = app_url();

    let new_ad4m_window = WindowBuilder::new(app, "AD4M", WindowUrl::App(url.into()))
        .center()
        .focused(true)
        .inner_size(1000.0, 700.0)
        .title("AD4M");

    let _ = new_ad4m_window.build();

    let tray_window = app.get_window("AD4M").unwrap();
    let _ = tray_window.set_decorations(false);
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
            if !f && window_clone.inner_size().unwrap().width == 400 {
                let _ = window_clone.hide();
            }
        }
    });
}

pub fn save_executor_port(port: u16) {
    let _ = remove_file(executor_port_path());

    let mut file = File::create(executor_port_path()).unwrap();

    file.write_all(port.to_string().as_bytes()).unwrap();
}

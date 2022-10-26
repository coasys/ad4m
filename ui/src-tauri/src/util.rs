use sysinfo::{ProcessExt, System, SystemExt, Signal};
use tauri::{WindowBuilder, WindowEvent, WindowUrl, Wry, AppHandle, Manager};
use crate::app_url;
use crate::config::executor_port_path;
use std::fs::remove_file;
use std::fs::File;
use std::io::prelude::*;

pub fn find_port(start_port: u16, end_port: u16) -> u16 {
  for x in start_port..end_port {
    if portpicker::is_free(x) {
      return x;
    }
  }

  panic!("No open port found between: [{:?}, {:?}]", start_port, end_port);
}

pub fn find_and_kill_processes(name: &str) {
  let processes = System::new_all();

  for process in processes.processes_by_exact_name(name) {
      log::info!("Prosses running: {} {}", process.pid(), process.name());
      
      match process.kill_with(Signal::Term) {
        None => {
          log::error!("This signal isn't supported on this platform");
        },
        _ => {}
     }
  }
}

pub fn create_main_window(app: &AppHandle<Wry>) {
  let url = app_url();

  let new_ad4min_window = WindowBuilder::new(
      app,
      "AD4MIN",
      WindowUrl::App(url.into()),
  )
  .center()
  .focus()
  .inner_size(1000.0, 700.0)
  .title("AD4MIN");

  let _ = new_ad4min_window.build();

  let tray_window = app.get_window("AD4MIN").unwrap();
  let _ = tray_window.set_decorations(false);
  let _ = tray_window.set_always_on_top(true);
  //let _ = tray_window.move_window(Position::TrayCenter);

  let window_clone = tray_window.clone();
  tray_window.on_window_event(move |event| {
    //println!("window event: {:?}", event);
    match event {
      WindowEvent::Focused(f) => {
        //println!("focused: {}", f);
        if !f {
          let _ = window_clone.hide();
        }
      },
      _ => {}
    }
  });

  log::info!("Creating ad4min UI {:?}", tray_window);
}

pub fn save_executor_port(port: u16) {
  let _ = remove_file(executor_port_path());

  let mut file = File::create(executor_port_path()).unwrap();

  file.write_all(port.to_string().as_bytes()).unwrap();
}

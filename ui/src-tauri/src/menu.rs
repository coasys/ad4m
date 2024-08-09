use crate::config::data_path;
use tauri::{CustomMenuItem, Window, Wry};
use tauri::{Menu, MenuItem, Submenu};

pub fn build_menu() -> Menu {
    let open_logs = CustomMenuItem::new("open_logs".to_string(), "Open Logs");
    let report_issue = CustomMenuItem::new("report_issue".to_string(), "Report Issue");

    let edit_menu = Submenu::new(
        "Edit",
        Menu::new()
            .add_native_item(MenuItem::Cut)
            .add_native_item(MenuItem::Copy)
            .add_native_item(MenuItem::Paste)
            .add_native_item(MenuItem::SelectAll),
    );

    let help_menu = Submenu::new(
        "Help",
        Menu::new().add_item(open_logs).add_item(report_issue),
    );

    Menu::new().add_submenu(edit_menu).add_submenu(help_menu)
}

pub fn handle_menu_event(event_id: &str, _window: &Window<Wry>) {
    match event_id {
        "open_logs" => {
            open_logs_folder();
        }
        "report_issue" => {
            report_issue();
        }
        _ => {}
    }
}

fn report_issue() {
    tauri::async_runtime::spawn(async move {
        open::that("https://github.com/perspect3vism/ad4m/issues/new")
            .map_err(|err| format!("Could not open url: {}", err))
    });
}

pub fn open_logs_folder() {
    if let Err(err) = opener::open(data_path()) {
        log::error!("Error opening logs folder: {}", err);
    }
}

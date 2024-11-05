use crate::config::data_path;
use tauri::{AppHandle, Result};
use tauri::menu::{MenuBuilder, SubmenuBuilder};

pub fn build_menu(app: &AppHandle) -> Result<()> {
    let edit_menu = SubmenuBuilder::new(app, "Edit")
        .cut()
        .copy()
        .paste()
        .select_all()
        .build()?;

    let help_menu = SubmenuBuilder::new(app, "Help")
        .text("open_logs", "Open Logs")
        .text("report_issue", "Report Issue")
        .build()?;

    let main_menu = MenuBuilder::new(app).item(&edit_menu).item(&help_menu).build()?;

    app.set_menu(main_menu)?;
    app.on_menu_event(move |_app, event| {
        match event.id().0.as_str() {
            "open_logs" => {
                open_logs_folder();
            }
            "report_issue" => {
                report_issue();
            }
            _ => {}
        }
    });

    Ok(())
}

fn report_issue() {
    tauri::async_runtime::spawn(async move {
        open::that("https://github.com/coasys/ad4m/issues/new")
            .map_err(|err| format!("Could not open url: {}", err))
    });
}

pub fn open_logs_folder() {
    if let Err(err) = opener::open(data_path()) {
        log::error!("Error opening logs folder: {}", err);
    }
}

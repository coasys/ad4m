use crate::config::data_path;
use tauri::menu::{MenuBuilder, SubmenuBuilder};
use tauri::{AppHandle, Result};
use tauri_plugin_opener::OpenerExt;

pub fn build_menu(app: &AppHandle) -> Result<()> {
    let edit_menu = SubmenuBuilder::new(app, "Edit")
        .cut()
        .copy()
        .paste()
        .select_all()
        .build()?;

    let help_menu = SubmenuBuilder::new(app, "Help")
        .text("open_logs", "Reveal Log File")
        .text("report_issue", "Report Issue")
        .build()?;

    let main_menu = MenuBuilder::new(app)
        .item(&edit_menu)
        .item(&help_menu)
        .build()?;

    app.set_menu(main_menu)?;
    let app_clone = app.clone();
    app.on_menu_event(move |_app, event| match event.id().0.as_str() {
        "open_logs" => {
            reveal_log_file(&app_clone);
        }
        "report_issue" => {
            report_issue();
        }
        _ => {}
    });

    Ok(())
}

fn report_issue() {
    tauri::async_runtime::spawn(async move {
        open::that("https://github.com/coasys/ad4m/issues/new")
            .map_err(|err| format!("Could not open url: {}", err))
    });
}

pub fn reveal_log_file(app: &AppHandle) {
    if let Err(err) = app
        .opener()
        .reveal_item_in_dir(data_path().join("ad4m.log"))
    {
        log::error!("Error opening logs folder: {}", err);
    }
}

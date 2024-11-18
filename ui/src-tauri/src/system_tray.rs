use crate::config::executor_port_path;
use crate::create_main_window;
use crate::Payload;
use std::fs::remove_file;
use tauri::{
    image::Image,
    menu::{MenuBuilder, MenuItemBuilder},
    tray::{MouseButton, MouseButtonState, TrayIconBuilder, TrayIconEvent},
    AppHandle, Emitter, LogicalSize, Manager, Result, Size,
};

fn toggle_main_window(app: &AppHandle) {
    let window = if let Some(window) = app.get_webview_window("AD4M") {
        if let Ok(true) = window.is_visible() {
            let _ = window.hide();
        } else {
            window.show().unwrap();
            window.set_focus().unwrap();
        }
        window
    } else {
        create_main_window(app);
        let main = app.get_webview_window("AD4M").unwrap();
        main.emit(
            "ready",
            Payload {
                message: "ad4m-executor is ready".into(),
            },
        )
        .unwrap();
        main
    };

    let _ = window.set_size(Size::Logical(LogicalSize {
        width: 400.0,
        height: 700.0,
    }));
    let _ = window.set_always_on_top(true);
}

pub fn build_system_tray(app: &AppHandle) -> Result<()> {
    // if env::consts::OS == "linux" {
    let toggle = MenuItemBuilder::with_id("toggle_window", "Show Window").build(app)?;
    let quit = MenuItemBuilder::with_id("quit", "Quit").build(app)?;
    let menu = MenuBuilder::new(app).items(&[&toggle, &quit]).build()?;
    TrayIconBuilder::new()
        .icon(
            Image::from_bytes(include_bytes!("../icons/icon.png"))
                .expect("couldn't create tray image from resource"),
        )
        .menu(&menu)
        .on_menu_event(move |app, event| match event.id().as_ref() {
            "toggle_window" => toggle_main_window(app),
            "quit" => {
                let _ = remove_file(executor_port_path());
                app.exit(0);
                std::process::exit(0);
            }
            _ => log::error!("Event is not defined."),
        })
        .menu_on_left_click(false)
        .on_tray_icon_event(|tray, event| {
            if let TrayIconEvent::Click {
                button: MouseButton::Left,
                button_state: MouseButtonState::Up,
                ..
            } = event
            {
                toggle_main_window(tray.app_handle())
            }
        })
        .build(app)?;

    //} else {
    //    SystemTray::new()
    //}

    Ok(())
}

use std::net::Ipv4Addr;
use std::path::Path;

use rocket::fs::FileServer;
use rocket::Config;
use include_dir::{include_dir, Dir};

#[cfg(target_os = "windows")]
fn get_dapp_dir() -> Dir<'static> {
    let dapp: Dir = include_dir!("../dapp/dist");
    dapp
}

#[cfg(not(target_os = "windows"))]
fn get_dapp_dir() -> Dir<'static> {
    let dapp: Dir = include_dir!("dapp/dist");
    dapp
}

pub(crate) async fn serve_dapp(port: u16, app_dir: String) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let dapp = get_dapp_dir();
    
    let config = Config {
        port,
        address: Ipv4Addr::new(127, 0, 0, 1).into(),
        ..Config::debug_default()
    };

    let dir = Path::new(&app_dir).join("dapp");
    if !dir.exists() {
        dapp.extract(dir.clone())?;
    }

    rocket::build()
        .configure(&config)
        .mount("/", FileServer::from(dir.to_str().expect("Failed to convert path to string")))
        .launch()
        .await?;

    Ok(())
}
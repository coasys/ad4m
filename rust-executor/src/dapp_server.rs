use std::net::Ipv4Addr;
use std::path::Path;

use rocket::fs::{FileServer, relative};
use rocket::Config;

pub(crate) async fn serve_dapp(port: u16) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let config = Config {
        port,
        address: Ipv4Addr::new(127, 0, 0, 1).into(),
        ..Config::debug_default()
    };

    let dir = relative!("dapp");
    if !Path::new(dir).exists() {
        return Err("Dapp directory not found".into());
    }

    rocket::build()
        .configure(&config)
        .mount("/", FileServer::from(relative!("dapp")))
        .launch()
        .await?;

    Ok(())
}
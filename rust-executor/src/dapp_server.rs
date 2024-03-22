use std::net::Ipv4Addr;
use std::path::Path;

use rocket::fs::{FileServer, relative};
use rocket::{Config, Route, State};
use include_dir::{include_dir, Dir};

const DAPP: Dir = include_dir!("dapp/dist");

pub(crate) async fn serve_dapp(port: u16) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let config = Config {
        port,
        address: Ipv4Addr::new(127, 0, 0, 1).into(),
        ..Config::debug_default()
    };

    let dir = relative!("dapp/dist");
    if !Path::new(dir).exists() {
        DAPP.extract("dapp/dist")?;
    }

    rocket::build()
        .configure(&config)
        .mount("/", FileServer::from(dir))
        .launch()
        .await?;

    Ok(())
}
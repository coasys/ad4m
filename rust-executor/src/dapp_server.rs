use std::net::Ipv4Addr;
use std::path::Path;

use include_dir::{include_dir, Dir};
use rocket::fs::FileServer;
use rocket::Config;

const DAPP: Dir = include_dir!("dapp/dist");

pub(crate) async fn serve_dapp(
    port: u16,
    app_dir: String,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let config = Config {
        port,
        address: Ipv4Addr::new(127, 0, 0, 1).into(),
        ..Config::debug_default()
    };

    let dir = Path::new(&app_dir).join("dapp");
    if !dir.exists() {
        DAPP.extract(dir.clone())?;
    }

    rocket::build()
        .configure(&config)
        .mount(
            "/",
            FileServer::from(dir.to_str().expect("Failed to convert path to string")),
        )
        .launch()
        .await?;

    Ok(())
}

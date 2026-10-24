//! `identity-store [serve]` runs the store; `identity-store keygen` makes the head key and
//! prints its public multikey for the executors' `identity.storeKeys`.
//!
//! Configuration (environment):
//!
//! | Variable | Default | |
//! |---|---|---|
//! | `IDENTITY_STORE_DB` | `identity-store.sqlite` | the SQLite file |
//! | `IDENTITY_STORE_KEY` | `store-key` | the head key file; `serve` makes one when it is missing |
//! | `IDENTITY_STORE_LISTEN` | `0.0.0.0:8080` | the listen address; port 0 picks a free port |
//! | `POW_BITS` | `22` | the proof-of-work cost `GET /v1/pow` publishes |
//! | `CORS_ORIGINS` | none | comma-separated origins allowed from a browser |
//! | `FEED_TOKEN` | none | the mirror token of `GET /v1/feed` |
//!
//! On start, `serve` prints `identity-store listening on http://<addr> store-key <multikey>`.

use std::path::PathBuf;
use std::process::ExitCode;

use identity_store::{db::Db, router, system_clock, Settings, Store, StoreKey};

fn env_or(name: &str, default: &str) -> String {
    std::env::var(name).unwrap_or_else(|_| default.to_string())
}

fn settings() -> Result<Settings, String> {
    let pow_bits = match std::env::var("POW_BITS") {
        Ok(v) => v
            .parse::<u8>()
            .map_err(|_| format!("POW_BITS={v} is not a number of bits"))?,
        Err(_) => Settings::default().pow_bits,
    };
    let cors_origins = std::env::var("CORS_ORIGINS")
        .map(|v| {
            v.split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect()
        })
        .unwrap_or_default();
    let feed_token = std::env::var("FEED_TOKEN").ok().filter(|t| !t.is_empty());
    Ok(Settings {
        pow_bits,
        feed_token,
        cors_origins,
    })
}

async fn serve() -> Result<(), String> {
    let key_path = PathBuf::from(env_or("IDENTITY_STORE_KEY", "store-key"));
    let key = StoreKey::load_or_create(&key_path).map_err(|e| e.to_string())?;
    let db = Db::open(env_or("IDENTITY_STORE_DB", "identity-store.sqlite"))
        .map_err(|e| format!("{:?}", e.body))?;
    let store = Store::new(db, key, system_clock(), settings()?);
    let listen = env_or("IDENTITY_STORE_LISTEN", "0.0.0.0:8080");
    let listener = tokio::net::TcpListener::bind(&listen)
        .await
        .map_err(|e| format!("bind {listen}: {e}"))?;
    let addr = listener.local_addr().map_err(|e| e.to_string())?;
    println!(
        "identity-store listening on http://{addr} store-key {}",
        store.key.multikey()
    );
    axum::serve(listener, router(store))
        .with_graceful_shutdown(async {
            let _ = tokio::signal::ctrl_c().await;
        })
        .await
        .map_err(|e| e.to_string())
}

fn keygen() -> Result<(), String> {
    let key_path = PathBuf::from(env_or("IDENTITY_STORE_KEY", "store-key"));
    let key = StoreKey::create(&key_path).map_err(|e| e.to_string())?;
    println!("{}", key.multikey());
    Ok(())
}

#[tokio::main]
async fn main() -> ExitCode {
    let command = std::env::args().nth(1).unwrap_or_else(|| "serve".into());
    let result = match command.as_str() {
        "serve" => serve().await,
        "keygen" => keygen(),
        other => Err(format!("unknown command {other}; use serve or keygen")),
    };
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("identity-store: {e}");
            ExitCode::FAILURE
        }
    }
}

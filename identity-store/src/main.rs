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
//! | `IDENTITY_BURST` | `5` | writes per identity at once (the identity bucket) |
//! | `IDENTITY_REFILL_S` | `60` | seconds per write the identity bucket refills |
//!
//! `serve` sweeps expired inbox messages and requests every minute.
//! On start, `serve` prints `identity-store listening on http://<addr> store-key <multikey>`.

use std::path::PathBuf;
use std::process::ExitCode;

use identity_store::{db::Db, router, system_clock, Settings, Store, StoreKey, SWEEP_EVERY_S};

fn env_or(name: &str, default: &str) -> String {
    std::env::var(name).unwrap_or_else(|_| default.to_string())
}

fn number<T: std::str::FromStr>(name: &str, default: T) -> Result<T, String> {
    match std::env::var(name) {
        Ok(v) => v
            .parse::<T>()
            .map_err(|_| format!("{name}={v} is not a whole number")),
        Err(_) => Ok(default),
    }
}

fn settings() -> Result<Settings, String> {
    let defaults = Settings::default();
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
        pow_bits: number("POW_BITS", defaults.pow_bits)?,
        feed_token,
        cors_origins,
        identity_burst: number("IDENTITY_BURST", defaults.identity_burst)?,
        identity_refill_s: number("IDENTITY_REFILL_S", defaults.identity_refill_s)?,
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
    let sweeper = store.clone();
    tokio::spawn(async move {
        let mut every = tokio::time::interval(std::time::Duration::from_secs(SWEEP_EVERY_S));
        loop {
            every.tick().await;
            if let Err(e) = sweeper.sweep() {
                eprintln!("identity-store: sweep failed: {:?}", e.body);
            }
        }
    });
    axum::serve(listener, router(store))
        .with_graceful_shutdown(shutdown())
        .await
        .map_err(|e| e.to_string())
}

/// Resolves on SIGINT, or on SIGTERM (what `docker stop` and service managers send).
async fn shutdown() {
    let interrupt = async {
        let _ = tokio::signal::ctrl_c().await;
    };
    #[cfg(unix)]
    let terminate = async {
        match tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate()) {
            Ok(mut signal) => {
                signal.recv().await;
            }
            Err(_) => std::future::pending().await,
        }
    };
    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();
    tokio::select! {
        () = interrupt => {}
        () = terminate => {}
    }
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

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
//! | `POW_BITS` | `22` | the proof-of-work cost `GET /v1/pow` publishes, over the one-use challenge each answer mints (300 s); the previous bits pass for 10 minutes after a change |
//! | `BRAKE_GLOBAL_PER_HOUR` | `120` | new logs without a note store-wide per hour |
//! | `BRAKE_ADDR_BURST` | `3` | new logs without a note per address at once (IPv6 by /64) |
//! | `BRAKE_ADDR_PER_DAY` | `3` | what the address bucket refills per 24 h |
//! | `TRUSTED_PROXIES` | none | comma-separated proxy addresses or ranges whose `CF-Connecting-IP` / `X-Forwarded-For` the brake reads; required when the store runs behind a proxy — unset there, every client counts as the proxy's one address |
//! | `VOUCH_URL` | none | the vouching service's base URL; unset: tokens get `503 vouch-unavailable` |
//! | `VOUCH_CREDENTIAL` | none | the store's bearer credential at the vouching service |
//! | `VOUCH_KEYS` | none | comma-separated multikeys of the trusted vouching keys |
//! | `CORS_ORIGINS` | none | comma-separated origins allowed from a browser |
//! | `FEED_TOKEN` | none | the mirror token of `GET /v1/feed` |
//! | `IDENTITY_BURST` | `10` | writes per identity at once (the identity bucket) |
//! | `IDENTITY_REFILL_S` | `60` | seconds per write the identity bucket refills |
//! | `HELD_LOG_BYTES` | 256 MiB | the most heap the held folds take together; at least twice `LOG_FOLD_BYTES` |
//! | `LOG_FOLD_BYTES` | 16 MiB | the per-log budget: the most heap one log's fold may take without revoked keys; the whole fold at most twice it |
//!
//! `serve` sweeps expired inbox messages and requests every minute.
//! On start, `serve` prints `identity-store listening on http://<addr> store-key <multikey>`.

use std::path::PathBuf;
use std::process::ExitCode;

use identity_store::{db::Db, router, system_clock, Settings, Store, StoreKey, SWEEP_EVERY_S};

fn env_or(name: &str, default: &str) -> String {
    std::env::var(name).unwrap_or_else(|_| default.to_string())
}

fn settings() -> Result<Settings, String> {
    Settings::from_config(|name| std::env::var(name).ok())
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
    let shutdown = shutdown();
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
    // The socket peer of each request: the brake's address (`brake.rs`).
    let app = router(store).into_make_service_with_connect_info::<std::net::SocketAddr>();
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown)
        .await
        .map_err(|e| e.to_string())
}

/// Resolves on SIGINT, or on SIGTERM (what `docker stop` and service managers send).
///
/// The handlers are registered when this is called, not when the future is first polled:
/// `serve` calls it before it prints that it listens, so a signal sent from then on shuts it
/// down cleanly instead of taking the default action.
fn shutdown() -> impl std::future::Future<Output = ()> {
    #[cfg(unix)]
    {
        use tokio::signal::unix::{signal, SignalKind};
        let interrupt = signal(SignalKind::interrupt());
        let terminate = signal(SignalKind::terminate());
        async move {
            let wait = |stream: std::io::Result<tokio::signal::unix::Signal>| async move {
                match stream {
                    Ok(mut signal) => {
                        signal.recv().await;
                    }
                    Err(_) => std::future::pending().await,
                }
            };
            tokio::select! {
                () = wait(interrupt) => {}
                () = wait(terminate) => {}
            }
        }
    }
    #[cfg(not(unix))]
    async {
        let _ = tokio::signal::ctrl_c().await;
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

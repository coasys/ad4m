//! Shared real-executor harness: binary lookup, spawn, connect-poll.
//!
//! The binary is `EXECUTOR_PATH` if set, else `release/ad4m-executor` under
//! `CARGO_TARGET_DIR` or the workspace `target/`. A missing binary skips —
//! unless `AD4M_REQUIRE_EXECUTOR=1`, which makes it a failure.
//!
//! Each test binary uses only part of this module.
#![allow(dead_code)]

use ad4m_client::Ad4mClient;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::Duration;

fn executor_binary() -> Option<PathBuf> {
    if let Ok(p) = std::env::var("EXECUTOR_PATH") {
        let p = PathBuf::from(p);
        // An explicit path that doesn't resolve is a misspelled
        // environment, not an absent executor — skipping here reports
        // success without reaching an assertion (F11).
        assert!(p.exists(), "EXECUTOR_PATH does not exist: {}", p.display());
        return Some(p);
    }
    let mut candidates = Vec::new();
    if let Ok(dir) = std::env::var("CARGO_TARGET_DIR") {
        candidates.push(PathBuf::from(dir).join("release/ad4m-executor"));
    }
    // Workspace root = rust-client/..
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    candidates.push(root.join("target/release/ad4m-executor"));
    candidates.into_iter().find(|p| p.exists())
}

/// `Some(binary)` or, without `AD4M_REQUIRE_EXECUTOR`, a printed skip (`None`
/// — the caller `let Some(..) else { return }`s). With the variable set a
/// missing binary must fail the run, not silently pass.
pub fn require_binary() -> Option<PathBuf> {
    match executor_binary() {
        Some(p) => Some(p),
        None => {
            assert!(
                !std::env::var("AD4M_REQUIRE_EXECUTOR").is_ok_and(|v| v != "0" && !v.is_empty()),
                "AD4M_REQUIRE_EXECUTOR is set but no ad4m-executor binary was found \
                 (EXECUTOR_PATH, $CARGO_TARGET_DIR/release/ad4m-executor, \
                 target/release/ad4m-executor)"
            );
            eprintln!("SKIP: no ad4m-executor binary (set EXECUTOR_PATH or build --release)");
            None
        }
    }
}

fn free_port() -> u16 {
    std::net::TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

pub struct Executor {
    child: Child,
    pub url: String,
    pub port: u16,
    pub admin_credential: Option<String>,
    _data: tempfile::TempDir,
}

impl Drop for Executor {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Single-user executor without an admin credential
/// (`--insecure-no-admin-credential`): an anonymous loopback caller is the
/// operator.
pub fn start_executor() -> Option<Executor> {
    start(None)
}

/// `--admin-credential <credential>`: anonymous callers hold only the
/// anonymous set — the real path apps take; `credential` as the WS token is
/// the operator.
pub fn start_executor_with_admin(credential: &str) -> Option<Executor> {
    start(Some(credential))
}

fn start(admin_credential: Option<&str>) -> Option<Executor> {
    let binary = require_binary()?;
    let data = tempfile::tempdir().unwrap();
    let port = free_port();
    // `init` first — as `tests/js/startExecutor` does: `run` on a data dir
    // that never saw `init` leaves the runtime service without its seed and
    // `agent.generate` panics executor-side. The committed JS seed leaves
    // `languageLanguageBundle` empty (the JS suite injects a built bundle,
    // then strips it), and `init` refuses that field. This harness boots
    // with the executor's real seed. Only the language language is started
    // (`--language-language-only`); these reads do not need the other
    // system languages.
    let seed =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../rust-executor/src/mainnet_seed.json");
    assert!(seed.is_file(), "bootstrap seed missing: {}", seed.display());
    let init = Command::new(&binary)
        .args([
            "init",
            "--data-path",
            data.path().to_str().unwrap(),
            "--network-bootstrap-seed",
            seed.to_str().unwrap(),
        ])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("ad4m-executor init");
    assert!(init.success(), "ad4m-executor init failed");

    let mut args = vec![
        "run".to_string(),
        "--app-data-path".to_string(),
        data.path().to_str().unwrap().to_string(),
        "--port".to_string(),
        port.to_string(),
        "--run-holochain".to_string(),
        "false".to_string(),
        "--language-language-only".to_string(),
        "true".to_string(),
        "--run-dapp-server".to_string(),
        "false".to_string(),
    ];
    match admin_credential {
        Some(credential) => args.extend(["--admin-credential".to_string(), credential.to_string()]),
        None => args.push("--insecure-no-admin-credential".to_string()),
    }
    let child = Command::new(&binary)
        .args(&args)
        .env("AD4M_EXIT_WITH_PARENT", "true")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn ad4m-executor");
    Some(Executor {
        child,
        url: format!("http://127.0.0.1:{port}"),
        port,
        admin_credential: admin_credential.map(str::to_string),
        _data: data,
    })
}

/// The executor takes seconds to come up — poll the socket until it takes.
pub async fn connect(url: &str, token: &str) -> Ad4mClient {
    let deadline = std::time::Instant::now() + Duration::from_secs(120);
    loop {
        match Ad4mClient::connect(url.to_string(), token.to_string()).await {
            Ok(c) => return c,
            Err(e) => {
                assert!(
                    std::time::Instant::now() < deadline,
                    "executor did not come up in 120s: {e}"
                );
                tokio::time::sleep(Duration::from_millis(500)).await;
            }
        }
    }
}

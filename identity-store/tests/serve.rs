//! The `identity-store serve` process: it shuts down cleanly on SIGTERM as on SIGINT.

#![cfg(unix)]

use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn stops_cleanly_on(signal: &str) {
    let dir = tempfile::tempdir().unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_identity-store"))
        .arg("serve")
        .env("IDENTITY_STORE_DB", dir.path().join("store.sqlite"))
        .env("IDENTITY_STORE_KEY", dir.path().join("store-key"))
        .env("IDENTITY_STORE_LISTEN", "127.0.0.1:0")
        .stdout(Stdio::piped())
        .spawn()
        .expect("identity-store starts");
    let mut line = String::new();
    BufReader::new(child.stdout.take().unwrap())
        .read_line(&mut line)
        .unwrap();
    assert!(line.starts_with("identity-store listening on"), "{line}");
    let sent = Command::new("kill")
        .args([&format!("-{signal}"), &child.id().to_string()])
        .status()
        .unwrap();
    assert!(sent.success());
    let deadline = Instant::now() + Duration::from_secs(10);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() > deadline {
            let _ = child.kill();
            panic!("identity-store did not stop on SIG{signal}");
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    assert!(status.success(), "SIG{signal}: {status:?}");
}

#[test]
fn serve_stops_cleanly_on_sigterm() {
    stops_cleanly_on("TERM");
}

#[test]
fn serve_stops_cleanly_on_sigint() {
    stops_cleanly_on("INT");
}

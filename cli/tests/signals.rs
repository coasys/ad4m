//! An executor whose launcher is gone must still end on a signal.
//!
//! When the process that started `ad4m-executor` dies without stopping it (a test runner killed
//! by CI, a closed terminal, a crashed Electron), the executor's stdout is a pipe nobody reads.
//! A signal that then reaches it must end it: a handler that prints to stdout first panics on the
//! broken pipe and leaves an executor that only SIGKILL ends.
#![cfg(unix)]

use std::io::{BufRead, BufReader};
use std::net::TcpListener;
use std::path::PathBuf;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

const READY: &str = "API server starting on http://127.0.0.1:";
const STARTUP: Duration = Duration::from_secs(120);
/// How long the executor may take to end after the signal. Callers escalate to SIGKILL after 5 s
/// (tests/js SFU harness) or 10 s (tests/js `gracefulShutdown`, WE's `stopProcess`).
const EXIT_WITHIN: Duration = Duration::from_secs(10);

struct Executor {
    child: Child,
    data_path: PathBuf,
}

impl Drop for Executor {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
        let _ = std::fs::remove_dir_all(&self.data_path);
    }
}

fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

/// Starts an executor without Holochain, waits until its API listens, then closes the read end of
/// its stdout, as a launcher that died would have.
fn start_with_stdout_reader_gone(name: &str) -> Executor {
    let data_path =
        std::env::temp_dir().join(format!("ad4m-signal-test-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&data_path);
    std::fs::create_dir_all(&data_path).unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_ad4m-executor"))
        .args(["run", "--app-data-path"])
        .arg(&data_path)
        .args(["--port", &free_port().to_string()])
        .args(["--run-holochain", "false", "--run-dapp-server", "false"])
        .args([
            "--language-language-only",
            "false",
            "--insecure-no-admin-credential",
        ])
        // Readiness is an info line: an inherited RUST_LOG=warn would hide it.
        .env("RUST_LOG", "rust_executor=info")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("ad4m-executor starts");

    // The log goes to stderr: wait for the API line, then keep draining it.
    let stderr = child.stderr.take().unwrap();
    let (ready_tx, ready_rx) = mpsc::channel();
    std::thread::spawn(move || {
        let mut ready_tx = Some(ready_tx);
        for line in BufReader::new(stderr).lines().map_while(Result::ok) {
            if line.contains(READY) {
                if let Some(tx) = ready_tx.take() {
                    let _ = tx.send(());
                }
            }
        }
    });
    // Built before the wait, so a startup that times out still kills the process.
    let mut executor = Executor { child, data_path };
    ready_rx
        .recv_timeout(STARTUP)
        .expect("the executor's API listens");

    drop(executor.child.stdout.take());
    executor
}

fn signal(child: &Child, name: &str) {
    let status = Command::new("kill")
        .args([&format!("-{name}"), &child.id().to_string()])
        .status()
        .unwrap();
    assert!(status.success(), "kill -{name} {}", child.id());
}

fn exit_within(child: &mut Child, limit: Duration) -> Option<ExitStatus> {
    let started = Instant::now();
    while started.elapsed() < limit {
        if let Some(status) = child.try_wait().unwrap() {
            return Some(status);
        }
        std::thread::sleep(Duration::from_millis(100));
    }
    None
}

fn assert_signal_ends_it(name: &str, sig: &str) {
    let mut executor = start_with_stdout_reader_gone(name);
    signal(&executor.child, sig);
    let status = exit_within(&mut executor.child, EXIT_WITHIN);
    assert!(
        status.is_some(),
        "the executor was still running {EXIT_WITHIN:?} after SIG{sig}"
    );
}

#[test]
fn sigterm_ends_an_executor_whose_launcher_is_gone() {
    assert_signal_ends_it("term", "TERM");
}

#[test]
fn sigint_ends_an_executor_whose_launcher_is_gone() {
    assert_signal_ends_it("int", "INT");
}

/// A closed terminal sends SIGHUP.
#[test]
fn sighup_ends_an_executor_whose_launcher_is_gone() {
    assert_signal_ends_it("hup", "HUP");
}

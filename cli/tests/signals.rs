//! An executor whose launcher is gone must not run on.
//!
//! When the process that started `ad4m-executor` dies without stopping it (a test runner killed
//! by CI, a closed terminal, a crashed Electron), the executor's stdout is a pipe nobody reads.
//! A signal that then reaches it must end it: a handler that prints to stdout first panics on the
//! broken pipe and leaves an executor that only SIGKILL ends. With `exit_with_parent` it must end
//! without any signal.
#![cfg(unix)]

use std::io::{BufRead, BufReader};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStderr, Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

const READY: &str = "API server starting on http://127.0.0.1:";
const STARTUP: Duration = Duration::from_secs(120);
/// How long the executor may take to end. Callers escalate to SIGKILL after 5 s (tests/js SFU
/// harness) or 10 s (tests/js `gracefulShutdown`, WE's `stopProcess`).
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

/// An executor that is not our child (its launcher was): killed by PID when the test ends.
struct Orphan {
    pid: u32,
    data_path: PathBuf,
}

impl Drop for Orphan {
    fn drop(&mut self) {
        if alive(self.pid) {
            signal(self.pid, "KILL");
        }
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

fn fresh_data_path(name: &str) -> PathBuf {
    let data_path =
        std::env::temp_dir().join(format!("ad4m-signal-test-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&data_path);
    std::fs::create_dir_all(&data_path).unwrap();
    data_path
}

/// `run` without Holochain on a free port.
fn run_args(data_path: &Path) -> Vec<String> {
    let mut args = vec!["run".to_string(), "--app-data-path".to_string()];
    args.push(data_path.to_str().unwrap().to_string());
    args.extend(["--port".to_string(), free_port().to_string()]);
    args.extend(
        [
            "--run-holochain",
            "false",
            "--run-dapp-server",
            "false",
            "--language-language-only",
            "false",
            "--insecure-no-admin-credential",
        ]
        .map(String::from),
    );
    args
}

/// Waits for the API line in the log (stderr), and keeps draining it.
fn wait_until_ready(stderr: ChildStderr) {
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
    ready_rx
        .recv_timeout(STARTUP)
        .expect("the executor's API listens");
}

/// Starts an executor, waits until its API listens, then closes the read end of its stdout, as a
/// launcher that died would have.
fn start_with_stdout_reader_gone(name: &str) -> Executor {
    let data_path = fresh_data_path(name);
    let mut child = Command::new(env!("CARGO_BIN_EXE_ad4m-executor"))
        .args(run_args(&data_path))
        // Readiness is an info line: an inherited RUST_LOG=warn would hide it.
        .env("RUST_LOG", "rust_executor=info")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("ad4m-executor starts");
    let stderr = child.stderr.take().unwrap();
    // Built before the wait, so a startup that times out still kills the process.
    let mut executor = Executor { child, data_path };
    wait_until_ready(stderr);
    drop(executor.child.stdout.take());
    executor
}

fn signal(pid: u32, name: &str) {
    let status = Command::new("kill")
        .args([&format!("-{name}"), &pid.to_string()])
        .status()
        .unwrap();
    assert!(status.success(), "kill -{name} {pid}");
}

fn alive(pid: u32) -> bool {
    // An orphan that exited stays a zombie until its new parent reaps it: that counts as gone.
    if let Ok(stat) = std::fs::read_to_string(format!("/proc/{pid}/stat")) {
        let state = stat.rsplit(')').next().unwrap_or("").trim_start();
        return !state.starts_with('Z');
    }
    Command::new("kill")
        .args(["-0", &pid.to_string()])
        .stderr(Stdio::null())
        .status()
        .unwrap()
        .success()
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
    signal(executor.child.id(), sig);
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

/// The launcher, `sh` here, starts the executor and is SIGKILLed, as a cancelled CI job's test
/// runner or a crashed Electron is: nobody signals the executor. It is told through
/// `AD4M_EXIT_WITH_PARENT` rather than the flag, so a build without the setting ignores it
/// instead of refusing to start, and this test fails on it rather than passing.
#[test]
fn exit_with_parent_ends_the_executor_once_its_launcher_is_killed() {
    let data_path = fresh_data_path("parent");
    let mut launcher = Command::new("sh")
        .args(["-c", r#""$@" & echo "$!"; wait"#, "sh"])
        .arg(env!("CARGO_BIN_EXE_ad4m-executor"))
        .args(run_args(&data_path))
        .env("AD4M_EXIT_WITH_PARENT", "true")
        .env("RUST_LOG", "rust_executor=info")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("sh starts");
    let mut stdout = BufReader::new(launcher.stdout.take().unwrap());
    let mut pid = String::new();
    stdout.read_line(&mut pid).unwrap();
    let executor = Orphan {
        pid: pid.trim().parse().expect("sh prints the executor's PID"),
        data_path,
    };
    wait_until_ready(launcher.stderr.take().unwrap());

    launcher.kill().unwrap();
    launcher.wait().unwrap();

    let started = Instant::now();
    while alive(executor.pid) && started.elapsed() < EXIT_WITHIN {
        std::thread::sleep(Duration::from_millis(100));
    }
    assert!(
        !alive(executor.pid),
        "the executor was still running {EXIT_WITHIN:?} after its launcher was killed"
    );
}

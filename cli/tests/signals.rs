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
use std::process::{Child, ChildStderr, ChildStdout, Command, ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

const READY: &str = "API server starting on http://127.0.0.1:";
/// The graceful shutdown's last line: the cleanup ran to its end.
const SHUT_DOWN: &str = "Graceful shutdown complete, exiting.";
const STARTUP: Duration = Duration::from_secs(120);
/// How long the executor may take to end. Callers escalate to SIGKILL after 5 s (tests/js SFU
/// harness) or 10 s (tests/js `gracefulShutdown`, WE's `stopProcess`).
const EXIT_WITHIN: Duration = Duration::from_secs(10);

struct Executor {
    child: Child,
    data_path: PathBuf,
    /// Set once the log shows the graceful shutdown's last line.
    shut_down: Arc<AtomicBool>,
    /// Drains stderr; ends at the process's exit, once every line is read.
    reader: Option<JoinHandle<()>>,
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

/// Drains the log (stderr) on a thread, noting the shutdown's last line, and waits for the API
/// line. Returns that note and the thread, which ends once the process exited and its log is read.
fn read_log(stderr: ChildStderr) -> (mpsc::Receiver<()>, Arc<AtomicBool>, JoinHandle<()>) {
    let (ready_tx, ready_rx) = mpsc::channel();
    let shut_down = Arc::new(AtomicBool::new(false));
    let seen = shut_down.clone();
    let reader = std::thread::spawn(move || {
        let mut ready_tx = Some(ready_tx);
        for line in BufReader::new(stderr).lines().map_while(Result::ok) {
            if line.contains(READY) {
                if let Some(tx) = ready_tx.take() {
                    let _ = tx.send(());
                }
            }
            if line.contains(SHUT_DOWN) {
                seen.store(true, Ordering::SeqCst);
            }
        }
    });
    (ready_rx, shut_down, reader)
}

fn wait_until_ready(ready: &mpsc::Receiver<()>) {
    ready
        .recv_timeout(STARTUP)
        .expect("the executor's API listens");
}

/// Starts an executor, waits until its API listens, then closes the read end of its stdout, as a
/// launcher that died would have.
fn start_with_stdout_reader_gone(name: &str) -> Executor {
    start(name, false)
}

/// As `start_with_stdout_reader_gone`; with `sighup_ignored` the executor starts with SIGHUP
/// ignored, as `nohup` starts it (`sh` sets it, `exec` keeps the PID and the disposition).
fn start(name: &str, sighup_ignored: bool) -> Executor {
    let data_path = fresh_data_path(name);
    let mut command = if sighup_ignored {
        let mut sh = Command::new("sh");
        sh.args(["-c", r#"trap '' HUP; exec "$@""#, "sh"]);
        sh.arg(env!("CARGO_BIN_EXE_ad4m-executor"));
        sh
    } else {
        Command::new(env!("CARGO_BIN_EXE_ad4m-executor"))
    };
    let mut child = command
        .args(run_args(&data_path))
        // Readiness is an info line: an inherited RUST_LOG=warn would hide it.
        .env("RUST_LOG", "rust_executor=info")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("ad4m-executor starts");
    let (ready, shut_down, reader) = read_log(child.stderr.take().unwrap());
    // Built before the wait, so a startup that times out still kills the process.
    let mut executor = Executor {
        child,
        data_path,
        shut_down,
        reader: Some(reader),
    };
    wait_until_ready(&ready);
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

/// Sends the signal and expects the executor to end through its graceful shutdown: its log shows
/// the shutdown's last line. Neither the deadline that ends a cleanup that hangs, nor the signal's
/// default action (no handler), writes it. A rare crash inside `exit` after it still passes: the
/// cleanup had finished.
fn assert_signal_ends_it_gracefully(name: &str, sig: &str) {
    let mut executor = start_with_stdout_reader_gone(name);
    signal(executor.child.id(), sig);
    let status = exit_within(&mut executor.child, EXIT_WITHIN);
    assert!(
        status.is_some(),
        "the executor was still running {EXIT_WITHIN:?} after SIG{sig}"
    );
    // The reader ends once the exited process's stderr is drained.
    executor.reader.take().unwrap().join().unwrap();
    assert!(
        executor.shut_down.load(Ordering::SeqCst),
        "SIG{sig} ended the executor ({:?}) without its graceful shutdown",
        status.unwrap()
    );
}

#[test]
fn sigterm_ends_an_executor_whose_launcher_is_gone() {
    assert_signal_ends_it_gracefully("term", "TERM");
}

#[test]
fn sigint_ends_an_executor_whose_launcher_is_gone() {
    assert_signal_ends_it_gracefully("int", "INT");
}

/// A closed terminal sends SIGHUP.
#[test]
fn sighup_ends_an_executor_whose_launcher_is_gone() {
    assert_signal_ends_it_gracefully("hup", "HUP");
}

/// `nohup ad4m-executor run &` must survive the terminal closing: an executor started with SIGHUP
/// ignored keeps it ignored, and still shuts down on SIGTERM.
#[test]
fn sighup_leaves_an_executor_started_under_nohup_running() {
    let mut executor = start("nohup", true);
    signal(executor.child.id(), "HUP");
    assert!(
        exit_within(&mut executor.child, Duration::from_secs(3)).is_none(),
        "an executor started with SIGHUP ignored ended on SIGHUP"
    );
    signal(executor.child.id(), "TERM");
    assert!(
        exit_within(&mut executor.child, EXIT_WITHIN).is_some(),
        "the executor was still running {EXIT_WITHIN:?} after SIGTERM"
    );
    executor.reader.take().unwrap().join().unwrap();
    assert!(
        executor.shut_down.load(Ordering::SeqCst),
        "SIGTERM ended the executor without its graceful shutdown"
    );
}

/// Written by the executor at startup and removed only by its graceful shutdown.
fn pid_file(data_path: &Path) -> PathBuf {
    data_path.join("executor.pid")
}

/// Starts the executor under `sh`, its launcher, and waits until its API listens. With
/// `exit_with_parent` it is told through `AD4M_EXIT_WITH_PARENT` rather than the flag, so a build
/// without the setting ignores it instead of refusing to start, and a test that needs it fails
/// rather than passing. Without it the variable is removed, so an inherited one cannot turn it on.
fn start_under_sh(name: &str, exit_with_parent: bool) -> (Child, Orphan, BufReader<ChildStdout>) {
    let data_path = fresh_data_path(name);
    let mut command = Command::new("sh");
    command
        .args(["-c", r#""$@" & echo "$!"; wait"#, "sh"])
        .arg(env!("CARGO_BIN_EXE_ad4m-executor"))
        .args(run_args(&data_path))
        .arg("--pid-file")
        .arg(pid_file(&data_path))
        // Readiness is an info line: an inherited RUST_LOG=warn would hide it.
        .env("RUST_LOG", "rust_executor=info")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if exit_with_parent {
        command.env("AD4M_EXIT_WITH_PARENT", "true");
    } else {
        command.env_remove("AD4M_EXIT_WITH_PARENT");
    }
    let mut launcher = command.spawn().expect("sh starts");
    let mut stdout = BufReader::new(launcher.stdout.take().unwrap());
    let mut pid = String::new();
    stdout.read_line(&mut pid).unwrap();
    let executor = Orphan {
        pid: pid.trim().parse().expect("sh prints the executor's PID"),
        data_path,
    };
    let (ready, _, _) = read_log(launcher.stderr.take().unwrap());
    wait_until_ready(&ready);
    (launcher, executor, stdout)
}

/// The launcher, `sh` here, starts the executor and is SIGKILLed, as a cancelled CI job's test
/// runner or a crashed Electron is: nobody signals the executor.
#[test]
fn exit_with_parent_ends_the_executor_once_its_launcher_is_killed() {
    let (mut launcher, executor, _stdout) = start_under_sh("parent", true);
    assert!(
        pid_file(&executor.data_path).exists(),
        "the executor writes its PID file"
    );

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
    // The parent watch starts the graceful shutdown; its deadline exits without it.
    assert!(
        !pid_file(&executor.data_path).exists(),
        "the executor ended without its graceful shutdown (the PID file is still there)"
    );
}

/// Off by default: an executor started in the background from a shell that then exits
/// (`nohup ad4m-executor run &`) keeps running.
#[test]
fn without_exit_with_parent_the_executor_outlives_its_killed_launcher() {
    let (mut launcher, executor, _stdout) = start_under_sh("no-parent-watch", false);

    launcher.kill().unwrap();
    launcher.wait().unwrap();

    // Several of the parent watch's 500 ms polls, and longer than a shutdown without Holochain.
    std::thread::sleep(Duration::from_secs(3));
    assert!(
        alive(executor.pid),
        "the executor ended with its launcher although exit_with_parent is off"
    );
}

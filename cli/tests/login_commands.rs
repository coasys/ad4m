//! `ad4m login` and the commands that follow it, against a started executor.
//!
//! Each test starts `ad4m-executor` on a fresh data folder (no Holochain, the
//! language-language only, no dapp server) and drives the `ad4m` binary.
//! Tokens go to a temp `AD4M_TOKEN_DIR`, one entry per executor URL.
//!
//! The tests run one at a time ([`SERIAL`]): an executor starts a Deno
//! runtime, and two of these at once only slow each other down.

use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::time::{Duration, Instant};

use ad4m_client::ws_rpc::WsRpcClient;
use serde_json::{json, Value};

const EXECUTOR: &str = env!("CARGO_BIN_EXE_ad4m-executor");
const CLI: &str = env!("CARGO_BIN_EXE_ad4m");
const APP_NAME: &str = "AD4M cli";
const APP_DESC: &str = "Command line administration tool for AD4M";
/// How long an executor may take to answer `/health`.
const READY: Duration = Duration::from_secs(120);
/// How long one CLI command may take.
const CLI_BOUND: Duration = Duration::from_secs(60);

static SERIAL: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

struct Executor {
    child: Child,
    port: u16,
    url: String,
    log: PathBuf,
    /// Kept so the data folder lives as long as the process.
    _root: tempfile::TempDir,
}

impl Drop for Executor {
    fn drop(&mut self) {
        if self.child.try_wait().ok().flatten().is_none() {
            let _ = self.child.kill();
            let _ = self.child.wait();
        }
    }
}

impl Executor {
    fn start(admin_credential: &str) -> Self {
        let root = tempfile::Builder::new()
            .prefix("cli-login-")
            .tempdir()
            .expect("a temp folder");
        let data = root.path().join("data");
        let seed = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../tests/js/bootstrapSeed.json");
        assert!(seed.is_file(), "missing bootstrap seed {}", seed.display());

        let init = Command::new(EXECUTOR)
            .args(["init", "--data-path"])
            .arg(&data)
            .arg("--network-bootstrap-seed")
            .arg(&seed)
            .output()
            .expect("ad4m-executor init runs");
        assert!(
            init.status.success(),
            "init failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&init.stdout),
            String::from_utf8_lossy(&init.stderr)
        );

        let port = free_port();
        let log = root.path().join("executor.log");
        let out = std::fs::File::create(&log).unwrap();
        let mut command = Command::new(EXECUTOR);
        command
            .args([
                "run",
                "--app-data-path",
                data.to_str().unwrap(),
                "--port",
                &port.to_string(),
                "--run-holochain",
                "false",
                "--language-language-only",
                "true",
                "--run-dapp-server",
                "false",
                "--admin-credential",
                admin_credential,
            ])
            .stdin(Stdio::null())
            .stdout(out.try_clone().unwrap())
            .stderr(out)
            .env("RUST_LOG", "info");
        // An inherited `AD4M_*` variable would change the start. Set the
        // parent-exit flag after that strip so the strip cannot remove it.
        for (name, _) in std::env::vars().filter(|(name, _)| name.starts_with("AD4M_")) {
            command.env_remove(name);
        }
        command.env("AD4M_EXIT_WITH_PARENT", "true");
        let child = command.spawn().expect("ad4m-executor starts");
        let mut executor = Executor {
            child,
            port,
            url: format!("http://127.0.0.1:{port}"),
            log,
            _root: root,
        };
        executor.ready();
        executor
    }

    fn output(&self) -> String {
        std::fs::read_to_string(&self.log).unwrap_or_default()
    }

    fn ready(&mut self) {
        let started = Instant::now();
        while started.elapsed() < READY {
            if let Some(status) = self.child.try_wait().unwrap() {
                panic!(
                    "the executor exited ({status}) before it was ready:\n{}",
                    tail(&self.output())
                );
            }
            if health(self.port) {
                return;
            }
            std::thread::sleep(Duration::from_millis(200));
        }
        panic!(
            "the executor was not ready in {READY:?}:\n{}",
            tail(&self.output())
        );
    }
}

fn free_port() -> u16 {
    std::net::TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

fn health(port: u16) -> bool {
    use std::io::{Read, Write};
    use std::net::{SocketAddr, TcpStream};
    let Ok(mut stream) = TcpStream::connect_timeout(
        &SocketAddr::from(([127, 0, 0, 1], port)),
        Duration::from_secs(1),
    ) else {
        return false;
    };
    let _ = stream.set_read_timeout(Some(Duration::from_secs(2)));
    let request =
        format!("GET /health HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nConnection: close\r\n\r\n");
    if stream.write_all(request.as_bytes()).is_err() {
        return false;
    }
    let mut response = String::new();
    let _ = stream.read_to_string(&mut response);
    response.starts_with("HTTP/1.1 200") && response.contains(r#""status":"ok""#)
}

fn tail(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    let start = lines.len().saturating_sub(80);
    lines[start..].join("\n")
}

/// A running `ad4m`. Stdout lines arrive on `lines` as the process prints them.
struct Cli {
    child: Child,
    stdin: Option<std::process::ChildStdin>,
    lines: Receiver<String>,
    stdout_thread: Option<std::thread::JoinHandle<()>>,
    stderr_thread: Option<std::thread::JoinHandle<String>>,
}

fn read_stderr(stderr: impl std::io::Read) -> String {
    let mut err = String::new();
    let mut reader = BufReader::new(stderr);
    let _ = std::io::Read::read_to_string(&mut reader, &mut err);
    err
}

fn spawn_cli(token_dir: &Path, args: &[&str]) -> Cli {
    let mut command = Command::new(CLI);
    command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    for (name, _) in std::env::vars().filter(|(name, _)| name.starts_with("AD4M_")) {
        command.env_remove(name);
    }
    command.env("AD4M_TOKEN_DIR", token_dir);
    let mut child = command.spawn().expect("ad4m starts");
    let stdout = child.stdout.take().unwrap();
    let stderr = child.stderr.take().unwrap();
    let (tx, lines) = mpsc::channel();
    let stdout_thread = std::thread::spawn(move || {
        let reader = BufReader::new(stdout);
        for line in reader.lines() {
            let Ok(line) = line else { break };
            if tx.send(line).is_err() {
                break;
            }
        }
    });
    let stderr_thread = std::thread::spawn(move || read_stderr(stderr));
    let stdin = child.stdin.take();
    Cli {
        child,
        stdin,
        lines,
        stdout_thread: Some(stdout_thread),
        stderr_thread: Some(stderr_thread),
    }
}

struct CliOutput {
    status: ExitStatus,
    stdout: String,
    stderr: String,
}

impl Cli {
    /// Block until the process exits, or kill it at `bound`.
    fn finish(mut self, bound: Duration) -> CliOutput {
        let started = Instant::now();
        let status = loop {
            if let Some(status) = self.child.try_wait().expect("try_wait") {
                break status;
            }
            if started.elapsed() > bound {
                let _ = self.child.kill();
                let _ = self.child.wait();
                let (stdout, stderr) = self.collect();
                panic!("ad4m did not exit within {bound:?}\nstdout:\n{stdout}\nstderr:\n{stderr}");
            }
            std::thread::sleep(Duration::from_millis(50));
        };
        drop(self.stdin.take());
        let (stdout, stderr) = self.collect();
        CliOutput {
            status,
            stdout,
            stderr,
        }
    }

    fn collect(&mut self) -> (String, String) {
        // Closing the child's pipes lets the reader threads finish. The
        // stdout thread exits when the pipe closes, which happens when the
        // process exits. Drain what it already sent, then join.
        let mut stdout = String::new();
        while let Ok(line) = self.lines.try_recv() {
            stdout.push_str(&line);
            stdout.push('\n');
        }
        if let Some(handle) = self.stdout_thread.take() {
            let _ = handle.join();
        }
        while let Ok(line) = self.lines.try_recv() {
            stdout.push_str(&line);
            stdout.push('\n');
        }
        let stderr = self
            .stderr_thread
            .take()
            .map(|handle| handle.join().unwrap_or_default())
            .unwrap_or_default();
        (stdout, stderr)
    }

    /// The next stdout line that starts with `prefix`, within `bound`.
    fn wait_line(&mut self, prefix: &str, bound: Duration) -> String {
        let started = Instant::now();
        let mut seen = String::new();
        loop {
            let left = bound.saturating_sub(started.elapsed());
            if left.is_zero() {
                panic!("timed out waiting for {prefix:?}\nstdout so far:\n{seen}");
            }
            match self
                .lines
                .recv_timeout(left.min(Duration::from_millis(200)))
            {
                Ok(line) if line.starts_with(prefix) => return line,
                Ok(line) => {
                    seen.push_str(&line);
                    seen.push('\n');
                }
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    if let Some(status) = self.child.try_wait().ok().flatten() {
                        panic!("ad4m exited ({status}) before printing {prefix:?}\nstdout so far:\n{seen}");
                    }
                }
                Err(mpsc::RecvTimeoutError::Disconnected) => {
                    panic!("stdout closed before {prefix:?}\nstdout so far:\n{seen}");
                }
            }
        }
    }

    fn write_line(&mut self, line: &str) {
        let stdin = self.stdin.as_mut().expect("stdin is piped");
        writeln!(stdin, "{line}").expect("write stdin");
        stdin.flush().expect("flush stdin");
    }
}

impl Drop for Cli {
    fn drop(&mut self) {
        if self.child.try_wait().ok().flatten().is_none() {
            let _ = self.child.kill();
            let _ = self.child.wait();
        }
    }
}

fn run_cli(token_dir: &Path, args: &[&str]) -> CliOutput {
    spawn_cli(token_dir, args).finish(CLI_BOUND)
}

fn assert_ok(out: &CliOutput, what: &str) {
    assert!(
        out.status.success(),
        "{what} failed ({})\nstdout:\n{}\nstderr:\n{}",
        out.status,
        out.stdout,
        out.stderr
    );
}

fn token_for(token_dir: &Path, url: &str) -> String {
    let path = token_dir.join("tokens.json");
    let text =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
    let store: Value =
        serde_json::from_str(&text).unwrap_or_else(|e| panic!("parse tokens: {e}\n{text}"));
    let key = format!("{url}|{APP_NAME}");
    let versions = store["versions"]
        .as_object()
        .unwrap_or_else(|| panic!("no versions in {text}"));
    for session in versions.values() {
        if let Some(token) = session["tokens"].get(&key).and_then(Value::as_str) {
            return token.to_string();
        }
    }
    panic!("no token for {key} in {text}");
}

/// `runtime.info` answers 404 until an agent exists. This is executor setup.
/// It does not write a CLI token.
async fn generate_agent(url: &str, credential: &str) {
    let admin = admin_ws(url, credential).await;
    admin
        .call::<Value>("agent.generate", json!({ "passphrase": "passphrase" }))
        .await
        .unwrap_or_else(|e| panic!("agent.generate on {url}: {e}"));
    admin.close().await;
}

/// The main commands, against `url`, with whatever token the dir holds.
fn main_commands(token_dir: &Path, url: &str) {
    let status = run_cli(token_dir, &["--executor-url", url, "agent", "status"]);
    assert_ok(&status, "agent status");
    assert!(
        status.stdout.contains("is_unlocked"),
        "agent status output:\n{}",
        status.stdout
    );

    let info = run_cli(token_dir, &["--executor-url", url, "runtime", "info"]);
    assert_ok(&info, "runtime info");
    assert!(
        info.stdout.contains("ad4m_executor_version"),
        "runtime info output:\n{}",
        info.stdout
    );

    let languages = run_cli(token_dir, &["--executor-url", url, "languages"]);
    assert_ok(&languages, "languages");
}

async fn admin_ws(url: &str, credential: &str) -> WsRpcClient {
    let deadline = Instant::now() + READY;
    loop {
        match WsRpcClient::connect(url, credential).await {
            Ok(ws) => return ws,
            Err(e) => {
                assert!(Instant::now() < deadline, "admin socket did not open: {e}");
                tokio::time::sleep(Duration::from_millis(300)).await;
            }
        }
    }
}

/// Approve the capability request the CLI printed and return the 6-digit code.
async fn permit(admin: &WsRpcClient, request_id: &str) -> String {
    let auth = serde_json::to_string(&json!({
        "requestId": request_id,
        "auth": {
            "appName": APP_NAME,
            "appDesc": APP_DESC,
            "capabilities": [{
                "can": ["*"],
                "with": { "domain": "*", "pointers": ["*"] }
            }],
        }
    }))
    .unwrap();
    admin
        .call("agent.permitCapability", json!({ "auth": auth }))
        .await
        .unwrap_or_else(|e| panic!("permitCapability: {e}"))
}

/// Admin credential: the CLI stores it for that URL. A second executor keeps
/// its own token, and the first executor still accepts the first token.
/// `ws://` and `http://` for one executor share the token.
#[tokio::test(flavor = "multi_thread")]
async fn admin_login_stores_one_token_per_executor_and_commands_use_it() {
    let _serial = SERIAL.lock().await;
    let token_dir = tempfile::tempdir().unwrap();
    let first = Executor::start("cli-1779-admin-a");
    let second = Executor::start("cli-1779-admin-b");

    let before = run_cli(
        token_dir.path(),
        &["--executor-url", &first.url, "agent", "status"],
    );
    assert!(
        !before.status.success(),
        "agent status without a token must fail on an executor that has an admin credential\n{}",
        before.stdout
    );
    generate_agent(&first.url, "cli-1779-admin-a").await;
    generate_agent(&second.url, "cli-1779-admin-b").await;

    let login = run_cli(
        token_dir.path(),
        &[
            "--admin-credential",
            "cli-1779-admin-a",
            "login",
            &first.url,
        ],
    );
    assert_ok(&login, "login first");
    assert!(
        login.stdout.contains("Logged in"),
        "login output:\n{}",
        login.stdout
    );
    assert_eq!(token_for(token_dir.path(), &first.url), "cli-1779-admin-a");

    main_commands(token_dir.path(), &first.url);
    // The same executor under the ws URL: one stored token.
    let ws = first.url.replacen("http://", "ws://", 1);
    let via_ws = run_cli(
        token_dir.path(),
        &["--executor-url", &ws, "agent", "status"],
    );
    assert_ok(&via_ws, "agent status via ws URL");

    let login_b = run_cli(
        token_dir.path(),
        &[
            "--admin-credential",
            "cli-1779-admin-b",
            "login",
            &second.url,
        ],
    );
    assert_ok(&login_b, "login second");
    assert_eq!(
        token_for(token_dir.path(), &first.url),
        "cli-1779-admin-a",
        "logging in to the second executor must keep the first token"
    );
    assert_eq!(token_for(token_dir.path(), &second.url), "cli-1779-admin-b");

    main_commands(token_dir.path(), &first.url);
    main_commands(token_dir.path(), &second.url);
}

/// Capability handshake: the CLI prints `request_id`, the test approves it
/// the way the launcher would, and the CLI stores the minted token.
#[tokio::test(flavor = "multi_thread")]
async fn capability_login_stores_a_token_and_commands_use_it() {
    let _serial = SERIAL.lock().await;
    let executor = Executor::start("cli-1779-admin-cap");
    let admin = admin_ws(&executor.url, "cli-1779-admin-cap").await;
    admin
        .call::<Value>("agent.generate", json!({ "passphrase": "passphrase" }))
        .await
        .unwrap_or_else(|e| panic!("agent.generate: {e}"));

    let token_dir = tempfile::tempdir().unwrap();
    let mut cli = spawn_cli(token_dir.path(), &["login", &executor.url]);
    let line = cli.wait_line("request_id: ", CLI_BOUND);
    let request_id = line.trim_start_matches("request_id: ").trim().to_string();
    assert!(!request_id.is_empty(), "empty request id in {line}");
    let code = permit(&admin, &request_id).await;
    cli.write_line(&code);
    let out = cli.finish(CLI_BOUND);
    assert_ok(&out, "capability login");
    assert!(
        out.stdout.contains("Logged in"),
        "login output:\n{}\nstderr:\n{}",
        out.stdout,
        out.stderr
    );
    let stored = token_for(token_dir.path(), &executor.url);
    assert!(!stored.is_empty(), "the minted token is empty");
    assert_ne!(stored, "cli-1779-admin-cap");

    main_commands(token_dir.path(), &executor.url);
    admin.close().await;
}

/// Multi-user: email and a password create the account (email test mode, so
/// no SMTP). The stored token is that user's, and the commands run with it.
#[tokio::test(flavor = "multi_thread")]
async fn email_login_stores_a_token_and_commands_use_it() {
    let _serial = SERIAL.lock().await;
    let executor = Executor::start("cli-1779-admin-mail");
    let admin = admin_ws(&executor.url, "cli-1779-admin-mail").await;
    admin
        .call::<Value>("agent.generate", json!({ "passphrase": "passphrase" }))
        .await
        .unwrap_or_else(|e| panic!("agent.generate: {e}"));
    admin
        .call::<bool>("user.setMultiUserEnabled", json!({ "enabled": true }))
        .await
        .unwrap_or_else(|e| panic!("setMultiUserEnabled: {e}"));
    admin
        .call::<bool>("user.emailTest", json!({ "action": "enable" }))
        .await
        .unwrap_or_else(|e| panic!("emailTest enable: {e}"));

    let token_dir = tempfile::tempdir().unwrap();
    let mut cli = spawn_cli(
        token_dir.path(),
        &["login", &executor.url, "--email", "ada@example.com"],
    );
    // New account: one password prompt. The line is already in the pipe
    // when the prompt reads stdin.
    cli.write_line("correct horse");
    let out = cli.finish(Duration::from_secs(90));
    assert_ok(&out, "email login");
    assert!(
        out.stdout.contains("Logged in"),
        "login output:\n{}\nstderr:\n{}",
        out.stdout,
        out.stderr
    );
    let stored = token_for(token_dir.path(), &executor.url);
    assert!(!stored.is_empty());
    assert_ne!(stored, "cli-1779-admin-mail");

    main_commands(token_dir.path(), &executor.url);
    admin.close().await;
}

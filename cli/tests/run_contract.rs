//! The executor's run contract, end to end: each test starts the built `ad4m-executor` binary
//! on a fresh data folder (no Holochain, language-language only, no dapp server) and checks
//! what a supervisor sees: the exit code, the data-folder lock, the stop on stdin's end, the
//! custody channel and `.local_only()` over a real WebSocket.
//!
//! The tests run one at a time ([`serial`]): each executor starts a Deno runtime and its
//! services, and a few of them at once only slow each other down.

use std::fs::File;
use std::io::{Read, Write};
use std::net::{IpAddr, SocketAddr, TcpListener, TcpStream, UdpSocket};
use std::path::PathBuf;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

use ad4m_client::ws_rpc::WsRpcClient;
use serde_json::{json, Value};

const BINARY: &str = env!("CARGO_BIN_EXE_ad4m-executor");
const ADMIN: &str = "run-contract-admin-credential";
const CUSTODY: &str = "run-contract-custody-credential";
/// How long an executor may take to answer `/health`.
const READY: Duration = Duration::from_secs(120);
/// How long a stop, or a refused start, may take to exit.
const EXIT: Duration = Duration::from_secs(30);

/// An async mutex: the async tests hold it across their awaits, the others take it blocking.
static SERIAL: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

/// For the tests that run outside a runtime.
fn serial() -> tokio::sync::MutexGuard<'static, ()> {
    SERIAL.blocking_lock()
}

/// A data folder that `ad4m-executor init` has prepared, removed with the test.
struct DataFolder {
    root: tempfile::TempDir,
}

impl DataFolder {
    fn new() -> Self {
        let root = tempfile::Builder::new()
            .prefix("run-contract-")
            .tempdir()
            .expect("a temp folder");
        let folder = DataFolder { root };
        write_private(&folder.secret(), &format!("{}\n", "5a".repeat(32)));
        let init = Command::new(BINARY)
            .args(["init", "--data-path"])
            .arg(folder.data())
            .output()
            .expect("ad4m-executor init runs");
        assert!(
            init.status.success(),
            "init failed: {}",
            String::from_utf8_lossy(&init.stdout)
        );
        folder
    }

    fn data(&self) -> PathBuf {
        self.root.path().join("data")
    }

    fn file(&self, name: &str) -> PathBuf {
        self.root.path().join(name)
    }

    /// The storage secret file the executor opens its data with (beside the data folder, on
    /// its disk: `run_args` allows that).
    fn secret(&self) -> PathBuf {
        self.file("storage-secret")
    }
}

/// Writes `content` to a new file of mode 0600.
fn write_private(path: &std::path::Path, content: &str) {
    use std::os::unix::fs::OpenOptionsExt;
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(path)
        .unwrap();
    file.write_all(content.as_bytes()).unwrap();
}

/// The variable naming the storage secret file. A test that gives it empty starts the
/// executor without a secret: sealed.
const SECRET_FILE_VAR: &str = "AD4M_STORAGE_SECRET_FILE";

/// A port nothing listens on right now.
fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

/// A running `ad4m-executor run`, killed when dropped. Its output goes to a file, printed
/// when an assertion on it fails.
struct Executor {
    child: Child,
    port: u16,
    log: PathBuf,
}

impl Drop for Executor {
    fn drop(&mut self) {
        if self.child.try_wait().ok().flatten().is_none() {
            let _ = self.child.kill();
            let _ = self.child.wait();
        }
    }
}

/// The arguments every test starts with, plus `extra`.
fn run_args(folder: &DataFolder, port: u16, extra: &[&str]) -> Vec<String> {
    let mut args: Vec<String> = [
        "run",
        "--app-data-path",
        folder.data().to_str().unwrap(),
        "--port",
        &port.to_string(),
        "--run-holochain",
        "false",
        "--language-language-only",
        "true",
        "--run-dapp-server",
        "false",
        "--allow-secret-on-data-disk",
        "true",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();
    args.extend(extra.iter().map(|s| s.to_string()));
    args
}

/// Starts the executor with `args`; `stdin` is what its stdin is. `log` names its output file.
fn spawn(
    folder: &DataFolder,
    port: u16,
    args: Vec<String>,
    stdin: Stdio,
    log: &str,
    env: &[(&str, &str)],
) -> Executor {
    let log = folder.file(log);
    let out = File::create(&log).unwrap();
    let mut command = Command::new(BINARY);
    command
        .args(&args)
        .stdin(stdin)
        .stdout(out.try_clone().unwrap())
        .stderr(out)
        .env("RUST_LOG", "info");
    // Only the settings a test gives: an inherited `AD4M_*` variable (a credential,
    // `AD4M_WATCH_STDIN`) would change the start.
    for (name, _) in std::env::vars().filter(|(name, _)| name.starts_with("AD4M_")) {
        command.env_remove(name);
    }
    command.env(SECRET_FILE_VAR, folder.secret());
    for (name, value) in env {
        if *name == SECRET_FILE_VAR && value.is_empty() {
            command.env_remove(name);
        } else {
            command.env(name, value);
        }
    }
    Executor {
        child: command.spawn().expect("ad4m-executor starts"),
        port,
        log,
    }
}

/// Starts the executor as a custody service's supervisor does: stdin is a pipe, `first` (when
/// given) is written to it, and the pipe stays open while the [`Executor`] holds it. `None`
/// writes nothing.
fn spawn_with_stdin(
    folder: &DataFolder,
    port: u16,
    args: Vec<String>,
    first: Option<&[u8]>,
    log: &str,
    env: &[(&str, &str)],
) -> Executor {
    let mut executor = spawn(folder, port, args, Stdio::piped(), log, env);
    if let Some(bytes) = first {
        let stdin = executor.child.stdin.as_mut().expect("a piped stdin");
        stdin.write_all(bytes).expect("write the first line");
        stdin.flush().expect("flush the first line");
    }
    executor
}

/// The custody credential as a supervisor writes it: one line on stdin.
fn custody_line() -> Vec<u8> {
    format!("{CUSTODY}\n").into_bytes()
}

impl Executor {
    fn output(&self) -> String {
        std::fs::read_to_string(&self.log).unwrap_or_default()
    }

    /// Waits until `/health` answers on loopback; panics with the output if the executor exits
    /// first or is not ready in time.
    fn ready(&mut self) -> &mut Self {
        let started = Instant::now();
        while started.elapsed() < READY {
            if let Some(status) = self.child.try_wait().unwrap() {
                panic!(
                    "the executor exited ({status}) before it was ready:\n{}",
                    self.output()
                );
            }
            if health(self.port) {
                return self;
            }
            std::thread::sleep(Duration::from_millis(200));
        }
        panic!(
            "the executor was not ready in {READY:?}:\n{}",
            self.output()
        );
    }

    /// Waits for the process to exit, at most `bound`.
    fn exit_within(&mut self, bound: Duration) -> ExitStatus {
        let started = Instant::now();
        while started.elapsed() < bound {
            if let Some(status) = self.child.try_wait().unwrap() {
                return status;
            }
            std::thread::sleep(Duration::from_millis(100));
        }
        panic!(
            "the executor did not exit within {bound:?}:\n{}",
            self.output()
        );
    }

    fn running(&mut self) -> bool {
        self.child.try_wait().unwrap().is_none()
    }

    fn url(&self) -> String {
        format!("http://127.0.0.1:{}", self.port)
    }
}

/// Whether `GET /health` on loopback answers `{"status":"ok"}`.
fn health(port: u16) -> bool {
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

fn code(status: ExitStatus) -> Option<i32> {
    status.code()
}

/// The message of a refused call (`RPC error <code>: <message>`).
fn refusal<T: std::fmt::Debug>(result: anyhow::Result<T>) -> String {
    result.expect_err("the call is refused").to_string()
}

/// A token for an app with `agent:READ`, approved by the operator: not the admin credential.
async fn app_token(url: &str) -> String {
    let anonymous = WsRpcClient::connect(url, "").await.unwrap();
    let capabilities =
        json!([{ "with": { "domain": "agent", "pointers": ["*"] }, "can": ["READ"] }]);
    let request_id: String = anonymous
        .call(
            "agent.requestCapability",
            json!({ "authInfo": {
                "appName": "run-contract", "appDesc": "the run contract tests",
                "appDomain": "test.ad4m.org", "appUrl": "https://test.ad4m.org",
                "capabilities": capabilities,
            }}),
        )
        .await
        .unwrap();
    let admin = WsRpcClient::connect(url, ADMIN).await.unwrap();
    let auth = json!({ "requestId": request_id, "auth": {
        "appName": "run-contract", "appDesc": "the run contract tests",
        "appUrl": "https://test.ad4m.org", "capabilities": capabilities,
    }});
    let rand: String = admin
        .call(
            "agent.permitCapability",
            json!({ "auth": auth.to_string() }),
        )
        .await
        .unwrap();
    anonymous
        .call(
            "agent.generateJwt",
            json!({ "requestId": request_id, "rand": rand }),
        )
        .await
        .unwrap()
}

/// `--watch-stdin true`: when the supervisor's end of stdin closes, the executor stops with 0.
#[test]
fn closing_stdin_with_watch_stdin_stops_the_executor_with_0() {
    let _serial = serial();
    let folder = DataFolder::new();
    let port = free_port();
    let args = run_args(
        &folder,
        port,
        &["--insecure-no-admin-credential", "--watch-stdin", "true"],
    );
    let mut executor = spawn(&folder, port, args, Stdio::piped(), "watch.log", &[]);
    executor.ready();
    // Still running while the pipe is open.
    std::thread::sleep(Duration::from_secs(1));
    assert!(executor.running(), "{}", executor.output());
    drop(executor.child.stdin.take());
    let status = executor.exit_within(EXIT);
    assert_eq!(code(status), Some(0), "{}", executor.output());
    assert!(
        executor.output().contains("stdin closed"),
        "{}",
        executor.output()
    );
}

/// A supervisor that is gone before the executor starts: with `--watch-stdin` and stdin
/// already closed, the executor stops with 0 instead of running on without one. The stop comes
/// while start-up still opens the databases; it used to end one start in three with
/// SIGSEGV (the exit ran SQLCipher's `atexit` teardown under them), so the test stops several.
#[test]
fn watch_stdin_with_stdin_closed_at_start_stops_the_executor_with_0() {
    let _serial = serial();
    for _ in 0..8 {
        let folder = DataFolder::new();
        let port = free_port();
        let args = run_args(
            &folder,
            port,
            &["--insecure-no-admin-credential", "--watch-stdin", "true"],
        );
        let mut executor = spawn(&folder, port, args, Stdio::null(), "watch-closed.log", &[]);
        let status = executor.exit_within(EXIT);
        assert_eq!(code(status), Some(0), "{}", executor.output());
    }
}

/// One executor per data folder: a second on the same folder exits 79 and the first keeps
/// serving. With the same `--log-dir`, the second leaves the first's log in place: it reports
/// to stdout and rotates nothing.
#[test]
fn a_second_executor_on_the_same_data_folder_exits_79_and_the_first_keeps_running() {
    let _serial = serial();
    let folder = DataFolder::new();
    let logs = folder.file("logs");
    let log_dir = ["--log-dir", logs.to_str().unwrap()];
    let first_port = free_port();
    let mut flags = vec!["--insecure-no-admin-credential"];
    flags.extend(log_dir);
    let args = run_args(&folder, first_port, &flags);
    let mut first = spawn(&folder, first_port, args, Stdio::null(), "first.log", &[]);
    first.ready();
    let live = logs.join("executor-0.log");

    let second_port = free_port();
    let args = run_args(&folder, second_port, &flags);
    let mut second = spawn(&folder, second_port, args, Stdio::null(), "second.log", &[]);
    let status = second.exit_within(EXIT);
    assert_eq!(code(status), Some(79), "{}", second.output());
    assert!(
        second.output().contains("another executor"),
        "{}",
        second.output()
    );

    assert!(first.running(), "{}", first.output());
    assert!(health(first_port), "{}", first.output());
    // The first run started on an empty log folder: an `executor-1.log` is a rotation.
    assert!(
        !logs.join("executor-1.log").exists(),
        "the refused start rotated the log"
    );
    let first_log = std::fs::read_to_string(&live).unwrap();
    assert!(!first_log.contains("another executor"), "{first_log}");
}

/// A log folder the executor cannot make fails the start with 1, and the output says why.
#[test]
fn a_log_folder_that_cannot_be_made_fails_the_start_and_says_why() {
    let _serial = serial();
    let folder = DataFolder::new();
    let not_a_folder = folder.file("not-a-folder");
    std::fs::write(&not_a_folder, "a file").unwrap();
    let log_dir = not_a_folder.join("logs");
    let port = free_port();
    let args = run_args(
        &folder,
        port,
        &[
            "--insecure-no-admin-credential",
            "--log-dir",
            log_dir.to_str().unwrap(),
        ],
    );
    let mut executor = spawn(&folder, port, args, Stdio::null(), "log-dir.log", &[]);
    assert_eq!(
        code(executor.exit_within(EXIT)),
        Some(1),
        "{}",
        executor.output()
    );
    assert!(
        executor.output().contains("Cannot write the executor log"),
        "{}",
        executor.output()
    );
}

/// A start the executor refuses exits 78, whether the CLI refuses it (the custody channel with
/// multi-user mode on the command line, the custody channel with stdin closed before its line, a
/// config file with an unknown key, an empty `AD4M_*` variable) or `run` does (the custody channel with multi-user mode left on in the database
/// by an earlier run).
#[tokio::test(flavor = "multi_thread")]
async fn a_refused_start_exits_78() {
    let _serial = SERIAL.lock().await;
    let folder = DataFolder::new();
    let credential = [("AD4M_ADMIN_CREDENTIAL", ADMIN)];

    let port = free_port();
    let args = run_args(
        &folder,
        port,
        &["--custody-channel", "--enable-multi-user", "true"],
    );
    let mut flags = spawn(&folder, port, args, Stdio::null(), "flags.log", &credential);
    assert_eq!(
        code(flags.exit_within(EXIT)),
        Some(78),
        "{}",
        flags.output()
    );
    assert!(
        flags.output().contains("custody channel"),
        "{}",
        flags.output()
    );

    let port = free_port();
    let args = run_args(&folder, port, &["--custody-channel"]);
    let mut no_custody_credential = spawn(
        &folder,
        port,
        args,
        Stdio::null(),
        "no-custody-credential.log",
        &credential,
    );
    assert_eq!(
        code(no_custody_credential.exit_within(EXIT)),
        Some(78),
        "{}",
        no_custody_credential.output()
    );
    assert!(
        no_custody_credential
            .output()
            .contains("first line of stdin"),
        "{}",
        no_custody_credential.output()
    );

    let config = folder.file("config.json");
    std::fs::write(&config, r#"{"no_such_key": true}"#).unwrap();
    let port = free_port();
    let args = run_args(&folder, port, &["--config", config.to_str().unwrap()]);
    let mut invalid = spawn(
        &folder,
        port,
        args,
        Stdio::null(),
        "invalid.log",
        &credential,
    );
    assert_eq!(
        code(invalid.exit_within(EXIT)),
        Some(78),
        "{}",
        invalid.output()
    );
    assert!(
        invalid.output().contains("no_such_key"),
        "{}",
        invalid.output()
    );

    // A flag clap refuses (an empty variable is never laid over the file).
    let port = free_port();
    let args = run_args(&folder, port, &[]);
    let mut empty = spawn(
        &folder,
        port,
        args,
        Stdio::null(),
        "empty.log",
        &[("AD4M_ADMIN_CREDENTIAL", ADMIN), ("AD4M_LOG_DIR", "")],
    );
    assert_eq!(
        code(empty.exit_within(EXIT)),
        Some(78),
        "{}",
        empty.output()
    );
    assert!(
        empty.output().contains("AD4M_LOG_DIR is set but empty"),
        "{}",
        empty.output()
    );

    // Multi-user mode on in the database, then a start with the custody channel.
    let port = free_port();
    let args = run_args(&folder, port, &[]);
    let mut earlier = spawn(
        &folder,
        port,
        args,
        Stdio::null(),
        "earlier.log",
        &credential,
    );
    earlier.ready();
    let admin = WsRpcClient::connect(&earlier.url(), ADMIN).await.unwrap();
    let on: bool = admin
        .call("user.setMultiUserEnabled", json!({ "enabled": true }))
        .await
        .unwrap();
    assert!(on);
    drop(admin);
    drop(earlier);

    // `run` refuses this one with the databases open and start-up work still running: the exit
    // used to run SQLCipher's `atexit` teardown under that work and end three starts in four
    // with SIGSEGV, so the test refuses several.
    for _ in 0..4 {
        let port = free_port();
        let args = run_args(&folder, port, &["--custody-channel"]);
        let mut custody = spawn_with_stdin(
            &folder,
            port,
            args,
            Some(&custody_line()),
            "custody.log",
            &credential,
        );
        assert_eq!(
            code(custody.exit_within(READY)),
            Some(78),
            "{}",
            custody.output()
        );
        assert!(
            custody.output().contains("custody channel"),
            "{}",
            custody.output()
        );
    }
}

/// The custody channel over a real WebSocket on a loopback listener: with `--custody-channel`
/// the custody credential gets the main agent's device keys; the admin credential gets
/// `no-custody-channel` (an app's window may hold it); an app's token and no token are refused
/// by the method's `Operator` guard; the tokenless operator of an executor without an admin
/// credential, and the admin credential without the flag, get `no-custody-channel`.
#[tokio::test(flavor = "multi_thread")]
async fn the_custody_channel_answers_only_the_custody_credential_on_loopback_with_the_flag() {
    let _serial = SERIAL.lock().await;
    let folder = DataFolder::new();
    let credential = [("AD4M_ADMIN_CREDENTIAL", ADMIN)];

    let port = free_port();
    let args = run_args(&folder, port, &["--custody-channel"]);
    let mut on = spawn_with_stdin(
        &folder,
        port,
        args,
        Some(&custody_line()),
        "on.log",
        &credential,
    );
    on.ready();
    let admin = WsRpcClient::connect(&on.url(), ADMIN).await.unwrap();
    let _: Value = admin
        .call("agent.generate", json!({ "passphrase": "run-contract" }))
        .await
        .unwrap();
    for method in ["identity.custodyStatus", "identity.takeMaster"] {
        let refused = refusal(admin.call::<Value>(method, json!({})).await);
        assert!(
            refused.starts_with("RPC error 403: no-custody-channel"),
            "{method}: {refused}"
        );
    }
    let custody = WsRpcClient::connect(&on.url(), CUSTODY).await.unwrap();
    let keys: Value = custody
        .call("identity.custodyStatus", json!({}))
        .await
        .unwrap();
    assert!(
        keys["sign"].as_str().unwrap().starts_with("did:key:z6Mk"),
        "{keys}"
    );
    assert!(!keys["enc"].as_str().unwrap().is_empty(), "{keys}");

    let app = WsRpcClient::connect(&on.url(), &app_token(&on.url()).await)
        .await
        .unwrap();
    let refused = refusal(app.call::<Value>("identity.custodyStatus", json!({})).await);
    assert_eq!(refused, "RPC error 403: Admin credential required");
    let anonymous = WsRpcClient::connect(&on.url(), "").await.unwrap();
    let refused = refusal(
        anonymous
            .call::<Value>("identity.custodyStatus", json!({}))
            .await,
    );
    assert_eq!(refused, "RPC error 403: Admin credential required");
    drop((admin, custody, app, anonymous));
    drop(on);

    // No admin credential: the tokenless operator passes the guard but not the channel.
    let port = free_port();
    let args = run_args(
        &folder,
        port,
        &["--custody-channel", "--insecure-no-admin-credential"],
    );
    let mut tokenless = spawn_with_stdin(
        &folder,
        port,
        args,
        Some(&custody_line()),
        "tokenless.log",
        &[],
    );
    tokenless.ready();
    let operator = WsRpcClient::connect(&tokenless.url(), "").await.unwrap();
    let refused = refusal(
        operator
            .call::<Value>("identity.custodyStatus", json!({}))
            .await,
    );
    assert!(
        refused.starts_with("RPC error 403: no-custody-channel"),
        "{refused}"
    );
    drop(operator);
    drop(tokenless);

    // The flag off: the admin credential on loopback is refused.
    let port = free_port();
    let args = run_args(&folder, port, &[]);
    let mut off = spawn(&folder, port, args, Stdio::null(), "off.log", &credential);
    off.ready();
    let admin = WsRpcClient::connect(&off.url(), ADMIN).await.unwrap();
    let refused = refusal(
        admin
            .call::<Value>("identity.custodyStatus", json!({}))
            .await,
    );
    assert!(
        refused.starts_with("RPC error 403: no-custody-channel"),
        "{refused}"
    );
}

/// The custody credential reaches the executor only as the first line of its stdin: it opens
/// the custody channel, and it is in no file the admin credential can read. Not in the process
/// environment (`/proc/<pid>/environ`) nor on the command line (`/proc/<pid>/cmdline`), and
/// not in the log, `runtime.config` or `runtime.info`.
#[tokio::test(flavor = "multi_thread")]
async fn the_custody_credential_comes_as_the_first_line_of_stdin_and_nowhere_else() {
    let _serial = SERIAL.lock().await;
    let folder = DataFolder::new();
    let config = folder.file("config.json");
    std::fs::write(&config, "{}").unwrap();
    let port = free_port();
    let args = run_args(
        &folder,
        port,
        &["--custody-channel", "--config", config.to_str().unwrap()],
    );
    let mut executor = spawn_with_stdin(
        &folder,
        port,
        args,
        Some(&custody_line()),
        "stdin-credential.log",
        &[("AD4M_ADMIN_CREDENTIAL", ADMIN)],
    );
    executor.ready();

    let admin = WsRpcClient::connect(&executor.url(), ADMIN).await.unwrap();
    let _: Value = admin
        .call("agent.generate", json!({ "passphrase": "run-contract" }))
        .await
        .unwrap();
    let custody = WsRpcClient::connect(&executor.url(), CUSTODY)
        .await
        .unwrap();
    let keys: Value = custody
        .call("identity.custodyStatus", json!({}))
        .await
        .unwrap();
    assert!(
        keys["sign"].as_str().unwrap().starts_with("did:key:z6Mk"),
        "{keys}"
    );

    for method in ["runtime.config", "runtime.info"] {
        let answer: Value = admin.call(method, json!({})).await.unwrap();
        assert!(!answer.to_string().contains(CUSTODY), "{method}: {answer}");
    }
    assert!(
        !executor.output().contains(CUSTODY),
        "{}",
        executor.output()
    );

    #[cfg(target_os = "linux")]
    {
        let pid = executor.child.id();
        for file in ["environ", "cmdline"] {
            let bytes = match std::fs::read(format!("/proc/{pid}/{file}")) {
                Ok(bytes) => bytes,
                // The executor made itself not dumpable (no core dumps): the kernel then closes
                // its environ to every other process of the user too. Nobody reads it.
                Err(e) if file == "environ" && e.kind() == std::io::ErrorKind::PermissionDenied => {
                    continue
                }
                Err(e) => panic!("/proc/{pid}/{file}: {e}"),
            };
            assert!(!bytes.is_empty() || file == "environ", "empty {file}");
            assert!(
                !bytes
                    .windows(CUSTODY.len())
                    .any(|window| window == CUSTODY.as_bytes()),
                "the custody credential is in /proc/{pid}/{file}"
            );
        }
    }
}

/// The custody credential in the environment is refused with 78, even with the line on stdin:
/// any process of the same user reads another's environment (`/proc/<pid>/environ`), and the
/// admin credential reads host files. `AD4M_CUSTODY_CREDENTIAL_FILE` likewise.
#[test]
fn the_custody_credential_in_the_environment_is_refused_with_78() {
    let _serial = serial();
    let folder = DataFolder::new();
    for (name, log) in [
        ("AD4M_CUSTODY_CREDENTIAL", "env-credential.log"),
        ("AD4M_CUSTODY_CREDENTIAL_FILE", "env-credential-file.log"),
    ] {
        let port = free_port();
        let args = run_args(&folder, port, &["--custody-channel"]);
        let mut executor = spawn_with_stdin(
            &folder,
            port,
            args,
            Some(&custody_line()),
            log,
            &[("AD4M_ADMIN_CREDENTIAL", ADMIN), (name, CUSTODY)],
        );
        assert_eq!(
            code(executor.exit_within(EXIT)),
            Some(78),
            "{name}: {}",
            executor.output()
        );
        assert!(
            executor.output().contains(name) && executor.output().contains("stdin"),
            "{name}: {}",
            executor.output()
        );
    }
}

/// Every first line that is not a custody credential of its own is refused with 78: stdin closed
/// at start, stdin closed before a line, an empty line, a whitespace line, the admin credential.
#[test]
fn a_custody_channel_without_a_good_first_line_is_refused_with_78() {
    let _serial = serial();
    let folder = DataFolder::new();
    let admin_line = format!("{ADMIN}\n");
    let cases: [(&str, Option<&[u8]>, &str); 5] = [
        ("closed.log", None, "first line of stdin"),
        ("no-newline-eof.log", Some(b""), "first line of stdin"),
        ("empty-line.log", Some(b"\n"), "first line of stdin"),
        ("blank-line.log", Some(b"  \r\n"), "first line of stdin"),
        (
            "admin-line.log",
            Some(admin_line.as_bytes()),
            "is the admin credential",
        ),
    ];
    for (log, first, message) in cases {
        let port = free_port();
        let args = run_args(&folder, port, &["--custody-channel"]);
        let mut executor = spawn_with_stdin(
            &folder,
            port,
            args,
            first,
            log,
            &[("AD4M_ADMIN_CREDENTIAL", ADMIN)],
        );
        // The supervisor closes its end: the executor never waits for a line that won't come.
        drop(executor.child.stdin.take());
        assert_eq!(
            code(executor.exit_within(EXIT)),
            Some(78),
            "{log}: {}",
            executor.output()
        );
        assert!(
            executor.output().contains(message),
            "{log}: {}",
            executor.output()
        );
        assert!(
            !executor.output().contains(ADMIN),
            "{log}: {}",
            executor.output()
        );
    }
}

/// `--custody-channel` with `--watch-stdin`: the first line is the credential, then the watcher
/// reads on, and the executor stops with 0 when the supervisor's end closes.
#[tokio::test(flavor = "multi_thread")]
async fn the_custody_line_and_watch_stdin_share_stdin() {
    let _serial = SERIAL.lock().await;
    let folder = DataFolder::new();
    let port = free_port();
    let args = run_args(
        &folder,
        port,
        &["--custody-channel", "--watch-stdin", "true"],
    );
    let mut executor = spawn_with_stdin(
        &folder,
        port,
        args,
        Some(&custody_line()),
        "custody-watch.log",
        &[("AD4M_ADMIN_CREDENTIAL", ADMIN)],
    );
    executor.ready();
    let custody = WsRpcClient::connect(&executor.url(), CUSTODY)
        .await
        .unwrap();
    // No agent yet: the channel is open to the credential, so the refusal is not about it.
    let answer = custody
        .call::<Value>("identity.custodyStatus", json!({}))
        .await;
    if let Err(e) = &answer {
        assert!(!e.to_string().contains("no-custody-channel"), "{e}");
        assert!(!e.to_string().contains("Admin credential required"), "{e}");
    }
    drop(custody);
    // Data after the line is no stop.
    executor
        .child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"more\n")
        .unwrap();
    std::thread::sleep(Duration::from_secs(1));
    assert!(executor.running(), "{}", executor.output());
    drop(executor.child.stdin.take());
    let status = executor.exit_within(EXIT);
    assert_eq!(code(status), Some(0), "{}", executor.output());
    assert!(
        executor.output().contains("stdin closed"),
        "{}",
        executor.output()
    );
}

/// An address of this machine that is not loopback, if it has one: the source address of a
/// route out (a UDP connect sends nothing).
fn non_loopback_address() -> Option<IpAddr> {
    let socket = UdpSocket::bind("0.0.0.0:0").ok()?;
    socket.connect("192.0.2.1:9").ok()?;
    let ip = socket.local_addr().ok()?.ip();
    (!ip.is_loopback() && !ip.is_unspecified()).then_some(ip)
}

/// With `--localhost false` (no TLS) the API listener is 0.0.0.0, a network listener: a
/// `.local_only()` method answers the admin credential 403 `local-only`, whatever address the
/// caller connects through, 127.0.0.1 included, since the reach is the listener's. The methods
/// that are not local only (`agent.generate`) still answer there.
#[tokio::test(flavor = "multi_thread")]
async fn a_local_only_method_on_a_network_listener_answers_403_local_only() {
    let _serial = SERIAL.lock().await;
    let folder = DataFolder::new();
    let port = free_port();
    let args = run_args(&folder, port, &["--localhost", "false"]);
    let mut executor = spawn(
        &folder,
        port,
        args,
        Stdio::null(),
        "network.log",
        &[("AD4M_ADMIN_CREDENTIAL", ADMIN)],
    );
    executor.ready();
    assert!(
        executor
            .output()
            .contains(&format!("API server starting on http://0.0.0.0:{port}")),
        "{}",
        executor.output()
    );

    let mut urls = vec![executor.url()];
    if let Some(ip) = non_loopback_address() {
        urls.push(format!("http://{}", SocketAddr::new(ip, port)));
    }
    let admin = WsRpcClient::connect(&urls[0], ADMIN).await.unwrap();
    let status: Value = admin
        .call("agent.generate", json!({ "passphrase": "run-contract" }))
        .await
        .unwrap();
    assert_eq!(status["isInitialized"], json!(true), "{status}");
    for url in &urls {
        let admin = WsRpcClient::connect(url, ADMIN).await.unwrap();
        for method in ["runtime.quit", "agent.unlock", "identity.rekey"] {
            let params = if method == "agent.unlock" {
                json!({ "passphrase": "run-contract", "holochain": false })
            } else {
                json!({})
            };
            let refused = refusal(admin.call::<Value>(method, params).await);
            assert!(
                refused.starts_with("RPC error 403: local-only"),
                "{method} through {url}: {refused}"
            );
        }
    }
    assert!(executor.running(), "{}", executor.output());
}

/// `ad4m-executor <args>` with no inherited `AD4M_*` variable plus `env`: its exit code, stdout
/// and stderr.
fn cli(args: &[&str], env: &[(&str, &str)]) -> (Option<i32>, String, String) {
    let mut command = Command::new(BINARY);
    command.args(args).stdin(Stdio::null());
    for (name, _) in std::env::vars().filter(|(name, _)| name.starts_with("AD4M_")) {
        command.env_remove(name);
    }
    for (name, value) in env {
        command.env(name, value);
    }
    let out = command.output().expect("ad4m-executor runs");
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[cfg(unix)]
fn mode(path: &std::path::Path) -> u32 {
    use std::os::unix::fs::PermissionsExt;
    std::fs::metadata(path).unwrap().permissions().mode() & 0o777
}

/// Without a storage secret the executor comes up sealed: `/health`, the storage status and
/// the unlock answer, every other method is `storage-sealed`, and no store is opened. The
/// `storage` subcommands read the status and refuse a secret given on the command line or in
/// the environment without repeating it. `storage.unlock` opens it under the same keyring id;
/// the next start with the secret file opens at once, under that id again. The folder is 0700,
/// the database and the keyring 0600.
#[cfg(unix)]
#[tokio::test(flavor = "multi_thread")]
async fn without_a_secret_the_executor_is_sealed_until_the_operator_unlocks_it() {
    let _serial = SERIAL.lock().await;
    let folder = DataFolder::new();
    let database = folder.data().join("ad4m_db.sqlite");
    let keyring = rust_executor::at_rest::keyring_file::path_in(&folder.data());
    let secret = "5a".repeat(32);

    let port = free_port();
    let args = run_args(&folder, port, &[]);
    let mut sealed = spawn(
        &folder,
        port,
        args,
        Stdio::null(),
        "sealed.log",
        &[("AD4M_ADMIN_CREDENTIAL", ADMIN), (SECRET_FILE_VAR, "")],
    );
    sealed.ready();
    let url = sealed.url();
    let admin = WsRpcClient::connect(&url, ADMIN).await.unwrap();
    let status: Value = admin
        .call("runtime.storageStatus", json!({}))
        .await
        .unwrap();
    assert_eq!(status["state"], "sealed", "{status}");
    assert_eq!(status["stores"], Value::Null, "{status}");
    let keyring_id = status["keyringId"].as_str().unwrap().to_string();
    let refused = refusal(admin.call::<Value>("agent.status", json!({})).await);
    assert!(
        refused.starts_with("RPC error 503: storage-sealed"),
        "{refused}"
    );
    assert!(!database.exists(), "no store opens while sealed");
    assert!(!keyring.exists(), "no keyring is written before the secret");

    let (code, out, err) = cli(&["-e", &url, "-a", ADMIN, "storage", "status"], &[]);
    assert_eq!(code, Some(0), "{out}{err}");
    assert!(
        out.contains("Storage sealed") && out.contains(&keyring_id),
        "{out}"
    );
    let (code, out, err) = cli(
        &["-e", &url, "-a", ADMIN, "storage", "unlock", &secret],
        &[],
    );
    assert_eq!(code, Some(1), "{out}{err}");
    assert!(err.contains("takes no argument"), "{err}");
    assert!(!out.contains(&secret) && !err.contains(&secret));
    let (code, out, err) = cli(
        &["-e", &url, "-a", ADMIN, "storage", "unlock"],
        &[("AD4M_STORAGE_SECRET", &secret)],
    );
    assert_eq!(code, Some(1), "{out}{err}");
    assert!(err.contains("AD4M_STORAGE_SECRET is set"), "{err}");
    assert!(!out.contains(&secret) && !err.contains(&secret));

    let unlocked: Value = admin
        .call("storage.unlock", json!({ "secret": secret }))
        .await
        .unwrap();
    assert_eq!(unlocked["created"], true, "{unlocked}");
    assert_eq!(unlocked["status"]["keyringId"], keyring_id.as_str());
    let started = Instant::now();
    loop {
        let status: Value = admin
            .call("runtime.storageStatus", json!({}))
            .await
            .unwrap();
        if status["state"] == "open" {
            assert_eq!(status["keyringId"], keyring_id.as_str());
            assert!(status["stores"].is_object(), "{status}");
            break;
        }
        assert!(started.elapsed() < READY, "not open: {}", sealed.output());
        tokio::time::sleep(Duration::from_millis(200)).await;
    }
    let _: Value = admin.call("agent.status", json!({})).await.unwrap();
    assert_eq!(mode(&folder.data()), 0o700);
    assert_eq!(mode(&database), 0o600);
    assert_eq!(mode(&keyring), 0o600);
    drop(admin);
    drop(sealed);

    // The secret file opens it at the next start, under the same keyring.
    let port = free_port();
    let args = run_args(&folder, port, &[]);
    let mut opened = spawn(
        &folder,
        port,
        args,
        Stdio::null(),
        "opened.log",
        &[("AD4M_ADMIN_CREDENTIAL", ADMIN)],
    );
    opened.ready();
    let admin = WsRpcClient::connect(&opened.url(), ADMIN).await.unwrap();
    let status: Value = admin
        .call("runtime.storageStatus", json!({}))
        .await
        .unwrap();
    assert_eq!(status["state"], "open", "{status}");
    assert_eq!(status["keyringId"], keyring_id.as_str());
    assert_eq!(status["secretSource"], "file", "{status}");
    let (code, out, err) = cli(
        &["-e", &opened.url(), "-a", ADMIN, "storage", "status"],
        &[],
    );
    assert_eq!(code, Some(0), "{out}{err}");
    assert!(out.contains("Storage open"), "{out}");
}

/// The keyring file as JSON.
fn keyring_json(folder: &DataFolder) -> Value {
    let path = rust_executor::at_rest::keyring_file::path_in(&folder.data());
    serde_json::from_slice(&std::fs::read(path).unwrap()).unwrap()
}

/// `storage.keys`'s entry for `store`.
async fn store_summary(admin: &WsRpcClient, store: &str) -> Value {
    let keys: Value = admin.call("storage.keys", json!({})).await.unwrap();
    keys["stores"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["id"] == store)
        .unwrap_or_else(|| panic!("{store} in {keys}"))
        .clone()
}

/// The rotations end to end. An agent made after the keyring (its passphrase derived from the
/// root key) and a perspective; `storage.rotateSecret` moves the keyring to a new secret and
/// `storage.reencrypt` moves the perspective's data to a new key version. A restart with the
/// new secret opens everything: the agent unlocks with no passphrase given (the keyring now
/// holds the one derived from the old root), the perspective reads, and its store carries only
/// the new version. A restart with the old secret stays sealed (`wrong-secret`).
/// `storage rotate-secret` takes no secret as an argument or in the environment.
#[cfg(unix)]
#[tokio::test(flavor = "multi_thread")]
async fn a_rotated_secret_and_a_reencrypted_store_open_everything_after_a_restart() {
    let _serial = SERIAL.lock().await;
    let folder = DataFolder::new();
    let old = "5a".repeat(32);
    let new = "6b".repeat(32);

    let port = free_port();
    let mut first = spawn(
        &folder,
        port,
        run_args(&folder, port, &[]),
        Stdio::null(),
        "rotate.log",
        &[("AD4M_ADMIN_CREDENTIAL", ADMIN)],
    );
    first.ready();
    let url = first.url();
    let admin = WsRpcClient::connect(&url, ADMIN).await.unwrap();
    let _: Value = admin
        .call("agent.generate", json!({ "passphrase": "run-contract" }))
        .await
        .unwrap();
    assert_eq!(
        keyring_json(&folder)["agent_passphrase"]["kind"],
        "derived",
        "an agent made after the keyring has a derived passphrase"
    );
    let handle: Value = admin
        .call(
            "perspective.create",
            json!({ "name": "rotated perspective" }),
        )
        .await
        .unwrap();
    let uuid = handle["uuid"].as_str().unwrap().to_string();
    let store = format!("perspective:{uuid}");
    assert_eq!(store_summary(&admin, &store).await["current"], 1);

    let (code, out, err) = cli(
        &["-e", &url, "-a", ADMIN, "storage", "rotate-secret", &new],
        &[],
    );
    assert_eq!(code, Some(1), "{out}{err}");
    assert!(
        err.contains("storage rotate-secret takes no argument"),
        "{err}"
    );
    assert!(!out.contains(&new) && !err.contains(&new));
    let (code, out, err) = cli(
        &["-e", &url, "-a", ADMIN, "storage", "rotate-secret"],
        &[("AD4M_STORAGE_SECRET", &new)],
    );
    assert_eq!(code, Some(1), "{out}{err}");
    assert!(err.contains("AD4M_STORAGE_SECRET is set"), "{err}");
    assert!(!out.contains(&new) && !err.contains(&new));

    let rotated: Value = admin
        .call(
            "storage.rotateSecret",
            json!({ "currentSecret": old, "newSecret": new }),
        )
        .await
        .unwrap();
    assert_eq!(rotated["generation"], 2, "{rotated}");
    let keyring = keyring_json(&folder);
    assert_eq!(keyring["root_generation"], 2);
    assert_eq!(keyring["agent_passphrase"]["kind"], "held", "{keyring}");
    assert_eq!(keyring["agent_passphrase"]["generation"], 2);

    let started: Value = admin
        .call("storage.reencrypt", json!({ "store": store }))
        .await
        .unwrap();
    assert_eq!(started["version"], 2, "{started}");
    assert_eq!(started["resumed"], false, "{started}");
    let since = Instant::now();
    loop {
        let summary = store_summary(&admin, &store).await;
        if summary["retiring"].as_array().unwrap().is_empty() {
            assert_eq!(summary["current"], 2, "{summary}");
            break;
        }
        assert!(
            since.elapsed() < Duration::from_secs(60),
            "the re-encryption did not end: {summary}\n{}",
            first.output()
        );
        tokio::time::sleep(Duration::from_millis(200)).await;
    }
    drop(admin);
    drop(first);

    // The new secret opens everything; the agent unlocks from the keyring alone.
    let new_file = folder.file("new-storage-secret");
    write_private(&new_file, &format!("{new}\n"));
    let port = free_port();
    let mut second = spawn(
        &folder,
        port,
        run_args(&folder, port, &[]),
        Stdio::null(),
        "rotated.log",
        &[
            ("AD4M_ADMIN_CREDENTIAL", ADMIN),
            (SECRET_FILE_VAR, new_file.to_str().unwrap()),
        ],
    );
    second.ready();
    let admin = WsRpcClient::connect(&second.url(), ADMIN).await.unwrap();
    let status: Value = admin
        .call("runtime.storageStatus", json!({}))
        .await
        .unwrap();
    assert_eq!(status["state"], "open", "{status}");
    assert_eq!(status["rootGeneration"], 2, "{status}");
    let since = Instant::now();
    loop {
        let agent: Value = admin.call("agent.status", json!({})).await.unwrap();
        if agent["isUnlocked"] == true {
            assert_eq!(agent["isInitialized"], true, "{agent}");
            break;
        }
        assert!(
            since.elapsed() < Duration::from_secs(60),
            "the agent did not unlock from the keyring: {agent}\n{}",
            second.output()
        );
        tokio::time::sleep(Duration::from_millis(200)).await;
    }
    let read: Value = admin
        .call("perspective.get", json!({ "uuid": uuid }))
        .await
        .unwrap();
    assert_eq!(read["name"], "rotated perspective", "{read}");
    let summary = store_summary(&admin, &store).await;
    assert_eq!(summary["current"], 2, "{summary}");
    let versions: Vec<String> = keyring_json(&folder)["stores"][&store]["key"]["versions"]
        .as_object()
        .unwrap()
        .keys()
        .cloned()
        .collect();
    assert_eq!(versions, vec!["2".to_string()], "only the new version");
    drop(admin);
    drop(second);

    // The old secret no longer opens the keyring.
    let port = free_port();
    let mut third = spawn(
        &folder,
        port,
        run_args(&folder, port, &[]),
        Stdio::null(),
        "old-secret.log",
        &[("AD4M_ADMIN_CREDENTIAL", ADMIN)],
    );
    third.ready();
    let admin = WsRpcClient::connect(&third.url(), ADMIN).await.unwrap();
    let status: Value = admin
        .call("runtime.storageStatus", json!({}))
        .await
        .unwrap();
    assert_eq!(status["state"], "sealed", "{status}");
    assert_eq!(status["lastError"], "wrong-secret", "{status}");
    assert!(third.running());
}

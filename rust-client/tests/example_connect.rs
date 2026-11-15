//! The terminal example (`examples/connect.rs`) against a real executor, in
//! a pseudo-terminal: a typed secret (the approval code here; passwords and
//! email codes take the same path) must not echo, and the terminal must
//! have echo back on after the example ends on an error (F6) or on ^C (N4).
//!
//! The pty is driven by Python's `pty` module (`python3 -I`), which lets the
//! test type only once the read has switched the tty's echo off: typed
//! earlier, the tty would echo it. Same harness as
//! `integration.rs`: skips without a binary unless `AD4M_REQUIRE_EXECUTOR=1`.

mod common;

use std::path::PathBuf;
use std::process::Command;

/// Types a wrong code at the first `code> ` prompt, waits for the rejection
/// and the second prompt, then ends the second read: `eof` types ^D (the
/// example cancels), `intr` types ^C (an interrupt). Prints the tty's echo
/// flag after the example ended, its exit status, and what the terminal
/// showed after the first prompt.
///
/// Every wait is on a condition, bounded: a prompt is searched for only in
/// output after the previous match (the first `code> ` cannot satisfy the
/// wait for the second), and a key is typed only once the tty's echo is off
/// (the prompt is written before the read disables echo).
const PTY_DRIVER: &str = r#"
import os, pty, select, signal, sys, termios, time
mode = sys.argv[1]
pid, fd = pty.fork()
if pid == 0:
    os.execv(sys.argv[2], sys.argv[2:])
buf = b""
def fail(msg):
    try:
        os.kill(pid, signal.SIGKILL)
    except OSError:
        pass
    sys.exit("%s; screen: %r" % (msg, buf))
def read_more(end):
    global buf
    left = end - time.time()
    r = select.select([fd], [], [], left)[0] if left > 0 else []
    if not r:
        return None
    try:
        chunk = os.read(fd, 4096)
    except OSError:
        chunk = b""
    buf += chunk
    return chunk
def read_until(marker, start, secs=60):
    end = time.time() + secs
    while True:
        i = buf.find(marker, start)
        if i >= 0:
            return i + len(marker)
        chunk = read_more(end)
        if chunk is None:
            fail("timed out waiting for %r" % marker)
        if not chunk:
            fail("the example ended waiting for %r" % marker)
def echo_on():
    return bool(termios.tcgetattr(fd)[3] & termios.ECHO)
def wait_echo_off(secs=30):
    end = time.time() + secs
    while echo_on():
        if time.time() > end:
            fail("the hidden read never switched echo off")
        time.sleep(0.01)
p = read_until(b"code> ", 0)
wait_echo_off()
mark = p
os.write(fd, b"987654\n")
p = read_until(b"did not take", p)
p = read_until(b"code> ", p, 30)
wait_echo_off()
shown = buf[mark:p]
os.write(fd, b"\x04" if mode == "eof" else b"\x03")
end = time.time() + 30
while True:
    chunk = read_more(end)
    if chunk is None:
        fail("the example did not end after the %s" % mode)
    if not chunk:
        break
echo = echo_on()
status = None
while status is None:
    done, st = os.waitpid(pid, os.WNOHANG)
    if done:
        status = os.waitstatus_to_exitcode(st)
    elif time.time() > end:
        fail("the example did not exit after the %s" % mode)
    else:
        time.sleep(0.01)
sys.stdout.write("ECHO=%d\nSTATUS=%d\n" % (echo, status))
sys.stdout.write(shown.decode(errors="replace"))
"#;

/// Runs the pty driver in `mode` against `url` under a throwaway home;
/// returns its report.
fn run_driver(mode: &str, url: &str) -> String {
    let home = tempfile::tempdir().unwrap();
    let out = Command::new("python3")
        .args(["-I", "-c", PTY_DRIVER, mode])
        .arg(example_binary())
        .args(["--url", url])
        // Token store and port file under a throwaway home.
        .env("HOME", home.path())
        .env("XDG_CONFIG_HOME", home.path().join(".config"))
        .output()
        .expect("python3 (the pty driver)");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "pty driver ({mode}) failed: {}\n{stdout}",
        String::from_utf8_lossy(&out.stderr)
    );
    stdout
}

fn example_binary() -> PathBuf {
    // target/<profile>/deps/<this test> → target/<profile>/examples/connect
    let exe = std::env::current_exe().unwrap();
    let path = exe
        .parent()
        .and_then(|deps| deps.parent())
        .unwrap()
        .join("examples")
        .join("connect");
    assert!(
        path.exists(),
        "the connect example is not built at {} (cargo test builds it)",
        path.display()
    );
    path
}

#[tokio::test]
async fn the_example_reads_secrets_without_echo() {
    let Some(executor) = common::start_executor_with_admin("a5-echo-admin") else {
        return;
    };
    let credential = executor.admin_credential.clone().unwrap();
    let admin = common::connect(&executor.url, &credential).await;
    admin
        .agent
        .generate("passphrase".to_string())
        .await
        .unwrap();

    // ^D at the hidden read: the example cancels and ends with an error.
    let stdout = run_driver("eof", &executor.url);
    assert!(
        !stdout.contains("987654"),
        "the typed code was echoed:\n{stdout}"
    );
    assert!(
        stdout.starts_with("ECHO=1\n"),
        "the terminal was left without echo after ^D:\n{stdout}"
    );
    // ^C at the hidden read (N4): the terminal gets its echo back and the
    // example exits like an interrupted program (130).
    let stdout = run_driver("intr", &executor.url);
    assert!(
        stdout.starts_with("ECHO=1\nSTATUS=130\n"),
        "after ^C the terminal was left without echo, or the exit was not 130:\n{stdout}"
    );
    admin.ws().close().await;
}

//! The terminal example (`examples/connect.rs`) against a real executor, in
//! a pseudo-terminal: a typed secret (the approval code here; passwords and
//! email codes take the same path) must not echo, and the terminal must
//! have echo back on after the example ends on an error (F6).
//!
//! The pty is driven by Python's `pty` module (`python3 -I`), which lets the
//! test type only after the prompt is up: typed earlier, the tty would echo
//! it before the example could switch echo off. Same harness as
//! `integration.rs`: skips without a binary unless `AD4M_REQUIRE_EXECUTOR=1`.

mod common;

use std::path::PathBuf;
use std::process::Command;

/// Types a wrong code at the first `code> ` prompt, waits for the rejection
/// and the second prompt, ends input (^D: the example cancels), then prints
/// what the terminal showed after the first prompt and the tty's echo flag.
const PTY_DRIVER: &str = r#"
import os, pty, select, sys, termios, time
pid, fd = pty.fork()
if pid == 0:
    os.execv(sys.argv[1], sys.argv[1:])
buf = b""
def read_until(marker, secs=60):
    global buf
    end = time.time() + secs
    while marker not in buf:
        left = end - time.time()
        r = select.select([fd], [], [], max(0, left))[0] if left > 0 else []
        if not r:
            sys.exit("timed out waiting for %r; screen: %r" % (marker, buf))
        try:
            chunk = os.read(fd, 4096)
        except OSError:
            chunk = b""
        if not chunk:
            sys.exit("the example ended waiting for %r; screen: %r" % (marker, buf))
        buf += chunk
read_until(b"code> ")
mark = len(buf)
os.write(fd, b"987654\n")
read_until(b"did not take")
read_until(b"code> ", 30)
shown = buf[mark:]
os.write(fd, b"\x04")
try:
    while os.read(fd, 4096):
        pass
except OSError:
    pass
echo = bool(termios.tcgetattr(fd)[3] & termios.ECHO)
os.waitpid(pid, 0)
sys.stdout.write("ECHO=%d\n" % echo)
sys.stdout.write(shown.decode(errors="replace"))
"#;

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

    let home = tempfile::tempdir().unwrap();
    let out = Command::new("python3")
        .args(["-I", "-c", PTY_DRIVER])
        .arg(example_binary())
        .args(["--url", &executor.url])
        // Token store and port file under a throwaway home.
        .env("HOME", home.path())
        .env("XDG_CONFIG_HOME", home.path().join(".config"))
        .output()
        .expect("python3 (the pty driver)");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "pty driver failed: {}\n{stdout}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !stdout.contains("987654"),
        "the typed code was echoed:\n{stdout}"
    );
    assert!(
        stdout.starts_with("ECHO=1"),
        "the terminal was left without echo:\n{stdout}"
    );
    admin.ws().close().await;
}

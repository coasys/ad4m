//! The test vouching service (`tests/common/vouch.rs`) as a process, for the executor's
//! identity suite (`tests/js/tests/identity.test.ts`): a store started with its `VOUCH_URL`,
//! `VOUCH_CREDENTIAL` and `VOUCH_KEYS` redeems the tokens given here.
//!
//! ```text
//! cargo run -q -p identity-store --example vouch_service
//! ```
//!
//! It listens on 127.0.0.1 (a free port) and writes one line when it is ready:
//! `vouch-service listening on <url> key <multikey> credential <credential>`. Its notes carry
//! the time it started, less 5 s. Then each line on stdin is one request, `{"op": …, …}`, and
//! each answer one line, `{"ok": value}` or `{"error": text}`:
//!
//! | op | fields | answer |
//! |---|---|---|
//! | `give` | `token`, `email` | `true`: a redeem of `token` now vouches its first DID |
//! | `spend` | `token` | `true`: a redeem of `token` now answers `410` |
//! | `redeems` | | how many redeems reached the service |
//!
//! It stops when stdin closes.

#[path = "../tests/common/vouch.rs"]
mod vouch;

use std::io::{BufRead, Write};
use std::time::{SystemTime, UNIX_EPOCH};

use serde_json::{json, Value};
use vouch::{vouching_multikey, VouchServer, CREDENTIAL};

fn text<'a>(req: &'a Value, name: &str) -> Result<&'a str, String> {
    req.get(name)
        .and_then(Value::as_str)
        .ok_or_else(|| format!("{name} is a string"))
}

fn handle(server: &VouchServer, req: &Value) -> Result<Value, String> {
    match text(req, "op")? {
        "give" => {
            server.give(text(req, "token")?, text(req, "email")?);
            Ok(json!(true))
        }
        "spend" => {
            server.spend(text(req, "token")?);
            Ok(json!(true))
        }
        "redeems" => Ok(json!(server.redeems())),
        other => Err(format!("no op {other}")),
    }
}

fn main() {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("the runtime starts");
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let server = runtime.block_on(VouchServer::start(now.saturating_sub(5)));
    let mut stdout = std::io::stdout().lock();
    writeln!(
        stdout,
        "vouch-service listening on {} key {} credential {CREDENTIAL}",
        server.url(),
        vouching_multikey()
    )
    .and_then(|()| stdout.flush())
    .expect("stdout is open");
    for line in std::io::stdin().lock().lines() {
        let Ok(line) = line else { break };
        if line.trim().is_empty() {
            continue;
        }
        let answer = match serde_json::from_str::<Value>(&line) {
            Ok(req) => match handle(&server, &req) {
                Ok(value) => json!({ "ok": value }),
                Err(error) => json!({ "error": error }),
            },
            Err(e) => json!({ "error": format!("not JSON: {e}") }),
        };
        if writeln!(stdout, "{answer}")
            .and_then(|()| stdout.flush())
            .is_err()
        {
            break;
        }
    }
}

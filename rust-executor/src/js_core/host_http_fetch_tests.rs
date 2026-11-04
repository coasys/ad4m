//! `httpFetch` from `ad4m:host` (host.js), run inside a real language isolate
//! against local TCP servers that stall at different points (#1041).

use super::JsCore;
use std::io::{Read, Write};
use std::net::TcpListener;
use std::sync::mpsc;
use std::time::{Duration, Instant};

/// Longer than host.js's `HTTP_FETCH_TIMEOUT_MS` (10 s) by a margin; without a
/// timeout in host.js a stalled server holds the call open indefinitely, so
/// the test fails here instead of hanging.
const SETTLE_BOUND: Duration = Duration::from_secs(20);

/// What the local server sends after reading the request, before it stops
/// writing and holds the connection open until the test ends.
enum Reply {
    Nothing,
    Partial(&'static str),
    Full(&'static str),
}

/// Serve one connection on 127.0.0.1. The returned sender keeps the socket
/// open; dropping it lets the server thread exit.
fn serve_once(reply: Reply) -> (String, mpsc::Sender<()>) {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
    let url = format!("http://{}/probe", listener.local_addr().unwrap());
    let (hold_tx, hold_rx) = mpsc::channel::<()>();
    std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept");
        let mut buf = [0u8; 4096];
        let _ = stream.read(&mut buf);
        let bytes = match reply {
            Reply::Nothing => "",
            Reply::Partial(s) | Reply::Full(s) => s,
        };
        let _ = stream.write_all(bytes.as_bytes());
        let _ = stream.flush();
        if !matches!(reply, Reply::Full(_)) {
            let _ = hold_rx.recv();
        }
    });
    (url, hold_tx)
}

async fn language_isolate() -> (JsCore, tempfile::TempDir) {
    // Deno's fetch client needs a process-level provider; `lib.rs` installs
    // it at executor start, which unit tests skip.
    let _ = rustls::crypto::aws_lc_rs::default_provider().install_default();
    let dir = tempfile::tempdir().expect("tempdir");
    let js = JsCore::new_for_language(dir.path().to_path_buf(), false);
    js.init_for_language().await.expect("bootstrap");
    js.load_module_from_source(
        "https://ad4m.language/http-fetch-test/bundle.js",
        r#"import { httpFetch } from "ad4m:host"; globalThis.__httpFetch = httpFetch;"#.to_string(),
    )
    .await
    .expect("load test bundle");
    (js, dir)
}

/// Run `httpFetch(url, "GET")` and return `ok:<json>` or `err:<message>`,
/// with the wall time it took. Fails the test if the call has not settled
/// within [`SETTLE_BOUND`].
async fn fetch(js: &JsCore, url: &str) -> (String, Duration) {
    let url_lit = serde_json::to_string(url).unwrap();
    let script = format!(
        r#"globalThis.__httpFetch({url_lit}, "GET", "", "").then(
            r => "ok:" + JSON.stringify(r),
            e => "err:" + (e && e.message))"#
    );
    let started = Instant::now();
    let out = tokio::time::timeout(SETTLE_BOUND, js.execute(&script)).await;
    let elapsed = started.elapsed();
    let out = out.unwrap_or_else(|_| {
        panic!("httpFetch({url}) did not settle within {SETTLE_BOUND:?}: no timeout in host.js")
    });
    (out.expect("script ran"), elapsed)
}

fn assert_timed_out(result: &str, url: &str) {
    assert!(
        result.starts_with("err:") && result.contains("timed out") && result.contains(url),
        "expected a timeout error naming {url}, got: {result}"
    );
}

#[tokio::test]
async fn http_fetch_times_out_when_the_server_never_sends_headers() {
    let (js, _dir) = language_isolate().await;
    let (url, _hold) = serve_once(Reply::Nothing);
    let (result, elapsed) = fetch(&js, &url).await;
    assert_timed_out(&result, &url);
    assert!(elapsed < SETTLE_BOUND, "took {elapsed:?}");
}

/// The timeout error reaches the executor log and clients, so it names the
/// host and path but never the URL's credentials or query string.
#[tokio::test]
async fn http_fetch_timeout_error_redacts_credentials_and_query() {
    let (js, _dir) = language_isolate().await;
    let (url, _hold) = serve_once(Reply::Nothing);
    let secret_url = format!(
        "{}?token=SECRET",
        url.replacen("http://", "http://user:hunter2@", 1)
    );
    let (result, _) = fetch(&js, &secret_url).await;
    assert_timed_out(&result, &url);
    assert!(
        !result.contains("hunter2") && !result.contains("SECRET"),
        "timeout error leaks URL credentials or query: {result}"
    );
}

/// A fetch that fails for another reason names the redacted URL. The
/// runtime's own "fetch failed" does not say which call failed.
#[tokio::test]
async fn http_fetch_other_errors_name_the_redacted_url() {
    let (js, _dir) = language_isolate().await;
    let url = {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        format!("http://{}/probe", listener.local_addr().unwrap())
    };
    let secret_url = format!(
        "{}?token=SECRET",
        url.replacen("http://", "http://user:hunter2@", 1)
    );
    let (result, _) = fetch(&js, &secret_url).await;
    assert!(
        result.starts_with("err:") && !result.contains("timed out") && result.contains(&url),
        "expected a non-timeout error naming {url}, got: {result}"
    );
    assert!(
        !result.contains("hunter2") && !result.contains("SECRET"),
        "fetch error leaks URL credentials or query: {result}"
    );
}

/// The runtime's error for a URL it cannot parse quotes the raw URL,
/// credentials and query included; it must not reach the caller as is.
#[tokio::test]
async fn http_fetch_invalid_url_error_does_not_leak_credentials_or_query() {
    let (js, _dir) = language_isolate().await;
    let (result, _) = fetch(
        &js,
        "http://user:hunter2@127.0.0.1:99999/probe?token=SECRET",
    )
    .await;
    assert!(
        result.starts_with("err:"),
        "expected an error, got: {result}"
    );
    assert!(
        !result.contains("hunter2") && !result.contains("SECRET"),
        "fetch error leaks URL credentials or query: {result}"
    );
}

#[tokio::test]
async fn http_fetch_times_out_when_the_body_stalls() {
    let (js, _dir) = language_isolate().await;
    let (url, _hold) = serve_once(Reply::Partial(
        "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\n\r\npartial",
    ));
    let (result, _) = fetch(&js, &url).await;
    assert_timed_out(&result, &url);
}

/// host.js's `HTTP_FETCH_MAX_BYTES`.
const HOST_MAX_BYTES: usize = 32 * 1024 * 1024;

/// Run `httpFetch(url, "GET", "", "", maxBytes)` and return `ok:<status>:<body length>` or
/// `err:<code>:<message>`.
async fn fetch_capped(js: &JsCore, url: &str, max_bytes: Option<usize>) -> String {
    let url_lit = serde_json::to_string(url).unwrap();
    let max = max_bytes.map_or("undefined".to_string(), |m| m.to_string());
    let script = format!(
        r#"globalThis.__httpFetch({url_lit}, "GET", "", "", {max}).then(
            r => "ok:" + r.status + ":" + r.body.length,
            e => "err:" + (e && e.code) + ":" + (e && e.message))"#
    );
    tokio::time::timeout(SETTLE_BOUND, js.execute(&script))
        .await
        .unwrap_or_else(|_| panic!("httpFetch({url}) did not settle within {SETTLE_BOUND:?}"))
        .expect("script ran")
}

/// Serve one connection with a chunked 200 whose body would be `total` bytes of `x`, in
/// 64 KiB chunks, counting the body bytes written until the client goes away.
fn serve_chunked(total: usize) -> (String, std::sync::Arc<std::sync::atomic::AtomicUsize>) {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
    let url = format!("http://{}/big", listener.local_addr().unwrap());
    let sent = std::sync::Arc::new(AtomicUsize::new(0));
    let counter = sent.clone();
    std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept");
        let mut buf = [0u8; 4096];
        let _ = stream.read(&mut buf);
        if stream
            .write_all(b"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")
            .is_err()
        {
            return;
        }
        let chunk = vec![b'x'; 64 * 1024];
        let mut left = total;
        while left > 0 {
            let n = left.min(chunk.len());
            let framed = [
                format!("{n:x}\r\n").as_bytes(),
                &chunk[..n],
                b"\r\n".as_slice(),
            ]
            .concat();
            if stream.write_all(&framed).is_err() {
                return;
            }
            counter.fetch_add(n, Ordering::SeqCst);
            left -= n;
        }
        let _ = stream.write_all(b"0\r\n\r\n");
        let _ = stream.flush();
    });
    (url, sent)
}

/// A body past the caller's `maxBytes` is refused (`too-large`) while it streams: the server
/// meant to send 256 MiB and could write only a little past the limit before the client went
/// away.
#[tokio::test]
async fn http_fetch_refuses_a_reply_over_max_bytes_before_reading_it_all() {
    let (js, _dir) = language_isolate().await;
    let limit = 1024 * 1024;
    let total = 256 * 1024 * 1024;
    let (url, sent) = serve_chunked(total);
    let result = fetch_capped(&js, &url, Some(limit)).await;
    assert!(
        result.starts_with("err:too-large:") && result.contains(&url),
        "expected a too-large refusal naming {url}, got: {result}"
    );
    tokio::time::sleep(Duration::from_millis(500)).await;
    let sent = sent.load(std::sync::atomic::Ordering::SeqCst);
    assert!(
        sent < 32 * 1024 * 1024,
        "the server wrote {sent} of {total} bytes: the body was read past the limit"
    );
}

/// Without `maxBytes` the host-wide limit applies, and a larger `maxBytes` does not raise it.
#[tokio::test]
async fn http_fetch_refuses_a_reply_over_the_host_limit() {
    let (js, _dir) = language_isolate().await;
    for max in [None, Some(HOST_MAX_BYTES * 4)] {
        let (url, _) = serve_chunked(HOST_MAX_BYTES + 1);
        let result = fetch_capped(&js, &url, max).await;
        assert!(
            result.starts_with("err:too-large:") && result.contains(&HOST_MAX_BYTES.to_string()),
            "maxBytes {max:?}: expected a too-large refusal at the host limit, got: {result}"
        );
    }
}

/// A `Content-Length` over the limit is refused at once, without waiting for the body.
#[tokio::test]
async fn http_fetch_refuses_a_declared_length_over_the_limit_at_once() {
    let (js, _dir) = language_isolate().await;
    let (url, _hold) = serve_once(Reply::Partial(
        "HTTP/1.1 200 OK\r\nContent-Length: 2000000\r\n\r\nxxxx",
    ));
    let started = Instant::now();
    let result = fetch_capped(&js, &url, Some(1_000_000)).await;
    assert!(
        result.starts_with("err:too-large:"),
        "expected a too-large refusal, got: {result}"
    );
    assert!(
        started.elapsed() < Duration::from_secs(5),
        "waited for the body"
    );
}

/// A body of exactly the limit passes, streamed or with its length declared.
#[tokio::test]
async fn http_fetch_takes_a_reply_at_the_limit() {
    let (js, _dir) = language_isolate().await;
    let limit = 1024 * 1024 + 7;
    let (url, _) = serve_chunked(limit);
    assert_eq!(
        fetch_capped(&js, &url, Some(limit)).await,
        format!("ok:200:{limit}")
    );

    let body = "y".repeat(limit);
    let reply: &'static str = Box::leak(
        format!("HTTP/1.1 200 OK\r\nContent-Length: {limit}\r\nConnection: close\r\n\r\n{body}")
            .into_boxed_str(),
    );
    let (url, _hold) = serve_once(Reply::Full(reply));
    assert_eq!(
        fetch_capped(&js, &url, Some(limit)).await,
        format!("ok:200:{limit}")
    );
}

/// Serve one connection with a 200 whose body is `len` bytes of `x`, gzip-compressed, with
/// `Content-Encoding: gzip` and the compressed length as `Content-Length`.
fn serve_gzip(len: usize) -> String {
    use flate2::{write::GzEncoder, Compression};
    let mut encoder = GzEncoder::new(Vec::new(), Compression::best());
    encoder.write_all(&vec![b'x'; len]).unwrap();
    let body = encoder.finish().unwrap();
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
    let url = format!("http://{}/gzip", listener.local_addr().unwrap());
    std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept");
        let mut buf = [0u8; 4096];
        let _ = stream.read(&mut buf);
        let head = format!(
            "HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
            body.len()
        );
        let _ = stream.write_all(head.as_bytes());
        let _ = stream.write_all(&body);
        let _ = stream.flush();
    });
    url
}

/// `maxBytes` counts the decompressed body: Deno's fetch decompresses a gzip reply and its
/// body stream yields only the decompressed bytes, which `readCapped` counts. A reply of a few
/// kilobytes on the wire (its declared length far under the limit) that decompresses past the
/// limit is refused `too-large`; one that decompresses under it passes at its decompressed
/// length.
#[tokio::test]
async fn http_fetch_counts_the_decompressed_bytes_of_a_gzip_reply() {
    let (js, _dir) = language_isolate().await;
    let limit = 1024 * 1024;

    let url = serve_gzip(limit * 8);
    let result = fetch_capped(&js, &url, Some(limit)).await;
    assert!(
        result.starts_with("err:too-large:") && result.contains(&limit.to_string()),
        "expected a too-large refusal of the decompressed body, got: {result}"
    );

    let under = limit / 2;
    let url = serve_gzip(under);
    assert_eq!(
        fetch_capped(&js, &url, Some(limit)).await,
        format!("ok:200:{under}")
    );
}

#[tokio::test]
async fn http_fetch_returns_status_and_body_from_a_prompt_server() {
    let (js, _dir) = language_isolate().await;
    let (url, _hold) = serve_once(Reply::Full(
        "HTTP/1.1 404 Not Found\r\nContent-Length: 5\r\nConnection: close\r\n\r\nnope!",
    ));
    let (result, _) = fetch(&js, &url).await;
    assert_eq!(result, r#"ok:{"status":404,"body":"nope!"}"#);
}

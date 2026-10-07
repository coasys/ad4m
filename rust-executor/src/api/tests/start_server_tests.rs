//! `start_server` failure paths: which ones are fatal to the API.
//!
//! The launcher embeds the executor, so a bad TLS certificate must leave the
//! cleartext API serving.

use crate::api::start_server;
use crate::config::TlsConfig;
use crate::Ad4mConfig;
use std::time::Duration;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;

fn free_port() -> u16 {
    std::net::TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

/// GET /health on 127.0.0.1:`port`. `None` while nothing answers there.
async fn get_health(port: u16) -> Option<String> {
    let mut stream = TcpStream::connect(("127.0.0.1", port)).await.ok()?;
    stream
        .write_all(b"GET /health HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
        .await
        .ok()?;
    let mut response = String::new();
    stream.read_to_string(&mut response).await.ok()?;
    Some(response)
}

#[tokio::test]
async fn a_bad_tls_certificate_leaves_the_cleartext_api_serving() {
    let port = free_port();
    let tls_port = free_port();
    let config = Ad4mConfig {
        port: Some(port),
        tls: Some(TlsConfig {
            cert_file_path: "/nonexistent/ad4m-test-cert.pem".to_string(),
            key_file_path: "/nonexistent/ad4m-test-key.pem".to_string(),
            tls_port,
        }),
        ..Default::default()
    };
    let server = tokio::spawn(start_server(config));

    let mut health = None;
    for _ in 0..100 {
        if server.is_finished() {
            break;
        }
        health = get_health(port).await;
        if health.is_some() {
            break;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    // Checked before the response: if start_server returned, say what it
    // returned instead of only "no response".
    if server.is_finished() {
        panic!(
            "start_server returned instead of serving cleartext: {:?}",
            server.await.unwrap()
        );
    }
    let health = health.expect("no response on the cleartext port within 5 s");
    assert!(
        health.starts_with("HTTP/1.1 200") && health.contains(r#""status":"ok""#),
        "unexpected /health response: {health}"
    );
    // The HTTPS listener did not start.
    assert!(TcpStream::connect(("127.0.0.1", tls_port)).await.is_err());

    server.abort();
}

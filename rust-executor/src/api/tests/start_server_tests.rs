//! `start_server` failure paths: which ones are fatal to the API.
//!
//! The launcher embeds the executor, so a bad TLS certificate must leave the
//! cleartext API serving. A taken API port must come back as an error that
//! names the address, so executor binaries can exit with it.

use crate::api::{cleartext_ip, start_server};
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

#[tokio::test]
async fn a_taken_api_port_is_an_error_that_names_the_address() {
    let taken = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
    let port = taken.local_addr().unwrap().port();
    let config = Ad4mConfig {
        port: Some(port),
        ..Default::default()
    };

    let result = tokio::time::timeout(Duration::from_secs(10), start_server(config))
        .await
        .expect("start_server kept running on a taken port");
    let error = result.expect_err("start_server bound a port that was taken");
    assert!(
        error
            .to_string()
            .contains(&format!("could not bind the API to 127.0.0.1:{port}")),
        "unexpected error: {error}"
    );
}

#[test]
fn with_tls_configured_the_cleartext_api_stays_on_loopback() {
    let tls = Some(TlsConfig {
        cert_file_path: String::new(),
        key_file_path: String::new(),
        tls_port: 0,
    });
    // localhost: false asks for 0.0.0.0, but TLS wins: a broken TLS setup
    // must not put the API on the network in cleartext.
    let with_tls = Ad4mConfig {
        localhost: Some(false),
        tls,
        ..Default::default()
    };
    assert_eq!(cleartext_ip(&with_tls), [127, 0, 0, 1]);

    let without_tls = Ad4mConfig {
        localhost: Some(false),
        ..Default::default()
    };
    assert_eq!(cleartext_ip(&without_tls), [0, 0, 0, 0]);
    assert_eq!(cleartext_ip(&Ad4mConfig::default()), [127, 0, 0, 1]);
}

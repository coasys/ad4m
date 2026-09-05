//! TURN credential minting and lifecycle management (RFC 7635).
//!
//! Generates short-lived HMAC-SHA1 credentials for TURN servers
//! following the ephemeral credential pattern described in
//! <https://datatracker.ietf.org/doc/html/draft-uberti-behave-turn-rest-00>.
//!
//! A background task periodically refreshes credentials and pushes
//! updates to connected participants via SDP renegotiation offers.
//!
//! ## Credential format
//!
//! - **username**: `<expiry-unix-timestamp>:<user-label>`
//! - **credential**: `base64(HMAC-SHA1(<shared-secret>, <username>))`
//!
//! The TURN server validates by extracting the username, recomputing
//! the HMAC against its own copy of the shared secret, and rejecting
//! expired timestamps.

use hmac::{Hmac, Mac};
use sha1::Sha1;
use std::time::{SystemTime, UNIX_EPOCH};

use super::types::IceServer;

type HmacSha1 = Hmac<Sha1>;

/// Default credential lifetime: 12 hours.  Refresh happens well
/// before expiry; the long lifetime provides buffer for clients that
/// miss a refresh cycle.
pub const DEFAULT_TTL_SECS: u64 = 12 * 3600;

/// How often the background task checks / refreshes credentials.
/// Refresh triggers when remaining lifetime drops below this
/// fraction of the TTL.
pub const REFRESH_FRACTION: f64 = 0.5;

/// Mint a single short-lived TURN credential.
///
/// `shared_secret` — the secret shared between the SFU host and the
///   TURN server.
/// `user_label` — a caller-chosen label embedded in the username;
///   typically the agent's DID or a session identifier.
/// `turn_urls` — one or more TURN URLs (`turn:host:port`,
///   `turns:host:port?transport=tcp`).
/// `ttl_secs` — credential lifetime in seconds.
pub fn mint_turn_credential(
    shared_secret: &[u8],
    user_label: &str,
    turn_urls: &[String],
    ttl_secs: u64,
) -> IceServer {
    let expiry = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
        + ttl_secs;

    let username = format!("{}:{}", expiry, user_label);

    let mut mac = HmacSha1::new_from_slice(shared_secret).expect("HMAC accepts any key length");
    mac.update(username.as_bytes());
    let result = mac.finalize();
    let credential = base64_encode(result.into_bytes().as_slice());

    IceServer {
        urls: turn_urls.to_vec(),
        username: Some(username),
        credential: Some(credential),
    }
}

/// Check whether a minted credential has passed the refresh
/// threshold (remaining lifetime < `REFRESH_FRACTION` of total TTL).
pub fn needs_refresh(username: &str, ttl_secs: u64) -> bool {
    let expiry: u64 = username
        .split(':')
        .next()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    let remaining = expiry.saturating_sub(now);
    let threshold = (ttl_secs as f64 * REFRESH_FRACTION) as u64;
    remaining < threshold
}

/// Standard base64 encoding (no padding stripping — TURN servers
/// expect standard base64 per the draft spec).
fn base64_encode(data: &[u8]) -> String {
    use base64::Engine;
    base64::engine::general_purpose::STANDARD.encode(data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mint_credential() {
        let secret = b"test-shared-secret";
        let cred = mint_turn_credential(
            secret,
            "did:test:alice",
            &["turn:relay.example.com:3478".to_string()],
            3600,
        );

        assert_eq!(cred.urls, vec!["turn:relay.example.com:3478"]);
        let username = cred.username.as_ref().unwrap();
        assert!(username.contains("did:test:alice"));
        // Username format: <expiry>:<label>
        let parts: Vec<&str> = username.splitn(2, ':').collect();
        assert_eq!(parts.len(), 2);
        let expiry: u64 = parts[0].parse().unwrap();
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        assert!(expiry > now);
        assert!(expiry <= now + 3600 + 1);

        // Credential should be non-empty base64
        let credential = cred.credential.as_ref().unwrap();
        assert!(!credential.is_empty());
        // Verify HMAC by re-computing
        let mut mac = HmacSha1::new_from_slice(secret).unwrap();
        mac.update(username.as_bytes());
        let expected = base64_encode(mac.finalize().into_bytes().as_slice());
        assert_eq!(credential, &expected);
    }

    #[test]
    fn test_needs_refresh() {
        // Expiry far in the future — no refresh needed
        let future = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs()
            + 10_000;
        let username = format!("{}:test", future);
        assert!(!needs_refresh(&username, 3600));

        // Expiry in the past — definitely needs refresh
        let past = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs()
            - 100;
        let username = format!("{}:test", past);
        assert!(needs_refresh(&username, 3600));

        // Expiry close to now — within refresh fraction
        let soon = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs()
            + 100; // 100s remaining, threshold = 1800s
        let username = format!("{}:test", soon);
        assert!(needs_refresh(&username, 3600));
    }
}

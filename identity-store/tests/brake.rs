//! The brake on new logs without a vouching note (slice 3.C, C1): the global and per-address
//! buckets, the proof of work at its challenge's bits, the failure bucket, the order of
//! the checks before any signature replays, trusted proxies, `/v1/metrics` and the
//! configuration keys.

mod common;

use std::net::SocketAddr;

use ad4m_identity_core::pow;
use axum::http::StatusCode;
use identity_store::brake::{Cidr, ClientIpHeader};
use identity_store::Settings;
use serde_json::{json, Value};

use common::agent::Person;
use common::{Reply, TestStore, T0, TEST_POW_BITS};

fn from(addr: &str) -> SocketAddr {
    addr.parse().unwrap()
}

/// `POST` of the person's #0 from `peer` with `headers`; `pow` is the proof sent, if any.
async fn post_new(
    t: &TestStore,
    peer: SocketAddr,
    headers: &[(&str, &str)],
    person: &Person,
    pow: Option<Value>,
) -> Reply {
    let mut body = TestStore::append_body(&[&person.events[0]]);
    if let Some(pow) = pow {
        body["pow"] = pow;
    }
    t.send_from(
        peer,
        "POST",
        &format!("/v1/logs/{}/events", person.scid()),
        Some(serde_json::to_vec(&body).unwrap()),
        None,
        headers,
    )
    .await
}

/// The person's #0 from `peer` with a good proof.
async fn publish_from(t: &TestStore, peer: &str, person: &Person) -> Reply {
    post_new(t, from(peer), &[], person, Some(t.pow_for(&person.scid()))).await
}

/// The person's #0 with its first signature swapped for another event's: well formed, but no
/// signature over it.
fn broken(person: &Person) -> Value {
    let mut event = person.json(0);
    let other = Person::new(&format!("{} other", person.name)).json(0);
    assert_ne!(
        event["proof"][0]["proofValue"],
        other["proof"][0]["proofValue"]
    );
    event["proof"][0]["proofValue"] = other["proof"][0]["proofValue"].clone();
    event
}

fn assert_limited(reply: &Reply, scope: &str) -> u64 {
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );
    assert_eq!(reply.body["error"], "rate-limited");
    assert_eq!(reply.body["scope"], scope, "{}", reply.body);
    let retry = reply.body["retryAfter"].as_u64().unwrap();
    assert!(retry >= 1);
    assert_eq!(
        reply.headers.get("retry-after").unwrap().to_str().unwrap(),
        retry.to_string()
    );
    retry
}

fn assert_ok(reply: &Reply) {
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

/// A proof for `scid` over `challenge` that meets `low` bits but not `high`.
fn proof_between(scid: &str, challenge: &str, low: u8, high: u8) -> pow::Pow {
    let mut start = 0u128;
    loop {
        let p = pow::solve_range(scid, challenge, low, start, 1 << 20)
            .expect("a proof at the low bits");
        if !pow::check(scid, &p, high) {
            return p;
        }
        let bytes: [u8; 16] = bs58::decode(&p.nonce)
            .into_vec()
            .unwrap()
            .try_into()
            .unwrap();
        start = u128::from_be_bytes(bytes) + 1;
    }
}

async fn metrics(t: &TestStore) -> String {
    let reply = t.get("/v1/metrics").await;
    assert_eq!(reply.status, StatusCode::OK);
    assert!(reply
        .headers
        .get("content-type")
        .unwrap()
        .to_str()
        .unwrap()
        .starts_with("text/plain"));
    String::from_utf8(reply.raw).unwrap()
}

#[tokio::test]
async fn the_fourth_no_note_log_from_one_address_in_a_day_is_429_and_ipv6_counts_by_its_64() {
    let t = TestStore::with_settings(TestStore::brake_settings());
    // IPv4: three logs, then 429 address.
    for i in 0..3 {
        assert_ok(&publish_from(&t, "198.51.100.1:1000", &Person::new(&format!("v4 {i}"))).await);
    }
    let fourth = Person::new("v4 3");
    let retry = assert_limited(
        &publish_from(&t, "198.51.100.1:1001", &fourth).await,
        "address",
    );
    // One token back takes a third of a day.
    assert_eq!(retry, 8 * 3600);
    assert_eq!(t.count("logs"), 3);
    // Another IPv4 address has its own bucket.
    assert_ok(&publish_from(&t, "198.51.100.2:1000", &fourth).await);

    // IPv6: three logs from one address, then another address in the same /64 is 429 too.
    for i in 0..3 {
        let p = Person::new(&format!("v6 {i}"));
        assert_ok(&publish_from(&t, "[2001:db8:1:1::1]:1000", &p).await);
    }
    let next = Person::new("v6 3");
    assert_limited(
        &publish_from(&t, "[2001:db8:1:1:ffff::2]:1000", &next).await,
        "address",
    );
    // Another /64: 200.
    assert_ok(&publish_from(&t, "[2001:db8:1:2::1]:1000", &next).await);

    // A third of a day later the first address has one token again, and only one.
    t.set_now(T0 + retry);
    assert_ok(&publish_from(&t, "198.51.100.1:1000", &Person::new("v4 4")).await);
    assert_limited(
        &publish_from(&t, "198.51.100.1:1000", &Person::new("v4 5")).await,
        "address",
    );
    assert_eq!(t.admitted_by(&fourth.scid()), "brake");
}

#[tokio::test]
async fn the_121st_no_note_log_in_an_hour_store_wide_is_429_global() {
    let t = TestStore::with_settings(TestStore::brake_settings());
    for i in 0..120u32 {
        let peer = format!("10.0.{}.{}:1000", i / 250, i % 250 + 1);
        assert_ok(&publish_from(&t, &peer, &Person::new(&format!("g {i}"))).await);
    }
    let late = Person::new("g 120");
    let retry = assert_limited(&publish_from(&t, "10.1.0.1:1000", &late).await, "global");
    // 120 an hour: one token every 30 seconds.
    assert_eq!(retry, 30);
    assert_eq!(t.count("logs"), 120);
    // The empty episode logged its one line (`Store::notice_global_empty`), and the log the
    // brake admits next resets the flag, so the next empty episode logs again.
    assert!(t.store.global_empty_logged());
    t.set_now(T0 + 30);
    assert_ok(&publish_from(&t, "10.1.0.1:1000", &late).await);
    assert!(!t.store.global_empty_logged());
    assert_limited(
        &publish_from(&t, "10.1.0.2:1000", &Person::new("g 121")).await,
        "global",
    );
    assert!(t.store.global_empty_logged());
}

#[tokio::test]
async fn with_a_bucket_empty_a_new_log_with_a_broken_signature_is_429_not_400() {
    let t = TestStore::with_settings(TestStore::brake_settings());
    for i in 0..3 {
        assert_ok(&publish_from(&t, "198.51.100.1:1000", &Person::new(&format!("o {i}"))).await);
    }
    let alice = Person::new("alice");
    let body = json!({ "events": [broken(&alice)], "pow": t.pow_for(&alice.scid()) });
    let path = format!("/v1/logs/{}/events", alice.scid());
    let send = |peer: &'static str| {
        t.send_from(
            from(peer),
            "POST",
            &path,
            Some(serde_json::to_vec(&body).unwrap()),
            None,
            &[],
        )
    };
    // The bucket is empty: 429, no signature checked.
    assert_limited(&send("198.51.100.1:1000").await, "address");
    // From an address with tokens the same request reaches the replay: 400.
    let reply = send("198.51.100.2:1000").await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    // And with no proof at all the bucket is still checked first.
    assert_limited(
        &post_new(&t, from("198.51.100.1:1000"), &[], &alice, None).await,
        "address",
    );
    assert_eq!(t.count("logs"), 3);
}

#[tokio::test]
async fn a_bad_proof_is_403_pow_and_the_failure_bucket_fills_after_30() {
    let t = TestStore::with_settings(TestStore::brake_settings());
    let peer = from("198.51.100.9:1000");
    let alice = Person::new("alice");
    let bob = Person::new("bob");
    // A proof over a fresh challenge for the wrong SCID: its challenge is never spent (the
    // proof fails before it could be), so the same request repeats.
    let wrong = t.pow_for(&bob.scid());
    for i in 0..30 {
        // Bad proofs, a missing one and a broken signature with a good proof all count.
        let reply = match i % 3 {
            0 => post_new(&t, peer, &[], &alice, Some(wrong.clone())).await,
            1 => post_new(&t, peer, &[], &alice, None).await,
            _ => {
                let body = json!({ "events": [broken(&alice)], "pow": t.pow_for(&alice.scid()) });
                t.send_from(
                    peer,
                    "POST",
                    &format!("/v1/logs/{}/events", alice.scid()),
                    Some(serde_json::to_vec(&body).unwrap()),
                    None,
                    &[],
                )
                .await
            }
        };
        if i % 3 == 2 {
            assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
        } else {
            assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
            assert_eq!(reply.body["error"], "pow");
        }
    }
    // The 31st request, even a good one, is 429 failure before any check.
    let retry = assert_limited(
        &post_new(&t, peer, &[], &alice, Some(t.pow_for(&alice.scid()))).await,
        "failure",
    );
    // 30 an hour: one token every 2 minutes.
    assert_eq!(retry, 120);
    assert_eq!(t.count("logs"), 0);
    // Another address is not affected.
    assert_ok(&publish_from(&t, "198.51.100.10:1000", &bob).await);
    // Neither a 429 nor an admitted log is charged: two minutes on, one request passes and the
    // failure bucket keeps its token.
    t.set_now(T0 + 120);
    assert_ok(&post_new(&t, peer, &[], &alice, Some(t.pow_for(&alice.scid()))).await);
    let levels = metrics(&t).await;
    assert!(
        levels.contains("failure buckets: 1 in use, 0 empty, lowest 1.00 of 30"),
        "{levels}"
    );
}

#[tokio::test]
async fn a_challenge_keeps_the_bits_it_was_issued_at_when_the_cost_changes() {
    let t = TestStore::with_settings(TestStore::brake_settings());
    let pow = t.get("/v1/pow").await.body;
    assert_eq!(pow["bits"], TEST_POW_BITS);
    // The answer is the bits and a fresh one-use challenge, good for 300 seconds; nothing else.
    assert_eq!(pow.as_object().unwrap().len(), 2, "{pow}");
    let challenge = pow["challenge"].clone();
    assert!(challenge["nonce"].is_string());
    assert_eq!(challenge["expires"], T0 + 300);

    let alice = Person::new("alice");
    let bob = Person::new("bob");
    let carol = Person::new("carol");
    let peer = from("198.51.100.1:1000");
    // A challenge issued at the old bits, and a proof over it, before the cost is raised.
    let old_nonce = t.store.pow_challenge().nonce;
    let old_alice = proof_between(&alice.scid(), &old_nonce, TEST_POW_BITS, 12);

    // Raised to 12 bits at T0 + 100.
    t.set_now(T0 + 100);
    let raised = Settings {
        pow_bits: 12,
        ..TestStore::brake_settings()
    };
    let t = t.restarted_with(raised);
    assert_eq!(t.get("/v1/pow").await.body["bits"], 12);

    // A new challenge carries the new bits: a proof at the old bits over it is refused, one
    // at the new bits passes.
    let new_nonce = t.store.pow_challenge().nonce;
    let short_bob = proof_between(&bob.scid(), &new_nonce, TEST_POW_BITS, 12);
    let reply = post_new(
        &t,
        peer,
        &[],
        &bob,
        Some(serde_json::to_value(&short_bob).unwrap()),
    )
    .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert_eq!(reply.body["error"], "pow");
    let new_bob = pow::solve(&bob.scid(), &new_nonce, 12);
    assert_ok(
        &post_new(
            &t,
            peer,
            &[],
            &bob,
            Some(serde_json::to_value(&new_bob).unwrap()),
        )
        .await,
    );

    // The proof over the challenge issued at the old bits still passes, for as long as that
    // challenge lives (no grace window).
    t.set_now(T0 + 299);
    assert_ok(
        &post_new(
            &t,
            peer,
            &[],
            &alice,
            Some(serde_json::to_value(&old_alice).unwrap()),
        )
        .await,
    );

    // The bits are under the challenge's tag: a challenge issued at 12 bits with its bits byte
    // lowered fails the tag, even with a proof that meets the lowered bits.
    let minted = t.store.pow_challenge().nonce;
    let mut bytes = bs58::decode(&minted).into_vec().unwrap();
    assert_eq!(bytes[8], 12);
    bytes[8] = 0;
    let forged = bs58::encode(&bytes).into_string();
    let forged_proof = pow::solve(&carol.scid(), &forged, 0);
    let reply = post_new(
        &t,
        peer,
        &[],
        &carol,
        Some(serde_json::to_value(&forged_proof).unwrap()),
    )
    .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert_eq!(reply.body["error"], "pow");
    // The untouched challenge, solved at its bits, admits carol.
    let carol_proof = pow::solve(&carol.scid(), &minted, 12);
    assert_ok(
        &post_new(
            &t,
            peer,
            &[],
            &carol,
            Some(serde_json::to_value(&carol_proof).unwrap()),
        )
        .await,
    );
}

#[tokio::test]
async fn behind_a_trusted_proxy_the_forwarded_address_counts_and_elsewhere_the_socket_does() {
    // The default `CLIENT_IP_HEADER`: X-Forwarded-For. A CF-Connecting-IP the client wrote and
    // the proxy passed through changes nothing.
    let t = TestStore::with_settings(Settings {
        trusted_proxies: vec!["10.9.0.0/16".parse::<Cidr>().unwrap()],
        ..TestStore::brake_settings()
    });
    assert_eq!(
        t.store.settings.client_ip_header,
        ClientIpHeader::XForwardedFor
    );
    let proxy = from("10.9.0.5:1000");
    let new = |name: &str| Person::new(name);
    let with_pow = |p: &Person| Some(t.pow_for(&p.scid()));
    // Two clients behind the proxy fill separate buckets.
    for i in 0..3 {
        let p = new(&format!("a {i}"));
        let reply = post_new(
            &t,
            proxy,
            &[("x-forwarded-for", "198.51.100.1")],
            &p,
            with_pow(&p),
        );
        assert_ok(&reply.await);
    }
    let p = new("a 3");
    let reply = post_new(
        &t,
        proxy,
        &[("x-forwarded-for", "198.51.100.1")],
        &p,
        with_pow(&p),
    );
    assert_limited(&reply.await, "address");
    // A rotating CF-Connecting-IP does not dodge the bucket: the X-Forwarded-For address counts.
    let reply = post_new(
        &t,
        proxy,
        &[
            ("x-forwarded-for", "198.51.100.1"),
            ("cf-connecting-ip", "198.51.100.77"),
        ],
        &p,
        with_pow(&p),
    );
    assert_limited(&reply.await, "address");
    let reply = post_new(
        &t,
        proxy,
        &[("x-forwarded-for", "198.51.100.2")],
        &p,
        with_pow(&p),
    );
    assert_ok(&reply.await);
    // The nearest X-Forwarded-For entry that is no trusted proxy.
    let p = new("x 0");
    let reply = post_new(
        &t,
        proxy,
        &[("x-forwarded-for", "203.0.113.50, 198.51.100.3, 10.9.0.6")],
        &p,
        with_pow(&p),
    );
    assert_ok(&reply.await);

    // The same header from a socket outside the list is ignored: the socket counts.
    let outside = from("192.0.2.77:1000");
    for i in 0..3 {
        let p = new(&format!("o {i}"));
        let client = format!("198.51.100.{}", 100 + i);
        let headers = [("x-forwarded-for", client.as_str())];
        assert_ok(&post_new(&t, outside, &headers, &p, with_pow(&p)).await);
    }
    let p = new("o 3");
    let reply = post_new(
        &t,
        outside,
        &[("x-forwarded-for", "198.51.100.200")],
        &p,
        with_pow(&p),
    );
    assert_limited(&reply.await, "address");

    // The X-Forwarded-For client took one token: two more pass, then 429.
    for i in 1..3 {
        let p = new(&format!("x {i}"));
        let headers = [("x-forwarded-for", "198.51.100.3")];
        assert_ok(&post_new(&t, proxy, &headers, &p, with_pow(&p)).await);
    }
    let p = new("x 3");
    let headers = [("x-forwarded-for", "198.51.100.3")];
    assert_limited(
        &post_new(&t, proxy, &headers, &p, with_pow(&p)).await,
        "address",
    );
}

/// `CLIENT_IP_HEADER=cf-connecting-ip` (Cloudflare ingress): the CF header names the client, and
/// an X-Forwarded-For the client wrote is ignored.
#[tokio::test]
async fn with_the_cloudflare_setting_only_cf_connecting_ip_names_the_client() {
    let t = TestStore::with_settings(Settings {
        trusted_proxies: vec!["10.9.0.0/16".parse::<Cidr>().unwrap()],
        client_ip_header: ClientIpHeader::CfConnectingIp,
        ..TestStore::brake_settings()
    });
    let proxy = from("10.9.0.5:1000");
    for i in 0..3 {
        let p = Person::new(&format!("cf {i}"));
        // A different X-Forwarded-For each time does not make a new client.
        let forwarded = format!("203.0.113.{i}");
        let headers = [
            ("cf-connecting-ip", "198.51.100.1"),
            ("x-forwarded-for", forwarded.as_str()),
        ];
        assert_ok(&post_new(&t, proxy, &headers, &p, Some(t.pow_for(&p.scid()))).await);
    }
    let p = Person::new("cf 3");
    let headers = [
        ("cf-connecting-ip", "198.51.100.1"),
        ("x-forwarded-for", "203.0.113.99"),
    ];
    assert_limited(
        &post_new(&t, proxy, &headers, &p, Some(t.pow_for(&p.scid()))).await,
        "address",
    );
    // Another CF address has its own bucket.
    let headers = [("cf-connecting-ip", "198.51.100.2")];
    assert_ok(&post_new(&t, proxy, &headers, &p, Some(t.pow_for(&p.scid()))).await);
}

/// `TRUSTED_PROXIES` empty but a request carries a proxy header (the store sits behind a proxy
/// it was never told about): every client would count as the proxy's one address for the
/// brake, so the first such request logs the warning once (`Store::note_untrusted_proxy_headers`).
#[tokio::test]
async fn proxy_headers_with_no_trusted_proxy_log_the_warning_once() {
    let t = TestStore::new();
    assert!(!t.store.untrusted_proxy_warned());
    let p = Person::new("px");
    let reply = post_new(
        &t,
        from("10.9.0.5:1000"),
        &[("x-forwarded-for", "198.51.100.9")],
        &p,
        Some(t.pow_for(&p.scid())),
    )
    .await;
    assert_ok(&reply);
    assert!(t.store.untrusted_proxy_warned());
    // The socket address counted, not the forwarded one: the header was never trusted.
    assert_eq!(t.admitted_by(&p.scid()), "brake");
}

#[tokio::test]
async fn metrics_show_the_levels_without_addresses_and_fall_after_an_admitted_no_note_log() {
    let t = TestStore::with_settings(TestStore::brake_settings());
    let before = metrics(&t).await;
    assert!(before.contains("global 120.00 of 120"), "{before}");
    assert!(
        before.contains("address buckets: 0 in use, 0 empty, lowest 3.00 of 3"),
        "{before}"
    );

    let alice = Person::new("alice");
    assert_ok(&publish_from(&t, "198.51.100.1:1000", &alice).await);
    let bob = Person::new("bob");
    let reply = post_new(&t, from("[2001:db8:5:6::7]:1"), &[], &bob, None).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);

    let after = metrics(&t).await;
    assert!(after.contains("global 119.00 of 120"), "{after}");
    // A refused request takes nothing from the global or the address buckets.
    assert!(
        after.contains("address buckets: 1 in use, 0 empty, lowest 2.00 of 3"),
        "{after}"
    );
    assert!(
        after.contains("failure buckets: 1 in use, 0 empty, lowest 29.00 of 30"),
        "{after}"
    );
    assert!(!after.contains("198.51.100"), "{after}");
    assert!(!after.contains("2001:db8"), "{after}");
    // Levels refill with the clock.
    t.set_now(T0 + 30);
    assert!(metrics(&t).await.contains("global 120.00 of 120"));
}

#[tokio::test]
async fn the_rows_of_refilled_buckets_are_dropped() {
    let t = TestStore::with_settings(TestStore::brake_settings());
    assert_ok(&publish_from(&t, "198.51.100.1:1000", &Person::new("alice")).await);
    let reply = post_new(&t, from("198.51.100.2:1"), &[], &Person::new("bob"), None).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    // global, one address and one failure bucket.
    assert_eq!(t.count("brake"), 3);
    // A day on, both refilled: the next admission and refusal leave only their own rows.
    t.set_now(T0 + 86_400);
    assert_ok(&publish_from(&t, "198.51.100.3:1000", &Person::new("carol")).await);
    let reply = post_new(&t, from("198.51.100.4:1"), &[], &Person::new("dan"), None).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(t.count("brake"), 3);
    let levels = metrics(&t).await;
    assert!(
        levels.contains("address buckets: 1 in use, 0 empty, lowest 2.00 of 3"),
        "{levels}"
    );
}

#[test]
fn the_defaults_from_an_empty_configuration_are_3_120_and_22_and_each_key_changes_its_own() {
    let empty = Settings::from_config(|_| None).unwrap();
    assert_eq!(empty.brake_addr_burst, 3);
    assert_eq!(empty.brake_addr_per_day, 3);
    assert_eq!(empty.brake_global_per_hour, 120);
    assert_eq!(empty.pow_bits, 22);
    assert!(empty.trusted_proxies.is_empty());
    assert_eq!(empty.client_ip_header, ClientIpHeader::XForwardedFor);
    assert_eq!(empty.vouch_url, None);
    assert_eq!(empty.vouch_credential, None);
    assert!(empty.vouch_keys.is_empty());

    let config = |pairs: &'static [(&'static str, &'static str)]| {
        Settings::from_config(move |name| {
            pairs
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, v)| v.to_string())
        })
    };
    let s = config(&[("BRAKE_ADDR_BURST", "5")]).unwrap();
    assert_eq!((s.brake_addr_burst, s.brake_addr_per_day), (5, 3));
    let s = config(&[("BRAKE_ADDR_PER_DAY", "7")]).unwrap();
    assert_eq!((s.brake_addr_burst, s.brake_addr_per_day), (3, 7));
    let s = config(&[("BRAKE_GLOBAL_PER_HOUR", "40")]).unwrap();
    assert_eq!(s.brake_global_per_hour, 40);
    assert_eq!(s.pow_bits, 22);
    let s = config(&[("POW_BITS", "20")]).unwrap();
    assert_eq!((s.pow_bits, s.brake_global_per_hour), (20, 120));
    let s = config(&[(
        "TRUSTED_PROXIES",
        "173.245.48.0/20, 2400:cb00::/32,10.0.0.1",
    )])
    .unwrap();
    assert_eq!(s.trusted_proxies.len(), 3);
    let s = config(&[("CLIENT_IP_HEADER", " CF-Connecting-IP ")]).unwrap();
    assert_eq!(s.client_ip_header, ClientIpHeader::CfConnectingIp);
    let s = config(&[("CLIENT_IP_HEADER", "x-forwarded-for")]).unwrap();
    assert_eq!(s.client_ip_header, ClientIpHeader::XForwardedFor);
    let key = common::vouch::vouching_multikey();
    let s = Settings::from_config(|name| match name {
        "VOUCH_URL" => Some("https://platform.example".into()),
        "VOUCH_CREDENTIAL" => Some("secret".into()),
        "VOUCH_KEYS" => Some(format!(" {key} ,")),
        _ => None,
    })
    .unwrap();
    assert_eq!(s.vouch_url.as_deref(), Some("https://platform.example"));
    assert_eq!(
        s.vouch_credential,
        Some(identity_store::vouch::Credential("secret".into()))
    );
    assert!(!format!("{s:?}").contains("secret"));
    assert_eq!(s.vouch_keys, vec![key]);

    // Values that are not numbers, ranges or keys stop the store.
    assert!(config(&[("BRAKE_ADDR_BURST", "three")]).is_err());
    assert!(config(&[("POW_BITS", "300")]).is_err());
    assert!(config(&[("TRUSTED_PROXIES", "10.0.0.0/40")]).is_err());
    assert!(config(&[("TRUSTED_PROXIES", "proxy.example")]).is_err());
    assert!(config(&[("CLIENT_IP_HEADER", "x-real-ip")]).is_err());
    assert!(config(&[("VOUCH_KEYS", "z6MkNotAKey")]).is_err());
}

#[tokio::test]
async fn a_store_from_an_empty_configuration_publishes_22_bits() {
    let t = TestStore::with_settings(Settings::from_config(|_| None).unwrap());
    let pow = t.get("/v1/pow").await.body;
    assert_eq!(pow["bits"], 22);
}

//! The identities the link server's tests sign with (`vectors/link-server-identities.json`):
//! a store key, persons with key logs built on the `build` module, a signed head at every seq
//! of each log, and a device-only key. The link server's fake identity store serves them as
//! the identity store does (`/v1/keys`, `/v1/heads`, `/v1/logs`); its tests sign links and auth
//! messages with the device secrets.
//!
//! Persons: `alice` (d1), `bob` (d1), `carol` (d1, and the bot key d2 with `write`), `dave`
//! (d1, d2; seq 1 revokes d2), `frank` (d1). `eve` is a device-only key.
//!
//! Regenerate with
//! `AD4M_IDENTITY_WRITE_VECTORS=1 cargo test -p ad4m-identity-core --features build --test link_server_fixture`.
#![cfg(all(feature = "build", not(target_arch = "wasm32")))]

mod common;

use std::path::PathBuf;

use ad4m_hash_chain::{encode_ed25519_multikey, encode_proof_value, sha256};
use ad4m_identity_core::build;
use ad4m_identity_core::wire::{trusted_seq, SignedHead};
use ad4m_identity_core::{
    commitment, encode_x25519_multikey, fold, BotScope, DeviceKey, HeadStatement, KelChain,
    KelPolicy, KeyLife,
};
use common::hex;
use ed25519_dalek::{Signer, SigningKey};
use serde_json::{json, Map, Value};

/// When the store "signed" every head: a fixed time, so the file is stable.
const HEAD_TIME: u64 = 1_790_000_000;
/// The bot key's expiry (unix seconds): far past any test clock.
const BOT_EXPIRES: u64 = 4_000_000_000;

fn key(label: &str) -> SigningKey {
    SigningKey::from_bytes(&sha256(format!("link-server {label}").as_bytes()))
}

fn multikey(key: &SigningKey) -> String {
    encode_ed25519_multikey(key.verifying_key().as_bytes())
}

fn salt(name: &str, n: u32) -> String {
    bs58::encode(&sha256(format!("link-server {name} salt {n}").as_bytes())[..16]).into_string()
}

fn master(name: &str, n: u32) -> SigningKey {
    key(&format!("{name} master {n}"))
}

fn device_key(name: &str, n: u32) -> SigningKey {
    key(&format!("{name} d{n}"))
}

fn device(name: &str, n: u32, life: KeyLife) -> DeviceKey {
    DeviceKey {
        id: format!("d{n}"),
        sign: multikey(&device_key(name, n)),
        enc: encode_x25519_multikey(&sha256(format!("link-server {name} enc d{n}").as_bytes())),
        life,
    }
}

fn signed_head(chain: &KelChain, seq: u64, store: &SigningKey) -> SignedHead {
    let store_did = format!("did:key:{}", multikey(store));
    let hash = chain.hash_at(seq).unwrap().clone();
    let statement = HeadStatement {
        scid: chain.id(),
        seq,
        hash: &hash,
        time: HEAD_TIME,
        store: &store_did,
    };
    let sig = store.sign(&statement.signing_bytes().unwrap()).to_bytes();
    SignedHead {
        scid: chain.id().clone(),
        seq,
        hash,
        time: HEAD_TIME,
        store: store_did,
        sig: encode_proof_value(&sig),
    }
}

/// One person: devices at inception, then `adds` (device n, life) and `revokes` in order.
enum Step {
    Add(u32, KeyLife),
    Revoke(&'static str),
}

fn person(name: &str, devices: &[u32], steps: Vec<Step>, store: &SigningKey) -> Value {
    let inception: Vec<DeviceKey> = devices
        .iter()
        .map(|n| device(name, *n, KeyLife::Standing))
        .collect();
    let mut event = build::incept_human(
        &master(name, 0),
        commitment(&multikey(&master(name, 1)), &salt(name, 1)),
        inception,
    )
    .unwrap();
    for n in devices {
        build::cosign(&mut event, &format!("d{n}"), &device_key(name, *n)).unwrap();
    }
    let mut events = vec![event];
    let mut chain = fold(&events).unwrap();
    let mut numbers: Vec<u32> = devices.to_vec();
    for step in steps {
        let event = match step {
            Step::Add(n, life) => {
                let mut event =
                    build::add_unsigned(&chain, device(name, n, life), "offer-1", "z3coSig")
                        .unwrap();
                build::cosign(&mut event, "m0", &master(name, 0)).unwrap();
                numbers.push(n);
                event
            }
            Step::Revoke(id) => build::revoke(&chain, &master(name, 0), &[id.to_string()]).unwrap(),
        };
        chain.append(&KelPolicy, &event).unwrap();
        events.push(event);
    }
    let head = chain.head().seq;
    let heads: Vec<SignedHead> = (0..=head).map(|s| signed_head(&chain, s, store)).collect();
    for h in &heads {
        assert_eq!(trusted_seq(&chain, h, &[multikey(store)]), Some(h.seq));
    }
    let state = chain.state();
    let mut keys = Map::new();
    for n in numbers {
        let secret = device_key(name, n);
        let id = format!("d{n}");
        let record = &state.keys[&id];
        let scope = state.bot_scope_at(&id, head).cloned();
        keys.insert(
            id.clone(),
            json!({
                "secret": hex(secret.as_bytes()),
                "multikey": multikey(&secret),
                "did": format!("did:key:{}", multikey(&secret)),
                "keyId": id,
                "added": record.added,
                "revoked": record.revoked,
                "bot": scope,
                "expires": scope.as_ref().map(|s: &BotScope| s.expires),
            }),
        );
    }
    json!({
        "did": state.did,
        "scid": chain.id(),
        "events": events,
        "heads": heads,
        "keys": keys,
    })
}

fn build_fixture() -> Value {
    let store = key("store");
    let bot = KeyLife::Bot(BotScope {
        can: vec!["write".into()],
        expires: BOT_EXPIRES,
    });
    let eve = key("eve device only");
    json!({
        "comment": "Generated by ad4m-identity-core/tests/link_server_fixture.rs. Test identities only: every secret is sha256 of a public label.",
        "store": {
            "secret": hex(store.as_bytes()),
            "multikey": multikey(&store),
            "did": format!("did:key:{}", multikey(&store)),
        },
        "persons": {
            "alice": person("alice", &[1], vec![], &store),
            "bob": person("bob", &[1], vec![], &store),
            "carol": person("carol", &[1], vec![Step::Add(2, bot)], &store),
            "dave": person("dave", &[1, 2], vec![Step::Revoke("d2")], &store),
            "frank": person("frank", &[1], vec![], &store),
        },
        "deviceOnly": {
            "eve": {
                "secret": hex(eve.as_bytes()),
                "multikey": multikey(&eve),
                "did": format!("did:key:{}", multikey(&eve)),
            },
        },
    })
}

#[test]
fn link_server_identities_are_fresh() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("vectors")
        .join("link-server-identities.json");
    let text = serde_json::to_string_pretty(&build_fixture()).unwrap() + "\n";
    if std::env::var_os("AD4M_IDENTITY_WRITE_VECTORS").is_some() {
        std::fs::write(&path, &text).unwrap();
    }
    let committed =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    assert_eq!(
        committed, text,
        "link-server-identities.json is stale: regenerate with AD4M_IDENTITY_WRITE_VECTORS=1"
    );
}

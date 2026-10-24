//! Fixtures for the key-log tests: fixed keys, a log builder on the `build` module, the crafted
//! refusal cases, and the shared-vector builders and checkers.
#![allow(dead_code)]

use ad4m_hash_chain::{encode_ed25519_multikey, sha256, verify_proof};
use ad4m_identity_core::build::{self, Signer};
use ad4m_identity_core::domain::{LABELS, SIGNING_DOMAINS};
use ad4m_identity_core::link::{
    check_add, check_secret, claim_body, claim_view, decode_secret, make_claim, match_code,
    match_input, offer_commit, offer_signing_bytes, sign_offer, verify_claim, verify_offer, Claim,
    ClaimView, DeviceString, Offer, OfferKind, OfferString, SignedBy, SignedOffer, CLAIM_DOMAIN,
    OFFER_TTL_S,
};
use ad4m_identity_core::{
    commitment, derive_scid, encode_x25519_multikey, error_code, event_hash, fold, fold_json, pow,
    signing_bytes, verify_signed, BotScope, DeviceKey, Event, FoldError, KelChain, KelPolicy,
    KeyLife, Multikey, Op, KEL_DOMAIN,
};
use ed25519_dalek::SigningKey;
use serde_json::{json, Value};

/// A key from a label: `sha256(label)` as the Ed25519 secret.
pub fn sk(label: &str) -> SigningKey {
    SigningKey::from_bytes(&sha256(label.as_bytes()))
}

pub fn mk(key: &SigningKey) -> Multikey {
    encode_ed25519_multikey(key.verifying_key().as_bytes())
}

pub fn enc(label: &str) -> Multikey {
    encode_x25519_multikey(&sha256(format!("enc {label}").as_bytes()))
}

pub fn master(n: u32) -> SigningKey {
    sk(&format!("master {n}"))
}

/// `salt_n` as the log carries it: base58btc of 16 bytes.
pub fn salt(n: u32) -> String {
    bs58::encode(&sha256(format!("salt {n}").as_bytes())[..16]).into_string()
}

/// The commitment to master *n*.
pub fn next(n: u32) -> String {
    commitment(&mk(&master(n)), &salt(n))
}

/// The secret of device key `d<n>`.
pub fn dev(n: u32) -> SigningKey {
    sk(&format!("device {n}"))
}

pub fn device(n: u32, life: KeyLife) -> DeviceKey {
    DeviceKey {
        id: format!("d{n}"),
        sign: mk(&dev(n)),
        enc: enc(&format!("d{n}")),
        life,
    }
}

pub fn bot(can: &[&str], expires: u64) -> KeyLife {
    KeyLife::Bot(BotScope {
        can: can.iter().map(|s| s.to_string()).collect(),
        expires,
    })
}

fn id_number(id: &str) -> u32 {
    id[1..].parse().unwrap()
}

/// Signs `event` as `key_id` with `key` (no checks).
pub fn sign(event: &mut Event, key_id: &str, key: &SigningKey) {
    build::cosign(event, key_id, key).unwrap();
}

/// Event #0 listing `devices`, signed by master 0 and each listed device; next = commitment to master 1.
pub fn incept_event(devices: Vec<DeviceKey>) -> Event {
    let ids: Vec<String> = devices.iter().map(|d| d.id.clone()).collect();
    let mut event = build::incept_human(&master(0), next(1), devices).unwrap();
    for id in ids {
        sign(&mut event, &id, &dev(id_number(&id)));
    }
    event
}

/// A valid log built on the `build` module, with its chain.
pub struct Log {
    pub events: Vec<Event>,
    pub chain: KelChain,
    /// The number of the current master key.
    pub n: u32,
}

impl Log {
    pub fn incept(devices: Vec<DeviceKey>) -> Log {
        let event = incept_event(devices);
        let chain = fold(std::slice::from_ref(&event)).unwrap();
        Log {
            events: vec![event],
            chain,
            n: 0,
        }
    }

    /// One standing device key `d1`.
    pub fn new() -> Log {
        Log::incept(vec![device(1, KeyLife::Standing)])
    }

    pub fn seq(&self) -> u64 {
        self.chain.head().seq
    }

    pub fn push(&mut self, event: Event) {
        self.chain.append(&KelPolicy, &event).unwrap();
        self.events.push(event);
    }

    pub fn try_push(&self, event: &Event) -> Result<(), ad4m_identity_core::ChainError<FoldError>> {
        let mut chain = self.chain.clone();
        chain.append(&KelPolicy, event).map(|_| ())
    }

    /// The `add` of the next device key with `life`, signed by the current master (not appended).
    pub fn add_event(&self, life: KeyLife) -> Event {
        let id = self.chain.state().next_device_id();
        let key = device(id_number(&id), life);
        let mut event = build::add_unsigned(&self.chain, key, "offer-1", "z3coSig").unwrap();
        let id = self.chain.state().current_master().unwrap().id.clone();
        sign(&mut event, &id, &master(self.n));
        event
    }

    pub fn add(&mut self, life: KeyLife) -> String {
        let id = self.chain.state().next_device_id();
        let event = self.add_event(life);
        self.push(event);
        id
    }

    pub fn revoke(&mut self, ids: &[&str]) {
        let ids: Vec<String> = ids.iter().map(|s| s.to_string()).collect();
        let event = build::revoke(&self.chain, &master(self.n), &ids).unwrap();
        self.push(event);
    }

    pub fn scope(&mut self, id: &str, can: &[&str], expires: u64) {
        let scope = BotScope {
            can: can.iter().map(|s| s.to_string()).collect(),
            expires,
        };
        let event = build::scope(&self.chain, &master(self.n), &id.to_string(), &scope).unwrap();
        self.push(event);
    }

    /// The `rotate` to master n+1 (not appended); `end` commits to nothing.
    pub fn rotate_event(&self, revoke: &[&str], end: bool) -> Event {
        let n = self.n + 1;
        let ids: Vec<String> = revoke.iter().map(|s| s.to_string()).collect();
        let next_commitment = if end { None } else { Some(next(n + 1)) };
        build::rotate(&self.chain, &master(n), &salt(n), next_commitment, &ids).unwrap()
    }

    pub fn rotate(&mut self, revoke: &[&str], end: bool) {
        let event = self.rotate_event(revoke, end);
        self.push(event);
        self.n += 1;
    }

    /// The events as JSON.
    pub fn json(&self) -> Vec<Value> {
        self.events.iter().map(json).collect()
    }
}

impl Default for Log {
    fn default() -> Self {
        Log::new()
    }
}

pub fn json(event: &Event) -> Value {
    serde_json::to_value(event).unwrap()
}

/// The log of the vectors: every op, every key life.
pub fn vector_log() -> Log {
    let mut log = Log::incept(vec![
        device(1, KeyLife::Standing),
        device(2, KeyLife::Until(1_800_000_000)),
    ]);
    let bot_id = log.add(bot(&["read", "write"], 1_750_000_000)); // d3, seq 1
    log.scope(&bot_id, &["read"], 1_900_000_000); // seq 2
    log.revoke(&["d2"]); // seq 3
    log.rotate(&["d1"], false); // seq 4: m1
    log.add(KeyLife::Standing); // d4, seq 5
    log.rotate(&[], true); // seq 6: m2, end
    log
}

/// One crafted refusal: the events and the error code that `fold_json` must give.
pub struct Case {
    pub name: &'static str,
    pub rule: &'static str,
    pub events: Vec<Value>,
    pub error: Value,
}

fn policy(reason: &str) -> Value {
    json!({"code": "policy", "reason": reason})
}

fn format(name: &str) -> Value {
    json!({"code": "policy", "reason": "Format", "format": name})
}

/// The incept and one later event, as JSON, with `edit` applied to the later one.
fn with_second(log: &Log, second: &Event, edit: impl FnOnce(&mut Value)) -> Vec<Value> {
    let mut events = log.json();
    let mut value = json(second);
    edit(&mut value);
    events.push(value);
    events
}

fn incept_json(edit: impl FnOnce(&mut Value)) -> Vec<Value> {
    let mut value = json(&incept_event(vec![device(1, KeyLife::Standing)]));
    edit(&mut value);
    vec![value]
}

/// Every refusal case of the format spec, one per rule.
pub fn refusal_cases() -> Vec<Case> {
    let base = Log::new();
    let add = base.add_event(KeyLife::Standing);
    let mut cases = Vec::new();
    let mut case = |name, rule, events, error| {
        cases.push(Case {
            name,
            rule,
            events,
            error,
        })
    };

    case(
        "unknown op",
        "10",
        with_second(&base, &add, |v| v["op"] = json!("merge")),
        format("UnknownOp"),
    );
    case(
        "unknown field",
        "10",
        incept_json(|v| v["name"] = json!("Alice's laptop")),
        format("UnknownField"),
    );
    case(
        "unknown device-key field",
        "10",
        with_second(&base, &add, |v| v["add"][0]["model"] = json!("phone")),
        format("UnknownField"),
    );
    case(
        "float",
        "10",
        incept_json(|v| v["add"][0]["expires"] = json!(1.5)),
        format("Float"),
    );
    case(
        "big integer",
        "10",
        incept_json(|v| v["add"][0]["expires"] = json!(9_007_199_254_740_992u64)),
        format("BigInteger"),
    );
    case(
        "group",
        "10",
        incept_json(|v| v["kind"] = json!("group")),
        format("UnsupportedKind"),
    );
    case(
        "other format",
        "10",
        incept_json(|v| v["format"] = json!("coasys/2")),
        format("Version"),
    );
    case(
        "empty proof array",
        "10",
        incept_json(|v| v["proof"] = json!([])),
        format("NotCanonical"),
    );
    case(
        "can without bot mark",
        "8",
        with_second(&base, &add, |v| v["add"][0]["can"] = json!(["read"])),
        policy("CanWithoutBotMark"),
    );
    case(
        "wrong prev",
        "1",
        with_second(&base, &add, |v| {
            v["prev"] = json!(commitment("x", "y"));
        }),
        json!({"code": "prev"}),
    );
    case(
        "wrong seq",
        "1",
        with_second(&base, &add, |v| v["seq"] = json!(2)),
        json!({"code": "seq"}),
    );

    // Typed events, each signed as the rules ask but for the rule it breaks.
    let typed = |events: &[Event]| events.iter().map(json).collect::<Vec<_>>();
    let first = base.events[0].clone();

    let mut wrong_scid = add.clone();
    wrong_scid.scid = commitment("other", "log");
    wrong_scid.proof.clear();
    sign(&mut wrong_scid, "m0", &master(0));
    case(
        "other scid",
        "2",
        typed(&[first.clone(), wrong_scid]),
        json!({"code": "wrong-chain"}),
    );

    let mut forged_scid = first.clone();
    forged_scid.scid = commitment("not", "derived");
    forged_scid.proof.clear();
    sign(&mut forged_scid, "m0", &master(0));
    sign(&mut forged_scid, "d1", &dev(1));
    case(
        "scid not derived",
        "2",
        typed(&[forged_scid]),
        format("Scid"),
    );

    let mut not_incept = add.clone();
    not_incept.seq = 0;
    not_incept.prev = None;
    case(
        "first event not an incept",
        "2",
        typed(&[not_incept]),
        format("NotIncept"),
    );

    let mut d2_first =
        build::incept_unsigned(&mk(&master(0)), next(1), vec![device(2, KeyLife::Standing)])
            .unwrap();
    sign(&mut d2_first, "m0", &master(0));
    sign(&mut d2_first, "d2", &dev(2));
    case(
        "out-of-order key id",
        "6",
        typed(&[d2_first]),
        policy("KeyIdOrder"),
    );

    let mut dup = build::incept_unsigned(
        &mk(&master(0)),
        next(1),
        vec![DeviceKey {
            id: "d1".into(),
            sign: mk(&master(0)),
            enc: enc("dup"),
            life: KeyLife::Standing,
        }],
    )
    .unwrap();
    sign(&mut dup, "m0", &master(0));
    case("duplicate key", "7", typed(&[dup]), policy("DuplicateKey"));

    let mut bot_incept = build::incept_unsigned(
        &mk(&master(0)),
        next(1),
        vec![device(1, bot(&["read"], 10))],
    )
    .unwrap();
    sign(&mut bot_incept, "m0", &master(0));
    sign(&mut bot_incept, "d1", &dev(1));
    case(
        "bot in incept",
        "8",
        typed(&[bot_incept]),
        policy("BotInIncept"),
    );

    let mut two = build::add_unsigned(&base.chain, device(2, KeyLife::Standing), "o", "c").unwrap();
    if let Op::Add { add, .. } = &mut two.op {
        add.push(device(3, KeyLife::Standing));
    }
    sign(&mut two, "m0", &master(0));
    case(
        "add with two keys",
        "4",
        typed(&[first.clone(), two]),
        policy("AddNotOne"),
    );

    let mut empty_scope =
        build::add_unsigned(&base.chain, device(2, bot(&[], 10)), "o", "c").unwrap();
    sign(&mut empty_scope, "m0", &master(0));
    case(
        "bot with no action",
        "8",
        typed(&[first.clone(), empty_scope]),
        policy("EmptyScope"),
    );

    let mut bad_action =
        build::add_unsigned(&base.chain, device(2, bot(&["admin"], 10)), "o", "c").unwrap();
    sign(&mut bad_action, "m0", &master(0));
    case(
        "bot with an unknown action",
        "8",
        typed(&[first.clone(), bad_action]),
        format("UnknownAction"),
    );

    let mut x_sign = build::add_unsigned(
        &base.chain,
        DeviceKey {
            id: "d2".into(),
            sign: enc("as-sign"),
            enc: enc("d2"),
            life: KeyLife::Standing,
        },
        "o",
        "c",
    )
    .unwrap();
    sign(&mut x_sign, "m0", &master(0));
    case(
        "X25519 key as a signing key",
        "10",
        typed(&[first.clone(), x_sign]),
        policy("UnsupportedKeyType"),
    );

    let mut p256 = vec![0x80, 0x24];
    p256.extend_from_slice(&[2u8; 33]);
    let passkey = format!("z{}", bs58::encode(p256).into_string());
    let mut passkey_add = build::add_unsigned(
        &base.chain,
        DeviceKey {
            id: "d2".into(),
            sign: passkey,
            enc: enc("d2"),
            life: KeyLife::Standing,
        },
        "o",
        "c",
    )
    .unwrap();
    sign(&mut passkey_add, "m0", &master(0));
    case(
        "P-256 passkey key",
        "10",
        typed(&[first.clone(), passkey_add]),
        policy("UnsupportedKeyType"),
    );

    let mut no_cosign = first.clone();
    no_cosign.proof.truncate(1);
    case(
        "incept without the device's proof",
        "4",
        typed(&[no_cosign]),
        policy("MissingProof"),
    );

    let mut by_device =
        build::add_unsigned(&base.chain, device(2, KeyLife::Standing), "o", "c").unwrap();
    sign(&mut by_device, "d1", &dev(1));
    case(
        "add signed by a device key",
        "4",
        typed(&[first.clone(), by_device]),
        policy("MissingProof"),
    );

    let mut forged =
        build::add_unsigned(&base.chain, device(2, KeyLife::Standing), "o", "c").unwrap();
    sign(&mut forged, "m0", &dev(1));
    case(
        "add with a forged master proof",
        "3",
        typed(&[first.clone(), forged]),
        policy("BadProof"),
    );

    let mut other_domain =
        build::add_unsigned(&base.chain, device(2, KeyLife::Standing), "o", "c").unwrap();
    let mut proof = build::proof_config(&other_domain, "m0");
    proof.domain = "coasys:link".into();
    let input = ad4m_hash_chain::signing_input(&other_domain, &proof).unwrap();
    proof.proof_value = ad4m_hash_chain::encode_proof_value(&master(0).sign(&input));
    other_domain.proof.push(proof);
    case(
        "proof in another domain",
        "3",
        typed(&[first.clone(), other_domain]),
        policy("BadProof"),
    );

    let mut extra = add.clone();
    sign(&mut extra, "d1", &dev(1));
    case(
        "an extra proof by a device key",
        "4",
        typed(&[first.clone(), extra]),
        policy("BadProof"),
    );

    let unknown = build::revoke(&base.chain, &master(0), &["d9".to_string()]).unwrap();
    case(
        "revoke of an unknown key",
        "4",
        typed(&[first.clone(), unknown]),
        policy("UnknownKey"),
    );

    let mut revoked = Log::new();
    revoked.revoke(&["d1"]);
    let again = build::revoke(&revoked.chain, &master(0), &["d1".to_string()]).unwrap();
    let mut events = revoked.events.clone();
    events.push(again);
    case("revoke twice", "4", typed(&events), policy("RevokedKey"));

    let not_bot = build::scope(
        &base.chain,
        &master(0),
        &"d1".to_string(),
        &BotScope {
            can: vec!["read".into()],
            expires: 10,
        },
    )
    .unwrap();
    case(
        "scope on a key without the bot mark",
        "8",
        typed(&[first.clone(), not_bot]),
        policy("NotABot"),
    );

    let mut wrong_salt =
        build::rotate_unsigned(&base.chain, &mk(&master(1)), &salt(2), Some(next(2)), &[]).unwrap();
    sign(&mut wrong_salt, "m1", &master(1));
    case(
        "rotate with a wrong salt",
        "5",
        typed(&[first.clone(), wrong_salt]),
        policy("NextMismatch"),
    );

    let mut ended = Log::new();
    ended.rotate(&[], true);
    assert_eq!(
        build::revoke_unsigned(&ended.chain, &["d1".to_string()]).unwrap_err(),
        build::BuildError::Ended,
        "the builder refuses to build on an ended log"
    );
    let mut late = Event {
        format: ad4m_identity_core::FORMAT.into(),
        scid: ended.chain.id().clone(),
        seq: 2,
        prev: Some(ended.chain.head().hash.clone()),
        op: Op::Revoke {
            revoke: vec!["d1".into()],
        },
        proof: Vec::new(),
    };
    sign(&mut late, "m1", &master(1));
    let mut events = ended.events.clone();
    events.push(late);
    case("event after the end", "9", typed(&events), policy("Ended"));

    let mut second_incept = incept_event(vec![device(1, KeyLife::Standing)]);
    second_incept.seq = 1;
    second_incept.scid = first.scid.clone();
    second_incept.prev = Some(base.chain.head().hash.clone());
    case(
        "incept after #0",
        "2",
        typed(&[first.clone(), second_incept]),
        format("InceptAfterGenesis"),
    );

    cases
}

/// The `{code, reason, format}` part of an error code that vectors pin.
pub fn pinned(code: &Value) -> Value {
    let mut out = serde_json::Map::new();
    for key in ["code", "reason", "format"] {
        if let Some(v) = code.get(key) {
            out.insert(key.into(), v.clone());
        }
    }
    Value::Object(out)
}

pub fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn unhex(text: &str) -> Vec<u8> {
    (0..text.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&text[i..i + 2], 16).unwrap())
        .collect()
}

pub const VECTOR_NAMES: &[&str] = &[
    "log",
    "fold",
    "proofs",
    "commitment",
    "pow",
    "domains",
    "link",
];

/// The fixed entropy of the words vector.
pub const WORDS_ENTROPY: [u8; 16] = [
    0x7f, 0x3a, 0x91, 0x0c, 0x5e, 0x22, 0xd4, 0x86, 0x19, 0xab, 0x40, 0xee, 0x63, 0x08, 0xc7, 0x5d,
];

/// Builds every vector file from the fixtures. Needs the `words` feature for `commitment`.
#[cfg(feature = "words")]
pub fn build_vectors() -> Vec<(&'static str, Value)> {
    vec![
        ("log", build_log()),
        ("fold", build_fold()),
        ("proofs", build_proofs()),
        ("commitment", build_commitment()),
        ("pow", build_pow()),
        ("domains", build_domains()),
        ("link", build_link()),
    ]
}

fn build_log() -> Value {
    let log = vector_log();
    let state = log.chain.state();
    let mut validity = Vec::new();
    for key in ["m0", "m1", "m2", "d1", "d2", "d3", "d4"] {
        for seq in 0..=log.seq() {
            for now in [1_700_000_000u64, 1_850_000_000] {
                validity.push(json!({
                    "key": key, "seq": seq, "now": now,
                    "validAt": state.key_valid_at(key, seq),
                    "validAtTime": state.key_valid_at_time(key, seq, now),
                }));
            }
        }
    }
    json!({
        "description": "A valid log with every op and every key life; masters and salts from the test labels `master n` and `salt n`.",
        "scid": state.scid,
        "did": state.did,
        "events": log.json(),
        "hashes": log.chain.hashes(),
        "state": serde_json::to_value(state).unwrap(),
        "validity": validity,
    })
}

fn build_fold() -> Value {
    let cases: Vec<Value> = refusal_cases()
        .into_iter()
        .map(|c| {
            let got = match fold_json(&c.events) {
                Ok(_) => panic!("{} folds", c.name),
                Err(e) => pinned(&error_code(&e)),
            };
            assert_eq!(got, c.error, "{}", c.name);
            json!({"name": c.name, "rule": c.rule, "events": c.events, "error": c.error})
        })
        .collect();
    json!({"description": "One invalid log per rule; `error` is the pinned part of the ChainErrorCode.", "cases": cases})
}

fn build_proofs() -> Value {
    let log = vector_log();
    let state = log.chain.state();
    let mut cases = Vec::new();
    for event in &log.events {
        for proof in &event.proof {
            let key_id = proof.verification_method.rsplit('#').next().unwrap();
            let key = state.record(key_id).unwrap().sign.clone();
            let mut config = serde_json::to_value(build::proof_config(event, key_id)).unwrap();
            config.as_object_mut().unwrap().remove("proofValue");
            cases.push(json!({
                "name": format!("seq {} by {key_id}", event.seq),
                "event": json(event),
                "keyId": key_id,
                "key": key,
                "proof": proof,
                "signingInput": hex(&build::signing_input(event, key_id).unwrap()),
                "proofConfig": config,
                "configHash": hex(&sha256(ad4m_hash_chain::canonical_json(&config).unwrap().as_bytes())),
                "eventHash": event_hash(event).unwrap(),
                "result": "ok",
            }));
        }
    }
    // A tampered body, a wrong key and another domain.
    let event = &log.events[1];
    let proof = &event.proof[0];
    let mut tampered = event.clone();
    if let Op::Add { offer, .. } = &mut tampered.op {
        *offer = "offer-2".into();
    }
    cases.push(
        json!({"name": "tampered body", "event": json(&tampered), "keyId": "m0",
        "key": mk(&master(0)), "proof": proof, "result": "Signature"}),
    );
    cases.push(
        json!({"name": "wrong key", "event": json(event), "keyId": "m0",
        "key": mk(&master(1)), "proof": proof, "result": "Signature"}),
    );
    let mut linked = proof.clone();
    linked.domain = "coasys:link".into();
    cases.push(
        json!({"name": "other domain", "event": json(event), "keyId": "m0",
        "key": mk(&master(0)), "proof": linked, "result": "Domain"}),
    );
    json!({"description": "eddsa-jcs-2022 proofs of the log vector under the domain coasys:kel.", "cases": cases})
}

#[cfg(feature = "words")]
fn build_commitment() -> Value {
    use ad4m_identity_core::words;
    let w = words::from_entropy(&WORDS_ENTROPY);
    let seed = words::seed(&w);
    let masters: Vec<Value> = (0..4u32)
        .map(|n| {
            let key = words::master_key(&seed, n).unwrap();
            let (c, s) = words::next_commitment(&seed, n).unwrap();
            json!({"n": n, "path": format!("m/7133'/0'/{n}'"), "key": mk(&key),
                   "salt": s, "saltHex": hex(&words::salt(&seed, n)), "commitment": c})
        })
        .collect();
    let recovery = x25519_dalek::PublicKey::from(&words::recovery_secret(&seed));
    json!({
        "description": "BIP-39 words (empty passphrase), SLIP-0010 Ed25519 masters at m/7133'/0'/n', HKDF salts and commitments, the recovery key.",
        "entropy": hex(&WORDS_ENTROPY),
        "words": w.phrase(),
        "seed": hex(seed.as_bytes()),
        "masters": masters,
        "recoveryPublic": encode_x25519_multikey(recovery.as_bytes()),
    })
}

fn build_pow() -> Value {
    let scid = vector_log().chain.state().scid.clone();
    let cases: Vec<Value> = [8u8, 12, 16]
        .iter()
        .map(|&bits| {
            let p = pow::solve(&scid, bits);
            let nonce: [u8; 16] = bs58::decode(&p.nonce)
                .into_vec()
                .unwrap()
                .try_into()
                .unwrap();
            let hash = pow::pow_hash(&scid, &nonce);
            json!({"scid": scid, "bits": bits, "pow": p, "hash": hex(&hash),
                   "zeroBits": pow::leading_zero_bits(&hash)})
        })
        .collect();
    json!({"description": "sha256(\"coasys:pow\" ‖ scid ‖ nonce) with at least `bits` leading zero bits; the solver counts nonces from 0 as 16 big-endian bytes.", "cases": cases})
}

fn domain_value(domain: &str) -> Value {
    json!({"subject": domain, "seq": 3, "nested": {"b": [1, 2], "a": "ü"}})
}

fn build_domains() -> Value {
    let key = sk("domain signer");
    let cases: Vec<Value> = SIGNING_DOMAINS
        .iter()
        .filter(|d| d.name != KEL_DOMAIN)
        .map(|d| {
            let value = domain_value(d.name);
            let bytes = signing_bytes(d.name, &value).unwrap();
            let sig = build::sign_domain(d.name, &value, &key).unwrap();
            json!({"domain": d.name, "value": value, "key": mk(&key),
                   "signingBytes": String::from_utf8(bytes).unwrap(), "signature": hex(&sig)})
        })
        .collect();
    let refused: Vec<Value> = LABELS
        .iter()
        .map(|l| json!({"domain": l, "error": "Label"}))
        .chain([
            json!({"domain": KEL_DOMAIN, "error": "Scheme"}),
            json!({"domain": "coasys:lang-key", "error": "Unknown"}),
        ])
        .collect();
    json!({
        "description": "The domain-field scheme: Ed25519 over JCS(value + {\"domain\": d}). A signature verifies only under its own domain; labels and coasys:kel are refused.",
        "domains": SIGNING_DOMAINS.iter().map(|d| json!({"name": d.name, "owner": d.owner,
            "scheme": if d.name == KEL_DOMAIN { "data-integrity" } else { "domain-field" }})).collect::<Vec<_>>(),
        "labels": LABELS,
        "cases": cases,
        "refused": refused,
    })
}

/// Checks one vector file against the core.
pub fn check_vector_file(name: &str, file: &Value) {
    match name {
        "log" => check_log(file),
        "fold" => check_fold(file),
        "proofs" => check_proofs(file),
        "commitment" => check_commitment(file),
        "pow" => check_pow(file),
        "domains" => check_domains(file),
        "link" => check_link(file),
        other => panic!("unknown vector file {other}"),
    }
}

fn check_log(file: &Value) {
    let events: Vec<Value> = serde_json::from_value(file["events"].clone()).unwrap();
    let chain = fold_json(&events).unwrap();
    let typed: Vec<Event> = events
        .iter()
        .map(|v| serde_json::from_value(v.clone()).unwrap())
        .collect();
    assert_eq!(derive_scid(&typed[0]).unwrap(), file["scid"]);
    for (event, hash) in typed.iter().zip(file["hashes"].as_array().unwrap()) {
        assert_eq!(&event_hash(event).unwrap(), hash);
    }
    assert_eq!(
        serde_json::to_value(chain.hashes()).unwrap(),
        file["hashes"]
    );
    let state = chain.state();
    assert_eq!(state.did, file["did"]);
    assert_eq!(serde_json::to_value(state).unwrap(), file["state"]);
    for row in file["validity"].as_array().unwrap() {
        let key = row["key"].as_str().unwrap();
        let seq = row["seq"].as_u64().unwrap();
        let now = row["now"].as_u64().unwrap();
        assert_eq!(state.key_valid_at(key, seq), row["validAt"], "{row}");
        assert_eq!(
            state.key_valid_at_time(key, seq, now),
            row["validAtTime"],
            "{row}"
        );
    }
}

fn check_fold(file: &Value) {
    for case in file["cases"].as_array().unwrap() {
        let events: Vec<Value> = serde_json::from_value(case["events"].clone()).unwrap();
        let err = fold_json(&events)
            .err()
            .unwrap_or_else(|| panic!("{} folds", case["name"]));
        assert_eq!(pinned(&error_code(&err)), case["error"], "{}", case["name"]);
    }
}

fn check_proofs(file: &Value) {
    for case in file["cases"].as_array().unwrap() {
        let event: Event = serde_json::from_value(case["event"].clone()).unwrap();
        let proof: ad4m_identity_core::Proof =
            serde_json::from_value(case["proof"].clone()).unwrap();
        let key = case["key"].as_str().unwrap().to_string();
        let got = verify_proof(&event, &proof, &key, KEL_DOMAIN);
        match case["result"].as_str().unwrap() {
            "ok" => {
                got.unwrap_or_else(|e| panic!("{}: {e}", case["name"]));
                let key_id = case["keyId"].as_str().unwrap();
                assert_eq!(
                    hex(&build::signing_input(&event, key_id).unwrap()),
                    case["signingInput"]
                );
                assert_eq!(event_hash(&event).unwrap(), case["eventHash"]);
                let input = unhex(case["signingInput"].as_str().unwrap());
                assert_eq!(hex(&input[..32]), case["configHash"]);
                let config_jcs = ad4m_hash_chain::canonical_json(&case["proofConfig"]).unwrap();
                assert_eq!(hex(&sha256(config_jcs.as_bytes())), case["configHash"]);
                let mut body = case["event"].clone();
                body.as_object_mut().unwrap().remove("proof");
                let body_jcs = ad4m_hash_chain::canonical_json(&body).unwrap();
                assert_eq!(hex(&input[32..]), hex(&sha256(body_jcs.as_bytes())));
            }
            "Signature" => assert_eq!(
                got,
                Err(ad4m_hash_chain::ProofError::Signature),
                "{}",
                case["name"]
            ),
            "Domain" => assert!(
                matches!(got, Err(ad4m_hash_chain::ProofError::Domain { .. })),
                "{}",
                case["name"]
            ),
            other => panic!("unknown result {other}"),
        }
    }
}

fn check_commitment(file: &Value) {
    for m in file["masters"].as_array().unwrap() {
        assert_eq!(
            commitment(m["key"].as_str().unwrap(), m["salt"].as_str().unwrap()),
            m["commitment"]
        );
        assert_eq!(
            bs58::encode(unhex(m["saltHex"].as_str().unwrap())).into_string(),
            m["salt"]
        );
    }
    #[cfg(feature = "words")]
    {
        use ad4m_identity_core::words;
        let w = words::parse(file["words"].as_str().unwrap()).unwrap();
        let entropy: [u8; 16] = unhex(file["entropy"].as_str().unwrap()).try_into().unwrap();
        assert_eq!(words::from_entropy(&entropy).phrase(), w.phrase());
        let seed = words::seed(&w);
        assert_eq!(hex(seed.as_bytes()), file["seed"]);
        for m in file["masters"].as_array().unwrap() {
            let n = m["n"].as_u64().unwrap() as u32;
            assert_eq!(mk(&words::master_key(&seed, n).unwrap()), m["key"]);
            let (c, s) = words::next_commitment(&seed, n).unwrap();
            assert_eq!(c, m["commitment"]);
            assert_eq!(s, m["salt"]);
        }
        let recovery = x25519_dalek::PublicKey::from(&words::recovery_secret(&seed));
        assert_eq!(
            encode_x25519_multikey(recovery.as_bytes()),
            file["recoveryPublic"]
        );
    }
}

fn check_pow(file: &Value) {
    for case in file["cases"].as_array().unwrap() {
        let scid = case["scid"].as_str().unwrap();
        let bits = case["bits"].as_u64().unwrap() as u8;
        let p: pow::Pow = serde_json::from_value(case["pow"].clone()).unwrap();
        assert!(pow::check(scid, &p, bits));
        assert_eq!(pow::solve(scid, bits), p);
        let nonce: [u8; 16] = bs58::decode(&p.nonce)
            .into_vec()
            .unwrap()
            .try_into()
            .unwrap();
        let hash = pow::pow_hash(scid, &nonce);
        assert_eq!(hex(&hash), case["hash"]);
        let zeros = pow::leading_zero_bits(&hash);
        assert_eq!(zeros, case["zeroBits"].as_u64().unwrap() as u32);
        assert!(!pow::check(scid, &p, (zeros + 1) as u8));
    }
}

fn check_domains(file: &Value) {
    let names: Vec<&str> = SIGNING_DOMAINS.iter().map(|d| d.name).collect();
    let listed: Vec<&str> = file["domains"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["name"].as_str().unwrap())
        .collect();
    assert_eq!(names, listed);
    assert_eq!(serde_json::to_value(LABELS).unwrap(), file["labels"]);
    for case in file["cases"].as_array().unwrap() {
        let domain = case["domain"].as_str().unwrap();
        let value = &case["value"];
        let key = case["key"].as_str().unwrap();
        let sig = unhex(case["signature"].as_str().unwrap());
        assert_eq!(
            String::from_utf8(signing_bytes(domain, value).unwrap()).unwrap(),
            case["signingBytes"]
        );
        for other in &names {
            assert_eq!(
                verify_signed(other, value, key, &sig),
                *other == domain,
                "{domain} signature under {other}"
            );
        }
    }
    for refusal in file["refused"].as_array().unwrap() {
        let domain = refusal["domain"].as_str().unwrap();
        let err = signing_bytes(domain, &json!({"a": 1})).unwrap_err();
        let name = match err {
            ad4m_identity_core::DomainError::Label(_) => "Label",
            ad4m_identity_core::DomainError::Scheme(_) => "Scheme",
            ad4m_identity_core::DomainError::Unknown(_) => "Unknown",
            _ => "other",
        };
        assert_eq!(name, refusal["error"], "{domain}");
    }
}

// ----- Offers and claims -----

/// The fixed offer secret `s`.
pub fn link_secret() -> [u8; 32] {
    sha256(b"offer secret")
}

/// The fixed offer id: 16 bytes, base58btc.
pub fn link_offer_id() -> String {
    bs58::encode(&sha256(b"offer id")[..16]).into_string()
}

/// The time the link fixtures are issued at.
pub const LINK_NOW: u64 = 1_700_000_000;

/// An offer of `kind` on `log`'s identity, issued at [`LINK_NOW`], expiring 300 s later.
pub fn link_offer(log: &Log, kind: OfferKind, bot: Option<BotScope>) -> Offer {
    Offer {
        scid: log.chain.state().scid.clone(),
        offer_id: link_offer_id(),
        kind,
        commit: offer_commit(&link_secret()),
        expires: LINK_NOW + OFFER_TTL_S,
        device_key: None,
        bot,
    }
}

/// The position of the current master at the head of `log`.
pub fn signed_by(log: &Log) -> SignedBy {
    let head = log.chain.head();
    SignedBy {
        key_id: log.chain.state().current_master().unwrap().id.clone(),
        kel_seq: head.seq,
        kel_hash: head.hash,
    }
}

/// `offer` signed by `log`'s current master at its head.
pub fn signed_offer(log: &Log, offer: Offer) -> SignedOffer {
    sign_offer(offer, signed_by(log), &master(log.n)).unwrap()
}

/// The claim of device key `d<n>` on the fixed offer.
pub fn link_claim(log: &Log, n: u32, can_keep_master: bool) -> Claim {
    make_claim(
        &log.chain.state().scid,
        &link_offer_id(),
        link_secret(),
        &dev(n),
        enc(&format!("d{n}")),
        can_keep_master,
        Some("Pixel 8".into()),
    )
    .unwrap()
}

fn build_link() -> Value {
    let log = Log::new();
    let scid = log.chain.state().scid.clone();
    let s = link_secret();
    let offer = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    let claim = link_claim(&log, 2, true);
    let view = claim_view(&claim, LINK_NOW + 10);
    let add = build::add_claimed(&log.chain, &master(0), &offer.offer, &view, None).unwrap();
    let bot_scope = BotScope {
        can: vec!["read".into()],
        expires: 1_800_000_000,
    };
    let bot_offer = signed_offer(&log, link_offer(&log, OfferKind::Bot, Some(bot_scope)));
    let bot_add =
        build::add_claimed(&log.chain, &master(0), &bot_offer.offer, &view, None).unwrap();
    let offer_string = OfferString {
        store: "https://store.coasys.org".into(),
        scid: scid.clone(),
        offer_id: link_offer_id(),
        s: bs58::encode(s).into_string(),
    };
    let device_string = DeviceString {
        sign: mk(&dev(2)),
        enc: enc("d2"),
        r: bs58::encode(&sha256(b"device r")[..16]).into_string(),
        exp: LINK_NOW + 300,
        hint: Some("Pixel 8".into()),
        store: "https://store.coasys.org".into(),
    };
    let claim_bytes = signing_bytes(CLAIM_DOMAIN, &claim_body(&scid, &claim)).unwrap();
    json!({
        "description": "Offers and claims: commit = multihash(sha256(\"coasys:offer\" || s)); the offer signature is Ed25519 over signing_bytes(\"coasys:offer\", {offer, by}); the claim co-signature over signing_bytes(\"coasys:claim\", ClaimBody); the match code is base32(sha256(JCS({scid, offerId, deviceKey, domain: \"coasys:match\"})))[0..6] in two groups of three; offer and device strings are prefix + base58btc(JCS(value)).",
        "log": log.json(),
        "now": LINK_NOW,
        "commit": {"s": bs58::encode(s).into_string(), "commit": offer_commit(&s)},
        "signedOffer": offer,
        "offerSigningBytes": String::from_utf8(offer_signing_bytes(&offer.offer, &offer.by).unwrap()).unwrap(),
        "masterKey": mk(&master(0)),
        "claim": claim,
        "claimSigningBytes": String::from_utf8(claim_bytes).unwrap(),
        "claimView": view,
        "match": {
            "scid": scid,
            "offerId": link_offer_id(),
            "deviceKey": mk(&dev(2)),
            "input": String::from_utf8(match_input(&scid, &link_offer_id(), &mk(&dev(2)))).unwrap(),
            "code": match_code(&scid, &link_offer_id(), &mk(&dev(2))),
        },
        "addClaimed": json(&add),
        "botOffer": bot_offer,
        "botAddClaimed": json(&bot_add),
        "offerString": {"value": offer_string, "string": offer_string.encode(), "link": offer_string.link()},
        "deviceString": {"value": device_string, "string": device_string.encode()},
    })
}

fn check_link(file: &Value) {
    let events: Vec<Value> = serde_json::from_value(file["log"].clone()).unwrap();
    let chain = fold_json(&events).unwrap();
    let now = file["now"].as_u64().unwrap();
    let s = decode_secret(file["commit"]["s"].as_str().unwrap()).unwrap();
    assert_eq!(offer_commit(&s), file["commit"]["commit"].as_str().unwrap());

    let offer: SignedOffer = serde_json::from_value(file["signedOffer"].clone()).unwrap();
    let bytes = offer_signing_bytes(&offer.offer, &offer.by).unwrap();
    assert_eq!(
        bytes,
        file["offerSigningBytes"].as_str().unwrap().as_bytes()
    );
    assert_eq!(
        chain.state().current_master().unwrap().sign,
        file["masterKey"].as_str().unwrap()
    );
    verify_offer(&offer, &chain, now).unwrap();

    let claim: Claim = serde_json::from_value(file["claim"].clone()).unwrap();
    let claim_bytes =
        signing_bytes(CLAIM_DOMAIN, &claim_body(&chain.state().scid, &claim)).unwrap();
    assert_eq!(
        claim_bytes,
        file["claimSigningBytes"].as_str().unwrap().as_bytes()
    );
    verify_claim(&chain.state().scid, &claim).unwrap();
    assert!(check_secret(&offer.offer, &claim));
    let view: ClaimView = serde_json::from_value(file["claimView"].clone()).unwrap();
    assert_eq!(view, claim_view(&claim, view.claimed_at));

    let m = &file["match"];
    let (scid, offer_id, key) = (
        m["scid"].as_str().unwrap(),
        m["offerId"].as_str().unwrap(),
        m["deviceKey"].as_str().unwrap(),
    );
    assert_eq!(
        match_input(scid, offer_id, key),
        m["input"].as_str().unwrap().as_bytes()
    );
    assert_eq!(match_code(scid, offer_id, key), m["code"].as_str().unwrap());

    for (offer_key, add_key) in [("signedOffer", "addClaimed"), ("botOffer", "botAddClaimed")] {
        let offer: SignedOffer = serde_json::from_value(file[offer_key].clone()).unwrap();
        let add = Event::from_json(&file[add_key]).unwrap();
        let mut extended = chain.clone();
        extended.append(&KelPolicy, &add).unwrap();
        check_add(&offer, &view, chain.state(), &add, now).unwrap();
    }

    let string: OfferString = serde_json::from_value(file["offerString"]["value"].clone()).unwrap();
    assert_eq!(
        string.encode(),
        file["offerString"]["string"].as_str().unwrap()
    );
    assert_eq!(string.link(), file["offerString"]["link"].as_str().unwrap());
    assert_eq!(OfferString::parse(&string.encode()).unwrap(), string);
    assert_eq!(OfferString::parse_link(&string.link()).unwrap(), string);
    let device: DeviceString =
        serde_json::from_value(file["deviceString"]["value"].clone()).unwrap();
    assert_eq!(
        device.encode(),
        file["deviceString"]["string"].as_str().unwrap()
    );
    assert_eq!(DeviceString::parse(&device.encode()).unwrap(), device);
}

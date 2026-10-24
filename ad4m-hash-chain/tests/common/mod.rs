//! The test chain type and the shared-vector builders and checkers.
//!
//! Test chain type `test:chain` (domain `test:chain`): every entry names its chain in
//! `chain` and carries a free `body`. Entry 0 names the signing key in `key`. Every entry
//! carries exactly one proof, by the key in force before it (entry 0: its own `key`). A
//! later entry may name a new `key`; it takes effect after that entry. The state is the key
//! in force and the number of entries after entry 0.
#![allow(dead_code)]

use ad4m_hash_chain::{
    canonical_json, compare, encode_ed25519_multikey, encode_proof_value, entry_hash,
    entry_hash_json, fold, import, sha256, signing_input, verify_proof, verify_proof_json,
    CanonError, Chain, ChainError, ChainId, ChainPolicy, Entry, Hash, Multikey, Proof, ProofError,
};
use ed25519_dalek::{Signer, SigningKey};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::fmt;

pub const DOMAIN: &str = "test:chain";

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TestEntry {
    pub chain: String,
    pub seq: u64,
    pub prev: Option<Hash>,
    pub body: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub key: Option<Multikey>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub proof: Vec<Proof>,
}

impl Entry for TestEntry {
    fn seq(&self) -> u64 {
        self.seq
    }
    fn prev(&self) -> Option<&Hash> {
        self.prev.as_ref()
    }
    fn proofs(&self) -> &[Proof] {
        &self.proof
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TestState {
    pub key: Multikey,
    pub count: u64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TestError {
    NoKey,
    MissingProof,
    BadProof(ProofError),
}

impl fmt::Display for TestError {
    // Only the name: the vectors compare it across implementations.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TestError::NoKey => write!(f, "NoKey"),
            TestError::MissingProof => write!(f, "MissingProof"),
            TestError::BadProof(_) => write!(f, "BadProof"),
        }
    }
}

impl std::error::Error for TestError {}

pub struct TestPolicy;

fn one_proof(entry: &TestEntry, key: &Multikey) -> Result<(), TestError> {
    match entry.proofs() {
        [proof] => verify_proof(entry, proof, key, DOMAIN).map_err(TestError::BadProof),
        _ => Err(TestError::MissingProof),
    }
}

impl ChainPolicy for TestPolicy {
    type Entry = TestEntry;
    type State = TestState;
    type Error = TestError;
    const TYPE: &'static str = "test:chain";
    const DOMAIN: &'static str = DOMAIN;

    fn genesis(&self, entry: &TestEntry) -> Result<(ChainId, TestState), TestError> {
        let key = entry.key.clone().ok_or(TestError::NoKey)?;
        one_proof(entry, &key)?;
        Ok((entry.chain.clone(), TestState { key, count: 0 }))
    }

    fn chain_of(&self, entry: &TestEntry) -> ChainId {
        entry.chain.clone()
    }

    fn apply(&self, state: &mut TestState, entry: &TestEntry) -> Result<(), TestError> {
        one_proof(entry, &state.key)?;
        if let Some(key) = &entry.key {
            state.key = key.clone();
        }
        state.count += 1;
        Ok(())
    }
}

pub type TestChain = Chain<TestPolicy>;

pub fn signing_key(seed: u8) -> SigningKey {
    SigningKey::from_bytes(&[seed; 32])
}

pub fn multikey(key: &SigningKey) -> Multikey {
    encode_ed25519_multikey(&key.verifying_key().to_bytes())
}

/// Replaces the proofs of `entry` with one proof by `key` under `domain`.
pub fn sign(mut entry: TestEntry, key: &SigningKey, domain: &str) -> TestEntry {
    entry.proof.clear();
    let mut proof = Proof::unsigned(
        &format!("test:{}#{}", entry.chain, multikey(key)),
        "assertionMethod",
        domain,
    );
    let input = signing_input(&entry, &proof).unwrap();
    proof.proof_value = encode_proof_value(&key.sign(&input).to_bytes());
    entry.proof.push(proof);
    entry
}

pub fn genesis_entry(chain: &str, key: &SigningKey) -> TestEntry {
    sign(
        TestEntry {
            chain: chain.into(),
            seq: 0,
            prev: None,
            body: "genesis".into(),
            key: Some(multikey(key)),
            proof: vec![],
        },
        key,
        DOMAIN,
    )
}

/// The signed entry after `prev`, by `key`, optionally rotating to `rotate`.
pub fn next_entry(
    prev: &TestEntry,
    body: &str,
    key: &SigningKey,
    rotate: Option<&SigningKey>,
) -> TestEntry {
    sign(
        TestEntry {
            chain: prev.chain.clone(),
            seq: prev.seq + 1,
            prev: Some(entry_hash(prev).unwrap()),
            body: body.into(),
            key: rotate.map(multikey),
            proof: vec![],
        },
        key,
        DOMAIN,
    )
}

/// A valid chain of `len` entries signed by the key of `seed`.
pub fn make_chain(chain: &str, seed: u8, len: usize) -> Vec<TestEntry> {
    let key = signing_key(seed);
    let mut entries = vec![genesis_entry(chain, &key)];
    while entries.len() < len {
        let body = format!("entry {}", entries.len());
        let next = next_entry(entries.last().unwrap(), &body, &key, None);
        entries.push(next);
    }
    entries
}

/// A valid chain whose key rotates at each seq in `rotate_at` (seeds `seed`, `seed+1`, …),
/// with entry bodies from `bodies`.
pub fn make_rotating_chain(
    chain: &str,
    seed: u8,
    bodies: &[String],
    rotate_at: &[bool],
) -> Vec<TestEntry> {
    let mut key = signing_key(seed);
    let mut next_seed = seed.wrapping_add(1);
    let mut entries = vec![genesis_entry(chain, &key)];
    for (body, rotate) in bodies.iter().zip(rotate_at) {
        let new_key = rotate.then(|| signing_key(next_seed));
        let next = next_entry(entries.last().unwrap(), body, &key, new_key.as_ref());
        if let Some(k) = new_key {
            key = k;
            next_seed = next_seed.wrapping_add(1);
        }
        entries.push(next);
    }
    entries
}

pub fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

// ---------------------------------------------------------------- vectors

pub const VECTOR_NAMES: &[&str] = &[
    "entry-hash",
    "signing-input",
    "proof-check",
    "fold",
    "import",
    "compare",
];

fn to_json<T: Serialize>(value: &T) -> Value {
    serde_json::to_value(value).unwrap()
}

fn entries_json(entries: &[TestEntry]) -> Value {
    to_json(&entries)
}

fn fold_result(entries: &[TestEntry]) -> Value {
    match fold(&TestPolicy, entries) {
        Ok(chain) => json!({"ok": chain_json(&chain)}),
        Err(e) => json!({"error": e.code()}),
    }
}

pub fn chain_json(chain: &TestChain) -> Value {
    json!({
        "chain": chain.id(),
        "head": chain.head(),
        "hashes": chain.hashes(),
        "state": {"key": chain.state().key, "count": chain.state().count},
    })
}

fn proof_result(r: Result<(), ProofError>) -> &'static str {
    match r {
        Ok(()) => "ok",
        Err(ProofError::Domain { .. }) => "domain",
        Err(ProofError::Suite(_)) => "suite",
        Err(ProofError::Encoding) => "encoding",
        Err(ProofError::Signature) => "signature",
    }
}

fn import_result(base: &[TestEntry], batch: &[TestEntry]) -> Value {
    let mut chain = fold(&TestPolicy, base).expect("import vector base folds");
    match import(&mut chain, &TestPolicy, batch) {
        Ok(outcome) => json!({"outcome": outcome, "after": chain_json(&chain)}),
        Err(e) => json!({"error": e.code(), "after": chain_json(&chain)}),
    }
}

fn entry_hash_cases() -> Value {
    let key = signing_key(1);
    let signed = genesis_entry("vector-chain", &key);
    let mut unsigned = signed.clone();
    unsigned.proof.clear();
    let inputs: Vec<(&str, Value)> = vec![
        ("test entry 0 with its proof", to_json(&signed)),
        (
            "the same entry without proof: same hash",
            to_json(&unsigned),
        ),
        (
            "member order and nesting (JCS sorts by UTF-16 code units)",
            json!({"seq": 1, "prev": null, "z": {"b": [3, 2, 1], "a": true},
                   "\u{20ac}": 1, "\r": 2, "\u{fb33}": 3, "1": 4, "\u{1f600}": 5, "\u{80}": 6, "\u{f6}": 7}),
        ),
        (
            "string escapes",
            json!({"seq": 0, "prev": null, "s": "\u{20ac}$\u{f}\nA'B\"\\\\\"/\u{8}\u{c}\t\r\u{1f}\u{7f}\u{2028}"}),
        ),
        (
            "largest safe integers",
            json!({"seq": 9007199254740991u64, "prev": null, "n": -9007199254740991i64}),
        ),
        (
            "a nested proof member enters the hash",
            json!({"seq": 0, "prev": null, "x": {"proof": 1}}),
        ),
        ("float refused", json!({"seq": 0, "prev": null, "f": 1.5})),
        (
            "integer 2^53 refused",
            json!({"seq": 9007199254740992u64, "prev": null}),
        ),
        ("not an object", json!([1, 2])),
    ];
    let cases: Vec<Value> = inputs
        .into_iter()
        .map(|(name, entry)| entry_hash_case(name, &entry))
        .collect();
    json!({
        "description": "entryHash = base58btc(0x12 0x20 ‖ sha256(JCS(entry without its top-level \"proof\" member))). \"jcs\" is the canonical text that is hashed. \"error\": \"malformed\" means the entry has no canonical form.",
        "cases": cases,
    })
}

/// The eddsa-jcs-2022 signing input over an entry given as JSON, built from the spec text
/// (the vectors hold entries that no Rust entry type parses, such as one with `@context`):
/// `sha256(JCS(proof without proofValue, plus the entry's @context)) || sha256(JCS(entry
/// without "proof"))`.
fn signing_input_json(entry: &Value, proof: &Proof) -> Result<Vec<u8>, CanonError> {
    let mut body = entry.as_object().ok_or(CanonError::NotObject)?.clone();
    let mut config = to_json(proof);
    let config_map = config.as_object_mut().unwrap();
    config_map.remove("proofValue");
    if let Some(context) = body.get("@context") {
        config_map.insert("@context".into(), context.clone());
    }
    body.remove("proof");
    let mut input = Vec::with_capacity(64);
    input.extend_from_slice(&sha256(canonical_json(&config)?.as_bytes()));
    input.extend_from_slice(&sha256(canonical_json(&Value::Object(body))?.as_bytes()));
    Ok(input)
}

fn entry_hash_case(name: &str, entry: &Value) -> Value {
    match entry_hash_json(entry) {
        Ok(hash) => {
            let mut body = entry.as_object().unwrap().clone();
            body.remove("proof");
            json!({"name": name, "entry": entry,
                   "jcs": ad4m_hash_chain::canonical_json(&Value::Object(body)).unwrap(),
                   "hash": hash})
        }
        Err(_) => json!({"name": name, "entry": entry, "error": "malformed"}),
    }
}

fn signing_input_cases() -> Value {
    let entries = make_chain("vector-chain", 2, 2);
    let proof = Proof::unsigned("test:vector-chain#k", "assertionMethod", DOMAIN);
    let with_context = json!({"@context": ["https://www.w3.org/ns/credentials/v2"], "seq": 0, "prev": null, "body": "x"});
    let cases = vec![
        (
            "entry 1 of a test chain",
            to_json(&entries[1]),
            proof.clone(),
        ),
        (
            "entry 0 of a test chain",
            to_json(&entries[0]),
            proof.clone(),
        ),
        (
            "an entry with @context: the proof config gets it",
            with_context,
            proof.clone(),
        ),
        (
            "another domain changes the proof-config half",
            to_json(&entries[1]),
            Proof::unsigned("test:vector-chain#k", "assertionMethod", "other:domain"),
        ),
    ];
    let cases: Vec<Value> = cases
        .into_iter()
        .map(|(name, entry, proof)| {
            let input = signing_input_json(&entry, &proof).unwrap();
            json!({"name": name, "entry": entry, "proof": proof, "signingInput": hex(&input)})
        })
        .collect();
    json!({
        "description": "eddsa-jcs-2022 signing input = sha256(JCS(proof without proofValue, plus the entry's @context if any)) ‖ sha256(JCS(entry without \"proof\")), hex. proofValue of the given proof is ignored.",
        "cases": cases,
    })
}

fn proof_check_cases() -> Value {
    let key = signing_key(3);
    let other = signing_key(4);
    let entries = make_chain("vector-chain", 3, 2);
    let entry = entries[1].clone();
    let good = entry.proof[0].clone();
    let mk = multikey(&key);

    let mut flipped = good.clone();
    let mut sig = ad4m_hash_chain::decode_proof_value(&good.proof_value).unwrap();
    sig[10] ^= 1;
    flipped.proof_value = encode_proof_value(&sig);

    let mut tampered = entry.clone();
    tampered.body.push('!');

    let other_domain = sign(entry.clone(), &key, "other:domain");
    let mut suite = good.clone();
    suite.cryptosuite = "ecdsa-jcs-2019".into();
    let mut short = good.clone();
    short.proof_value = "z3FX".into();

    let mut x25519 = vec![0xec, 0x01];
    x25519.extend_from_slice(&[5u8; 32]);
    let x25519 = format!("z{}", bs58::encode(&x25519).into_string());

    let cases: Vec<(&str, Value, Proof, String)> = vec![
        ("valid proof", to_json(&entry), good.clone(), mk.clone()),
        ("changed proofValue", to_json(&entry), flipped, mk.clone()),
        ("changed body", to_json(&tampered), good.clone(), mk.clone()),
        (
            "another key",
            to_json(&entry),
            good.clone(),
            multikey(&other),
        ),
        (
            "signed under another domain",
            to_json(&other_domain),
            other_domain.proof[0].clone(),
            mk.clone(),
        ),
        ("another cryptosuite", to_json(&entry), suite, mk.clone()),
        (
            "proofValue not 64 bytes",
            to_json(&entry),
            short,
            mk.clone(),
        ),
        ("an X25519 multikey", to_json(&entry), good, x25519),
    ];
    let cases: Vec<Value> = cases
        .into_iter()
        .map(|(name, entry, proof, key)| {
            let result = proof_result(verify_proof_json(&entry, &proof, &key, DOMAIN));
            json!({"name": name, "entry": entry, "proof": proof, "key": key, "domain": DOMAIN, "result": result})
        })
        .collect();
    json!({
        "description": "verifyProof(entry, proof, key, domain). result: ok | domain | suite | encoding | signature.",
        "cases": cases,
    })
}

const POLICY_TEXT: &str = "Test chain type test:chain, domain test:chain. Entries {chain, seq, prev, body, key?, proof}. Entry 0 names the signing key in key; every entry carries exactly one eddsa-jcs-2022 proof by the key in force before it (entry 0: its own key); a later entry with key rotates to that key after it. Chain id = chain. State {key, count}: the key in force and the number of entries after entry 0. Policy errors: NoKey, MissingProof, BadProof.";

fn fold_cases() -> Value {
    let key = signing_key(5);
    let valid = make_chain("vector-chain", 5, 4);
    let bodies: Vec<String> = (1..5).map(|i| format!("entry {i}")).collect();
    let rotating = make_rotating_chain("vector-chain", 6, &bodies, &[false, true, false, true]);

    let mut wrong_seq = valid.clone();
    wrong_seq[2].seq = 3;
    wrong_seq[2] = sign(wrong_seq[2].clone(), &key, DOMAIN);

    let mut wrong_prev = valid.clone();
    wrong_prev[2].prev = Some(entry_hash(&valid[0]).unwrap());
    wrong_prev[2] = sign(wrong_prev[2].clone(), &key, DOMAIN);

    let mut other_chain = valid.clone();
    other_chain[1].chain = "another-chain".into();
    other_chain[1] = sign(other_chain[1].clone(), &key, DOMAIN);

    let mut tampered_resigned = valid.clone();
    tampered_resigned[1].body = "entry 1!".into();
    tampered_resigned[1] = sign(tampered_resigned[1].clone(), &key, DOMAIN);

    let mut tampered = valid.clone();
    tampered[1].body = "entry 1!".into();

    let mut unsigned = valid.clone();
    unsigned[2].proof.clear();

    let mut no_key = valid[..1].to_vec();
    no_key[0].key = None;
    no_key[0] = sign(no_key[0].clone(), &key, DOMAIN);

    let mut wrong_signer = valid[..2].to_vec();
    wrong_signer[1] = sign(wrong_signer[1].clone(), &signing_key(9), DOMAIN);

    let mut other_domain = valid[..2].to_vec();
    other_domain[1] = sign(other_domain[1].clone(), &key, "other:domain");

    // Entry 2 rotates from the key of seed 6 to seed 7; entry 3 signed by the old key fails.
    let mut old_key_after_rotation = rotating[..4].to_vec();
    old_key_after_rotation[3] = sign(old_key_after_rotation[3].clone(), &signing_key(6), DOMAIN);

    let cases: Vec<(&str, Vec<TestEntry>)> = vec![
        ("valid chain of 4", valid.clone()),
        ("valid chain with two rotations", rotating),
        ("genesis only", valid[..1].to_vec()),
        ("empty", vec![]),
        ("entry 0 at seq 1", valid[1..].to_vec()),
        ("wrong seq", wrong_seq),
        ("wrong prev", wrong_prev),
        ("entry of another chain", other_chain),
        (
            "one changed body byte, re-signed: the next entry fails prev",
            tampered_resigned,
        ),
        ("one changed body byte: its own proof fails", tampered),
        ("an entry with no proof", unsigned),
        ("entry 0 names no key", no_key),
        ("signed by another key", wrong_signer),
        ("signed under another domain", other_domain),
        ("old key after a rotation", old_key_after_rotation),
    ];
    let cases: Vec<Value> = cases
        .into_iter()
        .map(|(name, entries)| {
            json!({"name": name, "entries": entries_json(&entries), "result": fold_result(&entries)})
        })
        .collect();
    json!({
        "description": "fold(entries) under the test chain type. result.ok: {chain, head, hashes, state}; result.error: a ChainErrorCode.",
        "policy": POLICY_TEXT,
        "cases": cases,
    })
}

fn import_cases() -> Value {
    let key = signing_key(7);
    let full = make_chain("vector-chain", 7, 6);
    let base = full[..3].to_vec();
    let mut shuffled = full[3..].to_vec();
    shuffled.reverse();
    shuffled.swap(0, 1);

    let fork_entry = next_entry(&full[1], "a different entry 2", &key, None);
    let mut other = full[3].clone();
    other.chain = "another-chain".into();
    let other = sign(other, &key, DOMAIN);
    let mut bad = full[3..5].to_vec();
    bad[1].proof.clear();
    let dup_conflict = vec![
        full[3].clone(),
        next_entry(&full[2], "another 3", &key, None),
    ];

    let cases: Vec<(&str, Vec<TestEntry>)> = vec![
        ("continues the head", full[3..].to_vec()),
        ("shuffled batch imports as the sorted batch", shuffled),
        ("held entries and new ones", full[1..5].to_vec()),
        ("repeated batch", base.clone()),
        ("a shorter peer chain", full[..2].to_vec()),
        ("empty batch", vec![]),
        ("starts past our head", full[4..].to_vec()),
        ("a different entry at a held seq", vec![fork_entry]),
        ("an entry of another chain", vec![other]),
        ("a bad entry in the batch: nothing appends", bad),
        ("two different entries at one new seq", dup_conflict),
    ];
    let cases: Vec<Value> = cases
        .into_iter()
        .map(|(name, batch)| {
            json!({"name": name, "base": entries_json(&base), "batch": entries_json(&batch),
                   "result": import_result(&base, &batch)})
        })
        .collect();
    json!({
        "description": "fold(base), then import(batch). result.outcome: an ImportOutcome, or result.error: a ChainErrorCode; result.after: the chain after the call.",
        "policy": POLICY_TEXT,
        "cases": cases,
    })
}

fn compare_cases() -> Value {
    let h: Vec<Hash> = make_chain("vector-chain", 8, 4)
        .iter()
        .map(|e| entry_hash(e).unwrap())
        .collect();
    let x = entry_hash(&next_entry(
        &make_chain("vector-chain", 8, 2)[1],
        "other",
        &signing_key(8),
        None,
    ))
    .unwrap();
    let cases: Vec<(&str, Vec<Hash>, Vec<Hash>, u64)> = vec![
        ("same", h.clone(), h.clone(), 0),
        ("they extend", h[..2].to_vec(), h.clone(), 0),
        ("we extend", h.clone(), h[..3].to_vec(), 0),
        (
            "fork",
            h.clone(),
            vec![h[0].clone(), h[1].clone(), x.clone()],
            0,
        ),
        ("from 2", h[2..].to_vec(), vec![x], 2),
        ("both empty", vec![], vec![], 5),
    ];
    let cases: Vec<Value> = cases
        .into_iter()
        .map(|(name, ours, theirs, from)| {
            let result = compare(&ours, &theirs, from, &"vector-chain".to_string());
            json!({"name": name, "ours": ours, "theirs": theirs, "from": from, "chain": "vector-chain", "result": result})
        })
        .collect();
    json!({
        "description": "compare(ours, theirs, from, chain): ours[i] and theirs[i] are the hashes at seq from + i.",
        "cases": cases,
    })
}

/// Builds every vector file from the fixed seeds above.
pub fn build_vectors() -> Vec<(&'static str, Value)> {
    vec![
        ("entry-hash", entry_hash_cases()),
        ("signing-input", signing_input_cases()),
        ("proof-check", proof_check_cases()),
        ("fold", fold_cases()),
        ("import", import_cases()),
        ("compare", compare_cases()),
    ]
}

fn entries_of(v: &Value) -> Vec<TestEntry> {
    serde_json::from_value(v.clone()).expect("vector entries parse")
}

/// Recomputes every case of one vector file from its inputs and checks the expected answer.
pub fn check_vector_file(name: &str, file: &Value) {
    let cases = file["cases"].as_array().expect("cases");
    assert!(!cases.is_empty(), "{name}: no cases");
    for case in cases {
        let label = format!("{name} / {}", case["name"]);
        match name {
            "entry-hash" => {
                let got = entry_hash_case(case["name"].as_str().unwrap(), &case["entry"]);
                assert_eq!(&got, case, "{label}");
            }
            "signing-input" => {
                let proof: Proof = serde_json::from_value(case["proof"].clone()).unwrap();
                let input = signing_input_json(&case["entry"], &proof).unwrap();
                assert_eq!(
                    hex(&input),
                    case["signingInput"].as_str().unwrap(),
                    "{label}"
                );
            }
            "proof-check" => {
                let proof: Proof = serde_json::from_value(case["proof"].clone()).unwrap();
                let got = proof_result(verify_proof_json(
                    &case["entry"],
                    &proof,
                    case["key"].as_str().unwrap(),
                    case["domain"].as_str().unwrap(),
                ));
                assert_eq!(got, case["result"].as_str().unwrap(), "{label}");
            }
            "fold" => {
                let entries = entries_of(&case["entries"]);
                assert_eq!(fold_result(&entries), case["result"], "{label}");
            }
            "import" => {
                let base = entries_of(&case["base"]);
                let batch = entries_of(&case["batch"]);
                assert_eq!(import_result(&base, &batch), case["result"], "{label}");
            }
            "compare" => {
                let ours: Vec<Hash> = serde_json::from_value(case["ours"].clone()).unwrap();
                let theirs: Vec<Hash> = serde_json::from_value(case["theirs"].clone()).unwrap();
                let got = compare(
                    &ours,
                    &theirs,
                    case["from"].as_u64().unwrap(),
                    &case["chain"].as_str().unwrap().to_string(),
                );
                assert_eq!(to_json(&got), case["result"], "{label}");
            }
            other => panic!("unknown vector file {other}"),
        }
    }
}

/// `ChainError` equality helper for tests.
pub fn policy_err(e: &ChainError<TestError>) -> Option<&TestError> {
    match e {
        ChainError::Policy(p) => Some(p),
        _ => None,
    }
}

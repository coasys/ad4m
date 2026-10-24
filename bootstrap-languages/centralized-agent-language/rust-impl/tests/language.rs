//! The Rust port's acceptance tests: the same cases as `../tests/language.test.ts`,
//! run natively against a real `identity-store` process on a temporary SQLite file. The host's
//! `httpFetch` is a plain HTTP/1.1 client that records every call; the test person's apps are
//! `identity-store/examples/test_agent.rs`.

use std::cell::RefCell;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::rc::Rc;
use std::sync::OnceLock;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use ad4m_hash_chain::{encode_ed25519_multikey, sha256};
use ad4m_ldk::identity::{AgentRecord, AppendRequest, IdentityErrorCode, SignedRequest};
use ad4m_ldk::prelude::{LanguageError, LanguageResult};
use centralized_agent_language::agents::{self, AgentKey};
use centralized_agent_language::store::{encode_request, Http, HttpReply, Store, REQUEST_HEADER};
use ed25519_dalek::{Signer, SigningKey};
use futures::executor::block_on;
use serde_json::{json, Value};

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..")
}

fn bin(name: &str) -> PathBuf {
    let target = std::env::var("IDENTITY_STORE_TARGET")
        .map(PathBuf::from)
        .unwrap_or_else(|_| root().join("target"));
    target.join("debug").join(name)
}

/// Builds the store and the helper once per test binary (a no-op when up to date).
fn build() {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".into());
        let status = Command::new(cargo)
            .args([
                "build",
                "-q",
                "-p",
                "identity-store",
                "--bins",
                "--examples",
            ])
            .current_dir(root())
            .status()
            .expect("cargo runs");
        assert!(status.success(), "cargo build -p identity-store failed");
    });
}

fn now_s() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

/// A running `identity-store serve`.
struct StoreProcess {
    child: Child,
    url: String,
    key: String,
    dir: tempfile::TempDir,
}

impl StoreProcess {
    fn start() -> StoreProcess {
        build();
        let dir = tempfile::tempdir().unwrap();
        let mut child = Command::new(bin("identity-store"))
            .arg("serve")
            .env("IDENTITY_STORE_DB", dir.path().join("store.sqlite"))
            .env("IDENTITY_STORE_KEY", dir.path().join("store-key"))
            .env("IDENTITY_STORE_LISTEN", "127.0.0.1:0")
            .env("POW_BITS", "8")
            .stdout(Stdio::piped())
            .spawn()
            .expect("identity-store starts");
        let mut line = String::new();
        BufReader::new(child.stdout.take().unwrap())
            .read_line(&mut line)
            .unwrap();
        let mut words = line.split_whitespace();
        let url = words
            .nth(3)
            .expect("the listening line names the URL")
            .to_string();
        let key = words
            .nth(1)
            .expect("the listening line names the key")
            .to_string();
        StoreProcess {
            child,
            url,
            key,
            dir,
        }
    }

    fn db(&self) -> PathBuf {
        self.dir.path().join("store.sqlite")
    }
}

impl Drop for StoreProcess {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// The test person's apps over JSON lines.
struct Apps {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

impl Apps {
    fn start() -> Apps {
        build();
        let mut child = Command::new(bin("examples/test_agent"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("test_agent starts");
        let stdin = child.stdin.take().unwrap();
        let stdout = BufReader::new(child.stdout.take().unwrap());
        Apps {
            child,
            stdin,
            stdout,
        }
    }

    fn run(&mut self, op: &str, fields: Value) -> Value {
        let mut request = fields;
        request["op"] = json!(op);
        writeln!(self.stdin, "{request}").unwrap();
        self.stdin.flush().unwrap();
        let mut line = String::new();
        self.stdout.read_line(&mut line).unwrap();
        let answer: Value = serde_json::from_str(&line).unwrap();
        assert!(answer.get("error").is_none(), "test_agent {op}: {answer}");
        answer["ok"].clone()
    }

    /// A signed request of `name` by `signer` for `method path` with `body` sent as its JCS.
    fn signed(
        &mut self,
        name: &str,
        signer: &str,
        key_id: Option<&str>,
        method: &str,
        path: &str,
        body: Option<&Value>,
    ) -> SignedRequest {
        let body = body
            .map(|b| ad4m_hash_chain::canonical_json(b).unwrap())
            .unwrap_or_default();
        let mut fields = json!({
            "name": name, "signer": signer, "method": method, "path": path,
            "body": body, "expires": now_s() + 120,
        });
        if let Some(id) = key_id {
            fields["keyId"] = json!(id);
        }
        serde_json::from_value(self.run("request", fields)).unwrap()
    }

    fn offer(&mut self, name: &str) -> (Value, String) {
        let answer = self.run(
            "offer",
            json!({ "name": name, "kind": "device", "ttl": 300, "now": now_s() }),
        );
        (
            answer["offer"].clone(),
            answer["s"].as_str().unwrap().to_string(),
        )
    }
}

impl Drop for Apps {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// One call the host's `httpFetch` made.
#[derive(Clone, Debug)]
struct Recorded {
    url: String,
    method: String,
    headers: serde_json::Map<String, Value>,
    body: String,
}

/// The host's `httpFetch`: HTTP/1.1 over a TCP stream, recording each call.
#[derive(Clone, Default)]
struct Host {
    calls: Rc<RefCell<Vec<Recorded>>>,
    url: Rc<RefCell<String>>,
}

fn fetch_blocking(
    url: &str,
    method: &str,
    headers_json: &str,
    body: &str,
) -> Result<HttpReply, String> {
    let rest = url.strip_prefix("http://").ok_or("http only")?;
    let split = rest.find('/').unwrap_or(rest.len());
    let (authority, path) = rest.split_at(split);
    let path = if path.is_empty() { "/" } else { path };
    let mut stream = TcpStream::connect(authority).map_err(|e| e.to_string())?;
    stream
        .set_read_timeout(Some(Duration::from_secs(10)))
        .map_err(|e| e.to_string())?;
    let headers: serde_json::Map<String, Value> = if headers_json.is_empty() {
        serde_json::Map::new()
    } else {
        serde_json::from_str(headers_json).map_err(|e| e.to_string())?
    };
    let mut request = format!(
        "{method} {path} HTTP/1.1\r\nHost: {authority}\r\nConnection: close\r\nContent-Length: {}\r\n",
        body.len()
    );
    for (name, value) in &headers {
        request.push_str(&format!(
            "{name}: {}\r\n",
            value.as_str().unwrap_or_default()
        ));
    }
    request.push_str("\r\n");
    request.push_str(body);
    stream
        .write_all(request.as_bytes())
        .map_err(|e| e.to_string())?;
    let mut raw = Vec::new();
    stream.read_to_end(&mut raw).map_err(|e| e.to_string())?;
    let text = String::from_utf8(raw).map_err(|e| e.to_string())?;
    let (head, rest) = text.split_once("\r\n\r\n").ok_or("no header end")?;
    let status: u16 = head
        .split_whitespace()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .ok_or("no status")?;
    let chunked = head.lines().any(|l| {
        l.to_ascii_lowercase()
            .starts_with("transfer-encoding: chunked")
    });
    let body = if chunked {
        dechunk(rest)?
    } else {
        rest.to_string()
    };
    Ok(HttpReply { status, body })
}

fn dechunk(mut rest: &str) -> Result<String, String> {
    let mut out = String::new();
    loop {
        let (size, after) = rest.split_once("\r\n").ok_or("bad chunk")?;
        let size = usize::from_str_radix(size.trim(), 16).map_err(|e| e.to_string())?;
        if size == 0 {
            return Ok(out);
        }
        out.push_str(after.get(..size).ok_or("short chunk")?);
        rest = after.get(size + 2..).ok_or("short chunk")?;
    }
}

impl Http for Host {
    async fn fetch(
        &self,
        url: &str,
        method: &str,
        headers_json: &str,
        body: &str,
    ) -> Result<HttpReply, String> {
        self.calls.borrow_mut().push(Recorded {
            url: url.to_string(),
            method: method.to_string(),
            headers: serde_json::from_str(headers_json).unwrap_or_default(),
            body: body.to_string(),
        });
        fetch_blocking(url, method, headers_json, body)
    }
}

impl Host {
    fn on(url: &str) -> (Host, Store<Host>) {
        let host = Host::default();
        *host.url.borrow_mut() = url.to_string();
        let settings = host.url.clone();
        // The language's settings JSON, as the executor fills it.
        let store = Store::new(host.clone(), move || {
            centralized_agent_language::store::store_url_from(
                &json!({ "storeUrl": settings.borrow().clone() }).to_string(),
            )
        });
        (host, store)
    }

    fn last(&self) -> Recorded {
        self.calls.borrow().last().unwrap().clone()
    }
}

/// The agent the host runs the language for: an Ed25519 key from a label.
struct TestAgentKey(SigningKey);

impl TestAgentKey {
    fn new(label: &str) -> Self {
        TestAgentKey(SigningKey::from_bytes(&sha256(label.as_bytes())))
    }
}

impl AgentKey for TestAgentKey {
    fn did(&self) -> LanguageResult<String> {
        Ok(format!(
            "did:key:{}",
            encode_ed25519_multikey(self.0.verifying_key().as_bytes())
        ))
    }
    fn sign(&self, bytes: &[u8]) -> LanguageResult<Vec<u8>> {
        Ok(self.0.sign(bytes).to_bytes().to_vec())
    }
    fn now_ms(&self) -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64
    }
}

fn refusal<T: std::fmt::Debug>(result: LanguageResult<T>) -> IdentityErrorCode {
    let error: LanguageError = result.expect_err("the call refuses");
    IdentityErrorCode::from_language_error(&error)
        .unwrap_or_else(|| panic!("no refusal in {error}"))
}

fn append_req(events: Vec<Value>) -> AppendRequest {
    AppendRequest {
        events,
        vouch: None,
        pow: None,
    }
}

/// Publishes a fresh person through the language.
fn person(apps: &mut Apps, store: &Store<Host>, name: &str) -> Value {
    let p = apps.run("person", json!({ "name": name }));
    block_on(store.append(
        p["did"].as_str().unwrap(),
        &append_req(vec![p["incept"].clone()]),
    ))
    .unwrap();
    p
}

/// Links device `label` through the language: offer, claim, add.
fn link(apps: &mut Apps, store: &Store<Host>, name: &str, did: &str, label: &str) {
    let (offer, s) = apps.offer(name);
    let offer_id = offer["offer"]["offerId"].as_str().unwrap().to_string();
    block_on(store.open_offer(&serde_json::from_value(offer.clone()).unwrap())).unwrap();
    let claim = apps.run(
        "claim",
        json!({ "name": name, "device": label, "offerId": offer_id, "s": s }),
    );
    block_on(store.claim_offer(
        did,
        &offer_id,
        &serde_json::from_value(claim.clone()).unwrap(),
    ))
    .unwrap();
    let add = apps.run(
        "add",
        json!({ "name": name, "offer": offer, "claim": claim, "claimedAt": now_s() }),
    );
    block_on(store.append(did, &append_req(vec![add.clone()]))).unwrap();
    apps.run("push", json!({ "name": name, "event": add }));
}

#[test]
fn the_log_calls_answer_and_refuse_as_the_store_does() {
    let server = StoreProcess::start();
    let mut apps = Apps::start();
    let (_host, store) = Host::on(&server.url);
    let alice = apps.run("person", json!({ "name": "alice log" }));
    let did = alice["did"].as_str().unwrap();

    assert!(block_on(store.head(did)).unwrap().is_none());
    assert!(block_on(store.log(did, 0)).unwrap().is_none());
    let head = block_on(store.append(did, &append_req(vec![alice["incept"].clone()]))).unwrap();
    assert_eq!(head.seq, 0);
    assert_eq!(head.store, format!("did:key:{}", server.key));
    assert_eq!(block_on(store.head(did)).unwrap(), Some(head.clone()));
    let page = block_on(store.log(did, 0)).unwrap().unwrap();
    assert_eq!(page.events, vec![alice["incept"].clone()]);
    assert!(!page.vouched);

    assert_eq!(
        refusal(block_on(
            store.append(did, &append_req(vec![alice["incept"].clone()]))
        )),
        IdentityErrorCode::Conflict { head }
    );
    assert_eq!(
        refusal(block_on(
            store.append(did, &append_req(vec![alice["incept"].clone(); 33]))
        )),
        IdentityErrorCode::Invalid {
            reason: "too-many-events".into()
        }
    );
    let bob = apps.run("person", json!({ "name": "bob log" }));
    let bob_did = bob["did"].as_str().unwrap();
    let mut vouched = append_req(vec![bob["incept"].clone()]);
    vouched.vouch = Some("tok".into());
    assert_eq!(
        refusal(block_on(store.append(bob_did, &vouched))),
        IdentityErrorCode::Unavailable
    );
    let mut wrong_pow = append_req(vec![bob["incept"].clone()]);
    wrong_pow.pow = Some(ad4m_ldk::identity::Pow {
        nonce: "11111111111111111111111".into(),
        bits: 8,
    });
    assert!(matches!(
        refusal(block_on(store.append(bob_did, &wrong_pow))),
        IdentityErrorCode::Forbidden { .. }
    ));
    assert_eq!(
        refusal(block_on(store.head("did:key:z6Mk"))),
        IdentityErrorCode::Invalid {
            reason: "did".into()
        }
    );

    let d1 = alice["incept"]["add"][0]["sign"].as_str().unwrap();
    let entry = block_on(store.key_owner(d1, 0)).unwrap().unwrap();
    assert_eq!(entry.did, did);
    assert_eq!(entry.key_id, "d1");
    assert!(
        block_on(store.key_owner("z6MkpTHR8VNsBxYAAWHut2Geadd9jSwuBV8xRoAnwWsdvktH", 0))
            .unwrap()
            .is_none()
    );
    assert!(matches!(
        refusal(block_on(store.key_owner(d1, 9))),
        IdentityErrorCode::Invalid { .. }
    ));
    assert_eq!(block_on(store.pow_params()).unwrap().bits, 8);
}

#[test]
fn a_key_index_wait_answers_within_a_second_of_the_append() {
    let server = StoreProcess::start();
    let mut apps = Apps::start();
    let (_host, store) = Host::on(&server.url);
    let name = "carol wait";
    let p = person(&mut apps, &store, name);
    let did = p["did"].as_str().unwrap().to_string();
    let (offer, s) = apps.offer(name);
    let offer_id = offer["offer"]["offerId"].as_str().unwrap().to_string();
    block_on(store.open_offer(&serde_json::from_value(offer.clone()).unwrap())).unwrap();
    let device = apps.run("device", json!({ "label": "carol phone" }));
    let claim = apps.run(
        "claim",
        json!({ "name": name, "device": "carol phone", "offerId": offer_id, "s": s }),
    );
    block_on(store.claim_offer(
        &did,
        &offer_id,
        &serde_json::from_value(claim.clone()).unwrap(),
    ))
    .unwrap();

    let url = server.url.clone();
    let key = device["sign"].as_str().unwrap().to_string();
    let waiting = std::thread::spawn(move || {
        let (_host, store) = Host::on(&url);
        let entry = block_on(store.key_owner(&key, 8)).unwrap();
        (entry, Instant::now())
    });
    std::thread::sleep(Duration::from_millis(300));
    let add = apps.run(
        "add",
        json!({ "name": name, "offer": offer, "claim": claim, "claimedAt": now_s() }),
    );
    let appended = Instant::now();
    block_on(store.append(&did, &append_req(vec![add]))).unwrap();
    let (entry, at) = waiting.join().unwrap();
    assert_eq!(entry.unwrap().key_id, "d2");
    assert!(at.duration_since(appended) < Duration::from_secs(1));
}

#[test]
fn the_offer_calls_answer_and_refuse_as_the_store_does() {
    let server = StoreProcess::start();
    let mut apps = Apps::start();
    let (_host, store) = Host::on(&server.url);
    let name = "dave offers";
    let p = person(&mut apps, &store, name);
    let did = p["did"].as_str().unwrap().to_string();
    let scid = p["scid"].as_str().unwrap().to_string();

    let (offer, s) = apps.offer(name);
    let signed_offer = serde_json::from_value(offer.clone()).unwrap();
    let offer_id = offer["offer"]["offerId"].as_str().unwrap().to_string();
    assert_eq!(
        block_on(store.open_offer(&signed_offer)).unwrap().offer_id,
        offer_id
    );
    assert!(matches!(
        refusal(block_on(store.open_offer(&signed_offer))),
        IdentityErrorCode::Invalid { .. }
    ));
    let list_path = format!("/v1/offers/{scid}");
    let req = apps.signed(name, "master", None, "GET", &list_path, None);
    let listed = block_on(store.list_offers(&did, &req, 0)).unwrap();
    assert_eq!(listed.len(), 1);
    assert_eq!(serde_json::to_value(&listed[0].offer).unwrap(), offer);
    let by_device = apps.signed(name, "d1", Some("d1"), "GET", &list_path, None);
    assert_eq!(
        refusal(block_on(store.list_offers(&did, &by_device, 0))),
        IdentityErrorCode::Forbidden {
            reason: ad4m_ldk::identity::ForbiddenReason::Signer
        }
    );

    let wrong = apps.run(
        "claim",
        json!({ "name": name, "device": "dave phone", "offerId": offer_id,
        "s": "11111111111111111111111111111111" }),
    );
    assert_eq!(
        refusal(block_on(store.claim_offer(
            &did,
            &offer_id,
            &serde_json::from_value(wrong).unwrap()
        ))),
        IdentityErrorCode::Forbidden {
            reason: ad4m_ldk::identity::ForbiddenReason::Secret
        }
    );
    let claim = apps.run(
        "claim",
        json!({ "name": name, "device": "dave phone", "offerId": offer_id, "s": s,
        "canKeepMaster": true }),
    );
    let typed_claim = serde_json::from_value(claim.clone()).unwrap();
    let receipt = block_on(store.claim_offer(&did, &offer_id, &typed_claim)).unwrap();
    assert_eq!(receipt.state, "claimed");
    assert_eq!(receipt.expires, offer["offer"]["expires"].as_u64().unwrap());
    let phone = apps.run("device", json!({ "label": "dave phone" }));
    let phone_key = phone["sign"].as_str().unwrap();
    assert_eq!(
        block_on(store.offer_state(&did, &offer_id, phone_key, 0))
            .unwrap()
            .state,
        "claimed"
    );
    let stranger = apps.run("device", json!({ "label": "dave stranger" }));
    assert_eq!(
        refusal(block_on(store.offer_state(
            &did,
            &offer_id,
            stranger["sign"].as_str().unwrap(),
            0
        ))),
        IdentityErrorCode::NotFound
    );

    let (open, _) = apps.offer(name);
    block_on(store.open_offer(&serde_json::from_value(open.clone()).unwrap())).unwrap();
    let unclaimed = apps.run(
        "add",
        json!({ "name": name, "offer": open, "claim": claim, "claimedAt": now_s() }),
    );
    assert_eq!(
        refusal(block_on(store.append(&did, &append_req(vec![unclaimed])))),
        IdentityErrorCode::Forbidden {
            reason: ad4m_ldk::identity::ForbiddenReason::Claim
        }
    );

    let add = apps.run(
        "add",
        json!({ "name": name, "offer": offer, "claim": claim, "claimedAt": now_s() }),
    );
    assert_eq!(
        block_on(store.append(&did, &append_req(vec![add.clone()])))
            .unwrap()
            .seq,
        1
    );
    apps.run("push", json!({ "name": name, "event": add }));
    let progress = block_on(store.offer_state(&did, &offer_id, phone_key, 0)).unwrap();
    assert_eq!((progress.state.as_str(), progress.seq), ("spent", Some(1)));
    assert_eq!(
        refusal(block_on(store.claim_offer(&did, &offer_id, &typed_claim))),
        IdentityErrorCode::Gone {
            state: "spent".into()
        }
    );
    let cancel_path = format!("/v1/offers/{scid}/{offer_id}");
    let req = apps.signed(name, "master", None, "DELETE", &cancel_path, None);
    assert_eq!(
        refusal(block_on(store.cancel_offer(&did, &offer_id, &req))),
        IdentityErrorCode::NotFound
    );
    let open_id = open["offer"]["offerId"].as_str().unwrap();
    let req = apps.signed(
        name,
        "master",
        None,
        "DELETE",
        &format!("/v1/offers/{scid}/{open_id}"),
        None,
    );
    block_on(store.cancel_offer(&did, open_id, &req)).unwrap();

    let (third, third_s) = apps.offer(name);
    let third_id = third["offer"]["offerId"].as_str().unwrap().to_string();
    block_on(store.open_offer(&serde_json::from_value(third).unwrap())).unwrap();
    let taken = apps.run(
        "claim",
        json!({ "name": name, "device": "dave phone", "offerId": third_id, "s": third_s }),
    );
    assert_eq!(
        refusal(block_on(store.claim_offer(
            &did,
            &third_id,
            &serde_json::from_value(taken).unwrap()
        ))),
        IdentityErrorCode::KeyTaken {
            key: phone_key.to_string()
        }
    );
    for _ in 0..4 {
        let (more, _) = apps.offer(name);
        block_on(store.open_offer(&serde_json::from_value(more).unwrap())).unwrap();
    }
    let (sixth, _) = apps.offer(name);
    assert!(matches!(
        refusal(block_on(
            store.open_offer(&serde_json::from_value(sixth).unwrap())
        )),
        IdentityErrorCode::RateLimited {
            scope: ad4m_ldk::identity::RateLimitScope::Identity,
            ..
        }
    ));
}

#[test]
fn attach_note_checks_the_signed_request_then_the_store_has_no_vouching_service() {
    let server = StoreProcess::start();
    let mut apps = Apps::start();
    let (_host, store) = Host::on(&server.url);
    let name = "fay notes";
    let p = person(&mut apps, &store, name);
    let did = p["did"].as_str().unwrap();
    let path = format!("/v1/notes/{}", p["scid"].as_str().unwrap());
    let req = apps.signed(
        name,
        "d1",
        Some("d1"),
        "POST",
        &path,
        Some(&json!({ "token": "tok" })),
    );
    assert_eq!(
        refusal(block_on(store.attach_note(did, "tok", &req))),
        IdentityErrorCode::Unavailable
    );
    assert_eq!(
        refusal(block_on(store.attach_note(did, "tok", &req))),
        IdentityErrorCode::Forbidden {
            reason: ad4m_ldk::identity::ForbiddenReason::Signer
        }
    );
}

#[test]
fn put_agent_stores_a_did_scid_record_and_records_by_a_revoked_or_foreign_key_are_ignored() {
    let server = StoreProcess::start();
    let mut apps = Apps::start();
    let (_host, store) = Host::on(&server.url);
    let name = "gus records";
    let p = person(&mut apps, &store, name);
    let did = p["did"].as_str().unwrap().to_string();
    link(&mut apps, &store, name, &did, "gus phone");
    let profile = json!({ "perspective": { "links": [] }, "directMessageLanguage": "lang://dm" });
    let record: AgentRecord = serde_json::from_value(apps.run(
        "record",
        json!({ "name": name, "signer": "gus phone", "keyId": "d2", "profile": profile, "version": 1 }),
    ))
    .unwrap();
    let path = format!("/v1/agents/{did}");
    assert_eq!(
        refusal(block_on(store.put_agent(&record, None))),
        IdentityErrorCode::Forbidden {
            reason: ad4m_ldk::identity::ForbiddenReason::Signer
        }
    );
    let body = serde_json::to_value(&record).unwrap();
    let req = apps.signed(name, "d1", Some("d1"), "PUT", &path, Some(&body));
    block_on(store.put_agent(&record, Some(&req))).unwrap();
    let expression = block_on(agents::get(&store, &did, now_s()))
        .unwrap()
        .unwrap();
    assert_eq!(expression.data, profile);
    assert_eq!(expression.author, did);

    // A record of this did signed by a key of another log: a hostile store's row, ignored.
    let other = person(&mut apps, &store, "ivy records");
    let hostile = apps.run(
        "record",
        json!({ "name": "ivy records", "signer": "d1", "keyId": "d1", "profile": profile, "version": 9,
                "did": did, "anchor": record.anchor }),
    );
    assert_eq!(
        hostile["signedBy"],
        format!(
            "did:key:{}",
            other["incept"]["add"][0]["sign"].as_str().unwrap()
        )
    );
    {
        let db = rusqlite::Connection::open(server.db()).unwrap();
        db.execute(
            "UPDATE agents SET record = ?1, version = 9 WHERE did = ?2",
            rusqlite::params![hostile.to_string(), did],
        )
        .unwrap();
    }
    assert!(block_on(agents::get(&store, &did, now_s()))
        .unwrap()
        .is_none());
    {
        let db = rusqlite::Connection::open(server.db()).unwrap();
        db.execute(
            "UPDATE agents SET record = ?1, version = 1 WHERE did = ?2",
            rusqlite::params![body.to_string(), did],
        )
        .unwrap();
    }
    assert!(block_on(agents::get(&store, &did, now_s()))
        .unwrap()
        .is_some());

    // Revoke d2: its record is ignored.
    let revoke = apps.run("revoke", json!({ "name": name, "ids": ["d2"] }));
    block_on(store.append(&did, &append_req(vec![revoke.clone()]))).unwrap();
    apps.run("push", json!({ "name": name, "event": revoke }));
    assert!(block_on(agents::get(&store, &did, now_s()))
        .unwrap()
        .is_none());
}

#[test]
fn expression_create_and_get_for_a_did_key_agent_use_the_stores_agent_route() {
    let server = StoreProcess::start();
    let (host, store) = Host::on(&server.url);
    let agent = TestAgentKey::new("agent jo");
    let did = agent.did().unwrap();
    let content = json!({
        "did": did,
        "perspective": { "links": [{ "author": did, "timestamp": "2026-10-24T00:00:00.000Z",
            "data": { "source": "a", "target": "b" }, "proof": { "signature": "s", "key": "k", "valid": true } }] },
        "directMessageLanguage": "lang://dm",
    });
    assert_eq!(
        block_on(agents::create(&store, &agent, content.clone())).unwrap(),
        did
    );
    let expression = block_on(agents::get(&store, &did, now_s()))
        .unwrap()
        .unwrap();
    assert_eq!(expression.author, did);
    assert!(expression.data["perspective"]["links"][0]["proof"]
        .get("valid")
        .is_none());
    assert_eq!(expression.data["directMessageLanguage"], "lang://dm");
    assert_eq!(
        block_on(agents::create(&store, &agent, content.clone())).unwrap(),
        did
    );
    let mut foreign = content.clone();
    foreign["did"] = json!("did:key:z6MkOther");
    assert!(block_on(agents::create(&store, &agent, foreign)).is_err());

    // A record of this did signed by another key: ignored.
    {
        let db = rusqlite::Connection::open(server.db()).unwrap();
        let text: String = db
            .query_row("SELECT record FROM agents WHERE did = ?1", [&did], |r| {
                r.get(0)
            })
            .unwrap();
        let mut forged: Value = serde_json::from_str(&text).unwrap();
        forged["profile"] = json!({ "did": did, "perspective": { "links": [] } });
        db.execute(
            "UPDATE agents SET record = ?1 WHERE did = ?2",
            rusqlite::params![forged.to_string(), did],
        )
        .unwrap();
    }
    assert!(block_on(agents::get(&store, &did, now_s()))
        .unwrap()
        .is_none());

    let calls = host.calls.borrow();
    assert!(!calls.is_empty());
    for call in calls.iter() {
        assert!(
            call.url.starts_with(&format!("{}/v1/agents/", server.url)),
            "{}",
            call.url
        );
        assert!(!call.url.contains("mock.ad4m.dev"));
    }
    assert!(calls
        .iter()
        .any(|c| c.method == "PUT" && c.url == format!("{}/v1/agents/{did}", server.url)));
}

#[test]
fn bodies_are_the_inputs_canonical_json_and_the_signed_request_travels_unchanged() {
    let server = StoreProcess::start();
    let mut apps = Apps::start();
    let (host, store) = Host::on(&server.url);
    let name = "kim bytes";
    let p = apps.run("person", json!({ "name": name }));
    let did = p["did"].as_str().unwrap();
    let request = append_req(vec![p["incept"].clone()]);
    block_on(store.append(did, &request)).unwrap();
    let sent = host.last();
    assert_eq!(
        sent.body,
        ad4m_hash_chain::canonical_json(&serde_json::to_value(&request).unwrap()).unwrap()
    );
    assert_eq!(
        block_on(store.log(did, 0)).unwrap().unwrap().events[0],
        p["incept"]
    );

    let req = apps.signed(
        name,
        "master",
        None,
        "GET",
        &format!("/v1/offers/{}", p["scid"].as_str().unwrap()),
        None,
    );
    block_on(store.list_offers(did, &req, 0)).unwrap();
    let listed = host.last();
    assert_eq!(
        listed.headers[REQUEST_HEADER],
        encode_request(&req).unwrap()
    );
    assert_eq!(listed.body, "");
}

#[test]
fn changing_the_store_setting_moves_every_call_and_a_silent_store_is_unavailable() {
    let first = StoreProcess::start();
    let second = StoreProcess::start();
    let mut apps = Apps::start();
    let (host, store) = Host::on(&first.url);
    let p = person(&mut apps, &store, "lea moves");
    let did = p["did"].as_str().unwrap();
    assert!(block_on(store.head(did)).unwrap().is_some());
    *host.url.borrow_mut() = format!("{}/", second.url);
    assert!(block_on(store.head(did)).unwrap().is_none());
    block_on(store.append(did, &append_req(vec![p["incept"].clone()]))).unwrap();
    assert!(block_on(store.head(did)).unwrap().is_some());
    for call in host.calls.borrow().iter().rev().take(3) {
        assert!(
            call.url.starts_with(&format!("{}/v1/", second.url)),
            "{}",
            call.url
        );
    }
    *host.url.borrow_mut() = "http://127.0.0.1:9".into();
    assert_eq!(
        refusal(block_on(store.head(did))),
        IdentityErrorCode::Unavailable
    );
    assert_eq!(
        refusal(block_on(store.pow_params())),
        IdentityErrorCode::Unavailable
    );
}

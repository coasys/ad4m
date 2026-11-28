//! `LanguageController::get_expression` and the agent profile cache: what reaches the agent
//! language, and when. The language is a channel the test answers (`from_channel`); time is
//! tokio's paused clock, which `advance` moves without a sleep.
use super::agent_profile_cache::{FOUND_TTL, NOT_FOUND_TTL};
use super::language_runtime::LanguageOperation;
use super::language_runtime_handle::LanguageRuntimeHandle;
use super::LanguageController;
use crate::agent::AgentContext;
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc, Mutex,
};
use std::time::Duration;
use tokio::sync::oneshot;
use uuid::Uuid;

/// A language the test plays: `expressionGet(did)` answers `profiles[did]` (JSON text; absent
/// means `null`, `"ERR"` means the call fails), `expressionCreate` answers an address. A DID
/// put on hold (`hold`) gets the answer read when the call came, sent only on release.
#[derive(Clone, Default)]
struct FakeLanguage {
    profiles: Arc<Mutex<HashMap<String, String>>>,
    gets: Arc<AtomicUsize>,
    held: Arc<Mutex<HashMap<String, oneshot::Receiver<()>>>>,
}

impl FakeLanguage {
    fn set(&self, did: &str, answer: &str) {
        self.profiles
            .lock()
            .unwrap()
            .insert(did.to_string(), answer.to_string());
    }

    fn gets(&self) -> usize {
        self.gets.load(Ordering::SeqCst)
    }

    /// Holds the next `expressionGet(did)` answer until the returned sender fires.
    fn hold(&self, did: &str) -> oneshot::Sender<()> {
        let (release, held) = oneshot::channel();
        self.held.lock().unwrap().insert(did.to_string(), held);
        release
    }

    /// Serves `address` from this fake; the returned guard-less handle is torn down by `drop_language`.
    async fn serve(&self, controller: &LanguageController, address: &str) {
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
        controller
            .set_runtime_for_test(
                address,
                Some(LanguageRuntimeHandle::from_channel(address.to_string(), tx)),
            )
            .await;
        let this = self.clone();
        tokio::spawn(async move {
            while let Some(request) = requests.recv().await {
                let LanguageOperation::Execute(script, ..) = &request.operation else {
                    continue;
                };
                let mut held = None;
                let answer = if script.contains("isImmutableExpression") {
                    Ok("false".to_string())
                } else if script.contains("expressionGet(") {
                    this.gets.fetch_add(1, Ordering::SeqCst);
                    let did = script
                        .split("expressionGet(")
                        .nth(1)
                        .and_then(|rest| rest.split(')').next())
                        .and_then(|arg| serde_json::from_str::<String>(arg).ok())
                        .expect("a DID argument");
                    held = this.held.lock().unwrap().remove(&did);
                    match this.profiles.lock().unwrap().get(&did).map(String::as_str) {
                        Some("ERR") => Err("the store is down".to_string()),
                        Some(text) => Ok(text.to_string()),
                        None => Ok("null".to_string()),
                    }
                } else if script.contains("expressionCreate(") {
                    Ok("\"QmCreated\"".to_string())
                } else {
                    Err(format!("unexpected script: {script}"))
                };
                match held {
                    Some(held) => {
                        tokio::spawn(async move {
                            let _ = held.await;
                            let _ = request.response_tx.send(answer);
                        });
                    }
                    None => {
                        let _ = request.response_tx.send(answer);
                    }
                }
            }
        });
    }
}

fn profile(name: &str) -> String {
    json!({"author": "did:test:x", "timestamp": "2026-01-01T00:00:00Z", "data": {"name": name}})
        .to_string()
}

/// A fresh fake agent language, installed as the agent language, and a fresh controller state
/// for it. Returns `(controller, address, fake, previous agent language)`.
async fn agent_language() -> (LanguageController, String, FakeLanguage, Option<String>) {
    let controller = LanguageController::global_instance();
    let address = format!("QmFakeAgentLanguage{}", Uuid::new_v4().simple());
    let fake = FakeLanguage::default();
    fake.serve(&controller, &address).await;
    let previous = controller
        .system_addresses
        .lock()
        .await
        .agent_language
        .replace(address.clone());
    (controller, address, fake, previous)
}

async fn restore(controller: &LanguageController, address: &str, previous: Option<String>) {
    controller.system_addresses.lock().await.agent_language = previous;
    controller.set_runtime_for_test(address, None).await;
}

async fn get(controller: &LanguageController, address: &str, did: &str) -> Option<Value> {
    controller
        .get_expression(address, did)
        .await
        .expect("get_expression")
}

fn name_of(expression: &Option<Value>) -> Option<&str> {
    expression.as_ref()?["data"]["name"].as_str()
}

#[tokio::test(start_paused = true)]
async fn a_repeat_read_inside_the_ttl_does_not_reach_the_agent_language() {
    let (controller, address, fake, previous) = agent_language().await;
    fake.set("did:test:a", &profile("A"));

    let first = get(&controller, &address, "did:test:a").await;
    tokio::time::advance(FOUND_TTL - Duration::from_secs(1)).await;
    let second = get(&controller, &address, "did:test:a").await;
    let gets = fake.gets();
    restore(&controller, &address, previous).await;

    assert_eq!(name_of(&first), Some("A"));
    assert_eq!(name_of(&second), Some("A"));
    assert_eq!(gets, 1);
}

#[tokio::test(start_paused = true)]
async fn a_found_profile_is_asked_again_after_five_minutes() {
    let (controller, address, fake, previous) = agent_language().await;
    fake.set("did:test:a", &profile("A"));
    get(&controller, &address, "did:test:a").await;
    fake.set("did:test:a", &profile("A2"));

    tokio::time::advance(FOUND_TTL).await;
    let after = get(&controller, &address, "did:test:a").await;
    let gets = fake.gets();
    restore(&controller, &address, previous).await;

    assert_eq!(name_of(&after), Some("A2"));
    assert_eq!(gets, 2);
}

#[tokio::test(start_paused = true)]
async fn a_missing_profile_is_cached_for_a_minute_and_then_asked_again() {
    let (controller, address, fake, previous) = agent_language().await;

    assert_eq!(get(&controller, &address, "did:test:new").await, None);
    tokio::time::advance(NOT_FOUND_TTL - Duration::from_secs(1)).await;
    assert_eq!(get(&controller, &address, "did:test:new").await, None);
    let inside = fake.gets();

    // The agent publishes on another executor; the cached "none" ends within a minute.
    fake.set("did:test:new", &profile("New"));
    tokio::time::advance(Duration::from_secs(1)).await;
    let after = get(&controller, &address, "did:test:new").await;
    let gets = fake.gets();
    restore(&controller, &address, previous).await;

    assert_eq!(inside, 1);
    assert_eq!(name_of(&after), Some("New"));
    assert_eq!(gets, 2);
}

#[tokio::test(start_paused = true)]
async fn an_error_is_not_cached() {
    let (controller, address, fake, previous) = agent_language().await;
    fake.set("did:test:a", "ERR");
    let failed = controller.get_expression(&address, "did:test:a").await;

    fake.set("did:test:a", &profile("A"));
    let after = get(&controller, &address, "did:test:a").await;
    let gets = fake.gets();
    restore(&controller, &address, previous).await;

    assert!(failed.is_err());
    assert_eq!(name_of(&after), Some("A"));
    assert_eq!(gets, 2);
}

#[tokio::test(start_paused = true)]
async fn creating_an_expression_in_the_agent_language_drops_the_authors_entry() {
    let (controller, address, fake, previous) = agent_language().await;
    fake.set("did:test:a", &profile("old"));
    fake.set("did:test:b", &profile("B"));
    get(&controller, &address, "did:test:a").await;
    get(&controller, &address, "did:test:b").await;

    // The author is the agent that signs (the context)...
    fake.set("did:test:a", &profile("new"));
    controller
        .expression_create(
            &address,
            json!({"did": "did:test:a"}),
            &AgentContext::managed("did:test:a".to_string()),
        )
        .await
        .expect("expression_create");
    let own = get(&controller, &address, "did:test:a").await;

    // ...or the DID the content names (the profile of a person the agent speaks for).
    get(&controller, &address, "did:test:b").await;
    fake.set("did:test:b", &profile("B2"));
    controller
        .expression_create(
            &address,
            json!({"did": "did:test:b"}),
            &AgentContext::managed("did:test:device".to_string()),
        )
        .await
        .expect("expression_create");
    let named = get(&controller, &address, "did:test:b").await;
    restore(&controller, &address, previous).await;

    assert_eq!(name_of(&own), Some("new"));
    assert_eq!(name_of(&named), Some("B2"));
}

#[tokio::test(start_paused = true)]
async fn a_put_of_an_identity_record_drops_the_entry() {
    use crate::identity::backend::{AgentLanguageBackend, IdentityBackend};
    let (controller, address, fake, previous) = agent_language().await;
    fake.set("did:test:a", &profile("old"));
    get(&controller, &address, "did:test:a").await;
    fake.set("did:test:a", &profile("new"));

    // The fake serves no identity capability, so the put itself fails: a put that reports a
    // failure still drops the entry (it may have been applied).
    let record = ad4m_ldk::identity::AgentRecord {
        did: "did:test:a".to_string(),
        profile: json!({}),
        version: 1,
        signed_by: "did:test:a".to_string(),
        anchor: None,
        sig: String::new(),
    };
    let _ = AgentLanguageBackend.put_agent(&record, None).await;
    let after = get(&controller, &address, "did:test:a").await;
    restore(&controller, &address, previous).await;

    assert_eq!(name_of(&after), Some("new"));
}

/// A read that fetched the old profile and resumes after a local write must not put the old
/// profile back: the next read sees the write.
#[tokio::test(start_paused = true)]
async fn a_read_in_flight_across_a_write_does_not_put_back_the_old_profile() {
    let (controller, address, fake, previous) = agent_language().await;
    fake.set("did:test:a", &profile("old"));
    let release = fake.hold("did:test:a");
    let reader = tokio::spawn({
        let address = address.clone();
        async move {
            get(
                &LanguageController::global_instance(),
                &address,
                "did:test:a",
            )
            .await
        }
    });
    while fake.gets() == 0 {
        tokio::task::yield_now().await;
    }

    fake.set("did:test:a", &profile("new"));
    controller
        .expression_create(
            &address,
            json!({"did": "did:test:a"}),
            &AgentContext::managed("did:test:a".to_string()),
        )
        .await
        .expect("expression_create");
    release.send(()).unwrap();
    let in_flight = reader.await.unwrap();
    let after = get(&controller, &address, "did:test:a").await;
    let gets = fake.gets();
    restore(&controller, &address, previous).await;

    assert_eq!(name_of(&in_flight), Some("old"));
    assert_eq!(name_of(&after), Some("new"));
    assert_eq!(gets, 2);
}

/// The proof verdict depends on the time (a device key can expire) and on the identity module,
/// so the cache keeps the expression as the language returned it, and every read, a hit too,
/// carries a verdict made at that read.
#[tokio::test(start_paused = true)]
async fn the_cache_keeps_no_proof_verdict_and_every_read_carries_one() {
    let (controller, address, fake, previous) = agent_language().await;
    let signed = json!({
        "author": "did:test:x",
        "timestamp": "2026-01-01T00:00:00Z",
        "data": {"name": "A"},
        "proof": {"key": "did:test:x#key", "signature": "00"},
    });
    fake.set("did:test:a", &signed.to_string());

    let first = get(&controller, &address, "did:test:a").await;
    let hit = get(&controller, &address, "did:test:a").await;
    let stored = controller.agent_profiles.lock().unwrap().get(
        &address,
        "did:test:a",
        tokio::time::Instant::now(),
    );
    let gets = fake.gets();
    restore(&controller, &address, previous).await;

    assert_eq!(gets, 1);
    for read in [&first, &hit] {
        assert_eq!(read.as_ref().unwrap()["proof"]["invalid"], json!(true));
    }
    let stored_proof = &stored.flatten().expect("cached")["proof"];
    assert!(stored_proof.get("valid").is_none() && stored_proof.get("invalid").is_none());
}

#[tokio::test(start_paused = true)]
async fn another_languages_expressions_are_not_cached() {
    let (controller, address, fake, previous) = agent_language().await;
    let other_address = format!("QmFakeOtherLanguage{}", Uuid::new_v4().simple());
    let other = FakeLanguage::default();
    other.serve(&controller, &other_address).await;
    other.set("did:test:a", &profile("A"));

    get(&controller, &other_address, "did:test:a").await;
    get(&controller, &other_address, "did:test:a").await;
    let other_gets = other.gets();
    let agent_gets = fake.gets();
    controller.set_runtime_for_test(&other_address, None).await;
    restore(&controller, &address, previous).await;

    assert_eq!(other_gets, 2);
    assert_eq!(agent_gets, 0);
}

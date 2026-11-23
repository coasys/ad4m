//! Mesh signalling must not wait on the link-language queue: a language
//! runtime serves one call at a time, and on 2026-11-19 a job in one held
//! it for 20 s at a time while every `sendBroadcast` — presence beats, mesh
//! offers, ICE candidates — every `onlineAgents` and every `getConfig`
//! behind it died waiting, and a failed `getConfig` membership check
//! silently answered "mesh". These tests put a stub link language in place
//! whose calls sleep `STUB_DELAY`, past every deadline the callers have
//! (`start_paused` makes the delay cost no real time), and check what the
//! RPC paths answer while it is blocked.

use super::*;
use crate::db::Ad4mDb;
use crate::languages::capability::{register_capabilities, remove_capabilities, Capability};
use crate::languages::language_runtime::LanguageOperation;
use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
use crate::languages::LanguageController;
use crate::pubsub::get_global_pubsub;
use crate::sfu::config_store::{read_policy, CallPolicy};
use crate::test_utils::setup_wallet;
use tokio::time::timeout;

/// Longer than every deadline the caller side has: a code path that still
/// waits on the language misses its deadline, one that does not returns at
/// once. `start_paused` advances the clock, so the delay is free in real
/// time and never reached by a shorter test timeout.
const STUB_DELAY: Duration = Duration::from_secs(20);

/// Shorter than the membership check's deadline, also for two calls in a row.
const SLOW_DELAY: Duration = Duration::from_millis(500);

/// What the stub does with a call it is asked to run.
#[derive(Clone, Copy)]
enum StubMode {
    /// Answer at once.
    Serve,
    /// Sleep `STUB_DELAY` first, then answer — the 2026-11-19 block.
    Block,
    /// Sleep `STUB_DELAY`, then fail — a language call that errors.
    Fail,
    /// Sleep `SLOW_DELAY`, then answer: a call that ends inside the caller's
    /// deadline, leaving time for something to happen while it runs.
    Slow,
    /// Never answer, and take the next call at once: a call the runtime
    /// lost (the reply sender is kept, so the caller waits forever).
    Hang,
}

/// A link language served by this test. `Execute` calls arrive on its
/// channel one at a time (like the real serial runtime); the drainer
/// records every script in `scripts`, then answers per `mode`.
struct StubLanguage {
    address: String,
    mode: Arc<std::sync::Mutex<StubMode>>,
    peers: Arc<std::sync::Mutex<Vec<String>>>,
    scripts: Arc<std::sync::Mutex<Vec<String>>>,
}

impl StubLanguage {
    fn set(&self, mode: StubMode) {
        *self.mode.lock().unwrap() = mode;
    }

    /// What `peersRemote()` answers from now on.
    fn set_peers(&self, peers: Vec<String>) {
        *self.peers.lock().unwrap() = peers;
    }

    /// Every script the language was asked to run, in order.
    fn scripts(&self) -> Vec<String> {
        self.scripts.lock().unwrap().clone()
    }

    /// How many `telepresenceSendBroadcast` calls reached the language.
    fn send_count(&self) -> usize {
        self.scripts()
            .iter()
            .filter(|s| s.contains("telepresenceSendBroadcast"))
            .count()
    }

    async fn stop(&self) {
        LanguageController::global_instance()
            .set_runtime_for_test(&self.address, None)
            .await;
        remove_capabilities(&self.address);
    }
}

/// Install a test-served link language at a fresh address: `peers` is what
/// `peersRemote()` answers, `caps` the capability set a `Language` reads at
/// `Language::new`. Calls start in `StubMode::Serve`.
async fn stub_language(peers: Vec<String>, caps: &[Capability]) -> StubLanguage {
    let address = format!("QmStub{}", Uuid::new_v4().simple());
    let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel::<
        crate::languages::language_runtime::LanguageRuntimeRequest,
    >();
    LanguageController::global_instance()
        .set_runtime_for_test(
            &address,
            Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
        )
        .await;
    register_capabilities(&address, caps.iter().copied().collect());
    let mode = Arc::new(std::sync::Mutex::new(StubMode::Serve));
    let peers = Arc::new(std::sync::Mutex::new(peers));
    let scripts = Arc::new(std::sync::Mutex::new(Vec::new()));
    {
        let mode = mode.clone();
        let peers = peers.clone();
        let scripts = scripts.clone();
        tokio::spawn(async move {
            let mut lost = Vec::new();
            while let Some(request) = requests.recv().await {
                let reply = match &request.operation {
                    LanguageOperation::Execute(script, ..) => {
                        scripts.lock().unwrap().push(script.clone());
                        if script.contains("peersRemote") {
                            Ok(serde_json::to_string(&*peers.lock().unwrap()).unwrap())
                        } else if script.contains("telepresenceGetOnlineAgents") {
                            Ok("[]".to_string())
                        } else {
                            Ok("null".to_string())
                        }
                    }
                    _ => Ok("null".to_string()),
                };
                if matches!(*mode.lock().unwrap(), StubMode::Hang) {
                    lost.push(request.response_tx);
                    continue;
                }
                let blocked = matches!(*mode.lock().unwrap(), StubMode::Block | StubMode::Fail);
                if blocked {
                    sleep(STUB_DELAY).await;
                } else if matches!(*mode.lock().unwrap(), StubMode::Slow) {
                    sleep(SLOW_DELAY).await;
                }
                let reply = match *mode.lock().unwrap() {
                    StubMode::Fail => Err("stub language is down".to_string()),
                    _ => reply,
                };
                let _ = request.response_tx.send(reply);
            }
        });
    }
    StubLanguage {
        address,
        mode,
        peers,
        scripts,
    }
}

/// A neighbourhood perspective on `language`'s address, owned by `owners`,
/// registered with the global registry so policy lookups find it by URL.
async fn neighbourhood(language: &StubLanguage, owners: Vec<String>) -> PerspectiveInstance {
    let url = format!("neighbourhood://{}", Uuid::new_v4());
    let handle = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some("telepresence-test".to_string()),
        shared_url: Some(url),
        neighbourhood: Some(DecoratedNeighbourhoodExpression {
            author: crate::agent::did(),
            data: crate::types::domain::Neighbourhood {
                link_language: language.address.clone(),
                ..Default::default()
            },
            ..Default::default()
        }),
        state: PerspectiveState::Synced,
        owners: Some(owners),
    };
    Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
    let instance = PerspectiveInstance::new(handle, None);
    *instance.link_language.write().await = Some(Language::new(language.address.clone()));
    crate::perspectives::register_perspective(instance.uuid.clone(), instance.clone());
    instance
}

/// One telepresence link whose `source` is the transport's
/// `{device, message}` encoding (`MeshManager` writes the message for
/// `source`, `ad4m://session/rtc` for `predicate`, `*` for `target`).
fn signal(message: serde_json::Value) -> PerspectiveExpression {
    signal_from(&crate::agent::did(), &[("desk", message)])
}

/// A broadcast by `author` with one telepresence link per `(device, message)`.
fn signal_from(author: &str, links: &[(&str, serde_json::Value)]) -> PerspectiveExpression {
    PerspectiveExpression {
        author: author.to_string(),
        timestamp: "2026-11-19T00:00:00.000Z".to_string(),
        data: crate::types::domain::Perspective {
            links: links
                .iter()
                .map(|(device, message)| DecoratedLinkExpression {
                    author: String::new(),
                    timestamp: String::new(),
                    data: Link {
                        source: serde_json::to_string(&serde_json::json!({
                            "device": device,
                            "message": message,
                        }))
                        .unwrap(),
                        predicate: Some("ad4m://session/rtc".to_string()),
                        target: "*".to_string(),
                    },
                    proof: DecoratedExpressionProof {
                        key: String::new(),
                        signature: String::new(),
                        valid: Some(true),
                        invalid: Some(false),
                    },
                    status: None,
                })
                .collect(),
        },
        proof: DecoratedExpressionProof {
            key: String::new(),
            signature: String::new(),
            valid: Some(true),
            invalid: Some(false),
        },
    }
}

/// A mesh presence beat, as `Session`'s beat timer publishes it.
fn beat() -> PerspectiveExpression {
    signal(beat_in("room:x"))
}

/// A beat message in `call`.
fn beat_in(call: &str) -> serde_json::Value {
    serde_json::json!({"v": 1, "call": call, "to": "*", "kind": "beat"})
}

/// How many link-language sends carried `needle` (a call id, a device, an
/// SDP: the payload arrives JSON-escaped inside the script, which keeps
/// such plain words intact).
fn sends_with(stub: &StubLanguage, needle: &str) -> usize {
    stub.scripts()
        .iter()
        .filter(|s| s.contains("telepresenceSendBroadcast") && s.contains(needle))
        .count()
}

/// A mesh message that must not be lost: an SDP offer.
fn offer() -> PerspectiveExpression {
    signal(serde_json::json!({
        "v": 1, "call": "room:x", "to": "*",
        "kind": "description", "description": {"type": "offer", "sdp": "sdp"}
    }))
}

/// `read_policy`'s return type, either side of the change: the tests run
/// against today's `CallPolicy` (red: no cache, errors become mesh) and
/// the fixed `Result<CallPolicy>` alike.
trait AsPolicyResult {
    fn into_result(self) -> Result<CallPolicy, AnyError>;
}

impl AsPolicyResult for Result<CallPolicy, AnyError> {
    fn into_result(self) -> Result<CallPolicy, AnyError> {
        self
    }
}

impl AsPolicyResult for CallPolicy {
    fn into_result(self) -> Result<CallPolicy, AnyError> {
        Ok(self)
    }
}

fn test_setup() {
    setup_wallet();
    Ad4mDb::init_global_instance(":memory:").unwrap();
    AgentService::init_global_test_instance();
}

/// S6: `sendBroadcast` answers once local delivery is done — it does not
/// wait on the link language. Local members still receive the signal, and
/// the remote send still reaches the language afterwards.
#[tokio::test(start_paused = true)]
async fn a_broadcast_is_answered_before_the_link_language() {
    test_setup();
    let stub = stub_language(
        vec!["did:key:remote".to_string()],
        &[
            Capability::PeersRemote,
            Capability::TelepresenceSendBroadcast,
        ],
    )
    .await;
    let owner = crate::agent::did();
    let instance = neighbourhood(&stub, vec![owner.clone()]).await;
    let mut events = get_global_pubsub().await.subscribe_events();
    stub.set(StubMode::Block);

    let answered = timeout(
        Duration::from_millis(100),
        instance.send_broadcast(offer(), false),
    )
    .await;
    assert!(
        matches!(answered, Ok(Ok(()))),
        "sendBroadcast did not answer within 100 ms while the language was blocked: {answered:?}"
    );

    // The local owner got the signal.
    let local = tokio::time::timeout(Duration::from_millis(200), async {
        loop {
            match events.try_recv() {
                Ok(event)
                    if event.topic == Topic::NeighbourhoodSignal
                        && event.perspective.as_deref() == Some(instance.uuid.as_str()) =>
                {
                    return serde_json::to_string(&event.payload).unwrap()
                }
                Ok(_) => continue,
                Err(_) => sleep(Duration::from_millis(5)).await,
            }
        }
    })
    .await;
    assert!(
        local.is_ok(),
        "the local owner received no signal while the language was blocked"
    );

    // The remote send was still attempted: the request reached the language.
    sleep(Duration::from_millis(50)).await;
    assert_eq!(
        stub.send_count(),
        1,
        "the broadcast never reached the link language: {:?}",
        stub.scripts()
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// S6: a burst of sends while the language is blocked must not queue
/// without bound behind it — the queue refuses past its bound and a
/// superseded presence beat replaces every queued one.
#[tokio::test(start_paused = true)]
async fn queued_broadcasts_coalesce_beats_and_bound_the_burst() {
    test_setup();
    let stub = stub_language(
        vec!["did:key:remote".to_string()],
        &[
            Capability::PeersRemote,
            Capability::TelepresenceSendBroadcast,
        ],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    stub.set(StubMode::Block);

    // Beats are superseded by newer ones: ten queued beats become at most
    // the first (running) plus the newest (queued).
    for _ in 0..10 {
        let _ = timeout(
            Duration::from_millis(200),
            instance.send_broadcast(beat(), false),
        )
        .await;
    }
    tokio::time::advance(STUB_DELAY).await;
    sleep(Duration::from_millis(50)).await;
    // The payload's `source` JSON arrives inside the script JSON-escaped;
    // "beat" only ever names the presence beat's kind in it.
    let beats = stub
        .scripts()
        .iter()
        .filter(|s| s.contains("telepresenceSendBroadcast"))
        .filter(|s| s.contains("beat"))
        .count();
    assert!(
        beats <= 2,
        "superseded presence beats were not dropped: {beats} reached the language"
    );

    // Non-beats must arrive, but the queue is bounded: past the bound the
    // send is refused rather than piled onto the language.
    let mut accepted = 0;
    for _ in 0..20 {
        match timeout(
            Duration::from_millis(200),
            instance.send_broadcast(offer(), false),
        )
        .await
        {
            Ok(Ok(())) => accepted += 1,
            Ok(Err(_)) => {} // refused: the queue was full
            Err(_) => {}     // did not even answer in time (dev: every send)
        }
    }
    tokio::time::advance(STUB_DELAY * 2).await;
    sleep(Duration::from_millis(50)).await;
    let reached = stub.send_count();
    assert!(
        reached <= 10, // the running send plus at most a bounded queue
        "queued broadcasts piled onto the language unbounded: {reached} of 20 offers + 10 beats"
    );
    assert!(
        accepted <= 10,
        "more sends were accepted than the bound allows: {accepted}"
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// S6: when the membership check has verified every member is local, the
/// remote send is skipped entirely.
#[tokio::test(start_paused = true)]
async fn a_broadcast_to_only_local_members_skips_the_language() {
    test_setup();
    // Nobody remote: peersRemote is empty and the only owner is local.
    let stub = stub_language(
        Vec::new(),
        &[
            Capability::PeersRemote,
            Capability::TelepresenceSendBroadcast,
        ],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    let url = instance.persisted.lock().await.shared_url.clone().unwrap();

    // One policy read verifies "all hosted" and caches that verdict.
    assert_eq!(
        read_policy(&url).await.into_result().unwrap().mode,
        "designated"
    );

    stub.set(StubMode::Block);
    let answered = timeout(
        Duration::from_millis(100),
        instance.send_broadcast(offer(), false),
    )
    .await;
    assert!(
        matches!(answered, Ok(Ok(()))),
        "sendBroadcast did not answer within 100 ms: {answered:?}"
    );
    assert_eq!(
        stub.send_count(),
        0,
        "an all-local neighbourhood still paid a language call"
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// No link language: local delivery still happens and the call fails, as
/// before.
#[tokio::test(start_paused = true)]
async fn a_broadcast_without_a_link_language_still_fails_after_local_delivery() {
    test_setup();
    let url = format!("neighbourhood://{}", Uuid::new_v4());
    let owner = crate::agent::did();
    let handle = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some("telepresence-test".to_string()),
        shared_url: Some(url),
        neighbourhood: None,
        state: PerspectiveState::Synced,
        owners: Some(vec![owner.clone()]),
    };
    Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
    let instance = PerspectiveInstance::new(handle, None);
    let mut events = get_global_pubsub().await.subscribe_events();

    let answered = timeout(
        Duration::from_millis(500),
        instance.send_broadcast(offer(), false),
    )
    .await;
    match answered {
        Ok(Err(_)) => {} // local delivery done, then no-link-language error, as before
        other => panic!("a perspective without a link language changed: {other:?}"),
    }
    let delivered = timeout(Duration::from_millis(200), async {
        loop {
            match events.try_recv() {
                Ok(event)
                    if event.topic == Topic::NeighbourhoodSignal
                        && event.perspective.as_deref() == Some(instance.uuid.as_str()) =>
                {
                    return true;
                }
                Ok(_) => continue,
                Err(_) => sleep(Duration::from_millis(5)).await,
            }
        }
    })
    .await;
    assert_eq!(delivered, Ok(true), "local delivery did not happen");
}

/// S7: `onlineAgents` does not wait on a blocked language past its 2 s
/// time-box, and what it answers then is "unknown", not a roster: an `Ok`
/// with only the co-located users would make every client drop its remote
/// call participants (`MeshManager.setRoster` tears down whoever is
/// missing). The error leaves the client's roster as it was.
#[tokio::test(start_paused = true)]
async fn online_agents_on_a_blocked_language_is_unknown_not_a_local_only_roster() {
    test_setup();
    let now = chrono::Utc::now().timestamp();
    let user = "did:key:telepresence-user";
    Ad4mDb::with_global_instance(|db| {
        db.import_from_json(
            serde_json::json!({
                "users": [{
                    "username": "tp-user@test",
                    "did": user,
                    "password_hash": "x",
                    "last_seen": now,
                }]
            }),
            None,
        )
    })
    .expect("seed users");

    let stub = stub_language(
        Vec::new(),
        &[Capability::PeersRemote, Capability::TelepresenceGetAgents],
    )
    .await;
    let instance = neighbourhood(&stub, vec![user.to_string()]).await;

    // A healthy language: the co-located user is listed.
    let agents = instance
        .online_agents()
        .await
        .expect("onlineAgents failed on a healthy language");
    assert!(
        agents.iter().any(|a| a.did == user),
        "setup: the co-located user is missing: {agents:?}"
    );

    stub.set(StubMode::Block);
    let answered = timeout(Duration::from_millis(2500), instance.online_agents())
        .await
        .expect("onlineAgents did not answer within 2.5 s while the language was blocked");
    assert!(
        answered.is_err(),
        "a timed-out language call answered an authoritative roster without the remote agents: {answered:?}"
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// S8: after one good read, a blocked membership check keeps answering the
/// same policy — every time — rather than waiting on the language or
/// falling to "mesh".
#[tokio::test(start_paused = true)]
async fn a_blocked_policy_check_serves_the_last_policy() {
    test_setup();
    // No remote peers, the only owner is this executor's main agent: the
    // neighbourhood's honest policy is this node's own SFU.
    let stub = stub_language(
        Vec::new(),
        &[Capability::PeersRemote, Capability::TelepresenceGetAgents],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    let url = instance.persisted.lock().await.shared_url.clone().unwrap();

    let first = timeout(Duration::from_secs(3), read_policy(&url))
        .await
        .expect("the first policy read waited on the language")
        .into_result()
        .expect("the first policy read failed");
    assert_eq!(
        first.mode, "designated",
        "setup: expected the honest policy"
    );

    stub.set(StubMode::Block);
    for i in 0..10 {
        let policy = timeout(Duration::from_secs(3), read_policy(&url))
            .await
            .unwrap_or_else(|_| panic!("policy read {i} waited on the blocked language"))
            .into_result()
            .unwrap_or_else(|e| panic!("policy read {i} failed: {e}"));
        assert_eq!(
            policy.mode, "designated",
            "policy read {i} did not serve the last policy"
        );
    }

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// A member this executor does not host signals while the membership check
/// runs, before any policy is cached: the check's "every member local" is
/// out of date when it returns. It must not be cached, or broadcasts skip
/// the remote member for the cache's lifetime.
#[tokio::test(start_paused = true)]
async fn a_remote_member_seen_during_the_check_is_not_lost() {
    test_setup();
    let stub = stub_language(
        Vec::new(),
        &[Capability::PeersRemote, Capability::TelepresenceGetAgents],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    let url = instance.persisted.lock().await.shared_url.clone().unwrap();
    stub.set(StubMode::Slow);

    let read = tokio::spawn({
        let url = url.clone();
        async move { read_policy(&url).await.into_result() }
    });
    // Time stands still while this task yields: the check is in the
    // language, and stays there until the sighting is made.
    while stub.scripts().is_empty() {
        tokio::task::yield_now().await;
    }
    crate::sfu::config_store::saw_member(&url, "did:key:remote-member");

    let policy = timeout(Duration::from_secs(3), read)
        .await
        .expect("the policy read waited past its deadline")
        .unwrap()
        .expect("the policy read failed");
    assert_eq!(
        policy.mode, "mesh",
        "a check that started before a remote member showed up answered every member local"
    );
    assert!(
        !crate::sfu::config_store::every_member_local(&url),
        "broadcasts skip the remote member that signalled during the check"
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// The link language names a signal's author: only a signal whose signature
/// verified may end an "every member local" verdict. Local delivery is the
/// same either way.
#[tokio::test(start_paused = true)]
async fn only_a_verified_signal_counts_as_a_remote_member() {
    test_setup();
    let stub = stub_language(
        Vec::new(),
        &[Capability::PeersRemote, Capability::TelepresenceGetAgents],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    let handle = instance.persisted.lock().await.clone();
    let url = handle.shared_url.clone().unwrap();
    let policy = read_policy(&url).await.into_result().unwrap();
    assert_eq!(
        policy.mode, "designated",
        "setup: expected the honest policy"
    );
    assert!(crate::sfu::config_store::every_member_local(&url));

    let signed = |valid: bool| {
        let mut signal = signal_from(
            "did:key:named-by-the-language",
            &[("desk", serde_json::json!({}))],
        );
        signal.proof.valid = Some(valid);
        signal.proof.invalid = Some(!valid);
        signal
    };
    crate::perspectives::publish_telepresence_signal(handle.clone(), signed(false), None);
    assert!(
        crate::sfu::config_store::every_member_local(&url),
        "a signal whose signature failed ended the every-member-local verdict"
    );
    assert_eq!(
        read_policy(&url).await.into_result().unwrap().mode,
        "designated"
    );

    crate::perspectives::publish_telepresence_signal(handle, signed(true), None);
    assert!(
        !crate::sfu::config_store::every_member_local(&url),
        "a verified signal from a remote member left the verdict in place"
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// S8: a failed check with no earlier value answers an error — never a
/// "mesh" the check did not compute.
#[tokio::test(start_paused = true)]
async fn a_failed_policy_check_without_an_earlier_one_errors() {
    test_setup();
    let stub = stub_language(
        vec!["did:key:remote".to_string()],
        &[Capability::PeersRemote, Capability::TelepresenceGetAgents],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    let url = instance.persisted.lock().await.shared_url.clone().unwrap();
    stub.set(StubMode::Fail);

    let policy = timeout(Duration::from_secs(60), read_policy(&url))
        .await
        .expect("the policy read waited past its deadline")
        .into_result();
    assert!(
        policy.is_err(),
        "a failed check with no earlier policy answered {policy:?} instead of an error"
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// No link language: the policy for a neighbourhood with no remote
/// members is still "mesh", as before — no error where there was none.
#[tokio::test(start_paused = true)]
async fn a_policy_read_without_a_link_language_stays_mesh() {
    test_setup();
    let url = format!("neighbourhood://{}", Uuid::new_v4());
    let handle = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some("telepresence-test".to_string()),
        shared_url: Some(url.clone()),
        neighbourhood: Some(DecoratedNeighbourhoodExpression {
            author: crate::agent::did(),
            data: crate::types::domain::Neighbourhood {
                link_language: "QmNotInstalled".to_string(),
                ..Default::default()
            },
            ..Default::default()
        }),
        state: PerspectiveState::Synced,
        owners: Some(vec![crate::agent::did()]),
    };
    Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
    let instance = PerspectiveInstance::new(handle, None);
    crate::perspectives::register_perspective(instance.uuid.clone(), instance.clone());

    let policy = timeout(Duration::from_secs(3), read_policy(&url))
        .await
        .expect("the policy read waited on something")
        .into_result()
        .expect("a neighbourhood without a link language changed: it must not error");
    assert_eq!(policy.mode, "mesh");

    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// Coalescing drops only a beat a newer one supersedes: the same sender,
/// device and call. Beats of other devices or calls are other news, and a
/// payload with anything besides beats in it is never dropped as one.
#[tokio::test(start_paused = true)]
async fn only_a_beat_of_the_same_sender_device_and_call_is_superseded() {
    test_setup();
    let stub = stub_language(
        vec!["did:key:remote".to_string()],
        &[
            Capability::PeersRemote,
            Capability::TelepresenceSendBroadcast,
        ],
    )
    .await;
    let me = crate::agent::did();
    let instance = neighbourhood(&stub, vec![me.clone(), "did:key:co-user".to_string()]).await;
    stub.set(StubMode::Block);

    // Holds the language while the rest queue.
    instance.send_broadcast(offer(), false).await.unwrap();
    let mixed = signal_from(
        &me,
        &[
            ("desk", beat_in("room:x")),
            (
                "desk",
                serde_json::json!({"v": 1, "call": "room:x", "to": "*", "kind": "ice",
                    "candidate": {"candidate": "mixed-candidate"}}),
            ),
        ],
    );
    for payload in [
        mixed,
        signal_from(&me, &[("desk", beat_in("room:x"))]),
        signal_from(&me, &[("phone", beat_in("room:x"))]),
        signal_from(&me, &[("desk", beat_in("room:y"))]),
        signal_from("did:key:co-user", &[("desk", beat_in("room:x"))]),
    ] {
        instance.send_broadcast(payload, false).await.unwrap();
    }
    // `sleep`, not one `advance`: the paused clock then steps through each
    // send's delay in turn.
    sleep(STUB_DELAY * 8).await;

    assert_eq!(
        sends_with(&stub, "mixed-candidate"),
        1,
        "a payload with an ICE candidate beside a beat was dropped as a beat"
    );
    assert_eq!(
        sends_with(&stub, "phone"),
        1,
        "another device's beat was dropped"
    );
    assert_eq!(
        sends_with(&stub, "room:y"),
        1,
        "another call's beat was dropped"
    );
    assert_eq!(
        sends_with(&stub, "co-user"),
        1,
        "another sender's beat was dropped"
    );
    // offer + mixed + me/desk/room:x + phone + room:y + co-user.
    assert_eq!(stub.send_count(), 6, "{:?}", stub.scripts());

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// A remote member who joins after a membership check found everyone local
/// gets the next signal: its first signal here ends the "every member local"
/// verdict, which would otherwise skip the remote send for the cache's 60 s.
#[tokio::test(start_paused = true)]
async fn a_remote_member_who_joins_inside_the_cache_window_gets_the_next_signal() {
    test_setup();
    let stub = stub_language(
        Vec::new(),
        &[
            Capability::PeersRemote,
            Capability::TelepresenceSendBroadcast,
        ],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    let url = instance.persisted.lock().await.shared_url.clone().unwrap();
    assert_eq!(
        read_policy(&url).await.into_result().unwrap().mode,
        "designated",
        "setup: everyone is local"
    );
    instance.send_broadcast(offer(), false).await.unwrap();
    sleep(Duration::from_millis(50)).await;
    assert_eq!(
        stub.send_count(),
        0,
        "setup: an all-local send was not skipped"
    );

    // A remote member joins 5 s later and beats through the link language.
    // Published after signature checking: the stub cannot sign as another
    // agent, and only a verified author counts as a member.
    sleep(Duration::from_secs(5)).await;
    stub.set_peers(vec!["did:key:remote".to_string()]);
    crate::perspectives::publish_telepresence_signal(
        instance.persisted.lock().await.clone(),
        signal_from("did:key:remote", &[("desk", beat_in("room:x"))]),
        None,
    );
    sleep(Duration::from_millis(50)).await;

    instance.send_broadcast(offer(), false).await.unwrap();
    sleep(Duration::from_millis(50)).await;
    assert_eq!(
        stub.send_count(),
        1,
        "the signal after a remote member joined never reached the link language"
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// A language call that never answers holds one send for at most the
/// send's time-box; the broadcasts behind it still go out.
#[tokio::test(start_paused = true)]
async fn a_hung_language_send_is_abandoned_after_its_time_box() {
    test_setup();
    let stub = stub_language(
        vec!["did:key:remote".to_string()],
        &[
            Capability::PeersRemote,
            Capability::TelepresenceSendBroadcast,
        ],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    stub.set(StubMode::Hang);

    instance.send_broadcast(offer(), false).await.unwrap();
    instance
        .send_broadcast(
            signal(
                serde_json::json!({"v": 1, "call": "room:x", "to": "*", "kind": "ice",
                "candidate": {"candidate": "behind-the-hung-send"}}),
            ),
            false,
        )
        .await
        .unwrap();
    sleep(Duration::from_millis(50)).await;
    assert_eq!(stub.send_count(), 1, "setup: the first send is in flight");

    sleep(Duration::from_secs(120)).await;
    assert_eq!(
        sends_with(&stub, "behind-the-hung-send"),
        1,
        "the broadcast behind a hung send never went out"
    );

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

/// Teardown during a hung send ends the drain task at once: it holds a
/// clone of the perspective, and with it the perspective's store.
#[tokio::test(start_paused = true)]
async fn teardown_during_a_hung_send_ends_the_drain_task() {
    test_setup();
    let stub = stub_language(
        vec!["did:key:remote".to_string()],
        &[
            Capability::PeersRemote,
            Capability::TelepresenceSendBroadcast,
        ],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    crate::perspectives::unregister_perspective(&instance.uuid);
    let held = || Arc::strong_count(&instance.language_broadcasts);
    let before = held();
    stub.set(StubMode::Hang);

    instance.send_broadcast(offer(), false).await.unwrap();
    sleep(Duration::from_millis(50)).await;
    assert_eq!(stub.send_count(), 1, "setup: the send is in flight");
    assert!(
        held() > before,
        "setup: the drain task holds the perspective"
    );

    instance.teardown_background_tasks().await;
    sleep(Duration::from_millis(50)).await;
    assert_eq!(
        held(),
        before,
        "the drain task still holds the perspective after teardown"
    );

    stub.stop().await;
}

/// S8, past the cache's 60 s: a blocked membership check still answers the
/// last policy within its own time-box, not after the language's stall.
/// And once the language serves again, the expired entry is checked anew.
#[tokio::test(start_paused = true)]
async fn an_expired_policy_with_a_blocked_check_serves_the_last_policy() {
    test_setup();
    let stub = stub_language(
        Vec::new(),
        &[Capability::PeersRemote, Capability::TelepresenceGetAgents],
    )
    .await;
    let instance = neighbourhood(&stub, vec![crate::agent::did()]).await;
    let url = instance.persisted.lock().await.shared_url.clone().unwrap();
    assert_eq!(
        read_policy(&url).await.into_result().unwrap().mode,
        "designated",
        "setup: expected the honest policy"
    );

    sleep(Duration::from_secs(61)).await;
    stub.set(StubMode::Block);
    let policy = timeout(Duration::from_secs(3), read_policy(&url))
        .await
        .expect("a policy read past the cache's lifetime waited on the blocked language")
        .into_result()
        .expect("a policy read past the cache's lifetime failed");
    assert_eq!(policy.mode, "designated");

    // The language serves again and a remote member is there: the expired
    // entry does not hide it.
    stub.set(StubMode::Serve);
    stub.set_peers(vec!["did:key:remote".to_string()]);
    sleep(Duration::from_secs(61)).await;
    let policy = timeout(Duration::from_secs(3), read_policy(&url))
        .await
        .expect("the policy read waited")
        .into_result()
        .expect("the policy read failed");
    assert_eq!(policy.mode, "mesh", "the expired entry hid a remote member");

    stub.stop().await;
    crate::perspectives::unregister_perspective(&instance.uuid);
}

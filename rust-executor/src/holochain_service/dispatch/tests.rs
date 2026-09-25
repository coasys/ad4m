//! Concurrency contract of `run_dispatch_loop` (#1133), checked against a mock
//! `ZomeDispatch` so no conductor is needed.
//!
//! Invariants proved here:
//! 1. a cheap zome call is answered while a slow one is still running
//!    (`fast_call_is_not_blocked_by_slow_call`, the test that pins the fix);
//! 2. a lifecycle request waits for in-flight zome calls and holds back new ones
//!    (`install_app_waits_for_in_flight_zome_calls_and_blocks_new_ones`);
//! 3. at most `ZOME_CALL_CONCURRENCY` non-lifecycle requests run at once, and that many do
//!    (`concurrency_is_bounded`);
//! 4. a request admitted past its deadline is refused by the loop and never reaches the
//!    dispatcher (`expired_request_is_refused_without_reaching_the_service`), and so is one
//!    whose deadline passes while it waits for a permit
//!    (`request_expiring_while_waiting_for_a_permit_is_refused`);
//! 5. `Shutdown` is answered only after every in-flight request has finished
//!    (`shutdown_drains_in_flight_calls`);
//! 6. the `call_zome_function` op's deadline is the instant the op gives up: it starts only
//!    once the conductor is up (`op_deadline_starts_after_the_conductor_is_up`) and it
//!    reaches the loop (`op_passes_its_deadline_to_the_loop`).
//!
//! Requests go through the real `HolochainServiceInterface` methods, so what is tested is
//! what `holochain_service_extension.rs` and `unyt_service.rs` call. The mock reads the
//! zome call's `zome_name` as a sleep in milliseconds and `fn_name` as a label; each test
//! asserts the exact event order the mock recorded, not just that something errored.

use super::*;
use deno_core::error::AnyError;
use holochain::prelude::{
    AppBundleSource, ExternIO, InstallAppPayload, Signature, ZomeCallResponse,
};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex as StdMutex;
use std::task::Poll;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, Mutex};
use tokio::task::JoinHandle;

use crate::holochain_service::holochain_service_extension::call_zome_within;
use crate::holochain_service::interface::HolochainServiceInterface;

struct MockDispatch {
    /// Everything the mock ran, in the order it ran: `start:<label>`, `done:<label>`,
    /// `sign:<data>`, `install`, `shutdown`.
    events: StdMutex<Vec<String>>,
    /// Label of every zome call as it starts, so a test can wait for one to be in flight.
    started: mpsc::UnboundedSender<String>,
    zome_calls: AtomicUsize,
    in_flight: AtomicUsize,
    max_in_flight: AtomicUsize,
}

impl MockDispatch {
    fn record(&self, event: String) {
        self.events.lock().unwrap().push(event);
    }

    fn events(&self) -> Vec<String> {
        self.events.lock().unwrap().clone()
    }
}

#[async_trait]
impl ZomeDispatch for MockDispatch {
    async fn handle(&self, request: HolochainServiceRequest) {
        match request {
            HolochainServiceRequest::CallZomeFunction {
                zome_name,
                fn_name,
                response,
                ..
            } => {
                self.zome_calls.fetch_add(1, Ordering::SeqCst);
                let now = self.in_flight.fetch_add(1, Ordering::SeqCst) + 1;
                self.max_in_flight.fetch_max(now, Ordering::SeqCst);
                self.record(format!("start:{fn_name}"));
                let _ = self.started.send(fn_name.clone());

                let sleep_ms: u64 = zome_name.parse().expect("zome_name is the sleep in ms");
                tokio::time::sleep(Duration::from_millis(sleep_ms)).await;

                self.in_flight.fetch_sub(1, Ordering::SeqCst);
                self.record(format!("done:{fn_name}"));
                let value = ExternIO::encode(fn_name).unwrap();
                let _ = response.send(HolochainServiceResponse::CallZomeFunction(Ok(
                    ZomeCallResponse::Ok(value),
                )));
            }
            HolochainServiceRequest::InstallApp(_, response) => {
                tokio::time::sleep(Duration::from_millis(50)).await;
                self.record("install".into());
                let _ = response.send(HolochainServiceResponse::InstallApp(Err(anyhow!(
                    "mock install: no AppInfo to return"
                ))));
            }
            HolochainServiceRequest::Sign(data, response) => {
                self.record(format!("sign:{data}"));
                let _ = response.send(HolochainServiceResponse::Sign(Ok(Signature([0; 64]))));
            }
            HolochainServiceRequest::Shutdown(response) => {
                self.record("shutdown".into());
                let _ = response.send(HolochainServiceResponse::Shutdown(Ok(())));
            }
            other => {
                let name = other.name();
                other.refuse(anyhow!("mock does not handle {name}"));
            }
        }
    }
}

struct Harness {
    iface: HolochainServiceInterface,
    mock: Arc<MockDispatch>,
    started: Mutex<mpsc::UnboundedReceiver<String>>,
    loop_task: JoinHandle<()>,
}

impl Harness {
    fn start() -> Self {
        let (sender, receiver) = mpsc::unbounded_channel();
        let (_signals, signal_rx) = mpsc::unbounded_channel();
        let (started_tx, started_rx) = mpsc::unbounded_channel();
        let mock = Arc::new(MockDispatch {
            events: StdMutex::new(Vec::new()),
            started: started_tx,
            zome_calls: AtomicUsize::new(0),
            in_flight: AtomicUsize::new(0),
            max_in_flight: AtomicUsize::new(0),
        });
        let loop_task = tokio::spawn(run_dispatch_loop(receiver, mock.clone()));
        Self {
            iface: HolochainServiceInterface {
                sender,
                stream_receiver: Arc::new(Mutex::new(signal_rx)),
            },
            mock,
            started: Mutex::new(started_rx),
            loop_task,
        }
    }

    /// A zome call the mock will hold for `sleep_ms` before answering with `label`.
    fn zome_call(
        &self,
        label: &str,
        sleep_ms: u64,
        deadline: Option<Instant>,
    ) -> impl std::future::Future<Output = Result<ZomeCallResponse, AnyError>> + 'static {
        let iface = self.iface.clone();
        let label = label.to_string();
        async move {
            iface
                .call_zome_function(
                    "app".into(),
                    "cell".into(),
                    sleep_ms.to_string(),
                    label,
                    None,
                    deadline,
                )
                .await
        }
    }

    /// Blocks until the mock has started running the zome call labelled `label`.
    async fn wait_started(&self, label: &str) {
        let mut started = self.started.lock().await;
        tokio::time::timeout(Duration::from_secs(5), async {
            while let Some(l) = started.recv().await {
                if l == label {
                    return;
                }
            }
            panic!("started channel closed before {label} started");
        })
        .await
        .unwrap_or_else(|_| panic!("{label} did not start within 5 s"));
    }

    fn install_app(&self) -> impl std::future::Future<Output = Result<(), String>> + 'static {
        let iface = self.iface.clone();
        async move {
            let payload = InstallAppPayload {
                source: AppBundleSource::Path("/nonexistent/mock.happ".into()),
                agent_key: None,
                installed_app_id: Some("mock-app".into()),
                network_seed: None,
                roles_settings: None,
                ignore_genesis_failure: false,
                restore_from_dht: false,
            };
            iface
                .install_app(payload)
                .await
                .map(|_| ())
                .map_err(|e| e.to_string())
        }
    }
}

/// Spawns `request` after polling it once, so it is in the dispatch loop's channel when this
/// returns. `tokio::spawn` alone sends only on the task's first poll, so two requests spawned
/// back to back reach the channel in either order.
async fn enqueue<F>(request: F) -> JoinHandle<F::Output>
where
    F: std::future::Future + Send + 'static,
    F::Output: Send + 'static,
{
    let mut request = Box::pin(request);
    match futures::poll!(&mut request) {
        Poll::Ready(out) => tokio::spawn(async move { out }),
        Poll::Pending => tokio::spawn(request),
    }
}

fn label_of(response: ZomeCallResponse) -> String {
    match response {
        ZomeCallResponse::Ok(io) => io.decode().unwrap(),
        other => panic!("unexpected response {other:?}"),
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn fast_call_is_not_blocked_by_slow_call() {
    let h = Harness::start();

    let slow = tokio::spawn(h.zome_call("slow", 300, None));
    h.wait_started("slow").await;

    let fast = h.zome_call("fast", 0, None).await.unwrap();
    assert_eq!(label_of(fast), "fast");

    // The verdict: "fast" was answered while "slow" was still running.
    assert_eq!(
        h.mock.events(),
        vec!["start:slow", "start:fast", "done:fast"],
        "fast call must not queue behind the slow one"
    );

    assert_eq!(label_of(slow.await.unwrap().unwrap()), "slow");
    assert_eq!(
        h.mock.events(),
        vec!["start:slow", "start:fast", "done:fast", "done:slow"]
    );
}

/// A keystore request must not wait for a zome call permit, nor behind zome calls queued
/// for one: with every permit held and another call queued, `Sign` is still answered at once.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn sign_is_not_blocked_by_a_full_zome_call_pool() {
    let h = Harness::start();
    let busy = fill_permits(&h, 500).await;
    let queued = enqueue(h.zome_call("queued", 0, None)).await;

    let signature = tokio::time::timeout(Duration::from_millis(250), h.iface.sign("hi".into()))
        .await
        .expect("Sign must be answered while every permit is held")
        .unwrap();
    assert_eq!(signature, Signature([0; 64]));

    // The verdict: "sign" ran before any busy call finished and before "queued" started.
    let events = h.mock.events();
    let sign_at = events.iter().position(|e| e == "sign:hi").unwrap();
    assert!(
        !events[..sign_at]
            .iter()
            .any(|e| e.starts_with("done:") || e == "start:queued"),
        "Sign must not wait behind the zome call pool: {events:?}"
    );

    for call in busy {
        call.await.unwrap().unwrap();
    }
    assert_eq!(label_of(queued.await.unwrap().unwrap()), "queued");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn install_app_waits_for_in_flight_zome_calls_and_blocks_new_ones() {
    let h = Harness::start();

    let a = tokio::spawn(h.zome_call("a", 200, None));
    h.wait_started("a").await;

    // Queued in this order; the loop dequeues in this order.
    let install = enqueue(h.install_app()).await;
    let b = enqueue(h.zome_call("b", 0, None)).await;

    assert_eq!(label_of(a.await.unwrap().unwrap()), "a");
    let install_err = install.await.unwrap().unwrap_err();
    assert!(
        install_err.contains("mock install"),
        "install must be answered by the dispatcher, got: {install_err}"
    );
    assert_eq!(label_of(b.await.unwrap().unwrap()), "b");

    assert_eq!(
        h.mock.events(),
        vec!["start:a", "done:a", "install", "start:b", "done:b"],
        "install must run after a finished and before b starts"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrency_is_bounded() {
    let h = Harness::start();
    let total = 2 * ZOME_CALL_CONCURRENCY;

    let calls: Vec<_> = (0..total)
        .map(|i| tokio::spawn(h.zome_call(&format!("c{i}"), 300, None)))
        .collect();
    for call in calls {
        call.await.unwrap().unwrap();
    }

    assert_eq!(h.mock.zome_calls.load(Ordering::SeqCst), total);
    assert_eq!(
        h.mock.max_in_flight.load(Ordering::SeqCst),
        ZOME_CALL_CONCURRENCY,
        "exactly ZOME_CALL_CONCURRENCY calls must run at once: the bound holds and is reached"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn expired_request_is_refused_without_reaching_the_service() {
    let h = Harness::start();

    let past = Instant::now() - Duration::from_millis(10);
    let err = h.zome_call("stale", 0, Some(past)).await.unwrap_err();

    // The verdict: the loop's expiry check answered, not the dispatcher.
    assert!(
        err.to_string().contains("expired after"),
        "expected the expiry refusal, got: {err}"
    );
    assert_eq!(
        h.mock.zome_calls.load(Ordering::SeqCst),
        0,
        "an expired request must never reach the dispatcher"
    );
    assert!(h.mock.events().is_empty());

    // The loop is still serving.
    let live = h.zome_call("live", 0, None).await.unwrap();
    assert_eq!(label_of(live), "live");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn request_expiring_while_waiting_for_a_permit_is_refused() {
    let h = Harness::start();

    let busy = fill_permits(&h, 300).await;

    // Dequeued at once, well before its deadline, but no permit frees up until the busy
    // calls finish ~300 ms later: by then its caller has given up.
    let deadline = Instant::now() + Duration::from_millis(100);
    let err = h.zome_call("late", 0, Some(deadline)).await.unwrap_err();

    // The verdict: refused after the permit wait, never run.
    assert!(
        err.to_string().contains("expired after"),
        "expected the expiry refusal, got: {err}"
    );
    for call in busy {
        call.await.unwrap().unwrap();
    }
    assert!(
        !h.mock.events().contains(&"start:late".to_string()),
        "a request that expired waiting for a permit must never reach the dispatcher"
    );
    assert_eq!(
        h.mock.zome_calls.load(Ordering::SeqCst),
        ZOME_CALL_CONCURRENCY
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn shutdown_drains_in_flight_calls() {
    let h = Harness::start();

    let a = tokio::spawn(h.zome_call("a", 200, None));
    h.wait_started("a").await;

    h.iface.shutdown().await.unwrap();

    assert_eq!(
        h.mock.events(),
        vec!["start:a", "done:a", "shutdown"],
        "shutdown must be answered only after the in-flight call finished"
    );
    assert_eq!(label_of(a.await.unwrap().unwrap()), "a");

    // Shutdown ends the loop.
    tokio::time::timeout(Duration::from_secs(5), h.loop_task)
        .await
        .expect("loop must exit after Shutdown")
        .unwrap();
}

/// Takes every permit with a call the mock holds for `hold_ms`, and returns once all run.
async fn fill_permits(
    h: &Harness,
    hold_ms: u64,
) -> Vec<JoinHandle<Result<ZomeCallResponse, AnyError>>> {
    let busy: Vec<_> = (0..ZOME_CALL_CONCURRENCY)
        .map(|i| tokio::spawn(h.zome_call(&format!("busy{i}"), hold_ms, None)))
        .collect();
    tokio::time::timeout(Duration::from_secs(5), async {
        while h.mock.in_flight.load(Ordering::SeqCst) < ZOME_CALL_CONCURRENCY {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .expect("every permit must be taken within 5 s");
    busy
}

/// The op's deadline must not count the wait for the conductor: a conductor start longer
/// than the op's budget still leaves the call its whole budget.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn op_deadline_starts_after_the_conductor_is_up() {
    let h = Harness::start();
    let budget = Duration::from_millis(200);

    // The conductor comes up only after longer than the op's whole budget.
    let iface = h.iface.clone();
    let started = async move {
        tokio::time::sleep(Duration::from_millis(400)).await;
        Some(iface)
    };
    let response = call_zome_within(
        started,
        budget,
        "app".into(),
        "cell".into(),
        "0".into(),
        "after-start".into(),
        None,
    )
    .await
    .expect("a call made while the conductor starts must get its whole budget");

    assert_eq!(label_of(response), "after-start");
    assert_eq!(
        h.mock.events(),
        vec!["start:after-start", "done:after-start"]
    );
}

/// The op must hand its give-up instant to the loop, so a call still queued when the op
/// times out never reaches the conductor.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn op_passes_its_deadline_to_the_loop() {
    let h = Harness::start();
    let busy = fill_permits(&h, 300).await;

    // Waits for a permit for ~300 ms, but the op gives up after 100 ms.
    let iface = h.iface.clone();
    let err = call_zome_within(
        async move { Some(iface) },
        Duration::from_millis(100),
        "app".into(),
        "cell".into(),
        "0".into(),
        "late".into(),
        None,
    )
    .await
    .unwrap_err();
    assert!(err.to_string().contains("Timeout"), "got: {err}");

    for call in busy {
        call.await.unwrap().unwrap();
    }
    // Queued after "late", so answered only once the loop has admitted or refused it.
    assert_eq!(
        label_of(h.zome_call("probe", 0, None).await.unwrap()),
        "probe"
    );

    // The verdict: the loop refused "late" at its deadline instead of running it.
    let events = h.mock.events();
    assert!(
        !events.contains(&"start:late".to_string()),
        "a call whose op already timed out must never reach the dispatcher: {events:?}"
    );
}

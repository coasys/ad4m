//! Calls into one language runtime: their deadline, their order and the agent each runs as.
//!
//! Everything here runs JS through a real `LanguageRuntimeHandle` (its own thread, Deno
//! isolate and request loop), the path every language call takes, except the test that a
//! refused call is not queued: it reads the channel, so it plays the runtime itself.
//!
//! The `#[ignore]` tests are the benchmarks behind the deadline change. Run them with
//! `cargo test --release -p ad4m-executor --lib call_deadline_bench -- --ignored --nocapture --test-threads=1`,
//! but `call_deadline_bench_e2` on its own: its looping threads spin until the process exits.

use super::language_runtime_handle::LanguageRuntimeHandle;
use crate::agent::AgentContext;
use std::time::{Duration, Instant};

/// The process-wide V8 start: a second `Once` here set the flags after another test had
/// started V8, which aborts the process (`Check failed: !IsFrozen()`).
fn init_v8_platform() {
    crate::test_utils::init_v8_platform();
}

fn spawn_runtime(name: &str) -> (LanguageRuntimeHandle, tempfile::TempDir) {
    init_v8_platform();
    let dir = tempfile::tempdir().unwrap();
    let handle =
        LanguageRuntimeHandle::spawn(name.to_string(), dir.path().to_path_buf(), false).unwrap();
    (handle, dir)
}

/// A JS expression that resolves after `ms` without using the CPU: a file-storage upload,
/// a zome call or an HTTP round trip as the language sees it.
fn sleep_js(ms: u64) -> String {
    format!("await new Promise((resolve) => setTimeout(resolve, {ms}))")
}

/// `n` calls, sent in order; each logs its index when it starts and when it ends, after
/// awaiting a latency that differs from call to call. Returns the start and end logs.
async fn ordered_burst(handle: &LanguageRuntimeHandle, n: usize) -> (Vec<usize>, Vec<usize>) {
    handle
        .execute("(globalThis.__started = [], globalThis.__ended = [], 0)".into())
        .await
        .unwrap();
    let calls = (0..n).map(|i| {
        // "+x" on even calls, "-x" on odd ones, with latencies of 0..47 ms out of order.
        let op = if i % 2 == 0 { "+x" } else { "-x" };
        let latency = (i * 37) % 48;
        handle.execute(format!(
            r#"(async () => {{
                globalThis.__started.push({i});
                {sleep};
                globalThis.__ended.push({i});
                return "{op}";
            }})()"#,
            sleep = sleep_js(latency as u64)
        ))
    });
    for result in futures::future::join_all(calls).await {
        result.expect("every call in the burst succeeds");
    }
    let raw = handle
        .execute("JSON.stringify([globalThis.__started, globalThis.__ended])".into())
        .await
        .unwrap();
    serde_json::from_str(&raw).unwrap()
}

// ---------------------------------------------------------------------------
// Contract
// ---------------------------------------------------------------------------

/// Bounds a test that would otherwise wait for ever on a call that never returns.
async fn within<T>(limit: Duration, what: &str, fut: impl std::future::Future<Output = T>) -> T {
    tokio::time::timeout(limit, fut)
        .await
        .unwrap_or_else(|_| panic!("{what} did not return within {limit:?}"))
}

/// Two managed users, Alice and Bob, with their own keys. Returns their DIDs.
fn two_users() -> (String, String) {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let user = |name: &str| {
        crate::test_utils::create_test_user(&format!("{name}.{}@example.org", uuid::Uuid::new_v4()))
    };
    (user("alice"), user("bob"))
}

/// When Alice's caller drops its call in the identity scenario: before the call ends, as an
/// outer timeout or a dropped request handler does.
const ALICE_GIVES_UP: Duration = Duration::from_millis(1000);

struct SignerOutcome {
    /// `None`: her caller dropped the call before it ended.
    alice: Option<Result<String, String>>,
    bob: Result<String, String>,
    /// The author of the expression Alice's call signed after its await.
    signer: String,
}

/// Alice's call awaits 1.2 s, then signs an expression; her caller drops the call at
/// `ALICE_GIVES_UP`. Bob's call is sent then and awaits 0.4 s. Returns both outcomes and the
/// agent Alice's call signed as.
async fn signer_of_a_call_whose_caller_has_gone(
    handle: &LanguageRuntimeHandle,
    alice: &str,
    bob: &str,
) -> SignerOutcome {
    handle
        .execute("(globalThis.__signer = null, 0)".into())
        .await
        .unwrap();
    let alice_outcome = tokio::time::timeout(
        ALICE_GIVES_UP,
        handle.execute_with_context(
            format!(
                r#"(async () => {{
                    {};
                    globalThis.__signer = AGENT.createSignedExpression({{ note: "from alice" }}).author;
                    return "signed";
                }})()"#,
                sleep_js(1200)
            ),
            AgentContext::managed(alice.to_string()),
        ),
    )
    .await
    .ok();
    let bob_outcome = handle
        .execute_with_context(
            format!("({}, AGENT.did())", sleep_js(400)),
            AgentContext::managed(bob.to_string()),
        )
        .await;
    let signer = handle
        .execute("String(globalThis.__signer)".into())
        .await
        .unwrap()
        .trim()
        .to_string();
    SignerOutcome {
        alice: alice_outcome,
        bob: bob_outcome,
        signer,
    }
}

/// The hung call keeps its language and its own caller: nothing can tell it from a slow call.
/// The call queued behind it gets an error once the hung call has run for the deadline.
#[tokio::test(flavor = "multi_thread")]
async fn a_call_queued_behind_a_hung_call_gets_an_error_at_the_deadline() {
    let (handle, _dir) = spawn_runtime("QmDeadlineHung");
    let handle = handle.with_call_deadline(Duration::from_millis(500));

    let hung = {
        let handle = handle.clone();
        tokio::spawn(async move { handle.execute("await new Promise(() => {})".into()).await })
    };
    tokio::time::sleep(Duration::from_millis(50)).await;
    let t = Instant::now();
    let behind = within(
        Duration::from_secs(10),
        "the call queued behind it",
        async { handle.execute("1 + 1".into()).await },
    )
    .await;
    let waited = t.elapsed();

    let error = behind.expect_err("the call behind a hung call returns an error");
    assert!(error.contains("deadline"), "unexpected error: {error}");
    assert!(
        waited < Duration::from_millis(1500),
        "the call behind waited {waited:?}, past the hung call's deadline"
    );
    tokio::time::sleep(Duration::from_millis(500)).await;
    assert!(
        !hung.is_finished(),
        "the hung call's own caller got an answer: {:?}",
        hung.await
    );
    hung.abort();
}

/// A call that runs past the deadline answers its caller. The call queued behind it gets an
/// error and is never run, so its caller can retry without doing the work twice.
#[tokio::test(flavor = "multi_thread")]
async fn a_call_still_queued_at_its_deadline_is_not_run() {
    let (handle, _dir) = spawn_runtime("QmDeadlineQueued");
    let handle = handle.with_call_deadline(Duration::from_millis(500));

    // Holds the isolate's thread for 2 s: nothing on that thread can interrupt it.
    let busy = {
        let handle = handle.clone();
        tokio::spawn(async move {
            let t = Instant::now();
            let r = handle
                .execute(
                    "(() => { const end = Date.now() + 2000; while (Date.now() < end) {} return 1; })()"
                        .into(),
                )
                .await;
            (t.elapsed(), r)
        })
    };
    tokio::time::sleep(Duration::from_millis(50)).await;
    let t = Instant::now();
    let queued = within(Duration::from_secs(10), "the queued call", async {
        handle
            .execute("(globalThis.__queuedRan = true)".into())
            .await
    })
    .await;
    let queued_after = t.elapsed();
    let (busy_after, busy) = within(Duration::from_secs(10), "the busy call", busy)
        .await
        .unwrap();

    assert_eq!(busy.expect("the busy call answers its caller").trim(), "1");
    assert!(busy_after >= Duration::from_millis(2000), "{busy_after:?}");
    assert!(queued
        .expect_err("the call ahead ran past the deadline")
        .contains("deadline"));
    assert!(
        queued_after < Duration::from_millis(1500),
        "the queued caller waited {queued_after:?}, past the deadline"
    );
    let ran = within(Duration::from_secs(10), "the check", async {
        handle
            .execute("String(globalThis.__queuedRan === true)".into())
            .await
    })
    .await
    .unwrap();
    assert_eq!(ran.trim(), "false", "a call ran after its caller gave up");
}

/// A file-storage upload makes one zome call per chunk inside one call: its total time is not
/// bounded, and its caller must get the address.
#[tokio::test(flavor = "multi_thread")]
async fn a_call_that_keeps_making_progress_past_the_deadline_returns_its_result() {
    let (handle, _dir) = spawn_runtime("QmDeadlineLong");
    let handle = handle.with_call_deadline(Duration::from_millis(1000));

    let t = Instant::now();
    let upload = within(Duration::from_secs(10), "the upload", async {
        handle
            .execute(format!(
                r#"(async () => {{
                    for (let i = 0; i < 10; i++) {{ {}; }}
                    return "address";
                }})()"#,
                sleep_js(200)
            ))
            .await
    })
    .await;

    assert_eq!(
        upload.expect("the upload answers its caller").trim(),
        "address"
    );
    assert!(
        t.elapsed() >= Duration::from_millis(2000),
        "{:?}",
        t.elapsed()
    );
}

/// While the running call is past the deadline, a new call fails at once and is not queued:
/// retries during a hang do not pile up in the channel.
#[tokio::test]
async fn a_call_sent_while_the_running_call_is_past_the_deadline_is_not_queued() {
    let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
    let handle = LanguageRuntimeHandle::from_channel("QmDeadlinePileUp".into(), tx)
        .with_call_deadline(Duration::from_millis(500));
    let running = handle.running_request();

    // A call that has run for 100 ms: the next call is queued behind it.
    running.lock().unwrap().ordered = Some((u64::MAX, Instant::now() - Duration::from_millis(100)));
    let queued = tokio::spawn({
        let handle = handle.clone();
        async move { handle.execute("1 + 1".into()).await }
    });
    within(Duration::from_secs(1), "the queued request", rx.recv())
        .await
        .expect("the call is queued");

    // The same call, 1 s on.
    running.lock().unwrap().ordered = Some((u64::MAX, Instant::now() - Duration::from_secs(1)));
    let refused = within(Duration::from_secs(1), "the refused call", async {
        handle.execute("1 + 1".into()).await
    })
    .await;
    assert!(refused
        .expect_err("the running call is past the deadline")
        .contains("deadline"));
    assert!(rx.try_recv().is_err(), "the refused call was queued");
    queued.abort();
}

#[tokio::test(flavor = "multi_thread")]
async fn calls_to_one_language_start_and_finish_in_send_order() {
    let (handle, _dir) = spawn_runtime("QmDeadlineOrder");
    let (started, ended) = ordered_burst(&handle, 20).await;
    let sent: Vec<usize> = (0..20).collect();
    assert_eq!(
        started, sent,
        "the language received the calls out of order"
    );
    assert_eq!(ended, sent, "a later call finished before an earlier one");
}

/// A commit whose caller has gone (an outer timeout) is queued again with the diffs made since.
/// The call still running must land before that retry, not after it.
#[tokio::test(flavor = "multi_thread")]
async fn a_started_call_whose_caller_has_gone_ends_before_the_next_call_starts() {
    let (handle, _dir) = spawn_runtime("QmDeadlineLateCommit");
    handle
        .execute("(globalThis.__log = [], 0)".into())
        .await
        .unwrap();

    let late = tokio::time::timeout(
        Duration::from_millis(500),
        handle.execute(format!(
            "({}, globalThis.__log.push('+x landed'), 0)",
            sleep_js(800)
        )),
    )
    .await;
    assert!(late.is_err(), "the caller gave up before the commit ended");
    within(Duration::from_secs(10), "the retry", async {
        handle
            .execute("(globalThis.__log.push('+x -x landed'), 0)".into())
            .await
    })
    .await
    .expect("the retry runs");

    let log = handle
        .execute("JSON.stringify(globalThis.__log)".into())
        .await
        .unwrap();
    assert_eq!(log.trim(), r#"["+x landed","+x -x landed"]"#);
}

#[tokio::test(flavor = "multi_thread")]
async fn a_started_call_whose_caller_has_gone_signs_as_its_own_agent() {
    let (alice, bob) = two_users();
    let (handle, _dir) = spawn_runtime("QmDeadlineAgent");

    let outcome = within(
        Duration::from_secs(10),
        "the two calls",
        signer_of_a_call_whose_caller_has_gone(&handle, &alice, &bob),
    )
    .await;

    assert!(outcome.alice.is_none(), "alice's caller gave up first");
    assert_eq!(outcome.bob.expect("bob's call runs").trim(), bob);
    assert_eq!(
        outcome.signer, alice,
        "alice's call signed as another agent after her caller had gone"
    );
}

// ---------------------------------------------------------------------------
// Reads run beside other calls
// ---------------------------------------------------------------------------

/// An expression read does not wait for a slow call to the same language: a model query's
/// expression reads used to queue behind every upload and commit.
#[tokio::test(flavor = "multi_thread")]
async fn a_read_does_not_wait_for_a_slow_call() {
    let (handle, _dir) = spawn_runtime("QmConcurrentRead");
    let slow = {
        let handle = handle.clone();
        tokio::spawn(async move {
            handle
                .execute(format!("({}, \"slow\")", sleep_js(2000)))
                .await
        })
    };
    tokio::time::sleep(Duration::from_millis(100)).await;

    let t = Instant::now();
    let read = within(
        Duration::from_secs(10),
        "the read",
        handle.execute_read("1 + 1".into()),
    )
    .await;
    let read_after = t.elapsed();

    assert_eq!(read.expect("the read answers").trim(), "2");
    assert!(
        read_after < Duration::from_millis(500),
        "the read waited {read_after:?} for the slow call"
    );
    let slow = within(Duration::from_secs(10), "the slow call", slow)
        .await
        .unwrap();
    assert_eq!(slow.expect("the slow call answers").trim(), "slow");
}

/// Reads overlap each other: 8 reads that each await 500 ms end together.
#[tokio::test(flavor = "multi_thread")]
async fn reads_to_one_language_overlap() {
    let (handle, _dir) = spawn_runtime("QmConcurrentReads");
    let t = Instant::now();
    let reads = (0..8).map(|i| handle.execute_read(format!("({}, {i})", sleep_js(500))));
    let results = within(
        Duration::from_secs(10),
        "the reads",
        futures::future::join_all(reads),
    )
    .await;
    let wall = t.elapsed();

    for (i, result) in results.into_iter().enumerate() {
        assert_eq!(result.expect("every read answers").trim(), i.to_string());
    }
    assert!(
        wall < Duration::from_millis(1500),
        "8 reads of 500 ms took {wall:?}: they ran one at a time"
    );
}

/// A call that reads the agent it acts as, awaits `ms`, reads it again and signs. Logs `label`
/// in `globalThis.__ends` when it ends. Returns `[did before, did after, author]`.
fn agent_probe_js(label: &str, ms: u64) -> String {
    format!(
        r#"(async () => {{
            const before = AGENT.did();
            {sleep};
            const after = AGENT.did();
            const author = AGENT.createSignedExpression({{ note: "{label}" }}).author;
            globalThis.__ends.push("{label}");
            return JSON.stringify([before, after, author]);
        }})()"#,
        sleep = sleep_js(ms)
    )
}

/// Calls that overlap act each as its own agent, also after an await: Alice's call awaits
/// while a read as the main agent and a read as Bob start and end.
#[tokio::test(flavor = "multi_thread")]
async fn overlapping_calls_each_act_as_their_own_agent() {
    let (alice, bob) = two_users();
    let main = crate::agent::did();
    let (handle, _dir) = spawn_runtime("QmConcurrentAgents");
    handle
        .execute("(globalThis.__ends = [], 0)".into())
        .await
        .unwrap();

    // Alice: 0 to 800 ms. Main agent: 100 to 500 ms. Bob: 200 to 400 ms.
    let alice_call = handle.execute_with_context(
        agent_probe_js("alice", 800),
        AgentContext::managed(alice.clone()),
    );
    let main_call = async {
        tokio::time::sleep(Duration::from_millis(100)).await;
        handle.execute_read(agent_probe_js("main", 400)).await
    };
    let bob_call = async {
        tokio::time::sleep(Duration::from_millis(200)).await;
        handle
            .execute_read_with_context(
                agent_probe_js("bob", 200),
                AgentContext::managed(bob.clone()),
            )
            .await
    };
    let (alice_seen, main_seen, bob_seen) = within(
        Duration::from_secs(10),
        "the three calls",
        futures::future::join3(alice_call, main_call, bob_call),
    )
    .await;

    let seen = |result: Result<String, String>| -> Vec<String> {
        serde_json::from_str(result.expect("the call answers").trim()).unwrap()
    };
    assert_eq!(seen(alice_seen), vec![alice.clone(); 3], "alice's call");
    assert_eq!(
        seen(main_seen),
        vec![main.clone(); 3],
        "the main agent's call"
    );
    assert_eq!(seen(bob_seen), vec![bob.clone(); 3], "bob's call");
    let ends = handle
        .execute("JSON.stringify(globalThis.__ends)".into())
        .await
        .unwrap();
    assert_eq!(
        ends.trim(),
        r#"["bob","main","alice"]"#,
        "the calls did not overlap"
    );
}

/// Calls other than reads (commits, uploads, sync) still start and end in send order while
/// reads run beside them.
#[tokio::test(flavor = "multi_thread")]
async fn writes_stay_in_send_order_while_reads_run_beside_them() {
    let (handle, _dir) = spawn_runtime("QmConcurrentOrder");
    let reads = (0..20).map(|i| {
        let latency = (i * 29) % 40;
        handle.execute_read(format!("({}, {i})", sleep_js(latency)))
    });
    let ((started, ended), reads) = within(
        Duration::from_secs(20),
        "the burst",
        futures::future::join(ordered_burst(&handle, 20), futures::future::join_all(reads)),
    )
    .await;

    let sent: Vec<usize> = (0..20).collect();
    assert_eq!(
        started, sent,
        "the language received the writes out of order"
    );
    assert_eq!(ended, sent, "a later write finished before an earlier one");
    for (i, read) in reads.into_iter().enumerate() {
        assert_eq!(read.expect("every read answers").trim(), i.to_string());
    }
}

/// A read that cannot start by the deadline (a call holds the isolate's thread) gets an error
/// and is never run.
#[tokio::test(flavor = "multi_thread")]
async fn a_read_that_cannot_start_by_the_deadline_gets_an_error_and_is_not_run() {
    let (handle, _dir) = spawn_runtime("QmConcurrentReadDeadline");
    let handle = handle.with_call_deadline(Duration::from_millis(500));

    let busy = {
        let handle = handle.clone();
        tokio::spawn(async move {
            handle
                .execute(
                    "(() => { const end = Date.now() + 2000; while (Date.now() < end) {} return 1; })()"
                        .into(),
                )
                .await
        })
    };
    tokio::time::sleep(Duration::from_millis(50)).await;
    let t = Instant::now();
    let read = within(Duration::from_secs(10), "the read", async {
        handle
            .execute_read("(globalThis.__readRan = true)".into())
            .await
    })
    .await;
    let read_after = t.elapsed();
    within(Duration::from_secs(10), "the busy call", busy)
        .await
        .unwrap()
        .expect("the busy call answers its caller");

    assert!(read
        .expect_err("the read could not start by its deadline")
        .contains("deadline"));
    assert!(
        read_after < Duration::from_millis(1500),
        "the read waited {read_after:?}, past the deadline"
    );
    let ran = handle
        .execute("String(globalThis.__readRan === true)".into())
        .await
        .unwrap();
    assert_eq!(ran.trim(), "false", "a read ran after its caller gave up");
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

const RUNS: usize = 5;

struct Stats {
    median: Duration,
    min: Duration,
    max: Duration,
}

fn stats(mut samples: Vec<Duration>) -> Stats {
    samples.sort();
    Stats {
        median: samples[samples.len() / 2],
        min: samples[0],
        max: samples[samples.len() - 1],
    }
}

fn report(scenario: &str, metric: &str, samples: Vec<Duration>) {
    let n = samples.len();
    let s = stats(samples);
    println!(
        "BENCH {scenario} | {metric} | n={n} | median {:.3} s | min {:.3} s | max {:.3} s",
        s.median.as_secs_f64(),
        s.min.as_secs_f64(),
        s.max.as_secs_f64()
    );
}

/// A: 16 slow calls in flight at once, each awaiting 1.4 s (an upload, not CPU).
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_a_concurrent_slow_reads() {
    const N: usize = 16;
    let mut wall = Vec::new();
    let mut per_call = Vec::new();
    for run in 0..RUNS {
        let (handle, _dir) = spawn_runtime(&format!("QmBenchA{run}"));
        let start = Instant::now();
        let calls = (0..N).map(|_| {
            let handle = handle.clone();
            async move {
                let t = Instant::now();
                handle
                    .execute(format!("({}, \"stored\")", sleep_js(1400)))
                    .await
                    .unwrap();
                t.elapsed()
            }
        });
        let latencies = futures::future::join_all(calls).await;
        wall.push(start.elapsed());
        per_call.extend(latencies);
        let _ = handle.teardown().await;
    }
    report("A", "wall time for 16 calls", wall);
    report("A", "latency per call", per_call);
}

/// B: a call that never resolves, then a fast call to the same language.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_b_head_of_line() {
    // Longer than the deadline, so "after" shows the deadline and "before" shows the hang.
    const CAP: Duration = Duration::from_secs(70);
    let mut fast = Vec::new();
    let mut hung = Vec::new();
    let mut retry_answered = Vec::new();
    let mut retry_wall = Vec::new();
    for run in 0..RUNS {
        let (handle, _dir) = spawn_runtime(&format!("QmBenchB{run}"));
        let start = Instant::now();
        let hung_call = {
            let handle = handle.clone();
            tokio::spawn(async move {
                let r = handle.execute("await new Promise(() => {})".into()).await;
                (start.elapsed(), r)
            })
        };
        tokio::time::sleep(Duration::from_millis(50)).await;
        let t = Instant::now();
        let fast_outcome = tokio::time::timeout(CAP, handle.execute("1 + 1".into())).await;
        let fast_latency = t.elapsed();
        // Then 100 more calls, as retries arrive while the call still hangs: how many get an
        // answer within 1 s.
        let t = Instant::now();
        let retries =
            futures::future::join_all((0..100).map(|_| {
                tokio::time::timeout(Duration::from_secs(1), handle.execute("1 + 1".into()))
            }))
            .await;
        let answered = retries.iter().filter(|r| r.is_ok()).count();
        retry_answered.push(answered);
        retry_wall.push(t.elapsed());
        println!("B run {run}: {answered}/100 later calls answered within 1 s");
        let hung_outcome =
            tokio::time::timeout(CAP.saturating_sub(start.elapsed()), hung_call).await;
        println!(
            "B run {run}: fast call {:?} after {:.3} s; hung call {}",
            fast_outcome
                .as_ref()
                .map(|r| r.as_ref().map(|s| s.trim().to_string())),
            fast_latency.as_secs_f64(),
            match &hung_outcome {
                Ok(Ok((after, r))) =>
                    format!("returned {:?} after {:.3} s", r, after.as_secs_f64()),
                Ok(Err(e)) => format!("task failed: {e}"),
                Err(_) => format!("still pending after {:.0} s", CAP.as_secs_f64()),
            }
        );
        fast.push(fast_latency);
        if let Ok(Ok((after, _))) = hung_outcome {
            hung.push(after);
        }
    }
    report("B", "fast call latency behind a hung call", fast);
    report("B", "100 later calls, wall time (1 s cap)", retry_wall);
    println!("BENCH B | later calls answered within 1 s per run | {retry_answered:?}");
    if hung.is_empty() {
        println!("BENCH B | hung call | never returned within {CAP:?} in any run");
    } else {
        report("B", "hung call returns after", hung);
    }
}

/// C: 20 commits (+x / -x pairs) with varying latency: the order the language starts and
/// finishes them must be the send order.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_c_commit_order() {
    const N: usize = 20;
    let mut wall = Vec::new();
    for run in 0..RUNS {
        let (handle, _dir) = spawn_runtime(&format!("QmBenchC{run}"));
        let start = Instant::now();
        let (started, ended) = ordered_burst(&handle, N).await;
        wall.push(start.elapsed());
        let expected: Vec<usize> = (0..N).collect();
        println!(
            "C run {run}: received in send order: {}, finished in send order: {}",
            started == expected,
            ended == expected
        );
        assert_eq!(started, expected);
        assert_eq!(ended, expected);
        let _ = handle.teardown().await;
    }
    report("C", "wall time for 20 ordered commits", wall);
}

/// D: a slow call on one language and a fast call on another.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_d_two_languages() {
    let mut fast = Vec::new();
    for run in 0..RUNS {
        let (slow_lang, _d1) = spawn_runtime(&format!("QmBenchD{run}slow"));
        let (fast_lang, _d2) = spawn_runtime(&format!("QmBenchD{run}fast"));
        let slow = {
            let slow_lang = slow_lang.clone();
            tokio::spawn(async move { slow_lang.execute(format!("({}, 1)", sleep_js(5000))).await })
        };
        tokio::time::sleep(Duration::from_millis(50)).await;
        let t = Instant::now();
        fast_lang.execute("1 + 1".into()).await.unwrap();
        fast.push(t.elapsed());
        slow.await.unwrap().unwrap();
        let _ = slow_lang.teardown().await;
        let _ = fast_lang.teardown().await;
    }
    report(
        "D",
        "fast call on language 2 while language 1 is busy 5 s",
        fast,
    );
}

/// G: one healthy call whose total work passes the 60 s deadline: 70 sequential awaits of
/// 1 s, as a file-storage upload makes one zome call per chunk. Then a read of how many
/// "chunks" were stored.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_g_long_healthy_call() {
    const CHUNKS: usize = 70;
    let mut latency = Vec::new();
    let mut ok = 0;
    for run in 0..RUNS {
        let (handle, _dir) = spawn_runtime(&format!("QmBenchG{run}"));
        handle
            .execute("(globalThis.__chunks = 0, 0)".into())
            .await
            .unwrap();
        let t = Instant::now();
        let upload = handle
            .execute(format!(
                r#"(async () => {{
                    for (let i = 0; i < {CHUNKS}; i++) {{ {}; globalThis.__chunks++; }}
                    return "address";
                }})()"#,
                sleep_js(1000)
            ))
            .await;
        latency.push(t.elapsed());
        let stored = handle.execute("globalThis.__chunks".into()).await;
        println!(
            "G run {run}: caller got {:?} after {:.3} s; chunks stored {:?}",
            upload.as_ref().map(|s| s.trim().to_string()),
            t.elapsed().as_secs_f64(),
            stored.as_ref().map(|s| s.trim().to_string())
        );
        if upload.is_ok() {
            ok += 1;
        }
        let _ = handle.teardown().await;
    }
    report("G", "caller of a 70 x 1 s healthy call", latency);
    println!("BENCH G | caller got Ok in {ok}/{RUNS} runs");
}

/// E: a call that keeps the isolate's CPU busy for 3 s, then a fast call to the same
/// language.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_e_cpu_bound() {
    let mut fast = Vec::new();
    for run in 0..RUNS {
        let (handle, _dir) = spawn_runtime(&format!("QmBenchE{run}"));
        let busy = {
            let handle = handle.clone();
            tokio::spawn(async move {
                handle
                    .execute(
                        "(() => { const end = Date.now() + 3000; while (Date.now() < end) {} return 1; })()"
                            .into(),
                    )
                    .await
            })
        };
        tokio::time::sleep(Duration::from_millis(50)).await;
        let t = Instant::now();
        handle.execute("1 + 1".into()).await.unwrap();
        fast.push(t.elapsed());
        busy.await.unwrap().unwrap();
        let _ = handle.teardown().await;
    }
    report("E", "fast call behind a 3 s CPU-bound call", fast);
}

/// E2: a call that loops on the CPU for ever, then a fast call to the same language. Run it
/// on its own and last: the looping threads spin until the test process exits.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_e2_endless_cpu_loop() {
    const CAP: Duration = Duration::from_secs(70);
    let mut fast = Vec::new();
    for run in 0..RUNS {
        let (handle, dir) = spawn_runtime(&format!("QmBenchE2{run}"));
        let looping = {
            let handle = handle.clone();
            tokio::spawn(
                async move { handle.execute("(() => { while (true) {} })()".into()).await },
            )
        };
        tokio::time::sleep(Duration::from_millis(50)).await;
        let t = Instant::now();
        let outcome = tokio::time::timeout(CAP, handle.execute("1 + 1".into())).await;
        fast.push(t.elapsed());
        println!(
            "E2 run {run}: fast call {:?} after {:.3} s",
            outcome,
            t.elapsed().as_secs_f64()
        );
        drop(looping);
        std::mem::forget(dir);
    }
    report("E2", "fast call behind an endless CPU loop", fast);
}

/// F: Alice's caller drops her call while it awaits; Bob's call is sent then. Counts the agent
/// Alice's call signed as when it resumed.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_f_identity() {
    let (alice, bob) = two_users();
    let (mut as_alice, mut as_bob, mut other) = (0, 0, 0);
    for run in 0..RUNS {
        let (handle, _dir) = spawn_runtime(&format!("QmBenchF{run}"));
        let o = signer_of_a_call_whose_caller_has_gone(&handle, &alice, &bob).await;
        println!(
            "F run {run}: alice's caller got {:?}; bob's call {:?}; alice's call signed as {}",
            o.alice,
            o.bob.as_ref().map(|s| s.trim() == bob),
            if o.signer == alice {
                "alice"
            } else if o.signer == bob {
                "bob"
            } else {
                o.signer.as_str()
            }
        );
        if o.signer == alice {
            as_alice += 1
        } else if o.signer == bob {
            as_bob += 1
        } else {
            other += 1
        }
        let _ = handle.teardown().await;
    }
    println!("BENCH F | n={RUNS} | signed as alice {as_alice} | as bob {as_bob} | other {other}");
}

/// H: a fast call sent 100 ms into a 2 s call to the same language: its latency as an ordered
/// call (the path every call took before reads) and as a read.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_h_fast_read_beside_a_slow_call() {
    let mut as_ordered = Vec::new();
    let mut as_read = Vec::new();
    for run in 0..RUNS {
        for read in [false, true] {
            let (handle, _dir) = spawn_runtime(&format!("QmBenchH{run}{read}"));
            let slow = {
                let handle = handle.clone();
                tokio::spawn(async move {
                    handle
                        .execute(format!("({}, \"slow\")", sleep_js(2000)))
                        .await
                })
            };
            tokio::time::sleep(Duration::from_millis(100)).await;
            let t = Instant::now();
            let fast = if read {
                handle.execute_read("1 + 1".into()).await
            } else {
                handle.execute("1 + 1".into()).await
            };
            let latency = t.elapsed();
            assert_eq!(fast.unwrap().trim(), "2");
            slow.await.unwrap().unwrap();
            if read {
                as_read.push(latency);
            } else {
                as_ordered.push(latency);
            }
            let _ = handle.teardown().await;
        }
    }
    report("H", "fast call beside a 2 s call, ordered", as_ordered);
    report("H", "fast call beside a 2 s call, read", as_read);
}

//! Calls into one language runtime: their deadline, their order and the agent each runs as.
//!
//! Everything here runs JS through a real `LanguageRuntimeHandle` (its own thread, Deno
//! isolate and request loop), the path every language call takes.
//!
//! The `#[ignore]` tests are the benchmarks behind the deadline change. Run them with
//! `cargo test --release -p ad4m-executor --lib call_deadline_bench -- --ignored --nocapture --test-threads=1`,
//! but `call_deadline_bench_e2` on its own: its looping threads spin until the process exits.

use super::language_runtime_handle::LanguageRuntimeHandle;
use crate::agent::AgentContext;
use std::time::{Duration, Instant};

fn init_v8_platform() {
    static V8_INIT: std::sync::Once = std::sync::Once::new();
    V8_INIT.call_once(|| {
        deno_core::v8::V8::set_flags_from_string("--max-opt=0");
        deno_core::JsRuntime::init_platform(None);
    });
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

/// The deadline in the identity scenario; Alice's call awaits past it, Bob's call is sent at
/// it and awaits across the moment Alice's call resumes.
const DEADLINE_F: Duration = Duration::from_millis(1000);

struct SignerOutcome {
    alice: Result<String, String>,
    bob: Result<String, String>,
    /// The author of the expression Alice's call signed after its await.
    signer: String,
}

/// Alice's call awaits 1.2 s (past `DEADLINE_F`), then signs an expression. Bob's call is
/// sent when Alice's caller gets its answer and awaits 0.4 s. Returns both outcomes and the
/// agent Alice's call signed as.
async fn signer_of_a_call_past_its_deadline(
    handle: &LanguageRuntimeHandle,
    alice: &str,
    bob: &str,
) -> SignerOutcome {
    handle
        .execute("(globalThis.__signer = null, 0)".into())
        .await
        .unwrap();
    let alice_outcome = handle
        .execute_with_context(
            format!(
                r#"(async () => {{
                    {};
                    globalThis.__signer = AGENT.createSignedExpression({{ note: "from alice" }}).author;
                    return "signed";
                }})()"#,
                sleep_js(1200)
            ),
            AgentContext::managed(alice.to_string()),
        )
        .await;
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

#[tokio::test(flavor = "multi_thread")]
async fn a_call_that_never_resolves_fails_at_its_deadline_and_so_does_the_call_behind_it() {
    let (handle, _dir) = spawn_runtime("QmDeadlineHung");
    let handle = handle.with_call_deadline(Duration::from_millis(500));

    let hung = {
        let handle = handle.clone();
        tokio::spawn(async move { handle.execute("await new Promise(() => {})".into()).await })
    };
    tokio::time::sleep(Duration::from_millis(50)).await;
    let behind = within(
        Duration::from_secs(10),
        "the call queued behind it",
        async { handle.execute("1 + 1".into()).await },
    )
    .await;
    let hung = within(Duration::from_secs(10), "the hung call", hung)
        .await
        .unwrap();

    let error = hung.expect_err("a call that never resolves returns an error");
    assert!(error.contains("deadline"), "unexpected error: {error}");
    // The hung call still holds the language: the call behind it never starts.
    let error = behind.expect_err("the call behind a hung call returns an error");
    assert!(error.contains("deadline"), "unexpected error: {error}");
}

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
    let queued = within(Duration::from_secs(10), "the queued call", async {
        handle
            .execute("(globalThis.__queuedRan = true)".into())
            .await
    })
    .await;
    let (busy_after, busy) = within(Duration::from_secs(10), "the busy call", busy)
        .await
        .unwrap();

    assert!(busy
        .expect_err("the busy call outlives its deadline")
        .contains("deadline"));
    assert!(
        busy_after < Duration::from_millis(1500),
        "its caller waited {busy_after:?}, past the deadline"
    );
    assert!(queued
        .expect_err("its caller has gone")
        .contains("deadline"));
    // Wait until the isolate is free (the loop ends 2 s after it started), then ask whether
    // the queued call ran.
    tokio::time::sleep(Duration::from_millis(2000)).await;
    let ran = within(Duration::from_secs(10), "the check", async {
        handle
            .execute("String(globalThis.__queuedRan === true)".into())
            .await
    })
    .await
    .unwrap();
    assert_eq!(ran.trim(), "false", "a call ran after its caller gave up");
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

/// A commit whose caller gave up at the deadline is queued again with the diffs made since.
/// The call still running must land before that retry, not after it.
#[tokio::test(flavor = "multi_thread")]
async fn a_call_past_its_deadline_ends_before_the_next_call_starts() {
    let (handle, _dir) = spawn_runtime("QmDeadlineLateCommit");
    let handle = handle.with_call_deadline(Duration::from_millis(500));
    handle
        .execute("(globalThis.__log = [], 0)".into())
        .await
        .unwrap();

    let late = within(Duration::from_secs(10), "the late commit", async {
        handle
            .execute(format!(
                "({}, globalThis.__log.push('+x landed'), 0)",
                sleep_js(800)
            ))
            .await
    })
    .await;
    assert!(late.expect_err("its caller gave up").contains("deadline"));
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
async fn a_call_past_its_deadline_signs_as_its_own_agent_while_the_next_call_runs() {
    let (alice, bob) = two_users();
    let (handle, _dir) = spawn_runtime("QmDeadlineAgent");
    let handle = handle.with_call_deadline(DEADLINE_F);

    let outcome = within(
        Duration::from_secs(10),
        "the two calls",
        signer_of_a_call_past_its_deadline(&handle, &alice, &bob),
    )
    .await;

    assert!(outcome
        .alice
        .expect_err("alice's caller stops waiting at the deadline")
        .contains("deadline"));
    assert_eq!(outcome.bob.expect("bob's call runs").trim(), bob);
    assert_eq!(
        outcome.signer, alice,
        "alice's call signed as another agent after its deadline"
    );
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

/// F: Alice's call awaits past its deadline, then signs while Bob's call runs. Counts the
/// agent it signed as.
#[tokio::test(flavor = "multi_thread")]
#[ignore]
async fn call_deadline_bench_f_identity() {
    let (alice, bob) = two_users();
    let (mut as_alice, mut as_bob, mut other) = (0, 0, 0);
    for run in 0..RUNS {
        let (handle, _dir) = spawn_runtime(&format!("QmBenchF{run}"));
        let handle = handle.with_call_deadline(DEADLINE_F);
        let o = signer_of_a_call_past_its_deadline(&handle, &alice, &bob).await;
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

//! The web executor's timers (`rt::time` on wasm32, `src/web/time.rs`) under Node. The first
//! test is the regression for wasmtimer 0.4.5, under which every timer in the Worker stopped
//! after 19 cleared timeouts.
#![cfg(target_arch = "wasm32")]

use rust_executor::rt::time::{interval, sleep, sleep_until, timeout, Instant, TimerInstant};
use std::time::Duration;
use wasm_bindgen_test::wasm_bindgen_test;

#[wasm_bindgen_test]
async fn timers_still_fire_after_many_cancelled_sleeps() {
    for _ in 0..100 {
        // A timeout whose future wins drops its sleep before it fires: one cleared timeout.
        assert_eq!(timeout(Duration::from_secs(5), async { 1 }).await, Ok(1));
        drop(sleep(Duration::from_secs(5)));
        // A sleep polled once (armed), then dropped.
        let _ = timeout(Duration::from_millis(1), sleep(Duration::from_secs(5))).await;
    }
    let start = Instant::now();
    sleep(Duration::from_millis(20)).await;
    assert!(start.elapsed() >= Duration::from_millis(20));
}

#[wasm_bindgen_test]
async fn sleep_waits_about_as_long_as_asked() {
    let start = Instant::now();
    sleep(Duration::from_millis(50)).await;
    let took = start.elapsed();
    assert!(
        took >= Duration::from_millis(50) && took <= Duration::from_millis(200),
        "sleep(50ms) took {took:?}"
    );
}

#[wasm_bindgen_test]
async fn timeout_gives_up_on_a_slower_future() {
    let result = timeout(Duration::from_millis(10), sleep(Duration::from_secs(1))).await;
    assert_eq!(result.unwrap_err().to_string(), "deadline has elapsed");
    let result = timeout(Duration::from_secs(1), async { 3 }).await;
    assert_eq!(result.unwrap(), 3);
}

#[wasm_bindgen_test]
async fn sleep_until_and_interval_run_on_the_timer_clock() {
    let start = TimerInstant::now();
    sleep_until(start + Duration::from_millis(20)).await;
    assert!(TimerInstant::now() >= start + Duration::from_millis(20));

    let mut ticks = interval(Duration::from_millis(10));
    let first = ticks.tick().await; // at once, as tokio's
    let second = ticks.tick().await;
    assert_eq!(second - first, Duration::from_millis(10));
    assert!(start.elapsed() >= Duration::from_millis(30));
}

#[wasm_bindgen_test]
async fn many_sleeps_at_once_each_wake() {
    let start = Instant::now();
    let sleeps = (0..50).map(|i| sleep(Duration::from_millis(5 + i % 10)));
    futures::future::join_all(sleeps).await;
    assert!(start.elapsed() >= Duration::from_millis(14));
}

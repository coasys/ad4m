//! Clocks and timers on the JS host: the wasm32 half of `crate::rt::time`.
//!
//! - `Instant`, `SystemTime`, `UNIX_EPOCH`: `web_time`, which reads `performance.now()`
//!   and `Date.now()` (std's clocks panic on wasm32-unknown-unknown).
//! - The timers and their clock (`TimerInstant`): `wasmtimer`, the tokio timer API on
//!   `setTimeout`. A `sleep` wakes no earlier than asked, as tokio's does; the JS host
//!   decides how much later (a background tab throttles `setTimeout` to 1 s or more).

pub use wasmtimer::std::Instant as TimerInstant;
pub use wasmtimer::tokio::{interval, sleep, sleep_until, timeout};
pub use web_time::{Instant, SystemTime, UNIX_EPOCH};

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    async fn sleep_waits_about_as_long_as_asked() {
        let start = Instant::now();
        sleep(Duration::from_millis(50)).await;
        let took = start.elapsed();
        assert!(
            took >= Duration::from_millis(40) && took <= Duration::from_millis(200),
            "sleep(50ms) took {took:?}"
        );
    }

    #[wasm_bindgen_test]
    async fn timeout_gives_up_on_a_slower_future() {
        let result = timeout(Duration::from_millis(10), sleep(Duration::from_secs(1))).await;
        assert!(result.is_err());
        let result = timeout(Duration::from_secs(1), async { 3 }).await;
        assert_eq!(result.unwrap(), 3);
    }

    #[wasm_bindgen_test]
    async fn sleep_until_and_interval_run_on_the_timer_clock() {
        let start = TimerInstant::now();
        sleep_until(start + Duration::from_millis(20)).await;
        assert!(TimerInstant::now() >= start + Duration::from_millis(20));

        let mut ticks = interval(Duration::from_millis(10));
        ticks.tick().await; // the first tick is at once, as tokio's
        ticks.tick().await;
        assert!(start.elapsed() >= Duration::from_millis(30));
    }

    #[wasm_bindgen_test]
    fn the_clocks_read_without_panicking() {
        let a = Instant::now();
        let b = Instant::now();
        assert!(b >= a);
        let since_epoch = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        // After 2020-01-01: the clock is the host's wall clock, not zero.
        assert!(since_epoch > Duration::from_secs(1_577_836_800));
        let _ = TimerInstant::now();
    }
}

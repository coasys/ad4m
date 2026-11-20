//! Clocks and timers on the JS host: the wasm32 half of `crate::rt::time`.
//!
//! - `Instant`, `SystemTime`, `UNIX_EPOCH`: `web_time`, which reads `performance.now()`
//!   and `Date.now()` (std's clocks panic on wasm32-unknown-unknown).
//! - The timers (`sleep`, `sleep_until`, `timeout`, `interval`) and their clock
//!   (`TimerInstant`, the same monotonic clock as `Instant`): one `setTimeout` per pending
//!   sleep, cleared when the sleep is dropped. A `sleep` wakes no earlier than asked, as
//!   tokio's does; the JS host decides how much later (a background tab throttles
//!   `setTimeout` to 1 s or more).
//!
//! Not wasmtimer (S1-S8 used it): 0.4.5 coalesces every sleep onto one `setTimeout`, and each
//! timeout it clears leaks the callback with a reference to its timer. After 19 clears a
//! guard (`strong_count > 20`) stops rescheduling, and every timer in the Worker stops for
//! good: the executor's background loops did so 1.3 s after the first perspective started.
//!
//! A `Sleep` holds no JS value (the handle and callback sit in a thread-local table by id), so
//! it is `Send` like tokio's and fits the executor's `Send` futures.

use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::task::{Context, Poll};
use std::time::Duration;

use futures::task::AtomicWaker;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::{JsCast, JsValue};

pub use web_time::Instant as TimerInstant;
pub use web_time::{Instant, SystemTime, UNIX_EPOCH};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = setTimeout)]
    fn set_timeout(handler: &js_sys::Function, ms: i32) -> JsValue;
    #[wasm_bindgen(js_name = clearTimeout)]
    fn clear_timeout(handle: &JsValue);
}

/// A `setTimeout` that has not been cleared, with the callback it calls. The callback is
/// dropped (freed) when its sleep completes or is dropped, never from inside itself.
struct Armed {
    handle: JsValue,
    _callback: Closure<dyn FnMut()>,
}

thread_local! {
    static ARMED: RefCell<HashMap<u64, Armed>> = RefCell::new(HashMap::new());
}

static NEXT_TIMER: AtomicU64 = AtomicU64::new(0);

/// What a pending `setTimeout` shares with its sleep.
#[derive(Default)]
struct Fired {
    fired: AtomicBool,
    waker: AtomicWaker,
}

/// The future `sleep` and `sleep_until` return: ready at its deadline or later.
pub struct Sleep {
    deadline: TimerInstant,
    timer: Option<(u64, Arc<Fired>)>,
}

/// Waits until `duration` has passed.
pub fn sleep(duration: Duration) -> Sleep {
    sleep_until(TimerInstant::now() + duration)
}

/// Waits until `deadline`.
pub fn sleep_until(deadline: TimerInstant) -> Sleep {
    Sleep {
        deadline,
        timer: None,
    }
}

impl Sleep {
    /// The instant this sleep ends.
    pub fn deadline(&self) -> TimerInstant {
        self.deadline
    }

    /// Whether the deadline has passed.
    pub fn is_elapsed(&self) -> bool {
        TimerInstant::now() >= self.deadline
    }

    fn arm(&mut self, waker: &std::task::Waker) {
        let remaining = self.deadline.saturating_duration_since(TimerInstant::now());
        // Rounded up, plus 1 ms: the host's timer clock and `performance.now()` differ by up to
        // a millisecond, and a timeout that fires before the deadline costs a second one.
        let ms = (remaining.as_micros().div_ceil(1000) + 1).min(i32::MAX as u128) as i32;
        let fired = Arc::new(Fired::default());
        fired.waker.register(waker);
        let on_fire = fired.clone();
        let callback = Closure::<dyn FnMut()>::new(move || {
            on_fire.fired.store(true, Ordering::Release);
            on_fire.waker.wake();
        });
        let handle = set_timeout(callback.as_ref().unchecked_ref(), ms);
        let id = NEXT_TIMER.fetch_add(1, Ordering::Relaxed);
        ARMED.with(|armed| {
            armed.borrow_mut().insert(
                id,
                Armed {
                    handle,
                    _callback: callback,
                },
            )
        });
        self.timer = Some((id, fired));
    }

    fn disarm(&mut self) {
        if let Some((id, fired)) = self.timer.take() {
            if let Some(armed) = ARMED.with(|armed| armed.borrow_mut().remove(&id)) {
                if !fired.fired.load(Ordering::Acquire) {
                    clear_timeout(&armed.handle);
                }
            }
        }
    }
}

impl Future for Sleep {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
        let this = self.get_mut();
        if this.is_elapsed() {
            this.disarm();
            return Poll::Ready(());
        }
        match &this.timer {
            Some((_, fired)) if !fired.fired.load(Ordering::Acquire) => {
                fired.waker.register(cx.waker());
            }
            // Not armed yet, or the host fired before the deadline: arm (again).
            _ => {
                this.disarm();
                this.arm(cx.waker());
            }
        }
        Poll::Pending
    }
}

impl Drop for Sleep {
    fn drop(&mut self) {
        self.disarm();
    }
}

pub mod error {
    /// `timeout`'s error: the deadline passed before the future finished.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Elapsed(pub(super) ());

    impl std::fmt::Display for Elapsed {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("deadline has elapsed")
        }
    }

    impl std::error::Error for Elapsed {}
}

/// The future `timeout` returns.
pub struct Timeout<F> {
    future: Pin<Box<F>>,
    sleep: Sleep,
}

/// `future`'s output, or `Elapsed` if `duration` passes first.
pub fn timeout<F: Future>(duration: Duration, future: F) -> Timeout<F> {
    Timeout {
        future: Box::pin(future),
        sleep: sleep(duration),
    }
}

impl<F: Future> Future for Timeout<F> {
    type Output = Result<F::Output, error::Elapsed>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        if let Poll::Ready(output) = this.future.as_mut().poll(cx) {
            return Poll::Ready(Ok(output));
        }
        Pin::new(&mut this.sleep)
            .poll(cx)
            .map(|()| Err(error::Elapsed(())))
    }
}

/// Ticks every `period`, the first tick at once (tokio's `interval`, whose default
/// `MissedTickBehavior::Burst` this keeps: a late tick is followed by the missed ones).
pub struct Interval {
    period: Duration,
    next: TimerInstant,
}

/// An `Interval` with this period. Panics on a zero period, as tokio's does.
pub fn interval(period: Duration) -> Interval {
    assert!(period > Duration::ZERO, "`period` must be non-zero.");
    Interval {
        period,
        next: TimerInstant::now(),
    }
}

impl Interval {
    /// Waits for the next tick and returns its scheduled instant. Cancel-safe: dropping the
    /// future leaves the tick pending.
    pub async fn tick(&mut self) -> TimerInstant {
        sleep_until(self.next).await;
        let tick = self.next;
        self.next = tick + self.period;
        tick
    }

    /// The period.
    pub fn period(&self) -> Duration {
        self.period
    }
}

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

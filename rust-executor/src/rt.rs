//! The runtime the executor's code spawns tasks on.
//!
//! Native: tokio's own items, re-exported as they are, so `crate::rt::spawn` is
//! `tokio::spawn` and a `crate::rt::JoinHandle` is a `tokio::task::JoinHandle`.
//! wasm32 (the web executor): the JS event loop of the page or Worker, in
//! `crate::web::rt`, with the same names and methods.

#[cfg(not(target_arch = "wasm32"))]
pub use tokio::task::{spawn, spawn_blocking, AbortHandle, JoinError, JoinHandle};

#[cfg(target_arch = "wasm32")]
pub use crate::web::rt::{spawn, spawn_blocking, AbortHandle, JoinError, JoinHandle};

/// Clocks and timers.
///
/// Two clocks, because the executor reads two today and native must keep each:
/// - `Instant`, `SystemTime`, `UNIX_EPOCH`: the wall and monotonic clocks of `std::time`
///   (`web_time` on wasm32, which reads `performance.now()` and `Date.now()`).
/// - `TimerInstant`: the clock the timers run on, `tokio::time::Instant` on native. A
///   test that pauses tokio's clock (`tokio::time::pause`) pauses this one, never
///   `Instant`. `sleep_until` takes it.
///
/// The timers (`sleep`, `sleep_until`, `timeout`, `interval`) are tokio's on native and
/// `setTimeout`'s on wasm32 (`crate::web::time`).
pub mod time {
    pub use std::time::Duration;

    #[cfg(not(target_arch = "wasm32"))]
    pub use std::time::{Instant, SystemTime, UNIX_EPOCH};
    #[cfg(not(target_arch = "wasm32"))]
    pub use tokio::time::{interval, sleep, sleep_until, timeout, Instant as TimerInstant};

    #[cfg(target_arch = "wasm32")]
    pub use crate::web::time::{
        interval, sleep, sleep_until, timeout, Instant, SystemTime, TimerInstant, UNIX_EPOCH,
    };
}

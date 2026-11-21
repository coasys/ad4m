//! The runtime the executor's code spawns tasks on.
//!
//! Native: tokio's own items, re-exported as they are, so `crate::rt::spawn` is
//! `tokio::spawn`, a `crate::rt::JoinHandle` is a `tokio::task::JoinHandle` and a
//! `crate::rt::JoinSet` is a `tokio::task::JoinSet`.
//! wasm32 (the web executor): the JS event loop of the page or Worker, in
//! `crate::web::rt`, with the same names and methods.

#[cfg(not(target_arch = "wasm32"))]
pub use tokio::task::{spawn, spawn_blocking, AbortHandle, JoinError, JoinHandle, JoinSet};

#[cfg(target_arch = "wasm32")]
pub use crate::web::rt::{spawn, spawn_blocking, AbortHandle, JoinError, JoinHandle, JoinSet};

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

/// Files the executor keeps by path: the agent file and the agent profiles (`agent/mod.rs`).
///
/// Native: `std::fs`'s own functions, re-exported as they are. wasm32 (the web executor):
/// an in-memory map from path to bytes in `crate::web::fs`, with the same signatures. It
/// lives as long as the Worker: a reload starts with no files.
pub mod fs {
    #[cfg(not(target_arch = "wasm32"))]
    pub use std::fs::{create_dir_all, exists, read_to_string, write};

    #[cfg(target_arch = "wasm32")]
    pub use crate::web::fs::{create_dir_all, exists, read_to_string, write};
}

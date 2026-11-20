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

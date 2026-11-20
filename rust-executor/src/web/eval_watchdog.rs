//! `perspectives::sparql_store`'s eval watchdog on wasm32: a task on the JS event loop
//! instead of a thread (a Worker cannot start one). Declared by path inside `sparql_store`, so
//! it reaches the store's private stop record.
//!
//! It fires the stop on the request's cancel or on the deadline, whichever comes first, and
//! ends when the gate drops. One limit native does not have: an eval runs inline on the
//! Worker's one thread (`rt::spawn_blocking`), so this task cannot run while an eval does. A
//! deadline or a cancel that falls inside an eval stops only the evals after it (S15).

use std::time::Duration;

use tokio_util::sync::CancellationToken;

use super::{EvalStop, STOP_CANCELLED, STOP_DEADLINE};

pub(super) struct Watchdog {
    /// Cancelled when the gate drops: the task ends without firing.
    done: CancellationToken,
}

impl Watchdog {
    pub(super) fn arm(
        stop: EvalStop,
        deadline: Duration,
        cancel: Option<CancellationToken>,
    ) -> Watchdog {
        let done = CancellationToken::new();
        let gate_dropped = done.clone();
        crate::rt::spawn(async move {
            let request_cancelled = async {
                match &cancel {
                    Some(cancel) => cancel.cancelled().await,
                    None => std::future::pending().await,
                }
            };
            let kind = tokio::select! {
                biased;
                _ = gate_dropped.cancelled() => return,
                _ = request_cancelled => STOP_CANCELLED,
                _ = crate::rt::time::sleep(deadline) => STOP_DEADLINE,
            };
            stop.stop.mark(kind);
            stop.token.cancel();
        });
        Watchdog { done }
    }
}

impl Drop for Watchdog {
    fn drop(&mut self) {
        self.done.cancel();
    }
}

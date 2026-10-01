//! Per-module state helper. Languages compiled to WASM are single-instance
//! per isolate, so a thread_local RefCell is sufficient for per-Language state.

use std::cell::RefCell;
use std::rc::Rc;

use futures::lock::Mutex;

use crate::errors::{LanguageError, LanguageResult};

pub struct State<T: 'static> {
    inner: &'static std::thread::LocalKey<RefCell<Option<T>>>,
}

impl<T: 'static> State<T> {
    pub const fn new(key: &'static std::thread::LocalKey<RefCell<Option<T>>>) -> Self {
        Self { inner: key }
    }

    pub fn set(&self, value: T) {
        self.inner.with(|cell| *cell.borrow_mut() = Some(value));
    }

    pub fn clear(&self) {
        self.inner.with(|cell| *cell.borrow_mut() = None);
    }

    pub fn with<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        self.inner.with(|cell| {
            let borrow = cell.borrow();
            let v = borrow.as_ref().expect("State not initialized (call init() first)");
            f(v)
        })
    }

    pub fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        self.inner.with(|cell| {
            let mut borrow = cell.borrow_mut();
            let v = borrow.as_mut().expect("State not initialized (call init() first)");
            f(v)
        })
    }

    pub fn is_set(&self) -> bool {
        self.inner.with(|cell| cell.borrow().is_some())
    }
}

/// Declare a thread_local state slot for a Language struct.
///
/// Usage:
/// ```ignore
/// language_state!(STATE: MyLanguage);
/// ```
#[macro_export]
macro_rules! language_state {
    ($name:ident : $ty:ty) => {
        thread_local! {
            static __LANG_STATE_CELL: std::cell::RefCell<Option<$ty>> =
                std::cell::RefCell::new(None);
        }
        static $name: $crate::state::State<$ty> =
            $crate::state::State::new(&__LANG_STATE_CELL);
    };
}

/// The slot that holds the instance of an `ad4m_language!` language.
///
/// The runtime can call into a language while an earlier async call
/// still waits on a host import, so the slot does not assume serial
/// calls. An async lock guards the instance:
///   * async calls (`lock`) wait their turn, one after the other;
///   * sync calls (`try_with`) cannot wait, so they fail with a
///     "busy" `LanguageError` while an async call holds the lock;
///   * every call before `init()` (or after `teardown()`) fails with a
///     "not initialized" `LanguageError`.
///
/// No path panics, so one bad call cannot abort the WASM module.
pub struct LanguageSlot<T: 'static> {
    inner: RefCell<Option<Rc<Mutex<T>>>>,
}

impl<T: 'static> Default for LanguageSlot<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: 'static> LanguageSlot<T> {
    pub const fn new() -> Self {
        Self { inner: RefCell::new(None) }
    }

    /// Store a new instance. A call that still holds the old instance
    /// finishes against the old instance.
    pub fn set(&self, value: T) {
        *self.inner.borrow_mut() = Some(Rc::new(Mutex::new(value)));
    }

    /// The shared instance, or a "not initialized" error.
    pub fn get(&self, op: &str) -> LanguageResult<Rc<Mutex<T>>> {
        self.inner
            .borrow()
            .clone()
            .ok_or_else(|| not_initialized(op))
    }

    /// Run a sync call against the instance. Fails with a "busy" error
    /// when an async call holds the instance.
    pub fn try_with<R>(&self, op: &str, f: impl FnOnce(&mut T) -> R) -> LanguageResult<R> {
        let m = self.get(op)?;
        let mut guard = m.try_lock().ok_or_else(|| {
            LanguageError::transient(format!(
                "Language busy: {op} called while an async call is in progress"
            ))
        })?;
        Ok(f(&mut guard))
    }

    /// Remove the instance from the slot. Later calls fail with "not
    /// initialized". Returns `None` when the slot is empty.
    pub fn take(&self) -> Option<Rc<Mutex<T>>> {
        self.inner.borrow_mut().take()
    }

    pub fn is_set(&self) -> bool {
        self.inner.borrow().is_some()
    }
}

fn not_initialized(op: &str) -> LanguageError {
    LanguageError::internal(format!(
        "Language not initialized: {op} called before init() or after teardown()"
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::errors::ErrorCode;
    use futures::channel::oneshot;
    use futures::executor::block_on;
    use futures::future::join;

    #[derive(Debug)]
    struct Counter {
        log: Vec<&'static str>,
    }

    #[test]
    fn calls_before_init_error_instead_of_panicking() {
        let slot: LanguageSlot<Counter> = LanguageSlot::new();
        let e = slot.get("expressionGet").unwrap_err();
        assert!(matches!(e.code, ErrorCode::Internal));
        assert!(e.message.contains("not initialized"));
        let e = slot.try_with("interactions", |_| ()).unwrap_err();
        assert!(e.message.contains("not initialized"));
    }

    #[test]
    fn concurrent_async_calls_run_one_after_the_other() {
        let slot = LanguageSlot::new();
        slot.set(Counter { log: vec![] });
        let (tx, rx) = oneshot::channel::<()>();

        // First call holds the lock across an await that only completes
        // after the second call has started waiting.
        let first = async {
            let m = slot.get("first").unwrap();
            let mut g = m.lock().await;
            g.log.push("first:start");
            rx.await.unwrap();
            g.log.push("first:end");
        };
        let second = async {
            let m = slot.get("second").unwrap();
            tx.send(()).unwrap();
            let mut g = m.lock().await;
            g.log.push("second");
        };
        block_on(join(first, second));

        let m = slot.get("check").unwrap();
        let g = m.try_lock().unwrap();
        assert_eq!(g.log, vec!["first:start", "first:end", "second"]);
    }

    #[test]
    fn sync_call_while_async_call_holds_lock_returns_busy() {
        let slot = LanguageSlot::new();
        slot.set(Counter { log: vec![] });
        let m = slot.get("async").unwrap();
        let guard = block_on(m.lock());

        let e = slot.try_with("perspectiveCommit", |l| l.log.push("sync")).unwrap_err();
        assert!(matches!(e.code, ErrorCode::Transient));
        assert!(e.message.contains("busy"));

        drop(guard);
        slot.try_with("perspectiveCommit", |l| l.log.push("sync")).unwrap();
        assert_eq!(m.try_lock().unwrap().log, vec!["sync"]);
    }

    #[test]
    fn take_empties_slot_and_waits_for_in_flight_call() {
        let slot = LanguageSlot::new();
        slot.set(Counter { log: vec![] });
        let held = slot.get("async").unwrap();
        let guard = block_on(held.lock());

        let taken = slot.take().expect("instance");
        assert!(!slot.is_set());
        assert!(slot.get("expressionGet").is_err());
        assert!(taken.try_lock().is_none());
        drop(guard);
        assert!(block_on(taken.lock()).log.is_empty());
    }
}

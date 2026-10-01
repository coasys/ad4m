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
///     "not initialized" `LanguageError`;
///   * every call after a panic fails with a "panicked" `LanguageError`
///     (see `mark_panicked`).
///
/// No path in the slot panics. A panic in the language's own code still
/// aborts the WASM module, which does not unwind: a call that held the
/// lock never releases it. Without the panic record, every later async
/// call (and `teardown`) would wait for that lock forever.
pub struct LanguageSlot<T: 'static> {
    inner: RefCell<Option<Rc<Mutex<T>>>>,
    panicked: RefCell<Option<String>>,
}

impl<T: 'static> Default for LanguageSlot<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: 'static> LanguageSlot<T> {
    pub const fn new() -> Self {
        Self {
            inner: RefCell::new(None),
            panicked: RefCell::new(None),
        }
    }

    /// Store a new instance. A call that still holds the old instance
    /// finishes against the old instance.
    pub fn set(&self, value: T) {
        *self.inner.borrow_mut() = Some(Rc::new(Mutex::new(value)));
    }

    /// The shared instance, or a "panicked" / "not initialized" error.
    pub fn get(&self, op: &str) -> LanguageResult<Rc<Mutex<T>>> {
        if let Some(message) = self.panic_message() {
            return Err(LanguageError::internal(format!(
                "Language unusable: {op} refused because an earlier call panicked: {message}"
            )));
        }
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

    /// Record that the module panicked. Called from the panic hook, so it
    /// must not panic itself: it skips the record when the cell is
    /// borrowed and keeps the first message.
    pub fn mark_panicked(&self, message: &str) {
        if let Ok(mut p) = self.panicked.try_borrow_mut() {
            if p.is_none() {
                *p = Some(message.chars().take(500).collect());
            }
        }
    }

    pub fn has_panicked(&self) -> bool {
        self.panic_message().is_some()
    }

    fn panic_message(&self) -> Option<String> {
        self.panicked.try_borrow().ok().and_then(|p| p.clone())
    }
}

/// Install a panic hook that calls `mark` with the panic message, then
/// the hook that was installed before. `ad4m_language!` calls this once,
/// after `Language::init()`, so it wraps a hook the language set there
/// (e.g. `console_error_panic_hook`).
pub fn install_panic_marker(mark: fn(&str)) {
    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        mark(&info.to_string());
        previous(info);
    }));
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

    #[test]
    fn calls_after_a_panic_error_instead_of_waiting_for_the_lock() {
        let slot = LanguageSlot::new();
        slot.set(Counter { log: vec![] });
        // A call that panicked in WASM never releases its guard.
        let held = slot.get("expressionGet").unwrap();
        std::mem::forget(block_on(held.lock()));
        slot.mark_panicked("boom at lib.rs:1");
        slot.mark_panicked("a later panic");

        assert!(slot.has_panicked());
        let e = slot
            .get("expressionGet")
            .expect_err("get() must refuse after a panic, not hand out the locked instance");
        assert!(matches!(e.code, ErrorCode::Internal));
        assert!(e.message.contains("expressionGet"), "{}", e.message);
        assert!(
            e.message
                .contains("earlier call panicked: boom at lib.rs:1"),
            "{}",
            e.message
        );
        let e = slot
            .try_with("interactions", |_| ())
            .expect_err("try_with() must refuse after a panic");
        assert!(e.message.contains("earlier call panicked"), "{}", e.message);
    }

    thread_local! {
        static HOOKED: LanguageSlot<Counter> = const { LanguageSlot::new() };
    }

    fn mark_hooked(message: &str) {
        let _ = HOOKED.try_with(|slot| slot.mark_panicked(message));
    }

    #[test]
    fn panic_marker_records_the_panic_message() {
        HOOKED.with(|slot| slot.set(Counter { log: vec![] }));
        install_panic_marker(mark_hooked);
        let r = std::panic::catch_unwind(|| panic!("probe panic in a language method"));
        assert!(r.is_err());

        let e = HOOKED
            .with(|slot| slot.get("expressionGet"))
            .expect_err("the hook must record the panic on the slot");
        assert!(
            e.message.contains("probe panic in a language method"),
            "{}",
            e.message
        );
    }
}

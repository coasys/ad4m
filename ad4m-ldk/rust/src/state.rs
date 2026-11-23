//! Per-module state helper. Languages compiled to WASM are single-instance
//! per isolate, so a thread_local RefCell is sufficient for per-Language state.

use std::cell::{Cell, Ref, RefCell, RefMut};
use std::future::Future;
use std::rc::Rc;
use std::task::Waker;

use futures::channel::oneshot;
use futures::future::{select, Either, FutureExt, Shared};

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
            let v = borrow
                .as_ref()
                .expect("State not initialized (call init() first)");
            f(v)
        })
    }

    pub fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        self.inner.with(|cell| {
            let mut borrow = cell.borrow_mut();
            let v = borrow
                .as_mut()
                .expect("State not initialized (call init() first)");
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
        static $name: $crate::state::State<$ty> = $crate::state::State::new(&__LANG_STATE_CELL);
    };
}

/// The slot that holds the instance of an `ad4m_language!` language.
///
/// The runtime can call into a language while an earlier async call
/// still waits on a host import, so the slot does not assume serial
/// calls. A read/write lock guards the instance, by the export's declared
/// kind (the `concurrent:` list of `ad4m_language!`):
///   * concurrent exports share the read side (`read`) — two of them
///     overlap;
///   * ordered exports take the write side (`write`) — they run alone,
///     beside no read and no other write;
///   * sync `&self` exports (`try_with`) share the read side without
///     waiting — they answer while only concurrent work is in flight, and
///     report "busy" only while an ordered call holds the instance;
///   * every call before `init()` (or after `teardown()`) fails with a
///     "not initialized" `LanguageError`;
///   * every call after a panic fails with a "panicked" `LanguageError`
///     (see `mark_panicked`).
///
/// No path in the slot panics. A panic in the language's own code still
/// aborts the WASM module, which does not unwind: a call that held the
/// lock never releases it. Without the panic record, every later async
/// call (and `teardown`) would wait for that lock forever. A call that
/// already waits for the lock when the panic happens is woken by the
/// panic signal (see `Instance::read`/`write`).
pub struct LanguageSlot<T: 'static> {
    inner: RefCell<Option<Rc<CallLock<T>>>>,
    panicked: RefCell<Option<String>>,
    // Created on the first `get`, because `new` is `const`. `mark_panicked`
    // sends the panic message on it, which wakes every waiting lock.
    panic_tx: RefCell<Option<oneshot::Sender<String>>>,
    panic_rx: RefCell<Option<PanicSignal>>,
}

type PanicSignal = Shared<oneshot::Receiver<String>>;

/// How an export enters the instance: a `concurrent:`-declared export
/// shares the read side with other concurrent exports; every other
/// (ordered) export takes the write side alone.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Access {
    Read,
    Write,
}

/// The per-instance read/write lock. WASM runs on one thread, so counts,
/// not atomics: `RefCell` borrows provide the `&`/`&mut` the guards hand
/// out — the counters only decide who may take which borrow.
///
/// A waiting writer turns new readers away so an ordered call is not
/// starved by reads that keep arriving. Waiting is waker-based: a future
/// that stops waiting (the panic side of `select` won) leaves a dead
/// waker the next release drains, so only a live waiter can hold the
/// door against readers.
struct CallLock<T> {
    data: RefCell<T>,
    readers: Cell<usize>,
    writer: Cell<bool>,
    read_waiters: RefCell<Vec<Waker>>,
    write_waiters: RefCell<Vec<Waker>>,
}

impl<T> CallLock<T> {
    fn new(data: T) -> Self {
        Self {
            data: RefCell::new(data),
            readers: Cell::new(0),
            writer: Cell::new(false),
            read_waiters: RefCell::new(Vec::new()),
            write_waiters: RefCell::new(Vec::new()),
        }
    }

    fn can_read(&self) -> bool {
        !self.writer.get() && self.write_waiters.borrow().is_empty()
    }

    fn can_write(&self) -> bool {
        !self.writer.get() && self.readers.get() == 0
    }

    fn take_read(&self) -> ReadGuard<'_, T> {
        self.readers.set(self.readers.get() + 1);
        ReadGuard {
            lock: self,
            inner: self.data.borrow(),
        }
    }

    fn take_write(&self) -> WriteGuard<'_, T> {
        self.writer.set(true);
        WriteGuard {
            lock: self,
            inner: self.data.borrow_mut(),
        }
    }

    fn register(waiters: &RefCell<Vec<Waker>>, waker: &Waker) {
        let mut waiters = waiters.borrow_mut();
        if !waiters.iter().any(|w| w.will_wake(waker)) {
            waiters.push(waker.clone());
        }
    }

    fn drain(waiters: &RefCell<Vec<Waker>>) -> Vec<Waker> {
        waiters.borrow_mut().drain(..).collect()
    }

    fn read(&self) -> impl Future<Output = ReadGuard<'_, T>> + '_ {
        std::future::poll_fn(move |cx| {
            if self.can_read() {
                std::task::Poll::Ready(self.take_read())
            } else {
                Self::register(&self.read_waiters, cx.waker());
                std::task::Poll::Pending
            }
        })
    }

    fn write(&self) -> impl Future<Output = WriteGuard<'_, T>> + '_ {
        std::future::poll_fn(move |cx| {
            if self.can_write() {
                std::task::Poll::Ready(self.take_write())
            } else {
                Self::register(&self.write_waiters, cx.waker());
                std::task::Poll::Pending
            }
        })
    }

    fn try_read(&self) -> Option<ReadGuard<'_, T>> {
        self.can_read().then(|| self.take_read())
    }

    /// Wake every waiting reader — all of them may proceed together.
    fn wake_readers(&self) {
        for w in Self::drain(&self.read_waiters) {
            w.wake();
        }
    }

    /// Wake every waiting writer: they re-poll and the first takes the
    /// write side; the rest re-register. Dead wakers (dropped futures)
    /// drop out.
    fn wake_writers(&self) {
        for w in Self::drain(&self.write_waiters) {
            w.wake();
        }
    }
}

impl<T> Drop for CallLock<T> {
    fn drop(&mut self) {
        for w in self.read_waiters.get_mut().drain(..) {
            w.wake();
        }
        for w in self.write_waiters.get_mut().drain(..) {
            w.wake();
        }
    }
}

/// Shared access to the instance — every concurrent export holds one.
pub struct ReadGuard<'a, T> {
    lock: &'a CallLock<T>,
    inner: Ref<'a, T>,
}

impl<T> std::ops::Deref for ReadGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> Drop for ReadGuard<'_, T> {
    fn drop(&mut self) {
        self.lock.readers.set(self.lock.readers.get() - 1);
        if self.lock.readers.get() == 0 {
            self.lock.wake_writers();
        }
    }
}

/// Alone access to the instance — every ordered export holds one.
pub struct WriteGuard<'a, T> {
    lock: &'a CallLock<T>,
    inner: RefMut<'a, T>,
}

impl<T> std::ops::Deref for WriteGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for WriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

impl<T> Drop for WriteGuard<'_, T> {
    fn drop(&mut self) {
        self.lock.writer.set(false);
        // Writers first: a queued ordered call should not wait behind
        // reads that keep arriving.
        if self.lock.write_waiters.borrow().is_empty() {
            self.lock.wake_readers();
        } else {
            self.lock.wake_writers();
        }
    }
}

/// The guard an export holds across its call: `Read` shares, `Write` is
/// alone. `Deref` gives `&T` to `&self` methods under either; `DerefMut`
/// answers `&mut T` only for `Write` — a `&mut` method declared
/// `concurrent:` reaches it through `Read` and panics with the cause.
pub enum InstanceGuard<'a, T> {
    Read(ReadGuard<'a, T>),
    Write(WriteGuard<'a, T>),
}

impl<T> std::ops::Deref for InstanceGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        match self {
            InstanceGuard::Read(g) => g,
            InstanceGuard::Write(g) => g,
        }
    }
}

impl<T> std::ops::DerefMut for InstanceGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        match self {
            InstanceGuard::Write(g) => g,
            InstanceGuard::Read(_) => panic!(
                "a &mut export cannot be declared `concurrent:` — \
                 it needs the instance alone"
            ),
        }
    }
}

/// The instance an export's future locks, with the slot's panic signal.
pub struct Instance<T: 'static> {
    lock: Rc<CallLock<T>>,
    panicked: PanicSignal,
}

impl<T: 'static> std::fmt::Debug for Instance<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Instance").finish_non_exhaustive()
    }
}

impl<T: 'static> Instance<T> {
    /// Wait for the instance as `access` says: shared (`Read`) or alone
    /// (`Write`). Fails with the "panicked" error when the module panics
    /// while this call waits, because the call that panicked never
    /// releases the lock.
    pub async fn acquire(&self, access: Access, op: &str) -> LanguageResult<InstanceGuard<'_, T>> {
        match access {
            Access::Read => Ok(InstanceGuard::Read(self.read(op).await?)),
            Access::Write => Ok(InstanceGuard::Write(self.write(op).await?)),
        }
    }

    /// Wait for the shared read side. Concurrent exports hold the
    /// instance together.
    pub async fn read(&self, op: &str) -> LanguageResult<ReadGuard<'_, T>> {
        match select(self.lock.read(), self.panicked.clone()).await {
            Either::Left((guard, _)) => Ok(guard),
            Either::Right((Ok(message), _)) => Err(panicked(op, &message)),
            // The sender only drops with the slot. Keep waiting for the lock.
            Either::Right((Err(oneshot::Canceled), lock)) => Ok(lock.await),
        }
    }

    /// Wait for the write side. Ordered exports take the instance alone.
    pub async fn write(&self, op: &str) -> LanguageResult<WriteGuard<'_, T>> {
        match select(self.lock.write(), self.panicked.clone()).await {
            Either::Left((guard, _)) => Ok(guard),
            Either::Right((Ok(message), _)) => Err(panicked(op, &message)),
            // The sender only drops with the slot. Keep waiting for the lock.
            Either::Right((Err(oneshot::Canceled), lock)) => Ok(lock.await),
        }
    }

    /// The read side without waiting, for a sync export — `None` while an
    /// ordered call holds the instance.
    pub fn try_read(&self) -> Option<ReadGuard<'_, T>> {
        self.lock.try_read()
    }
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
            panic_tx: RefCell::new(None),
            panic_rx: RefCell::new(None),
        }
    }

    /// Store a new instance. A call that still holds the old instance
    /// finishes against the old instance.
    pub fn set(&self, value: T) {
        *self.inner.borrow_mut() = Some(Rc::new(CallLock::new(value)));
    }

    /// The shared instance, or a "panicked" / "not initialized" error.
    pub fn get(&self, op: &str) -> LanguageResult<Instance<T>> {
        if let Some(message) = self.panic_message() {
            return Err(panicked(op, &message));
        }
        let lock = self
            .inner
            .borrow()
            .clone()
            .ok_or_else(|| not_initialized(op))?;
        Ok(Instance {
            lock,
            panicked: self.panic_signal(),
        })
    }

    fn panic_signal(&self) -> PanicSignal {
        self.panic_rx
            .borrow_mut()
            .get_or_insert_with(|| {
                let (tx, rx) = oneshot::channel();
                *self.panic_tx.borrow_mut() = Some(tx);
                rx.shared()
            })
            .clone()
    }

    /// Run a sync `&self` call against the shared read side. Fails with a
    /// "busy" error only while an ordered call holds the instance — it
    /// answers beside concurrent async work.
    pub fn try_with<R>(&self, op: &str, f: impl FnOnce(&T) -> R) -> LanguageResult<R> {
        let instance = self.get(op)?;
        let guard = instance.try_read().ok_or_else(|| {
            LanguageError::transient(format!(
                "Language busy: {op} called while an ordered call is in progress"
            ))
        })?;
        Ok(f(&guard))
    }

    /// Remove the instance from the slot. Later calls fail with "not
    /// initialized". Returns `None` when the slot is empty.
    pub fn take(&self) -> Option<Instance<T>> {
        let lock = self.inner.borrow_mut().take()?;
        Some(Instance {
            lock,
            panicked: self.panic_signal(),
        })
    }

    pub fn is_set(&self) -> bool {
        self.inner.borrow().is_some()
    }

    /// Record that the module panicked and wake every call that waits
    /// for the lock. Called from the panic hook, so it must not panic
    /// itself: it skips the record when a cell is borrowed and keeps the
    /// first message.
    pub fn mark_panicked(&self, message: &str) {
        let message: String = message.chars().take(500).collect();
        if let Ok(mut p) = self.panicked.try_borrow_mut() {
            if p.is_none() {
                *p = Some(message.clone());
            }
        }
        if let Ok(mut tx) = self.panic_tx.try_borrow_mut() {
            if let Some(tx) = tx.take() {
                let _ = tx.send(message);
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

fn panicked(op: &str, message: &str) -> LanguageError {
    LanguageError::internal(format!(
        "Language unusable: {op} refused because an earlier call panicked: {message}"
    ))
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
    use futures::task::noop_waker_ref;
    use std::future::Future;
    use std::task::{Context, Poll};

    #[derive(Debug)]
    struct Counter {
        log: RefCell<Vec<&'static str>>,
    }

    impl Counter {
        fn new() -> Self {
            Self {
                log: RefCell::new(vec![]),
            }
        }
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
    fn ordered_calls_run_one_after_the_other() {
        let slot = LanguageSlot::new();
        slot.set(Counter::new());
        let (tx, rx) = oneshot::channel::<()>();

        // First call holds the write side across an await that only
        // completes after the second call has started waiting.
        let first = async {
            let m = slot.get("first").unwrap();
            let g = m.write("first").await.unwrap();
            g.log.borrow_mut().push("first:start");
            rx.await.unwrap();
            g.log.borrow_mut().push("first:end");
        };
        let second = async {
            let m = slot.get("second").unwrap();
            tx.send(()).unwrap();
            let g = m.write("second").await.unwrap();
            g.log.borrow_mut().push("second");
        };
        block_on(join(first, second));

        let m = slot.get("check").unwrap();
        let g = m.try_read().unwrap();
        assert_eq!(*g.log.borrow(), vec!["first:start", "first:end", "second"]);
    }

    #[test]
    fn concurrent_calls_share_the_instance() {
        let slot = LanguageSlot::new();
        slot.set(Counter::new());
        let (tx, rx) = oneshot::channel::<()>();

        // The first reader awaits inside its read; the second reader
        // completes while the first still holds the instance.
        let first = async {
            let m = slot.get("first").unwrap();
            let g = m.read("first").await.unwrap();
            g.log.borrow_mut().push("first:start");
            rx.await.unwrap();
            g.log.borrow_mut().push("first:end");
        };
        let second = async {
            let m = slot.get("second").unwrap();
            tx.send(()).unwrap();
            let g = m.read("second").await.unwrap();
            g.log.borrow_mut().push("second");
        };
        block_on(join(first, second));

        let m = slot.get("check").unwrap();
        let g = m.try_read().unwrap();
        assert_eq!(*g.log.borrow(), vec!["first:start", "second", "first:end"]);
    }

    #[test]
    fn a_read_does_not_overtake_a_waiting_write() {
        let slot = LanguageSlot::new();
        slot.set(Counter::new());
        let first = slot.get("first").unwrap();
        let second = slot.get("second").unwrap();
        let mut cx = Context::from_waker(noop_waker_ref());

        let held = block_on(first.read("first")).unwrap();
        // A queued writer turns new readers away.
        let mut write = Box::pin(second.write("ordered"));
        assert!(write.as_mut().poll(&mut cx).is_pending());
        assert!(second.try_read().is_none());

        drop(held);
        assert!(write.as_mut().poll(&mut cx).is_ready());
    }

    #[test]
    fn sync_call_answers_beside_concurrent_work_and_is_busy_beside_a_write() {
        let slot = LanguageSlot::new();
        slot.set(Counter::new());
        let m = slot.get("async").unwrap();
        let read = block_on(m.read("concurrent")).unwrap();

        // A sync &self export answers while only concurrent work is in flight.
        slot.try_with("interactions", |l| l.log.borrow_mut().push("sync"))
            .unwrap();
        drop(read);

        // While an ordered call holds the instance it still reports busy.
        let write = block_on(m.write("ordered")).unwrap();
        let e = slot
            .try_with("interactions", |_| ())
            .expect_err("sync export must not share the instance with a write");
        assert!(matches!(e.code, ErrorCode::Transient));
        assert!(e.message.contains("busy"));
        drop(write);

        assert_eq!(*m.try_read().unwrap().log.borrow(), vec!["sync"]);
    }

    #[test]
    fn take_empties_slot_and_waits_for_in_flight_call() {
        let slot = LanguageSlot::new();
        slot.set(Counter::new());
        let held = slot.get("async").unwrap();
        let guard = block_on(held.write("async")).unwrap();

        let taken = slot.take().expect("instance");
        assert!(!slot.is_set());
        assert!(slot.get("expressionGet").is_err());
        assert!(taken.try_read().is_none());
        drop(guard);
        assert!(block_on(taken.write("teardown"))
            .unwrap()
            .log
            .borrow()
            .is_empty());
    }

    #[test]
    fn calls_after_a_panic_error_instead_of_waiting_for_the_lock() {
        let slot = LanguageSlot::new();
        slot.set(Counter::new());
        // A call that panicked in WASM never releases its guard.
        let held = slot.get("expressionGet").unwrap();
        std::mem::forget(block_on(held.write("expressionGet")).unwrap());
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

    #[test]
    fn a_call_waiting_for_the_lock_fails_when_the_holder_panics() {
        let slot = LanguageSlot::new();
        slot.set(Counter::new());
        let held = slot.get("expressionCreate").unwrap();
        let guard = block_on(held.write("expressionCreate")).unwrap();

        // Both wait for the lock before the panic: a shim call, and a
        // teardown that has already taken the instance from the slot.
        let waiting = slot.get("expressionGet").unwrap();
        let mut call = Box::pin(waiting.write("expressionGet"));
        let taken = slot.take().expect("instance");
        let mut teardown = Box::pin(taken.write("teardown"));
        let mut cx = Context::from_waker(noop_waker_ref());
        assert!(call.as_mut().poll(&mut cx).is_pending());
        assert!(teardown.as_mut().poll(&mut cx).is_pending());

        // The holder panics in WASM: it never releases the guard.
        std::mem::forget(guard);
        slot.mark_panicked("boom at lib.rs:7");

        assert_refused_after_panic("expressionGet", call.as_mut().poll(&mut cx));
        assert_refused_after_panic("teardown", teardown.as_mut().poll(&mut cx));
    }

    #[test]
    fn a_waiting_reader_fails_when_the_holder_panics() {
        let slot = LanguageSlot::new();
        slot.set(Counter::new());
        let held = slot.get("expressionCreate").unwrap();
        let guard = block_on(held.write("expressionCreate")).unwrap();

        let waiting = slot.get("expressionGet").unwrap();
        let mut call = Box::pin(waiting.read("expressionGet"));
        let mut cx = Context::from_waker(noop_waker_ref());
        assert!(call.as_mut().poll(&mut cx).is_pending());

        std::mem::forget(guard);
        slot.mark_panicked("boom at lib.rs:7");

        assert_refused_after_panic("expressionGet", call.as_mut().poll(&mut cx));
    }

    fn assert_refused_after_panic<G>(op: &str, polled: Poll<LanguageResult<G>>) {
        match polled {
            Poll::Ready(Err(e)) => {
                assert!(matches!(e.code, ErrorCode::Internal));
                assert!(e.message.contains(op), "{}", e.message);
                assert!(
                    e.message
                        .contains("earlier call panicked: boom at lib.rs:7"),
                    "{}",
                    e.message
                );
            }
            Poll::Ready(Ok(_)) => panic!("{op} must not get the lock of a panicked call"),
            Poll::Pending => panic!("{op} still waits for a lock that is never released"),
        }
    }

    thread_local! {
        static HOOKED: LanguageSlot<Counter> = const { LanguageSlot::new() };
    }

    fn mark_hooked(message: &str) {
        let _ = HOOKED.try_with(|slot| slot.mark_panicked(message));
    }

    #[test]
    fn panic_marker_records_the_panic_message() {
        HOOKED.with(|slot| slot.set(Counter::new()));
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

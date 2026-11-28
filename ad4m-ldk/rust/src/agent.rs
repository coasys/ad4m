//! Per-call agent propagation for WASM exports.
//!
//! The runtime wraps every script call in `__ad4mRunCall`, which enters the
//! call's `CallAgent` token into a V8 async-context variable the agent
//! imports read. wasm-bindgen's microtask queue polls every exported future
//! under whichever context scheduled the drain, not under the caller's — so
//! a call resumed beside another would read the other call's agent.
//!
//! The exports therefore take the token once, at export entry, where the
//! ambient context is still the call's own (`globalThis.__ad4mCallAgentToken`,
//! a runtime global like `__ad4mRunCall`), and [`WithAgent`] installs it in a
//! thread-local around every poll, restoring the previous value after. Every
//! agent import hands its call's token to the host explicitly, as the last
//! argument — a host that predates it ignores the extra argument and answers
//! as the ambient call agent. Sync `&self` exports run entirely at entry, so
//! between polls the thread-local is empty and their host calls fall back to
//! the ambient context, which is then the call's own.
//!
//! Three values travel on the `call` argument:
//!   * a `CallAgent` — the call's own token;
//!   * `null` — the call has no agent: the host must refuse, not fall back;
//!   * `undefined` — nothing pinned (between polls, or a host without the
//!     token getter): the host answers as the ambient call agent.

use std::cell::RefCell;
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll};

use wasm_bindgen::JsValue;

/// A call's `CallAgent` token. Shared behind an `Rc` because `JsValue`
/// cannot be cloned off-wasm, so tests drive the machinery there.
type Token = Rc<JsValue>;

thread_local! {
    /// The token of the call whose export is polling now; `None` between
    /// polls, where the ambient context is again the right answer.
    static CALL_AGENT: RefCell<Option<Token>> = const { RefCell::new(None) };
}

/// The `CallAgent` token of the call whose export is polling, or `None`
/// between polls.
#[doc(hidden)]
pub fn current() -> Option<Token> {
    CALL_AGENT.with(|a| a.borrow().clone())
}

/// The token as the `call` argument the agent imports take: the polling
/// call's own token, `null` when the call has no agent, `undefined` outside
/// an export's poll so the host falls back to the ambient context.
pub(crate) fn current_arg() -> JsValue {
    current()
        .map(|token| (*token).clone())
        .unwrap_or(JsValue::UNDEFINED)
}

/// Read the calling call's `CallAgent` token. Only correct at export entry:
/// the ambient context is then still the call's own. A host that predates
/// `__ad4mCallAgentToken` answers `undefined`, which `current_arg` passes on
/// as the ambient fallback; a `null` token pins `null` so the host refuses
/// rather than fall back to another call's ambient context.
fn enter_token() -> Token {
    Rc::new(host_call_agent_token())
}

#[cfg(target_family = "wasm")]
fn host_call_agent_token() -> JsValue {
    use wasm_bindgen::JsCast;
    let getter = js_sys::Reflect::get(
        js_sys::global().as_ref(),
        &JsValue::from_str("__ad4mCallAgentToken"),
    )
    .unwrap_or(JsValue::UNDEFINED);
    if !getter.is_function() {
        return JsValue::UNDEFINED;
    }
    getter
        .unchecked_into::<js_sys::Function>()
        .call0(&JsValue::NULL)
        .unwrap_or(JsValue::UNDEFINED)
}

#[cfg(not(target_family = "wasm"))]
fn host_call_agent_token() -> JsValue {
    JsValue::UNDEFINED
}

/// Wrap an export's body: takes the call's agent token now and hands the
/// future to the JS caller as a `Promise` that polls it under that token.
/// Every generated export that can suspend goes through here.
pub fn export_future<F>(future: F) -> js_sys::Promise
where
    F: Future<Output = Result<JsValue, JsValue>> + 'static,
{
    wasm_bindgen_futures::future_to_promise(WithAgent {
        agent: enter_token(),
        future: Box::pin(future),
    })
}

/// A future that installs its call's agent token in the thread-local around
/// every poll and restores the previous value afterwards, so nested polls
/// (one export future resuming while another's poll left a token) keep
/// their own.
pub struct WithAgent<F> {
    agent: Token,
    future: Pin<Box<F>>,
}

impl<F> WithAgent<F> {
    /// Wrap `future` to poll it with `agent` installed (for callers that
    /// already hold the token, e.g. tests).
    pub fn new(agent: Token, future: F) -> Self
    where
        F: Future + 'static,
    {
        Self {
            agent,
            future: Box::pin(future),
        }
    }
}

impl<F: Future> Future for WithAgent<F> {
    type Output = F::Output;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let prev = CALL_AGENT.with(|a| a.borrow_mut().replace(self.agent.clone()));
        let result = self.future.as_mut().poll(cx);
        CALL_AGENT.with(|a| *a.borrow_mut() = prev);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::channel::oneshot;
    use futures::task::noop_waker_ref;
    use std::rc::Rc;

    fn token() -> Token {
        // `JsValue` constructors call into JS (absent off-wasm), but every
        // `Rc` is a distinct allocation — `Rc::ptr_eq` tells them apart.
        Rc::new(JsValue::UNDEFINED)
    }

    #[test]
    fn every_poll_sees_its_own_agent() {
        let alice = token();
        let bob = token();

        let (gate_tx, gate_rx) = oneshot::channel::<()>();
        let seen = Rc::new(RefCell::new(Vec::new()));

        let mut alice_call = WithAgent::new(alice.clone(), {
            let seen = seen.clone();
            async move {
                seen.borrow_mut()
                    .push(current().map(|t| Rc::ptr_eq(&t, &alice)).unwrap_or(false));
                let _ = gate_rx.await;
                seen.borrow_mut()
                    .push(current().map(|t| Rc::ptr_eq(&t, &alice)).unwrap_or(false));
            }
        });
        let mut bob_call = WithAgent::new(bob.clone(), {
            let seen = seen.clone();
            async move {
                seen.borrow_mut()
                    .push(current().map(|t| Rc::ptr_eq(&t, &bob)).unwrap_or(false));
            }
        });

        let mut cx = Context::from_waker(noop_waker_ref());

        // Alice polls once, then Bob's whole call runs inside her suspension
        // window; Alice's resumed poll must still read her own token.
        assert!(Pin::new(&mut alice_call).poll(&mut cx).is_pending());
        assert!(Pin::new(&mut bob_call).poll(&mut cx).is_ready());
        assert!(current().is_none(), "the local is left clear between polls");
        gate_tx.send(()).unwrap();
        assert!(Pin::new(&mut alice_call).poll(&mut cx).is_ready());

        assert_eq!(seen.borrow().as_slice(), [true, true, true]);
    }
}

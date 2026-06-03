//! Holograph language wires — Rust ↔ JS bridge surface for the
//! `holograph-link` Language module.
//!
//! ## What this is
//!
//! Step 5 of the holograph spike scaffolds the trait + ops surface a
//! future runtime installer will expose to JS Languages as
//! `globalThis.__holographDelegate__`. The JS side of that surface lives
//! in `rust-executor/src/js_core/host.js` (`holograph*` exports) and the
//! Language module imports them via `ad4m:host`.
//!
//! ## What this is NOT
//!
//! Step 5 ships a **stub** — every method returns
//! `HolographWireError::NotImplemented`. Step 6 (the orchestrator's next
//! dispatch) wires the real `HolographSpace` (from
//! `holograph::space`) into a `HolographDelegate` impl and installs it
//! onto the v8 isolate.
//!
//! Keeping the stub here gives Step 5 three things:
//!
//! 1. A documented, type-correct contract Step 6 can implement against.
//! 2. A place for the `holograph-link` Language to compile against
//!    today (the JS host functions in `host.js` route through the
//!    delegate global; the JS bundle builds even though calling a
//!    method will throw "NotImplemented" until Step 6).
//! 3. A single load-bearing definition of the wire surface so the
//!    Language module and the runtime stay in lockstep.
//!
//! ## Tokio runtime nesting (SPIKE §2.6)
//!
//! When Step 6 fills these stubs, every async path that crosses the
//! delegate boundary MUST go through the dedicated `tokio::runtime::Handle`
//! that `holograph::HolographSpace` already owns. The Step 4 unit + 4d
//! integration tests demonstrate the pattern. Do not pass the executor's
//! main runtime here.

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Opaque per-neighborhood handle the JS side holds onto. v1 uses an
/// auto-incrementing `u64` keyed in a host-side registry. Step 6 picks
/// the concrete shape; this type is the contract Step 5's JS bundle
/// imports.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct HolographHandle(pub u64);

/// One integrated op surfaced to the JS subscriber. Field shapes mirror
/// `holograph::space::EmittedOp` (op-id bytes + ms timestamp + raw
/// envelope bytes). Strings are base64 because JS doesn't carry raw
/// byte sequences across the v8 ↔ Rust boundary without re-encoding.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EmittedOpWire {
    /// 36-byte hash id, base64-encoded (URL-safe, no padding).
    pub op_id_b64: String,
    /// Authoring timestamp in milliseconds since Unix epoch.
    pub created_at_ms: i64,
    /// Raw CBOR envelope bytes, base64-encoded.
    pub envelope_b64: String,
}

/// Error type returned across the wire. v1 stubs everything with
/// `NotImplemented`; Step 6 will widen to cover the K2-side error
/// surface (`K2Error`, sled errors, envelope decode failures).
#[derive(Debug, Error, Clone, Serialize, Deserialize)]
pub enum HolographWireError {
    /// Step 5 stub default. Step 6 must remove every site that returns
    /// this before the language module is usable in production.
    #[error("holograph wire not yet implemented; the holograph_wires module is a stub (Step 5). The full HolographSpace ↔ JS wiring lands in Step 6.")]
    NotImplemented,

    /// Future Step 6 use: handle was never registered or was already
    /// closed via `close_neighborhood`. Documented in the wire surface
    /// here so the JS module can pattern-match it.
    #[error("unknown holograph handle: {handle:?}")]
    UnknownHandle { handle: HolographHandle },

    /// Future Step 6 use: caller-supplied envelope bytes failed to
    /// decode. Carries the inner error message verbatim.
    #[error("invalid envelope: {0}")]
    InvalidEnvelope(String),

    /// Future Step 6 use: bubble up `K2Error::other`, sled errors, etc.
    #[error("substrate error: {0}")]
    Substrate(String),
}

pub type HolographWireResult<T> = Result<T, HolographWireError>;

/// The trait Step 6 will implement against a `holograph::HolographSpace`.
/// The JS host functions in `host.js` (under `# Holograph`) call these
/// through `globalThis.__holographDelegate__`.
///
/// All methods are described as if they will be `async` once Step 6
/// fills them in. The v8 isolate sees them as async functions returning
/// promises; the Rust-side install will use `deno_core::Op` async ops
/// (or sync ops for the synchronous getters) hung off the runtime
/// handle `HolographSpace` already owns.
pub trait HolographDelegate: Send + Sync + 'static {
    /// Open or create a neighborhood-scoped substrate. `space_id` is
    /// the AD4M neighborhood identifier (typically derived from the
    /// language address + uuid); `storage_dir` is the
    /// `LANGUAGE_CONTROLLER.languageStorageDirectory()` value the JS
    /// side passes in.
    ///
    /// Returns a `HolographHandle` the JS side holds onto and threads
    /// through every subsequent call.
    fn create_neighborhood(
        &self,
        space_id: &str,
        storage_dir: &str,
    ) -> HolographWireResult<HolographHandle>;

    /// Commit a locally-authored envelope. The JS side serializes a
    /// `PerspectiveDiff` into an `OpEnvelope` (Step 6 may move that
    /// serialization into Rust); the returned string is the op-id
    /// base64 (matches `EmittedOpWire::op_id_b64`'s encoding).
    fn commit(
        &self,
        handle: HolographHandle,
        envelope_bytes: &[u8],
    ) -> HolographWireResult<String>;

    /// Drive the algorithm crate's render entry point against the
    /// neighborhood's current revision. Step 6 wires this through
    /// `KitsuneRetreiver` + `perspective_diff_sync::link_adapter::render`.
    /// v1 stub returns `NotImplemented`; the eventual real shape is
    /// `{ links: [LinkExpression, ...] }`, matching the existing
    /// p-diff-sync `render` contract.
    fn render(&self, handle: HolographHandle) -> HolographWireResult<serde_json::Value>;

    /// Pop the next-available `EmittedOp` for the given handle, or
    /// `None` if the channel is currently drained. Step 6 backs this
    /// with the `mpsc::UnboundedReceiver<EmittedOp>` half of
    /// `ChannelNotifier::new()`.
    ///
    /// JS-side `holographSubscribe` is implemented as a polling loop
    /// over `next_emitted` returning a `null` to signal "no new ops
    /// yet" — the loop awaits a deno op which itself awaits the
    /// receiver, so no JS polling/sleep is required.
    fn next_emitted(
        &self,
        handle: HolographHandle,
    ) -> HolographWireResult<Option<EmittedOpWire>>;

    /// The JS side hands us its DID; we map it to a `kitsune2_api::AgentId`
    /// and `local_agent_join` the agent into the K2 space so this node
    /// participates in gossip.
    ///
    /// Returns the K2 URL (canonical `ws://host:port`) this node is
    /// reachable at — handy for the JS module's logging and for the
    /// existing `peers.remote()` Language capability.
    fn join_agent(
        &self,
        handle: HolographHandle,
        agent_key_bytes: &[u8],
    ) -> HolographWireResult<String>;

    /// Read the current revision pointer from the neighborhood's
    /// `KitsuneRetreiverState::revisions` sled tree. Returns `None`
    /// before the first commit lands.
    fn current_revision(
        &self,
        handle: HolographHandle,
    ) -> HolographWireResult<Option<String>>;

    /// Read the latest revision pointer (the network's known head, not
    /// just ours). v1's first-pass implementation will read the same
    /// tree as `current_revision` since p-diff-sync's distinction
    /// between current and latest doesn't carry through into the K2
    /// substrate; Step 6 may collapse the two if the surface ends up
    /// redundant.
    fn latest_revision(
        &self,
        handle: HolographHandle,
    ) -> HolographWireResult<Option<String>>;

    /// Tear down a neighborhood. Releases sled handles, stops the
    /// queue watcher, drops the `DynSpace`. Idempotent — calling on an
    /// already-closed handle returns `Ok(())`.
    fn close_neighborhood(&self, handle: HolographHandle) -> HolographWireResult<()>;
}

/// Step 5 stub. Every method returns `NotImplemented`. Step 6 replaces
/// this with `HolographRuntime { spaces: DashMap<HolographHandle, …> }`
/// or equivalent.
#[derive(Debug, Default)]
pub struct NotImplementedHolographDelegate;

impl HolographDelegate for NotImplementedHolographDelegate {
    fn create_neighborhood(
        &self,
        _space_id: &str,
        _storage_dir: &str,
    ) -> HolographWireResult<HolographHandle> {
        Err(HolographWireError::NotImplemented)
    }

    fn commit(
        &self,
        _handle: HolographHandle,
        _envelope_bytes: &[u8],
    ) -> HolographWireResult<String> {
        Err(HolographWireError::NotImplemented)
    }

    fn render(&self, _handle: HolographHandle) -> HolographWireResult<serde_json::Value> {
        Err(HolographWireError::NotImplemented)
    }

    fn next_emitted(
        &self,
        _handle: HolographHandle,
    ) -> HolographWireResult<Option<EmittedOpWire>> {
        Err(HolographWireError::NotImplemented)
    }

    fn join_agent(
        &self,
        _handle: HolographHandle,
        _agent_key_bytes: &[u8],
    ) -> HolographWireResult<String> {
        Err(HolographWireError::NotImplemented)
    }

    fn current_revision(
        &self,
        _handle: HolographHandle,
    ) -> HolographWireResult<Option<String>> {
        Err(HolographWireError::NotImplemented)
    }

    fn latest_revision(
        &self,
        _handle: HolographHandle,
    ) -> HolographWireResult<Option<String>> {
        Err(HolographWireError::NotImplemented)
    }

    fn close_neighborhood(&self, _handle: HolographHandle) -> HolographWireResult<()> {
        Err(HolographWireError::NotImplemented)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stub_returns_not_implemented_on_every_method() {
        let stub = NotImplementedHolographDelegate;
        let h = HolographHandle(0);
        assert!(matches!(
            stub.create_neighborhood("sp", "/tmp"),
            Err(HolographWireError::NotImplemented)
        ));
        assert!(matches!(
            stub.commit(h, &[]),
            Err(HolographWireError::NotImplemented)
        ));
        assert!(matches!(
            stub.render(h),
            Err(HolographWireError::NotImplemented)
        ));
        assert!(matches!(
            stub.next_emitted(h),
            Err(HolographWireError::NotImplemented)
        ));
        assert!(matches!(
            stub.join_agent(h, &[]),
            Err(HolographWireError::NotImplemented)
        ));
        assert!(matches!(
            stub.current_revision(h),
            Err(HolographWireError::NotImplemented)
        ));
        assert!(matches!(
            stub.latest_revision(h),
            Err(HolographWireError::NotImplemented)
        ));
        assert!(matches!(
            stub.close_neighborhood(h),
            Err(HolographWireError::NotImplemented)
        ));
    }

    #[test]
    fn emitted_op_wire_round_trips_serde() {
        let item = EmittedOpWire {
            op_id_b64: "abc".to_string(),
            created_at_ms: 1_700_000_000_000,
            envelope_b64: "def==".to_string(),
        };
        let s = serde_json::to_string(&item).unwrap();
        let back: EmittedOpWire = serde_json::from_str(&s).unwrap();
        assert_eq!(item, back);
    }
}

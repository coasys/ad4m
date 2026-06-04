//! Holograph language wires — Rust ↔ JS bridge for the
//! `holograph-link` AD4M Language module.
//!
//! Step 6 lands the real wiring: the `NotImplementedHolographDelegate`
//! stub from Step 5 is gone. `HolographRuntime` owns a pool of
//! `HolographSpace`s keyed by `HolographHandle` and is exposed to JS
//! via the `holograph_service` deno extension
//! (`rust-executor/src/js_core/holograph_service_extension.rs`).
//!
//! ## Architecture
//!
//! ```text
//! JS Language module (index.ts) — bundles holograph-link
//!     |  awaits holographCommit(handle, diff) etc.
//!     v
//! ad4m:host (host.js)  — exposes globalThis.__holographDelegate__
//!     |  delegates to HOLOGRAPH_SERVICE.commit etc.
//!     v
//! HOLOGRAPH_SERVICE (holograph_service_extension.js)
//!     |  calls into op2 ops
//!     v
//! holograph_service_extension.rs — deno op2(async) entry points
//!     |  forwards to HOLOGRAPH_RUNTIME
//!     v
//! HolographRuntime (this file) — DashMap<HolographHandle, Arc<HolographSpace>>
//!     |  per-handle ChannelNotifier receivers held in Mutex<Option<mpsc>>
//!     |  dedicated tokio::runtime::Runtime
//!     v
//! holograph::HolographSpace — Step 4 substrate
//! ```
//!
//! ## Tokio runtime nesting (SPIKE §2.6)
//!
//! `HolographRuntime` owns a dedicated `tokio::runtime::Runtime` (2
//! worker threads) and passes its `Handle` to every `HolographSpace`.
//! Deno ops run on the executor's main runtime; when they call into
//! `HolographRuntime::commit` etc. they `await` an async closure that
//! itself routes through `HolographSpace::on_local_commit`. The
//! `HolographSpace` uses *its* runtime handle for the integration
//! queue's watcher task; that handle is the dedicated runtime, not the
//! executor's. So no JS-call ever blocks the executor's main runtime,
//! and no integration-queue task ever runs on the executor's main
//! runtime. See SPIKE.md §2.6.

use std::path::PathBuf;
use std::sync::Arc;

use bytes::Bytes;
use dashmap::DashMap;
use holograph::{
    holograph_envelope_decoder, ArcPolicy, ChannelNotifier, EmittedOp, HolographSpace,
    HolographSpaceConfig, K2DynSpaceTarget, K2FetcherAdapter, K2OpStoreShim, K2PeerPickerAdapter,
    KvOpStore, NotifyUp, OpEnvelope, SpaceConfig,
};
use kitsune2_api::{
    Builder, Config, DynLocalAgent, DynOpStore, DynSpaceHandler, K2Result, OpStoreFactory, SpaceId,
};
use kitsune2_api::{DhtArc, DynKitsuneHandler, KitsuneHandler};
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tokio::runtime::Runtime;
use tokio::sync::{mpsc, Mutex};

/// Opaque per-neighborhood handle the JS side holds onto. Auto-incremented
/// at `create_neighborhood` time and threaded through every subsequent
/// holograph wire call.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct HolographHandle(pub u64);

impl std::fmt::Display for HolographHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "h{}", self.0)
    }
}

/// One integrated op surfaced to the JS subscriber via `holographNextEmitted`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EmittedOpWire {
    /// 36-byte op-id, base64-encoded (URL-safe, no padding).
    pub op_id_b64: String,
    /// Authoring timestamp in milliseconds since Unix epoch.
    pub created_at_ms: i64,
    /// Decoded perspective-diff (additions + removals). The Rust side
    /// owns the envelope + CBOR shape so JS sees pure data.
    pub diff: WireDiff,
}

/// Wire-shape of a perspective-diff committed through the holograph
/// substrate. v1's storage envelope's payload is JSON of this same
/// struct — Step 6e moved CBOR/envelope construction to Rust, so JS
/// hands and receives this shape directly.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct WireDiff {
    #[serde(default)]
    pub additions: Vec<serde_json::Value>,
    #[serde(default)]
    pub removals: Vec<serde_json::Value>,
}

/// Errors raised across the wire.
#[derive(Debug, Error, Clone, Serialize, Deserialize)]
pub enum HolographWireError {
    #[error("unknown holograph handle: {handle:?}")]
    UnknownHandle { handle: HolographHandle },
    #[error("invalid envelope: {0}")]
    InvalidEnvelope(String),
    #[error("substrate error: {0}")]
    Substrate(String),
}

pub type HolographWireResult<T> = Result<T, HolographWireError>;

// ----- helpers -----

fn url_safe_b64_no_pad(bytes: &[u8]) -> String {
    use base64::engine::{general_purpose::URL_SAFE_NO_PAD, Engine};
    URL_SAFE_NO_PAD.encode(bytes)
}

fn substrate(err: impl std::fmt::Display) -> HolographWireError {
    HolographWireError::Substrate(err.to_string())
}

fn invalid_envelope(err: impl std::fmt::Display) -> HolographWireError {
    HolographWireError::InvalidEnvelope(err.to_string())
}

/// Decode a wire diff into a CBOR-encoded `OpEnvelope` payload. The
/// envelope's `payload` is JSON of `WireDiff` for v1 — Step 6e narrows
/// this if we later move to a more compact wire shape, but JSON keeps
/// the smoke tests + the existing Language module's diff shape stable.
fn encode_envelope(diff: &WireDiff) -> Result<(Bytes, i64), HolographWireError> {
    let payload_json =
        serde_json::to_vec(diff).map_err(|e| invalid_envelope(format!("payload JSON: {e}")))?;
    let now_micros = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_micros() as i64)
        .unwrap_or(0);
    let env = OpEnvelope::new_at(
        std::iter::empty(),
        Bytes::from(payload_json),
        Bytes::from_static(b"holograph-v1-author"),
        Bytes::from_static(b"holograph-v1-sig"),
        None,
        now_micros,
    );
    let bytes = env
        .encode()
        .map_err(|e| invalid_envelope(format!("encode envelope: {e}")))?;
    Ok((Bytes::from(bytes), now_micros))
}

fn decode_envelope(envelope_bytes: &[u8]) -> Result<WireDiff, HolographWireError> {
    let env = OpEnvelope::decode(envelope_bytes)
        .map_err(|e| invalid_envelope(format!("decode envelope: {e}")))?;
    let diff: WireDiff = serde_json::from_slice(env.payload.as_ref())
        .map_err(|e| invalid_envelope(format!("decode payload JSON: {e}")))?;
    Ok(diff)
}

// ----- per-neighborhood state -----

struct NeighborhoodState {
    space: Arc<HolographSpace>,
    /// Receiver half of the `ChannelNotifier`. Drained by
    /// `next_emitted`. Wrapped in a `Mutex` because multiple deno ops
    /// could in principle race (in practice the JS subscriber loop is
    /// single-flight, but we want correctness regardless).
    receiver: Mutex<mpsc::UnboundedReceiver<EmittedOp>>,
    /// Live K2 space handle. Step 6b stored this implicitly via the
    /// adapters; Step 9 keeps it here so `join_agent` can call
    /// `current_url()` to publish the conductor's reachable address
    /// (Iroh transport) instead of returning a placeholder.
    dyn_space: kitsune2_api::DynSpace,
}

// ----- the runtime -----

/// Process-global holograph runtime. Lazily initialized on first call
/// to `get` so the deno op surface can be installed before the runtime
/// is ever asked to do work — matching the pattern
/// `get_holochain_service()` uses.
pub struct HolographRuntime {
    /// Per-neighborhood spaces + receivers.
    neighborhoods: DashMap<HolographHandle, Arc<NeighborhoodState>>,
    /// Dedicated tokio runtime that owns the integration-queue watcher
    /// tasks. v1 uses 2 worker threads — see SPIKE §2.6 risk register.
    runtime: Arc<Runtime>,
    /// Auto-incrementing handle id source.
    next_handle: std::sync::atomic::AtomicU64,
}

impl std::fmt::Debug for HolographRuntime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HolographRuntime")
            .field("active_handles", &self.neighborhoods.len())
            .finish()
    }
}

static HOLOGRAPH_RUNTIME: Lazy<HolographRuntime> = Lazy::new(|| {
    // Wake-19 E2: install the holograph loc-callback once, before any
    // K2 space is constructed. Idempotent: if K2 already locked in
    // its default (e.g., because another path called `OpId::loc()`
    // before we got here), this returns false and we log a warning;
    // K2's API gives us no way to unlock + replace.
    if !holograph::install_loc_callback() {
        log::warn!(
            "[holograph] OpId::set_loc_callback returned false — K2 OpId loc \
             already initialised before holograph could install its callback. \
             Head ops will route via the default xor-fold instead of fixed \
             loc=0. Sharded-mode replication of Heads will be wrong; v1 \
             FULL-arc setups are unaffected."
        );
    }
    let runtime = Runtime::new()
        .or_else(|_| {
            tokio::runtime::Builder::new_multi_thread()
                .worker_threads(2)
                .thread_name("holograph-substrate")
                .enable_all()
                .build()
        })
        .expect("build holograph dedicated runtime");
    HolographRuntime {
        neighborhoods: DashMap::new(),
        runtime: Arc::new(runtime),
        next_handle: std::sync::atomic::AtomicU64::new(1),
    }
});

impl HolographRuntime {
    /// Borrow the process-global runtime. Lazily initialized.
    pub fn get() -> &'static HolographRuntime {
        &HOLOGRAPH_RUNTIME
    }

    /// Number of registered neighborhoods. Test-only observability.
    pub fn handle_count(&self) -> usize {
        self.neighborhoods.len()
    }

    fn state(&self, handle: HolographHandle) -> HolographWireResult<Arc<NeighborhoodState>> {
        self.neighborhoods
            .get(&handle)
            .map(|r| r.value().clone())
            .ok_or(HolographWireError::UnknownHandle { handle })
    }

    /// Construct or look up a neighborhood-scoped `HolographSpace`. v1
    /// uses a unique per-(space_id, storage_dir) handle each call —
    /// repeated calls produce distinct handles, distinct sled DBs in
    /// per-handle subdirectories, and distinct K2 spaces. The JS side
    /// is expected to keep one handle per Language-instance lifetime.
    pub async fn create_neighborhood(
        &self,
        space_id: &str,
        storage_dir: &str,
    ) -> HolographWireResult<HolographHandle> {
        let id = self
            .next_handle
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let handle = HolographHandle(id);

        // Per-handle storage subdir so multiple Language instances
        // sharing the same parent dir don't collide on sled locks.
        let base = PathBuf::from(storage_dir).join(format!("h{}", id));
        std::fs::create_dir_all(&base).map_err(substrate)?;

        let space_id_bytes = SpaceId::from(Bytes::copy_from_slice(space_id.as_bytes()));

        // Build the K2 op-store this neighborhood owns. The shim wraps
        // it for K2; the queue installs on the shim during HolographSpace
        // construction.
        let op_store = KvOpStore::open(
            base.join("ops"),
            space_id_bytes.clone(),
            ArcPolicy::Full,
            holograph_envelope_decoder(),
        )
        .map_err(substrate)?;
        let pending_db = sled::open(base.join("pending")).map_err(substrate)?;
        let pending = pending_db.open_tree(b"pending").map_err(substrate)?;
        // Keep the db handle alive for the neighborhood's lifetime by
        // leaking it into the runtime — Step 6 is fine with this; Step 7
        // multi-process semantics will revisit.
        let _ = Box::leak(Box::new(pending_db));

        let space_owner: SpaceId = space_id_bytes.clone();
        let shim = K2OpStoreShim::new(Arc::clone(&op_store));
        let dyn_space = build_dyn_space(
            self.runtime.clone(),
            Arc::clone(&op_store),
            shim.clone(),
            space_owner,
        )
        .await?;

        let fetcher = K2FetcherAdapter::new(dyn_space.fetch().clone());
        let peer_picker = K2PeerPickerAdapter::new(dyn_space.peer_store().clone());
        let (notifier, receiver) = ChannelNotifier::new();
        let commit_target = K2DynSpaceTarget::new(dyn_space.clone());

        let space = HolographSpace::new(HolographSpaceConfig::defaults(
            SpaceConfig::full_replication_single_doc(),
            Arc::clone(&op_store),
            pending,
            holograph_envelope_decoder(),
            fetcher,
            peer_picker,
            notifier as Arc<dyn NotifyUp>,
            commit_target,
            self.runtime.handle().clone(),
        ));

        shim.install_queue(Arc::clone(space.queue()));

        // Local-agent join. For the cross-process (Iroh) path we need
        // a process-unique AgentId — TestLocalAgent::default() uses a
        // static counter so every fresh process starts at "test-1" and
        // the bootstrap server can't tell two conductors apart. The
        // in-process tests (Step 4d / Step 6f) still want TestLocalAgent
        // because they pair with TestVerifier in the same Builder.
        //
        // Production identity (AD4M DID-bound) is PR-B / morning work.
        let cross_process = std::env::var("HOLOGRAPH_IROH_RELAY_URL").is_ok();
        let agent: DynLocalAgent = if cross_process {
            Arc::new(kitsune2_core::Ed25519LocalAgent::default()) as DynLocalAgent
        } else {
            Arc::new(kitsune2_test_utils::agent::TestLocalAgent::default()) as DynLocalAgent
        };
        agent.set_cur_storage_arc(DhtArc::FULL);
        agent.set_tgt_storage_arc_hint(DhtArc::FULL);
        // AgentId Display invokes HoloHash-shaped decoding (only valid
        // for 32-byte ids); print the raw byte length + an URL-safe
        // base64 of the bytes instead so this works for both
        // TestLocalAgent (13B) and Ed25519LocalAgent (32B).
        let agent_b64 = url_safe_b64_no_pad(agent.agent().as_ref());
        log::info!(
            "[holograph] local agent join: agent_id_b64={} ({}B) cross_process={}",
            agent_b64,
            agent.agent().as_ref().len(),
            cross_process,
        );
        dyn_space
            .local_agent_join(agent.clone())
            .await
            .map_err(substrate)?;

        // Keep dyn_space + kitsune handle alive in NeighborhoodState by
        // stashing them inside the closure environment of an upcoming
        // helper. For Step 6 we just leak the kitsune instance — see
        // build_dyn_space below for the kitsune handle.
        // (Already leaked inside build_dyn_space.)

        let state = Arc::new(NeighborhoodState {
            space,
            receiver: Mutex::new(receiver),
            dyn_space: dyn_space.clone(),
        });
        self.neighborhoods.insert(handle, state);
        Ok(handle)
    }

    /// Commit a locally-authored diff. Wraps + encodes the envelope on
    /// the Rust side (Step 6e) so the JS side hands typed
    /// `PerspectiveDiff` data across, not bytes.
    pub async fn commit(
        &self,
        handle: HolographHandle,
        diff: WireDiff,
    ) -> HolographWireResult<String> {
        let state = self.state(handle)?;
        let (envelope_bytes, _ts) = encode_envelope(&diff)?;
        let op_id = state
            .space
            .on_local_commit(envelope_bytes)
            .await
            .map_err(substrate)?;
        Ok(url_safe_b64_no_pad(Bytes::from(op_id).as_ref()))
    }

    /// Render a `Perspective` snapshot. v1 returns `{ links: [] }` —
    /// the substrate-agnostic algorithm crate's render entry point
    /// isn't wired yet (Step 1.5 spec divergence). When `KitsuneRetreiver`
    /// is integrated end-to-end (post-spike PR-B), this returns the
    /// real Perspective view.
    pub async fn render(&self, handle: HolographHandle) -> HolographWireResult<serde_json::Value> {
        let _state = self.state(handle)?;
        Ok(serde_json::json!({ "links": [] }))
    }

    /// Pop the next-available `EmittedOp` for this neighborhood,
    /// awaiting it inside Rust so the JS side never spins. Returns
    /// `None` only on receiver close (i.e., neighborhood closed).
    pub async fn next_emitted(
        &self,
        handle: HolographHandle,
    ) -> HolographWireResult<Option<EmittedOpWire>> {
        let state = self.state(handle)?;
        let mut rx = state.receiver.lock().await;
        match rx.recv().await {
            Some(emit) => {
                let diff = decode_envelope(emit.envelope_bytes.as_ref())?;
                Ok(Some(EmittedOpWire {
                    op_id_b64: url_safe_b64_no_pad(Bytes::from(emit.op_id).as_ref()),
                    created_at_ms: emit.created_at.as_micros() / 1000,
                    diff,
                }))
            }
            None => Ok(None),
        }
    }

    /// Register an additional local agent. v1 substrate spins up its
    /// own sentinel agent at `create_neighborhood` time, so this is
    /// effectively a no-op for the spike — Step 7 will plumb the AD4M
    /// DID through.
    ///
    /// Returns the reachable URL the K2 transport published for this
    /// node (Iroh path: a node-id URL exposed via the iroh relay; mem
    /// path: the placeholder `ws://holograph-local:0` because mem
    /// transport isn't process-routable). The JS test harness uses
    /// this URL to cross-register peers between conductors.
    pub async fn join_agent(
        &self,
        handle: HolographHandle,
        _agent_key_b64: String,
    ) -> HolographWireResult<String> {
        let state = self.state(handle)?;
        Ok(state
            .dyn_space
            .current_url()
            .map(|u| u.to_string())
            .unwrap_or_else(|| "ws://holograph-local:0".to_string()))
    }

    pub async fn current_revision(
        &self,
        _handle: HolographHandle,
    ) -> HolographWireResult<Option<String>> {
        Ok(None)
    }

    pub async fn latest_revision(
        &self,
        _handle: HolographHandle,
    ) -> HolographWireResult<Option<String>> {
        Ok(None)
    }

    /// Tear down a neighborhood. Drops the space + receiver. Idempotent.
    pub async fn close_neighborhood(&self, handle: HolographHandle) -> HolographWireResult<()> {
        self.neighborhoods.remove(&handle);
        Ok(())
    }
}

/// Build a K2 `DynSpace` for our `HolographRuntime` neighborhood.
///
/// Two transport modes, selected by env at first call:
///   * `HOLOGRAPH_IROH_RELAY_URL=<http(s)://relay/relay>` → Iroh
///     transport (QUIC; the `kitsune2-bootstrap-srv` binary doubles as
///     the iroh relay at `<addr>/relay`). Cross-process; suitable for
///     two-conductor JS tests. Matches the rest of the ad4m repo
///     which uses Holochain's `transport-iroh` feature.
///   * unset → mem transport (in-process only). Used by Step 4d /
///     Step 6f Rust integration tests so they keep running fast and
///     deterministic.
///
/// `HOLOGRAPH_IROH_PLAINTEXT=1` allows `http://` relays instead of
/// `https://` — the test harness's bootstrap-srv ships plaintext on
/// loopback.
async fn build_dyn_space(
    runtime: Arc<Runtime>,
    op_store: Arc<KvOpStore>,
    shim: Arc<K2OpStoreShim>,
    space_id: SpaceId,
) -> HolographWireResult<kitsune2_api::DynSpace> {
    // Construct on the dedicated runtime so all K2 internal tasks live
    // there, not on the executor's main runtime.
    let join = runtime.spawn(build_dyn_space_inner(op_store, shim, space_id));
    join.await
        .map_err(|e| substrate(format!("spawn dyn_space build: {e}")))?
}

#[derive(Debug)]
struct ShimFactory {
    #[allow(dead_code)]
    op_store: Arc<KvOpStore>,
    shim: Arc<K2OpStoreShim>,
}
impl OpStoreFactory for ShimFactory {
    fn default_config(&self, _: &mut Config) -> K2Result<()> {
        Ok(())
    }
    fn validate_config(&self, _: &Config) -> K2Result<()> {
        Ok(())
    }
    fn create(
        &self,
        _builder: Arc<Builder>,
        _space_id: SpaceId,
    ) -> futures::future::BoxFuture<'static, K2Result<DynOpStore>> {
        let shim = Arc::clone(&self.shim);
        Box::pin(async move {
            let dyn_store: DynOpStore = shim;
            Ok(dyn_store)
        })
    }
}

#[derive(Debug)]
struct NoopSpaceHandler;
impl kitsune2_api::SpaceHandler for NoopSpaceHandler {}

#[derive(Debug)]
struct NoopKitsuneHandler;
impl KitsuneHandler for NoopKitsuneHandler {
    fn create_space(
        &self,
        _: SpaceId,
        _: Option<&Config>,
    ) -> futures::future::BoxFuture<'_, K2Result<DynSpaceHandler>> {
        Box::pin(async move {
            let s: DynSpaceHandler = Arc::new(NoopSpaceHandler);
            Ok(s)
        })
    }
}

async fn build_dyn_space_inner(
    op_store: Arc<KvOpStore>,
    shim: Arc<K2OpStoreShim>,
    space_id: SpaceId,
) -> HolographWireResult<kitsune2_api::DynSpace> {
    use kitsune2_core::default_test_builder;
    use kitsune2_test_utils::agent::TestVerifier;

    let relay_url = std::env::var("HOLOGRAPH_IROH_RELAY_URL").ok();
    let boot_url = std::env::var("HOLOGRAPH_BOOTSTRAP_URL").ok();
    let shim_factory = Arc::new(ShimFactory { op_store, shim });

    let builder = if let Some(url) = relay_url.as_deref() {
        // Cross-process path: Iroh transport (QUIC + relay-assisted
        // hole-punching) + CoreBootstrap (peer discovery via
        // kitsune2-bootstrap-srv). The kitsune2-bootstrap-srv binary
        // doubles as the iroh relay at `<addr>/relay` (per K2's
        // test_utils::bootstrap::TestBootstrapSrv pattern).
        use kitsune2_core::factories::CoreBootstrapFactory;
        use kitsune2_core::factories::{CoreBootstrapConfig, CoreBootstrapModConfig};
        use kitsune2_transport_iroh::{
            IrohTransportConfig, IrohTransportFactory, IrohTransportModConfig,
        };
        let allow_plain = std::env::var("HOLOGRAPH_IROH_PLAINTEXT")
            .map(|v| v.trim() == "1")
            .unwrap_or(false);
        let b = Builder {
            // Ed25519 pair (verifier+agent) so cross-process signing
            // round-trips; TestVerifier only accepts the literal
            // TEST_SIG constant which Ed25519LocalAgent doesn't
            // produce.
            verifier: Arc::new(kitsune2_core::Ed25519Verifier),
            op_store: shim_factory,
            transport: IrohTransportFactory::create(),
            bootstrap: CoreBootstrapFactory::create(),
            gossip: kitsune2_gossip::K2GossipFactory::create(),
            ..default_test_builder()
        }
        .with_default_config()
        .map_err(substrate)?;
        b.config
            .set_module_config(&IrohTransportModConfig {
                iroh_transport: IrohTransportConfig {
                    relay_url: Some(url.to_string()),
                    relay_allow_plain_text: allow_plain,
                    ..Default::default()
                },
            })
            .map_err(substrate)?;
        // CoreBootstrap requires server_url to be set for spaces; for
        // a typical spike test setup the bootstrap server lives at the
        // same host:port as the relay (just without the `/relay` path
        // segment).
        let boot_server = boot_url.clone().unwrap_or_else(|| {
            // Strip trailing "/relay" if present so we get the root URL
            // of the bootstrap-srv.
            url.trim_end_matches("/relay").to_string()
        });
        // Default backoff_min_ms is 5000 (production-safe); for the
        // spike's loopback test we tighten it to 500ms so two
        // conductors converge inside the 15s test deadline. Production
        // / non-test consumers can override via env if they need the
        // default again.
        let backoff_min_ms = std::env::var("HOLOGRAPH_BOOTSTRAP_BACKOFF_MIN_MS")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(500u32);
        b.config
            .set_module_config(&CoreBootstrapModConfig {
                core_bootstrap: CoreBootstrapConfig {
                    server_url: Some(boot_server.clone()),
                    backoff_min_ms,
                    ..Default::default()
                },
            })
            .map_err(substrate)?;
        log::info!(
            "[holograph] DynSpace built with Iroh (relay={}, plain={}) + CoreBootstrap (server={})",
            url,
            allow_plain,
            boot_server
        );
        b
    } else {
        log::debug!("[holograph] HOLOGRAPH_IROH_RELAY_URL unset; using mem transport");
        Builder {
            verifier: Arc::new(TestVerifier),
            op_store: shim_factory,
            ..default_test_builder()
        }
        .with_default_config()
        .map_err(substrate)?
    };

    let kitsune = builder.build().await.map_err(substrate)?;
    kitsune
        .register_handler(Arc::new(NoopKitsuneHandler) as DynKitsuneHandler)
        .await
        .map_err(substrate)?;
    let dyn_space = kitsune.space(space_id, None).await.map_err(substrate)?;
    // Leak the kitsune instance so the DynSpace's transport / fetch /
    // publish modules don't get torn down. Spike-acceptable; PR-B
    // moves ownership into NeighborhoodState.
    let _: &'static kitsune2_api::DynKitsune = Box::leak(Box::new(kitsune));
    Ok(dyn_space)
}

// ----- bookkeeping used by deno op tests + helpers -----

/// Convenience for tests: list current neighborhood handles. Production
/// should not call this in a hot path.
pub fn current_handles() -> Vec<HolographHandle> {
    let rt = HolographRuntime::get();
    let mut out: Vec<_> = rt.neighborhoods.iter().map(|e| *e.key()).collect();
    out.sort_by_key(|h| h.0);
    out
}

// ----- legacy export so existing call sites still compile -----

/// The `LanguageController` / test scaffolding may still hold a name
/// reference; expose the runtime under both `HolographRuntime` and
/// `__HOLOGRAPH_DELEGATE__` for compat.
pub fn runtime() -> &'static HolographRuntime {
    HolographRuntime::get()
}

// ----- a typed view that other modules can use without depending on
// the deno op layer -----

#[derive(Debug, Default)]
pub struct WireDiffBuilder {
    additions: Vec<serde_json::Value>,
    removals: Vec<serde_json::Value>,
}

impl WireDiffBuilder {
    pub fn add(mut self, v: serde_json::Value) -> Self {
        self.additions.push(v);
        self
    }
    pub fn remove(mut self, v: serde_json::Value) -> Self {
        self.removals.push(v);
        self
    }
    pub fn build(self) -> WireDiff {
        WireDiff {
            additions: self.additions,
            removals: self.removals,
        }
    }
}

#[cfg(test)]
mod tests;

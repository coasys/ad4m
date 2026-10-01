//! The `ad4m_language!` macro. Spec §9 (Rust ALDK).
//!
//! Usage:
//! ```ignore
//! use ad4m_ldk::prelude::*;
//!
//! pub struct MyLang { /* ... */ }
//!
//! impl Language for MyLang { /* ... */ }
//! impl PerspectiveCommitCapability for MyLang { /* ... */ }
//! impl PerspectiveSyncCapability for MyLang { /* ... */ }
//!
//! ad4m_language! {
//!     language: MyLang,
//!     capabilities: [perspective_commit, perspective_sync, peers],
//!     holochain_signal: true,
//! }
//! ```
//!
//! The macro emits:
//!   * a thread_local `LanguageSlot<MyLang>` (see `state.rs`). The runtime
//!     does NOT guarantee serial calls: a second call can arrive while an
//!     async call waits on a host import. The slot's async lock makes
//!     async calls wait their turn; a sync call that finds the language
//!     busy, or any call before `init()`, returns a `LanguageError`
//!     instead of panicking (a panic aborts the whole WASM module).
//!     A panic hook records a panic in the language's own code; every
//!     later call then returns a "panicked" `LanguageError` instead of
//!     waiting for the lock that the aborted call still holds.
//!   * lifecycle exports: `name`, `version`, `isPublic`, `init`, `teardown`, `interactions`
//!   * capability exports — **only** for the listed capabilities. The WASM
//!     export table therefore carries exactly the functions the runtime
//!     uses for capability detection.
//!
//! All emitted exports are `#[wasm_bindgen]` functions so wasm-bindgen
//! glue handles the JS ⇄ Rust value marshalling.

#[macro_export]
macro_rules! ad4m_language {
    (
        language: $lang:ty,
        capabilities: [$($cap:ident),* $(,)?]
        $(, holochain_signal: $hc_signal:tt)?
        $(,)?
    ) => {
        thread_local! {
            static __AD4M_LANG_STATE: $crate::state::LanguageSlot<$lang> =
                const { $crate::state::LanguageSlot::new() };
        }

        /// Run a sync trait method. Errors (never panics) when the
        /// language is not initialized or an async call holds it.
        fn __ad4m_with<R>(
            op: &str,
            f: impl FnOnce(&mut $lang) -> R,
        ) -> $crate::errors::LanguageResult<R> {
            __AD4M_LANG_STATE.with(|slot| slot.try_with(op, f))
        }

        /// The shared instance for an async shim. The shim awaits
        /// `.lock()` on it, so overlapping async calls run one after
        /// the other.
        fn __ad4m_instance(
            op: &str,
        ) -> $crate::errors::LanguageResult<
            ::std::rc::Rc<$crate::__futures::lock::Mutex<$lang>>,
        > {
            __AD4M_LANG_STATE.with(|slot| slot.get(op))
        }

        fn __ad4m_mark_panicked(message: &str) {
            let _ = __AD4M_LANG_STATE.try_with(|slot| slot.mark_panicked(message));
        }

        // -------- Lifecycle --------

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "name")]
        pub fn __ad4m_name() -> String {
            <$lang as $crate::traits::Language>::name().to_string()
        }

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "version")]
        pub fn __ad4m_version() -> String {
            <$lang as $crate::traits::Language>::version().to_string()
        }

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "isPublic")]
        pub fn __ad4m_is_public() -> bool {
            <$lang as $crate::traits::Language>::is_public()
        }

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "init")]
        pub async fn __ad4m_init() -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            // Awaiting the async trait method lets languages call
            // Promise-returning imports (holochain_register_dnas,
            // holochain_call, etc.) during initialization. wasm-bindgen
            // emits a JS `async function init()` so the bootstrap shim's
            // `await mod.init()` transparently waits for completion.
            let instance = <$lang as $crate::traits::Language>::init().await?;
            __AD4M_LANG_STATE.with(|slot| slot.set(instance));
            // After `Language::init()`, so the hook wraps one the language
            // installed there.
            static __AD4M_PANIC_HOOK: ::std::sync::Once = ::std::sync::Once::new();
            __AD4M_PANIC_HOOK.call_once(|| $crate::state::install_panic_marker(__ad4m_mark_panicked));
            Ok(())
        }

        /// Async so it can wait for an in-flight async call to finish
        /// before the teardown hook runs. The runtime always awaits
        /// `language.teardown()`. The slot empties first, so calls that
        /// arrive during teardown get "not initialized". After a panic it
        /// skips the hook: the aborted call never releases the lock.
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "teardown")]
        pub async fn __ad4m_teardown() -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let (taken, panicked) = __AD4M_LANG_STATE.with(|slot| (slot.take(), slot.has_panicked()));
            if let (Some(m), false) = (taken, panicked) {
                let mut guard = m.lock().await;
                <$lang as $crate::traits::Language>::teardown(&mut *guard)?;
            }
            Ok(())
        }

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "interactions")]
        pub fn __ad4m_interactions(address: String) -> ::wasm_bindgen::JsValue {
            // NULL on any error, as for serde errors: the runtime maps a
            // non-array result to "no interactions".
            match __ad4m_with("interactions", |l| <$lang as $crate::traits::Language>::interactions(l, address)) {
                Ok(v) => $crate::__serde::to_js(&v).unwrap_or(::wasm_bindgen::JsValue::NULL),
                Err(_) => ::wasm_bindgen::JsValue::NULL,
            }
        }

        /// Execute a named interaction. Spec §5.7 — the runtime calls
        /// this when the JS object returned from `interactions()` has
        /// no callable `execute` field (always the case for Rust
        /// languages, since interaction lists cross the wasm-bindgen
        /// boundary as plain JSON).
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionInteract")]
        pub fn __ad4m_expression_interact(
            address: String,
            name: String,
            parameters: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let params: ::serde_json::Value = ::serde_wasm_bindgen::from_value(parameters)
                .map_err($crate::errors::LanguageError::from)?;
            let result = __ad4m_with("expressionInteract", |l| {
                <$lang as $crate::traits::Language>::expression_interact(l, address, name, params)
            })??;
            Ok(match result {
                Some(v) => $crate::__serde::to_js(&v)
                    .map_err($crate::errors::LanguageError::from)?,
                None => ::wasm_bindgen::JsValue::NULL,
            })
        }

        // -------- Capability shims --------
        $( $crate::__ad4m_cap!($cap, $lang); )*

        // -------- Optional Holochain signal handler --------
        $crate::__ad4m_maybe_hc_signal!($lang $(, $hc_signal)?);
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __ad4m_cap {
    (expression, $lang:ty) => {
        // Async shims: the trait methods return `impl Future` so they
        // can call async host imports (`holochain_call`, etc.). The
        // runtime can start a second call while the first one waits on
        // such an import. Each shim holds the slot's async lock across
        // the await, so a concurrent call waits for its turn. The guard
        // releases when the method completes or its future drops.

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionCreate")]
        pub async fn __ad4m_expression_create(
            content: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<String, ::wasm_bindgen::JsValue> {
            let v: ::serde_json::Value = ::serde_wasm_bindgen::from_value(content)
                .map_err($crate::errors::LanguageError::from)?;
            let m = __ad4m_instance("expressionCreate")?;
            let mut guard = m.lock().await;
            Ok(<$lang as $crate::traits::ExpressionCapability>::expression_create(&mut *guard, v).await?)
        }

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionGet")]
        pub async fn __ad4m_expression_get(
            address: String,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let m = __ad4m_instance("expressionGet")?;
            let exp = {
                let mut guard = m.lock().await;
                <$lang as $crate::traits::ExpressionCapability>::expression_get(&mut *guard, address).await?
            };
            Ok($crate::__serde::to_js(&exp).map_err($crate::errors::LanguageError::from)?)
        }
    };

    (perspective_commit, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveCommit")]
        pub fn __ad4m_perspective_commit(
            diff: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let d: $crate::types::PerspectiveDiff = ::serde_wasm_bindgen::from_value(diff)
                .map_err($crate::errors::LanguageError::from)?;
            __ad4m_with("perspectiveCommit", |l| <$lang as $crate::traits::PerspectiveCommitCapability>::perspective_commit(l, d))??;
            Ok(())
        }
    };

    (perspective_sync, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncSync")]
        pub fn __ad4m_perspective_sync_sync()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let d = __ad4m_with("perspectiveSyncSync", |l| <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_sync(l))??;
            Ok($crate::__serde::to_js(&d).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncRender")]
        pub fn __ad4m_perspective_sync_render()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let p = __ad4m_with("perspectiveSyncRender", |l| <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_render(l))??;
            Ok($crate::__serde::to_js(&p).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncCurrentRevision")]
        pub fn __ad4m_perspective_sync_current_revision()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let r = __ad4m_with("perspectiveSyncCurrentRevision", |l| <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_current_revision(l))??;
            Ok(match r {
                Some(s) => ::wasm_bindgen::JsValue::from_str(&s),
                None => ::wasm_bindgen::JsValue::NULL,
            })
        }
    };

    (perspective_query, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveQuerySupportedKinds")]
        pub fn __ad4m_perspective_query_supported_kinds() -> ::wasm_bindgen::JsValue {
            match __ad4m_with("perspectiveQuerySupportedKinds", |l| <$lang as $crate::traits::PerspectiveQueryCapability>::perspective_query_supported_kinds(l)) {
                Ok(v) => $crate::__serde::to_js(&v).unwrap_or(::wasm_bindgen::JsValue::NULL),
                Err(_) => ::wasm_bindgen::JsValue::NULL,
            }
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveQueryRun")]
        pub fn __ad4m_perspective_query_run(
            request: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let r: $crate::types::QueryRequest = ::serde_wasm_bindgen::from_value(request)
                .map_err($crate::errors::LanguageError::from)?;
            let resp = __ad4m_with("perspectiveQueryRun", |l| <$lang as $crate::traits::PerspectiveQueryCapability>::perspective_query_run(l, r))??;
            Ok($crate::__serde::to_js(&resp).map_err($crate::errors::LanguageError::from)?)
        }
    };

    (peers, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "peersSetLocal")]
        pub fn __ad4m_peers_set_local(
            agents: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let v: Vec<String> = ::serde_wasm_bindgen::from_value(agents)
                .map_err($crate::errors::LanguageError::from)?;
            __ad4m_with("peersSetLocal", |l| <$lang as $crate::traits::PeersCapability>::peers_set_local(l, v))??;
            Ok(())
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "peersRemote")]
        pub fn __ad4m_peers_remote()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let v = __ad4m_with("peersRemote", |l| <$lang as $crate::traits::PeersCapability>::peers_remote(l))??;
            Ok($crate::__serde::to_js(&v).map_err($crate::errors::LanguageError::from)?)
        }
    };

    (language_source, $lang:ty) => {
        // Async shim — same lock-across-await pattern as expression.
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "languageGetSource")]
        pub async fn __ad4m_language_get_source(
            address: String,
        ) -> ::std::result::Result<String, ::wasm_bindgen::JsValue> {
            let m = __ad4m_instance("languageGetSource")?;
            let mut guard = m.lock().await;
            Ok(<$lang as $crate::traits::LanguageSourceCapability>::language_get_source(&mut *guard, address).await?)
        }
    };

    (telepresence, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceSetOnlineStatus")]
        pub fn __ad4m_telepresence_set_online_status(
            status: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let s: ::serde_json::Value = ::serde_wasm_bindgen::from_value(status)
                .map_err($crate::errors::LanguageError::from)?;
            __ad4m_with("telepresenceSetOnlineStatus", |l| <$lang as $crate::traits::TelepresenceCapability>::telepresence_set_online_status(l, s))??;
            Ok(())
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceGetOnlineAgents")]
        pub fn __ad4m_telepresence_get_online_agents()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let v = __ad4m_with("telepresenceGetOnlineAgents", |l| <$lang as $crate::traits::TelepresenceCapability>::telepresence_get_online_agents(l))??;
            Ok($crate::__serde::to_js(&v).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceSendSignal")]
        pub fn __ad4m_telepresence_send_signal(
            remote_did: String,
            payload: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let p: ::serde_json::Value = ::serde_wasm_bindgen::from_value(payload)
                .map_err($crate::errors::LanguageError::from)?;
            let r = __ad4m_with("telepresenceSendSignal", |l| <$lang as $crate::traits::TelepresenceCapability>::telepresence_send_signal(l, remote_did, p))??;
            Ok($crate::__serde::to_js(&r).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceSendBroadcast")]
        pub fn __ad4m_telepresence_send_broadcast(
            payload: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let p: ::serde_json::Value = ::serde_wasm_bindgen::from_value(payload)
                .map_err($crate::errors::LanguageError::from)?;
            let r = __ad4m_with("telepresenceSendBroadcast", |l| <$lang as $crate::traits::TelepresenceCapability>::telepresence_send_broadcast(l, p))??;
            Ok($crate::__serde::to_js(&r).map_err($crate::errors::LanguageError::from)?)
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __ad4m_maybe_hc_signal {
    ($lang:ty) => {};
    ($lang:ty, true) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "handleHolochainSignal")]
        pub fn __ad4m_handle_holochain_signal(
            signal: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let s: ::serde_json::Value = ::serde_wasm_bindgen::from_value(signal)
                .map_err($crate::errors::LanguageError::from)?;
            __ad4m_with("handleHolochainSignal", |l| <$lang as $crate::traits::HolochainSignalHandler>::handle_holochain_signal(l, s))??;
            Ok(())
        }
    };
    ($lang:ty, false) => {};
}

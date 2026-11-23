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
//!     concurrent: [peers_remote],
//! }
//! ```
//!
//! The macro emits:
//!   * a thread_local `LanguageSlot<MyLang>` (see `state.rs`). The runtime
//!     does NOT guarantee serial calls: a second call can arrive while an
//!     async call waits on a host import. The slot's read/write lock goes
//!     by each export's declared kind: exports named in `concurrent:`
//!     share the read side (they overlap), every other export takes the
//!     write side alone, and a sync `&self` export answers beside held
//!     reads, reporting "busy" only while an ordered call holds the
//!     instance. Only `&self` methods may be declared `concurrent:` — a
//!     `&mut` method needs the instance alone, so naming one aborts the
//!     module with an explanatory panic. Every call before `init()` (or
//!     after `teardown()`) returns a "not initialized" `LanguageError`,
//!     never a panic (a panic aborts the whole WASM module).
//!     A panic hook records a panic in the language's own code; every
//!     later call then returns a "panicked" `LanguageError` instead of
//!     waiting for the lock that the aborted call still holds.
//!   * lifecycle exports: `name`, `version`, `isPublic`, `init`, `teardown`, `interactions`
//!   * capability exports — **only** for the listed capabilities. The WASM
//!     export table therefore carries exactly the functions the runtime
//!     uses for capability detection.
//!
//! All emitted exports are `#[wasm_bindgen]` functions so wasm-bindgen
//! glue handles the JS ⇄ Rust value marshalling. Exports that can suspend
//! — every `async` export and every `&mut` sync export — return a
//! `Promise` built by `agent::export_future`, which pins the calling
//! call's agent token for every host import the body makes; sync `&self`
//! exports still answer directly.

#[macro_export]
macro_rules! ad4m_language {
    (
        language: $lang:ty,
        capabilities: [$($cap:ident),* $(,)?]
        $(, holochain_signal: $hc_signal:tt)?
        $(, concurrent: [$($conc:ident),* $(,)?])?
        $(,)?
    ) => {
        thread_local! {
            static __AD4M_LANG_STATE: $crate::state::LanguageSlot<$lang> =
                const { $crate::state::LanguageSlot::new() };
        }

        /// The exports that share the instance's read side, by Rust method
        /// name; every other export takes the write side alone.
        const __AD4M_CONCURRENT: &[&'static str] = &[$( $(stringify!($conc),)* )?];

        fn __ad4m_access(method: &'static str) -> $crate::state::Access {
            if __AD4M_CONCURRENT.contains(&method) {
                $crate::state::Access::Read
            } else {
                $crate::state::Access::Write
            }
        }

        /// Run a sync `&self` trait method against the shared read side.
        /// Errors (never panics) when the language is not initialized or an
        /// ordered call holds it.
        fn __ad4m_with<R>(
            op: &str,
            f: impl FnOnce(&$lang) -> R,
        ) -> $crate::errors::LanguageResult<R> {
            __AD4M_LANG_STATE.with(|slot| slot.try_with(op, f))
        }

        /// The shared instance for an export's future. It acquires the side
        /// the export's declared kind says: read for `concurrent:`, write
        /// for the rest.
        fn __ad4m_instance(
            op: &str,
        ) -> $crate::errors::LanguageResult<$crate::state::Instance<$lang>> {
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
        pub fn __ad4m_init() -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                // Awaiting the async trait method lets languages call
                // Promise-returning imports (holochain_register_dnas,
                // holochain_call, etc.) during initialization.
                let instance = <$lang as $crate::traits::Language>::init().await?;
                __AD4M_LANG_STATE.with(|slot| slot.set(instance));
                // After `Language::init()`, so the hook wraps one the language
                // installed there.
                static __AD4M_PANIC_HOOK: ::std::sync::Once = ::std::sync::Once::new();
                __AD4M_PANIC_HOOK.call_once(|| $crate::state::install_panic_marker(__ad4m_mark_panicked));
                Ok(::wasm_bindgen::JsValue::UNDEFINED)
            })
        }

        /// Async so it can wait for an in-flight call to finish before the
        /// teardown hook runs. The runtime always awaits
        /// `language.teardown()`. The slot empties first, so calls that
        /// arrive during teardown get "not initialized". After a panic,
        /// including one while teardown waits, it skips the hook: the
        /// aborted call never releases the lock.
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "teardown")]
        pub fn __ad4m_teardown() -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let (taken, panicked) =
                    __AD4M_LANG_STATE.with(|slot| (slot.take(), slot.has_panicked()));
                if let (Some(instance), false) = (taken, panicked) {
                    if let Ok(mut guard) = instance.write("teardown").await {
                        <$lang as $crate::traits::Language>::teardown(&mut *guard)?;
                    }
                }
                Ok(::wasm_bindgen::JsValue::UNDEFINED)
            })
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
        ///
        /// A `&mut` method, so it waits for the write side and runs alone.
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionInteract")]
        pub fn __ad4m_expression_interact(
            address: String,
            name: String,
            parameters: ::wasm_bindgen::JsValue,
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let params: ::serde_json::Value = ::serde_wasm_bindgen::from_value(parameters)
                    .map_err($crate::errors::LanguageError::from)?;
                let m = __ad4m_instance("expressionInteract")?;
                let mut guard = m
                    .acquire(__ad4m_access("expression_interact"), "expressionInteract")
                    .await?;
                let result = <$lang as $crate::traits::Language>::expression_interact(
                    &mut *guard,
                    address,
                    name,
                    params,
                )?;
                Ok(match result {
                    Some(v) => $crate::__serde::to_js(&v)
                        .map_err($crate::errors::LanguageError::from)?,
                    None => ::wasm_bindgen::JsValue::NULL,
                })
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
        // such an import. Each export's future holds the lock side its
        // declared kind says — `concurrent:` shares the read side,
        // otherwise the write side — across every await; the guard
        // releases when the method completes or its future drops.

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionCreate")]
        pub fn __ad4m_expression_create(
            content: ::wasm_bindgen::JsValue,
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let v: ::serde_json::Value = ::serde_wasm_bindgen::from_value(content)
                    .map_err($crate::errors::LanguageError::from)?;
                let m = __ad4m_instance("expressionCreate")?;
                let mut guard = m
                    .acquire(__ad4m_access("expression_create"), "expressionCreate")
                    .await?;
                let a = <$lang as $crate::traits::ExpressionCapability>::expression_create(&mut *guard, v).await?;
                Ok(::wasm_bindgen::JsValue::from_str(&a))
            })
        }

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionGet")]
        pub fn __ad4m_expression_get(
            address: String,
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let m = __ad4m_instance("expressionGet")?;
                let exp = {
                    let guard = m
                        .acquire(__ad4m_access("expression_get"), "expressionGet")
                        .await?;
                    <$lang as $crate::traits::ExpressionCapability>::expression_get(&*guard, address).await?
                };
                Ok($crate::__serde::to_js(&exp)?)
            })
        }
    };

    (identity, $lang:ty) => {
        // Async shims, as for expression: the store is reached through
        // `httpFetch`, so each call holds its lock side across the await.
        // Numbers and optional arguments arrive as `JsValue` (a JS number
        // is not a wasm-bindgen u64); answers go out through `to_json_js`,
        // so `None` is `null`. `self`/`mut` is the receiver: read-only
        // methods take `&self`, mutating ones `&mut self`.
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityLog",
            __ad4m_identity_log, identity_log, self, (did: String), [from: u64]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityHead",
            __ad4m_identity_head, identity_head, self, (did: String), []);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityAppend",
            __ad4m_identity_append, identity_append, mut, (did: String), [req: $crate::identity::AppendRequest]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityKeyOwner",
            __ad4m_identity_key_owner, identity_key_owner, self, (multikey: String), [], default [wait_s: u8]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityPowParams",
            __ad4m_identity_pow_params, identity_pow_params, self, (), []);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityAttachNote",
            __ad4m_identity_attach_note, identity_attach_note, mut, (did: String, token: String), [req: $crate::identity::SignedRequest]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityPutAgent",
            __ad4m_identity_put_agent, identity_put_agent, mut, (), [record: $crate::identity::AgentRecord, req: Option<$crate::identity::SignedRequest>]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityOpenOffer",
            __ad4m_identity_open_offer, identity_open_offer, mut, (), [offer: $crate::identity::SignedOffer]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityListOffers",
            __ad4m_identity_list_offers, identity_list_offers, self, (did: String), [req: $crate::identity::SignedRequest], default [wait_s: u8]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityCancelOffer",
            __ad4m_identity_cancel_offer, identity_cancel_offer, mut, (did: String, offer_id: String), [req: $crate::identity::SignedRequest]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityClaimOffer",
            __ad4m_identity_claim_offer, identity_claim_offer, mut, (did: String, offer_id: String), [claim: $crate::identity::Claim]);
        $crate::__ad4m_async_shim!($lang, IdentityCapability, "identityOfferState",
            __ad4m_identity_offer_state, identity_offer_state, self, (did: String, offer_id: String, key: String), [], default [wait_s: u8]);
    };

    (token, $lang:ty) => {
        $crate::__ad4m_async_shim!($lang, TokenCapability, "tokenAssets",
            __ad4m_token_assets, token_assets, self, (), []);
        $crate::__ad4m_async_shim!($lang, TokenCapability, "tokenBalance",
            __ad4m_token_balance, token_balance, self, (account: String, asset: String), []);
        $crate::__ad4m_async_shim!($lang, TokenCapability, "tokenSend",
            __ad4m_token_send, token_send, mut, (to: String, asset: String), [amount: $crate::token::Amount, memo: Option<String>]);
        $crate::__ad4m_async_shim!($lang, TokenCapability, "tokenReceived",
            __ad4m_token_received, token_received, self, (), [cursor: Option<String>, limit: u32]);
    };

    (perspective_commit, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveCommit")]
        pub fn __ad4m_perspective_commit(
            diff: ::wasm_bindgen::JsValue,
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let d: $crate::types::PerspectiveDiff = ::serde_wasm_bindgen::from_value(diff)
                    .map_err($crate::errors::LanguageError::from)?;
                let m = __ad4m_instance("perspectiveCommit")?;
                let mut guard = m
                    .acquire(__ad4m_access("perspective_commit"), "perspectiveCommit")
                    .await?;
                <$lang as $crate::traits::PerspectiveCommitCapability>::perspective_commit(&mut *guard, d)?;
                Ok(::wasm_bindgen::JsValue::UNDEFINED)
            })
        }
    };

    (perspective_sync, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncSync")]
        pub fn __ad4m_perspective_sync_sync() -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let m = __ad4m_instance("perspectiveSyncSync")?;
                let mut guard = m
                    .acquire(__ad4m_access("perspective_sync_sync"), "perspectiveSyncSync")
                    .await?;
                let d = <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_sync(&mut *guard)?;
                Ok($crate::__serde::to_js(&d)?)
            })
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncRender")]
        pub fn __ad4m_perspective_sync_render() -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let m = __ad4m_instance("perspectiveSyncRender")?;
                let mut guard = m
                    .acquire(__ad4m_access("perspective_sync_render"), "perspectiveSyncRender")
                    .await?;
                let p = <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_render(&mut *guard)?;
                Ok($crate::__serde::to_js(&p)?)
            })
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncCurrentRevision")]
        pub fn __ad4m_perspective_sync_current_revision() -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let m = __ad4m_instance("perspectiveSyncCurrentRevision")?;
                let mut guard = m
                    .acquire(__ad4m_access("perspective_sync_current_revision"), "perspectiveSyncCurrentRevision")
                    .await?;
                let r = <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_current_revision(&mut *guard)?;
                Ok(match r {
                    Some(s) => ::wasm_bindgen::JsValue::from_str(&s),
                    None => ::wasm_bindgen::JsValue::NULL,
                })
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
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let r: $crate::types::QueryRequest = ::serde_wasm_bindgen::from_value(request)
                    .map_err($crate::errors::LanguageError::from)?;
                let m = __ad4m_instance("perspectiveQueryRun")?;
                let mut guard = m
                    .acquire(__ad4m_access("perspective_query_run"), "perspectiveQueryRun")
                    .await?;
                let resp = <$lang as $crate::traits::PerspectiveQueryCapability>::perspective_query_run(&mut *guard, r)?;
                Ok($crate::__serde::to_js(&resp)?)
            })
        }
    };

    (peers, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "peersSetLocal")]
        pub fn __ad4m_peers_set_local(
            agents: ::wasm_bindgen::JsValue,
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let v: Vec<String> = ::serde_wasm_bindgen::from_value(agents)
                    .map_err($crate::errors::LanguageError::from)?;
                let m = __ad4m_instance("peersSetLocal")?;
                let mut guard = m
                    .acquire(__ad4m_access("peers_set_local"), "peersSetLocal")
                    .await?;
                <$lang as $crate::traits::PeersCapability>::peers_set_local(&mut *guard, v)?;
                Ok(::wasm_bindgen::JsValue::UNDEFINED)
            })
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
        // Async shim — same lock-side-across-await pattern as expression.
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "languageGetSource")]
        pub fn __ad4m_language_get_source(
            address: String,
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let m = __ad4m_instance("languageGetSource")?;
                let guard = m
                    .acquire(__ad4m_access("language_get_source"), "languageGetSource")
                    .await?;
                let s = <$lang as $crate::traits::LanguageSourceCapability>::language_get_source(&*guard, address).await?;
                Ok(::wasm_bindgen::JsValue::from_str(&s))
            })
        }
    };

    (telepresence, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceSetOnlineStatus")]
        pub fn __ad4m_telepresence_set_online_status(
            status: ::wasm_bindgen::JsValue,
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let s: ::serde_json::Value = ::serde_wasm_bindgen::from_value(status)
                    .map_err($crate::errors::LanguageError::from)?;
                let m = __ad4m_instance("telepresenceSetOnlineStatus")?;
                let mut guard = m
                    .acquire(__ad4m_access("telepresence_set_online_status"), "telepresenceSetOnlineStatus")
                    .await?;
                <$lang as $crate::traits::TelepresenceCapability>::telepresence_set_online_status(&mut *guard, s)?;
                Ok(::wasm_bindgen::JsValue::UNDEFINED)
            })
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

/// One async capability shim. `self`/`mut` is the trait method's receiver
/// — `&self` methods may be declared `concurrent:` and then share the
/// read side; `&mut` methods always run alone. `(name: String, …)` are
/// JS strings passed as they are; `[name: Type, …]` arrive as `JsValue`
/// and deserialise into `Type`; `default [name: Type, …]`, last, are
/// optional JS arguments that read as `Type::default()` when absent (a
/// `waitSec` the caller left out is 0).
#[doc(hidden)]
#[macro_export]
macro_rules! __ad4m_async_shim {
    ($lang:ty, $trait:ident, $js:literal, $shim:ident, $method:ident, $recv:tt,
     ($($s:ident: String),*), [$($v:ident: $vty:ty),*]
     $(, default [$($o:ident: $oty:ty),*])?) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = $js)]
        pub fn $shim(
            $($s: String,)*
            $($v: ::wasm_bindgen::JsValue,)*
            $($($o: ::wasm_bindgen::JsValue,)*)?
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                $(let $v: $vty = $crate::__serde::from_js($v)?;)*
                $($(let $o: $oty = $crate::__serde::from_js::<Option<$oty>>($o)?.unwrap_or_default();)*)?
                let m = __ad4m_instance($js)?;
                let answer = {
                    #[allow(unused_mut)]
                    let mut guard = m.acquire(__ad4m_access(stringify!($method)), $js).await?;
                    <$lang as $crate::traits::$trait>::$method(
                        $crate::__ad4m_borrow!($recv, guard),
                        $($s,)* $($v,)* $($($o,)*)?
                    ).await?
                };
                $crate::__serde::to_json_js(&answer).map_err(::wasm_bindgen::JsValue::from)
            })
        }
    };
}

/// The receiver a generated shim borrows from its guard: `self` is the
/// shared `&T` (a `concurrent:`-declared export's read side gives it),
/// `mut` is `&mut T` (needs the write side, so it can never be declared
/// `concurrent:` — a `Read` guard reaching it aborts the module).
#[doc(hidden)]
#[macro_export]
macro_rules! __ad4m_borrow {
    (self, $guard:ident) => {
        &*$guard
    };
    (mut, $guard:ident) => {
        &mut *$guard
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
        ) -> ::js_sys::Promise {
            $crate::agent::export_future(async move {
                let s: ::serde_json::Value = ::serde_wasm_bindgen::from_value(signal)
                    .map_err($crate::errors::LanguageError::from)?;
                let m = __ad4m_instance("handleHolochainSignal")?;
                let mut guard = m
                    .acquire(
                        __ad4m_access("handle_holochain_signal"),
                        "handleHolochainSignal",
                    )
                    .await?;
                <$lang as $crate::traits::HolochainSignalHandler>::handle_holochain_signal(
                    &mut *guard,
                    s,
                )?;
                Ok(::wasm_bindgen::JsValue::UNDEFINED)
            })
        }
    };
    ($lang:ty, false) => {};
}

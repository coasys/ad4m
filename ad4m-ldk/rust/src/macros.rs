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
//!   * a thread_local state slot for an `Option<MyLang>`
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
            static __AD4M_LANG_STATE: ::std::cell::RefCell<Option<$lang>> =
                ::std::cell::RefCell::new(None);
        }

        fn __ad4m_with<R>(f: impl FnOnce(&mut $lang) -> R) -> R {
            __AD4M_LANG_STATE.with(|cell| {
                let mut b = cell.borrow_mut();
                let v = b.as_mut().expect("Language not initialized; init() must be called first");
                f(v)
            })
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
        pub fn __ad4m_init() -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let instance = <$lang as $crate::traits::Language>::init()?;
            __AD4M_LANG_STATE.with(|c| *c.borrow_mut() = Some(instance));
            Ok(())
        }

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "teardown")]
        pub fn __ad4m_teardown() -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            __AD4M_LANG_STATE.with(|c| {
                if let Some(mut inst) = c.borrow_mut().take() {
                    <$lang as $crate::traits::Language>::teardown(&mut inst)?;
                }
                Ok::<(), $crate::errors::LanguageError>(())
            })?;
            Ok(())
        }

        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "interactions")]
        pub fn __ad4m_interactions() -> ::wasm_bindgen::JsValue {
            let v = __ad4m_with(|l| <$lang as $crate::traits::Language>::interactions(l));
            ::serde_wasm_bindgen::to_value(&v).unwrap_or(::wasm_bindgen::JsValue::NULL)
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
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionCreate")]
        pub fn __ad4m_expression_create(
            content: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<String, ::wasm_bindgen::JsValue> {
            let v: ::serde_json::Value = ::serde_wasm_bindgen::from_value(content)
                .map_err($crate::errors::LanguageError::from)?;
            let addr = __ad4m_with(|l| <$lang as $crate::traits::ExpressionCapability>::expression_create(l, v))?;
            Ok(addr)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionGet")]
        pub fn __ad4m_expression_get(
            address: String,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let exp = __ad4m_with(|l| <$lang as $crate::traits::ExpressionCapability>::expression_get(l, address))?;
            Ok(::serde_wasm_bindgen::to_value(&exp).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "expressionInteract")]
        pub fn __ad4m_expression_interact(
            address: String,
            interaction: String,
            params: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let p: ::serde_json::Value = ::serde_wasm_bindgen::from_value(params)
                .map_err($crate::errors::LanguageError::from)?;
            let r = __ad4m_with(|l| <$lang as $crate::traits::ExpressionCapability>::expression_interact(l, address, interaction, p))?;
            Ok(::serde_wasm_bindgen::to_value(&r).map_err($crate::errors::LanguageError::from)?)
        }
    };

    (perspective_commit, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveCommit")]
        pub fn __ad4m_perspective_commit(
            diff: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let d: $crate::types::PerspectiveDiff = ::serde_wasm_bindgen::from_value(diff)
                .map_err($crate::errors::LanguageError::from)?;
            __ad4m_with(|l| <$lang as $crate::traits::PerspectiveCommitCapability>::perspective_commit(l, d))?;
            Ok(())
        }
    };

    (perspective_sync, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncSync")]
        pub fn __ad4m_perspective_sync_sync()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let d = __ad4m_with(|l| <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_sync(l))?;
            Ok(::serde_wasm_bindgen::to_value(&d).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncRender")]
        pub fn __ad4m_perspective_sync_render()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let p = __ad4m_with(|l| <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_render(l))?;
            Ok(::serde_wasm_bindgen::to_value(&p).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveSyncCurrentRevision")]
        pub fn __ad4m_perspective_sync_current_revision()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let r = __ad4m_with(|l| <$lang as $crate::traits::PerspectiveSyncCapability>::perspective_sync_current_revision(l))?;
            Ok(match r {
                Some(s) => ::wasm_bindgen::JsValue::from_str(&s),
                None => ::wasm_bindgen::JsValue::NULL,
            })
        }
    };

    (perspective_query, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveQuerySupportedKinds")]
        pub fn __ad4m_perspective_query_supported_kinds() -> ::wasm_bindgen::JsValue {
            let v = __ad4m_with(|l| <$lang as $crate::traits::PerspectiveQueryCapability>::perspective_query_supported_kinds(l));
            ::serde_wasm_bindgen::to_value(&v).unwrap_or(::wasm_bindgen::JsValue::NULL)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "perspectiveQueryRun")]
        pub fn __ad4m_perspective_query_run(
            request: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let r: $crate::types::QueryRequest = ::serde_wasm_bindgen::from_value(request)
                .map_err($crate::errors::LanguageError::from)?;
            let resp = __ad4m_with(|l| <$lang as $crate::traits::PerspectiveQueryCapability>::perspective_query_run(l, r))?;
            Ok(::serde_wasm_bindgen::to_value(&resp).map_err($crate::errors::LanguageError::from)?)
        }
    };

    (peers, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "peersSetLocal")]
        pub fn __ad4m_peers_set_local(
            agents: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let v: Vec<String> = ::serde_wasm_bindgen::from_value(agents)
                .map_err($crate::errors::LanguageError::from)?;
            __ad4m_with(|l| <$lang as $crate::traits::PeersCapability>::peers_set_local(l, v))?;
            Ok(())
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "peersRemote")]
        pub fn __ad4m_peers_remote()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let v = __ad4m_with(|l| <$lang as $crate::traits::PeersCapability>::peers_remote(l))?;
            Ok(::serde_wasm_bindgen::to_value(&v).map_err($crate::errors::LanguageError::from)?)
        }
    };

    (telepresence, $lang:ty) => {
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceSetOnlineStatus")]
        pub fn __ad4m_telepresence_set_online_status(
            status: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<(), ::wasm_bindgen::JsValue> {
            let s: ::serde_json::Value = ::serde_wasm_bindgen::from_value(status)
                .map_err($crate::errors::LanguageError::from)?;
            __ad4m_with(|l| <$lang as $crate::traits::TelepresenceCapability>::telepresence_set_online_status(l, s))?;
            Ok(())
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceGetOnlineAgents")]
        pub fn __ad4m_telepresence_get_online_agents()
            -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue>
        {
            let v = __ad4m_with(|l| <$lang as $crate::traits::TelepresenceCapability>::telepresence_get_online_agents(l))?;
            Ok(::serde_wasm_bindgen::to_value(&v).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceSendSignal")]
        pub fn __ad4m_telepresence_send_signal(
            remote_did: String,
            payload: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let p: ::serde_json::Value = ::serde_wasm_bindgen::from_value(payload)
                .map_err($crate::errors::LanguageError::from)?;
            let r = __ad4m_with(|l| <$lang as $crate::traits::TelepresenceCapability>::telepresence_send_signal(l, remote_did, p))?;
            Ok(::serde_wasm_bindgen::to_value(&r).map_err($crate::errors::LanguageError::from)?)
        }
        #[::wasm_bindgen::prelude::wasm_bindgen(js_name = "telepresenceSendBroadcast")]
        pub fn __ad4m_telepresence_send_broadcast(
            payload: ::wasm_bindgen::JsValue,
        ) -> ::std::result::Result<::wasm_bindgen::JsValue, ::wasm_bindgen::JsValue> {
            let p: ::serde_json::Value = ::serde_wasm_bindgen::from_value(payload)
                .map_err($crate::errors::LanguageError::from)?;
            let r = __ad4m_with(|l| <$lang as $crate::traits::TelepresenceCapability>::telepresence_send_broadcast(l, p))?;
            Ok(::serde_wasm_bindgen::to_value(&r).map_err($crate::errors::LanguageError::from)?)
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
            __ad4m_with(|l| <$lang as $crate::traits::HolochainSignalHandler>::handle_holochain_signal(l, s))?;
            Ok(())
        }
    };
    ($lang:ty, false) => {};
}
